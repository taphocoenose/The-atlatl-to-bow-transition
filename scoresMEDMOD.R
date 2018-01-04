# last edited December 8, 2017 by Ryan Breslawski

# DESCRIPTION

# This script reads two csv files into data frames: bowdata.csv 
# and atlatldata.csv. The script converts these data frames into
# datasets containing only competitors who competed for 4+ years.
# Additionally, for each competitor, only the highest score for 
# each year is retained.

# The script then fits six errors-in-variable models to the median
# scores for each competition year in the converted datasets
# using RStan. Finally, the fitted models are plotted.

# This script also redraws the median-score figures from Grund 2017.

# The script has three parts:
#   Part 1: Read, clean, and sort data. Lines 23-196.
#   Part 2: Fit median-score models. Lines 199-313.
#   Part 3: Plots. Lines 316-604.


##################################################################
############### Part 1: Read, clean, and sort data ###############
##################################################################

# Load Rethinking and RStan libraries. 
library(rethinking)
library(rstan)

# FUNCTION: Convert atlatl score of the 00xxx format
# into a number with decimal places, where each 'x'
# in the string represents 0.1. This takes a string
# as the sole argument and returns a numeric value.
getatlatlscore <- function(scorestring){
  
  scorestring <- as.character(scorestring)
  
  # If the string contains an 'x'...
  if(grepl("x", scorestring, ignore.case=TRUE)){
    
    # Find the position of the first 'x', then extract the prec-
    # preceding characters and convert them to a numeric value. 
    # Convert the string of 'x's to a numeric value representing
    # number of 'x's * 0.1. Add this value to the previously
    # number obtained from the character conversion.
    firstx <- regexpr("x",scorestring, ignore.case=TRUE)[1]
    returnscore <- as.numeric(substr(scorestring, 1, firstx-1))
    returnscore <- returnscore + 0.1*(nchar(scorestring)-firstx+1)
    
    # If the string does not contain 'x'...
  } else {
    
    # Convert the string to a number.
    returnscore <- as.numeric(scorestring)
  }
  return(returnscore)
}

# FUNCTION: Take raw data and retrieve only those participants who
# competed for at least four years. Also, find the highest score
# for each participant for each year.
selectscore <- function(rawdata){
  
  # Paste years and names together into new variable
  rawdata$yn <- sapply(1:nrow(rawdata), 
                  function(x) paste(rawdata$year[x], rawdata$name[x]))
  yns <- unique(rawdata$yn)
  
  # Find maximum score for each year-name
  score <- sapply(yns, 
                  function(x) max(rawdata$score[which(rawdata$yn==x)]))
  
  # Find year and name associated with each maximum score.
  year <- sapply(1:length(yns), 
                 function(x) rawdata$year[which(rawdata$yn==yns[x] & 
                                          rawdata$score==score[x])[1]])
  name <- sapply(1:length(yns), 
                 function(x) rawdata$name[which(rawdata$yn==yns[x] & 
                                          rawdata$score==score[x])[1]])
  
  # Find number of competition years for each individual
  nameyears <- data.frame(table(name))
  # Select only individuals with 4+ compeition years
  fouryears <- nameyears$name[which(nameyears$Freq > 3)]
  
  # Combine max score, year, and name into data frame.
  fulldata <- data.frame(score, year, name)
  # Subset data frame to include only names with 4+ competition years.
  returndata <- fulldata[which(fulldata$name %in% fouryears),]
  
  return(returndata)
}


# Read csv files of bow and atlatl scores into data frames.
# Tables should contain the following columms with identical
# header names:
#
#   year    score   name
#   2010    55      Name1
#   2012    88      Name1
#   2009    45      Name2
#   2010    50      Name2
#   ....    ...     ...
#
# ENSURE THAT THE FILE NAMES MATCH THE "name.csv" STRING ARGUMENT IN THE
# FOLLOWING 2 LINES OF CODE. THEY MAY HAVE CHANGED IF THEY HAVE BEEN 
# DOWNLOADED AS SUPPLEMENTAL FILES.
bow1 <- data.frame(read.csv("bowdata.csv",header=TRUE),stringsAsFactors=FALSE)
atlatl1 <- data.frame(read.csv("atlatldata.csv",header=TRUE),stringsAsFactors=FALSE)

# Remove any trailing/leading whitespace in competitor names
bow1$name <- trimws(bow1$name)
atlatl1$name <- trimws(atlatl1$name)

# Convert strings of atlatl scores of the 00xx format into numeric values.
colnames(atlatl1)[which(names(atlatl1)=="score")] <- "scorestring"
atlatl1$scorestring <- trimws(atlatl1$scorestring)
atlatl1$score <- sapply(atlatl1$scorestring, getatlatlscore)

# Create new data frames with only those competitors that competed for 4+
# years and only include their maximum scores for each year.
bow <- selectscore(bow1)
atlatl <- selectscore(atlatl1)

# Convert calendar years to sequences of years based on each individual's
# first year of competition for archery.
fyearb <- sapply(1:nrow(bow), 
                 function(x) min(bow$year[which(bow$name==bow$name[x])]))
colnames(bow)[which(names(bow)=="year")] <- "calyear"
bow$year <- bow$calyear - (fyearb-1)

# Convert calendar years to sequences of years based on each individual's
# first year of competition for atlatls.
fyeara <- sapply(1:nrow(atlatl), 
                 function(x) min(atlatl$year[which(atlatl$name==atlatl$name[x])]))
colnames(atlatl)[which(names(atlatl)=="year")] <- "calyear"
atlatl$year <- atlatl$calyear - (fyeara-1)


# Create data frame for the frequency of bow scores for each year
bowdf <- as.data.frame(table(bow$year),stringsAsFactors=FALSE)
colnames(bowdf) <- c("year","n")
bowdf$year <- as.numeric(bowdf$year)
# Obtain median bow scores for each year
bowmedians <- sapply(bowdf$year, 
                     function(x) median(bow$score[which(bow$year==x)]))
# Obtain bow score stdevs for each year
bowsd <- sapply(bowdf$year, 
                function(x) sd(bow$score[which(bow$year==x)]))
# Obtain std errors for each year's median score
bowse <- 1.2533*bowsd/sqrt(bowdf$n)
# Create vector of ln(year)
bowlnyear <- log(bowdf$year)
# Create 2 se bounds for each median
bow2seu <- bowmedians + 2*bowse
bow2sel <- bowmedians - 2*bowse
# Add medians, median std errors, and ln(year) to bowdf
bowdf <- cbind(bowdf, bowmedians, bowse, bowlnyear,
               bow2seu, bow2sel)


# Create data frame for the frequency of atlatl scores for each year
atlatldf <- as.data.frame(table(atlatl$year),stringsAsFactors=FALSE)
colnames(atlatldf) <- c("year","n")
atlatldf$year <- as.numeric(atlatldf$year)
# Obtain median atlatl scores for each year
atlatlmedians <- sapply(atlatldf$year, 
                        function(x) median(atlatl$score[which(atlatl$year==x)]))
# Obtain atlatl score stdevs for each year
atlatlsd <- sapply(atlatldf$year, 
                   function(x) sd(atlatl$score[which(atlatl$year==x)]))
# Obtain std errors for each year's median score
atlatlse <- 1.2533*atlatlsd/sqrt(atlatldf$n)
# Create vector of ln(year)
atlatllnyear <- log(atlatldf$year)
# Create 2 se bounds for each median
atlatl2seu <- atlatlmedians + 2*atlatlse
atlatl2sel <- atlatlmedians - 2*atlatlse
# Add medians, median std errors, and ln(year) to atlatldf
atlatldf <- cbind(atlatldf, atlatlmedians, atlatlse, atlatllnyear,
                  atlatl2seu, atlatl2sel)

# Store dataframes in a list that is suited for model arguments.
# Variables are converted to standard scores to aid in model fitting.
blist <- list(med=(bowdf$bowmedians-mean(bowdf$bowmedians))/sd(bowdf$bowmedians), 
              lnyear=(bowdf$bowlnyear-mean(bowdf$bowlnyear))/sd(bowdf$bowlnyear),
              se=bowdf$bowse/sd(bowdf$bowmedians), 
              year=(bowdf$year-mean(bowdf$year))/sd(bowdf$year))
atlist <- list(med=(atlatldf$atlatlmedians-mean(atlatldf$atlatlmedians))/
                 sd(atlatldf$atlatlmedians), 
               lnyear=(atlatldf$atlatllnyear-mean(atlatldf$atlatllnyear))/
                 sd(atlatldf$atlatllnyear),
               se=atlatldf$atlatlse/sd(atlatldf$atlatlmedians), 
               year=(atlatldf$year-mean(atlatldf$year))/sd(atlatldf$year))


##################################################################
################ Part 2: Fit median-score models #################
##################################################################

# Below are six models that predict median scores as a function of year. Three
# models are for bow scores and three models are for atlatl scores. Mildly inform-
# ative priors are used.
# 
# For each weapon, the first model takes the simple form:
#
# medianscore = a + Y*year
#
# Following Grund 2017, the second model for each weapon takes the form:
#
# medianscore = a + Y*ln(year)
#
# Finally, following Grund 2017, each weapon is modelled with a generalized 
# logistic growth function taking the form:
#
# medianscore = A + K/(1+Q*e^[-B(year-M)])
#
# Unlike Grund 2017, these models include the uncertainty associated with each
# median. Inclusion of std error terms for medians follows McElreath 2015:424-431.

# MODEL: medianbowscore = a + Y*year
bowline <- map2stan(alist(
  medest ~ dnorm(mu, sigma),
  mu <- a + Y*year,
  med ~ dnorm(medest,se),
  Y ~ dnorm(0,1),
  a ~ dnorm(0,1),
  sigma ~ dgamma2(1,0.2)),
  data=blist, start=list(medest=blist$med), 
  WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2,
  control=list(max_treedepth=20, adapt_delta=0.999))

# MODEL: medianbowscore = a + Y*log(year)
bowln <- map2stan(alist(
  medest ~ dnorm(mu, sigma),
  mu <- a + Y*lnyear,
  med ~ dnorm(medest,se),
  Y ~ dnorm(0,1),
  a ~ dnorm(0,1),
  sigma ~ dgamma2(1,0.2)),
  data=blist, start=list(medest=blist$med), 
  WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2,
  control=list(max_treedepth=20, stepsize=0.1, adapt_delta=0.999))

# MODEL: medianatlatlscore = a + Y*year
atlatlline <- map2stan(alist(
    medest ~ dnorm(mu, sigma),
    mu <- a + Y*year,
    med ~ dnorm(medest,se),
    Y ~ dnorm(0,1),
    a ~ dnorm(0,1),
    sigma ~ dgamma2(0.8,0.2)),
    data=atlist, start=list(medest=atlist$med), 
    WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2, 
    control=list(max_treedepth=20, adapt_delta=0.99))

# MODEL: medianatlatlscore = a + Y*log(year)
atlatlln <- map2stan(alist(
    medest ~ dnorm(mu, sigma),
    mu <- a + Y*lnyear,
    med ~ dnorm(medest,se),
    Y ~ dnorm(0,1),
    a ~ dnorm(0,1),
    sigma ~ dgamma2(1,0.2)),
    data=atlist, start=list(medest=atlist$med), 
    WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2, 
    control=list(max_treedepth=20, adapt_delta=0.99))

# MODEL: medianbowscore = A + (K/1+Q*2.71828^(B*(year+Mt)))
bowlogistic <- map2stan(alist(
  medest ~ dnorm(mu, sigma),
  mu <- A + K/(1+Q*2.71828^(-B*(year-Mt))),
  med ~ dnorm(medest,se),
  B ~ dexp(1.5),
  sigma ~ dgamma2(1,0.2),
  Q ~ dexp(1.5),
  Mt ~ dnorm(0,0.5),
  A ~ dnorm(-2,2),
  K ~ dnorm(2,2)),
  data=blist, start=list(medest=blist$med), 
  WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2,  
  control=list(stepsize=0.0001, adapt_delta=0.9999, 
               max_treedepth=20),  
  constraints=list(B="lower=0", Q="lower=0"))

# MODEL: medianatlatlscore = A + (K/1+Q*2.71828^(B*(year+Mt)))
atlatllogistic <- map2stan(alist(
  medest ~ dnorm(mu, sigma),
  mu <- A + K/(1+Q*2.71828^(-B*(year-Mt))),
  med ~ dnorm(medest,se),
  B ~ dexp(1.5),
  sigma ~ dgamma2(1,0.2),
  Q ~ dexp(1.5),
  Mt ~ dnorm(0,0.5),
  A ~ dnorm(-2,2),
  K ~ dnorm(2,2)),
  data=atlist, start=list(medest=atlist$med), 
  WAIC=FALSE, chains=4, iter=4e3, warmup=2e3, cores=2, 
  control=list(adapt_delta=0.99, max_treedepth=20),  
  constraints=list(B="lower=0", Q="lower=0"))

# print model summaries
precis(bowline, depth=2, prob=0.95)
precis(bowln, depth=2, prob=0.95)
precis(atlatlline, depth=2, prob=0.95)
precis(atlatlln, depth=2, prob=0.95)
precis(bowlogistic, depth=2, prob=0.95)
precis(atlatllogistic, depth=2, prob=0.95)

# Bow and atlatl model comparisons
compare(bowline, bowln, bowlogistic, func=DIC)
compare(atlatlline, atlatlln, atlatllogistic, func=DIC)


##################################################################
########################## Part 3: Plots #########################
##################################################################

# Set plotting frame for 3x2 panels
par(mfrow=c(3,2))

# Create sequence of years over which to simulate data for bows and atlatls
byearseq <- seq(from=(min(bowdf$year)-mean(bowdf$year))/sd(bowdf$year), 
               to=(max(bowdf$year)-mean(bowdf$year))/sd(bowdf$year), 
               length.out=200)
bpredscore <- list(year=byearseq)

ayearseq <- seq(from=(min(bowdf$year)-mean(atlatldf$year))/sd(atlatldf$year), 
                to=(max(bowdf$year)-mean(atlatldf$year))/sd(atlatldf$year), 
                length.out=200)
apredscore <- list(year=ayearseq)

# Create sequence of log years over which to simulate data for bows and atlatls
blnyearseq <- seq(from=(min(bowdf$bowlnyear)-mean(bowdf$bowlnyear))/
                    sd(bowdf$bowlnyear), 
                to=(max(bowdf$bowlnyear)-mean(bowdf$bowlnyear))/
                  sd(bowdf$bowlnyear), 
                length.out=200)
blnpredscore <- list(lnyear=blnyearseq)

alnyearseq <- seq(from=(min(bowdf$bowlnyear)-mean(atlatldf$atlatllnyear))/
                    sd(atlatldf$atlatllnyear), 
                to=(max(bowdf$bowlnyear)-mean(atlatldf$atlatllnyear))/
                  sd(atlatldf$atlatllnyear), 
                length.out=200)
alnpredscore <- list(lnyear=alnyearseq)

# The following blocks of code first generate 95% mu cred
# intervals for each model. They plot those intervals against
# the observed median scores, median score standard errors,
# and posterior median estimates.

# Simple linear bow plot.
mu1 <- link(bowline, data=bpredscore)
mu.mean1 <- apply(mu1, 2, mean)
mu.mean1 <- (mu.mean1*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
mu.PI1 <- apply(mu1, 2, PI, prob=0.95)
mu.PI1 <- (mu.PI1*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
plot(bowmedians ~ year, bowdf, ylim=c(100,250), 
     col=col.alpha("grey",0), xlim=c(1,max(bow$year)), 
     xlab="Competition year", ylab="Median bow score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
shade(mu.PI1, (byearseq*sd(bowdf$year))+mean(bowdf$year), 
      col=col.alpha("black",0.2))
lines((byearseq*sd(bowdf$year))+mean(bowdf$year), mu.mean1, 
      col="black")
segments(x0=bowdf$year, y0=bowdf$bow2seu, x1=bowdf$year, 
         y1=bowdf$bow2sel, col=col.alpha("black",0.75), 
         lwd=rep(2.5,nrow(bowdf)))
points(bowdf$year, bowdf$bowmedians, col=col.alpha("black",0.75), 
       bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
blinepostM <- (precis(bowline, depth=2,prob=0.95)@output$Mean[1:nrow(bowdf)]*
                sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blinepostU <- (precis(bowline, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blinepostL <- (precis(bowline, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
segments(x0=bowdf$year, y0=blinepostL, x1=bowdf$year, 
         y1=blinepostU, lwd=rep(1.5,nrow(bowdf)), col="white")
points(bowdf$year, blinepostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(bowdf)))
text(2, 235, labels="a", cex=1.6)


# Simple linear atlatl plot.
mu4 <- link(atlatlline, data=apredscore)
mu.mean4 <- apply(mu4, 2, mean)
mu.mean4 <- (mu.mean4*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
mu.PI4 <- apply(mu4, 2, PI, prob=0.95)
mu.PI4 <- (mu.PI4*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
plot(atlatlmedians ~ year, atlatldf, ylim=c(40,100), 
     col=col.alpha("grey",0), xlim=c(1,max(bow$year)), 
     xlab="Competition year", ylab="Median atlatl score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
shade(mu.PI4, (ayearseq*sd(atlatldf$year))+mean(atlatldf$year), 
      col=col.alpha("black",0.2))
lines((ayearseq*sd(atlatldf$year))+mean(atlatldf$year), mu.mean4, 
      col="black")
segments(x0=atlatldf$year, y0=atlatldf$atlatl2seu, x1=atlatldf$year, 
         y1=atlatldf$atlatl2sel, col=col.alpha("black",0.75),
         lwd=rep(2.5,nrow(atlatldf)), points(atlatldf$year), 
         atlatldf$atlatlmedians, bg="black", pch=19, cex=1)
points(atlatldf$year, atlatldf$atlatlmedians, col=col.alpha("black",0.75), 
       bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
alinepostM <- (precis(atlatlline, depth=2,prob=0.95)@output$Mean[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alinepostU <- (precis(atlatlline, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alinepostL <- (precis(atlatlline, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
segments(x0=atlatldf$year, y0=alinepostL, x1=atlatldf$year, 
         y1=alinepostU, lwd=rep(1.5,nrow(atlatldf)), col="white")
points(atlatldf$year, alinepostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(bowdf)))
text(2, 94, labels="b", cex=1.6)


# Log-linear bow plot.
mu2 <- link(bowln, data=blnpredscore)
mu.mean2 <- apply(mu2, 2, mean)
mu.mean2 <- (mu.mean2*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
mu.PI2 <- apply(mu2, 2, PI, prob=0.95)
mu.PI2 <- (mu.PI2*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
plot(bowmedians ~ bowlnyear, bowdf, ylim=c(100,250), 
     col=col.alpha("grey",0), xlim=c(0,max(bowdf$bowlnyear)), 
     xlab="Log competition year", ylab="Median bow score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
shade(mu.PI2, (blnyearseq*sd(bowdf$bowlnyear))+mean(bowdf$bowlnyear), 
      col=col.alpha("black",0.2))
lines((blnyearseq*sd(bowdf$bowlnyear))+mean(bowdf$bowlnyear), mu.mean2, 
      col="black")
segments(x0=bowdf$bowlnyear, y0=bowdf$bow2seu, x1=bowdf$bowlnyear, 
         y1=bowdf$bow2sel, col=col.alpha("black",0.75), 
         lwd=rep(2.5,nrow(bowdf)))
points(bowdf$bowlnyear, bowdf$bowmedians, col=col.alpha("black",0.75), 
       bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
blnpostM <- (precis(bowln, depth=2,prob=0.95)@output$Mean[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blnpostU <- (precis(bowln, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blnpostL <- (precis(bowln, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
segments(x0=bowdf$bowlnyear, y0=blnpostL, x1=bowdf$bowlnyear, 
         y1=blnpostU, lwd=rep(1.5,nrow(bowdf)), col="white")
points(bowdf$bowlnyear, blnpostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(bowdf)))
text(0.2466, 235, labels="c", cex=1.6)

# Log-linear atlatl plot.
mu5 <- link(atlatlln, data=alnpredscore)
mu.mean5 <- apply(mu5, 2, mean)
mu.mean5 <- (mu.mean5*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
mu.PI5 <- apply(mu5, 2, PI, prob=0.95)
mu.PI5 <- (mu.PI5*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
plot(atlatlmedians ~ atlatllnyear, atlatldf, ylim=c(40,100), 
     col=col.alpha("grey",0), xlim=c(0,max(bowdf$bowlnyear)), 
     xlab="Log competition year", ylab="Median atlatl score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
curve(9.5*x + 47.9, add=TRUE, col="white", lwd=3.5) # Grund's model
curve(9.5*x + 47.9, add=TRUE, col="grey", lwd=2) # Grund's model
curve(9.5*x + 47.9, add=TRUE, col="white", lty=3, lwd=2) # Grund's model
shade(mu.PI5, (alnyearseq*sd(atlatldf$atlatllnyear))+mean(atlatldf$atlatllnyear), 
      col=col.alpha("black",0.2))
lines((alnyearseq*sd(atlatldf$atlatllnyear))+mean(atlatldf$atlatllnyear), mu.mean5, 
      col="black")
segments(x0=atlatldf$atlatllnyear, y0=atlatldf$atlatl2seu, x1=atlatldf$atlatllnyear, 
         y1=atlatldf$atlatl2sel, col=col.alpha("black",0.75), 
         lwd=rep(2.5,nrow(atlatldf)))
points(atlatldf$atlatllnyear, atlatldf$atlatlmedians, col=col.alpha("black",0.75), 
       bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
alnpostM <- (precis(atlatlln, depth=2,prob=0.95)@output$Mean[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alnpostU <- (precis(atlatlln, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alnpostL <- (precis(atlatlln, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(atlatldf)]*
                 sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
segments(x0=atlatldf$atlatllnyear, y0=alnpostL, x1=atlatldf$atlatllnyear, 
         y1=alnpostU, lwd=rep(1.5,nrow(atlatldf)), col="white")
points(atlatldf$atlatllnyear, alnpostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(atlatldf)))
text(0.2466, 94, labels="d", cex=1.6)

# Logistic bow plot.
mu3 <- link(bowlogistic, data=bpredscore)
mu.mean3 <- apply(mu3, 2, mean)
mu.mean3 <- (mu.mean3*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
mu.PI3 <- apply(mu3, 2, PI, prob=0.95)
mu.PI3 <- (mu.PI3*sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
plot(bowmedians ~ year, bowdf, ylim=c(100,250), 
     col=col.alpha("grey",0), xlim=c(1,max(bow$year)), 
     xlab="Competition year", ylab="Median bow score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
curve(166.5+(59.2/(1+0.022*(2.718^(-12.4*(x-5.36))))), add=TRUE,
      col="white", lwd=3.5) # Grund's model
curve(166.5+(59.2/(1+0.022*(2.718^(-12.4*(x-5.36))))), add=TRUE,
      lwd=2, col="grey") # Grund's model
curve(166.5+(59.2/(1+0.022*(2.718^(-12.4*(x-5.36))))), add=TRUE, 
      col="white", lty=3, lwd=2) # Grund's model
shade(mu.PI3, (byearseq*sd(bowdf$year))+mean(bowdf$year), 
      col=col.alpha("black",0.2))
lines((byearseq*sd(bowdf$year))+mean(bowdf$year), mu.mean3, 
      col="black")
segments(x0=bowdf$year, y0=bowdf$bow2seu, x1=bowdf$year, 
         y1=bowdf$bow2sel, col=col.alpha("black",0.75), 
         lwd=rep(2.5,nrow(bowdf)))
points(bowdf$year, bowdf$bowmedians, col=col.alpha("black",0.75), 
       bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
blogisticpostM <- (precis(bowlogistic, depth=2,prob=0.95)@output$Mean[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blogisticpostU <- (precis(bowlogistic, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
blogisticpostL <- (precis(bowlogistic, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(bowdf)]*
                 sd(bowdf$bowmedians))+mean(bowdf$bowmedians)
segments(x0=bowdf$year, y0=blogisticpostL, x1=bowdf$year, 
         y1=blogisticpostU, lwd=rep(1.5,nrow(bowdf)), col="white")
points(bowdf$year, blogisticpostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(bowdf)))
text(2, 235, labels="e", cex=1.6)


# Logistic atlatl plot.
mu6 <- link(atlatllogistic, data=apredscore)
mu.mean6 <- apply(mu6, 2, mean)
mu.mean6 <- (mu.mean6*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
mu.PI6 <- apply(mu6, 2, PI, prob=0.95)
mu.PI6 <- (mu.PI6*sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
plot(atlatlmedians ~ year, atlatldf, ylim=c(40,100), 
     col=col.alpha("blue",0), xlim=c(1,max(bow$year)), 
     xlab="Competition year", ylab="Median atlatl score",
     cex.lab=1.4, cex.axis=1.2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col=col.alpha("gray",0.2))
shade(mu.PI6, (ayearseq*sd(atlatldf$year))+mean(atlatldf$year), 
      col=col.alpha("black",0.2))
lines((ayearseq*sd(atlatldf$year))+mean(atlatldf$year), mu.mean6, 
      col="black")
segments(x0=atlatldf$year, y0=atlatldf$atlatl2seu, x1=atlatldf$year, 
         y1=atlatldf$atlatl2sel, col=col.alpha("black",0.75),
         lwd=rep(2.5,nrow(atlatldf)))
points(atlatldf$year, atlatldf$atlatlmedians, 
       col=col.alpha("black",0.75), bg="black", pch=19, cex=1.2)
# Obtain and plot posterior medians.
alogisticpostM <- (precis(atlatllogistic, depth=2,prob=0.95)@output$Mean[1:nrow(atlatldf)]*
                    sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alogisticpostU <- (precis(atlatllogistic, depth=2,prob=0.95)@output$'upper 0.95'[1:nrow(atlatldf)]*
                     sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
alogisticpostL <- (precis(atlatllogistic, depth=2,prob=0.95)@output$'lower 0.95'[1:nrow(atlatldf)]*
                     sd(atlatldf$atlatlmedians))+mean(atlatldf$atlatlmedians)
segments(x0=atlatldf$year, y0=alogisticpostL, x1=atlatldf$year, 
         y1=alogisticpostU, lwd=rep(1.5,nrow(atlatldf)), col="white")
points(atlatldf$year, alogisticpostM, pch=16, cex=1, col="white",
       lwd=rep(1.5,nrow(bowdf)))
text(2, 94, labels="f", cex=1.6)

############################################################
####### REDRAWN MEDIAN SCORE PLOTS FROM GRUND 2017 #########
############################################################

# Data frames of estimated medians
bowrd <- data.frame(year=seq(from=1, to=7), 
                    median=c(165,169,170,164,186,232,218))
atlrd <- data.frame(year=seq(from=1, to=8), 
                    median=c(47.5,56,59,58,60.5,65.5,68.5,68))

# Set plotting frame to 2x1 panels.
par(mfrow=c(1,2))

plot(x=bowrd$year, y=bowrd$median, pch=16,
     ylab="Median bow score", xlab="Competition year", 
     col="black", xlim=c(1,8), ylim=c(144,252), 
     cex.lab=1.4, cex.axis=1.2, yaxt="n")
axis(2, at=c(144, 162, 180, 198, 216, 234, 252))
points(x=bowrd$year, y=bowrd$median, pch=16)
lines(x=bowrd$year, y=bowrd$median, lwd=2)
curve(166.5+(59.2/(1+0.022*(2.718^(-12.4*(x-5.36))))), add=TRUE,
      col=col.alpha("grey",1), lwd=2) # Grund's bow model
text(1.4, 241.2, labels="a", cex=1.6)

plot(x=atlrd$year, y=atlrd$median, pch=16,
     ylab="Median atlatl score", xlab="Competition year", 
     col="black", xlim=c(1,8), ylim=c(40,70), 
     cex.lab=1.4, cex.axis=1.2, yaxt="n")
axis(2, at=c(40, 45, 50, 55, 60, 65, 70))
points(x=atlrd$year, y=atlrd$median, pch=16)
lines(x=atlrd$year, y=atlrd$median, lwd=2)
curve(9.5*log(x) + 47.9, add=TRUE, col=col.alpha("grey",1), 
      lwd=2) # Grund's atlatl model
text(1.4, 67, labels="b", cex=1.6)