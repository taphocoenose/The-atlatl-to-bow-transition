# last edited November 25, 2017 by Ryan Breslawski

# DESCRIPTION

# This script reads two csv files into data frames: bowdata.csv 
# and atlatldata.csv. The script converts these data frames into
# datasets containing only competitors who competed for 4+ years.
# Additionally, for each competitor, only the highest score for 
# each year is retained.

# The script then fits five multilevel models to these converted
# datasets using RStan. Finally, the fitted models are presented
# in two plots.

# This script has three main parts:
#   Part 1: Read, clean, and sort data. Lines 20-170.
#   Part 2: Fit multilevel models. Lines 173-251.
#   Part 3: Plot multilelevel models. Lines 253-676.

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
    # obtained number from the character conversion.
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
# DOWNLOADED AS SUPPLEMENTAL ONLINE FILES.
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

# Estimate score limit for bow data.
bm <- max(bow$score)/(max(atlatl$score/100))

# Standardize both sets of scores to [0,1], assign to new column.
bow$scorest <- bow$score/bm
atlatl$scorest <- atlatl$score/100

# Transform data in accordance with Smithson & Verkuilen 2006
# to create an open interval (0,1) form of scores for each weapon.
# Also, create vectors of unique names within each dataset.
bow$scoretr <- ((bow$score/bm)*(nrow(bow)-1)+0.5)/nrow(bow)
pbnamest <- unique(bow$name)
atlatl$scoretr <- ((atlatl$score/100)*(nrow(atlatl)-1)+0.5)/nrow(atlatl)
panamest <- unique(atlatl$name)

# Create alternative datasets to remove names with scores of 1 or 0
# in the closed [0,1] datasets (if such values occur).
if(any(bow$scorest == 1) | any(bow$scorest == 0)){
  nobnames <- unique(bow$name[which((bow$scorest == 0)|(bow$scorest == 1))])
  bowr <- bow[-which(bow$name %in% nobnames),]
  } else {
  bowr <- bow
}
if(any(atlatl$scorest == 1) | any(atlatl$scorest == 0)){
  noanames <- unique(atlatl$name[which((atlatl$scorest == 0)|(atlatl$scorest == 1))])
  atlatlr <- atlatl[-which(atlatl$name %in% noanames),]
  } else {
  atlatlr <- atlatl
}

# make vectors of unique names in bow and atlatl data lacking 1s and 0s.
pbnames <- unique(bowr$name)
panames <- unique(atlatlr$name)


##################################################################
################# Part 2: Fit multilevel models ##################
##################################################################

# Create lists of bow and atlatl data for MLMs
bowl <- list(score=bow$scoretr, year=bow$year, name1=bow$name)
bowrl <- list(score=bowr$scorest, year=bowr$year, name1=bowr$name, scor2=bowr$score)
atlatll <- list(score=atlatl$scoretr, year=atlatl$year, name1=atlatl$name)
atlatlrl <- list(score=atlatlr$scorest, year=atlatlr$year, name1=atlatlr$name)

# MLM for the transformed scaled bow data.
bowtMLM <- map2stan(alist(
  score ~ dbeta2(pbar, theta),
  logit(pbar) <- a_name[name1] + b_name[name1]*year,
  c(a_name, b_name)[name1] ~ dmvnorm2(c(a,bY),sigma_name,rho),
  a ~ dnorm(0,10),
  bY ~ dnorm(0,10),
  sigma_name ~ dcauchy(0,2),
  theta ~ dexp(1),
  rho ~ dlkjcorr(2)),
  data=bowl, iter=4000, warmup=200, chains=4, cores=2, 
  constraints=list(theta="lower=0"))

# MLM for scaled bow data with removed 0 and 1 scores.
bowrMLM <- map2stan(alist(
  score ~ dbeta2(pbar, theta),
  logit(pbar) <- a_name[name1] + b_name[name1]*year,
  c(a_name, b_name)[name1] ~ dmvnorm2(c(a,b),sigma_name,rho),
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sigma_name ~ dcauchy(0,2),
  theta ~ dexp(1),
  rho ~ dlkjcorr(2)),
  data=bowrl, iter=4000, warmup=2000, chains=4, cores=2, 
  constraints=list(theta="lower=0"))

# MLM for unscaled bow data with gamma likelihood.
bowgMLM <- map2stan(alist(
  scor2 ~ dgamma2(mu, theta),
  log(mu) <- a_name[name1] + b_name[name1]*year,
  c(a_name, b_name)[name1] ~ dmvnorm2(c(a,b),sigma_name,rho),
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sigma_name ~ dcauchy(0,2),
  theta ~ dexp(1),
  rho ~ dlkjcorr(2)),
  data=bowrl, iter=4000, warmup=2000, chains=4, cores=2, 
  constraints=list(phi="lower=0"))

# MLM for the transformed scaled atlatl data.
atlatltMLM <- map2stan(alist(
  score ~ dbeta2(pbar, theta),
  logit(pbar) <- a_name[name1] + b_name[name1]*year,
  c(a_name, b_name)[name1] ~ dmvnorm2(c(a,b),sigma_name,rho),
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sigma_name ~ dcauchy(0,2),
  theta ~ dexp(1),
  rho ~ dlkjcorr(2)),
  data=atlatll, iter=4000, warmup=2000, chains=4, cores=2, 
  constraints=list(theta="lower=0"))

# MLM for scaled atlatl data with removed 0 and 1 scores.
atlatlrMLM <- map2stan(alist(
  score ~ dbeta2(pbar, theta),
  logit(pbar) <- a_name[name1] + b_name[name1]*year,
  c(a_name, b_name)[name1] ~ dmvnorm2(c(a,b),sigma_name,rho),
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sigma_name ~ dcauchy(0,2),
  theta ~ dexp(1),
  rho ~ dlkjcorr(2)),
  data=atlatlrl, iter=4000, warmup=2000, chains=4, cores=2, 
  constraints=list(theta="lower=0"))

# Summarize MLMs
precis(bowtMLM, prob=0.95, depth=2)
precis(bowrMLM, prob=0.95, depth=2)
precis(bowgMLM, prob=0.95, depth=2)
precis(atlatltMLM, prob=0.95, depth=2)
precis(atlatlrMLM, prob=0.95, depth=2)

##################################################################
################# Part 3: Plot multilevel models #################
##################################################################

# 7 plots:
## 1. atlatl data -0s, -1s
## 2. atlatl data transformed
## 3. bow data -0s, -1s
## 4. bow data transformed
## 5. Slopes plot for -0s and -1s data
## 6. slopes plot for transformed data
## 7. bow data not transformed
## 8. atlatl mean scores X competition years scatterplot
## 9. bow mean scores X competition years scatterplot
#########################################################
## Plots 1-4 are combined within a four panel figure.
## Plots 5-6 are combined in a two panel figure. 
## Plots 8-9 are combined within a two panel figure.
#########################################################
#########################################################

# Set plotting frame for 2x2 panels
par(mfrow=c(2,2))

#############################
### ATLATL PLOT: -0s, -1s ###
#############################

# Extract beta coefs
atlatlB <- coef(atlatlrMLM)[1:length(panames)]

# Extract intercept coefficients
atlatlA <- coef(atlatlrMLM)[(length(panames)+1):(2*length(panames))]

# Find sample size by competitor
acompetitors <- as.data.frame(table(atlatlr$name),
                              stringsAsFactors=FALSE)
colnames(acompetitors) <- c("atlatlnames","n")
if(any(acompetitors$n==0)){
  acompetitors <- acompetitors[-which(acompetitors$n==0),]}
# Find last competition year for each competitor
acompetitors$last <- sapply(acompetitors$atlatlnames,
                            function(x) max(atlatlr$year[which(atlatlr$name==x)]))

# Pooled a
PaAr <- coef(atlatlrMLM)[2*length(panames)+1]
# Pooled B
PbAr <- coef(atlatlrMLM)[2*length(panames)+2]

# Set empty plot frame, scale y-axis from 1 back to 100
plot(1, type="n", xlab="Competition year", ylab="Atlatl score", 
     xlim=c(0,max(atlatlr$year)), ylim=c(0,1), yaxt='n',
     cex.lab=1.4, cex.axis=1.2)
axis(2, at=seq(from=0, to=1, length.out=6), 
     labels=seq(from=0,to=100, by=20))
# Plot individual models
for(i in 1:length(panames)){
  if(atlatlB[i]>0){Bcolor <- "blue"} else {Bcolor <- "red"}
  curve(exp((atlatlA[i])+(x*atlatlB[i]))/(1+exp((atlatlA[i])+(x*atlatlB[i]))), 
        from=0, to=acompetitors$last[i],
        col=alpha(Bcolor,0.12), add=TRUE)
}
# Plot curve for pooled model
curve(exp((PaAr)+(x*PbAr))/(1+exp((PaAr)+(x*PbAr))), 
      from=0, to=max(atlatlr$year),
      col=alpha("purple", 0.8), lwd=4, add=TRUE)
text(11*15/16, 1/16, labels="a", cex=1.6)

################################
### ATLATL PLOT: transformed ###
################################

# Extract beta coefs
atlatlBt <- coef(atlatltMLM)[1:length(panamest)]

# Extract intercept coefficients
atlatlAt <- coef(atlatltMLM)[(length(panamest)+1):(2*length(panamest))]

# Find sample size by competitor
acompetitorst <- as.data.frame(table(as.character(atlatl$name)),
                               stringsAsFactors=FALSE)
colnames(acompetitorst) <- c("atlatlnames","n")
# Find last competition year for each competitor
acompetitorst$last <- sapply(acompetitorst$atlatlnames,
                             function(x) max(atlatl$year[which(atlatl$name==x)]))

# Pooled a
PaAt <- coef(atlatltMLM)[2*length(panamest)+1]
# Pooled B
PbAt <- coef(atlatltMLM)[2*length(panamest)+2]

# Set empty plot frame, scale y-axis from 1 back to 100
plot(1, type="n", xlab="Competition year", 
     ylab="Atlatl score (transformed)", 
     xlim=c(0,max(atlatl$year)), ylim=c(0,1), yaxt='n',
     cex.lab=1.4, cex.axis=1.2)
axis(2, at=seq(from=0, to=1, length.out=6), 
     labels=seq(from=0,to=100, by=20))
# Plot individual models
for(i in 1:length(panamest)){
  if(atlatlBt[i]>0){Bcolor <- "blue"} else {Bcolor <- "red"}
  curve(exp((atlatlAt[i])+(x*atlatlBt[i]))/(1+exp((atlatlAt[i])+(x*atlatlBt[i]))), 
        from=0, to=acompetitorst$last[i],
        col=alpha(Bcolor,0.12), add=TRUE)
}
# Plot curve for pooled model
curve(exp((PaAt)+(x*PbAt))/(1+exp((PaAt)+(x*PbAt))), 
      from=0, to=max(atlatl$year),
      col=alpha("purple", 0.8), lwd=4, add=TRUE)
text(11*15/16, 1/16, labels="b", cex=1.6)



##########################
### BOW PLOT: -0s, -1s ###
##########################

# Extract beta coefs
bowB <- coef(bowrMLM)[1:length(pbnames)]

# Extract intercept coefficients
bowA <- coef(bowrMLM)[(length(pbnames)+1):(2*length(pbnames))]

# Find sample size by competitor
bowcompetitors <- as.data.frame(table(bowr$name),
                                stringsAsFactors=FALSE)
colnames(bowcompetitors) <- c("bownames","n")
if(any(bowcompetitors$n==0)){
  bowcompetitors <- bowcompetitors[-which(bowcompetitors$n==0),]}
# Find last competition year for each competitor
bowcompetitors$last <- sapply(bowcompetitors$bownames,
                      function(x) max(bowr$year[which(bowr$name==x)]))

# Pooled a
Pa <- coef(bowrMLM)[2*length(pbnames)+1]
# Pooled B
Pb <- coef(bowrMLM)[2*length(pbnames)+2]

# Set empty plot frame, scale y-axis from 1 back to 360
plot(1, type="n", xlab="Competition year", ylab="Bow score", 
     xlim=c(0,max(bowr$year)), ylim=c(0,1), yaxt='n',
     cex.lab=1.4, cex.axis=1.2)
axis(2, at=seq(from=0, to=1, length.out=7), 
     labels=seq(from=0,to=360, by=60))
# Plot individual models
for(i in 1:length(pbnames)){
  if(bowB[i]>0){Bcolor <- "blue"} else {Bcolor <- "red"}
  curve(exp((bowA[i])+(x*bowB[i]))/(1+exp((bowA[i])+(x*bowB[i]))), 
        from=0, to=bowcompetitors$last[i],
        col=alpha(Bcolor,0.16), add=TRUE)
}
# Plot curve for pooled model
curve(exp((Pa)+(x*Pb))/(1+exp((Pa)+(x*Pb))), 
      from=0, to=max(bowr$year),
      col=alpha("purple", 0.8), lwd=4, add=TRUE)
text(15, 1/16, labels="c", cex=1.6)


#############################
### BOW PLOT: transformed ###
#############################

# Extract beta coefs
bowBt <- coef(bowtMLM)[1:length(pbnamest)]

# Extract intercept coefficients
bowAt <- coef(bowtMLM)[(length(pbnamest)+1):(2*length(pbnamest))]

# Find sample size by competitor
bowcompetitorst <- as.data.frame(table(as.character(bow$name)),
                                 stringsAsFactors=FALSE)
colnames(bowcompetitorst) <- c("bownames","n")
# Find last competition year for each competitor
bowcompetitorst$last <- sapply(bowcompetitorst$bownames,
                               function(x) max(bow$year[which(bow$name==x)]))

# Pooled a
Pat <- coef(bowtMLM)[2*length(pbnamest)+1]
# Pooled B
Pbt <- coef(bowtMLM)[2*length(pbnamest)+2]

# Set empty plot frame, scale y-axis from 1 back to 360
plot(1, type="n", xlab="Competition year", 
     ylab="Bow score (transformed)", 
     xlim=c(0,max(bow$year)), ylim=c(0,1), yaxt='n',
     cex.lab=1.4, cex.axis=1.2)
axis(2, at=seq(from=0, to=1, length.out=7), 
     labels=seq(from=0,to=360, by=60))
# Plot individual models
for(i in 1:length(pbnamest)){
  if(bowBt[i]>0){Bcolor <- "blue"} else {Bcolor <- "red"}
  curve(exp((bowAt[i])+(x*bowBt[i]))/(1+exp((bowAt[i])+(x*bowBt[i]))), 
        from=0, to=bowcompetitorst$last[i],
        col=alpha(Bcolor,0.16), add=TRUE)
}
# Plot curve for pooled model
curve(exp((Pat)+(x*Pbt))/(1+exp((Pat)+(x*Pbt))), 
      from=0, to=max(bow$year),
      col=alpha("purple", 0.8), lwd=4, add=TRUE)
text(15, 1/16, labels="d", cex=1.6)


################################
############ SLOPES PLOTS ######
################################

# Reset plotting frame to 1x1 panel
par(mfrow=c(1,2))

#####################
### PLOT SLOPES 1 ###
#####################

# Create density objects for individual slopes for each weapon
bowBs <- coef(bowrMLM)[1:length(pbnames)]
bowdens <- density(bowBs)
atlBs <- coef(atlatlrMLM)[1:length(panames)]
atldens <- density(atlBs)

# Obtain 95% cred intervals for pooled slopes
bowrLO <- precis(bowrMLM,prob=0.95)@output$'lower 0.95'[2]
bowrUP <- precis(bowrMLM,prob=0.95)@output$'upper 0.95'[2]
atlrLO <- precis(atlatlrMLM,prob=0.95)@output$'lower 0.95'[2]
atlrUP <- precis(atlatlrMLM,prob=0.95)@output$'upper 0.95'[2]

# Initialize plotting frame
plot(bowdens, xlab=expression(beta), 
     ylab="Density", xlim=c(-0.2,0.6), ylim=c(0,10), 
     col=col.alpha("blue",0), main=NA, 
     cex.lab=1.4, cex.axis=1.2)
# Plot density of individuals' slopes
polygon(bowdens, col=col.alpha("black",0.7), border=NA)
polygon(atldens, col=col.alpha("grey",0.8), border=NA)
# Plot credible intervals for pooled slopes
rect(xleft=bowrLO, xright=bowrUP, ybottom=0, ytop=10,
     col=col.alpha("grey",0), border="grey")
rect(xleft=atlrLO, xright=atlrUP, ybottom=0, ytop=10,
     col=col.alpha("black",0), border="black")
# Plot pooled means for slopes
segments(x0=Pb, x1=Pb, y0=0, y1=10, col="grey", lwd=2, lty=5)
segments(x0=PbAr, x1=PbAr, y0=0, y1=10, col="black", lwd=2, lty=5)
text(0.345, 9, labels="Bow", pos=4, cex=1.2)
text(0.345, 8.2, labels="Atlatl", pos=4, cex=1.2)
rect(xleft=0.22, xright=0.35, ybottom=8.8, ytop=9.2,
     col=col.alpha("black",0.7), border=NA)
rect(xleft=0.22, xright=0.35, ybottom=8, ytop=8.4,
     col=col.alpha("grey",0.8), border=NA)
segments(x0=0.285, x1=0.285, y0=8.8, y1=9.2, col="grey", lwd=2, lty=5)
segments(x0=0.285, x1=0.285, y0=8, y1=8.4, col="black", lwd=2, lty=5)
rect(xleft=0.27, xright=0.30, ybottom=8.8, ytop=9.2,
     col=col.alpha("grey",0), border="grey")
rect(xleft=0.27, xright=0.30, ybottom=8, ytop=8.4,
     col=col.alpha("black",0), border="black")
text(-0.17, 9.6, labels="a", cex=1.6)


#####################
### PLOT SLOPES 2 ###
#####################

# Create density objects for individual slopes for each weapon
bowBs2 <- coef(bowtMLM)[1:length(pbnamest)]
bowdens2 <- density(bowBs2)
atlBs2 <- coef(atlatltMLM)[1:length(panamest)]
atldens2 <- density(atlBs2)

# Obtain 95% cred intervals for pooled slopes
bowrLO2 <- precis(bowtMLM,prob=0.95)@output$'lower 0.95'[2]
bowrUP2 <- precis(bowtMLM,prob=0.95)@output$'upper 0.95'[2]
atlrLO2 <- precis(atlatltMLM,prob=0.95)@output$'lower 0.95'[2]
atlrUP2 <- precis(atlatltMLM,prob=0.95)@output$'upper 0.95'[2]

# Initialize plotting frame
plot(bowdens2, xlab=expression(beta), 
     ylab="Density", xlim=c(-0.2,0.6), ylim=c(0,10), 
     col=col.alpha("blue",0), main=NA,
     cex.lab=1.4, cex.axis=1.2)
# Plot density of individuals' slopes
polygon(bowdens2, col=col.alpha("black",0.7), border=NA)
polygon(atldens2, col=col.alpha("grey",0.8), border=NA)
# Plot credible intervals for pooled slopes
rect(xleft=bowrLO2, xright=bowrUP2, ybottom=0, ytop=10,
     col=col.alpha("grey",0), border="grey")
rect(xleft=atlrLO2, xright=atlrUP2, ybottom=0, ytop=10,
     col=col.alpha("black",0), border="black")
# Plot pooled means for slopes
segments(x0=Pbt, x1=Pbt, y0=0, y1=10, col="grey", lwd=2, lty=5)
segments(x0=PbAt, x1=PbAt, y0=0, y1=10, col="black", lwd=2, lty=5)
text(0.345, 9, labels="Bow", pos=4, cex=1.2)
text(0.345, 8.2, labels="Atlatl", pos=4, cex=1.2)
rect(xleft=0.22, xright=0.35, ybottom=8.8, ytop=9.2,
     col=col.alpha("black",0.7), border=NA)
rect(xleft=0.22, xright=0.35, ybottom=8, ytop=8.4,
     col=col.alpha("grey",0.8), border=NA)
segments(x0=0.285, x1=0.285, y0=8.8, y1=9.2, col="grey", lwd=2, lty=5)
segments(x0=0.285, x1=0.285, y0=8, y1=8.4, col="black", lwd=2, lty=5)
rect(xleft=0.27, xright=0.30, ybottom=8.8, ytop=9.2,
     col=col.alpha("grey",0), border="grey")
rect(xleft=0.27, xright=0.30, ybottom=8, ytop=8.4,
     col=col.alpha("black",0), border="black")
text(-0.17, 9.6, labels="b", cex=1.6)



#####################################
# BOW PLOT: gamma (not transformed) #
#####################################

# Reset plotting frame to 1x1 panel
par(mfrow=c(1,1))

# Extract beta coefs
bowBp <- coef(bowgMLM)[1:length(pbnamest)]

# Extract intercept coefficients
bowAp <- coef(bowgMLM)[(length(pbnamest)+1):(2*length(pbnamest))]

# Find sample size by competitor
bowcompetitorsp <- as.data.frame(table(as.character(bow$name)),
                                 stringsAsFactors=FALSE)
colnames(bowcompetitorsp) <- c("bownames","n")
# Find last competition year for each competitor
bowcompetitorsp$last <- sapply(bowcompetitorsp$bownames,
                               function(x) max(bow$year[which(bow$name==x)]))

# Pooled a
Pap <- coef(bowgMLM)[2*length(pbnamest)+1]
# Pooled B
Pbp <- coef(bowgMLM)[2*length(pbnamest)+2]

# Set empty plot frame
plot(1, type="n", xlab="Competition year", ylab="Bow score", 
     xlim=c(0,max(bow$year)), ylim=c(0,360), yaxt='n',
     cex.lab=1.4, cex.axis=1.2)
axis(2, at=seq(from=0, to=360, length.out=7))
# Plot individual models
for(i in 1:length(pbnamest)){
  if(bowBp[i]>0){Bcolor <- "blue"} else {Bcolor <- "red"}
  curve(exp(1)^(bowAp[i]+(x*bowBp[i])), 
        from=0, to=bowcompetitorsp$last[i],
        col=alpha(Bcolor,0.12), add=TRUE)
}
# Plot curve for pooled model
curve(exp(1)^(Pap+(x*Pbp)), from=0, to=max(bow$year),
      col=alpha("purple", 0.8), lwd=4, add=TRUE)




##############################################################################
########### MEAN SCORE AND NUMBER OF COMPETITION YEARS SCATTER PLOTS #########
##############################################################################

# Find mean scores for each competitor
acompetitorst$scoreMU <- sapply(acompetitorst$atlatlnames, 
                  function(x) mean(atlatl$score[which(atlatl$name==x)]))
bowcompetitorst$scoreMU <- sapply(bowcompetitorst$bownames, 
                  function(x) mean(bow$score[which(bow$name==x)]))

# Create matrix to store bow summary stats for each number 
# of competition years
bowstats <- as.data.frame(matrix(0, 6, 4))
colnames(bowstats) <- c("numyear", "median", "lquart", "uquart")
bowstats$numyear <- seq(from=4, to=9)
bowstats$median <- sapply(4:9, function(x) 
  median(bowcompetitorst$scoreMU[which(bowcompetitorst$n==x)]))
bowstats$lquart <- sapply(4:9, function(x) 
  quantile(bowcompetitorst$scoreMU[which(bowcompetitorst$n==x)], 0.25))
bowstats$uquart <- sapply(4:9, function(x) 
  quantile(bowcompetitorst$scoreMU[which(bowcompetitorst$n==x)], 0.75))
# Creat polygon vertices for quantiles
bverts <- data.frame("x"=rep(0,12), "y"=rep(0,12))
for (k in 1:6){
  bverts$x[k] <- bowstats$numyear[k]
  bverts$x[13-k] <- bowstats$numyear[k]
  bverts$y[k] <- bowstats$uquart[k]
  bverts$y[13-k] <- bowstats$lquart[k]
}

# Create matrix to store atlatl summary stats for each number 
# of competition years
atlstats <- as.data.frame(matrix(0, 8, 4))
colnames(atlstats) <- c("numyear", "median", "lquart", "uquart")
atlstats$numyear <- seq(from=4, to=11)
atlstats$median <- sapply(4:11, function(x) 
  median(acompetitorst$scoreMU[which(acompetitorst$n==x)]))
atlstats$lquart <- sapply(4:11, function(x) 
  quantile(acompetitorst$scoreMU[which(acompetitorst$n==x)], 0.25))
atlstats$uquart <- sapply(4:11, function(x) 
  quantile(acompetitorst$scoreMU[which(acompetitorst$n==x)], 0.75))
# Creat polygon vertices for quantiles
averts <- data.frame("x"=rep(0,16), "y"=rep(0,16))
for (k in 1:8){
  averts$x[k] <- atlstats$numyear[k]
  averts$x[17-k] <- atlstats$numyear[k]
  averts$y[k] <- atlstats$uquart[k]
  averts$y[17-k] <- atlstats$lquart[k]
}

# Set plotting frame to 2x1 panels.
par(mfrow=c(1,2))

plot(x=acompetitorst$n, y=acompetitorst$scoreMU, ylab="Mean score",
     xlab="Number of competition years", col=col.alpha("black",0.8),
     xlim=c(4, 13), ylim=c(0,100), cex.lab=1.4, cex.axis=1.2)
polygon(x=averts$x, y=averts$y, col=col.alpha("grey",0.3),border=NA)
lines(x=atlstats$numyear, y=atlstats$median, col="grey", lwd=2)
points(x=atlstats$numyear, y=atlstats$median, col="grey", pch=21,
       bg="grey", cex=1.5)
points(x=acompetitorst$n, y=acompetitorst$scoreMU, bg=NA, 
       col=col.alpha("black",0.6))
text(4.2, 97, labels="a", cex=1.6)

plot(x=bowcompetitorst$n, y=bowcompetitorst$scoreMU, ylab="Mean score",
     xlab="Number of competition years", col=col.alpha("black",0.8),
     xlim=c(4, 13), ylim=c(0,360), cex.lab=1.4, cex.axis=1.2)
polygon(x=bverts$x, y=bverts$y, col=col.alpha("grey",0.3),border=NA)
lines(x=bowstats$numyear, y=bowstats$median, col="grey", lwd=2)
points(x=bowstats$numyear, y=bowstats$median, col="grey", pch=21,
       bg="grey", cex=1.5)
points(x=bowcompetitorst$n, y=bowcompetitorst$scoreMU, bg=NA, 
       col=col.alpha("black",0.6))
text(4.2, 350, labels="b", cex=1.6)