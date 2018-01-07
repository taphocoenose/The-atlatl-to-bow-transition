<b>The Atlatl to Bow Transition</b><br>
Supplemental R scripts and csv files for:

Breslawski RP, Etter BL, Jorgeson I, Boulanger MT (nd). The Atlatl to Bow Transition: What Can We Learn from Modern Recreational Competitions? <i>Lithic Technology</i>, in press, DOI:10.1080/01977261.2017.1416918

<b>Files</b><br>
atlatldata.csv: Table of atlatl competition scores.<br>
bowdata.csv: Table of archery competition scores.<br>
scoresMEDMOD.R: R script to reproduce median score models and associated figures.<br>
scoresMLMOD.R: R script to reproduce multilevel score models and associated figures.<br>

<b>Description</b><br>
Both R scripts require the <a href="http://xcelab.net/rm/software/">Rethinking</a> and <a href="http://mc-stan.org/">RStan</a> packages. You will need to save both csv files to the working directory when running each script. scoresMEDMOD.R will complete in under 20 minutes on most systems; it will generate 29.2 MB of data. scoresMLMOD.R requires a bit more computing power, and should complete in 2.5-3.5 hours. scoresMLMOD.R will create 328.3 MB of data. Both scripts are set to run Stan on 2 cores. If you are using a computer with more than 2 cores, you can adjust the 'cores' argument in each map2stan function to reduce the time spent on HMC simulation. The script currently uses 4 chains, so increasing 'cores' up to 4 will result in quicker completion.

Atlatl data are International Standard Accuracy Contest scores, and they were compiled from <a href="https://worldatlatl.org/waa_events/competition-scores/">publicly available data hosted and collected by the World Atlatl Association.</a>

Archery data are Inter-Kingdom Archery Competition scores, and they were compiled from <a href="https://scores-sca.org/home/index.php?R=10">publicly available data hosted and collected by the Society for Creative Anachronism.</a>
