# Privacy experiment support code
Supporting code files for experiment on the effect of visual privacy on psychological privacy in a space-like context

Experiment: code for generating counterbalancing files

Knit file: code for generating knit patterns used for inflatable knit structure

Pneumatics: code for operating/testing pneumatic system

For running privacy experiment analyses:
first run data-cleaning.R with everything in the data folder; it will output data.RData which can be fed to data-analysis.R
data-analysis.R will produce data-formodeling.RData, which can be fed to data-modeling.R
data-modeling.R will take a while to run because of all the Bayesian scripts, but will output modeldata.RData, which can be used to directly call summaries of models without running the simulations again. 
(make sure to change the file directory to your own)
