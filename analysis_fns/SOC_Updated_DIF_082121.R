library(tidyverse)
library(dplyr)
library(aMNLFA)
library(MplusAutomation)

################# All below code written by Joshua Rutsohn and Isa Stallworthy (some edits by KD) ####################

setwd('../mnlfa_results/soc_files')
homedir <- getwd()
#source("fixPath.R")

x_var_data_soc <- data.frame(read_csv("../../Data/x_var_data_soc.csv"))

x_var_data_rrb <- data.frame(read_csv("../../Data/x_var_data_rrb.csv"))

items_soc <- names(x_var_data_soc)[-which(names(x_var_data_soc)%in% c("ID","AGE","VNUM","GNUM","GCHAR","IQMULL","DXV24", "DX_GNUM", "DX_AGE", "DXGENCHA",
                                                                      "IQBIN", "DX_G1", "DX_G2", "DX_G3"))]

items_rrb <- names(x_var_data_rrb)[-which(names(x_var_data_rrb)%in% c("ID","AGE","VNUM","GNUM","GCHAR","IQMULL","DXV24", "DX_GNUM", "DX_AGE", "DXGENCHA",
                                                                      "IQBIN", "DX_G1", "DX_G2", "DX_G3"))]

ob <-aMNLFA.object(dir          = homedir, # location of data
                   mrdata        = x_var_data_soc, #read in dataframe from R
                   indicators    = # list a set of indicators of a single factor; make names as short as possible
                     items_soc,
                   catindicators = c("EYEC", "RNAME", "SOCINT", "SOCREF", "QSHOW", "QQSO", "Q5", "Q9"),       
                   time        = "AGE", #age variable (can be centered)
                   #mean and var are for things you are substantively interested in
                   meanimpact    =  #what your moderators of interest are
                     c("GNUM", "AGE"), # Contrast coding of nominal variables
                   varimpact     = "AGE", # contrast coding of nominal variables; this is computational expensive; JUST DO TIME VARIABLE
                   #this part: specific indicators impacted by mods? should included all mean/var impact items
                   measinvar     = c("GNUM", "AGE"), 
                   factors       = c("GCHAR"),#which of variables are factors
                   ID            = "ID",
                   auxiliary     = c("ID","VNUM"), 
                   thresholds    = FALSE) # indicate whether you would like to test measurement invariance of the different thresholds for ordinal indicators 

#source(paste(homedir, 'aMNLFA_itemplots.R', sep='/')) 
aMNLFA.itemplots(ob) #can give clues to invariance, produces item plots over time for each other moderators in folder


#################################################### 
# 3.	Draw a calibration sample.
# Sample one calibration per ID. Outputs a calibration file. Was originally designed for independent obs. Build initial models w/ calib sample. 
#do for a couple of calibration samples to check robustness.
#################################################### 
# Author modified code to allow user to input calibration sample
#source(paste(homedir, 'aMNLFA_sample.R', sep='/')) 
aMNLFA.sample(ob) #produces calibration.dat, full.dat, header.tx, header2.txt, srdata.dat

####################################################
# 4.	Create Mplus input files for mean impact, variance impact, 
# and item-by-item measurement non-invariance (aMNLFA.initial)
#makes initial models, populated in folder w/ MPlus input files wa
####################################################
#source(paste(homedir, 'aMNLFA_initial.R', sep ='/')) #IS has an amended version this function to accommodate more items (and maintain the 90 character line limit for MPlus) if you get errors 
aMNLFA.initial(ob) #produces a series of measinvariance.imp files for each item, varimpact.imp, and meanimpact.imp

##################################
# 5. Incorporate all 'marginally significant' terms into a simultaneous Mplus input file;
################################
#source(paste(homedir, 'aMNLFA_simultaneous.R', sep ='/')) #this should pull in IS's corrected version that gets all lambda DIF 
aMNLFA.simultaneous(ob)
runModels(homedir, replaceOutfile = 'never')

##################################
#5a. NEW: Shows which effects will be significant based on different thresholds. 
# First, it will give you alphas corresponding to two possible options for how to the number of tests m: (1) set as the number of all possible tests (i.e., the number of items multiplied by the number of covariates; shown as "ibc" in the results); or (2) the number of tests actually included in the simultaneous model, shown as "actual" in the results. 
# Second, it will give you the option to use either (1) a Benjamini-Hochberg correction, shown as "BH" in the results; or (2) a Bonferroni correction, shown as "bon" in the results. 
# Third, if you're testing threshold DIF, it will give you the results if you trim non-significant effects based on (1) all categories' thresholds for each item, shown as "tdif" in the results; and (2) the category with the highest test statistic for each item, shown as "intdif" in the results. 
# The main difference here is that the aMNLFA.prune() function creates a different type of object (i.e., not an aMNLFA.object), which can be inspected and passed to aMNLFA.plot(). This function will give you plots for each different type of DIF parameter you have, as well as how your decisions would change based on different thresholds. So, as an example, suppose you have an aMNLFA.object called some.object. You could do:
##################################
#source(paste(homedir, 'aMNLFA_prune.R', sep = '/'))
prune.object <- aMNLFA.prune(ob) #makes an object with DIF by various kinds of MC strategies
prune.object.save <- prune.object
prune.object.save$summary$loadingDIF$item.name <- 
  prune.object.save$summary$indicators[prune.object.save$summary$loadingDIF$item.label]
save(prune.object.save, file="soc_prune_object.RData")

##################################
#5b. NEW: makes plot of intercept and loading DIF to show what would be signfiicant based on different thresholds. you will need to manually save these out. 
##################################
#source(paste(homedir, 'aMNLFA_DIFplot.R', sep ='/'))
aMNLFA.DIFplot(prune.object, "loading", log = FALSE) #makes plot of loading DIF by MC strategies 

aMNLFA.DIFplot(prune.object, "intercept", log = FALSE) #makes plot of intercept DIF by MC strategies 

#################################
#5c. Extract table of DIF results for manuscript
#################################
source("../../../aMNFLA_edit_fns/aMNLFA_DIFextract.R") 
loading_table <- aMNLFA.DIFextract(prune.object, "loading", log = FALSE) #makes plot of loading DIF by MC strategies 

intercept_table <- aMNLFA.DIFextract(prune.object, "intercept", log = FALSE) #makes plot of intercept DIF by MC strategies 

write_csv(loading_table, "dif_loading_table.csv")
write_csv(intercept_table, "dif_intercept_table.csv")

##################################
# 6. Trim non-sig terms.  
################################
#source(paste(homedir, 'aMNLFA_final.R', sep = '/')) #gets IS's corrected version 
aMNLFA.final(ob, mchoice = "actual", method = "BH") #set for correcting for actual nubmers of tests using BH correction (default/standard) BH MUST BE CAPITALIZED!
runModels(homedir, replaceOutfile = 'never') 

##################################
# 7. (only for longitudinal data) Use parameter values generated from the last calibration model to fix
# parameter values in the scoring model using the full, longitudinal dataset
################################
#source(paste(homedir, 'aMNLFA_scores.R', sep = '/'))
aMNLFA.scores(ob)
#The resulting Mplus script uses the long (mr.dat) data file and outputs factor
# score estimates for each observation. Run the resulting scores.inp script manually.
# This script produces a file containing factor score estimates if data are cross-sectional.

##################################
# 8. Describe and visualize factor score estimates and generate empirical item
# characteristic curves
##################################
# NOTE: Step 7 saves "scores.dat" in the wrong directory (was in my root dir). 
# Need to move this file to wdir before running this last line. 
#source(paste(homedir,'aMNLFA_scoreplots.R', sep = '/'))
aMNLFA.scoreplots(ob)
