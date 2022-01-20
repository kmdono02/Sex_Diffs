library(tidyverse)
library(dplyr)
library(lavaan)

setwd('../mnlfa_results/rrb_files')

# Load data, group visit correctly
x_var_data_soc <- data.frame(read_csv("../../Data/x_var_data_soc.csv")) %>% 
  mutate(VNUM_2=ifelse(VNUM==6|VNUM==9, 6,
                       ifelse(VNUM==12|VNUM==15, 12, VNUM)))
x_var_data_rrb <- data.frame(read_csv("../../Data/x_var_data_rrb.csv")) %>% 
  mutate(VNUM_2=ifelse(VNUM==6|VNUM==9, 6,
                       ifelse(VNUM==12|VNUM==15, 12, VNUM)))

## Check visit grouping
ftable(x_var_data_soc$VNUM, x_var_data_soc$VNUM_2)
ftable(x_var_data_rrb$VNUM, x_var_data_rrb$VNUM_2)

## Extract domain items
items_soc <- names(x_var_data_soc)[-which(names(x_var_data_soc)%in% c("ID","AGE","VNUM","GNUM","GCHAR","IQMULL","DXV24", "DX_GNUM", "DX_AGE", "DXGENCHA",
                                                                      "IQBIN", "DX_G1", "DX_G2", "DX_G3", "VNUM_2"))]

items_rrb <- names(x_var_data_rrb)[-which(names(x_var_data_rrb)%in% c("ID","AGE","VNUM","GNUM","GCHAR","IQMULL","DXV24", "DX_GNUM", "DX_AGE", "DXGENCHA",
                                                                      "IQBIN", "DX_G1", "DX_G2", "DX_G3", "VNUM_2"))]

# Want to fit CFAs by age and sex
## Create objs to hold factor model fits and scores
cfa_fit <- list()
cfa_fit_data <- list()
cfa_fit_data_merge <- list()

## Extract unique time and sex values
timepoints <- unique(x_var_data_rrb$VNUM_2)
sex_vals <- unique(x_var_data_rrb$GCHAR)

for(i in 1:length(timepoints)){
  cfa_fit[[i]] <- list()
  cfa_fit_data[[i]] <- list()
  
  ## Define FA model based on items at given time point
  if(timepoints[i]%in%c("6","12")){
    formula_vars_timespecific <- 
      items_rrb[!grepl("QREPB",items_rrb)]
  }else{
    if(timepoints[i]%in%c("24","36")){
      formula_vars_timespecific <- 
        items_rrb[!grepl("Q19|Q16",items_rrb)]
    }else{
      formula_vars_timespecific <- items_rrb
    }
  }
  
  for(j in 1:length(sex_vals)){
    ## Extract only data for current time, sex
    x_var_data_rrb_subset <- x_var_data_rrb %>% filter(VNUM_2==timepoints[i]&GCHAR==sex_vals[j])
    
    ## Create CFA model spec
    efa_1fact_model_obj <- 
      paste0('#factor loadings \n ', 
             "rrbcfa","=~ ",paste0(formula_vars_timespecific, 
                                   collapse = " + "),
             "\n #covariances")
    
    ## Fit CFA, extract scores
    cfa_fit[[i]][[j]] <- cfa(efa_1fact_model_obj, data=x_var_data_rrb_subset, meanstructure=TRUE,
                             missing="fiml")
    cfa_fit_data[[i]][[j]] <- cbind(x_var_data_rrb_subset, lavPredict(cfa_fit[[i]][[j]]))
  }
  
  ## Bind all scores for given time point
  cfa_fit_data_merge[[i]] <- do.call("rbind", cfa_fit_data[[i]])
}

## Bind all scores for entire factor data
cfa_fit_data_merge_final <- do.call("rbind", cfa_fit_data_merge) %>% arrange(ID, VNUM)

# Save scores to use with GMMs
write_csv(x=cfa_fit_data_merge_final, file="../../cfa_results/rrb_files/cfa_scores_wdx.csv")

