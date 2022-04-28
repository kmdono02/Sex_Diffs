library(tidyverse)
library(readxl)
library(haven)
library(expss)
library(stringr)

raw_data <- read_sav(file="Data/Sex_diffs_20210412_trimmed_send.sav")
old_data <- read_sav(file="Data/IBIS_Sexdiffs_data_6.17_SHARE.sav")

data_names <- data.frame("variable_names"=names(raw_data))
write_csv(data_names, file="Data/variable_names.csv")

dataset_wide <- raw_data %>%
  mutate(Gender_Dummy=factor(Gender_Dummy),
         #Gender_Dummy=factor(Gender_dummy),
         Gender_Proband_dummy=factor(Gender_Proband_dummy),
         Risk_dummy=factor(Risk_dummy),
         V24_Group=factor(V24_Group),
         V36_ASD_dummy=factor(V36_ASD_dummy),
         #V36_Dx = factor(V36_Dx),
         ASD_DX_ever_Dummy=factor(ASD_DX_ever_Dummy)) %>%
  plyr::rename(replace = c("V36_Candidate_age"="V36_Candidate_Age", 
                           "V37plus_Candidate_age"="V37plus_Candidate_Age")) %>%
  data.frame()

add_labels <- function(x, y){
  #attr(x, "labels") <- attr(old_data[[y]], "labels")
  if(is.null(attr(raw_data[[y]], "labels"))==TRUE){
    attr(x, "labels") <- attr(old_data[[y]], "labels")
  }else{
    attr(x, "labels") <- attr(raw_data[[y]], "labels")
  }
  x
}

vars_to_change <- c("Gender_Dummy", "Gender_Proband_dummy", "Risk_dummy", "V24_Group",
                    "V36_ASD_dummy"
                    #"V36_Dx",
                    #"ASD_DX_ever_Dummy"
                    )

for(i in 1:length(vars_to_change)){
  dataset_wide[[vars_to_change[i]]] <- 
    sjlabelled::as_label(add_labels(x=dataset_wide[[vars_to_change[i]]],
                                    y=vars_to_change[i]))
}

# Checked factor assignments using ftables with rawdata, checks out
# Make sure all are in time_ format
to_replace <- c("V06A","V09A","V12A","V15A","V24A","V36A")
replace_with <- c("V06_A","V09_A", "V12_A","V15_A", "V24_A","V36_A")
target_text <- names(dataset_wide)

names(replace_with) <- to_replace

names(dataset_wide) <- str_replace_all(target_text, replace_with)
time_vars <- names(dataset_wide[grepl("V06|V09|V12|V15|V24|V36",
                                                    names(dataset_wide))])
vars_to_convert <- time_vars[!grepl("V24_Group|V36_ASD_dummy|V36_Dx", time_vars)]

dataset_long <- 
  dataset_wide %>%
  gather(variable, var_value, vars_to_convert) %>%
  separate(variable,c("Visit","Variable"),sep=4) %>%
  spread(key=Variable, value=var_value) %>%
  mutate(Visit=gsub("_","",Visit)) %>%
  arrange(Identifiers, Visit) %>%
  select(Identifiers, Visit, everything())

# Convert back to numeric
var_numeric_index <- 
which(grepl(x=names(dataset_long), 
      pattern = "ADOS|aosi|mullen|Candidate_Age|vineland|composite|AGE_"))

dataset_long[,var_numeric_index] <- 
  lapply(X = dataset_long[,var_numeric_index], FUN = as.numeric)

# Check new and old data at 6, 12, 24, 36 month points (should be the same!)
## Do the same process with the old data
dataset_wide_old <- old_data %>%
  mutate(#Gender_Dummy=factor(Gender_Dummy),
         Gender_Dummy=factor(Gender_dummy),
         Gender_Proband_dummy=factor(Gender_Proband_dummy),
         Risk_dummy=factor(Risk_dummy),
         V24_Group=factor(V24_Group),
         #V36_ASD_dummy=factor(V36_ASD_dummy),
         V36_Dx = factor(V36_Dx),
         ASD_DX_ever_Dummy=factor(ASD_DX_ever_Dummy)) %>%
  data.frame()

vars_to_change_old <- c("Gender_Dummy", "Gender_Proband_dummy", "Risk_dummy", "V24_Group",
                    #"V36_ASD_dummy"
                    "V36_Dx",
                    "ASD_DX_ever_Dummy"
)

for(i in 1:length(vars_to_change)){
  dataset_wide_old[[vars_to_change_old[i]]] <- 
    sjlabelled::as_label(add_labels(x=dataset_wide_old[[vars_to_change_old[i]]],
                                    y=vars_to_change_old[i]))
}

to_replace <- c("V06A","V12A","V24A","V36A")
replace_with <- c("V06_A", "V12_A", "V24_A","V36_A")
target_text <- names(dataset_wide_old)

names(replace_with) <- to_replace

names(dataset_wide_old) <- str_replace_all(target_text, replace_with)
time_vars <- names(dataset_wide_old[grepl("V06|V09|V12|V15|V24|V36",
                                      names(dataset_wide_old))])
vars_to_convert <- time_vars[!grepl("V24_Group|V36_ASD_dummy|V36_Dx", time_vars)]

dataset_long_old <- 
  dataset_wide_old %>%
  gather(variable, var_value, vars_to_convert) %>%
  separate(variable,c("Visit","Variable"),sep=4) %>%
  spread(key=Variable, value=var_value) %>%
  mutate(Visit=gsub("_","",Visit)) %>%
  arrange(Identifiers, Visit) %>%
  select(Identifiers, Visit, everything())

# Convert back to numeric
var_numeric_index_old <- 
  which(grepl(x=names(dataset_long_old), 
              pattern = "ADOS|aosi|mullen|Candidate_Age|vineland|composite|AGE_"))

dataset_long_old[,var_numeric_index_old] <- 
  lapply(X = dataset_long_old[,var_numeric_index_old], FUN = as.numeric)

## Now check datasets
vars_to_check <- names(dataset_long)[grepl("ADOS|aosi", names(dataset_long))]
vars_to_check <- vars_to_check[!grepl("V37|DONOTUSE|valid|aosi_total_score", vars_to_check)]
summary(dataset_long %>% 
          filter(Visit%in%c("V06", "V12", "V24", "V36")) %>% 
          select(vars_to_check))
summary(dataset_long_old %>% 
          filter(Visit%in%c("V06", "V12", "V24", "V36")) %>% 
          select(vars_to_check))

dataset_new_check <- dataset_long %>% 
  filter(Visit%in%c("V06", "V12", "V24", "V36")&Risk_dummy=="HR"&is.na(V24_Group)==0)
dataset_old_check <- dataset_long_old %>% 
  filter(Visit%in%c("V06", "V12", "V24", "V36")&Risk_dummy=="HR"&is.na(V24_Group)==0)
dataset_old_check$Identifiers[which(!(dataset_old_check$Identifiers %in% dataset_new_check$Identifiers))]
# View(dataset_new_check)
# View(dataset_old_check)

# Converted to long form
save(dataset_wide, file="Sex_Diff_WideData_6_27.RData")
save(dataset_long, file="Sex_Diff_LongData_6_27.RData")

save(dataset_wide_old, file="Sex_Diff_WideData_Old_6_27.RData")
save(dataset_long_old, file="Sex_Diff_LongData_Old_6_27.RData")

# Create standardizations of Social and RRB composites at each time point (i.e., time point specific)
# Calc. means for typical infants at each time point, for social and RRB composites
typical_means_bytime <- dataset_long %>% 
  filter(V24_Group=="Typical") %>%
  group_by(V24_Group, Visit) %>%
  summarise(mean_RRB_aff_typ = mean(as.numeric(RRB_composite), na.rm=TRUE),
            mean_social_aff_typ = mean(as.numeric(SocAff_composite), na.rm=TRUE)) %>%
  ungroup() %>%
  select(-V24_Group)

typical_sds_bytime <- dataset_long %>% 
  filter(V24_Group=="Typical") %>%
  group_by(V24_Group, Visit) %>%
  summarise(sd_RRB_aff_typ = sd(as.numeric(RRB_composite), na.rm=TRUE),
            sd_social_aff_typ = sd(as.numeric(SocAff_composite), na.rm=TRUE)) %>%
  ungroup() %>%
  select(-V24_Group)

dataset_long_v2 <-
  dataset_long %>%
  merge(typical_means_bytime) %>%
  merge(typical_sds_bytime) %>%
  mutate(RRB_composite_center=(RRB_composite-mean_RRB_aff_typ)/sd_RRB_aff_typ,
         SocAff_composite_center=(SocAff_composite-mean_social_aff_typ)/sd_social_aff_typ) %>%
  arrange(Identifiers, Visit)

dataset_long_forplot <-
  dataset_long_v2 %>%
  gather(c("RRB_composite_center", "SocAff_composite_center"), 
         key=composite_center_type, 
         value=composite_center_value) %>%
  arrange(Identifiers, Visit, composite_center_type)

ggplot(data=dataset_long_v2, 
       mapping=aes(x=SocAff_composite)) +
  facet_grid(V24_Group~Visit) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(data=dataset_long_v2, 
       mapping=aes(x=RRB_composite)) +
  facet_grid(V24_Group~Visit) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(data=dataset_long_v2, 
       mapping=aes(x=SocAff_composite_center)) +
  facet_grid(V24_Group~Visit) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(data=dataset_long_v2, 
       mapping=aes(x=RRB_composite_center)) +
  facet_grid(V24_Group~Visit) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")



  
