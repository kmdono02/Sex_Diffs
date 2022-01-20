# Table creation script
library(tidyverse)
library(flextable)
library(officer)
library(readxl)
library(aMNLFA)
library(gtsummary)
library(tools)
library(MplusAutomation)
library(mgsub)
library(ggpubr)
library(gridExtra)
library(VennDiagram)
library(moments)

### 0: Summary Statistics Table
load("Sex_Diff_LongData_6_27.RData")
load("Sex_Diff_WideData_6_27.RData")
dataset_num_ids <- read_csv(file="Docs/num_ids_og_ids_key.csv") %>% plyr::rename(replace=c("num_ids"="ID"))

# Filter out people not used in MNLFA+LR
rrb_factor_data <- read_csv(file="mnlfa_results/rrb_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  merge(dataset_num_ids, by="ID", all.x = TRUE, all.y=FALSE)
soc_factor_data <- read_csv(file="mnlfa_results/soc_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  merge(dataset_num_ids, by="ID", all.x = TRUE, all.y=FALSE)

# Extract out their IBIS IDs, only find summ stats for these
sum(!rrb_factor_data$Identifiers%in%soc_factor_data$Identifiers)
sum(!soc_factor_data$Identifiers%in%rrb_factor_data$Identifiers) # same IDs in both

# Need to replace values with labels for certain variables
dataset_wide_forsum_stats <- 
  dataset_wide %>% filter(Identifiers%in%soc_factor_data$Identifiers|Risk_dummy=="LR")

race_labs <- names(attr(dataset_wide_forsum_stats$Race, "label"))
ethnicity_labs <- names(attr(dataset_wide_forsum_stats$Ethnicity, "label"))

dataset_wide_forsum_stats <- dataset_wide_forsum_stats %>%
  mutate(Race=factor(Race),
         Ethnicity=factor(Ethnicity),
         Gender_Dummy=factor(toTitleCase(as.character(Gender_Dummy))),
         V24_ASD_dummy=ifelse(V24_ASD_dummy==0, "Negative",
                               ifelse(V24_ASD_dummy==1, "ASD", NA)))

levels(dataset_wide_forsum_stats$Race) <- race_labs[as.numeric(levels(dataset_wide_forsum_stats$Race))]
levels(dataset_wide_forsum_stats$Ethnicity) <- ethnicity_labs[as.numeric(levels(dataset_wide_forsum_stats$Ethnicity))]

summ_stats_table <-
  tbl_summary(dataset_wide_forsum_stats %>%
                mutate(), by=Risk_dummy, 
              include=c("Gender_Dummy", "Race", "Ethnicity", "V24_ASD_dummy"),
              label=c(Gender_Dummy~"Gender", V24_ASD_dummy~"24 Month Diagnosis")) %>% 
    add_p() %>%
  as_flex_table() %>%
  autofit()

save_as_docx(summ_stats_table, path = "Plots_Tables/summ_stats_table.docx")

#Reset here
rm(list=ls())

### 1: ADOS/AOSI Question Summary Table
## Mapping questions to constructs
question_data <- read_xlsx("Docs/Composites_AOSI_ADOS.xlsx") %>%
  mutate(Number=as.numeric(gsub("Could consider adding:", NA, Number))) %>%
  filter(is.na(Number)==0) %>%
  mutate(Construct=ifelse(Number>=1&Number<=10, "Social",
                          ifelse(Number%in%12:16, "RRB", NA))) %>%
  select(-`Justification?`, -`Look at % endorsed?`, -Number) %>%
  filter(is.na(Construct)==0) %>%
  pivot_longer(cols=c("AOSI Code 6 mo", "AOSI Code 12 mo",
                      "ADOS Code 24 mo", "ADOS Code 36 mo"),
               names_to="Instrument",
               values_to="Question") %>%
  mutate(Times_Obs=ifelse(grepl(" 6 mo", Instrument), "06 Months",
                          ifelse(grepl("12 mo", Instrument), "12 Months",
                                 ifelse(grepl("24 mo", Instrument), "24 Months",
                                        ifelse(grepl("36 mo", Instrument), "36 Months", NA)))),
         Instrument_only=ifelse(grepl("AOSI", Instrument), "AOSI",
                           ifelse(grepl("ADOS", Instrument), "ADOS", NA))) %>%
  filter(is.na(Question)==0) %>% 
  arrange(Construct, Behavior, `Overlapping across AOSI/ADOS?`, Instrument) %>%
  mutate(Question=gsub("M1", "_",
                       gsub("V24|V06_|V12_|V36", "", Question))) %>%
  mutate(Question_map=ifelse(is.na(`Overlapping across AOSI/ADOS?`)==1, Question, Behavior))

question_data$Question_map[grepl("aosi",question_data$Question_map)] <- 
  lapply(str_split(question_data$Question_map[grepl("aosi",question_data$Question_map)],"_"),
         function(x){paste(x[c(1,2)], collapse = "")}) %>%
  toupper()

question_data$Question_map <- str_replace(question_data$Question_map, "AOSI", "")

question_data$Question_map[grepl("ADOS",question_data$Question_map)] <-
  substr(x=lapply(str_split(question_data$Question_map[grepl("ADOS",
                                                             question_data$Question_map)],"_"),
                  function(x){paste(x[c(1,3)], collapse = "")}) %>%
           toupper(), start = 1, stop=8)

question_data$Question_map <- 
  str_replace(question_data$Question_map, "ADOS", "Q")

# Remap overlapping
question_data <- question_data %>%
  mutate(Question_map=ifelse(Question_map=="Atypical Motor", "MOTOR",
                             ifelse(Question_map=="Atypical Sensory", "SENS",
                                    ifelse(Question_map=="Eye Contact", "EYEC",
                                           ifelse(Question_map=="Response to name", "RNAME",
                                                  ifelse(Question_map=="Social Interest", "SOCINT",
                                                         ifelse(Question_map=="Social Referencing", "SOCREF",
                                                                Question_map)))))))

## Now read in DIF-related results
sc_dif_results <- merge(read_csv("mnlfa_results/soc_files/lambda_dif_from_aMNLFA_final.csv") %>%
                          plyr::rename(c("GNUM"="GNUM_Lambda", "AGE"="AGE_Lambda")),
  read_csv("mnlfa_results/soc_files/intercept_dif_from_aMNLFA_final.csv") %>%
    plyr::rename(c("GNUM"="GNUM_Intercept", "AGE"="AGE_Intercept"))) %>%
  plyr::rename(replace=c("...1"="Question_map"))

rrb_dif_results <- merge(read_csv("mnlfa_results/rrb_files/lambda_dif_from_aMNLFA_final.csv") %>%
                           plyr::rename(c("GNUM"="GNUM_Lambda", "AGE"="AGE_Lambda")),
  read_csv("mnlfa_results/rrb_files/intercept_dif_from_aMNLFA_final.csv") %>%
    plyr::rename(c("GNUM"="GNUM_Intercept", "AGE"="AGE_Intercept"))) %>%
  plyr::rename(replace=c("...1"="Question_map"))

# Merge into table 
question_data_dif <- merge(question_data, rbind(sc_dif_results, rrb_dif_results), 
                           all=TRUE, by="Question_map") %>%
  arrange(Construct, Behavior, Times_Obs)

# Check times and instruments
ftable(question_data_dif$Instrument, question_data_dif$Times_Obs)

# Finally change Question field to publication quality
question_data_dif$Question_pub <- question_data_dif$Question

question_data_dif$Question_pub[grepl("aosi", question_data_dif$Question_pub)] <- 
  lapply(str_split(question_data_dif$Question_pub[grepl("aosi",question_data_dif$Question_pub)],"_"),
         function(x){paste(x[c(1,2)], collapse = " ")}) %>%
  toupper()

question_data_dif$Question_pub[grepl("ADOS", question_data_dif$Question_pub)] <-
  lapply(str_split(question_data_dif$Question_pub[grepl("ADOS",
                                                                 question_data_dif$Question_pub)],"_"),
                  function(x){paste(x[c(1,3)], collapse = " ")}) %>%
  unlist()

question_data_final <- question_data_dif %>%
  select(Behavior, `Overlapping across AOSI/ADOS?`, Construct, Question_pub, Times_Obs,
         GNUM_Lambda:AGE_Intercept) %>%
  group_by(Question_pub) %>%
  mutate(Times_Obs=paste0(Times_Obs, collapse="; ")) %>%
  ungroup() %>%
  distinct() %>%
  group_by(Behavior) %>%
  mutate(Question_pub=paste0(Question_pub, collapse="; "),
         Times_Obs=paste0(Times_Obs, collapse="; ")) %>%
  distinct() %>%
  arrange(Construct, Behavior, Question_pub) %>%
  filter(!(Question_pub %in% c("ADOS Face", "ADOS Point"))) %>%
  select(Construct, Behavior, everything()) %>%
  mutate(GNUM_Lambda=ifelse(GNUM_Lambda==1, "*", ""),
         GNUM_Intercept=ifelse(GNUM_Intercept==1, "*", ""),
         AGE_Lambda=ifelse(AGE_Lambda==1, "*", ""),
         AGE_Intercept=ifelse(AGE_Intercept==1, "*", ""))

## Now create table
question_data_table <- 
  flextable(question_data_final %>% select(-`Overlapping across AOSI/ADOS?`)) %>%
    merge_v(j=c("Behavior", "Construct")) %>%
    valign(j=c("Behavior", "Construct"), valign="top") %>%
    align(j=c("GNUM_Lambda", "GNUM_Intercept", "AGE_Lambda", "AGE_Intercept"),
          align="center") %>%
    fontsize(j=c("GNUM_Lambda", "GNUM_Intercept", "AGE_Lambda", "AGE_Intercept"),
             size=20) %>%
    hline(i=rle(question_data_final$Construct)$lengths[1], 
          border=fp_border(color="black", width = 2)) %>%
    delete_part() %>%
    add_header(values=list(Construct="Construct",
                           Behavior="Behavior",
                           Question_pub="Question(s)",
                                  Times_Obs="Visits",
                                  GNUM_Lambda="Sex",
                                  AGE_Lambda="Age",
                                  GNUM_Intercept="Sex",
                                  AGE_Intercept="Age"), top=FALSE) %>%
    add_header(values=list(GNUM_Lambda="Loading DIF", AGE_Lambda="Loading DIF",
                           GNUM_Intercept="Intercept DIF", AGE_Intercept="Intercept DIF"), top=TRUE) %>%
    merge_h(part="header") %>%
    hline(part="header", 
          border=fp_border(color="black", width = 2)) %>%
    fix_border_issues() %>%
    autofit()

save_as_docx(question_data_table, path="Plots_Tables/question_table.docx")

#Reset here
rm(list=ls())

### 2: MNLFA model fit tables
## RRB
# Load in final model results
rrb_round3 <- readModels(target="mnlfa_results/rrb_files/round3calibration.out")$parameters$unstandardized %>%
  mutate(param_type=ifelse(grepl("ON", paramHeader)&!grepl("ETA.ON", paramHeader), "Intercept DIF",
                           ifelse(grepl("ETA.BY", paramHeader), "Loading",
                                  ifelse(grepl("New.Additional.Parameters", paramHeader), "Loading DIF",
                                         ifelse(grepl("Intercepts", paramHeader), "Intercept", 
                                                ifelse(grepl("Residual.Variances", paramHeader), "Variance",
                                                        ifelse(grepl("ETA.ON", paramHeader), "Mean Factor",
                                                               "Threshold")))))),
         param_name=ifelse(grepl("ON", paramHeader), paste0(paramHeader, param), 
                           ifelse(grepl("New.Additional.Parameters", paramHeader), NA, 
                                  param))) %>%
  mutate(param_name=mgsub(param_name, c(".ON", "[$]"), c(" on ", " = ")))

# Load in DIF results (for lambda dif)
load(file="mnlfa_results/rrb_files/rrb_prune_object.RData")
lambda_dif <- prune.object.save$summary$loadingDIF %>%
  select(param, covariate.name, item.name) %>%
  mutate(param_name = paste0(item.name, " on ", covariate.name)) %>%
  select(param, param_name)

# Merge in to create table
rrb_mnlfa_fit <- left_join(rrb_round3, lambda_dif, by="param") %>%
  mutate(param_name.x = ifelse(is.na(param_name.x)==1&is.na(param_name.y)==0,
                               param_name.y, param_name.x)) %>%
  select(-param_name.y) %>%
  filter(is.na(param_name.x)==0) %>%
  arrange(factor(param_type, levels = c("Intercept", "Loading", "Mean Factor", "Intercept DIF", "Loading DIF", "Variance")))

# flextable it
rrb_mnlfa_table <-
  flextable(rrb_mnlfa_fit %>% 
              select(param_type, param_name.x, est, se, pval) %>%
              mutate(est=ifelse(est==999, NA, est),
                     se=ifelse(se==0, NA, se),
                     pval=ifelse(pval==999, NA, pval)) %>%
              filter(param_type!="Threshold")) %>%
    set_header_labels(param_type="Parameter Type", param_name.x="Variables", 
                      est="Estimate", se="SE", pval="P-value") %>%
    merge_v(j=1) %>%
    valign(j=1, valign="top") %>%
    autofit() %>%
    fix_border_issues()
  
save_as_docx(rrb_mnlfa_table, path="Plots_Tables/rrb_mnlfa_results.docx")

#Reset here
rm(list=ls())

## SC
# Load in final model results
soc_round3 <- readModels(target="mnlfa_results/soc_files/round3calibration.out")$parameters$unstandardized %>%
  mutate(param_type=ifelse(grepl("ON", paramHeader)&!grepl("ETA.ON", paramHeader), "Intercept DIF",
                           ifelse(grepl("ETA.BY", paramHeader), "Loading",
                                  ifelse(grepl("New.Additional.Parameters", paramHeader), "Loading DIF",
                                         ifelse(grepl("Intercepts", paramHeader), "Intercept", 
                                                ifelse(grepl("Residual.Variances", paramHeader), "Variance",
                                                       ifelse(grepl("ETA.ON", paramHeader), "Mean Factor",
                                                              "Threshold")))))),
         param_name=ifelse(grepl("ON", paramHeader), paste0(paramHeader, param), 
                           ifelse(grepl("New.Additional.Parameters", paramHeader), NA, 
                                  param))) %>%
  mutate(param_name=mgsub(param_name, c(".ON", "[$]"), c(" on ", " = ")))

# Load in DIF results (for lambda dif)
load(file="mnlfa_results/soc_files/soc_prune_object.RData")
lambda_dif <- prune.object.save$summary$loadingDIF %>%
  select(param, covariate.name, item.name) %>%
  mutate(param_name = paste0(item.name, " on ", covariate.name)) %>%
  select(param, param_name)

# Merge in to create table
soc_mnlfa_fit <- left_join(soc_round3, lambda_dif, by="param") %>%
  mutate(param_name.x = ifelse(is.na(param_name.x)==1&is.na(param_name.y)==0,
                               param_name.y, param_name.x)) %>%
  select(-param_name.y) %>%
  filter(is.na(param_name.x)==0) %>%
  arrange(factor(param_type, levels = c("Intercept", "Loading", "Mean Factor", "Intercept DIF", "Loading DIF", "Variance")))

soc_mnlfa_table <-
  flextable(soc_mnlfa_fit %>% 
              select(param_type, param_name.x, est, se, pval) %>%
              mutate(est=ifelse(est==999, NA, est),
                     se=ifelse(se==0, NA, se),
                     pval=ifelse(pval==999, NA, pval)) %>%
              filter(param_type!="Threshold")) %>%
  set_header_labels(param_type="Parameter Type", param_name.x="Variables", 
                    est="Estimate", se="SE", pval="P-value") %>%
  merge_v(j=1) %>%
  valign(j=1, valign="top") %>%
  autofit() %>%
  fix_border_issues()

save_as_docx(soc_mnlfa_table, path="Plots_Tables/soc_mnlfa_results.docx")

#Reset here
rm(list=ls())

### 3: GMM model fit table
## RRB

## SC

### 4: Sex ratios of GMM groups vs ASD
## RRB
load("traj_results/rrb/gmm_data.RData")
load("traj_results/rrb/gmm_predprob.RData")
load("traj_results/rrb/gmm_pred_outcome.RData")

gmm_full_data <- 
  left_join(gmm_predprob, gmm_data) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male"))

# First ratios of ASD and class
freq_data <- gmm_full_data %>% select(ID, dx_char, class, GCHAR) %>% unique() %>% select(ID, dx_char, class, GCHAR)

freqs_by_dx <- 
  freq_data %>% 
    group_by(dx_char) %>%
    count(GCHAR) %>%
    mutate(prop=n/sum(n))

freqs_by_class <-
  freq_data %>% 
    group_by(GCHAR) %>%
    count(class) %>%
    mutate(prop=n/sum(n))

flextable(freqs_by_dx) %>%
  set_header_labels('dx_char'="DX", "prop"="%", "GCHAR"="Gender", "n"="N") %>%
  colformat_double(digits=2) %>%
  merge_v() %>%
  valign(valign="top") %>%
  fix_border_issues() %>%
  save_as_docx(path="traj_results/rrb/class_freqs_by_dx.docx")

flextable(freqs_by_class) %>%
  set_header_labels("class"="Class", 'dx_char'="DX", "prop"="%", "GCHAR"="Gender", "n"="N") %>%
  colformat_double(digits=2) %>%
  merge_v() %>%
  valign(valign="top") %>%
  fix_border_issues() %>%
  save_as_docx(path="traj_results/rrb/class_freqs_by_class.docx")

# Create 2x2 table of classes and diagnosis
rbind(PercTable(class~dx_char, data=freq_data, subset=GCHAR=="Female", rfrq="001")$ftab %>%
        as.data.frame() %>%
        pivot_wider(names_from="Var2", values_from="Freq") %>%
        mutate(metric=paste0(as.numeric(freq), "\n(", p.col, ")")) %>%
        select(-freq, -p.col) %>%
        pivot_wider(names_from="dx_char", values_from="metric") %>%
        mutate(Sex="Female") %>%
        select(Sex, everything()),
      
  PercTable(class~dx_char, data=freq_data, subset=GCHAR=="Male", rfrq="001")$ftab %>%
  as.data.frame() %>%
  pivot_wider(names_from="Var2", values_from="Freq") %>%
  mutate(metric=paste0(as.numeric(freq), "\n(", p.col, ")")) %>%
  select(-freq, -p.col) %>%
  pivot_wider(names_from="dx_char", values_from="metric") %>%
  mutate(Sex="Male") %>%
  select(Sex, everything())) %>%
    flextable() %>%
    set_header_labels("class"="Class") %>%
    merge_v(j=1) %>%
    fix_border_issues() %>%
    save_as_docx(path="traj_results/rrb/class_asd_2x2_table.docx")



# Plot predicted probabilities
ggplot(data=merge(gmm_predprob, gmm_data %>% 
                    select(ID, DXGENCHA) %>% 
                    unique() %>%
                    mutate(DXGENCHA=fct_recode(DXGENCHA, `Female ASD`="female_ASD", `Female Neg`="female_nonASD",
                                               `Male ASD`="male_ASD", `Male Neg`="male_nonASD")), 
                  by="ID")) +
  geom_point(mapping=aes(x=prob1, y=prob2, color=DXGENCHA, shape=as.factor(class)))+
  labs(x="Probability in class 1", y="Probability in class 2", color="24 Month DX, Sex", shape="Class")+
  theme_bw()+
  theme(text=element_text(size=20))
ggsave("traj_results/rrb/gmm_pred_prob_plot.jpg", scale=2, limitsize = FALSE)

#Reset here
rm(list=ls())

## SOC
load("traj_results/soc/gmm_data.RData")
load("traj_results/soc/gmm_predprob.RData")
load("traj_results/soc/gmm_pred_outcome.RData")

gmm_full_data <- 
  left_join(gmm_predprob, gmm_data) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male"))

# First ratios of ASD and class
freq_data <- gmm_full_data %>% select(ID, dx_char, class, GCHAR) %>% unique() %>% select(ID, dx_char, class, GCHAR)

freqs_by_dx <- 
  freq_data %>% 
  group_by(dx_char) %>%
  count(GCHAR) %>%
  mutate(prop=n/sum(n))

freqs_by_class <-
  freq_data %>% 
  group_by(GCHAR) %>%
  count(class) %>%
  mutate(prop=n/sum(n))

flextable(freqs_by_dx) %>%
  set_header_labels('dx_char'="DX", "prop"="%", "GCHAR"="Gender", "n"="N") %>%
  colformat_double(digits=2) %>%
  merge_v() %>%
  valign(valign="top") %>%
  fix_border_issues() %>%
  save_as_docx(path="traj_results/soc/class_freqs_by_dx.docx")

flextable(freqs_by_class) %>%
  set_header_labels("class"="Class", 'dx_char'="DX", "prop"="%", "GCHAR"="Gender", "n"="N") %>%
  colformat_double(digits=2) %>%
  merge_v() %>%
  valign(valign="top") %>%
  fix_border_issues() %>%
  save_as_docx(path="traj_results/soc/class_freqs_by_class.docx")

rbind(PercTable(class~dx_char, data=freq_data, subset=GCHAR=="Female", rfrq="001")$ftab %>%
        as.data.frame() %>%
        pivot_wider(names_from="Var2", values_from="Freq") %>%
        mutate(metric=paste0(as.numeric(freq), "\n(", p.col, ")")) %>%
        select(-freq, -p.col) %>%
        pivot_wider(names_from="dx_char", values_from="metric") %>%
        mutate(Sex="Female") %>%
        select(Sex, everything()),
      
      PercTable(class~dx_char, data=freq_data, subset=GCHAR=="Male", rfrq="001")$ftab %>%
        as.data.frame() %>%
        pivot_wider(names_from="Var2", values_from="Freq") %>%
        mutate(metric=paste0(as.numeric(freq), "\n(", p.col, ")")) %>%
        select(-freq, -p.col) %>%
        pivot_wider(names_from="dx_char", values_from="metric") %>%
        mutate(Sex="Male") %>%
        select(Sex, everything())) %>%
  flextable() %>%
  set_header_labels("class"="Class") %>%
  merge_v(j=1) %>%
  fix_border_issues() %>%
  save_as_docx(path="traj_results/soc/class_asd_2x2_table.docx")

# Plot predicted probabilities
ggplot(data=merge(gmm_predprob, gmm_data %>% 
                    select(ID, DXGENCHA) %>% 
                    unique() %>%
                    mutate(DXGENCHA=fct_recode(DXGENCHA, `Female ASD`="female_ASD", `Female Neg`="female_nonASD",
                                               `Male ASD`="male_ASD", `Male Neg`="male_nonASD")), 
                  by="ID")) +
  geom_point(mapping=aes(x=prob1, y=prob2, color=DXGENCHA, shape=as.factor(class)))+
  labs(x="Probability in class 1", y="Probability in class 2", color="24 Month DX, Sex", shape="Class")+
  theme_bw()+
  theme(text=element_text(size=20))
ggsave("traj_results/soc/gmm_pred_prob_plot.jpg", scale=2, limitsize = FALSE)

#Reset here
rm(list=ls())

### 5. Associations and histograms between "raw" and "corrected" SC and RRB score by visit
load("Sex_Diff_LongData_6_27.RData")
load("Sex_Diff_WideData_6_27.RData")
dataset_num_ids <- read_csv(file="Docs/num_ids_og_ids_key.csv") %>% plyr::rename(replace=c("num_ids"="ID"))

# Calc RRB and SC "raw" scores by averages
dataset_long <- 
  dataset_long %>%
  mutate(visit_num = as.numeric(gsub("V","",Visit))) %>%
  mutate(social_factor = 
           ifelse(visit_num==6|visit_num==9|visit_num==12|visit_num==15, 
                  (aosi_q8_eye_contact+aosi_q3_orients_to_name_score+
                     aosi_q14_social_interest_recode+
                     aosi_q21_social_referencing+
                     aosi_q5_anticipatory_response_score_recode+
                     aosi_q9_reciprocal_social_smile_recode)/6,
                  ifelse(visit_num==24|visit_num==36,
                         (ADOS_Comb_EC+ADOS_Comb_RespName+ADOS_Comb_ShEnj+ADOS_Comb_Show+
                            ADOS_Comb_QSO+ADOS_Comb_Point+ADOS_Comb_Face)/8, NA)),
         rrb_factor=
           ifelse(visit_num==6|visit_num==9|visit_num==12|visit_num==15,
                  (aosi_q10_coordination_of_eye_gaze_and_action_recode+
                     aosi_q17_atypical_motor+
                     aosi_q18_atypical_sensory+aosi_q19_engagement_of_attention+
                     aosi_q16_motor_control_and_behaviour)/5,
                  ifelse(visit_num==24|visit_num==36,
                         (ADOS_Comb_HFMann+ADOS_Comb_SensInt+ADOS_Comb_RepBeh)/3, NA))) %>%
  merge(dataset_num_ids, by="Identifiers")

# Add in "corrected" RRB and SC scores
## SOC
load("traj_results/soc/gmm_data.RData")
soc_factor_data <- gmm_data %>% 
  select(ID, AGE, ETA) %>%
  plyr::rename(replace=c("AGE"="Candidate_Age", 
                         "ETA"="ETA_SOC"))

## RRB
load("traj_results/rrb/gmm_data.RData")
rrb_factor_data <- gmm_data %>% 
  select(ID, AGE, ETA) %>%
  plyr::rename(replace=c("AGE"="Candidate_Age", 
                         "ETA"="ETA_RRB"))

## Merge
dataset_long_merge <- reduce(list(dataset_long, soc_factor_data, rrb_factor_data), 
                             function(x,y){merge(x,y)}) %>%
  mutate(visit_num_edit=factor(ifelse(visit_num==6|visit_num==9, "6 or 9 months",
                               ifelse(visit_num==12|visit_num==15, "12 or 15 months", 
                                      paste0(visit_num, " months"))),
                               levels = c("6 or 9 months", "12 or 15 months", "24 months",
                                          "36 months")))

## Scatterplot by visit
ggplot(data=dataset_long_merge, mapping=aes(x=social_factor, y=ETA_SOC))+
    geom_point()+
    facet_wrap(~visit_num_edit, nrow=2)+
    geom_smooth(method="lm", se=FALSE)+
    stat_cor(size = 7)+
    theme_bw()+
    labs(x="Raw Score (Average)", y="Corrected Factor Score")+
    theme(text=element_text(size=30))
ggsave(filename = "Plots_Tables/sc_raw_v_corrected_scatter.jpg", scale=2, limitsize = FALSE)

ggplot(data=dataset_long_merge, mapping=aes(x=rrb_factor, y=ETA_RRB))+
    geom_point()+
    facet_wrap(~visit_num_edit, nrow=2)+
    geom_smooth(method="lm", se=FALSE)+
    stat_cor(size = 7)+
    theme_bw()+
    labs(x="Raw Score (Average)", y="Corrected Factor Score")+
    theme(text=element_text(size=30))
ggsave(filename = "Plots_Tables/rrb_raw_v_corrected_scatter.jpg", scale=2, limitsize = FALSE)

## Histograms and kernal density by visit
ggplot(data=dataset_long_merge %>% filter(Risk_dummy=="HR"))+
  geom_histogram(mapping=aes(x=social_factor, y=stat(density)*0.25), binwidth=0.25)+
  geom_histogram(mapping=aes(x=ETA_SOC, y=stat(density)*0.25), binwidth=0.25, fill="red", alpha=0.5)+
  facet_wrap(~visit_num_edit, nrow=2)+
  theme_bw()+
  labs(x="Score", y="Percent")+
  theme(text=element_text(size=30))
ggsave(filename = "Plots_Tables/sc_raw_v_corrected_hist.jpg", scale=2, limitsize = FALSE)

ggplot(data=dataset_long_merge %>% filter(Risk_dummy=="HR"))+
  geom_histogram(mapping=aes(x=rrb_factor, y=stat(density)*0.25), binwidth=0.25)+
  geom_histogram(mapping=aes(x=ETA_RRB, y=stat(density)*0.25), binwidth=0.25, fill="red", alpha=0.5)+
  facet_wrap(~visit_num_edit, nrow=2)+
  theme_bw()+
  labs(x="Score", y="Percent")+
  theme(text=element_text(size=30))
ggsave(filename = "Plots_Tables/rrb_raw_v_corrected_hist.jpg", scale=2, limitsize = FALSE)

## Table of means, SDs, skewness, kurtosis by visit for "raw" and "corrected" scores
sum_stats_factor_df <- dataset_long_merge %>% 
  filter(Risk_dummy=="HR") %>%
  group_by(visit_num_edit) %>%
  summarise(rrb_factor_mean_sd = paste0(round(mean(rrb_factor, na.rm=TRUE), 2), " (",
                                        round(sd(rrb_factor, na.rm=TRUE), 2), ")"),
            soc_factor_mean_sd = paste0(round(mean(social_factor, na.rm=TRUE), 2), " (",
                                        round(sd(social_factor, na.rm=TRUE), 2), ")"),
            rrb_factor_skew_kurt = paste0(round(skewness(rrb_factor, na.rm=TRUE), 2), "; ", 
                                          round(kurtosis(rrb_factor, na.rm=TRUE), 2)),
            soc_factor_skew_kurt = paste0(round(skewness(social_factor, na.rm=TRUE), 2), "; ", 
                                          round(kurtosis(social_factor, na.rm=TRUE), 2)),
            # Corrected
            rrb_eta_mean_sd = paste0(round(mean(ETA_RRB, na.rm=TRUE), 2), " (",
                                        round(sd(ETA_RRB, na.rm=TRUE), 2), ")"),
            soc_eta_mean_sd = paste0(round(mean(ETA_SOC, na.rm=TRUE), 2), " (",
                                        round(sd(ETA_SOC, na.rm=TRUE), 2), ")"),
            rrb_eta_skew_kurt = paste0(round(skewness(ETA_RRB, na.rm=TRUE), 2), "; ", 
                                          round(kurtosis(ETA_RRB, na.rm=TRUE), 2)),
            soc_eta_skew_kurt = paste0(round(skewness(ETA_SOC, na.rm=TRUE), 2), "; ", 
                                          round(kurtosis(ETA_SOC, na.rm=TRUE), 2))) %>%
  pivot_longer(cols=tidyr::contains(c("rrb", "soc"))) %>%
  mutate(subscore=ifelse(grepl("rrb", name), "RRB", "SC"),
         score_type=ifelse(grepl("eta", name), "Corrected", "Raw"),
         stat=ifelse(grepl("mean", name), "Mean (SD)", "Skew; Kurtosis")) %>%
  select(-name) %>%
  #pivot_wider(names_from = "stat", values_from = "value") %>%
  pivot_wider(names_from = c("visit_num_edit", "stat"), values_from = "value",
              names_sep = ":")

sum_stats_factor_tbl <- flextable(sum_stats_factor_df) %>%
  merge_v(j=1:2) %>%
  valign(j=1:2, valign = "top") %>%
  fix_border_issues() %>%
  add_header(`6 or 9 months:Mean (SD)`="6 or 9 months",
                 `6 or 9 months:Skew; Kurtosis`="6 or 9 months",
             `12 or 15 months:Mean (SD)`="12 or 15 months",
             `12 or 15 months:Skew; Kurtosis`="12 or 15 months",
             `24 months:Mean (SD)`="24 months",
             `24 months:Skew; Kurtosis`="24 months",
             `36 months:Mean (SD)`="36 months",
             `36 months:Skew; Kurtosis`="36 months") %>%
  merge_h(part="header") %>%
  align(align="center", part="header") %>%
  set_header_labels(subscore="Domain",
                    `score_type`="Score Type",
                    `6 or 9 months:Mean (SD)`="Mean (SD)",
                    `6 or 9 months:Skew; Kurtosis`="Skew; Kurtosis",
                    `12 or 15 months:Mean (SD)`="Mean (SD)",
                    `12 or 15 months:Skew; Kurtosis`="Skew; Kurtosis",
                    `24 months:Mean (SD)`="Mean (SD)",
                    `24 months:Skew; Kurtosis`="Skew; Kurtosis",
                    `36 months:Mean (SD)`="Mean (SD)",
                    `36 months:Skew; Kurtosis`="Skew; Kurtosis") %>%
  hline_top(border=fp_border(color="black", width = 2),
            part="header") %>%
  autofit()

save_as_docx(sum_stats_factor_tbl, path="Plots_Tables/raw_v_corrected_sum_stats.docx")
#save_as_image(sum_stats_factor_tbl, path="Plots_Tables/raw_v_corrected_sum_stats.png")

#Reset here
rm(list=ls())

### 6. Correlations between predicted probabilities with sex in model and separate by sex
## RRB
load("traj_results/rrb/gnum_0/gmm_predprob.RData")
gmm_predprob_gnum0 <- gmm_predprob %>%
  plyr::rename(replace=c("prob1"="prob1_sex_sep",
                         "prob2"="prob2_sex_sep")) %>%
  mutate(GNUM=0)

load("traj_results/rrb/gnum_1/gmm_predprob.RData")
gmm_predprob_gnum1 <- gmm_predprob %>%
  plyr::rename(replace=c("prob1"="prob1_sex_sep",
                         "prob2"="prob2_sex_sep")) %>%
  mutate(GNUM=1)
load("traj_results/rrb/gmm_predprob.RData")
load("traj_results/rrb/gmm_data.RData")
gmm_predprob <- merge(gmm_predprob, gmm_data %>% select(ID, GNUM) %>% unique())

gmm_predprob_merge <- merge(gmm_predprob %>% select(-class), 
                            rbind(gmm_predprob_gnum0, gmm_predprob_gnum1) %>% select(-class), 
                            all.x=TRUE)
class_1_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==0), mapping=aes(x=prob1, y=prob1_sex_sep))+
    geom_point()+
    stat_cor()+
    labs(x="Including Sex", y="Within Sex", title = "Class 1")+
    theme_bw()+
    theme(text=element_text(size=20), 
          plot.title = element_text(hjust = 0.5))

class_2_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==0), mapping=aes(x=prob2, y=prob2_sex_sep))+
    geom_point()+
    stat_cor()+
    labs(x="Including Sex", y="Within Sex", title = "Class 2")+
    theme_bw()+
    theme(text=element_text(size=20), 
          plot.title = element_text(hjust = 0.5))

gnum_0_scatter <- ggarrange(class_1_scatter, class_2_scatter, nrow=1)
rm(list=setdiff(ls(), c("gmm_predprob_merge", "gnum_0_scatter")))

class_1_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==1), mapping=aes(x=prob1, y=prob1_sex_sep))+
  geom_point()+
  stat_cor()+
  labs(x="Including Sex", y="Within Sex", title = "Class 1")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

class_2_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==1), mapping=aes(x=prob2, y=prob2_sex_sep))+
  geom_point()+
  stat_cor()+
  labs(x="Including Sex", y="Within Sex", title = "Class 2")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

gnum_1_scatter <- ggarrange(class_1_scatter, class_2_scatter, nrow=1)
ggarrange(gnum_1_scatter, gnum_0_scatter, ncol=1, labels=c("Female", "Male"))
ggsave("traj_results/rrb/pred_prob_sex_corrs.jpg", scale=1, limitsize = FALSE)

#Reset here
rm(list=ls())

##SOC
load("traj_results/soc/gnum_0/gmm_predprob.RData")
gmm_predprob_gnum0 <- gmm_predprob %>%
  plyr::rename(replace=c("prob1"="prob1_sex_sep",
                         "prob2"="prob2_sex_sep")) %>%
  mutate(GNUM=0)

load("traj_results/soc/gnum_1/gmm_predprob.RData")
gmm_predprob_gnum1 <- gmm_predprob %>%
  plyr::rename(replace=c("prob1"="prob1_sex_sep",
                         "prob2"="prob2_sex_sep")) %>%
  mutate(GNUM=1)
load("traj_results/soc/gmm_predprob.RData")
load("traj_results/soc/gmm_data.RData")
gmm_predprob <- merge(gmm_predprob, gmm_data %>% select(ID, GNUM) %>% unique())

gmm_predprob_merge <- merge(gmm_predprob %>% select(-class), 
                            rbind(gmm_predprob_gnum0, gmm_predprob_gnum1) %>% select(-class), 
                            all.x=TRUE)
ggplot(data=gmm_predprob_merge %>% filter(GNUM==0))+
  geom_histogram(mapping=aes(x=prob1), fill="red", alpha=0.5)+
  #geom_histogram(mapping=aes(x=prob2), fill="blue", alpha=0.5)+
  geom_histogram(mapping=aes(x=prob1_sex_sep), fill="green", alpha=0.5)
  #geom_histogram(mapping=aes(x=prob2_sex_sep), fill="yellow", alpha=0.5)

class_1_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==0), mapping=aes(x=prob1, y=prob1_sex_sep))+
  geom_point()+
  stat_cor(label.x=0.3)+
  labs(x="Including Sex", y="Within Sex", title = "Class 1")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

class_2_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==0), mapping=aes(x=prob2, y=prob2_sex_sep))+
  geom_point()+
  stat_cor(label.x=0.3, label.y=0)+
  labs(x="Including Sex", y="Within Sex", title = "Class 2")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

gnum_0_scatter <- ggarrange(class_1_scatter, class_2_scatter, nrow=1)
rm(list=setdiff(ls(), c("gmm_predprob_merge", "gnum_0_scatter")))

class_1_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==1), mapping=aes(x=prob1, y=prob1_sex_sep))+
  geom_point()+
  stat_cor()+
  labs(x="Including Sex", y="Within Sex", title = "Class 1")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

class_2_scatter <- 
  ggplot(data=gmm_predprob_merge %>% filter(GNUM==1), mapping=aes(x=prob2, y=prob2_sex_sep))+
  geom_point()+
  stat_cor()+
  labs(x="Including Sex", y="Within Sex", title = "Class 2")+
  theme_bw()+
  theme(text=element_text(size=20), 
        plot.title = element_text(hjust = 0.5))

gnum_1_scatter <- ggarrange(class_1_scatter, class_2_scatter, nrow=1)
ggarrange(gnum_1_scatter, gnum_0_scatter, ncol=1, labels=c("Female", "Male"))
ggsave("traj_results/soc/pred_prob_sex_corrs.jpg", scale=1, limitsize = FALSE)

#Reset here
rm(list=ls())

### 7.Venn Diagram of membership by sex
# Function used to load files and name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# RRB
gmm_data_rrb <- loadRData("traj_results/rrb/gmm_data.RData")
gmm_predprob_rrb <- loadRData("traj_results/rrb/gmm_predprob.RData")

gmm_full_data_rrb <- 
  left_join(gmm_predprob_rrb, gmm_data_rrb) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male")) %>%
  select(ID, class, dx_char, GCHAR) %>%
  unique() %>%
  plyr::rename(replace=c("class"="rrb_class"))

# SC
gmm_data_soc <- loadRData("traj_results/soc/gmm_data.RData")
gmm_predprob_soc <- loadRData("traj_results/soc/gmm_predprob.RData")

gmm_full_data_soc <- 
  left_join(gmm_predprob_soc, gmm_data_soc) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male")) %>%
  select(ID, class, dx_char, GCHAR) %>%
  unique() %>%
  plyr::rename(replace=c("class"="soc_class"))

# Merge
gmm_full_data_merge <- merge(gmm_full_data_soc, gmm_full_data_rrb) %>%
  arrange(ID)

# Replace with class names
# Key: SOC 2=HSSC, 1=LDSC
#      RRB 2=MIRC, 1=LIRC

gmm_full_data_merge <- gmm_full_data_merge %>%
  mutate(soc_class_char = ifelse(soc_class==1, "LDSC",
                                 ifelse(soc_class==2, "HSSC", NA)),
         rrb_class_char = ifelse(rrb_class==1, "LIRC",
                                 ifelse(rrb_class==2, "MIRC", NA)))

## Males
males_gmm_data <- gmm_full_data_merge %>% filter(GCHAR=="Male")

# Generate sets for venn diagram
asd <- males_gmm_data %>% filter(dx_char=="ASD") %>% select(ID) %>% unlist() %>% as.character()
hssc_sc <- males_gmm_data %>% filter(soc_class_char=="HSSC") %>% select(ID) %>% unlist() %>% as.character()
mirc_rrb <- males_gmm_data %>% filter(rrb_class_char=="MIRC") %>% select(ID) %>% unlist() %>% as.character()

# Chart
venn.diagram(
  x = list(asd, hssc_sc, mirc_rrb),
  category.names = c("ASD DX" , "HSSC Group (SC) " , "MIRC Group (RRB)"),
  filename = "Plots_Tables/male_venn_diagram.jpg",
  output=TRUE,
  col=c("#440154ff", '#21908dff', 'orange'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('orange',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', 'orange'),
  rotation = 1,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  main="MALES",
  main.cex = 0.5)

## Females
females_gmm_data <- gmm_full_data_merge %>% filter(GCHAR=="Female")

# Generate sets for venn diagram
asd <- females_gmm_data %>% filter(dx_char=="ASD") %>% select(ID) %>% unlist() %>% as.character()
hssc_sc <- females_gmm_data %>% filter(soc_class_char=="HSSC") %>% select(ID) %>% unlist() %>% as.character()
mirc_rrb <- females_gmm_data %>% filter(rrb_class_char=="MIRC") %>% select(ID) %>% unlist() %>% as.character()

# Chart
flog.threshold(ERROR, name = "VennDiagramLogger")
venn.diagram(
  x = list(asd, hssc_sc, mirc_rrb),
  category.names = c("ASD DX" , "HSSC Group (SC) " , "MIRC Group (RRB)"),
  filename = "Plots_Tables/female_venn_diagram.jpg",
  output=TRUE,
  col=c("#440154ff", '#21908dff', 'orange'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('orange',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', 'orange'),
  rotation = 1,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  main="FEMALES",
  main.cex = 0.5)

### Boxplots of factor scores
#Reset here
rm(list=ls())

#Load ID key and other day to get visit num
load("Sex_Diff_LongData_6_27.RData")
dataset_num_ids <- read_csv(file="Docs/num_ids_og_ids_key.csv") %>% plyr::rename(replace=c("num_ids"="ID"))

dataset_long_edit <- dataset_long %>% 
  select(Identifiers, Visit, Candidate_Age, Gender_Dummy) %>% 
  mutate(visit_num = as.numeric(gsub("V","",Visit)),
         visit_group = factor(ifelse(Visit=="V06"|Visit=="V09", "6/9",
                              ifelse(Visit=="V12"|Visit=="V15", "12/15",
                                     ifelse(is.na(Visit)==1, NA, as.character(visit_num)))),
                              levels=c("6/9", "12/15", "24", "36")),
         Gender_Dummy=factor(Gender_Dummy, levels=c("female","male"))) %>%
  plyr::rename(replace=c("Candidate_Age"="AGE"))

# RRB
rrb_factor_data <- read_csv(file="mnlfa_results/rrb_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  select(-ETA_Z) %>%
  merge(dataset_num_ids) %>%
  merge(dataset_long_edit) %>%
  arrange(ID, AGE) %>%
  plyr::rename(replace=c("ETA"="ETA_RRB", "ETA_SE"="ETA_RRB_SE"))

# SOC
soc_factor_data <- read_csv(file="mnlfa_results/soc_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  select(-ETA_Z) %>%
  merge(dataset_num_ids) %>%
  merge(dataset_long_edit) %>%
  arrange(ID, AGE) %>%
  plyr::rename(replace=c("ETA"="ETA_SOC", "ETA_SE"="ETA_SOC_SE"))

# Merge and pivot
factor_data_all <- merge(rrb_factor_data, soc_factor_data, all = TRUE) %>%
  arrange(ID, AGE)

write_csv(x=factor_data_all, file="mnlfa_results/factor_score_data_all.csv")
factor_data_all_long <- factor_data_all %>%
  select(-ETA_RRB_SE, -ETA_SOC_SE) %>%
  pivot_longer(cols=c("ETA_RRB", "ETA_SOC"), names_to = "ETA_TYPE", values_to="ETA_SCORE")

# Now do boxplots
ggplot(data = factor_data_all_long, mapping=aes(y=ETA_SCORE, x=visit_group, fill=Gender_Dummy)) +
  facet_grid(cols = vars(ETA_TYPE), labeller = as_labeller(c(`ETA_RRB` = "RRB",
                                                 `ETA_SOC` = "SOC"))) +
  geom_boxplot()+
  labs(x="Visit (months)", y="Factor Score", fill="Sex")+
  scale_fill_manual(values=c("#009E73", "#E69F00"))+
  theme_bw()+
  theme(text=element_text(size=35))
ggsave("Plots_Tables/boxplot_eta_by_sex_type_visit.jpg", scale=2, limitsize = FALSE)

# Look at histogram too
# Need to compute data ahead of time
breaks <- seq(round(min(factor_data_all_long$ETA_SCORE, na.rm = TRUE), 0),
              round(max(factor_data_all_long$ETA_SCORE, na.rm = TRUE), 0),
              by=0.5)
factor_data_all_long$eta_cut <- cut(factor_data_all_long$ETA_SCORE, breaks=breaks)

histo_data_bysex <- factor_data_all_long %>%
  group_by(visit_group, ETA_TYPE, Gender_Dummy, eta_cut) %>%
  summarise(n=n()) %>%
  mutate(perc = n/sum(n))

ggplot(data = histo_data_bysex, mapping=aes(x=eta_cut, y=perc, fill=Gender_Dummy)) +
  facet_grid(rows = vars(visit_group), cols = vars(ETA_TYPE), 
             labeller = as_labeller(c(`ETA_RRB` = "RRB",
                                      `ETA_SOC` = "SOC",
                                      `6/9`="6/9 months",
                                      `12/15`="12/15 months",
                                      `24`="24 months",
                                      `36`="36 months"))) +
  geom_bar(stat="identity", position="dodge")+
  labs(x="Factor Score", fill="Sex", y="Percentage")+
  scale_x_discrete(labels = breaks)+
  scale_fill_manual(values=c("#009E73", "#E69F00"))+
  theme_bw()+
  theme(text=element_text(size=35))
ggsave("Plots_Tables/histo_eta_by_sex_type_visit.jpg", scale=2, limitsize = FALSE)

#Reset here
rm(list=ls())

### 8. Validate classes using VABS
# Function used to load files and name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Load and edit factor data
gmm_data_rrb <- loadRData("traj_results/rrb/gmm_data.RData")
gmm_data_soc <- loadRData("traj_results/soc/gmm_data.RData")

gmm_predprob_rrb <- loadRData("traj_results/rrb/gmm_predprob.RData")
gmm_predprob_soc <- loadRData("traj_results/soc/gmm_predprob.RData")

gmm_full_data_rrb <- 
  left_join(gmm_predprob_rrb, gmm_data_rrb) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male")) %>%
  unique() %>%
  plyr::rename(replace=c("class"="rrb_class",
                         "ETA"="ETA_RRB"))

gmm_full_data_soc <- 
  left_join(gmm_predprob_soc, gmm_data_soc) %>%
  mutate(dx_char = ifelse(grepl("nonASD", DXGENCHA), "Neg",
                          ifelse(grepl("ASD", DXGENCHA), "ASD", NA)),
         class = factor(class),
         ID=factor(ID),
         GCHAR=ifelse(grepl("female", DXGENCHA), "Female", "Male")) %>%
  unique() %>%
  plyr::rename(replace=c("class"="soc_class",
                         "ETA"="ETA_SOC"))

gmm_full_data_merge <- 
  merge(gmm_full_data_soc %>% select(-prob1, -prob2, -ETA_SE, -ETA_Z), 
        gmm_full_data_rrb %>% select(-prob1, -prob2, -ETA_SE, -ETA_Z)) %>%
  arrange(ID) %>%
  mutate(soc_class_char = ifelse(soc_class==1, "LDSC",
                                 ifelse(soc_class==2, "HSSC", NA)),
         rrb_class_char = ifelse(rrb_class==1, "LIRC",
                                 ifelse(rrb_class==2, "MIRC", NA)))

# Load ID key and other to get VABS
load("Sex_Diff_WideData_6_27.RData")
dataset_num_ids <- read_csv(file="Docs/num_ids_og_ids_key.csv") %>% plyr::rename(replace=c("num_ids"="ID"))

full_data_merge <- merge(gmm_full_data_merge, dataset_num_ids) %>% 
  select(ID, Identifiers, GCHAR, dx_char, soc_class_char, rrb_class_char, soc_class, rrb_class) %>%
  arrange(ID) %>%
  unique()

full_data_merge_vabs <- merge(full_data_merge, dataset_wide %>% 
                                select(Identifiers, V24_vineland_subject_SOC_STD_SCORE, V24_vineland_subject_ABC_STD_SCORE))

# Create tables of means and SDs by group
vabs_soc_soc_stats <- 
  full_data_merge_vabs %>% 
    group_by(soc_class_char) %>%
    summarise(mean=round(mean(V24_vineland_subject_SOC_STD_SCORE, na.rm=TRUE), 2),
              sd=round(sd(V24_vineland_subject_SOC_STD_SCORE, na.rm=TRUE), 2),
              n=n()) %>%
    pivot_longer(cols = c("mean", "sd", "n")) %>%
    mutate(class_char_var = paste0(soc_class_char, "_", name)) %>%
    select(class_char_var, value) %>%
    pivot_wider(names_from = "class_char_var", values_from = "value") %>%
    mutate(pooled_sd=sqrt(((HSSC_n-1)*HSSC_sd^2+(LDSC_n-1)*LDSC_sd^2)/(HSSC_n+LDSC_n-2)),
           cohens_d=as.character(round((HSSC_mean-LDSC_mean)/pooled_sd, 2))) %>%
    gather(variable, var_value, c("HSSC_mean", "HSSC_sd", "HSSC_n", 
                                  "LDSC_mean", "LDSC_sd", "LDSC_n")) %>%
    separate(variable, c("class", "variable"), sep="_") %>%
    spread(key=variable, value=var_value) %>%
    select(class, n, mean, sd, cohens_d)
vabs_soc_soc_stats[2, dim(vabs_soc_soc_stats)[2]] <- ""
  
vabs_soc_rrb_stats <-
  full_data_merge_vabs %>% 
    group_by(rrb_class_char) %>%
    summarise(mean=round(mean(V24_vineland_subject_SOC_STD_SCORE, na.rm=TRUE), 2),
              sd=round(sd(V24_vineland_subject_SOC_STD_SCORE, na.rm=TRUE), 2),
              n=n()) %>%
    pivot_longer(cols = c("mean", "sd", "n")) %>%
    mutate(class_char_var = paste0(rrb_class_char, "_", name)) %>%
    select(class_char_var, value) %>%
    pivot_wider(names_from = "class_char_var", values_from = "value") %>%
    mutate(pooled_sd=sqrt(((LIRC_n-1)*LIRC_sd^2+(MIRC_n-1)*MIRC_sd^2)/(LIRC_n+MIRC_n-2)),
           cohens_d=as.character(round((LIRC_mean-MIRC_mean)/pooled_sd, 2))) %>%
    gather(variable, var_value, c("LIRC_mean", "LIRC_sd", "LIRC_n", 
                                  "MIRC_mean", "MIRC_sd", "MIRC_n")) %>%
    separate(variable, c("class", "variable"), sep="_") %>%
    spread(key=variable, value=var_value) %>%
    select(class, n, mean, sd, cohens_d)
vabs_soc_rrb_stats[2, dim(vabs_soc_rrb_stats)[2]] <- ""
  
vabs_abc_soc_stats <- 
  full_data_merge_vabs %>% 
    group_by(soc_class_char) %>%
    summarise(mean=round(mean(V24_vineland_subject_ABC_STD_SCORE, na.rm=TRUE), 2),
              sd=round(sd(V24_vineland_subject_ABC_STD_SCORE, na.rm=TRUE), 2),
              n=n()) %>%
    pivot_longer(cols = c("mean", "sd", "n")) %>%
    mutate(class_char_var = paste0(soc_class_char, "_", name)) %>%
    select(class_char_var, value) %>%
    pivot_wider(names_from = "class_char_var", values_from = "value") %>%
    mutate(pooled_sd=sqrt(((HSSC_n-1)*HSSC_sd^2+(LDSC_n-1)*LDSC_sd^2)/(HSSC_n+LDSC_n-2)),
           cohens_d=as.character(round((HSSC_mean-LDSC_mean)/pooled_sd, 2))) %>%
    gather(variable, var_value, c("HSSC_mean", "HSSC_sd", "HSSC_n", 
                                  "LDSC_mean", "LDSC_sd", "LDSC_n")) %>%
    separate(variable, c("class", "variable"), sep="_") %>%
    spread(key=variable, value=var_value) %>%
    select(class, n, mean, sd, cohens_d)
vabs_abc_soc_stats[2, dim(vabs_abc_soc_stats)[2]] <- ""
  
vabs_abc_rrb_stats <-
  full_data_merge_vabs %>% 
    group_by(rrb_class_char) %>%
    summarise(mean=round(mean(V24_vineland_subject_ABC_STD_SCORE, na.rm=TRUE), 2),
              sd=round(sd(V24_vineland_subject_ABC_STD_SCORE, na.rm=TRUE), 2),
              n=n()) %>%
    pivot_longer(cols = c("mean", "sd", "n")) %>%
    mutate(class_char_var = paste0(rrb_class_char, "_", name)) %>%
    select(class_char_var, value) %>%
    pivot_wider(names_from = "class_char_var", values_from = "value") %>%
    mutate(pooled_sd=sqrt(((LIRC_n-1)*LIRC_sd^2+(MIRC_n-1)*MIRC_sd^2)/(LIRC_n+MIRC_n-2)),
           cohens_d=as.character(round((LIRC_mean-MIRC_mean)/pooled_sd, 2))) %>%
    gather(variable, var_value, c("LIRC_mean", "LIRC_sd", "LIRC_n", 
                                  "MIRC_mean", "MIRC_sd", "MIRC_n")) %>%
    separate(variable, c("class", "variable"), sep="_") %>%
    spread(key=variable, value=var_value) %>%
    select(class, n, mean, sd, cohens_d)
vabs_abc_rrb_stats[2, dim(vabs_abc_rrb_stats)[2]] <- ""

# Plot boxplots
ggplot(data=full_data_merge_vabs %>% filter(V24_vineland_subject_SOC_STD_SCORE!=0), 
       mapping=aes(x=soc_class_char, y=V24_vineland_subject_SOC_STD_SCORE, fill=soc_class_char))+
  geom_boxplot()+
  annotation_custom(tableGrob(vabs_soc_soc_stats %>% 
                                plyr::rename(replace=c("class"="Class", "n"="N", "mean"="Mean", "sd"="SD", "cohens_d"="Cohen's D")), 
                              rows=NULL), ymin=25, ymax=50)+
  labs(y="24 Month VABS SOC\nStandard Score", x="SC Class")+
  ylim(c(25, 150))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=25))
ggsave("plot1.png", limitsize = FALSE, scale=1, dpi=320)

ggplot(data=full_data_merge_vabs %>% filter(V24_vineland_subject_SOC_STD_SCORE!=0), 
       mapping=aes(x=rrb_class_char, y=V24_vineland_subject_SOC_STD_SCORE, fill=rrb_class_char))+
  geom_boxplot()+
  annotation_custom(tableGrob(vabs_soc_rrb_stats %>% 
                                plyr::rename(replace=c("class"="Class", "n"="N", "mean"="Mean", "sd"="SD", "cohens_d"="Cohen's D")),
                              rows=NULL), ymin=25, ymax=50)+
  ylim(c(25, 150))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  labs(y="24 Month VABS SOC\nStandard Score", x="RRB Class")+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=25))
ggsave("plot2.png", limitsize = FALSE, scale=1, dpi=320)

ggplot(data=full_data_merge_vabs %>% filter(V24_vineland_subject_ABC_STD_SCORE!=0), 
       mapping=aes(x=soc_class_char, y=V24_vineland_subject_ABC_STD_SCORE, fill=soc_class_char))+
  geom_boxplot()+
  ylim(c(25, 150))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  annotation_custom(tableGrob(vabs_abc_soc_stats %>% 
                                plyr::rename(replace=c("class"="Class", "n"="N", "mean"="Mean", "sd"="SD", "cohens_d"="Cohen's D")), 
                              rows=NULL), ymin=25, ymax=50)+
  labs(y="24 Month VABS ABC\nStandard Score", x="SC Class")+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=25))
ggsave("plot3.png", limitsize = FALSE, scale=1, dpi=320)

ggplot(data=full_data_merge_vabs %>% filter(V24_vineland_subject_ABC_STD_SCORE!=0), 
       mapping=aes(x=rrb_class_char, y=V24_vineland_subject_ABC_STD_SCORE, fill=rrb_class_char))+
  geom_boxplot()+
  ylim(c(25, 150))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  annotation_custom(tableGrob(vabs_abc_rrb_stats %>% 
                                plyr::rename(replace=c("class"="Class", "n"="N", "mean"="Mean", "sd"="SD", "cohens_d"="Cohen's D")),
                              rows=NULL), ymin=25, ymax=50)+
  labs(y="24 Month VABS ABC\nStandard Score", x="RRB Class")+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=25))
ggsave("plot4.png", limitsize = FALSE, scale=1, dpi=320)

#Reset here
rm(list=ls())
