library(tidyverse)
library(tidyLPA)

# Use IBIS_Sexdiffs_data_5_19_forKevin.sav for CSS scores.  Merge with 6_17, only keep though those in 6_17
# Load data: need 24 month ADOS SA CSS and ADOS RRB CSS in HR sample
load("../Sex_Diff_LongData_6_27.RData")

dataset_long_hr <- dataset_long %>%
  mutate(visit_num = as.numeric(gsub("V","",Visit))) %>%
  filter(Visit=="V24"&Risk_dummy=="HR") %>%
  select(Identifiers, Visit, Risk_dummy, Gender_Dummy, V24_Group, V36_ASD_dummy,
         Race, Ethnicity, Candidate_Age,
         ADOS_Comb_EC, ADOS_Comb_RespName, ADOS_Comb_ShEnj, ADOS_Comb_Show,
         ADOS_Comb_QSO, ADOS_Comb_Point, ADOS_Comb_Face,
         ADOS_Comb_HFMann, ADOS_Comb_SensInt, ADOS_Comb_RepBeh) %>%
  mutate(ADOS_CSS_SOC=(ADOS_Comb_EC+ADOS_Comb_RespName+ADOS_Comb_ShEnj+ADOS_Comb_Show+
                         ADOS_Comb_QSO+ADOS_Comb_Point+ADOS_Comb_Face)/8,
         ADOS_CSS_RRB=(ADOS_Comb_HFMann+ADOS_Comb_SensInt+ADOS_Comb_RepBeh)/3)

# Run univariate LPA for SA, RRB
## SA
soc_lpa <- estimate_profiles(dataset_long_hr %>% 
                               select(ADOS_CSS_SOC) %>%
                               drop_na() %>%
                               scale(), 
                             n_profiles = 1:10, 
                             variances = c("equal"),
                             covariances = c("zero")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

estimate_profiles(dataset_long_hr %>% 
                    select(ADOS_CSS_SOC) %>%
                    drop_na() %>%
                    scale(), 
                  n_profiles = 2, 
                  variances = c("equal"),
                  covariances = c("zero")) %>%
  plot_profiles()

## RRB
rrb_lpa <- estimate_profiles(dataset_long_hr %>% 
                               select(ADOS_CSS_RRB) %>%
                               drop_na() %>%
                               scale(), 
                             n_profiles = 1:10, 
                             variances = c("equal"),
                             covariances = c("zero")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

estimate_profiles(dataset_long_hr %>% 
                    select(ADOS_CSS_RRB) %>%
                    drop_na() %>%
                    scale(), 
                  n_profiles = 2, 
                  variances = c("equal"),
                  covariances = c("zero")) %>%
  plot_profiles()

# Run multivariate (?) LPA on both
soc_rrb_lpa <- estimate_profiles(dataset_long_hr %>% 
                                   select(ADOS_CSS_SOC, ADOS_CSS_RRB) %>%
                                   drop_na() %>%
                                   scale(), 
                                 n_profiles = 1:10, 
                                 variances = c("equal"),
                                 covariances = c("zero")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

estimate_profiles(dataset_long_hr %>% 
                    select(ADOS_CSS_SOC, ADOS_CSS_RRB) %>%
                    drop_na() %>%
                    scale(), 
                  n_profiles = 2, 
                  variances = c("equal"),
                  covariances = c("zero")) %>%
  plot_profiles()