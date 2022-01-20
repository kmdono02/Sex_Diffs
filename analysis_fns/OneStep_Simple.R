library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(DiagrammeR)
library(multiplex)
library(RColorBrewer)
library(lcmm)
library(gtsummary)
library(flextable)

############
#RRB Scores
############
factor_data <- read_csv(file="../mnlfa_results/rrb_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--"))

factor_data <- 
  factor_data %>%
  as.data.frame()


factor_data$ETA_Z <- scale(factor_data$ETA)
factor_data$AGE_CENT <- factor_data$AGE-6

## Simple model (AGE class-level)
#One cluster solution
gmm1_si <- hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT, ng=1, 
                data=factor_data)

#Two-Four clusters
set.seed(12)
gmm2_si_sex <- gridsearch(rep = 500, maxiter = 20, minit = gmm1_si, 
                          hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT,
                               ng = 2, data = factor_data, mixture = ~ AGE_CENT+GNUM, nwg=T))

gmm3_si <- gridsearch(rep = 50, maxiter = 10, minit = gmm1_si, 
                      hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT,
                           ng = 3, data = factor_data, mixture = ~ AGE_CENT+GNUM, nwg=T))

gmm4_si <- gridsearch(rep = 50, maxiter = 10, minit = gmm1_si, 
                      hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT,
                           ng = 4, data = factor_data, mixture = ~ AGE_CENT+GNUM, nwg=T))

# Compare BICs of all three
rrb_bic_results <-
  summarytable(gmm1_si, gmm2_si_sex, gmm3_si, gmm4_si,
               which = c("G", "loglik", "npm", "BIC", "AIC", "%class")) %>%
  as.data.frame() %>%
  rownames_to_column(var="model_name") %>%
  mutate(model_name=plyr::revalue(factor(model_name),
                                  c("gmm1_si"="1-class",
                                    "gmm2_si_sex"="2-class",
                                    "gmm3_si"="3-class",
                                    "gmm4_si"="4-class"))) %>%
  select(model_name:AIC) %>%
  flextable() %>%
  set_header_labels("model_name"="Model", "G"="# of Groups", "npm"="p") %>%
  autofit()
save_as_docx(rrb_bic_results, path = "../traj_results/rrb/gmm_fit_table.docx")
# simple model has better BIC

#1-step solution
#Starting values for 1-step solution. Need multinomial intercept, a probability for classmb = GNUM, and the estimates for
#the other coefficients from the 2-cluster solution
start.2p <- c(gmm2_si_sex$best[1],0,gmm2_si_sex$best[2:13]) 
gmm2_si.2p <- hlme(ETA_Z ~ AGE_CENT+GNUM+AGE_CENT*GNUM, data=factor_data, subject="ID", random = ~1+AGE_CENT, ng = 2, mixture = ~ AGE_CENT+GNUM, 
                   classmb = ~ GNUM,
                   nwg = T, B = start.2p, returndata = TRUE)

sink("../traj_results/rrb/RRB Simple 1-step Solution.txt")
summary(gmm2_si.2p)
sink()

flextable(rownames_to_column(as.data.frame(summary(gmm2_si.2p))) %>%
            mutate(`p-value`=ifelse(`p-value`<0.005, "<0.005", round(`p-value`, 3)),
                   rowname=gsub("intercept", "Intercept",
                                gsub("AGE_CENT","Age",
                                  gsub("GNUM","Sex",
                                    gsub("class2","MIRC",
                                      gsub("class1", "LIRC",rowname))))))) %>%
  colformat_double(digits=3) %>%
  set_header_labels("rowname"="Fixed Effect", "coef"="Estimate", "Se"="SE", "p-value"="P-value") %>%
  autofit() %>%
  save_as_docx(path="../traj_results/rrb/gmm_estimates_table.docx")

#Output class membership and posterior probs for sample
gmm_predprob <- gmm2_si.2p$pprob
gmm_data <- gmm2_si.2p$data
gmm_pred_outcome <- gmm2_si.2p$pred
gmm_pred_outcome_and_data <- cbind(gmm_data, gmm_pred_outcome %>% select(-ID))

save(gmm_predprob, file = "../traj_results/rrb/gmm_predprob.RData")
save(gmm_pred_outcome, file = "../traj_results/rrb/gmm_pred_outcome.RData")
save(gmm_data, file = "../traj_results/rrb/gmm_data.RData")
save(gmm_pred_outcome_and_data, file = "../traj_results/rrb/gmm_pred_outcome_and_data.RData")

# Output residual plots
jpeg(file="../traj_results/rrb/gmm_diagnostics.jpeg", width = 900, height=900)
plot(gmm2_si.2p)
dev.off()

surrogate_data_male <- data.frame(AGE_CENT=seq(0, 40, length=100))
surrogate_data_male$GNUM <- 0
male_pred <- predictY(gmm2_si.2p, newdata=surrogate_data_male, var.time="AGE_CENT", draws=T)

#plot(male_pred)

surrogate_data_female <- data.frame(AGE_CENT=seq(0, 40, length=100))
surrogate_data_female$GNUM <- 1
female_pred <- predictY(gmm2_si.2p, newdata=surrogate_data_female, var.time="AGE_CENT", draws=T)

#plot(male_pred)
#plot(female_pred, add=TRUE)

# Remove CIs
vars_to_convert <- c("Ypred_class1",
                     "Ypred_class2",
                     "lower.Ypred_class1",
                     "lower.Ypred_class2",
                     "upper.Ypred_class1", 
                     "upper.Ypred_class2")

pred_data <- rbind(cbind(female_pred$pred, female_pred$times, "Sex"="Female"),
                   cbind(male_pred$pred, male_pred$times, "Sex"="Male")) %>%
  rownames_to_column(var="ID") %>%
  gather(variable, var_value, vars_to_convert) %>%
  separate(variable,c("variable","class"),sep="_class") %>%
  spread(key=variable, value=var_value) %>%
  mutate(AGE=AGE_CENT+6,
         class_char = ifelse(class=="1", "LIRC", "MIRC"))

ggplot(data=pred_data, mapping=aes(x=AGE, y=Ypred, color=Sex))+
  geom_line(mapping=aes(linetype=class_char), size=1)+
  geom_ribbon(mapping=aes(ymin = lower.Ypred, ymax = upper.Ypred, by=class, fill=Sex), 
              size=0.5, alpha=0.25, linetype=0)+
  labs(y="Eta", x="Age (months)", linetype="Class")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  scale_linetype_manual(values=c("solid", "dashed"))+
  theme_bw()+
  theme(text = element_text(size=20))+
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave("../traj_results/rrb/mean_trends_class_centered.png", limitsize = FALSE, dpi=320)

# Test figure
## Observed trends
plot_1 <- ggplot(data=gmm_pred_outcome_and_data %>% mutate(Sex=ifelse(GNUM==0, "Male", "Female")), 
                                                 mapping=aes(x=AGE, y=ETA_Z, color=Sex))+
  geom_line(stat = "smooth", method="lm", mapping=aes(group=ID), alpha=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  labs(y="Eta", x="Age (months)")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  scale_y_continuous(limits=c(-2,4))+
  theme_bw()+
  theme(text = element_text(size=20))+
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))

## Predicted trends
plot_2 <- ggplot(data=pred_data, mapping=aes(x=AGE, y=Ypred, color=Sex))+
  geom_line(data=gmm_pred_outcome_and_data %>% mutate(Sex=ifelse(GNUM==0, "Male", "Female")), 
            mapping=aes(y=pred_ss, group=ID), alpha=0.2)+
  geom_line(mapping=aes(linetype=class_char), size=1)+
  geom_ribbon(mapping=aes(ymin = lower.Ypred, ymax = upper.Ypred, by=class, fill=Sex), 
              size=0.5, alpha=0.25, linetype=0)+
  labs(y="Eta", x="Age (months)", linetype="Class")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  scale_linetype_manual(values=c("solid", "dashed"))+
  scale_y_continuous(limits=c(-2,4))+
  theme_bw()+
  theme(text = element_text(size=20))+
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))

ggarrange(plotlist = list(plot_1, plot_2), nrow = 1)


#Reset here
rm(list=ls())

##################
#SOC Scores
##################
# Load factor scores
factor_data <- read_csv(file="../mnlfa_results/soc_files/factor_scores_wdx.csv",na=c("*","","NA","N/A","--"))

factor_data <- 
  factor_data %>%
  as.data.frame()


factor_data$ETA_Z <- scale(factor_data$ETA)
factor_data$AGE_CENT <- factor_data$AGE-6

## Simple model (AGE class-level)
#One cluster solution
gmm1_si <- hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT, ng=1, 
                data=factor_data)

#Two-Four clusters
set.seed(12)
gmm2_si_sex <- gridsearch(rep = 500, maxiter = 20, minit = gmm1_si, 
                          hlme(ETA_Z~AGE_CENT+GNUM+AGE_CENT*GNUM, subject="ID", random=~1+AGE_CENT,
                               ng = 2, data = factor_data, mixture = ~ AGE_CENT+GNUM, nwg=T))

gmm3_si <- gridsearch(rep = 50, maxiter = 10, minit = gmm1_si, 
                      hlme(ETA_Z~AGE+GNUM+AGE*GNUM, subject="ID", random=~1+AGE,
                           ng = 3, data = factor_data, mixture = ~ AGE+GNUM, nwg=T))

gmm4_si <- gridsearch(rep = 50, maxiter = 10, minit = gmm1_si, 
                      hlme(ETA_Z~AGE+GNUM+AGE*GNUM, subject="ID", random=~1+AGE,
                           ng = 4, data = factor_data, mixture = ~ AGE+GNUM, nwg=T))

# Compare BICs of all three
soc_bic_results <-
  summarytable(gmm1_si, gmm2_si_sex, gmm3_si, gmm4_si,
               which = c("G", "loglik", "npm", "BIC", "AIC", "%class")) %>%
  as.data.frame() %>%
  rownames_to_column(var="model_name") %>%
  mutate(model_name=plyr::revalue(factor(model_name),
                                  c("gmm1_si"="1-class",
                                    "gmm2_si_sex"="2-class",
                                    "gmm3_si"="3-class",
                                    "gmm4_si"="4-class"))) %>%
  select(model_name:AIC) %>%
  flextable() %>%
  set_header_labels("model_name"="Model", "G"="# of Groups", "npm"="p") %>%
  autofit()
save_as_docx(soc_bic_results, path = "../traj_results/soc/gmm_fit_table.docx")
# simple model has better BIC

#1-step solution
#Starting values for 1-step solution. Need multinomial intercept, a probability for classmb = GNUM, and the estimates for
#the other coefficients from the 2-cluster solution
start.2p <- c(gmm2_si_sex$best[1],0,gmm2_si_sex$best[2:13]) 
gmm2_si.2p <- hlme(ETA_Z ~ AGE_CENT+GNUM+AGE_CENT*GNUM, data=factor_data, subject="ID", random = ~1+AGE_CENT, ng = 2, 
                   mixture = ~ AGE_CENT+GNUM, classmb = ~ GNUM,
                   nwg = T, B = start.2p,
                   returndata = TRUE)

sink("../traj_results/soc/SOC Simple 1-step Solution.txt")
summary(gmm2_si.2p)
sink()

flextable(rownames_to_column(as.data.frame(summary(gmm2_si.2p))) %>%
            mutate(`p-value`=ifelse(`p-value`<0.005, "<0.005", round(`p-value`, 3)),
                   rowname=gsub("intercept", "Intercept",
                                gsub("AGE_CENT","Age",
                                     gsub("GNUM","Sex",
                                          gsub("class2","HSSC",
                                               gsub("class1", "LDSC",rowname))))))) %>%
  colformat_double(digits=3) %>%
  set_header_labels("rowname"="Fixed Effect", "coef"="Estimate", "Se"="SE", "p-value"="P-value") %>%
  autofit() %>%
  save_as_docx(path="../traj_results/soc/gmm_estimates_table.docx")

# Output class membership and posterior probs for sample
gmm_predprob <- gmm2_si.2p$pprob
gmm_pred_outcome <- gmm2_si.2p$pred
gmm_data <- gmm2_si.2p$data
gmm_pred_outcome_and_data <- cbind(gmm_data, gmm_pred_outcome)

save(gmm_predprob, file = "../traj_results/soc/gmm_predprob.RData")
save(gmm_pred_outcome, file = "../traj_results/soc/gmm_pred_outcome.RData")
save(gmm_data, file = "../traj_results/soc/gmm_data.RData")
save(gmm_pred_outcome_and_data, file = "../traj_results/soc/gmm_pred_outcome_and_data.RData")

# Output residual plots
jpeg(file="../traj_results/soc/gmm_diagnostics.jpeg", width = 900, height=900)
plot(gmm2_si.2p)
dev.off()

surrogate_data_male <- data.frame(AGE_CENT=seq(0, 40, length=100))
surrogate_data_male$GNUM <- 0
male_pred <- predictY(gmm2_si.2p, newdata=surrogate_data_male, var.time="AGE_CENT", draws=T)

#plot(male_pred)

surrogate_data_female <- data.frame(AGE_CENT=seq(0, 40, length=100))
surrogate_data_female$GNUM <- 1
female_pred <- predictY(gmm2_si.2p, newdata=surrogate_data_female, var.time="AGE_CENT", draws=T)

#plot(male_pred)
#plot(female_pred, add=TRUE)

# Remove CIs
vars_to_convert <- c("Ypred_class1",
                     "Ypred_class2",
                     "lower.Ypred_class1",
                     "lower.Ypred_class2",
                     "upper.Ypred_class1", 
                     "upper.Ypred_class2")

pred_data <- rbind(cbind(female_pred$pred, female_pred$times, "Sex"="Female"),
                   cbind(male_pred$pred, male_pred$times, "Sex"="Male")) %>%
  rownames_to_column(var="ID") %>%
  gather(variable, var_value, vars_to_convert) %>%
  separate(variable,c("variable","class"),sep="_class") %>%
  spread(key=variable, value=var_value) %>%
  mutate(AGE=AGE_CENT+6,
         class_char = ifelse(class=="1", "LDSC", "HSSC"))

ggplot(data=pred_data, mapping=aes(x=AGE, y=Ypred, color=Sex))+
  geom_line(mapping=aes(linetype=class_char), size=1)+
  geom_ribbon(mapping=aes(ymin = lower.Ypred, ymax = upper.Ypred, by=class_char, fill=Sex), 
              size=0.5, alpha=0.25, linetype=0)+
  labs(y="Eta", x="Age (months)", linetype="Class")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  scale_linetype_manual(values=c("solid", "dashed"))+
  theme_bw()+
  theme(text = element_text(size=20))+
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave("../traj_results/soc/mean_trends_class_centered.png", scale=1, limitsize = FALSE, dpi=320)
