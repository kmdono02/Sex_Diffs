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
####### 
#GNUM=0
#######
factor_data <- read_csv(file="../mnlfa_results/rrb_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  filter(GNUM==0)

factor_data <- 
  factor_data %>%
  as.data.frame()

factor_data$ETA_Z <- scale(factor_data$ETA)
factor_data$AGE_CENT <- factor_data$AGE-6

## Simple model (AGE class-level)
#One cluster solution
gmm1_si <- hlme(ETA_Z~AGE_CENT, subject="ID", random=~1+AGE, ng=1, 
                data=factor_data)

#Two clusters
set.seed(12)

gmm2_si <- gridsearch(rep = 500, maxiter = 20, minit = gmm1_si, 
                      hlme(ETA_Z~AGE_CENT, subject="ID", random=~1+AGE,
                           ng = 2, data = factor_data, mixture = ~ AGE_CENT, nwg=T))
#1-step solution
#Starting values for 1-step solution. Need multinomial intercept, a probability for classmb = GNUM, and the estimates for
#the other coefficients from the 2-cluster solution
start.2p <- gmm2_si$best
gmm2_si.2p <- hlme(ETA_Z ~ AGE_CENT, data=factor_data, subject="ID", random = ~1+AGE_CENT, ng = 2, 
                   mixture = ~ AGE_CENT,
                   nwg = T, B = start.2p, returndata = TRUE)

#Output class membership and posterior probs for sample
gmm_predprob <- gmm2_si.2p$pprob
gmm_data <- gmm2_si.2p$data
gmm_pred_outcome <- gmm2_si.2p$pred
gmm_pred_outcome_and_data <- cbind(gmm_data, gmm_pred_outcome)

save(gmm_predprob, file = "../traj_results/rrb/gnum_0/gmm_predprob.RData")
save(gmm_pred_outcome, file = "../traj_results/rrb/gnum_0/gmm_pred_outcome.RData")
save(gmm_data, file = "../traj_results/rrb/gnum_0/gmm_data.RData")
save(gmm_pred_outcome_and_data, file = "../traj_results/rrb/gnum_0/gmm_pred_outcome_and_data.RData")

surrogate_data <- data.frame(AGE_CENT=seq(0, 40, length=100), GNUM=0)
y_pred_gnum0 <- predictY(gmm2_si.2p, newdata=surrogate_data, var.time="AGE_CENT", draws=T)

rm(list=setdiff(ls(), "y_pred_gnum0"))

####### 
#GNUM=1
#######
factor_data <- read_csv(file="../mnlfa_results/rrb_files/factor_scores_wdx.csv", na=c("*","","NA","N/A","--")) %>%
  filter(GNUM==1)

factor_data <- 
  factor_data %>%
  as.data.frame()


factor_data$ETA_Z <- scale(factor_data$ETA)
factor_data$AGE_CENT <- factor_data$AGE-6


## Simple model (AGE class-level)
#One cluster solution
gmm1_si <- hlme(ETA_Z~AGE_CENT, subject="ID", random=~1+AGE_CENT, ng=1, 
                data=factor_data)

#Two clusters
set.seed(12)

gmm2_si <- gridsearch(rep = 500, maxiter = 20, minit = gmm1_si, 
                      hlme(ETA_Z~AGE_CENT, subject="ID", random=~1+AGE_CENT,
                           ng = 2, data = factor_data, mixture = ~ AGE_CENT, nwg=T))

#1-step solution
#Starting values for 1-step solution. Need multinomial intercept, a probability for classmb = GNUM, and the estimates for
#the other coefficients from the 2-cluster solution
start.2p <- gmm2_si$best
gmm2_si.2p <- hlme(ETA_Z ~ AGE_CENT, data=factor_data, subject="ID", random = ~1+AGE_CENT, ng = 2, 
                   mixture = ~ AGE_CENT,
                   nwg = T, B = start.2p, returndata = TRUE)

#Output class membership and posterior probs for sample
gmm_predprob <- gmm2_si.2p$pprob
gmm_data <- gmm2_si.2p$data
gmm_pred_outcome <- gmm2_si.2p$pred
gmm_pred_outcome_and_data <- cbind(gmm_data, gmm_pred_outcome)

save(gmm_predprob, file = "../traj_results/rrb/gnum_1/gmm_predprob.RData")
save(gmm_pred_outcome, file = "../traj_results/rrb/gnum_1/gmm_pred_outcome.RData")
save(gmm_data, file = "../traj_results/rrb/gnum_1/gmm_data.RData")
save(gmm_pred_outcome_and_data, file = "../traj_results/rrb/gnum_1/gmm_pred_outcome_and_data.RData")

surrogate_data <- data.frame(AGE_CENT=seq(0, 40, length=100), GNUM=1)
y_pred_gnum1 <- predictY(gmm2_si.2p, newdata=surrogate_data, var.time="AGE_CENT", draws=T)

# Remove CIs
vars_to_convert <- c("Ypred_class1",
                     "Ypred_class2",
                     "lower.Ypred_class1",
                     "lower.Ypred_class2",
                     "upper.Ypred_class1", 
                     "upper.Ypred_class2")

pred_data <- rbind(cbind(y_pred_gnum0$pred, y_pred_gnum0$times, GNUM=0),
                   cbind(y_pred_gnum1$pred, y_pred_gnum1$times, GNUM=1)) %>%
  rownames_to_column(var="ID") %>%
  gather(variable, var_value, vars_to_convert) %>%
  separate(variable,c("variable","class"),sep="_class") %>%
  spread(key=variable, value=var_value) %>%
  mutate(GNUM=factor(GNUM))

# ggplot(data=pred_data, mapping=aes(x=AGE, y=Ypred, color=GNUM))+
#   geom_line(mapping=aes(linetype=class), size=0.5)+
#   geom_ribbon(mapping=aes(ymin = lower.Ypred, ymax = upper.Ypred, by=class, fill=GNUM), 
#               size=0.5, alpha=0.25, linetype=0)+
#   labs(y="Eta", x="Age (months)", linetype="Class")+
#   scale_color_manual(values=c("#E69F00", "#56B4E9"))+
#   scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
#   scale_linetype_manual(values=c("solid", "dashed"))+
#   theme_bw()+
#   theme(text = element_text(size=20))
# ggsave("../traj_results/rrb/mean_trends_class.png", scale=2, limitsize = FALSE)


#Reset here
rm(list=ls())