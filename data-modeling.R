# data modeling for space privacy experiment
# exploring bayesian techniques

# author: Mich Lin (shuyulin)
# date created: JUL 13 2026
# date last modified: SEP 25 2026


##### housekeeping #####
# packages needed: ggplot2, corrr, ggcorrplot, FactoMineR, factoextra
# load libraries
library("readxl")
library(ggplot2)
library(tibble)
library(dplyr)
library("ggpubr")
library(reticulate)
library(nlme)
library(lme4)
library(tidyr)
library(magrittr)
library('corrr')
library('ggcorrplot')
library('FactoMineR')
library('factoextra')
library(glmmTMB)
library(glmnet)
library(brms)
library(ggridges)
library(ggradar)
library(beeswarm)
library(eRm)
library(TAM)
library(bayesplot)

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

###### data import ###### 
setwd("/Users/michellelin/MIT Dropbox/Mich Lin/Research/space_architecture/tech_dev/experiment/data")
load("data-formodeling.RData")

##### data prep - leveling/factorization #####
# convert invasiveness to ordered factor
achieved_privacy <- achieved_privacy %>% mutate(invasiveness = factor(as.numeric(invasiveness) + 1, 
                                                                      levels = 1:7, ordered = TRUE),
                                                privacy_loss = factor(as.numeric(privacy_loss) + 7, 
                                                                      levels = 1:13, ordered = TRUE),
                                                awareness = factor(as.numeric(awareness) + 1, 
                                                                   levels = 1:7, ordered = TRUE),
                                                worry_see = factor(as.numeric(worry_see) + 1, 
                                                                   levels = 1:7, ordered = TRUE),
                                                worry_hear = factor(as.numeric(worry_hear) + 1, 
                                                                    levels = 1:7, ordered = TRUE),
                                                comfort = factor(as.numeric(comfort) + 1, 
                                                                 levels = 1:7, ordered = TRUE),
                                                personal_space = factor(as.numeric(personal_space) + 1, 
                                                                        levels = 1:7, ordered = TRUE),
                                                energy = factor(as.numeric(energy) + 1, 
                                                                levels = 1:7, ordered = TRUE),
                                                sightlines = factor(as.numeric(sightlines) + 1, 
                                                                    levels = 1:7, ordered = TRUE),
                                                notice = factor(as.numeric(notice) + 1, 
                                                                levels = 1:7, ordered = TRUE))

# convert partition to level 
experimental_data <- experimental_data %>% mutate(partition = factor(as.numeric(partition)+1, 
                                                                     levels = 1:4, ordered = TRUE))
# combine dataframes
data <- full_join(experimental_data, achieved_privacy, by = c("PID", "task_num"))%>% select(!note)

# label tasks by invasiveness
task_invasion <- data.frame(task = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                                     "demographics","NFP introspective","NFP behavioral","IPIP",
                                     "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes"), 
                            lvl = as.factor(c("L","L","L","L","L","L","M","M","M","M","H","H","H","H","H","H")))
task_invasion$lvl <- factor(task_invasion$lvl, levels = c("L","M","H"), ordered = TRUE)

data <- left_join(data, task_invasion, by = "task")

##### histograms - relevant visualizations #####
# skewed
hist(data$privacy_loss %>% as.numeric(), 
     xlab = "too much privacy                       not enough privacy", 
     main = "Privacy loss") # not normal; bimodal
hist(as.numeric(data$invasiveness),
     xlab = "none at all                                                  a lot", 
     main = "Invasiveness to privacy")
hist(as.numeric(data$awareness),
     xlab = "not at all                 moderately                 extremely", 
     main = "I am aware of the external environment.")
hist(as.numeric(data$worry_see),
     xlab = "not at all                 moderately                 extremely", 
     main = "I am worried if others can see me.")
hist(as.numeric(data$worry_hear),
     xlab = "not at all                 moderately                 extremely", 
     main = "I am worried if others can hear me.")
hist(as.numeric(data$comfort),
     xlab = "not at all                 moderately                 extremely", 
     main = "I feel comfortable with how much others\n  can see me.") # right skewed
hist(as.numeric(data$personal_space),
     xlab = "not at all                 moderately                 extremely", 
     main = "I have enough personal space.") # right skewed
hist(as.numeric(data$energy),
     xlab = "not at all                 moderately                 extremely", 
     main = "I am putting energy into monitoring\n the external environment.")
hist(as.numeric(data$sightlines),
     xlab = "not at all                 moderately                 extremely", 
     main = "I am out of the sightlines of other people.") # right skewed
hist(as.numeric(data$notice),
     xlab = "not at all                 moderately                 extremely", 
     main = "I notice myself glancing outside of my\n personal space for other people.")

##### plotting grouped by relevant disaggregators #####
# grouping invasiveness by sex
ggplot(left_join(data, demographics %>% select(PID, Sex_M), by = "PID")) + 
  geom_boxplot(aes(invasiveness%>% as.numeric(), task, fill = as.factor(Sex_M))) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) +
  # facet_wrap(~Sex_M) +
  scale_fill_discrete(name = "Sex", label = c("F","M")) +
  theme_bw()

# grouping privacy loss by sex
ggplot(left_join(data, demographics %>% select(PID, Sex_M), by = "PID")) + 
  geom_boxplot(aes(privacy_loss%>% as.numeric(), task, fill = as.factor(Sex_M))) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) +
  # facet_wrap(~Sex_M) +
  scale_fill_discrete(name = "Sex", label = c("F","M")) +
  theme_bw()

# grouping invasiveness by student
ggplot(left_join(data, demographics %>% select(PID, Employ_Student), by = "PID")) + 
  geom_boxplot(aes(invasiveness%>% as.numeric(), task, fill = as.factor(Employ_Student))) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) +
  scale_fill_discrete(name = "Student", label = c("Y","N")) +
  theme_bw()

# grouping privacy loss by student
ggplot(left_join(data, demographics %>% select(PID, Employ_Student), by = "PID")) + 
  geom_boxplot(aes(privacy_loss%>% as.numeric(), task, fill = as.factor(Employ_Student))) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) +
  scale_fill_discrete(name = "Student", label = c("Y","N")) +
  theme_bw()


##### RQ1 - mediator model of perceived invasion #####
# prior specification
priors_RQ1 <- c(
  # weakly informative for regression coefficients
  prior(normal(0, 2), class = "b"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "sd")
)

# X = level of invasion
# M = perceived privacy
# Y = privacy loss

lvl_invasiveness <- brm(
  invasiveness ~ lvl + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ1,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(lvl_invasiveness)
pp_check(lvl_invasiveness)
prior_summary(lvl_invasiveness)
# plotting conditional effects of probabilities
plot(conditional_effects(lvl_invasiveness, "lvl", 
                    categorical = TRUE), plot = FALSE)[[1]] + 
  theme_minimal() + 
  scale_x_discrete(name = "Level of invasion", 
                   labels = c("low","medium","high")) +
  scale_color_discrete(labels = seq(0,6)) +
  scale_fill_discrete(labels = seq(0,6))


invasiveness_mediation <- brm(
  privacy_loss ~ lvl + invasiveness + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ1,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(invasiveness_mediation)
pp_check(invasiveness_mediation)
# plotting
plot(conditional_effects(invasiveness_mediation, "invasiveness",
                         categorical = TRUE), plot = FALSE)[[1]] +
  theme_minimal() +
  # scale_x_discrete(name = "Task level of invasion")+
  scale_x_discrete(name = "Invasiveness", labels = seq(0,6))+
  scale_color_discrete(labels = seq(-6,6)) +
  scale_fill_discrete(labels = seq(-6,6))

# modeling by task so we can plot marginal est by task too
task_mediation <- brm(
  privacy_loss ~ task + lvl + invasiveness + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ1,
  cores = 4,
  chains = 4,
  seed = 1
)
# the task plot is weird and not matching w data? 
plot(conditional_effects(task_mediation, "lvl",
                         categorical = TRUE), plot = FALSE)[[1]] +
  theme_minimal() +
  # scale_x_discrete(name = "Task")+
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(name = "Task level",
                   labels = c("low","medium","high"))+
  # scale_x_discrete(name = "Invasiveness", labels = seq(0,6))+
  scale_color_discrete(labels = seq(-6,6)) +
  scale_fill_discrete(labels = seq(-6,6)) 

# Extract posterior draws
draws_m1 <- as_draws_df(lvl_invasiveness)
draws_m2 <- as_draws_df(invasiveness_mediation)

# For ordinal mediation, use linear contrasts
# a-path: lvl.L from model1
a_path <- draws_m1$b_lvl.L

# b-path: invasiveness.L from model2
b_path <- draws_m2$b_invasiveness.L

# Indirect effect of the total effect of lvl of invasion on privacy loss
indirect <- a_path * b_path

posterior_summary(indirect)
# estimate = 16.1, much more substantial than the direct effect [lvl->priv loss] (0.57), so we can 
# conclude that perceived invasion was an appropriate mediation variable

# https://r-statistics.co/Bayesian-Logistic-Regression-in-R.html

##### RQ2 - does privacy loss lead to affective declines? ##### 
# standardizing predictor variable for convergence to group mean
data <- data %>% mutate(privacy_loss_std = scale(as.numeric(privacy_loss))[,1])
# standardizing this to individual means
data <- data %>% group_by(PID) %>% mutate(privacy_loss_std_indiv = 
                                            scale(as.numeric(privacy_loss))[,1])

# set priors for multivariate model
priors_RQ2 <- c(
  # weakly informative for regression coefficients
  prior(normal(0, 2), class = "b"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "sd", resp = "awareness"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "worrysee"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "worryhear"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "comfort"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "personalspace"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "energy"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "sightlines"),
  prior(student_t(3, 0, 2.5), class = "sd", resp = "notice")
)

# predicting privacy loss from all affective outcomes
affect <- brm(
  mvbind(awareness, worry_see, worry_hear, comfort, personal_space, 
         energy, sightlines, notice) ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ2,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(affect)
pp_check(affect, resp = "awareness")
pp_check(affect, resp = "worrysee")
pp_check(affect, resp = "worryhear")
pp_check(affect, resp = "comfort")
pp_check(affect, resp = "personalspace")
pp_check(affect, resp = "energy")
pp_check(affect, resp = "sightlines")
pp_check(affect, resp = "notice")
conditional_effects(affect, "privacy_loss_std", categorial = FALSE)

plot(conditional_effects(affect, "privacy_loss_std", categorial = TRUE), 
     plot = FALSE)[[8]] + # iterate through 1-8 to see all the plots
  theme_minimal() +
  scale_y_continuous(name = "",
                     limits = c(1,7), 
                     breaks = seq(1,7), 
                     labels = seq(0,6))+
  scale_x_continuous(name = "Privacy loss (standardized)")

# look at this for individual means
affect_indiv <- brm(
  mvbind(awareness, worry_see, worry_hear, comfort, personal_space, 
         energy, sightlines, notice) ~ privacy_loss_std_indiv + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ2,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(affect_indiv)
conditional_effects(affect, "privacy_loss_std", categorial = FALSE)

plot(conditional_effects(affect_indiv, "privacy_loss_std_indiv", categorial = TRUE), 
     plot = FALSE)[[8]] + # iterate through 1-8 to see all the plots
  theme_minimal() +
  scale_y_continuous(name = "",
                     limits = c(1,7), 
                     breaks = seq(1,7), 
                     labels = seq(0,6))+
  scale_x_continuous(name = "Privacy loss (standardized)")

# now include individually varying slopes
affect_slope <- brm(
  mvbind(awareness, worry_see, worry_hear, comfort, personal_space, 
         energy, sightlines, notice) ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ2,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(affect_slope) # converged! 
pp_check(affect_slope, resp = "awareness")
pp_check(affect_slope, resp = "worrysee")
pp_check(affect_slope, resp = "worryhear")
pp_check(affect_slope, resp = "comfort")
pp_check(affect_slope, resp = "personalspace")
pp_check(affect_slope, resp = "energy")
pp_check(affect_slope, resp = "sightlines")
pp_check(affect_slope, resp = "notice")
# plot (344,281)
plot(conditional_effects(affect_slope, "privacy_loss_std", categorial = TRUE), 
     plot = FALSE)[[8]] + # iterate through 1-8 to see all the plots
  theme_minimal() +
  scale_y_continuous(name = "",
                     limits = c(1,7), 
                     breaks = seq(1,7), 
                     labels = seq(0,6))+
  scale_x_continuous(name = "Privacy loss (standardized)")

##### ARCHIVED RQ2 - modeling each affective outcome separately #####
# predicting the impact of privacy loss on individual affective outcomes
awareness <- brm(
  awareness ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(awareness)
pp_check(awareness)

worry_see <- brm(
  worry_see ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(worry_see)
pp_check(worry_see)

worry_hear <- brm(
  worry_hear ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(worry_hear)
pp_check(worry_hear)

comfort <- brm(
  comfort ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(comfort)
pp_check(comfort)

personal_space <- brm(
  personal_space ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(personal_space)
pp_check(personal_space)

energy <- brm(
  energy ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(energy)
pp_check(energy)

sightlines <- brm(
  sightlines ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(sightlines)
pp_check(sightlines)

notice <- brm(
  notice ~ privacy_loss_std + (1 | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(notice)
pp_check(notice)

# predicting the impact of privacy loss on affective outcomes
# i think we have to run individualized slope models on all of these, can use
# all had non-zero CIs; suggests random slopes improve the model
awareness_a <- brm(
  awareness ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(awareness_a)
pp_check(awareness_a)

worry_see_a <- brm(
  worry_see ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(worry_see_a)
pp_check(worry_see_a)

worry_hear_a <- brm(
  worry_hear ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(worry_hear_a)
pp_check(worry_hear_a)

comfort_a <- brm(
  comfort ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(comfort_a)
pp_check(comfort_a)

personal_space_a <- brm(
  personal_space ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(personal_space_a)
pp_check(personal_space_a)

energy_a <- brm(
  energy ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(energy_a)
pp_check(energy_a)

sightlines_a <- brm(
  sightlines ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(sightlines_a)
pp_check(sightlines_a)

notice_a <- brm(
  notice ~ privacy_loss_std + (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data,
  cores = 4,
  chains = 4
)
summary(notice_a)
pp_check(notice_a)

##### RQ3 - moderation of environmental condition on privacy loss #####
# set priors 
priors_RQ3 <- c(
  # weakly informative for regression coefficients
  prior(normal(0, 2), class = "b"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  
  # this is what brms defaults to
  prior(student_t(3, 0, 2.5), class = "sd")
)
moderation_environment <- brm(
  privacy_loss ~ lvl + invasiveness + partition + (1 | PID),
  family = cumulative("logit"),
  data = data,
  prior = priors_RQ3,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(moderation_environment)
pp_check(moderation_environment)
# plotting
plot(conditional_effects(moderation_environment, "partition", 
                         categorical = TRUE), plot = FALSE)[[1]] + 
  theme_minimal() + 
  scale_x_discrete(name = "Covering",
                   labels = c("none","open","medium","full")) +
  scale_color_discrete(labels = seq(-6,6)) +
  scale_fill_discrete(labels = seq(-6,6))

# plotting posteriors
mcmc_areas(as.matrix(moderation_environment),
           pars = c("b_lvl.L", 
                    "b_invasiveness.L", 
                    "b_partition.L"),
           prob = 0.8)

# we can try a model where partition is a binary thing between 0/1 and 2/3
data <- data %>% mutate(
  condition_env = case_when(partition == 1 ~ as.factor(0),
                            partition == 2 ~ as.factor(1),
                            partition == 3 ~ as.factor(1),
                            partition == 4 ~ as.factor(1))
) 

# interaction effects
moderation_environment_int <- brm(
  privacy_loss ~ lvl + invasiveness*condition_env + (1 | PID),
  family = cumulative("logit"),
  data = data,
  iter = 4000,
  # warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(moderation_environment_int)
pp_check(moderation_environment_int)
# plotting
plot(conditional_effects(moderation_environment_int, "condition_env", 
                         categorical = TRUE), plot = FALSE)[[1]] + 
  theme_minimal() + 
  scale_x_discrete(name = "",
                   labels = c("no covering","with covering")) +
  scale_color_discrete(labels = seq(-6,6)) +
  scale_fill_discrete(labels = seq(-6,6))

# plotting posteriors
mcmc_areas(as.matrix(moderation_environment_int),
           pars = c("b_lvl.L", 
                    "b_invasiveness.L", 
                    "b_condition_env",
                    "b_invasiveness.L:condition_env",
                    "b_invasiveness.Q:condition_env"),
           prob = 0.8)

# try another model where baseline and passive are lumped (no control)
# performs a bit better than the other model! more clear + converges with interaction
data <- data %>% mutate(
      condition_ctrl = case_when(condition == "active" ~ as.factor(1),
                                 .default = as.factor(0))
   ) 
# reference the levels to 1 so we can see the effect of control
data$condition_ctrl <- factor(data$condition_ctrl, levels = c(0,1))

moderation_control_int <- brm(
  privacy_loss ~ lvl + invasiveness*condition_ctrl + (1 | PID),
  family = cumulative("logit"),
  data = data,
  iter = 4000,
  # warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(moderation_control_int)
pp_check(moderation_control_int)
# plotting
plot(conditional_effects(moderation_control_int, "condition_ctrl",
                         categorical = TRUE), plot = FALSE)[[1]] + 
  theme_minimal() + 
  scale_x_discrete(name = "",
                   labels = c("no control","with control")) +
  scale_color_discrete(labels = seq(-6,6)) +
  scale_fill_discrete(labels = seq(-6,6))

# plotting interaction effect
plot(conditional_effects(moderation_control_int, "invasiveness:condition_ctrl"), plot = FALSE)[[1]] + 
  theme_minimal() + 
  scale_x_discrete(name = "invasiveness",
                   labels = seq(0,6)) +
  scale_y_continuous(name = "privacy loss",
                   breaks = seq(1,13,2),
                   labels = seq(-6,6,2)) +
  scale_color_manual(name = "", 
                     values = c("grey", "red"),
                     breaks = c(0,1),
                     labels = c("no control","with control")) +
  scale_fill_manual(name = "", 
                    values = c("grey", "red"),
                    breaks = c(0,1),
                    labels = c("no control","with control"))

# plotting posteriors
mcmc_areas(as.matrix(moderation_control_int),
           pars = c("b_lvl.L", "b_invasiveness.L", "b_condition_ctrl",
                    "b_invasiveness.L:condition_ctrl",
                    "b_invasiveness.Q:condition_ctrl"),
           prob = 0.8)

##### RQ4 - protective individual mechanisms against privacy loss #####
# include trait predictors in dataframe
# IPIP, nfp-beh (nfp_scores), nfp-int
data_indivtraits <- data %>% left_join(IPIP %>% select(PID, Neuroticism, Extraversion,
                                     Openness, Agreeableness, 
                                     Conscientiousness), by = "PID") %>%
                             left_join(nfp_scores, by = "PID") %>%
                             left_join(NFP_int %>% mutate(
                               nfp_inf = (int_1+int_2+int_3+int_4)/4,
                               nfp_psych = (int_5+int_6+int_7+int_8)/4,
                               nfp_phys = (int_9+int_10+int_11+int_12)/4
                             ) %>%
                               select(PID, nfp_inf, nfp_psych, nfp_phys), by = "PID") # averaging to subconstruct

# privacy loss as predicted by env stressors and individual traits
privloss_mediation_indivtraits <- brm(
  privacy_loss ~ lvl + invasiveness + 
    Neuroticism + Extraversion + Openness +
    Agreeableness + Conscientiousness + # IPIP
    nfp_theta + # nfp behavioral
    nfp_inf + nfp_psych + nfp_phys + # nfp introspective
    (1 | PID),
  family = cumulative("logit"),
  data = data_indivtraits,
  cores = 4,
  chains = 4,
  seed = 1
) # converged
summary(privloss_mediation_indivtraits)
pp_check(privloss_mediation_indivtraits)


# privacy loss as predicted by env stressors and individual traits
invasiveness_mediation_indivtraits <- brm(
  invasiveness ~ lvl + 
    Neuroticism + Extraversion + Openness +
    Agreeableness + Conscientiousness + # IPIP
    nfp_theta + # nfp behavioral
    nfp_inf + nfp_psych + nfp_phys + # nfp introspective
    (1 | PID),
  family = cumulative("logit"),
  data = data_indivtraits,
  cores = 4,
  chains = 4,
  seed = 1
) # converged
summary(invasiveness_mediation_indivtraits)
pp_check(invasiveness_mediation_indivtraits)

# how individual traits influence affective outcomes; maybe not as relevant
affect_slope_indivtraits <- brm(
  mvbind(awareness, worry_see, worry_hear, comfort, personal_space, 
         energy, sightlines, notice) ~ privacy_loss_std + 
    Neuroticism + Extraversion + Openness +
    Agreeableness + Conscientiousness + # IPIP
    nfp_theta + # nfp behavioral
    nfp_inf + nfp_psych + nfp_phys + # nfp introspective
    (1 + privacy_loss_std | PID),
  family = cumulative("logit"),
  data = data_indivtraits,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  seed = 1
)
summary(affect_slope_indivtraits)

save.image(file = "modeldata.RData")
