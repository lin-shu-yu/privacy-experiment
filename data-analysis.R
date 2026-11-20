# data analysis for space privacy experiment

# author: Mich Lin (shuyulin)
# date created: MAY 7 2026
# date last modified: SEP 23 2026


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
library(rstatix)
library(psych)
library(BSDA)
library(emmeans)
library(forcats)

# set color palette for ggplot (colorblind accessible)
coolors <- c("#ef6f6c","#5386e4","#a3a500","#1b998b",
             "#88596f","#A36500")

###### data import ###### 
setwd("/Users/michellelin/MIT Dropbox/Mich Lin/Research/space_architecture/tech_dev/experiment/data")
load("data.RData")

##### task completion metrics ##### 
mean(data$time %>% as.numeric()) %>% as.period()
median(data$time %>% as.numeric()) %>% as.period()
quantile(data$time %>% as.numeric()) %>% as.period()

quantile(data %>% subset(task == "LEGO") %>% .$time %>% as.numeric()) %>% as.period() %>% hms::hms()

##### checking for normality/variances #####
# checking for HERA vs study IPIP MANOVA test 
  # reference: https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/manova/
IPIP_combined <- rbind(HERA_IPIP %>% 
                         # C4 selection was not as strict, so excluding
                         subset(Campaign != "4") %>% # but comment out if including C4
                         select(Neuroticism, Extraversion,
                                Openness, Agreeableness,
                                Conscientiousness) %>% "*"(5/7) %>% 
                         mutate(population = "HERA"),
                       IPIP %>% select(Neuroticism, Extraversion,
                                       Openness, Agreeableness,
                                       Conscientiousness) %>% 
                         mutate(population = "study")
) 
# Independent Random Sampling: PASS
# similar group sizes: study(30) vs HERA(39 without C4, incl backups)
  # needs to be within 1.5x 
# Level and Measurement of the Variables: PASS, scale variables
# Absence of multicollinearity: PASS, nothing above 0.9
  # HERA - below moderate correlation
  ggcorrplot(cor(IPIP_combined %>% subset(population == "HERA") %>%
                   select(!population)), hc.order = TRUE, type = "lower",
           lab = TRUE) + ggtitle("HERA C5+C6 (N=39)")
  # study - below moderate correlation 
  ggcorrplot(cor(IPIP_combined %>% subset(population == "study") %>%
                   select(!population)), hc.order = TRUE, type = "lower",
             lab = TRUE) + ggtitle("study (N=30)")
# Normality: PASS mostly, some left skew in neuroticism-study
  # MANOVA is fairly robust to violations of normality assumptions when N balanced
  # reference: https://rstudio-pubs-static.s3.amazonaws.com/694324_35fdeed13d4b4b319bf7cdc668bce5a2.html
    # visual checks with histograms, save as (600,500)
      # HERA
      pivot_longer(IPIP_combined %>% subset(population == "HERA") %>%
                   select(!population), cols = everything(), 
                   names_to = "construct", values_to = "value") %>%
        ggplot(., aes(x = value, fill = construct)) +
        geom_histogram(color = "white", bins = 8, show.legend = FALSE) +
        facet_wrap(~ construct, scales = "free") + # "free" allows each plot to have its own x/y scale limits
        scale_fill_manual(values = coolors)+
        theme_minimal() +
        ggtitle("HERA C5+C6 (N=39)")
      # study
      pivot_longer(IPIP_combined %>% subset(population == "study") %>%
                     select(!population), cols = everything(), 
                   names_to = "construct", values_to = "value") %>%
        ggplot(., aes(x = value, fill = construct)) +
        geom_histogram(color = "white", bins = 8, show.legend = FALSE) +
        facet_wrap(~ construct, scales = "free") + # "free" allows each plot to have its own x/y scale limits
        scale_fill_manual(values = coolors)+ 
        theme_minimal() +
        ggtitle("study (N=30)")
    # visual checks with qqplots
      # HERA
      pivot_longer(IPIP_combined %>% subset(population == "HERA") %>%
                     select(!population), cols = everything(), 
                   names_to = "construct", values_to = "value") %>%
      ggqqplot(.,
               x = "value",
               color = "construct",
               legend = "right"
      ) + 
        scale_color_manual(values = coolors)+ 
      ggtitle("HERA C5+C6 (N=39)")
      # study
      pivot_longer(IPIP_combined %>% subset(population == "study") %>%
                     select(!population), cols = everything(), 
                   names_to = "construct", values_to = "value") %>%
        ggqqplot(.,
                 x = "value",
                 color = "construct",
                 legend = "right"
        ) + 
        scale_color_manual(values = coolors)+ 
        ggtitle("study (N=30)")
    # shapiro-wilk tests
      # HERA
      shapiro.test(IPIP_combined %>% subset(population == "HERA") %>%
                     .$Agreeableness) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "HERA") %>%
                     .$Conscientiousness) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "HERA") %>%
                     .$Extraversion) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "HERA") %>%
                     .$Neuroticism) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "HERA") %>%
                     .$Openness) # FAIL
      # study
      shapiro.test(IPIP_combined %>% subset(population == "study") %>%
                     .$Agreeableness) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "study") %>%
                     .$Conscientiousness) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "study") %>%
                     .$Extraversion) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "study") %>%
                     .$Neuroticism) # PASS
      shapiro.test(IPIP_combined %>% subset(population == "study") %>%
                     .$Openness) # PASS
# Homogeneity of Variance: Variance between groups is equal.
box_m(data = IPIP_combined %>% select(!population), 
      group = IPIP_combined$population) # FAIL (p < 0.001)
# use MANOVA, but report Pillai's trace as the robust statistic

##### demographics #####
# counts
demographics %>% count(Sex_M == 1)
demographics %>% count(Sex_F == 1)
demographics %>% count(Sex_O == 1)

demographics %>% count(Gender_Cis == 1 & Gender_M == 1)
demographics %>% count(Gender_M == 1)
demographics %>% count(Gender_Cis == 1 & Gender_W == 1)
demographics %>% count(Gender_W == 1)

demographics %>% count(Hispanic_Y == 1)

demographics %>% count(Race_W == 1)
demographics %>% count(Race_B == 1)
demographics %>% count(Race_N == 1)
demographics %>% count(Race_A == 1)
demographics %>% count(Race_HI == 1)

demographics %>% count(Edu_B == 1)
demographics %>% count(Edu_M == 1)
demographics %>% count(Edu_PhD == 1)
demographics %>% count(Edu_MD == 1)

# demographics
demographics %>% subset(Sex_M == 1) %>% 
  select(PID, starts_with('Gender'))

demographics %>% subset(Sex_F == 1) %>% 
  select(PID, starts_with('Gender'))

demographics %>% subset(Sex_O == 1) %>% 
  select(PID, starts_with('Gender'))

median(age_PID$Age)
mean(age_PID$Age)
sd(age_PID$Age)
max(age_PID$Age)
min(age_PID$Age)

demographics %>% subset(Employ_Student == 1) %>% count()

# demographics plots
ggplot(demographics %>% select(PID, starts_with("Sex")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+ 
  scale_fill_discrete(name = "Sex", labels = c("Female","Male","Other")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

demographics %>% select(PID, starts_with("Sex")) %>% pivot_longer(cols = !PID) %>% 
  subset(value == 1) %>% group_by(name) %>% count()

ggplot(demographics %>% select(PID, starts_with("Gender")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Gender", breaks = c("Gender_Cis","Gender_M","Gender_W","Gender_PND"),
                      labels = c("Cis","Man","Woman","Prefer not to disclose")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

demographics %>% select(PID, starts_with("Gender")) %>% pivot_longer(cols = !PID) %>% 
  subset(value == 1) %>% group_by(name) %>% count()

ggplot(demographics %>% select(PID, starts_with("Hispanic")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Hispanic", labels = c("N","Y")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

ggplot(demographics %>% select(PID, starts_with("Race")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Race", labels = c("Asian","Black","Native","Other","White")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

ggplot(demographics %>% select(PID, starts_with("Edu")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Education", labels = c("Bachelor's","Master's",
                                                "MD","PhD")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

ggplot(demographics %>% select(PID, starts_with("Course")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Course") +
  # ggtitle("Participant breakdown by...") +
  theme_void()

ggplot(demographics %>% select(PID, starts_with("Military")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Military/Veteran", labels = c("N", "Y")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

ggplot(demographics %>% select(PID, starts_with("Employ")) %>% pivot_longer(cols = !PID) %>% subset(value == 1),
       aes(x = "", fill = name)) +
  geom_bar(width = 1, color = "white") + # Creates a stacked bar chart
  coord_polar(theta = "y")+
  scale_fill_discrete(name = "Student?", labels = c("N", "Y")) +
  # ggtitle("Participant breakdown by...") +
  theme_void()

# IPIP comparison
ggplot(data = HERA_IPIP %>% select(PID, Neuroticism, Extraversion,
                              Openness, Agreeableness,
                              Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct")) + 
  geom_violin(aes(value*5/7, construct), alpha = 0.5, draw_quantiles = c(0.5)) +
  geom_point(aes(value*5/7, construct), size = 1.5, alpha = 0.6, 
             position = position_jitter(seed = 1, width = 0.01)) + 
  scale_x_discrete(name = "score", limits = c(1:5)) + 
  labs(title = "IPIP-NEO-120 (Johnson 2014) HERA summary") +
  theme_bw()

# HERA summary, excluding C4
ggplot(data = HERA_IPIP %>% subset(Campaign != "4") %>%
         select(PID, Neuroticism, Extraversion,
                                   Openness, Agreeableness,
                                   Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct")) + 
  geom_violin(aes(value*5/7, construct), alpha = 0.5, draw_quantiles = c(0.5)) +
  geom_point(aes(value*5/7, construct), size = 1.5, alpha = 0.6, 
             position = position_jitter(seed = 1, width = 0.01)) + 
  scale_x_discrete(name = "score", limits = c(1:5)) + 
  scale_y_discrete(limits = c("Openness","Neuroticism",
                              "Extraversion", "Conscientiousness","Agreeableness"))+
  labs(title = "IPIP-NEO-120 (Johnson 2014) HERA summary") +
  theme_minimal()

# study summary
ggplot(data = IPIP %>% select(PID, Neuroticism, Extraversion,
                              Openness, Agreeableness,
                              Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct")) + 
  geom_violin(aes(value, construct), alpha = 0.5, color = "blue", , draw_quantiles = c(0.5)) +
  geom_point(aes(value, construct), size = 1.5, color = "blue", alpha = 0.6, 
             position = position_jitter(seed = 1, width = 0.01)) + 
  scale_x_discrete(name = "score", limits = c(1:5)) + 
  scale_y_discrete(limits = c("Openness","Neuroticism",
                              "Extraversion", "Conscientiousness","Agreeableness"))+  labs(title = "IPIP-NEO-120 (Johnson 2014) study summary") +
  theme_minimal()

# comparison between HERA and study - save (700,400)
ggplot() + 
  # HERA population
  geom_violin(data = HERA_IPIP %>% 
                subset(Campaign != "4") %>%
                select(PID, Neuroticism, Extraversion,
                                     Openness, Agreeableness,
                                     Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct"),
              aes(value*5/7, construct), alpha = 0.3, , draw_quantiles = c(0.5)) +
  geom_point(data = HERA_IPIP %>% 
               subset(Campaign != "4") %>%
               select(PID, Neuroticism, Extraversion,
                                    Openness, Agreeableness,
                                    Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct"),
             aes(value*5/7, construct), size = 1.5, alpha = 0.3, 
             position = position_jitter(seed = 1, width = 0.01)) + 
  # study sample
  geom_violin(data = IPIP %>% select(PID, Neuroticism, Extraversion,
                                     Openness, Agreeableness,
                                     Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct"),
              aes(value, construct), alpha = 0.5, color = "blue", , draw_quantiles = c(0.5)) +
  geom_point(data = IPIP %>% select(PID, Neuroticism, Extraversion,
                                    Openness, Agreeableness,
                                    Conscientiousness) %>% pivot_longer(cols = !PID, names_to = "construct"),
             aes(value, construct), size = 1.5, color = "blue", alpha = 0.6, 
             position = position_jitter(seed = 1, width = 0.01)) + 
  # plot aesthetics
  scale_x_discrete(name = "score", limits = c(1:5)) + 
  scale_y_discrete(limits = c("Openness","Neuroticism",
                              "Extraversion", "Conscientiousness","Agreeableness"))+
  labs(title = "IPIP-NEO-120 (Johnson 2014) HERA vs. study (blue) summary") +
  # labs(title = "Spaceflight analog vs. study (blue) IPIP summary") + # for paper
  theme_minimal()

# comparing study to HERA population
describe(IPIP_combined %>% subset(population == "study"))
describe(IPIP_combined %>% subset(population == "HERA"))
# from prechecks, use MANOVA, but report Pillai's trace as the robust statistic
IPIP_manova <- manova(data = IPIP_combined, cbind(Neuroticism, Extraversion,
                                 Openness, Agreeableness,
                                 Conscientiousness) ~ population)

# manova test
summary(IPIP_manova, test = "Pillai")
# anova post-hoc tests
summary.aov(IPIP_manova)
# pairwise p-value saved to a list
sapply(summary.aov(IPIP_manova), 
       function(tab) tab[["Pr(>F)"]][1]) %>%
  p.adjust(., method = "holm")  
# all less than 0.05 except Openness

# NFP-int comparisons (just item level)
ggplot(cbind(std_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
             std_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,]) + 
  geom_point(aes(x = c(1:4,6:9,11:14), y = mean)) + 
  geom_errorbar(aes(x = c(1:4,6:9,11:14), y = mean, ymin = mean - sd, ymax = mean+sd)) +
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:4,6:9,11:14), y = 0, label = c("inf-1","inf-2","inf-3","inf-4","psy-1","psy-2",
                              "psy-3","psy-4","phy-1","phy-2","phy-3","phy-4"))) +
  labs(x = "item", y ="value", title = "NFP-S (Frener et al. 2024) population summary")+
  theme_bw()


ggplot(cbind(summary_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
             summary_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,]) + 
  geom_point(aes(x = c(1:4,6:9,11:14), y = mean), color = "blue") + 
  geom_errorbar(aes(x = c(1:4,6:9,11:14), y = mean, ymin = mean - sd, ymax = mean+sd), color = "blue") +
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:4,6:9,11:14), y = 0, label = c("inf-1","inf-2","inf-3","inf-4","psy-1","psy-2",
                                                        "psy-3","psy-4","phy-1","phy-2","phy-3","phy-4"))) +
  labs(x = "item", y ="value", title = "NFP-S (Frener et al. 2024) study summary") +
  theme_bw()

ggplot() +
  # population statistics
  geom_point(data = cbind(std_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                          std_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,],
             aes(x = c(1:4,6:9,11:14), y = mean), alpha = 0.4) + 
  geom_errorbar(data = cbind(std_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                             std_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,],
                aes(x = c(1:4,6:9,11:14), y = mean, ymin = mean - sd, ymax = mean+sd), alpha = 0.4) +
# study statistics
  geom_point(data = cbind(summary_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                        summary_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,],
           aes(x = c(1:4,6:9,11:14), y = mean), color = "blue") + 
  geom_errorbar(data = cbind(summary_NFP_int %>% select(starts_with("m")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                             summary_NFP_int %>% select(starts_with("s")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))[1:12,],
                aes(x = c(1:4,6:9,11:14), y = mean, ymin = mean - sd, ymax = mean+sd), color = "blue") +
  # plot aesthetics
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:4,6:9,11:14), y = 0, label = c("inf-1","inf-2","inf-3","inf-4","psy-1","psy-2",
                                                        "psy-3","psy-4","phy-1","phy-2","phy-3","phy-4"))) +
  labs(x = "item", y ="value", title = "NFP-S (Frener et al. 2024) population vs. study (blue) summary") + 
  theme_bw()

# NFP-int comparisons (group level)
ggplot(cbind(std_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
             std_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))) + 
  geom_point(aes(x = c(1:3), y = mean)) + 
  geom_errorbar(aes(x = c(1:3), y = mean, ymin = mean - sd, ymax = mean+sd)) +
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:3), y = 0, label = c("informational","psychological","physical"))) +
  labs(x = "privacy construct", y ="value", title = "NFP-S (Frener et al. 2024) population construct summary") + 
  theme_bw()

ggplot(cbind(summary_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
             summary_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd"))) + 
  geom_point(aes(x = c(1:3), y = mean), color = "blue") + 
  geom_errorbar(aes(x = c(1:3), y = mean, ymin = mean - sd, ymax = mean+sd), color = "blue") +
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:3), y = 0, label = c("informational","psychological","physical"))) +
  labs(x = "privacy construct", y ="value", title = "NFP-S (Frener et al. 2024) study construct summary") + 
  theme_bw()

ggplot() +
  # population statistics
  geom_point(data = cbind(std_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                          std_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd")),
             aes(x = c(1:3), y = mean), alpha = 0.4) + 
  geom_errorbar(data = cbind(std_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                             std_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd")),
                aes(x = c(1:3), y = mean, ymin = mean - sd, ymax = mean+sd), alpha = 0.4) +
  # study statistics
  geom_point(data = cbind(summary_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                          summary_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd")),
             aes(x = c(1:3), y = mean), color = "blue") + 
  geom_errorbar(data = cbind(summary_NFP_int %>% select(starts_with("m_")) %>% pivot_longer(everything(), names_to = "var_m", values_to = "mean"),
                             summary_NFP_int %>% select(starts_with("sd_")) %>% pivot_longer(everything(), names_to = "var_s", values_to = "sd")),
                aes(x = c(1:3), y = mean, ymin = mean - sd, ymax = mean+sd), color = "blue") +
  # plot aesthetics
  geom_hline(yintercept = 5, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = 'dashed', alpha = 0.5) +
  scale_y_continuous(limits = c(0,6), breaks = c(0:6)) +
  scale_x_discrete(labels = "") +
  geom_label(aes(x = c(1:3), y = 0, label = c("informational","psychological","physical"))) +
  labs(x = "privacy construct", y ="value", title = "NFP-S (Frener et al. 2024) population vs. study (blue) construct summary") + 
  theme_bw()

##### initial data visualizations #####
# box plots
# perceived invasiveness
ggplot(data) + geom_boxplot(aes(invasiveness, task)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  theme_bw()

# perceived privacy loss
ggplot(data) + geom_boxplot(aes(privacy_loss, task)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  xlab("privacy loss") +
  theme_bw()

# rating of privacy provided by coverings
ggplot(partition_survey %>% select(!c(choice_privacy, choice_experience)) %>% pivot_longer(cols = !PID)) + 
  geom_boxplot(aes(value, name)) +
  scale_y_discrete(limits = c("privacy_none", "privacy_open","privacy_med","privacy_full"), labels = c("none","open","medium","full")) + 
  ylab("covering") + xlab("importance") + ggtitle("\"How private were the coverings?\"") + 
  theme_bw()

# rating of importance of control to privacy
ggplot(partition_survey %>% select(c(PID, choice_privacy, choice_experience)) %>% pivot_longer(cols = !PID)) + 
  geom_boxplot(aes(value, name)) + 
  scale_y_discrete(label = c("privacy", "overall experience")) + ylab("") + xlab("importance") +
  ggtitle("Subjective rating of importance on...") + 
  theme_bw()

# violin plots 
# perceived invasiveness
ggplot(data, aes(invasiveness, task)) + geom_violin() + 
  geom_point(size = 0.7, alpha = 0.5, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  theme_bw()

# perceived privacy loss
ggplot(data, aes(privacy_loss, task)) + geom_violin() +
  geom_point(size = 0.7, alpha = 0.5, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  xlab("privacy loss") +
  theme_bw()

# rating of privacy provided by coverings
ggplot(partition_survey %>% select(!c(choice_privacy, choice_experience)) %>% pivot_longer(cols = !PID)) + 
  geom_violin(aes(value, name, fill = name), color = NA, alpha = 0.6) +
  geom_point(aes(value, name, color = name), size = 2, alpha = 0.5, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("privacy_none", "privacy_open","privacy_med","privacy_full"),
                   labels = c("none","open","medium","full")) + 
  scale_fill_manual(name = "covering",
                    values = c("privacy_full"=coolors[5], "privacy_med" = coolors[4],
                               "privacy_open" = coolors[3], "privacy_none" = coolors[1]),
                    breaks = c("privacy_full", "privacy_med","privacy_open","privacy_none"),
                    labels = c("full","medium","open","none")
                    )+
  scale_color_manual(name = "covering",
                     values = c("privacy_full"=coolors[5], "privacy_med" = coolors[4],
                                "privacy_open" = coolors[3], "privacy_none" = coolors[1]),
                     breaks = c("privacy_full", "privacy_med","privacy_open","privacy_none"),
                     labels = c("full","medium","open","none")
  )+
  ylab("") + 
  xlab("not at all                                                                    very") + 
  ggtitle("\"How private were the coverings?\"") + 
  theme_minimal() + theme(legend.position = "none")

# rating of importance of control to privacy
ggplot(partition_survey %>% select(c(PID, choice_privacy, choice_experience)) %>% pivot_longer(cols = !PID),
       aes(value, name)) + 
  geom_violin(aes(fill=name, color = name), draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.6) + 
  geom_point(aes(color=name),size = 2, alpha = 0.6, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(label = c("privacy", "overall experience")) + ylab("") + 
  scale_fill_manual(values = c("choice_privacy"= "blue", "choice_experience" = "red")
  )+
  scale_color_manual(values = c("choice_privacy"= "blue", "choice_experience" = "red")
  )+
  xlab("not at all                                                                very") + 
  ggtitle("\"How important was control for...\"") + 
  theme_minimal() + theme(legend.position = "none")

data$partition <- as.factor(data$partition)

# violin plots with group overlays
# perceived invasiveness
ggplot() + geom_violin(data = data, aes(invasiveness, task, fill = lvl), alpha = 0.3) + 
  geom_point(data = data, aes(invasiveness, task), size = 0.7, alpha = 0.3, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  scale_fill_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  geom_point(data = data %>% group_by(lvl) %>%
               summarize(mean_invasion=mean(invasiveness),
                         sd_invasion = sd(invasiveness)), 
             aes(x = mean_invasion, y = c("fastener assembly", "NFP introspective", "nose swab"), color = lvl),
             size = 4) +
  geom_errorbar(data = data %>% group_by(lvl) %>%
               summarize(mean_invasion=mean(invasiveness),
                         sd_invasion = sd(invasiveness)), 
             aes(x = mean_invasion, y = c("fastener assembly", "NFP introspective", "nose swab"),
                 xmin = mean_invasion - sd_invasion, xmax = mean_invasion + sd_invasion, color = lvl),
             size = 1.5) +
  scale_color_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  guides(color = "none") +
  labs(fill = "Level of invasion") +
  theme_bw()

# perceived privacy loss
ggplot() + geom_violin(data = data, aes(privacy_loss, task, fill = lvl), alpha = 0.3) + 
  geom_point(data = data, aes(privacy_loss, task), size = 0.7, alpha = 0.3, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  scale_fill_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  geom_point(data = data %>% group_by(lvl) %>%
               summarize(mean_privloss=mean(privacy_loss),
                         sd_privloss = sd(privacy_loss)), 
             aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"), color = lvl),
             size = 4) +
  geom_errorbar(data = data %>% group_by(lvl) %>%
                  summarize(mean_privloss=mean(privacy_loss),
                            sd_privloss = sd(privacy_loss)), 
                aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"),
                    xmin = mean_privloss - sd_privloss, xmax = mean_privloss + sd_privloss, color = lvl),
                size = 1.5) +
  scale_color_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  guides(color = "none") +
  labs(fill = "Level of invasion", x = "privacy loss") +
  theme_bw()

# perceived privacy desired
ggplot() + geom_violin(data = data, aes(privacy_desired, task, fill = lvl), alpha = 0.3) + 
  geom_point(data = data, aes(privacy_desired, task), size = 0.7, alpha = 0.3, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  scale_fill_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  geom_point(data = data %>% group_by(lvl) %>%
               summarize(mean_privloss=mean(privacy_desired),
                         sd_privloss = sd(privacy_desired)), 
             aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"), color = lvl),
             size = 4) +
  geom_errorbar(data = data %>% group_by(lvl) %>%
                  summarize(mean_privloss=mean(privacy_desired),
                            sd_privloss = sd(privacy_desired)), 
                aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"),
                    xmin = mean_privloss - sd_privloss, xmax = mean_privloss + sd_privloss, color = lvl),
                size = 1.5) +
  scale_color_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  guides(color = "none") +
  labs(fill = "Level of invasion", x = "privacy desired") +
  theme_bw()

# perceived privacy achieved
ggplot() + geom_violin(data = data, aes(privacy_achieved, task, fill = lvl), alpha = 0.3) + 
  geom_point(data = data, aes(privacy_achieved, task), size = 0.7, alpha = 0.3, position = position_jitter(seed = 1, width = 0.1)) +
  scale_y_discrete(limits = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
                              "demographics","NFP introspective","NFP behavioral","IPIP",
                              "cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) + 
  scale_fill_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  # geom_point(data = data %>% group_by(lvl) %>%
  #              summarize(mean_privloss=mean(privacy_achieved),
  #                        sd_privloss = sd(privacy_achieved)), 
  #            aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"), color = lvl),
  #            size = 4) +
  # geom_errorbar(data = data %>% group_by(lvl) %>%
  #                 summarize(mean_privloss=mean(privacy_achieved),
  #                           sd_privloss = sd(privacy_achieved)), 
  #               aes(x = mean_privloss, y = c("fastener assembly", "NFP introspective", "nose swab"),
  #                   xmin = mean_privloss - sd_privloss, xmax = mean_privloss + sd_privloss, color = lvl),
  #               size = 1.5) +
  scale_color_discrete(limits = c("H","M","L"), labels = c("high","moderate","low")) + 
  guides(color = "none") +
  labs(fill = "Level of invasion", x = "privacy achieved") +
  facet_wrap(~partition)+
  theme_bw()

# bar chart for partitions selected
ggplot(data %>% subset(condition == "active" & task %in% c('floss','cheek swab',
                                                           'nose swab','talk PI',
                                                           'talk relationship',
                                                           'change clothes', 'NFP behavioral')) %>% select(task, partition)) + 
  geom_bar(aes(x = task, fill = fct_rev(partition))) + 
  # scale_fill_discrete()  + 
  scale_fill_manual(name = "covering setting",
                    values = coolors[c(5,4,3,1)], 
                    labels = c("full","medium","open","none"))+
  scale_x_discrete(limits = c('NFP behavioral',"cheek swab","talk PI","nose swab","floss","talk relationship","change clothes")) +
  scale_y_discrete(limits = c(0, 2, 4, 6, 8, 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


##### initial stats #####
# distinguishing subjective invasiveness ratings by level
invasiveness_aov <- lme(fixed = invasiveness ~ lvl, 
                        random = ~ 1 | PID, 
                        data = data)
emmeans(invasiveness_aov, pairwise ~ lvl, adjust = "Holm")

# distinguishing subjective privacy loss ratings by level
privacyloss_aov <- lme(fixed = privacy_loss ~ lvl, 
                        random = ~ 1 | PID, 
                        data = data)
emmeans(privacyloss_aov, pairwise ~ lvl, adjust = "Holm")

##### testing for equality of variances between NFP-int samples #####
# Input your summary statistics
f_test <- function(sd1,sd2){
  # group 1 is from Frener 2024
  # group 2 is from our data
  n1 <- 4504
  n2 <- 30
  var1 <- sd1^2
  var2 <- sd2^2
  
  f_stat <- var1 / var2
  df1 <- n1 - 1
  df2 <- n2 - 1
  
  p_value <- 2 * min(pf(f_stat, df1, df2), 1 - pf(f_stat, df1, df2))
  
  print(paste("F-statistic:", f_stat))
  print(paste("P-value:", p_value))
}

sd1_list <- std_NFP_int %>% select(starts_with("sd"))
sd2_list <- summary_NFP_int %>% select(starts_with("sd"))

for (i in c(1:15)) {
  print(i)
  f_test(sd1_list[[i]],sd2_list[[i]])
}

# run f tests on all sd pairs; only psy-2 and inf-group were sig. different
# but not when corrected for FWER
f_test(std_NFP_int$sd1, summary_NFP_int$sd1)

# so can compare differences across means 
# summary_nfp_int vs std_nfp_int
for (i in seq(1,12)){
  print(tsum.test(
    mean.x = summary_NFP_int[[paste0("m",i)]], 
    s.x = summary_NFP_int[[paste0("sd",i)]], 
    n.x = 30,   # Group 1: mean, SD, sample size
    
    mean.y = std_NFP_int[[paste0("m",i)]], 
    s.y = std_NFP_int[[paste0("sd",i)]], 
    n.y = 4504,   # Group 2: mean, SD, sample size
    var.equal = TRUE                   # Set TRUE for Student's t-test, FALSE for Welch's
  ) 
  )
}

for (i in c("_inf","_psy","_phy")){
  print(tsum.test(
    mean.x = summary_NFP_int[[paste0("m",i)]], 
    s.x = summary_NFP_int[[paste0("sd",i)]], 
    n.x = 30,   # Group 1: mean, SD, sample size
    
    mean.y = std_NFP_int[[paste0("m",i)]], 
    s.y = std_NFP_int[[paste0("sd",i)]], 
    n.y = 4504,   # Group 2: mean, SD, sample size
    var.equal = TRUE                   # Set TRUE for Student's t-test, FALSE for Welch's
  ) 
  )
}

##### building NFP-beh Rasch model #####
NFP_beh_Rasch <- RM(NFP_beh_dich)
NFP_beh_PCM <- PCM(NFP_beh[,c(2:26)] - 1)
NFP_beh_MML <- tam.mml(resp = NFP_beh_dich)
        # beta.fixed = cbind(c(1:25), 1, std_NFP_beh_delta),
        # verbose = TRUE)

# Get item difficulties
item_diffs <- NFP_beh_Rasch$betapar

# Get person scores (theta)
person_scores <- person.parameter(NFP_beh_Rasch)
nfp_scores <- data.frame("PID" = NFP_beh$PID %>% as.factor(), 
                    "nfp_theta" = person_scores$theta.table$`Person Parameter`)

# Check model fit
summary(NFP_beh_Rasch)
LRtest(NFP_beh_Rasch) # p-val indicated no issues; excluded about 1/2 of the items
# checking with Haans 2007
ggplot(data.frame(delta = std_NFP_beh_delta[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,19,
                                 20, 21, 22,23,24)], 
                  beta = item_diffs)) +
  geom_point(aes(x = delta, y = beta))

# plotICC(NFP_beh_Rasch)
# # plot ICC from study model
# dev.new(width = 14, height = 9)
# plotjointICC(NFP_beh_Rasch,legend = FALSE,xlim = c(-4, 4), ylim = c(0, 1))
# plot ICC from Haans
ggplot(NFP_beh_Haans)+
  geom_line(aes(x = theta, y = probability, group = factor(item), color = factor(item))) + 
  scale_y_continuous(limits = c(0,1)) 

summary(NFP_beh_PCM)

save.image(file = "data-formodeling.RData")
