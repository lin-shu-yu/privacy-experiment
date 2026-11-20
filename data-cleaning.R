# data cleaning for space privacy experiment

# author: Mich Lin (shuyulin)
# date created: MAY 7 2026
# date last modified: JUL 7 2026


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

# set color palette for ggplot (colorblind accessible)
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")

###### data import ###### 
setwd("/Users/michellelin/MIT Dropbox/Mich Lin/Research/space_architecture/tech_dev/experiment/data")
condition_PID <- read_excel("counterbalancing.xlsx",range = "Condition orders!B1:C42",
                            col_types = c("text","numeric")) %>% na.omit()

age_PID <- read_excel("counterbalancing.xlsx",range = "PID!A1:B34",
                      col_types = c("text","numeric")) %>% na.omit()

# append to dataframe for all sheets
sheet_list <- list()

for(i in 1:30){
    sheet_i = read_excel("counterbalancing.xlsx",
             range = paste0("C",i,"!A18:K38"),
             col_types = c("text","text","skip","skip","skip","date",
                           "text","skip","skip","numeric","numeric"),
             col_names = c("condition","task","time","note",
                           "ppl_count","partition")) %>%
      filter(!row_number() %in% c(1,8,15)) %>%
    mutate(PID = condition_PID$PID[condition_PID$CONDITION==i] %>% 
             as.factor(), .before = 1)
   
    sheet_i <- sheet_i %>% mutate(task_num = c(1:18), .after = 2)
    sheet_list[[i]] <- sheet_i
}

# make dataframe for entire dataset
experimental_data <- do.call(rbind, sheet_list)
experimental_data$PID <- as.factor(experimental_data$PID)
experimental_data$condition <- relevel(factor(experimental_data$condition), ref = "baseline")

# reformatting time column to M/S
experimental_data$time <- experimental_data$time %>% format("%H:%M") %>% lubridate::ms()

# import achieved privacy scales
sheet_list <- list()

for(i in 1:18){
  participant_i = read_excel(paste0("achieved-privacy-",i,"_scores.xlsx"),
                             range = "A2:A31", col_names = c("PID"))
  
  sheet_i = read_excel(paste0("achieved-privacy-",i,"_scores.xlsx"),
                       range = "L2:V31",
                       col_names = c("invasiveness","privacy_desired","privacy_achieved",
                                     "awareness","worry_see","worry_hear","comfort","personal_space",
                                     "energy","sightlines","notice")) %>%
    mutate(PID = as.factor(participant_i$PID) %>% as.factor(), .before = 1)
  
  sheet_i <- sheet_i %>% mutate(task_num = i, .after = 1)
  sheet_list[[i]] <- sheet_i
}

# make dataframe for entire dataset
achieved_privacy <- do.call(rbind, sheet_list)
# create privacy loss column
achieved_privacy <- achieved_privacy %>% mutate(privacy_loss = privacy_desired - privacy_achieved, .after = 5)

# combine dataframes
data = full_join(experimental_data, achieved_privacy, by = c("PID", "task_num"))%>% select(!note)

# label tasks by invasiveness
task_invasion <- data.frame(task = c("LEGO","component sorting","fastener assembly","timed origami","PVT", "med kit",
"demographics","NFP introspective","NFP behavioral","IPIP",
"cheek swab","talk PI","nose swab","floss","talk relationship","change clothes"), 
lvl = as.factor(c("L","L","L","L","L","L","M","M","M","M","H","H","H","H","H","H")))
task_invasion$lvl <- factor(task_invasion$lvl, levels = c("L","M","H"), ordered = TRUE)

data <- left_join(data, task_invasion, by = "task")


##### demographics #####
demographics = read_excel("Demographics_scores.xlsx",
                          range = "L2:BQ31", 
                          col_names = c("Sex_M","Sex_F","Sex_O","Sex_PND",
                                        "Gender_A","Gender_Cis","Gender_Trans","Gender_M","Gender_W",
                                        "Gender_Q","Gender_NB","Gender_NA","Gender_PND","Gender_O",
                                        "Hispanic_Y","Hispanic_N",
                                        "Race_W","Race_B","Race_N",
                                        "Race_A","Race_HI","Race_PND","Race_O",
                                        "Edu_B","Edu_M","Edu_PhD","Edu_MD","Edu_JD","Edu_O",
                                        "Course_1","Course_2","Course_3","Course_4","Course_5","Course_6",
                                        "Course_7","Course_8","Course_9","Course_10","Course_11","Course_12",
                                        "Course_14","Course_15","Course_16","Course_17","Course_18",
                                        "Course_20","Course_21","Course_22","Course_24","Course_HST",
                                        "Course_IDS","Course_O",
                                        "Military_Y","Military_N","Military_PND",
                                        "Employ_Student","Employ_O")) %>% 
  mutate(PID = read_excel("Demographics_scores.xlsx",
                          range = "A2:A31", col_names = c("PID"))$PID %>% 
           as.factor(), .before = 1)

IPIP = read_excel("IPIP_scores.xlsx",
                          range = "L2:EA31", col_names = paste0("IPIP_", c(1:120))) %>% 
  mutate(PID = read_excel("IPIP_scores.xlsx",
                          range = "A2:A31", col_names = c("PID"))$PID %>%
           as.factor(), .before = 1) %>%
  mutate(
    RC_IPIP_9 = 6 - IPIP_9,
    RC_IPIP_19 = 6 - IPIP_19,
    RC_IPIP_24 = 6 - IPIP_24,
    RC_IPIP_30 = 6 - IPIP_30,
    RC_IPIP_39 = 6 - IPIP_39,
    RC_IPIP_40 = 6 - IPIP_40,
    RC_IPIP_48 = 6 - IPIP_48,
    RC_IPIP_49 = 6 - IPIP_49,
    RC_IPIP_51 = 6 - IPIP_51,
    RC_IPIP_53 = 6 - IPIP_53,
    RC_IPIP_54 = 6 - IPIP_54,
    RC_IPIP_60 = 6 - IPIP_60,
    RC_IPIP_62 = 6 - IPIP_62,
    RC_IPIP_67 = 6 - IPIP_67,
    RC_IPIP_68 = 6 - IPIP_68,
    RC_IPIP_69 = 6 - IPIP_69,
    RC_IPIP_70 = 6 - IPIP_70,
    RC_IPIP_73 = 6 - IPIP_73,
    RC_IPIP_74 = 6 - IPIP_74,
    RC_IPIP_75 = 6 - IPIP_75,
    RC_IPIP_78 = 6 - IPIP_78,
    RC_IPIP_79 = 6 - IPIP_79,
    RC_IPIP_80 = 6 - IPIP_80,
    RC_IPIP_81 = 6 - IPIP_81,
    RC_IPIP_83 = 6 - IPIP_83,
    RC_IPIP_84 = 6 - IPIP_84,
    RC_IPIP_85 = 6 - IPIP_85,
    RC_IPIP_88 = 6 - IPIP_88,
    RC_IPIP_89 = 6 - IPIP_89,
    RC_IPIP_90 = 6 - IPIP_90,
    RC_IPIP_92 = 6 - IPIP_92,
    RC_IPIP_94 = 6 - IPIP_94,
    RC_IPIP_96 = 6 - IPIP_96,
    RC_IPIP_97 = 6 - IPIP_97,
    RC_IPIP_98 = 6 - IPIP_98,
    RC_IPIP_99 = 6 - IPIP_99,
    RC_IPIP_100 = 6 - IPIP_100,
    RC_IPIP_101 = 6 - IPIP_101,
    RC_IPIP_102 = 6 - IPIP_102,
    RC_IPIP_103 = 6 - IPIP_103,
    RC_IPIP_104 = 6 - IPIP_104,
    RC_IPIP_105 = 6 - IPIP_105,
    RC_IPIP_106 = 6 - IPIP_106,
    RC_IPIP_107 = 6 - IPIP_107,
    RC_IPIP_108 = 6 - IPIP_108,
    RC_IPIP_109 = 6 - IPIP_109,
    RC_IPIP_110 = 6 - IPIP_110,
    RC_IPIP_111 = 6 - IPIP_111,
    RC_IPIP_113 = 6 - IPIP_113,
    RC_IPIP_114 = 6 - IPIP_114,
    RC_IPIP_115 = 6 - IPIP_115,
    RC_IPIP_116 = 6 - IPIP_116,
    RC_IPIP_118 = 6 - IPIP_118,
    RC_IPIP_119 = 6 - IPIP_119,
    RC_IPIP_120 = 6 - IPIP_120,
  ) %>%
  mutate(Neuroticism = rowMeans(pick(IPIP_1, IPIP_6, IPIP_11, IPIP_16, IPIP_21, 
                            IPIP_26, IPIP_31, IPIP_36, IPIP_41, IPIP_46, 
                            RC_IPIP_51, IPIP_56, IPIP_61, IPIP_66, IPIP_71, 
                            IPIP_76, RC_IPIP_81, IPIP_86, IPIP_91, RC_IPIP_96, 
                            RC_IPIP_101, RC_IPIP_106, RC_IPIP_111, RC_IPIP_116)),
         Extraversion = rowMeans(pick(IPIP_2, IPIP_7, IPIP_12, IPIP_17, IPIP_22, 
                                      IPIP_27, IPIP_32, IPIP_37, IPIP_42, IPIP_47, 
                                      IPIP_52, IPIP_57, RC_IPIP_62, RC_IPIP_67, IPIP_72, 
                                      IPIP_77, IPIP_82, IPIP_87, RC_IPIP_92, RC_IPIP_97, 
                                      RC_IPIP_102, RC_IPIP_107, IPIP_112, IPIP_117)),
         Openness = rowMeans(pick(IPIP_3, IPIP_8, IPIP_13, IPIP_18, IPIP_23, 
                                  IPIP_28, IPIP_33, IPIP_38, IPIP_43, RC_IPIP_48, 
                                  RC_IPIP_53, IPIP_58, IPIP_63, RC_IPIP_68, RC_IPIP_73, 
                                  RC_IPIP_78, RC_IPIP_83, RC_IPIP_88, IPIP_93, RC_IPIP_98, 
                                  RC_IPIP_103, RC_IPIP_108, RC_IPIP_113, RC_IPIP_118)),
         Agreeableness = rowMeans(pick(IPIP_4, RC_IPIP_9, IPIP_14, RC_IPIP_19, RC_IPIP_24, 
                                       IPIP_29, IPIP_34, RC_IPIP_39, IPIP_44, RC_IPIP_49, 
                                       RC_IPIP_54, IPIP_59, IPIP_64, RC_IPIP_69, RC_IPIP_74, 
                                       RC_IPIP_79, RC_IPIP_84, RC_IPIP_89, RC_IPIP_94, RC_IPIP_99, 
                                       RC_IPIP_104, RC_IPIP_109, RC_IPIP_114, RC_IPIP_119)),
         Conscientiousness = rowMeans(pick(IPIP_5, IPIP_10, IPIP_15, IPIP_20, IPIP_25, 
                                            RC_IPIP_30, IPIP_35, RC_IPIP_40, IPIP_45, IPIP_50, 
                                           IPIP_55, RC_IPIP_60, IPIP_65, RC_IPIP_70, 
                                           RC_IPIP_75, RC_IPIP_80, RC_IPIP_85, 
                                           RC_IPIP_90, IPIP_95, RC_IPIP_100, RC_IPIP_105, 
                                           RC_IPIP_110, RC_IPIP_115, RC_IPIP_120))
  )

# from HERA campaigns 4, 5, 6, including backups
HERA_IPIP <- read_excel("HERA_IPIP.xlsx",
                   range = "C2:DR59", col_names = paste0("IPIP_", c(1:120))) %>% 
  mutate(Campaign = read_excel("HERA_IPIP.xlsx",
                          range = "B2:B59", col_names = c("Campaign"))$Campaign, .before = 1) %>%
  mutate(PID = read_excel("HERA_IPIP.xlsx",
                          range = "A2:A59", col_names = c("PID"))$PID %>%
           as.factor(), .before = 1) %>%
  mutate(
    RC_IPIP_9 = 8 - IPIP_9,
    RC_IPIP_19 = 8 - IPIP_19,
    RC_IPIP_24 = 8 - IPIP_24,
    RC_IPIP_30 = 8 - IPIP_30,
    RC_IPIP_39 = 8 - IPIP_39,
    RC_IPIP_40 = 8 - IPIP_40,
    RC_IPIP_48 = 8 - IPIP_48,
    RC_IPIP_49 = 8 - IPIP_49,
    RC_IPIP_51 = 8 - IPIP_51,
    RC_IPIP_53 = 8 - IPIP_53,
    RC_IPIP_54 = 8 - IPIP_54,
    RC_IPIP_60 = 8 - IPIP_60,
    RC_IPIP_62 = 8 - IPIP_62,
    RC_IPIP_67 = 8 - IPIP_67,
    RC_IPIP_68 = 8 - IPIP_68,
    RC_IPIP_69 = 8 - IPIP_69,
    RC_IPIP_70 = 8 - IPIP_70,
    RC_IPIP_73 = 8 - IPIP_73,
    RC_IPIP_74 = 8 - IPIP_74,
    RC_IPIP_75 = 8 - IPIP_75,
    RC_IPIP_78 = 8 - IPIP_78,
    RC_IPIP_79 = 8 - IPIP_79,
    RC_IPIP_80 = 8 - IPIP_80,
    RC_IPIP_81 = 8 - IPIP_81,
    RC_IPIP_83 = 8 - IPIP_83,
    RC_IPIP_84 = 8 - IPIP_84,
    RC_IPIP_85 = 8 - IPIP_85,
    RC_IPIP_88 = 8 - IPIP_88,
    RC_IPIP_89 = 8 - IPIP_89,
    RC_IPIP_90 = 8 - IPIP_90,
    RC_IPIP_92 = 8 - IPIP_92,
    RC_IPIP_94 = 8 - IPIP_94,
    RC_IPIP_96 = 8 - IPIP_96,
    RC_IPIP_97 = 8 - IPIP_97,
    RC_IPIP_98 = 8 - IPIP_98,
    RC_IPIP_99 = 8 - IPIP_99,
    RC_IPIP_100 = 8 - IPIP_100,
    RC_IPIP_101 = 8 - IPIP_101,
    RC_IPIP_102 = 8 - IPIP_102,
    RC_IPIP_103 = 8 - IPIP_103,
    RC_IPIP_104 = 8 - IPIP_104,
    RC_IPIP_105 = 8 - IPIP_105,
    RC_IPIP_106 = 8 - IPIP_106,
    RC_IPIP_107 = 8 - IPIP_107,
    RC_IPIP_108 = 8 - IPIP_108,
    RC_IPIP_109 = 8 - IPIP_109,
    RC_IPIP_110 = 8 - IPIP_110,
    RC_IPIP_111 = 8 - IPIP_111,
    RC_IPIP_113 = 8 - IPIP_113,
    RC_IPIP_114 = 8 - IPIP_114,
    RC_IPIP_115 = 8 - IPIP_115,
    RC_IPIP_116 = 8 - IPIP_116,
    RC_IPIP_118 = 8 - IPIP_118,
    RC_IPIP_119 = 8 - IPIP_119,
    RC_IPIP_120 = 8 - IPIP_120,
  ) %>%
  mutate(Neuroticism = rowMeans(pick(IPIP_1, IPIP_6, IPIP_11, IPIP_16, IPIP_21, 
                                     IPIP_26, IPIP_31, IPIP_36, IPIP_41, IPIP_46, 
                                     RC_IPIP_51, IPIP_56, IPIP_61, IPIP_66, IPIP_71, 
                                     IPIP_76, RC_IPIP_81, IPIP_86, IPIP_91, RC_IPIP_96, 
                                     RC_IPIP_101, RC_IPIP_106, RC_IPIP_111, RC_IPIP_116)),
         Extraversion = rowMeans(pick(IPIP_2, IPIP_7, IPIP_12, IPIP_17, IPIP_22, 
                                      IPIP_27, IPIP_32, IPIP_37, IPIP_42, IPIP_47, 
                                      IPIP_52, IPIP_57, RC_IPIP_62, RC_IPIP_67, IPIP_72, 
                                      IPIP_77, IPIP_82, IPIP_87, RC_IPIP_92, RC_IPIP_97, 
                                      RC_IPIP_102, RC_IPIP_107, IPIP_112, IPIP_117)),
         Openness = rowMeans(pick(IPIP_3, IPIP_8, IPIP_13, IPIP_18, IPIP_23, 
                                  IPIP_28, IPIP_33, IPIP_38, IPIP_43, RC_IPIP_48, 
                                  RC_IPIP_53, IPIP_58, IPIP_63, RC_IPIP_68, RC_IPIP_73, 
                                  RC_IPIP_78, RC_IPIP_83, RC_IPIP_88, IPIP_93, RC_IPIP_98, 
                                  RC_IPIP_103, RC_IPIP_108, RC_IPIP_113, RC_IPIP_118)),
         Agreeableness = rowMeans(pick(IPIP_4, RC_IPIP_9, IPIP_14, RC_IPIP_19, RC_IPIP_24, 
                                       IPIP_29, IPIP_34, RC_IPIP_39, IPIP_44, RC_IPIP_49, 
                                       RC_IPIP_54, IPIP_59, IPIP_64, RC_IPIP_69, RC_IPIP_74, 
                                       RC_IPIP_79, RC_IPIP_84, RC_IPIP_89, RC_IPIP_94, RC_IPIP_99, 
                                       RC_IPIP_104, RC_IPIP_109, RC_IPIP_114, RC_IPIP_119)),
         Conscientiousness = rowMeans(pick(IPIP_5, IPIP_10, IPIP_15, IPIP_20, IPIP_25, 
                                           RC_IPIP_30, IPIP_35, RC_IPIP_40, IPIP_45, IPIP_50, 
                                           IPIP_55, RC_IPIP_60, IPIP_65, RC_IPIP_70, 
                                           RC_IPIP_75, RC_IPIP_80, RC_IPIP_85, 
                                           RC_IPIP_90, IPIP_95, RC_IPIP_100, RC_IPIP_105, 
                                           RC_IPIP_110, RC_IPIP_115, RC_IPIP_120))
         )

NFP_beh = read_excel("NFP-beh_scores.xlsx",
                  range = "L2:AJ31", col_names = paste0("beh_", c(1:25))) %>% 
  mutate(PID = read_excel("NFP-beh_scores.xlsx",
                          range = "A2:A31", col_names = c("PID"))$PID %>%
           as.factor(), .before = 1)

NFP_beh_dich <- NFP_beh %>% select(starts_with("beh_")) %>%
                                    mutate(across(everything(),
                                                   ~case_when(
                                                     . == 1 ~ 0,
                                                     .default = 1
                                                   )
                                          )
                                   )

# table of delta from Haans
NFP_beh_Haans <- data.frame(theta = seq(-4, 4, length.out = 100))

theta = seq(-4, 4, length.out = 100)
std_NFP_beh_delta <- c(3.97, 2.53, 2.30, 2.13, 2.04, 1.82, 1.59, 0.82, 0.59, 
                       -0.07, -0.18, -0.30, -0.39, -0.38, -0.54, -0.69, -0.75,
                       -0.77, -0.94, -1.46, -1.58, -1.67, -2.13, -2.64, -3.31)

for (i in c(1:25)){
  prob <- exp(theta - std_NFP_beh_delta[i])/(1+exp(theta - std_NFP_beh_delta[i]))
  if (i==1){
    NFP_beh_Haans <- NFP_beh_Haans %>% mutate(
      item = i,
      probability = prob
    )
  }
  else{
    temp <- data.frame(theta = seq(-4, 4, length.out = 100)) %>%
      mutate(item = i,
             probability = prob)
    NFP_beh_Haans <- rbind(NFP_beh_Haans, temp)
  }
}

NFP_int = read_excel("NFP-int_scores.xlsx",
                     range = "L2:W31", col_names = paste0("int_", c(1:12))) %>% 
  mutate(PID = read_excel("NFP-int_scores.xlsx",
                          range = "A2:A31", col_names = c("PID"))$PID %>%
           as.factor(), .before = 1)

summary_NFP_int <- data.frame()
summary_NFP_int <- NFP_int %>% 
  summarize(
    m1 = mean(int_1),
    m2 = mean(int_2),
    m3 = mean(int_3),
    m4 = mean(int_4),
    m5 = mean(int_5),
    m6 = mean(int_6),
    m7 = mean(int_7),
    m8 = mean(int_8),
    m9 = mean(int_9),
    m10 = mean(int_10),
    m11 = mean(int_11),
    m12 = mean(int_12),
    m_inf = mean(rbind(int_1, int_2, int_3, int_4)),
    m_psy = mean(rbind(int_5, int_6, int_7, int_8)),
    m_phy = mean(rbind(int_9, int_10, int_11, int_12)),
    sd1 = sd(int_1),
    sd2 = sd(int_2),
    sd3 = sd(int_3),
    sd4 = sd(int_4),
    sd5 = sd(int_5),
    sd6 = sd(int_6),
    sd7 = sd(int_7),
    sd8 = sd(int_8),
    sd9 = sd(int_9),
    sd10 = sd(int_10),
    sd11 = sd(int_11),
    sd12 = sd(int_12),
    sd_inf = sd(rbind(int_1, int_2, int_3, int_4)),
    sd_psy = sd(rbind(int_5, int_6, int_7, int_8)),
    sd_phy = sd(rbind(int_9, int_10, int_11, int_12))
  ) 

# from Frener 2024
std_NFP_int <- data.frame(
  m1 = 4.01,
  m2 = 3.85,
  m3 = 4.23,
  m4 = 4.39,
  m5 = 3.51,
  m6 = 2.83,
  m7 = 2.92,
  m8 = 3.16,
  m9 = 4.00,
  m10 = 4.12,
  m11 = 4.06,
  m12 = 3.74,
  sd1 = 1.00,
  sd2 = 1.08,
  sd3 = 1.00,
  sd4 = 0.88,
  sd5 = 1.15,
  sd6 = 1.22,
  sd7 = 1.23,
  sd8 = 1.22,
  sd9 = 1.08,
  sd10 = 1.09,
  sd11 = 1.09,
  sd12 = 1.13
) %>%
  mutate(
    m_inf = mean(c(m1, m2, m3, m4)),
    m_psy = mean(c(m5, m6, m7, m8)),
    m_phy = mean(c(m9, m10, m11, m12)),
    # Cohen (1988) for equal sample sizes
    sd_inf = sqrt((sd1^2 + sd2^2 + sd3^2 + sd4^2)/4),
    sd_psy = sqrt((sd5^2 + sd6^2 + sd7^2 + sd8^2)/4),
    sd_phy = sqrt((sd9^2 + sd10^2 + sd11^2 + sd12^2)/4)
  )

partition_survey = read_excel("partition-survey_scores.xlsx",
                     range = "L2:Q31", col_names = c("privacy_none","privacy_full","privacy_med","privacy_open",
                                                     "choice_privacy","choice_experience")) %>% 
  mutate(PID = read_excel("partition-survey_scores.xlsx",
                          range = "A2:A31", col_names = c("PID"))$PID %>%
           as.factor(), .before = 1)


save.image(file = "data.RData")

