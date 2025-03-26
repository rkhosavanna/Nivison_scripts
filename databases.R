rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
library(readxl)
#library(xlsx)
library(dplyr)
library(tibble)
library(VIM)
library(tidyverse)
library(magrittr)
library(tableone)
library(gtsummary)
library(flextable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(epiR)
library(hrbrthemes)
library(scales)
library(plotly)
library(pROC)

setwd("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/")

######### Base de dados MSD IgG3 PRNT - Pau da Lima----

ZIKV_IgG3_results_L37_L38 <- read_excel("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/Database/ajustadas/Bases para compartilhar/ZIKV_IgG3_results_L37_L38.xlsx")
ZIKV_PRNT_results_L37_L38 <- read_excel("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/Database/ajustadas/Bases para compartilhar/ZIKV_PRNT_results_L37_L38.xlsx")
ZIKV_MSD_results_L37_L49 <- read_excel("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/Database/ajustadas/Bases para compartilhar/ZIKV_DENV_MSD_Results_L37toL49.xlsx")

ZIKV_MSD_results_L37_L38 <- ZIKV_MSD_results_L37_L49 %>% filter(Cohort  %in% c("L37", "L38") & is.na(Duplicate)) %>% select(-c(idnova))


IgG3_PRNT_L37_L38 <- merge(ZIKV_IgG3_results_L37_L38, ZIKV_PRNT_results_L37_L38, by = "idnova", all.x = T)

IgG3_PRNT_L37_L38$group_MSD[IgG3_PRNT_L37_L38$denviggt_titers_l37>50 & IgG3_PRNT_L37_L38$zikvigg3posn2_l37=="Negative" & IgG3_PRNT_L37_L38$zikvigg3posn2_l38=="Positive"] <- 1
IgG3_PRNT_L37_L38$group_MSD[IgG3_PRNT_L37_L38$denviggt_titers_l37==25 & IgG3_PRNT_L37_L38$zikvigg3posn2_l37=="Negative" & IgG3_PRNT_L37_L38$zikvigg3posn2_l38=="Positive"] <- 2
IgG3_PRNT_L37_L38$group_MSD[IgG3_PRNT_L37_L38$denviggt_titers_l37>50 & IgG3_PRNT_L37_L38$zikvigg3posn2_l37=="Negative" & IgG3_PRNT_L37_L38$zikvigg3posn2_l38=="Negative"] <- 3
IgG3_PRNT_L37_L38$group_MSD[IgG3_PRNT_L37_L38$denviggt_titers_l37==25 & IgG3_PRNT_L37_L38$zikvigg3posn2_l37=="Negative" & IgG3_PRNT_L37_L38$zikvigg3posn2_l38=="Negative"] <- 4
summary(as.factor(IgG3_PRNT_L37_L38$group_MSD))

IgG3_PRNT_L37_L38$group_MSD_PRNT[IgG3_PRNT_L37_L38$denviggt_titers_l37>50 & IgG3_PRNT_L37_L38$l37zikv==4 & IgG3_PRNT_L37_L38$l38zikv>4] <- 1
IgG3_PRNT_L37_L38$group_MSD_PRNT[IgG3_PRNT_L37_L38$denviggt_titers_l37==25 & IgG3_PRNT_L37_L38$l37zikv==4 & IgG3_PRNT_L37_L38$l38zikv>4] <- 2
IgG3_PRNT_L37_L38$group_MSD_PRNT[IgG3_PRNT_L37_L38$denviggt_titers_l37>50 & IgG3_PRNT_L37_L38$l37zikv==4 & IgG3_PRNT_L37_L38$l38zikv==4] <- 3
IgG3_PRNT_L37_L38$group_MSD_PRNT[IgG3_PRNT_L37_L38$denviggt_titers_l37==25 & IgG3_PRNT_L37_L38$l37zikv==4 & IgG3_PRNT_L37_L38$l38zikv==4] <- 4
summary(as.factor(IgG3_PRNT_L37_L38$group_MSD_PRNT))

data_comp_l37 <- IgG3_PRNT_L37_L38 %>% dplyr::select(idnova, group_MSD, denviggt_titers_l37, avgzikvigg3_l37, l37rden1, l37den2,
                                             l37den3, l37den4, l37zikv, group_MSD_PRNT)
data_comp_l37$time <- "L37"

names(data_comp_l37) <- c("idnova", "group_MSD", "denviggt_titers", "avgzikvigg3", "prnt_den1", "prnt_den2","prnt_den3", "prnt_den4", "prnt_zikv", "group_MSD_PRNT","time")

data_comp_l37$id <- paste(data_comp_l37$time, data_comp_l37$idnova, sep = "-")


data_comp_l38 <- IgG3_PRNT_L37_L38 %>% dplyr::select(idnova, group_MSD, denviggt_titers_l38, avgzikvigg3_l38, l38rden1, l38den2,
                                             l38den3, l38den4, l38zikv, group_MSD_PRNT)
data_comp_l38$time <- "L38"

names(data_comp_l38) <- c("idnova", "group_MSD", "denviggt_titers","avgzikvigg3", "prnt_den1", "prnt_den2","prnt_den3", "prnt_den4", "prnt_zikv", "group_MSD_PRNT", "time")

data_comp_l38$id <- paste(data_comp_l38$time, data_comp_l38$idnova, sep = "-")

data_comp_l37_l38 <- rbind(data_comp_l37, data_comp_l38)

data_comp_l37_l38$zika_posIgG3 <- ifelse(data_comp_l37_l38$avgzikvigg3 >= 0.586, "Yes", "No")

data_comp_l37_l38$dengue_total_pos <- ifelse(data_comp_l37_l38$denviggt_titers >= 50, "Yes", "No")
summary(as.factor(data_comp_l37_l38$dengue_total_pos))

data_comp_l37_l38$zika_sc[data_comp_l37_l38$group_MSD==1 | data_comp_l37_l38$group_MSD==2] <- "Yes"
data_comp_l37_l38$zika_sc[data_comp_l37_l38$group_MSD==3 | data_comp_l37_l38$group_MSD==4] <- "No"

summary(as.factor(data_comp_l37_l38$zika_sc))

data_comp_l37_l38 <- data_comp_l37_l38 %>% select(-c(idnova, time))

#MSD_IgG3_PRNT_l37_l38 <- merge(data_comp_l37_l38, ZIKV_MSD_results_L37_L38, by = "id")
MSD_IgG3_PRNT_l37_l49 <- merge(ZIKV_MSD_results_L37_L49, data_comp_l37_l38, by = "id", all.x = T)
MSD_IgG3_PRNT_l37_l49$date_sample <- as.Date(MSD_IgG3_PRNT_l37_l49$date_sample)
library(openxlsx) 
write.xlsx(
  MSD_IgG3_PRNT_l37_l49, 
  file = "Database/ajustadas/Bases para compartilhar/MSD_IgG3_PRNT_l37_l49.xlsx")







