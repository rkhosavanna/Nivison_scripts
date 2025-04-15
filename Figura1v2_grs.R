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
library(ggbreak)
library(ggplot2)
#setwd("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/Database/ajustadas/Bases para compartilhar/Bases de dados do artigo do MSD/")
setwd("~/Documents/ZIKV project/MSD/data_MSD")

TRDP <- read_excel(#"Brazilian ZIKV PCR positive/
                   "ZIKV_NS1_09JUL24_clean2.xlsx")

Emory <- read_excel(#"USA ZIKV PCR positive/
  "Emory ZIKV MSD 11MAY24.xlsx")

######## old thai file? ########
#dfthai_KFCS <- read_excel(#"Thai cohort/
#  "KFCS MSD IgG 03APR24_clean.xlsx")

#freq1 <- dfthai %>% filter(!is.na(ZKNS1)) %>% group_by(Subject) %>% dplyr ::summarise(count=n())

#summary(as.factor(dfthai_KFCS$time4))
#dfthai_KFCS$time4 <- factor(dfthai_KFCS$time4, levels=c(-4, -3, -2, -1, 0, 0.1, 0.2, 1,2,3,4))

#subjects_dfthai_KFCS <- dfthai_KFCS %>%
#  group_split(subject)

#infection_dfthai_KFCS <- dfthai_KFCS %>%
#  group_split(infecting_serotype, infection)

#dfthai_KFCS_prim <- dfthai_KFCS %>% filter(infection=="Primary")
#dfthai_KFCS_second <- dfthai_KFCS %>% filter(infection=="Secondary")

#dfthai_KFCS2 <- dfthai_KFCS %>%
#  filter(time4 %in% c("-1", "0", "0.1", "0.2", "1", NA))

#dfthai_KFCS_prim2 <- dfthai_KFCS2 %>% filter(infection=="Primary")
#dfthai_KFCS_second2 <- dfthai_KFCS2 %>% filter(infection=="Secondary")

########################################
####### new thai file from Marco from 25Mar2025

dfthai <- read.csv("combined_HAI_ELISA_MSD.csv")
dim(dfthai) #15737 28
names(dfthai)

## choose only subjects with confirmed DENV PCR
table(dfthai$PCR_update, useNA = "always")

dfthai <- dfthai |>
  group_by(subjectID) |>
  filter(any(PCR_update == TRUE, na.rm = TRUE)) |>
  ungroup()
dim(dfthai) #6888 28
length(unique(dfthai$subjectID)) #n = 49

#choose only MSD values (exclude ELISA)
dfthai <- dfthai |>
  filter(platform == "MSD")
dim(dfthai) #4476 28
length(unique(dfthai$subjectID)) #n = 49

#choose only IgG (from IgG, IgA, IgM)
dfthai <- dfthai |>
  filter(isotype == "IgG")
length(unique(dfthai$subjectID)) #n = 23

#create a column to mirror "infection" based off of Marco's criteria
#dfthai <- dfthai |>
#  group_by(subjectID) |>
#  mutate(prior_status = if_else(any(PCR_update == T & prior_infections_est == 0, na.rm = T), "naive", "exposed")) |>
#  ungroup()
# above yields only 2 in primary infection

dfthai <- dfthai |>
  group_by(subjectID) |>
  mutate(
    prior_status = case_when(
      endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_raw == 0, na.rm = TRUE), "naive", "exposed"),
      !endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_est == 0, na.rm = TRUE), "naive", "exposed")
    )
  ) |>
  ungroup()
    
#alternative, to separate out 2ndary (post-primary) into 2ndary and post-2ndary
dfthai <- dfthai |>
  group_by(subjectID) |>
  mutate(
    prior_status2 = case_when(
      endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_raw == 0, na.rm = TRUE), "naive", 
                                         if_else(any(PCR_update == TRUE & prior_infections_est == 1, na.rm = TRUE), "2nd", "post-2nd")),
      !endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_est == 0, na.rm = TRUE), "naive", 
                                          if_else(any(PCR_update == TRUE & prior_infections_est == 1, na.rm = TRUE), "2nd", "post-2nd"))
    )
  ) |>
  ungroup()
 
#check_consistency <- dfthai |>
#  group_by(subjectID) |>
#  summarise(n_status = n_distinct(prior_status)) |>
#  filter(n_status > 1)
#print(check_consistency)

#create a new column to identify if recruited as neonates or otherwise
dfthai <- dfthai |>
  mutate(Group1 = if_else(endsWith(subjectID, "N"), "Neonate", "Adult"))


#create a column to identify if infection occur as an infant (<1y)
dfthai <- dfthai |>
  group_by(subjectID) |>
  mutate(Group2 = if_else(any(PCR_update == T & ageAtCollection < 1, na.rm = T), "Infant","Adult")) |>
  ungroup()

#check_consistency <- dfthai |>
#  group_by(subjectID) |>
#  summarise(n_group2 = n_distinct(group2)) |>
#  filter(n_group2 > 1)
#print(check_consistency)


#create a column to mirror original "time5", days from pcr_infection
##first convert character to date
dfthai$collection_date <- as.Date(dfthai$collection_date, format = "%Y-%m-%d")
dfthai$datePcrInfection_1 <- as.Date(dfthai$datePcrInfection_1, format = "%Y-%m-%d")

dfthai <- dfthai |>
  mutate(time5 = as.numeric(collection_date - datePcrInfection_1))


#pivot to wide format to isolate out ZIKV value
dfthai <- dfthai |>
  distinct(subjectID, visitnum, antigen, .keep_all = T) |>
  pivot_wider(names_from = antigen, values_from = signal)
dim(dfthai) #201 #36

#exclude the earlier value prior to infection
##dfthai_KFCS2 <- dfthai_KFCS %>%
##  filter(time4 %in% c("-1", "0", "0.1", "0.2", "1", NA))

dfthai <- dfthai |>
  group_by(subjectID) |>
  filter(row_number() >= max(1, min(which(PCR_update == T), na.rm = T) - 2)) |>
  ungroup()

#subset into different df by naive vs exposed
dfthai_naive <- dfthai %>% filter(prior_status=="naive")
dfthai_exposed <- dfthai %>% filter(prior_status=="exposed")

#subset into different df by naive vs 2nd vs post-2nd
dfthai_naive2 <- dfthai |> filter(prior_status2=="naive")
dfthai_secondary <- dfthai |> filter(prior_status2=="2nd")
dfthai_postsec <- dfthai |> filter(prior_status2=="post-2nd")
  
  


#dfthai_KFCS_prim2 <- dfthai_KFCS2 %>% filter(infection=="Primary")
#dfthai_KFCS_second2 <- dfthai_KFCS2 %>% filter(infection=="Secondary")


########################################

dfthai_DHIM <- read_excel(#"DHIM/
  "MSD DHIM 03APR24_clean.xlsx")
TRDP$Group1 <- "Adult"
TRDP$Group2 <- "Adult"
Emory$Group1 <- "Adult"
Emory$Group2 <- "Adult"
dfthai_DHIM$Group1 <- "Adult"
dfthai_DHIM$Group2 <- "Adult"
#dfthai_KFCS_second2$Group <- "Adult"
Group1 <- c("black", "red")
Group2 <- c("black", "red")

#### FIGURA 1 - ZKNS1 ----
df_plot = rbind(TRDP %>% rename(subject=project_id) %>% mutate(dataset="Brazilian\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2) ,
                Emory %>% rename(Dia=DPSO) %>% mutate(dataset="United States\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                #dfthai_KFCS_prim2 %>% rename(Dia=time5) %>% mutate(dataset="Thai cases\n1° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group),
                dfthai_naive %>% rename(Dia=time5, ZKNS1=ZIKV, subject=subjectID) %>% mutate(dataset="Thai cases\n1° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                #dfthai_KFCS_second2 %>% rename(Dia=time5) %>% mutate(dataset="Thai cases\n2° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group),
                dfthai_exposed %>% rename(Dia=time5, ZKNS1=ZIKV, subject=subjectID) %>% mutate(dataset="Thai cases\n2° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                dfthai_DHIM %>% rename(Dia=studyday) %>% mutate(dataset="Dengue Human\nInfection model") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2)) %>% 
  mutate(dataset=factor(dataset, levels=c("Brazilian\nZIKV infection","United States\nZIKV infection","Thai cases\n1° DENV infection","Thai cases\n2° DENV infection","Dengue Human\nInfection model")))

#group1
p1 <- df_plot %>% 
  ggplot(aes(x = Dia, y = ZKNS1, group = subject, color = Group1)) +  # Adicione color = Group aqui
  geom_line(color = "gray") + 
  geom_point(size = 1, alpha = 0.8, shape = 1, stroke =1) + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol = 1, strip.position = "left") +
  scale_x_continuous(
    limits = c(-360, 3000),
    breaks = c(seq(-360, -180, by = 180), seq(-90, 360, by = 90), seq(810, 900, by = 90), seq(2100, 3080, by = 720))
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5)
  ) + 
  scale_x_break(c(-170, -100), scales = 10) +
  scale_x_break(c(450, 800), scales = 1) +
  scale_x_break(c(930, 2000), scales = 1) +
  scale_color_manual(values = Group1)  # aqui você define as cores dos grupos

p1
ggplotly(p1)
ggsave(p1, file="figures/option_1.png", width = 7, height = 10)


View(df_plot |>
  filter(dataset == "Thai cases\n2° DENV infection")) |>
  dplyr::group_by(dataset) |>
  dplyr::summarize(range = range(Dia, na.rm = T))


#group2
p1_g2 <- df_plot %>% 
  ggplot(aes(x = Dia, y = ZKNS1, group = subject, color = Group2)) +  # Adicione color = Group aqui
  geom_point(size = 2) + 
  geom_line(color = "gray") + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol = 1, strip.position = "left") +
  scale_x_continuous(
    limits = c(-360, 2900),
    breaks = c(seq(-360, -180, by = 180), seq(-90, 360, by = 90), seq(810, 900, by = 90), seq(2100, 3080, by = 720))
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) + 
  scale_x_break(c(-170, -100), scales = 10) +
  scale_x_break(c(360, 800), scales = 1) +
  scale_x_break(c(930, 2000), scales = 1) +
  scale_color_manual(values = Group2)  # aqui você define as cores dos grupos

p1_g2
ggsave(p1, file="figures/p1_g2.png", width = 7, height = 10)

##################
#6panels style
df_plot2 = rbind(TRDP %>% rename(subject=project_id) %>% mutate(dataset="Brazilian\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2) ,
                Emory %>% rename(Dia=DPSO) %>% mutate(dataset="United States\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                dfthai_naive2 %>% rename(Dia=time5, ZKNS1=ZIKV, subject=subjectID) %>% mutate(dataset="Thai cases\n1° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                dfthai_secondary %>% rename(Dia=time5, ZKNS1=ZIKV, subject=subjectID) %>% mutate(dataset="Thai cases\n2° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                dfthai_postsec %>% rename(Dia=time5, ZKNS1=ZIKV, subject=subjectID) %>% mutate(dataset="Thai cases\npost 2° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2),
                dfthai_DHIM %>% rename(Dia=studyday) %>% mutate(dataset="Dengue Human\nInfection model") %>% select(Dia, ZKNS1, subject, dataset, Group1, Group2)) %>% 
  mutate(dataset=factor(dataset, levels=c("Brazilian\nZIKV infection","United States\nZIKV infection","Thai cases\n1° DENV infection","Thai cases\n2° DENV infection","Thai cases\npost 2° DENV infection","Dengue Human\nInfection model")))


#group1
p1.2 <- df_plot2 %>% 
  ggplot(aes(x = Dia, y = ZKNS1, group = subject, color = Group1)) +  # Adicione color = Group aqui
  geom_line(color = "gray") + 
  geom_point(size = 1, alpha = 0.8, shape = 1, stroke =1) + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol = 1, strip.position = "left") +
  scale_x_continuous(
    limits = c(-360, 2900),
    breaks = c(seq(-360, -180, by = 180), seq(-90, 360, by = 90), seq(810, 900, by = 90), seq(2100, 3080, by = 720))
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5)
  ) + 
  scale_x_break(c(-170, -100), scales = 10) +
  scale_x_break(c(450, 800), scales = 1) +
  scale_x_break(c(930, 2000), scales = 1) +
  scale_color_manual(values = Group1)  # aqui você define as cores dos grupos

p1.2
ggsave(p1.2, file="figures/option_2.png", width = 7, height = 10)




p2 = df_plot %>% ggplot(aes(x = Dia, y = ZKNS1, group = subject)) +
  geom_point(size = 2) + 
  geom_line(color = "gray") + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol=1, strip.position = "left")+
  scale_x_continuous(
    limits = c(-360, 2900),
    breaks = c(-330,seq(-100,500,by=50),500,875,2875),
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.7))+ 
  scale_x_break(c(-240, -100), scales = 15) +
  scale_x_break(c(400, 475), scales = 1) +
  scale_x_break(c(510, 850), scales=1) +
  scale_x_break(c(900, 2820), scales=1)
p2
ggsave(p2, file="option_2.png", width = 7, height = 10)


p3 = df_plot %>% ggplot(aes(x = Dia, y = ZKNS1, group = subject)) +
  geom_point(size = 2) + 
  geom_line(color = "gray") + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol=1, strip.position = "left")+
  scale_x_continuous(
    limits = c(-360, 3000),
    breaks = c(-300,-250,seq(-100,150,by=50),500,3000)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.7))+ 
  scale_x_break(c(-200, -100), scales = 15, expand=F) +
  scale_x_break(c(200, 210), scales=1, expand=F) +
  xlab("Time (days)")

p3
ggsave(p3, file="option_3.png", width = 7, height = 10)


p4 = df_plot %>% ggplot(aes(x = Dia, y = ZKNS1, group = subject)) +
  geom_point(size = 2) + 
  geom_line(color = "gray") + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10, 1000000)
  ) +
  facet_wrap(~dataset, ncol=1, strip.position = "left")+
  scale_x_continuous(
    limits = c(-360, 3000),
    breaks = c(-300,seq(-100,150,by=50),500,2900)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.7))+ 
  scale_x_break(c(-200, -100), scales = 15, expand=F) +
  scale_x_break(c(200, 210), scales=1, expand=F) +
  scale_x_break(c(1000, 2800), scales=1, expand=F) +
  xlab("Time (days)")

p4
ggsave(p4, file="option_4.png", width = 7, height = 10)

