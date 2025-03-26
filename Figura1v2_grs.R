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
setwd("G:/My Drive/Yale/Analises/Yale/Zika project/Samples/Database/ajustadas/Bases para compartilhar/Bases de dados do artigo do MSD/")

TRDP <- read_excel("Brazilian ZIKV PCR positive/ZIKV_NS1_09JUL24_clean2.xlsx")

Emory <- read_excel("USA ZIKV PCR positive/Emory ZIKV MSD 11MAY24.xlsx")

dfthai_KFCS <- read_excel("Thai cohort/KFCS MSD IgG 03APR24_clean.xlsx")

#freq1 <- dfthai %>% filter(!is.na(ZKNS1)) %>% group_by(Subject) %>% dplyr ::summarise(count=n())

summary(as.factor(dfthai_KFCS$time4))
dfthai_KFCS$time4 <- factor(dfthai_KFCS$time4, levels=c(-4, -3, -2, -1, 0, 0.1, 0.2, 1,2,3,4))

subjects_dfthai_KFCS <- dfthai_KFCS %>%
  group_split(subject)

infection_dfthai_KFCS <- dfthai_KFCS %>%
  group_split(infecting_serotype, infection)

dfthai_KFCS_prim <- dfthai_KFCS %>% filter(infection=="Primary")
dfthai_KFCS_second <- dfthai_KFCS %>% filter(infection=="Secondary")

dfthai_KFCS2 <- dfthai_KFCS %>%
  filter(time4 %in% c("-1", "0", "0.1", "0.2", "1", NA))

dfthai_KFCS_prim2 <- dfthai_KFCS2 %>% filter(infection=="Primary")
dfthai_KFCS_second2 <- dfthai_KFCS2 %>% filter(infection=="Secondary")

dfthai_DHIM <- read_excel("DHIM/MSD DHIM 03APR24_clean.xlsx")
TRDP$Group <- "Adult"
Emory$Group <- "Adult"
dfthai_DHIM$Group <- "Adult"
dfthai_KFCS_second2$Group <- "Adult"
Group <- c("black", "red")
#### FIGURA 1 - ZKNS1 ----
df_plot = rbind(TRDP %>% rename(subject=project_id) %>% mutate(dataset="Brazilian\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group) ,
                Emory %>% rename(Dia=DPSO) %>% mutate(dataset="United States\nZIKV infection") %>% select(Dia, ZKNS1, subject, dataset, Group),
                dfthai_KFCS_prim2 %>% rename(Dia=time5) %>% mutate(dataset="Thai cases\n1° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group),
                dfthai_KFCS_second2 %>% rename(Dia=time5) %>% mutate(dataset="Thai cases\n2° DENV infection") %>% select(Dia, ZKNS1, subject, dataset, Group),
                dfthai_DHIM %>% rename(Dia=studyday) %>% mutate(dataset="Dengue Infection\nHuman model") %>% select(Dia, ZKNS1, subject, dataset, Group)) %>% 
  mutate(dataset=factor(dataset, levels=c("Brazilian\nZIKV infection","United States\nZIKV infection","Thai cases\n1° DENV infection","Thai cases\n2° DENV infection","Dengue Infection\nHuman model")))


p1 <- df_plot %>% 
  ggplot(aes(x = Dia, y = ZKNS1, group = subject, color = Group)) +  # Adicione color = Group aqui
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
  scale_color_manual(values = Group)  # aqui você define as cores dos grupos

p1
ggsave(p1, file="option_1.png", width = 7, height = 10)

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

