#Clear existing data and graphics
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


### Figura Coorte de Pau da LIma ----

dfL37_L44 <- read_excel("validacao cate/DenV IgG SUmmary- Plates 3-25_clean.xlsx", sheet = "clean")
#dfL37_L44 <- read_excel("validacao cate/08-05-2024 DenV Plate 3-4-5-6 L37-L44 sera.xlsx")

dfL37_L44$idnova2 <- as.numeric(dfL37_L44$idnova)

dfL37_L44 <- dfL37_L44 %>% filter(is.na(duplicado))

freq<- dfL37_L44 %>% group_by(idnova) %>% dplyr ::summarise(count=n())
write.table(freq, file = "Lista/freq_testadas.csv", row.names = F, sep = ",", na = "")


data6 <- read_delim("Lista/data6.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

data8 <- data6 %>% select(idnova, grupo_MSDZika, denviggt_titers_l37, zikvigg3posn2_l37, zikvigg3posn2_l38)

dfL37_L44v2 <- merge(dfL37_L44, data8, by.x = "idnova2", by.y = "idnova", all.x = T)

summary(as.factor(dfL37_L44v2$grupo_MSDZika))

verificar <- dfL37_L44v2 %>% filter(is.na(grupo_MSDZika))

summary(as.factor(dfL37_L44v2$repeticao))

############# verficar aumento nos títulos L37 até L49

dfL37_L44v2 <- dfL37_L44v2 %>%
  mutate(Means_MSD = rowMeans(select(., c(D1NS1, D2NS1, D3NS1, D4NS1))))

dfL37_L44v2 <- dfL37_L44v2 %>%
  mutate(Max_MSD = pmax(D1NS1, D2NS1, D3NS1, D4NS1, na.rm = TRUE))

dfL37_L44v2 <- dfL37_L44v2 #%>% filter(is.na(repeticao))
D1NS1 <- dfL37_L44v2 %>% dplyr::select(idnova, D1NS1, grupo_MSDZika, Cohort, `Sample ID`)
D1NS1$test <- "D1NS1"
names(D1NS1)[2] <- "Results"

D2NS1 <- dfL37_L44v2 %>% dplyr::select(idnova, D2NS1, grupo_MSDZika, Cohort, `Sample ID`)
D2NS1$test <- "D2NS1"
names(D2NS1)[2] <- "Results"

D3NS1 <- dfL37_L44v2 %>% dplyr::select(idnova, D3NS1, grupo_MSDZika, Cohort, `Sample ID`)
D3NS1$test <- "D3NS1"
names(D3NS1)[2] <- "Results"

D4NS1 <- dfL37_L44v2 %>% dplyr::select(idnova, D4NS1, grupo_MSDZika, Cohort, `Sample ID`)
D4NS1$test <- "D4NS1"
names(D4NS1)[2] <- "Results"

Means_MSD <- dfL37_L44v2 %>% dplyr::select(idnova, Means_MSD, grupo_MSDZika, Cohort, `Sample ID`)
Means_MSD$test <- "Means_MSD"
names(Means_MSD)[2] <- "Results"

Max_MSD <- dfL37_L44v2 %>% dplyr::select(idnova, Max_MSD, grupo_MSDZika, Cohort, `Sample ID`)
Max_MSD$test <- "Max_MSD"
names(Max_MSD)[2] <- "Results"

ZKNS1 <- dfL37_L44v2 %>% dplyr::select(idnova, ZKNS1, grupo_MSDZika, Cohort, `Sample ID`)
ZKNS1$test <- "ZKNS1"
names(ZKNS1)[2] <- "Results"

JENS1 <- dfL37_L44v2 %>% dplyr::select(idnova, JENS1, grupo_MSDZika, Cohort, `Sample ID`)
JENS1$test <- "JENS1"
names(JENS1)[2] <- "Results"

results_comp <- rbind(ZKNS1, D1NS1, D2NS1, D3NS1, D4NS1, Means_MSD, Max_MSD, JENS1)

summary(as.factor(results_comp$Cohort))
summary(as.factor(results_comp$test))

# Definir a ordem dos níveis para a variável 'test'
results_comp$test <- factor(results_comp$test, levels = c("ZKNS1","D1NS1", "D2NS1", "D3NS1", "D4NS1",  "Means_MSD",  "Max_MSD", "JENS1" ))
results_comp$Cohort <- factor(results_comp$Cohort, levels = c("L37", "L38", "L39", "L40",  "L41", "L42", "L43", "L44", "L45", "L46", "L47", "L48", "L49" ))

#### Seleção de amostras do L37 até o L44
L37 <- read_csv("Lista/Seguimento L37 - L44/L37VSoroinquerito-PabloTakedaL37_DATA_2024-07-19_1204.csv")
L38 <- read_csv("Lista/Seguimento L37 - L44/L38VISoroinquerito-Takeda_DATA_2024-07-19_1206.csv")
L39 <- read_csv("Lista/Seguimento L37 - L44/L39VIISoroinquerito-PabloSeguimentoZika_DATA_2024-07-19_1208.csv")
L40 <- read_csv("Lista/Seguimento L37 - L44/L40NovaCoorte-PabloSeguimentoZika_DATA_2024-07-19_1209.csv")
L41 <- read_csv("Lista/Seguimento L37 - L44/L41Soroinquerito-PabloSeguimentoZika_DATA_2024-07-19_1209.csv")
L42 <- read_csv("Lista/Seguimento L37 - L44/L42Soroinquerito-PabloSeguimentoZika_DATA_2024-07-19_1210.csv")
L43 <- read_csv("Lista/Seguimento L37 - L44/L43Soroinquerito-PabloSeguimentoZika_DATA_2024-07-19_1214.csv")
L44 <- read_csv("Lista/Seguimento L37 - L44/L44Soroinquerito-PabloSeguimentoZika_DATA_2024-07-19_1218.csv")
L45 <- read_delim("Lista/Seguimento L37 - L44/L45Soroinquerito-SeguimentoL45_DATA_2024-08-12_1754.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
L46 <- read_delim("Lista/Seguimento L37 - L44/L46Soroinquerito-SeguimentoL46_DATA_2024-08-12_1755.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
L47 <- read_delim("Lista/Seguimento L37 - L44/L47Soroinquerito-SeguimentoL47_DATA_2024-08-12_1755.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
L48 <- read_delim("Lista/Seguimento L37 - L44/L48Soroinquerito-SeguimentoL48_DATA_2024-08-12_1757.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
L49 <- read_delim("Lista/Seguimento L37 - L44/L49Soroinquerito-SeguimentoL49_DATA_2024-08-12_1804.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)


merged_data <- L37 %>%
  full_join(L38, by = "idnova", suffix = c(".L37", ".L38")) %>%
  full_join(L39, by = "idnova", suffix = c("", ".L39")) %>%
  full_join(L40, by = "idnova", suffix = c("", ".L40")) %>%
  full_join(L41, by = "idnova", suffix = c("", ".L41")) %>%
  full_join(L42, by = "idnova", suffix = c("", ".L42")) %>%
  full_join(L43, by = "idnova", suffix = c("", ".L43")) %>%
  full_join(L44, by = "idnova", suffix = c("", ".L44")) %>%
  full_join(L45, by = "idnova", suffix = c("", ".L45")) %>%
  full_join(L46, by = "idnova", suffix = c("", ".L46")) %>%
  full_join(L47, by = "idnova", suffix = c("", ".L47")) %>%
  full_join(L48, by = "idnova", suffix = c("", ".L48")) %>%
  full_join(L49, by = "idnova", suffix = c("", ".L49"))


merged_data$casanova.L39 <- merged_data$casanova
merged_data$dtcoleta.L39 <- merged_data$dtcoleta

dfL37 <- merged_data %>% filter(!is.na(dtcoleta.L37)) %>% select(idnova, dtcoleta.L37)
names(dfL37)[2] <- "data_coleta"
dfL37$coorte <- "L37"

dfL38 <- merged_data %>% filter(!is.na(dtcoleta.L38)) %>% select(idnova, dtcoleta.L38)
names(dfL38)[2] <- "data_coleta"
dfL38$coorte <- "L38"

dfL39 <- merged_data %>% filter(!is.na(dtcoleta.L39)) %>% select(idnova, dtcoleta.L39)
names(dfL39)[2] <- "data_coleta"
dfL39$coorte <- "L39"

dfL40 <- merged_data %>% filter(!is.na(dtcoleta.L40)) %>% select(idnova, dtcoleta.L40)
names(dfL40)[2] <- "data_coleta"
dfL40$coorte <- "L40"

dfL41 <- merged_data %>% filter(!is.na(dtcoleta.L41)) %>% select(idnova, dtcoleta.L41)
names(dfL41)[2] <- "data_coleta"
dfL41$coorte <- "L41"

dfL42 <- merged_data %>% filter(!is.na(dtcoleta.L42)) %>% select(idnova, dtcoleta.L42)
names(dfL42)[2] <- "data_coleta"
dfL42$coorte <- "L42"

dfL43 <- merged_data %>% filter(!is.na(dtcoleta.L43)) %>% select(idnova, dtcoleta.L43)
names(dfL43)[2] <- "data_coleta"
dfL43$coorte <- "L43"

dfL44 <- merged_data %>% filter(!is.na(dtcoleta.L44)) %>% select(idnova, dtcoleta.L44)
names(dfL44)[2] <- "data_coleta"
dfL44$coorte <- "L44"

dfL45 <- merged_data %>% filter(!is.na(dtcoleta.L45)) %>% select(idnova, dtcoleta.L45)
names(dfL45)[2] <- "data_coleta"
dfL45$coorte <- "L45"

dfL46 <- merged_data %>% filter(!is.na(dtcoleta.L46)) %>% select(idnova, dtcoleta.L46)
names(dfL46)[2] <- "data_coleta"
dfL46$coorte <- "L46"

dfL47 <- merged_data %>% filter(!is.na(dtcoleta.L47)) %>% select(idnova, dtcoleta.L47)
names(dfL47)[2] <- "data_coleta"
dfL47$coorte <- "L47"

dfL48 <- merged_data %>% filter(!is.na(dtcoleta.L48)) %>% select(idnova, dtcoleta.L48)
names(dfL48)[2] <- "data_coleta"
dfL48$coorte <- "L48"

dfL49 <- merged_data %>% filter(!is.na(dtcoleta.L49)) %>% select(idnova, dtcoleta.L49)
names(dfL49)[2] <- "data_coleta"
dfL49$coorte <- "L49"

# Criando um dataframe único a partir das coortes de L37 a L49
df_combined <- bind_rows(dfL37, dfL38, dfL39, dfL40, dfL41, dfL42, dfL43, dfL44, dfL45, dfL46, dfL47, dfL48, dfL49)

# Criando a variável que concatena a coorte e o idnova
df_combined <- df_combined %>%
  mutate(coorte_idnova = paste(coorte, idnova, sep = "-"))

write.table(df_combined, file = "Lista/df_combined.csv", row.names = F, sep = ",", na = "")

results_compv2 <- merge(results_comp, df_combined, by.x = "Sample ID", by.y = "coorte_idnova")
names(results_compv2)[2] <- "idnova"

write.table(results_compv2, file = "Lista/results_compv2.csv", row.names = F, sep = ",", na = "")
write.table(results_comp, file = "Lista/results_comp.csv", row.names = F, sep = ",", na = "")

### Abrir base ajustada Pau da Lima----
results_compv2 <- read_csv("Lista/results_compv2.csv")
results_compv2$ResultsLog <- log2(results_compv2$Results)

## Fold change

# Agora, calculamos as divisões apenas para os pares consecutivos presentes
dados_div <- results_compv2 %>%
  group_by(idnova, test) %>%
  arrange(Cohort) %>%  # Organiza os dados pela ordem das coortes
  mutate(
    # Cálculo das proporções
    prop_L38_L37 = ifelse("L38" %in% Cohort & "L37" %in% Cohort, 
                          Results[Cohort == "L38"] / Results[Cohort == "L37"], NA),
    prop_L38_L37Log = ifelse("L38" %in% Cohort & "L37" %in% Cohort, 
                             ResultsLog[Cohort == "L38"] - ResultsLog[Cohort == "L37"], NA),
    prop_L39_L38 = ifelse("L39" %in% Cohort & "L38" %in% Cohort, 
                          Results[Cohort == "L39"] / Results[Cohort == "L38"], NA),
    prop_L39_L38Log = ifelse("L39" %in% Cohort & "L38" %in% Cohort, 
                             ResultsLog[Cohort == "L39"] - ResultsLog[Cohort == "L38"], NA),
    prop_L40_L39 = ifelse("L40" %in% Cohort & "L39" %in% Cohort, 
                          Results[Cohort == "L40"] / Results[Cohort == "L39"], NA),
    prop_L40_L39Log = ifelse("L40" %in% Cohort & "L39" %in% Cohort, 
                             ResultsLog[Cohort == "L40"] - ResultsLog[Cohort == "L39"], NA),
    prop_L41_L40 = ifelse("L41" %in% Cohort & "L40" %in% Cohort, 
                          Results[Cohort == "L41"] / Results[Cohort == "L40"], NA),
    prop_L41_L40Log = ifelse("L41" %in% Cohort & "L40" %in% Cohort, 
                             ResultsLog[Cohort == "L41"] - ResultsLog[Cohort == "L40"], NA),
    prop_L42_L41 = ifelse("L42" %in% Cohort & "L41" %in% Cohort, 
                          Results[Cohort == "L42"] / Results[Cohort == "L41"], NA),
    prop_L42_L41Log = ifelse("L42" %in% Cohort & "41" %in% Cohort, 
                             ResultsLog[Cohort == "L42"] - ResultsLog[Cohort == "L41"], NA),
    prop_L43_L42 = ifelse("L43" %in% Cohort & "L42" %in% Cohort, 
                          Results[Cohort == "L43"] / Results[Cohort == "L42"], NA),
    prop_L43_L42Log = ifelse("L43" %in% Cohort & "L42" %in% Cohort, 
                             ResultsLog[Cohort == "L43"] - ResultsLog[Cohort == "L42"], NA),
    prop_L44_L43 = ifelse("L44" %in% Cohort & "L43" %in% Cohort, 
                          Results[Cohort == "L44"] / Results[Cohort == "L43"], NA),
    prop_L44_L43Log = ifelse("L44" %in% Cohort & "L43" %in% Cohort, 
                             ResultsLog[Cohort == "L44"] - ResultsLog[Cohort == "L43"], NA),
    prop_L45_L44 = ifelse("L45" %in% Cohort & "L44" %in% Cohort, 
                          Results[Cohort == "L45"] / Results[Cohort == "L44"], NA),
    prop_L45_L44Log = ifelse("L45" %in% Cohort & "L44" %in% Cohort, 
                             ResultsLog[Cohort == "L45"] - ResultsLog[Cohort == "L44"], NA),
    prop_L46_L45 = ifelse("L46" %in% Cohort & "L45" %in% Cohort, 
                          Results[Cohort == "L46"] / Results[Cohort == "L45"], NA),
    prop_L46_L45Log = ifelse("L46" %in% Cohort & "L45" %in% Cohort, 
                             ResultsLog[Cohort == "L46"] - ResultsLog[Cohort == "L45"], NA),
    prop_L47_L46 = ifelse("L47" %in% Cohort & "L46" %in% Cohort, 
                          Results[Cohort == "L47"] / Results[Cohort == "L46"], NA),
    prop_L47_L46Log = ifelse("L47" %in% Cohort & "L46" %in% Cohort, 
                             ResultsLog[Cohort == "L47"] - ResultsLog[Cohort == "L46"], NA),
    prop_L48_L47 = ifelse("L48" %in% Cohort & "L47" %in% Cohort, 
                          Results[Cohort == "L48"] / Results[Cohort == "L47"], NA),
    prop_L48_L47Log = ifelse("L48" %in% Cohort & "L47" %in% Cohort, 
                             ResultsLog[Cohort == "L48"] - ResultsLog[Cohort == "L47"], NA),
    prop_L49_L48 = ifelse("L49" %in% Cohort & "L48" %in% Cohort, 
                          Results[Cohort == "L49"] / Results[Cohort == "L48"], NA),
    prop_L49_L48Log = ifelse("L49" %in% Cohort & "L48" %in% Cohort, 
                             ResultsLog[Cohort == "L49"] - ResultsLog[Cohort == "L48"], NA),
    
    # Salvando a data do segundo inquérito e garantindo formato de data
    data_coleta_L38_L37 = as.Date(ifelse("L38" %in% Cohort & "L37" %in% Cohort, 
                                         data_coleta[Cohort == "L38"], NA)),
    data_coleta_L39_L38 = as.Date(ifelse("L39" %in% Cohort & "L38" %in% Cohort, 
                                         data_coleta[Cohort == "L39"], NA)),
    data_coleta_L40_L39 = as.Date(ifelse("L40" %in% Cohort & "L39" %in% Cohort, 
                                         data_coleta[Cohort == "L40"], NA)),
    data_coleta_L41_L40 = as.Date(ifelse("L41" %in% Cohort & "L40" %in% Cohort, 
                                         data_coleta[Cohort == "L41"], NA)),
    data_coleta_L42_L41 = as.Date(ifelse("L42" %in% Cohort & "L41" %in% Cohort, 
                                         data_coleta[Cohort == "L42"], NA)),
    data_coleta_L43_L42 = as.Date(ifelse("L43" %in% Cohort & "L42" %in% Cohort, 
                                         data_coleta[Cohort == "L43"], NA)),
    data_coleta_L44_L43 = as.Date(ifelse("L44" %in% Cohort & "L43" %in% Cohort, 
                                         data_coleta[Cohort == "L44"], NA)),
    data_coleta_L45_L44 = as.Date(ifelse("L45" %in% Cohort & "L44" %in% Cohort, 
                                         data_coleta[Cohort == "L45"], NA)),
    data_coleta_L46_L45 = as.Date(ifelse("L46" %in% Cohort & "L45" %in% Cohort, 
                                         data_coleta[Cohort == "L46"], NA)),
    data_coleta_L47_L46 = as.Date(ifelse("L47" %in% Cohort & "L46" %in% Cohort, 
                                         data_coleta[Cohort == "L47"], NA)),
    data_coleta_L48_L47 = as.Date(ifelse("L48" %in% Cohort & "L47" %in% Cohort, 
                                         data_coleta[Cohort == "L48"], NA)),
    data_coleta_L49_L48 = as.Date(ifelse("L49" %in% Cohort & "L48" %in% Cohort, 
                                         data_coleta[Cohort == "L49"], NA))
  ) %>%
  ungroup()
    

write.table(dados_div, file = "Lista/dados_div.csv", row.names = F, sep = ",", na = "")

dados_div_l38 <- dados_div %>% filter(Cohort=="L38")

# individuos_com_aumento2.54 <- dados_div_l38 %>% filter(test=="ZKNS1") %>% 
#   filter(
#       prop_L39_L38 >= 2.54 |
#       prop_L40_L39 >= 2.54 |
#       prop_L41_L40 >= 2.54 |
#       prop_L42_L41 >= 2.54 |
#       prop_L43_L42 >= 2.54 |
#       prop_L44_L43 >= 2.54 |
#       prop_L45_L44 >= 2.54 |
#       prop_L46_L45 >= 2.54 |
#       prop_L47_L46 >= 2.54 |
#       prop_L48_L47 >= 2.54 |
#       prop_L49_L48 >= 2.54
#   ) %>%
#   select(idnova) %>%
#   distinct()
# individuos_com_aumento2.54$aumento2.5 <- 1
# 
# individuos_com_aumento4 <- dados_div_l38 %>% filter(test=="ZKNS1") %>% 
#   filter(
#     prop_L39_L38 >= 4 |
#       prop_L40_L39 >= 4 |
#       prop_L41_L40 >= 4 |
#       prop_L42_L41 >= 4 |
#       prop_L43_L42 >= 4 |
#       prop_L44_L43 >= 4 |
#       prop_L45_L44 >= 4 |
#       prop_L46_L45 >= 4 |
#       prop_L47_L46 >= 4 |
#       prop_L48_L47 >= 4 |
#       prop_L49_L48 >= 4
#   ) %>%
#   #select(idnova,grupo_MSDZika, starts_with("prop_")) %>%
#   select(idnova) %>%
#   distinct()

individuos_com_aumento1.26 <- dados_div_l38 %>% filter(test=="ZKNS1") %>% 
  filter(
    prop_L39_L38Log >= 1.26 |
      prop_L40_L39Log >= 1.26 |
      prop_L41_L40Log >= 1.26 |
      prop_L42_L41Log >= 1.26 |
      prop_L43_L42Log >= 1.26 |
      prop_L44_L43Log >= 1.26 |
      prop_L45_L44Log >= 1.26 |
      prop_L46_L45Log >= 1.26 |
      prop_L47_L46Log >= 1.26 |
      prop_L48_L47Log >= 1.26 |
      prop_L49_L48Log >= 1.26
  ) %>%
  select(idnova) %>%
  distinct()
individuos_com_aumento1.26$aumento1.26 <- 1

individuos_com_aumento1.89 <- dados_div_l38 %>% filter(test=="ZKNS1") %>% 
  filter(
    prop_L39_L38Log >= 1.89 |
      prop_L40_L39Log >= 1.89 |
      prop_L41_L40Log >= 1.89 |
      prop_L42_L41Log >= 1.89 |
      prop_L43_L42Log >= 1.89 |
      prop_L44_L43Log >= 1.89 |
      prop_L45_L44Log >= 1.89 |
      prop_L46_L45Log >= 1.89 |
      prop_L47_L46Log >= 1.89 |
      prop_L48_L47Log >= 1.89 |
      prop_L49_L48Log >= 1.89
  ) %>%
  #select(idnova,grupo_MSDZika, starts_with("prop_")) %>%
  select(idnova) %>%
  distinct()

individuos_com_aumento1.89$aumento1.89 <- 1

individuos_com_aumento <- merge(individuos_com_aumento1.26, individuos_com_aumento1.89, by = "idnova", all.x = T)

individuos_com_aumento$aumento1.26[individuos_com_aumento$aumento1.89==1] <- NA
summary(as.factor(individuos_com_aumento$aumento1.26))

write.table(individuos_com_aumento, file = "Lista/individuos_com_aumento.csv", row.names = F, sep = ",", na = "")

#results_compv3 <- merge(results_compv2, individuos_com_aumento2.54_zika, by = "idnova")

results_compv4 <- merge(results_compv2, individuos_com_aumento, by = "idnova")

com_aumento4_zika <- results_compv4 %>% filter(Cohort=="L38")

write.table(results_compv4, file = "Lista/results_compv4.csv", row.names = F, sep = ",", na = "")

###### Base ajustada
results_compv4 <- read_csv("Lista/results_compv4.csv")

results_compv4$test <- factor(results_compv4$test, levels = c("ZKNS1","D1NS1", "D2NS1", "D3NS1", "D4NS1",  "Means_MSD",  "Max_MSD", "JENS1" ))

num_col <- 1  # Número de colunas por página
num_row <- 1  # Número de linhas por página

# results_compv4_4 <- results_compv4 %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento4==1)
# results_compv4_2.5 <- results_compv4 %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento2.5==1)

results_compv4_4 <- results_compv4 %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento1.89==1)
results_compv4_2.5 <- results_compv4 %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento1.26==1)


g1a4 <- results_compv4_4 %>% filter(grupo_MSDZika==1)
g2a4 <- results_compv4_4 %>% filter(grupo_MSDZika==2)
g3a4 <- results_compv4_4 %>% filter(grupo_MSDZika==3)
g4a4 <- results_compv4_4 %>% filter(grupo_MSDZika==4)

# Número de páginas necessárias
total_pages1a4 <- ceiling(length(unique(g1a4$idnova)) / (num_col * num_row))
total_pages2a4 <- ceiling(length(unique(g2a4$idnova)) / (num_col * num_row))
total_pages3a4 <- ceiling(length(unique(g3a4$idnova)) / (num_col * num_row))
total_pages4a4 <- ceiling(length(unique(g4a4$idnova)) / (num_col * num_row))

g1a2 <- results_compv4_2.5 %>% filter(grupo_MSDZika==1)
g2a2 <- results_compv4_2.5 %>% filter(grupo_MSDZika==2)
g3a2 <- results_compv4_2.5 %>% filter(grupo_MSDZika==3)
g4a2 <- results_compv4_2.5 %>% filter(grupo_MSDZika==4)

# Número de páginas necessárias
total_pages1a2 <- ceiling(length(unique(g1a2$idnova)) / (num_col * num_row))
total_pages2a2 <- ceiling(length(unique(g2a2$idnova)) / (num_col * num_row))
total_pages3a2 <- ceiling(length(unique(g3a2$idnova)) / (num_col * num_row))
total_pages4a2 <- ceiling(length(unique(g4a2$idnova)) / (num_col * num_row))

plots <- lapply(1:total_pages1a4, function(page) {
  ggplot(g1a4, 
         aes(x = data_coleta, y = Results, color = test, group = interaction(idnova, test))) +
    geom_point(size = 2) + 
    geom_line() +  
    scale_color_manual(name = "", values = c("red","purple", "blue", "green", "orange",  "pink"),
                       labels = c("ZKNS1" = "ZIKV", "D1NS1" = "DENV1", "D2NS1" = "DENV2", 
                                  "D3NS1" = "DENV3", "D4NS1" = "DENV4", "Outros" = "Other Antigen")) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(10, 10000000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-01-01", "2024-03-31"))  # Ajuste essas datas conforme necessário
    ) +
    labs(
      x = "",
      y = ""
    ) +
    geom_hline(yintercept = 10^3, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    theme_minimal() +
    theme(
      legend.position = "none") +  # Correção do erro na posição da legenda
    ggforce::facet_wrap_paginate(~ idnova, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})


plots <- lapply(1:total_pages1a4, function(page) {
  ggplot(g1a4, 
         aes(x = data_coleta, y = ResultsLog, color = test, group = interaction(idnova, test))) +
    geom_point(size = 2) + 
    geom_line() +  
    scale_color_manual(
      name = "", 
      values = c("red", "purple", "blue", "green", "orange", "pink"),
      labels = c("ZKNS1" = "ZIKV", "D1NS1" = "DENV1", "D2NS1" = "DENV2", 
                 "D3NS1" = "DENV3", "D4NS1" = "DENV4", "Outros" = "Other Antigen")
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),  # Quebras automáticas legíveis
      labels = function(x) paste0("2^", x),  # Rótulos com notação de potência
      limits = c(2, 24)  # Ajuste conforme a escala dos seus dados em log2
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),
      limits = as.Date(c("2015-01-01", "2024-03-31"))
    ) +
    labs(
      x = "Ano",
      y = "Concentração (log2)"
    ) +
    geom_hline(yintercept = log2(1000), linetype = "dashed", color = "black") +  # Linha de referência ajustada
    theme_minimal() +
    theme(
      legend.position = "right"
    ) +
    ggforce::facet_wrap_paginate(~ idnova, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})

# Exibir todas as páginas
plots #tamanho 1060 x 254

# 6. Reorganizar os dados: todos os valores de ratio em uma coluna e outra coluna indicando a que ratio pertence
dados_div_l38_zk <- merge(dados_div_l38, individuos_com_aumento, by = "idnova")

# Combinar as colunas de proporções e datas para as mesmas comparações
df_long_fold_change_combined <- dados_div_l38_zk %>%
  pivot_longer(
    cols = starts_with("prop_"), 
    names_to = "visit_comparison", 
    values_to = "prop_value"
  ) %>%
  mutate(visit_suffix = str_replace(visit_comparison, "prop_", "")) %>% # Extrai o sufixo das visitas (ex: "2_1")
  
  pivot_longer(
    cols = starts_with("data_coleta_"), 
    names_to = "date_comparison", 
    values_to = "prop_date"
  ) %>%
  mutate(date_suffix = str_replace(date_comparison, "data_coleta_", "")) %>% # Extrai o sufixo das datas correspondentes
  
  filter(visit_suffix == date_suffix) %>%  # Mantém apenas as linhas com o mesmo sufixo para visita e data
  select(-visit_suffix, -date_suffix)  # Remove as colunas auxiliares com sufixo




df_long_fold_change_4 <- df_long_fold_change_combined %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento1.89==1)
df_long_fold_change_2.5 <- df_long_fold_change_combined %>% filter(test != "JENS1" & test != "Means_MSD" & test != "Max_MSD" & aumento1.26==1)


g1fc4 <- df_long_fold_change_4 %>% filter(grupo_MSDZika==1 & !is.na(prop_value))
g2fc4 <- df_long_fold_change_4 %>% filter(grupo_MSDZika==2 & !is.na(prop_value))
g3fc4 <- df_long_fold_change_4 %>% filter(grupo_MSDZika==3 & !is.na(prop_value))
#g4fc4 <- df_long_fold_change_4 %>% filter(grupo_MSDZika==4)

# Número de páginas necessárias
total_pages1a4 <- ceiling(length(unique(g1fc4$idnova)) / (num_col * num_row))
total_pages2a4 <- ceiling(length(unique(g2fc4$idnova)) / (num_col * num_row))
total_pages3a4 <- ceiling(length(unique(g3fc4$idnova)) / (num_col * num_row))
#total_pages4a4 <- ceiling(length(unique(g4fc4$idnova)) / (num_col * num_row))

g1fc2 <- df_long_fold_change_2.5 %>% filter(grupo_MSDZika==1 & !is.na(prop_value))
g2fc2 <- df_long_fold_change_2.5 %>% filter(grupo_MSDZika==2 & !is.na(prop_value))
g3fc2 <- df_long_fold_change_2.5 %>% filter(grupo_MSDZika==3 & !is.na(prop_value))
g4fc2 <- df_long_fold_change_2.5 %>% filter(grupo_MSDZika==4 & !is.na(prop_value))

# Número de páginas necessárias
total_pages1a2 <- ceiling(length(unique(g1fc2$idnova)) / (num_col * num_row))
total_pages2a2 <- ceiling(length(unique(g2fc2$idnova)) / (num_col * num_row))
total_pages3a2 <- ceiling(length(unique(g3fc2$idnova)) / (num_col * num_row))
total_pages4a2 <- ceiling(length(unique(g4fc2$idnova)) / (num_col * num_row))

#labelsPL <- c("2/1", "3/2", "4/3", "5/4", "6/5", "7/6", "8/7", "9/8", "10/9", "11/10", "12/11", "13/12")

# Gerar cada página separadamente
plots <- lapply(1:total_pages1a4, function(page) {
  ggplot(g1fc4, 
         aes(x = prop_date, y = prop_value, color = test, group = interaction(idnova, test))) +
    geom_point(size = 2) + 
    geom_line() +  # Remove a cor preta para usar as cores baseadas no 'test'
    #scale_x_discrete(labels = labelsPL) +
    scale_color_manual(
      name = "",
      values = c("red", "purple", "blue", "green", "orange", "pink"),
      labels = c(
        "ZIKV" = "ZIKV", 
        "D1" = "DENV1", 
        "D2" = "DENV2", 
        "D3" = "DENV3", 
        "D4" = "DENV4", 
        "Outros" = "Other Antigen"
      )
    ) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(0.0001, 10000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-01-01", "2024-03-31"))  # Ajuste essas datas conforme necessário
    ) +
    geom_hline(yintercept = 2.54, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    labs(
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      legend.position = "none"  # Correção do erro na posição da legenda
    ) +
    ggforce::facet_wrap_paginate(~ idnova, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})

plots # Tamanho 1006 x 254

## Ratio Fold change

dados_div_zk_maxdenv <- dados_div_l38_zk %>%
  group_by(idnova) %>%
  mutate(
    prop_ZKV_MaxDENV_L38_L37 = prop_L38_L37[test == "ZKNS1"] / prop_L38_L37[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L39_L38 = prop_L39_L38[test == "ZKNS1"] / prop_L39_L38[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L40_L39 = prop_L40_L39[test == "ZKNS1"] / prop_L40_L39[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L41_L40 = prop_L41_L40[test == "ZKNS1"] / prop_L41_L40[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L42_L41 = prop_L42_L41[test == "ZKNS1"] / prop_L42_L41[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L43_L42 = prop_L43_L42[test == "ZKNS1"] / prop_L43_L42[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L44_L43 = prop_L44_L43[test == "ZKNS1"] / prop_L44_L43[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L45_L44 = prop_L45_L44[test == "ZKNS1"] / prop_L45_L44[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L46_L45 = prop_L46_L45[test == "ZKNS1"] / prop_L46_L45[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L47_L46 = prop_L47_L46[test == "ZKNS1"] / prop_L47_L46[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L48_L47 = prop_L48_L47[test == "ZKNS1"] / prop_L48_L47[test == "Max_MSD"],
    prop_ZKV_MaxDENV_L49_L48 = prop_L49_L48[test == "ZKNS1"] / prop_L49_L48[test == "Max_MSD"]
  ) %>%
  ungroup()


razao_foldchange <- dados_div_zk_maxdenv %>% filter(test=="ZKNS1")

# L38/L37
RFC_L38_L37 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L38_L37)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L38_L37, `Sample ID`, data_coleta_L38_L37)
RFC_L38_L37$razao <- "L38/L37"
names(RFC_L38_L37)[3] <- "Results"
names(RFC_L38_L37)[5] <- "data_coleta_ratio"

# L39/L38
RFC_L39_L38 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L39_L38)) %>% 
  dplyr::select(idnova, grupo_MSDZika,prop_ZKV_MaxDENV_L39_L38, `Sample ID`, data_coleta_L39_L38)
RFC_L39_L38$razao <- "L39/L38"
names(RFC_L39_L38)[3] <- "Results"
names(RFC_L39_L38)[5] <- "data_coleta_ratio"

# L40/L39
RFC_L40_L39 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L40_L39)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L40_L39, `Sample ID`, data_coleta_L40_L39)
RFC_L40_L39$razao <- "L40/L39"
names(RFC_L40_L39)[3] <- "Results"
names(RFC_L40_L39)[5] <- "data_coleta_ratio"

# L41/L40
RFC_L41_L40 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L41_L40)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L41_L40, `Sample ID`, data_coleta_L41_L40)
RFC_L41_L40$razao <- "L41/L40"
names(RFC_L41_L40)[3] <- "Results"
names(RFC_L41_L40)[5] <- "data_coleta_ratio"

# L42/L41
RFC_L42_L41 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L42_L41)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L42_L41, `Sample ID`, data_coleta_L42_L41)
RFC_L42_L41$razao <- "L42/L41"
names(RFC_L42_L41)[3] <- "Results"
names(RFC_L42_L41)[5] <- "data_coleta_ratio"

# L43/L42
RFC_L43_L42 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L43_L42)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L43_L42, `Sample ID`, data_coleta_L43_L42)
RFC_L43_L42$razao <- "L43/L42"
names(RFC_L43_L42)[3] <- "Results"
names(RFC_L43_L42)[5] <- "data_coleta_ratio"

# L44/L43
RFC_L44_L43 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L44_L43)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L44_L43, `Sample ID`, data_coleta_L44_L43)
RFC_L44_L43$razao <- "L44/L43"
names(RFC_L44_L43)[3] <- "Results"
names(RFC_L44_L43)[5] <- "data_coleta_ratio"

# L45/L44
RFC_L45_L44 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L45_L44)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L45_L44, `Sample ID`, data_coleta_L45_L44)
RFC_L45_L44$razao <- "L45/L44"
names(RFC_L45_L44)[3] <- "Results"
names(RFC_L45_L44)[5] <- "data_coleta_ratio"

# L46/L45
RFC_L46_L45 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L46_L45)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L46_L45, `Sample ID`, data_coleta_L46_L45)
RFC_L46_L45$razao <- "L46/L45"
names(RFC_L46_L45)[3] <- "Results"
names(RFC_L46_L45)[5] <- "data_coleta_ratio"

# L47/L46
RFC_L47_L46 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L47_L46)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L47_L46, `Sample ID`, data_coleta_L47_L46)
RFC_L47_L46$razao <- "L47/L46"
names(RFC_L47_L46)[3] <- "Results"
names(RFC_L47_L46)[5] <- "data_coleta_ratio"

# L48/L47
RFC_L48_L47 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L48_L47)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L48_L47, `Sample ID`, data_coleta_L48_L47)
RFC_L48_L47$razao <- "L48/L47"
names(RFC_L48_L47)[3] <- "Results"
names(RFC_L48_L47)[5] <- "data_coleta_ratio"

# L49/L48
RFC_L49_L48 <- razao_foldchange %>% 
  filter(!is.na(prop_ZKV_MaxDENV_L49_L48)) %>% 
  dplyr::select(idnova, grupo_MSDZika, prop_ZKV_MaxDENV_L49_L48, `Sample ID`, data_coleta_L49_L48)
RFC_L49_L48$razao <- "L49/L48"
names(RFC_L49_L48)[3] <- "Results"
names(RFC_L49_L48)[5] <- "data_coleta_ratio"


RFC_combined <- rbind(
  RFC_L38_L37,
  RFC_L39_L38,
  RFC_L40_L39,
  RFC_L41_L40,
  RFC_L42_L41,
  RFC_L43_L42,
  RFC_L44_L43,
  RFC_L45_L44,
  RFC_L46_L45,
  RFC_L47_L46,
  RFC_L48_L47,
  RFC_L49_L48
)

#results_compv5 <- results_compv4 %>% select(`Sample ID`, data_coleta)

RFC_combinedV2 <- merge(RFC_combined, individuos_com_aumento, by = "idnova")
#RFC_combinedV3 <- merge(RFC_combinedV2, results_compv5, by = "Sample ID")

write.table(RFC_combinedV2, file = "Lista/RFC_combinedV2.csv", row.names = F, sep = ",", na = "")

summary(as.factor(RFC_combinedV2$grupo_MSDZika))

RFC_combinedV2_4 <- RFC_combinedV2 %>% filter(aumento1.89==1)
RFC_combinedV2_2.5 <- RFC_combinedV2 %>% filter(aumento1.26==1)


g1rfc4 <- RFC_combinedV2_4 %>% filter(grupo_MSDZika==1)
g2rfc4 <- RFC_combinedV2_4 %>% filter(grupo_MSDZika==2)
g3rfc4 <- RFC_combinedV2_4 %>% filter(grupo_MSDZika==3)
#g4fc4 <- df_long_fold_change_4 %>% filter(grupo_MSDZika==4)

# Número de páginas necessárias
total_pages1a4 <- ceiling(length(unique(g1rfc4$idnova)) / (num_col * num_row))
total_pages2a4 <- ceiling(length(unique(g2rfc4$idnova)) / (num_col * num_row))
total_pages3a4 <- ceiling(length(unique(g3rfc4$idnova)) / (num_col * num_row))
#total_pages4a4 <- ceiling(length(unique(g4fc4$idnova)) / (num_col * num_row))

g1rfc2 <- RFC_combinedV2_2.5 %>% filter(grupo_MSDZika==1)
g2rfc2 <- RFC_combinedV2_2.5 %>% filter(grupo_MSDZika==2)
g3rfc2 <- RFC_combinedV2_2.5 %>% filter(grupo_MSDZika==3)
g4rfc2 <- RFC_combinedV2_2.5 %>% filter(grupo_MSDZika==4)

# Número de páginas necessárias
total_pages1a2 <- ceiling(length(unique(g1rfc2$idnova)) / (num_col * num_row))
total_pages2a2 <- ceiling(length(unique(g2rfc2$idnova)) / (num_col * num_row))
total_pages3a2 <- ceiling(length(unique(g3rfc2$idnova)) / (num_col * num_row))
total_pages4a2 <- ceiling(length(unique(g4rfc2$idnova)) / (num_col * num_row))

#labelsPL <- c("2/1", "3/2", "4/3", "5/4", "6/5", "7/6", "8/7", "9/8", "10/9", "11/10", "12/11", "13/12")

plots <- lapply(1:total_pages1a4, function(page) {
  ggplot(g1rfc4, 
         aes(x = data_coleta_ratio, y = Results, group = interaction(idnova))) +
    geom_point(size = 2, color = "red") +  # Define a cor dos pontos como vermelho
    geom_line(color = "red") +  # Define a cor das linhas como vermelho
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(0.0001, 10000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-01-01", "2024-03-31"))  # Ajuste essas datas conforme necessário
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    labs(
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      legend.position = "none"  # Correção do erro na posição da legenda
    ) +
    ggforce::facet_wrap_paginate(~ idnova, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})

plots # tamanho 1019 x 254


### Novas figuras da Tailândia----

dfthai_KFCS_new <- read_excel("Dados Tailandia/KFCS Mega Family Results IgG.xlsx")

KFCS_PCR_results <- read_delim("Dados Tailandia/KFCS illness episodes- PCR results – DHF Project.csv",
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

dfthai_KFCS_new <- merge(dfthai_KFCS_new,KFCS_PCR_results, by = "id", all.x = T )

dfthai_KFCS_contacts <- read_delim("Dados Tailandia/dfthai_KFCS_dados_expandidos.csv",
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

dfthai_KFCS_new <- merge(dfthai_KFCS_new,dfthai_KFCS_contacts, by = "id", all.x = T )

write.table(dfthai_KFCS_new, file = "Dados Tailandia/dfthai_KFCS_new_completo.csv", row.names = F, sep = ";", na = "")

#calclo da media das DENGUES
# Filtrar apenas os antígenos relacionados a Dengue (D1, D2, D3, D4)
df_dengue <- dfthai_KFCS_new %>% 
  filter(antigen %in% c("D1", "D2", "D3", "D4"))

# Calcular a média dos sinais de Dengue (D1, D2, D3, D4) por subjectID, visita e incluir a data
df_dengue_means <- df_dengue %>%
  group_by(subjectID, visitnum) %>%
  dplyr::summarize(
    mean_dengue_signal = mean(signal, na.rm = TRUE), 
    date = first(date), # Incluir a primeira data correspondente àquela visita
    .groups = "drop"
  ) %>%
  mutate(
    sample = NA, 
    index = NA, 
    infectsero = "DENGUE_MEAN", 
    signal = mean_dengue_signal,
    isotype = NA, 
    antigen = "Dengue_Mean", 
    grupo_infec = NA, 
    type = NA, 
    primary = NA
  ) %>%
  select(-mean_dengue_signal)

# Calcular o máximo dos sinais de Dengue (D1, D2, D3, D4) por subjectID, visita e incluir a data
df_dengue_max <- df_dengue %>%
  group_by(subjectID, visitnum) %>%
  dplyr::summarize(
    max_dengue_signal = max(signal, na.rm = TRUE), 
    date = first(date), # Incluir a primeira data correspondente àquela visita
    .groups = "drop"
  ) %>%
  mutate(
    sample = NA, 
    index = NA, 
    infectsero = "DENGUE_MAX", 
    signal = max_dengue_signal,
    isotype = NA, 
    antigen = "Dengue_Max", 
    grupo_infec = NA, 
    type = NA, 
    primary = NA
  ) %>%
  select(-max_dengue_signal)


freq<- df_dengue_means %>% group_by(subjectID) %>% dplyr ::summarise(count=n())

# Juntar a média calculada como uma nova linha ao conjunto de dados original
df_completo <- bind_rows(dfthai_KFCS_new, df_dengue_means, df_dengue_max) %>%
  arrange(subjectID, visitnum)

freq<- df_completo %>% group_by(subjectID) %>% dplyr ::summarise(count=n())

df_completo <- df_completo %>%
  mutate(date = as.Date(date))
#dfthai_KFCS_new$visitnum <- as.numeric(dfthai_KFCS_new$visitnum)
#summary(as.factor(dfthai_KFCS_new$visitnum))
# Exemplo de código para calcular a proporção entre visitas consecutivas
df_fold_change <- df_completo %>%
  group_by(subjectID, antigen) %>%
  arrange(subjectID, antigen, visitnum) %>%  # Organiza os dados pela ordem das visitas
  mutate(
    # Calcula as proporções (fold changes) entre as visitas
    prop_2_1 = ifelse(any(visitnum == 2) & any(visitnum == 1), signal[visitnum == 2] / signal[visitnum == 1], NA),
    prop_3_2 = ifelse(any(visitnum == 3) & any(visitnum == 2), signal[visitnum == 3] / signal[visitnum == 2], NA),
    prop_4_3 = ifelse(any(visitnum == 4) & any(visitnum == 3), signal[visitnum == 4] / signal[visitnum == 3], NA),
    prop_5_4 = ifelse(any(visitnum == 5) & any(visitnum == 4), signal[visitnum == 5] / signal[visitnum == 4], NA),
    prop_6_5 = ifelse(any(visitnum == 6) & any(visitnum == 5), signal[visitnum == 6] / signal[visitnum == 5], NA),
    prop_7_6 = ifelse(any(visitnum == 7) & any(visitnum == 6), signal[visitnum == 7] / signal[visitnum == 6], NA),
    
    # Adiciona a data correspondente ao segundo momento (visita mais recente na comparação)
    date_2_1 = as.Date(ifelse(any(visitnum == 2) & any(visitnum == 1), date[visitnum == 2], NA)),
    date_3_2 = as.Date(ifelse(any(visitnum == 3) & any(visitnum == 2), date[visitnum == 3], NA)),
    date_4_3 = as.Date(ifelse(any(visitnum == 4) & any(visitnum == 3), date[visitnum == 4], NA)),
    date_5_4 = as.Date(ifelse(any(visitnum == 5) & any(visitnum == 4), date[visitnum == 5], NA)),
    date_6_5 = as.Date(ifelse(any(visitnum == 6) & any(visitnum == 5), date[visitnum == 6], NA)),
    date_7_6 = as.Date(ifelse(any(visitnum == 7) & any(visitnum == 6), date[visitnum == 7], NA))
  ) %>%
  ungroup()

# Filtrar apenas as linhas da visita 1
df_fold_change <- df_fold_change %>% 
  filter(visitnum == 1)

# Combinar as colunas de proporções e datas para as mesmas comparações
df_long_fold_change_combined <- df_fold_change %>%
  pivot_longer(
    cols = starts_with("prop_"), 
    names_to = "visit_comparison", 
    values_to = "prop_value"
  ) %>%
  mutate(visit_suffix = str_replace(visit_comparison, "prop_", "")) %>% # Extrai o sufixo das visitas (ex: "2_1")
  
  pivot_longer(
    cols = starts_with("date_"), 
    names_to = "date_comparison", 
    values_to = "prop_date"
  ) %>%
  mutate(date_suffix = str_replace(date_comparison, "date_", "")) %>% # Extrai o sufixo das datas correspondentes
  
  filter(visit_suffix == date_suffix) %>%  # Mantém apenas as linhas com o mesmo sufixo para visita e data
  select(-visit_suffix, -date_suffix)  # Remove as colunas auxiliares com sufixo

dados_div_1_zk <- df_fold_change %>% filter(visitnum=="1" & antigen=="ZIKV")

individuos_com_aumento2.54_zika <- dados_div_1_zk  %>% 
  filter(
    prop_2_1 >= 2.54 |
      prop_3_2 >= 2.54 |
      prop_4_3 >= 2.54 |
      prop_5_4 >= 2.54 |
      prop_6_5 >= 2.54
  ) %>%
  select(subjectID) %>%
  distinct()
individuos_com_aumento2.54_zika$aumento2.5 <- 1

individuos_com_aumento4_zika <- dados_div_1_zk %>%
  filter(
    prop_2_1 >= 4 |
      prop_3_2 >= 4 |
      prop_4_3 >= 4 |
      prop_5_4 >= 4 |
      prop_6_5 >= 4 
  ) %>%
  #select(idnova,grupo_MSDZika, starts_with("prop_")) %>%
  select(subjectID) %>%
  distinct()
individuos_com_aumento4_zika$aumento4 <- 1

individuos_com_aumento <- merge(individuos_com_aumento2.54_zika, individuos_com_aumento4_zika, by = "subjectID", all.x = T)

individuos_com_aumento$aumento2.5[individuos_com_aumento$aumento4==1] <- NA
summary(as.factor(individuos_com_aumento$aumento2.5))

results_compv4 <- merge(dfthai_KFCS_new, individuos_com_aumento, by = "subjectID")

summary(as.factor(results_compv4$type))

# Para Dengue
dados_div_dengue <- df_completo %>%
  filter(antigen %in% c("D1", "D2", "D3", "D4", "Dengue_Mean" , "Dengue_Max")) %>%
  group_by(subjectID, antigen) %>%
  arrange(subjectID, antigen, visitnum) %>%
  mutate(
    prop_2_1 = ifelse("2" %in% visitnum & "1" %in% visitnum, signal[visitnum == "2"] / signal[visitnum == "1"], NA),
    prop_3_2 = ifelse("3" %in% visitnum & "2" %in% visitnum, signal[visitnum == "3"] / signal[visitnum == "2"], NA),
    prop_4_3 = ifelse("4" %in% visitnum & "3" %in% visitnum, signal[visitnum == "4"] / signal[visitnum == "3"], NA),
    prop_5_4 = ifelse("5" %in% visitnum & "4" %in% visitnum, signal[visitnum == "5"] / signal[visitnum == "4"], NA),
    prop_6_5 = ifelse("6" %in% visitnum & "5" %in% visitnum, signal[visitnum == "6"] / signal[visitnum == "5"], NA),
    prop_7_6 = ifelse("7" %in% visitnum & "6" %in% visitnum, signal[visitnum == "7"] / signal[visitnum == "6"], NA),
    
    # Adicionando a data do segundo momento para cada proporção e convertendo para formato Date
    date_2_1 = as.Date(ifelse("2" %in% visitnum & "1" %in% visitnum, date[visitnum == "2"], NA)),
    date_3_2 = as.Date(ifelse("3" %in% visitnum & "2" %in% visitnum, date[visitnum == "3"], NA)),
    date_4_3 = as.Date(ifelse("4" %in% visitnum & "3" %in% visitnum, date[visitnum == "4"], NA)),
    date_5_4 = as.Date(ifelse("5" %in% visitnum & "4" %in% visitnum, date[visitnum == "5"], NA)),
    date_6_5 = as.Date(ifelse("6" %in% visitnum & "5" %in% visitnum, date[visitnum == "6"], NA)),
    date_7_6 = as.Date(ifelse("7" %in% visitnum & "6" %in% visitnum, date[visitnum == "7"], NA))
  ) %>%
  ungroup()

dados_div_dengue2 <- dados_div_dengue %>% filter(antigen %in% c("Dengue_Max"))
dados_div_dengue3 <- dados_div_dengue2 %>% filter(visitnum %in% c(1))

# Para Zika
dados_div_zika <- df_completo %>%
  filter(antigen == "ZIKV") %>%
  group_by(subjectID) %>%
  arrange(subjectID, visitnum) %>%
  mutate(
    prop_2_1_zika = ifelse("2" %in% visitnum & "1" %in% visitnum, signal[visitnum == "2"] / signal[visitnum == "1"], NA),
    prop_3_2_zika = ifelse("3" %in% visitnum & "2" %in% visitnum, signal[visitnum == "3"] / signal[visitnum == "2"], NA),
    prop_4_3_zika = ifelse("4" %in% visitnum & "3" %in% visitnum, signal[visitnum == "4"] / signal[visitnum == "3"], NA),
    prop_5_4_zika = ifelse("5" %in% visitnum & "4" %in% visitnum, signal[visitnum == "5"] / signal[visitnum == "4"], NA),
    prop_6_5_zika = ifelse("6" %in% visitnum & "5" %in% visitnum, signal[visitnum == "6"] / signal[visitnum == "5"], NA),
    prop_7_6_zika = ifelse("7" %in% visitnum & "6" %in% visitnum, signal[visitnum == "7"] / signal[visitnum == "6"], NA),
    
    # Adicionando a data do segundo momento para cada proporção de Zika e convertendo para formato Date
    date_2_1_zika = as.Date(ifelse("2" %in% visitnum & "1" %in% visitnum, date[visitnum == "2"], NA)),
    date_3_2_zika = as.Date(ifelse("3" %in% visitnum & "2" %in% visitnum, date[visitnum == "3"], NA)),
    date_4_3_zika = as.Date(ifelse("4" %in% visitnum & "3" %in% visitnum, date[visitnum == "4"], NA)),
    date_5_4_zika = as.Date(ifelse("5" %in% visitnum & "4" %in% visitnum, date[visitnum == "5"], NA)),
    date_6_5_zika = as.Date(ifelse("6" %in% visitnum & "5" %in% visitnum, date[visitnum == "6"], NA)),
    date_7_6_zika = as.Date(ifelse("7" %in% visitnum & "6" %in% visitnum, date[visitnum == "7"], NA))
  ) %>%
  ungroup()


#############
  freq<- dados_div_zika %>% group_by(subjectID) %>% dplyr ::summarise(count=n())
  
dados_div_zika2 <- dados_div_zika %>% filter(visitnum %in% c(1))

# 5. Calcular a razão do fold change de Zika em relação ao fold change médio de Dengue
df_fold_change_ratio <- dados_div_zika2 %>%
  left_join(dados_div_dengue3, by = "subjectID") %>%
  group_by(visitnum.x) %>%
  mutate(
    ratio_2_1 = prop_2_1_zika / prop_2_1,
    ratio_3_2 = prop_3_2_zika / prop_3_2,
    ratio_4_3 = prop_4_3_zika / prop_4_3,
    ratio_5_4 = prop_5_4_zika / prop_5_4,
    ratio_6_5 = prop_6_5_zika / prop_6_5,
    ratio_7_6 = prop_7_6_zika / prop_7_6
  ) %>%
  select(subjectID, visitnum.x, starts_with("ratio"), starts_with("date"), -c("date.x", "date.y", date_2_1_zika, date_3_2_zika, date_4_3_zika, date_5_4_zika, date_6_5_zika, date_7_6_zika))

# # 6. Reorganizar os dados: todos os valores de ratio em uma coluna e outra coluna indicando a que ratio pertence
# df_long_ratio <- df_fold_change_ratio %>% 
#   pivot_longer(cols = starts_with("ratio"), names_to = "visit_comparison", values_to = "ratio_value")
# 
# df_long_ratio_date <- df_fold_change_ratio %>%
#   pivot_longer(cols = starts_with("date"), names_to = "date_comparison", values_to = "ratio_date")


# Primeiro, unimos as colunas de ratio e date que têm o mesmo sufixo
df_long_ratio_combined <- df_fold_change_ratio %>%
  pivot_longer(
    cols = starts_with("ratio"), 
    names_to = "visit_comparison", 
    values_to = "ratio_value"
  ) %>%
  mutate(visit_suffix = str_replace(visit_comparison, "ratio_", "")) %>% # Extrai o sufixo (por exemplo, "2_1")
  pivot_longer(
    cols = starts_with("date"), 
    names_to = "date_comparison", 
    values_to = "ratio_date"
  ) %>%
  mutate(date_suffix = str_replace(date_comparison, "date_", "")) %>% # Extrai o sufixo correspondente às datas
  filter(visit_suffix == date_suffix) %>%  # Filtra para alinhar as linhas com o mesmo sufixo
  select(-visit_suffix, -date_suffix)  # Remove as colunas auxiliares de sufixo

# Ordena o resultado final para clareza
df_long_ratio_combined <- df_long_ratio_combined %>%
  arrange(subjectID, visit_comparison)


# banco para SU MSD: results_compv4
# banco para Fold change: df_long_fold_change
# banco para Ratio: df_long_ratio.combined'

library(ggforce)

# Supondo que você queira 9 gráficos por página
num_col <- 1  # Número de colunas por página
num_row <- 1  # Número de linhas por página

results_compv4$antigen <- factor(results_compv4$antigen, levels = c("ZIKV", "D1", "D2", "D3", "D4", "JEV" ))

selected_subjects <- c("361KF03F", "361KF02F", "361KF01N", "361KF01F")
results_compv5 <- results_compv4[results_compv4$subjectID %in% selected_subjects, ]
results_compv5 <- results_compv5 %>% filter(antigen!="JEV")
write.table(results_compv5, file = "Lista/results_compv4_thai.csv", row.names = F, sep = ",", na = "")

summary(as.factor(results_compv4$antigen))

results_compv4 <- results_compv4 %>%
  mutate(date = as.Date(date))

results_compv4_4 <- results_compv4 %>% filter(antigen != "JEV" & aumento4==1)
results_compv4_2.5 <- results_compv4 %>% filter(antigen != "JEV" & aumento2.5==1)

# Número de páginas necessárias
total_pages <- ceiling(length(unique(results_compv4_4$subjectID)) / (num_col * num_row))
total_pages <- ceiling(length(unique(results_compv4_2.5$subjectID)) / (num_col * num_row))

# Gerar cada página separadamente
plots <- lapply(1:total_pages, function(page) {
  ggplot(results_compv4_4, 
         aes(x = date, y = signal, color = antigen, group = interaction(subjectID, antigen))) +
    geom_point(size = 2) + 
    geom_line() +  
    scale_color_manual(name = "", values = c("red","purple", "blue", "green", "orange",  "pink"),
                       labels = c("ZIKV" = "ZIKV", "D1" = "DENV1", "D2" = "DENV2", 
                                  "D3" = "DENV3", "D4" = "DENV4", "Outros" = "Other Antigen")) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(10, 10000000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-09-01", "2022-05-31"))  # Ajuste essas datas conforme necessário
    ) +
    labs(
      x = "",
      y = ""
    ) +
    geom_hline(yintercept = 10^3, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      legend.position = "none"  # Correção do erro na posição da legenda
    ) + 
    ggforce::facet_wrap_paginate(~ subjectID, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})

# Exibir todas as páginas
plots



## Fold change
num_col <- 1  # Número de colunas por página
num_row <- 1  # Número de linhas por página


df_long_fold_change_combined2 <- merge(df_long_fold_change_combined, individuos_com_aumento, by = "subjectID")

df_long_fold_change_combined2$antigen <- factor(df_long_fold_change_combined2$antigen, levels = c("ZIKV", "D1", "D2", "D3", "D4", "Dengue_Mean", "Dengue_Max","JEV" ))

# Define os rótulos do eixo x
#labels <- c("2/1", "3/2", "4/3", "5/4", "6/5", "7/6")

df_long_fold_change_combined2 <- df_long_fold_change_combined2 %>%
  mutate(date = as.Date(date))

selected_subjects <- c("361KF03F", "361KF02F", "361KF01N", "361KF01F")
df_long_fold_change_combined3 <- df_long_fold_change_combined2[df_long_fold_change_combined2$subjectID %in% selected_subjects, ]

write.table(df_long_fold_change_combined3, file = "Lista/df_long_fold_change_combined_thai.csv", row.names = F, sep = ",", na = "")

df_long_fold_change4 <- df_long_fold_change_combined2 %>% filter(aumento4==1)
df_long_fold_change2.5 <- df_long_fold_change_combined2 %>% filter(aumento2.5==1)

total_pages <- ceiling(length(unique(df_long_fold_change4$subjectID)) / (num_col * num_row))
total_pages <- ceiling(length(unique(df_long_fold_change2.5$subjectID)) / (num_col * num_row))


# ggplot(df_long_fold_change4 %>% filter(antigen!="JEV" & antigen!="Dengue_Mean"), 
#        aes(x = visit_comparison, y = prop_value, color = antigen, group = interaction(subjectID, antigen))) +
#   geom_point(size = 2) + 
#   geom_line() +  # Remove a cor preta para usar as cores baseadas no 'test'
#   scale_x_discrete(labels = labels) +
#   scale_color_manual(name = "", values = c("red","purple", "blue", "green", "orange",  "pink"),
#                      labels = c("ZIKV" = "ZIKV", "D1" = "DENV1", "D2" = "DENV2", 
#                                 "D3" = "DENV3", "D4" = "DENV4", "Outros" = "Other Antigen")) +
#   labs(
#     x = "",
#     y = "Fold change between serosurveys"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0)) # +  # Rotacionando os rótulos do eixo x
#   facet_wrap(~ idnova, scales = "free_y", ncol = 2)  # Facet para gráficos independentes por idnova com 4 colunas
  
 

log_fold_change = log10(4.04)
log_fold_change2 = log10(2.54)

plots <- lapply(1:total_pages, function(page) {
  ggplot(
    df_long_fold_change2.5 %>% 
      filter(antigen != "JEV" & antigen != "Dengue_Mean" & antigen != "Dengue_Max"), 
    aes(x = prop_date, y = prop_value, color = antigen, group = interaction(subjectID, antigen))
  ) +
    geom_point(size = 2) + 
    geom_line() +  # Remove a cor preta para usar as cores baseadas no 'test'
    scale_color_manual(
      name = "",
      values = c("red", "purple", "blue", "green", "orange", "pink"),
      labels = c(
        "ZIKV" = "ZIKV", 
        "D1" = "DENV1", 
        "D2" = "DENV2", 
        "D3" = "DENV3", 
        "D4" = "DENV4", 
        "Outros" = "Other Antigen"
      )
    ) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(0.0001, 10000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-09-01", "2022-05-31"))  # Ajuste essas datas conforme necessário
    ) +
    geom_hline(yintercept = 2.54, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    labs(
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      legend.position = "none"
    ) +
    ggforce::facet_wrap_paginate(~ subjectID, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})


plots

## Ratio Fold change
num_col <- 1  # Número de colunas por página
num_row <- 1  # Número de linhas por página


df_long_ratio2 <- merge(df_long_ratio_combined, individuos_com_aumento, by = "subjectID")

selected_subjects <- c("361KF03F", "361KF02F", "361KF01N", "361KF01F")
df_long_ratio3 <- df_long_ratio2[df_long_ratio2$subjectID %in% selected_subjects, ]

write.table(df_long_ratio3, file = "Lista/df_long_ratio_thai.csv", row.names = F, sep = ",", na = "")
# Define os rótulos do eixo x
#labels2 <- c("2/1", "3/2", "4/3", "5/4", "6/5", "7/6")


df_long_ratio4 <- df_long_ratio2 %>% filter(aumento4==1)
df_long_ratio2.5 <- df_long_ratio2 %>% filter(aumento2.5==1)

total_pages <- ceiling(length(unique(df_long_ratio4$subjectID)) / (num_col * num_row))
total_pages <- ceiling(length(unique(df_long_ratio2.5$subjectID)) / (num_col * num_row))


# ggplot(df_long_ratio4, 
#        aes(x = visit_comparison, y = ratio_value, group = interaction(subjectID))) +
#   geom_point(size = 2) + 
#   geom_line() +  # Remove a cor preta para usar as cores baseadas no 'test'
#   scale_x_discrete(labels = labels) +
#   labs(
#     x = "",
#     y = "Ratio fold change between serosurveys\nZIKV/DENV average fold change"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0)) # +  # Rotacionando os rótulos do eixo x
# facet_wrap(~ idnova, scales = "free_y", ncol = 2)  # Facet para gráficos independentes por idnova com 4 colunas


plots <- lapply(1:total_pages, function(page) {
  ggplot(df_long_ratio4, 
         aes(x = ratio_date, y = ratio_value, group = interaction(subjectID))) +
    geom_point(size = 2, color = "red") +  # Define a cor dos pontos como vermelho
    geom_line(color = "red") +  # Define a cor das linhas como vermelho
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(0.0001, 10000)
    ) +
    scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = c(0, 0),  # Remove o espaço extra nas extremidades
      limits = as.Date(c("2015-09-01", "2022-05-31"))  # Ajuste essas datas conforme necessário
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Linha tracejada no Y = 10^3
    labs(
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0),
      legend.position = "none"  # Correção do erro na posição da legenda
    ) +
    ggforce::facet_wrap_paginate(~ subjectID, scales = "free_y", ncol = num_col, nrow = num_row, page = page)
})

plots    #size 774 x 233
