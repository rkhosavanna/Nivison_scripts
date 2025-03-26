

l37_l49_data_comp <- read_delim("Lista/ZIKV_DENV_MSD_Results_comp.csv",
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

l37_l38_data_comp <- l37_l49_data_comp %>% filter(Cohort  %in% c("L37", "L38") & is.na(Duplicate))

freq<- l37_l38_data_comp %>% filter(!is.na(grupo_MSDZika)) %>% group_by(idnova) %>% dplyr ::summarise(count=n())

l37_l38_data_comp2 <- l37_l38_data_comp %>% filter(!is.na(grupo_MSDZika))

summary(as.factor(l37_l38_data_comp2$Cohort))

g1 <- l37_l38_data_comp %>% filter(grupo_MSDZika==1)
g2 <- l37_l38_data_comp %>% filter(grupo_MSDZika==2)
g3 <- l37_l38_data_comp %>% filter(grupo_MSDZika==3)
g4 <- l37_l38_data_comp %>% filter(grupo_MSDZika==4)




####Figura 3 - Painel A Radar plot----
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(ggradar)
library(tidyverse)
library(tidyquant)
library(scales)
library(corrr)

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g1_df <- g1 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g1_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G1
#Média
g1_summary_tbl <- g1_df %>%
  select(time, where(is.numeric), -idnova) %>%
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)
g1_summary_tbl

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g2_df <- g2 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g2_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G2
g2_summary_tbl <- g2_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)


# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g3_df <- g3 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g3_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G3
g3_summary_tbl <- g3_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g4_df <- g4 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g4_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G4
g4_summary_tbl <- g4_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

g4_summary_tbl

colors = c("blue", "red")

g1_summary_tbl %>%
  ggradar(
    base.size = 15,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    axis.label.offset = 1.2,
    #x.centre.range = 0 * (grid.max - centre.y) ,
    grid.label.size = 6,
    #gridline.label.offset = 0 * (grid.max - centre.y),
    group.line.width = 1.5, 
    group.point.size = 4,
    gridline.mid.colour = "grey",
    gridline.max.colour = "black",
    plot.legend	= FALSE,
    #fill = TRUE,
    #fill.alpha = 0.5
  ) +
  scale_color_manual(values = colors, name = "Cohort sampling"
  )

centre.y = grid.min - ((1/9) * (grid.max - grid.min))


g1_summary_tbl %>%
  ggradar(
    base.size = 4,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    #centre.y = grid.min - ((1/9) * (grid.max - grid.min)),
    label.centre.y = FALSE,
    gridline.min.linetype = "longdash",
    gridline.mid.linetype = "longdash",
    gridline.max.linetype = "longdash",
    gridline.label.offset = 0.2 * (grid.max - centre.y),
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    #background.circle.colour = "#808080",
    
    axis.label.size = 4,
    axis.label.offset = 1.15,    
    grid.label.size = 4,
    group.line.width = 1.2, 
    group.point.size = 3,
    gridline.mid.colour = "grey",
    gridline.max.colour = "darkgrey",
    plot.legend	= FALSE
  ) +
  scale_color_manual(values = colors, name = "Cohort sampling") #+
  theme(text = element_text(size = 5))  # Ajusta todos os textos do gráfico


#### Dados da Tailandia

dfthai_KFCS <- read_excel("Dados Tailandia/KFCS MSD IgG 03APR24_clean.xlsx")

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
thai_prim_df <-dfthai_KFCS %>% filter(infection=="Primary" & (time4=="-1"| time4=="1")) %>% 
  dplyr::select(subject, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,time4)
names(thai_prim_df) <- c("subject", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados da infecção primária
thai_prim_summary_tbl <- thai_prim_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -subject) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), median)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

thai_prim_summary_tbl %>%
  ggradar(
    base.size = 4,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    #centre.y = grid.min - ((1/9) * (grid.max - grid.min)),
    label.centre.y = FALSE,
    gridline.min.linetype = "longdash",
    gridline.mid.linetype = "longdash",
    gridline.max.linetype = "longdash",
    gridline.label.offset = 0.2 * (grid.max - centre.y),
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    #background.circle.colour = "#808080",
    
    axis.label.size = 4,
    axis.label.offset = 1.15,    
    grid.label.size = 4,
    group.line.width = 1.2, 
    group.point.size = 3,
    gridline.mid.colour = "grey",
    gridline.max.colour = "darkgrey",
    plot.legend	= FALSE
  ) +
  scale_color_manual(values = colors, name = "Sample",
                     breaks = c("-1", "1"),
                     labels = c("Before", "After")
  )

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
thai_second_df <-dfthai_KFCS %>% filter(infection=="Secondary" & (time4=="-1"| time4=="1")) %>% 
  dplyr::select(subject, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,time4)
names(thai_second_df) <- c("subject", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados da infecção primária
thai_second_summary_tbl <- thai_second_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -subject) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), median)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

thai_second_summary_tbl %>%
  ggradar(
    base.size = 4,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    #centre.y = grid.min - ((1/9) * (grid.max - grid.min)),
    label.centre.y = FALSE,
    gridline.min.linetype = "longdash",
    gridline.mid.linetype = "longdash",
    gridline.max.linetype = "longdash",
    gridline.label.offset = 0.2 * (grid.max - centre.y),
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    #background.circle.colour = "#808080",
    
    axis.label.size = 4,
    axis.label.offset = 1.15,    
    grid.label.size = 4,
    group.line.width = 1.2, 
    group.point.size = 3,
    gridline.mid.colour = "grey",
    gridline.max.colour = "darkgrey",
    plot.legend	= FALSE
  ) +
  scale_color_manual(values = colors, name = "Sample",
                     breaks = c("-1", "1"),
                     labels = c("Before", "After")
  )



########### Figura com os resultados de PRNT ----

MSD_IgG3_PRNT_l37_l38 <- read_excel("Database/ajustadas/Bases para compartilhar/MSD_IgG3_PRNT_l37_l38.xlsx")

g1 <- MSD_IgG3_PRNT_l37_l38 %>% filter(grupo_MSDZikaPRNT==1)
g2 <- MSD_IgG3_PRNT_l37_l38 %>% filter(grupo_MSDZikaPRNT==2)
g3 <- MSD_IgG3_PRNT_l37_l38 %>% filter(grupo_MSDZikaPRNT==3)
g4 <- MSD_IgG3_PRNT_l37_l38 %>% filter(grupo_MSDZikaPRNT==4)




####Figura 3 - Painel A Radar plot----
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(ggradar)
library(tidyverse)
library(tidyquant)
library(scales)
library(corrr)

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g1_df <- g1 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g1_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G1
#Média
g1_summary_tbl <- g1_df %>%
  dplyr::select(time, where(is.numeric), -idnova) %>%
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)
g1_summary_tbl

g1_summary_tbl <- g1_df %>%
  dplyr::select(time, where(is.numeric), -idnova) %>%
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  dplyr::rename(group = time) %>%
  mutate(across(.cols = setdiff(names(.), "group"), ~ log10(. + 1)))

g1_summary_tbl

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g2_df <- g2 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g2_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G2
g2_summary_tbl <- g2_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)


# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g3_df <- g3 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g3_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G3
g3_summary_tbl <- g3_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
g4_df <- g4 %>%
  dplyr::select(idnova, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,Cohort)
names(g4_df) <- c("idnova", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time")

# Resumo dos dados do G4
g4_summary_tbl <- g4_df %>%
  # Selecione apenas as variáveis numéricas, exceto 'idnova'
  select(time, where(is.numeric), -idnova) %>%
  # Calcule as medianas 
  group_by(time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  # Renomeie a coluna de grupo e aplique a transformação logarítmica nas outras colunas
  rename(group = time) %>%
  mutate(across(-group, ~ log10(. + 1)))  # Adiciona 1 antes de aplicar o log para evitar log(0)

g4_summary_tbl

colors = c("blue", "red")

g1_summary_tbl %>%
  ggradar(
    base.size = 15,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    axis.label.offset = 1.2,
    #x.centre.range = 0 * (grid.max - centre.y) ,
    grid.label.size = 6,
    #gridline.label.offset = 0 * (grid.max - centre.y),
    group.line.width = 1.5, 
    group.point.size = 4,
    gridline.mid.colour = "grey",
    gridline.max.colour = "black",
    plot.legend	= FALSE,
    #fill = TRUE,
    #fill.alpha = 0.5
  ) +
  scale_color_manual(values = colors, name = "Cohort sampling"
  )

centre.y = grid.min - ((1/9) * (grid.max - grid.min))

grid.min = 1
grid.mid = 3
grid.max = 6

g4_summary_tbl %>%
  ggradar(
    base.size = 4,
    values.radar = c("1", "3", "6"),
    grid.min = 1, grid.mid = 3, grid.max = 6,
    #centre.y = grid.min - ((1/9) * (grid.max - grid.min)),
    label.centre.y = FALSE,
    gridline.min.linetype = "longdash",
    gridline.mid.linetype = "longdash",
    gridline.max.linetype = "longdash",
    gridline.label.offset = 0.2 * (grid.max - centre.y),
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    #background.circle.colour = "#808080",
    
    axis.label.size = 4,
    axis.label.offset = 1.15,    
    grid.label.size = 4,
    group.line.width = 1.2, 
    group.point.size = 3,
    gridline.mid.colour = "grey",
    gridline.max.colour = "darkgrey",
    plot.legend	= FALSE
  ) +
  scale_color_manual(values = colors, name = "Cohort sampling") #+
theme(text = element_text(size = 5))  # Ajusta todos os textos do gráfico
