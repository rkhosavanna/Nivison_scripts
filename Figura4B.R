df_normalized <- read_delim("Lista/df_normalized_com_SUNY_Yale.csv",
                            delim = ",", escape_double = FALSE, trim_ws = TRUE)

df_normalized <- df_normalized %>% filter(is.na(duplicado))

results_comp <- df_normalized %>% select(id, idnova, Cohort, grupo_MSDZika, test, Normalized_ResultsLog)

results_comp$ResultsLog <- log2(results_comp$Results)
results_comp$ResultsLog10 <- log10(results_comp$Results)

# Definir a ordem dos n?veis para a vari?vel 'Cohort'
results_comp$Cohort <- factor(results_comp$Cohort, levels = c("L37", "L38"))

# Definir a ordem dos n?veis para a vari?vel 'test'
results_comp$test <- factor(results_comp$test, levels = c("D1NS1", "D2NS1", "D3NS1", "D4NS1", "Max_MSD", "Means_MSD", "ZKNS1", "JENS1" ))

# Calculando a m?dia e atribuindo ao teste 'Max_MSD'
results_comp2 <- results_comp %>%
  group_by(idnova, Cohort) %>%
  mutate(
    max_normalized = max(Normalized_ResultsLog[test %in% c("D1NS1", "D2NS1", "D3NS1", "D4NS1")], na.rm = TRUE),
    Normalized_ResultsLog = ifelse(test == "Max_MSD", max_normalized, Normalized_ResultsLog)
  ) %>%
  ungroup()

results_comp2$Normalized_ResultsLog2 <- results_comp2$Normalized_ResultsLog
results_comp2$Normalized_ResultsLog2[results_comp2$test == "Max_MSD"] <- results_comp2$max_normalized[results_comp2$test == "Max_MSD"]

# Removendo a coluna auxiliar
results_comp2 <- results_comp2 %>% select(-max_normalized)

results_comp2$zika_sc[results_comp2$grupo_MSDZika==1 | results_comp2$grupo_MSDZika==2] <- "Sim"
results_comp2$zika_sc[results_comp2$grupo_MSDZika==3 | results_comp2$grupo_MSDZika==4] <- "N?o"

dados <- results_comp2 %>%
  group_by(idnova, test) %>%
  mutate(
    prop_L38_L37Log = ifelse("L38" %in% Cohort & "L37" %in% Cohort, 
                             Normalized_ResultsLog2[Cohort == "L38"] - Normalized_ResultsLog2[Cohort == "L37"], NA)
  ) %>%
  ungroup()  # Remove o agrupamento ap?s o c?lculo

# Visualizando o resultado
head(dados)


labels5 <- c("Yes", "No")

facet_labels <- c(
  "ZKNS1" = "ZIKV",
  "Max_MSD" = "MaxDENV"
)

naive <- dados %>% filter(Cohort == "L38" & grupo_MSDZika  %in% c(2, 4) & test  %in% c("ZKNS1", "Max_MSD") )
naive$grupo_MSDZika <- factor(naive$grupo_MSDZika, levels = c("2", "4"))
naive$test <- factor(naive$test, levels = c("ZKNS1", "Max_MSD"))

exposure <- dados %>% filter(Cohort == "L38" & grupo_MSDZika  %in% c(1, 3)  & test  %in% c("ZKNS1", "Max_MSD"))
exposure$grupo_MSDZika <- factor(exposure$grupo_MSDZika, levels = c("1", "3"))
exposure$test <- factor(exposure$test, levels = c("ZKNS1", "Max_MSD"))

ggplot(naive, aes(x = as.factor(grupo_MSDZika), y = prop_L38_L37Log)) +
  geom_violin(fill = "red", color = "black") +  # Cria o violin plot
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a m?dia
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "darkgray") +  # Adiciona barras de erro para IC de 95%
  scale_x_discrete(labels = labels5) +
  # scale_y_continuous(
  #   trans = "log2", 
  #   breaks = scales::trans_breaks("log2", function(x) 2^x, n = 10),
  #   labels = scales::trans_format("log2", scales::math_format(2^.x)),
  #   limits = c(0.1, 10000)
  # ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-4, 14)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7), # Linhas dos eixos mais grossas
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marca??es nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os r?tulos do eixo x
    strip.background = element_blank(),  # Remove a borda do t?tulo da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com r?tulos personalizados


library(pROC)  # Certifique-se de ter o pacote pROC instalado
library(grid)  # Para adicionar gr?ficos como inset


# ROC ----
# Calcular as curvas ROC para AvDENV
roc_fc_avdp <- roc(zika_sc ~ prop_L38_L37Log, data = exposure,percent=TRUE)
roc_fc_avdn <- roc(zika_sc ~ prop_L38_L37Log, data = naive,percent=TRUE)

# Plotar a curva ROC para o grupo DENV+
plot(roc_fc_avdp, print.auc = TRUE, col = "red", lwd = 3)

# Plotar a curva ROC para o grupo DENV- e adicionar ao gr?fico existente
plot(roc_fc_avdn, print.auc = FALSE, col = "blue", lwd = 3, print.auc.y = 0.4, add = TRUE)
plot(roc_fc_avdn, print.auc = TRUE, col = "blue", lwd = 3)

### Positivos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_fc_avdp, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdp <- coords(roc_fc_avdp, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdp

### Negativos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_fc_avdn, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdn <- coords(roc_fc_avdn, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdn






# Plotar a curva ROC principal
plot(roc_fc_p, print.auc = T, col = "red", lwd = 3)
plot(roc_fc_n, print.auc = T, col = "blue", lwd = 3)


# Thai Coorte
dados_KFCSp <- read_delim("Database/ajustadas/dados_KFCSp.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

dados_KFCSp$test <- factor(dados_KFCSp$test, levels = c("D1NS1", "D2NS1", "D3NS1", "D4NS1",  "Means_MSD",  "Max_MSD", "ZKNS1", "JENS1" ))
dados_KFCSp$propLog <- log2(dados_KFCSp$prop)
infection_dados_KFCSp <- dados_KFCSp %>%
  group_split(infecting_serotype_time_neg1, infection_time_neg1)

denv1p_p <- infection_dados_KFCSp[[1]]
denv1s_p <- infection_dados_KFCSp[[2]]
denv4p_p <- infection_dados_KFCSp[[3]]
denv4s_p <- infection_dados_KFCSp[[4]]

denvp_p <- rbind(denv1p_p, denv4p_p)
denvs_p <- rbind(denv1s_p, denv4s_p)

denvp_p <- denvp_p %>% filter(test=="Max_MSD" | test=="ZKNS1")
denvs_p <- denvs_p %>% filter(test=="Max_MSD" | test=="ZKNS1")

denvp_p$test <- factor(denvp_p$test, levels=c("ZKNS1", "Max_MSD"))
denvs_p$test <- factor(denvs_p$test, levels=c("ZKNS1", "Max_MSD"))


ggplot(denvs_p, aes(x = as.factor(time4_time1), y = propLog)) +
  geom_violin(fill = "red", color = "black") +  # Cria o violin plot
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a m?dia
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "darkgray") +  # Adiciona barras de erro para IC de 95%
  scale_x_discrete(labels = "Yes") +
  # scale_y_continuous(
  #   trans = "log2", 
  #   breaks = scales::trans_breaks("log2", function(x) 2^x, n = 10),
  #   labels = scales::trans_format("log2", scales::math_format(2^.x)),
  #   limits = c(0.1, 10000)
  # ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-4, 14)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7), # Linhas dos eixos mais grossas
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marca??es nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os r?tulos do eixo x
    strip.background = element_blank(),  # Remove a borda do t?tulo da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com r?tulos personalizados



###############################################
####new figure 3 ~~ figure 4B #####
##comparing MSD with PRNT


foldchange <- MSD_IgG3_PRNT_l37_l49 |>
  select(id,idnova,Cohort,date_sample,ZKNS1,D1NS1,D2NS1,D3NS1,D4NS1, group_MSD,denviggt_titers,avgzikvigg3,prnt_zikv,group_MSD_PRNT,zika_posIgG3,dengue_total_pos,zika_sc)

foldchange <- foldchange |>
  filter(Cohort %in% c("L37", "L38")) |>
  mutate(date_sample = as.Date(date_sample)) |>
  distinct(id, .keep_all = TRUE)

#foldchange <- foldchange %>%
#  mutate(idnova = format(idnova, scientific = FALSE))

#foldchange <- foldchange |>
#  filter(Cohort %in% c("L37", "L38")) |>
#  mutate(date_sample = as.Date(date_sample)) |>
#  group_by(idnova, Cohort) |>
#  slice(1) |>
#  ungroup()


foldchange_wide <- foldchange %>%
  pivot_wider(
    id_cols = idnova,  # Ensure rows are matched by ID
    #id_cols = setdiff(names(foldchange), "Cohort"),  # Include all columns except Cohort
    names_from = Cohort,  # Create new columns based on Cohort values (L37, L38)
    values_from = c(date_sample, ZKNS1,D1NS1,D2NS1,D3NS1,D4NS1, group_MSD_PRNT, dengue_total_pos, zika_sc),  # Expand Date and Value for each Cohort
    names_glue = "{.value}_{Cohort}"  # Ensure column names remain structured
  )

foldchange_wide <- foldchange_wide |>
  mutate(logzikv_l37 = log2(ZKNS1_L37),
         logzikv_l38 = log2(ZKNS1_L38),
         fold_change = logzikv_l38 - logzikv_l37,
         logd1_l37 = log2(D1NS1_L37),
         logd1_l38 = log2(D1NS1_L38),
         logd2_l37 = log2(D2NS1_L37),
         logd2_l38 = log2(D2NS1_L38),
         logd3_l37 = log2(D3NS1_L37),
         logd3_l38 = log2(D3NS1_L38),
         logd4_l37 = log2(D4NS1_L37),
         logd4_l38 = log2(D4NS1_L38),
         denv_fold = 
           pmax((logd1_l38 - logd1_l37), (logd2_l38 - logd2_l37), (logd3_l38 - logd3_l37), (logd4_l38 - logd4_l37))
  )

MSD_PRNT_l38 <- foldchange_wide |>
  select(idnova, date_sample_L38, ZKNS1_L38, group_MSD_PRNT_L38, 
          ZKNS1_fold = fold_change, 
         Max_MSD = denv_fold)
dim(MSD_PRNT_l38) #273 6

MSD_PRNT_l38 %>%
  mutate(id_str = format(idnova, scientific = FALSE)) %>%
  count(id_str) %>%
  filter(n > 1) %>% 
  print(n = 52)

MSD_PRNT_l38 <- MSD_PRNT_l38 %>%
  mutate(idnova = format(idnova, scientific = FALSE))


MSD_PRNT_l38 <- MSD_PRNT_l38 %>%
  group_by(idnova) %>%
  arrange(is.na(group_MSD_PRNT_L38), .by_group = TRUE) %>%  # Prioritize rows with non-NA
  slice(1) %>%  # Keep only the first row per group
  ungroup()
dim(MSD_PRNT_l38) #204 6

MSD_PRNT_l38_long <- MSD_PRNT_l38 %>%
  pivot_longer(
    cols = c(ZKNS1_fold, Max_MSD),  # Columns to pivot
    names_to = "test",  # New column containing the original column names
    values_to = "value"  # New column containing the values
  )

#naive <- dados %>% filter(Cohort == "L38" & grupo_MSDZika  %in% c(2, 4) & test  %in% c("ZKNS1", "Max_MSD") )
naive <- MSD_PRNT_l38_long %>% filter(group_MSD_PRNT_L38 %in% c(2, 4) & test %in% c("ZKNS1_fold", "Max_MSD") )
naive$group_MSD_PRNT_L38 <- factor(naive$group_MSD_PRNT_L38, levels = c("2", "4"))
naive$test <- factor(naive$test, levels = c("ZKNS1_fold", "Max_MSD"))

#exposure <- dados %>% filter(Cohort == "L38" & grupo_MSDZika  %in% c(1, 3)  & test  %in% c("ZKNS1", "Max_MSD"))
exposure <- MSD_PRNT_l38_long %>% filter(group_MSD_PRNT_L38  %in% c(1, 3)  & test  %in% c("ZKNS1_fold", "Max_MSD"))
exposure$group_MSD_PRNT_L38 <- factor(exposure$group_MSD_PRNT_L38, levels = c("1", "3"))
exposure$test <- factor(exposure$test, levels = c("ZKNS1_fold", "Max_MSD"))

facet_labels <- c(
  "ZKNS1_fold" = "ZIKV",
  "Max_MSD" = "MaxDENV"
)
labels5 <- c("Yes", "No")

ggplot(naive, aes(x = as.factor(group_MSD_PRNT_L38), y = value)) +
  geom_violin(fill = "red", color = "black") +  # Cria o violin plot
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a m?dia
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "darkgray") +  # Adiciona barras de erro para IC de 95%
  scale_x_discrete(labels = labels5) +
  # scale_y_continuous(
  #   trans = "log2", 
  #   breaks = scales::trans_breaks("log2", function(x) 2^x, n = 10),
  #   labels = scales::trans_format("log2", scales::math_format(2^.x)),
  #   limits = c(0.1, 10000)
  # ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-4, 14)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7), # Linhas dos eixos mais grossas
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marca??es nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os r?tulos do eixo x
    strip.background = element_blank(),  # Remove a borda do t?tulo da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com r?tulos personalizados


ggplot(exposure, aes(x = as.factor(group_MSD_PRNT_L38), y = value)) +
  geom_violin(fill = "red", color = "black") +  # Cria o violin plot
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a m?dia
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "darkgray") +  # Adiciona barras de erro para IC de 95%
  scale_x_discrete(labels = labels5) +
  # scale_y_continuous(
  #   trans = "log2", 
  #   breaks = scales::trans_breaks("log2", function(x) 2^x, n = 10),
  #   labels = scales::trans_format("log2", scales::math_format(2^.x)),
  #   limits = c(0.1, 10000)
  # ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-4, 14)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7), # Linhas dos eixos mais grossas
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marca??es nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os r?tulos do eixo x
    strip.background = element_blank(),  # Remove a borda do t?tulo da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com r?tulos personalizados


# ROC ----
# Calcular as curvas ROC para AvDENV
naive2 <- MSD_PRNT_l38 %>% filter(group_MSD_PRNT_L38 %in% c(2, 4)) |>
  mutate(zika_sc_prnt = ifelse(group_MSD_PRNT_L38 == 2, "Yes", "No"))

exposure2 <- MSD_PRNT_l38 %>% filter(group_MSD_PRNT_L38 %in% c(1, 3)) |>
  mutate(zika_sc_prnt = ifelse(group_MSD_PRNT_L38 == 1, "Yes", "No"))

roc_fc_avdp <- roc(zika_sc_prnt ~ ZKNS1_fold, data = exposure2,percent=TRUE)
roc_fc_avdn <- roc(zika_sc_prnt ~ ZKNS1_fold, data = naive2,percent=TRUE)

# Plotar a curva ROC para o grupo DENV+
plot(roc_fc_avdp, print.auc = TRUE, col = "red", lwd = 3)

# Plotar a curva ROC para o grupo DENV- e adicionar ao gr?fico existente
plot(roc_fc_avdn, print.auc = FALSE, col = "blue", lwd = 3, print.auc.y = 0.4, add = TRUE)
plot(roc_fc_avdn, print.auc = TRUE, col = "blue", lwd = 3)

### Positivos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_fc_avdp, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdp <- coords(roc_fc_avdp, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdp

coords_fc_avdp_all <- coords(
  roc_fc_avdp,
  x = "all",
  ret = c("threshold", "sens", "spec", "ppv", "npv"),
  transpose = FALSE
)
coords_fc_avdp_all

### Negativos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_fc_avdn, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdn <- coords(roc_fc_avdn, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_fc_avdn


dim(naive2) #28
dim(exposure2) #55

table(roc_fc_avdp$zika_sc_prnt, ZKNS1_fold)
sum(exposure2$ZKNS1_fold <= 2.73)
sum(naive2$ZKNS1_fold < 1.26)




foldchange_wide |>
  filter(!is.na(zika_sc_L38)) |>
  filter(dengue_total_pos_L37 == "No") |>
  ggplot(aes(x=factor(zika_sc_L38, levels = c("Yes", "No")), y=fold_change)) +
  geom_violin(color = "black", fill = "red") +
  scale_y_continuous(breaks = seq(-2, 8, by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),  # Increase x-axis font size
    axis.text.y = element_text(size = 16)   # Increase y-axis font size
  ) +
  labs(x="ZIKV PRNT infection",
       y="Delta log2 SU ZIKV NS1")



foldchange_wide |>
  filter(!is.na(zika_sc_L38)) |>
  filter(dengue_total_pos_L37 == "Yes") |>
  ggplot(aes(x=factor(zika_sc_L38, levels = c("Yes", "No")), y=fold_change)) +
  geom_violin(color = "black", fill = "red") +
  scale_y_continuous(breaks = seq(-2, 8, by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),  # Increase x-axis font size
    axis.text.y = element_text(size = 16)   # Increase y-axis font size
  ) +
  labs(x="ZIKV PRNT infection",
       y="Delta log2 SU ZIKV NS1")
