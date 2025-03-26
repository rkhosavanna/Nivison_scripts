df_normalized <- read_delim("Lista/df_normalized_com_SUNY_Yale.csv",
                            delim = ",", escape_double = FALSE, trim_ws = TRUE)

df_normalized <- df_normalized %>% filter(is.na(duplicado))

results_comp <- df_normalized %>% select(id, idnova, Cohort, grupo_MSDZika, test, Normalized_ResultsLog)

results_comp$ResultsLog <- log2(results_comp$Results)
results_comp$ResultsLog10 <- log10(results_comp$Results)

# Definir a ordem dos níveis para a variável 'Cohort'
results_comp$Cohort <- factor(results_comp$Cohort, levels = c("L37", "L38"))

# Definir a ordem dos níveis para a variável 'test'
results_comp$test <- factor(results_comp$test, levels = c("D1NS1", "D2NS1", "D3NS1", "D4NS1", "Max_MSD", "Means_MSD", "ZKNS1", "JENS1" ))

# Calculando a média e atribuindo ao teste 'Max_MSD'
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
results_comp2$zika_sc[results_comp2$grupo_MSDZika==3 | results_comp2$grupo_MSDZika==4] <- "Não"

dados <- results_comp2 %>%
  group_by(idnova, test) %>%
  mutate(
    prop_L38_L37Log = ifelse("L38" %in% Cohort & "L37" %in% Cohort, 
                             Normalized_ResultsLog2[Cohort == "L38"] - Normalized_ResultsLog2[Cohort == "L37"], NA)
  ) %>%
  ungroup()  # Remove o agrupamento após o cálculo

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
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a média
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
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marcações nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os rótulos do eixo x
    strip.background = element_blank(),  # Remove a borda do título da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com rótulos personalizados


library(pROC)  # Certifique-se de ter o pacote pROC instalado
library(grid)  # Para adicionar gráficos como inset


# ROC ----
# Calcular as curvas ROC para AvDENV
roc_fc_avdp <- roc(zika_sc ~ prop_L38_L37Log, data = exposure,percent=TRUE)
roc_fc_avdn <- roc(zika_sc ~ prop_L38_L37Log, data = naive,percent=TRUE)

# Plotar a curva ROC para o grupo DENV+
plot(roc_fc_avdp, print.auc = TRUE, col = "red", lwd = 3)

# Plotar a curva ROC para o grupo DENV- e adicionar ao gráfico existente
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
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +  # Adiciona pontos para a média
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
    axis.ticks = element_line(size = 0.7),  # Ajusta a espessura das marcações nos eixos
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rotacionando os rótulos do eixo x
    strip.background = element_blank(),  # Remove a borda do título da faceta
    strip.text = element_text(size = 10)  # Ajusta o tamanho do texto da faceta
  ) +
  facet_wrap(~ test, scales = "free_y", ncol = 2, labeller = as_labeller(facet_labels))  # Facet com rótulos personalizados
