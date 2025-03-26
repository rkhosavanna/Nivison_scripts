df_joe2 <- read_delim("Lista/df_joe2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


df_normalized <- read_delim("Lista/df_normalized_com_SUNY_Yale.csv",
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)

df_normalized <- df_normalized %>% filter(is.na(duplicado))

msd_l38 <- df_normalized %>% filter(Cohort=="L38" & !is.na(grupo_MSDZika))
msd_l38_semgrupo <- df_normalized %>% filter(Cohort=="L38" & is.na(grupo_MSDZika))

dfthai_KFCS <- read_excel("Dados Tailandia/KFCS MSD IgG 03APR24_clean.xlsx")
dfthai_KFCS$ZKNS1log <- log2(dfthai_KFCS$ZKNS1)

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
thai_prim_df <-dfthai_KFCS %>% filter(infection=="Primary" & (time4=="-1"| time4=="1")) %>% 
  dplyr::select(subject, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,time4, ZKNS1log)
names(thai_prim_df) <- c("subject", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time", "ZKNS1log")

# Selecione apenas as colunas necessárias e renomeie-as conforme necessário
thai_second_df <-dfthai_KFCS %>% filter(infection=="Secondary" & (time4=="-1"| time4=="1")) %>% 
  dplyr::select(subject, ZKNS1,D1NS1, D2NS1, D3NS1, D4NS1, JENS1,time4, ZKNS1log)
names(thai_second_df) <- c("subject", "ZIKV","DENV1", "DENV2", "DENV3", "DENV4", "JEV","time", "ZKNS1log")


l37_l38_data_comp2 <- read_delim("Database/ajustadas/l37_l38_data_comp2.csv",
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

msd_l38suny <- l37_l38_data_comp2 %>% filter(time=="L38")

summary(as.factor(msd_l38suny$Group))

#### Figura 5 ----
##### Figure 5: Panel A application of MSD assay to seroprevalence surveys ----

L38_ZIKA_SC_DENVP <- msd_l38 %>% filter(grupo_MSDZika==1 & test=="ZKNS1") %>% dplyr::select(idnova, Normalized_ResultsLog)
names(L38_ZIKA_SC_DENVP)[1] <- "subject"
names(L38_ZIKA_SC_DENVP)[2] <- "ZKNS1"
L38_ZIKA_SC_DENVP$group <- "L38 ZIKV SC+ DENV+"

L38_ZIKA_SC_DENVN <- msd_l38 %>% filter(grupo_MSDZika==2 & test=="ZKNS1") %>% dplyr::select(idnova, Normalized_ResultsLog)
names(L38_ZIKA_SC_DENVN)[1] <- "subject"
names(L38_ZIKA_SC_DENVN)[2] <- "ZKNS1"
L38_ZIKA_SC_DENVN$group <- "L38 ZIKV SC+ DENV-"

L38_ZIKA_noSC_DENVP <- msd_l38 %>% filter(grupo_MSDZika==3 & test=="ZKNS1") %>% dplyr::select(idnova, Normalized_ResultsLog)
names(L38_ZIKA_noSC_DENVP)[1] <- "subject"
names(L38_ZIKA_noSC_DENVP)[2] <- "ZKNS1"
L38_ZIKA_noSC_DENVP$group <- "L38 ZIKV SC- DENV+"

L38_ZIKA_noSC_DENVN <- msd_l38 %>% filter(grupo_MSDZika==4 & test=="ZKNS1") %>% dplyr::select(idnova, Normalized_ResultsLog)
names(L38_ZIKA_noSC_DENVN)[1] <- "subject"
names(L38_ZIKA_noSC_DENVN)[2] <- "ZKNS1"
L38_ZIKA_noSC_DENVN$group <- "L38 ZIKV SC- DENV-"

thai_second_df2 <- thai_second_df %>% filter(time==1)%>% dplyr::select(subject, ZKNS1log)
names(thai_second_df2)[2] <- "ZKNS1"
thai_second_df2$group <- "Thai DENV+ Denv+"

thai_prim_df2 <- thai_prim_df %>% filter(time==1)%>% dplyr::select(subject, ZKNS1log)
names(thai_prim_df2)[2] <- "ZKNS1"
thai_prim_df2$group <- "Thai DENV+ Denv-"

zika_df <- rbind(L38_ZIKA_SC_DENVN, L38_ZIKA_SC_DENVP, L38_ZIKA_noSC_DENVN, L38_ZIKA_noSC_DENVP, thai_prim_df2, thai_second_df2)

# Define as cores dos pontos com base na variável time
colors <- c("black", "red")

zika_df$group <- factor(zika_df$group, levels = c("L38 ZIKV SC+ DENV-", "L38 ZIKV SC+ DENV+", "L38 ZIKV SC- DENV-", "L38 ZIKV SC- DENV+",  "Thai DENV+ Denv-",  "Thai DENV+ Denv+"))

# Define os rótulos do eixo x
#labels5 <- c("L38 ZIKV SC+\nDenv naïve", "L38 ZIKV SC+\nDENV exposure", "L38 ZIKV SC-\nDenv naïve", "L38 ZIKV SC-\nDENV exposure", "Thai DENV+\nDenv naïve", "Thai DENV+\nDENV exposure")
labels5 <- c("Brazil ZIKV SC+\nDenv naïve", "Brazil ZIKV SC+\nDENV exposure", "Brazil ZIKV SC-\nDenv naïve", "Brazil ZIKV SC-\nDENV exposure", "Thai DENV+\nDenv naïve", "Thai DENV+\nDENV exposure")
# Cria o gráfico com a personalização das cores dos pontos
ggplot(zika_df, aes(x = group, y = ZKNS1)) +
  geom_point(size = 2, color="red") + # Define as cores dos pontos com base na variável time
  scale_x_discrete(labels = labels5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(6, 20)) +
  labs(x = NULL, y = "SU ZIKV NS1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

### Boxplot----
ggplot(zika_df, aes(x = group, y = ZKNS1)) +
  geom_boxplot(color = "red") + # Cria o boxplot
  stat_summary(fun = mean, geom = "point", shape = 22, size = 2, fill = "black") + # Adiciona pontos para a média
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2, color = "black") + # Adiciona barras de erro para desvio padrão
  scale_x_discrete(labels = labels5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(6, 20)) +
  labs(x = NULL, y = "SU ZIKV NS1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#ROC----
# Defina a variável resposta

df_normalizedL38 <- df_normalized %>% filter(Cohort=="L38" & test=="ZKNS1")

df_normalizedL38$zika_sc[df_normalizedL38$grupo_MSDZika==1 | df_normalizedL38$grupo_MSDZika==2] <- "Sim"
df_normalizedL38$zika_sc[df_normalizedL38$grupo_MSDZika==3 | df_normalizedL38$grupo_MSDZika==4] <- "Não"

df_normalizedL38$denv_totalp[df_normalizedL38$grupo_MSDZika==1 | df_normalizedL38$grupo_MSDZika==3] <- "Sim"
df_normalizedL38$denv_totalp[df_normalizedL38$grupo_MSDZika==2 | df_normalizedL38$grupo_MSDZika==4] <- "Não"

table(df_normalizedL38$denv_totalp, df_normalizedL38$zika_sc)

denv_pos <- df_normalizedL38 %>% filter(df_normalizedL38$denv_totalp=="Sim")
denv_neg <- df_normalizedL38 %>% filter(df_normalizedL38$denv_totalp!="Sim")

# Calcular as curvas ROC para os grupos DENV+ e DENV-
roc_pos <- roc(zika_sc ~ Normalized_ResultsLog, data = denv_pos,percent=TRUE)
roc_neg <- roc(zika_sc ~ Normalized_ResultsLog, data = denv_neg,percent=TRUE)
roc_pos_l38 <- roc(zika_sc ~ Normalized_ResultsLog, data = df_normalizedL38,percent=TRUE)

# Plotar a curva ROC para o grupo DENV+
plot(roc_pos, print.auc = FALSE, col = "red", lwd = 3)

# Plotar a curva ROC para o grupo DENV- e adicionar ao gráfico existente
plot(roc_neg, print.auc = FALSE, col = "blue", lwd = 3, print.auc.y = 0.4, add = TRUE)

# Plotar a curva ROC para o grupo DENV- e adicionar ao gráfico existente
### Albert pediu para ver uma ROC com 3 curvas (antes e depois  e total)
plot(roc_pos_l38, print.auc = FALSE, col = "darkgreen", lwd = 3, print.auc.y = 0.4, add = TRUE)

# Calcular as curvas ROC para todos os indivíduos, independente da história prévia de dengue
coords(roc_pos_l38, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_pos <- coords(roc_pos_l38, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_pos

### Positivos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_pos, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_pos <- coords(roc_pos, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_pos

# O valor correspondente ao ponto na curva ROC que maximiza a sensibilidade e a especificidade
melhor_valor_pos <- coords_pos$threshold
log10(melhor_valor_pos)

### Negativos
# Encontre as coordenadas do ponto na curva ROC que maximiza a sensibilidade e a especificidade
coords(roc_neg, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
coords_neg <- coords(roc_neg, "best", ret = c("threshold", "sens", "spec", "ppv", "npv"))
coords_neg
# O valor correspondente ao ponto na curva ROC que maximiza a sensibilidade e a especificidade
melhor_valor_neg <- coords_neg$threshold
log10(melhor_valor_neg)