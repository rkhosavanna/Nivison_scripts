library(ggplot2)
library(scales)

# Certifique-se de que as colunas estão em formato numérico
data_comp_l37_l38$prnt_den1 <- as.numeric(data_comp_l37_l38$prnt_den1)
data_comp_l37_l38$D1NS1 <- as.numeric(data_comp_l37_l38$D1NS1)
data_comp_l37_l38$prnt_den2 <- as.numeric(data_comp_l37_l38$prnt_den2)
data_comp_l37_l38$D2NS1 <- as.numeric(data_comp_l37_l38$D2NS1)
data_comp_l37_l38$prnt_den3 <- as.numeric(data_comp_l37_l38$prnt_den3)
data_comp_l37_l38$D3NS1 <- as.numeric(data_comp_l37_l38$D3NS1)
data_comp_l37_l38$prnt_den4 <- as.numeric(data_comp_l37_l38$prnt_den4)
data_comp_l37_l38$D4NS1 <- as.numeric(data_comp_l37_l38$D4NS1)
data_comp_l37_l38$prnt_zikv <- as.numeric(data_comp_l37_l38$prnt_zikv)
data_comp_l37_l38$ZKNS1 <- as.numeric(data_comp_l37_l38$ZKNS1)
# Calcular a regressão e obter os valores de R² e p
model <- lm(prnt_den1 ~ D1NS1, data = data_comp_l37_l38)
model <- lm(prnt_den2 ~ D2NS1, data = data_comp_l37_l38)
model <- lm(prnt_den3 ~ D3NS1, data = data_comp_l37_l38)
model <- lm(prnt_den4 ~ D4NS1, data = data_comp_l37_l38)
model <- lm(ZKNS1 ~ prnt_zikv, data = data_comp_l37_l38)

summary_model <- summary(model)
r2 <- summary_model$r.squared
p_value <- summary_model$coefficients[2, 4] # p-valor da variável preditora

summary_model
plot(model, which = 1) # Residuals vs Fitted plot

model_quadratic <- lm(prnt_zikv ~ ZKNS1 + I(ZKNS1^2), data = data_comp_l37_l38)
summary(model_quadratic)

# library(ggplot2)
# ggplot(data_comp_l37_l38, aes(x = prnt_zikv, y = ZKNS1)) +
#   geom_point() +
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(1, 10000)) +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10, 10000000)) +
#   geom_smooth(method = "lm", color = "blue", se = FALSE) +
#   geom_smooth(method = "loess", color = "red", se = FALSE) # Linha LOESS para verificar curvatura
# 

# Formatação do p-value
p_value_formatted <- ifelse(p_value < 0.001, "<0.001", 
                            ifelse(p_value < 0.0001, "<0.0001", 
                                   formatC(p_value, format = "f", digits = 3)))

# Gerar o gráfico com R² e p no canto superior direito
ggplot(data_comp_l37_l38, aes(x = prnt_zikv, y = ZKNS1)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10, 10000000)) +
  labs(
    x = "",
    y = ""
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1.2), # Linhas dos eixos mais grossas
    axis.ticks = element_line(size = 1.2)  # Ajusta a espessura das marcações nos eixos
  ) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "red") #+ # Linha vertical de referência no eixo X
  
  # Área padrão para exibir R² e p-value
  annotate("text", 
           x = max(data_comp_l37_l38$prnt_den1, na.rm = TRUE) * 30, 
           y = max(data_comp_l37_l38$D1NS1, na.rm = TRUE) * 0.0001,
           label = paste("R² =", round(r2, 4), "\np =", p_value_formatted),
           size = 4, 
           hjust = 1, # Ajusta a posição horizontal para alinhar à direita
           vjust = 1, # Ajusta a posição vertical
           color = "red") # Cor vermelha para os rótulos

  
  cor.test(as.numeric(data_comp_l37_l38$prnt_den1), data_comp_l37_l38$D1NS1, method = "spearman")
  
  
  
  
  # Certifique-se de que as colunas estão em formato numérico
  data_comp_l37_l38$prnt_den1 <- as.numeric(data_comp_l37_l38$prnt_den1)
  data_comp_l37_l38$D1NS1 <- as.numeric(data_comp_l37_l38$D1NS1)
  data_comp_l37_l38$prnt_den2 <- as.numeric(data_comp_l37_l38$prnt_den2)
  data_comp_l37_l38$D2NS1 <- as.numeric(data_comp_l37_l38$D2NS1)
  data_comp_l37_l38$prnt_den3 <- as.numeric(data_comp_l37_l38$prnt_den3)
  data_comp_l37_l38$D3NS1 <- as.numeric(data_comp_l37_l38$D3NS1)
  data_comp_l37_l38$prnt_den4 <- as.numeric(data_comp_l37_l38$prnt_den4)
  data_comp_l37_l38$D4NS1 <- as.numeric(data_comp_l37_l38$D4NS1)
  data_comp_l37_l38$prnt_zikv <- as.numeric(data_comp_l37_l38$prnt_zikv)
  data_comp_l37_l38$ZKNS1 <- as.numeric(data_comp_l37_l38$ZKNS1)
  # Calcular a regressão e obter os valores de R² e p
  model <- lm(prnt_den1 ~ D1NS1, data = data_comp_l37_l38)
  model <- lm(prnt_den2 ~ D2NS1, data = data_comp_l37_l38)
  model <- lm(prnt_den3 ~ D3NS1, data = data_comp_l37_l38)
  model <- lm(prnt_den4 ~ D4NS1, data = data_comp_l37_l38)
  model <- lm(prnt_zikv ~ ZKNS1, data = data_comp_l37_l38)
  model <- lm(prnt_zikv ~ ZKNS1, data = data_comp_l37_l38)
  
  summary_model <- summary(model)
  r2 <- summary_model$r.squared
  p_value <- summary_model$coefficients[2, 4] # p-valor da variável preditora
  
  # Formatação do p-value
  p_value_formatted <- ifelse(p_value < 0.001, "<0.001", 
                              ifelse(p_value < 0.0001, "<0.0001", 
                                     formatC(p_value, format = "f", digits = 3)))
  
  # Gerar o gráfico com R² e p no canto superior direito
  ggplot(data_comp_l37_l38, aes(x = prnt_zikv, y = ZKNS1)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_smooth(method = "loess", color = "red", se = FALSE)+ # Linha LOESS para verificar curvatura
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(1, 10000)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(10, 10000000)) +
    labs(
      x = "",
      y = ""
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(size = 1.2), # Linhas dos eixos mais grossas
      axis.ticks = element_line(size = 1.2)  # Ajusta a espessura das marcações nos eixos
    ) +
    geom_vline(xintercept = 4, linetype = "dashed", color = "red") #+ # Linha vertical de referência no eixo X
  
  
  # Área padrão para exibir R² e p-value
  annotate("text", 
           x = max(data_comp_l37_l38$prnt_den1, na.rm = TRUE) * 30, 
           y = max(data_comp_l37_l38$D1NS1, na.rm = TRUE) * 0.0001,
           label = paste("R² =", round(r2, 4), "\np =", p_value_formatted),
           size = 4, 
           hjust = 1, # Ajusta a posição horizontal para alinhar à direita
           vjust = 1, # Ajusta a posição vertical
           color = "red") # Cor vermelha para os rótulos
  
  
  cor.test(as.numeric(data_comp_l37_l38$prnt_den1), data_comp_l37_l38$D1NS1, method = "spearman")
  
  
  
### Figura com IgG3----
  
MSD_IgG3_PRNT_l37_l38 <- read_excel("Database/ajustadas/Bases para compartilhar/MSD_IgG3_PRNT_l37_l38.xlsx")

  model <- lm(avgzikvigg3 ~ ZKNS1, data = MSD_IgG3_PRNT_l37_l38)
  
  summary_model <- summary(model)
  r2 <- summary_model$r.squared
  p_value <- summary_model$coefficients[2, 4] # p-valor da variável preditora
  
  # Formatação do p-value
  p_value_formatted <- ifelse(p_value < 0.001, "<0.001", 
                              ifelse(p_value < 0.0001, "<0.0001", 
                                     formatC(p_value, format = "f", digits = 3)))
  
  # Gerar o gráfico com R² e p no canto superior direito
  ggplot(MSD_IgG3_PRNT_l37_l38, aes(x = avgzikvigg3, y = ZKNS1)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    #geom_smooth(method = "loess", color = "red", se = FALSE)+ # Linha LOESS para verificar curvatura
    # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
    #               labels = trans_format("log10", math_format(10^.x)),
    #               limits = c(1, 10000)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(10, 10000000)) +
    labs(
      x = "",
      y = ""
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(size = 1.2), # Linhas dos eixos mais grossas
      axis.ticks = element_line(size = 1.2)  # Ajusta a espessura das marcações nos eixos
    ) +
    geom_vline(xintercept = 0.58, linetype = "dashed", color = "red") #+ # Linha vertical de referência no eixo X
  
  
  # Área padrão para exibir R² e p-value
  annotate("text", 
           x = max(MSD_IgG3_PRNT_l37_l38$avgzikvigg3, na.rm = TRUE) * 30, 
           y = max(MSD_IgG3_PRNT_l37_l38$ZKNS1, na.rm = TRUE) * 0.0001,
           label = paste("R² =", round(r2, 4), "\np =", p_value_formatted),
           size = 4, 
           hjust = 1, # Ajusta a posição horizontal para alinhar à direita
           vjust = 1, # Ajusta a posição vertical
           color = "red") # Cor vermelha para os rótulos
  
  
  cor.test(as.numeric(MSD_IgG3_PRNT_l37_l38$avgzikvigg3), MSD_IgG3_PRNT_l37_l38$ZKNS1, method = "spearman")  
  
############## Thai ----
  dfthai <- read_excel("Dados Tailandia/KFCS NT vs NS1 MSD 15MAR24_clean2.xlsx")
  
  # Certifique-se de que as colunas estão em formato numérico
  dfthai$NT_D1 <- as.numeric(dfthai$NT_D1)
  dfthai$D1NS1 <- as.numeric(dfthai$D1NS1)
  dfthai$NT_D2 <- as.numeric(dfthai$NT_D2)
  dfthai$D2NS1 <- as.numeric(dfthai$D2NS1)
  dfthai$NT_D3 <- as.numeric(dfthai$NT_D3)
  dfthai$D3NS1 <- as.numeric(dfthai$D3NS1)
  dfthai$NT_D4 <- as.numeric(dfthai$NT_D4)
  dfthai$D4NS1 <- as.numeric(dfthai$D4NS1)
  dfthai$NT_ZK <- as.numeric(dfthai$NT_ZK)
  dfthai$ZKNS1 <- as.numeric(dfthai$ZKNS1)
  # Calcular a regressão e obter os valores de R² e p
  model <- lm(NT_D1 ~ D1NS1, data = dfthai)
  model <- lm(NT_D2 ~ D2NS1, data = dfthai)
  model <- lm(NT_D3 ~ D3NS1, data = dfthai)
  model <- lm(NT_D4 ~ D4NS1, data = dfthai)
  model <- lm(NT_ZK ~ ZKNS1, data = dfthai)
  
  summary_model <- summary(model)
  r2 <- summary_model$r.squared
  p_value <- summary_model$coefficients[2, 4] # p-valor da variável preditora
  
  # Formatação do p-value
  p_value_formatted <- ifelse(p_value < 0.001, "<0.001", 
                              ifelse(p_value < 0.0001, "<0.0001", 
                                     formatC(p_value, format = "f", digits = 3)))
  
  # Gerar o gráfico com R² e p no canto superior direito
  ggplot(dfthai, aes(NT_D4 , D4NS1)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(1, 10000)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(10, 10000000)) +
    labs(
      x = "",
      y = ""
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(size = 1.2), # Linhas dos eixos mais grossas
      axis.ticks = element_line(size = 1.2)  # Ajusta a espessura das marcações nos eixos
    ) +
    geom_vline(xintercept = 4, linetype = "dashed", color = "red") #+ # Linha vertical de referência no eixo X
  
  # Área padrão para exibir R² e p-value
  annotate("text", 
           x = max(dfthai$NT_D1, na.rm = TRUE) * 0.1, 
           y = max(dfthai$D1NS1, na.rm = TRUE) * 0.001,
           label = paste("R² =", round(r2, 4), "\np =", p_value_formatted),
           size = 4, 
           hjust = 1, # Ajusta a posição horizontal para alinhar à direita
           vjust = 1, # Ajusta a posição vertical
           color = "red") # Cor vermelha para os rótulos
  
  
  cor.test(as.numeric(dfthai$NT_D1), dfthai$D1NS1, method = "spearman")
  