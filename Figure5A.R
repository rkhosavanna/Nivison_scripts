thai <- read.csv("combined_HAI_ELISA_MSD.csv")
f5_thai <- thai |>
  filter(startsWith(subjectID, "361KF")) |>
  filter(platform == "MSD") |>
  filter(isotype == "IgG") |>
  filter(antigen != "JEV")

f5_thai$collection_date <- as.Date(f5_thai$collection_date)

f5_thai |>
  filter(subjectID == "361KF03F") |>
  ggplot(aes(x=collection_date, y = signal, group = antigen, color = antigen)) +
  geom_point() +
  geom_line() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10, 10000000)) +
  scale_x_date(
    limits = as.Date(c("2015-01-01", "2023-01-01")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "MSD IgG ZIKV"
  )


fig5A <- f5_thai |>
  filter(subjectID %in% c("361KF01N","361KF01F","361KF02F","361KF03F")) |>
  ggplot(aes(x=collection_date, y = signal, group = antigen, color = antigen)) +
  geom_point() +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 1e3, linetype = "dashed", color = "black") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(50, 1000000)) +
  #scale_y_continuous(
  #  trans = "log2",  # Apply log2 transformation
  #  breaks = trans_breaks("log2", function(x) 2^x),  # Define breaks based on log2 scale
  #  labels = trans_format("log2", math_format(2^.x)),  # Label the breaks in log2 format
  #  limits = c(50, 1000000)  # Set the axis limits
  #) + 
  scale_x_date(
    limits = as.Date(c("2016-01-01", "2022-06-30")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  scale_color_manual(
    values = c("D1" = "skyblue", "D2" = "deepskyblue", "D3" = "dodgerblue", "D4" = "blue", "ZIKV" = "red")
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw(base_size = 16) +  # Controls base font size
  theme(
    axis.title = element_text(size = 38),      # Axis titles
    axis.text = element_text(size = 34),       # Axis tick labels
    legend.title = element_text(size = 38),    # Legend title
    legend.text = element_text(size = 34),     # Legend items
    strip.text = element_text(size = 36),       # Facet labels
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    x = "Year",
    y = "MSD: SU ZIKV NS1"
  ) +
  facet_wrap(~subjectID, ncol = 4) +
  theme(legend.position = "top")

fig5A

ggsave(fig5A, file="figures/fig5A.png", width = 35, height = 10)



#Ratio fold change  ZIKV/MaxDENV

#first get fold change between yearly serosurveys of each antigen

f5_thai_fold <- f5_thai |>
  mutate(year = year(collection_date)) |>
  group_by(subjectID, antigen, year) |>
  arrange(collection_date) |>
  slice(1) |>  # Keep only the earliest date per year, per antigen, per subject
  ungroup() |>
  mutate(log_signal = log2(signal)) |>
  group_by(subjectID, antigen) |>
  arrange(collection_date) |>
  mutate(signal_lag = lag(log_signal),
         fold_change = log_signal - signal_lag) |>
  ungroup()

#diagnostic check
f5_thai_fold |>
  filter(subjectID == "361KF02F") |>
  arrange(antigen) |>
  select(collection_date, 
         #visitnum, 
         antigen, signal, log_signal, signal_lag, fold_change
         , max_ratio_dengue, zikv_normalized
  ) |> print(n=30)


#now find ratio of the fold change between ZIKV and max DENV
f5_thai_fold <- f5_thai_fold %>%
  group_by(subjectID, collection_date) %>%
  # Step 1: find max ratio_change from D1–D4
  mutate(max_ratio_dengue = max(fold_change[antigen %in% c("D1", "D2", "D3", "D4")], na.rm = TRUE)) %>%
  # Step 2: normalize ZIKV ratio_change by max DENV value
  mutate(zikv_normalized = if_else(antigen == "ZIKV", fold_change / max_ratio_dengue, NA_real_)) %>%
  ungroup()


fig5C <- f5_thai_fold |>
  filter(subjectID %in% c("361KF01N","361KF01F","361KF02F","361KF03F")) |>
  ggplot(aes(x=collection_date, y = zikv_normalized, group = antigen, color = antigen)) +
  geom_point() +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x)),
  #              limits = c(10, 10000000)) +
  scale_x_date(
    limits = as.Date(c("2015-01-01", "2023-01-01")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  scale_color_manual(
    values = c("D1" = "skyblue", "D2" = "deepskyblue", "D3" = "dodgerblue", "D4" = "blue", "ZIKV" = "red")
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw(base_size = 16) +  # Controls base font size
  theme(
    axis.title = element_text(size = 38),      # Axis titles
    axis.text = element_text(size = 34),       # Axis tick labels
    legend.title = element_text(size = 36),    # Legend title
    legend.text = element_text(size = 34),     # Legend items
    strip.text = element_text(size = 36),       # Facet labels
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    x = "Year",
    y = "Ratio fold change  ZIKV/MaxDENV"
  ) +
  facet_wrap(~subjectID, ncol = 4) +
  theme(legend.position = "none")

fig5C

ggsave(fig5C, file="figures/fig5C.png", width = 35, height = 10)






#fold change between serosurveys

fig5B <- f5_thai_fold |>
  filter(subjectID %in% c("361KF01N","361KF01F","361KF02F","361KF03F")) |>
  ggplot(aes(x=collection_date, y = fold_change, group = antigen, color = antigen)) +
  geom_point() +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x)),
  #              limits = c(10, 10000000)) +
  scale_x_date(
    limits = as.Date(c("2015-01-01", "2023-01-01")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  theme_bw(base_size = 16) +  # Controls base font size
  theme(
    axis.title = element_text(size = 38),      # Axis titles
    axis.text = element_text(size = 34),       # Axis tick labels
    legend.title = element_text(size = 36),    # Legend title
    legend.text = element_text(size = 34),     # Legend items
    strip.text = element_text(size = 36)       # Facet labels
  ) +
  labs(
    x = "Year",
    y = "Fold change between serosurveys"
  ) +
  facet_wrap(~subjectID, ncol = 4) +
  theme(legend.position = "top")

fig5B

ggsave(figB, file="figures/fig5B.png", width = 35, height = 10)



#################

f5b_thai <- thai |>
  filter(startsWith(subjectID, "019KF")) |>
  filter(platform == "MSD") |>
  filter(isotype == "IgG") |>
  filter(antigen != "JEV")

f5b_thai$collection_date <- as.Date(f5b_thai$collection_date)

fig5bA <- f5b_thai |>
  filter(subjectID %in% c("019KF01N","019KF01P","019KF06F","019KF07F")) |>
  ggplot(aes(x=collection_date, y = signal, group = antigen, color = antigen)) +
  geom_point() +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 1e3, linetype = "dashed", color = "black") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(50, 1000000)) +
  #scale_y_continuous(
  #  trans = "log2",  # Apply log2 transformation
  #  breaks = trans_breaks("log2", function(x) 2^x),  # Define breaks based on log2 scale
  #  labels = trans_format("log2", math_format(2^.x)),  # Label the breaks in log2 format
  #  limits = c(50, 1000000)  # Set the axis limits
  #) + 
  scale_x_date(
    limits = as.Date(c("2016-01-01", "2022-06-30")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  theme_bw(base_size = 16) +  # Controls base font size
  theme(
    axis.title = element_text(size = 38),      # Axis titles
    axis.text = element_text(size = 34),       # Axis tick labels
    legend.title = element_text(size = 38),    # Legend title
    legend.text = element_text(size = 34),     # Legend items
    strip.text = element_text(size = 36)       # Facet labels
  ) +
  labs(
    x = "Year",
    y = "MSD IgG ZIKV"
  ) +
  facet_wrap(~subjectID, ncol = 4) +
  theme(legend.position = "top")

fig5bA

ggsave(fig5bA, file="figures/fig5bA.png", width = 35, height = 10)


#now find ratio of the fold change between ZIKV and max DENV
f5b_thai_fold <- f5b_thai |>
  mutate(year = year(collection_date)) |>
  group_by(subjectID, antigen, year) |>
  arrange(collection_date) |>
  slice(1) |>  # Keep only the earliest date per year, per antigen, per subject
  ungroup() |>
  mutate(log_signal = log2(signal)) |>
  group_by(subjectID, antigen) |>
  arrange(collection_date) |>
  mutate(signal_lag = lag(log_signal),
         fold_change = log_signal - signal_lag) |>
  ungroup()

f5b_thai_fold <- f5b_thai_fold %>%
  group_by(subjectID, collection_date) %>%
  # Step 1: find max ratio_change from D1–D4
  mutate(max_ratio_dengue = max(fold_change[antigen %in% c("D1", "D2", "D3", "D4")], na.rm = TRUE)) %>%
  # Step 2: normalize ZIKV ratio_change by max DENV value
  mutate(zikv_normalized = if_else(antigen == "ZIKV", fold_change / max_ratio_dengue, NA_real_)) %>%
  ungroup()


fig5bC <- f5b_thai_fold |>
  filter(subjectID %in% c("019KF01N","019KF01P","019KF06F","019KF07F")) |>
  ggplot(aes(x=collection_date, y = zikv_normalized, group = antigen, color = antigen)) +
  geom_point() +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x)),
  #              limits = c(10, 10000000)) +
  scale_x_date(
    limits = as.Date(c("2015-01-01", "2023-01-01")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  theme_bw(base_size = 16) +  # Controls base font size
  theme(
    axis.title = element_text(size = 38),      # Axis titles
    axis.text = element_text(size = 34),       # Axis tick labels
    legend.title = element_text(size = 36),    # Legend title
    legend.text = element_text(size = 34),     # Legend items
    strip.text = element_text(size = 36)       # Facet labels
  ) +
  labs(
    x = "Year",
    y = "Ratio fold change  ZIKV/MaxDENV"
  ) +
  facet_wrap(~subjectID, ncol = 4) +
  theme(legend.position = "none")

fig5bC

ggsave(fig5bC, file="figures/fig5bC.png", width = 35, height = 10)


