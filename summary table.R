setwd("~/Documents/ZIKV project/MSD/data_MSD")


## initial part from figure 1 codes 
dfthai <- read.csv("combined_HAI_ELISA_MSD.csv")
dfthai <- dfthai |>
  group_by(subjectID) |>
  filter(any(PCR_update == TRUE, na.rm = TRUE)) |>
  ungroup()

dfthai <- dfthai |>
  filter(platform == "MSD")

dfthai <- dfthai |>
  filter(isotype == "IgG")

dfthai <- dfthai |>
  group_by(subjectID) |>
  mutate(
    prior_status = case_when(
      endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_raw == 0, na.rm = TRUE), "naive", "exposed"),
      !endsWith(subjectID, "N") ~ if_else(any(PCR_update == TRUE & prior_infections_est == 0, na.rm = TRUE), "naive", "exposed")
    )
  ) |>
  ungroup()

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


#create a new column to identify if recruited as neonates or otherwise
dfthai <- dfthai |>
  mutate(Group1 = if_else(endsWith(subjectID, "N"), "Neonate", "Adult"))

#create a column to identify if infection occur as an infant (<1y)
dfthai <- dfthai |>
  group_by(subjectID) |>
  mutate(Group2 = if_else(any(PCR_update == T & ageAtCollection < 1, na.rm = T), "Infant","Adult")) |>
  ungroup()

#create a column to mirror original "time5", days from pcr_infection
##first convert character to date
dfthai$collection_date <- as.Date(dfthai$collection_date, format = "%Y-%m-%d")
dfthai$datePcrInfection_1 <- as.Date(dfthai$datePcrInfection_1, format = "%Y-%m-%d")

dfthai <- dfthai |>
  mutate(time5 = as.numeric(collection_date - datePcrInfection_1))

#####################
#####################
#####################

length(unique(dfthai$subjectID)) #23

summary_table <- dfthai |>
  select(subjectID,visitnum) |>
  group_by(subjectID) |>
  mutate(max_visit = max(visitnum))

summary_table <- dfthai |>
  dplyr::group_by(subjectID) |>
  dplyr::summarize(
    max_visit = max(visitnum, na.rm = TRUE), 
    followup_days = as.numeric(difftime(max(collection_date), min(collection_date), units = "days")),
    .groups = "drop")

  summarize(
    mean_followup = mean(followup_days, na.rm = TRUE),
    sd_followup = sd(followup_days, na.rm = TRUE)
  )

sum(summary_table$max_visit) #127
127/23 #5.52

mean(summary_table$followup_days) #1854 
sd(summary_table$followup_days) #277.8
hist(summary_table$max_visit)


summary_table_n <- summary_table |>
  filter(endsWith(subjectID, "N"))

summary_table_f <- summary_table |>
  filter(!endsWith(subjectID, "N"))


sum(summary_table$max_visit) #127
127/23 #5.52


summary_table_n |>
  dplyr::summarize(mean_followup = mean(followup_days), sd_followup = sd(followup_days))
#1823 +/- 316
sum(summary_table_n$max_visit) #25

summary_table_f |>
  dplyr::summarize(mean_followup = mean(followup_days), sd_followup = sd(followup_days))
#1863 +/- 276
sum(summary_table_f$max_visit) #102



########################################
########################################
########################################
########################################


## from figure 5A

thai <- read.csv("combined_HAI_ELISA_MSD.csv")
summary_thai <- thai |>
  filter(platform == "MSD") |>
  filter(isotype == "IgG") |>
  filter(antigen != "JEV")

summary_thai$collection_date <- as.Date(summary_thai$collection_date)

length(unique(summary_thai$subjectID)) #89

summary_thai_fold <- summary_thai |>
  group_by(subjectID) |>
  filter(any(PCR_update == TRUE, na.rm = TRUE)) |>
  ungroup()
length(unique(summary_thai_fold$subjectID)) #23

summary_thai_fold <- summary_thai_fold |>
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

summary_thai_fold <- summary_thai_fold %>%
  group_by(subjectID, collection_date) %>%
  # Step 1: find max ratio_change from D1–D4
  mutate(max_ratio_dengue = max(fold_change[antigen %in% c("D1", "D2", "D3", "D4")], na.rm = TRUE)) %>%
  # Step 2: normalize ZIKV ratio_change by max DENV value
  mutate(zikv_normalized = if_else(antigen == "ZIKV", fold_change / max_ratio_dengue, NA_real_)) %>%
  ungroup()


###

summary_thai_fold |>
  filter(antigen == "ZIKV") |>
  mutate(
    sc_MSD = fold_change >= 2.73,
    sc_ZDfold = (fold_change >= 2.73 & zikv_normalized > 1)) |>
  dplyr::summarize(
    n_true = sum(sc_MSD, na.rm = TRUE),
    n_ZD_true = sum(sc_ZDfold, na.rm = T))

#infant
summary_thai_fold |>
  filter(endsWith(subjectID, "N")) |>
  filter(antigen == "ZIKV") |>
  mutate(
    sc_MSD = fold_change >= 2.73,
    sc_ZDfold = (fold_change >= 2.73 & zikv_normalized > 1)) |>
  dplyr::summarize(
    n_true = sum(sc_MSD, na.rm = TRUE),
    n_ZD_true = sum(sc_ZDfold, na.rm = T))

#family
summary_thai_fold |>
  filter(!endsWith(subjectID, "N")) |>
  filter(antigen == "ZIKV") |>
  mutate(
    sc_MSD = fold_change >= 2.73,
    sc_ZDfold = (fold_change >= 2.73 & zikv_normalized > 1)) |>
  dplyr::summarize(
    n_true = sum(sc_MSD, na.rm = TRUE),
    n_ZD_true = sum(sc_ZDfold, na.rm = T))







#######
#Brazil

summary_B <- MSD_PRNT_l38 |>
  filter(!is.na(group_MSD_PRNT_L38))
summary_B12 <- summary_B |>
  filter(group_MSD_PRNT_L38 %in% c(1,2))
dim(summary_B12)
summary_B34 <- summary_B |>
  filter(group_MSD_PRNT_L38 %in% c(3,4))
dim(summary_B34)
B12_id <- summary_B12$idnova
B34_id <- summary_B34$idnova

MSD_new <- MSD_IgG3_PRNT_l37_l49 |>
  mutate(idnova = format(idnova, scientific = FALSE))
MSD_B12 <- MSD_new |>
  filter(idnova %in% B12_id) |>
  filter(Cohort %in% c("L37", "L38"))
dim(MSD_B12) #162 26


table_B12 <- MSD_B12 |>
  dplyr::group_by(idnova) |>
  dplyr::summarize(
    #max_visit = max(visitnum, na.rm = TRUE), 
    followup_days = as.numeric(difftime(max(date_sample), min(date_sample), units = "days")),
    .groups = "drop")

mean(table_B12$followup_days)
sd(table_B12$followup_days)
hist(table_B12$followup_days)

MSD_B34 <- MSD_new |>
  filter(idnova %in% B34_id) |>
  filter(Cohort %in% c("L37", "L38"))
dim(MSD_B34) #106 26
table_B34 <- MSD_B34 |>
  dplyr::group_by(idnova) |>
  dplyr::summarize(
    #max_visit = max(visitnum, na.rm = TRUE), 
    followup_days = as.numeric(difftime(max(date_sample), min(date_sample), units = "days")),
    .groups = "drop")

mean(table_B34$followup_days)
sd(table_B34$followup_days)
hist(table_B34$followup_days)



MSD_PRNT_l38 %>% 
  filter(group_MSD_PRNT_L38 %in% c(1, 2)) |>
  mutate(
    sc_MSD = ZKNS1_fold >= 2.73,
    sc_ZDfold = (ZKNS1_fold >= 2.73 & ZKNS1_fold/Max_MSD > 1)) |>
  dplyr::summarize(
    n_true = sum(sc_MSD, na.rm = TRUE),
    n_ZD_true = sum(sc_ZDfold, na.rm = T))



MSD_PRNT_l38 %>% 
  filter(group_MSD_PRNT_L38 %in% c(3, 4)) |>
  mutate(
    sc_MSD = ZKNS1_fold >= 2.73,
    sc_ZDfold = (ZKNS1_fold >= 2.73 & ZKNS1_fold/Max_MSD > 1)) |>
  dplyr::summarize(
    n_true = sum(sc_MSD, na.rm = TRUE),
    n_ZD_true = sum(sc_ZDfold, na.rm = T))

           