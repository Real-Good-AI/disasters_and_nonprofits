####### Cleaning BMF File #######
# Before dropping orgs with missing county FIPS code in BMF cleaning 
df.temp <- bmf_subset |> filter(county.census.geoid == "00000") # 6815 records
sum(is.na(df.temp$NTEEV2)) # 189 records

df.temp <- df.temp |> filter(!is.na(NTEEV2))
plot(table(df.temp$NTEEV2), 
     main = "NTEE Distribution in Organizations with Missing County Location",
     xlab = "NTEE Broad Category",
     ylab = "Count")

df.temp <- bmf_subset |> filter(county.census.geoid != "00000") # 3,437,320 records
sum(is.na(df.temp$NTEEV2)) # 166,256 records

df.temp <- df.temp |> filter(!is.na(NTEEV2))
plot(table(df.temp$NTEEV2), 
     main = "NTEE Distribution in Organizations with no Missing County Location",
     xlab = "NTEE Broad Category",
     ylab = "Count")

# After creating a column n to indicate if record is duplicated
df.temp <- bmf_subset |> filter(n > 1) |> mutate(state.FIPS = as.integer(state.FIPS)) # 14,306 records
df.temp <- df.temp |> filter(state.FIPS <= 56) # because these codes correspond to US States, not territories, left with 14,258
df.temp <- df.temp |> select(EIN2, state.FIPS) |> distinct() # 7128 records
plot(table(as.numeric(df.temp$state.FIPS)), 
     main = "State Distribution, Organizations with Conflicting NTEE code",
     xlab = "State FIPS code",
     ylab = "Count")

df.temp <- bmf_subset |> filter(n == 1) |> mutate(state.FIPS = as.integer(state.FIPS)) # 3,423,014 records
df.temp <- df.temp |> filter(state.FIPS <= 56) # because these codes correspond to US States, not territories, left with 3,416,270
df.temp <- df.temp |> select(EIN2, state.FIPS) |> distinct() # doesn't change because these were unique to begin with
plot(table(as.numeric(df.temp$state.FIPS)), 
     main = "State Distribution, Organizations with no NTEE Conflict",
     xlab = "State FIPS code",
     ylab = "Count")

####### Results and Analysis #######
library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)
source("matches_helper.R")

covs_to_check <- c("bachelors_perc", "med_household_income_adj", "white_perc",  "total_population", "TOT_REV", "TOT_ASSET", "TOT_EXP", "NTEEV2_ART", "NTEEV2_EDU", "NTEEV2_ENV", "NTEEV2_HEL", "NTEEV2_HMS", "NTEEV2_HOS", "NTEEV2_IFA", "NTEEV2_MMB", "NTEEV2_PSB", "NTEEV2_REL", "NTEEV2_UNI", "NTEEV2_UNU", "NTEEV2_NA", "REGION_MIDWEST", "REGION_NORTHEAST", "REGION_SOUTH", "REGION_WEST")
L = 3 # lag, options: 1, 3, 5
save_name <- paste0("Lag", L)

# No SVC
folder_path_match <- "matches/no_svc/int_panelV1/config3/"
folder_path_balance <- "balances/no_svc/int_panelV1/config3/"
matches_list <- readRDS(paste0(folder_path_match, save_name, ".rds"))
balances_list <- readRDS(paste0(folder_path_balance, save_name, ".rds"))

bal_df <- lapply(balances_list, summary)
bal_df <- lapply(bal_df, as.data.frame)
bal_df <- lapply(bal_df, tibble::rownames_to_column, var = "time")

bal_none <- tidy_bal_df(bal_data = bal_df$none, covariates = paste0("m.out.",covs_to_check))
bal_ps.match <- tidy_bal_df(bal_data = bal_df$ps.match, covariates = paste0("m.out.",covs_to_check))
bal_cbps.match <- tidy_bal_df(bal_data = bal_df$CBPS.match, covariates = paste0("m.out.",covs_to_check))
bal_ps.weight <- tidy_bal_df(bal_data = bal_df$ps.weight, covariates = paste0("m.out.",covs_to_check))
bal_cbps.weight <- tidy_bal_df(bal_data = bal_df$CBPS.weight, covariates = paste0("m.out.",covs_to_check))

q_none <- quantile(signif(abs(bal_none$value[bal_none$unrefined == FALSE]), 3))
q_ps.mat <- quantile(signif(abs(bal_ps.match$value[bal_ps.match$unrefined == FALSE]), 3))
q_cbps.mat <- quantile(signif(abs(bal_cbps.match$value[bal_cbps.match$unrefined == FALSE]), 3))
q_ps.w <- quantile(signif(abs(bal_ps.weight$value[bal_ps.weight$unrefined == FALSE]), 3))
q_cbps.w <- quantile(signif(abs(bal_cbps.weight$value[bal_cbps.weight$unrefined == FALSE]), 3))

df.noSVC <- cbind(as.data.frame(q_none), as.data.frame(q_ps.mat), as.data.frame(q_cbps.mat), as.data.frame(q_ps.w), as.data.frame(q_cbps.w))

df.noSVC$winner <- colnames(df.noSVC)[max.col(-df.noSVC, ties.method = "first")]

# With SVC
folder_path_match <- "matches/with_svc/int_panelV2/config3/"
folder_path_balance <- "balances/with_svc/int_panelV2/config3/"
matches_list <- readRDS(paste0(folder_path_match, save_name, ".rds"))
balances_list <- readRDS(paste0(folder_path_balance, save_name, ".rds"))

bal_df <- lapply(balances_list, summary)
bal_df <- lapply(bal_df, as.data.frame)
bal_df <- lapply(bal_df, tibble::rownames_to_column, var = "time")

bal_none <- tidy_bal_df(bal_data = bal_df$none, covariates = paste0("m.out.",covs_to_check))
bal_ps.match <- tidy_bal_df(bal_data = bal_df$ps.match, covariates = paste0("m.out.",covs_to_check))
bal_cbps.match <- tidy_bal_df(bal_data = bal_df$CBPS.match, covariates = paste0("m.out.",covs_to_check))
bal_ps.weight <- tidy_bal_df(bal_data = bal_df$ps.weight, covariates = paste0("m.out.",covs_to_check))
bal_cbps.weight <- tidy_bal_df(bal_data = bal_df$CBPS.weight, covariates = paste0("m.out.",covs_to_check))

q_none <- quantile(signif(abs(bal_none$value[bal_none$unrefined == FALSE]), 3))
q_ps.mat <- quantile(signif(abs(bal_ps.match$value[bal_ps.match$unrefined == FALSE]), 3))
q_cbps.mat <- quantile(signif(abs(bal_cbps.match$value[bal_cbps.match$unrefined == FALSE]), 3))
q_ps.w <- quantile(signif(abs(bal_ps.weight$value[bal_ps.weight$unrefined == FALSE]), 3))
q_cbps.w <- quantile(signif(abs(bal_cbps.weight$value[bal_cbps.weight$unrefined == FALSE]), 3))

df.SVC <- cbind(as.data.frame(q_none), as.data.frame(q_ps.mat), as.data.frame(q_cbps.mat), as.data.frame(q_ps.w), as.data.frame(q_cbps.w))

df.SVC$winner <- colnames(df.SVC)[max.col(-df.SVC, ties.method = "first")]

# Make the pretty table
df.SVC <- df.SVC |> rename(`No Match (SVC)` = q_none,
                           `PS Match (SVC)` = q_ps.mat,
                           `CBPS Match (SVC)` = q_cbps.mat,
                           `PS Weight (SVC)` = q_ps.w,
                           `CBPS Weight (SVC)` = q_cbps.w) 

df.noSVC <- df.noSVC |> rename(`No Match (no SVC)` = q_none,
                           `PS Match (no SVC)` = q_ps.mat,
                           `CBPS Match (no SVC)` = q_cbps.mat,
                           `PS Weight (no SVC)` = q_ps.w,
                           `CBPS Weight (no SVC)` = q_cbps.w)

df <- cbind(df.SVC |> select(-winner), df.noSVC |> select(-winner))
df$winner <- colnames(df)[max.col(-df, ties.method = "first")]
write.csv(df, "figures/quantiles_int.csv")


############# NTEE Distribution
library(khroma)
df <- readRDS("mega.rds") 

ntee_labs <- c("Arts, Culture, and Humanities", "Education (minus Universities)", "Environment and Animals", "Health (minus Hospitals)", "Human Services", "Hospitals", "International, Foreign Affairs", "Mutual/Membership Benefit", "Public, Societal Benefit", "Religion Related", "Universities", "Other" , "Missing")
ntee_palette <- c("#1796D2", "#8DBCD2", "#989898", "#131313", "#F99C28", "#8A6908", "#EF2174", "#A7546C", "#5A7254", "#8A4846", "#DC2B28", "#55AE49")
ntee_codes <- c("ART", "EDU", "ENV", "HEL", "HMS", "HOS", "IFA", "MMB", "PSB", "REL", "UNI", "UNU", "NTEE_NA")

df |> mutate(NTEEV2 = replace_na(NTEEV2, "NTEE_NA")) |> 
      mutate(NTEEV2 = factor(NTEEV2)) |>
      group_by(TAX_YEAR, NTEEV2) |>
      summarise(tot_log_rev = sum(TOT_REV, na.rm = TRUE),
                tot_log_rev = log1p(tot_log_rev)) |>
      ggplot(aes(x = TAX_YEAR, y = tot_log_rev, color=NTEEV2)) +
      geom_line() +
      labs(x = "Year", y = "Log Dollars", title = "Total Logged Revenues, 1991-2021")

ntee_dist <- df |> select(EIN2, NTEEV2) |> distinct()
sum(is.na(ntee_dist$NTEEV2)) # 88,295
ntee_dist <- ntee_dist |> mutate(NTEEV2 = replace_na(NTEEV2, "NTEE_NA"))
ntee_dist <- as.data.frame(table(ntee_dist$NTEEV2)) |>
      rename(NTEE = Var1, Count = Freq)

ntee_dist <- df |> mutate(NTEEV2 = replace_na(NTEEV2, "Missing"))
ntee_dist |> select(EIN2, NTEEV2) |> distinct() |>
      ggplot(aes(x = NTEEV2)) +
      geom_bar(aes(y = ..count.. / sum(..count..))) +
      labs(x = "NTEE Broad Sector", y = "Proportion", title = "Proportion of Nonprofits in Each NTEE Sector") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=1))






