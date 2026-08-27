# Script to clean CORE files (data from individual organizations)

# Note that there are different PZ file types: CE-NONPROFIT and C3-CHARITIES
# This file must be run separately for each, appropriately changing files names as necessary
# There will be a comment "SPECIFIC TO PZ TYPE" at lines of code where you would need to change the name accordingly

library(data.table)
library(readr)

###################################
######## STEP ONE #################
###################################
vars.keep <- c("EIN2", "TAX_YEAR", "F9_10_ASSET_TOT_EOY", "F9_01_REV_TOT_CY", "F9_08_REV_TOT_TOT", "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY")

dt.original <- data.table(matrix(ncol=length(vars.keep),nrow=0))
colnames(dt.original) <- vars.keep

dt.pz <- data.table(matrix(ncol=length(vars.keep),nrow=0))
colnames(dt.pz) <- vars.keep

# Stacking all the years of data on top of each other
year_values <- seq(from = 1991, to = 2021, by = 1)
pz_type <- "C3-CHARITIES" # SPECIFIC TO PZ TYPE; options are "C3-CHARITIES" or "CE-NONPROFIT"
for (year in year_values){
      # Get data, restrict to variables we care about, remove records that are exact duplicates, and make EIN2 the first column
      file_name <- paste0("data/pz/CORE-", year, "-501", pz_type, "-PZ-HRMN.csv")
      
      # load data into environment 
      temp <- as.data.table(read_csv(file_name, show_col_types = FALSE))
      temp.vars.keep <- intersect(vars.keep, colnames(temp))
      temp <- unique(temp[, ..temp.vars.keep])
      
      # Combine
      dt.original <- rbindlist(list(dt.original, temp), fill = TRUE)
      dt.pz <- rbindlist(list(dt.pz, temp), fill = TRUE)
}

# Status of missing variables
source("../SCRIPTS/clean_helper.R")
dt.na_counts <- na_counts_df(dt.original)

# There are two variables for revenue and expenses but each point to the same information in theory
# Create new REV variable where, if one revenue variable is missing but not the other, we take the non-missing one; if both are missing then just missing; if both filled but different... will have to think that through later
#tst <- as.data.table(read_csv("SCRATCHWORK/test_revenue_records.csv", show_col_types = FALSE))
dt.pz[, TOT_REV :=
            fcase(
                  
                  # 1. exactly one missing → take the non-missing value
                  is.na(F9_01_REV_TOT_CY) & !is.na(F9_08_REV_TOT_TOT), F9_08_REV_TOT_TOT,
                  !is.na(F9_01_REV_TOT_CY) & is.na(F9_08_REV_TOT_TOT), F9_01_REV_TOT_CY,
                  
                  # 2. both present & equal → take the value
                  F9_01_REV_TOT_CY == F9_08_REV_TOT_TOT, F9_01_REV_TOT_CY,
                  
                  # 3. both present & *not* equal or both missing
                  default =  NA_real_
            )
]
# Same for TOT_EXP
dt.pz[, TOT_EXP :=
            fcase(
                  # 1. exactly one missing → take the non-missing value
                  is.na(F9_01_EXP_TOT_CY) & !is.na(F9_09_EXP_TOT_TOT), F9_09_EXP_TOT_TOT,
                  !is.na(F9_01_EXP_TOT_CY) & is.na(F9_09_EXP_TOT_TOT), F9_01_EXP_TOT_CY,
                  
                  # 2. both present & equal → take the value
                  F9_01_EXP_TOT_CY == F9_09_EXP_TOT_TOT, F9_01_EXP_TOT_CY,
                  
                  # 3. both present & *not* equal or both missing
                  default = NA_real_
            )
]
pz.na_counts <- na_counts_df(dt.pz) 
# In PZ_CHARITY TOT_REV and TOT_EXP are never missing! So I think its fine to drop the other rev/exp columns; but in PZ_NONPROFIT TOT_REV sometimes missing

# Checking TOT_REV for PZ_NONPROFIT
dt.pz[, FLAG := fcase(
                  is.na(F9_01_REV_TOT_CY) & is.na(F9_08_REV_TOT_TOT), TRUE,
                  default = FALSE
)] # with this, I have confirmed that when TOT_REV is missing that's because it was missing in both the revenue variables, so safe to drop

dt.pz <- dt.pz[, !c("F9_01_REV_TOT_CY", "F9_08_REV_TOT_TOT", "F9_01_EXP_TOT_CY", "F9_09_EXP_TOT_TOT"), with = FALSE]

# Looking for duplicate records

key_var <- c("EIN2", "TAX_YEAR")
# duplicates that need to be resolved
dup_groups <- dt.pz[, .N, by = key_var][N > 1] #ASSET BOY / EOY charity: 170,907 / 33,102, nonprofit: 97,927 / 62,068

# subset duplicate records
dups <- dt.pz[dup_groups, on = key_var]

# identify the dollar-valued columns
dollar_cols <- c("F9_10_ASSET_TOT_EOY", "TOT_REV", "TOT_EXP")
diagnostics <- dups[, compare_pair_dt(.SD, dollar_cols), by = .(EIN2, TAX_YEAR)] #information about the duplicates

probs <- seq(0,1,0.1)
quantile(diagnostics$max_abs_diff, probs = probs)

# Whenever the difference was due to a single missing value (i.e. every other column matched except for one and the mismatch was due to missing variable), just pick the complete record
just_1_NA_groups <- diagnostics[diagnostics$max_abs_diff == 0 & diagnostics$had_missing_diff & !diagnostics$multi_col_diff, .(EIN2, TAX_YEAR)]

# Step 1: subset dt.pz to only the relevant EIN2–TAX_YEAR pairs
dt.sub <- dt.pz[just_1_NA_groups, on = .(EIN2, TAX_YEAR)]

# Step 2: within each EIN2–TAX_YEAR group, compute number of NAs
dt.sub[, na_count := rowSums(is.na(.SD)), by = .(EIN2, TAX_YEAR)]

# Step 3: keep only the row with the fewest NAs (the more complete record)
dt.sub <- dt.sub[order(na_count)][, .SD[1], by = .(EIN2, TAX_YEAR)]

# Now need to merge this back into dt.pz--first we'll just drop the EIN2,TAX_YEAR pairs in question
temp <- dt.pz[!just_1_NA_groups, on = .(EIN2, TAX_YEAR)]
dt.pz <- rbind(temp, dt.sub, fill = TRUE)

# of the remaining duplicates, how many have differences greater than $1000?

dup_groups <- dt.pz[, .N, by = key_var][N > 1] #(EOY) charity: , nonprofit: 7366
dups <- dt.pz[dup_groups, on = key_var]
diagnostics <- dups[, compare_pair_dt(.SD, dollar_cols), by = .(EIN2, TAX_YEAR)] #information about the duplicates

gt1000_summary <- data.table(
      VAR_NAME = dollar_cols,
      N_GT1000 = sapply(diagnostics[, ..dollar_cols], function(x) sum(abs(x) > 1000, na.rm = TRUE))
)
# groups where differences are all small and there are exactly two rows; exclude cases where there is a difference in one of the financial variables due to a missing variable
insignificant_groups <- diagnostics[max_abs_diff <= 1000 & n_records == 2 & !had_missing_diff, .(EIN2, TAX_YEAR)] #charity: 13596, nonprofit: 960

# keep first row within each insignificant group
# add a temporary flag column
dt.pz[, keep := TRUE]  # default: keep everything

# for insignificant groups, keep only the first row per group
dt.pz[insignificant_groups,
   on = .(EIN2, TAX_YEAR),
   keep := seq_len(.N) == 1,
   by = .EACHI]

dt.pz <- dt.pz[keep == TRUE]
dt.pz[, keep := NULL]  # optional: drop temporary column

# update the duplicates that need to be resolved
dup_groups <- dt.pz[, .N, by = key_var][N > 1] #charity: 19506 / nonprofit: 6406

# subset duplicate records
dups <- dt.pz[dup_groups, on = key_var] 

#diagnostics <- dups[, compare_pair_dt(.SD, dollar_cols), by = .(EIN2, TAX_YEAR)]

savePath <- "data/pz_merged.rds" # SPECIFIC TO PZ TYPE; options are "data/pz_merged.rds" or "data/pz_ce_merged.rds"
# saveRDS(dt.pz, savePath) # uncomment this line of code to save the file

###################################
######## STEP TWO #################
###################################
rm(list=ls())
library(data.table)
library(readr)

dt <- readRDS("data/pz_merged.rds") # SPECIFIC TO PZ TYPE; options are "data/pz_merged.rds" or "data/pz_ce_merged.rds"
bmf <- readRDS("data/cleanBMF.rds")

# 1. Get all the records from organizations whose EIN is missing from bmf
# Use the `key_col` as the join key and perform an anti-join
only_in_dt1_clean_bmf <- dt[!bmf, on = .(EIN2)] # PZ-C3: 13,657 records from 4058 organizations; PZ-CE: 101,707 records from 49,212 organizations

# 2. Count the number of records for each of these (i.e. time series length)
# Group by `key_col` and count the number of rows (.N)
count_only_in_dt1_clean <- only_in_dt1_clean_bmf[, .N, by = EIN2]

nrow(count_only_in_dt1_clean[N < 5]) # PZ-C3: 3383 out of the 4058 have < 5 records; PZ-C3: 44,676 out of 49,212 have < 5 records (that's 90%)

merged.dt <- merge(dt, bmf, by = "EIN2") # PZ-C3: 8,439,047 records from 788,372 unique orgs; PZ-CE: 3,923,315 records from 359,495 unique orgs

savePath <- "data/pz_merged_bmf.rds" # SPECIFIC TO PZ TYPE; options are "data/pz_merged_bmf.rds" or "data/pz_ce_merged_bmf.rds"
# saveRDS(merged.dt, savePath) # uncomment this line of code to save the file

###################################
######## STEP THREE ###############
###################################
rm(list=ls())
library(data.table)
library(readr)
library(dplyr)

merged.dt <- readRDS("data/pz_merged_bmf.rds") # SPECIFIC TO PZ TYPE; options are "pz_merged_bmf.rds" or "pz_ce_merged_bmf.rds"

# Remove any orgs from state codes above 56 (because they don't correspond to actual states)
merged.dt <- merged.dt |> mutate(state.FIPS = as.integer(state.FIPS)) |> filter(state.FIPS <= 56)

# Fixing any records with 02066 and 02063.. using Census Geocoder found that in 2010 these corresponded to Valdez-Cordova Census Area, with geoid 02261
merged.dt[county.census.geoid %in% c("02066", "02063"), 
   county.census.geoid := "02261"]

setnames(merged.dt,
         old = c("F9_10_ASSET_TOT_EOY"),
         new = c("TOT_ASSET"))

merged.dt[, na_count := NULL]

# saveRDS(merged.dt, "data/pz_processed.rds") # SPECIFIC TO PZ TYPE; options are "pz_processed.rds" or "pz_ce_processed.rds"; uncomment this line of code to save the file