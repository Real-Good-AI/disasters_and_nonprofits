library(data.table)
library(readr)
library(dplyr)

######### Download and stack the data #########
vars.keep <- c("EIN2", "TAX_YEAR", "PF_02_ASSET_TOT_EOY_BV", "PF_01_REV_TOT_BOOKS", "PF_01_EXP_TOT_EXP_DISBMT_BOOKS")

dt.original <- data.table(matrix(ncol=length(vars.keep),nrow=0))
colnames(dt.original) <- vars.keep

dt <- data.table(matrix(ncol=length(vars.keep),nrow=0))
colnames(dt) <- vars.keep

year_values <- seq(from = 1991, to = 2021, by = 1)
for (year in year_values){
      # Get data, restrict to variables we care about, remove records that are exact duplicates, and make EIN2 the first column
      file_name <- sprintf("data/pf/CORE-%s-501C3-PRIVFOUND-PF-HRMN-V0.csv", year)
      
      # load data into environment 
      temp <- as.data.table(read_csv(file_name, show_col_types = FALSE))
      temp <- unique(temp[, ..vars.keep])
      
      # Combine
      dt.original <- rbindlist(list(dt.original, temp))
      dt <- rbindlist(list(dt, temp))
}

rm(temp, file_name, year, year_values)

# There is literally only one missing value. I was able to manually fill it in by pulling up the organizations 2018 tax return on the IRS website
dt[EIN2 == "EIN-82-3863484" & TAX_YEAR == 2018, PF_01_EXP_TOT_EXP_DISBMT_BOOKS := 0]


######### Resolve Duplicate Records #########
# duplicates that need to be resolved
key_var <- c("EIN2", "TAX_YEAR")
dup_groups <- dt[, .N, by = key_var][N > 1] # 7951 org,year pairs
dups <- dt[dup_groups, on = key_var] # subset duplicate records; 15,953 records

# Distribution of differences
compare_pair_dt <- function(dt_group) {
      # number of records in this group
      n_records <- nrow(dt_group)
      
      # drop keys (convert to numeric matrix)
      mat <- as.matrix(dt_group)
      
      # names of all columns in the group
      coln <- colnames(dt_group)
      
      # pick out the dollar-valued columns
      dollar_cols <- c("PF_02_ASSET_TOT_EOY_BV", "PF_01_REV_TOT_BOOKS", "PF_01_EXP_TOT_EXP_DISBMT_BOOKS")
      
      # indices of dollar columns
      dollar_idx <- match(dollar_cols, coln)
      
      # compute differences only if there are exactly 2 rows
      if (n_records == 2) {
            r1 <- as.numeric(mat[1, ])
            r2 <- as.numeric(mat[2, ])
            
            diffs <- r1 - r2
            abs_diffs <- abs(diffs)
            
            # check missing-caused differences only for dollar columns
            had_missing_diff <- any(xor(is.na(r1[dollar_idx]), is.na(r2[dollar_idx])))
            
      } else {
            # for groups with != 2 rows, set diffs to NA
            diffs <- abs_diffs <- numeric(ncol(dt_group))
            diffs[] <- NA
            abs_diffs[] <- NA
            had_missing_diff <- NA
      }
      
      # differences for dollar columns (NA if group != 2 rows)
      diffs_named <- as.list(setNames(diffs[match(dollar_cols, coln)], dollar_cols))
      
      # combine into one data.table row
      data.table(
            n_records      = n_records,                 # <-- new column
            n_diff_cols    = sum(abs_diffs != 0 & !is.na(abs_diffs)),
            n_diff_gt1     = sum(abs_diffs > 1, na.rm = TRUE),
            max_abs_diff   = max(abs_diffs, na.rm = TRUE),
            had_missing_diff = had_missing_diff,
            multi_col_diff = sum(abs_diffs != 0, na.rm = TRUE) > 1
      )[, c(.SD, diffs_named)]
}

diagnostics <- dups[, compare_pair_dt(.SD), by = .(EIN2, TAX_YEAR)]

dollar_cols <- c("PF_02_ASSET_TOT_EOY_BV", "PF_01_REV_TOT_BOOKS", "PF_01_EXP_TOT_EXP_DISBMT_BOOKS")

# build the summary table
gt1000_summary <- data.table(
      VAR_NAME = dollar_cols,
      N_GT1000 = sapply(diagnostics[, ..dollar_cols], function(x) sum(abs(x) > 1000, na.rm = TRUE))
)
# groups where differences are all small and there are exactly two rows; exclude cases where there is a difference in one of the financial variables due to a missing variable
insignificant_groups <- diagnostics[max_abs_diff <= 1000 & n_records == 2 & !had_missing_diff, .(EIN2, TAX_YEAR)] # 5554 total

# keep first row within each insignificant group
# add a temporary flag column
dt[, keep := TRUE]  # default: keep everything

# for insignificant groups, keep only the first row per group
dt[insignificant_groups,
   on = .(EIN2, TAX_YEAR),
   keep := seq_len(.N) == 1,
   by = .EACHI]

dt <- dt[keep == TRUE]
dt[, keep := NULL]  # We've now resolved the 5,554 records where the discrepancy was less than $1000

# update the duplicates that need to be resolved
dup_groups <- dt[, .N, by = key_var][N > 1] # 7951 down to 2397

# subset duplicate records
dups <- dt[dup_groups, on = key_var] # 15,953 down to 4845

diagnostics <- dups[, compare_pair_dt(.SD), by = .(EIN2, TAX_YEAR)]

table(dups$TAX_YEAR) # pretty steady across years, I would say 1993, 2016, and 2018 under-represented

# saveRDS(dt, "pf_merged.rds")

rm(dt.original, dup_groups, gt1000_summary, insignificant_groups)

########## Merge with BMF File ##########
bmf <- readRDS("data/cleanBMF.rds")

# 1. Get all the records from organizations whose EIN is missing from bmf
# Use the `key_col` as the join key and perform an anti-join
only_in_dt1_clean_bmf <- dt[!bmf, on = .(EIN2)] # 4777 records from 1452 organizations

# 2. Count the number of records for each of these (i.e. time series length)
# Group by `key_col` and count the number of rows (.N)
count_only_in_dt1_clean <- only_in_dt1_clean_bmf[, .N, by = EIN2]

nrow(count_only_in_dt1_clean[N < 5]) # 1203 out of the 1452 only have 4 records so would be thrown out anyway

merged.dt <- merge(dt, bmf, by = "EIN2")

# saveRDS(merged.dt, "pf_merged_bmf.rds") # Final product has 2,088,035 records from 189,052 unique orgs; 9% missing NTEE but no other NAs

# na_counts_df(merged.dt)

rm(bmf, count_only_in_dt1_clean, diagnostics, dt, dups, only_in_dt1_clean_bmf)

######### Final Processing #########

# Remove any orgs from state codes above 56 (because they don't correspond to actual states)
merged.dt <- merged.dt |> mutate(state.FIPS = as.integer(state.FIPS)) |> filter(state.FIPS <= 56)

# Fixing 38-3737079 and 83-3967403 county geoid... using Census Geocoder found that in 2010 this latitude/longitude corresponded to Valdez-Cordova Census Area, with geoid 02261
merged.dt[EIN2=="EIN-38-3737079", county.census.geoid := "02261"]
merged.dt[EIN2=="EIN-83-3967403", county.census.geoid := "02261"]


# saveRDS(merged.dt, "data/pf_processed.rds") #2,087,387 from 188,922 unique orgs; uncomment this line of code to save the file