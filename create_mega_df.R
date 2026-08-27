library(data.table)
library(readr)
library(dplyr)

pf <- readRDS("data/pf_processed.rds")
pz <- readRDS("data/pz_processed.rds")
pz.ce <- readRDS("data/pz_ce_processed.rds")

# remove FLAG column because unused
pz <- pz[, !c("FLAG"), with = FALSE]
pz.ce <- pz.ce[, !c("FLAG"), with = FALSE]

# rename relevant columns
setnames(pf,
         old = c("PF_02_ASSET_TOT_EOY_BV", "PF_01_REV_TOT_BOOKS", "PF_01_EXP_TOT_EXP_DISBMT_BOOKS"),
         new = c("TOT_ASSET", "TOT_REV", "TOT_EXP"))

dt <- rbindlist(list(pf, pz, pz.ce), use.names = TRUE)
dt <- distinct(dt)

rm(pf, pz, pz.ce)

# exploring duplicates in the mega dt
source("../SCRIPTS/clean_helper.R")

key_var <- c("EIN2", "TAX_YEAR")
dup_groups <- dt[, .N, by = key_var][N > 1] #
dups <- dt[dup_groups, on = key_var]

dollar_cols <- c("TOT_ASSET", "TOT_REV", "TOT_EXP")
diagnostics <- dups[, compare_pair_dt(.SD, dollar_cols), by = .(EIN2, TAX_YEAR)] #information about the duplicates
probs <- seq(0,1,0.1)
quantile(diagnostics$max_abs_diff, probs = probs)

# Final dupe counts: 30,684 org,year unresolved pairs

dt[, DATA_COUNT := .N, by = c("EIN2")] #recalculate because now may have from different sources... but will be overcount because of duplicates
quantile(dt$DATA_COUNT, probs = probs)

saveRDS(dt, "data/mega.rds") # 14,432,124 records, 1,284,073 orgs
