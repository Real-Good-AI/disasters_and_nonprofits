# This file processes the raw data output downloaded from SHELDUS to create disasters.rds

library(data.table)
library(readr)
library(dplyr)
library(tidyverse)

file_name <- "data/direct_loss_aggregated_output_28975.csv" # THIS MUST BE CHANGED ACCORDING TO THE SHELDUS DATA YOU DOWNLOAD
disasters <- as.data.table(read_csv(file_name, show_col_types = FALSE)) |>
      select(StateName, CountyName, County_FIPS, Year, Hazard, CropDmg, `CropDmg(ADJ 2021)`, PropertyDmg, `PropertyDmg(ADJ 2021)`)

# Filter out disasters in areas that are not U.S. states (e.g. Puerto Rico)
disasters <- disasters |> mutate(State_FIPS = as.numeric(substr(County_FIPS, 1, 2)))
disasters <- disasters |> filter(State_FIPS <= 56) |> select(-State_FIPS)

setnames(disasters,
         old = c("County_FIPS", "Year"),
         new = c("county.geoid", "TAX_YEAR"))

# Manually fix possible FIPS code conflicts
disasters <- disasters |> mutate(fips_changes = county.geoid)

disasters$fips_changes[disasters$county.geoid == "02158"] <- "02270" # 02270 got new FIPS 02158 in 2015
disasters$fips_changes[disasters$county.geoid == "46102"] <- "46113" # 46113 got new FIPS 46102 in 2015
disasters$fips_changes[disasters$county.geoid == "51780"] <- "51083" # 51780 added to 51083 in 1995
disasters$fips_changes[disasters$county.geoid == "51560"] <- "51005" # 51560 added to 51005 in 2001
disasters$fips_changes[disasters$county.geoid == "51515"] <- "51019" # 51515 added to 51019 in 2013
disasters$fips_changes[disasters$county.geoid == "12025"] <- "12086" # 12025 got new FIPS 12086 in 1997

# Checking for duplicates
key_var <- c("county.geoid", "TAX_YEAR", "Hazard")
dup_groups <- disasters[, .N, by = key_var][N > 1]
dups <- disasters[dup_groups, on = key_var]

# Checking for duplicates
key_var <- c("fips_changes", "TAX_YEAR", "Hazard")
dup_groups2 <- disasters[, .N, by = key_var][N > 1]
dups2 <- disasters[dup_groups2, on = key_var] 

# We can safely remove the records where CountyName begins with an asterick because those are "historical" counties that no longer exist
dups <- dups2[substr(CountyName, 1, 1) == "*"]
disasters <- fsetdiff(disasters, dups[, !c("N"), with = FALSE])

# Collapse to a per-county-per-year dataset
disasters <- disasters |> 
      group_by(fips_changes, TAX_YEAR) |>
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE), .names = "{.col}_TOT"),
                n_disasters = n()) |>
      ungroup()

# saveRDS(disasters, "data/disasters.rds") # uncomment this line of code to save the file
