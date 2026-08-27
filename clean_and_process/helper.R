library(tidyverse)
library(hash)
library(curl)
library(tibble)
library(dplyr)

###########################################################################################################################
# Step 1(a): Download desired data files from NCCS Core Data Catalog: https://nccs.urban.org/nccs/catalogs/catalog-core.html
###########################################################################################################################
download_CORE <- function(tscope_values, fscope_values, year_values, directory = "CORE"){
      if (!dir.exists(directory)) {dir.create(directory)}
      setwd(directory)
      
      if (!dir.exists("pz")) {dir.create("pz")}
      if (!dir.exists("pc")) {dir.create("pc")}
      if (!dir.exists("pf")) {dir.create("pf")}
      
      keys <- c("501c3-pz", "501c3-pc", "501ce-pz", "501ce-pc", "501c3-pf")
      values <- c("-501C3-CHARITIES-PZ-HRMN.csv", "-501C3-CHARITIES-PC-HRMN.csv", "-501CE-NONPROFIT-PZ-HRMN.csv", "-501CE-NONPROFIT-PC-HRMN.csv", "-501C3-PRIVFOUND-PF-HRMN-V0.csv")
      filename_dict <- hash(keys, values) # dictionary returning the file name convention determined by tscope and fscope
      base_url     <- "https://nccsdata.s3.amazonaws.com/harmonized/core/"
      
      for (tscope in tscope_values){
            for (fscope in fscope_values){
                  for (year in year_values){
                        # PC only collected starting in 2012; if year is before 2012, skip this iteration
                        if (fscope == "-pc" & year < 2012){next}
                        # PF files only have 501c3
                        if (fscope == "-pf" & tscope == "501ce"){next}
                        t_and_f_scope <- paste(tscope, fscope, sep="")
                        filename <- paste("CORE-", year, filename_dict[[t_and_f_scope]], sep = "")
                        # slightly different URL depending on whether fscope is pf
                        if (fscope != "-pf"){
                              full_url <- paste(base_url, t_and_f_scope, "/", filename, sep = "")
                        } else {
                              full_url <- paste(base_url, t_and_f_scope, "/marts/", filename, sep = "")
                        }
                        dest_path <- paste(substrRight(fscope, 2), "/", filename, sep="")
                        download.file( url=full_url, destfile=dest_path, method="curl" )
                        print(paste("Downloaded", filename, sep = " "))
                  }
            }    
      }
}

substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
}
###########################################################################################################################
# Step 1(b): Download data dictionaries and Unified BMF file
###########################################################################################################################
download_dicts <- function(){
      if (!dir.exists("CORE")) dir.create("CORE")
      
      # CORE Data dictionary as a .csv file
      core_dd_url <- "https://nccsdata.s3.amazonaws.com/harmonized/core/CORE-HRMN_dd.csv"
      download.file(url=core_dd_url, destfile="CORE/CORE-HRMN_dd.csv", method="curl")
      
      # Excel spreadsheet containing data dictionary for core data, BMF, and some other info
      harmonized_dd_url <- "https://nccsdata.s3.amazonaws.com/harmonized/harmonized_data_dictionary.xlsx"
      download.file(url=harmonized_dd_url, destfile="CORE/harmonized_data_dictionary.xlsx", method="curl")
}


download_bmf <- function(){
      if (!dir.exists("CORE")) dir.create("CORE")
      
      # Unified BMF file (contains organizational attributes like 501c type and NTEE code, which are not included in CORE data by default)
      unified_bmf_url <- "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv"
      download.file(url=unified_bmf_url, destfile="CORE/BMF_UNIFIED_V1.1.csv", method="curl")
}
###########################################################################################################################
# Example of how to call from another file
###########################################################################################################################
# source("download_data.R")
# tscope_values <- c("501c3") # Download files from Tax Exempt Types in this list; possible values: "501c3", "501ce" 
# fscope_values <- c("-pc") # Download files from IRS 990 Form Scope in this list; possible values: "-pz", "-pc", "-pf"
# year_values <- c(2021) # Download files from years in this list
# download_CORE(tscope_values, fscope_values, year_values)
# download_dicts()

compare_pair_dt <- function(dt_group, dollar_cols) {
      # number of records in this group
      n_records <- nrow(dt_group)
      
      # drop keys (convert to numeric matrix)
      mat <- as.matrix(dt_group)
      
      # names of all columns in the group
      coln <- colnames(dt_group)
      
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

na_counts_df <- function(df){
      n <- nrow(df)
      na_count <- sapply(df, function(y) sum(length(which(is.na(y))))) # get na_counts per column
      na_count <- data.frame(na_cnt = na_count)
      na_count <- na_count |>
            mutate(percent = na_cnt/n)
      na_count <- tibble::rownames_to_column(na_count, "var_name")
      return(na_count)
}

add_regions_and_divisions <- function(df){
      # Census Regions
      west <- c("AK","WA", "OR", "CA", "HI", "ID", "NV", "AZ", "MT", "UT", "WY", "CO", "NM")
      midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH")
      northeast <- c("NY", "PA", "NJ", "VT", "MA", "CT", "NH", "RI", "ME")
      south <- c("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "NC", "SC", "GA", "FL", "MD", "DC", "DE")
      
      # Add regions
      df <- df |> mutate(REGION = case_when(
            is.na(CENSUS_STATE_ABBR) ~ NA,
            CENSUS_STATE_ABBR %in% west ~ "WEST",
            CENSUS_STATE_ABBR %in% midwest ~ "MIDWEST",
            CENSUS_STATE_ABBR %in% northeast ~ "NORTHEAST",
            CENSUS_STATE_ABBR %in% south ~ "SOUTH",
            .default = "TERRITORY"
      ))
      
      # Census Divisions
      pacific <- c("AK","WA", "OR", "CA", "HI")
      mountain <- c("ID", "NV", "AZ", "MT", "UT", "WY", "CO", "NM")
      northWest.central <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO")
      northEast.central <- c("WI", "IL", "MI", "IN", "OH")
      southWest.central <- c("OK", "TX", "AR", "LA")
      southEast.central <- c("KY", "TN", "MS", "AL")
      south.atlantic <- c("WV", "VA", "NC", "SC", "GA", "FL", "MD", "DC", "DE")
      middle.atlantic <- c("NY", "PA", "NJ")
      newEngland <- c("VT", "MA", "CT", "NH", "RI", "ME")
      
      # Add divisions
      df <- df |> mutate(DIVISION = case_when(
            is.na(CENSUS_STATE_ABBR) ~ NA,
            CENSUS_STATE_ABBR %in% pacific ~ "PACIFIC",
            CENSUS_STATE_ABBR %in% mountain ~ "MOUNTAIN",
            CENSUS_STATE_ABBR %in% northWest.central ~ "CENTRAL-NW",
            CENSUS_STATE_ABBR %in% northEast.central ~ "CENTRAL-NE",
            CENSUS_STATE_ABBR %in% southWest.central ~ "CENTRAL-SW",
            CENSUS_STATE_ABBR %in% southEast.central ~ "CENTRAL-SE",
            CENSUS_STATE_ABBR %in% south.atlantic ~ "ATLANTIC-S",
            CENSUS_STATE_ABBR %in% middle.atlantic ~ "ATLANTIC-M",
            CENSUS_STATE_ABBR %in% newEngland ~ "NEW-ENGLAND",
            .default = "TERRITORY"
      ))
      
      return(df)
}