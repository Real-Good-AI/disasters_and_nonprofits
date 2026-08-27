library(tidyverse)
library(hash)
library(curl)

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