library(curl)

# Make sure you are in the rgai_summer2025_nccs/disasters_project directory before running this file

# Download the Unified Business Master File (BMF)
unified_bmf_url <- "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv"
download.file(url=unified_bmf_url, destfile="data/BMF_UNIFIED_V1.1.csv", method="curl")

# Download the CORE Files
source("../SCRIPTS/download_data.R")
tscope_values <- c("501c3", "501ce") # Download files from Tax Exempt Types in this list; possible values: "501c3", "501ce" 
fscope_values <- c("-pf", "-pz") # Download files from IRS 990 Form Scope in this list; possible values: "-pz", "-pc", "-pf"
year_values <- seq(from = 1991, to = 2021, by = 1) # Download files from years in this list
download_CORE(tscope_values, fscope_values, year_values, directory = "data")