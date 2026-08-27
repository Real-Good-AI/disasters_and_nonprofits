library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr)
library(tibble)

unified_bmf <- as.data.frame(read_csv("data/BMF_UNIFIED_V1.1.csv", show_col_types = FALSE)) # 3,462,997 records, 49 variables, 3,436,969 unique organizations
unified_bmf <- unified_bmf[!(unified_bmf$EIN2 %in% "EIN-00-0000000"),] # 16 organizations dropped
unified_bmf <- unified_bmf |> mutate_if(is.character, ~na_if(.,'')) # Replace any empty strings '' with NA values
      
# Keep only variables we care about
vars_to_keep <- c("EIN2", "NTEEV2", "CENSUS_BLOCK_FIPS")
bmf_subset <- unified_bmf[vars_to_keep]

# Get the NTEE 3-character Broad Category (should be the first 3 characters of NTEEV2)
bmf_subset$NTEEV2 <- substr(bmf_subset$NTEEV2, 1, 3)
bmf_subset <- bmf_subset |> distinct() # remove any exactly duplicated rows: 18,846 records dropped; 3,444,135 left

# CENSUS_BLOCK_FIPS should be a character and should be formatted with 15 characters including leading 0's
bmf_subset$CENSUS_BLOCK_FIPS <- as.character(bmf_subset$CENSUS_BLOCK_FIPS)
bmf_subset$CENSUS_BLOCK_FIPS <- sprintf("%015s", bmf_subset$CENSUS_BLOCK_FIPS)

# The county FIPS code is the first 5 characters of CENSUS_BLOCK_FIPS; then we can drop CENSUS_BLOCK_FIPS since we just need county code
bmf_subset$county.census.geoid <- substr(bmf_subset$CENSUS_BLOCK_FIPS, 1, 5)
bmf_subset$state.FIPS <- substr(bmf_subset$county.census.geoid, 1, 2)
bmf_subset$CENSUS_BLOCK_FIPS <- NULL

# Drop 6815 records where the county FIPS code is 00000; 189 of these have a missing NTEEV2 / left with 3,437,320 records
bmf_subset <- bmf_subset |> filter(county.census.geoid != "00000")

bmf_subset |> mutate(NTEEV2 = replace_na(NTEEV2, "Missing")) |>
      ggplot(aes(x = NTEEV2)) +
      geom_bar(aes(y = ..count.. / sum(..count..))) +
      labs(x = "NTEE Broad Sector", y = "Proportion", title = "Proportion of Nonprofits in Each NTEE Sector") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=1))
      

# There are still 7,152 organizations that have at least 2 records in the BMF; all issues are in NTEEV2 column and only about 500 due to missing values
# Set these to NA due to discrepancy
bmf_subset <- bmf_subset |>
      group_by(EIN2) |>
      mutate(n = n()) |>
      ungroup() 

bmf_subset <- bmf_subset |>
      mutate(NTEEV2 = ifelse(n > 1, NA_character_, NTEEV2)) |>
      distinct() 

bmf_subset |> mutate(NTEEV2 = replace_na(NTEEV2, "Missing")) |>
      ggplot(aes(x = NTEEV2)) +
      geom_bar(aes(y = ..count.. / sum(..count..))) +
      labs(x = "NTEE Broad Sector", y = "Proportion", title = "Proportion of Nonprofits in Each NTEE Sector") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=1))

# Final counts: 3,430,166 records; 172,827 records with missing NTEEV2 (~5% of all records)
saveRDS(bmf_subset |> select(-n), "data/cleanBMF.rds")