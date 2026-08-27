# RGAI Rating: 4
# https://chatgpt.com/share/69962c59-3838-8003-84f4-340e126a124b
# https://chatgpt.com/share/6996104a-dbec-8003-8d0c-0655d888c942
library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(fastDummies)

#################################################################################################
# Aggregate nonprofit data by county and year
#################################################################################################

df <- readRDS("data/mega.rds") # 14,432,124 records; 1,284,073 organizations

# Adjust dollar amounts for inflation to the year 2019
adj2019 <- as.data.frame(read_csv("data/usd_2019_conv.csv", show_col_types = FALSE))
df <- merge(df, adj2019, by.x = "TAX_YEAR", by.y = "YEAR")
df <- df |> mutate(TOT_ASSET_OG = TOT_ASSET,
                   TOT_REV_OG = TOT_REV,
                   TOT_EXP_OG = TOT_EXP,
                   TOT_ASSET = TOT_ASSET * ADJ_2019,
                   TOT_REV = TOT_REV * ADJ_2019,
                   TOT_EXP = TOT_EXP * ADJ_2019)
# Optional, just to keep minimal number of columns
df <- df |> select(-TOT_ASSET_OG, -TOT_REV_OG, -TOT_EXP_OG, -DATA_COUNT)

# Split up NTEEV2 into 14 dummy variables (12 categories, 1 unknown category, 1 missing category)
ntee <- df |> 
      select(EIN2, NTEEV2, TAX_YEAR, county.census.geoid) |>
      dummy_cols(select_columns = c("NTEEV2"), ignore_na = FALSE, remove_selected_columns = TRUE)

# Aggregate the totals of each NTEEV2 category by county and year
ntee <- ntee |> select(-EIN2) |>
      group_by(county.census.geoid, TAX_YEAR) |>
      summarise(across(everything(), ~sum(.x, na.rm = TRUE))) |>
      ungroup()

# Add in total number of organizations and proportion missing column
ntee <- ntee |> mutate(NUM_ORGS = rowSums(ntee |> select(-county.census.geoid, -TAX_YEAR)),
                       NTEE_NA_PROP = NTEEV2_NA/NUM_ORGS)

# Within each county,year pair, I want to keep track of the proportion of records with missing financial data
df.tot_rev <- df |> select(TAX_YEAR, TOT_REV, TOT_EXP, TOT_ASSET, county.census.geoid) |>
      rename(REV = TOT_REV, EXP = TOT_EXP, ASSET = TOT_ASSET) |>
      group_by(county.census.geoid, TAX_YEAR) |> 
      summarize(across(where(is.numeric), ~sum(is.na(.x))/n(), .names = "{.col}_prop_miss"),
                across(any_of(c("REV", "EXP", "ASSET")), ~sum(.x, na.rm = TRUE)),
                .groups = "drop",
                ) |>
      ungroup() |>
      rename(TOT_REV = REV, TOT_EXP = EXP, TOT_ASSET = ASSET)

# I need the total assets and revenue stratified by county, year, and NTEE category
df.ntee_rev <- df |> select(TAX_YEAR, TOT_REV, TOT_EXP, TOT_ASSET, county.census.geoid, NTEEV2) |>
      rename(REV = TOT_REV, EXP = TOT_EXP, ASSET = TOT_ASSET) |>
      mutate(NTEEV2 = replace_na(NTEEV2, "NA")) |>
      group_by(county.census.geoid, TAX_YEAR, NTEEV2) |> 
      summarize(across(any_of(c("REV", "EXP", "ASSET")), ~sum(.x, na.rm = TRUE)),
                .groups = "drop") |>
      ungroup()

df.ntee_rev <- df.ntee_rev |> select(-EXP) |>
      pivot_wider(names_from = NTEEV2, values_from = c("REV", "ASSET"), values_fill = 0)

# rev_cols <- grep("^REV_", names(df.ntee_rev), value = TRUE)
# ass_cols <- grep("^ASSET_", names(df.ntee_rev), value = TRUE)
# exp_cols <- grep("^EXP_", names(df.ntee_rev), value = TRUE)

# Now add in stratified by size as well (according to NCCS size categories)
# Levels: [0,100000) [100000,500000) [500000,1000000) [1000000,5000000) [5000000,10000000) [10000000,7.02e+10)
breaks_vec = c(0,100000,500000,1000000,5000000,10000000,max(df$TOT_EXP, na.rm = TRUE)+1)
df$SIZE.CAT <- df$TOT_EXP |> cut(breaks = breaks_vec, right = FALSE, labels = FALSE) |> factor()

df.size_rev <- df |> select(TAX_YEAR, TOT_REV, TOT_ASSET, SIZE.CAT, county.census.geoid, NTEEV2) |>
      rename(REV = TOT_REV, ASSET = TOT_ASSET) |>
      group_by(county.census.geoid, TAX_YEAR, SIZE.CAT) |> 
      summarize(across(any_of(c("REV", "EXP", "ASSET")), ~sum(.x, na.rm = TRUE)),
                .groups = "drop") |>
      ungroup() 

df.size_rev <- df.size_rev |>
      pivot_wider(names_from = SIZE.CAT, values_from = c("REV", "ASSET"), values_fill = 0, names_prefix = "SIZE.")

# df now will have total assets and revenue per county,year pair stratified by NTEE category
# as well as the number of nonprofits  per county,year pair stratified by NTEE category
df <- merge(df.ntee_rev, ntee, by = c("county.census.geoid", "TAX_YEAR")) 
df <- merge(df, df.tot_rev, by = c("county.census.geoid", "TAX_YEAR")) 
df <- merge(df, df.size_rev, by = c("county.census.geoid", "TAX_YEAR")) 

# IDs for two counties were changed after 2010. Some datasets use latest ID, others use 2010 ID
df <- df |> mutate(geoid_2010 = county.census.geoid)
df$geoid_2010[df$county.census.geoid == "02158"] <- "02270"
df$geoid_2010[df$county.census.geoid == "46102"] <- "46113"

rm(ntee, df.ntee_rev, df.tot_rev, df.size_rev)
#################################################################################################
# Add location data: county name, latitude & longitude of centroid, state, region, and division
#################################################################################################
counties <- as.data.table(read_csv("data/counties_data/uscounties.csv", show_col_types = FALSE))
counties <- counties |> select(-county_ascii, - county, - population) |> rename(CENSUS_STATE_ABBR = state_id)

source("../SCRIPTS/clean_helper.R")
counties <- add_regions_and_divisions(counties)

# Some counties must be filled in manually
cnames <- c("Valdez-Cordova Census Area", 
            "Fairfield County", "Hartford County", "Litchfield County", "Middlesex County",
            "New Haven County", "New London County", "Tolland County", "Windham County")
fips <- c("02261", "09001", "09003", "09005", "09007", "09009", "09011", "09013", "09015")
states <- c("AK", "CT", "CT", "CT", "CT", "CT", "CT", "CT", "CT")
state_names <- c("Alaska", 
                 "Connecticut", "Connecticut", "Connecticut", "Connecticut", 
                 "Connecticut", "Connecticut", "Connecticut", "Connecticut")
lats <- c(61.34984, 41.227413, 41.806053, 41.79188, 41.433003, 41.349717, 41.472652, 41.858081, 41.824999)
longs <- c(-145.023141, -73.367061, -72.732916, -73.235404, -72.52278, -72.900204, -72.108634, -72.340978, -71.990702)
regs <- c("WEST",
          "NORTHEAST", "NORTHEAST", "NORTHEAST", "NORTHEAST", 
          "NORTHEAST", "NORTHEAST", "NORTHEAST", "NORTHEAST")
divs <- c("PACIFIC", 
          "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", 
          "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND", "NEW-ENGLAND")

additions <- data.frame(cnames, fips, states, state_names, lats, longs, regs, divs)
names(additions) <- names(counties)

counties <- rbind(counties, additions)

df <- merge(df, counties, by.x = "county.census.geoid", by.y = "county_fips", all.x = TRUE) # df and counties use the newer FIPS code not 2010

rm(counties, additions, cnames, divs, fips, lats, longs, regs, state_names, states, compare_pair_dt)
#################################################################################################
# add census crosswalk data (socioeconomic factors)
# Since some of these may be used as pre-treatment variables to adjust for, make sure they never happen AFTER treatment
# So, for the year 2000 my variables need to come from before 2000. For 2010, before 2010.
# The following creates a LAG_YEAR which I will join with TAX_YEAR and make sure the data is always from BEFORE the TAX_YEAR to be safe
#################################################################################################
source("https://raw.githubusercontent.com/UI-Research/nccs-geo/main/get_census_data.R")
census_df <- get_census_data(geo="county", 
                             years=c(1990, 2000, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

dt <- as.data.table(census_df)
dt[, CENSUS_YEAR := as.integer(substr(year, nchar(year)-3, nchar(year)))] # since year column is char with ranges, get last year

# Create all county × LAG_YEAR combinations
lag_years <- CJ(geoid_2010 = unique(dt$geoid_2010),
                LAG_YEAR = 1991:2021)

# Create helper column for rolling join
lag_years[, JOIN_YEAR := LAG_YEAR - 1]

# Set keys for rolling join
setkey(dt, geoid_2010, CENSUS_YEAR)
setkey(lag_years, geoid_2010, JOIN_YEAR)

# Rolling join: get most recent census year < LAG_YEAR
result <- dt[lag_years, on = .(geoid_2010, CENSUS_YEAR = JOIN_YEAR), roll = Inf]

result[, .(year, CENSUS_YEAR, LAG_YEAR)]

# Now, I can merge with df!
df <- merge(df, result, 
            by.x = c("geoid_2010", "TAX_YEAR"), 
            by.y = c("geoid_2010", "LAG_YEAR"), all.x = TRUE)

rm(census_df, dt, lag_years, result)
#################################################################################################
# Disaster Data 
#################################################################################################
disasters <- readRDS("data/disasters.rds")

disasters <- merge(disasters, adj2019, by.x = "TAX_YEAR", by.y = "YEAR")
disasters <- disasters |>  mutate(TotDmg = CropDmg_TOT + PropertyDmg_TOT,
                                  TotDmgADJ = TotDmg * ADJ_2019)       

df <- merge(df, 
            disasters |> select(fips_changes, TAX_YEAR, TotDmg, TotDmgADJ, n_disasters), 
            by.x = c('geoid_2010', 'TAX_YEAR'), 
            by.y = c('fips_changes', 'TAX_YEAR'), all.x = TRUE)

df <- df |> mutate(TotDmgADJ = replace_na(TotDmgADJ, 0),
                   n_disasters = replace_na(n_disasters, 0))

#################################################################################################
# Disaster Data Treatment Variable for each year
#################################################################################################
# Based on FEMA county per capita impact indicator
county_FEMA_indicator <- 3.78 # 2019: 3.78, 2021: 3.89

n.counties <- length(unique(df$county.census.geoid))
avg.n.dis.per.year <- disasters |> 
      group_by(TAX_YEAR) |> 
      summarise(avg.num.dis = sum(n_disasters)/n.counties) |>
      mutate(FEMA_threshADJ = county_FEMA_indicator * avg.num.dis)
# write.csv(avg.n.dis.per.year, "FEMA_thresholds_adj.csv")

df <- merge(df, avg.n.dis.per.year, by = "TAX_YEAR")

df <- df |> mutate(total_population = as.numeric(total_population),
                   TotPerCapADJ = TotDmgADJ/total_population,
                   treat.FEMA = as.numeric(TotPerCapADJ >= FEMA_threshADJ))

# saveRDS(df, "data/county_long.rds")

