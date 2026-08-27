library(tidyr)
library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(geosphere)
library(fields)
library(khroma)

create_formulas <- function(panel_df, svc_flag = FALSE){
      se_vars <- colnames(panel_df)[which(colnames(panel_df) == "bachelors_perc"):which(colnames(panel_df) == "total_population")] 
      org_vars <- colnames(panel_df)[which(colnames(panel_df) == "TOT_REV"):which(colnames(panel_df) == "NTEEV2_NA")] 
      loc_vars <- colnames(panel_df)[which(colnames(panel_df) == "REGION_MIDWEST"):which(colnames(panel_df) == "REGION_WEST")] 
      
      if (svc_flag){
            svc_vars <- colnames(panel_df)[which(colnames(panel_df) == "x_s1"):ncol(panel_df)] # all spatially varying coefficients
            formulas <- list("L1" = paste("~ ",
                                          paste0("I(lag(", se_vars, ", 0:1))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:1))", collapse = " + "), "+",
                                          paste(svc_vars, collapse = " + ")), 
                             "L3" = paste("~ ",
                                          paste0("I(lag(", se_vars, ", 0:3))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:3))", collapse = " + "), "+",
                                          paste(svc_vars, collapse = " + ")), 
                             "L5" = paste("~ ",
                                          paste0("I(lag(", se_vars, ", 0:5))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:5))", collapse = " + "), "+",
                                          paste(svc_vars, collapse = " + "))
                             )
      } else {
            formulas <- list("L1" = paste("~ lat*lng +",
                                          paste0("I(lag(", se_vars, ", 0:1))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:1))", collapse = " + ")), 
                             "L3" = paste("~ lat*lng +",
                                          paste0("I(lag(", se_vars, ", 0:3))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:3))", collapse = " + ")), 
                             "L5" = paste("~ lat*lng +",
                                          paste0("I(lag(", se_vars, ", 0:5))", collapse = " + "), "+",
                                          paste0("I(lag(", org_vars, ", 1:5))", collapse = " + "))
            )
      }
      return(list("formulas" = formulas, "covs" = c(se_vars, org_vars, loc_vars)))
      
}

create_adj_list <- function(panel_data, dist_cutoff = 25){
      coords <- as.data.frame(panel_data |> dplyr::select(geoid_2010, lng, lat) |> unique())
      county_IDs <- coords$geoid_2010
      
      # Border-based adj list
      census_df <- read.delim("counties_data/county_adjacency2010.txt",
                              header = FALSE,
                              sep = "\t",
                              col.names = c("county_name", "county_geoid",
                                            "neighbor_name", "neighbor_geoid"),
                              colClasses = c("character", "character", "character", "character"),
                              stringsAsFactors = FALSE)
      
      census_df <- census_df |>
            mutate(across(everything(), \(x) na_if(x, y = ""))) # encode empty strings as missing values
      
      census_df <- census_df |>
            fill(county_name, county_geoid, .direction = "down") # fill down with last value 
      
      census_df <- census_df[census_df$county_geoid != census_df$neighbor_geoid, ] # remove self-edges
      
      census_df <- census_df |> filter((county_geoid %in% county_IDs) & (neighbor_geoid %in% county_IDs)) # only keep the counties that are in my panel data
      
      census_adj_list <- split(x = census_df$neighbor_geoid, f = census_df$county_geoid) 
      
      missing <- setdiff(county_IDs, names(census_adj_list))
      for (m in missing) {
            census_adj_list[[m]] <- character(0)
      } # fill in any missing counties
      
      # Distance based adj list
      D <- distm(coords |> dplyr::select(-geoid_2010), fun = distHaversine) / 1609.344 # meters to miles conversion
      dimnames(D) <- list(county_IDs, county_IDs)
      
      idx <- which(D <= dist_cutoff & D > 0, arr.ind = TRUE) # list of array indices where distance is within cutoff
      
      edges <- data.frame(county1 = county_IDs[idx[, 1]], 
                          county2 = county_IDs[idx[, 2]], 
                          stringsAsFactors = FALSE)
      
      dist_adj_list <- split(edges$county2, edges$county1)
      
      # If there are missing counties, that means that they had no neighboring counties within the cutoff distance -- so can list an empty vector for them
      missing <- setdiff(unique(df$geoid_2010), names(dist_adj_list))
      for (m in missing) {
            dist_adj_list[[m]] <- character(0)
      }
      
      # for every county, new adj_list is union of the two
      adj_union <- setNames(
            lapply(names(dist_adj_list), function(n) union(dist_adj_list[[n]], census_adj_list[[n]])),
            names(dist_adj_list)
      )
      
      return(list("union" = adj_union, "border" = census_adj_list, "dist" = dist_adj_list))
}

add_exposed <- function(panel_data, adj_list, new_col = "treat.FEMA.adj", treatment = "treat.FEMA"){
      panel_data[[new_col]] <- panel_data[[treatment]] # initialize adjusted treatment with original
      years <- unique(panel_data$TAX_YEAR)
      
      for (yr in years) {
            idx <- which(panel_data$TAX_YEAR == yr) # Subset indices for this year
            treated <- panel_data$geoid_2010[idx][panel_data[[treatment]][idx] == 1]
            
            if (length(treated) == 0) next # If no treated counties, skip
            
            neighbors <- unique(unlist(adj_list[treated])) # Get neighbors of treated counties
            
            untreated_idx <- idx[panel_data[[treatment]][idx] == 0] # Identify untreated counties in this year
            
            exposed <- panel_data$geoid_2010[untreated_idx] %in% neighbors # Among untreated, find those exposed
            
            panel_data[[new_col]][untreated_idx][exposed] <- NA # Set exposed controls to NA
      }
      
      # per year I want to see the counts of treated, control and missing for treat.FEMA.adj
      per_year_summary <- panel_data |> dplyr::group_by(TAX_YEAR) |>
            dplyr::summarise(
                  n_1 = sum(.data[[new_col]] == 1, na.rm = TRUE),
                  n_0 = sum(.data[[new_col]] == 0, na.rm = TRUE),
                  n_NA = sum(is.na(.data[[new_col]])),
                  n = n())
      
      return(list("df" = panel_data, "summary" = per_year_summary))
}

add_SVC <- function(df, mat_smooth = 0.5, num_E_vec = 30){
      library(MASS)
      df_sub <- df |> dplyr::select(CountyID, lat, lng) |> unique()
      x <- cbind(df_sub$lat, df_sub$lng) # inputs: latitude and longitude
      
      # SVC
      d <- rdist(x)
      K <- Matern(d, smoothness=mat_smooth)
      E <- eigen(K)
      x_s <- cbind(x[,1] * E$vectors[,1:num_E_vec], x[,2] * E$vectors[,1:num_E_vec]) 
      
      # add SVC back into panel data
      xs_vars <- paste0("x_s", seq_len(ncol(x_s)))
      colnames(x_s) <- xs_vars
      df_sub <- cbind(df_sub, x_s)
      
      df <- merge(df, df_sub |> dplyr::select(-lat, -lng), by = "CountyID", all = TRUE)
      
      return(df)
}

tidy_bal_df <- function(bal_data, covariates, long_or_wide = "long"){
      df <- bal_data |> 
            mutate(time = as.integer(str_remove(time, "t_"))) |>
            pivot_longer(cols = all_of(c(covariates, paste0(covariates, "_unrefined"))),
                         names_to = "covariate",
                         values_to = "value") |>
            mutate(unrefined = str_ends(covariate, "_unrefined"),
                   covariate = str_remove(covariate, "_unrefined$"),
                   covariate = str_remove(covariate, "m.out."))
      
      if (long_or_wide == "wide"){
            df <- df |> 
                  pivot_wider(id_cols = c(time, unrefined),
                              names_from = covariate,
                              values_from = value)
      }
      
      return(df)
}

no_int_no_SVC_plots <- function(bal_df_tidy, match_method, ylim_base = c(-1,1), ylim_ntee = c(-1,1), ylim_loc = c(-1,1), ylim_all = c(-1,1), ylim_unrefined = c(-1,1), include.unrefined.panel = FALSE){
      bal_df_tidy <- bal_df_tidy |> 
            mutate(covariate = case_when(covariate == "NTEEV2_NA" ~ "NTEE_NA", .default = covariate),
                   covariate = str_remove(covariate, "NTEEV2_"),
                   covariate = str_remove(covariate, "REGION_"))
      
      se_vars <- c("bachelors_perc", "med_household_income_adj", "white_perc",  "total_population")
      nonprofit_vars <- c("TOT_REV", "TOT_ASSET", "TOT_EXP")
      ntee_vars <- c("ART", "EDU", "ENV", "HEL", "HMS", "HOS", "IFA", "MMB", "PSB", "REL", "UNI", "UNU", "NTEE_NA")
      loc_vars <- c("MIDWEST", "NORTHEAST", "SOUTH", "WEST")
      
      
      df <- bal_df_tidy |> 
            mutate(variable_type = case_when(
                  covariate %in% se_vars ~ "socioeconomic",
                  covariate %in% nonprofit_vars ~ "nonprofit",
                  covariate %in% ntee_vars ~ "ntee",
                  covariate %in% loc_vars ~ "location",
                  .default = "misc"
            ))
      
      p1 <- df |> 
            filter(!unrefined & variable_type != "ntee" & variable_type != "location") |> 
            arrange(variable_type, covariate) |>
            mutate(covariate = factor(covariate, levels = unique(covariate))) |>
            ggplot(mapping = aes(x = time, y = value)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(mapping = aes(colour = covariate)) +
            geom_line(mapping = aes(colour = covariate, linetype = variable_type)) +
            scale_linetype_manual(values = c("socioeconomic" = "solid", "nonprofit" = "dotdash")) +
            scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
            theme_bw() +
            coord_cartesian(ylim = ylim_base) +
            scale_color_bright() +
            labs(title = paste(match_method, ":", "Covariate Balance"))
      
      p2 <- df |> 
            filter(!unrefined & variable_type == "ntee") |> 
            arrange(variable_type, covariate) |>
            mutate(covariate = factor(covariate, levels = unique(covariate))) |>
            ggplot(mapping = aes(x = time, y = value)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(mapping = aes(colour = covariate)) +
            geom_line(mapping = aes(colour = covariate)) +
            scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
            theme_bw() +
            coord_cartesian(ylim = ylim_ntee) +
            labs(title = paste(match_method, ":", "Covariate Balance - NTEE Categories"))
      
      p3 <- df |> 
            filter(!unrefined & variable_type == "location") |> 
            arrange(variable_type, covariate) |>
            mutate(covariate = factor(covariate, levels = unique(covariate))) |>
            ggplot(mapping = aes(x = time, y = value)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(mapping = aes(colour = covariate)) +
            geom_line(mapping = aes(colour = covariate)) +
            scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
            theme_bw() +
            coord_cartesian(ylim = ylim_loc) +
            labs(title = paste(match_method, ":", "Covariate Balance - Location"))
      
      p4 <- df |> 
            filter(!unrefined) |> 
            arrange(variable_type, covariate) |>
            mutate(covariate = factor(covariate, levels = unique(covariate))) |>
            ggplot(mapping = aes(x = time, y = value)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(mapping = aes(colour = covariate)) + #mapping = aes(colour = covariate)
            geom_line(mapping = aes(colour = covariate)) + #mapping = aes(colour = covariate, linetype = variable_type) # scale_linetype_manual(values = c("socioeconomic" = "solid", "nonprofit" = "dotdash", "ntee" = "F1", "location" = "dotted")) + 
            scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
            theme_bw() +
            coord_cartesian(ylim = ylim_all) +
            labs(title = paste(match_method, ":", "Covariate Balance - All Variables"))
      
      
      if (include.unrefined.panel){
            p1_un <- df |> 
                  filter(unrefined & variable_type != "ntee" & variable_type != "location") |> 
                  arrange(variable_type, covariate) |>
                  mutate(covariate = factor(covariate, levels = unique(covariate))) |>
                  ggplot(mapping = aes(x = time, y = value)) +
                  geom_hline(yintercept = 0, linetype = "dashed") +
                  geom_point(mapping = aes(colour = covariate)) + #mapping = aes(colour = covariate)
                  geom_line(mapping = aes(colour = covariate)) + #mapping = aes(colour = covariate, linetype = variable_type) # scale_linetype_manual(values = c("socioeconomic" = "solid", "nonprofit" = "dotdash", "ntee" = "F1", "location" = "dotted")) + 
                  scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
                  theme_bw() +
                  coord_cartesian(ylim = ylim_unrefined) +
                  scale_color_bright() +
                  labs(title = paste(match_method, ":", "Covariate Balance (unrefined)"))
            
            p2_un <- df |> 
                  filter(unrefined & variable_type == "ntee") |> 
                  arrange(variable_type, covariate) |>
                  mutate(covariate = factor(covariate, levels = unique(covariate))) |>
                  ggplot(mapping = aes(x = time, y = value)) +
                  geom_hline(yintercept = 0, linetype = "dashed") +
                  geom_point(mapping = aes(colour = covariate)) +
                  geom_line(mapping = aes(colour = covariate)) +
                  scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
                  theme_bw() +
                  coord_cartesian(ylim = ylim_unrefined) +
                  labs(title = paste(match_method, ":", "Covariate Balance (unrefined) - NTEE Categories"))
            
            p3_un <- df |> 
                  filter(unrefined & variable_type == "location") |> 
                  arrange(variable_type, covariate) |>
                  mutate(covariate = factor(covariate, levels = unique(covariate))) |>
                  ggplot(mapping = aes(x = time, y = value)) +
                  geom_hline(yintercept = 0, linetype = "dashed") +
                  geom_point(mapping = aes(colour = covariate)) +
                  geom_line(mapping = aes(colour = covariate)) +
                  scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
                  theme_bw() +
                  coord_cartesian(ylim = ylim_unrefined) +
                  labs(title = paste(match_method, ":", "Covariate Balance (unrefined) - Location"))
            
            p4_un <- df |> 
                  filter(unrefined) |> 
                  arrange(variable_type, covariate) |>
                  mutate(covariate = factor(covariate, levels = unique(covariate))) |>
                  ggplot(mapping = aes(x = time, y = value)) +
                  geom_hline(yintercept = 0, linetype = "dashed") +
                  geom_point(mapping = aes(colour = covariate)) +
                  geom_line(mapping = aes(colour = covariate)) +
                  scale_x_reverse(breaks = c(5, 4, 3, 2, 1, 0), labels = c("t-5", "t-4", "t-3", "t-2", "t-1", "t")) +
                  theme_bw() +
                  coord_cartesian(ylim = ylim_all) +
                  labs(title = paste(match_method, ":", "Covariate Balance (Unrefined) - All Variables"))
            return(list("base" = p1, "base_unrefined" = p1_un, "ntee" = p2, "ntee_unrefined" = p2_un, "loc" = p3, "loc_unrefined" = p3_un, "all" = p4, "all_unrefined" = p4_un))
      } else {
            return(list("base" = p1, "ntee" = p2, "location" = p3, "all" = p4))
      }
}