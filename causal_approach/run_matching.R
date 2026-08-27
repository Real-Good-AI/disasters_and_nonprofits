# Refer to Section "Matching Phase" in the file main_analysis.Rmd for instructions

library(data.table)
library(readr)
library(dplyr)
library(tidyverse)
library(PanelMatch)
library(writexl)
library(foreach)
library(doParallel)
source("matches_helper.R")
rm(add_exposed, add_SVC, create_adj_list, no_int_no_SVC_plots, tidy_bal_df)

default <- TRUE # CHANGE TO FALSE IF NOT RUNNING THE DEFAULT ANALYSIS

if (default){
      refine_methods <- c("none")
      lags <- c(3)
      no_interference_model_list <- list()
      interference_model_list <- list(
            list("full_path" = "no_svc/int_panelV1/config3/",
                 "data" = "panel_data/int_panelV1.rds",
                 "match_on_NA" = FALSE,
                 "svc_flag" = FALSE,
                 "J" = 20))
} else {
      refine_methods <- c("none", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")
      lags <- c(3) # Pena et al paper uses up to lag = 3 so we will too
}

save_methods <- list("none" = "no_match", "mahalanobis" = "maha_match", "ps.match" = "ps_match", "CBPS.match" = "CBPS_match", "ps.weight" = "ps_weight", "CBPS.weight" = "CBPS_weight")
matching_flags <- list("none" = FALSE, "mahalanobis" = TRUE, "ps.match" = TRUE, "CBPS.match" = TRUE, "ps.weight" = TRUE, "CBPS.weight" = TRUE)

no_interference_model_list <- list(
      # list("full_path" = "no_svc/no_int_panelV1/config2/",
      #      "data" = "panel_data/no_int_panelV1.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE, "J" = 20),
      # list("full_path" = "with_svc/no_int_panelV2/config3/",
      #      "data" = "panel_data/no_int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE, "J" = 20),
      # list("full_path" = "with_svc/no_int_panelV3/config4/",
      #      "data" = "panel_data/no_int_panelV3.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE, "J" = 20)
      # list("full_path" = "misc_analysis/no_int_none_L1_",
      #      "data" = "panel_data/no_int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE,
      #      "J" = 20)
)

interference_model_list <- list(
      # list("full_path" = "no_svc/int_panelV1/config2/",
      #      "data" = "panel_data/int_panelV1.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE, "J" = 20),
      # list("full_path" = "with_svc/int_panelV2/config3/",
      #      "data" = "panel_data/int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE,
      #      "J" = 20)
      # list("full_path" = "with_svc/int_panelV3/config4/",
      #      "data" = "panel_data/int_panelV3.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE, "J" = 20)
      # list("full_path" = "no_svc/int_panelV1/config3/",
      #      "data" = "panel_data/int_panelV1.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE, "J" = 20)
      # list("full_path" = "misc_analysis/dist00_",
      #      "data" = "panel_data/int_panel_d00.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE,
      #      "J" = 20),
      # list("full_path" = "misc_analysis/dist50_",
      #      "data" = "panel_data/int_panel_d50.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE,
      #      "J" = 20),
      # list("full_path" = "misc_analysis/int_svc_C3_J5_",
      #      "data" = "panel_data/int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE,
      #      "J" = 5),
      # list("full_path" = "misc_analysis/int_svc_C3_J50_",
      #      "data" = "panel_data/int_panelV2.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = TRUE,
      #      "J" = 50)
      # list("full_path" = "misc_analysis/int_none_L1_",
      #      "data" = "panel_data/int_panelV1.rds",
      #      "match_on_NA" = FALSE,
      #      "svc_flag" = FALSE,
      #      "J" = 20)
)

if (length(no_interference_model_list) != 0){
      cat("\n",paste(as.character(Sys.Date())), file = "run_matching_times.txt", append = TRUE)
      startTime.noInt <- Sys.time()
      for (mdl in no_interference_model_list){
            print(mdl$full_path)
      
            panel_df <- readRDS(mdl$data)
            res <- create_formulas(panel_df, svc_flag = mdl$svc_flag)
            formulas <- res$formulas
            covs_to_check <- res$covs
            
            startTime <- Sys.time()
            # cluster <- makeCluster(length(lags), outfile = "cl_log.txt")
            # registerDoParallel(cluster)
            # 
            # foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
            for(cur_LAG in lags){
                  print(paste("Lag =", cur_LAG))
      
                  match_list = list()
                  balance_list = list()
                  for (cur_METHOD in refine_methods){
                        print(paste("Method =", cur_METHOD))
      
                        m.out <- PanelMatch(panel.data = panel_df,
                                            lag = cur_LAG,
                                            refinement.method = cur_METHOD,
                                            verbose = TRUE,
                                            qoi = "att",
                                            size.match = mdl$J,
                                            match.missing = mdl$match_on_NA,
                                            covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                            lead = 1:5,
                                            matching = matching_flags[[cur_METHOD]],
                                            use.diagonal.variance.matrix = TRUE)
      
                        match_list[[cur_METHOD]] <- m.out
                        
                        print("balances...")
                        balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                            panel.data = panel_df,
                                                                            covariates = covs_to_check)
                  }
      
                  filename <- paste0("matches/", mdl$full_path, "Lag", cur_LAG, ".rds")
                  print(filename)
                  saveRDS(match_list, file = filename)
      
                  filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".rds")
                  print(filename)
                  saveRDS(balance_list, file = filename)
      
                  temp <- lapply(balance_list, summary)
                  temp <- lapply(temp, as.data.frame)
                  temp <- lapply(temp, tibble::rownames_to_column, var = "time")
      
                  filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".xlsx")
                  print(filename)
                  write_xlsx(temp, filename)
      
            }
            # stopCluster(cl = cluster)
            
            print(paste("Finished", mdl$full_path))
            timeDiff <- Sys.time() - startTime
            
            cat("\n",paste("Finished", mdl$full_path, timeDiff, attr(timeDiff, "units")), file = "run_matching_times.txt", append = TRUE)
      }
      print("Finished No Interference runs")
      print(Sys.time() - startTime.noInt)
}

if (length(interference_model_list) != 0){
      cat("\n",paste(as.character(Sys.Date())), file = "run_matching_times.txt", append = TRUE)
      startTime.int <- Sys.time()
      for (mdl in interference_model_list){
            print(mdl$full_path)
            
            panel_df <- readRDS(mdl$data)
            res <- create_formulas(panel_df, svc_flag = mdl$svc_flag)
            formulas <- res$formulas
            covs_to_check <- res$covs
            
            startTime <- Sys.time()
            # cluster <- makeCluster(length(lags), outfile = "cl_log.txt")
            # registerDoParallel(cluster)
            # 
            # foreach(cur_LAG = lags, .packages = c("PanelMatch", "readr", "writexl", "dplyr"), .verbose = TRUE) %dopar% {
            for(cur_LAG in lags){
                  print(paste("Lag =", cur_LAG))
                  
                  match_list = list()
                  balance_list = list()
                  for (cur_METHOD in refine_methods){
                        print(paste("Method =", cur_METHOD))
                        
                        m.out <- PanelMatch(panel.data = panel_df,
                                            lag = cur_LAG,
                                            refinement.method = cur_METHOD,
                                            verbose = TRUE,
                                            qoi = "att",
                                            size.match = mdl$J,
                                            match.missing = mdl$match_on_NA,
                                            covs.formula = as.formula(formulas[[paste0("L", cur_LAG)]]),
                                            lead = 1:5,
                                            matching = matching_flags[[cur_METHOD]],
                                            use.diagonal.variance.matrix = TRUE)
      
                        match_list[[cur_METHOD]] <- m.out
      
                        balance_list[[cur_METHOD]] <- get_covariate_balance(m.out,
                                                                            panel.data = panel_df,
                                                                            covariates = covs_to_check)
                  }
                  
                  filename <- paste0("matches/", mdl$full_path, "Lag", cur_LAG, ".rds")
                  print(filename)
                  saveRDS(match_list, file = filename)
      
                  filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".rds")
                  print(filename)
                  saveRDS(balance_list, file = filename)
      
                  temp <- lapply(balance_list, summary)
                  temp <- lapply(temp, as.data.frame)
                  temp <- lapply(temp, tibble::rownames_to_column, var = "time")
      
                  filename <- paste0("balances/", mdl$full_path, "Lag", cur_LAG, ".xlsx")
                  print(filename)
                  write_xlsx(temp, filename)
                  
            }
            # stopCluster(cl = cluster)
            
            print(paste("Finished", mdl$full_path))
            timeDiff <- Sys.time() - startTime
            cat("\n",paste("Finished", mdl$full_path, timeDiff, attr(timeDiff, "units")), file = "run_matching_times.txt", append = TRUE)
      }
      print("Finished Interference runs")
      print(Sys.time() - startTime.int)
}
