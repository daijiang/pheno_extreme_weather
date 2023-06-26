# before running this script, make sure to clear environment, and restart r session

if(!file.exists("data_output/df_25km_anomaly.rds")){
  job::job({
    source("code/func_daily.R")
    df_25k_pheno_anom_1_3000 = vector("list", length = 3000)
    for(i in 1:3000){
      cat("i = ", i, "out of 3000")
      df_25k_pheno_anom_1_3000[[i]] = detect_unsual_days2(i, n_day_ahead = 90)
      if(i %% 100 == 0) saveRDS(df_25k_pheno_anom_1_3000, "data_output/df_25km_anomaly_1_3000.rds")
    }
    saveRDS(df_25k_pheno_anom_1_3000, "data_output/df_25km_anomaly_1_3000.rds")
  })
  
  job::job({
    df_25k_pheno_anom_3001_6000 = vector("list", length = 3000)
    source("func_daily.R")
    
    for(i in 3001:6000){
      cat("i = ", i, "out of 3000")
      df_25k_pheno_anom_3001_6000[[i]] = detect_unsual_days2(i, n_day_ahead = 90)
      if(i %% 100 == 0) saveRDS(df_25k_pheno_anom_3001_6000, "data_output/df_25km_anomaly_3001_6000.rds")
    }
    saveRDS(df_25k_pheno_anom_3001_6000, "data_output/df_25km_anomaly_3001_6000.rds")
  })
  
  job::job({
    df_25k_pheno_anom_6001_9000 = vector("list", length = 3000)
    source("func_daily.R")
    
    for(i in 6001:9000){
      cat("i = ", i, "out of 3000")
      df_25k_pheno_anom_6001_9000[[i]] = detect_unsual_days2(i, n_day_ahead = 90)
      if(i %% 100 == 0) saveRDS(df_25k_pheno_anom_6001_9000, "data_output/df_25km_anomaly_6001_9000.rds")
    }
    saveRDS(df_25k_pheno_anom_6001_9000, "data_output/df_25km_anomaly_6001_9000.rds")
  })
  
  job::job({
    df_25k_pheno_anom_9001_13435 = vector("list", length = 4435)
    source("func_daily.R")
    
    for(i in 9001:nrow(df_25k_pheno)){
      cat("i = ", i, "out of 4435")
      df_25k_pheno_anom_9001_13435[[i]] = detect_unsual_days2(i, n_day_ahead = 90)
      if(i %% 100 == 0) saveRDS(df_25k_pheno_anom_9001_13435, "data_output/df_25km_anomaly_9001_13435.rds")
    }
    saveRDS(df_25k_pheno_anom_9001_13435, "data_output/df_25km_anomaly_9001_13435.rds")
  })
  
  
  anom = bind_rows(read_rds("data_output/df_25km_anomaly_1_3000.rds") |> bind_rows(),
                   read_rds("data_output/df_25km_anomaly_3001_6000.rds") |> bind_rows(),
                   read_rds("data_output/df_25km_anomaly_6001_9000.rds") |> bind_rows(),
                   read_rds("data_output/df_25km_anomaly_9001_13435.rds") |> bind_rows())
  
  write_rds(anom, "data_output/df_25km_anomaly.rds")
} else {
  anom = readRDS("data_output/df_25km_anomaly.rds")
}
