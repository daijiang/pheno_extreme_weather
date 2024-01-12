# before running this script, make sure to clear environment, and restart r session

# # https://static-content.springer.com/esm/art%3A10.1038%2Fnature09763/MediaObjects/41586_2011_BFnature09763_MOESM64_ESM.pdf
# # We assume that the annual extreme precipitation amounts analyzed here, RX1D and RX5D, follow the generalized extreme value (GEV) distribution which incorporates Gumbel, Frechet, and Weibull distributions and has a cumulative distribution function (CDF) as described in Eq. (1) in the Methods Summary of the main paper.
# 
# 
# daymet_daily_date = read_rds("data/daymet_daily_date.rds")
# 
# tst = filter(daymet_daily_date, id_cells == "cell_2", doy == 1)
# hist(tst$precip)
# sum(tst$precip == 0) # 34 / 43
# sort(tst$precip)
# 
# mean(tst$precip) + 2 * sd(tst$precip)
# 
# 
# install.packages("EnvStats")
# library(EnvStats)
# 
# x = rgevd(20, location = 2, scale = 1, shape = 0.2) 
# hist(x)
# sort(x)
# 
# x = egevd(tst$precip, method = "mle")
# x = egevd(tst$precip, method = "pwme")
# x = egevd(tst$precip, method = "tsoe")
# x
# qgevd(p = seq(0.05, 0.95, 0.05), location = x$parameters["location"], 
#       scale = x$parameters["scale"], shape = x$parameters["shape"])
# 
# hist(tst$tmin)
# x = egevd(tst$tmin, method = "pwme")
# x
# qgevd(p = seq(0.05, 0.95, 0.05), location = x$parameters["location"], 
#       scale = x$parameters["scale"], shape = x$parameters["shape"])



if(!file.exists("data_output/df_25km_anomaly.rds")){
  do_i_j = function(ij = c(1:1000)){
    job::job({
      source("code/func_daily.R")
      df_25k_pheno_anom = vector("list", length = length(ij))
      f_s = paste0("data_output/df_25k_pheno_anom_", min(ij), "_", max(ij), ".rds")
      for(m in seq_along(ij)){
        cat("m = ", m, "out of", length(ij))
        df_25k_pheno_anom[[m]] = detect_unsual_days2_window(ij[m], n_day_ahead = 60)
        if(m %% 100 == 0) saveRDS(df_25k_pheno_anom, file = f_s)
      }
      saveRDS(df_25k_pheno_anom, file = f_s)
    })
  }
  
  do_i_j(1:1000)
  do_i_j(1001:2000)
  do_i_j(2001:3000)
  do_i_j(3001:4000)
  do_i_j(4001:5000)
  do_i_j(5001:6000)
  do_i_j(6001:7000)
  do_i_j(7001:8000)
  do_i_j(8001:9000)
  do_i_j(9001:10000)
  
  do_i_j(10001:11000)
  do_i_j(11001:12000)
  do_i_j(12001:13000)
  do_i_j(13001:14000)
  do_i_j(14001:15000)
  do_i_j(15001:16000)
  do_i_j(16001:17000)
  do_i_j(17001:18000)
  do_i_j(18001:19000)
  do_i_j(19001:20000)
  
  do_i_j(20001:21000)
  do_i_j(21001:21317)
  # do_i_j(22001:23000)
  # do_i_j(23001:24000)
  # do_i_j(24001:24550)
  
  read_i_j = function(ij){
    bind_rows(readRDS(paste0("data_output/df_25k_pheno_anom_", min(ij), "_", max(ij), ".rds")))
  }
  
  anom = bind_rows(
    read_i_j(1:1000),
    read_i_j(1001:2000),
    read_i_j(2001:3000),
    read_i_j(3001:4000),
    read_i_j(4001:5000),
    read_i_j(5001:6000),
    read_i_j(6001:7000),
    read_i_j(7001:8000),
    read_i_j(8001:9000),
    read_i_j(9001:10000),
    read_i_j(10001:11000),
    read_i_j(11001:12000),
    read_i_j(12001:13000),
    read_i_j(13001:14000),
    read_i_j(14001:15000),
    read_i_j(15001:16000),
    read_i_j(16001:17000),
    read_i_j(17001:18000),
    read_i_j(18001:19000),
    read_i_j(19001:20000),
    read_i_j(20001:21000),
    read_i_j(21001:21317) # ,
    # read_i_j(22001:23000),
    # read_i_j(23001:24000),
    # read_i_j(24001:24550)
  )
  
  write_rds(anom, "data_output/df_25km_anomaly.rds")
} else {
  anom = readRDS("data_output/df_25km_anomaly.rds")
}
