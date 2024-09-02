# comparison plots for MREP stock assessment toy
# Base case and under reported catch
library(tidyverse)
library(here)
library(Rmisc)
library(gmRi)
# include projections in plots?

base <- readRDS(here::here("WHAM_runs/Base/Base.rds"))
#bias <- readRDS(here::here("WHAM_runs/BiasCatch/BiasCatch.rds"))
bias <- readRDS(here::here("WHAM_runs/BiasCatch/HighCatch.rds"))
bias <- readRDS(here::here("WHAM_runs/BiasIndex/BiasIndex.rds"))

#### SSB plot ####

base_metrics <- sapply(base$reps, function(x) {
    return(x$SSB)
  }) %>%
  as.data.frame()
base_metrics_transpose <- t(base_metrics) %>%
  as.data.frame()

cols <- length(base_metrics_transpose)
SSB_base <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(base_metrics_transpose[, i], ci = 0.95)
  SSB_base[i, ] <- temp
}
SSB_base_data <- as.data.frame(SSB_base) %>%
  dplyr::rename(upper = V1, SSB = V2, lower = V3)
year <- 1980:2021

# bias
bias_metrics <- sapply(bias$reps, function(x) {
    return(x$SSB)
  }) %>%
  as.data.frame()
bias_metrics_transpose <- t(bias_metrics) %>%
  as.data.frame()

cols <- length(bias_metrics_transpose)
SSB_bias <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(bias_metrics_transpose[, i], ci = 0.95)
  SSB_bias[i, ] <- temp
}
SSB_bias_data <- as.data.frame(SSB_bias) %>%
  dplyr::rename(upper = V1, SSB = V2, lower = V3)

ggplot() +
  geom_line(data = SSB_base_data, aes(x = year, y = SSB), color = gmri_cols("green")) +
  geom_ribbon(data = SSB_base_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = SSB_bias_data, aes(x = year, y = SSB), color = gmri_cols("gmri blue")) +
  geom_ribbon(data = SSB_bias_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), alpha = 0.5)


#### F plot ####

base_metrics <- sapply(base$reps, function(x) {
    return(x$F)
  }) %>%
  as.data.frame()
base_metrics_transpose <- t(base_metrics) %>%
  as.data.frame()

cols <- length(base_metrics_transpose)
F_base <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(base_metrics_transpose[, i], ci = 0.95)
  F_base[i, ] <- temp
}
F_base_data <- as.data.frame(F_base) %>%
  dplyr::rename(upper = V1, F = V2, lower = V3)
year <- 1980:2021

# bias
bias_metrics <- sapply(bias$reps, function(x) {
    return(x$F)
  }) %>%
  as.data.frame()
bias_metrics_transpose <- t(bias_metrics) %>%
  as.data.frame()

cols <- length(bias_metrics_transpose)
F_bias <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(bias_metrics_transpose[, i], ci = 0.95)
  F_bias[i, ] <- temp
}
F_bias_data <- as.data.frame(F_bias) %>%
  dplyr::rename(upper = V1, F = V2, lower = V3)

ggplot() +
  geom_line(data = F_base_data, aes(x = year, y = F), color = gmri_cols("green"), linewidth = 1.5) +
  geom_ribbon(data = F_base_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = F_bias_data, aes(x = year, y = F), color = gmri_cols("gmri blue"), linetype = 2, linewidth = 1.5) +
  geom_ribbon(data = F_bias_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), linetype = 2, alpha = 0.5)


#### Catch plot ####

base_metrics <- sapply(base$reps, function(x) {
    return(x$pred_log_catch)
  }) %>%
  as.data.frame()
base_metrics <- exp(base_metrics)
base_metrics_transpose <- t(base_metrics) %>%
  as.data.frame()

cols <- length(base_metrics_transpose)
F_base <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(base_metrics_transpose[, i], ci = 0.95)
  F_base[i, ] <- temp
}
F_base_data <- as.data.frame(F_base) %>%
  dplyr::rename(upper = V1, Catch = V2, lower = V3)
year <- 1980:2021

# bias
bias_metrics <- sapply(bias$reps, function(x) {
    return(x$pred_log_catch)
  }) %>%
  as.data.frame()
bias_metrics <- exp(bias_metrics)
bias_metrics_transpose <- t(bias_metrics) %>%
  as.data.frame()

cols <- length(bias_metrics_transpose)
F_bias <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(bias_metrics_transpose[, i], ci = 0.95)
  F_bias[i, ] <- temp
}
F_bias_data <- as.data.frame(F_bias) %>%
  dplyr::rename(upper = V1, Catch = V2, lower = V3)

ggplot() +
  geom_line(data = F_base_data, aes(x = year, y = Catch), color = gmri_cols("green")) +
  geom_ribbon(data = F_base_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = F_bias_data, aes(x = year, y = Catch), color = gmri_cols("gmri blue")) +
  geom_ribbon(data = F_bias_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), alpha = 0.5)

#### Recruitment ####
base_metrics <- sapply(base$reps, function(x) {
    return(x$NAA[, 1])
  }) %>%
  as.data.frame()
base_metrics_transpose <- t(base_metrics) %>%
  as.data.frame()

cols <- length(base_metrics_transpose)
R_base <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(base_metrics_transpose[, i], ci = 0.95)
  R_base[i, ] <- temp
}
R_base_data <- as.data.frame(R_base) %>%
  dplyr::rename(upper = V1, Recruitment = V2, lower = V3)
year <- 1980:2021

# bias
bias_metrics <- sapply(bias$reps, function(x) {
    return(x$NAA[, 1])
  }) %>%
  as.data.frame()
bias_metrics_transpose <- t(bias_metrics) %>%
  as.data.frame()

cols <- length(bias_metrics_transpose)
R_bias <- matrix(NA, 42, 3)

for (i in 1:cols) {
  temp <- CI(bias_metrics_transpose[, i], ci = 0.95)
  R_bias[i, ] <- temp
}
R_bias_data <- as.data.frame(R_bias) %>%
  dplyr::rename(upper = V1, Recruitment = V2, lower = V3)

ggplot() +
  geom_line(data = R_base_data, aes(x = year, y = Recruitment), color = gmri_cols("green")) +
  geom_ribbon(data = R_base_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = R_bias_data, aes(x = year, y = Recruitment), color = gmri_cols("gmri blue")) +
  geom_ribbon(data = R_bias_data, aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), alpha = 0.5)


#### Index ####
# base
n_sims <- 10
n_years <- 42
n_indices <- 4
indices <- matrix(NA, n_sims, n_years)
index <- matrix(NA, n_years, 3)
index_mean <- list()

for (y in 1:n_indices) {
  for (x in 1:n_sims) {
    indices[x, ] <- base$inputs[[x]]$data$agg_indices[, y]
  }
  for (j in 1:n_years) {
    index[j, ] <- CI(indices[, j], ci = 0.95)
  }
  # create list object to save
  index_mean[[y]] <- index
}
for (x in 1:n_indices) {
  index_mean[[x]] <- as.data.frame(index_mean[[x]]) %>% dplyr::rename(upper = V1, Index = V2, lower = V3)
  index_mean[[x]]$year <- 1980:2021
}

# index used in certain years only
for (x in 1:3) {
  index_mean[[1]][[x]][index_mean[[1]]$year > 2008] <- NA
  index_mean[[2]][[x]][index_mean[[2]]$year < 2009] <- NA
  index_mean[[3]][[x]][index_mean[[3]]$year > 2008] <- NA
  index_mean[[4]][[x]][index_mean[[4]]$year < 2009] <- NA
}

# bias
n_sims <- 10
n_years <- 42
n_indices <- 4
bias_indices <- matrix(NA, n_sims, n_years)
bias_index <- matrix(NA, n_years, 3)
bias_index_mean <- list()

for (y in 1:n_indices) {
  for (x in 1:n_sims) {
    bias_indices[x, ] <- bias$inputs[[x]]$data$agg_indices[, y]
  }
  for (j in 1:n_years) {
    bias_index[j, ] <- CI(bias_indices[, j], ci = 0.95)
  }
  # create list object to save
  bias_index_mean[[y]] <- bias_index
}
for (x in 1:n_indices) {
  bias_index_mean[[x]] <- as.data.frame(bias_index_mean[[x]]) %>% dplyr::rename(upper = V1, Index = V2, lower = V3)
  bias_index_mean[[x]]$year <- 1980:2021
}

# index used in certain years only
for (x in 1:3) {
  bias_index_mean[[1]][[x]][bias_index_mean[[1]]$year > 2008] <- NA
  bias_index_mean[[2]][[x]][bias_index_mean[[2]]$year < 2009] <- NA
  bias_index_mean[[3]][[x]][bias_index_mean[[3]]$year > 2008] <- NA
  bias_index_mean[[4]][[x]][bias_index_mean[[4]]$year < 2009] <- NA
}

ggplot() +
  geom_line(data = index_mean[[1]], aes(x = year, y = Index), color = gmri_cols("green"), linewidth = 1) +
  geom_ribbon(data = index_mean[[1]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = index_mean[[2]], aes(x = year, y = Index), color = gmri_cols("orange"), linewidth = 1) +
  geom_ribbon(data = index_mean[[2]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("orange"), alpha = 0.5) +
  geom_line(data = index_mean[[3]], aes(x = year, y = Index), color = gmri_cols("gmri blue"), linewidth = 1) +
  geom_ribbon(data = index_mean[[3]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), alpha = 0.5) +
  geom_line(data = index_mean[[4]], aes(x = year, y = Index), color = gmri_cols("gmri green"), linewidth = 1) +
  geom_ribbon(data = index_mean[[4]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri green"), alpha = 0.5) +
  geom_line(data = bias_index_mean[[1]], aes(x = year, y = Index), color = gmri_cols("green"), linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = bias_index_mean[[1]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("green"), alpha = 0.5) +
  geom_line(data = bias_index_mean[[2]], aes(x = year, y = Index), color = gmri_cols("orange"), linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = bias_index_mean[[2]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("orange"), alpha = 0.5) +
  geom_line(data = bias_index_mean[[3]], aes(x = year, y = Index), color = gmri_cols("gmri blue"), linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = bias_index_mean[[3]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri blue"), alpha = 0.5) +
  geom_line(data = bias_index_mean[[4]], aes(x = year, y = Index), color = gmri_cols("gmri green"), linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = bias_index_mean[[4]], aes(x = year, ymin = lower, ymax = upper), fill = gmri_cols("gmri green"), alpha = 0.5) +
  ylab("Indices")


#### stock status ####
# F/F40 Base

FAA <- list()
n_sims <- 10

for (x in 1:n_sims) {
  FAA[[x]] <- base$reps[[x]]$FAA_tot
}

FAA_mean <- apply(simplify2array(FAA), 1:2, mean)
age.full.f <- apply(FAA_mean,1, function(x) max(which(x == max(x))))
f_full <- FAA_mean[,11]

F40 <- list()
for (x in 1:n_sims) {
  F40[[x]] <- base$reps[[x]]$log_FXSPR_static%>%
    exp()
}
F40_mean <- apply(simplify2array(F40), 1, mean)

Fratio_base <- f_full/F40_mean


# F/F40 Bias
FAA <- list()
n_sims <- 10

for (x in 1:n_sims) {
  FAA[[x]] <- bias$reps[[x]]$FAA_tot
}

FAA_mean <- apply(simplify2array(FAA), 1:2, mean)
age.full.f <- apply(FAA_mean,1, function(x) max(which(x == max(x))))
f_full <- FAA_mean[,11]

F40 <- list()
for (x in 1:n_sims) {
  F40[[x]] <- bias$reps[[x]]$log_FXSPR%>%
    exp()
}
F40_mean <- apply(simplify2array(F40), 1, mean)

Fratio_bias <- f_full/F40_mean

Year <- 1980:2021

ggplot()+geom_line(aes(x=Year, y=Fratio_base), color=gmri_cols("gmri green"), linewidth=1)+
  geom_line(aes(x=Year, y=Fratio_bias), color= gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
  labs(y= "F / F40")

#SSB/SSB40 base
SSB <- list()
SSB40 <- list()
n_sims <- 10

for (x in 1:n_sims) {
  SSB[[x]] <- base$reps[[x]]$SSB  
  
  SSB40[[x]] <- base$reps[[x]]$log_SSB_FXSPR%>%
    exp()
}

SSB_mean <- apply(simplify2array(SSB), 1, mean)
SSB40_mean <- apply(simplify2array(SSB40), 1, mean)

SSBratio_base <- SSB_mean/SSB40_mean

#SSB/SSB40 bias
SSB <- list()
SSB40 <- list()
n_sims <- 10

for (x in 1:n_sims) {
  SSB[[x]] <- bias$reps[[x]]$SSB  
  
  SSB40[[x]] <- bias$reps[[x]]$log_SSB_FXSPR%>%
    exp()
}

SSB_mean <- apply(simplify2array(SSB), 1, mean)
SSB40_mean <- apply(simplify2array(SSB40), 1, mean)

SSBratio_bias <- SSB_mean/SSB40_mean

ggplot()+geom_line(aes(x=Year, y=SSBratio_base), color=gmri_cols("gmri green"), linewidth=1)+
  geom_line(aes(x=Year, y=SSBratio_bias), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")
  labs(y= "SSB / SSB40")
  
  
#### relative error #### #these should be averaged over each sim!!
  #SSB base
  true <- base$inputs[[1]]$data$SSB
  est <- base$reps[[1]]$SSB
  year <- 1980:2021
  SSB_rel_error_base <- (est-true)/true
  
  # SSB bias
  true <- bias$inputs[[1]]$data$SSB
  est <- bias$reps[[1]]$SSB
  year <- 1980:2021
  SSB_rel_error_bias <- (est-true)/true
  
  
  ggplot()+ geom_line(aes(x=Year, y=SSB_rel_error_base), color=gmri_cols("gmri green"), linewidth=1)+
    geom_line(aes(x=Year, y=SSB_rel_error_bias), color=gmri_cols("gmri blue"), linewidth=1)+
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    labs(y="Relative Error in SSB")
  
 # F base 
  true <- base$inputs[[1]]$data$F
  est <- base$reps[[1]]$F
  year <- 1980:2021
  F_rel_error_base <- (est-true)/true
  
  # F bias 
  true <- bias$inputs[[1]]$data$F
  est <- bias$reps[[1]]$F
  year <- 1980:2021
  F_rel_error_bias <- (est-true)/true
  
  ggplot()+ geom_line(aes(x=Year, y=F_rel_error_base), color=gmri_cols("gmri green"), linewidth=1)+
    geom_line(aes(x=Year, y=F_rel_error_bias), color=gmri_cols("gmri blue"), linewidth=1)+
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    labs(y="Relative Error in F")
  
  
