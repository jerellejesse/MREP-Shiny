# Gather data for input into shiny app

# stock assessment input and results plots

library(tidyverse)
library(here)
library(Rmisc)
library(gmRi)

data <- readRDS(here::here("WHAM_runs/Base/Base.rds"))
data <- readRDS(here::here("WHAM_runs/BiasCatch/BiasCatch.rds"))
data <- readRDS(here::here("WHAM_runs/BiasIndex/BiasIndex.rds"))
data <- readRDS(here::here("WHAM_runs/BiasCatch/HighCatch.rds"))

#### Index
n_sims <- 10
n_years <- 42
n_indices <-4
indices <- matrix(NA,  n_sims,n_years)
index <- matrix(NA, n_years, n_indices)

for (y in 1:n_indices) {
  for (x in 1:n_sims) {
    indices[x,] <- data$inputs[[x]]$data$agg_indices[,y]
  }
  index[,y] <- colMeans(indices)
}
index_mean<-as.data.frame(index)
index_mean$year <- 1980:2021
#index used in certain years only
index_mean$V1[index_mean$year>2008]<-NA
index_mean$V2[index_mean$year<2009]<-NA
index_mean$V3[index_mean$year>2008]<-NA
index_mean$V4[index_mean$year<2009]<-NA

#### Catch
catch <- sapply(data$reps, function(x) return(x$pred_log_catch))%>%
  as.data.frame()
catch <- exp(catch)
catch_transpose <- t(catch)%>%
  as.data.frame()

cols <- length(catch_transpose)
catch_metric <- matrix(NA, 42,3)

for (i in 1:cols) {
  temp <- CI(catch_transpose[,i], ci=0.95)
  catch_metric [i,] <- temp
}
catch_metric_data<-as.data.frame(catch_metric)%>%
  dplyr::rename(catch_upper=V1, catch=V2, catch_lower=V3)
catch_metric_data$year <- 1980:2021

#### SSB
pull_ssb <- sapply(data$reps, function(x) return(x$SSB))%>%
  as.data.frame()
pull_ssb_transpose <- t(pull_ssb)%>%
  as.data.frame()

cols <- length(pull_ssb_transpose)
SSB <- matrix(NA, 42,3)

for (i in 1:cols) {
  temp <- CI(pull_ssb_transpose[,i], ci=0.95)
  SSB [i,] <- temp
}
SSB_data<-as.data.frame(SSB)%>%
  dplyr::rename(SSB_upper=V1, SSB=V2, SSB_lower=V3)
SSB_data$year <- 1980:2021

#### F
pull_f <- sapply(data$reps, function(x) return(x$F))%>%
  as.data.frame()
pull_f_transpose <- t(pull_f)%>%
  as.data.frame()

cols <- length(pull_f_transpose)
F <- matrix(NA, 42,3)

for (i in 1:cols) {
  temp <- CI(pull_f_transpose[,i], ci=0.95)
  F [i,] <- temp
}
F_data<-as.data.frame(F) %>% dplyr::rename(F_upper=V1, F=V2, F_lower=V3)
F_data$year <- 1980:2021

#### recruits
pull_r <- sapply(data$reps, function(x) return(x$NAA[,1]))%>%
  as.data.frame()
pull_r_transpose <- t(pull_r)%>%
  as.data.frame()

cols <- length(pull_r_transpose)
R <- matrix(NA, 42,3)

for (i in 1:cols) {
  temp <- CI(pull_r_transpose[,i], ci=0.95)
  R [i,] <- temp
}
R_data<-as.data.frame(R)%>% dplyr::rename(R_upper=V1, R=V2, R_lower=V3)
R_data$year <- 1980:2021


#### Data by year ####
dfs <- list(index_mean,catch_metric_data, SSB_data, F_data, R_data)
input_year <- Reduce(function(x,y) full_join(x,y, by="year"), dfs)
#write.csv(input_year, here::here("MREP-Shiny/data/yearly_data.csv"))

# catch bias
dfs <- list(catch_metric_data, SSB_data, F_data, R_data)
input_catch <- Reduce(function(x,y) full_join(x,y, by="year"), dfs)
#write.csv(input_catch, here::here("MREP-Shiny/data/catch_bias_data.csv"))
#write.csv(input_catch, here::here("MREP-Shiny/data/high_catch.csv"))

# index bias
dfs <- list(index_mean,catch_metric_data, SSB_data, F_data, R_data)
input_index <- Reduce(function(x,y) full_join(x,y, by="year"), dfs)
#write.csv(input_index, here::here("MREP-Shiny/data/index_bias_data.csv"))

#### natural mortality
n_sims <- 10
n_ages <- 11
MAA <- matrix(NA,n_sims,n_ages)

for (x in 1:n_sims) {
  MAA[x,] <- data$reps[[x]]$MAA%>%
    colMeans()
}
MAA_mean <- colMeans(MAA)%>% as.data.frame()
colnames(MAA_mean)<- "M"
MAA_mean$Age <- 1:n_ages

#### weight at age
WAA <- data$inputs[[1]]$data$waa[1,,]%>% # first waa is for ssb?
  as.data.frame()
WAA$Year <- 1980:2021    

WAA_tidy <- gather(WAA,"Age", "Weight",1:11 )%>%
  separate(Age, c("junk", "Age"), sep="(?<=[A-Za-z])(?=[0-9])")%>%
  select(!junk)%>%
  mutate(Age=fct_relevel(Age,"1","2","3","4","5","6","7","8","9","10","11"))

#write.csv(WAA_tidy, here::here("MREP-Shiny/data/weight_data.csv"))

#### Maturity 
Mat <- data$inputs[[1]]$data$mature[1,]%>%
  as.data.frame()

Mat<- dplyr::rename(Mat, maturity =.)

Mat$Age <- 1:11   

#### selectivity
selectivity <- data$reps[[1]]$selAA[[1]][1,]%>%
  as.data.frame()
Sel<- dplyr::rename(selectivity, selectivity =.)
Sel$Age <- 1:11

#### Catchability
catchability <- data$reps[[1]]$q%>%
  as.data.frame()

catchability$Year <-1980:2021
tidy_cat <- catchability %>%
  pivot_longer(
    cols = starts_with("V"),    
    names_to = "survey",          
    values_to = "catchability"          
  )
tidy_cat<- tidy_cat <- tidy_cat %>%
  mutate(survey = str_replace(survey, "V", "Index"))
  # separate(tidy_cat, survey,c("junk", "Survey"), sep="(?<=[A-Za-z])(?=[0-9])")%>%
  # select(!junk)
#write.csv(tidy_cat, here::here("MREP-Shiny/data/catchability.csv"))

dfs <-list(Mat, MAA_mean, Sel)
input_age <- Reduce(function(x,y) full_join(x,y, by="Age"), dfs)
#write.csv(input_age, here::here("MREP-Shiny/data/input_age.csv"))

#### Reference points
F40 <- list()
for (x in 1:n_sims) {
  F40[[x]] <- data$reps[[x]]$log_FXSPR_static%>%
    exp()
}
F40_mean <- mean(unlist(F40))
  #apply(simplify2array(F40), 1, mean)


base <- F40_mean
index <- F40_mean
catch_low <- F40_mean
catch_high <- F40_mean
Fref <- data.frame(base, catch_low,catch_high, index)

#####################
SSB40 <- list()
n_sims <- 10
for (x in 1:n_sims) {
  SSB40[[x]] <- data$reps[[x]]$log_SSB_FXSPR_static%>%
    exp()
}

SSB40_mean <- mean(unlist(SSB40))
  #apply(simplify2array(SSB40), 1, mean)

base <-SSB40_mean
catch_low <- SSB40_mean
catch_high <- SSB40_mean
index <- SSB40_mean
SSBref <- data.frame(base, catch_low,catch_high, index)

refs <- rbind(Fref, SSBref)
rownames(refs)<-c("Fref", "SSBref")

#write.csv(refs, here::here("MREP-Shiny/data/Ref_data.csv"))
