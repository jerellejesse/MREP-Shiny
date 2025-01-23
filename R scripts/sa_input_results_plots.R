# stock assessment input and results plots

library(tidyverse)
library(here)
library(Rmisc)
library(gmRi)

data <- readRDS(here::here("WHAM_runs/Base/Base.rds"))
#data <- readRDS(here::here("WHAM_runs/BiasCatch/BiasCatch.rds"))
#data <- readRDS(here::here("WHAM_runs/BiasIndex/BiasIndex.rds"))
#### Inputs ####
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

ggplot(index_mean)+ 
  geom_line(aes(x=year, y=V1), color=gmri_cols("green"), linewidth=1)+
  geom_line(aes(x=year, y=V2), color=gmri_cols("gmri blue"), linewidth=1)+
  geom_line(aes(x=year, y=V3), color=gmri_cols("orange"), linewidth=1)+
  geom_line(aes(x=year, y=V4), color=gmri_cols("gmri green"), linewidth=1)+
  ylab("Indices")

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
  dplyr::rename(upper=V1, catch=V2, lower=V3)
year <- 1980:2021

ggplot(catch_metric_data)+ 
  geom_col(aes(x=year, y= catch), width= 0.8, color=gmri_cols("green"),fill=gmri_cols("green"))#+
  geom_ribbon(aes(x=year, ymin= lower, ymax=upper),fill=gmri_cols("green") , alpha=0.5)

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

ggplot(MAA_mean)+geom_line(aes(x=Age, y=M), linewidth=1.5,color=gmri_cols("green"))

#### weight at age
  
WAA <- data$inputs[[1]]$data$waa[1,,]%>% # first waa is for ssb?
as.data.frame()
WAA$Year <- 1980:2021    

WAA_tidy <- gather(WAA,"Age", "Weight",1:11 )%>%
  separate(Age, c("junk", "Age"), sep="(?<=[A-Za-z])(?=[0-9])")%>%
  select(!junk)%>%
  mutate(Age=fct_relevel(Age,"1","2","3","4","5","6","7","8","9","10","11"))


ggplot(WAA_tidy)+geom_line(aes(x=Year, y=Weight, color=Age), linewidth=1.5)+
  labs(y= "Weight-at-age for SSB")+
  scale_color_gmri(palette = "main", guide="none")


#### Maturity 
Mat <- data$inputs[[1]]$data$mature[1,]%>%
  as.data.frame()
Mat$Age <- 1:11   

ggplot(Mat)+geom_line(aes(x=Age, y=.),color=gmri_cols("green"), linewidth=1.5)+
  labs(y= "Maturity")


#### Results ####
#### spawning stock biomass
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
year <- 1980:2021

ggplot(SSB_data)+ geom_line(aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)#+
  geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)

#### fishing mortality
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
F_data<-as.data.frame(F) %>% dplyr::rename(upper=V1, F=V2, lower=V3)
year <- 1980:2021

ggplot(F_data)+ geom_line(aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
  geom_ribbon( aes(x=year, ymin= lower, ymax=upper),fill=gmri_cols("green") , alpha=0.5)
  
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
R_data<-as.data.frame(R)%>% dplyr::rename(upper=V1, R=V2, lower=V3)
year <- 1980:2021

ggplot(R_data)+ geom_line(aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
  geom_ribbon(aes(x=year, ymin= lower, ymax=upper),fill=gmri_cols("green") , alpha=0.5)
  
#### relative error SSB
true <- data$inputs[[1]]$data$SSB
est <- data$reps[[1]]$SSB
year <- 1980:2021
SSB_rel_error <- (est-true)/true

ggplot()+ geom_line(aes(x=year, y=true), color=gmri_cols("green"), linewidth=.9)+
  geom_line(aes(x=year, y=est), linetype="dashed", color=gmri_cols("gmri blue"), linewidth=.9)+
  labs(y= "SSB")

ggplot()+ geom_line(aes(x=year, y=SSB_rel_error), color=gmri_cols("green"))+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  labs(y="Relative Error in SSB")

#### relative error F
true <- data$inputs[[1]]$data$F
est <- data$reps[[1]]$F
year <- 1980:2021
F_rel_error <- (est-true)/true

ggplot()+ geom_line(aes(x=year, y=true), color=gmri_cols("green"), linewidth=.9)+
  geom_line(aes(x=year, y=est), linetype="dashed", color=gmri_cols("gmri blue"), linewidth=.9)+
  labs(y= "F")

ggplot()+ geom_line(aes(x=year, y=F_rel_error), color=gmri_cols("green"))+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  labs(y="Relative Error in F")


#### retrospective error (not currently on)

#### stock status
# F/F40

FAA <- list()
n_sims <- 10

for (x in 1:n_sims) {
FAA[[x]] <- data$reps[[x]]$FAA_tot
}

FAA_mean <- apply(simplify2array(FAA), 1:2, mean)
age.full.f <- apply(FAA_mean,1, function(x) max(which(x == max(x))))
f_full <- FAA_mean[,11]

F40 <- list()
for (x in 1:n_sims) {
  F40[[x]] <- data$reps[[x]]$log_FXSPR%>%
    exp()
}
F40_mean <- apply(simplify2array(F40), 1, mean)

Fratio <- f_full/F40_mean
Year <- 1980:2021

ggplot()+geom_line(aes(x=Year, y=Fratio), color=gmri_cols("green"), linewidth=1)+
  labs(y= "F / F40")


#SSB/SSB40
SSB <- list()
SSB40 <- list()
n_sims <- 10

for (x in 1:n_sims) {
 SSB[[x]] <- data$reps[[x]]$SSB  
 
 SSB40[[x]] <- data$reps[[x]]$log_SSB_FXSPR%>%
    exp()
}

SSB_mean <- apply(simplify2array(SSB), 1, mean)
SSB40_mean <- apply(simplify2array(SSB40), 1, mean)

SSBratio <- SSB_mean/SSB40_mean

ggplot()+geom_line(aes(x=Year, y=SSBratio), color=gmri_cols("green"), linewidth=1)+
  labs(y= "SSB / SSB40")
