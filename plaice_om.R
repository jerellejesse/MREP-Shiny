## WHAM vignette simulation and MSE
library(here)
library(tidyverse)
library(wham)

###################
#### Base Case ####
###################

# pull saved data input from Run 4 WHAM plaice assessment
input <- readRDS(here::here("inputs/WHAM_MT_Run4_input.rds"))

setwd(here::here("WHAM_runs/Base"))

# fit model
mod <- fit_wham(input, do.osa = F, do.retro = F, MakeADFun.silent = T)
check_convergence(mod)
mod_proj <- project_wham(model = mod)
plot_wham_output(mod_proj, out.type = "pdf")

# function to simulate data from OM
sim_fn <- function(om, self.fit = FALSE) {
  input <- om$input
  input$data <- om$simulate(complete = TRUE)
  if (self.fit) {
    fit <- fit_wham(input, do.osa = F, do.retro = F, MakeADFun.silent = T)
    return(fit)
  } else {
    return(input)
  }
}

# simulate data
set.seed(123)
sim_inputs <- replicate(10, sim_fn(mod), simplify = F)
res <- list(reps = list(), par.est = list(), par.se = list(), adrep.est = list(), adrep.se = list(), inputs = list())
for (i in 1:length(sim_inputs)) {
  cat(paste0("i: ", i, "\n"))
  tfit <- fit_wham(sim_inputs[[i]], do.osa = F, do.retro = F, MakeADFun.silent = T)
  res$reps[[i]] <- tfit$rep
  res$par.est[[i]] <- as.list(tfit$sdrep, "Estimate")
  res$par.se[[i]] <- as.list(tfit$sdrep, "Std. Error")
  res$adrep.est[[i]] <- as.list(tfit$sdrep, "Estimate", report = TRUE)
  res$adrep.se[[i]] <- as.list(tfit$sdrep, "Std. Error", report = TRUE)
  res$inputs[[i]] <- as.list(tfit$input, "Inputs")
}

# save results
saveRDS(res, file = "Base.rds")

# #################################
# #### Edit Catch Data ####
# ##################################
# run.name <- "WHAM_MT_Run4"
# 
# asap3 <- read_asap3_dat(here("inputs/2022_AMP_MT_ASAP_WHAM_MODEL_INPUT_REVISED_WAA_FINAL.DAT"))
# # change catch
# catch_em <- asap3$dat$CAA_mats[[1]]*0.5 #divide the whole matrix in half, not paa, but actual catchaa
# asap3$dat$CAA_mats[[1]]<- catch_em
# 
# nage <- asap3$dat$n_ages
# 
# indices <- c('Spring.Alb','Spring.Big','Fall.Alb','Fall.Big')
# nindices <- length(indices)
# 
# ##### Set up preliminary input with no selectivity parameters fixed
# NAA_re = list(sigma = "rec+1") # Full state-space model
# NAA_re$cor = "iid" # iid random effects
# NAA_re$recruit_model = 2 # recruitment is random about the mean
# NAA_re$recruit_pars = exp(10) #initial guess for mean recruitment
# 
# # Setup initial selectivity model and parameters
# modelsetup <- c(rep("logistic", asap3$dat$n_fleet_sel_blocks), # Fishery selectivity
#                 rep("age-specific", nindices)) # index selectivity
# 
# # Setup fixed parameters
# # KLC question: Element objects list the ages to be fixed??
# fix_fleet_sel <- lapply(1:asap3$dat$n_fleets, function(x) NA)   # Set up fishery object (All NAs)
# fix_index_sel <- lapply(1:nindices, function(x) NA) # Set up index object (all NAs)
# 
# # The following code could be used to fix selectivity for a particular age
# # fix_index_sel[[1]] <- c(4) # Fix age 4  for for index 1 (NEFSC spring Albatross)
# 
# # Initial estimates for selectivity
# init_fleet_sel <- list(c(2,0.4)) # logistic parameters, based on model type
# init_index_sel <- lapply(1:nindices, function(x) rep(0.5, nage))
# # If any selectivity parameters are fixed, run the following (replace initial values for 1 for the ages where selectivity fixed)
# # for(i in 1:4) init_index_sel[[i]][fix_index_sel[[i]]] <- 1
# 
# # Setup random effect by selectivity block (here: fleet, index1, index2, index3, index4)
# randeffect <- c(rep("iid", asap3$dat$n_fleet_sel_blocks),
#                 rep("none", nindices)) # Don't include selectivity random effects for any surveys
# 
# # Setup selectivity list
# sel_list <- list(model = modelsetup, # list selectivity model for each fleet and index
#                  re = randeffect,
#                  initial_pars = c(init_fleet_sel, init_index_sel),
#                  fix_pars = c(fix_fleet_sel, fix_index_sel))
# 
# #fix q at some safely large value
# catchability <- list(
#   initial_q = rep(2,nindices))
# 
# input <- prepare_wham_input(asap3,
#                             NAA_re = NAA_re,
#                             selectivity = sel_list,
#                             model_name = "Init_WHAM_Run",
#                             age_comp = "logistic-normal-miss0" # logistic normal age comp, 0s treated as missing
# )
# 
# # Set specification of FXSPR_static to be based on the last five years
# #### Ask Tim about years pulled -- there are 42 years in the model but Tim said to pull years 37:41 for FXSPR_static
# input$data$avg_years_ind = (input$data$n_years_model-5):(input$data$n_years_model-1)
# 
# #fix the q values
# input$map$logit_q = factor(rep(NA,nindices)) #don't estimate q
# 
# ##### Run model with freely estimated selectivity
# Init_WHAM_Run <- fit_wham(input, MakeADFun.silent = TRUE, do.osa = FALSE, do.retro=FALSE)
# check_convergence(Init_WHAM_Run)
# print(paste("Number of parameters", length(Init_WHAM_Run$par), sep=" "))
# 
# # ID ages that should be fixed at full selectivity
# index.sel.list <- Init_WHAM_Run$rep$selAA[2:(nindices+1)]
# names(index.sel.list) <- indices
# # For each index, divide by max selectivity
# scaled.index.sel <- data.frame(t(sapply(index.sel.list, function(x) x[1,]/max(x[1,]))))
# # Age of maximum selectivity for each index
# maxage.index.sel <- apply(scaled.index.sel, 1, function(x){ (1:nage)[which(x==1)] })
# # Matrix with time-invariant selectivity-at-age for each index
# index.sel.mat <- bind_cols(
#   Alb.spr  = index.sel.list[[1]][1,], # Albatross spring, max sel = age 4 (used to be age-6)
#   Big.spr  = index.sel.list[[2]][1,], # Bigelow spring, max sel = age 5
#   Alb.fall = index.sel.list[[3]][1,], # Albatross fall, max sel = age 11 (used to be age-4)
#   Big.fall = index.sel.list[[4]][1,]  # Bigelow fall, max sel = age 5 (used to be age-3)
# )
# 
# 
# input <- prepare_wham_input(asap3,
#                             NAA_re = NAA_re,
#                             selectivity = sel_list,
#                             model_name = run.name,
#                             age_comp = "logistic-normal-miss0" # logistic normal age comp, 0s treated as missing
# )
# 
# # Set specification of FXSPR_static to be based on the last five years
# input$data$avg_years_ind = (input$data$n_years_model-5):(input$data$n_years_model-1)
# 
# ### Save input
# # Save model data input
# saveRDS(input, file=file.path("inputs", paste(run.name, "input_high_catch.rds", sep='_')) )#

#############################
#### Bias Catch Scenario ####
#############################
# read in biased input
#input_bias <- readRDS(here("inputs/WHAM_MT_Run4_input_bias.rds"))
input_bias <- readRDS(here("inputs/WHAM_MT_Run4_input_high_catch.rds"))
setwd(here("WHAM_runs/BiasCatch"))

# fit model
mod_bias <- fit_wham(input_bias, do.osa = F, do.retro = F, MakeADFun.silent = T)
check_convergence(mod_bias)
mod_proj <- project_wham(model = mod_bias)
plot_wham_output(mod_proj, out.type = "pdf")

# simulate data
set.seed(123)
sim_inputs <- replicate(10, sim_fn(mod_bias), simplify = F)
res <- list(reps = list(), par.est = list(), par.se = list(), adrep.est = list(), adrep.se = list(), inputs = list())
for (i in 1:length(sim_inputs)) {
  cat(paste0("i: ", i, "\n"))
  tfit <- fit_wham(sim_inputs[[i]], do.osa = F, do.retro = F, MakeADFun.silent = T)
  res$reps[[i]] <- tfit$rep
  res$par.est[[i]] <- as.list(tfit$sdrep, "Estimate")
  res$par.se[[i]] <- as.list(tfit$sdrep, "Std. Error")
  res$adrep.est[[i]] <- as.list(tfit$sdrep, "Estimate", report = TRUE)
  res$adrep.se[[i]] <- as.list(tfit$sdrep, "Std. Error", report = TRUE)
  res$inputs[[i]] <- as.list(tfit$input, "Inputs")
}

# save results
saveRDS(res, file = "HighCatch.rds")


###########################
#### Modify index data ####
###########################
run.name <- "WHAM_MT_Run4"
asap3 <- read_asap3_dat(here("inputs/2022_AMP_MT_ASAP_WHAM_MODEL_INPUT_REVISED_WAA_FINAL.DAT"))

# change index
mats <- asap3$dat$IAA_mats
for (i in 1:length(mats)) {
  index <- asap3$dat$IAA_mats[[i]] * 0.5 # divide the whole matrix in half
  index[index == -499.5] <- -999
  index[, 1] <- 1980:2021
  asap3$dat$IAA_mats[[i]] <- index
}


nage <- asap3$dat$n_ages

indices <- c("Spring.Alb", "Spring.Big", "Fall.Alb", "Fall.Big")
nindices <- length(indices)

##### Set up preliminary input with no selectivity parameters fixed
NAA_re <- list(sigma = "rec+1") # Full state-space model
NAA_re$cor <- "iid" # iid random effects
NAA_re$recruit_model <- 2 # recruitment is random about the mean
NAA_re$recruit_pars <- exp(10) # initial guess for mean recruitment

# Setup initial selectivity model and parameters
modelsetup <- c(
  rep("logistic", asap3$dat$n_fleet_sel_blocks), # Fishery selectivity
  rep("age-specific", nindices)
) # index selectivity

# Setup fixed parameters
# KLC question: Element objects list the ages to be fixed??
fix_fleet_sel <- lapply(1:asap3$dat$n_fleets, function(x) NA) # Set up fishery object (All NAs)
fix_index_sel <- lapply(1:nindices, function(x) NA) # Set up index object (all NAs)

# The following code could be used to fix selectivity for a particular age
# fix_index_sel[[1]] <- c(4) # Fix age 4  for for index 1 (NEFSC spring Albatross)

# Initial estimates for selectivity
init_fleet_sel <- list(c(2, 0.4)) # logistic parameters, based on model type
init_index_sel <- lapply(1:nindices, function(x) rep(0.5, nage))
# If any selectivity parameters are fixed, run the following (replace initial values for 1 for the ages where selectivity fixed)
# for(i in 1:4) init_index_sel[[i]][fix_index_sel[[i]]] <- 1

# Setup random effect by selectivity block (here: fleet, index1, index2, index3, index4)
randeffect <- c(
  rep("iid", asap3$dat$n_fleet_sel_blocks),
  rep("none", nindices)
) # Don't include selectivity random effects for any surveys

# Setup selectivity list
sel_list <- list(
  model = modelsetup, # list selectivity model for each fleet and index
  re = randeffect,
  initial_pars = c(init_fleet_sel, init_index_sel),
  fix_pars = c(fix_fleet_sel, fix_index_sel)
)

# fix q at some safely large value
catchability <- list(
  initial_q = rep(2, nindices)
)

input <- prepare_wham_input(asap3,
  NAA_re = NAA_re,
  selectivity = sel_list,
  model_name = "Init_WHAM_Run",
  age_comp = "logistic-normal-miss0" # logistic normal age comp, 0s treated as missing
)

# Set specification of FXSPR_static to be based on the last five years
#### Ask Tim about years pulled -- there are 42 years in the model but Tim said to pull years 37:41 for FXSPR_static
input$data$avg_years_ind <- (input$data$n_years_model - 5):(input$data$n_years_model - 1)

# fix the q values
input$map$logit_q <- factor(rep(NA, nindices)) # don't estimate q

##### Run model with freely estimated selectivity
Init_WHAM_Run <- fit_wham(input, MakeADFun.silent = TRUE, do.osa = FALSE, do.retro = FALSE)
check_convergence(Init_WHAM_Run)
print(paste("Number of parameters", length(Init_WHAM_Run$par), sep = " "))

# ID ages that should be fixed at full selectivity
index.sel.list <- Init_WHAM_Run$rep$selAA[2:(nindices + 1)]
names(index.sel.list) <- indices
# For each index, divide by max selectivity
scaled.index.sel <- data.frame(t(sapply(index.sel.list, function(x) x[1, ] / max(x[1, ]))))
# Age of maximum selectivity for each index
maxage.index.sel <- apply(scaled.index.sel, 1, function(x) {
  (1:nage)[which(x == 1)]
})
# Matrix with time-invariant selectivity-at-age for each index
index.sel.mat <- bind_cols(
  Alb.spr  = index.sel.list[[1]][1, ], # Albatross spring, max sel = age 4 (used to be age-6)
  Big.spr  = index.sel.list[[2]][1, ], # Bigelow spring, max sel = age 5
  Alb.fall = index.sel.list[[3]][1, ], # Albatross fall, max sel = age 11 (used to be age-4)
  Big.fall = index.sel.list[[4]][1, ] # Bigelow fall, max sel = age 5 (used to be age-3)
)


input <- prepare_wham_input(asap3,
  NAA_re = NAA_re,
  selectivity = sel_list,
  model_name = run.name,
  age_comp = "logistic-normal-miss0" # logistic normal age comp, 0s treated as missing
)

# Set specification of FXSPR_static to be based on the last five years
input$data$avg_years_ind <- (input$data$n_years_model - 5):(input$data$n_years_model - 1)

### Save input
# Save model data input
setwd(here::here())
saveRDS(input, file = file.path("inputs", paste(run.name, "input_index.rds", sep = "_"))) #


#############################
#### Bias Index Scenario ####
#############################
# read in biased input
input_bias <- readRDS(here::here("inputs/WHAM_MT_Run4_input_index.rds"))
setwd(here::here("WHAM_runs/BiasIndex"))

# fit model
mod_bias <- fit_wham(input_bias, do.osa = F, do.retro = F, MakeADFun.silent = T)
check_convergence(mod_bias)
mod_proj <- project_wham(model = mod_bias)
plot_wham_output(mod_proj, out.type = "pdf")

# simulate data
set.seed(123)
sim_inputs <- replicate(10, sim_fn(mod_bias), simplify = F)
res <- list(reps = list(), par.est = list(), par.se = list(), adrep.est = list(), adrep.se = list(), inputs = list())
for (i in 1:length(sim_inputs)) {
  cat(paste0("i: ", i, "\n"))
  tfit <- fit_wham(sim_inputs[[i]], do.osa = F, do.retro = F, MakeADFun.silent = T)
  res$reps[[i]] <- tfit$rep
  res$par.est[[i]] <- as.list(tfit$sdrep, "Estimate")
  res$par.se[[i]] <- as.list(tfit$sdrep, "Std. Error")
  res$adrep.est[[i]] <- as.list(tfit$sdrep, "Estimate", report = TRUE)
  res$adrep.se[[i]] <- as.list(tfit$sdrep, "Std. Error", report = TRUE)
  res$inputs[[i]] <- as.list(tfit$input, "Inputs")
}

# save results
saveRDS(res, file = "BiasIndex.rds")
