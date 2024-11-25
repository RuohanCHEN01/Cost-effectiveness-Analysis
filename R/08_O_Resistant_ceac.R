## To change the resistance scenario 
rm(list=ls())
options(warn = - 1)
library(CEAutil)
library(dampack)


# set.p_O_Suscep_scenario <- C(1, 0.9, 0.8,......)
set.p_O_Suscep_scenario <- 0.5

#Input the model and basic data
setwd("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export")
treetxt <- parse_amua_tree("main.R")

#Print Parameter
print(treetxt$param_ls)
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]
#Output
tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 1)
# print(tree_output)

#ICER(BvsC,OvsC)
basic_result <- data.frame(tree_output)
##################################Negative##################################
basic_result$expectedQALYs <- -basic_result$expectedQALYs

basic_result$Inc_cost[1] <- basic_result$Cost[1]-basic_result$Cost[3]
basic_result$Inc_cost[2] <- basic_result$Cost[2]-basic_result$Cost[3]
basic_result$Inc_cost[3] <- NA
basic_result$Inc_eff[1] <- basic_result$expectedQALYs[1]-basic_result$expectedQALYs[3]
basic_result$Inc_eff[2] <- basic_result$expectedQALYs[2]-basic_result$expectedQALYs[3]
basic_result$Inc_eff[3] <- NA
basic_result$icer <-  basic_result$Inc_cost/basic_result$Inc_eff
basic_result

# #ICER(BvsO)
# Bal_vs_ose_result <- data.frame(tree_output[1:2,])
# Bal_vs_ose_result$Inc_cost[1] <- Bal_vs_ose_result$Cost[1]-Bal_vs_ose_result$Cost[2]
# Bal_vs_ose_result$Inc_cost[2] <- NA
# Bal_vs_ose_result$Inc_eff[1] <- Bal_vs_ose_result$expectedQALYs[1]-Bal_vs_ose_result$expectedQALYs[2]
# Bal_vs_ose_result$Inc_eff[2] <- NA
# Bal_vs_ose_result$icer <- Bal_vs_ose_result$Inc_cost/Bal_vs_ose_result$Inc_eff
# Bal_vs_ose_result


###List parameter range
params_list <- read.csv("C:/Users/chenr/Desktop/AMUA/param_all.csv")
params_list <- data.frame(params_list)
params_range <- params_list
# print(params_range)

outcomes = c("expectedQALYs", "Cost")
# print(param_ls)

# Resistance rete

param_ls$p_O_Suscep_scenario <- set.p_O_Suscep_scenario
owsa_out <- run_owsa_det(params_range = params_range, 
                         params_basecase = param_ls, 
                         nsamp = 2, 
                         FUN = dectree_wrapper, 
                         treefunc = treefunc, 
                         outcomes = c("expectedQALYs", "Cost"),
                         popsize = 1)
# print(str(owsa_out))
# print(owsa_out$owsa_Cost)
# print(owsa_out$owsa_expectedQALYs)

# Output OWSA result
owsa_output <- data.frame()
owsa_output <- data.frame(owsa_out$owsa_expectedQALYs)
names(owsa_output)[names(owsa_output) == "outcome_val"] <- "QALYs"
owsa_output$Cost <- owsa_out$owsa_Cost$outcome_val
##################################Negative##################################
owsa_output$QALYs <- -owsa_output$QALYs
# View(owsa_output)
# write.csv(owsa_output,"owsa_output.csv")


library(dplyr)
library(tidyr)
library(dampack)
# ktop <- 152667
ktop <- 600000
nsamp <- 2000

set.seed(200)
# ########################################OwH########################################
# params = c("p_True_influ","d_TRAE","d_B","d_O","d_R","d_ILI","d_Com","d_Hos","d_ICU","p_B_Hos","p_B_S","p_B_O","p_B_B","p_B_P","p_O_Suscep_Hos","p_O_Suscep_S","p_O_Suscep_O","p_O_Suscep_B","p_O_Suscep_P","p_O_Res_Hos","p_O_Res_S","p_O_Res_O","p_O_Res_B","p_O_Res_P","p_ICU","p_B_Hos_Death","p_O_Hos_Death","p_O_Res_Hos_Death","q_Hos","q_ICU","q_S","q_O","q_B","q_P","q_ILI","q_TRAE","c_Outpatient","c_Test","c_B","c_O","c_Baseline","c_Inpatient","c_ICU","q_influ")
# dists = c("beta","log-normal","normal","normal","normal","gamma","gamma","gamma","gamma","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","normal","normal","normal","normal","normal","normal","normal","beta")
# parameterization_types = c("a, b","mean, sd","mean, sd","mean, sd","mean, sd","shape, scale","shape, scale","shape, scale","shape, scale","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","a, b")
# dists_params = list(c(3.6,0.04),c(6.9,1.29),c(53.5,2.68),c(53.8,1.58),c(80.2,3.7),c(404.91,0.019),c(28.5616,0.3232),c(15.37,0.26),c(15.37,0.2),c(6.9,2290.07),c(0.93,114.88),c(0.45,88.7),c(3.52,143),c(0.45,88.7),c(24.64,4538.928),c(0.01,50),c(0.22,73.87),c(2.27,139.38),c(0.01,33),c(34.59,5368.84),c(0.63,69.02),c(0.01,30),c(3.15,86.96),c(3.14,75.34),c(187.66,5299.44),c(2.83,76.91),c(102.68,1376.87),c(174.31,1720.37),c(47.42,43.77),c(54.05,26.62),c(40.67,230.44),c(40.67,230.44),c(40.67,230.44),c(1.53,4.58),c(33.24,89.88),c(38.21,152.86),c(195,47.0925),c(58.5,3.3163),c(123.5,3.3163),c(91,13.2652),c(1.95,0.3315),c(663,67.6546),c(1995.5,47.099),c(34.91,148.84))
# ########################################OwH########################################

#########################################HR##########################################
# params = c("p_True_influ","d_TRAE","d_B","d_O","d_R","d_ILI","d_Com","d_Hos","d_ICU","p_B_Hos","p_B_S","p_B_O","p_B_B","p_B_P","p_O_Suscep_Hos","p_O_Suscep_S","p_O_Suscep_O","p_O_Suscep_B","p_O_Suscep_P","p_O_Res_Hos","p_O_Res_S","p_O_Res_O","p_O_Res_B","p_O_Res_P","p_ICU","p_B_Hos_Death","p_O_Hos_Death","p_O_Res_Hos_Death","q_ICU","q_Hos","q_S","q_O","q_B","q_P","q_ILI","q_TRAE","c_Outpatient","c_Test","c_B","c_O","c_Baseline","c_Inpatient","c_ICU","q_influ")
# dists = c("beta","log-normal","normal","normal","normal","gamma","gamma","gamma","gamma","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","normal","normal","normal","normal","normal","normal","normal","beta")
# parameterization_types = c("a, b","mean, sd","mean, sd","mean, sd","mean, sd","shape, scale","shape, scale","shape, scale","shape, scale","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","a, b")
# dists_params = list(c(3.6,0.04),c(6.9,1.29),c(73.2,4.57),c(81,5.64),c(102.3,5.2),c(404.91,0.0019),c(28.5616,0.3232),c(24.01,0.21),c(15.37,0.2),c(3.96,1095.64),c(0.26,87.12),c(0.01,53),c(2.52,137.36),c(0.01,53),c(18.38,1652.27),c(0.51,101.79),c(0.26,87.12),c(3.63,154.05),c(0.51,101.79),c(17.87,1033.5),c(3.26,152.13),c(0.93,114.88),c(10.19,159.67),c(0.93,114.88),c(166.97,1117.44),c(2.67,52.99),c(43.92,399.69),c(165.82,1053.41),c(47.42,43.77),c(54.05,26.62),c(40.67,230.44),c(40.67,230.44),c(40.67,230.44),c(1.53,4.58),c(33.24,89.88),c(38.21,152.86),c(195,47.0925),c(58.5,3.3163),c(123.5,3.3163),c(91,13.2652),c(1.95,0.3315),c(663,67.6546),c(1995.5,47.099),c(34.91,148.84))
#########################################HR##########################################

###############################General Population################################
params = c("p_True_influ","d_TRAE","d_B","d_O","d_R","d_ILI","d_Com","d_Hos","d_ICU","p_B_Hos","p_B_S","p_B_O","p_B_B","p_B_P","p_O_Suscep_Hos","p_O_Suscep_S","p_O_Suscep_O","p_O_Suscep_B","p_O_Suscep_P","p_O_Res_Hos","p_O_Res_S","p_O_Res_O","p_O_Res_B","p_O_Res_P","p_ICU","p_B_Hos_Death","p_O_Hos_Death","p_O_Res_Hos_Death","q_ICU","q_Hos","q_S","q_O","q_B","q_P","q_ILI","q_TRAE","c_Outpatient","c_Test","c_B","c_O","c_Baseline","c_Inpatient","c_ICU","q_influ","p_HR","d_HR_B","d_HR_O","d_HR_R","d_HR_Hos","p_HR_B_Hos","p_HR_B_S","p_HR_B_O","p_HR_B_B","p_HR_B_P","p_HR_O_Suscep_Hos","p_HR_O_Suscep_S","p_HR_O_Suscep_O","p_HR_O_Suscep_B","p_HR_O_Suscep_P","p_HR_O_Res_Hos","p_HR_O_Res_S","p_HR_O_Res_O","p_HR_O_Res_B","p_HR_O_Res_P","p_HR_ICU","p_HR_B_Hos_Death","p_HR_O_Hos_Death","p_HR_O_Res_Hos_Death")
dists = c("beta","log-normal","normal","normal","normal","gamma","gamma","gamma","gamma","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","normal","normal","normal","normal","normal","normal","normal","beta","normal","normal","normal","normal","gamma","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta","beta")
parameterization_types = c("a, b","mean, sd","mean, sd","mean, sd","mean, sd","shape, scale","shape, scale","shape, scale","shape, scale","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","mean, sd","a, b","mean, sd","mean, sd","mean, sd","mean, sd","shape, scale","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b","a, b")
dists_params = list(c(3.6,0.04),c(6.9,1.29),c(53.5,2.68),c(53.8,1.58),c(80.2,3.7),c(404.91,0.019),c(28.5616,0.3232),c(15.37,0.26),c(15.37,0.2),c(6.9,2290.07),c(0.93,114.88),c(0.45,88.7),c(3.52,143),c(0.45,88.7),c(24.64,4538.928),c(0.01,50),c(0.22,73.87),c(2.27,139.38),c(0.01,33),c(34.59,5368.84),c(0.63,69.02),c(0.01,30),c(3.15,86.96),c(3.14,75.34),c(187.66,5299.44),c(2.83,76.91),c(102.68,1376.87),c(174.31,1720.37),c(47.42,43.77),c(54.05,26.62),c(40.67,230.44),c(40.67,230.44),c(40.67,230.44),c(1.53,4.58),c(33.24,89.88),c(38.21,152.86),c(195,47.0925),c(58.5,3.3163),c(123.5,3.3163),c(91,13.2652),c(1.95,0.3315),c(663,67.6546),c(1995.5,47.099),c(34.91,148.84),c(0.2772,0.45),c(73.2,4.57),c(81,5.64),c(102.3,5.2),c(24.01,0.21),c(3.96,1095.64),c(0.26,87.12),c(0.01,53),c(2.52,137.36),c(0.01,53),c(18.38,1652.27),c(0.51,101.79),c(0.26,87.12),c(3.63,154.05),c(0.51,101.79),c(17.87,1033.5),c(3.26,152.13),c(0.93,114.88),c(10.19,159.67),c(0.93,114.88),c(166.97,1117.44),c(2.67,52.99),c(43.92,399.69),c(165.82,1053.41))
#######################################HR########################################




mysamp <- gen_psa_samp(params = params, 
                       dists = dists, 
                       parameterization_types = parameterization_types, 
                       dists_params = dists_params, 
                       nsamp = nsamp) #10000

head(mysamp)

psa_out <- run_psa(psa_samp = mysamp, 
                   params_basecase = param_ls, 
                   FUN = dectree_wrapper, 
                   outcomes = c("expectedQALYs", "Cost"), 
                   strategies = c("st_Baloxavir", "st_Oseltamivir", "st_Current"), 
                   currency = "$", 
                   treefunc = treefunc)
# print(str(psa_out))

##################################Negative##################################
psa_out$expectedQALYs$other_outcome <- -psa_out$expectedQALYs$other_outcome
# View(psa_out)

psa_obj <- make_psa_obj(cost = psa_out$Cost$other_outcome,
                        effectiveness = psa_out$expectedQALYs$other_outcome,
                        parameters = psa_out$Cost$parameters,
                        strategies = psa_out$Cost$strategies,
                        currency = "$")
# View(psa_obj)
# dev.new()
plot(psa_obj)

psa_out$Cost$other_outcome
head(psa_out$expectedQALYs$other_outcome)
length(psa_out$expectedQALYs$other_outcome$st_Current)
head(psa_out$Cost$other_outcome)


ceac <- ceac(wtp = seq(0, ktop, 5000), psa_obj)
ceac
ceac_new <- ceac[which(ceac$Strategy!="st_Current"),]
ceac_new
plot(ceac) 
theme_set(theme_bw())
plot(ceac_new)

write.csv(ceac,paste("ceac","_",set.p_O_Suscep_scenario,".csv",sep = ""))

