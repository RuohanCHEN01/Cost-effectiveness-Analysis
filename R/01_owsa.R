######################################################
### Title: Cost-Effectiveness of Baloxavir Marboxil Versus Oseltamivir for treating Influenza in the Hong Kong.
### @Author: Chen Ruohan (HKU)
### R version 4.3.1 (2023-06-16 ucrt)
### Updated: 22/10/2024
######################################################

rm(list=ls())
options(warn = - 1)
library(CEAutil)
library(dampack)

#Input the model and basic data

setwd("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export")
treetxt <- parse_amua_tree("main.R")

#Print Parameter
print(treetxt$param_ls)
param_ls <- treetxt[["param_ls"]]
treefunc <- treetxt[["treefunc"]]


# View(param_ls)

#Output
tree_output <- dectree_wrapper(params_basecase = param_ls, treefunc = treefunc, popsize = 1)
print(tree_output)
#ICER(BvsO,OvsC)
# dracula_icer <- calculate_icers(tree_output$Cost,
#                                 tree_output$expectedQALYs,
#                                 tree_output$strategy)
# print(dracula_icer)
# summary(dracula_icer)

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

Bal_vs_ose_result <- data.frame(tree_output[1:2,])
##################################Negative##################################
Bal_vs_ose_result$expectedQALYs <- -Bal_vs_ose_result$expectedQALYs
Bal_vs_ose_result$Inc_cost[1] <- Bal_vs_ose_result$Cost[1]-Bal_vs_ose_result$Cost[2]
Bal_vs_ose_result$Inc_cost[2] <- NA
Bal_vs_ose_result$Inc_eff[1] <- Bal_vs_ose_result$expectedQALYs[1]-Bal_vs_ose_result$expectedQALYs[2]
Bal_vs_ose_result$Inc_eff[2] <- NA
Bal_vs_ose_result$icer <- Bal_vs_ose_result$Inc_cost/Bal_vs_ose_result$Inc_eff
Bal_vs_ose_result

#Plot
# plot(dracula_icer, effect_units = "mL")

#Sensitivity analysis with dampack
  ##One-way sensitivity analysis use: "run_owsa_det(params_range, params_basecase, nsamp = 100, FUN, outcomes = NULL, strategies = NULL, ...)"

###List parameter range
params_list <- read.csv("C:/Users/chenr/Desktop/AMUA/param_all.csv")
params_list <- data.frame(params_list)
params_range <- params_list
print(params_range)

outcomes = c("expectedQALYs", "Cost")
print(param_ls)
owsa_out <- run_owsa_det(params_range = params_range, 
                         params_basecase = param_ls, 
                         nsamp = 2, 
                         FUN = dectree_wrapper, 
                         treefunc = treefunc, 
                         outcomes = c("expectedQALYs", "Cost"),
                         popsize = 1)
# print(str(owsa_out))
print(owsa_out$owsa_Cost)
print(owsa_out$owsa_expectedQALYs)

# Output OWSA result
owsa_output <- data.frame()
owsa_output <- data.frame(owsa_out$owsa_expectedQALYs)
names(owsa_output)[names(owsa_output) == "outcome_val"] <- "QALYs"
owsa_output$Cost <- owsa_out$owsa_Cost$outcome_val
##################################Negative##################################
owsa_output$QALYs <- -owsa_output$QALYs
# View(owsa_output)
# write.csv(owsa_output,"owsa_output.csv")






