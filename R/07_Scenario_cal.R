## Sensitivity of Oseltamivir Resistant
library(dplyr)

################################################################
# Oseltamivir resistant 
O_Res <- data.frame(c(1,0.9,0.8,0.7,0.6,0.5))
O_Res <- dplyr::rename(O_Res,"O_res"="c.1..0.9..0.8..0.7..0.6..0.5.")

for (i in 0:5){
  param_ls_O <- param_ls
  param_ls_O$p_O_Suscep_scenario <- 1-i/10
  tree_output_O <- dectree_wrapper(params_basecase = param_ls_O, treefunc = treefunc, popsize = 1)
  # print(tree_output_O)
  # print(str(owsa_out))
  # print(owsa_out$owsa_Cost)
  # print(owsa_out$owsa_expectedQALYs)
  basic_result_O <- data.frame(tree_output_O)
  ##################################Negative##################################
  basic_result_O$expectedQALYs <- -basic_result_O$expectedQALYs
  basic_result_O
  basic_result_O$Inc_cost[1] <- basic_result_O$Cost[1]-basic_result_O$Cost[3]
  basic_result_O$Inc_cost[2] <- basic_result_O$Cost[2]-basic_result_O$Cost[3]
  basic_result_O$Inc_cost[3] <- NA
  # print(basic_result_O$Inc_cost[2])
  basic_result_O$Inc_eff[1] <- basic_result_O$expectedQALYs[1]-basic_result_O$expectedQALYs[3]
  basic_result_O$Inc_eff[2] <- basic_result_O$expectedQALYs[2]-basic_result_O$expectedQALYs[3]
  basic_result_O$Inc_eff[3] <- NA
  # print(basic_result_O$Inc_eff[2])
  O_Res$COST[i+1] <- basic_result_O$Cost[2]
  O_Res$EFF[i+1] <- basic_result_O$expectedQALYs[2]
  O_Res$inc_COST[i+1] <- basic_result_O$Inc_cost[2]
  O_Res$inc_EFF[i+1] <- basic_result_O$Inc_eff[2]
  
  Bal_vs_ose_result <- data.frame(tree_output_O[1:2,])
  ##################################Negative##################################
  Bal_vs_ose_result$expectedQALYs <- -Bal_vs_ose_result$expectedQALYs
  Bal_vs_ose_result$Inc_cost[1] <- Bal_vs_ose_result$Cost[1]-Bal_vs_ose_result$Cost[2]
  Bal_vs_ose_result$Inc_cost[2] <- NA
  Bal_vs_ose_result$Inc_eff[1] <- Bal_vs_ose_result$expectedQALYs[1]-Bal_vs_ose_result$expectedQALYs[2]
  Bal_vs_ose_result$Inc_eff[2] <- NA
  Bal_vs_ose_result$icer <- Bal_vs_ose_result$Inc_cost/Bal_vs_ose_result$Inc_eff
  print(i)
  print(Bal_vs_ose_result)
  
}
O_Res$icer <- O_Res$inc_COST/O_Res$inc_EFF
O_Res

# O_Res[202,] <- c("B",27.878134,0.0007508522)
# write.csv(O_Res,"O_Res.csv")


# Oseltamivir cost
O_Cost <- data.frame(c(0:5)*50)
O_Cost <- dplyr::rename(O_Cost,"O_cost"="c.0.5....50")

for (i in 0:5){
  param_ls_O <- param_ls
  param_ls_O$c_O <- 50*i
  tree_output_O <- dectree_wrapper(params_basecase = param_ls_O, treefunc = treefunc, popsize = 1)
  # print(tree_output_O)
  # print(str(owsa_out))
  # print(owsa_out$owsa_Cost)
  # print(owsa_out$owsa_expectedQALYs)
  basic_result_O <- data.frame(tree_output_O)
  ##################################Negative##################################
  basic_result_O$expectedQALYs <- -basic_result_O$expectedQALYs
  basic_result_O$Inc_cost[1] <- basic_result_O$Cost[1]-basic_result_O$Cost[3]
  basic_result_O$Inc_cost[2] <- basic_result_O$Cost[2]-basic_result_O$Cost[3]
  basic_result_O$Inc_cost[3] <- NA
  # print(basic_result_O$Inc_cost[2])
  basic_result_O$Inc_eff[1] <- basic_result_O$expectedQALYs[1]-basic_result_O$expectedQALYs[3]
  basic_result_O$Inc_eff[2] <- basic_result_O$expectedQALYs[2]-basic_result_O$expectedQALYs[3]
  basic_result_O$Inc_eff[3] <- NA
  # print(basic_result_O$Inc_eff[2])
  O_Cost$COST[i+1] <- basic_result_O$Cost[2]
  O_Cost$EFF[i+1] <- basic_result_O$expectedQALYs[2] 
  O_Cost$inc_COST[i+1] <- basic_result_O$Inc_cost[2]
  O_Cost$inc_EFF[i+1] <- basic_result_O$Inc_eff[2]

  Bal_vs_ose_result <- data.frame(tree_output_O[1:2,])
  ##################################Negative##################################
  Bal_vs_ose_result$expectedQALYs <- -Bal_vs_ose_result$expectedQALYs
  Bal_vs_ose_result$Inc_cost[1] <- Bal_vs_ose_result$Cost[1]-Bal_vs_ose_result$Cost[2]
  Bal_vs_ose_result$Inc_cost[2] <- NA
  Bal_vs_ose_result$Inc_eff[1] <- Bal_vs_ose_result$expectedQALYs[1]-Bal_vs_ose_result$expectedQALYs[2]
  Bal_vs_ose_result$Inc_eff[2] <- NA
  Bal_vs_ose_result$icer <- Bal_vs_ose_result$Inc_cost/Bal_vs_ose_result$Inc_eff
  print(i)
  print(Bal_vs_ose_result)
  
}
O_Cost$icer <- O_Cost$inc_COST/O_Cost$inc_EFF
O_Cost

# write.csv(O_Cost,"O_Cost.csv")


# Baloxavir Cost
B_Cost <- data.frame(c(0:5)*50)
B_Cost <- dplyr::rename(B_Cost,"B_cost"="c.0.5....50")

for (i in 0:5){
  param_ls_B <- param_ls
  param_ls_B$c_B <- 50*i
  tree_output_B <- dectree_wrapper(params_basecase = param_ls_B, treefunc = treefunc, popsize = 1)
  # print(tree_output_B)

  basic_result_B <- data.frame(tree_output_B)
  ##################################Negative##################################
  basic_result_B$expectedQALYs <- -basic_result_B$expectedQALYs
  basic_result_B$Inc_cost[1] <- basic_result_B$Cost[1]-basic_result_B$Cost[3]
  basic_result_B$Inc_cost[2] <- basic_result_B$Cost[2]-basic_result_B$Cost[3]
  basic_result_B$Inc_cost[3] <- NA
  # print(basic_result_B$Inc_cost[1])
  basic_result_B$Inc_eff[1] <- basic_result_B$expectedQALYs[1]-basic_result_B$expectedQALYs[3]
  basic_result_B$Inc_eff[2] <- basic_result_B$expectedQALYs[2]-basic_result_B$expectedQALYs[3]
  basic_result_B$Inc_eff[3] <- NA
  # print(basic_result_B$Inc_eff[1])
  B_Cost$COST[i+1] <- basic_result_B$Cost[1]
  B_Cost$EFF[i+1] <- basic_result_B$expectedQALYs[1]
  B_Cost$inc_COST[i+1] <- basic_result_B$Inc_cost[1]
  B_Cost$inc_EFF[i+1] <- basic_result_B$Inc_eff[1]
  
  
  Bal_vs_ose_result <- data.frame(tree_output_B[1:2,])
  ##################################Negative##################################
  Bal_vs_ose_result$expectedQALYs <- -Bal_vs_ose_result$expectedQALYs
  Bal_vs_ose_result$Inc_cost[1] <- Bal_vs_ose_result$Cost[1]-Bal_vs_ose_result$Cost[2]
  Bal_vs_ose_result$Inc_cost[2] <- NA
  Bal_vs_ose_result$Inc_eff[1] <- Bal_vs_ose_result$expectedQALYs[1]-Bal_vs_ose_result$expectedQALYs[2]
  Bal_vs_ose_result$Inc_eff[2] <- NA
  Bal_vs_ose_result$icer <- Bal_vs_ose_result$Inc_cost/Bal_vs_ose_result$Inc_eff
  print(i)
  print(Bal_vs_ose_result)
  
}
B_Cost$icer <- B_Cost$inc_COST/B_Cost$inc_EFF
B_Cost
# write.csv(B_Cost,"B_Cost.csv")

