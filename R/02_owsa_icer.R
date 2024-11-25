# Cont. 

# To calculate the ICERs of per parameter by strategy
strategy <- c("st_Baloxavir")
# strategy <- c("st_Oseltamivir")
# Current_strategy <- c("st_Current")
Current_strategy <- c("st_Oseltamivir")
basic_result
basic_icer_Bal <- basic_result$icer[1]
basic_icer_Ose <- basic_result$icer[2]
Bal_vs_ose_result_icer <- Bal_vs_ose_result$icer[1]

# View(owsa_output)

df_owsa <- owsa_output[which(owsa_output$strategy == strategy),]
df_owsa_C <- owsa_output[which(owsa_output$strategy == Current_strategy),]
df_owsa <- data.frame(df_owsa)
df_owsa_C <- data.frame(df_owsa_C)
df_owsa
df_owsa_C
# View(df_owsa)
# View(df_owsa_C)

df_diff <- data.frame()
df_diff <- data.frame(df_owsa[,1:3])
df_diff$param_val <- rep(c("low","high"),length(df_diff$param_val)/2)
df_diff$Inc_cost <- df_owsa$Cost - df_owsa_C$Cost
df_diff$Inc_QALYs <- df_owsa$QALYs - df_owsa_C$QALYs
df_diff$icer <- df_diff$Inc_cost/df_diff$Inc_QALYs
# View(df_diff)

# print(paste(strategy,"owsa_icer.csv",sep = "_"))
write.csv(df_diff,paste(strategy,Current_strategy,"owsa_icer.csv",sep = "_"))

  
  
