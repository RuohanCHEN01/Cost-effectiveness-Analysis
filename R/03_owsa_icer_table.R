## Cont.2

library(ggplot2)
library(jph)
library(tornado)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)

# strategy <- c("st_Oseltamivir")
strategy <- c("st_Baloxavir")
# Current_strategy <- c("st_Current")
Current_strategy <- c("st_Oseltamivir")
filename <- paste("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_HR_Export/",strategy,"_",Current_strategy,"_owsa_icer.csv",sep="")
print(filename)

df <- read.csv(filename)
# View(df)
parameter_list <- unique(df$parameter)
# parameter_list

# rep(parameter_list,each=2)
owsa_icer_table <- data.frame(parameter_list)
# owsa_icer_table
for (i in 1:length(parameter_list)){
  owsa_icer_table$LB[i] <- df[which(df$parameter == parameter_list[i]),]$icer[1]
  owsa_icer_table$UB[i] <- df[which(df$parameter == parameter_list[i]),]$icer[2]
  }

owsa_icer_table$Diff <- abs(owsa_icer_table$LB-owsa_icer_table$UB)
owsa_icer_table
# View(owsa_icer_table)

# write.csv(owsa_icer_table,paste(strategy,Current_strategy,"owsa_icer_table.csv",sep = "_"))

# paste(strategy,"_owsa_icer_table.csv",sep = "")
order(owsa_icer_table$Diff,decreasing = T)
data_out <-owsa_icer_table[order(owsa_icer_table$Diff,decreasing = T),][1:10,1:3]
data_out <- data.frame(data_out)
data_out$parameter_list
data_out
# First one dataset top 10 parameters of st_strategy_icer
no <- c()
new <- c()
for (j in 1:20){
  new <- which(df$parameter==data_out$parameter_list[j])
  no <- append(no,new)
}
print(no)
# First one dataset top 10 parameters of st_strategy_icer_table
number <- c()
new <- c()
for (top in 1:10){
  new <- which(owsa_icer_table$parameter==data_out$parameter_list[top])
  number <- append(number,new)
}
print(number)








