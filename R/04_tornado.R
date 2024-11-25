# rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(jph)
library(tornado)

# original value of output
# Keep "owsa_icer_table_3.R" strategy is same with "tornado_4.R"
# strategy <- c("st_Oseltamivir")
strategy <- c("st_Baloxavir")
# Current_strategy <- c("st_Current")
Current_strategy <- c("st_Oseltamivir")
#########################
basic_icer_Bal <- basic_result$icer[1]
basic_icer_Ose <- basic_result$icer[2]
basic_icer_Bal
basic_icer_Ose
basic_icer_BalvsOse <- Bal_vs_ose_result$icer[1]

# ifelse(strategy=="st_Baloxavir", base.value <- basic_icer_Bal, base.value <- basic_icer_Ose)

  
if(strategy=="st_Baloxavir") {
   if(Current_strategy == "st_Current"){
     base.value <- basic_icer_Bal
   }else {
     base.value <- basic_icer_BalvsOse
   }
 }else {
base.value <- basic_icer_Ose   
 }

base.value
dev.new()


file <- paste("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_HR_Export/",strategy,"_",Current_strategy,"_owsa_icer_table.csv",sep = "")
file
df <- read.csv(file)
df <- data.frame(df)
df <- df[c(number),2:5]
df
df <- plyr::rename(df,replace = c("parameter_list"="Parameter",
                            "LB"="Lower_Bound",
                            "UB"="Upper_Bound",
                            "Diff"="UL_Difference"))
df

#######################################################
# negative value delete

# for (x in 1:10){
#   for (y in 2:3){
#     ifelse(df[x,y]>=0,df[x,y]<-df[x,y],df[x,y]<-NA)
#   }
# }
#######################################################

# get order of parameters according to size of intervals
# (I use this to define the ordering of the factors which I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95

# get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
  # just reordering columns
  select(Parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
# png(width = 960, height = 540)
ggplot() + 
  geom_rect(data = df.2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  coord_flip()
# dev.off()
