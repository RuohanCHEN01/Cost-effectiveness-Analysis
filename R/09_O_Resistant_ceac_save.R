# Combine the dataset of pr-WTP

library(tidyr)
library(dplyr)


for (i in c(1,0.9,0.8,0.7,0.6,0.5)){
  p_O_sup <- i
  print(i)
  path <- paste("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export/","ceac_",p_O_sup,".csv",sep = "")
  data <- read.csv(path)
  data <- data[,2:4]
  data
  
  piv_data <- pivot_wider(data, names_from = "Strategy", values_from = "Proportion")
  piv_data[,2:3]  
  
  
  if (p_O_sup==1){
    piv_save <- dplyr:::rename(data.frame(unique(data$WTP)),"WTP" = unique.data.WTP.)
    # piv_save <- cbind(piv_save,piv_data[,2:3])
    piv_save <- cbind(piv_save,piv_data[,2:4])
  } else{
    # piv_save <- cbind(piv_save,piv_data[,2:3])
    piv_save <- cbind(piv_save,piv_data[,2:4])
  }

}
View(piv_save)

write.csv(piv_save,"C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export/ceac_piv_all_save.csv")
