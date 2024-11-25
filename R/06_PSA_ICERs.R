library(hesim)
library(magrittr) # Use pipes
library(ggplot2)

# https://cran.r-project.org/web/packages/hesim/vignettes/cea.html
# https://hesim-dev.github.io/hesim/reference/index.html
# https://hesim-dev.github.io/hesim/reference/cea.html

ktop <- 152667
nsamp <- 2000
# setwd("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export")setwd("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export")
# ce <- read.csv("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_HR_Export/psa_data_result.csv")
ce <- read.csv("C:/Users/chenr/Desktop/AMUA/Tree_Basic/check_base_usd_Sep11_all_Export/psa_data_result.csv")
# ce == psa_data_result
ce <- data.frame(ce)
ce$sample <- rep(seq(nsamp),each = 3)
# View(ce)

cea_out <-  cea(ce, k = seq(0, ktop, 5000), sample = "sample", strategy = "strategy",
                grp = "grp", e = "QALYs", c = "cost")
cea_out

# Baloxavir vs. basline & Oseltamivir vs. basline
ie <- incr_effect(ce, comparator = "st_Current", sample = "sample",
                  strategy = "strategy", grp = "grp", outcomes = c("QALYs", "cost"))
ie
# View(ie) # Correct--Result

ie <- data.frame(ie)

# write.csv(ie,"ie.csv")

# Baloxavir vs. Oseltamivir
ie_BvsO <- incr_effect(ce, comparator = "st_Oseltamivir", sample = "sample",
                  strategy = "strategy", grp = "grp", outcomes = c("QALYs", "cost"))
ie_BvsO
ie_BvsO <- data.frame(ie_BvsO[which(ie_BvsO$strategy=="st_Baloxavir"),])

# View(ie_BvsO) # Correct--Result

# write.csv(ie_BvsO,"ie_BvsO.csv")

##############################
cea_pw_out <- cea_pw(ce,  k = seq(0, ktop, 5000), comparator = "st_Current",
                      sample = "sample", strategy = "strategy", grp = "grp",
                      e = "QALYs", c = "cost")
icer(cea_pw_out, k = ktop) %>% format()
cea_pw_out$ceac
head(cea_pw_out$delta)

# PSA plot
theme_set(theme_bw())
############################################################
# comparator = "st_Current"
############################################################
# col = c("F94040","55A0FB"))
# First_draw
plot_ceplane(cea_pw_out, k = ktop) + xlim(-0.01,0.01) + ylim(c(-400,400))
plot_ceplane(cea_pw_out, k = ktop) + xlim(-0.0005,0.0015) + ylim(c(0,150))
plot_ceac(cea_out)
plot_ceac(cea_pw_out)
plot_ceaf(cea_out)

# Second_draw
# Create the initial ggplot object with data and aesthetics
p <- ggplot(ie, aes(x = iQALYs, y = icost, color = strategy))
p <- p + geom_point(size = 1)
p <- p + scale_color_manual(values = c("st_Baloxavir" = "#FF6666", "st_Oseltamivir" = "#87CEEB"))
p <- p + labs(x = "Incremental QALYs", y = "Incremental costs")
p <- p + geom_abline(intercept = 0, slope = ktop, linetype = "dashed")
p <- p + geom_hline(yintercept = 0, color = "black", linetype = "solid")
p <- p + geom_vline(xintercept = 0, color = "black", linetype = "solid")
# p <- p + xlim(-0.0002,0.0035) + ylim(c(0,150))

# Display the plot
print(p)


############################################################
# comparator = "st_Oseltamivir"
############################################################
p_BvsO <- ggplot(ie_BvsO, aes(x = iQALYs, y = icost, color = strategy))
p_BvsO <- p_BvsO + geom_point(size = 1)
p_BvsO <- p_BvsO + scale_color_manual(values = c("st_Baloxavir" = "#2ECC71"))
p_BvsO <- p_BvsO + labs(x = "Incremental QALYs", y = "Incremental costs")
p_BvsO <- p_BvsO + geom_abline(intercept = 0, color = "black", slope = ktop, linetype = "dashed")
p_BvsO <- p_BvsO + geom_hline(yintercept = 0, color = "black", linetype = "solid")
p_BvsO <- p_BvsO + geom_vline(xintercept = 0, color = "black", linetype = "solid")
# p_BvsO <- p_BvsO + xlim(-0.00025,0.001) + ylim(c(-25,75))

# Display the plot
print(p_BvsO)

