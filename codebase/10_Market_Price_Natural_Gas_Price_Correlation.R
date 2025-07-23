#______________________________________________________#
###Natural Gas and Market Price Plot###
#Script to plot the relationship between market price and natural gas prices

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")


#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(ggplot2)

#______________________________________________________________________________#
###Reading the input data###
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)

#Simulation abronmality
rm_sim <- 101 #Remove the 101 simulation run

#_________________________________________________________________________#
###generate the plot
plt_dataset <- data.frame(Price = colMeans(Price)[-rm_sim],
                          Gas = Yearly_gas$V1[-rm_sim])


pdf("figures/Market_Price_Gas_Price_Corr.pdf", 
    height=7, width=9)
ggplot(plt_dataset) +
  geom_point(aes(x=Gas, y = Price), size = 1.75) +
  ylab("Market Price ($/MWhr)") +  
  xlab("Natural Gas Price ($/MMBtu)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))
dev.off()
