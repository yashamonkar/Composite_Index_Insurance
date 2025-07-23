#______________________________________________________________________________#
#This script code to generate the plots necessary for the alternative regulatory scenario

#______________________________________________________________________#
#Load data and dependencies

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")

#Library
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)

#Load the libraries
source("functions/get_revenue_comp.R")
source("functions/generic/generic_corr_plot.R")

#______________________________________________________________________#
#Reading the Input Data Files

#Revenues (Unmanaged and Managed using composite index)
tax_revenue <- read.csv("sims/Net_Revenue_Pollution_tax.csv",header = TRUE, sep=",")
composite_index_revenue_tax <- read.csv("sims/All_revenues_pollution_tax.csv")
no_tax_rev <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")

#Hydrometerological and Market Conditions
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)

#Pollution Damages
Pollution_Damages <- read.csv("sims/Prevented_Pollution_Damages.csv", header = TRUE, sep=",")

#Simulation abronmality
rm_sim <- 101 #Remove the 101 simulation run

#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)
streamflow = streamflow[-rm_sim]

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)
CDD = CDD[-rm_sim]

#Yearly Gas
Yearly_gas = Yearly_gas[-rm_sim,]

#______________________________________________________________________________#
#Final Plots for the paper
####-----------------Changes in the Revenue--------------------------###
pdf("figures/Revenue_Comparision.pdf",height=7, width=9)
get_revenue_comp(tax = tax_revenue$Net_revenue, 
                 no_tax = no_tax_rev$Net_revenue)
dev.off()

1000*(min(no_tax_rev$Net_revenue) - min(tax_revenue$Net_revenue))
1000*(mean(no_tax_rev$Net_revenue) - mean(tax_revenue$Net_revenue))
1000*(quantile(no_tax_rev$Net_revenue, 0.05) - quantile(tax_revenue$Net_revenue, 0.05))

####--------------Composite Index-Pollution Tax---------------------###
plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO,
                          Unmanaged = tax_revenue$Net_revenue,
                          Index = composite_index_revenue_tax$Composite_index)

p1 <- ggplot(plt_dataset) +
  geom_point(aes(x=Damages, y = Unmanaged), color='blue', size = 1.5) +
  geom_hline(aes(yintercept = mean(plt_dataset$Unmanaged)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Prevented Pollution Damages ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(2)),  
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))

p2 <- ggplot(plt_dataset) +
  geom_point(aes(x=Damages, y = Index), color='red', size = 1.5) +
  geom_hline(aes(yintercept = mean(plt_dataset$Index)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Prevented Pollution Damages ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(2)),  
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))

pdf("figures/Pollution_Tax.pdf",height=7, width=18)
plot_grid(p1,p2,
          nrow =1,
          labels = c('A', 'B'), 
          label_size = 18)
dev.off()
