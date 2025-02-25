#______________________________________________________________________________#
#Compound Climate Risk Plot used in the figure

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")

#Load libraries
library(ggplot2)
library(corrplot)
library(zoo)
library(dplyr)

#______________________________________________________________________________#
#Simulation abronmality
rm_sim <- 101 #Remove the 101 simulation run

###Demand Data###
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Demand <- matrix(Demand$V1, nrow = 8760, ncol = 500)
Demand <- as.data.frame(Demand)
max_demand <- apply(Demand, 2, max)
max_demand <- max_demand[-rm_sim]


#Daily Steps
daily_demand <- sapply(Demand, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_demand <- daily_demand[1:363,]
daily_demand <- as.numeric(unlist(daily_demand))

#Annual Time Steps
yearly_demand <- colMeans(Demand)
yearly_demand <- yearly_demand[-rm_sim]
Demand <- NULL


#Price Data
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
max_price <- apply(Price, 2, max)
daily_price <- sapply(Price, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_price <- as.numeric(unlist(daily_price))
annual_price <- colMeans(Price)
annual_price <- annual_price[-rm_sim]


#Max Temperature -- Daily
Temp <- read.csv("data/temp_annual.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
Temp = Temp[,pge_cities]
Temp = data.frame(Temp = rowMeans(Temp),
                  Year = rep(1:500, each = 365))
Temp = Temp %>% group_by(Year) %>% summarize(max(Temp))
max_temp = Temp$`max(Temp)`
max_temp <- max_temp[-rm_sim]
Temp = NULL

#CDD
CDD <- read.csv("data/CDD.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)
CDD = CDD[-rm_sim]


#Streamflow
streamflow <- read.csv("data/Streamflow.csv")
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)
streamflow = streamflow[-rm_sim]

#Net Revenue
net_revenue <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")

#Naural Gas
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)
Yearly_gas <- Yearly_gas[-rm_sim,]

plt_dataset <- data.frame(Demand = yearly_demand, 
                          streamflow = streamflow,
                          CDD = CDD,
                          Price = annual_price,
                          NG = Yearly_gas,
                          Revenue = net_revenue$Net_revenue)

quants <- quantile(plt_dataset$streamflow, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
plt_dataset$discrete_streamflow <- cut(plt_dataset$streamflow, breaks = 4, labels = c("Low", "Medium", "High" , " Very High"))

#Select values to highlight
top_values <- sort(plt_dataset$Price, decreasing = TRUE)[1:3]
high_price <- plt_dataset[plt_dataset$Price %in% top_values,]

top_values <- sort(plt_dataset$CDD, decreasing = TRUE)[1:3]
high_cdd <- plt_dataset[plt_dataset$CDD %in% top_values,]

top_values <- sort(plt_dataset$NG, decreasing = TRUE)[1:3]
high_gas <- plt_dataset[plt_dataset$NG %in% top_values,]


pdf("figures/Revenue_Correlation.pdf", 
    height=7, width=10)


high_cdd$Labels <- c("B", "A", "C")
high_price$Labels <- c("E", "D", "C")

ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=NG, 
                 shape = factor(discrete_streamflow),
                 col = CDD), size = 2) +
  geom_point(data = high_cdd, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) + 
  geom_text(data = high_cdd, 
            mapping = aes(x=NG,y=Revenue, label = Labels), 
            nudge_y = 0.05, vjust = 0) +
  geom_point(data = high_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 7) +
  geom_text(data = high_price, 
            mapping = aes(x=NG,y=Revenue, label = Labels), 
            nudge_y = 0.05, vjust = 0) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.15), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  ylab("Net Revenue ($B)") +  
  xlab("Natural Gas Price ($/Million Btu)") +
  guides(color = guide_colourbar(barwidth = rel(1), barheight = rel(8))) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = c(0.01, 0.01),
        legend.justification = c(0.01, 0.01),
        legend.box.just = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  labs(shape = "Streamflow") 

dev.off()


