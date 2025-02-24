###Input
#1. Total Deliveries Deliveries = unlist(Total_Deliveries)
#2. Net Revenue Net_revenue = fin_results$Net_Revenue

get_validation <- function(Deliveries,Net_revenue){
  
  #Library
  library(ggplot2)
  library(cowplot)
  
  #--------Plot for deliveries-----------------#
  Deliveries = data.frame(Deliveries)
  
  p1 <- ggplot(Deliveries, aes(x = Deliveries)) + 
    geom_histogram(bins = 30, fill = "grey", color = "black") +
    geom_point(aes(x = 79774, y = 10), color = "red", size = 5) +
    geom_point(aes(x = 82226, y = 10), color = "red", size = 5) +
    geom_point(aes(x = 83017, y = 10), color = "red", size = 5) +
    xlab("Annual Energy Deliveries (GWh)") +  
    ylab("Frequency/Count") +  
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.text.y = element_text(size = rel(1.75)),
          axis.text.x = element_text(size = rel(1.75)),
          axis.title.x = element_text(size = 16),  
          axis.title.y = element_text(size = 16))
  
  
  #--------Plot for Net Revenue-----------------#
  Revenue = data.frame(Net_revenue)
  
  p2 <- ggplot(Revenue, aes(x = Net_revenue)) + 
    geom_histogram(bins = 30, fill = "grey", color = "black") +
    geom_point(aes(x = 12.261, y = 10), color = "red", size = 5) +
    geom_point(aes(x = 12.083, y = 10), color = "red", size = 5) +
    geom_point(aes(x = 12.3, y = 10), color = "red", size = 5) +
    xlab("Net Revenues ($B)") +  
    ylab("Frequency/Count") +  
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.text.y = element_text(size = rel(1.75)),
          axis.text.x = element_text(size = rel(1.75)),
          axis.title.x = element_text(size = 16),  
          axis.title.y = element_text(size = 16))
  
  plot_grid(p1,p2,
            nrow =1,
            labels = c('A', 'B'), 
            label_size = 18)
  
 
}