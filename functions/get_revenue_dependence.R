###Input
#1. Net Revenue #Net_revenue = fin_results$Net_Revenue
#2. Streamflow #streamflow = streamflow
#3. CDD #CDD = CDD
#4. Market Price #Market_Price = fin_results$Market_Price
#5. Natural Gas Price #NG = Yearly_gas$V1

get_revenue_dependence <- function(Net_revenue, streamflow, CDD,
                                   Market_Price, NG_Price){
  
  
  #Library
  library(ggplot2)
  library(cowplot)
  
  #Set up the data set. 
  plt_dataset <- data.frame(streamflow = streamflow, 
                            CDD = CDD,
                            Market_Price = fin_results$Market_Price,
                            NG_Price = Yearly_gas$V1,
                            Net_revenue = fin_results$Net_Revenue)
  plt_dataset <- plt_dataset[-which.min(plt_dataset$Net_revenue), ]
  
  #--------------------------All Years----------------------------------#

  p1 <- ggplot(plt_dataset, aes(x = Net_revenue)) + 
    geom_histogram(bins = 30, fill = "grey", color = "black") +
    xlab(" ") +  
    ylab("Count") +  
    geom_text(aes(x = 11.5, y = 60, label = "All Years"), size = 8) +
    xlim(c(11.2,13.15)) +
    geom_vline(xintercept = mean(plt_dataset$Net_revenue), 
               col = "black", linetype ='dashed', size = 2) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(3), hjust = 0.5),
          axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(3)),
          axis.title.x = element_text(size = 14),  
          axis.title.y = element_text(size = 20))
  
  #--------------------------Dry Years----------------------------------#
  thresh <- quantile(plt_dataset$streamflow, 0.25)
  mod_dataset <- plt_dataset[plt_dataset$streamflow < thresh,]
  
  p2 <- ggplot(mod_dataset, aes(x = Net_revenue)) +
    geom_histogram(bins = 30, fill = "#EE4B2B", color = "black") +
    xlab(" ") +  
    geom_text(aes(x = 11.5, y = 17, label = "Dry Years"), size = 8) +
    ylab("Count") +  
    xlim(c(11.2,13.15)) +
    geom_vline(xintercept = mean(mod_dataset$Net_revenue), 
               col = "black", linetype ='dashed', size = 2) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(3)),
          axis.title.x = element_text(size = 14),  
          axis.title.y = element_text(size = 20))
  
  
  #--------------------------Cool Summers----------------------------------#
  thresh <- quantile(plt_dataset$CDD, 0.25)
  mod_dataset <- plt_dataset[plt_dataset$CDD < thresh,]
  
  p3 <- ggplot(mod_dataset, aes(x = Net_revenue)) +
    geom_histogram(bins = 30, fill = "yellow", color = "black") +
    xlab(" ") +  
    ylab("Count") +  
    geom_vline(xintercept = mean(mod_dataset$Net_revenue), 
               col = "black", linetype ='dashed', size = 2) +
    geom_text(aes(x = 11.5, y = 19, label = "Cool Summers"), size = 8) +
    xlim(c(11.2,13.15)) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(3)),
          axis.title.x = element_text(size = 14),  
          axis.title.y = element_text(size = 20))
  
  #--------------------------High Prices----------------------------------#
  thresh <- quantile(plt_dataset$NG_Price, 0.75)
  mod_dataset <- plt_dataset[plt_dataset$NG_Price > thresh,]
  
  
  p4 <- ggplot(mod_dataset, aes(x = Net_revenue)) +
    geom_histogram(bins = 30, fill = "green", color = "black") +
    xlab("Net Revenues ($B)") +  
    ylab("Count") + 
    geom_vline(xintercept = mean(mod_dataset$Net_revenue), 
               col = "black", linetype ='dashed', size = 2) +
    xlim(c(11.2,13.15)) +
    geom_text(aes(x = 11.5, y = 19, label = "High Natural Gas \n Prices"), size = 8) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(3)),
          axis.title.x = element_text(size = 24),  
          axis.title.y = element_text(size = 20))
  
  
  plot_grid(p1,p2,p3,p4,
            ncol =1,
            labels = c('A', 'B','C','D'), 
            label_size = 22)
  
  
  
}