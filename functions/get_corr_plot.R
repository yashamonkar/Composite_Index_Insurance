###Input
#1. Net Revenue #Net_revenue = fin_results$Net_Revenue
#2. Streamflow #streamflow = streamflow
#3. CDD #CDD = CDD
#4. Market Price #Market_Price = fin_results$Market_Price
#5. Natural Gas Price #NG = Yearly_gas$V1

get_corr_plot <- function(Net_revenue, streamflow, CDD, NG_Price, Fitted_Values){
  
  
  #Library
  library(ggplot2)
  library(cowplot)
  
  #Set up the data set. 
  plt_dataset <- data.frame(streamflow = streamflow, 
                            CDD = CDD,
                            NG_Price = NG_Price,
                            Composite_Index = Fitted_Values,
                            Net_revenue = Net_revenue)
  #plt_dataset <- plt_dataset[-which.min(plt_dataset$Net_revenue), ]
  
  plt_dataset <- data.frame(scale(plt_dataset))
  
  #Create a null plot 
  pnull <- ggplot() +
    theme_void() +
    geom_blank()
  
  #-------------------First Column------------------------------#
  p11 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 10, 
             label = "Streamflow", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    geom_blank()
  
  #Streamflow-CDD
  correlation_coefficient <- round(cor(plt_dataset$streamflow, plt_dataset$CDD), digits = 2)
  p21 <- ggplot(plt_dataset, aes(x = streamflow, y = CDD)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 0, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
  theme_bw() +
    theme(axis.text.y = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  #Streamflow-NG
  correlation_coefficient <- round(cor(plt_dataset$streamflow, plt_dataset$NG_Price), digits = 2)
  p31 <- ggplot(plt_dataset, aes(x = streamflow, y = NG_Price)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 0, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  #Streamflow-Composite Index
  correlation_coefficient <- round(cor(plt_dataset$streamflow, plt_dataset$Composite_Index), digits = 2)
  p41 <- ggplot(plt_dataset, aes(x = streamflow, y = Composite_Index)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 0, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  #Streamflow-Composite Index
  correlation_coefficient <- round(cor(plt_dataset$streamflow, plt_dataset$Net_revenue), digits = 2)
  p51 <- ggplot(plt_dataset, aes(x = streamflow, y = Net_revenue)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 0, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #-------------------Second Column------------------------------#
  p22 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 10, 
             label = "CDD", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    geom_blank()
  
  #CDD-Natural Gas
  correlation_coefficient <- round(cor(plt_dataset$CDD, plt_dataset$NG_Price), digits = 2)
  p32 <- ggplot(plt_dataset, aes(x = CDD, y = NG_Price)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 1.5, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #CDD-Composite Index
  correlation_coefficient <- round(cor(plt_dataset$CDD, plt_dataset$Composite_Index), digits = 2)
  p42 <- ggplot(plt_dataset, aes(x = CDD, y = Composite_Index)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 1.7, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #CDD-Composite Index
  correlation_coefficient <- round(cor(plt_dataset$CDD, plt_dataset$Net_revenue), digits = 2)
  p52 <- ggplot(plt_dataset, aes(x = CDD, y = Net_revenue)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 1.7, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #-------------------Third Column------------------------------#
  p33 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 10, 
             label = "Natural \n Gas", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    geom_blank()
  
  #Natural Gas - Composite Index
  correlation_coefficient <- round(cor(plt_dataset$NG_Price, plt_dataset$Composite_Index), digits = 2)
  p43 <- ggplot(plt_dataset, aes(x = NG_Price, y = Composite_Index)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 1.7, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(0)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  #Natural Gas - Net Revenue
  correlation_coefficient <- round(cor(plt_dataset$NG_Price, plt_dataset$Net_revenue), digits = 2)
  p53 <- ggplot(plt_dataset, aes(x = NG_Price, y = Net_revenue)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = 1.7, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #-------------------Fourth Column------------------------------#
  p44 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 10, 
             label = "Composite \n Index", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    geom_blank()
  
  #Composite Index - Net Revenues
  correlation_coefficient <- round(cor(plt_dataset$Composite_Index, plt_dataset$Net_revenue), digits = 2)
  p54 <- ggplot(plt_dataset, aes(x = Composite_Index, y = Net_revenue)) +
    geom_point() +
    xlab(" ") +  
    ylab(" ") + 
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    annotate("text", x = -1.5, y = 3, 
             label = paste("R =", correlation_coefficient), size = 8, col='red') +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12))
  
  
  #-------------------Fifth Column------------------------------#
  p55 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, size = 10, 
             label = "Net \n Revenue", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    geom_blank()
  
  
  plot_grid(p11,pnull,pnull,pnull,pnull, #First Row
            p21,p22,pnull,pnull,pnull,
            p31,p32,p33,pnull,pnull,
            p41,p42,p43,p44,pnull,
            p51,p52,p53,p54,p55,
            ncol =5, nrow = 5)
  
}
  
  
  
  
