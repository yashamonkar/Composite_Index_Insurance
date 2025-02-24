###Input
#1. Tax Revenue #tax = fin_results$Net_Revenue
#2. Regular Revenues #no_tax = no_tax_rev$Net_revenue

get_revenue_comp <- function(tax, no_tax){
  
  #Library
  library(ggplot2)
  library(cowplot)
  library(gridExtra)
  
  
  #Set up the data set. 
  plt_dataset <- data.frame(Revenue = c(tax, no_tax),
                            Scenario = rep(c("Tax", "No Tax"), each = length(tax)))
  
  mydata <- data.frame(Type= c("Decrease in Mean", "Decrease in Minimum", "Decrease in 95% VaR"))
  mydata$Value <- 0
  mydata$Value[1] <- round(1000*(mean(no_tax)-mean(tax)))
  mydata$Value[2] <- round(1000*(min(no_tax)-min(tax)))
  mydata$Value[3] <- round(1000*(quantile(no_tax,0.05)-quantile(tax,0.05)))
  colnames(mydata) <- c("Effect of Pollution Tax", "($ Million)")

  
  ggplot(plt_dataset, aes(x = Revenue, fill = Scenario)) +
    geom_histogram(position = "identity", alpha = 0.35, bins = 30, color = 'black') +
    scale_fill_manual(values = c("Tax" = "red", "No Tax" = "grey")) +
    geom_vline(aes(xintercept = mean(plt_dataset$Revenue[Scenario == "Tax"])),
               color = "red", size = 1.5) +
    geom_vline(aes(xintercept = mean(plt_dataset$Revenue[Scenario == "No Tax"])),
               color = "grey", size = 1.5) +
    geom_vline(aes(xintercept = min(plt_dataset$Revenue[Scenario == "Tax"])),
               color = "red", linetype = "dashed", size = 1.5) +
    geom_vline(aes(xintercept = min(plt_dataset$Revenue[Scenario == "No Tax"])),
               color = "grey", linetype = "dashed", size = 1.5) +
    labs(x = "Net Revenue ($B)", y = "Frequency") +
    theme_bw() +
    theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
          legend.position = c(0.9, 0.9),
          legend.text = element_text(size = 14), 
          legend.title = element_text(size = 16),
          axis.text.x = element_text(size = rel(1.5)),  
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = 14),  
          axis.title.y = element_text(size = 14))
  
  
}