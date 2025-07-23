#______________________________________________________________________#
###Code for implementing the composite index insurance###
### Baseline Case
### Main criteria the variance of the revenues match ### 
### Step 1:- Compute the hedged revenues for the 3 indices. 
### Step 2:- Select the strike for the composite index 
### such that the variance of the composite index and portfolio is the same. 

###For each regression, the data are divided into training and testing
###The model fit is assessed. 
### For computing the strike, all the data (training + testing values) are used.


#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")

#______________________________________________________________________#
#Load Libraries
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)
library(patchwork)

#Load functions
source("functions/get_corr_plot.R")


#______________________________________________________________________#
#Initial Data Wrangling
net_revenue <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)


#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)

#Simulation abronmality
rm_sim <- 101 #Remove the 101 simulation run

#______________________________________________________________________#
#Divide into training and testing. 
set.seed(123)
train_frac <- 0.8

#Set-up regression dataset
reg_dataset <- data.frame(streamflow = scale(log(streamflow))[-rm_sim], 
                          CDD = scale(CDD)[-rm_sim],
                          Natural_Gas = scale(Yearly_gas$V1)[-rm_sim],
                          Net_revenue = net_revenue$Net_revenue)

#Separate into Testing and Training Dataset
train_years <- sample(1:nrow(reg_dataset), 
                      round(500*train_frac), 
                      replace = FALSE)
train_dataset <- reg_dataset[train_years,]
test_dataset <- reg_dataset[-train_years,]


#Global Hyper-parameters. 
ned_var <- 0.05 #95% VaR 

#______________________________________________________________________#
###--------------------Set up regressions----------------------------###

###------Streamflow----------###
reg_streamflow = lm(Net_revenue ~ streamflow,
                    train_dataset)

newdata = data.frame(streamflow=test_dataset$streamflow)
train_cor <- round(cor(train_dataset$Net_revenue, reg_streamflow$fitted.values),3)
adj_r <- round(summary(reg_streamflow)$adj.r.squared,3)


###------CDD----------###
reg_CDD = lm(Net_revenue ~ CDD,
             train_dataset)

newdata = data.frame(CDD=test_dataset$CDD)
train_cor <- round(cor(train_dataset$Net_revenue, reg_CDD$fitted.values),3)
adj_r <- round(summary(reg_CDD)$adj.r.squared,3)


###------NG----------###
reg_price = lm(Net_revenue ~ Natural_Gas,
               train_dataset)
train_cor <- round(cor(train_dataset$Net_revenue, reg_price$fitted.values),3)
adj_r <- round(summary(reg_price)$adj.r.squared,3)
newdata = data.frame(Natural_Gas=test_dataset$Natural_Gas)


###------Composite Index----------###
reg_composite = lm(Net_revenue ~ streamflow + CDD + Natural_Gas,
                   train_dataset)

newdata = data.frame(streamflow = test_dataset$streamflow,
                     CDD = test_dataset$CDD,
                     Natural_Gas=test_dataset$Natural_Gas)
train_cor <- round(cor(train_dataset$Net_revenue, reg_composite$fitted.values),3)
adj_r <- round(summary(reg_composite)$adj.r.squared,3)

#Create the correlation plot
test_pred <- predict(reg_composite, newdata)

pdf("figures/Corr_Plot.pdf",height=15, width=15)
get_corr_plot(Net_revenue = c(train_dataset$Net_revenue,test_dataset$Net_revenue),
              streamflow = c(train_dataset$streamflow,test_dataset$streamflow),
              CDD = c(train_dataset$CDD,test_dataset$CDD),
              NG_Price = c(train_dataset$Natural_Gas,test_dataset$Natural_Gas),
              Fitted_Values = c(reg_composite$fitted.values, test_pred))
dev.off()


###--------------------------------------------------------------------------###
#Develop Model fit estimates & plots

#Estimate Training & Testing Errors
paste0("The mean training error is ", mean((train_dataset$Net_revenue-reg_composite$fitted.values)^2))
paste0("The mean testing error is ", mean((test_dataset$Net_revenue-predict(reg_composite, newdata))^2))

plt_dataset <- data.frame(Revenues = c(train_dataset$Net_revenue, test_dataset$Net_revenue),
                          Fitted = c(reg_composite$fitted.values, predict(reg_composite, newdata)),
                          Type = c(rep("Training",400), rep("Testing",99) ))

pdf("figures/Testing_Training_Error.pdf", 
    height=7, width=9)
ggplot(plt_dataset) +
  geom_point(aes(x=Revenues, y = Fitted, color = Type), size = 2) +
  xlab("Net Revenues ($B)") +  
  xlim(c(11.5,13)) +
  ylim(c(11.5,13)) +
  ylab("Fitted Values ($B)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  
  scale_color_manual(values = c("Testing" = "red", "Training" = "black")) +  # Setting manual colors
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))
dev.off()

#_________________________________________________________________________#
####Functions 

#INPUTS
#1. Upper Bound of Search. upper = 0.3
#2. Lower Bound of Search. lower = 0.01
#3. The fitted Regression. reg = reg_streamflow
#4. Newdata (Testing and Training Data). newdata = data.frame(streamflow=reg_dataset$streamflow)
#5. Value at Risk Threshold (var_thresh) var_thresh = 0.05
#6. Unhedged Revenue. unhedged = reg_dataset$Net_revenue 

###Output
#1. Hedged Revenues
#2. Payout
#2. Strike Value in percentile
#3. Premium Costs

get_strike <- function(upper, lower, reg, newdata, var_thresh, unhedged){
  
  #Compute the predicted revenue
  pred_rev <- predict(reg , newdata)
  
  #Create sequences of strikes -- Percentiles
  strike_seq <- seq(from = lower, to = upper, length = 100) 
  strike <- quantile(pred_rev, strike_seq)
  
  #Payout Matrix
  payout <- matrix(NA, nrow = length(pred_rev), ncol = length(strike))
  
  #Positive Correlation
  if(upper < 0.5) {
    for(i in 1:length(strike)){payout[,i] <- strike[i] - pred_rev}
    payout[payout < 0] = 0
  } else {
    for(i in 1:length(strike)){payout[,i] <- pred_rev- strike[i]}
    payout[payout < 0] = 0
  }
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = unhedged + payout[,i] - braun_premium[i]
  }
  
  #Optimizing criteria
  revenue_var <- apply(hedged_revenues,2,function(x) quantile(x, probs=var_thresh))
  val_indx <- which.max(revenue_var)
  
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
  
}



#FUNCTION TO MATCH VARIANCE
match_var <- function(upper, lower, reg, newdata, portfolio_var, unhedged){
  
  #Compute the predicted revenue
  pred_rev <- predict(reg , newdata)
  
  #Create sequences of strikes -- Percentiles
  strike_seq <- seq(from = lower, to = upper, length = 400) 
  strike <- quantile(pred_rev, strike_seq)
  
  #Payout Matrix
  payout <- matrix(NA, nrow = length(pred_rev), ncol = length(strike))
  
  #Positive Correlation
  if(upper < 0.5) {
    for(i in 1:length(strike)){payout[,i] <- strike[i] - pred_rev}
    payout[payout < 0] = 0
  } else {
    for(i in 1:length(strike)){payout[,i] <- pred_rev- strike[i]}
    payout[payout < 0] = 0
  }
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = unhedged + payout[,i] - braun_premium[i]
  }
  
  #Optimizing criteria
  revenue_var <- apply(hedged_revenues,2,var)
  val_indx <- which.min(abs(revenue_var-portfolio_var))
  
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
  
}



#_____________________________________________________________________#
###Estimate the strikes for the individual indices

###-------------Streamflow-----------------#
sf_index <- get_strike(upper=0.3,
                       lower=0.1,
                       reg=reg_streamflow,
                       newdata = data.frame(streamflow=reg_dataset$streamflow),
                       var_thresh = 0.05,
                       unhedged = reg_dataset$Net_revenue)

###-------------CDD-------------------#
cdd_index <- get_strike(upper=0.3,
                        lower=0.1,
                        reg=reg_CDD,
                        newdata = data.frame(CDD=reg_dataset$CDD),
                        var_thresh = 0.05,
                        unhedged = reg_dataset$Net_revenue)


###-------------Natural Gas Price-------------------#
NG_index <- get_strike(upper=0.3,
                       lower=0.1,
                       reg=reg_price,
                       newdata = data.frame(Natural_Gas=reg_dataset$Natural_Gas),
                       var_thresh = 0.05,
                       unhedged = reg_dataset$Net_revenue)


###--------------Overall portfolio of insurance products---------------------###

#Total Payout Portfolio
payout_portfolio <- cdd_index$Payout + sf_index$Payout + NG_index$Payout

#Total Premium  Portfolio
premium_portfolio <- cdd_index$premium + sf_index$premium + NG_index$premium

#Hedged Revenue Portfolio
hedged_net_revenues_portfolio <- reg_dataset$Net_revenue + payout_portfolio - premium_portfolio

###-------------Composite Index-------------------#
Composite_index <- match_var(upper=0.3,
                             lower=0.01,
                             reg=reg_composite,
                             newdata = data.frame(streamflow = reg_dataset$streamflow,
                                                  CDD=reg_dataset$CDD,
                                                  Natural_Gas=reg_dataset$Natural_Gas),
                             portfolio_var = var(hedged_net_revenues_portfolio),
                             unhedged = reg_dataset$Net_revenue)


#Composite Index Plots for the paper
plt_dataset <- data.frame(Year = rep(1:nrow(reg_dataset),2),
                          Revenue = c(reg_dataset$Net_revenue,Composite_index$hedged_revenue),
                          Type = rep(c("Unmanaged Revenues", "Managed Revenues"), each=499))

pdf("figures/Composite_Revenues.pdf",height=8, width=12)
ggplot(plt_dataset) +
  geom_line(aes(x=Year, y=Revenue, color = Type), size = 1.15) +
  geom_hline(yintercept = min(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1.05) +
  geom_hline(yintercept = min(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1.05) +
  scale_x_continuous(name = "Years", limits = c(70,170)) +
  scale_y_continuous(name = "Net Revenue ($B)", limits = c(11.4,12.75)) +
  geom_hline(yintercept = mean(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1) +
  geom_hline(yintercept = mean(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1) +
  geom_segment(aes(x = 75, y = min(reg_dataset$Net_revenue), 
                   xend = 75, yend = min(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.5, "cm")), 
               color ='blue', size = 1.15) +
  geom_segment(aes(x = 70, y = mean(reg_dataset$Net_revenue), 
                   xend = 70, yend = mean(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.15, "cm")), 
               color ='blue', size = 1.15) +
  scale_x_continuous(name = "Years", limits = c(70,170)) +
  geom_text(aes(x = 85, y = 11.6, 
                label = "Net Revenue \n Floor Improvement"), 
            size = 5) +
  geom_text(aes(x = 70, y = 12.32, label = "Mean"), size = 5) +
  scale_color_manual(values = c("Unmanaged Revenues" = "#000000", 
                                "Managed Revenues" = "#FF0000")) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.08),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))
dev.off()



pdf("figures/Composite_Revenues_all.pdf",height=8, width=12)
ggplot(plt_dataset) +
  geom_line(aes(x=Year, y=Revenue, color = Type)) +
  geom_hline(yintercept = min(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1.05) +
  geom_hline(yintercept = min(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1.05) +
  scale_x_continuous(name = "Years") +
  scale_y_continuous(name = "Net Revenue ($B)", limits = c(11.4,13)) +
  geom_hline(yintercept = mean(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1) +
  geom_hline(yintercept = mean(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1) +
  geom_segment(aes(x = 5, y = min(reg_dataset$Net_revenue), 
                   xend = 5, yend = min(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.5, "cm")), 
               color ='blue', size = 1.15) +
  geom_segment(aes(x = 0, y = mean(reg_dataset$Net_revenue), 
                   xend = 0, yend = mean(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.15, "cm")), 
               color ='blue', size = 1.15) +
  scale_x_continuous(name = "Years") +
  geom_text(aes(x = 30, y = 11.6, 
                label = "Net Revenue \n Floor Improvement"), 
            size = 5) +
  geom_text(aes(x = 0, y = 12.32, label = "Mean"), size = 5) +
  scale_color_manual(values = c("Unmanaged Revenues" = "#000000", 
                                "Managed Revenues" = "#FF0000")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))
dev.off()


#______________________________________________________________________#
###Additional Plots and Checks 
print(paste0("The Variance of Unmanaged net revenues is ", var(1000*reg_dataset$Net_revenue)))
print(paste0("The Variance of portfolio index is ", var(1000*hedged_net_revenues_portfolio)))
print(paste0("The Variance of composite index is ", var(1000*Composite_index$hedged_revenue)))


print(paste0("The 5th percentile of Unmanaged net revenues is ", 1000*quantile(reg_dataset$Net_revenue, 0.05)))
print(paste0("The 5th percentile of portfolio index is ", 1000*quantile(hedged_net_revenues_portfolio, 0.05)))
print(paste0("The 5th percentile  of composite index is ", 1000*quantile(Composite_index$hedged_revenue, 0.05)))

#Save the Datasets
revenues <- data.frame(Unmanaged = reg_dataset$Net_revenue,
                       Portfolio = hedged_net_revenues_portfolio,
                       Composite_index = Composite_index$hedged_revenue)
write.table(revenues, "sims/All_revenues_no_tax.csv", sep=",")

Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, Composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0
payouts <- data.frame(Unmanaged = Unmanaged_Losses,
                      Portfolio = payout_portfolio,
                      Composite_index = Composite_index$Payout,
                      Streamflow = sf_index$Payout,
                      CDD = cdd_index$Payout,
                      NG = NG_index$Payout)
write.table(payouts, "sims/All_payouts_no_tax.csv", sep=",")



#______________________________________________________________________________#
#Historgram of a subset of the dataset
Net_Revenue <- reg_dataset$Net_revenue
Hedged_Revenue <- hedged_net_revenues_portfolio
Composite_Revene <- Composite_index$hedged_revenue

#Unmanaged Losses
Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, Composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0


#Set-up the input dataset
plt_dataset <- data.frame(Unmanaged_Loss = -Unmanaged_Losses*1000,
                          Composite = Composite_index$Payout*1000,
                          Streamflow = sf_index$Payout*1000,
                          CDD = cdd_index$Payout*1000,
                          Natural_Gas = NG_index$Payout*1000)
plt_dataset <- t(plt_dataset)

#Subset to 20-yrs
ns <- sample(1:ncol(plt_dataset), 20)
ns <- c(222,45,413,188,494,2, 408, 9,  120, 49, 122, 107, 424,241, 94, 341, 229, 23, 244, 488) #Selected for illustrative purposes.
plt_dataset <- plt_dataset[,ns]
colnames(plt_dataset) <- c(1:20)

#Restructure the dataset
data <- data.frame(
  Year = rep(1:20, times = 5),
  Type = rep(c("Unmanaged Loss", "Composite Index Payout", "Streamflow Payout", "CDD Payout", "Natural Gas Payout"), each = 20),
  Value = c(plt_dataset[1,], plt_dataset[2,], plt_dataset[3,], plt_dataset[4,], plt_dataset[5,]))
data$Type <- factor(data$Type, levels = c("Composite Index Payout", "Streamflow Payout",
                                          "CDD Payout", "Natural Gas Payout", "Unmanaged Loss"))

# Set-up the base ggplot
p <- ggplot(data, aes(x = Year, y = Value)) +
  geom_bar(data = subset(data, Type != "Unmanaged Loss"), 
           aes(fill = Type), stat = "identity", position = "dodge") +
  geom_line(data = subset(data, Type == "Unmanaged Loss"), 
            aes(color = "Losses"), size = 1, linetype = "dashed") +
  geom_point(data = subset(data, Type == "Unmanaged Loss"), 
             aes(color = "Losses"), size = 2.5) +
  scale_fill_manual(values = c("Composite Index Payout" = "orange", 
                               "Streamflow Payout" = "brown", 
                               "CDD Payout" = "green", 
                               "Natural Gas Payout" = "black"),
                    name = "") +
  scale_color_manual(values = c("Losses" = "blue"), name = "") +
  labs(x = "Years", y = "($ Million)") +
  ylim(c(-300,300)) +
  theme_bw() +
  theme(legend.text = element_text(size = 22), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))

# For an even more visible dashed line
legend_b <- get_legend(p + 
                         guides(
                           fill = guide_legend(override.aes = list(size = 5), order = 1),
                           color = guide_legend(override.aes = list(linetype = "dashed", size = 5, shape = 16), 
                                                keywidth = unit(2, "cm"), order = 2)
                         ))

# Add vertical lines and remove legend from main plot
for(i in 1:21){ 
  p <- p + geom_segment(x = i-0.5, y = 0, xend = i-0.5, yend = 300, 
                        color = "black", linetype = 'dashed')
}
p <- p + theme(legend.position = "none")

pdf("figures/Payouts.pdf",height=7, width=14)
p + inset_element(legend_b, left = 0.05, bottom = 0.05, right = 0.3, top = 0.3)
dev.off()

