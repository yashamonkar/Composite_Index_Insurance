#______________________________________________________________________#
###Code for implementing the composite index insurance###
###For the pollution tax scenario
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

#Load the functions
source("functions/get_corr_plot.R")

#______________________________________________________________________#
#Initial Data Wrangling
net_revenue <- read.csv("sims/Net_Revenue_Pollution_tax.csv", 
                        header = TRUE, sep=",")
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)
Pollution_Damages <- read.csv("sims/Net_Revenue_Pollution_tax.csv", 
                              header = TRUE, sep=",")


#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)

#______________________________________________________________________#
#Divide into training and testing. 
set.seed(123)
train_frac <- 0.8

#Set-up regression dataset
reg_dataset <- data.frame(streamflow = scale(log(streamflow)), 
                          CDD = scale(CDD),
                          Natural_Gas = scale(Yearly_gas$V1),
                          Net_revenue = net_revenue$Net_revenue)

#Separate into Testing and Training Datasets
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

pdf("figures/Corr_Plot_Pollution_Tax.pdf",height=15, width=15)
get_corr_plot(Net_revenue = c(train_dataset$Net_revenue,test_dataset$Net_revenue),
              streamflow = c(train_dataset$streamflow,test_dataset$streamflow),
              CDD = c(train_dataset$CDD,test_dataset$CDD),
              NG_Price = c(train_dataset$Natural_Gas,test_dataset$Natural_Gas),
              Fitted_Values = c(reg_composite$fitted.values, test_pred))
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
  
  #Output the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
  
}



#_____________________________________________________________________#

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


###-------------------Overall portfolio of insurance products-------------------------###

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


#______________________________________________________________________#
###Additional Plots and Checks
print(paste0("The Variance of Unmanaged net revenues is ", var(1000*reg_dataset$Net_revenue)))
print(paste0("The Variance of portfolio index is ", var(1000*hedged_net_revenues_portfolio)))
print(paste0("The Variance of composite index is ", var(1000*Composite_index$hedged_revenue)))


print(paste0("The 5th percentile of Unmanaged net revenues is ", 1000*quantile(reg_dataset$Net_revenue, 0.05)))
print(paste0("The 5th percentile of portfolio index is ", 1000*quantile(hedged_net_revenues_portfolio, 0.05)))
print(paste0("The 5th percentile  of composite index is ", 1000*quantile(Composite_index$hedged_revenue, 0.05)))


#______________________________________________________________________#
#Save the Datasets
revenues <- data.frame(Unmanaged = reg_dataset$Net_revenue,
                       Portfolio = hedged_net_revenues_portfolio,
                       Composite_index = Composite_index$hedged_revenue)
write.table(revenues, "sims/All_revenues_pollution_tax.csv", sep=",")