#--------------------------------------------------------------------#
###CODE TO GET BEST STRIKE VALUE FOR A TIME SERIES

###INPUT
###Input
#1. Upper Bound of Search upper = 0.3
#2. Lower Bound of Search. lower = 0.01
#4. Index Regression regression <- reg_streamflow
#5. Value at Risk Threshold (var_thresh) var_thresh = 0.05

###Output
#1. Hedged Revenues
#2. Payout
#2. Strike Value in percentile
#3. Premium Costs 


get_strike <- function(lower, upper, regression,var_thresh){
  
  #Get the grid of the strike
  strike_seq <- seq(from = lower, to = upper, length = 100) #These are percentiles
  strike <- quantile(regression$model[,2], strike_seq)
  
  #Compute the payout
  payout <- matrix(NA, nrow = length(regression$fitted.values), ncol = length(strike))
  
  if(upper < 0.5) {
  for(i in 1:length(strike)){
    payout[,i] <- (strike[i] - regression$model[,2])*regression$coefficients[2]}
  payout[payout < 0] = 0
  } else {
    for(i in 1:length(strike)){
      payout[,i] <- (regression$model[,2]- strike[i])*regression$coefficients[2]}
    payout[payout < 0] = 0
  }
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = regression$model[,1] + payout[,i] - braun_premium[i]
  }
  
  #%VAR of Hedged Revenues
  revenue_var <- apply(hedged_revenues,2,function(x) quantile(x, probs=var_thresh))
  val_indx <- which.max(revenue_var)
    
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
}