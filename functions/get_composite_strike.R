#____________________________________________________________________________#
#Function to optimize the composite index such that the variance matches the portfolio of contracts.


###Input
#1. Variance Needed.  needed_val <- var(hedged_net_revenues_portfolio)
#2. Upper Bound of Search upper = 0.3
#3. Lower Bound of Search. lower = 0.01
#4. Composite Index Regression composite_regression <- reg_composite
#5. methods (same_var, same_thresh, same_min) method = "same_var"
#6. Threshold Quantile (Optional) thresh = 0.05



###Output
#1. Hedged Revenues
#2. Strike Value in percentile
#3. Premium Costs 


get_composite_strike <- function(lower, upper, composite_regression, portfolio_rev, 
                                 method, thresh){
  
  #Get the grid of the strike
  strike_seq <- seq(from = lower, to = upper, length = 100) #These are percentiles
  strike <- quantile(composite_regression$fitted.values, strike_seq)
  
  #Compute the payout
  payout <- matrix(NA, nrow = length(composite_regression$fitted.values), ncol = length(strike))
  for(i in 1:length(strike)){
    payout[,i] <- strike[i] - composite_regression$fitted.values
  }
  payout[payout < 0] = 0
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = composite_regression$model$Net_revenue + payout[,i] - braun_premium[i]
  }
  
  if(method == "same_var") { 
    #Same Variance as portfolio of contracts
    needed_val <- var(portfolio_rev)
    revenue_val <- apply(hedged_revenues,2,var)
    val_indx <- which.min(abs(revenue_val - needed_val)) 
    print("Method is same variance")
  } else if(method == "same_thresh") { 
    #Same VAR as portfolio of contracts
    needed_val = quantile(portfolio_rev, thresh)
    revenue_val <- apply(hedged_revenues,2,function(x) quantile(x, probs=thresh))
    val_indx <- which.min(abs(revenue_val - needed_val))
    print("Method is same Value at Risk")
  } else { 
    #Same minimum as portfolio of contracts.
    needed_val = min(portfolio_rev)
    revenue_val <- apply(hedged_revenues,2,min)
    val_indx <- which.min(abs(revenue_val - needed_val))
    print("Method is same minimum")
    
  }
  
  
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx],
                      payout = payout[,val_indx]) 
  
  return(return_list)
}