#______________________________________________________________________________#
#This script includes the PGE Financial Model -- No Tax Scenario -- Variable NG
#Output is the net revenue across the simulated CAPOW years. 
#Output also includes some model diagnostics. 

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")

#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(corrplot)


#Load Functions
source("functions/get_validation_plots.R")
source("functions/get_revenue_dependence.R")
source("functions/get_corr_plot.R")


#______________________________________________________________________________#
###Reading the input data###

#Read the Demand, Market Price and Qualifying Generators (500 Runs)
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
Qualifying_Generators <- read.csv("data/no_tax/qf_generation.csv", header = FALSE)

#Additional CAPOW outputs
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)

#Load Profile 
Load_profile <- read.csv("data/Load_Profile.csv", header = FALSE)

#Simulation abronmality
rm_sim <- 101 #Remove the 101 simulation run


#______________________________________________________________________________#
###Model Hyper-parameters###
N_years <- 500


#------------------------------------------------------------------------------#
###--------Capacity data--------------###
#PG&E specific values taken from filings

#Capacity associated with solar
Solar_PPA = 3395  
Solar_cap_in_region = 2777
PGE_owned_solar_capacity_MW = 152  

#Capacity associated with wind
Wind_PPA = 1780 
Wind_cap_in_region = 950

#Capacity associated with hydro
hydro_total = 5844  
owned_hydro = 2655
Hydro_fraction = owned_hydro / hydro_total
percent_hydro_ppa = 0.5 
percent_hydo_owned = 9.7
Hydro_PPA = owned_hydro * (percent_hydro_ppa / percent_hydo_owned)

# Nuclear
pge_nuclear_capacity_MW = 2323.0  
pge_nuclear_capacity_factor = 0.8950  
nuclear_fuel_costs = 7.45 # $/MWh.


#------------------------------------------------------------------------------#
###--------Cost data--------------###
#PG&E specifc values taken from filings

#PPA and Qualifying Generators Costs
PPA_cost_per_MWh = 106.2  
QF_cost_per_MWh = 33.1553 

#Delivery Costs
Delivery_charge = 0.046

#Seasonal Rates
Res_rate_summer = 0.2097163 #cents/kwhr
Res_rate_winter = 0.1578837 #cents/kwhr
Com_rate_summer = 0.1865078 #cents/kwhr
Com_rate_winter = 0.1388922 #cents/kwhr
Ind_rate = 0.1010 #cents/kwhr
Ag_rate_summer = 0.219343 #cents/kwhr
Ag_rate_winter = 0.174257 #cents/kwhr

#Percent of sectoral load met by PG&E
pct_now = c(0.811384629, 0.75790101, 0.850178588, 0.757734342)

#Model Generation Costs
Model_gen_cost = 0  #Old Hyper-parameters -- Useless

#Season Indicator
Summer = c(5, 6, 7, 8, 9, 10)
Winter = c(1, 2, 3, 4, 11, 12)

st_date <- as.POSIXct("01-01-2023 00:00", format="%m-%d-%Y %H:%M")
end_date <- as.POSIXct("12-31-2023 23:00", format="%m-%d-%Y %H:%M")
months <- seq(st_date, end_date, by ="hours")
months <- as.numeric(format(months, "%m"))


#______________________________________________________________________________#
#Run the financial model.

financial_results <- Sectoral_Revenue <- Total_Deliveries <-  list()

for(yr in 1:N_years) {
  print(yr)
  
  #Hours in the year
  hr <- 8712
  
  #----------------------------------------------------------------------#
  ####------Demand for the current run. --------###
  year_demand = Demand[(1+(yr-1)*8760):(yr*8760),]
  
  #PGE Delivery portion
  PGE_delivery_load <- year_demand*0.82
  
  
  #----------------------------------------------------------------------#
  ####------CAISO Price --------###
  Wholesale_price <- Price[,yr]
  average_wholesale_price = mean(Wholesale_price) #---Output from CAPOW
  max_yearly_wholesale_price = max(Wholesale_price) #---Output from CAPOW
  
  
  #----------------------------------------------------------------------#
  ####------Generation --------###
  #Generation Amount
  name <- paste0('data/no_tax/PGE_GEN_', yr-1, '.csv')
  PGE_gen <- read.csv(name, sep=",", header = FALSE)
  
  
  #Generation Cost
  name <- paste0('data/no_tax/PGE_GEN_cost_', yr-1, '.csv')
  PGE_gen_cost <- read.csv(name, sep=",", header = FALSE)
  
  
  #Thermal Generation
  Thermal_gen = PGE_gen[, 1:4] #First four rows are thermal plants
  Thermal_cost = PGE_gen_cost[,1:4]
  Total_Thermal_cost = Thermal_gen*Thermal_cost
  
  
  #Hydro
  name2 = 'data/CA_hydro_mins.csv'
  Min_hydro = read.csv(name2, sep=",", header = TRUE)
  Min_hydro = Min_hydro$PGE_valley
  PGE_hydro = (PGE_gen[, 6] + Min_hydro[1:hr]) * Hydro_fraction
  
  
  #Solar
  name3 = paste0("data/no_tax/PGE_Solar_",yr-1,".csv")
  PGE_solar = read.csv(name3, sep = ",", header = FALSE)
  PGE_owned_solar = PGE_solar * (PGE_owned_solar_capacity_MW / Solar_cap_in_region) 
  PGE_total_annual_solar = sum(PGE_solar)
  
  #Wind
  name4 = paste0("data/no_tax/PGE_Wind_",yr-1,".csv")
  PGE_wind = read.csv(name4, sep=",", header = FALSE)
  PGE_total_annual_wind = sum(PGE_wind)
  
  #Wind, Solar, Hydro PPA
  PGE_PPA_solar = PGE_solar * (Solar_PPA / Solar_cap_in_region)
  PGE_PPA_wind = PGE_wind * (Wind_PPA / Wind_cap_in_region)
  for(i in 1:nrow(PGE_PPA_solar)){
    if(PGE_PPA_solar[i,] > Solar_PPA) {PGE_PPA_solar[i,] > Solar_PPA}
    if(PGE_PPA_wind[i,] > Wind_PPA) {PGE_PPA_wind[i,] > Wind_PPA}
  }
  PGE_PPA_hydro = ((PGE_gen[, 6] + Min_hydro[1:hr]) * (Hydro_PPA / hydro_total))
  
  #Qualifying facilities
  PGE_QF = Qualifying_Generators[1:hr, yr]
  
  #Nuclear
  hours_in_year = 8760.0  # Change to d
  pge_nuclear_generation = pge_nuclear_capacity_MW * pge_nuclear_capacity_factor * hours_in_year
  PGE_nuc = rep(pge_nuclear_capacity_MW*pge_nuclear_capacity_factor, 8712)
  
  
  #------------------Total Generation Cost----------------------------------#
  #------Total Generation Cost = Thermal + Nuclear Cost------#
  Total_cost_of_generation = sum(Total_Thermal_cost) + pge_nuclear_generation * nuclear_fuel_costs
  hourly_cost_of_generation = rowSums(Total_Thermal_cost) + (pge_nuclear_generation * nuclear_fuel_costs)/hours_in_year
  
  #Total PPA and Qualifying Gens Cost
  Total_PPAs = PGE_PPA_hydro + PGE_PPA_solar[,1] + PGE_PPA_wind[,1]
  hourly_PPA_cost = Total_PPAs * PPA_cost_per_MWh + PGE_QF * QF_cost_per_MWh   
  Total_PPAs_cost = sum(Total_PPAs * PPA_cost_per_MWh) + sum(PGE_QF * QF_cost_per_MWh)
  
  #----------------------------------------------------------------------------------------#
  #--------------------PG&E Financials-----------------------------#
  # all of these (excluding ROE) are in units of billions of $
  # From the 2020 10-K report and their Presentation and Complete Earnings
  Rate_base = 29.5  
  ROE = 0.1025  
  OM_cost = 6.51594  
  Depreciation = 2.60217  
  
  
  #--------------------Sectoral Load----------------------------------------#
  Load_total = year_demand 
  Ag_load = Load_profile[, 1] / rowSums(Load_profile) * Load_total
  Com_load = Load_profile[, 2] / rowSums(Load_profile) * Load_total
  Ind_load = Load_profile[, 3] / rowSums(Load_profile) * Load_total
  Res_load = Load_profile[, 4] / rowSums(Load_profile) * Load_total
  
  #Load percentages met by PG&E by sector
  pct_list = pct_now
  Res_pct = pct_list[1]
  Ind_pct = pct_list[2]
  Com_pct = pct_list[3]
  Ag_pct = pct_list[4]
  
  #Sectoral Load in MWhr met by PG&E
  PGE_res_load = Res_load * Res_pct
  PGE_com_load = Com_load * Com_pct
  PGE_ind_load = Ind_load * Ind_pct
  PGE_ag_load = Ag_load * Ag_pct
  
  #PG&E Load Base
  Load_base = PGE_ag_load + PGE_com_load + PGE_ind_load + PGE_res_load
  
  
  #--------------------CAISO Exchange---------------------------------------#
  #Total Generation and revenue of selling selling total generation 
  Market_sales = rowSums(Thermal_gen) + PGE_hydro + PGE_nuc + PGE_PPA_hydro + PGE_owned_solar$V1 + PGE_PPA_solar$V1 + PGE_PPA_wind$V1 + PGE_QF
  Total_market_sales = sum(Market_sales) #Total Annual Generated Power
  Market_sell_revenue = Market_sales * Wholesale_price
  Total_market_sales_money = sum(Market_sell_revenue)
  
  
  #Toal Demand -- for 8712 hours and Cost of meeting total demand (across 4 sectors at wholesale price)
  Market_buy = Load_base[1:hr]
  Total_market_buy = sum(Market_buy) #Total Annual Purchased Power
  Market_buy_cost = Market_buy * Wholesale_price
  Total_market_buy_money = sum(Market_buy_cost)
  
  
  #Revenue from individual generation components
  Market_thermal = rowSums(Thermal_gen) * Wholesale_price
  Market_hydro = PGE_hydro * Wholesale_price
  annual_hydro = sum(PGE_hydro)  
  Market_nuc = PGE_nuc * Wholesale_price
  Market_PPA_hydro = PGE_PPA_hydro * Wholesale_price
  Market_PPA_solar = PGE_PPA_solar * Wholesale_price
  Market_PPA_wind = PGE_PPA_wind * Wholesale_price
  Market_QF = PGE_QF * Wholesale_price
  
  
  #Fraction use of Natural Gas
  fraction_NG_generation = mean(rowSums(Thermal_gen) / (rowSums(Thermal_gen) + PGE_hydro + PGE_nuc + PGE_owned_solar$V1))
  
  
  #CAISO Exchage (Total Hourly Sectoral Demand - Total Hourly Generation)
  CAISO_Exchange = Market_buy - Market_sales
  CAISO_money_exchange = CAISO_Exchange * - Wholesale_price
  Total_market_exchange = sum(CAISO_money_exchange)
  
  
  #--------------------Total PG&E Costs-------------------------------------#
  Total_cost = (Total_PPAs_cost + Total_cost_of_generation) - Total_market_exchange 
  Average_rate = Total_cost / (sum(Load_base) * 1000)
  pct_sale_to_CAISO = sum(CAISO_Exchange) / sum(Load_base)
  
  
  #------------------Revenue by Demand Sectors----------------------------#
  #Residential Rates
  Res_generation_rate_sum = Res_rate_summer 
  Res_generation_rate_win = Res_rate_winter 
  Res_gen_modifier_sum = Res_generation_rate_sum 
  Res_gen_modifier_Win = Res_generation_rate_win 
  
  #Commercial Rates
  Com_generation_rate_sum = Com_rate_summer 
  Com_generation_rate_win = Com_rate_winter 
  Com_gen_modifier_sum = Com_generation_rate_sum
  Com_gen_modifier_Win = Com_generation_rate_win 
  
  #Industrial Rates
  Ind_generation_rate = Ind_rate 
  Ind_gen_modifier_Win = Ind_generation_rate 
  
  #Agricultural Rates
  Ag_generation_rate_sum = Ag_rate_summer 
  Ag_generation_rate_win = Ag_rate_winter
  Ag_gen_modifier_sum = Ag_generation_rate_sum 
  Ag_gen_modifier_Win = Ag_generation_rate_win 
  
  # Store revenue results
  PGE_res_revenue = rep(NA, hr)
  PGE_com_revenue = rep(NA, hr)
  PGE_ind_revenue = rep(NA, hr)
  PGE_ag_revenue = rep(NA, hr)
  
  for(i in 1:hr){
    if(months[[i]] %in% Summer){
      PGE_res_revenue[i] = PGE_res_load[i] * (Model_gen_cost + Res_gen_modifier_sum) * 1000  # load is in MWh
      PGE_com_revenue[i] = PGE_com_load[i] * (Model_gen_cost + Com_gen_modifier_sum) * 1000
      PGE_ind_revenue[i] = PGE_ind_load[i] * (Model_gen_cost + Ind_gen_modifier_Win) * 1000
      PGE_ag_revenue[i] = PGE_ag_load[i] * (Model_gen_cost + Ag_gen_modifier_sum) * 1000
    } else {
      PGE_res_revenue[i] = PGE_res_load[i] * (Model_gen_cost + Res_gen_modifier_Win) * 1000
      PGE_com_revenue[i] = PGE_com_load[i] * (Model_gen_cost + Com_gen_modifier_Win) * 1000
      PGE_ind_revenue[i] = PGE_ind_load[i] * (Model_gen_cost + Ind_gen_modifier_Win) * 1000
      PGE_ag_revenue[i] = PGE_ag_load[i] * (Model_gen_cost + Ag_gen_modifier_Win) * 1000
    }
  }
  
  #Compute total revenue
  PGE_total_serving_revenue = PGE_res_revenue + PGE_com_revenue + PGE_ind_revenue + PGE_ag_revenue
  PGE_delivery_revenue = Load_total * Delivery_charge * 1000
  
  
  #----------------------------Net Revenue---------------------------------#
  #Daily Time Scale
  
  days_in_model_year = hr/24
  hourly_cost = hourly_PPA_cost + hourly_cost_of_generation - CAISO_money_exchange
  daily_net_revenue = rep(NA,days_in_model_year)
  for (i in 1:days_in_model_year) {
    daily_net_revenue[i] = (sum(PGE_total_serving_revenue[((i-1)*24+1):(i*24)]) + sum(PGE_delivery_revenue[((i-1)*24+1):(i*24)])) - OM_cost/365 - (Rate_base * ROE)/365 - sum(hourly_cost[((i-1)*24+1):(i*24)]) }
  
  #Annual Time Scale
  PGE_total_serving_revenue_total = sum(PGE_total_serving_revenue)
  PGE_delivery_revenue_total = sum(PGE_delivery_revenue)
  Total_revenue = sum(PGE_total_serving_revenue) + sum(PGE_delivery_revenue) - sum(hourly_cost)
  Total_revenue_in_Billion = Total_revenue / 1000000000
  Net_revenue = Total_revenue_in_Billion - (Total_cost / 1000000000)
  
  #-------------------------------------------------------------------------#
  ###Data Required for validation
  
  ###Sectoral Revenues
  Res_Revenue <- (sum(PGE_res_revenue))/10^6
  Com_Revenue <- (sum(PGE_com_revenue))/10^6
  Ind_Revenue <- (sum(PGE_ind_revenue))/10^6
  Ag_Revenue <- (sum(PGE_ag_revenue))/10^6
  Sectoral_Rev <- c(Res_Revenue, Com_Revenue, Ind_Revenue, Ag_Revenue)
  
  
  ###Total Deliveries
  Total_Deliveries[[yr]] <- sum(PGE_delivery_load)/1000 #MWhr
  
  
  #-------------------------------------------------------------------------#
  financial_results[[yr]] <- c(Net_revenue, average_wholesale_price)
  Sectoral_Revenue[[yr]] <- Sectoral_Rev
  
}



#_____________________________________________________________________________________#
#Convert to data frame and save
fin_results <- bind_cols(lapply(financial_results,data.frame))
fin_results <- data.frame(t(fin_results))
colnames(fin_results) = c("Net_Revenue", "Market_Price")

#Save the net-revenues & Market Price Information
plt_dataset <- data.frame(Market_Price = fin_results$Market_Price,
                          Net_revenue = fin_results$Net_Revenue)

#Remove the results of the abnormal simulation!
plt_dataset <- plt_dataset[-rm_sim,]

write.table(plt_dataset, "sims/Net_Revenue_no_tax.csv", sep=",")


#______________________________________________________________________________#
###Creating plots of correlations within the PG&E region.

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

#Data for plots for Sector Revenue Validation. 
Sectoral_Rev <- bind_cols(lapply(Sectoral_Revenue,data.frame))
Sectoral_Rev <- data.frame(t(Sectoral_Rev))
colnames(Sectoral_Rev) = c("Residential", "Commercial", "Industrial", "Agriculture")
Sectoral_Fractions <- 100*Sectoral_Rev/rowSums(Sectoral_Rev)

#Validation values for sectors and revenues
res_frac <- c(0.42, 0.42, 0.3986)
com_frac <- c(0.4, 0.4, 0.4)
ind_frac <- c(0.1268, 0.118, 0.112)
ag_frac <- c(0.1, 0.08, 0.09)
pge_deliveries <- c(79774, 82226, 83017)
pge_10_k_rev <- c(12.261, 12.083, 12.3)


#__________________________________________________________________________________#
#Final Plots for paper

#Validation
pdf("figures/Validation.pdf",height=7, width=14)
get_validation(Deliveries = unlist(Total_Deliveries)[-rm_sim],
               Net_revenue = fin_results$Net_Revenue[-rm_sim])
dev.off()


pdf("figures/Unmanaged.pdf",height=15, width=15)
get_revenue_dependence(Net_revenue = plt_dataset$Net_revenue,
                       streamflow = streamflow, 
                       CDD = CDD,
                       Market_Price = plt_dataset$Market_Price,
                       NG_Price = Yearly_gas$V1[-rm_sim])
dev.off()

