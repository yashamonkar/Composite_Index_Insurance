#______________________________________________________________________________#
###---------------Estimate the prevented pollution damages-------------------###

#The prevented pollution damages is the difference in the generation times the pollution costs
#Formula - (No_Tax_Production - Tax_Production)*Pollution Tax

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")


#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(corrplot)
library(ggplot2)


#Input the files
tax_prod <- read.csv("data/tax/production_df_SNP_variable_gas.csv", sep=",")
no_tax_prod <- read.csv("data/no_tax/production_df_no_tax_variable_gas.csv", sep=",")
generators <- read.csv("data/generators.csv", sep=",")

#Streamflow
streamflow <- read.csv("data/Streamflow.csv")
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

#CDD
CDD <- read.csv("data/CDD.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)

###Model Hyper-parameters###
N_years <- 500

#______________________________________________________________________________#
######------------CASE 1 - PG&E Owned--------------------------------------#####

#Tax on Thermal Plants owned by PG&E. Source Zeighami et al (2023) 
thermal_tax <- c(7.911343, 34.68308, 34.68308, 1.383465)

#Compute the prevented pollution damages
prevented_damages <- list()

for(yr in 1:N_years) {
  print(yr)
  
  
  #----------------------------------------------------------------------#
  ####------Generation -- No Tax --------###
  #Generation Amount
  name <- paste0('data/no_tax/PGE_GEN_', yr-1, '.csv')
  no_tax_gen <- read.csv(name, sep=",", header = FALSE)
  no_tax_gen <- no_tax_gen[, 1:4] #First four rows are thermal plants
  
  
  ####------Generation -- Pollution Tax --------###
  #Generation Amount
  name <- paste0('data/tax/PGE_GEN_', yr-1, '.csv')
  tax_gen <- read.csv(name, sep=",", header = FALSE)
  tax_gen <- tax_gen[, 1:4] #First four rows are thermal plants
  
  #Subract the difference
  reduced_gen <- no_tax_gen - tax_gen
  reduced_pollution <- reduced_gen * thermal_tax
  prevented_damages[[yr]] <- sum(reduced_pollution)/10^6

  
}
pge_owned_damages <- unlist(prevented_damages)

#______________________________________________________________________________#
######------------CASE 2 - PG&E Owned + Qualified Producers----------------#####
facility_names = c('CROKET_7_UNIT', 'CHEVCO_6_UNIT_2', 'TANHIL_6_SOLART', 'CHEVCO_6_UNIT1',
                   'CHEVCD_6_UNIT', 'FRITO_1_LAY', 'SRINTL_6_UNIT', 'DEXZEL_1_UNIT', 
                   'BASICE_2_UNITS', 'DISCOV_1_CHEVRN', 'GRNLF1_1_UNITS',
                  'GRNLF2_1_UNIT', 'KINGCO_1_KINGBR', 'UNOCAL_1_UNITS', 'YUBACT_1_SUNSWT')

qf_facilities <-  generators[match(facility_names, generators$name),]
qf_tax <- qf_facilities$NOXTax...MWh. + qf_facilities$SO2Tax...MWh. + qf_facilities$PMTax...MWh.


#Subset the production values
tax_prod_qf <- tax_prod[,match(qf_facilities$name, colnames(tax_prod))]
no_tax_prod_qf <- no_tax_prod[,match(qf_facilities$name, colnames(tax_prod))]
prevented_damages <- tax_prod_qf

for(i in 1:ncol(tax_prod_qf)){
  prevented_damages[,i] <-  (no_tax_prod_qf[,i] - tax_prod_qf[,i])*qf_tax[i]
}
prevented_damages <- data.frame(Damages = rowSums(prevented_damages)/10^6,
                                Year = rep(1:500, each = 364))
prevented_damages <- prevented_damages %>% group_by(Year) %>% summarise(Damages = sum(Damages))

total_pge_damages <- pge_owned_damages + prevented_damages$Damages


#______________________________________________________________________________#
######------------CASE 3 - CAISO DAMAGES----------------#####
facilities <-  generators[match(generators$name, colnames(tax_prod)),]
facilities <- head(facilities,-1)

qf_tax <- facilities$NOXTax...MWh. + facilities$SO2Tax...MWh. + facilities$PMTax...MWh.


#Subset the production values
tax_prod_qf <- tax_prod[,match(facilities$name, colnames(tax_prod))]
no_tax_prod_qf <- no_tax_prod[,match(facilities$name, colnames(no_tax_prod))]
prevented_damages <- tax_prod_qf

for(i in 1:ncol(tax_prod_qf)){
  prevented_damages[,i] <-  (no_tax_prod_qf[,i] - tax_prod_qf[,i])*qf_tax[i]
}
prevented_damages <- data.frame(Damages = rowSums(prevented_damages)/10^6,
                                Year = rep(1:500, each = 364))
prevented_damages <- prevented_damages %>% group_by(Year) %>% summarise(Damages = sum(Damages))

CAISO_damages <- prevented_damages$Damages


#Save the values
prevented_damages <- data.frame(PGE_Owned = pge_owned_damages,
                                PGE_Total = total_pge_damages,
                                CAISO = CAISO_damages)
write.table(prevented_damages, "sims/Prevented_Pollution_Damages.csv", sep=",")



#______________________________________________________________________________#
#Generate the prevented damages plots

#Streamflow versus CAISO Damages
plt_dataset <- data.frame(Damages = CAISO_damages, 
                          Streamflow = streamflow,
                          CDD = CDD)

pdf("figures/Pollution_Damages.pdf", 
    height=7, width=9)

ggplot(plt_dataset) +
  geom_point(aes(x=Streamflow, y = Damages, color = CDD), size = 1.75) +
  scale_color_gradient2(midpoint=mean(plt_dataset$CDD),
                        low="blue", mid="yellow",high="red") +
  ylab("Prevented Pollution Damages ($ Millions)") +  
  xlab("Annual Average Streamflow (cfs)") +
  labs(color = "Annual CDD") + 
  xlim(c(0,32000))+
  ylim(c(600,750))+
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99),
        legend.box.just = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_colourbar(title.hjust = 0.5,
                                 barwidth = unit(0.3, "in"), barheight = unit(4, "in")))

dev.off()
