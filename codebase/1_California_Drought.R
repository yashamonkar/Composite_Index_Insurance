#______________________________________________________________________#
#Script includes code to create a plot of the spatial extent of drought 
#in California along with a time-series of hydropower and NG gen. 

#Point towards the working directory. 
setwd("C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance")

library(ggplot2)
library(cowplot)

#______________________________________________________________________#
#DO.D4 --> abnormally dry
#D1.D4 --> moderate
#D2.D4 --> severe
#D3.D4 --> extreme
#D4 --> Exceptional

#Plot 1 - Extent of Drought
drought <- read.csv("data/Data Tables  U.S. Drought Monitor.csv", sep=",",
                    header = TRUE)
drought$Week <- as.Date(as.character(drought$Week))

drought <- drought[drought$Week > as.Date("31/12/2009", format = "%d/%m/%Y"),]


plt_dataset <- data.frame(Date = rep(drought$Week, 5),
                          Drought = c(drought$D0.D4, drought$D1.D4, drought$D2.D4, drought$D3.D4, drought$D4),
                          Type = rep(c("abnormally dry", "moderate", "severe", "extreme", "exceptional"), each = nrow(drought)))




plt_dataset$Type <- factor(plt_dataset$Type, 
                                levels = c("abnormally dry", "moderate", "severe", "extreme", "exceptional"))


p1 <- ggplot(plt_dataset, aes(x = Date, y = Drought, fill = Type)) +
  geom_area(position = "identity") +
  scale_fill_manual(values = c("abnormally dry" = "yellow", 
                               "moderate" = "orange", 
                               "severe" = "red", 
                               "extreme" = "brown", 
                               "exceptional" = "black")) +
  xlab("") + 
  xlim(c(as.Date("01/01/2010", format = "%d/%m/%Y"), as.Date("31/12/2022", format = "%d/%m/%Y"))) +
  ylab("California in Drought \n (% of Land Mass)") +
  theme_bw() +
  theme(legend.position = c(0.05, 0.95), # c(x, y) where x is from 0 (left) to 1 (right), y is from 0 (bottom) to 1 (top)
        legend.justification = c(0, 1),
        legend.text = element_text(size = 15), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))


#______________________________________________________________________#
#Plot 2 - Natural Gas and Hydropower Generation. 
generation <- read.csv("data/Historic_NG_Hydro_Generation.csv", sep=",",
                       skip = 4, header =TRUE)
colnames(generation) <- c("Month", "Natural_Gas", "Hydro")

generation[,1] <- as.Date(paste0("01-", generation[,1]), format="%d-%b-%y")


#Set-up the plotting dataset 
plt_dataset <- data.frame(Month = rep(generation$Month,2),
                          Generation = c(generation$Natural_Gas, generation$Hydro),
                          Type = rep(c("Natural Gas", "Hydropower"), each = nrow(generation)))


p2 <- ggplot(plt_dataset) +
  geom_line(aes(x=Month, y = Generation, color = Type), size=1.) +
  xlab("Time") +
  ylab(paste0("California Electricity Generation \n (million MWh)")) +
  xlim(c(as.Date("01/01/2010", format = "%d/%m/%Y"), as.Date("31/12/2022", format = "%d/%m/%Y"))) +
  theme_bw() +
  theme(legend.position = c(0.05, 0.95), # c(x, y) where x is from 0 (left) to 1 (right), y is from 0 (bottom) to 1 (top)
        legend.justification = c(0, 1),
        legend.text = element_text(size = 15), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18)) +
  scale_color_manual(values = c("Hydropower" = "blue", "Natural Gas" = "black"))
  

pdf("figures/CA_Drought.pdf",height=7, width=18)
plot_grid(p1,p2,
          nrow =2)
dev.off()
