if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
setwd("C:/Users/Leslie Aleman/Documents/IBDAP-rice-yields-main/Brazil Rice")
library(ggplot2)

# Import final dataset
data <- read.csv("Final datasets/final_combined_data_2020_2021.csv", header = TRUE)
summary(data)
attach(data)
dim(data)

#prec data
ggplot(data, aes(x=prec)) +
  geom_density(color="darkblue", fill="lightblue") +
  ggtitle("Density plot of precipitation values (mm/h)") +
  theme(plot.title = element_text(hjust = 0.5))

#NDVI
hist(SAMPLE.VALUE,main = "Histogram of sample NDVI values",xlab="NDVI")

#LST
hist(LST,main = "Histogram of land surface temperature (K)",xlab="LST (K)")

#Surface moisture
hist(SM,main = "Histogram of soil moisture anomaly values",xlab="Soil moisture anomaly")

#ESI
#Surface moisture
hist(ESI,main = "Histogram of ESI values",xlab="ESI")
