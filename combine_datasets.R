if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
setwd("C:/Users/Leslie Aleman/Documents/IBDAP-rice-yields-main/Brazil Rice")

library(tidyverse)
library(dplyr)
library(readxl)
library(glmnet)
library(reshape2)
library(ggplot2)
library(rpart)

# Import Brazilian rice production data
rice_yields <- read.csv("crop data/rice_yields_2020_2021.csv", header = TRUE)

# Import LST data

LST_data <- read.csv("LST data/LST_2020.csv", header = TRUE)
LST_oct <- LST_data[275:305, ]
LST_oct <- subset(LST_oct, select = -c(system.time_start))
LST_oct <- LST_oct+273.15 #convert to kelvin
LST_oct <- colMeans(LST_oct) #get avg

LST_nov <- LST_data[306:335, ]
LST_nov <- subset(LST_nov, select = -c(system.time_start))
LST_nov <- LST_nov+273.15 #convert to kelvin 
LST_nov <- colMeans(LST_nov) #get avg

LST_dec <- LST_data[336:366, ]
LST_dec <- subset(LST_dec, select = -c(system.time_start))
LST_dec <- LST_dec+273.15 #convert to kelvin
LST_dec <- colMeans(LST_dec) #get avg

LST_data <- read.csv("LST data/LST_2021.csv", header = TRUE)
LST_jan <- LST_data[1:31, ]
LST_jan <- subset(LST_jan, select = -c(system.time_start))
LST_jan <- LST_jan+273.15 #convert to kelvin
LST_jan <- colMeans(LST_jan) #get avg

LST_feb <- LST_data[32:59, ]
LST_feb <- subset(LST_feb, select = -c(system.time_start))
LST_feb <- LST_feb+273.15 #convert to kelvin
LST_feb <- colMeans(LST_feb) #get avg

LST_mar <- LST_data[60:90, ]
LST_mar <- subset(LST_mar, select = -c(system.time_start))
LST_mar <- LST_mar+273.15 #convert to kelvin
LST_mar <- colMeans(LST_mar) #get avg

LST_apr <- LST_data[91:120, ]
LST_apr <- subset(LST_apr, select = -c(system.time_start))
LST_apr <- LST_apr+273.15 #convert to kelvin
LST_apr <- colMeans(LST_apr) #get avg

LST_may <- LST_data[121:151, ]
LST_may <- subset(LST_may, select = -c(system.time_start))
LST_may <- LST_may+273.15 #convert to kelvin
LST_may <- colMeans(LST_may) #get avg

# Import soil moisture data

SM_data <- read.csv("SM data/surface_soil_moisture_anom_Monthly.csv", header = TRUE)
SM_data <- SM_data[58:65,] #only save data from oct-may
SM_oct = SM_data[1,2];SM_nov = SM_data[2,2];SM_dec = SM_data[3,2]
SM_jan = SM_data[4,2];SM_feb = SM_data[5,2];SM_mar = SM_data[6,2]
SM_apr = SM_data[7,2];SM_may = SM_data[8,2]

# Import evaporative stress index data
ESI_data <- read.csv("ESI data/ESI_12_Monthly.csv", header = TRUE)
ESI_data <- ESI_data[58:65,] #only save data from oct-may
ESI_oct = ESI_data[1,2];ESI_nov = ESI_data[2,2];ESI_dec = ESI_data[3,2]
ESI_jan = ESI_data[4,2];ESI_feb = ESI_data[5,2];ESI_mar = ESI_data[6,2]
ESI_apr = ESI_data[7,2];ESI_may = ESI_data[8,2]

# Import NDVI data
dataset <- read.csv("NDVI data/Brazil_NDVI_Rice_RS_and_SC_ONLY.csv", header = TRUE, row.names=NULL, skip = 14)
dim(dataset)
str(dataset)

# Brazil rice production target months: Oct,Nov,Dec,Jan,Feb,Mar,Apr,May

# Start data parsing on Oct/2020
dataset_oct <- dataset[949:952, ]
dataset_oct <- subset(dataset_oct, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_oct <- sapply(dataset_oct, as.numeric)
dataset_oct <- colMeans(dataset_oct) #finds the mean of each column
dataset_oct["LST"]<-LST_oct
dataset_oct["SM"]<-SM_oct
dataset_oct["ESI"]<-ESI_oct
dataset_oct$variable <- rownames(dataset_oct) #coerces into a list of doubles

# Rio Grande do Sul (RS) data ************************************************
RSdataset_tmp <- read.csv("GPM data/2020/RS_oct_2020.csv", header = TRUE)
RSdataset_tmp <- merge(dataset_oct, RSdataset_tmp)
# Sdataset <-rbind(Sdataset, Sdataset_tmp)
RSdataset <- RSdataset_tmp
RSdataset["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[2]), each = nrow(RSdataset))

# Santa Catarina (SC) data ****************************************************
SCdataset_tmp <- read.csv("GPM data/2020/SC_oct_2020.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_oct, SCdataset_tmp)
# Ldataset <-rbind(Ldataset, Ldataset_tmp)
SCdataset <- SCdataset_tmp
SCdataset["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[2]), each = nrow(SCdataset))

#%%%%%%%%%%%%%%%%%%%%%%%%%NOVEMBER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Start data parsing on Nov/2020
dataset_nov <- dataset[953:956, ]
dataset_nov <- subset(dataset_nov, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_nov <- sapply(dataset_nov, as.numeric)
dataset_nov <- colMeans(dataset_nov)
dataset_nov["LST"]<-LST_nov
dataset_nov["SM"]<-SM_nov
dataset_nov["ESI"]<-ESI_nov
dataset_nov$variable <- rownames(dataset_nov)

RSdataset_tmp <- read.csv("GPM data/2020/RS_nov_2020.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_nov, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[3]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2020/SC_nov_2020.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_nov, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[3]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%%%%DECEMBER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_dec <- dataset[957:960, ]
dataset_dec <- subset(dataset_dec, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_dec <- sapply(dataset_dec, as.numeric)
dataset_dec <- colMeans(dataset_dec)
dataset_dec["LST"]<-LST_dec
dataset_dec["SM"]<-SM_dec
dataset_dec["ESI"]<-ESI_dec
dataset_dec$variable <- rownames(dataset_dec)

RSdataset_tmp <- read.csv("GPM data/2020/RS_dec_2020.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_dec, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[4]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2020/SC_dec_2020.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_dec, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[4]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%JANUARY%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_jan <- dataset[961:964, ]
dataset_jan <- subset(dataset_jan, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_jan <- sapply(dataset_jan, as.numeric)
dataset_jan <- colMeans(dataset_jan)
dataset_jan["LST"]<-LST_jan
dataset_jan["SM"]<-SM_jan
dataset_jan["ESI"]<-ESI_jan
dataset_jan$variable <- rownames(dataset_jan)

RSdataset_tmp <- read.csv("GPM data/2021/RS_jan_2021.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_jan, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[5]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2021/SC_jan_2021.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_jan, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[5]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%%FEBRUARY%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_feb <- dataset[965:967, ]
dataset_feb <- subset(dataset_feb, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_feb <- sapply(dataset_feb, as.numeric)
dataset_feb <- colMeans(dataset_feb)
dataset_feb["LST"]<-LST_feb
dataset_feb["SM"]<-SM_feb
dataset_feb["ESI"]<-ESI_feb
dataset_feb$variable <- rownames(dataset_feb)

RSdataset_tmp <- read.csv("GPM data/2021/RS_feb_2021.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_feb, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[6]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2021/SC_feb_2021.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_feb, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[6]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%%%%%MARCH%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_mar <- dataset[968:971, ]
dataset_mar <- subset(dataset_mar, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_mar <- sapply(dataset_mar, as.numeric)
dataset_mar <- colMeans(dataset_mar)
dataset_mar["LST"]<-LST_mar
dataset_mar["SM"]<-SM_mar
dataset_mar["ESI"]<-ESI_mar
dataset_mar$variable <- rownames(dataset_mar)

RSdataset_tmp <- read.csv("GPM data/2021/RS_mar_2021.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_mar, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[7]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2021/SC_mar_2021.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_mar, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[7]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%%APRIL%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_apr <- dataset[972:975, ]
dataset_apr <- subset(dataset_apr, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_apr <- sapply(dataset_apr, as.numeric)
dataset_apr <- colMeans(dataset_apr)
dataset_apr["LST"]<-LST_apr
dataset_apr["SM"]<-SM_apr
dataset_apr["ESI"]<-ESI_apr
dataset_apr$variable <- rownames(dataset_apr)

RSdataset_tmp <- read.csv("GPM data/2021/RS_apr_2021.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_apr, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[8]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2021/SC_apr_2021.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_apr, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[8]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%MAY%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataset_may <- dataset[976:979, ]
dataset_may <- subset(dataset_may, select = -c(ORDINAL.DATE,START.DATE, END.DATE, SOURCE))
dataset_may <- sapply(dataset_may, as.numeric)
dataset_may <- colMeans(dataset_may)
dataset_may["LST"]<-LST_may
dataset_may["SM"]<-SM_may
dataset_may["ESI"]<-ESI_may
dataset_may$variable <- rownames(dataset_may)

RSdataset_tmp <- read.csv("GPM data/2021/RS_may_2021.csv", header = TRUE)
#Sdataset_tmp <- subset(Sdataset_tmp, select = -c(Precipitation..mm.hr.) )
RSdataset_tmp <- merge(dataset_may, RSdataset_tmp)
RSdataset_tmp["Rice yield"] <- rep(c(rice_yields$RS.1000.MT.[9]), each = nrow(RSdataset_tmp))
RSdataset <-rbind(RSdataset, RSdataset_tmp)

SCdataset_tmp <- read.csv("GPM data/2021/SC_may_2021.csv", header = TRUE)
#SCdataset_tmp <- subset(SCdataset_tmp, select = -c(Precipitation..mm.hr.) )
SCdataset_tmp <- merge(dataset_may, SCdataset_tmp)
SCdataset_tmp["Rice yield"] <- rep(c(rice_yields$SC.1000.MT.[9]), each = nrow(SCdataset_tmp))
SCdataset <-rbind(SCdataset, SCdataset_tmp)

write.csv(RSdataset,"Final datasets/final_RS_data_2020_2021.csv",row.names = FALSE)
write.csv(SCdataset,"Final datasets/final_SC_data_2020_2021.csv",row.names = FALSE)

final_dataset <-rbind(RSdataset,SCdataset)
write.csv(final_dataset,"Final datasets/final_combined_data_2020_2021.csv",row.names = FALSE)
