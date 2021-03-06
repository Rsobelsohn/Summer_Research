library(plyr)
library(lubridate)
setwd("c:\\Users\\Rob\\Desktop\\NDVI_Research\\net-R\\netR\\NDVI_Summer")
datSDC <- read.csv("Dav_canopy.csv")
datSDC5 <- read.csv("Dav_canopy_2015.csv")
datSDU <- read.csv ("Dav_understory_veg.csv")
datSLC <- read.csv("LDF2_canopy.csv")
datSLC5 <- read.csv("LDF2_canopy_2015.csv")
datSLU <- read.csv("LDF2_understory_veg.csv")
################ A 5 means 2015 data ##################

################################################################################
###Make sure to keep the Time Stamp consistent for merging later################
###I tried excluding the first column in colnames but kept recieving an error###
###Two columns for timestamp in SLU, get rid of the non-corrected one###########
################################################################################

colnames(datSDC)[1:14] <- paste0(colnames(datSDC)[1:14],"SDC")
names(datSDC)[1] <- "TimeStamp"
colnames(datSDU)[1:12] <- paste0(colnames(datSDU)[1:12],"SDU")
names(datSDU)[1] <- "TimeStamp"
colnames(datSLC)[1:14] <- paste0(colnames(datSLC)[1:14],"SLC")
names(datSLC)[1] <- "TimeStamp"
colnames(datSLU)[1:9] <- paste0(colnames(datSLU)[1:9],"SLU")
datSLU <- datSLU[,-2]
names(datSLU)[1] <- "TimeStamp"

yday
####DATES ALREADY CORRECTED####
Merge1 <- merge (datSLU,datSLC, by= "TimeStamp", all=T)
Merge2 <- merge(datSDC,datSDU,by="TimeStamp",all=T)
Mergedat <- merge(Merge1,Merge2, by="TimeStamp", all=T)

################################################################################
#Converting TimeStamp into a POSIX?
#TimeStamp <- as.POSIXct(unlist(TimeStamp), format = "%y-%m-%d H")
#TimeStamp <- as.POSIXct(TimeStamp)
#as.POSIXlt(strptime(as.character(Mergedat[colnames(TimeStamp) %in% "timestamp"]), 
#                    format = "%Y-%m-%d %H"),
#           format = "%Y-%m-%d %H")


#Error "origin must be supplied"
#yday(252, Mergedat$TimeStamp)

Dates <- format(as.POSIXct(strptime
                           (Mergedat$TimeStamp,"%m/%d/%y %H:%M",tz="")) ,
                            format = "%m/%d/%y")
Hours <- format(as.POSIXct(strptime
                           (Mergedat$TimeStamp,"%m/%d/%y %H:%M",tz="est")) ,
                            format = "%H:%M")
Mergedat$Dates <- Dates
Mergedat$Hours <- Hours

doy <- format(as.POSIXct(strptime(Mergedat$Dates, "%m/%d/%y", tz="")) , format = "%j")
Mergedat$doy <- doy

#################################################################
### 2017 data is not displaying in the Dates and Hours Column ###
### Why will it not display? ####################################
#################################################################


#x <- as.Date(Mergedat$TimeStamp,"%m/%d/%Y")
#Mergedat$Date_V1 = x
#year(x)
#month(x)

#doy <- strftime("TimeStamp", format = "%j")
#as.numeric (doy)

#w <- as.Date(Mergedat$TimeStamp,"%m")
#x <- as.Date(Mergedat$TimeStamp, "%d")
#y <- as.Date(Mergedat$TimeStamp, "%Y")
#z <- as.Date(Mergedat$TimeStamp, "%H")
#Mergedat$Month = w
#Mergedat$Day = x
#Mergedat$Year = y
#Mergedat$Hour = z


#Mergedat$TimeStamp <- as.Date(TimeStamp)
#Mergedat$Time <- as.POSIXlt(x,sep= ".",format="%Y.%M.%j.")

as.POSIXct.date(Mergedat[1])
##How would I display TimeStamp as year.month.day.hour##
##since I didn't join columns?##
yday(Mergedat)



a=mdy(Dates)
year(a)

##Pull day of year from the Dates Column## 

yday(Mergedat$Dates)
doy <- strftime(Dates, format = "%j")
doy

#doy <- strftime(Mergedat$TimeStamp, format = "%j")
#Mergedat$doy=Mergedat


#################################################################
##All data is now in one environment tied together by TimeStamp##
##Still need to correct NDVI of upper canopies###################
###### Correct the data for upper canopy ########################
##Calculate new NDVI using NDVI= (NIR-R)/(NIR+R)#################
## NDVI as (alpha*800nm-630nm)/(alpha*800nm+630nm) ##############
## Need to correct LDF2_canopy and DAV_canopy_2015###############
#################################################################
Mergedat$ndvi.1SLC <- (Mergedat$up.alphaSDC*Mergedat$down1.800SLC-
                             Mergedat$down1.630SLC)/
  (Mergedat$up.alphaSDC*Mergedat$down1.800SLC+Mergedat$down1.630SLC)
Mergedat$Ndvi.2SLC <- (Mergedat$up.alphaSDC*Mergedat$down2.800SLC-
                             Mergedat$down2.630SLC)/
  (Mergedat$up.alphaSDC*Mergedat$down2.800SLC+Mergedat$down2.630SLC)

##Setting Conditions##
##Eliminate NDVI less than 0 and greater than 1##
Mergedat_NEWSDC <- ifelse(Mergedat$ndvi.1SDC<0|Mergedat$ndvi.1SDC>1, NA,
                          Mergedat$ndvi.1SDC)
Mergedat_NEWSDU <- ifelse(Mergedat$ndvi.1SDU<0|Mergedat$ndvi.1SDU>1, NA,
                          Mergedat$ndvi.1SDU)
Mergedat_NEWSLC <- ifelse(Mergedat$ndvi.1SLC<0|Mergedat$ndvi.1SLC>1, NA,
                          Mergedat$ndvi.1SLC)
Mergedat_NEWSLU <- ifelse(Mergedat$ndvi.1SLU<0|Mergedat$ndvi.1SLU>1, NA,
                          Mergedat$ndvi.1SLU)

###########################
##Daily Mean based on doy##
##Calculate mean per day###
###########################
AveNDVISDC <- aggregate(Mergedat_NEWSDC, by=list
                        (Mergedat_NEWSDC$TimeStamp), FUN="mean", 
                        na.action=na.omit, na.rm=TRUE)


