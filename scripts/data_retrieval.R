# Data retrieval

library(dataRetrieval) # see vignette https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.pdf


# GET SOME INFO ON RIVERS WITH USGS STATIONS ------------------------------

# USGS river info for North Yuba and NF American
readNWISsite(siteNumbers = c("11427000", "11413000")) # basic station info

head(whatNWISdata(siteNumbers = c("11427000", "11413000"))) # what data is available for these sites?


# Get the data of interest
siteNumber <- "11264500" # use Tuolumne above grand canyon/HH: 11274790, happy isles: 11264500
parameterCd <- c("00010","00060") # Temperature and discharge
statCd <- c("00001", "00003") # mean and max  
startDate <- "2006-10-01"
endDate <- "2016-04-01"
temperatureAndFlow <- readNWISdv(siteNumber, parameterCd,
                                 startDate, endDate, statCd=statCd)

head(temperatureAndFlow) # see first few rows
# head(renameNWISColumns(temperatureAndFlow))
statInfo <- attr(temperatureAndFlow, "statisticInfo")
variableInfo <- attr(temperatureAndFlow, "variableInfo")
siteInfo <- attr(temperatureAndFlow, "siteInfo")



# use DPLYR to aggregate and rename
library(dplyr)
df.HI<-temperatureAndFlow %>% 
  rename(wtemp_max_cd=X_LEFT.BANK.WATER.TEMP_00010_00001_cd,
         wtemp_max=X_LEFT.BANK.WATER.TEMP_00010_00001,
         flow_cfs=X_00060_00003,
         flow_cfs_cd=X_00060_00003_cd)

names(df.HI)


# quick plot
par(mar=c(5,5,5,5)) #sets the size of the plot window
plot(df.HI$Date, df.HI$wtemp_max,
     ylab=variableInfo$parameter_desc[1],xlab="" )
par(new=TRUE)
plot(df.HI$Date, df.HI$flow_cfs,
     col="red",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE
)
axis(4,col="red",col.axis="red")
mtext(variableInfo$parameter_desc[2],side=4,line=3,col="red")
title(paste(siteInfo$station_nm,"2006-2016"))
legend("topleft", variableInfo$param_units,
       col=c("black","red"),lty=c(NA,1),pch=c(1,NA))



# Great but what about 15 min data?

source("scripts/functions/f_USGS_15min.R")

# need to do this in about 5-6 yr chunks...too big otherwise
get.USGS(river = "Merced", gage = "11264500", sdate="2009-10-01") # get 15 min data

Merced15<-Merced_15 # rename so we can get the other chunk

get.USGS(river = "Merced", gage = "11264500", sdate="2006-10-01", edate = "2009-10-01") # get 15 min data

# lets MERGE DATA now using datetime

dfALL<-rbind(Merced_15, Merced15) # include all records
# dfALL<-merge(Merced_15, Merced15, by="datetime", all.x=T) # include all records
h(dfALL)

# redo the plot
ggplot(data=dfALL,aes(datetime,flow_cms))+geom_line(col="maroon")+
  xlab("") + ylab("Flow (cms)") + theme_bw()+
  ggtitle(paste0("Merced at Happy Isles (USGS: 11264500): Discharge (cms)"))


