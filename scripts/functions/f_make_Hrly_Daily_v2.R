## Convert Raw CSV Logger Outputs to Hourly Daily and save to csv/RData
## PEEK 25-JAN-2015

# PACKAGES YOU NEED: ------------------------------------------------------

# library(dplyr)
# library(caTools)
# library(lubridate)
# library(caTools)
# library(ggplot2)

make.Hrly.Daily<-function(site,skip){
  
  skip=skip
  cat("formatting as solinst logger...\n\n")
  
  require(lubridate)
  require(tools)
  require(dplyr)
  
  
  ## choose a file, use the raw csv logger output
  
  setwd(paste(root,"/PROJECTS/Loggers/data",sep="")) # set to the folder you store raw files
  cat("\n", "on PC? Y or N","\n\n")
  tt<-scan(what="character",n=1)
  if(tt=="Y"){
    inputfile <- choose.files()
  } else {
    inputfile <- file.choose()
  }
  
  file<-read.csv(inputfile,stringsAsFactors=FALSE,skip=skip) # Header lines
  filename<-basename(file_path_sans_ext(inputfile))
  file$Datetime<-paste(file$Date," ",file$Time,sep="")
  print(head(file))
  
  cat("\n","Enter Datetime format: MDY or YMD?","\n\n") # prompt 
  z<-scan(what="character",n=1)
  cat("\n","Enter Datetime format: HMS or HM?","\n\n")
  y<-scan(what="character",n=1)
  
  if(z=="MDY"& y=="HMS"){
    file$Datetime<-mdy_hms(file$Datetime) # convert to POSIXct
    } else{
      if(z=="MDY" & y=="HM"){
        file$Datetime<-mdy_hm(file$Datetime) # convert to POSIXct
      } else {
        if(z=="YMD" & y=="HM"){
          file$Datetime<-ymd_hm(file$Datetime) # convert to POSIXct
        } else{
          file$Datetime<-ymd_hms(file$Datetime) # convert to POSIXct
        }
      }
    }
  
  cat("date converted \n")
  
  ## Potential col names
  loggercols<-c("Datetime","Level","Temperature","LEVEL","TEMPERATURE")
  df<-data.frame(file[, colnames(file) %in% loggercols]) ## Select columns
  df<-df[,c(3,1,2)] # reorder
  colnames(df)<-c("Datetime","Level","Temperature") # rename
  summary(df)
  
  site=site # use for data in site column
  
  df.m<-df ## The "minutes" (raw) dataframe

#   df$Datetime<-floor_date(df$Datetime, "hour") # round down to hr
  
  ## Add other columns for processing
  df$year<-year(df$Datetime)
  #df$WY<-wtr_yr(df$Datetime) # new, add wtr_yr
  df$mon<-month(df$Datetime)
  df$yday<-yday(df$Datetime)
  #df$wyd<-ave(wtr_yr(df$Datetime), wtr_yr(df$Datetime), FUN=seq_along(df$Datetime))
  df$hour<-hour(df$Datetime)
  df$site<-as.factor(site)
  
  ## Make Hourly dataset
  df.hr<- df %>%
    group_by(site,year, mon, yday, hour)%>%
    dplyr::summarize(
      "lev.avg"=mean(Level,na.rm=TRUE),
      "lev.min"=min(Level,na.rm=TRUE),
      "lev.max"=max(Level,na.rm=TRUE),
      "temp.avg"=mean(Temperature,na.rm=TRUE),
      "temp.min"=min(Temperature,na.rm=TRUE),
      "temp.max"=max(Temperature,na.rm=TRUE))%>%
  mutate("datetime"=ymd_hms(strptime(paste0(year,"-", mon,"-", yday, " ",
                         hour,":00"),format = "%Y-%m-%j %H:%M"))) %>%
  select(datetime,year,mon,yday,lev.avg:temp.max) %>%
  as.data.frame()
  
  # Plot the Hourly Temperature
  print(ggplot(data=df.hr,aes(datetime,temp.avg))+geom_line(col="maroon")+
  theme_bw()+ggtitle(paste0(site, ": Avg Hourly Water Temperature (C)")))
  
  Pause()
  
  # Plot the Hourly Stage
  print(ggplot(data=df.hr,aes(datetime,lev.avg))+geom_line(col="blue4")+
  theme_bw()+ggtitle(paste0(site, ": Avg Hourly Stage (m)")))
  
  s(df.hr)
  
  Pause()
    
  ## Make Daily dataset
  require(caTools)
  df.dy<-df %>%
    group_by(site, year, mon, yday)%>%
    dplyr::summarize("lev.avg"=mean(Level,na.rm=TRUE),
              "lev.min"=min(Level,na.rm=TRUE),
              "lev.max"=max(Level,na.rm=TRUE),
              "temp.avg"=mean(Temperature,na.rm=TRUE),
              "temp.min"=min(Temperature,na.rm=TRUE),
              "temp.max"=max(Temperature,na.rm=TRUE)) %>%
    mutate("lev.7.avg"= runmean(lev.avg, k=7, endrule="mean",align="center"),
           "lev.7.min"= runmin(lev.min, k=7, align="center"),
           "lev.7.max"= runmax(lev.max, k=7, align="center"),
           "temp.7.avg"= runmean(temp.avg, k=7, endrule="mean",align="center"),
           "temp.7.min"= runmin(temp.min, k=7, align="center"),
           "temp.7.max"= runmax(temp.max, k=7, align="center")) %>%
    mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", yday),
                                   format = "%Y-%m-%j"))) %>%
    select(datetime,year,mon,yday,lev.avg:temp.7.max) %>%  
    as.data.frame()
  
  s(df.dy)
  
  # Plot the Daily Temperature  
  print(ggplot()+
  geom_ribbon(data=df.dy, aes(datetime,ymax=temp.7.max,ymin=temp.7.min),
              fill="gray70",colour="gray60",lty=2,size=0.7,alpha=0.4)+
  geom_line(data=df.dy,aes(datetime,temp.7.avg), col="maroon",size=1)+
  theme_bw()+ggtitle(paste0(site, ": 7 Day Avg Water Temperature (C)")))

  Pause()

  # Make Water Year column from date column (assumes your column is already POSIXct or as.Date)
#   wtr_yr <- function(dates, start_month=10) {
#     # Convert dates into POSIXlt
#     dates.posix = as.POSIXlt(dates)
#     # Year offset
#     offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
#     # Water year
#     adj.year = dates.posix$year + 1900 + offset
#     # Return the water year
#     adj.year
#   }
  
  # WY<-wtr_yr(df$datetime) # use water year function to add water year (Oct 1 through Sep 30)
  
  # add water year day
  #df$wyd<-ave(df$WY, df$WY, FUN=seq_along) # break by water year, df must be ordered by date
  
  # setwd(choose.dir())
  # setwd(paste(root,"/PROJECTS/Loggers/data/processed/",sep=""))
  
  ## Set up names for files:
  
  # rdata.names<-c(paste0(site,".m"),paste0(site,".hr"),paste0(site,".dy"))
#   assign(paste0(site,".m"), df)
#   assign(paste0(site,".hr"),df.hr)
#   assign(paste0(site,".dy"),df.dy)
  
#   save(df.hr, df.dy, list=c("TUO.hr","TUO.dy"),
#        file = paste0(filename,".RData"))
  
  cat("\n","Save as RData File (y) or csv (n)?", "\n\n")
  reply<-scan(what="character",n=1)
  if(reply=="y"){
    ### Set directory for saving to csv
    cat("Using current working directory: \n")
    setwd(paste(root, "PROJECTS/Loggers/data/processed/sites",sep=""))
    cat(getwd())
    save(df.m, df.hr, df.dy, file = paste0(filename,".rda"))
    cat("\n\n Saving files to directory....")
    
    } else {
      ### Write Hourly
      write.csv(df.hr, file=paste(filename,"_hourly.csv",sep=""), row.names=FALSE)
      ### Write Daily
      write.csv(df.dy, file=paste(filename,"_daily.csv",sep=""), row.names=FALSE)
      cat("\n\n Saving files to directory \n", getwd(), "\n\n")
      setwd(paste(root, "PROJECTS/Loggers",sep=""))
    }

  setwd(paste(root, "PROJECTS/Loggers",sep=""))
  cat(" DONE!")
  rm(list=ls())

}


# END ---------------------------------------------------------------------
