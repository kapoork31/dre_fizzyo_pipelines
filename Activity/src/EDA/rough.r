    sessionMetaData <- tbl(conn, heart_rate_vals_2020_618_clean) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,userid,time) %>%
        group_by(date, userid) %>%
        collect()
        
        
    sessionMetaData <- tbl(conn, kRawHRTableName) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,userid,time) %>%
        group_by(date, userid) %>%
        summarize()%>%
        collect()

conn = xap.conn

startDate <- "2019-01-10"
endDate <- "2019-01-11"

dataHRRaw <- tbl(conn, 'heart_rate_vals_2020_618') %>%
    filter(userid == 'cf518ed4-ed66-449d-a8ca-14cee26e80a9' &
    time >= startDate &
    time <= endDate) %>%
    arrange(time)%>%
    collect()
write.csv(dataHRRaw,dir,row.names = FALSE)

conn = xap.conn
startDate <- "2019-01-10"
endDate <- "2019-01-11"
dataHRclean <- tbl(conn, 'heart_rate_vals_2020_618_clean') %>%
    filter(userid == 'cf518ed4-ed66-449d-a8ca-14cee26e80a9' &
    time >= startDate &
    time <= endDate) %>%
    arrange(time)%>%
    collect()

write.csv(rawFitbitData,dir,row.names = FALSE)



plot(dataHRRaw$time,dataHRRaw$value)
plot(dataHRClean$time,dataHRClean$value)


cleanedData = removeRandomPoints(data)
cleanedData = removeOutlierPoints(cleanedData)
cleanedData = remove70(cleanedData)
cleanedData = removeRepPointsdata(cleanedData)


library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)

conn = xap.conn


dataHRRaw <- tbl(conn, 'heart_rate_vals_2020_618_clean') %>%
    filter(userid == 'cf518ed4-ed66-449d-a8ca-14cee26e80a9' &
    time >= '2019-01-10' &
    time <= 2019-01-20) %>%
    arrange(time)%>%
    collect()

raw <- tbl(conn, 'hr_new') %>%
    filter(value >=60 & value <= 80 & time < '2018-11-09')%>%
    group_by(value)%>%
    tally()%>%
    #arrange(time)%>%
    collect()
    
cleanOrig <- tbl(conn, 'hr_clean_date_april_2020') %>%
    filter(value >=60 & value <= 80 & time < '2018-11-09')%>%
    group_by(value)%>%
    tally()%>%
    #arrange(time)%>%
    collect()


cleanNew <- tbl(conn, 'hr_clean_date_april_2020_new_remove70s') %>%
    filter(value >=60 & value <= 80 & time < '2018-11-09')%>%
    group_by(value)%>%
    tally()%>%
    #arrange(time)%>%
    collect()
    
cleanNew_1 <- tbl(conn, 'hr_clean_date_april_2020_new_remove70s_1') %>%
    filter(value >=60 & value <= 80 & time < '2018-11-09')%>%
    group_by(value)%>%
    tally()%>%
    #arrange(time)%>%
    collect()   
    
plot(raw$value,raw$n, main = 'raw')
plot(cleanOrig$value,cleanOrig$n, main = 'previous clean')
plot(cleanNew_1$value,cleanNew_1$n, main = 'new clean_1')
plot(cleanNew_1$value,cleanNew_1$n, main = 'new clean_1')


raw = xap.read_table('raw_hr_counts_freq')
clean = xap.read_table('clean_hr_counts_freq')
plot(raw$value,raw$count)
plot(clean$value,clean$count)

raw = c()
clean =  c()
clean_new =  c()



remove70 <- function(data){ # legacy
  seventy = which(data$value == 70)
  if( length(seventy) > 0){
    remove70s = c()
    f = 0
    c = 0
    d = as.Date(data$time[1])
    cutoff = paste(d, '22:00:00', sep = ' ')
    cutoff = as.POSIXct(cutoff, format = "%Y-%m-%d %H:%M:%S") # convert time column to time variable
    for ( i in seventy){
      t = data$time[i]
      if(t < cutoff){
        if(i == 1){
          f = i
          j = i + 1
          val1 = data$value[i]
          val2 = data$value[j]
          val0 = data$value[i-1]
          while(propDiff(val2,val1) < 10 & (j<= nrow(data) -1 )){
            j = j + 1
            val2 = data$value[j]
            
          }
          c = j - 1
          remove70s = c(remove70s,f:c)
        }
        else if(i > 1 & i < nrow(data)){
          f = i
          j = i + 1
          val1 = data$value[i]
          val2 = data$value[j]
          val0 = data$value[i-1]
          while((propDiff(val2,val1) < 10) & (j<= nrow(data) -1 ) ){
            j = j + 1
            val2 = data$value[j]
            
          }
          c = j - 1
          if(propDiff(val1,val0) > 10){
            remove70s = c(remove70s,f:c)
          }
        }
        else if(i == nrow(data)){
          val1 = data$value[i]
          val0 = data$value[i-1]
          if(propDiff(val1,val0) > 10){
            remove70s = c(remove70s,i)
          }
        }
      }
      
    }
    
    res = dropIndexes(data,remove70s)
    return (res)
  }else{
    return(data)
  }
  
}


remove70Leniant <- function(data){ # legacy
    seventy = which(data$value == 70)
    seventy = sort(seventy)
    if(length(seventy) > 1){
        remove70s = c()
        if( 1 %in% seventy){
            remove70s = c(remove70s,1)
        }
        if(nrow(data) %in% seventy){
            remove70s = c(remove70s,nrow(data))
        }
        if(length(seventy) > 1){
            curRem = c()
            rep = 0
            for ( i in 1:(length(seventy)-1)){
                cur = seventy[i]
                nex = seventy[i + 1]
                if( nex == (cur + 1)){
                    #rep = rep + 1
                    curRem = c(curRem,cur,nex)
                    curRem = unique(curRem)
                }else{
                    remove70s = c(remove70s,curRem)
                    curRem = c()
                    #rep = 0
                }
                
            }
        }
        
        remove70s = unique(remove70s)
        res = dropIndexes(data,remove70s)
        return (res)
    
    }else{
        return(data)
    }
}































################################################################################
## This file contains the following fitbit cleaning functions:
##
## - dropIndexes: dropIndexes functions returns vector with given indexes removed
## - propDiff: propDiff functions returns proportinal difference between 2 values
## - removeRepPointsdata: removeRepPointsdata functions that removes repeating values
## - removeRandomPoints: removeRandomPoints functions that removes random values based on time
## - remove70Leniant1: remove70Leniant1 functions that removes values of 70 that are errors based on values around them and time
## - removeOutlierPoints: removeOutlierPoints functions that removes values based on being outlier value vs neighbourhood of values
################################################################################

dropIndexes <- function(data, vec){

    # remove given rows from a dataframe
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #   vec: vector, contains rows to remove from data 
    #
    # Returns:
    #   dataRowsRemoved: same columns as data
  
    if(!is.null(vec)){
        dataRowsRemoved = data[-c(vec), ]   # notice the -
    return (dataRowsRemoved)
    
    }else{
        return(data)
    }
}

propDiff <- function(big,small){ # legacy

    # calculate proportional diff between 2 numerical values
    #
    # Args:
    #   big: numeric
    #   small: numeric
    #
    # Returns:
    #   res: numeric
  
    res=  (abs(big - small)/small)*100
    
    return(res)
}


removeRepPointsdata <- function(data){

    # remove repetive points from data frame
    # based on value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data

    rep = 0
    repInds = c()
    if(nrow(data) >= 3){
        for ( i in 2:(nrow(data)-1)  ){
        cur = data$value[i] 
        nex = data$value[i + 1]
        prev = data$value[i - 1]
      
        curt = data$time[i] 
        nex_t = data$time[i + 1] 
      
        absDiffPost = (abs(nex - cur)/cur)*100
        absDiffPrev = (abs(cur - prev)/prev)*100
        if(cur == nex & i < nrow(data) - 1)
        {
            rep = rep + 1

        }else if((cur != nex & rep >= 10) | (i == nrow(data) - 1 & rep >= 10)){
            repInds = c(repInds,(i-rep):i)
            rep = 0
        }#else if(cur != nex & rep >= 25){
            #print(i)
            #print(rep)
            #repInds = c(repInds,(i-rep):i)
            #rep = 0
        #}
        if(cur !=nex){
            rep = 0
        }

    }
    #print(i)
    res = dropIndexes(data,repInds)
    return (res)
    }else{
        return(data)
    }
  
}


removeRandomPoints <- function(data){

    # remove random points from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data
  
    diffs <- c(difftime(data$time[-1],data$time[-nrow(data)],units="secs"))
    indsBigDiff = which(diffs >= 120)
    if(length(indsBigDiff) > 0){
        indsBigDiff = c(1,indsBigDiff,length(diffs))
        indsToRemove = c()
    
        for(i in 1:(length(indsBigDiff) - 1)){
            gap = indsBigDiff[i+1] - indsBigDiff[i]
            if(gap <=20)
            {
                f = indsBigDiff[i]+1
                c = indsBigDiff[i+1]
                indsToRemove = c(indsToRemove,f:c)
            }
        }
        res = dropIndexes(data,indsToRemove)
        return (res)
    }else{
        return (data)
    }
}


remove70 <- function(data){ # legacy
  seventy = which(data$value == 70)
  if( length(seventy) > 0){
    remove70s = c()
    f = 0
    c = 0
    d = as.Date(data$time[1])
    cutoff = paste(d, '22:00:00', sep = ' ')
    cutoff = as.POSIXct(cutoff, format = "%Y-%m-%d %H:%M:%S") # convert time column to time variable
    for ( i in seventy){
      t = data$time[i]
      if(t < cutoff){
        if(i == 1){
          f = i
          j = i + 1
          val1 = data$value[i]
          val2 = data$value[j]
          val0 = data$value[i-1]
          while(propDiff(val2,val1) < 10 & (j<= nrow(data) -1 )){
            j = j + 1
            val2 = data$value[j]
            
          }
          c = j - 1
          remove70s = c(remove70s,f:c)
        }
        else if(i > 1 & i < nrow(data)){
          f = i
          j = i + 1
          val1 = data$value[i]
          val2 = data$value[j]
          val0 = data$value[i-1]
          while((propDiff(val2,val1) < 10) & (j<= nrow(data) -1 ) ){
            j = j + 1
            val2 = data$value[j]
            
          }
          c = j - 1
          if(propDiff(val1,val0) > 10){
            remove70s = c(remove70s,f:c)
          }
        }
        else if(i == nrow(data)){
          val1 = data$value[i]
          val0 = data$value[i-1]
          if(propDiff(val1,val0) > 10){
            remove70s = c(remove70s,i)
          }
        }
      }
      
    }
    
    res = dropIndexes(data,remove70s)
    return (res)
  }else{
    return(data)
  }
  
}


remove70Leniant <- function(data){ # legacy
    seventy = which(data$value == 70)
    seventy = sort(seventy)
    if(length(seventy) > 1){
        remove70s = c()
        if( 1 %in% seventy){
            remove70s = c(remove70s,1)
        }
        if(nrow(data) %in% seventy){
            remove70s = c(remove70s,nrow(data))
        }
        if(length(seventy) > 1){
            curRem = c()
            rep = 0
            for ( i in 1:(length(seventy)-1)){
                cur = seventy[i]
                nex = seventy[i + 1]
                if( nex == (cur + 1)){
                    #rep = rep + 1
                    curRem = c(curRem,cur,nex)
                    curRem = unique(curRem)
                }else{
                    remove70s = c(remove70s,curRem)
                    curRem = c()
                    #rep = 0
                }
                
            }
        }
        
        remove70s = unique(remove70s)
        res = dropIndexes(data,remove70s)
        return (res)
    
    }else{
        return(data)
    }
}


remove70Leniant1 <- function(data){


    # remove error 70 values from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data
  
    seventy = which(data$value == 70)
    seventy = sort(seventy)
    if(length(seventy) > 1){
        remove70s = c()
        if(1 %in% seventy){
            remove70s = c(remove70s,1)
            i = 2
            prev = seventy[i-1]
            cur = seventy[i]
            while(cur == prev + 1 & i <= length(seventy)-1)
            {
                remove70s = c(remove70s,cur)
                i = i + 1
                prev = seventy[i-1]
                cur = seventy[i]

            }
        }
        if(nrow(data) %in% seventy){
            remove70s = c(remove70s,nrow(data))
            i = length(seventy)-1
            cur = seventy[i]
            nex = seventy[i + 1]
            while((nex == cur + 1) & (i >= 2))
            {
                remove70s = c(remove70s,cur)
                nex = i
                cur = seventy[i - 1]
                i = i - 1
                #prev = seventy[i - 1]
                #print(prev)
            }    
        }
        remove70s = unique(remove70s)
        res = dropIndexes(data,remove70s)
        return (res)

    }else{
        return(data)
    }
}

removeOutlierPoints <- function(data){

    # remove outlier values from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data
  
    prop <- abs(diff(data$value) / data$value[-length(data$value)]) * 100# get proportional difference of gaps,
    diffs <- c(difftime(data$time[-1],data$time[-nrow(data)],units="secs"))
    indsBigGap = which(prop >= 15)
    indsToRemove = c()
    if(length(indsBigGap) > 1){
    
        for(i in 1:(length(indsBigGap) - 1)){
            if(indsBigGap[i] + 1 < nrow(data)){
                cur = data$value[indsBigGap[i] + 1]
                timeDiff = diffs[indsBigGap[i]]
                gap = indsBigGap[i+1] - indsBigGap[i]
                if(gap <=5 & cur < 100 & timeDiff<30)
                {
                    f = indsBigGap[i]+1
                    c = indsBigGap[i+1]
                indsToRemove = c(indsToRemove,f:c)
                }
            }
        }
        res = dropIndexes(data,indsToRemove)
        return (res)
    }else{
        return(data)
    }
  
}

