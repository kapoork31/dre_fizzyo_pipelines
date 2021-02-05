xap.require('dplyr','chron','lubridate','ggplot2','DBI','tidyr','grid')
library(xts)
# load in data

df_all = xap.read_table('activity_period_mvpa_data') # load in activity data

fill <- function(monday_a_m_m){

    # add date to time column so its plotable
    # fil with all minutes in the day

    timerange1 <- "20160106 0000/20160106 2359"
    seqMinute <- format(timeBasedSeq(timerange1), "%H:%M")
    df = data.frame(time = seqMinute)
    d = Sys.Date()
    df$time = paste(d, df$time)
    df$time = as.POSIXct(df$time, format = "%Y-%m-%d %H:%M") # convert time column to time variable
    monday_a_m_m = merge(monday_a_m_m,df,by = 'time', all.y = TRUE)
    return(monday_a_m_m)

} 

normalize <- function(df){

# take in data and add accumuative % and sum of mvpa for each group per minute of the day
    
    colnames(df) = c('min','value','label')
    d = Sys.Date()
    df$time = paste(d, df$min)
    df$time = as.POSIXct(df$time, format = "%Y-%m-%d %H:%M") # convert time column to time variable
    s = sum(df$value)
    df$pc = df$value/s * 100
    df$cumsum = cumsum(df$pc)
    return (df)
}



normalize2 <- function(df_all, year, month){

# take in data based on given year and month
# just add to filter by daynumber if you want to filter by day of week

    monday_a_m = df_all[(df_all$year == year) & (df_all$month == month),]
    
    count = monday_a_m %>% 
        group_by(userid, year, month, m_day) %>% 
            tally
    
    count = nrow(count)
    # get number of days in this group

    monday_a_m_accum = monday_a_m %>% group_by(hour) %>%
        summarize(value = sum(value))
    # for the group get number of mvpa minutes for each minute
    colnames(monday_a_m_accum) = c('time','value')
    # change colnames
    monday_a_m_accum$label = as.character(month)
    # add label, set as month in this instance
    monday_a_m_accum = normalize(monday_a_m_accum)
    # get accumulative sum and % for each minute
    monday_a_m_accum$normValue = (monday_a_m_accum$value/count)*100
    # divide counts by number of days in this group and normalize to 100 days
    monday_a_m_accum = fill(monday_a_m_accum)
    # use fill function
    monday_a_m_accum = monday_a_m_accum %>% replace(is.na(.), 0)
    print(mean(monday_a_m_accum$value))
    return (monday_a_m_accum)
}

sept_2018 = normalize2(df_all,2019,9)
octob_2018 = normalize2(df_all,2019,10)
nov_2018 = normalize2(df_all,2019,11)
dec_2018 = normalize2(df_all,2019,12)
# obtain data for months of project in 2018
# can obtain data for any project data


ggplot() +  geom_line(data = octob_2018, aes(x=time, y=normValue, colour= '2018_september'))
# test plot october 2018

p = ggplot() + 
geom_line(data = sept_2018, aes(x=time, y=normValue, colour= '2018_september')) +
geom_line(data = octob_2018, aes(x=time, y=normValue, colour= '2018_october')) +
geom_line(data = nov_2018, aes(x=time, y=normValue, colour= '2018_november')) +
geom_line(data = dec_2018, aes(x=time, y=normValue, colour= '2018_december'))
# plot all 4 months on top of each other
