xap.require("dplyr", "chron", "lubridate", "ggplot2", "DBI", "grid")
# load libraries


data <- xap.read_table("fitbit_featurise_table_2020_12_post_featurised")
link <- xap.read_table('patient_key_id_list')
# load in post featurised fitbit data
# read in link file

data_filter <- data %>%
                filter(mins_wear_1121 >= 270 )

# choose days with minimum 270 minutes between 11am - 9pm as this was the window chosen

rhr <- data_filter[, c("userid", "date", "resting_hr_1121", "study_email", "age_recruited", "decimal_age")]
colnames(rhr) <- c("userid", "date", "rhrFiveLowest", "study_email", "age_recruited", "decimal_age")
# extract above columns and rename so I did not have to change code

ids <- unique(rhr$userid)
# all people who will provide rhr data

rollingfirst14 <- function(test, window) {

    if (nrow(test) < window){

        new_window <- nrow(test)
    }else{

        new_window <- window
    }
    newrhr <- test[1:(new_window), ]
    newrhr$rollingRHR <- mean(newrhr$rhrFiveLowest)
    if (nrow(test)>window){

        for (i in (window + 1):nrow(test)){
    
            cur<-test[i, ]
            prev <- test[(i - (window - 1)) : i, ]
            cur$rollingRHR <- mean(prev$rhrFiveLowest)
            newrhr <- rbind(newrhr, cur)
        }
    }
    return (newrhr)
}
# function to get 14 day rolling average of rhr,
# if less than 14 days, then first day is used for all days

newrhr_14 <- data.frame()
window = 14
# set window to 14 so to create 14 day rolling average

for (i in ids){

    rhrTemp <- rhr[rhr$userid == i, ]
    rhrTemp <- rhrTemp[order(rhrTemp$date), ]
    b <- nrow(rhrTemp)
    df <- rollingfirst14(rhrTemp, window)
    a <- nrow(df)
    newrhr_14 <- rbind(df, newrhr_14)
}

# for loop to running 14 day rolling rhr function

names(newrhr_14)[names(df) == "rollingRHR"] <- "rollingRHR14"
rhr <- newrhr_14
#store new rhr data in rhr df


medianRHR <- rhr %>%
    group_by(study_email, age_recruited)  %>%
    summarize(medRHRLowest = median(rhrFiveLowest)) %>%
    ungroup()

# get median rhr per person

l = lm(data = medianRHR, formula = medRHRLowest ~ age_recruited)
# create linear regression model to see relatinship between age and median rhr

new.ages <- data.frame(
  age_recruited = rhr$decimal_age
)

y = predict(l, newdata = new.ages)
rhr$rhr_pred = y
# get new rhr value for each day using this model and decimal age


#p = ggplot(rhr, aes(x = decimal_age, , y = rollingRHR14,colour = study_email)) + geom_point(aes(fill=study_email),show.legend = FALSE) + 
#    geom_point(x = rhr$decimal_age,y = rhr$rhr_pred , colour = 'red') + 
#    labs(title = 'rolling 14 day rhr with regression line') + ylab('Resting heart rate (bpm)')+ xlab('Age (years)') + #+ ylab('Resting heart rate (bpm)') +
#    theme(text = element_text(size=15)) + scale_x_continuous(name ="Age (years)", breaks=c(6,8,10,12,14))


polvsfb <- xap.read_table('max_polar_fitbit')
polvsfb$max_fitbit[!is.finite(polvsfb$max_fitbit)] <- polvsfb$max_polar_hr
polvsfb <- polvsfb[is.finite(polvsfb$max_fitbit),]
polvsfb <- merge(polvsfb,link[,c('fizzyo_hub_id','age_recruited')],by.x = 'fizzyo_hub_id',by.y = 'fizzyo_hub_id',all.x = TRUE )

# get polar max  data
# convert all na fitbit maxes to polar max
# only keep finite data

polvsfb <- polvsfb %>%
    group_by(fizzyo_hub_id) %>%
    mutate(max_test = max(max_polar_hr, max_fitbit))

# get max per person between fitbit and polar

set.seed(123) # set 101 as seed

# set seed
# only use people with max >= 180

lmdata = polvsfb[polvsfb$max_test >= 180,]

# make linear regression model of max vs age rectruited
# then get amx per day using decimal age
peakL = lm(data = lmdata, formula = max_test ~ age_recruited)

test <- data.frame(
  age_recruited = rhr$decimal_age
  
)

rhr$max_test = predict(peakL, newdata = test)
# predicted max value

rhr$thresh14 = round(rhr$rollingRHR14 + ((rhr$max_test - rhr$rollingRHR14)*0.4))
rhr$thresh14_low = rhr$rollingRHR14 + ((rhr$max_test - rhr$rollingRHR14)*0.2)
rhr$thresh14_high = rhr$rollingRHR14 + ((rhr$max_test - rhr$rollingRHR14)*0.6)

# create 3 mvpa threshold values using the formula of mvpa = rhr + (max - rhr)*0.4 etc

rhr$thresh14[rhr$thresh14 < 110] = 110
rhr$thresh14[rhr$thresh14 > 130] = 130

# cap mvpa threshold between 110 - 130

#ggplot(rhr, aes(x=thresh14)) + geom_histogram(breaks=seq(100, 150, by=1)) + labs(title = '12-8, rounded')

xap.db.writeframe(rhr,'mvpa_thresh_11_9_4_5')