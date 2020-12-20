library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)
library(dbplyr)
library(ggplot2)

xap.require('grid','gridExtra')

conn = xap.conn
options(digits.secs = 3)

data = xap.read_table('act_featurised_blind')
link = xap.read_table('patient_key_id_list')
low = data[data$breathCount <8,]


device = xap.read_table('devices_clean_test')
device$tag = gsub('Other ', '', device$tag)
device$tag = gsub(' Other', '', device$tag)
device$device1 = sapply(strsplit(device$tag," "), `[`, 1)
device$device1[sapply(strsplit(device$tag, " "), length) > 1] = 'Multiple'
link = merge(link,device[,c('study_email','device1')],by = 'study_email')

blind = xap.read_table('act_featurised_blind_2020_12_v2')
blind_m = merge(blind,link[,c('patient_record_id','device1')],by.x = 'patientId', by.y = 'patient_record_id',all.x = TRUE)
blind_m_Pari_PEP = blind_m[blind_m$device1 == 'Pari_PEP',]
plot(blind_m_Pari_PEP$meanBreathAmplitude)
plot(blind_m_Pari_PEP$meanBreathAmplitudeMiddle)

exfile_extension = 'ACT_v3/act_pipeline/utils'
dir1 = paste(paste(strsplit(getwd(),'/devices')[1],'scripts',sep = '/'),exfile_extension,sep = '/')

newFiles_extension = 'ACT_v3/act_pipeline/src/utils'
dir2 = paste(paste(strsplit(getwd(),'/devices')[1],'scripts',sep = '/'),newFiles_extension,sep = '/')


current_folder <- dir1
new_folder <- dir2
list_of_files <- list.files(current_folder) 
# ".py$" is the type of file you want to copy. Remove if copying all types of files. 
file.move(dir1,dir2)



mergeACT_Games  <- function(inputTableName, gamesData, table_return_name, linkName)
{
    
    act_merged <- data.frame()
    link  <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", linkName))
    act  <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", inputTableName))
    games <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", gamesData))
    games = merge(games,link[,c('fizzyo_hub_id','patient_record_id')],by.x = 'userid',by.y = 'fizzyo_hub_id')

    act$games = 0
    act$games_played = 0

    for(p in unique(act$patientId)){
        temppr = act[act$patientId == p,]
        tempg = games[games$patient_record_id == p,]
        #print(nrow(tempg))
        tempg_dates = unique(tempg$date)
        temppr_dates = unique(temppr$actDate)
        datesMatch = intersect(tempg_dates,temppr_dates)
        temppr$games[temppr$actDate %in% datesMatch] = 1
        #act_merged = rbind(temppr,act_merged)
        
        for (d in as.list(tempg_dates)){
            unique_games = unique(tempg[tempg$date == d,'display_name'])
            unique_games = sort(unique_games)
            unique_games = paste0(unique_games, collapse=",")
            #print(unique_games)
            temppr$games_played[temppr$actDate == d] <- unique_games
        }
        
        act_merged = rbind(temppr,act_merged)
    }
    
    xap.db.writeframe(act_merged,gamingOutputTableName)

}

paste0(unique_games, collapse=",")


odd = data[data$setCount >= 30,]

dragonfly_id =  link[link$animal == 'Dragonfly','patient_record_id']
dragonfly = data[data$patientId == dragonfly_id,]
dragonfly = dragonfly[order(dragonfly$actDate),]

dragonfly[1:20,c('treatmentId','actDate')]


dataTreatment = data[data$treatmentId == 'ab741200-fb82-42be-b891-15d0c4fdb652',]

dataraw  <- tbl(conn, 'pressure_raw_sept_oct') %>%
    filter(patient_record_id == '4c4c8f97-f70e-4db9-bbcb-a3cb7e3f641a' & 
    time >= '2018-12-18 10:54:29' & time <= '2018-12-18 11:07:17') %>%
    collect()
    
dataraw  <- tbl(conn, 'act_clean_test_new') %>%
    filter(patientId == '4c4c8f97-f70e-4db9-bbcb-a3cb7e3f641a' & 
    time >= '2018-12-18 10:54:29' & time <= '2018-12-18 11:07:17') %>%
    collect()
    
data_lab  <- tbl(conn, 'act_labeled_integration_test') %>%
    filter(treatment_id == 'ce8f3238-4a22-4c71-8e49-76aad6ee7fe4')

dataraw = dataraw[order(dataraw$time),]

plot(dataraw$pressurevalues,type = 'l')

cleaned = CleanRawACTData(dataraw)
plot(cleaned$pressureDetrend,type = 'l')


dataold  <- tbl(conn, 'act_clean_test_blind') %>%
    filter(patientId == '80693f51-864c-40b3-8091-67aef776f0f4' & 
    time >= '2019-04-27 08:00:00' & time <= '2019-04-27 09:00:00') %>%
    collect()

dataL  <- tbl(conn, 'act_labeled_new') %>%
    filter(treatmentId == 'd44097b9-9f04-4bb6-8cf0-60313d8c79cc') %>%
    collect()

dataL  <- tbl(conn, 'act_clean_test_new') %>%
    filter(treatmentId == 'd44097b9-9f04-4bb6-8cf0-60313d8c79cc') %>%
    collect()

write.csv(data_lab,dir,row.names = FALSE)

#dataL = dataL[order(dataL$time),]

pressureDetrend = dataL$pressureDetrend
prominentPeak = DetectPeaks(pressureDetrend)
breathId = TagBreathIds(pressureDetrend, prominentPeak)
breathCount = CountIds(breathId)



treatments = xap.read_table('labeled_treatment_meta')
five100 = sample_n(treatments,500)
ToProcessedIds = unique(five100$treatmentId)

data <- tbl(conn, 'act_labeled_new') %>%
    filter(treatmentId %in% ToProcessedIds) %>%
    collect()

meta <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", 'clean_meta_sessions'))
dataM = merge(data,meta[,c('sessionId','sessionStart')],by = 'sessionId')
i = 1
source("~/scripts/ACT_v3/1_featurise/utils/act_labelling_utils_kunal.R")


for(t in unique(ToProcessedIds)){
    temp = dataM[dataM$treatmentId == t,]
    temp = temp[order(temp$sessionStart),]
    pressureDetrend = temp$pressureDetrend
    prominentPeak = indexes(pressureDetrend)
    breathId = TagBreathIds(pressureDetrend, prominentPeak)
    breathCount = CountIds(breathId)
    setData = DetermineSets1(breathId,pressureDetrend)
    bc = paste('breathCount',breathCount,sep = '=')
    sets = paste('sets',setData[1],sep = '=')
    sets1 = paste('no_of_sets',(setData[2]),sep = '=')
    sets2 = paste('avg_breaths_in_set',setData[3],sep = '=')
    sets3 = paste('sd_of_sets',setData[4],sep = '=')
    bc = paste(paste(paste(paste(bc,sets,sep = '_'),sets1,sep = '_'),sets2,sep = '_'),sets3,sep = '_')
    title = paste(i,bc,sep = '_')
    #plot(temp$pressureDetrend, main = title,type = 'l' )
    p = ggplot(data=temp, aes(x=1:nrow(temp), y=pressureDetrend)) +
        geom_line() + 
        labs(title = title)
    i = i + 1
    dir = '/home/sejjkk4/scripts/set_images'
    fname = paste(dir,title, sep ='/')
    fname = paste(fname,'.png', sep ='')
    ggsave(file = fname, p)
}


list.files(dir)
files = list.files(dir)
for(f in files){
    dirf = paste(dir,f,sep = '/')
    file.remove(dirf)
}

res = data.frame()
for(f in files){
    image_number = as.integer(strsplit(f,"_")[[1]][[1]])
    no_of_breaths = as.integer(strsplit(strsplit(f,"breathCount=")[[1]][[2]],'_')[[1]][[1]])
    sets = as.integer(strsplit(strsplit(f,"sets=")[[1]][[2]],'_')[[1]][[1]])
    no_of_sets = as.integer(strsplit(strsplit(f,"no_of_sets=")[[1]][[2]],'_')[[1]][[1]])
    avg_breaths_in_sets = as.numeric(strsplit(strsplit(f,"avg_breaths_in_set=")[[1]][[2]],'_')[[1]][[1]])
    setsd = as.numeric(strsplit(strsplit(f,"sd_of_sets=")[[1]][[2]],'.png')[[1]])
    df = data.frame(image_number,no_of_breaths,sets,no_of_sets,avg_breaths_in_sets,setsd)
    res = rbind(res,df)
}

xap.db.writeframe(res,'set_meta_data')



findPeakIndex <- function(breathIds,pressureDetrend)
{
  # Given a vector of unique breathIds, this function finds the index of the
  # midpoint of each breath.
  # Returns the indices of breath midpoints.

  breathIdVals <- unique(breathIds)
  
  # Remove zero ID
  breathIdVals <- breathIdVals[!breathIdVals == 0]
  k <- 1 # iterator
  # Initialise vector to store peak indices
  peakIndices <- 0

  for (i in breathIdVals){
    # Breath width is all the points which have that breath ID
    breathIndexes <- which(breathIds == i)
    breathPressureDetrend = pressureDetrend[breathIndexes]
    peakIndex = which.max(breathPressureDetrend)
    # Mark the peak at its midpoint
    peakIndices[k] <- breathIndexes[peakIndex]
    k <- k + 1
  }

  peakIndices
}



DetermineSets1 <- function(breathIds,pressureDetrend) {
  # determine if treatment has sets or not.Based on breathIds column.
  # attempts to find gaps in breaths that represent gaps in sets.
  # once sets identified, then determines these are good sets.
  # sets found analysed by number of breaths inside them.

  # Args:
  #   breathIds: contains ids of breaths at respective indexes.

  # Returns
  #   1 if sets identifiable, 0 if not identifiable.
  
  realBreathSet = 2 # number of breaths in set for it to be real
  outlierFloor = 1 # minimum number of sets
  outlierCeiling = 30 # max number of sets
  avgBreathFloor = 4 # minimum average breaths per set
  avgBreathCeiling = 25 # max average breaths per set
  sdBaseline = 3.5 # max sd
  sdLeniantBaseline = 5.5 # max sd with high average breaths per set
  kPropCutOff <- 2 # Heuristic cut - off used to find the boundaries of sets, sets identified when a gap is 3 times bigger then previous gap
  # reason its 2 is because prop diff is 2 for something 3x bigger, so prop diff of 20 - 60 is 2 not 3 
  kabsDifCutOff <- 6 # absolute difference between breaths in a set to 10 must be <= 6 to be considered a set.
  minimumBreaths <- 8 # minimum breaths in treatment for sets to be even counted, else no sets exist
  bigSet <- 8
  assumedBreathsPerSet <- 10 # number of breaths per set, will be updated to be personal
  #peaks <- FindBreathCentres(breathIds) # indexes of breaths
  peaks <- findPeakIndex(breathIds, pressureDetrend)
  if (length(peaks) <= minimumBreaths) {
  # if <= 8 breaths
    ret = list(0,NA,NA,NA)
    return (ret)
  }
  gaps <- diff(peaks) # find first order of difference in peaks
  prop <- diff(gaps) / gaps[-length(gaps)]# get proportional difference of gaps,
  # each value is a proportional of the previous gap
  outliers <- which(prop >= kPropCutOff) # which gaps are 3 times bigger than the previous gap
  avgBreathBetweenOutliers = mean(diff(outliers))
  if(length(outliers) > outlierFloor & length(outliers)<=outlierCeiling & avgBreathBetweenOutliers >= avgBreathFloor){ # if average of 4 breaths per set
      actualOutliers = c()
      for( i in 1:(length(outliers) -1)){
        outlier = outliers[i]
        nex = outliers[i+1]
        if((nex - outlier) > realBreathSet){
            actualOutliers = c(actualOutliers,outlier)
        }
        if(i == (length(outliers) -1)){
            actualOutliers = c(actualOutliers,nex)
        }
          
      }
      outliers = actualOutliers # have removed all small sets and merged them into bigger sets

      if (length(outliers) > outlierFloor & length(outliers)<=outlierCeiling) { # more than 1 outlier
          # if big gaps exist
          BreathsinsideGaps <- c(outliers[1], diff(outliers))
          BreathsinsideGaps <- c(BreathsinsideGaps, length(peaks) - outliers[length(outliers)])
          # determine number of breaths in a set
          avgBreathsInSet = mean(BreathsinsideGaps) # avg breaths in a  set
          sdBreathsInSet = sd(BreathsinsideGaps)
          # find absolute difference to 10 of BreathsinsideGaps
          if((avgBreathsInSet >= avgBreathFloor) & (avgBreathsInSet <= avgBreathCeiling)) { # if avg breatsh in set is >=4 and <= 25
            if(avgBreathsInSet < bigSet & sdBreathsInSet <= sdBaseline){ 
                # if avg breaths in set <8 and sd <= 3 then sets
                ret = c(1,length(outliers) + 1,avgBreathsInSet,sdBreathsInSet)
                return (ret)
            } else if (avgBreathsInSet >= bigSet & sdBreathsInSet <= sdLeniantBaseline){ 
                #s if avg breaths in set >=8 and sd <= 6 then sets
                ret = c(1,length(outliers) + 1,avgBreathsInSet,sdBreathsInSet)
                return (ret)
            } else{
                ret = c(0,length(outliers) + 1,avgBreathsInSet,sdBreathsInSet)
                return (ret)
            }

          } else{
            ret = c(0,length(outliers) + 1,avgBreathsInSet,sdBreathsInSet)
            return (ret)
          }
      }else{
        ret = c(0,length(outliers) + 1,NA,NA)
        return (ret)
      }

  }else{
      ret = c(0,NA,NA,NA)
      return (ret)
  }
      
}


indexes <- function( y , thres = 10, min_dist = 5 , thresh_abs = TRUE)
{
  
  min_dist = as.integer(min_dist)
  if(thresh_abs == FALSE)
  {
    thres = thres * (max(y) - min(y)) + min(y);
  }
  
  dy = diff(y);
  
  zeroes = which(dy == 0)
  
  while(length(zeroes) > 0)
  {
    
    zeroesr = c()
    zeroesl = c()
    zeroesr = dy[2:length(dy)] # everytthing but first
    zeroesr[length(zeroesr) + 1] = 0
    
    zeroesl = dy[1:(length(dy) - 1)] # everything but last
    zeroesl = c(c(0),zeroesl)
    
    dy = replace(dy, zeroes, zeroesr[zeroes] )
    zeroes = which(dy == 0)
    
    dy = replace(dy, zeroes, zeroesl[zeroes] )
    zeroes = which(dy == 0)
    
  }
  
  dy1 = c(dy , c(0))
  dy2 = c(c(0) , dy)
  peaks = which(dy1 < 0 & dy2 > 0 & y > thres)
  
  if(length(peaks) > 1 & length(min_dist > 1))
  {
    highest = rev(peaks[order(y[peaks])])
    rem = rep(FALSE, length(y))
    rem[peaks] = TRUE
    
    for (peak in highest)
    {
      if(rem[peak] == TRUE)
      {
        floor = max(0,peak - min_dist)
        ceiling = peak + min_dist + 1
        rem[floor:ceiling] = FALSE
        rem[peak] = TRUE
      }
    
      #peaks = which(rem == FALSE)
      
    }
    
    
  }
  
  return (rem)
}


peaks = indexes(temp$pressureDetrend)



DetermineSetsOld <- function(breathIds,pressureDetrend) {
  # determine if treatment has sets or not.Based on breathIds column.
  # attempts to find gaps in breaths that represent gaps in sets.
  # once sets identified, then determines these are good sets.
  # sets found analysed by number of breaths inside them.

  # Args:
  #   breathIds: contains ids of breaths at respective indexes.

  # Returns
  #   1 if sets identifiable, 0 if not identifiable.

  kPropCutOff <- 2 # Heuristic cut - off used to find the boundaries of sets, sets identified when a gap is 3 times bigger then previous gap
  # reason its 2 is because prop diff is 2 for something 3x bigger, so prop diff of 20 - 60 is 2 not 3 
  kabsDifCutOff <- 6 # absolute difference between breaths in a set to 10 must be <= 6 to be considered a set.
  minimumBreaths <- 8 # minimum breaths in treatment for sets to be even counted, else no sets exist
  assumedBreathsPerSet <- 10 # number of breaths per set, will be updated to be personal
  #peaks <- FindBreathCentres(breathIds) # indexes of breaths
  peaks <- findPeakIndex(breathIds, pressureDetrend)
  if (length(peaks) <= minimumBreaths) {
  # if <= 8 breaths
    return (0)
  }
  gaps <- diff(peaks) # find first order of difference in peaks
  prop <- diff(gaps) / gaps[-length(gaps)]# get proportional difference of gaps,
  # each value is a proportional of the previous gap
  outliers <- which(prop >= kPropCutOff) # which gaps are 3 times bigger than the previous gap
  if (length(outliers) > 25 & length(peaks) < 100) { # if over 25 sets and less than 100 breaths, probably not real sets but just loads of breaths far apart from each other
    return (0) 
  } else if (length(outliers) >= 1) { # more than 1 outlier
      # if big gaps exist
      BreathsinsideGaps <- c(outliers[1], diff(outliers))
      BreathsinsideGaps <- c(BreathsinsideGaps, length(peaks) - outliers[length(outliers)])
      # determine number of breaths in a set
      avgBreathsInSet = mean(BreathsinsideGaps) # avg breaths in a  set
      # find absolute difference to 10 of BreathsinsideGaps
      if ((avgBreathsInSet >= 4) & (avgBreathsInSet <= 25)) { # if avg breaths in set >= 4 and <= 25 then sets, else no sets
        return (1)
      } else {
        return (0)
      }
  }
  return (0)
}



DetermineSetsStrict <- function(breathIds) {
  # determine if treatment has sets or not.Based on breathIds column.
  # attempts to find gaps in breaths that represent gaps in sets.
  # once sets identified, then determines these are good sets.
  # sets found analysed by number of breaths inside them.

  # Args:
  #   breathIds: contains ids of breaths at respective indexes.

  # Returns
  #   1 if sets identifiable, 0 if not identifiable.

  kPropCutOff <- 3 # Heuristic cut - off used to find the boundaries of sets
  # absolute difference between breaths in a set to 10 must be <= 4 to be considered a set.
  kabsDifCutOff <- 4
  minimumBreaths <- 8 # minimum breaths in treatment
  assumedBreathsPerSet <- 8 # number of breaths per set, will be updated to be personal
  peaks <- FindBreathCentres(breathIds)
  if (length(peaks) <= minimumBreaths) {
  # if no breaths return 0
    return (0)
  }
  gaps <- diff(peaks) # find first order of difference in peaks
  prop <- diff(gaps) / gaps[-length(gaps)]# get proportional difference of gaps,
  # each value is a proportional of the previous gap
  outliers <- which(prop >= kPropCutOff) # which gaps are 3 times bigger than the previous gap
  if (length(outliers) > 15 & length(peaks) < 100) {
    return (0)
  } else if (length(outliers) >= 1) {
    if (length(outliers) + 1 >= ((length(peaks) / assumedBreathsPerSet) - 2)) {
      # if big gaps exist
      BreathsinsideGaps <- c(outliers[1], diff(outliers))
      BreathsinsideGaps <- c(BreathsinsideGaps, length(peaks) - outliers[length(outliers)])
      # determine number of breaths between big gaps
      absDif <- mean(abs(BreathsinsideGaps - assumedBreathsPerSet))
      # find absolute difference to 10 of BreathsinsideGaps
      if (absDif <= kabsDifCutOff) {
      # if absDif <= 4, then sets identifiable
        return (1)
      } else {
        return (0)
      }
    } else {
      return (0)
    }
  }
  return (0)
}



FindBreathCentres <- function(breathIds){
  # Given a vector of unique breathIds, this function finds the index of the
  # midpoint of each breath.
  # Returns the indices of breath midpoints.

  breathIdVals <- unique(breathIds)

  # Remove zero ID
  breathIdVals <- breathIdVals[!breathIdVals == 0]
  k <- 1 # iterator
  # Initialise vector to store peak indices
  peakIndices <- 0

  for (i in breathIdVals){
    # Breath width is all the points which have that breath ID
    breathWidth <- which(breathIds == i)
    midpoint <- round(mean(breathWidth))

    # Mark the peak at its midpoint
    peakIndices[k] <- midpoint
    k <- k + 1
  }

  peakIndices
}

