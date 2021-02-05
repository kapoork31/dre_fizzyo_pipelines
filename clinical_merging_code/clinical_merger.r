library('dplyr','ggplot2')

pd = xap.read_table('rdv_patient_demographics') # demographic data
link = xap.read_table('patient_key_id_list') # linker file
#link$id = NULL

pdcf = xap.read_table('patient_diagnosis_cfis') # dataset containing chromosome type for each person. 
pdcf = pdcf[!duplicated(pdcf),] # remove duplicates

plfr = xap.read_table('patient_lung_function_results_spiro_gos_rlh_rbh') # lung function results
plfr = plfr[!duplicated(plfr),]# remove duplicates
plfr$date = as.Date(plfr$date) 

nonICUDrugs = xap.read_table('rdv_patient_antimicrobial_orders') # non icu drugs
nonICUDrugs = nonICUDrugs[!duplicated(nonICUDrugs),] # remove duplicates
nonICUDrugs = nonICUDrugs[is.na(nonICUDrugs$end_datetime) == F,]
nonICUDrugs = nonICUDrugs[is.na(nonICUDrugs$start_datetime) == F,]
nonICUDrugs = nonICUDrugs[is.na(nonICUDrugs$route_code) == F,]

nonICUDrugs$error = nonICUDrugs$start_datetime > nonICUDrugs$end_datetime
nonICUDrugs = nonICUDrugs[nonICUDrugs$error == F,]
nonICUDrugs$start_datetime = as.Date(nonICUDrugs$start_datetime)
nonICUDrugs$end_datetime = as.Date(nonICUDrugs$end_datetime)
nonICUDrugs$diffs = difftime(nonICUDrugs$end_datetime,nonICUDrugs$start_datetime, units ="days") # get difference
nonICUDrugs$new_enddate = nonICUDrugs$end_datetime 
nonICUDrugs <- nonICUDrugs %>% mutate(new_enddate = ifelse(diffs >= 364, start_datetime, new_enddate))
nonICUDrugs$new_enddate = as.Date(nonICUDrugs$new_enddate, origin = "1970-01-01")

nonICUDrugs = nonICUDrugs[, c('project_id', 'start_datetime', 'new_enddate', 'route_code', 'diffs','medication_order_type_code')] # take specific columns

#nonICUDrugsF <- nonICUDrugs %>%
#    filter(route_code == 'INTRAVENOUS'
#            | route_code == 'ORAL') # choose drugs given

nonICUDrugsF <- nonICUDrugs %>%
    filter(!grepl('LEVEL', route_code)
            & !grepl('DEVICE', route_code) 
            & !grepl('INHAL', route_code))

#nonICUDrugsF = nonICUDrugs[grepl('Intravenous',nonICUDrugs$route) | grepl('Oral',nonICUDrugs$route), ] # choose drugs given 

nonICUDrugsF$drug_code_i[grepl('INTRAVENOUS',nonICUDrugsF$route_code)] <- 10 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
nonICUDrugsF$drug_code_o[nonICUDrugsF$route_code == 'ORAL'] <- 10.25 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
nonICUDrugsF$drug_code_neb[grepl('NEBUL',nonICUDrugsF$route_code)] <- 10.5 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
nonICUDrugsF$drug_code_other[grepl('INTRAGASTRIC',nonICUDrugsF$route_code) | grepl('INTRAJEJUNAL',nonICUDrugsF$route_code)] <- 10.5 # give lavel so when drawing time spent on IV is drawn at 2.5 on y axis
nonICUDrugsF$drug_code_other[nonICUDrugsF$route_code == 'PO/NG'] <- 10.75 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
nonICUDrugsF$drug_code_other[nonICUDrugsF$route_code == 'PEG'] <- 10.75 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
nonICUDrugsF <- nonICUDrugsF %>%
    filter(project_id %in% unique(link$project_ref)) # choose drugs given 
#nonICUDrugsF = nonICUDrugs[grepl('INTRAVENOUS',nonICUDrugs$route_code) | grepl('ORAL',nonICUDrugs$route_code), ] # choose drugs given 
#nonICUDrugsF$drug_code_i[grepl('INTRAVENOUS',nonICUDrugsF$route_code)] <- 1 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
#nonICUDrugsF$drug_code_o[grepl('ORAL',nonICUDrugsF$route_code)] <- 1.5 # label of 1.5 so time on oral drugs is draw at 1.5 on y axis
#nonICUDrugsF$drug_code_i[nonICUDrugsF$route_code == 'INTRAVENOUS'] <- 1 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis
#nonICUDrugsF$drug_code_o[nonICUDrugsF$route_code == 'ORAL'] <- 1.5 # give lavel so when drawing time spent on IV is drawn at 1.5 on y axis

drugsAll = data.frame() # merge in patient ward movements

for (p in unique(nonICUDrugsF$project_id)){

  drugsT = nonICUDrugsF[nonICUDrugsF$project_id == p,] # loop through patient ward movements for each patient

  for (row in 1:nrow(drugsT)) # loop through each row
  {
    sdates = vector() # vector to populate, this vector will contain the dates where the patient is in hospital. Each row contains a start and end date. The prupose of this loop is to populate a vector with the individial dates for when they were in hospital
    codes_i = vector() # vector to populate
    codes_o = vector() # vector to populate
    codes_other = vector()
    codes_neb = vector()
    
    sdate = drugsT[row,'start_datetime']
    edate = drugsT[row,'new_enddate']
    code_i = drugsT[row,'drug_code_i'] # extract column info
    code_o = drugsT[row,'drug_code_o'] # extract column info
    code_other = drugsT[row,'drug_code_other'] # extract column info
    code_neb = drugsT[row,'drug_code_neb'] # extract column info    
    fullD = seq.Date(sdate, edate, by="day")# dont want any date gaps, create sequence of dates from first to last date for patient ever being to hospital
    temp_i = rep(code_i,length(fullD))
    temp_o = rep(code_o,length(fullD))
    temp_neb = rep(code_neb, length(fullD))
    temp_other = rep(code_other, length(fullD))
    sdates <- c(sdates, fullD)
    project_ids = rep(p, length(fullD)) # fill in hospital no
    codes_i = c(codes_i, temp_i)
    codes_o = c(codes_o, temp_o)
    codes_other = c(codes_other, temp_other)
    codes_neb = c(codes_neb, temp_neb)
    data <- data.frame(project_ids, sdates, codes_i, codes_o, codes_other, codes_neb) # data frame of sdates and classes
    colnames(data) = c("project_id", "date","drug_code_i", "drug_code_o", "drug_code_other", "drug_code_neb") # rename columns
    data$date = as.Date(data$date, origin = "1970-01-01")

    drugsAll = rbind(drugsAll, data)

  }
}

isos = xap.read_table('rdv_patient_all_isolates') # patient bugs grown
isos$date = as.Date(isos$start_datetime)
bugsChosen = c(  # only choose the bugs below
'Pseudomonas aeruginosa (matt)',
'Pseudomonas aeruginosa',
'Pseudomonas aeruginosa (mucoid)',
'Haemophilus parainfluenzae', 
'Haemophilus influenzae NOT type b', 
'Haemophilus influenzae', 
'Staphylococcus aureus', 
'Aspergillus fumigatus',
'Mycobacterium avium-intracellulare', 
'Mycobacterium avium', 
'Mycobacterium AIS complex', 
'Mycobacterium mucogenicum',
'Mycobacterium abscessus'
)
isos = isos[!duplicated(isos),] # remove duplicates
isosf = isos[isos$speciesname %in% bugsChosen,]
isosf$code = 0
isosf$code[grepl('Pseudomonas',isosf$speciesname)]<- 1 # label so drawn at different level on y axis 
isosf$code[grepl('Haemophilus',isosf$speciesname)]<- 2 #
isosf$code[grepl('Mycobacterium',isosf$speciesname)]<- 3
isosf$code[grepl('Staphylococcus',isosf$speciesname)]<- 4
isosf$code[grepl('Aspergillus',isosf$speciesname)]<- 5

isosf$isolate_name_plot[grepl('Pseudomonas',isosf$speciesname)]<- 'Pseudomonas' # label so drawn at different level on y axis 
isosf$isolate_name_plot[grepl('Haemophilus',isosf$speciesname)]<- 'Haemophilus' #
isosf$isolate_name_plot[grepl('Mycobacterium',isosf$speciesname)]<- 'Mycobacterium'
isosf$isolate_name_plot[grepl('Staphylococcus',isosf$speciesname)]<- 'Staphylococcus'
isosf$isolate_name_plot[grepl('Aspergillus',isosf$speciesname)]<- 'Aspergillus'

diag <-merge(pd,subset(link, select = -c(date_feedback_start,date_feedback_end,date_gaming_start,date_gaming_ended,date_withdrawn,date_midpoint_test,study_uid,study_email,fizzyo_hub_id)),by.x="hospital_no", by.y = "gos_id",all.y=TRUE) # merge demographic and link data

diag_cf = merge(diag,pdcf,by="hospital_no",all.x=TRUE) # merge 

ids = unique(diag_cf$patient_record_id)
df = data.frame()

for( id in ids){ # loop through each id, done so each row you know which day of trial it is. If before trial it will have a negative values. So a row with a daye 100 days before the trial starts the dayInTrial will be -100.
	#i = id
  	pdata = diag_cf[which(diag_cf$patient_record_id == id),] 
    daysToGoBack = 365 * pdata$age_recruited 
    befRecr = as.Date(pdata$date_recruited) - 1
    startRecord = befRecr - daysToGoBack
    date = rev(seq.Date(startRecord, befRecr, by="day"))
    dayInTrial = seq(-1, -(daysToGoBack + 1))
  	tempDf = data.frame(date,dayInTrial)
	tempDf$patient_record_id = id
  	df = rbind(df,tempDf)
}

res = merge(df,diag_cf,by = 'patient_record_id',all.x = TRUE)
res = diag_cf

movementsAll = data.frame() # merge in patient ward movements
pwm = xap.read_table('rdv_patient_hospital_admissions') # read in dataset
pwm = pwm[!duplicated(pwm),] # remove duplicates
pwm$sdate = as.Date(pwm$start_datetime)
pwm$edate = as.Date(pwm$end_datetime)
pwm$diffs = difftime(pwm$edate,pwm$sdate, units ="days") # get difference
#pwm$class = 1
pwm$class[pwm$diffs >= 2] = 11 # label all ward stays > 2 days with a 2

pwm_in_proj <- na.omit(pwm[pwm$project_id %in% na.omit(link$project_ref),])

for (p in unique(pwm_in_proj$project_id)){
  pwmT = pwm_in_proj[pwm_in_proj$project_id == p,] # loop through patient ward movements for each patient
  sdates = vector() # vector to populate, this vector will contain the dates where the patient is in hospital. Each row contains a start and end date. The prupose of this loop is to populate a vector with the individial dates for when they were in hospital
  classes = vector() # vector to populate
  for (row in 1:nrow(pwmT)) # loop through each row
  {
    sdate = pwmT[row,'sdate']
    edate = pwmT[row,'edate']
    class = pwmT[row,'class'] # extract column info
    x = sdate # set to start date
    while (x <= edate) # while x < end date of row
    {
      sdates = append(sdates,x) # append date to sdates
      classes = append(classes,class) # append class to classes vector
      x = x + 1 # add 1 day to x.
    } # this is done so to be able to draw the lines for inpatient in the clinicalDataViewer mini app
  }

  data <- data.frame(sdates, classes) # data frame of sdates and classes
  fullD = seq.Date(min(sdates), max(sdates), by="day")# dont want any date gaps, create sequence of dates from first to last date for patient ever being to hospital
  f = data.frame(fullD) # store in df
  colnames(f) <- c("sdates") # store in df
  total <- merge(x = f,y = data,by="sdates",all.x = TRUE) # merge with data df
  colnames(total) = c("date","inPatient") # rename columns
  project_ids = rep(p,nrow(total)) # fill in hospital no
  total$project_id = project_ids
  movementsAll = rbind(movementsAll,total) 
}

movementsAllM <- merge(movementsAll,link[,c('project_ref','gos_id')],by.x = 'project_id', by.y = 'project_ref')
names(movementsAllM)[names(movementsAllM) == 'gos_id'] <- 'hospital_no'
movementsAllM$project_id <- NULL

mgdIns = merge(movementsAllM,res, by = c('hospital_no') , all.y = TRUE) # merge movementsAll with res
mgdNICU = merge(drugsAll, mgdIns, by = c('project_id','date') , all.x = TRUE, all.y = TRUE) # merge with NICU
#drugsAll

featurised_act = xap.read_table('act_post_featurised_2020_12')
featurised_act_gos_id = merge(featurised_act,link[,c('gos_id','patient_record_id')],by.x = 'patient_id', by.y = 'patient_record_id')
fev = merge(plfr,featurised_act_gos_id[,c('gos_id','breaths_week_adherence_score','act_date')],by.x = c('gos_id','date'),by.y = c('gos_id','act_date'),all.x = TRUE,
all.y = TRUE)

total = merge(mgdNICU,fev[,c('gos_id','fev1_z','fev1_pct_pred','height','date','breaths_week_adherence_score')],by.x = c('hospital_no', 'date' ), by.y = c('gos_id', 'date' ),all.x = TRUE, all.y=TRUE) # merge with fev
#xap.db.writeframe(total,'first_stage_clinical_table')

total1 = merge(total , isosf[,c('hospital_no','code','speciesname','isolate_name_plot','date')], by = c('hospital_no','date') , all.x = TRUE) # merge with isolates
total1$diff_dates <- (as.numeric(difftime(total1$date_recruited, total1$date_of_diagnosis), units="days"))/365 # get age at diagnosis
total1$ageAtDiagnosis <- total1$age_recruited - total1$diff_dates # get age at diagnosis

xap.db.writeframe(total1,'first_stage_clinical_table_act') # save to dataset

featurised_fitbit = xap.read_table('fitbit_featurise_table_2020_12_post_featurised')
featurised_fitbit_gos_id = merge(featurised_fitbit,link[,c('gos_id','fizzyo_hub_id')],by.x = 'userid', by.y = 'fizzyo_hub_id')
fev = merge(plfr,featurised_fitbit_gos_id[,c('gos_id','mvpa_15_prev_method','date')],by.x = c('gos_id','date'),by.y = c('gos_id','date'),all.x = TRUE,
all.y = TRUE)


total_fitbit = merge(mgdNICU,fev[,c('gos_id','fev1_z','fev1_pct_pred','height','date','mvpa_15_prev_method')],by.x = c('hospital_no', 'date' ), by.y = c('gos_id', 'date' ),all.x = TRUE, all.y=TRUE) # merge with fev
#xap.db.writeframe(total,'first_stage_clinical_table')

total1_fitbit = merge(total_fitbit , isosf[,c('hospital_no','code','speciesname','isolate_name_plot','date')], by = c('hospital_no','date') , all.x = TRUE) # merge with isolates
total1_fitbit$diff_dates <- (as.numeric(difftime(total1_fitbit$date_recruited, total1_fitbit$date_of_diagnosis), units="days"))/365 # get age at diagnosis
total1_fitbit$ageAtDiagnosis <- total1_fitbit$age_recruited - total1_fitbit$diff_dates # get age at diagnosis

xap.db.writeframe(total1_fitbit,'first_stage_clinical_table_fitbit') # save to dataset
