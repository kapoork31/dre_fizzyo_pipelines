# A quick script script to allow Eleanor to visualise a treatment

library(dplyr)
library(ggplot2)

################################################################

# Paste the treatment id that you want to plot here:
treatment_name = "138e21d3-44a7-4a30-801f-1a04d765299d"


#################################################################



kActTableName <- "ACTcleanPressuresTable_April"

print("Collecting pressure data for plotting... please be patient")

actDB <- xap.conn %>% tbl(kActTableName) 

treatment <- actDB %>% select(treatmentId, pressureDetrend) %>% filter(treatmentId == treatment_name)  %>% collect()


ggplot(treatment, aes(x = seq(length(pressureDetrend)), y= pressureDetrend)) + 
    geom_line() +
    ggtitle(paste("Treatment ", treatment_name)) +
    xlab("Points") +
    ylab("Pressure / mm H2O") 
    
   
    
######################## Raw table names
# "breath_test_all_cleaned_dec_jan", "breath_test_all_data_02_cleaned", "breath_test_all_data_03_cleaned", "breath_test_all_cleaned_month")
 

######################## Feature table names
#kActFeatures1 <- "breath_test_all_data_1201_feat_v2"
#kActFeatures2 <- "breath_test_all_data_0203_feat_v2"
#kActFeatures3 <-  "breath_test_all_data_04_feat_v2"

#kActTableNames = c(kActFeatures1, kActFeatures2, kActFeatures3)


########################
# This is how the raw table is created:

#kTreatmentTableName <- "session_metadata_jul"
#kActTableName <- "breath_test_all_cleaned_month"

#actDB <- xap.conn %>% tbl(kActTableName) 
#treatments <- xap.conn %>% tbl(kTreatmentTableName) %>% select(sessionId, treatmentId)

#result <- left_join(actDB, treatments, by = "sessionId")
#xap.db.writeframe(result, "ACTcleanPressuresTable_April")

###########################
# Experiment
# experimenting with trying to join 5 months of raw data
#kTreatmentTableName <- "session_metadata_jul"
#kActTableName1 <- "breath_test_all_data_03_cleaned"
#kActTableName2 <- "breath_test_all_cleaned_month"

#actDB1 <- xap.conn %>% tbl(kActTableName1)
#actDB2 <- xap.conn %>% tbl(kActTableName2)

#test <- rbind(actDB1, actDB2) %>% collect()

#treatments <- xap.conn %>% tbl(kTreatmentTableName) %>% select(sessionId, treatmentId)

#result <- left_join(actDB, treatments, by = "sessionId")
