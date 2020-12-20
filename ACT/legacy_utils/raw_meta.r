library(DBI)
library(dplyr)
library(dbplyr)
library(zoo)


WriteSessionMetaData <- function(conn, rawTableName, rawMetaName, cleanTableName, cleanMetaName) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patientId <chr> | sessionId <chr> | treatmentId <chr> |
  # sessionStart <datetime> | sessionEnd <datetime>
  #
  # Processes the entire set of historical data.

    sessionMetaData <- tbl(conn, rawTableName) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,patient_record_id,time) %>%
        group_by(date, patient_record_id) %>%
        summarize()%>%
        collect()
        
    DBI::dbWriteTable(conn, rawMetaName, sessionMetaData,
                        overwrite = TRUE, row.names = FALSE)
                        
    writeLines("############ wrote raw pressure meta ############")        
    
}

WriteSessionMetaData2 <- function(conn, rawTableName, rawMetaName, cleanTableName, cleanMetaName) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patientId <chr> | sessionId <chr> | treatmentId <chr> |
  # sessionStart <datetime> | sessionEnd <datetime>
  #
  # Processes the entire set of historical data.

    sessionMetaData <- tbl(conn, rawTableName) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,patient_record_id,time) %>%
        group_by(date, patient_record_id) %>%
        summarize()%>%
        collect()
        
    DBI::dbWriteTable(conn, rawMetaName, sessionMetaData,
                        overwrite = TRUE, row.names = FALSE)
                        
    writeLines("############ wrote raw pressure meta ############")        
    
}

WriteSessionMetaData3 <- function(conn, rawTableName, rawMetaName, cleanTableName, cleanMetaName) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patientId <chr> | sessionId <chr> | treatmentId <chr> |
  # sessionStart <datetime> | sessionEnd <datetime>
  #
  # Processes the entire set of historical data.

    sessionMetaData <- tbl(conn, cleanTableName) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,patientId,time) %>%
        group_by(date, patientId) %>%
        summarize()%>%
        collect()
        
    DBI::dbWriteTable(conn, cleanMetaName, sessionMetaData,
                        overwrite = TRUE, row.names = FALSE)
                        
    writeLines("############ wrote raw pressure meta ############")        
    
}

WriteSessionMetaData4 <- function(conn, rawTableName, rawMetaName, cleanTableName, cleanMetaName) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patientId <chr> | sessionId <chr> | treatmentId <chr> |
  # sessionStart <datetime> | sessionEnd <datetime>
  #
  # Processes the entire set of historical data.

    sessionMetaData <- tbl(conn, rawTableName) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date,patient_record_id,time) %>%
        group_by(date, patient_record_id) %>%
        summarize()%>%
        collect()
        
    DBI::dbWriteTable(conn, rawMetaName, sessionMetaData,
                        overwrite = TRUE, row.names = FALSE)
                        
    writeLines("############ wrote raw pressure meta ############")        
    
}