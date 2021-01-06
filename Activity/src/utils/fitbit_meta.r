################################################################################
## This file contains the following fitbit meta creation functions:
##
## write_raw_hr_meta_data: convert raw hr data to meta table with userid, date
## write_fs_meta_data: convert raw fs data to meta table with userid, date
################################################################################

write_raw_hr_meta_data <- function(conn, k_raw_hr_table_name, raw_hr_meta) {

    # convert raw hr to meta with userid,name
    #
    # Args:
    #   k_raw_hr_table_name: name of raw hr table | string
    #   raw_hr_meta: name of meta table for raw hr data | string

    session_meta_data <- tbl(conn, k_raw_hr_table_name) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date, userid, time) %>%
        group_by(date, userid) %>%
        summarize() %>%
        collect()

    DBI::dbWriteTable(conn, raw_hr_meta, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)

    writeLines("############ wrote raw hr meta ############")

}

write_fs_meta_data <- function(conn, fs_table, fs_table_meta) {

    writeLines("############ wrote clean hr meta ############")

    # convert raw hr to meta with userid,name
    #
    # Args:
    #   fs_table: name of raw fs table | string
    #   fs_table_meta: name of meta table for raw fs data | string

    session_meta_data <- tbl(conn, fs_table) %>%
            mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
            select(date, userid, time) %>%
            group_by(date, userid) %>%
            summarize() %>%
        collect()

    DBI::dbWriteTable(conn, fs_table_meta, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)

    writeLines("############ wrote fs meta ############")

}
