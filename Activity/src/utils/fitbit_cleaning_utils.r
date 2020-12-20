################################################################################
## This file contains the following fitbit cleaning functions:
##
## drop_indexes: drop_indexes functions returns vector with given indexes removed
## propDiff: propDiff functions returns proportinal difference between 2 values
## removeRepPointsdata: removeRepPointsdata functions that removes repeating values
## removeRandomPoints: removeRandomPoints functions that removes random values based on time
## remove70Leniant1: remove70Leniant1 functions that removes values of 70 that are errors based on values around them and time
## removeOutlierPoints: removeOutlierPoints functions that removes values based on being outlier value vs neighbourhood of values
################################################################################

drop_indexes <- function(data, vec){

    # remove given rows from a dataframe
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #   vec: vector, contains rows to remove from data
    #
    # Returns:
    #   dataRowsRemoved: same columns as data

    if (!is.null(vec)){

        data_rows_removed <- data[-c(vec), ]
    return(data_rows_removed)

    }else{
        return(data)
    }
}

prop_diff <- function(big, small){

    # calculate proportional diff between 2 numerical values
    #
    # Args:
    #   big: numeric
    #   small: numeric
    #
    # Returns:
    #   res: numeric

    res <- (abs(big - small) / small) * 100

    return(res)
}


remove_rep_points_data <- function(data){

    # remove repetive points from data frame
    # based on value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data

    rep <- 0
    rep_inds <- c()
    if (nrow(data) >= 3){

        for (i in 2:(nrow(data) - 1)){

            cur <- data$value[i]
            nex <- data$value[i + 1]

            if (cur == nex & i < nrow(data) - 1){

                rep <- rep + 1

            }else if ( (cur != nex & rep >= 10) |
                (i == nrow(data) - 1 & rep >= 10)){

                rep_inds <- c(rep_inds, (i - rep):i)
                rep <- 0
            }
            if (cur != nex){
                rep <- 0
            }

        }

        res <- drop_indexes(data, rep_inds)
        return (res)
    }else{
        return(data)
    }

}


remove_random_points <- function(data){

    # remove random points from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data

    diffs <- c(difftime(data$time[-1], data$time[-nrow(data)], units = "secs"))
    inds_big_diff <- which(diffs >= 120)

    if (length(inds_big_diff) > 0){

        inds_big_diff <- c(1, inds_big_diff, length(diffs))
        inds_to_remove <- c()

        for (i in 1:(length(inds_big_diff) - 1)){

            gap <- inds_big_diff[i + 1] - inds_big_diff[i]
            if (gap <= 20){

                f <- inds_big_diff[i] + 1
                c <- inds_big_diff[i + 1]
                inds_to_remove <- c(inds_to_remove, f:c)
            }
        }
        res <- drop_indexes(data, inds_to_remove)
        return (res)
    }else{
        return (data)
    }
}


remove70_leniant_1 <- function(data){
    # remove error 70 values from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data

    seventy <- which(data$value == 70)
    seventy <- sort(seventy)

    if (length(seventy) > 1){

        remove70s <- c()

        if (1 %in% seventy){

            remove70s <- c(remove70s, 1)
            i <- 2
            prev <- seventy[i - 1]
            cur <- seventy[i]

            while (cur == prev + 1 & i <= length(seventy) - 1){

                remove70s <- c(remove70s, cur)
                i <- i + 1
                prev <- seventy[i - 1]
                cur <- seventy[i]

            }
        }
        if (nrow(data) %in% seventy){

            remove70s <- c(remove70s, nrow(data))
            i <- length(seventy) - 1
            cur <- seventy[i]
            nex <- seventy[i + 1]

            while ( (nex == cur + 1) & (i >= 2)){

                remove70s <- c(remove70s, cur)
                nex <- i
                cur <- seventy[i - 1]
                i <- i - 1
            }
        }

        remove70s <- unique(remove70s)
        res <- drop_indexes(data, remove70s)
        return (res)

    }else{
        return(data)
    }
}

remove_outlier_points <- function(data){

    # remove outlier values from data frame
    # based on time and value
    #
    # Args:
    #   data: data.frame containing |"value" <num>|
    #       | "time" <timestamp> | "userid" <string> -> variable schema
    #
    # Returns:
    #   res: same columns as data

    prop <- abs(diff(data$value) / data$value[-length(data$value)]) * 100
        # get proportional difference of gaps

    diffs <- c(difftime(data$time[-1], data$time[-nrow(data)], units = "secs"))
    inds_big_gap <- which(prop >= 15)
    inds_to_remove <- c()
    if (length(inds_big_gap) > 1){

        for (i in 1:(length(inds_big_gap) - 1)){

            if (inds_big_gap[i] + 1 < nrow(data)){

                cur <- data$value[inds_big_gap[i] + 1]
                time_diff <- diffs[inds_big_gap[i]]
                gap <- inds_big_gap[i + 1] - inds_big_gap[i]

                if (gap <= 5 & cur < 100 & time_diff < 30){

                    f <- inds_big_gap[i] + 1
                    c <- inds_big_gap[i + 1]
                    inds_to_remove <- c(inds_to_remove, f:c)
                }
            }
        }
        res <- drop_indexes(data, inds_to_remove)
        return (res)
    }else{
        return(data)
    }

}
