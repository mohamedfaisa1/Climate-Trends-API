# Function to create the station dataset that holds information about all the
# stations in the dataset and the weather dataset which holds all
# information of the weather dataset.

create_datasets <- function(direc){
    # original & main directories to get all directories and store
    # in file.dirs
    od <- getwd()
    md <- paste0(od, direc)
    file.dirs <- list.dirs(path=md, recursive=F)

    # make empty datasets
    station.info <- data.frame(station_id=character(0),
                   station_name=character(0),
                   state=character(0),
                   longitude=numeric(0),
                   latitude=numeric(0))

    weather.info <- data.frame(WBANNO=numeric(0),
                    state=character(0),
                    station_name=character(0),
                    LST_DATE=as.Date(as.character(numeric(0)),
                                     format="%Y%m%d"),
                    CRX_VN=character(0),
                    LONGITUDE=numeric(0),
                    LATITUDE=numeric(0),
                    T_DAILY_MAX=numeric(0),
                    T_DAILY_MIN=numeric(0),
                    T_DAILY_MEAN=numeric(0),
                    T_DAILY_AVG=numeric(0),
                    P_DAILY_CALC=numeric(0),
                    SOLARAD_DAILY=numeric(0))

    # loop over directories and fill in datasets information
    n <- length(file.dirs)
    stat_names_in <- c()
    wbanno_in <- c()

    for(i in 1:n){
      file.dr <- file.dirs[i]
      file.list <- list.files(path=file.dr, pattern=".txt", full.names=T)
      files_df <- lapply(file.list, function(x){read.table(x)[1:11]})
      fnames <- lapply(file.list,
                        function(x){strsplit(x, "\\d-")[[1]][4]})
      station_names <- unlist(lapply(fnames, function(x){strsplit(x, ".txt")}))
      states <- unlist(lapply(station_names,
                        function(x){strsplit(x, "_")[[1]][1]}))

      k <- length(file.list)
      for(j in 1:k){
        wban <- files_df[[j]][1]
        nrs <- nrow(wban)
        sts <- rep(states[j], nrs)
        stns <- rep(station_names[j], nrs)
        remaining.cols <- files_df[[j]][2:11]
        remaining.cols[1] <- as.Date(as.character(unlist(remaining.cols[1])),
                                     format="%Y%m%d")
        weather_df <- cbind.data.frame(wban, sts, stns, remaining.cols)
        weather.info <- rbind.data.frame(weather.info, weather_df)

        wbanno <- unlist(wban)[1]
        st <- sts[1]
        stn <- stns[1]
        lon <- unlist(remaining.cols[3])[1]
        lat <- unlist(remaining.cols[4])[1]

        if(!(stn %in% stat_names_in)){
          station.cols <- cbind.data.frame(wbanno, stn, st, lon, lat)
          station.info <- rbind.data.frame(station.info, station.cols)
        }
        stat_names_in <- c(stat_names_in, stn)
      }
    }
    ## FINAL EDITS TO DATA

    rownames(weather.info) <- NULL
    colnames(weather.info) <- c("WBANNO", "state", "station_name",
                  "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE", "T_DAILY_MAX",
                  "T_DAILY_MIN", "T_DAILY_MEAN", "T_DAILY_AVG",
                  "P_DAILY_CALC", "SOLARAD_DAILY")
    # making CRX_VN a character vector
    weather.info$CRX_VN <- as.character(weather.info$CRX_VN)


    # replacing missing data w/ NA
    replace_cols <- function(x){
      inds <- which(x == -9999 | x == -99)
      n.x <- replace(x, inds, NA)
      return(list(n.x))
    }

    name.list <- colnames(weather.info)
    for(ii in name.list){
      x <- weather.info[, ii]
      weather.info[, ii] <- replace_cols(x)
    }


    rownames(station.info) <- NULL
    colnames(station.info) <- c("station_id", "station_name", "state",
                              "LONGITUDE", "LATITUDE")

    # remove "AK_Huslia_27_W" (empty station)
    i.w <- which(weather.info$station_name == "AK_Huslia_27_W")
    weather.info <- weather.info[-i.w,]

    i.s <- which(station.info$station_name == "AK_Huslia_27_W")
    station.info <- station.info[-i.s,]


    return(list(weather.info, station.info))
}

dat <- create_datasets("/raw_data/CRND0103-202404080750")


weather_data <- dat[[1]]
save(weather_data, file="./raw_data/weather_data.RData")

station_data <- dat[[2]]
save(station_data, file="./raw_data/station_data.RData")
