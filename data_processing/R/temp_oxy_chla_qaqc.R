#NOTE - one day, it would be nice to change catdata to bvrdata in these scripts

temp_oxy_chla_qaqc <- function(realtime_file,
                               qaqc_file,
                               maintenance_file,
                               input_file_tz,
                               focal_depths,
                               local_tzone,
                               config){

  CATDATA_COL_NAMES <- c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                         "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                         "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                         "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                         "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                         "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1_5", "EXOCond_uScm_1_5",
                         "EXOSpCond_uScm_1_5", "EXOTDS_mgL_1_5", "EXODOsat_percent_1_5", "EXODO_mgL_1_5", "EXOChla_RFU_1_5",
                         "EXOChla_ugL_1_5", "EXOBGAPC_RFU_1_5", "EXOBGAPC_ugL_1_5", "EXOfDOM_RFU_1_5", "EXOfDOM_QSU_1_5",
                         "EXO_pressure_1_5", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper", "Lvl_psi_13", "LvlTemp_C_13")

  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1

  # columns where certain values are stored
  DO_MGL_COLS <- c(31, 18, 21)
  DO_SAT_COLS <- c(30, 19, 22)
  DO_FLAG_COLS <- c(46,47,48) 
  
  # depths at which DO is measured
  DO_DEPTHS <- c(1.5, 6, 13) #these are essentially meaningless because of the drastic water level changes...

  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4

  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  catdata <- readr::read_csv(realtime_file, skip = 4, col_names = CATDATA_COL_NAMES, 
                      col_types = readr::cols(.default = readr::col_double(), DateTime = readr::col_datetime()))

  log <- readr::read_tsv(maintenance_file, col_types = readr::cols(
    .default = readr::col_character(),
    TIMESTAMP_start = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = readr::col_integer()
  ))

  # remove NaN data at beginning
  catdata <- catdata %>% dplyr::filter(DateTime > ymd_hms("2020-08-13 11:30:00"))

  # add flag columns
  catdata$Flag_All <- 0
  catdata$Flag_DO_1_5 <- 0
  catdata$Flag_DO_6 <- 0
  catdata$Flag_DO_13 <- 0
  catdata$Flag_Chla <- 0
  catdata$Flag_Phyco <- 0
  catdata$Flag_TDS <- 0
  catdata$Flag_fDOM <- 0
  catdata$Flag_Cond <-0
  catdata$Flag_Lvl_13 <-0
  catdata$Flag_Temp_1 <-0
  catdata$Flag_Temp_2 <-0
  catdata$Flag_Temp_3 <-0
  catdata$Flag_Temp_4 <-0
  catdata$Flag_Temp_5 <-0
  catdata$Flag_Temp_6 <-0
  catdata$Flag_Temp_7 <-0
  catdata$Flag_Temp_8 <-0
  catdata$Flag_Temp_9 <-0
  catdata$Flag_Temp_10 <-0
  catdata$Flag_Temp_11 <-0
  catdata$Flag_Temp_12 <-0
  catdata$Flag_Temp_13 <-0
  
  # replace negative DO values with 0
  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_1_5 = ifelse((! is.na(EXODO_mgL_1_5) & EXODO_mgL_1_5 < 0)
                              | (! is.na(EXODOsat_percent_1_5) & EXODOsat_percent_1_5 < 0), 3, Flag_DO_1_5)) %>%
    dplyr::mutate(EXODO_mgL_1_5 = ifelse(EXODO_mgL_1_5 < 0, 0, EXODO_mgL_1_5)) %>%
    dplyr::mutate(EXODOsat_percent_1_5 = ifelse(EXODOsat_percent_1_5 <0, 0, EXODOsat_percent_1_5))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_6 = ifelse((! is.na(RDO_mgL_6) & RDO_mgL_6 < 0)
                              | (! is.na(RDOsat_percent_6) & RDOsat_percent_6 < 0), 3, Flag_DO_6)) %>%
    dplyr::mutate(RDO_mgL_6 = ifelse(RDO_mgL_6 < 0, 0, RDO_mgL_6)) %>%
    dplyr::mutate(RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0, 0, RDOsat_percent_6))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_13 = ifelse((! is.na(RDO_mgL_13) & RDO_mgL_13 < 0)
                              | (! is.na(RDOsat_percent_13) & RDOsat_percent_13 < 0), 3, Flag_DO_13)) %>%
    dplyr::mutate(RDO_mgL_13 = ifelse(RDO_mgL_13 < 0, 0, RDO_mgL_13)) %>%
    dplyr::mutate(RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0, 0, RDOsat_percent_13))

  # modify catdata based on the information in the log
  for(i in 1:nrow(log)){
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]

    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:39), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:39), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:39), c(bounds[1]:bounds[2]))
    }
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }

    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(21, 22))

    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }

    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- NA
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, "Flag_All"] <- 1

    # if DO data was affected by maintenance, set the appropriate DO flags, and replace DO data with NAs after maintenance
    # was in effect until value returns to within a threshold of the value when maintenance began, because the sensors take
    # time to re-adjust to ambient conditions
    last_row_before_maintenance <- tail(catdata %>% filter(DateTime < start), 1)
    for(j in 1:3)
    {
      # if maintenance was not in effect on DO data, then skip
      if(! (DO_MGL_COLS[j] %in% maintenance_cols | DO_SAT_COLS[j] %in% maintenance_cols))
      {
        next
      }

      # set the appropriate DO flag while maintenance was in effect
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, DO_FLAG_COLS[j]] <- 1

      last_DO_before_maintenance <- last_row_before_maintenance[[DO_MGL_COLS[j]]][1]
      if(is.na(last_DO_before_maintenance))
      {
        warning(paste("For row", i, "of the maintenance log, the pre-maintenance DO value at depth", DO_DEPTHS[j],
                      "could not be found. Not replacing DO values after the end of maintenance. This could occur because the start",
                      "date-time for maintenance is at or before the first date-time in the data, or simply because the value was",
                      "missing or replaced in prior maintenance."))
      }
      else
      {
        DO_recovery_time <- (catdata %>%
                               dplyr::filter(DateTime > end &
                                        abs(catdata[[DO_MGL_COLS[j]]] - last_DO_before_maintenance) <= DO_RECOVERY_THRESHOLD)
        )$DateTime[1]

        # if the recovery time cannot be found, then raise a warning and replace post-maintenance DO values until the end of
        # the file
        if(is.na(DO_recovery_time))
        {
          warning(paste("For row", i, "of the maintenance log, post-maintenance DO levels at depth", DO_DEPTHS[j],
                        "never returned within the given threshold of the pre-maintenance DO value. All post-maintenance DO values",
                        "have been replaced with NA. This could occur because the end date-time for maintenance is at or after the",
                        "last date-time in the data, or simply because post-maintenance levels never returned within the threshold."))
          catdata[catdata$DateTime > end, intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end, DO_FLAG_COLS[j]] <- 1
        }
        else
        {
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time,
                  intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }

  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October and March, due to sensor fouling
  Chla_RFU_1_5_mean <- mean(catdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_mean <- mean(catdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_mean <- mean(catdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_mean <- mean(catdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)
  Chla_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)

  catdata <- catdata %>%
    dplyr::mutate(Flag_Chla = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                (! is.na(EXOChla_RFU_1_5) & abs(EXOChla_RFU_1_5 - Chla_RFU_1_5_mean) > Chla_RFU_1_5_threshold |
                                   ! is.na(EXOChla_ugL_1_5) & abs(EXOChla_ugL_1_5 - Chla_ugL_1_5_mean) > Chla_ugL_1_5_threshold),
                              4, Flag_Chla)) %>%
    dplyr::mutate(Flag_Phyco = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1_5) & abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold |
                                    ! is.na(EXOBGAPC_ugL_1_5) & abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold),
                               4, Flag_Phyco)) %>%
    dplyr::mutate(EXOChla_RFU_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                    abs(EXOChla_RFU_1_5 - Chla_RFU_1_5_mean) > Chla_RFU_1_5_threshold, NA, EXOChla_RFU_1_5)) %>%
    dplyr::mutate(EXOChla_ugL_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                    abs(EXOChla_ugL_1_5 - Chla_ugL_1_5_mean) > Chla_ugL_1_5_threshold, NA, EXOChla_ugL_1_5)) %>%
    dplyr::mutate(EXOBGAPC_RFU_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                     abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold, NA, EXOBGAPC_RFU_1_5)) %>%
    dplyr::mutate(EXOBGAPC_ugL_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                     abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold, NA, EXOBGAPC_ugL_1_5))

  # flag EXO sonde sensor data of value above 4 * standard deviation at other times
  catdata <- catdata %>%
    dplyr::mutate(Flag_Phyco = ifelse(! is.na(EXOBGAPC_RFU_1_5) & abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold |
                                 ! is.na(EXOBGAPC_ugL_1_5) & abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold,
                               5, Flag_Phyco))

  #create depth column
  catdata=catdata%>%mutate(Depth_m_13=Lvl_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  #note that there are a decent amount of negative depths at 13m so unless BVR drained, this isn't the most robust method w/o additional QAQC
  
  # temp qaqc ----
  
  #Setting the temperature to NA when the thermistors are out of the water
  #add a depth column which we set NAs for when the thermistor is out of the water. Flag 2
  catdata=catdata%>%
    mutate(depth_1=Depth_m_13-11.82)%>%
    mutate(depth_2=Depth_m_13-11.478)
  
  
  #change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
  #negative depths are changed to NA
  #the date ifelse statement is when the pressure transducer was unplugged
  
  #for thermistor at position 1 when it was out of the water 
  catdata=catdata%>%
    mutate(Flag_Temp_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_1))%>%
    mutate(ThermistorTemp_C_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_1))%>%
    mutate(Flag_Temp_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_Temp_1))%>%
    mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))
  
  
  #for thermistor at position 2 when it was out of the water 
  catdata=catdata%>%
    mutate(Flag_Temp_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_2))%>%#this is when the pressure sensor was unplugged
    mutate(ThermistorTemp_C_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_2))%>%
    mutate(Flag_Temp_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_Temp_2))%>%
    mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))
  
  
  #take out the depth columns for thermisotrs 1 and 2 after you set the values to NA
  catdata=catdata%>%
    dplyr::select(-depth_1,-depth_2)
  
  catdata=catdata%>%
    mutate(Flag_Temp_11=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_11))%>%
    mutate(Flag_Temp_12=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_12))%>%
    mutate(Flag_Temp_13=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_13))
  
  #add flags and set to NA 
  catdata=catdata%>%
    mutate(Flag_Phyco= ifelse(DateTime=="2020-08-01 12:50:00 tz=Etc/GMT+5", 5, Flag_Phyco))%>%
    mutate(Flag_Phyco= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
    mutate(Flag_Phyco= ifelse(DateTime=="2020-12-25 06:20:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
    mutate(EXOBGAPC_RFU_1_5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_RFU_1_5))%>%
    mutate(EXOBGAPC_ugL_1_5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1_5))%>%
    mutate(EXOBGAPC_RFU_1_5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5",NA, EXOBGAPC_RFU_1_5))%>%
    mutate(EXOBGAPC_ugL_1_5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1_5))
  
  #Add flags when the wire was not connected
  catdata=catdata%>%
    mutate(Flag_Lvl_13=ifelse(is.na(Lvl_psi_13)& DateTime>="2020-10-26 12:00:00" & DateTime<="2020-10-30 09:40:00",
                              7, Flag_Lvl_13))
  
  # reorder columns
  catdata <- catdata %>% dplyr::select(-RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1_5, -Flag_DO_6,
                                       -Flag_DO_13, -Flag_Chla, -Flag_Phyco, -Flag_TDS, -EXO_wiper, -EXO_cablepower,
                                       -EXO_battery,-EXO_pressure_1_5)
  
  # replace NaNs with NAs
  catdata[is.na(catdata)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.character(catdata$DateTime)
  
  if(!is.na(realtime_file)){
    #Different lakes are going to have to modify this for their temperature data format

    d1 <- catdata

    if(!is.na(qaqc_file)){
    d2 <- read.csv(qaqc_file, na.strings = 'NA', stringsAsFactors = FALSE)
    
    #subset d1 to only dates in d2
    d1 <- d1[d1$DateTime %in% d2$DateTime,]
    d2 <- d2[d2$DateTime %in% d1$DateTime,]
    }

    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = "UTC")

    d3 <-  data.frame(TIMESTAMP = d1$TIMESTAMP, 
                      wtr_1 = d1$ThermistorTemp_C_1, wtr_2 = d1$ThermistorTemp_C_2,
                      wtr_3 = d1$ThermistorTemp_C_3, wtr_4 = d1$ThermistorTemp_C_4,
                      wtr_5 = d1$ThermistorTemp_C_5, wtr_6 = d1$ThermistorTemp_C_6,
                      wtr_7 = d1$ThermistorTemp_C_7, wtr_8 = d1$ThermistorTemp_C_8,
                      wtr_9 = d1$ThermistorTemp_C_9, wtr_10 = d1$ThermistorTemp_C_10,
                      wtr_11 = d1$ThermistorTemp_C_11, wtr_12 = d1$ThermistorTemp_C_12,
                      wtr_13 = d1$ThermistorTemp_C_13, wtr_1_5_exo = d1$EXOTemp_C_1_5,
                      wtr_6_do = d1$RDOTemp_C_6, wtr_13_do = d1$RDOTemp_C_13,
                      Chla_1_5 = d1$EXOChla_ugL_1_5, doobs_1_5 = d1$EXODO_mgL_1_5,
                      doobs_6 = d1$RDO_mgL_6, doobs_13 = d1$RDO_mgL_13,
                      fDOM_1_5 = d1$EXOfDOM_QSU_1_5, bgapc_1_5 = d1$EXOBGAPC_ugL_1_5,
                      depth_1_5 = d1$EXO_depth)
    
    if(!is.na(qaqc_file)){
    #TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = "UTC") #NA

    #d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]

    #d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, wtr_surf, wtr_1 = d1$wtr_1, wtr_2 = d1$wtr_2, wtr_3 = d1$wtr_3, wtr_4 = d1$wtr_4,
    #                 wtr_5 = d1$wtr_5, wtr_6 = d1$wtr_6, wtr_7 = d1$wtr_7, wtr_8 = d1$wtr_8, wtr_9 = d1$wtr_9, wtr_10 = d1$wtr_10, wtr_11 = d1$wtr_11, 
    #                 wtr_12 = d1$wtr_12, wtr_13 = d1$wtr_13, wtr_1_exo = d1$EXO_wtr_1, wtr_6_do = d1$dotemp_6, wtr_13_do = d1$dotemp_13)

     d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, 
                      wtr_1 = d2$ThermistorTemp_C_1, wtr_2 = d2$ThermistorTemp_C_2,
                      wtr_3 = d2$ThermistorTemp_C_3, wtr_4 = d2$ThermistorTemp_C_4,
                      wtr_5 = d2$ThermistorTemp_C_5, wtr_6 = d2$ThermistorTemp_C_6,
                      wtr_7 = d2$ThermistorTemp_C_7, wtr_8 = d2$ThermistorTemp_C_8,
                      wtr_9 = d2$ThermistorTemp_C_9, wtr_10 = d2$ThermistorTemp_C_10,
                      wtr_11 = d2$ThermistorTemp_C_11, wtr_12 = d2$ThermistorTemp_C_12,
                      wtr_13 = d2$ThermistorTemp_C_13, wtr_1_5_exo = d2$EXOTemp_C_1_5,
                      wtr_6_do = d2$RDOTemp_C_6, wtr_13_do = d2$RDOTemp_C_13,
                      Chla_1_5 = d2$EXOChla_ugL_1_5, doobs_1_5 = d2$EXODO_mgL_1_5,
                      doobs_6 = d2$RDO_mgL_6, doobs_13 = d2$RDO_mgL_13,
                      fDOM_1_5 = d2$EXOfDOM_QSU_1_5, bgapc_1_5 = d2$EXOBGAPC_ugL_1_5,
                      depth_1_5 = d2$EXO_depth)

    d <- rbind(d3,d4)
    } else{
      d <- d3
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- catdata

    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = "UTC")

    d <-  data.frame(TIMESTAMP = d1$DateTime, 
                     wtr_1 = d1$ThermistorTemp_C_1, wtr_2 = d1$ThermistorTemp_C_2,
                     wtr_3 = d1$ThermistorTemp_C_3, wtr_4 = d1$ThermistorTemp_C_4,
                     wtr_5 = d1$ThermistorTemp_C_5, wtr_6 = d1$ThermistorTemp_C_6,
                     wtr_7 = d1$ThermistorTemp_C_7, wtr_8 = d1$ThermistorTemp_C_8,
                     wtr_9 = d1$ThermistorTemp_C_9, wtr_10 = d1$ThermistorTemp_C_10,
                     wtr_11 = d1$ThermistorTemp_C_11, wtr_12 = d1$ThermistorTemp_C_12,
                     wtr_13 = d1$ThermistorTemp_C_13, wtr_1_5_exo = d1$EXOTemp_C_1_5,
                     wtr_6_do = d1$RDOTemp_C_6, wtr_13_do = d1$RDOTemp_C_13,
                     Chla_1_5 = d1$EXOChla_ugL_1_5, doobs_1_5 = d1$EXODO_mgL_1_5,
                     doobs_6 = d1$RDO_mgL_6, doobs_13 = d1$RDO_mgL_13,
                     fDOM_1_5 = d1$EXOfDOM_QSU_1_5, bgapc_1_5 = d1$EXOBGAPC_ugL_1_5,
                     depth_1_5 = d1$EXO_depth)
  }


  d$fDOM_1_5 <- config$exo_sensor_2_grab_sample_fdom[1] + config$exo_sensor_2_grab_sample_fdom[2] * d$fDOM_1_5

  #oxygen unit conversion
  d$doobs_1_5 <- d$doobs_1_5*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  d$doobs_6 <- d$doobs_6*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  d$doobs_13 <- d$doobs_13*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)

  d$Chla_1_5 <-  config$exo_sensor_2_ctd_chla[1] +  d$Chla_1_5 *  config$exo_sensor_2_ctd_chla[2]
  d$doobs_1_5 <- config$exo_sensor_2_ctd_do_1_5[1]  +   d$doobs_1_5 * config$exo_sensor_2_ctd_do_1[2]
  d$doobs_6 <- config$do_sensor_2_ctd_do_6[1] +   d$doobs_6 * config$do_sensor_2_ctd_do_6[2]
  d$doobs_13 <- config$do_sensor_2_ctd_do_13[1] +   d$doobs_13 * config$do_sensor_2_ctd_do_13[2]

  d <- d %>%
    dplyr::mutate(day = day(TIMESTAMP),
           year = year(TIMESTAMP),
           hour = hour(TIMESTAMP),
           month = month(TIMESTAMP)) %>%
    dplyr::group_by(day, year, hour, month) %>%
    dplyr::select(-TIMESTAMP) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day = as.numeric(day),
           hour = as.numeric(hour)) %>%
    dplyr::mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day),
           hour = ifelse(as.numeric(hour) < 10, paste0("0",hour),hour)) %>%
    dplyr::mutate(timestamp = as_datetime(paste0(year,"-",month,"-",day," ",hour,":00:00"),tz = "UTC")) %>%
    dplyr::arrange(timestamp)


  d_therm <- d %>%
    dplyr::select(timestamp, wtr_3, wtr_4, wtr_5, wtr_6, #dropping wtr_1, wtr_2
           wtr_7,wtr_8, wtr_9, wtr_10, wtr_11, wtr_12, wtr_13) %>%
    dplyr::rename(
           #"1.0" = wtr_1,
           #"2.0" = wtr_2,
           "1.0" = wtr_3,
           "2.0" = wtr_4,
           "3.0" = wtr_5,
           "4.0" = wtr_6,
           "5.0" = wtr_7,
           "6.0" = wtr_8,
           "7.0" = wtr_9,
           "8.0" = wtr_10,
           "9.0" = wtr_11,
           "10.0" = wtr_12,
           "11.0" = wtr_13) %>%
    tidyr::pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    dplyr::mutate(variable = "temperature",
           method = "thermistor",
           value = ifelse(is.nan(value), NA, value))


  d_do_temp <- d %>%
    dplyr::select(timestamp, wtr_6_do, wtr_13_do) %>%
    dplyr::rename("4.0" = wtr_6_do,
           "11.0" = wtr_13_do) %>%
    tidyr::pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    dplyr::mutate(variable = "temperature",
           method = "do_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_temp <- d %>%
    dplyr::select(timestamp, wtr_1_5_exo) %>%
    rename("1.0" = wtr_1_5_exo) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "temperature",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_do_do <- d %>%
    dplyr::select(timestamp, doobs_6, doobs_13) %>%
    rename("4.0" = doobs_6,
           "11.0" = doobs_13) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "oxygen",
           method = "do_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_do <- d %>%
    dplyr::select(timestamp, doobs_1_5) %>%
    rename("1.0" = doobs_1_5) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "oxygen",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_fdom <- d %>%
    dplyr::select(timestamp, fDOM_1_5) %>%
    rename("1.0" = fDOM_1_5) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "fdom",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_chla <- d %>%
    dplyr::select(timestamp, Chla_1_5) %>%
    rename("1.0" = Chla_1_5) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "chla",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_bgapc <- d %>%
    dplyr::select(timestamp, bgapc_1_5) %>%
    rename("1.0" = bgapc_1_5) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "bgapc",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  if(config$variable_obsevation_depths == TRUE){

    d_depth <- d %>% mutate(wtr_surface = depth_1_5 - 0.5 - 0.9,
                            wtr_1 = depth_1_5 - 0.5,
                            wtr_2 = depth_1_5 + 0.5,
                            wtr_3 = depth_1_5 + 2.5,
                            wtr_4 = depth_1_5 + 2.5,
                            wtr_5 = depth_1_5 + 3.5,
                            wtr_6 = depth_1_5 + 4.5,
                            wtr_7 = depth_1_5 + 5.5,
                            wtr_8 = depth_1_5 + 6.5,
                            wtr_9 = depth_1_5 + 7.5,
                            wtr_10 = depth_1_5 + 8.5,
                            wtr_11 = depth_1_5 + 9.5,
                            wtr_12 = depth_1_5 + 10.5,
                            wtr_13 = depth_1_5 + 11.5,
                            wtr_1_5_exo = depth_1_5,
                            wtr_6_do = depth_1_5 + 4.5,
                            wtr_13_do = depth_1_5 + 11.5,
                            Chla_1_5 = depth_1_5,
                            doobs_1_5 = depth_1_5,
                            doobs_6 = depth_1_5 + 4.5,
                            doobs_13 = depth_1_5 + 11.5,
                            fDOM_1 = depth_1_5,
                            bgapc_1 = depth_1_5) %>%
      dplyr::select(-c(depth_1_5, day,year, hour, month))


    d_therm_depth <- d_depth %>%
      dplyr::select(timestamp, wtr_surface, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6,
             wtr_7,wtr_8, wtr_9, wtr_10, wtr_11, wtr_12, wtr_13) %>%
      rename("NA" = wtr_surface, #what us wtr_surface?
             "NA" = wtr_1,
             "NA" = wtr_2,
             "1.0" = wtr_3,
             "2.0" = wtr_4,
             "3.0" = wtr_5,
             "4.0" = wtr_6,
             "5.0" = wtr_7,
             "6.0" = wtr_8,
             "7.0" = wtr_9,
             "8.0" = wtr_10,
             "9.0" = wtr_11,
             "10.0" = wtr_12,
             "11.0" = wtr_13) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "thermistor",
             depth_exo = ifelse(is.nan(depth_exo), NA, depth_exo))

    d_therm <- d_therm %>%
      left_join(d_therm_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_do_temp_depth <- d_depth %>%
      dplyr::select(timestamp, wtr_6_do, wtr_13_do) %>%
      rename("6.0" = wtr_6_do,
             "13.0" = wtr_13_do) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "do_sensor")

    d_do_temp <- d_do_temp %>%
      left_join(d_do_temp_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_temp_depth <- d_depth %>%
      dplyr::select(timestamp, wtr_1_5_exo) %>%
      rename("1_5" = wtr_1_5_exo) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "exo_sensor")

    d_exo_temp <- d_exo_temp %>%
      left_join(d_do_temp_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_do_do_depth <- d_depth %>%
      dplyr::select(timestamp, doobs_6, doobs_13) %>%
      rename("6.0" = doobs_6,
             "13.0" = doobs_13) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "do_sensor")

    d_do_do <- d_do_do %>%
      left_join(d_do_do_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_do_depth <- d_depth %>%
      dplyr::select(timestamp, doobs_1_5) %>%
      rename("1.5" = doobs_1_5) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "exo_sensor")

    d_exo_do <- d_exo_do %>%
      left_join(d_exo_do_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_fdom_depth <- d_depth %>%
      dplyr::select(timestamp, fDOM_1_5) %>%
      rename("1.5" = fDOM_1_5) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "exo_sensor")

    d_exo_fdom <- d_exo_fdom %>%
      left_join(d_exo_fdom_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_chla_depth <- d_depth %>%
      dplyr::select(timestamp, Chla_1_5) %>%
      rename("1.5" = Chla_1_5) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "chla",
             method = "exo_sensor")

    d_exo_chla <- d_exo_chla %>%
      left_join(d_exo_chla_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_bgapc_depth <- d_depth %>%
      dplyr::select(timestamp, bgapc_1_5) %>%
      rename("1.5" = bgapc_1_5) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "bgapc",
             method = "exo_sensor")

    d_exo_bgapc <- d_exo_bgapc %>%
      left_join(d_exo_chla_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      dplyr::select(-depth_exo) %>%
      filter(depth > 0.0)
  }


  d <- rbind(d_therm,d_do_temp,d_exo_temp,d_do_do,d_exo_do,d_exo_fdom,
             d_exo_chla,d_exo_bgapc)

  d <- d %>% mutate(depth = depth)

#drop wtr_1 and wtr_2
  d <- d[d$depth!="wtr_1" & d$depth!="wtr_2",]

  # write to output file
  return(d)
}


#figs
#plot(d$timestamp[d$variable=="oxygen" & d$depth==1.5], d$value[d$variable=="oxygen"& d$depth==1.5],col="magenta", xlab="",ylab="Dissolved oxygen (mg/L)",type="l", ylim=c(0,440))
#points(d$timestamp[d$variable=="oxygen" & d$depth=="6.0"], d$value[d$variable=="oxygen"& d$depth=="6.0"],col="black",type="l")
#points(d$timestamp[d$variable=="oxygen" & d$depth=="13.0"], d$value[d$variable=="oxygen"& d$depth=="13.0"],col="mediumseagreen",type="l")
#legend("bottomright", legend=c("1m", "6m", "13m"), text.col=c("magenta","black","mediumseagreen"), bty='n')

# example usage
# qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#      "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#      "Catwalk.csv")
