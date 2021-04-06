lake_directory <- getwd()

system("git clone https://github.com/FLARE-forecast/BVRE-data.git")


config <- yaml::read_yaml(file.path(lake_directory, "data_processing/observation_processing.yml"))

config$data_location <- file.path(lake_directory,"BVRE-data")

#download BVR temp data from EDI
#inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=9f4d77dc90db2d87e4cdec8b7584d504" 
#infile1 <- paste0(config$data_location,"/BVR_EDI_2020.csv")
#download.file(inUrl1,infile1,method="curl")

#inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=1ca1080101e38fb2b4bb9455c1faed5b" 
#infile1 <- paste0(config$data_location,"/BVR_Maintenance_2020.csv")
#download.file(inUrl1,infile1,method="curl")

#inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=e05f13c7e54a6e9a3b19a03a37a54e3d" 
#infile1 <- paste0(config$data_location,"/BVR_Depth_offsets_2020.csv")
#download.file(inUrl1,infile1,method="curl") #not sure if I need this, but can get depth of sensors by subtracting offsets from Depth_m_13 in BVR_EDI_2020.csv

#check data files on computer
if(!file.exists(file.path(config$data_location, config$realtime_insitu_location))){
    stop("Missing temperature data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$realtime_met_station_location))){
  stop("Missing met station data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$noaa_location))){
  stop("Missing NOAA forecast GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$manual_data_location))){
  stop("Missing Manual data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$realtime_inflow_data_location))){
  stop("Missing Inflow data GitHub repo")
}

#update git data files if necessary
setwd(file.path(config$data_location, config$realtime_insitu_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$realtime_met_station_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$noaa_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$manual_data_location))
system(paste0("git pull"))

#setwd(file.path(config$data_location, config$realtime_inflow_data_location))
#system(paste0("git pull"))
