saved_file <- "/Users/heatherwander/Documents/VirginiaTech/research/BVR_GLM/BVRE-forecast/glm/bvre_H_2019_07_29_2019_07_29_F_16_20210406T164552.nc"
qaqc_data_location <- file.path(lake_directory,"data_processing/qaqc_data")

#file_name <- saved_file
flare::plotting_general(file_name = saved_file,
                        qaqc_location = qaqc_data_location)

visualization_location <- file.path(lake_directory,"visualization")
source(paste0(visualization_location,"/manager_plot.R"))

manager_plot(file_name = saved_file,
             qaqc_location = qaqc_data_location,
             focal_depths = c(1, 5, 8))
