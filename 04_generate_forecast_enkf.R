# remotes::install_github("FLARE-forecast/GLM3r")
# remotes::install_github("FLARE-forecast/FLAREr")

lake_directory <- getwd()
forecast_location <- file.path(lake_directory, "glm")

config <- yaml::read_yaml(file.path(forecast_location, "configuration_files","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(forecast_location, "configuration_files","run_configuration.yml"))

config$run_config <- run_config
config$run_config$forecast_location <- forecast_location
config$data_location <- file.path(lake_directory,"BVRE-data")
config$qaqc_data_location <- file.path(lake_directory,"data_processing/qaqc_data")

config$ensemble_size <- 210
parallel::detectCores() # Check number of cores available on your
config$ncore <- 2 # Set number of cores to run FLARE on

da_method <- "enkf"
par_fit_method <- "inflate" # inflate perturb

sel_pars <- c("Kw", "wind_factor")

# Set up timings
start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
if(is.na(config$run_config$forecast_start_day_local)){
  end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  forecast_start_datetime_local <- end_datetime_local
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
}

#Weather Drivers
start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
noaa_forecast_path <- file.path(config$data_location, config$forecast_met_model,"fcre",lubridate::as_date(forecast_start_datetime_UTC),forecast_hour)

pacman::p_load(dplyr, lubridate, noaaGEFSpoint, magrittr)

forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)

if(length(forecast_files) > 0){
  
  message("Forecasting inflow and outflows")
  source(paste0(lake_directory, "/inflow_outflows/forecast_inflow_outflows.R"))
  #Forecast Inflows
  forecast_inflows_outflows(inflow_obs =  file.path(config$qaqc_data_location,"inflow_postQAQC.csv"),
                           forecast_files = forecast_files,
                           obs_met_file = file.path(config$qaqc_data_location,"observed-met_fcre.nc"),
                           output_dir = config$data_location,
                           inflow_model = config$forecast_inflow_model,
                           inflow_process_uncertainty = FALSE,
                           forecast_location = config$run_config$forecast_location)
  
  if(!dir.exists(config$run_config$execute_location)){
    dir.create(config$run_config$execute_location)
  }
  
  pars_config <- readr::read_csv(file.path(config$run_config$forecast_location, "configuration_files", config$par_file), col_types = readr::cols())
  pars_config <- pars_config[which(pars_config$par_names %in% sel_pars), ]
  pars_config$perturb_par <- pars_config$perturb_par * 2
  
  obs_config <- readr::read_csv(file.path(config$run_config$forecast_location,
                                          "configuration_files", config$obs_config_file),
                                col_types = readr::cols())
  obs_config$obs_sd <- 0.5
  
  states_config <- readr::read_csv(file.path(config$run_config$forecast_location, "configuration_files", config$states_config_file), col_types = readr::cols())
  
  # states_process <-  readr::read_csv(file.path(config$run_config$forecast_location, "configuration_files", "states_process_error.csv"), col_types = readr::cols())
  
  
  
  # Set up timings
  start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  if(is.na(config$run_config$forecast_start_day_local)){
    end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
    forecast_start_datetime_local <- end_datetime_local
  }else{
    forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
    end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
  }
  
  
  #Download and process observations (already done)
  
  cleaned_observations_file_long <- file.path(config$qaqc_data_location,"observations_postQAQC_long.csv")
  cleaned_inflow_file <- file.path(config$qaqc_data_location, "inflow_postQAQC.csv")
  observed_met_file <- file.path(config$qaqc_data_location,"observed-met_fcre.nc")
  
  #Step up Drivers
  
  #Weather Drivers
  start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
  end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
  forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
  forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
  if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
  forecast_path <- file.path(config$data_location, "NOAAGEFS_1hr",config$lake_name_code,lubridate::as_date(forecast_start_datetime_UTC),forecast_hour)
  
  met_out <- flare::generate_glm_met_files(obs_met_file = observed_met_file,
                                           out_dir = config$run_config$execute_location,
                                           forecast_dir = noaa_forecast_path,
                                           local_tzone = config$local_tzone,
                                           start_datetime_local = start_datetime_local,
                                           end_datetime_local = end_datetime_local,
                                           forecast_start_datetime = forecast_start_datetime_local,
                                           use_forecasted_met = TRUE)
  
  met_file_names <- met_out$filenames
  historical_met_error <- met_out$historical_met_error
  
  #Inflow Drivers (already done)
  
  inflow_forecast_path <- file.path(config$data_location, config$forecast_inflow_model,config$lake_name_code,lubridate::as_date(forecast_start_datetime_UTC),forecast_hour)
  
  inflow_outflow_files <- flare::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                 inflow_obs = cleaned_inflow_file,#paste0(config$data_location,"/FLOWS-NOAAGEFS-TMWB/bvre/2021-03-01/12/INFLOW-FLOWS-NOAAGEFS-TMWB_bvre_2021-03-01_2021-03-16_ens00.csv"),
                                                                 working_directory =config$run_config$execute_location,
                                                                 start_datetime_local = start_datetime_local,
                                                                 end_datetime_local = end_datetime_local,
                                                                 forecast_start_datetime_local = forecast_start_datetime_local,
                                                                 use_future_inflow = TRUE,
                                                                 state_names = NULL)
  
  inflow_file_names <- config$specified_inflow1 
  outflow_file_names <- config$specified_outflow1
  
  cleaned_observations_file_long <- file.path(config$qaqc_data_location, "observations_postQAQC_long.csv")
  # obs_config$distance_threshold <- 0.5 #  Play around with this to see which produces more observations
  
  obs <- flare::create_obs_matrix(cleaned_observations_file_long, 
                                  obs_config,                   
                                  start_datetime_local,           
                                  end_datetime_local,
                                  local_tzone = config$local_tzone,
                                  modeled_depths = config$modeled_depths)
  obs[1,,] # View observations and check for NA's
  
  #Set observations in the "future" to NA
  full_time_forecast <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  obs[ , which(full_time_forecast > forecast_start_datetime_local), ] <- NA
  
  
  states_config <- flare::generate_states_to_obs_mapping(states_config, obs_config)
  
  config_file_location <- file.path(config$run_config$forecast_location, "configuration_files")
  model_sd <- flare::initiate_model_error(config, states_config, config_file_location)
  
  #Set inital conditions
  if(is.na(run_config$restart_file)){
    init <- flare::generate_initial_conditions(states_config, 
                                               obs_config,
                                               pars_config,
                                               obs,
                                               config)
  }else{
    
    nc <- ncdf4::nc_open(paste0(run_config$restart_file))
    forecast <- ncdf4::ncvar_get(nc, "forecast") #issue here because NA is first value
    if(historical_met_error){
      restart_index <- max(which(forecast == 0)) + 1
    }else{
      restart_index <- max(which(forecast == 0))
    }
    if(max(which(forecast == 0)) == length(forecast)){
      restart_index <- max(which(forecast == 0))
    }
    
    init <- flare::generate_restart_initial_conditions(
      restart_file = run_config$restart_file,
      state_names = states_config$state_names,
      par_names = pars_config$par_names_save,
      restart_index = restart_index)
  }
  
  aux_states_init <- list()
  aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
  aux_states_init$avg_surf_temp <- init$avg_surf_temp
  aux_states_init$the_sals_init <- config$the_sals_init
  aux_states_init$mixing_vars <- init$mixing_vars
  aux_states_init$model_internal_depths <- init$model_internal_depths
  aux_states_init$lake_depth <- init$lake_depth
  aux_states_init$salt <- init$salt
  
  # Check if GLM is running ----
  # Run it for the entire period with the first met/inflow/outflow file
  working_directory = config$run_config$execute_location
  
  dir.create(file.path(working_directory,"1"))
  config$include_wq <- FALSE
  flare:::set_up_model(config,
                       ens_working_directory = file.path(working_directory,"1"),
                       state_names = states_config$state_names,
                       inflow_file_names = inflow_file_names,
                       outflow_file_names = outflow_file_names)
  
  nml <- glmtools::read_nml(file.path(working_directory,"1", "glm3.nml"))
  deps <- glmtools::get_nml_value(nml, "the_depths")
  nml <- glmtools::set_nml(nml, arg_list = list(
    "num_depths" = length(deps),
    "meteo_fl" = met_file_names[1],
    "inflow_fl" = inflow_file_names[1],
    "outflow_fl" = outflow_file_names[1],
    "start" = format(full_time_forecast[1], "%Y-%m-%d %H:%M:%S"),
    "stop" = format(full_time_forecast[length(full_time_forecast)], "%Y-%m-%d %H:%M:%S")
  ))
  glmtools::write_nml(nml, file.path(working_directory,"1", "glm3.nml"))
  
  unlink(file.path(working_directory,"1", "output"), recursive = TRUE)
  res <- GLM3r::run_glm(sim_folder = file.path(working_directory,"1"))
  if(res != 0) {
    stop("GLM crashed. Please inspect the output printed to console.")
  }
  
  glmtools::plot_temp(file.path(working_directory,"1", "output.nc"))
  # plot(glmtools::get_surface_height(file.path(working_directory,"1", "output.nc")))
  
  #####
  
  #Run EnKF
  t0 <- Sys.time()
  enkf_output <- flare:::run_da_forecast(states_init = init$states, 
                                         pars_init = init$pars,
                                         aux_states_init = aux_states_init,
                                         obs = obs,
                                         obs_sd = obs_config$obs_sd,
                                         model_sd = model_sd,
                                         working_directory = config$run_config$execute_location,
                                         met_file_names = met_file_names,
                                         inflow_file_names = inflow_file_names,
                                         outflow_file_names = outflow_file_names,
                                         start_datetime = start_datetime_local,
                                         end_datetime = end_datetime_local,
                                         forecast_start_datetime = forecast_start_datetime_local,
                                         config = config,
                                         pars_config = pars_config,
                                         states_config = states_config,
                                         obs_config = obs_config, 
                                         management = NULL, 
                                         da_method = da_method, 
                                         par_fit_method = par_fit_method
                                         
  )
  t1 <- Sys.time()
  
  # Save forecast
  saved_file <- flare::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_location)
  
  flare::plotting_general(saved_file,
                          qaqc_location = config$qaqc_data_location)
  
  dt <- data.frame(Time = t1, sim_name = run_config$sim_name, 
                   start = format(full_time_forecast[1], "%Y-%m-%d"), 
                   stop = format(full_time_forecast[length(full_time_forecast)], "%Y-%m-%d"),
                   file = saved_file, 
                   time_mins = difftime(t1, t0, units = "mins"), 
                   nens = config$ensemble_size, 
                   da_method = da_method, 
                   par_fit_method = par_fit_method)
  write.table(dt, file.path(lake_directory, "model_timing_v3.dat"), append = TRUE, row.names = FALSE,
              col.names = ifelse(file.exists(file.path(lake_directory, "model_timing_v3.dat")), F, T), sep = ",")
  
}else{
  # run_config$forecast_start_day_local <- as.character(lubridate::as_date(run_config$forecast_start_day_local) + lubridate::days(1))
  # yaml::write_yaml(run_config, file = file.path(forecast_location, "configuration_files","run_configuration.yml"))
}

