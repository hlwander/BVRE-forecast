extract_secchi <- function(fname,
                        input_file_tz,
                        local_tzone,
                        focal_depths){

  d <- read_csv(fname,
                col_types = readr::cols()) %>%
    filter(Reservoir == "BVR" & Site == 50) %>%
    dplyr::select(DateTime, Secchi_m) %>%
    mutate(DateTime = mdy_hm(DateTime),
           DateTime = force_tz(DateTime, input_file_tz),
           DateTime = with_tz(DateTime, "UTC")) %>%
    group_by(DateTime) %>%
    summarise(secchi = mean(Secchi_m, na.rm = TRUE), .groups = 'drop') %>%
    rename("timestamp" = DateTime) %>%
    pivot_longer(cols = -c(timestamp), names_to = "variable", values_to = "value") %>%
    mutate(depth = NA) %>%
    filter(!is.na(value)) %>%
    dplyr::select(timestamp , depth, value, variable)

  return(d)
}
