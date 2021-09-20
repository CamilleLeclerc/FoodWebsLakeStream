
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

# Copy from my hard drive
hd_path = "/media/alain/1d8cb75f-4ebf-4a4d-a65c-75d4c6836186/Andy/Omezix/fishcom/"

# Temperature and flow data

file_to_copy <- c(
  #temperature data
  paste0(hd_path, "data-raw/temp/yearly_temp_press_interp_mv_avg.rda"),
  #flow data
  paste0(hd_path, "data-raw/flow/yearly_flow_press_interp_mv_avg.rda"),
  # DBO, nitrates, etc...
  paste0(hd_path, "data/press_metrics.rda")
)

file.copy(
  from = file_to_copy,
  to = mypath("data/environment/")
)


new_name <- c(
  "yearly_avg_water_temperature_stream.rda",
  "yearly_avg_water_flow_stream.rda",
  "yearly_avg_water_chemical_stream.rda"
)

file.rename(
  from = c(
    mypath("data/environment/yearly_temp_press_interp_mv_avg.rda"),
    mypath("data/environment/yearly_flow_press_interp_mv_avg.rda"),
    mypath("data/environment/press_metrics.rda")),
  to = purrr::map_chr(new_name, ~mypath("data", "environment", .x))
)

myload(
  yearly_avg_water_chemical_stream,
  yearly_avg_water_flow_stream,
  yearly_avg_water_temperature_stream,
  dir = mypath("data", "environment")
)

yearly_avg_water_flow_stream <- yearly_flow_press_interp_mv_avg
yearly_avg_water_temperature_stream <- yearly_temp_press_interp_mv_avg
yearly_avg_water_chemical_stream <- press_metrics


mysave(yearly_avg_water_flow_stream,
  yearly_avg_water_temperature_stream,
  yearly_avg_water_chemical_stream,
  dir =  mypath("data", "environment"),
  overwrite = TRUE
)

