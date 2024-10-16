packs <- c("ggplot2", "maps", "mapproj", "rsinaica", "dplyr", "openxlsx")
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
if (length(names(success)[!success])) {
  install.packages(names(success)[!success])
  sapply(names(success)[!success], require, character.only = TRUE)
}
library(lubridate)

#valid stations
years <- 2019:2024
parameters <- c("NO2")
station_ids <- stations_sinaica$station_id 

valid_stations <- list()  
for (station_id in station_ids) {
  try({
    station_params <- sinaica_station_params(station_id)
    if (nrow(station_params) > 0) {
      for (param in parameters) {
        if (param %in% station_params$param_code) {
          station_info <- stations_sinaica[stations_sinaica$station_id == station_id, ]
          if (nrow(station_info) > 0 && station_info$lat > 22) {
            if (is.null(valid_stations[[param]])) {
              valid_stations[[param]] <- data.frame()
            }
            valid_stations[[param]] <- bind_rows(valid_stations[[param]], station_info)
          }
        }
      }
    }
  }, silent = TRUE)
}
for (param in parameters) {
  if (is.null(valid_stations[[param]]) || nrow(valid_stations[[param]]) == 0) {
    stop(paste("No", param, "Data"))
  }
}

#print stations
if (!is.null(valid_stations[["NO2"]]) && nrow(valid_stations[["NO2"]]) > 0) {
  o3_station_ids <- valid_stations[["NO2"]]$station_id
  print("valid NO2 stations:")
  print(o3_station_ids)
} else {
  print("no NO2 station ID.")
}


#NO2
years <- 2019:2024
parameters <- "NO2"  
valid_station_ids <- c(54, 53, 428, 59, 58, 60, 65, 66, 371, 39, 41, 305, 141, 144, 425, 142, 146, 446, 140, 145, 143, 147, 424, 139, 148, 426, 437, 306, 304, 174, 172, 171, 427, 47, 46, 56, 294)
data_list <- data.frame()

for (station_id in valid_station_ids) {
  for (year in years) {
    for (month in 1:12) {
      start_date <- sprintf("%d-%02d-01", year, month)
      end_date <- as.character(as.Date(start_date) + months(1) - days(1))
      for (attempt in 1:3) {  
        try({
          df <- sinaica_station_data(station_id, parameters, start_date, end_date, "Validated")
          if (!is.null(df) && nrow(df) > 0) {
            df$parameter <- parameters  
            data_list <- bind_rows(data_list, df)  
            print(paste("Success:", station_id, "Year:", year, "Month:", month))
          }
          break  
        }, silent = FALSE)  
        if (attempt < 3) {
          print(paste("Failed, try", attempt + 1, "times"))
        } else {
          print(paste("Station:", station_id, "Failed"))
        }
        Sys.sleep(2)
      }
    }
  }
}
if (nrow(data_list) > 0) {
  write.csv(data_list, file = "NO2_validated_air_quality_2019_2024.csv", row.names = FALSE)
} else {
  print("No data")
}
print(head(data_list))

library(dplyr)
library(readr)
file_paths <- c("")
combined_data <- bind_rows(lapply(file_paths, read_csv))
station_info <- valid_stations$NO2  
station_info_selected <- station_info %>%
  select(
    station_id,     
    station_name,    
    address,         
    lat,             
    lon,             
    state_code,      
    municipio_code,  
    zip             
  )

merged_data <- merge(combined_data, station_info_selected, by = "station_id", all.x = TRUE)
merged_data$Country <- "Mexico"
write.csv(merged_data, file = "", row.names = FALSE)
print("NO2 files combined and updated successfully.")

