#-----------------------------------------------------
# Continuous Glucose Monitoring Data Analysis Group Training
#-----------------------------------------------------
library(dplyr)
library(lubridate)
library(iglu)

data_AL001 <- read.csv("data/AL001_CGM_data_2days.csv", skip = 2, header = TRUE)
data_DG002 <- read.csv("data/DG002_CGM_data_2days.csv", skip = 1, header = TRUE)
data_DC003 <- read.csv("data/DC003_CGM_data_2days.csv")
data_DC003 <- read.csv("data/DC003_CGM_data_2days.csv", sep = ";")
data_MC004 <- read.csv("data/MC004_CGM_data_2days.csv", skip=6, sep=";")

glucinda_data <- read.csv("data2_parsed/parsed_just_cgm.csv", sep=";")

mgtommol_multiplier <- 0.0555
mmoltomg_multiplier <- 18.018018
add_converted_glucose_columns <- function(mydata) {
  mydata  %>% 
    dplyr::mutate(
      value_mgdl  = ifelse(grepl("mg", unit),
                           value_num,
                           value_num * mmoltomg_multiplier),
      value_mmoll = ifelse(grepl("mol", unit),
                           value_num,
                           value_num * mgtommol_multiplier)
    )
}

glucinda_data <- glucinda_data %>% 
  add_converted_glucose_columns()

glucinda_data <- glucinda_data  %>% 
  dplyr::mutate(
    datetime = lubridate::ymd_hms(paste(date, time))
  )

iglu_data <- glucinda_data  %>% 
  dplyr::select(
    id   = sid,
    time = datetime,
    gl   = value_mgdl
  )

iglu_data_14days <- iglu_data %>% 
  dplyr::group_by(id) %>%
  dplyr::filter(
    as.Date(time) >= min(as.Date(time)) + 20,
    as.Date(time) <= min(as.Date(time)) + 33
  ) %>%
  dplyr::ungroup()

# Check that the runtimes are now only 14 days and activity is sufficient
iglu_data_14days %>% 
  iglu::active_percent()  

iglu_data_14days %>% 
  group_by(id) %>% 
  summarise(mean_glucose = mean(gl, na.rm = TRUE),
            min_glucose = min(gl, na.rm = TRUE), 
            max_glucose = max(gl, na.rm = TRUE))