library(tidyverse)
bom_data <- read_csv("data/BOM_data.csv")
bom_stations <- read_csv("data/BOM_stations.csv")
view(bom_stations)
view(bom_data)
bom_data
bom_stations

#Question_1: For each station, how many days have a min temp, max temp
#and rainfall measurement
bom_min_max_rain <- bom_data %>% 
  separate(Temp_min_max, into = c("temp_min", "temp_max"), sep = "/") %>%
  filter(temp_min >= 0, temp_max >=0, Rainfall >=0) %>%
  group_by(Station_number) %>%
  summarise(num_row = n())

write_csv(bom_min_max_rain, "results/q1_bom_min_max_rain")

#Question_2: Which month saw the lowest average daily temp. difference?
low_temp_diff <- bom_data %>% 
  separate(Temp_min_max, into = c("temp_min", "temp_max"), sep = "/") %>%
  filter(temp_min >=0, temp_max >= 0) %>%
  mutate(temp_diff = as.numeric(temp_max) - as.numeric(temp_min)) %>%
  group_by(Month) %>% 
  summarise(average = mean(temp_diff)) %>% 
  arrange(average) 

write_csv(low_temp_diff, "results/q2_low_temp_diff")  
  
#Question_3: Which state saw the lowest average daily temp difference?
bom_stations_tidy <- bom_stations %>% 
  gather("station_number", values, -info) %>% 
  spread(key = info, value = values) %>% 
  mutate(station_number = as.numeric(station_number))

q3_ans <- left_join(bom_data, bom_stations_tidy, by = c("Station_number" = "station_number")) %>% 
  separate(Temp_min_max, into = c("temp_min", "temp_max"), sep = "/") %>% 
  filter(temp_min >= 0, temp_max >= 0) %>% 
  mutate(temp_diff = as.numeric(temp_max) - as.numeric(temp_min)) %>% 
  filter(!is.na(temp_diff)) %>% 
  group_by(state) %>% 
  summarise(ave_temp_diff = mean(temp_diff)) %>% 
  arrange(ave_temp_diff)

write_csv(q3_ans, "results/q3_avg_temp_diff_state")







  
  










