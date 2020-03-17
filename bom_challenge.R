bom_data <- read_csv("data/BOM_data.csv")
library(tidyverse)
bom_stations <- read_csv("data/BOM_stations.csv")
view(bom_stations)
view(bom_data)
bom_data
bom_stations
#For each station, how many days have a minimum temp, a max temp,
#and a rainfall measurement recorded?
separate(bom_data, col = Temp_min_max, into = Temp_min, Temp_max,
         sep = "/")
bom_data_separate <- separate(bom_data, col = 5,
                              into = c("Temp_min", "Temp_max"),
                              sep = "/")
bom_data_separate
filter(bom_data_separate, Temp_min >= 0,
       Temp_max >= 0, Rainfall >= 0)
group_by_temp_min <- group_by(bom_data_separate,
                               Temp_min)

#Question 1
ques_1 <- bom_data %>%
  separate(Temp_min_max, into = c("mean_temp", "max_temp")) %>%
  filter(mean_temp != "", max_temp != "", Rainfall != "-") %>%
  group_by(Station_number) %>%
  summarise(num_row = n())
ques_1

#Question 2: Which month saw the lowest average day temp difference
ques2 <- bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep = "/", remove = FALSE) %>%
  filter(min_Temp >= 0, max_Temp >= 0, Rainfall >= 0) %>%
  mutate(Temp_diff = as.numeric(max_Temp) - as.numeric(min_Temp)) %>%
  filter(min_Temp > max_Temp) %>% 
  group_by(Month) %>%
  summarise(average = mean(Temp_diff))

#question_3:
bom_station_long <- bom_stations %>% 
  gather(key = "Station_number", values, -info) %>%
  spread(key = info, value = values) %>% 
  mutate(Station_number = as.numeric(Station_number))



