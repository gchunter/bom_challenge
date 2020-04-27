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

#question 4: Does the westmost or eastmost weather station in our dataset have
# a higher average solar exposure?

bom_data_tidy_stations <- left_join(bom_data, bom_stations_tidy, by = c("Station_number" = "station_number"))

q4_answer <- bom_data_tidy_stations %>% 
  filter(Solar_exposure != "-") %>% 
  filter(lon == min(lon) | lon == max(lon)) %>%
  group_by(Station_number, lon) %>%
  summarise(avg_sol_exp = mean(Solar_exposure))

View(q4_answer)

#Introduction to data visualisation > Preparing plots for display
#Challenge: putting it all together

#Q1: For the Perth station (ID 9225), produce three scatter plots showing the 
#relationship between the maximum temperature and each other measurement recorded
#(minimum temperature, rainfall and solar exposure).

#Filter the perth station data, seperate temp and change variables to dbl
perth_data <- bom_data %>% 
  filter(Station_number == 9225) %>% 
  separate(Temp_min_max, into = c("temp_min", "temp_max"), sep = "/") %>% 
  mutate(temp_min = as.numeric(temp_min), temp_max = as.numeric(temp_max),
         Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure))


q1_temp <- ggplot(data = perth_data, aes(x = temp_max, y = temp_min)) +
  geom_point(alpha = 0.3)

q1_rain <- ggplot(data = perth_data, aes(x = temp_max, y = Rainfall)) + 
  geom_point(alpha = 0.3)

q1_solar <- ggplot(data = perth_data, aes(x = temp_max, y = Solar_exposure))  

ggsave(filename = "results/q1_temp.png", plot = q1_temp,        #Save the temperature comparison
       width = 12, height = 10, dpi = 300, units = "cm")
ggsave(filename = "results/q1_rain.png", plot = q1_rain,        #Save the rainfall comparison
       width = 12, height = 10, dpi = 300, units = "cm")
ggsave(filename = "results/q1_solar.png", plot = q1_solar,      #Save the solar comparison
       width = 12, height = 10, dpi = 300, units = "cm")

#Q2: Display the four measurements for the Perth station in a single scatter
#plot by using additional aesthetic mappings

q2 <- ggplot(perth_data, aes(x = temp_max, y = temp_min, colour = Solar_exposure, size = Rainfall)) +
  geom_point(alpha = 0.8)

ggsave(filename = "results/q2.png", plot = q2, width = 24, height = 10, dpi = 300, units = "cm")  

#Q3: Take the four plots produced in Q1 and Q2 and save them as a multi-panel figure
library(cowplot)                                              #load cowplot
perth_panel <- plot_grid(q1_temp, q1_rain, q1_solar, q2)      #produce a multi-panel
ggsave(filename = "results/perth_panel.png", plot = perth_panel,
       width = 24, height = 18, dpi = 300, units = "cm")      #Save the plot to an image file

#Q4: Use the entire BOM dataset, calculate the average monthly rainfall for each station.
#Produce a lineplot to visualise this data and the state each station is in.

bom_data %>%
  group_by(Station_number, Month) %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  filter(!is.na(Rainfall)) %>% 
  

