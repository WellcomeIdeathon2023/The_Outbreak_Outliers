# Code for reading in vaccine uptake data
# Gabriel - 1/7/23

library(tidyverse)

path_to_data <- "data/raw/"
covid_data <- read_csv(paste0(path_to_data,"owid-covid-data.csv"))

dim(covid_data)
names(covid_data)
#also have cases and other information if we need

unique(covid_data$location)


#pick the most relevant countries
covid_data %>% 
  filter(location == c("United States", "United Kingdom", "India", "Germany", "Canada")) %>% 
  select(date, total_vaccinations_per_hundred,location) %>% 
  ggplot(aes(x = date, y= total_vaccinations_per_hundred)) +
  geom_line(aes(col = location)) + 
  labs(
    title = "Cummulative vaccines by country",
    y = "Total vaccinations per 100"
  )


#   "new_vaccinations" 

covid_data %>% 
  filter(location == c("United States", "United Kingdom", "India", "Germany", "Canada")) %>% 
  select(date, new_vaccinations,location) %>% 
  ggplot(aes(x = date, y= new_vaccinations)) +
  geom_line(aes(col = location)) + 
  labs(
    title = "New vaccination rate vaccines by country",
    y = "New vaccinations"
  )




covid_data %>% 
  filter(location == c("United States", "United Kingdom", "India", "Germany", "Canada")) %>% 
  select(date, new_people_vaccinated_smoothed_per_hundred,location) %>% 
  ggplot(aes(x = date, y= new_people_vaccinated_smoothed_per_hundred)) +
  geom_line(aes(col = location)) + 
  labs(
    title = "New vaccination rate vaccines by country",
    y = "New vaccinations per 100"
  )
