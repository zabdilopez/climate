# Load libraries
library(tidyverse)
library(glue)
library(lubridate)


#global weather stations####
#first step is to obtain the invetory of weather stations from NOAA #

inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
                        col_names = c("station","lat", "lon","variable","start","end"))

#local weather station####
#Second step is to select the area where you want to find the weather stations
my_lat <- 14.595875057696952 *2 *pi/360 # you need to convert the coordinates                                            in radians for it to make sense
my_lon <- -90.58743121166637 *2 *pi/360

#Third step is to search in the inventory the closest weather station to your position
my_station <- inventory %>%
  mutate(lat_r = lat *2 *pi/360,
         lon_r = lon *2 *pi/360,
         d = 1.609344 * 3936 * acos((sin(lat_r) * sin(my_lat)) + cos(lat_r) * cos(my_lat) * cos(my_lon - lon_r))
  )%>%
  filter(start < 1970 & end > 2020) %>%
  top_n(n = -1, d) %>%
  distinct(station)%>%
  pull(station) 

#download data from the weather station####
#with the weather station close to your position now you can download the data from it
station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

#this will help to check which data does the weather station gather
table1 <- read_csv(station_daily)
unique(table1$TMAX)

#arranging the information####
local_weather <- read_csv(station_daily,
                          col_names = c("station", "date", "variable", "value", "a", "b", "c","d")) %>%
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value",
              values_fill = 0)%>%
  select(date, TAVG, TMAX, TMIN, PRCP) %>%
  mutate(date = ymd(date),
         TMAX = TMAX/ 10,
         TMIN = TMIN/ 10,
         TAVG = TAVG/ 10,
         PRCP = PRCP /10) %>%
  rename_all(tolower) %>%
  mutate(prcp = if_else(prcp < 150, prcp, NA_real_))%>%
  drop_na(prcp)


#precipitation

local_weather %>%
  ggplot(aes(x=prcp))+
  geom_histogram()+
  scale_y_continuous(limits = c(0,50))

local_weather %>%
  ggplot(aes(x=date, y=prcp))+
  geom_line()
  
  