library(tidyverse)
library(rtweet)
library(stringr)
library(maps)
library(ggthemes)
library(gganimate)


#get data
raw_tweets <- get_timeline("System001Wilson", n = 3200)

time_now <-  format(Sys.time(),"%Y-%m-%d %H_%M_%S")

filepath <- paste("data/raw_tweets", time_now, sep = "-")

save_as_csv(raw_tweets, filepath)


#keep date, position

gps_tweets <- raw_tweets %>% 
  select("created_at", "urls_expanded_url") %>% 
  filter(!is.na(urls_expanded_url)) %>% 
  filter(str_detect(urls_expanded_url, "map")) %>%  #filter's out non-map links of which there's a handful
  arrange(created_at)                               

gps_tweets <- mutate(
  gps_tweets, 
  gps = str_extract(gps_tweets$urls_expanded_url, "place/(.*)\\/|loc:?.*")) #See 1 regex made of place/(.*)\\/ and loc:?.*
                                                                            #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r


gps_tweets <-  separate(gps_tweets, gps, into = c("lat", "long"), sep = ",")

gps_tweets <- gps_tweets %>%
  mutate(lat = str_extract(lat, pattern = "-*\\d+\\.?\\d+")) %>%
  mutate(long = str_extract(long, pattern = "-*\\d+\\.?\\d+"))

gps_tweets$lat <- as.numeric(gps_tweets$lat)
gps_tweets$long <- as.numeric(gps_tweets$long)


#https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
world <- ggplot() +
  borders("world", 
          colour = "gray85", 
          fill = "gray80", 
          xlim = c(-180, -110),
          ylim = c(20,80)
          ) +
  theme_map()
  
  
world + geom_point(data = gps_tweets, 
             aes(x = long, y = lat), 
             colour = "purple",
             alpha = 0.5) +
    transition_time(created_at) +
    shadow_mark(past = TRUE)
 


