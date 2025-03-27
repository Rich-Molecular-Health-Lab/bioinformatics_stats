#load RNOAA

#install
#install.packages("rnoaa")
library(rnoaa)
library(tidyverse)

#get API key
options(noaakey = "TdwnXjSnvhUtsjjoTgeWMezLJFUkZlsY")
#this is my key, you may have to get your own here 
#https://www.ncdc.noaa.gov/cdo-web/token


station_data <- ghcnd_stations()

#this might take a while, it should be a list of weather stations

#load data 


setwd("~/Documents/Contract work/UNCG/2021/MYSE Project")
library(readr)
wf <- read_csv("all_bats_unvetted.csv")
wf$date<-as.character(wf$date)
wf$date<-as.Date(wf$date, format="%Y%m%d")
wf$year<-format(as.Date(wf$date, format="%Y-%m-%d"),"%Y")

wf<-plyr::rename(wf,c("site"="site_code"))


#add new sites through 2021
S21 <- read_csv("sites_2015_2021.csv")
S21<-plyr::rename(S21, c("site"="site_code"))


wf<-merge(S21,wf, by=c("site_code"))

#drop uneeded columns

#wf<-wf[,-c(2:4,7,10:25)]

wf<-wf[c(1,6,7,12,13,30)]

unique(S21$site_code)
unique(wf$site_code)

# 2015 --------------------------------------------------------------------



#subset out 2015
wf15<-subset(wf,wf$year=="2015")

#filter station data for sites with 2015 data
stat_15<-station_data %>%
  filter(last_year>=2015)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf15$site_code,
                         latitude = wf15$Latitude,
                         longitude = wf15$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=1)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_15, radius = 100,limit = 1)

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4


#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(sf$id,date_min = min(wf15$date), date_max = max(wf15$date))

#look at data
skim(tm)

#merge with 2015 dataset 
s15<-merge(sf,wf15,by ="site_code",all.y = TRUE)

#look at data 
skim(s15)

#cut out not needed cols
sd15<-s15[c(1:3,6:11)]

#rename for merge
#sd15<-plyr::rename(sd15,c("site_code"="site_code"))

# merge with weather data
wd15<-merge(sd15,tm,by=c("id","date"),all.x = TRUE)
skim(wd15)

#get station ids for stations with NAs
naw<-wd15 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#Get 5 closest stations to select stations without NAs, (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_15, radius = 100,limit = 5)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")

#filter out all but the closest station
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf15$date), date_max = max(wf15$date))

#look at data
skim(tm)

#merge with 2015 dataset 
s15<-merge(sf2,wf15,by ="site_code",all.y = TRUE)

skim(s15)

#cut out un needed cols
sd15<-s15[c(1:3,6:11)]

#rename for merge
#sd15<-plyr::rename(sd15,c("site_code"="site_code"))

# merge with weather data
wd15<-merge(sd15,tm,by=c("id","date"),all.x = TRUE)
skim(wd15)

#convert to degrees C 
wd15[c(10:12)]<-wd15[c(10:12)]/10

#cut out un needed cols
wd15<-wd15[c(1:12)]
skim(wd15)


# 2016 --------------------------------------------------------------------



#subset out 2016
wf16<-subset(wf,wf$year=="2016")

#filter station data for sites with 2016 data
stat_16<-station_data %>%
  filter(last_year>2016)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf16$site_code,
                         latitude = wf16$Latitude,
                         longitude = wf16$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_16, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(w_list,date_min = min(wf16$date), date_max = max(wf16$date))

#look at data
skim(tm)

#merge with 2016 dataset 
s16<-merge(sf,wf16,by ="site_code",all.y = TRUE)

#look at data 
skim(s16)

#cut out not needed cols
sd16<-s16[c(1:3,6:11)]

#rename for merge
#sd16<-plyr::rename(sd16,c("site_code"="site_code"))

# merge with weather data
wd16<-merge(sd16,tm,by=c("id","date"),all.x = TRUE)
skim(wd16)

#get station ids for stations with NAs
naw<-wd16 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd16 %>%
  filter(!is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_16, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf16$date), date_max = max(wf16$date))

#look at data
skim(tm)

#merge with 2016 dataset 
s16<-merge(sf2,wf16,by ="site_code",all.y = TRUE)

skim(s16)

#cut out un needed cols
sd16<-s16[c(1:3,6:11)]

#rename for merge
#sd16<-plyr::rename(sd16,c("site_code"="site_code"))

# merge with weather data
wd16<-merge(sd16,tm,by=c("id","date"),all.x = TRUE)
skim(wd16)

#convert to degrees C 
wd16[c(10:12)]<-wd16[c(10:12)]/10

#cut out un needed cols
wd16<-wd16[c(1:12)]
skim(wd16)







# 2017 --------------------------------------------------------------------


#subset out 2017
wf17<-subset(wf,wf$year=="2017")

#filter station data for sites with 2017 data
stat_17<-station_data %>%
  filter(last_year>2017)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf17$site_code,
                         latitude = wf17$Latitude,
                         longitude = wf17$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_17, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(w_list,date_min = min(wf17$date), date_max = max(wf17$date))

#look at data
skim(tm)

#merge with 2017 dataset 
s17<-merge(sf,wf17,by ="site_code",all.y = TRUE)

#look at data 
skim(s17)

#cut out not needed cols
sd17<-s17[c(1:3,6:11)]

#rename for merge
#sd17<-plyr::rename(sd17,c("site_code"="site_code"))

# merge with weather data
wd17<-merge(sd17,tm,by=c("id","date"),all.x = TRUE)
skim(wd17)

#get station ids for stations with NAs
naw<-wd17 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd17 %>%
  filter(is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_17, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf17$date), date_max = max(wf17$date))

#look at data
skim(tm)

#merge with 2017 dataset 
s17<-merge(sf2,wf17,by ="site_code",all.y = TRUE)

skim(s17)

#cut out un needed cols
sd17<-s17[c(1:3,6:11)]

#rename for merge
#sd17<-plyr::rename(sd17,c("site_code"="site_code"))

# merge with weather data
wd17<-merge(sd17,tm,by=c("id","date"),all.x = TRUE)
skim(wd17)

#convert to degrees C 
wd17[c(10:12)]<-wd17[c(10:12)]/10

#cut out un needed cols
wd17<-wd17[c(1:12)]
skim(wd17)


# 2018 --------------------------------------------------------------------


#subset out 2018
wf18<-subset(wf,wf$year=="2018")

#filter station data for sites with 2018 data
stat_18<-station_data %>%
  filter(last_year>2018)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf18$site_code,
                         latitude = wf18$Latitude,
                         longitude = wf18$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_18, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(w_list,date_min = min(wf18$date), date_max = max(wf18$date))

#look at data
skim(tm)

#merge with 2018 dataset 
s18<-merge(sf,wf18,by ="site_code",all.y = TRUE)

#look at data 
skim(s18)

#cut out not needed cols
sd18<-s18[c(1:3,6:11)]

#rename for merge
#sd18<-plyr::rename(sd18,c("site_code"="site_code"))

# merge with weather data
wd18<-merge(sd18,tm,by=c("id","date"),all.x = TRUE)
skim(wd18)

#get station ids for stations with NAs
naw<-wd18 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd18 %>%
  filter(is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_18, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf18$date), date_max = max(wf18$date))

#look at data
skim(tm)

#merge with 2018 dataset 
s18<-merge(sf2,wf18,by ="site_code",all.y = TRUE)

skim(s18)

#cut out un needed cols
sd18<-s18[c(1:3,6:11)]

#rename for merge
#sd18<-plyr::rename(sd18,c("site_code"="site_code"))

# merge with weather data
wd18<-merge(sd18,tm,by=c("id","date"),all.x = TRUE)
skim(wd18)

#convert to degrees C 
wd18[c(10:12)]<-wd18[c(10:12)]/10

#cut out un needed cols
wd18<-wd18[c(1:12)]
skim(wd18)


# 2019 --------------------------------------------------------------------


#subset out 2019
wf19<-subset(wf,wf$year=="2019")

#filter station data for sites with 2019 data
stat_19<-station_data %>%
  filter(last_year>2019)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf19$site_code,
                         latitude = wf19$Latitude,
                         longitude = wf19$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_19, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(w_list,date_min = min(wf19$date), date_max = max(wf19$date))

#look at data
skim(tm)

#merge with 2019 dataset 
s19<-merge(sf,wf19,by ="site_code",all.y = TRUE)

#look at data 
skim(s19)

#cut out not needed cols
sd19<-s19[c(1:3,6:11)]

#rename for merge
#sd19<-plyr::rename(sd19,c("site_code"="site_code"))

# merge with weather data
wd19<-merge(sd19,tm,by=c("id","date"),all.x = TRUE)
skim(wd19)

#get station ids for stations with NAs
naw<-wd19 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd19 %>%
  filter(is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_19, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf19$date), date_max = max(wf19$date))

#look at data
skim(tm)

#merge with 2019 dataset 
s19<-merge(sf2,wf19,by ="site_code",all.y = TRUE)

skim(s19)

#cut out un needed cols
sd19<-s19[c(1:3,6:11)]

#rename for merge
#sd19<-plyr::rename(sd19,c("site_code"="site_code"))

# merge with weather data
wd19<-merge(sd19,tm,by=c("id","date"),all.x = TRUE)
skim(wd19)

#convert to degrees C 
wd19[c(10:12)]<-wd19[c(10:12)]/10

#cut out un needed cols
wd19<-wd19[c(1:12)]
skim(wd19)


# 2020 --------------------------------------------------------------------

#subset out 2020
wf20<-subset(wf,wf$year=="2020")

#filter station data for sites with 2020 data
stat_20<-station_data %>%
  filter(last_year>2020)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf20$site_code,
                         latitude = wf20$Latitude,
                         longitude = wf20$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_20, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C200-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
tm<-meteo_pull_monitors(w_list,date_min = min(wf20$date), date_max = max(wf20$date))

#look at data
skim(tm)

#merge with 2020 dataset 
s20<-merge(sf,wf20,by ="site_code",all.y = TRUE)

#look at data 
skim(s20)

#cut out not needed cols
sd20<-s20[c(1:3,6:11)]

#rename for merge
#sd20<-plyr::rename(sd20,c("site_code"="site_code"))

# merge with weather data
wd20<-merge(sd20,tm,by=c("id","date"),all.x = TRUE)
skim(wd20)

#get station ids for stations with NAs
naw<-wd20 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd20 %>%
  filter(is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_20, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf20$date), date_max = max(wf20$date))

#look at data
skim(tm)

#merge with 2020 dataset 
s20<-merge(sf2,wf20,by ="site_code",all.y = TRUE)

skim(s20)

#cut out un needed cols
sd20<-s20[c(1:3,6:11)]

#rename for merge
#sd20<-plyr::rename(sd20,c("site_code"="site_code"))

# merge with weather data
wd20<-merge(sd20,tm,by=c("id","date"),all.x = TRUE)
skim(wd20)

#convert to degrees C 
wd20[c(10:12)]<-wd20[c(10:12)]/10

#cut out un needed cols
wd20<-wd20[c(1:12)]
skim(wd20)





# 2021 --------------------------------------------------------------------

#subset out 2021
wf21<-subset(wf,wf$year=="2021")

#filter station data for sites with 2021 data
stat_21<-station_data %>%
  filter(last_year>=2021)

#make dataframe of lat/longs
lat_lon_df <- data.frame(id = wf21$site_code,
                         latitude = wf21$Latitude,
                         longitude = wf21$Longitude)
#get unique rows
lat_lon_df<-unique(lat_lon_df)

#get nearby GHCND stations, max distance 100km (radius=100), return nearest station (limit=5)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_21, radius = 100,limit =5 )

#convert into dataframe
library(tidyverse)
sf<-bind_rows(nearby_stations, .id = "site_code")


#look at data
library(skimr)
skim(sf)

#look at station coverage
# mc<-meteo_coverage(tm)
# mmc<-mc$summary

#check for non matching (why 64 in one and 65 in the other?)
# matched <- intersect(sf$site_code,lat_lon_df$id)
# all <-  union(sf$site_code,lat_lon_df$id)
# non.matched <- all[!all %in% matched]

#looks like there are two different lat/longs for C210-S4

w_list<-unique(sf$id)

#Get the weather observations from nearby station list
#meteo_clear_cache()
tm<-meteo_pull_monitors(w_list,date_min = min(wf21$date), date_max = max(wf21$date))

#look at data
skim(tm)

#merge with 2021 dataset 
s21<-merge(sf,wf21,by ="site_code",all.y = TRUE)

#look at data 
skim(s21)

#cut out not needed cols
sd21<-s21[c(1:3,6:11)]

#rename for merge
#sd21<-plyr::rename(sd21,c("site_code"="site_code"))

# merge with weather data
wd21<-merge(sd21,tm,by=c("id","date"),all.x = TRUE)
skim(wd21)

#get station ids for stations with NAs (19 at closest 5 stations)
naw<-wd21 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#map of sites with NAs
library(leaflet)
bm<-wd21 %>%
  filter(is.na(tmax)) %>%
  group_by(site_code) %>%
  slice(which.min(distance))


leaflet(data = bm) %>% addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude,
                   radius = 1,
                   color = "red",
                   popup = ~as.character(name))


#Get 10 closest stations to select stations without NAs, (limit=10)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_21, radius = 100,limit = 10)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))

length(unique(sf2$site_code))#check for lost sites 
length(unique(wd21$site_code))

#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf21$date), date_max = max(wf21$date))

#look at data
skim(tm)

#merge with 2021 dataset 
s21<-merge(sf2,wf21,by ="site_code",all.y = TRUE)

skim(s21)

#cut out un needed cols
sd21<-s21[c(1:3,6:11)]

#rename for merge
#sd21<-plyr::rename(sd21,c("site_code"="site_code"))

# merge with weather data
wd21<-merge(sd21,tm,by=c("id","date"),all.x = TRUE)
skim(wd21)


#check for NAs (11 at closest 10 stations)
naw<-wd21 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

#Get 12 closest stations to select stations without NAs, (limit=12)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df, var=c("TMAX", "TMIN"),
                                          station_data = stat_21, radius = 100,limit = 12)
#convert to dataframe
sf<-bind_rows(nearby_stations, .id = "site_code")


#filter out all but the closest station, excluding NAs
sf2<-sf %>%
  filter(id!=naw$id) %>%
  group_by(site_code) %>%
  slice(which.min(distance))

length(unique(sf2$site_code))#check for lost sites 
length(unique(wd21$site_code))

#Get weather data from new stations without NAs  
tm<-meteo_pull_monitors(sf2$id,date_min = min(wf21$date), date_max = max(wf21$date))

#look at data
skim(tm)

#merge with 2021 dataset 
s21<-merge(sf2,wf21,by ="site_code",all.y = TRUE)

skim(s21)

#cut out un needed cols
sd21<-s21[c(1:3,6:11)]

#rename for merge
#sd21<-plyr::rename(sd21,c("site_code"="site_code"))

# merge with weather data
wd21<-merge(sd21,tm,by=c("id","date"),all.x = TRUE)
skim(wd21)


#check for NAs (5 at closest 12 stations)
naw<-wd21 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id)

wd21 %>% 
  filter(is.na(tmax)) %>% 
  distinct(id,date)

#convert to degrees C 
wd21[c(10:12)]<-wd21[c(10:12)]/10

#cut out un needed cols
wd21<-wd21[c(1:12)]
skim(wd21)


# put it together ---------------------------------------------------------



wd_all<-rbind(
      wd15,
      wd16,
      wd17,
      wd18,
      wd19,
      wd20,
      wd21)

skim(wd_all)

wd_all$tavg<-(wd_all$tmax+wd_all$tmin)/2



#write_csv(wd_all,"wd_15-21.csv")
#write_csv(wd_all,"wd_15-21_2.csv")

#convert to wide 
names(wd_all)
wff<-wf[,c(4:6,9,10)]

# wd_wall<-merge(wd_all,wff,by=c("site_code","date"))
# 
# unique(wd_wall$night)
# unique(wf$night)

wd_wall<-wd_all

wd_wall$night_n<-plyr::revalue(wd_wall$night,c("night1"="1",
                                         "night2"="2",
                                         "night3"="3",
                                         "night4"="4"
                                         ))

wd_wall$night_n<-as.numeric(wd_wall$night_n)

wd_wall$year<-as.numeric(wd_wall$year)

wd_wall$J_date<-format(wd_wall$date,"%j")

require(data.table)

#remove duplicates and rename Asheboro C204-S5
wd_wall<-wd_wall%>%
  group_by(site_code,date)%>%
  filter(!duplicated(site_name)) 

wd_wall%>%
  filter(site_code=="C200-S4")%>%
  arrange(date)

#rename Asheboro C200-S4 to S5
wd_wall<-wd_wall %>% mutate(site_code = ifelse(site_name == "Asheboro" & site_code == "C200-S4", "C200-S5", site_code))

#check to make sure it worked 
wd_wall%>%
  filter(site_code=="C200-S4")%>%
  arrange(date)

#check to make sure it worked 
wd_wall%>%
  filter(site_code=="C200-S5")%>%
  arrange(date)

#check it see if c29-s2 duplicate is still around 
wd_wall%>%
  filter(site_code=="C29-S2")%>%
  arrange(date)

wd_wall%>%
group_by(site_code)%>%
  arrange(year)%>%
  mutate()
  

#new dataset for formating umf 
nd<-data.frame(expand.grid(
  year=seq(2015,2021),
  night=seq(1,4),
    site_code=unique(S21$site_code)
  ))



#format observations for unmarked dataframe
library(dplyr)
ndd<-
  nd%>%
  arrange(year,night)%>%
  group_by(site_code) %>%
  mutate(visit = 1:n())


ndd<-plyr::rename(ndd,c("night"="night_n"))

names(wd_wall)

wd_wall<-wd_wall[-c(1,4:7)]


#hunting for duplicate night
dyno%>%
  group_by(site_code,visit)%>%
  filter(duplicated(site_code)) 

#C74-S2 2020 has 2 night 1s
dyno%>%
  filter(site_code=="C74-S2")

#did it start here? (nope)
wd_wall%>%
  filter(site_code=="C74-S2" & year=="2020")

#Here? Yep, it's in the original data
wf%>%
  filter(site_code=="C74-S2" & year=="2020")

#change nights numbering 

wd_wall<-
wd_wall %>% 
  mutate(night_n = 
           ifelse(site_code == "C74-S2" & date == "2020-07-21" , 2,
                  ifelse(site_code == "C74-S2" & date == "2020-07-22" , 3,
                         ifelse(site_code == "C74-S2" & date == "2020-07-23",4,night_n))))

wd_wall<-
  wd_wall %>% 
  mutate(night = 
           ifelse(site_code == "C74-S2" & date == "2020-07-21" , "night2",
                  ifelse(site_code == "C74-S2" & date == "2020-07-22" , "night3",
                         ifelse(site_code == "C74-S2" & date == "2020-07-23","night4",night))))

dyno<-merge(ndd,wd_wall,by=c("site_code","year","night_n"),all.x=TRUE)

dyno_l<-dyno
#visits sequential wide



dyno_w<-data.table::dcast(setDT(dyno),site_code~visit,value.var=c(
  "tmax", "tmin", "tavg","J_date","date"
))

write_csv(dyno_l,"Seq_visits_long.csv")
write_csv(dyno_w,"Seq_visits_wide.csv")


#visits by year wide 
wall<-data.table::dcast(setDT(wd_wall),site_code~year+night_n,value.var=c(
   "tmax", "tmin", "tavg","J_date"
))


#write_csv(wall,"weather_2015-2019.csv")

