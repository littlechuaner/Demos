library(tmap)
library(dplyr)
library(rgdal)
library(cartogram)
crime <- read.csv(file.choose(), header=TRUE)
str(crime)
#####
numCrimesByLSOA<-crime %>% 
  select(LSOA.code, LSOA.name, Crime.type) %>% 
  group_by(LSOA.code) %>% 
  summarise(Num.crimes=n())
#####
numCrimesByType<-crime %>% 
  select(LSOA.code, LSOA.name, Crime.type) %>% 
  group_by(Crime.type) %>% 
  summarise(Num.crimes=n())
#####
sheffieldShape<-readOGR(dsn=file.choose(), layer="england_lsoa_2011")
sheffieldShape@data<-left_join(sheffieldShape@data, numCrimesByLSOA, by=c('code'='LSOA.code'))
tmap_mode("view")
qtm(sheffieldShape, fill="Num.crimes", alpha=0.5)
#####
sheffCarto<-cartogram(sheffieldShape, weight="Num.crimes", itermax=10, prepare="adjust")
tm_shape(sheffCarto) +
  tm_fill("Num.crimes", style="jenks") +
  tm_borders() + tm_layout(frame=F)
