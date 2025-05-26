#Lab 3: Prepping data

library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(ncdf4)

#The daily ERA5 data can be found in this repo:
#https://github.com/communitymaplab/era5_1950-69_2014-2024

#Download those files and move the precip data to your lab folder if you want to start the script from here.
#List all the NetCDF files in the data folder
era5_files<-list.files(path="data",pattern=".nc",full.names=T)

#Load each nc file and combine
precip_50_54<-rast(era5_files[1])
precip_55_59<-rast(era5_files[2])
precip_60_64<-rast(era5_files[3])
precip_65_69<-rast(era5_files[4])
precip_04_08<-rast(era5_files[5])
precip_09_13<-rast(era5_files[6])
precip_14_18<-rast(era5_files[7])
precip_19_24<-rast(era5_files[8])

precip_all<-rast(list(precip_50_54,precip_55_59,precip_60_64,precip_65_69,
                      precip_04_08,precip_09_13,precip_14_18,precip_19_24))

#See time stamps and total number of layers for the 1950-54 data
precip_all
dates<-time(precip_all)
dates
years<-data.frame(dates=unique(substr(dates,1,4)))%>%
  filter(dates!="2004")

head(precip_all)
layers<-nlyr(precip_all)

#Creaye a sum of precip for these rasters in year year
#How to extract a certain date from the raster stack that terra uses.
# test<-precip_50_54[[time(precip_50_54)=="1950-07-01"]]
# plot(test)

#Function to sum precip for all dates in a year
year_sum<-function(year_sel){
  rast_sum<-sum(precip_all[[substr(time(precip_all),1,4)==year_sel]])
  time(rast_sum)<-as.Date(paste0(year_sel,"-01-01"))
  rast_sum
}

#Apply function and combine to a raster stack
years_rast<-map(years$dates,year_sum)
years_rast1<-rast(years_rast)

writeCDF(years_rast1,"data/era5_jan_yearlyprecip.nc",overwrite=T)

##Geog4/6300: the `years_rast1` file is what's saved in the lab repo.
#Load the county data and compute zonal stats
cty<-st_read("data/ACSCtyData_2022ACS_simplify.gpkg") %>%
  select(cty_fips:totpop_race,medinc,pov_pop_pct)
cty_west<-cty %>%
  dplyr::filter(st_name %in% c("California","Oregon","Washington","Montana","Idaho",
                               "Wyoming","Nevada","Utah","Colorado",
                               "Arizona","New Mexico"))
st_write(cty_west,"data/acsdata_cty_westreg.gpkg")

#Assign a year to these rasters
zonal_mean<-zonal(years_rast1,vect(cty_west),fun="mean",na.rm=T)

#Give dates to the columns
dates<-time(years_rast1)
names(zonal_mean)<-dates

cty_west_prcp<-bind_cols(cty_west,zonal_mean) %>%
  pivot_longer(`1950-01-01`:`2024-01-01`,names_to="year",values_to="prcp_m") %>%
  mutate(time_range=if_else(as.numeric(substr(year,1,4))<1980,"1950-69","2005-24"))

#Map
# tm_shape(cty_west_prcp)+
#   tm_polygons("prcp_m",style="jenks")+
#   tm_facets("year")
hist(cty_west_prcp$prcp_m)

#Write to csv
write_csv(cty_west_prcp %>% st_set_geometry(NULL),"data/precip_uswest_era5.csv")
