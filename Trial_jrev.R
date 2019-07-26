library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
#### Functions used in script ####

prep_col <- function(in_filename,Template_Column_Names,out_filename=NULL){
  # function to select columns from input files for excel file
  Template_Column_Names <- c('submission time', 'VC_ID',	'lat',	'long', 'market_type',	'Price_Ymaize_PerKg',	'price_Wmaize_perkg',	'price_Thailand_perkg',	'price_Indian_perkg') 
  #Template_Column_type <- c('date', 'numeric',	'numeric',	'numeric', 'text',	'numeric',	'numeric',	'numeric',	'numeric') 
  data_in <- read_excel(in_filename,sheet = 1, col_types = 'text')
  data_processed <- data_in %>%
    select(Template_Column_Names)
  if(!is.null(out_filename)){
    #out_filename <- 
    write.table(data_processed,file = out_filename, sep = ",")
    
  }
  return(data_processed)
}

Template_Column_type <- c('date', 'numeric',	'numeric',	'numeric', 'text',	'numeric',	'numeric',	'numeric',	'numeric') 

##############

#filename <- 'C:\\Users\\SChiejile\\Desktop\\FPCA\\Crowd Survey\\PILOT SURVEY DATA-20190711T092111Z-001\\PILOT SURVEY DATA\\compiled_WK1 to WK4.xlsx'

#week_1 <- read_excel(filename, sheet = 2)
#week_2 <- read_excel(filename, sheet = 3)

#Template_Column_Names <- ('submission time', 'VC_ID',	'lat	long', 'market_type',	'Price_Ymaize_PerKg',	'price_Wmaize_perkg',	'price_Thailand_perkg',	'price_Indian_perkg')
week_data <- week_1
Template_Column_Names <- c('submission time', 'VC_ID',	'lat',	'long', 'market_type',	'Price_Ymaize_PerKg',	'price_Wmaize_perkg',	'price_Thailand_perkg',	'price_Indian_perkg')

list_file_names <- list.files("C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724",
                              full.names = T)


#list_file_path <- paste0("C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724/",list_file_names)

#list_file_names <- c(filename,filename)

list_data_processed <- lapply(list_file_names,
                              FUN=prep_col)


data_combined <- purrr::reduce(list_data_processed, full_join)
out_dir <- getwd()

out_filename <- file.path(out_dir,"data_combined1.csv")

write.csv(Data1, file = out_filename)

View(data_combined)

Data1 <-   rename(data_combined, submit_time=`submission time`)

yr<-c("2018", "2019")

Data2 <- Data1 %>%
    select(submit_time, contains('2018'))











##Working with the combined data

filename_new <- read.csv('data_combined1.csv',
                         na.strings = c('n/a', ' ', '..', '_'),
                         stringsAsFactors = F)


tidy_data <- gather(filename_new,
                    key = 'All_Commodities',
                    value = 'Price_Naira',
                    -submit_time, -Serial, -VC_ID, -lat, -long, -mkt_type)  

class(tidy_data$mkt_type)

tidy_data <- tidy_data %>%
  mutate(day = str_sub(submit_time, start = 9, end = 10),
         year = str_sub(submit_time, start = 1, end = 4),
         month = str_sub(submit_time, start = 6, end = 7),
         date = as.Date(str_sub(submit_time, start = 1, end = 10)))

test <-filter(tidy_data,mkt_type != '2.5')
test <- tidy_data[tidy_data$mkt_type !="2.5",]
table(test$market_type)
dim(test)

tidy_data$Price_Naira<-as.numeric(tidy_data$Price_Naira)

tidy_data_grouped <- tidy_data %>%
      filter(mkt_type != '2.5') %>%
      group_by(date, All_Commodities, mkt_type) %>%
      summarize(mean = mean(Price_Naira, na.rm = TRUE))

local_grainYM <- tidy_data_grouped %>% filter(tidy_data_grouped,
              mutate('YMaize',All_Commodities))

<-tidy_data_grouped %>% 
  filter(tidy_data_grouped, All_Commodities=="Ymaize")

import_grain<-tidy_data_grouped %>%
  filter(tidy_data_grouped, All_Commodities=="IndianRic", All_Commodities=="ThaiRice")




ggplot(na.omit(tidy_data_grouped),
       aes(x = date, y = mean)) + 
  geom_point() +
  geom_smooth(
    method = 'lm',
    aes(group = All_Commodities)) +
  facet_wrap(vars(mkt_type))

##Plotting all Commodities
ggplot(na.omit(tidy_data_grouped),
       aes(x = date, y = mean, color=All_Commodities)) + 
  geom_line() +
    facet_wrap(vars(mkt_type)) +
  theme(legend.position="bottomleft")


##Plotting local Commodity by market segment

  ggplot(na.omit(local_grain),
       aes(x = date, y = mean, color=All_Commodities)) + 
  geom_line() +
  facet_wrap(vars(mkt_type)) +
  theme(legend.position="bottomleft")




shp <- 'C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724/Urban_bdry_NG/major_urban_centres_polygon.shp'

urban_rural_sp <- st_read(
  shp,
  stringsAsFactors = FALSE)

plot(urban_rural_sp$geometry)

##cleaning

test2 <-filter(tidy_data,!is.na(tidy_data$lat))

test3 <-filter(test2,!is.na(test2$long))


View(test3)

epsg_code = 4326
tidy_data_sf <- st_as_sf(test3, coords = c("long", "lat"), crs = epsg_code)

class(tidy_data_sf)

View(tidy_data)
plot(tidy_data_sf)





Nig_bdry <- 'C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724/Urban_bdry_NG/Nigeria_new_states_2003_wgs84.shp'
Nig_bdry_sf <- st_read(Nig_bdry,
                       stringsAsFactors = FALSE)
plot(Nig_bdry_sf$geometry,
     border = 'blue', add = TRUE)
focal_states <- 'C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724/Urban_bdry_NG/focalstates_fpca.shp'
focal_states_sf <- st_read(focal_states,
  stringsAsFactors = FALSE)
plot(focal_states_sf)
LGAs_within <- 'C:/Users/SChiejile/Dropbox/SESYNC_Training/FPCA_Data_20190724/Urban_bdry_NG/LGAs_within.shp'
LGAs_wthn_sf <- st_read(LGAs_within,
                stringsAsFactors = FALSE)
plot(LGAs_wthn_sf$geometry,
     border = 'blue', add = TRUE)


In_states <- filter (tidy_data_sf, ((st_within(tidy_data_sf, focal_states_sf,sparse=F))[,1]))

urban_pts<-filter(In_states, ((st_within(In_states, urban_rural_sp, sparse=F))[,1]))

rural_pts<-filter(In_states, ((st_within(In_states, urban_rural_sp, sparse=F))[,2]))


In_states$urban<-as.integer(((st_within(In_states, urban_rural_sp, sparse=F))[,1]))


#urban_pts<-filter (In_states, ((st_within(In_states, urban_rural_sp, sparse=F))[,1]))

urban_pts$urban <- as.integer(urban_pts[,1])










file.exists(Nig_bdry) #TRUE

class(focal_states)


Summary_Date <- summarise_all(tidy_data,
                              su


C:\Users\SChiejile\Dropbox\SESYNC_Training\FPCA_Data_20190724


##Ignore these
prep_col("data_combined.csv")

list_data_processed[[1]]
list_data_processed[[2]]

names(week_1) <- str_replace(tolower(names(week_1)), ' ', '')
names(week_2) <- str_replace(tolower(names(week_2)),' ', '')

which(names(week_1) != names(week_2))

week_all <- rbind(week_1, week_2)

week_3 <- read_excel(filename, sheet = 4)
week_4 <- read_excel(filename, sheet = 5)

week_all <- rbind(week_1, week_2, week_3, week_4)

which(names(week_1) != names(week_2))

which(names(week_3) != names(week_4))

which(names(week_1) != names(week_3))

names(week_4) [[1]]

pilot_week_1 <- week_1
pilot_week_2 <- week_2
pilot_week_3 <- week_3
pilot_week_4 <- week_4

filename_crowd <- 'C:\\Users\\SChiejile\\Desktop\\FPCA\\Crowd Survey\\CLEANED DATA-20190711T090220Z-001\\CLEANED DATA\\week 1.xlxs'

week_1 <- read_excel(filename_crowd, sheet = 1)
