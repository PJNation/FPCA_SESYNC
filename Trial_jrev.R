library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)


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


##Plotting all Commodities by market segment
ggplot(na.omit(tidy_data_grouped),
       aes(x = date, y = mean, color=All_Commodities)) + 
  geom_line() 
#+geom_smooth(
 #   method = 'lm',
   # aes(group = All_Commodities), color="black")
+facet_wrap(vars(mkt_type)) +
  theme(legend.position="bottomleft")



#Subsetting dataset for each commodity 

local_grainYM <- subset(tidy_data_grouped, tidy_data_grouped$All_Commodities=="Ymaize")
local_grainWM <- subset(tidy_data_grouped, tidy_data_grouped$All_Commodities=="Wmaize")
import_grainTR <- subset(tidy_data_grouped, tidy_data_grouped$All_Commodities=="ThaiRice")
import_grainIR <- subset(tidy_data_grouped, tidy_data_grouped$All_Commodities=="IndianRic")

#Testing for significant difference in variance through time as an indicator of price volatility

##Null hypothesis is that imported commodity prices are less volatile compared to local commodity

test1<-var.test(local_grainYM$mean,local_grainWM$mean)
test2<-var.test(local_grainYM$mean,import_grainIR$mean)
test3<-var.test(import_grainTR$mean,import_grainIR$mean)


headers<-colnames(c("Test1", "Test2", "Test3"))
rbind(headers, c(test1[[3]],test2[[3]],test3[[3]]))

