setwd("C:/Users/dez/Documents/R/R-Projects/Misc");

#install.packages("RJSONIO")
#library(RJSONIO);
#library(RCurl);

## grab the data
#url <- getURL("http://api.citybik.es/v2/networks/villo");
## Then covert from JSON into a list in R
#raw_data <- fromJSON(url);
#length(data);
## We can coerce this to a data.frame
#final_data <- do.call(rbind, data);
## Then write it to a flat csv file
#write.csv(final_data, "final_data.csv")

install.packages("googleVis")
install.packages("zoo")

install.packages("reshape2");
install.packages("stringr");

install.packages("jsonlite")
library(jsonlite)

#install.packages("tidyjson")
library(tidyjson)
library(dplyr)

#json <- fromJSON(txt="run_results_api-bikes-villo.json", method='C')
#tail(json, n=10)

#json %>%            # Use the %>% pipe operator to pass json through a pipeline 
#  as.tbl_json %>%   # Parse the JSON and setup a 'tbl_json' object
#  gather_array %>%  # Gather (stack) the array by index
#  spread_values(    # Spread (widen) values to widen the data.frame

#jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
json_data <- jsonlite::fromJSON(txt = "api.cotybik.es_v2_networks_villo.json", simplifyDataFrame = TRUE)

##examine the data structure
#str(json) 

str(json_data)
#'data.frame':	344 obs. of  8 variables:
#  $ empty_slots: int  23 12 19 18 2 35 9 15 4 3 ...
#$ extra      :'data.frame':	344 obs. of  7 variables:
#  ..$ address    : chr  "CHATEAU-KASTEEL

## Dimension of the data frame
#dim(json_data)
#[1] 344   8

## Check for the the data frame column names
#colnames(json_data)
#[1] "empty_slots" "extra"       "free_bikes"  "id"          "latitude"    "longitude"   "name"       
#[8] "timestamp"

## Check for the the data frame row names
# rownames(json_data)

library(reshape2);
library(stringr);

tidy <- arrange(json_data, name, timestamp)

#df_sub <- json_data[,-2]
#head(df_sub, n=10)
#sink("df_sub.txt");df_sub;sink()

#tail(json_data$extra$address, n=10)

tidy <- tidy[ c("free_bikes", "latitude", "longitude", "name", "timestamp") ]
#head(tidy, n=10)
#sink("tidy.txt");tidy;sink()

#summary(tidy)

## Let's bind json_data OR tidy with the address column, useful for geochart
#merge <- cbind(json_data,json_data$extra$address)
merge_tidy <- cbind(tidy,json_data$extra$address)

colnames(merge_tidy)
#[1] "free_bikes"              "latitude"                "longitude"              
#[4] "name"                    "timestamp"               "json_data$extra$address"

names(merge_tidy) <- c("free_bikes", "latitude", "longitude", "name", "timestamp", "address")
tidy <- merge_tidy[ c("free_bikes", "latitude", "longitude", "name", "timestamp", "address") ]
#tidy <- tidy[ c("free_bikes", "latitude", "longitude", "name", "timestamp", "address") ]

library(googleVis)
library(zoo)

#G1a <- gvisGeoChart(merge_tidy, locationvar='address', colorvar='free_bikes')
#plot(G1a)

library(RColorBrewer)
hist(merge_tidy$free_bikes, col=brewer.pal(8, 'Dark2'))
#hist(merge_tidy$free_bikes, main="Histogram for Free Bikes", xlab="Free Bikes", border="blue", col=brewer.pal(8, 'Dark2'), xlim=c(0,35), las=1, breaks=5)
lines(density(merge_tidy$free_bikes)) #Get a density curve to go along with the merge_tidy$free_bikes histogram

#idem for empty slots
hist(merge$empty_slots, main="Histogram for Empty Slots", xlab="Empty Slots", border="green", col=brewer.pal(8, 'Dark2'), las=1, prob = TRUE)
lines(density(merge$empty_slots))