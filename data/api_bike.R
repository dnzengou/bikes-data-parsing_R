json_data <- jsonlite::fromJSON(txt = "api.cotybik.es_v2_networks_villo_noheader.json", simplifyDataFrame = TRUE)
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

df_sub <- json_data[,-2]
#head(df_sub, n=10)
#sink("df_sub.txt");df_sub;sink()

## Let's bind json_data OR tidy with the address column, useful for geochart
#merge <- cbind(json_data,json_data$extra$address)
merge <- cbind(df_sub,json_data$extra$address)
#head(merge, n=10)

## Check for the the data frame row names
# rownames(json_data)

library(reshape2);
library(stringr);

#tidy <- arrange(merge, name, timestamp)
#tail(json_data$extra$address, n=10)

tidy <- tidy[ c("free_bikes", "latitude", "longitude", "name", "timestamp") ]
#head(tidy, n=10)
#sink("tidy.txt");tidy;sink()

#summary(tidy)

#colnames(merge)
#[1] "free_bikes"              "latitude"                "longitude"              
#[4] "name"                    "timestamp"               "json_data$extra$address"

names(merge) <- c("empty_slots", "free_bikes", "id", "latitude", "longitude", "name", "timestamp", "address")
tidy <- merge[ c("empty_slots", "free_bikes", "latitude", "longitude", "name", "timestamp", "address") ]
#head(tidy, n=10)

library(googleVis)
library(zoo)

#G1a <- gvisGeoChart(merge_tidy, locationvar='address', colorvar='free_bikes')
#plot(G1a)

# Simple Scatterplot
x <- tidy[ c("free_bikes") ]

attach(tidy)
plot(main="Scatterplot free_bikes by location", 
     xlab="name", ylab="free_bikes ", pch=19)

library(RColorBrewer)
hist(merge_tidy$free_bikes, col=brewer.pal(8, 'Dark2'))
#hist(merge_tidy$free_bikes, main="Histogram for Free Bikes", xlab="Free Bikes", border="blue", col=brewer.pal(8, 'Dark2'), xlim=c(0,35), las=1, breaks=5)
lines(density(merge_tidy$free_bikes)) #Get a density curve to go along with the merge_tidy$free_bikes histogram

#idem for empty slots
hist(merge$empty_slots, main="Histogram for Empty Slots", xlab="Empty Slots", border="green", col=brewer.pal(8, 'Dark2'), las=1, prob = TRUE)
lines(density(merge$empty_slots))

p <- plot(tidy$free_bikes,tidy$empty_slots, xlab="Empty Slots", ylab="Free bikes",main="Free bikes vs. Empty Slots", col=brewer.pal(8, 'Dark2'), pch=16);
#plot(tidy$free_bikes,tidy$empty_slots, xlab="Free bikes", ylab="Empty Slots",main="Empty Slots vs. Free bikes", col=rgb(0,100,0,50,maxColorValue=255), pch=16);
##OR
#p <- plot(tidy$free_bikes,tidy$empty_slots, xlab="Free bikes", ylab="Empty Slots",main="Free bikes vs. Empty Slots", col=brewer.pal(8, 'Dark2'), pch=16);
#w <- tidy$free_bikes
#h <- hist(tidy$free_bikes, col=brewer.pal(8, 'Dark2'))
#wfit<-seq(min(w),max(w),length=30) 
#yfit<-dnorm(wfit,mean=mean(w),sd=sd(w)) 
#yfit <- yfit*diff(h$mids[1:2])*length(w) 
#lines(wfit, yfit, col="blue", lwd=2)

##run a linear regression of Empty Slots on Free Bikes
reg<-lm(tidy$empty_slots~tidy$free_bikes, data=tidy)
##add the linreg to the plot
abline(reg)