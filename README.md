# Using News Headlines Sentiment Analysis to Predict Stock Prices
#####################################
# News Headline Sentiment Analysis  #
#####################################

library(rio)
github.url <- "https://github.com/VivianSihanZHENG/Final-Project/blob/master/"
file.url <- "rawnews.RData?raw=true" 
news <- import(paste0(github.url, file.url)) 

library(tidytext) #for easy text manipulation
library(SnowballC) #for stemming words
library(NLP)  
library(tm) 
library(dplyr) 

# Get a sentiment dictionary 
dict <- get_sentiments("bing") 

# Parse your text
# Create date formate 
toks <- news %>% 
  unnest_tokens(word, abstract)  %>% 
  mutate(word = wordStem(word)) 

toks$date <- as.Date(toks$date,"%Y-%m-%d") 

#Remove stopwords and blank tokens
data(stop_words)  
toks <- toks %>% 
  anti_join(stop_words) 

#Get Average Sentiment
toks <- toks %>% inner_join(dict) 

senti_character <- as.character(toks$sentiment)
senti_character <- gsub("negative","0", senti_character)  
senti_character <- gsub("positive","1", senti_character)  
toks$senti <- as.numeric(senti_character) 

senti_bydate <- aggregate(toks$senti, by = list(date = toks$date), FUN = mean) 
colnames(senti_bydate)[which(names(senti_bydate) == "x")] <- "avg.senti"

###################
##Load stock data##
###################

#Set Working Directory to Locate the 30 Companies' Data
##Need data path change 
setwd("/Users/shirleyfu/Desktop/Dow Jones") 

#Get the File List
##Need data path change
files <- list.files(path = "/Users/shirleyfu/Desktop/Dow Jones", recursive = TRUE,
                    pattern = "\\.csv$", full.names = FALSE)

# Loop the data frame consisting all 30 companies' data
data2=lapply(files, read.csv, header=TRUE, sep=",")
data_rbind <- do.call("rbind", data2) 

#Change the data type of Date as Date
data_rbind$date <- as.Date(data_rbind$date, format = "%Y-%m-%d")

#Get a new column of Year & Month so that we can subset the data for Year 2017
data_rbind$Year <- as.numeric(format(data_rbind$date, "%Y"))
data_rbind$Month <- as.numeric(format(data_rbind$date, "%m"))

#Generate a new data frame of year 2015-2017
data_4yrs <- subset(data_rbind, data_rbind$Year == 2017 | data_rbind$Year == 2016 | data_rbind$Year == 2015 | data_rbind$Year == 2014) 


#Create a new column of the status of price change, using open price
data_4yrs$ChangeOpening <- NA
data_4yrs$ChangeClose <- NA
data_4yrs$pctopen <- NA
data_4yrs$pctclose <- NA


firm_total <- data.frame(date = as.Date(NA),
                         open = NA,
                         close = NA,
                         name = NA,
                         ChangeOpening = NA,
                         ChangeClose = NA,
                         pctopen = NA,
                         pctclose = NA)  


#calculate opening price change 
firmlist <- unique(data_4yrs$Name) 

for (i in firmlist){ 
  firm <- data_4yrs[data_4yrs$Name == i, c("date","open","close","ChangeOpening","ChangeClose","pctopen","pctclose")] 
  
  #open price change 
  for (j in 2:nrow(firm)){ 
    if (firm$open[j] > firm$open[j-1]){
      firm$ChangeOpening[j-1] <- "increase" 
    } else if (firm$open[j] <= firm$open[j-1]){
      firm$ChangeOpening[j-1] <- "decrease"
    }
  }
  
  #close price change 
  for (j in 2:nrow(firm)){ 
    if (firm$close[j] > firm$close[(j-1)]){
      firm$ChangeClose[j] <- "increase"
    } else if (firm$close[j] <= firm$close[j-1]){
      firm$ChangeClose[j] <- "decrease"
    }
  } 
  
  #percentage change 
  for (j in 2:nrow(firm)){ 
    firm$pctopen[j-1] <- (firm$open[j] - firm$open[j-1])/firm$open[j-1] *100
    firm$pctclose[j] <- (firm$close[j] - firm$close[j-1])/firm$close[j-1] *100
  }
  
  firm$name <- i 
  firm_total <- rbind(firm_total, firm) 
} 

firm_total <- firm_total[-1,]

#######################
##Line Graph 30 Firms##
#######################

#2014-17 Stock Open Price
library(ggplot2)

ggplot(data = firm_total)+ 
  geom_line(mapping = aes(x=date,y=open),
                                     color="gold4")+
  facet_wrap(~name,ncol=6)+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10),
        axis.title.x=element_blank()) + 
  #scale_x_continuous(breaks = c(2015, 2018))+ 
  labs(y = "Firm Open Price", caption = "Kaggle") +
  ggtitle("30 Firms Open Price from 2014-2017") +
  theme(plot.title = element_text(hjust = 0.5))

#2014-17 Stock Close Price

ggplot(data = firm_total)+ 
  geom_line(mapping = aes(x=date,y=close),
            color="gold4")+
  facet_wrap(~name,ncol=6)+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10),
        axis.title.x=element_blank()) + 
  labs(y = "Firm Close Price", caption = "Kaggle") +
  ggtitle("30 Firms Close Price from 2014-2017") +
  theme(plot.title = element_text(hjust = 0.5))
  
  #################################################
# Merge news sentiment with stock price by date #
#################################################

sentiment_stock <- merge(senti_bydate, firm_total, by = "date", all = TRUE) 
sentiment_stock <- na.omit(sentiment_stock)
sentiment_stock$month <- as.numeric(format(sentiment_stock$date, "%m"))

sentiment_stock$ChangeOpening <- as.factor(sentiment_stock$ChangeOpening)
sentiment_stock$ChangeClose <- as.factor(sentiment_stock$ChangeClose)

sentiment_stock$quarter <- ceiling(as.numeric(sentiment_stock$month) / 3) 
sentiment_stock$year <- as.numeric(format(sentiment_stock$date, "%Y"))


#merge with GDP

#Load gdp 
library(rio)
github.url <- "https://github.com/VivianSihanZHENG/Final-Project/blob/master/"
file.url <- "gdp.xlsx?raw=true"

gdp <- import(paste0(github.url, file.url))
gdp$quarter <- gsub("Q","", gdp$quarter)
gdp <- gdp[,-3] 
gdp$quarter <- as.numeric(gdp$quarter) 
sentiment_stock <- merge(sentiment_stock, gdp, by = c("year","quarter")) 


#Interest rate
file.url <- "interest.xlsx?raw=true"
interest <- import(paste0(github.url, file.url))

colnames(interest)[which(names(interest) == "rate")] <- "interest.rate"
interest$interest.rate <- as.numeric(interest$interest.rate)
interest$year <- as.numeric(format(interest$date,"%Y"))
interest$month <- as.numeric(format(interest$date,"%m"))
interest <- interest[,-1]
sentiment_stock <- merge(sentiment_stock, interest, by = c("year","month")) 


#NASDQ
file.url <- "NASDQ.xlsx?raw=true"
NASDQ <- import(paste0(github.url, file.url))

colnames(NASDQ)[which(names(NASDQ) == "Date")] <- "date"
colnames(NASDQ)[which(names(NASDQ) == "Open")] <- "open_nasdq"
colnames(NASDQ)[which(names(NASDQ) == "Close")] <- "close_nasdq"
NASDQ <- NASDQ[,c("date", "open_nasdq","close_nasdq")]
NASDQ$date <- as.Date(NASDQ$date)
sentiment_stock <- merge(sentiment_stock, NASDQ, by = "date") 


#unemp
file.url <- "unemployment.xlsx?raw=true"
unemp <- import(paste0(github.url, file.url))

sentiment_stock$month <- as.numeric(sentiment_stock$month) 
sentiment_stock <- merge(sentiment_stock, unemp, by = c("year","month"))


#assets
file.url <- "asset.xlsx?raw=true"
asset<- import(paste0(github.url, file.url)) 
asset$name <- toupper(asset$name)
sentiment_stock <- merge(sentiment_stock, asset, by = c("year","quarter","name"))

