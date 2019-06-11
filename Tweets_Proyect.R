dir()
setwd("C:/Users/amtre/Downloads")
datos <- read.csv("russia_201901_tweets.csv",header = T)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(corrplot)
glimpse(datos)
datos_estudio <- datos[,-c(1,2,4,5,7,8,16:18,20,20:24,28:31)]
glimpse(datos_estudio)

#Text Recognition
number <- c(sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"Trump")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"Obama")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"Clinton")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"Muslim")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"America")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"kidnap")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"dark")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"fake")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"bomb")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"death")),
sum(str_detect(datos_estudio$tweet_text[1:nrow(datos_estudio)],"https")))
text <- c('Trump','Obama','Clinton','Muslim','America','kidnap','dark','fake','bomb','death','https')
text <- as.factor(text)
text_relation <- data.frame(text,number)
grafica1 <- ggplot(text_relation, aes(text,number)) +
 geom_col(fill = "orange", colour = "black") +
  xlab("Character") + 
  ylab("Frequency") +
  labs(title = "Most and least banned character in a tweet") +
  theme(plot.title = element_text(hjust = 0.5))

#Add a column of number of characters  
num_caracter <- nchar(datos_estudio$tweet_text[1:nrow(datos_estudio)])  
datos_estudio <- mutate(datos_estudio, numcaracter = num_caracter)

#check for missing values by column
apply(datos_estudio, 2, function(x) any(is.na(x)))
apply(datos_estudio, 2, function(x) sum(is.na(x)))
datos_estudio <- datos_estudio[!is.na(datos_estudio$reply_count),]

#Cleaning of location column
datos_estudio$user_reported_location <- as.character(datos_estudio$user_reported_location)
index <- which(datos_estudio$user_reported_location == "USA  #IslamIsTheProbem #WakeUp")
datos_estudio$user_reported_location[index] <- gsub("(USA).*","\\1",datos_estudio$user_reported_location[index])
index2 <- which(datos_estudio$user_reported_location == "World ðYO\u008d *BFF Of SamTheInfidel")
datos_estudio$user_reported_location[index2] <- gsub("(World).*","\\1",datos_estudio$user_reported_location[index2])
index3 <- which(datos_estudio$user_reported_location == "United States ðYOZðY???ºðY???¸")
datos_estudio$user_reported_location[index3] <- gsub("(United States).*","\\1",datos_estudio$user_reported_location[index3])
index4 <- which(datos_estudio$user_reported_location == "United States")
datos_estudio$user_reported_location[index4] <- gsub("United States","USA",datos_estudio$user_reported_location[index4])
index5 <- which(datos_estudio$user_reported_location == "United states")
datos_estudio$user_reported_location[index5] <- gsub("United states","USA",datos_estudio$user_reported_location[index5])
index6 <- which(datos_estudio$user_reported_location == "New York")
datos_estudio$user_reported_location[index6] <- gsub("New York","USA",datos_estudio$user_reported_location[index6])
index7 <- which(datos_estudio$user_reported_location == "New York, USA")
datos_estudio$user_reported_location[index7] <- gsub("New York, USA","USA",datos_estudio$user_reported_location[index7])
index8 <- which(datos_estudio$user_reported_location == "Jersey City, NJ")
datos_estudio$user_reported_location[index8] <- gsub("Jersey City, NJ","USA",datos_estudio$user_reported_location[index8])
index9 <- which(datos_estudio$user_reported_location == "Paris, France")
datos_estudio$user_reported_location[index9] <- gsub("Paris, France","France",datos_estudio$user_reported_location[index9])
index10 <- which(datos_estudio$user_reported_location == "Russia, Kaliningrad")
datos_estudio$user_reported_location[index10] <- gsub("Russia, Kaliningrad","Russia",datos_estudio$user_reported_location[index10])
index11 <- which(datos_estudio$user_reported_location == "Wichita, Kansas")
datos_estudio$user_reported_location[index11] <- gsub("Wichita, Kansas","USA",datos_estudio$user_reported_location[index11])
index12 <- which(datos_estudio$user_reported_location == "USA/England/Spain/Itay/Germany")
datos_estudio$user_reported_location[index12] <- gsub("USA/England/Spain/Itay/Germany","USA",datos_estudio$user_reported_location[index12])
index13 <- which(datos_estudio$user_reported_location == "Miami, FL")
datos_estudio$user_reported_location[index13] <- gsub("Miami, FL","USA",datos_estudio$user_reported_location[index13])
index14 <- which(datos_estudio$user_reported_location == "New York, NY")
datos_estudio$user_reported_location[index14] <- gsub("New York, NY","USA",datos_estudio$user_reported_location[index14])
index15 <- which(datos_estudio$user_reported_location == "Sankt-Petersburg")
datos_estudio$user_reported_location[index15] <- gsub("Sankt-Petersburg","Russia",datos_estudio$user_reported_location[index15])
index16 <- which(datos_estudio$user_reported_location == "New Jersey, USA")
datos_estudio$user_reported_location[index16] <- gsub("New Jersey, USA","USA",datos_estudio$user_reported_location[index16])
index17 <- which(datos_estudio$user_reported_location == "Lyon, France")
datos_estudio$user_reported_location[index17] <- gsub("Lyon, France","France",datos_estudio$user_reported_location[index17])
index18 <- which(datos_estudio$user_reported_location == "Newcastle")
datos_estudio$user_reported_location[index18] <- gsub("Newcastle" ,"England",datos_estudio$user_reported_location[index18])
index19 <- which(datos_estudio$user_reported_location == "")
datos_estudio$user_reported_location[index19] <- gsub("","Unknown Location",datos_estudio$user_reported_location[index19])
index20 <- which(datos_estudio$user_reported_location == "United States")
datos_estudio$user_reported_location[index20] <- gsub("United States","USA",datos_estudio$user_reported_location[index20])

#Column account_creation_date
account_year <- year(datos_estudio$account_creation_date <- ymd(datos_muestreo$account_creation_date))
account_month <- month(datos_estudio$account_creation_date <- ymd(datos_muestreo$account_creation_date)) 
datos_estudio <- mutate(datos_estudio, account_creation_year = account_year)
datos_estudio <- mutate(datos_estudio, account_creation_month = account_month)

#We descart account_creation_date column
datos_estudio <- datos_estudio[,-5]

#Column tweet_time
datos_estudio <- datos_estudio[,-5]
tweet_hour <- hour(datos_estudio$account_creation_date <- ymd_hm(datos_estudio$tweet_time))
datos_estudio <- mutate(datos_estudio, tweet_hour = tweet_hour)

#We descart tweet_time
datos_estudio <- datos_estudio[,-7]

#Column tweet_client_name
levels(datos_estudio$tweet_client_name)[levels(datos_estudio$tweet_client_name)==""] <- "Unknown Client"
highest_10_values <- order(-as.data.frame(table(datos_estudio$tweet_client_name))[,2])[1:10]
highest_10_clients <- as.factor(levels(datos_estudio$tweet_client_name)[highest_10_values])
subset(datos_estudio, tweet_client_name %in% highest_10_clients)
grafico2 <- ggplot(subset(datos_estudio, tweet_client_name %in% highest_10_clients),aes(tweet_client_name)) +
  geom_bar(fill = "green", colour = "black") + 
  xlab("Client Name") + 
  ylab("Frequency") +
  labs(title = "Top 10 clients used for sending tweets") +
  theme(plot.title = element_text(hjust = 0.5))


#Top 10 banned users
highest_10_values_2 <- order(-as.data.frame(table(datos_estudio$userid))[,2])[1:10]
highest_10_users <- as.factor(levels(datos_estudio$userid)[highest_10_values_2])
datos_estudio_topusers <- subset(datos_estudio, datos_estudio$userid %in% highest_10_users)
#Refresh factor levels
datos_estudio_topusers$userid <- factor(datos_estudio_topusers$userid)
#Rename userid levels for graphical purposes
levels(datos_estudio_topusers$userid) <- c("User1","User2","User3","User4","User5","User6","User7","User8","User9","User10")
grafico3 <- ggplot(datos_estudio_topusers,aes(userid)) +
  geom_bar(fill = "yellow", colour = "black") + 
  xlab("Renamed userid") + 
  ylab("Frequency") +
  labs(title = "Top 10 banned users") +
  theme(plot.title = element_text(hjust = 0.5))


#Most frequent tweet hour
ggplot(datos_estudio,aes(tweet_hour)) +
  geom_histogram(fill = "red", colour = "black", bins = 24) + 
  xlab("Hour") + 
  ylab("Frequency") +
  labs(title = "Tweet Hour") +
  theme(plot.title = element_text(hjust = 0.5))

#Backup Data
datos_respaldo <- datos_estudio
write.csv(datos_estudio,'russia_201901_tweets_clean_backup.csv')

##Convert variables to categorical## 

#account_language variable
account_language_code <- unique(datos_estudio$account_language)
datos_estudio$account_language  <- factor(datos_estudio$account_language, levels = account_language_code)
datos_estudio$account_language <- as.integer(datos_estudio$account_language) 

#tweet_client_name variable
tweet_client_name_code <- unique(datos_estudio$tweet_client_name)
datos_estudio$tweet_client_name <- factor(datos_estudio$tweet_client_name, levels = tweet_client_name_code)
datos_estudio$tweet_client_name <- as.integer(datos_estudio$tweet_client_name)

#is_retweet variable
is_retweet_code <- unique(datos_estudio$is_retweet)
datos_estudio$is_retweet <- factor(datos_estudio$is_retweet, levels = is_retweet_code)
datos_estudio$is_retweet <- as.integer(datos_estudio$is_retweet)
write.csv(datos_estudio,'russia_201901_tweets_cleaned_.csv')
datos_estudio <- read.csv("russia_201901_tweets_cleaned_.csv",header = T, row.names = 1)

#Correlation Matrix
X <- as.matrix(datos_estudio)
Mcor <- cor(X)
color <- colorRampPalette(c("blue","red","yellow"))(10)
M_correlaciones <- corrplot(Mcor, method = "number", col = color, type = "full",number.cex= .5)
M_correlaciones

#Sentiment Analysis
head(datos_estudio_topusers$tweet_text)
datos_respaldo <- datos_estudio_topusers
#Quitar RT
datos_respaldo <- datos_respaldo[which(datos_respaldo$is_retweet=="False"),]
datos_text <- gsub("http.*","",datos_respaldo$tweet_text)
datos_text <- gsub("https.*","",datos_text)
datos_text <- gsub("#.*","",datos_text)
datos_text <- gsub("@.*","",datos_text)
#Quitar ""
datos_text <- datos_text[-c(which(datos_text==""))]
#Data Lista
library(syuzhet)
words <- as.vector(datos_text)
emotions <- get_nrc_sentiment(words)
emotions_2 <- cbind(datos_text, emotions) 
head(emotions_2,n=2)
sentiment_value <- get_sentiment(words)
most_positive <- words[sentiment_value == max(sentiment_value)]
most_positive
most_negative <- words[sentiment_value <= min(sentiment_value)] 
most_negative
