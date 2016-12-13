## Read data frame from the RDS file
tweets.df <- readRDS("VS.RDS")

### PART1
## Word cloud
library(twitteR)
library(tm)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(ggplot2)

#Combine data into one character
tweets_sample <- subset(tweets.df,tweets.df$favourites_count >50000)
twitter_text <- as.vector(data.frame(tweets_sample[[1]]))
twitter_text <- sapply(twitter_text, function(row) iconv(row, "latin1", "ASCII", sub=""))
review_text <- paste(twitter_text,collapse = "")

# Clean text
clean_tweet = gsub("&amp", "", review_text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
clean_tweet <- str_replace_all(clean_tweet," "," ")#get rid of unnecessary spaces
#clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","") # Get rid of URLs
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")# Get rid of references to other screennames 


# turn into Corpus
corpus = Corpus(VectorSource(clean_tweet))
#continue cleaning text using "tm" package
corpus <- tm_map(corpus, content_transformer(tolower)) # transform all into lower cases
corpus <- tm_map(corpus, PlainTextDocument) # transform into plain texts
corpus <- tm_map(corpus, stripWhitespace) # strip white spaces
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove useless daily words 
corpus <- tm_map(corpus, removePunctuation) #remove punctuations

#define dtm as matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

# get word counts in decreasing order
word_freqs <- colSums(dtm2)
word_freqs <- sort(word_freqs,decreasing = TRUE)

# delete those useless words and create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
pos <- grep("vs",dm$word)
pos <- c(pos,grep("victoria",dm$word))
pos <- c(pos,grep("secret",dm$word))
pos <- c(pos,grep("just",dm$word))
pos <- c(pos,grep("cant",dm$word))
pos <- c(pos,grep("even",dm$word))
dm1 <- dm[-pos,]
dm2 <- dm1[which(dm1$freq > 30),] #reduce the sample space of wordcloud


# Wordcloud present
colorVec = rep(c('pink', 'skyblue'), length.out=nrow(dm2))
wordcloud2(data = dm2,shape = "square",color = colorVec,size = 0.8,gridSize = 0.4,fontWeight = "bold",minRotation = -pi/2, maxRotation = -pi/2)
#This letter cloud cannot be compiled into any kind of file
#letterCloud(dm2,word = "VS",color = "pink",size = 1,minRotation = -pi/2, maxRotation = -pi/2)


# Most frequent words
# top-20 most frequent
dm_20 <- head(dm1,20)
# barplot 
ggplot(dm_20, aes(x=reorder(word,freq),y=freq)) +
  geom_bar(stat="identity", width = 0.4,colour="skyblue",fill="skyblue", alpha=1)+
  labs(x="",y="Frequency")+
  ggtitle("Top 20 Most Frequent Words")


## Frequency Analysis
# characters per tweet
chars_per_tweet = sapply(twitter_text, nchar)
summary(chars_per_tweet)
hist(chars_per_tweet,xlim = c(0,150),ylim = c(0,700),border = "gray20",
     main = "Histgram of number of characters per tweet",
     xlab = "Number of characters",
     ylab = "Frequency",
     col= "skyblue")

# split words
words_list = strsplit(twitter_text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,col = "skyblue", ylim = c(0,250),
        main="Distribution of words per tweet", cex.main=1)

# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
# barplot
barplot(table(round(wsize_per_tweet)), border=NA,col = "skyblue", xlim = c(0,25),
        xlab = "word length in number of characters",
        main="Distribution of words length per tweet", cex.main=1)
# how many unique words per tweet
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), border=NA,col = "skyblue",
        main="Distribution of unique words per tweet", cex.main=1)
# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))
barplot(table(hash_per_tweet),,border=NA,col = "skyblue",
        main="Distribution of hashtags per tweet", cex.main=1)
# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))

# combine all into a data frame
text_freq_df = data.frame(
  chars=chars_per_tweet,
  words = words_per_tweet,
  lengths = wsize_per_tweet,
  uniqs = uniq_words_per_tweet,
  hashs = hash_per_tweet,
  ats = ats_per_tweet)

# words -vs- chars
ggplot(text_freq_df, aes(x=words, y=chars)) +
  geom_point(colour="blue", alpha=0.2) +
  stat_smooth(method="lm",colour="red") +
  labs(x="number of words per tweet", y="number of characters per tweet") +
  ggtitle ("Tweets about 'VSFasionShow' \nNumber of words -vs- Number of characters") + 
  theme(plot.title = element_text(size=12))
# words -vs- word length
ggplot(text_freq_df, aes(x=words, y=lengths)) +
  geom_point(colour="blue", alpha=0.2) +
  stat_smooth(method="lm",colour = "red",se = TRUE) +
  labs(x="number of words per tweet", y="size of words per tweet") +
  ggtitle("Tweets about 'VSFasionShow' \nNumber of words -vs- Length of words")+
  theme(plot.title = element_text(size=12))


###PART2
## Mapping
library(grid)
library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyr)
require(RColorBrewer)
library(stringr)
library(stringdist)

# Reduce data set into data with geo locations
tweets.geo <- tweets.df[-which(is.na(tweets.df$full_name)),]
tweets.usa <- tweets.geo[-which(tweets.geo$country!="United States"),]
# Save into RDS files
saveRDS(tweets.geo,"tweets_geo.RDS")
saveRDS(tweets.usa,"tweets_usa.RDS")

# read RDS files
tweets.geo <- readRDS("tweets_geo.RDS")
tweets.usa <- readRDS("tweets_usa.RDS")

geo_location <- data.frame(tweets.usa$place_lon,tweets.usa$place_lat)
location_name <- data.frame(tweets.usa$full_name)
names(location_name) <- c("fullname")
twitter_location <- separate(location_name,fullname,c("City","State"),",")
location.df <- data.frame(City = twitter_location$City,
                          State = twitter_location$State,
                          Longitude = geo_location$tweets.usa.place_lon,
                          Latitude = geo_location$tweets.usa.place_lat)
location.df <-  na.omit(location.df)
location.df <- location.df[location.df$Latitude > 25,]
location.df <- location.df[location.df$Latitude < 50,]
# save RDS files
saveRDS(location.df,"usa_location.RDS")
# read RDS files
location.df <- readRDS("usa_location.RDS")
# get map data by states 
map.data <- map_data("state")
points <- data.frame(x=as.numeric(location.df$Longitude),
                     y=as.numeric(location.df$Latitude))

#tweet distribution in USA
ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "gray20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) + 
  ggtitle("Distribution of tweets in USA")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(plot.title = element_text(color = "black",size = 14,face = "bold"),
        legend.position = ("right")) + 
  geom_point(data = points,       
             aes(x = x, y = y), size = 1, 
             alpha = 1, color = "#f768a1") +
  stat_density2d(data = points,aes(x = x, y = y,fill = ..level.., alpha = ..level..), 
                 size = 0.1, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "#a6bddb", high ="#0570b0" ) + 
  scale_alpha_continuous(range =  c(0, 0.5), guide = FALSE)

## Top 10 most frequecy states
# Couting twitter by state in USA
StateCount<-as.data.frame(table(location.df$State))
colnames(StateCount)<-c("State.abb","Frequency")
StateCount<-arrange(StateCount,desc(Frequency))
pos2 <- grep("USA",StateCount$State.abb)
pos2 <- c(pos2,grep("Puerto Rico",StateCount$State.abb))
StateCount <- StateCount[-pos2,]
# find top 10 most frequecy states
state_10 <- head(StateCount,10)
ggplot(state_10, aes(x=reorder(State.abb,Frequency),y=state_10$Frequency)) +
  geom_bar(stat="identity", width = 0.4,colour="skyblue",fill="skyblue", alpha=1)+
  labs(x="",y="Frequency of twittes")+
  ggtitle("Top 10 most frequency states")


## California data
tweets.ca <- tweets.usa[grep("CA", tweets.usa$full_nam, ignore.case=FALSE),]

saveRDS(tweets.ca,"tweets_ca.RDS")
tweets.ca <- readRDS("tweets_ca.RDS")

# clean tweet text in CA for further analysis
tweets.ca.text <- as.vector(data.frame(tweets.ca[[1]]))
tweets.ca.text <- sapply(tweets.ca.text, function(row) iconv(row, "latin1", "ASCII", sub=""))

# frequency analysis in CA
# split words
ca_word_list <-  strsplit(tweets.ca.text, " ")
# words per tweet
ca_word_length = sapply(ca_word_list, length)
ca.word.length.df <- as.data.frame(ca_word_length)
# barplot
barplot(table(ca_word_length), border=NA,col = "pink", ylim = c(0,25),
        main="Distribution of words per tweet in California", cex.main=1)

# how many unique words per tweet
ca_word_uniq = sapply(ca_word_list, function(x) length(unique(x)))
ca.word.uniq.df <- as.data.frame(ca_word_uniq)
# barplot
barplot(table(ca_word_uniq), border=NA,col = "skyblue",ylim = c(0,25),
        main="Distribution of unique words per tweet in CA", cex.main=1)

# combine all CA informations in one data frame
tweets.ca1 <- cbind(tweets.ca,ca.word.length.df,ca.word.uniq.df)
# get CA mapping geolocations
map.data.ca <- map_data("state",region = "california")
points.ca <- data.frame(x=as.numeric(tweets.ca1$place_lon),
                        y=as.numeric(tweets.ca1$place_lat))
# compare word length between locations in CA
ggplot(map.data.ca) + 
  geom_map(aes(map_id = region), 
           map = map.data.ca, 
           fill = "white",       
           color = "gray20", size = 0.25) + 
  expand_limits(x = map.data.ca$long, y = map.data.ca$lat) +  
  ggtitle("Distribution of Word Length in CA")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(plot.title = element_text(color = "black",size = 14,face = "bold"))+
  geom_point(data = points.ca,       
             aes(x = x, y = y,color = tweets.ca1$ca_word_length), size = 2, 
             alpha = 1)+
  scale_color_continuous(low = "#fa9fb5",high = "#7a0177")


#compare unique words between locations in CA
ggplot(map.data.ca) + 
  geom_map(aes(map_id = region), 
           map = map.data.ca, 
           fill = "white",       
           color = "gray20", size = 0.25) + 
  expand_limits(x = map.data.ca$long, y = map.data.ca$lat) +  
  ggtitle("Distribution of unique words in CA")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(plot.title = element_text(color = "black",size = 14,face = "bold"))+
  geom_point(data = points.ca,       
             aes(x = x, y = y,color = tweets.ca1$ca_word_uniq), size = 2, 
             alpha = 1)+
  scale_color_continuous(low = "#fa9fb5",high = "#7a0177")

# Sentiment Analysis
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
require(RColorBrewer)
library(tm)
library(stringr)


# tweet text in usa
twitter_text_usa <- as.vector(data.frame(tweets.usa[[1]]))
twitter_text_usa <- sapply(twitter_text_usa, function(row) iconv(row, "latin1", "ASCII", sub=""))
#remove twitter handles
nohandles_usa <- str_replace_all(twitter_text_usa, "@\\w+", "")

#get sentimental
mySentiment_usa <- get_nrc_sentiment(nohandles_usa)

#combine these information into tweet data frame
tweets_usa1 <- cbind(tweets.usa, mySentiment_usa)

#look at the sentiment scores for the eight emotions from the NRC lexicon in aggregate for all VS tweets. 
sentimentTotals_usa <- data.frame(colSums(tweets_usa1[,c(43:50)]))
names(sentimentTotals_usa) <- "count"
sentimentTotals_usa <- cbind("sentiment" = rownames(sentimentTotals_usa), sentimentTotals_usa)
rownames(sentimentTotals_usa) <- NULL
sentimentTotals_usa

# summarise 8 sentimental scores in barplot
ggplot(data = sentimentTotals_usa, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + 
  ylab("Total Count") + 
  ggtitle("Total Sentiment Score for All VSFS Tweets")

# the percentage of each emotion 
barplot(
  sort(colSums(prop.table(mySentiment_usa[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  col = ("skyblue"),
  main = "Emotions in VSFS tweet text", xlab="Percentage"
)  

# repeat summary for postive/negative emotions
sentimentPN <- data.frame(colSums(tweets_usa1[,c(51:52)]))
names(sentimentPN) <- "count"
sentimentPN <- cbind("sentiment" = rownames(sentimentPN), sentimentPN)
rownames(sentimentPN) <- NULL
sentimentPN <- mutate(sentimentPN,percentage = sentimentPN$count/sum(sentimentPN$count))
sentimentPN

ggplot(data = sentimentPN, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Postive/Negative Attitudes of All VSFS Tweets")

# subset data frame,including geolocations and sentimental scores
sentiment.df <- select(tweets_usa1,id_str,positive,negative,full_name,place_lat,place_lon)
sentiment.df <-  na.omit(sentiment.df)
# focus on tweets in USA
sentiment.df <- sentiment.df[sentiment.df$place_lat > 25,]
sentiment.df <- sentiment.df[sentiment.df$place_lat < 50,]
sentiment.df <- sentiment.df[sentiment.df$place_lon < 0 ,]
# transfor sentiment level into positive, negative, and neutral
sentiment.df$level <- ifelse(sentiment.df$negative > sentiment.df$positive , "negative", 
                             ifelse(sentiment.df$negative < sentiment.df$positive, "positive", "neutral"))

saveRDS(sentiment.df,"sentiment_location.RDS") # for shiny app 

# plot attitude differences between locations
# this map is further produced by shiny app
ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "gray20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) +
  ggtitle("Distribution of Attitude in USA")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(plot.title = element_text(color = "black",size = 14,face = "bold"))+
  geom_point(data = points,       
             aes(x = x, y = y,color = sentiment.df$level,shape = sentiment.df$level), size = 2, 
             alpha = 1) 


## Lady Gaga
tweets.gaga <- tweets.df[grep("gaga", tweets.df$text, ignore.case=TRUE),] 
twitter_text_gaga <- as.vector(data.frame(tweets.gaga[[1]]))
twitter_text_gaga <- sapply(twitter_text_gaga, function(row) iconv(row, "latin1", "ASCII", sub=""))
#remove twitter handles
nohandles_gaga <- str_replace_all(twitter_text_gaga, "@\\w+", "")
#get sentimental
mySentiment_gaga <- get_nrc_sentiment(nohandles_gaga)
#combine these information into tweet data frame
tweets.gaga1 <- cbind(tweets.gaga, mySentiment_gaga)
#look at the sentiment scores for the eight emotions from the NRC lexicon in aggregate for all VS tweets. 
sentimentTotals_gaga <- data.frame(colSums(tweets.gaga1[,c(43:50)]))
names(sentimentTotals_gaga) <- "count"
sentimentTotals_gaga <- cbind("sentiment" = rownames(sentimentTotals_gaga), sentimentTotals_gaga)
rownames(sentimentTotals_gaga) <- NULL
sentimentTotals_gaga 

##Adriana Lima
tweets.lima <- tweets.df[grep("lima", tweets.df$text, ignore.case=TRUE),]  
twitter_text_lima <- as.vector(data.frame(tweets.lima[[1]]))
twitter_text_lima <- sapply(twitter_text_lima, function(row) iconv(row, "latin1", "ASCII", sub=""))
#remove twitter handles
nohandles_lima <- str_replace_all(twitter_text_lima, "@\\w+", "")
#get sentimental
mySentiment_lima <- get_nrc_sentiment(nohandles_lima)
#combine these information into tweet data frame
tweets.lima1 <- cbind(tweets.lima, mySentiment_lima)
#look at the sentiment scores for the eight emotions from the NRC lexicon in aggregate for all VS tweets. 
sentimentTotals_lima <- data.frame(colSums(tweets.lima1[,c(43:50)]))
names(sentimentTotals_lima) <- "count"
sentimentTotals_lima <- cbind("sentiment" = rownames(sentimentTotals_lima), sentimentTotals_lima)
rownames(sentimentTotals_lima) <- NULL
sentimentTotals_lima 

#combine two data frame
sentimentTotals_GL <- left_join(sentimentTotals_gaga,sentimentTotals_lima,"sentiment")  
colnames(sentimentTotals_GL) <- c("sentiment","gaga","lima")
sentimentTotals_GL <- gather(sentimentTotals_GL,"name","count",2:3)
sentimentTotals_GL
# plot sentiment scores comparison
ggplot(sentimentTotals_GL,aes(x=sentiment,y=count,fill=factor(name)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Name",
                      breaks=c(1, 2),
                      labels=c("gaga", "lima"))+
  xlab("sentiment")+
  ylab("count")+
  ggtitle("Emotions comparison between Gaga and Lima")+
  theme(plot.title = element_text(color = "black",size = 14,face = "bold"),
        legend.position = ("right"))