library(rjson)
library(bit64)
library(httr)
library(devtools)
library(twitteR)
library(ROAuth)
library(streamR)

## Connect to twitter account 
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Xr8PPfaPbS4I0Joa0sRDydsNU"
consumerSecret <- "7ls1Wjt2febEZcQpCNPERc6ZNjsPLv2zP19FVM54maYBg71Vpm"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "my_oauth.Rdata")


##Search twitter: keyword = “VSFashionShow”
load("my_oauth.Rdata")

filterStream("tweets.json", 
             track=c("VSFashionShow"),
             timeout=6000, oauth=my_oauth,language = en)
tweets.df <- parseTweets("tweets.json", simplify=FALSE)


## Write table into RDS file
saveRDS(tweets.df,"VS.RDS")
