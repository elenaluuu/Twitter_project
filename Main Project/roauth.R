
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Xr8PPfaPbS4I0Joa0sRDydsNU"
consumerSecret <- "7ls1Wjt2febEZcQpCNPERc6ZNjsPLv2zP19FVM54maYBg71Vpm"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
9353013
save(my_oauth, file = "my_oauth.Rdata")
