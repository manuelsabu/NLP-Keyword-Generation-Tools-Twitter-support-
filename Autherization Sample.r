
library(ROAuth)
library(twitteR)
library(RCurl)
# Set SSL certs globally
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

require(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- # Your consumer Key goes here
consumerSecret <- # Your consumer Secret goes here

twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(list="twitCred", file="twitteR_credentials.Rdata")