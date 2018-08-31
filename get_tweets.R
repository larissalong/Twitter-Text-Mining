# Web Scraping 

## The first step is to create an app on https://dev.twitter.com (My apps menu) and find an API Key,
## API secret, Access Token and Access Token Secret on Keys and Access Tokens menu tab.

# Set up Twitter Search API connection by authonitical keys
consumer_key <- 'your_consumer_key'
consumer_secret <- 'your_consumer_secret'
access_token <- 'your_access_token'
access_secret <- 'your_access_secret'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search for tweets containing keywords:
tweets <- searchTwitter('your_keywords', lang = "en", n = 50000, since = "2018-06-01", resultType = "mixed")
head(tweets, 20)

n.tweet <- length(tweets)
## Notes: Search API is not meant to be an exhaustive source of Tweets.
## Not all Tweets will be indexed or made available via the search interface.

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)
head(tweets.df, 5)

# Save data
saveRDS(tweets.df, file = "data/tweets_park_deleted.rds")
