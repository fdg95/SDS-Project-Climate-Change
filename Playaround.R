library('rtweet')
library(dplyr)
library(tidygraph)
library(vader)
library(syuzhet)
library(tidytext)

#Read our list of twitter accounts from csv

df = read.csv("TwitterAccs.csv",
                        header=TRUE, stringsAsFactors=FALSE)


#### Section: Extract Data From Twitter ####

#Get information about our users
user = lookup_users(users = df$Acc)

#Get latest 200 entries from all users
timeline = get_timeline(user = df$Acc, n=200, check = FALSE, include_rts = FALSE)


save(timeline, file = "timelines.RData")

#### Section: Data Wrangling ####

#Filter out retweets
data = timeline[timeline$is_retweet == FALSE,]

data = data %>% select(created_at, screen_name, text, retweet_count)
user = user %>% select(screen_name, followers_count)

write_as_csv(storagedata, "data.csv")
write_as_csv(storageuser, "user.csv")

#### Section: Sentiment Analysis with Vader ####



vaderdf = vader_df(data$text)


#### Section: Sentiment Analysis with Syuzhet ####


syudf = get_sentiment(data$text)

#### Section: Adding UP ####


#Adding Sentiment to the data for better checking
data$vader = vaderdf$compound
data$syudf = syudf
write_as_csv(data, "data.csv")

#### Section: Loading the data ####

df = read.csv("data.csv")

#Add an id column
df$id = seq.int(nrow(df))


#### Section: Sentiment with World Libraries ####

#Store text and create tible
texts = df$text
#Column id and text
textdf = tibble(id = df$id, text = texts)

#Unnest all texts to a new df with columns word and id
textdf %>% unnest_tokens(word, text) -> wordsdf

#Import Stop words and remove from wordsdf
data(stop_words)
wordsdf = wordsdf %>% anti_join(stop_words)

#Find sentiment for each word and summarize by id, taking the average using afinn
wordsdf %>% inner_join(get_sentiments("afinn")) %>% group_by(id) %>%
  summarize(sent = mean(value), n=n()) -> afinndf

afinndf %>% head(20)

wordsdf %>% inner_join(get_sentiments("nrc")) -> nrcdf

#Only filter out desired emotions (e.g trust/joy/anticipation)
nrcdf %>% filter(sentiment %in% c("anger", "negative", "positive", "sadness", "disgust")) -> nrcdf2
nrcdf2$sentiment[nrcdf2$sentiment == "anger"] = "negative"
nrcdf2$sentiment[nrcdf2$sentiment == "disgust"] = "negative"
nrcdf2$sentiment[nrcdf2$sentiment == "sadness"] = "negative"

nrcdf3 = nrcdf2 %>% group_by(id) %>% summarize(sent = mlv(sentiment, method = 'mfv')[['M']])
