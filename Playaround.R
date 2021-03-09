library(rtweet)
library(dplyr)
library(tidygraph)
library(vader)
library(syuzhet)
library(tidytext)

#### Section: Extract Data From Twitter ####

#Read our list of twitter accounts from csv
df = read.csv("TwitterAccs.csv",
              header=TRUE, stringsAsFactors=FALSE)

#Get information about our users
user_db = lookup_users(users = df$Acc)
write_as_csv(user_db, "user_db.csv")
user_filtered = user_db %>% select(screen_name, followers_count, protected, statuses_count) #Only select relevant columns
write_as_csv(user_filtered, "user_filtered.csv")


user_filtered %>% arrange(statuses_count) %>% head(20) #display users with least number of tweets
user_filtered %>% arrange(followers_count) %>% head(20) #display users with least followers
user_filtered %>% arrange(desc(followers_count)) %>% head(20) #display users with most followers
user_filtered %>% group_by(protected) %>% summarise(number=n()) #Check how many users are protected/unprotected


#Get latest 200 entries from all users, which are at least 7 days old (excluding retweets)
timeline = get_timeline(user_db = df$Acc, n=200, check = FALSE, include_rts = FALSE)
timeline_db = timeline %>% filter(timeline$created_at < "2021-03-02 00:00:00 UTC")
timeline$created_at %>% head(5) #Check the dates
timeline_db$created_at %>% head(5) #Check dates after filtering out last week

write_as_csv(timeline, "timeline.csv") #10277 entries
write_as_csv(timeline_db, "timeline_db.csv") #7922 entries

#Create random samples for labeling

set.seed(14) #Set seed for randomization
rows = sample(nrow(timeline_db)) #Randomly sample rows
timeline_filtered = timeline_db[rows, ] %>% head(200)#Reshuffle dataframe and only select 200 records for labeling
timeline_filtered = timeline_filtered %>% select(created_at, screen_name,followers_count, retweet_count, text) #Only keep relevant columns
write_as_csv(timeline_filtered, "timeline_filtered.csv")

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

#Replace negative emotions with negative
nrcdf2$sentiment[nrcdf2$sentiment == "anger"] = "negative"
nrcdf2$sentiment[nrcdf2$sentiment == "disgust"] = "negative"
nrcdf2$sentiment[nrcdf2$sentiment == "sadness"] = "negative"

#Summarize (DOES NOT YET WORK)
nrcdf3 = nrcdf2 %>% group_by(id) %>% summarize(sent = mlv(sentiment, method = 'mfv')[['M']])
