library(rtweet)
library(dplyr)
library(tidygraph)
library(vader)
library(syuzhet)
library(tidytext)
library(tibble)

#Change for your system
setwd("C:/Users/fabia/Desktop/MSc RSC/FS 2021/SDS/Project")

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

#### Section: Sentiment with World Libraries ####
#THIS SECTION IS NOT USED

df = read.csv("data.csv")

#Add an id column
df$id = seq.int(nrow(df))

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


#### Section: Cleanliness ####


df = read.csv("timeline_filtered_anno.csv",
              header=TRUE, stringsAsFactors=FALSE)

#Split into the two groups
df1 = df %>% head(100)
df2 = df %>% tail(100)

#Replace sentiments of non-relevant tweets with NA
select = df1$Rel1 == 0
df1$Sentiment1[select] = NA
select = df1$Rel2 == 0
df1$Sentiment2[select] = NA

#For all 4 labelers get a distribution of what they labeled (Positive, Negative, Neutral, Non-relevant)
sent1_1 = df1 %>% group_by(Sentiment1) %>% summarise(number = n())
sent1_2 = df1 %>% group_by(Sentiment2) %>% summarise(number = n())
sent2_1 = df2 %>% group_by(Sentiment1) %>% summarise(number = n())
sent2_2 = df2 %>% group_by(Sentiment2) %>% summarise(number = n())

#Calculate percentage of relevant, positive, negative and neutral tweets
eval = data.frame(sent1_1$number, sent1_2$number, sent2_1$number, sent2_2$number)
non_relevant = eval[4,] / nrow(df1)
relevant = 1 -(eval[4,] /(nrow(df1)))

#If set to 'all', calculates positive, negative and neutral percentage wrt to all labeled tweets
#Otherwise wrt to only the relevant tweets
set_total = 'rel'

if(set_total == 'all'){
  negative = eval[1,] / (100)
  neutral = eval[2,] / (100)
  positive = eval[3,] / (100)
} else {
  negative = eval[1,] / (100 * relevant)
  neutral = eval[2,] / (100 * relevant)
  positive = eval[3,] / (100 * relevant)
} 
  
#Combine results into single df
analysis_labeling = rbind(relevant, negative, neutral, positive, non_relevant)
analysis_labeling = as_tibble(t(analysis_labeling))
colnames(analysis_labeling) = c("relevant", "negative", "neutral", "positive", "non_relevant")


#Check agreement between regarding relevance of the tweets
df1$agree = (df1$Rel1 == df1$Rel2)
agreement_relevance1 = df1 %>% group_by(agree) %>% summarise(number = n())

df2$agree = (df2$Rel1 == df2$Rel2)
agreement_relevance2 = df2 %>% group_by(agree) %>% summarise(number = n())


#Check agreement regarding sentiment and relevance
df1[is.na(df1)] = -5 #Set all NA values to -5, such that the == operator does not evaluate to NA
df2[is.na(df2)] = -5

df1$agree = (df1$Sentiment1 == df1$Sentiment2)
agreement_overall1 = df1 %>% group_by(agree) %>% summarise(number = n())

df2$agree = (df2$Sentiment1 == df2$Sentiment2)
agreement_overall2 = df2 %>% group_by(agree) %>% summarise(number = n())


#### Section: Sentiment Classification Trial Set ####

#Read in the annotated csv file for classification
df = read.csv("timeline_filtered_anno.csv",
              header=TRUE, stringsAsFactors=FALSE)

#Analyse the tweets with vader 
vaderdf = vader_df(df$text)
df$vader = vaderdf$compound
df$pos = vaderdf$pos
df$neg = vaderdf$neg
df$neu = vaderdf$neu

write_as_csv(df, 'timeline_filtered_classified_test.csv')

#Analyze the tweets with syuzhet
syudf = get_sentiment(df$text)
df$syuzhet = syudf

#Set threshold
positive_threshold = 0.8
negative_threshold = -0.3

#Assign all values as neutral/non-relevant
df$threshold = 0

df$threshold[df$vader >= positive_threshold] = 1
df$threshold[df$vader <= negative_threshold] = -1

#Set sentiments of non-relevant tweets to neutral (0)

df$Sentiment1[df$Rel1 == 0] = 0
df$Sentiment2[df$Rel2 == 0] = 0

#Create a confusion matrix and calculate true positives and true negatives
conf1 = table(df$Sentiment1, df$threshold)
true_neg1 = conf1[1,1] / (conf1[1,1] + conf1[2,1] + conf1[3,1])
true_pos1 = conf1[3,3] / (conf1[1,3] + conf1[2,3] + conf1[3,3])

conf2 = table(df$Sentiment2, df$threshold)
true_neg2 = conf2[1,1] / (conf2[1,1] + conf2[2,1] + conf2[3,1])
true_pos2 = conf2[3,3] / (conf2[1,3] + conf2[2,3] + conf2[3,3])

print(conf1)

write_as_csv(df, 'timeline_filtered_classified.csv')


#### Section: Sentiment Classification Full Set ####

#Import all tweets extracted and at least 1 week old at the time
df = read.csv("timeline_db.csv")

#Classify tweets according to vader and add precise score to df
#dfvader = vader_df(df$text)
#Store vader classification (takes ~20 min to run)
#write_as_csv(dfvader, 'vader_classification_timeline_db.csv')

dfvader = read.csv('vader_classification_timeline_db.csv')
df$vader = dfvader$compound

#Set threshold (same as in trial set)
positive_threshold = 0.8
negative_threshold = -0.3

#Set labels according to threshold
df$threshold = 0

df$threshold[df$vader >= positive_threshold] = 1
df$threshold[df$vader <= negative_threshold] = -1

#Get Insights
#Numbers of tweets classified as positive (937), neutral (5867) and negative(1118)
df %>% group_by(threshold) %>% summarise(count = n())

#Select 100 most positive and most negative tweets
df_positive = df %>% arrange(desc(vader)) %>% head(100)
df_negative = df %>% arrange(vader) %>% head(100)

#Save all the dataframes
write_as_csv(df, 'timeline_db_classified.csv')
write_as_csv(df_positive, 'timeline_db_positive.csv')
write_as_csv(df_negative, 'timeline_db_negative.csv')

#### Section: Graphs and Results ####
