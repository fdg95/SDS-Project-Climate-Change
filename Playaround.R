library(rtweet)
library(dplyr)
library(tidygraph)
library(vader)
library(syuzhet)
library(tidytext)
library(tibble)
library(stringr)
library(ggplot2)
library(vioplot)

#Change for your system
setwd("C:/Users/fabia/Desktop/MSc RSC/FS 2021/SDS/Project")
options(device = "windows")

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
timeline_filtered = timeline_filtered %>% select(created_at, screen_name, followers_count, retweet_count, text) #Only keep relevant columns
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

#See what kind of Holiday tweets are removed and remove them
holidays = df %>% filter(str_detect(text, "merry|Merry|happy birthday|Happy Birthday|happy Birthday|Holiday|New Year"))
df = df %>% filter(!str_detect(text, "merry|Merry|happy birthday|Happy Birthday|happy Birthday|Holiday|New Year"))
#Remove account with almost all tweets at least twice
climatedesk = df %>% filter(str_detect(screen_name, "ClimateDesk"))
df = df %>% filter(!str_detect(screen_name, "ClimateDesk"))
#Check duplicate tweets and keep only unique ones
duplicates = df[duplicated(df$text),]
df = df %>% distinct(text, .keep_all = TRUE)

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

#Select only relevant columns for the dataframes that require labeling
df_positive = df_positive %>% select(screen_name, followers_count, retweet_count, favorite_count, text, vader, threshold)
df_negative = df_negative %>% select(screen_name, followers_count, retweet_count, favorite_count, text, vader, threshold)

#Import classified dataframe
df_pos = read.csv('timeline_db_positive_labeled.csv')
df_neg = read.csv('timeline_db_negative_labeled.csv')

#Select only new columns and merger columns
df_pos = df_pos %>% select(text, Rel1, Sentiment1, Rel2, Sentiment2)
df_neg = df_neg %>% select(text, Rel1, Sentiment1, Rel2, Sentiment2)

#Merge with newly classified
df_positive = merge(x = df_positive, y = df_pos, by = 'text', all.x = TRUE)
df_negative = merge(x = df_negative, y = df_neg, by = 'text', all.x = TRUE)

#Save all the dataframes
write_as_csv(df, 'timeline_db_classified.csv')
write_as_csv(df_positive, 'timeline_db_positive.csv')
write_as_csv(df_negative, 'timeline_db_negative.csv')


#### Section: Bootstrapping ####

#Create dataframe one wants to test (both positive and negative data together! Threshold column will be selected!)
df = read.csv('timeline_db_classified.csv')

#Create dataframe from labeled data
dfpos = read.csv('timeline_db_positive_labeled2.csv')
dfneg = read.csv('timeline_db_negative_labeled2.csv')
#Assign labels of first person (can be changed later)
dfpos$threshold = dfpos$Sentiment1
dfneg$threshold = dfneg$Sentiment1
df = rbind(dfpos, dfneg)

#Select positive and negative tweets in separate dataframe
df_positive = df %>% filter(df$threshold == 1)
df_negative = df %>% filter(df$threshold == -1)

#Select only tweets with responses greater than 1
#df_positive = df_positive %>% filter(df_positive$retweet_count >= 1)
#df_negative = df_negative %>% filter(df_negative$retweet_count >= 1)

#Select only retweet count and follower count
df_positive = df_positive %>% select(retweet_count, followers_count)
df_negative = df_negative %>% select(retweet_count, followers_count)

#Calculate log of retweet and followers
df_positive$retweet_log = log(df_positive$retweet_count)
df_negative$retweet_log = log(df_negative$retweet_count)
df_positive$follower_log = log(df_positive$followers_count)
df_negative$follower_log = log(df_negative$followers_count)

#Replace 0 retweets with 0 in log-scale
df_positive$retweet_log[df_positive$retweet_log  == -Inf] = 0
df_negative$retweet_log[df_negative$retweet_log == -Inf] = 0

#Fit regression model
posreg = lm(df_positive$retweet_log ~ df_positive$follower_log)
negreg = lm(df_negative$retweet_log ~ df_negative$follower_log)

slope_diff_observed = negreg$coefficients[2] - posreg$coefficients[2]

#Plot results
plot(x = df_positive$follower_log, y = df_positive$retweet_log, pch = 20, col = rgb(0,0,1), xlab = "Log of Followers", ylab = "Log of Retweets")
points(x = df_negative$follower_log, y = df_negative$retweet_log, pch = 18, col = rgb(1,0,0))
legend("topleft", inset = .05, c('Positive', 'Negative'), fill = c(rgb(0,0,1), rgb(1,0,0)), horiz = TRUE)
abline(posreg$coefficients[1], posreg$coefficients[2], col = rgb(0,0,1), lwd = 2)
abline(negreg$coefficients[1], negreg$coefficients[2], col = rgb(1,0,0), lwd = 2)

#Bootstrapping for positive model
M = 1000
bootSlopes_pos = numeric(M)
bootIntercepts_pos = numeric(M)
for(i in 1:M)
{
  BootSample = sample(nrow(df_positive), replace = T)
  bootmodel = lm(df_positive$retweet_log[BootSample] ~ df_positive$follower_log[BootSample])
  bootSlopes_pos[i] = bootmodel$coefficients[2]
  bootIntercepts_pos[i] = bootmodel$coefficients[1]
}

#Repeat for negative models
bootSlopes_neg = numeric(M)
bootIntercepts_neg = numeric(M)
for(i in 1:M)
{
  BootSample = sample(nrow(df_positive), replace = T)
  bootmodel = lm(df_negative$retweet_log[BootSample] ~ df_negative$follower_log[BootSample])
  bootSlopes_neg[i] = bootmodel$coefficients[2]
  bootIntercepts_neg[i] = bootmodel$coefficients[1]
}

#Create common histogram to see distributions
hgpos = hist(bootSlopes_pos, breaks = 12)
hgneg = hist(bootSlopes_neg, breaks = 12)
range_x = range(c(hgpos$breaks - 0.1, hgneg$breaks + 0.1))
range_y = c(0, max(c(hgpos$count, hgneg$count)) + 250)
plot(hgpos, col = rgb(0,0, 1, 0.2), xlim = range_x, ylim = range_y, xlab = 'Slope coefficient', main = 'Histogram of slope coefficients')
plot(hgneg, add = TRUE, col = rgb(1,0,0, 0.2))
abline(v = posreg$coefficients[2], col = rgb(0,0,1), lwd = 3)
abline(v = negreg$coefficients[2], col = rgb(1,0,0), lwd = 3)

#Plot all the lines from Bootstrapping
plot(x = df_positive$follower_log, y = df_positive$retweet_log, pch = 20, col = rgb(0,0,1), xlab = "Log of Followers", ylab = "Log of Retweets")
legend("topleft", inset = .05, c('Positive', 'Negative'), fill = c(rgb(0,0,1), rgb(1,0,0)), horiz = TRUE)
points(x = df_negative$follower_log, y = df_negative$retweet_log, pch = 18, col = rgb(1,0,0))
for(i in 1:1000)
{
  abline(bootIntercepts_pos[i], bootSlopes_pos[i], col = rgb(0,0,1,0.01))
  abline(bootIntercepts_neg[i], bootSlopes_neg[i], col = rgb(1,0,0,0.01))
}
abline(posreg$coefficients[1], posreg$coefficients[2], col = rgb(0,0,1), lwd = 3)
abline(negreg$coefficients[1], negreg$coefficients[2], col = rgb(1,0,0), lwd = 3)

#### Section: Further Analysis ####

#Filter out all useful (i.e positive or negative) responses
df_good = df %>% filter(df$threshold == 1 | df$threshold == -1)

#Select users with most useful responses
best_users = df_good %>% group_by(screen_name) %>% summarise(count = n()) %>% arrange(desc(count))
#Select most popular users
popular_users = distinct(df_good %>% arrange(desc(followers_count)) %>% select(screen_name))

#User Analysis (take users with most useful tweets) or enter 1 you are interested in
username = best_users$screen_name[1]
username = popular_users$screen_name[1]
#Filter out user and relevant columns
usertweets = df_good %>% filter(screen_name == username) %>% select(retweet_count, followers_count, screen_name, threshold)
#Create positive frame for user
usertweets_pos = usertweets %>% filter(threshold == 1)
#Take log of retweet count and its mean
usertweets_pos$retweet_count = log(usertweets_pos$retweet_count)
usertweets_pos$retweet_count[usertweets_pos$retweet_count == -Inf | usertweets_pos$retweet_count == -1] = 0
mean_pos = mean(usertweets_pos$retweet_count)
#Set plot height
#Repeat for negative frame of user
usertweets_pos$const = 0.75
usertweets_neg = usertweets %>% filter(threshold == -1)
usertweets_neg$const = 0.25
usertweets_neg$retweet_count = log(usertweets_neg$retweet_count)
usertweets_neg$retweet_count[usertweets_neg$retweet_count == -Inf | usertweets_neg == -1] = 0
mean_neg = mean(usertweets_neg$retweet_count)

#Plot results
plot(usertweets_pos$retweet_count, usertweets_pos$const, xlab = 'Log of Retweets', ylab = '', pch = 20, ylim = c(0,1), xlim = c(0,10), col = rgb(0,0,1), yaxt = "n", main = username)
points(usertweets_neg$retweet_count, usertweets_neg$const, pch = 18, col = rgb(1,0,0))
legend("topleft", inset = .05, c('Positive', 'Negative'), fill = c(rgb(0,0,1), rgb(1,0,0)), horiz = TRUE)
abline(v = mean_pos, col = rgb(0,0,1), ylim = c(0.25, 0.75))
abline(v = mean_neg, col = rgb(1,0,0), ylim = c(0.25,0.75))

#Additional Plots

#Set all retweets to at least 1
df_good = df %>% filter(df$threshold == 1 | df$threshold == -1)
df_good$retweet_count[df_good$retweet_count == 0] = 1
df_good$retweet_count_log = log(df_good$retweet_count)
df_pos = df_good %>% filter(threshold == 1)
df_neg = df_good %>% filter(threshold == -1)

#Create a violin plot
vioplot(df_pos$retweet_count_log, df_neg$retweet_count_log, names = c("Positive", "Negative"), col = "gold")
title("Violin Plot of Log of Retweet Count")

#Create a boxplot
boxplot(retweet_count_log~threshold, data = df_good, main = "Retweet Count Box Plot", xlab = "Sentiment", ylab = "Log of Retweet Count")
#boxplot(retweet_count~threshold, data = df_good, main = "Retweet Count Box Plot", xlab = "Sentiment", ylab = "Retweet Count")

#### Section: Permutation Test ####

#Set number of permutations
M = 1000
slope_diff = numeric(length = M)
#Reread data and prepare it 
df = read.csv('timeline_db_classified.csv')

#Keeping only pos. and neg. rows
df = df %>% filter(threshold == 1 | threshold == -1)

#Calculate log of retweets and followers
df$followers_count = log(df$followers_count)
df$retweet_count = log(df$retweet_count)

for (i in 1:M)
{
  #Shuffle follower count column
  df$followers_count = sample(df$followers_count, replace = FALSE)
  
  #Select positive and negative tweets in separate dataframe
  df_positive = df %>% filter(df$threshold == 1)
  df_negative = df %>% filter(df$threshold == -1)
  
  #Select only retweet count and follower count
  df_positive = df_positive %>% select(retweet_count, followers_count)
  df_negative = df_negative %>% select(retweet_count, followers_count)
  
  #Replace 0 retweets with 0 in log-scale
  df_positive$retweet_count[df_positive$retweet_count  == -Inf] = 0
  df_negative$retweet_count[df_negative$retweet_count == -Inf] = 0
  
  #Fit regression model
  posreg = lm(df_positive$retweet_count ~ df_positive$followers_count)
  negreg = lm(df_negative$retweet_count ~ df_negative$followers_count)
  
  slope_diff[i] = negreg$coefficients[2] - posreg$coefficients[2]
}
hist(slope_diff, xlim = range(c(slope_diff, slope_diff_observed)), main = "Histogram of Slope Difference", xlab = "Slope Difference")
abline(v = slope_diff_observed, col = 'red')




