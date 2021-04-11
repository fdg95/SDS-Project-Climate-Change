# SDS-Project-Climate-Change

This github repository accompanies the social data science project of Lukas Dahlhaus, Martina Buck, Meijun Chen and Fabian Geissmann. The aim of this project was to determine, if there is a different impact (measured in retweets) from positive and negative tweets related to climate change. 
Initially, we required a dataset of tweets related to climate change. To gather this data, we created a list of users, tweeting almost exlusively about this topic. The original list, the more detailed list with infos extracted via the Twitter API and a shortened version thereof can be found in the following csv files:

- **TwitterAccs.csv**		List of the selected Twitter accounts
- **user_db.csv**  Info about our selected users extracted from Twitter
- **user_filtered.csv** Only relevant columns of the considered users

Using this user list, we extracted the latest 200 tweets of all users (not considering retweets) and saved them. Tweets no older than a week were filtered out afterwards, since they did not have sufficient time to get retweeted. In the following, we only worked with the filtered dataset.

- **timeline.csv** All tweets extracted without any filtering from our list of accounts on the date specified in the report
- **timeline_db.csv** All tweets extracted which are older than a week to ignore tweets that did not have sufficient time to be retweeted

To evaluate the cleanliness of our retrieved data, a random subsample of 200 tweets was handlabeld by 2 people. Furthermore, the sentiment of these tweets was also labeled by 2 humans each, to get an intuition about how much it agrees with VADER and to then choose the most suitbale thresholds to discretize the continous valence score of VADER into negative, neutral and positive tweets. Despite syuzhet labels also being included in the csv file, they turned out to perform equal or worse than VADER and were not considered for the rest of the work.

- **timeline_filtered.csv** 200 randomly shuffled Tweets for labeling (before adding labels)
- **timeline_filtered_classified.csv** Previously selected Tweets with hand labels and vader/syuzhet labels (after adding labels)

Afterwards, sentiment analysis using VADER was performed on the entire dataset and the tweets were grouped into the 3 categories using the previoulsy selected threshold. The dataset with classification can be found in the following csv file along with the actual output from VADER, since it takes some time to run and was thus saved as well.

- **timeline_db_classified.csv** All tweets older than a week with vader classification
- **vader_classification_timeline_db.csv** The actual output from the vader classification when analyzing timeline_db.csv

The final report describes the project, the data retrieval and processing as well as our findings more in detail. The accompanying R-Markdown shows all the essential code to recreate the findings from the report, while the more extensive R-code file contains some additional stuff that was not used in the final report. 

- **SDS_FullCode.R**        Extensive R-Code
- **Markdown.Rmd**  Markdown file containing all code to recreate the findings described in the report (less detailed than R-Code)
- **Project Description.pdf** Project Description
- **ADD REPORT!!!**

Over the course of this project, some additional files were created that are not explicitly used in the report. However, these files still proofed useful, as they aided in finding certain keywords that were used to fitler out non climate related tweets and to confirm, that more extreme VADER scores show in fact a higher agreement with the hand labels than the random subset.

- **timeline_db_positive.csv** The 100 most positive tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_positive_labeled.csv** The 100 most positive tweets with labels from first round
- **timeline_db_negative_labeled2.csv** The 100 most positive tweets with labels from second round (after duplicate and holiday filtering)
- **timeline_db_negative.csv** The 100 most negative tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_negative_labeled.csv** The 100 most negative tweets with lables from first round
- **timeline_db_negative_labeled2.csv** The 100 most negative tweets with labels from second round (after duplicate and holiday filtering)
