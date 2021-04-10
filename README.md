# SDS-Project-Climate-Change

This github repository accompanies the social data science project of Lukas Dahlhaus, Martina Buck, Meijun Chen and Fabian Geissmann. The aim of this project was to determine, if there is a different impact (measured in retweets) from positive and negative tweets related to climate change. The final report describes the project, the data retrieval and processing as well as our findings more in detail. The accompanying R-Markdown shows all the essential code to recreate the findings from the report, while the more extensive R-code file contains some additional stuff that was not used in the final report. Lastly, the various csv files contain our initial user list, the extracted tweets, the annotated tweets and the tweets after sentiment analysis. 

# Files Overview
- **TwitterAccs.csv**			List of the selected Twitter accounts
- **user_db.csv**  Info about our selected users extracted from Twitter
- **user_filtered.csv** Only relevant columns of the considered users


- **Playaround.R**        Extensive R-Code
- **Project Description.pdf** Project Description



- **timeline.csv** All tweets extracted without any filtering from our list of accounts on the date specified in the report
- **timeline_db.csv** All tweets extracted which are older than a week to ignore tweets that did not have sufficient time to be retweeted

- **timeline_filtered.csv** 200 randomly shuffled Tweets for labeling (before adding labels)
- **timeline_filtered_classified.csv** Previously selected Tweets with hand labels and vader/syuzhet labels (after adding labels)
- 
- **timeline_db_classified.csv** All tweets older than a week with vader classification
- **vader_classification_timeline_db.csv** The actual output from the vader classification when analyzing timeline_db.csv

- **timeline_db_positive.csv** The 100 most positive tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_positive_labeled.csv** The 100 most positive tweets with labels from first round
- **timeline_db_negative_labeled2.csv** The 100 most positive tweets with labels from second round (after duplicate and holiday filtering)
- **timeline_db_negative.csv** The 100 most negative tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_negative_labeled.csv** The 100 most negative tweets with lables from first round
- **timeline_db_negative_labeled2.csv** The 100 most negative tweets with labels from second round (after duplicate and holiday filtering)
- 
