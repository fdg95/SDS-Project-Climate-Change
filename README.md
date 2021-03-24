# SDS-Project-Climate-Change
Repository for the social data science project.

# Files Overview
- **TwitterAccs.csv**			Overview of the selected Twitter Accounts
- **Playaround.R**        R-code
- **Project Description.pdf** Project Description
- **timeline.csv** All tweets extracted without any filtering from our list of accounts
- **timeline_db.csv** All tweets extracted which are older than a week
- **user_db.csv** All user info extracted from Twitter
- **user_filtered.csv** Only relevant columns of the considered users
- **timeline_filtered.csv** 200 Randomly shuffled Tweets for labeling
- **timeline_filtered_classified.csv** Tweets with hand labels and vader/syuzhet labels
- **timeline_db_classified.csv** All tweets older than a week with vader classification
- **timeline_db_positive.csv** The 100 most positive tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_positive_labeled.csv** The 100 most positive tweets with labels from first round
- **timeline_db_negative.csv** The 100 most negative tweets from the classified tweets of timeline_db_classified.csv
- **timeline_db_negative_labeled.csv** The 100 most negative tweets with lables from first round
- **vader_classification_timeline_db.csv** The actual output from the vader classification when analyzing timeline_db.csv
