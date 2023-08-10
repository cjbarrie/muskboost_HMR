# muskboost_HMR
Repository of materials for article "Did the Musk takeover boost contentious actors on Twitter?

## Scripts

-   01_get_accounts.R: process raw account information
-   02_get_tweets.R: get tweets by contentious and general accounts
-   03_bind_tweets.R: bind tweets into data frame
-   04_update_get_tweets.R: update tweet collection
-   05_update_bind_tweets.R: update tweet binding
-   06_get_panel_ranked.R: create panel for contentious users
-   07_get_panel_verified.R: create panel for general users
-   08_get_time_trends_ranked.R: estimate Musk acquisition effects and
    time dummies for contentious users
-   09_get_time_trends_verified.R: estimate Musk acquisition effects and
    time dummies for general users
-   10_get_ideo.R: get ideology scores for accounts following seed accounts
-   11_get_retweeter_labels.R: get ideology scores for accounts retweeting contentious accounts

## Data

Twitter ToS do not permit sharing of raw Twitter data. I am able to make
the following data public:

-   tdf_tweetdays-ranked.rds: panel dataset of retweets and likes for
    anonymized contentious users
-   tdf_tweetdays-verified.rds: panel dataset of retweets and likes for
    anonymized general users
-   tweetIDs-ranked.txt: tweet IDs of collected tweets for contentious
    users. Download
    [here](https://www.dropbox.com/s/kntsws2z6jqkkqk/tweetIDs-ranked.txt?dl=1)
-   tweetIDs-verified.txt: tweet IDs of collected tweets for general
    users. Download
    [here](https://www.dropbox.com/s/h447ulgsxs383v3/tweetIDs-verified.txt?dl=1)