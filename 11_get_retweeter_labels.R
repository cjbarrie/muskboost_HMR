library(academictwitteR)
library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)
source("utils.R")
set.seed(123L)

## RAW DATA NOT INCLUDED DUE TO TWITTER SHARING RESTRICTIONS
#IDs OF ORIGINAL TWEETS IN public_data/tweetIDs-*.txt
tdf <- read_fst("data/processed/blue-ranked-tweetsALL.fst")

hist(tdf$retweet_count)

tdf_rts <- tdf %>%
  filter(retweet_count>=100 & retweet_count <=1000)

tdf_rts_sample<- tdf_rts %>%
  sample_n(1000)

tweet_ids <- tdf_rts_sample$tweet_id
data_path = "data/retweeted_by/"
for(i in seq_along(tweet_ids)) {
  
  rts <- get_retweeted_by(tweet_ids[i])
  saveRDS(rts, paste0(data_path,"rts_",rts$from_id[nrow(rts)],".rds"))
  
}

files <- paste0("data/retweeted_by/",list.files("data/retweeted_by/"))

followers_sample <- data.frame()
for (i in seq_along(files)) {
  
  flwrs <- readRDS(files[[i]])
  flwrs <- flwrs %>%
    select(username, from_id)
  
  followers_sample <- rbind(flwrs, followers_sample)
}


usernames <- followers_sample %>%
  sample_n(10000) %>%
  pull(username)

df.all <- data.frame()
for(i in seq_along(usernames)) {
  
  cat("Getting scores for ", usernames[[i]], "\n")
  
  error <- tryCatch(df <- get_partisanship(usernames[[i]]),
                    error=function(e) e)
  if (inherits(error, 'error')) {
    df <- data.frame(username = NA,
                     ideo_score = NA)
    next
  }
  
  df.all <- dplyr::bind_rows(df.all, df)
}

saveRDS(df.all, "data/processed/ideo-scores-retweeters.rds")

df.all <- readRDS("data/processed/ideo-scores-retweeters.rds")
ggplot(df.all, aes(x=ideo_score)) +
  geom_density() +
  labs(x = "Ideology score", y = "Density") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
        strip.text = element_text(size = 20)) +
  theme_classic()



set.seed(123L)
df.all.sample <- df.all %>%
  distinct(username, .keep_all = T) %>%
  sample_n(100)

usernames <- unique(df.all.sample$username)

tcs_all <- data.frame()

for (i in seq_along(usernames)) {
  
  ctquery = usernames[[i]]
  
  tcs <- count_all_tweets(
    query = ctquery,
    start_tweets = "2022-05-17T00:00:00Z",
    end_tweets = "2022-11-29T00:00:00Z",
    granularity = "day",
    # country = "GB",
    n = Inf
  )
  
  tcs <- tcs %>%
    mutate(date = as.Date(start),
           query = ctquery) %>%
    select(query, date, tweet_count)
  
  tcs_all <- rbind(tcs_all, tcs)
}

saveRDS(tcs_all, "data/processed/tweet-counts.rds")
tcs_all <- readRDS("data/processed/tweet-counts.rds")

tcs_all %>%
  group_by(date) %>%
  mutate(medntc = median(tweet_count)) %>%
  ggplot() +
  geom_line(aes(date, medntc)) +
  labs( x = "Date", y = "Median tweet count") +
  theme_classic(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8, angle = 325),
    axis.text.y = element_text(size =8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.01, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.01, linetype = "solid")
  ) + geom_vline(aes(xintercept = as.Date("2022-10-27")),
                 linetype="longdash", colour = "black", size=.5)

tcs_all %>%
  group_by(date) %>%
  mutate(medntc = median(tweet_count)) %>%
  ggplot() +
  geom_line(aes(date, medntc)) +
  labs( x = "Date", y = "Median tweet count") +
  theme_classic(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8, angle = 325),
    axis.text.y = element_text(size =8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.01, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.01, linetype = "solid")
  ) + geom_vline(aes(xintercept = as.Date("2022-10-27")),
                 linetype="longdash", colour = "black", size=.5)


tcs_all %>%
  group_by(date) %>%
  filter(tweet_count<=10) %>%
  mutate(medntc = mean(tweet_count)) %>%
  ggplot() +
  geom_line(aes(date, medntc)) +
  labs( x = "Date", y = "Mean tweet count") +
  theme_classic(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8, angle = 325),
    axis.text.y = element_text(size =8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.01, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.01, linetype = "solid")
  ) + geom_vline(aes(xintercept = as.Date("2022-10-27")),
                 linetype="longdash", colour = "black", size=.5)
