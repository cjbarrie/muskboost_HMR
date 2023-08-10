library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)
library(fixest)
library(zoo)
library(tsibble)
library(modelsummary)

## RAW DATA NOT INCLUDED DUE TO TWITTER SHARING RESTRICTIONS
#IDs OF ORIGINAL TWEETS IN public_data/tweetIDs-*.txt

tdf <- read_fst("data/processed/blue-verified-tweetsALL.fst")
tdf$username <- tdf$user_username

#remove accounts in ranked list for all verified analysis
ranks <- read.csv("data/raw/blue-ranked.csv")
ranks <- ranks$username

tdf <- tdf %>%
  filter(!username %in% ranks)

tdf_tweetdays <- tdf %>%
  group_by(username) %>%
  tidyr::complete(date = seq.Date(min(as.Date("2022-05-17")), max(as.Date("2022-11-29")), 
                                  by="day")) %>%
  mutate(obs = 1,
         rts = ifelse(is.na(retweet_count), 1, 1 +retweet_count), #note use of started log
         likes = ifelse(is.na(like_count), 1, 1 +like_count)) %>% #note use of started log
  group_by(date, username) %>%
  summarise(sum_tweets = sum(obs), 
            sum_rts = sum(rts),
            sum_likes = sum(likes)) %>%
  arrange(username, date)

tdf_tweetdays <- tdf_tweetdays %>%
  mutate(yearmon = as.Date(as.yearmon(date)),
         yearwk = as.Date(yearweek(date))) %>%
  filter(date <= "2022-11-27")

tdf_tweetdays$musk <- ifelse(tdf_tweetdays$date>="2022-10-27" & tdf_tweetdays$date <"2022-11-09", 1, 0)
tdf_tweetdays$twitblue <- ifelse(tdf_tweetdays$date>="2022-11-09", 1, 0)

saveRDS(tdf_tweetdays, "data/processed/tdf_tweetdays-verified.rds")



tdf_tweetdays_public <- tdf_tweetdays

tdf_tweetdays_public$userid <- sapply(tdf_tweetdays_public$username, 
                                      digest::digest, 
                                      algo = "md5")

tdf_tweetdays_public <- tdf_tweetdays_public %>%
  select(-username)

saveRDS(tdf_tweetdays_public, "public_data/tdf_tweetdays-verified.rds")

options(scipen = 999)
tdf1 <- read_fst("data/processed/blue-ranked-tweetsALL.fst")
tdf2 <- read_fst("data/processed/blue-verified-tweetsALL.fst")

tdf1 %>%
  ggplot() +
  geom_histogram(aes(user_followers_count)) + 
  xlab("Follower count") + ylab("Count")

tdf2 %>%
  ggplot() +
  geom_histogram(aes(user_followers_count))  + 
  xlab("Follower count") + ylab("Count")


tdf1$status <- "Contentious"
tdf2$status <- "General"
tdf <- rbind(tdf1, tdf2)

ggplot(tdf, aes(x=log(user_followers_count))) +
  geom_density(aes(color=status, fill=status), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Follower count (logged)", y = "Density") +
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
  guides(fill=guide_legend(title="Status"),
         color=guide_legend(title="Status")) +
  theme_classic()

tdf_tweetdays1 <- readRDS("data/processed/tdf_tweetdays-ranked.rds")
tdf_tweetdays2 <- readRDS("data/processed/tdf_tweetdays-verified.rds")

tdf_tweetdays1$status <- "Contentious"
tdf_tweetdays2$status <- "General"

tdf_tweetdays <- rbind(tdf_tweetdays1, tdf_tweetdays2)



tdf_tweetdays$muskblue <- ifelse(tdf_tweetdays$date>="2022-10-28" & tdf_tweetdays$date <"2022-10-31", 1,
                                 ifelse(tdf_tweetdays$date>="2022-10-31" & tdf_tweetdays$date <"2022-11-03", 2,
                                        ifelse(tdf_tweetdays$date>="2022-11-03" & tdf_tweetdays$date <"2022-11-06", 3,
                                               ifelse(tdf_tweetdays$date>="2022-11-06" & tdf_tweetdays$date <"2022-11-09", 4,
                                                      ifelse(tdf_tweetdays$date>="2022-11-09" & tdf_tweetdays$date <"2022-11-12", 5,
                                                             ifelse(tdf_tweetdays$date>="2022-11-12" & tdf_tweetdays$date <"2022-11-15", 6,
                                                                    ifelse(tdf_tweetdays$date>="2022-11-15" & tdf_tweetdays$date <"2022-11-18", 7,
                                                                           ifelse(tdf_tweetdays$date>="2022-11-18" & tdf_tweetdays$date <"2022-11-21", 8,
                                                                                  ifelse(tdf_tweetdays$date>="2022-11-21" & tdf_tweetdays$date <"2022-11-24", 9,
                                                                                         ifelse(tdf_tweetdays$date>="2022-11-24" & tdf_tweetdays$date <"2022-11-27", 10, 0))))))))))
tdf_tweetdays$muskblue <- as.factor(tdf_tweetdays$muskblue)

tdf_tweetdays$pmusk <- ifelse(tdf_tweetdays$date=="2022-10-27", 1, 0)
tdf_tweetdays$muskacq <- ifelse(tdf_tweetdays$date>="2022-10-27", 1, 0)
tdf_tweetdays$muskacqfac <- as.factor(tdf_tweetdays$muskacq)

p1 <- tdf_tweetdays %>%
  filter(status == "Contentious") %>%
ggplot() +
  geom_density(aes(x=log(sum_rts), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Retweet count (logged)", y = "Density") + ggtitle("Contentious") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()


p2 <- tdf_tweetdays %>%
  filter(status == "General") %>%
  ggplot() +
  geom_density(aes(x=log(sum_rts), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Retweet count (logged)", y = "Density") + ggtitle("General") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()

p1 <- tdf_tweetdays %>%
  filter(status == "Contentious") %>%
  ggplot() +
  geom_density(aes(x=log(sum_likes), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Retweet count (logged)", y = "Density") + ggtitle("Contentious") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()


p2 <- tdf_tweetdays %>%
  filter(status == "General") %>%
  ggplot() +
  geom_density(aes(x=log(sum_likes), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Retweet count (logged)", y = "Density") + ggtitle("General") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()

p1 <- tdf_tweetdays %>%
  filter(status == "Contentious") %>%
  ggplot() +
  geom_density(aes(x=log(sum_tweets), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Tweet count (logged)", y = "Density") + ggtitle("Contentious") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()


p2 <- tdf_tweetdays %>%
  filter(status == "General") %>%
  ggplot() +
  geom_density(aes(x=log(sum_tweets), color=muskacqfac, fill=muskacqfac), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
  labs(x = "Tweet count (logged)", y = "Density") + ggtitle("General") +
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
  guides(fill=guide_legend(title="Post-Musk"),
         color=guide_legend(title="Post-Musk")) +
  theme_classic()