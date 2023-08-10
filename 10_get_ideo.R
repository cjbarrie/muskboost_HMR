library(dplyr)
library(ggplot2)
library(fst)
library(tidylog)
source("utils.R")

## RAW DATA NOT INCLUDED DUE TO TWITTER SHARING RESTRICTIONS
#IDs OF ORIGINAL TWEETS IN public_data/tweetIDs-*.txt

#get ideology scores of users following seed accounts
tweets <- read_fst("data/processed/blue-ranked-tweetsALL.fst")
user_followers <- tweets %>%
  sample_n(100)

usernames <- unique(user_followers$user_username)

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

ggplot(df.all, aes(x=ideo_score)) +
  geom_density()

saveRDS(df.all, "data/processed/ideo-scores-ranked.rds")

tweets <- read_fst("data/processed/blue-verified-tweetsALL.fst")
user_followers <- tweets %>%
  sample_n(100)

usernames <- unique(user_followers$user_username)

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

ggplot(df.all, aes(x=ideo_score)) +
  geom_density()

saveRDS(df.all, "data/processed/ideo-scores-verified.rds")


fscores_v <- readRDS("data/processed/ideo-scores-verified.rds")
fscores_r <- readRDS("data/processed/ideo-scores-ranked.rds")
fscores_v$status <- "General"
fscores_r$status <- "Contentious"

fscores <- rbind(fscores_v, fscores_r)

ggplot(fscores, aes(x=ideo_score)) +
  geom_density(aes(color=status, fill=status), alpha = .4) +
  scale_fill_manual(values=c("#00AEF3", "#DE0100")) +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) + 
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
  guides(fill=guide_legend(title="Status"),
         color=guide_legend(title="Status")) +
  theme_classic()

