library(dplyr)
library(ggplot2)
library(tidylog)
library(fixest)
library(modelsummary)
library(scales)
library(ggthemes)

##change variable "username" to "userid" if using public repo data

tdf_tweetdays <- readRDS("data/processed/tdf_tweetdays-ranked.rds")


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


baseline1 <- feols(log(sum_rts) ~ muskacq + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

baseline2 <- feols(log(sum_likes) ~ muskacq + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  # "r.squared", "R2",            2,
  "nobs",      "Observations",             0,
  "FE: username",  "User fixed effect",      0)

modelsummary(models = list("Retweets" = baseline1,
                           "Likes" = baseline2),
             output = "latex",
             coef_rename = c(
               "muskacq" = "Post-Musk acquisition",
               "log(sum_rts)" = "Retweets (logged sum)",
               "log(sum_tweets)" = "Tweets (logged sum)"),
             gof_map = gm,
             notes = list('Outcomes are logged retweets and likes',
                          'Standard errors clustered by user'),
             title = 'Effect of Musk acquisition on contentious user engagement',
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001))


baseline3 <- feols(log(sum_rts) ~ pmusk + muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

baseline4 <- feols(log(sum_likes) ~ pmusk +  muskblue + log(sum_tweets) |
                     username,
                   data = tdf_tweetdays)

##alternative viz.

est_std1 = summary(baseline3, se = "iid")
test1 <- coefplot(est_std1, 
         dict = c(muskblue1="Oct 28-Oct 31", muskblue2 = "Oct 31-Nov 03", muskblue3="Nov 03-Nov 06", muskblue4 = "Nov 06-Nov 09",
                  muskblue5="Nov 09-Nov 12", muskblue6 = "Nov 12-Nov 15", muskblue7="Nov 15-Nov 18", muskblue8 = "Nov 18-Nov 21", 
                  muskblue9="Nov 21-Nov 24", muskblue10 = "Nov 24-Nov 27"),
         keep = c("Oct 28-Oct 31","Oct 31-Nov 03","Nov 03-Nov 06","Nov 06-Nov 09",
                  "Nov 09-Nov 12","Nov 12-Nov 15","Nov 15-Nov 18","Nov 18-Nov 21",
                  "Nov 21-Nov 24","Nov 24-Nov 27"),
         ylim= c(0,1.5),
         add = F, pch=15, main = "Effect on user post engagement",
         lab.cex = .75, pt.cex = 1.5, cex.axis = 1)


est_std1 = summary(baseline4, se = "iid")
# #exponentiate and get percentage
# est_std1$coefficients <- (exp(est_std1$coefficients) -1)*100
test2 <- coefplot(est_std1, 
         dict = c(muskblue1="Oct 28-Oct 31", muskblue2 = "Oct 31-Nov 03", muskblue3="Nov 03-Nov 06", muskblue4 = "Nov 06-Nov 09",
                  muskblue5="Nov 09-Nov 12", muskblue6 = "Nov 12-Nov 15", muskblue7="Nov 15-Nov 18", muskblue8 = "Nov 18-Nov 21", 
                  muskblue9="Nov 21-Nov 24", muskblue10 = "Nov 24-Nov 27"),
         keep = c("Oct 28-Oct 31","Oct 31-Nov 03","Nov 03-Nov 06","Nov 06-Nov 09",
                  "Nov 09-Nov 12","Nov 12-Nov 15","Nov 15-Nov 18","Nov 18-Nov 21",
                  "Nov 21-Nov 24","Nov 24-Nov 27"),
         ylim= c(0,1.5),
         add = T, col = 2, pch=15,
         lab.cex = .75, pt.cex = 1.5, cex.axis = 1)


values1 <- test1$prms
values2 <- test2$prms
values1$order <- seq(1:10)
values2$order <- seq(1:10)
values1$Outcome <-"Retweets"
values2$Outcome <-"Likes"

values <- rbind(values1, values2)

p1 <- ggplot(values, aes(order)) +
  geom_point(aes(y = estimate, color =Outcome)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha =1, width =.1) +
  scale_y_continuous(limits = c(0,1.25),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(breaks=values$order, label=values$estimate_names) +
  theme_tufte(base_family = "Helvetica") +
  ylab("Estimate and 95% Conf. Inf.") + xlab("Date") + ggtitle("Contentious") +
  scale_color_manual(values=c("#00AEF3", "#DE0100")) +
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
  ) + geom_vline(aes(xintercept = 5),
                   linetype="longdash", colour = "black", size=.5)