install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("textdata")
install.packages("sentimentr")

library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidytext)
library(textdata)
library(sentimentr)

# import the data into a dataframe
covid_tweets_df <- read.csv("covidtweets.csv")
custom_covid_df <- data.frame(
                              id = covid_tweets_df$id_str, 
                              created_at = covid_tweets_df$created_at, 
                              lang = covid_tweets_df$lang,
                              text = covid_tweets_df$text, 
                              screen_name = covid_tweets_df$screen_name, 
                              name = covid_tweets_df$name, 
                              favourites = covid_tweets_df$favourites_count, 
                              statuses = covid_tweets_df$statuses_count, 
                              followers = covid_tweets_df$followers_count, 
                              friends = covid_tweets_df$friends_count,
                              user_created_at = covid_tweets_df$user_created_at
                              )
# Reformat the created_at and user_created_at column as a data and time object
# and assign to a new data frame

clean_covid_tweets_df <- custom_covid_df %>%
  mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")) %>% 
  mutate(user_created_at = as.POSIXct(user_created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

# Add column for user_created_at details (year, month, day etc.)

clean_covid_tweets_df <- clean_covid_tweets_df %>%
  mutate(user_year = year(user_created_at),
         user_month = month(user_created_at),
         user_day = day(user_created_at),
         user_hour = hour(user_created_at),
         user_minute = minute(user_created_at),
         user_second = second(user_created_at),
         user_wday = wday(user_created_at))


# Remove URLs from the "text" column in clean_covid_tweets_df

clean_covid_tweets_df <- clean_covid_tweets_df %>% mutate(text = str_remove(text, "http://*|https://*")) %>% 
  mutate(text = str_remove(text, "RT "))

# Create a new data frame called covid_words_df that will contain a list of all
# words that occur in the clean_covid_tweets_df data frame, Filter out profanity,
# t.co, etc, 

profanity_list <- unique(tolower(lexicon::profanity_alvarez))

covid_words_df <- clean_covid_tweets_df %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co", profanity_list))

# Visualize the top 20 words in covid_words_df
covid_words_df %>% 
  count(word, sort = T) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count", y = "Words",
       title = "Top words tweeted in COVID dataset")

# visualize top 20 words from covid_tweets_df
# Export your visualization as a png ( min width = 2048 pixels)

#Number of tweets by hour user was created
ggplot(clean_covid_tweets_df, aes(x = user_hour))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Hour")+
  ylab("Number of Tweets")


ggplot(clean_covid_tweets_df, aes(x = user_hour))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Hour")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "navy", low= "#75aadb")


ggplot(clean_covid_tweets_df, aes(x = user_hour))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Hour")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "mediumorchid4", low= "chartreuse")

#Number of tweets by minute
ggplot(clean_covid_tweets_df, aes(x = user_minute))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Minute")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "firebrick4", low= "firebrick1")

#Number of tweets by year
ggplot(clean_covid_tweets_df, aes(x = user_year))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Year")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "paleturquoise1", low= "turquoise4")

#Number of tweets by month
ggplot(clean_covid_tweets_df, aes(x = user_month))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Month")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "maroon1", low= "maroon4")


#Number of tweets by year and month
ggplot(clean_covid_tweets_df)+
  geom_jitter(aes(x = user_year, y = user_month))

#Number of tweets by hour and minute
ggplot(clean_covid_tweets_df)+
  geom_jitter(aes(x = user_hour, y = user_minute))

#Number of tweets by week
ggplot(clean_covid_tweets_df, aes(x = user_wday))+
  geom_bar(aes(fill = ..count..))+
  theme(legend.position = "none")+
  xlab("Day of the week")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "gold", low= "gold4")

#Number of tweets by day of the week and hour
ggplot(clean_covid_tweets_df)+
  geom_jitter(aes(x = user_wday, y = user_hour))










