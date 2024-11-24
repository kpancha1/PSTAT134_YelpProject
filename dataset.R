rm(list=ls())

library(jsonlite)
library(dplyr)
library(ndjson)
library(jsonlite)
library(data.table)
library(magrittr)
setwd('/Users/yujisu/Desktop/Final project')

gc()
# Extract relevant fields from each dataset
tip_data <- fread("./dataset/tip_data.csv")
business_data <- fread("./dataset/business_data.csv")
review_data <- fread("./dataset/review_data.csv")
user_data <-  fread("./dataset/user_data.csv")
checkin_data <- fread("./dataset/checkin_data.csv")

list_user1 <- user_data %>% distinct(user_id) %>%
  mutate(user_id2 = paste0('i', sprintf('%07d', c(1:1987897)))); head(list_user1)

list_biz1 <- business_data %>% distinct(business_id) %>%
  mutate(business_id2 = paste0('j', sprintf('%07d', c(1:150243)))); head(list_biz1)

temp1 <- review_data %>%
  left_join(list_user1) %>% dplyr::select(-c(user_id)) %>%
  left_join(list_biz1) %>% dplyr::select(-c(business_id)) %>%
  rename_with(~ paste0('review_', .x), c(stars)) %>% 
  rename_with(~ paste0('vote_', .x), c(useful, funny, cool)) %>% 
  dplyr::select(c(user_id2, business_id2, everything())); head(temp1)

temp2 <- business_data %>%
  left_join(list_biz1) %>% dplyr::select(-c(business_id)) %>%
  rename_with(~ paste0('business_', .x), c(stars, review_count, name)) %>%  
  dplyr::select(c(business_id2, everything()))

temp3 <- user_data %>%
  left_join(list_user1) %>% dplyr::select(-c(user_id, friends)) %>%
  rename_with(~ paste0('user_', .x), c(useful, funny, cool, name, review_count)) %>% 
  dplyr::select(c(user_id2, everything()))

temp4 <- tip_data %>%
  left_join(list_user1) %>% dplyr::select(-c(user_id)) %>%
  left_join(list_biz1) %>% dplyr::select(-c(business_id)) %>%
  dplyr::select(c(user_id2, business_id2, everything())); head(temp4)

test1 <- temp4 %>%
  group_by(user_id2, business_id2) %>%
  summarise(
    text = paste(text, collapse = " "),   
    date = first(date),                  
    compliment_count = sum(compliment_count)) %>%
  ungroup()

df1 <- temp1 %>%
  filter(business_id2 %in% c(temp2 %>% distinct(business_id2) %>% pull(business_id2))) %>%
  filter(user_id2 %in% c(temp3 %>% distinct(user_id2) %>% pull(user_id2))) %>%
  left_join(temp2) %>%
  left_join(temp3) %>% 
  left_join(test1 %>% dplyr::select(-c(text, date)) %>% rename_with(~ paste0('tip_', .x), c(compliment_count)))


fwrite(test1, "tip_dataset.csv", row.names = FALSE)
fwrite(df1, "unified_dataset.csv", row.names = FALSE)





data <- fread("unified_dataset.csv")
tip <- fread("tip_dataset.csv")


tip_df <- tip_data %>%
  select(compliment_count)

user_df <- df1 %>%
  select(review_count, yelping_since, user_useful, user_funny, user_cool, user_fans, average_stars,
         compliment_hot, compliment_more, compliment_profile, compliment_cute,
         compliment_list, compliment_note, compliment_plain, compliment_cool,
         compliment_funny, compliment_writer, compliment_photos) %>%
  mutate(total_compliments = compliment_hot + compliment_more + compliment_profile +
           compliment_cute + compliment_list + compliment_note + compliment_plain +
           compliment_cool + compliment_funny + compliment_writer + compliment_photos)

review_df <- review_data %>%
  select(stars, text)

#business_df <- business_data %>%
#  select(city, stars, review_count, is_open, categories, hours)

# Combine the data into a unified dataset
unified_dataset <- data.frame(
  tip_compliment_count = tip_df$compliment_count,
  user_review_count = user_df$review_count,
  yelping_since = user_df$yelping_since,
  useful_votes = user_df$useful,
  funny_votes = user_df$funny,
  cool_votes = user_df$cool,
  fans = user_df$fans,
  average_rating = user_df$average_stars,
  total_compliments = user_df$total_compliments,
  review_stars = review_df$stars,
  review_text = review_df$text,
  business_city = business_df$city,
  business_stars = business_df$stars,
  business_review_count = business_df$review_count,
  is_business_open = business_df$is_open) 


# Save the unified dataset to a CSV file
write.csv(unified_dataset, "dataset.csv", row.names = FALSE)

# View the first few rows of the dataset
head(unified_dataset)
