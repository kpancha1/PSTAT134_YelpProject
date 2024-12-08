




yelp <- read_csv("cleaned_dataset(1).csv")



yelp |> 
  count(city,sort=T)
  

library(tidyverse)
library(tidytext)
library(data.table)
library(wordcloud)
library(ggplot2)

# Load the Yelp dataset
yelp <- fread("/Users/richardshim/Downloads/cleaned_dataset.csv")



# Top two cities with the highest number of reviews
top_cities <- yelp %>%
  count(city, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(city)


yelp |> 
  count(city,sort=T) |> 
  head(n=5) |> 
  ggplot(aes(x=fct_reorder(city,n) |> fct_rev(),y=n)) + 
  geom_col(aes(fill=city)) + 
  theme(axis.text.x=element_text(angle=45)) + 
  labs(
    title='Top 5 Cities by Number of Review',
    x='City',
    y='Count'
  )





# Filter data for top two cities
yelp_top_cities <- yelp %>% filter(city %in% top_cities)

yelp_top_cities



# Question 2 --------------------------------------------------------------

# Analyzing 6 Food Categories ---------------------------------------------


food_categories <- c("Mexican", "Chinese", "Italian", "Japanese",'French','Korean')

# Filter data for the selected categories
yelp_filtered <- yelp_top_cities %>%
  filter(str_detect(categories, paste(food_categories, collapse = "|")))

yelp_filtered |> 
  mutate(
    category=case_when(str_detect(categories,'Mexican') ~ 'Mexican',
                       str_detect(categories,'Chinese') ~ 'Chinese',
                       str_detect(categories,'Italian') ~ 'Italian',
                       str_detect(categories,'Japanese') ~ 'Japanese',
                       str_detect(categories,'Korean') ~ 'Korean',
                       str_detect(categories,'French') ~ 'French')
  ) -> yelp_filtered
  


yelp_filtered |> 
  ggplot(aes(x=category)) + 
  geom_bar()


# -------------------------------------------------------------------------




# Question 3 --------------------------------------------------------------

# Categories with highest proportion of 5 star reviews

yelp_filtered |> 
  group_by(category) |> 
  count(review_stars) |> 
  mutate(n=n/sum(n)) |> 
  ggplot(aes(x=as.factor(review_stars),y=n)) + 
  geom_col() + 
  facet_wrap(~category,nrow=2) + 
  labs(
    x='Stars',
    title='Proportion of Reviews by Stars By Category'
  )


yelp_filtered %>%
  filter(review_stars==5) |> 
  group_by(city) |> 
  count(category) |> 
  mutate(n=n/sum(n)) |> 
  ggplot(aes(x=category,y=n)) + 
  geom_col() + 
  facet_wrap(~city) + 
  labs(
    x='Category',
    title='Proportion of 5-Star Reviews by Category'
  ) + 
  coord_flip()



# Question 4 --------------------------------------------------------------

# Category with Most One Star Reviews

yelp_filtered |> 
  group_by(category,city) |> 
  count(review_stars) |> 
  mutate(n=n/sum(n)) |> 
  filter(review_stars == 1) |> 
  ggplot(aes(x=category,y=n)) + 
  geom_col() + 
  facet_wrap(~city) + 
  ylim(c(0,0.2)) + 
  labs(
    title='Percent of 1 Star Reviews by Category by City'
  ) + 
  coord_flip()


# Quesitno 5 --------------------------------------------------------------

# Histogrma of star ratings


yelp_filtered |> 
  ggplot(aes(x=review_stars,y=city))  + 
  ggridges::geom_density_ridges(aes(fill=city))


# Review Counts

yelp_filtered |> 
  count(city,category) |> 
  ggplot(aes(x=category,y=n)) + 
  geom_col() + 
  facet_wrap(~city,nrow=1) + 
  coord_flip()
  


# Question 6 --------------------------------------------------------------

# Word Cloud and NLP Analysis
library(tidytext)
library(wordcloud)

yelp_words <- yelp_filtered %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(stop_words)

palette <- brewer.pal(100,'Dark2')
# Word cloud for all reviews
yelp_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  with(wordcloud(word, n, max.words = 100,colors=palette))

# Separate word clouds for 5-star and 1-star reviews
for (rating in c(1, 5)) {
  yelp_words %>%
    filter(review_stars == rating) %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, max.words = 10,colors=palette))
}


yelp_words %>%
  filter(review_stars == 1) %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50,colors=palette))



yelp_words %>%
  filter(review_stars == 5) %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50,colors=palette))




# Question 7 --------------------------------------------------------------



yelp_themes <- yelp_words %>%
  mutate(theme = case_when(
    word %in% c("taste", "delicious", "bland", "flavor") ~ "Taste",
    word %in% c("portion", "size", "small", "large") ~ "Portion Size",
    word %in% c("quality", "fresh", "bad", "stale") ~ "Quality",
    word %in% c("location", "close", "far") ~ "Location",
    word %in% c("ambiance", "quiet", "noisy") ~ "Ambiance",
    word %in% c("staff", "friendly", "rude") ~ "Staff Friendliness",
    word %in% c("clean", "dirty") ~ "Cleanliness",
    word %in% c("restroom", "bathroom") ~ "Restroom Conditions",
    word %in% c("delivery", "quick", "slow") ~ "Food Delivery Speed"
  )) |> 
  drop_na()

# Visualize themes by star ratings
yelp_themes %>%
  count(theme, review_stars) %>%
  ggplot(aes(x = theme, y = n, fill = as.factor(review_stars))) +
  geom_col(position = "dodge") +
  labs(
    title = "Themes in 1-Star and 5-Star Reviews",
    x = "Themes",
    y = "Count"
  ) +
  theme_minimal() + 
  coord_flip()

