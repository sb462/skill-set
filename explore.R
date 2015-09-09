# these scripts are to be run on Kaggle browser.

#load the libraries

library(RSQLite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

# to load the data
db <- src_sqlite('../input/database.sqlite', create = F)

db_subset <- db %>%
              tbl('May2015')

# What are the top 10 popular links people are commenting on?

popular_links <- db_subset %>%
                    group_by(link_id) %>%
                      summarise(subreddit,n_posts=n()) %>%
                        arrange(desc(n_posts)) %>% 
                          select(link_id) %>% 
                            collect() %>%
                              data.frame() %>%
                                slice(1:10)

# What is the typical time difference between each posts for the above really popular post?

popular_db <- db_subset %>%
                filter(link_id %in% popular_links$link_id) %>%
                  select(created_utc,link_id,name,subreddit,score,id,author) %>%
                    collect()%>%
                      data.frame() %>%
                        group_by(link_id)%>%
                          mutate(difftime = created_utc - lag(created_utc))%>%
                            summarise(n_posts= n(),
                              median_time_diff = median(difftime, na.rm=TRUE),
                                median_time_deviation = mad(difftime, na.rm=TRUE))%>%
                                  arrange(desc(n_posts))

# typical time difference seems to be 0-2 seconds with deviation of 1.4 s. These links are commented over 30000 times. What if we compare these with posts that are commented moderately about 5000 times.

# Lets take a sample of links that are commented on around 5 K times

list_average_link <- db_subset %>%
                      group_by(link_id) %>%
                        summarise(n_posts = n(), subreddit) %>%
                          filter(n_posts > 10000 , n_posts < 20000 ) %>%
                            arrange((n_posts)) %>%
                              select(link_id) %>%
                                collect() %>%
                                  data.frame() %>%
                                    slice(1:10)

# what is the relative time difference between these posts?

average_db <- db_subset %>%
                filter(link_id %in% list_average_link$link_id) %>%
                  select(created_utc,link_id) %>%
                    collect()%>%
                      data.frame() %>%
                        group_by(link_id)%>%
                          mutate(difftime = created_utc - lag(created_utc))%>%
                            summarise(n_posts= n(),
                                      median_time_diff = median(difftime, na.rm=TRUE),
                                      median_time_deviation = mad(difftime, na.rm=TRUE))%>%
                              arrange(desc(n_posts))

# seems typical time difference between subsequent posts is 1-10 s

# so the time difference  on a broader scale does not necessarily mean the post will run long, # both the very long posts and moderate can have a large number of posts within a second of 
# each other, it is a question of how long the streaks run





  
