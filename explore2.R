# Lets look at some generic distribution of subreddit topics and users, we want to check if 
# there are typical topics users are interested in 

#which subreddits contribute to most posts

all_posts <- db_subset %>% 
                select(id) %>% 
                    collect() %>%
                        data.frame() %>%
                            nrow()
# number of total posts in May are 
#print(all_posts)

# Which subreddits contributed to most number of comments?
db_subreddit <- db_subset %>%
                  group_by(subreddit) %>%
                    summarise(percent_posts = n()/all_posts*100)%>%
                      arrange(desc(percent_posts))

# lets look at the distribution of relative contribution of posts
w_posts_vec <- collect(db_subreddit) %>% .$percent_posts 
q_posts <- w_posts_vec %>% quantile()

relative_w_group <- tapply(w_posts_vec, cut(w_posts_vec, breaks=q_posts), FUN=sum)
names(relative_w_group) <- c("Q1","Q2","Q3","Q4")
print(relative_w_group["Q4"])

# Q4 has more than 99% of the posts - top 25 % of subreddits contribute to more than 99% of
# comments. 

# what about users? If we ask the same question?
# 
db_user <- db_subset %>%
            filter(author != "[deleted]") %>%
              group_by(author) %>%
                summarise(percent_posts = n()/all_posts*100)%>%
                  arrange(desc(percent_posts))

w_user_vec <- collect(db_user) %>% .$percent_posts 
q_users <- w_user_vec %>% quantile()

print(q_users)

relative_w_3quser_group <- tapply(w_user_vec, cut(w_user_vec, breaks=q_users[3:5]), FUN=sum)
names(relative_w_3quser_group) <- c("Q1-Q3","Q4")
print(relative_w_3quser_group["Q4"])

# the value is 80.52367
# this means top-25% of users are responsible to generate 80% of the comments
# as expected the distribution of users in less skewed. 

# is there a large accumulation of users in some topics compared to others?
# group_by user subreddit and check 
# code to be written

# check the time for number of posts - what is the range of time redditors are most active?

total_posts <- db_subset %>% select(id)%>% collect() %>% data.frame() %>% nrow()
# we check the percent post roughly, that is we collect average number of posts 
# that hour and then divide by average number of posts per day for that month

# what is the average number of posts per day on May May2015?

avg_post_day <- db_subset %>% 
                  select(created_utc)  %>%
                    collect() %>%
                      data.frame() %>%
                        mutate(date = as.Date(as.POSIXct(created_utc,origin = '1970-01-01'))) %>%
                        select(date) %>%
                          group_by(date) %>%
                            summarise(n_posts = n())%>% 
                              .$n_posts %>%
                                mean()
print(avg_post_day)


db_post_hour <- db_subset %>% 
                  select(created_utc) %>%
                    collect() %>%
                      data.frame() %>%
                        mutate(hour = hour(as.POSIXct(created_utc,origin = '1970-01-01'))) %>%
                          group_by(hour) %>%
                            summarise(percent_posts = mean()/avg_post_day*100)




# lets visualize post per hour

ggplot(data = db_post_hour, aes(x=hour, y=percent_posts)) + 
  geom_bar(stat="identity", fill = "grey50") + 
  labs(x = "hour", 
       y = "average percent of posts", 
       title = "Average percent of posts per hour")



