# Provided code
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Head of edx
head(edx)

# plot distribution of Ratings
edx %>% ggplot(aes(rating)) +
  geom_bar(color = "Blue")
labs(title = "Distribution of Ratings",
     x = "Rating",
     y = "Frequency")

# Metrics per genre
edx_metrics_perGenre <- edx %>%
separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(Total_ratings_perGenre = n(), Avarage_ratings_perGenre = mean(rating), 
            Total_movies_perGenre = n_distinct(movieId),Total_users_perGenre = n_distinct(userId))
edx_metrics_perGenre

# Plot total ratings per genre
edx_metrics_perGenre %>%
  ggplot(aes(x = reorder(genres, -Total_ratings_perGenre), 
             y = Total_ratings_perGenre, fill = genres, label = Total_ratings_perGenre)) +
  geom_col() + 
  geom_text(aes(label = Total_ratings_perGenre), angle = 90, 
            color = "white", size = 3, check_overlap = T, position = position_stack((vjust = 0.5))) + 
  labs(title = "Total ratings per genre", x = "Genres", y = "Total ratings") +theme(axis.text.x = element_text(angle = 90))

# Baseline avarage rating of all movies
mu_base <- mean(edx$rating)
mu_base

# Baseline RMSE
RMSE_base <- RMSE(edx$rating, mu_base)
RMSE_base

# Creating tibble
RMSE_table <- tibble(Method = "Baseline", RMSE = RMSE_base) 
RMSE_table

# Modelling user and movie effect
mu <- mean(edx$rating)

movie_avg <- edx %>%
  group_by(movieId) %>%
  summarize(m_i = mean(rating - mu))

user_avg <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(u_i = mean(rating - mu - m_i))

predicted_ratings <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(pred = mu + m_i + u_i) %>% .$pred

model_RMSE <- RMSE(predicted_ratings, edx$rating)
model_RMSE 

# Update table
RMSE_table <- bind_rows(RMSE_table, tibble(Method = "User & Movie Effect", RMSE = model_RMSE))
RMSE_table


rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Select lambda
lambdas <- seq(0, 10, .2)

# Create regularization function
RMSE_reg_fun <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  m_i <- edx %>%
    group_by(movieId) %>%
    summarize(m_i = sum(rating - mu)/(n()+l))
  
  u_i <- edx %>%
    left_join(m_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_i = sum(rating - m_i - mu)/(n()+l))
  
  predicted_ratings <- edx %>%
    left_join(m_i, by = "movieId") %>% 
    left_join(u_i, by = "userId") %>%
    mutate(pred = mu + m_i + u_i) %>% .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})
# Plot lambda vs RMSE
qplot(lambdas, RMSE_reg_fun,
      main = "Regularisation",
      xlab = "Lambda", ylab = "RMSE")

# Optimal lambda
lambda_opt <- lambdas[which.min(RMSE_reg_fun)]
lambda_opt

# Results
min(RMSE_reg_fun)

# Updated table
RMSE_table <- bind_rows(RMSE_table, tibble(Method = "User & Movie Effect + Regularisation", RMSE = min(RMSE_reg_fun)))
RMSE_table

# Baseline avarage rating of all movies
mu_base <- mean(validation$rating)
mu_base

# Baseline RMSE
RMSE_base <- RMSE(validation$rating, mu_base)
RMSE_base

# Update table
RMSE_table <- bind_rows(RMSE_table, tibble(Method = "Baseline(validation)", RMSE = RMSE_base))
RMSE_table

# Modelling user and movie effect
mu <- mean(validation$rating)

movie_avg <- validation %>%
  group_by(movieId) %>%
  summarize(m_i = mean(rating - mu))

user_avg <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(u_i = mean(rating - mu - m_i))

predicted_ratings <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(pred = mu + m_i + u_i) %>% .$pred

model_RMSE <- RMSE(predicted_ratings, validation$rating)
model_RMSE 

# Update table
RMSE_table <- bind_rows(RMSE_table, tibble(Method = "User & Movie Effect (validation)", RMSE = model_RMSE))
RMSE_table

# Select lambda
lambdas <- seq(0, 10, .2)

# Create regularization function
RMSE_reg_fun <- sapply(lambdas, function(l){
  
  mu <- mean(validation$rating)
  
  m_i <- validation %>%
    group_by(movieId) %>%
    summarize(m_i = sum(rating - mu)/(n()+l))
  
  u_i <- validation %>%
    left_join(m_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_i = sum(rating - m_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>%
    left_join(m_i, by = "movieId") %>% 
    left_join(u_i, by = "userId") %>%
    mutate(pred = mu + m_i + u_i) %>% .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})
# Plot lambda vs RMSE
qplot(lambdas, RMSE_reg_fun,
      main = "Regularisation",
      xlab = "Lambda", ylab = "RMSE")

# Optimal lambda
lambda_opt <- lambdas[which.min(RMSE_reg_fun)]
lambda_opt

# Results
min(RMSE_reg_fun)

# Updated table
RMSE_table <- bind_rows(RMSE_table, tibble(Method = "User & Movie Effect + Regularisation (validation)", RMSE = min(RMSE_reg_fun)))
RMSE_table
