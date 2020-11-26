##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
#Data partition within edx dataset for training and testing
##########################################################

set.seed(1, sample.kind="Rounding")

#Randomly splitting dataset to obtain test index (train:test = 9:1)
test_index_edx <- createDataPartition(y = edx$rating, 
                                      times = 1, 
                                      p = 0.1, 
                                      list = FALSE)
train_set_edx <- edx[-test_index_edx,]
temp <- edx[test_index_edx,]

#userId and movieId in test dataset are also present in train dataset 
test_set_edx <- temp %>% 
  semi_join(train_set_edx, by = "userId") %>%
  semi_join(train_set_edx, by = "movieId")

#For removed rows in test dataset, add them back into train dataset
rejected <- anti_join(temp, test_set_edx)
train_set_edx <- rbind(train_set_edx, rejected)

remove(test_index_edx, temp, rejected)

##########################################################
#Exploring the dataset
##########################################################
#Installing ggthemes and scales for plotting graphs
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")

# Overview of edx dataset
head(edx)

# Summary of edx dataset
dim(edx) #9000055 rows and 6 columns

edx_summary <- data.frame(variables = c("userID","movieID","rating","timestamp","title","genres"),
                          variables_class = c(class(edx$userId),class(edx$movieId),class(edx$rating),class(edx$timestamp),class(edx$title),class(edx$genres)),
                          variables_distinct_n = c(n_distinct(edx$userId),n_distinct(edx$movieId),"N/A","N/A",n_distinct(edx$title),n_distinct(edx$genres)))
edx_summary 
#69878 unique users/raters, 10677 unique movies, 10676 titles and 797 combinations of genres

edx %>% ggplot(aes(x = rating)) + 
  geom_histogram(binwidth = 0.1) +
  xlab("Ratings") +
  ylab("Frequencies") +
  ggtitle("Distribution of ratings", subtitle = "Higher ratings generally appear more.") +
  theme_gdocs()
#ratings are from 0 to 5 in 0.5 intervals

#summary of validation set
dim(validation) #999999 rows and 6 columns

val_summary <- data.frame(variables = c("userID","movieID","title","genres"),
                          variables_distinct_n = c(n_distinct(validation$userId),n_distinct(validation$movieId),n_distinct(validation$title),n_distinct(validation$genres)))

val_summary 
#68534 unique users/raters, 9809 unique movies, 9809 unique titles and 773 combinations of genres

##########################################################
#Analysis by variables
##########################################################

#userId

#Create summary of user ratings
user_summary <- edx %>% group_by(userId) %>%
  summarize(n_ratings = n(),
            mu = mean(rating),
            sd = sd(rating))

head(user_summary)

#Plotting number of users and their ratings
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_density(fill="light blue") +
  geom_vline(aes(xintercept = mean(user_summary$n_ratings)),color="red",linetype="dashed") +
  scale_x_log10() + 
  xlab("Number of ratings") +
  ylab("Density") +
  ggtitle("Number of ratings by users",subtitle = "Distribution is skewed to the right. Average number of ratings given per user is 129.") +
  scale_y_continuous(labels = comma) + 
  theme_gdocs()

#movieID

#Summary of movies and their respective number of ratings
movie_summary <- edx %>% group_by(movieId) %>%
  summarize(n_ratings = n(),
            mu = mean(rating),
            sd = sd(rating))

head(movie_summary)

#Plotting movies and number of ratings
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_density(fill="light blue") +
  geom_vline(aes(xintercept=mean(movie_summary$n_ratings)),color="red",linetype="dashed") +
  scale_x_log10() +
  ggtitle("Number of ratings by movies", 
          subtitle = "Average number of ratings per movie is 843.") +
  xlab("Number of ratings") +
  ylab("Density") + 
  theme_gdocs()

#rating

#Creating summary of ratings 
ratings_summary <- edx %>% group_by(rating) %>% 
  summarize(n=n())

ratings_summary

#Plotting frequencies of each rating(from 0.5 to 5)
edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) +
  ggtitle("Distributions of ratings", subtitle = "Average rating is 3.5. In general, integer ratings appear more than decimal ratings.") + 
  xlab("Ratings") +
  ylab("Frequency") +
  theme_gdocs()

#genres

#Creating summary of genres (combinations of genres and their appearances)
genres_summary1 <- edx %>% group_by(genres) %>%
  summarise(n=n())

head(genres_summary1)

#Creating summary of genres (each individual genre and their appearances)
genres_summary2 <- edx %>% separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% 
  summarize(n=n())

head(genres_summary2)

#time

#Calling lubridate for date-time wrangling 
library(lubridate)

#Plotting years and their respective number of ratings
edx %>% mutate(year = year(as_datetime(timestamp))) %>%
  ggplot(aes(x=year)) +
  geom_histogram() +
  scale_y_continuous(labels = comma) +
  ggtitle("Number of ratings by year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  theme_gdocs()

##########################################################
#Defining rmse (Root-Mean-Square Error) function 
##########################################################
rmse <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

##########################################################
#Data Modelling
##########################################################
#Set RMSE Goal and creating result tibble
result <- tibble(Method = "Goal", RMSE = 0.86490)

##########################################################
#Mean
##########################################################
#Calculate mean rating in train dataset
mu <- mean(train_set_edx$rating)

#Calculate the RMSE and including RMSE by mean method into result tibble 
result <- bind_rows(result,tibble(Method = "Mean", 
                                  RMSE = rmse(test_set_edx$rating, mu)))

result #RMSE by mean is 1.06

##########################################################
#Mean + Movie Effect (bi)
##########################################################
#Calculating Movie Effect (bi)
bi <- train_set_edx %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu))

#Accounting movie effect (bi) into ratings of each movie   
y_hat_1 <- mu + test_set_edx %>% 
  left_join(bi, by = "movieId") %>% 
  pull(bi)

#Calculate the RMSE and including RMSE by Mean + Movie Effect into result tibble  
result <- bind_rows(result, 
                    tibble(Method = "Mean + Movie Effect", 
                           RMSE = rmse(test_set_edx$rating, y_hat_1)))


result #RMSE by Mean + Movie Effect is 0.943

##########################################################
#Mean + Movie Effect (bi) + User Effect (bu)
##########################################################
#Calculating User Effect (bu)
bu <- train_set_edx %>% 
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

#Prediction based on Mean, Movie Effect and User Effect
y_hat_2 <- test_set_edx %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(predict = mu + bi + bu) %>%
  pull(predict)

#Calculate the RMSE and including RMSE by Mean + Movie Effect + User Effect into result tibble
result <- bind_rows(result, 
                    tibble(Method = "Mean + Movie Effect + User Effect", 
                           RMSE = rmse(test_set_edx$rating, y_hat_2)))
  
result #RMSE by Mean + Movie Effect + User Effect is 0.865

##########################################################
#Regularization
##########################################################
#Defining range of values for lambda for testing
lambdas <- seq(0, 10, 0.25)

#Defining regularization function
regularization <- function(lambda, train, test){
  #Calculating mean rating
  mu <- mean(train$rating)
  #calculating Movie Effect (bi)
  bi <- train %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+lambda))
  #Calculating User Effect (bu)  
  bu <- train %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+lambda))
  #Mean + Movie Effect + User Effect   
  predictions <- test %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred_rating = mu + bi + bu) %>%
    pull(pred_rating)
  
  return(rmse(predictions, test$rating))
}

#Applying regularization function for each lambda 
rmse_reg <- sapply(lambdas,regularization,
                   train=train_set_edx,test=test_set_edx)

#Plotting values of lambda and their respective RMSE
qplot(lambdas, rmse_reg)  

#Determining which value of lambda yields the smallest value of RMSE
lambda <- lambdas[which.min(rmse_reg)]

lambda

#Calculating RMSE and including RMSE with Regularization Model into result tibble
result <- bind_rows(result,
                    tibble(Method="Regularized Model",  
                           RMSE = min(rmse_reg)))

result #RMSE by Regularization method is 0.864

##########################################################
#Matrix Factorization
##########################################################
#Installing recosystem for Matrix Factorization
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")

set.seed(1, sample.kind = "Rounding")

#Data wrangling for train dataset and test dataset so that they fit the required format for recosystem
training_data <-  with(train_set_edx, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))
testing_data  <-  with(test_set_edx,  data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Creating model object r
r <-  recosystem::Reco()

# Selecting tuning parameters
opts <- r$tune(training_data, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                          costp_l1 = 0, costq_l1 = 0,
                                          nthread = 1, niter = 10))
# Training algorithm  
r$train(training_data, opts = c(opts$min, nthread = 1, niter = 20))

#Extracting prediction from r
y_hat_3 <- r$predict(testing_data, out_memory())

#Calculating RMSE by Matrix Factorization and including it into result tibble
result <- bind_rows(result, 
                    tibble(Method = "Matrix Factorization by recosystem", 
                           RMSE = rmse(test_set_edx$rating, y_hat_3)))

result #RMSE by Matrix Factorization is 0.786

##########################################################
#Final Validation on validation dataset
##########################################################
set.seed(1, sample.kind = "Rounding")

#Data wrangling for edx dataset and validation dataset so that they fit the required format for recosystem
edx_recosystem <-  with(edx, data_memory(user_index = userId, 
                                         item_index = movieId, 
                                         rating = rating))
validation_recosystem  <-  with(validation, data_memory(user_index = userId, 
                                                        item_index = movieId, 
                                                        rating = rating))

# Creating model object r
r <-  recosystem::Reco()

# Selecting tuning parameters
opts <-  r$tune(edx_recosystem, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))

# Training algorithm
r$train(edx_recosystem, opts = c(opts$min, nthread = 1, niter = 20))

#Extracting prediction from r
y_hat_final <-  r$predict(validation_recosystem, out_memory())

#Calculating Final RMSE by Matrix Factorization and including into result tibble 
result <- bind_rows(result, 
                    tibble(Method = "Final Validation", 
                           RMSE = RMSE(validation$rating, y_hat_final)))

result #Final Validation yields RMSE of 0.783

#END