library('ggplot2')
library('ggthemes')
library('dplyr')

#read the data
train <- read.csv('data/train.csv', stringsAsFactors = F)
train$Age<-as.numeric(as.character(train$Age))
train$Age[is.na(train$Age)] = median(train$Age, na.rm=TRUE)
train$Age <- as.integer(train$Age)
train$Survived[train$Survived == 0] <- 'No'
train$Survived[train$Survived == 1] <- 'Yes'

#plotting embarked
embark_plot <- function(n1,n2){
  n_train <- train
  n_train$Age <- as.integer(n_train$Age)
  ggplot(data = n_train %>% filter(n_train$Age > n1 & n_train$Age < n2),
         aes(x = factor(Embarked), fill = factor(Survived))) +
    geom_bar(stat='count') +
    theme_few() +
    labs(x = "Embarked", fill = "Survived")+
    scale_x_discrete(limit = c("C", "Q", "S"),
                     labels = c("Cherbourg","Queenstown","Southampton"))
}

#plotting family size
fsize_plot <-  function(n1,n2){
  n_train <- train
  n_train$Fare <- as.integer(n_train$Fare)
  n_train$FamSize <- n_train$SibSp + n_train$Parch + 1
  ggplot(data = n_train %>% filter(n_train$Age > n1 & n_train$Age < n2),
         aes(x = factor(FamSize), fill = factor(Survived))) +
    geom_bar(stat='count') +
    theme_few() +
    labs(x = "Family Size", fill = "Survived")
}

#plotting gender
gender_plot <-  function( n1,n2){
  n_train <- train
  n_train$Fare <- as.integer(n_train$Fare)
  ggplot(data = n_train %>% filter(n_train$Age > n1 & n_train$Age < n2),
         aes(x = factor(Sex), fill = factor(Survived))) +
    geom_bar(stat='count') +
    theme_few() +
    labs(x = "Gender", fill = "Survived")
}

#plotting Pclass
pclass_plot <-  function( n1,n2){
  n_train <- train
  n_train$Fare <- as.integer(n_train$Fare)
  ggplot(data = n_train %>% filter(n_train$Age > n1 & n_train$Age < n2),
         aes(x = factor(Pclass), fill = factor(Survived))) +
    geom_bar(stat='count') +
    theme_few()+
    labs(x = "Ticket Class", fill = "Survived")+
    scale_x_discrete(limit = c("1", "2", "3"),
                     labels = c("1st","2nd","3rd"))
}

#embark_plot(40,50)
