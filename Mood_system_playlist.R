library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

#load dataset

spotify_data <- read.csv("dataset.csv")
print(spotify_data)
#select relevant features and preprocess

spotify_data <- spotify_data %>%
  select(valence,energy,tempo,danceability)%>%
  mutate(mood = case_when(
    valence > 0.7 & energy > 0.6 ~ "Happy",
    valence < 0.4 & energy < 0.5 ~ "Sad",
    valence > 0.4 & tempo < 100 ~ "Relaxed",
    TRUE ~ "Neutral"
  ))

#converting mood to factor
spotify_data$mood <- as.factor(spotify_data$mood)

#to view the data structure
str(spotify_data)

#split data into training(80%) and testing sets(20%)

set.seed(123)
data_train_index <- createDataPartition(spotify_data$mood, p=0.8, list = FALSE)
train_data <- spotify_data[data_train_index,]
test_data <- spotify_data[-data_train_index,]

#using decision tree model to train the data

decision_tree <- rpart(
  mood ~ valence + energy + tempo + danceability,
  data = train_data,
  method = "class"
)

#print the tree summary
summary(decision_tree)

#plot the decision tree
rpart.plot(decision_tree,type = 3, extra = 104, under = TRUE, fallen.leaves = TRUE)


#predict moods for the test
music_pred <- predict(decision_tree,newdata = test_data, type = "class")

#evaluate accuracy
confusionMatrix(music_pred,test_data$mood)

cm <- confusionMatrix(music_pred,test_data$mood)
cm_table <-as.table(cm)
cm_table
#predict moods
new_songs <- data.frame(
  song_name = c("Can't Help Falling In Love","Making All Things New","Sleepwalking in the Rain"),
  valence = c(0.8, 0.3, 0.5),
  energy = c(0.9, 0.2, 0.4),
  tempo = c(120, 60, 80),
  danceability = c(0.8, 0.5, 0.6)
)

new_songs$mood <- predict(decision_tree,newdata = new_songs,type = "class")
print(new_songs)