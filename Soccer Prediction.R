## HarvardX: PH125.9x
## Data Science: Turkish League Soccer Prediction with Machine Learning
## Sanver Gozen
## May 21, 2021

############################# introduction ###############################

# Note: This process could take a couple of minutes

if(!require(dplyr)) install.packages("dplyr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", 
                                       repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                      repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", 
                                      repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", 
                                      repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", 
                                    repos = "http://cran.us.r-project.org")
if(!require(clusterSim)) install.packages("clusterSim", 
                                    repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", 
                                    repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", 
                                      repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", 
                                        repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", 
                                       repos = "http://cran.us.r-project.org")
if(!require(skellam)) install.packages("skellam", 
                                      repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", 
                                      repos = "http://cran.us.r-project.org")
# Load Libraries
library(dplyr) # Provides a set of tools for efficiently manipulating datasets.
library(tidyverse) # An opinionated collection of R packages.
library(Metrics) # For Machine Learning, and predictions.
library(caret) # Classification And REgression Training.
library(kableExtra) # For better visualization of the tables.
library(nnet) # For compute Neural Networks model
library(MASS) # Support Functions and datasets
library(clusterSim) # Searching for Optimal Clustering Procedure for a Data Set
library(party) # A Laboratory for Recursive Partytioning
library(e1071) # Misc Functions of the Department of Statistics
library(neuralnet) # Training of Neural Networks
library(GGally) # Allows to build a great scatterplot matrix.
library(skellam) # Densities and Sampling for the Skellam Distribution
library(scales) # Scale Functions for Visualization

# Download the data
# Set the time frame
x <- c(2019:2010)
alldata <- data.frame()
# Download each season by the time frame
for (val in x) {
  url1 = "https://sports-statistics.com/database/soccer-data/turkey-futbol-ligi-1-"
  url2 = val 
  url3 = "-to-" 
  url4 = val + 1 
  url5 = ".csv"
  url6 = "turkey-futbol-ligi-"
  href <- paste0(url1,url2,url3,url4,url5)
  href2 <- paste0(url6,url2,url5)
  download.file(href,href2)
  data <- read.csv(href2)
  data$HTHG <- as.integer(data$HTHG)
  # Remove empty rows from each file and clean the data
  final_data <- na.omit(data)
  if("PSCH" %in% names(data)) {final_data <- final_data %>% mutate(PSCH = as.character(PSCH))}
  # Merge the new data to the old one
  alldata <- bind_rows(alldata,final_data)
}

# Clean the data
alldata <- alldata %>% select_if(~ !any(is.na(.))) 
# Remove unnecessary variables from environment
rm(data,final_data,href,href2,url1,url2,url3,url4,url5,url6,val,x)

############################# Analysis ###############################
## Data Summary

# Check the data
dim(alldata)

# Let's see what is inside
head(alldata)%>%  
  kable() %>% kable_styling(font_size = 10, position = "center", 
                            latex_options = c("scale_down","HOLD_position"))

# Summarize the data
summary(alldata[1:10])%>%  
  kable() %>% kable_styling(font_size = 10, position = "center", 
                            latex_options = c("scale_down","HOLD_position"))

## Explain the Heads
# Div = League Division
# Date = Match Date (dd/mm/yy)
# HomeTeam = Home Team
# AwayTeam = Away Team
# FTHG and HG = Full Time Home Team Goals
# FTAG and AG = Full Time Away Team Goals
# FTR and Res = Full Time Result (H=Home Win, D=Draw, A=Away Win)
# HTHG = Half Time Home Team Goals
# HTAG = Half Time Away Team Goals
# HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)
# B365H = Bet365 home win odds
# B365D = Bet365 draw odds
# B365A = Bet365 away win odds
# BWH = Bet&Win home win odds
# BWD = Bet&Win draw odds
# BWA = Bet&Win away win odds
# IWH = Interwetten home win odds
# IWD = Interwetten draw odds
# IWA = Interwetten away win odds
# WHH = William Hill home win odds
# WHD = William Hill draw odds
# WHA = William Hill away win odds
# VCH = VC Bet home win odds
# VCD = VC Bet draw odds
# VCA = VC Bet away win odds

# Check NA values
anyNA(alldata)

# See and set the Unique Home-Away Teams
alldata %>% summarise(Teams = n_distinct(HomeTeam))
alldata %>% summarise(Teams = n_distinct(AwayTeam))

# Remove the Difference Team
first <- c(alldata$HomeTeam )
second <- c(alldata$AwayTeam)
setdiff(second,first)
alldata <- alldata %>% filter(!str_detect(AwayTeam, 'Balikesirspor'))

# Set the Unique Team
unique_teams <- alldata %>% group_by(HomeTeam) %>% head(20)
unique_teams

# String replace to avoid duplicate teams
alldata$HomeTeam <- lapply(alldata$HomeTeam, gsub, pattern = "Gaziantepspor", 
                           replacement = "Gaziantep", fixed = TRUE)
alldata$HomeTeam <- unlist(alldata$HomeTeam)
alldata$AwayTeam <- lapply(alldata$AwayTeam, gsub, pattern = "Gaziantepspor", 
                           replacement = "Gaziantep", fixed = TRUE)
alldata$AwayTeam <- unlist(alldata$AwayTeam)

# Now we have exact unique teams
unique_teams <- alldata %>% group_by(HomeTeam) %>% summarise()
# Home Teams
alldata %>% summarise(Teams = n_distinct(HomeTeam))
# Away Teams
alldata %>% summarise(Teams = n_distinct(AwayTeam))

# Home, Away and Draw count
table(alldata$FTR)

# Summarize results
summary(alldata$FTR)

# Full Time Result by Seasons
# Check the class
class(alldata$Date)
# Create "somedata" to not lose or mess any data from "alldata" data set.
somedata <- alldata
# Change Date format
somedata$newdate <- strptime(as.character(alldata$Date), "%d/%m/%Y")
# Replace bad data with the valid ones
somedata$newdate <- lapply(somedata$newdate, gsub, pattern = "/00", replacement = "20", fixed = TRUE)
somedata$newdate <- lapply(somedata$newdate, gsub, pattern = "/2019", replacement = "19", fixed = TRUE)
# Create a for loop for fix the dates
url1 = "00"
url2 <- c(19:10)
url3 = "20"
for (val1 in url2) {
  d <- paste0(url1,val1)
  f <- paste0(url3,val1)
    somedata$newdate <- lapply(somedata$newdate, gsub, pattern = d, replacement = f, fixed = TRUE)
  }
# Unlist the dates column 
somedata$newdate <- unlist(somedata$newdate)
# Check the class
class(somedata$newdate)
# Change the class of the date data
somedata$newdate <- as.Date(somedata$newdate)
# Plot Full Time Result by Season
season_graph <- somedata %>%
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  group_by(month, year) %>% group_by(newdate) %>% ggplot(aes(x = year, fill = FTR)) +
  geom_bar(position = position_dodge()) +
  ylab("Count") + 
  xlab("Season")+ scale_fill_discrete(name = "Reults", labels = c("Away", "Draw", "Home"),guide = guide_legend(reverse=TRUE))
# Plot the data
season_graph

# See detailed data
glimpse(alldata)

# Plot Matrix for understand and observe data
ggpairs(data=alldata, columns=c(5,6,11,13), title="Score Data",cardinality_threshold=NULL)

# Away, Draw, Home Means
FTR_means <- alldata %>%
  group_by(FTR) %>%
  summarise(mean_scored = mean(FTHG)) %>%
  print()

# Average Goal Score per Team
alldata %>%
  group_by(FTHG) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = FTHG, y = count)) + 
  geom_point() +
  xlab("Home Team Avarage Score") +
  ylab("Played Matches") +
  scale_y_continuous(labels = scales::label_number_si())+
  labs(title = "Average Score Distribution Per Teams")+
  theme()

# Average Goal Conceded per Team
alldata %>%
  group_by(FTAG) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = FTAG, y = count)) + 
  geom_point() +
  xlab("Home Team Avarage Conced") +
  ylab("Played Matches") +
  scale_y_continuous(labels = scales::label_number_si())+
  labs(title = "Average Conced Distribution Per Teams")+
  theme()

# All teams number of played games
alldata %>%
  group_by(HomeTeam) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = HomeTeam, y = count)) + 
  geom_point() +
  xlab("Teams") +
  ylab("Played Matches") +
  labs(title = "Number of Matches for Each Team")+
  theme(axis.text.x  = element_text(angle= 90))

# See the Best Teams
best_teams <- alldata %>% group_by(HomeTeam) %>%
  summarise(Scored_Mean = mean(FTHG),
            Conceeded_Mean = mean(FTAG)) %>% 
  arrange(desc(Scored_Mean-Conceeded_Mean)) %>% head(20)
best_teams

# Filtered-Teams, Scored-Conceded Average per team
filtered_teams <- alldata %>% group_by(HomeTeam) %>%
  summarise(Scored_Mean = mean(FTHG),
            Conceeded_Mean = mean(FTAG),
            count=n()) %>% filter(count > 100)%>% 
  arrange(desc(Scored_Mean-Conceeded_Mean)) %>% head(20)
filtered_teams

# Average of Game Results
H <- alldata %>% group_by(HomeTeam) %>% filter(FTR=="H") %>%  
  summarise(count = n())

A <- alldata %>% group_by(HomeTeam) %>% filter(FTR== "A") %>%  
  summarise(count = n())

D <- alldata %>% group_by(HomeTeam) %>% filter(FTR=="D") %>%  
  summarise(count = n())

# See the result
mean(D$count)
mean(A$count)
mean(H$count)

# Plot Home Away Matrix
HomeAwayMatrix <- alldata %>%
  # Remove draw games
  filter(!is.na(FTHG) & !(FTHG-FTAG==0)) %>%
  ggplot(., aes(x = HomeTeam, y = AwayTeam, fill = FTHG-FTAG)) +
  geom_tile() +
  # Add the scorelines
  geom_label(aes(label = paste(FTHG-FTAG)), size = 2) +
  # Coloring - Where green shows home wins and red an away wins
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0) +
  scale_x_discrete(limits = levels(alldata$HomeTeam)) +
  scale_y_discrete(limits = levels(alldata$AwayTeam)) + 
  theme(axis.text.x = element_text(angle = 90))  
# Plot the Matrix
HomeAwayMatrix

# Best Teams by attack and defense rating
rating <- alldata %>% group_by(HomeTeam) %>%
  summarise(Attack = mean(FTHG) / mean(alldata$FTHG),
            Defense = mean(FTAG)/ mean(alldata$FTAG)) %>% 
  arrange(desc(Attack-Defense)) %>% head(20)
rating

# Change the data to Home and NotHome.
somedata <- alldata
# Replace "Away" with "Not Home"
somedata$FTR <- lapply(somedata$FTR, gsub, pattern = "A", replacement = "NH", fixed = TRUE)
# Replace "Draw" with "Not Home"
somedata$FTR <- lapply(somedata$FTR, gsub, pattern = "D", replacement = "NH", fixed = TRUE)
somedata$FTR <- unlist(somedata$FTR)

# plot Home Goals scored Home - Not Home
HomeGoalsSocred <- somedata %>%
  ggplot(., aes(x = (FTHG-FTAG), fill = FTR)) +
  # smooth densities
  geom_density(adjust = 8, alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Goals scored at Home and Away Teams",
       x = "Goals Scored",
       y = "Density") +
  theme_minimal()
# plot
HomeGoalsSocred


############################# Modeling ###############################
## Train and Test data
# Create "somedata" to not lose any info from "alldata" data set.
somedata = alldata
set.seed(2105, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# Split data to train and test data by portion of %10-%90
test_index <- createDataPartition(y = somedata$HomeTeam, times = 1, 
                                  p = 0.1, list = FALSE)
train <- somedata[-test_index,]
test <- somedata[test_index,]
rm(test_index)

# Poisson Distribution - Predict Games by Home and Away Team
# Create a Model
poisson_model <- 
  rbind(
    data.frame(goals = train$FTHG,
               team = train$HomeTeam,
               opponent = train$AwayTeam,
               home = 1
               ),
    data.frame(goals = train$FTAG,
               team = train$AwayTeam,
               opponent = train$HomeTeam,
               home=0)) %>%
  glm(goals ~ team + home + opponent, family=poisson(link=log),data=.)
summary(poisson_model)

# Set Home and Away Team from the Data set
phome = "Goztep"
paway = "Besiktas"

# Predict Home Goal
homeXg <- predict(poisson_model, 
              data.frame(home=1, team= phome, 
                         opponent=paway)
              , type="response")
# Predict Away Goal
awayXg <- predict(poisson_model, 
              data.frame(home=0, team=paway, 
                         opponent=phome)
              , type="response")

# Create a function to see the probability of the score distribution.
ScoreGrid <- function(homeXg,awayXg){
  # Get the score data
  A <- as.numeric()
  B <- as.numeric()
  # Limit the score with 7 goals
  for(i in 0:6) {
    A[(i+1)] <- dpois(i,homeXg)
    B[(i+1)] <- dpois(i,awayXg)
  }
  # Built the grid for 7 goals
  A[8] <- 1 - sum(A[1:7])
  B[8] <- 1 - sum(B[1:7])
  name <- c("0","1","2","3","4","5","6","7+")
  zero <- mat.or.vec(8,1)
  C <- data.frame(row.names=name, "0"=zero, "1"=zero, "2"=zero, "3"=zero, "4"=zero,
                  "5"=zero, "6"=zero, "7+"=zero)
  for(j in 1:8) {
    for(k in 1:8) {
      C[j,k] <- A[k]*B[j]
    }
  }
  colnames(C) <- name
  return(round(C*100,4))
}

# Create a Score Heat Map to see the probability of the full time scores
ScoreHeatMap <- function(home,away,homeXg,awayXg){
  adjustedHome<-as.character(sub("_", " ", home))
  adjustedAway<-as.character(sub("_"," ",away))
  df <- ScoreGrid(homeXg,awayXg)
  # Use ScoreGrid function to create a heat map
  df %>% 
    as_tibble(rownames = all_of(away)) %>%
    pivot_longer(cols = -all_of(away), 
                 names_to = home, 
                 values_to = "Probability") %>%
    mutate_at(vars(all_of(away), home), 
              ~forcats::fct_relevel(.x, "7+", after = 7)) %>% 
    # Use black and white colors to see the strong and weak probability
    ggplot() + 
    geom_tile(aes_string(x=all_of(away), y=all_of(home), fill = "Probability")) +   
    scale_fill_gradient2(mid="white", high = "black")+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          plot.title = element_text(size=20,hjust = 0.5,face="bold",vjust =4),
          plot.subtitle = element_text(size=12,hjust = 0.5,vjust=4),
          axis.title.x=element_text(size=14,vjust=-0.5,face="bold"),
          axis.title.y=element_text(size=14, vjust =0.5,face="bold")
    )+
    labs(x=adjustedAway,y=adjustedHome,fill='Probability (%)')+ 
    ggtitle(label = "Expected Scores", subtitle = paste("Home:",round(homeXg,2),"-",round(awayXg,2),": Away"))
  
}
# Show the Heat Map
ScoreHeatMap(phome, paway, homeXg,awayXg)

# Predict Match Function - Set max goal 7
predict_match <- function(foot_model, HomeTeam, AwayTeam, max_goals=7){
  # Set mu For Poisson Distribution 
  home_goals_avg <- predict(foot_model,
                            data.frame(home=1, team=HomeTeam, 
                                       opponent=AwayTeam), type="response")
  away_goals_avg <- predict(foot_model, 
                            data.frame(home=0, team=AwayTeam, 
                                       opponent=HomeTeam), type="response")
  # dpois(x, mu) is the probability of x successes in a period when the expected number of events is mu
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}
# Store the data generated by function
teams <- predict_match(poisson_model, phome, paway, max_goals=7)
# Store the results
H <- sum(teams[lower.tri(teams)])
D <- sum(diag(teams))
A <- sum(teams[upper.tri(teams)])
# Print the winning chance results by percentage
print (c(paste0("Home: %",round(H,digits=4)*100),
         paste0("Draw: %",round(D,digits=4)*100),
         paste0("Away: %",round(A,digits=4)*100)))

# Data changing
# Create "somedata" to not lose any info from "alldata".
somedata <- subset(alldata, select=-c(Div,Date))
# Change Home-Away-Draw to Home-NotHome for Binomial Models
somedata$FTR <- lapply(somedata$FTR, gsub, pattern = "A", replacement = "NH", fixed = TRUE)
somedata$FTR <- lapply(somedata$FTR, gsub, pattern = "D", replacement = "NH", fixed = TRUE)
somedata$FTR <- unlist(somedata$FTR)
somedata$HTR <- lapply(somedata$HTR, gsub, pattern = "A", replacement = "NH", fixed = TRUE)
somedata$HTR <- lapply(somedata$HTR, gsub, pattern = "D", replacement = "NH", fixed = TRUE)
somedata$HTR <- unlist(somedata$HTR)
# Factorization of the data for Binomial Models
somedata$FTR <- factor(somedata$FTR)
somedata$HTR <- factor(somedata$HTR)
somedata$FTHG <- factor(somedata$FTHG)
somedata$FTAG <- factor(somedata$FTAG)
somedata$HTHG <- factor(somedata$HTHG)
somedata$HTAG <- factor(somedata$HTAG)
somedata$HomeTeam <- factor(somedata$HomeTeam)
somedata$AwayTeam <- factor(somedata$AwayTeam)
# Summarize data
summary(somedata)
# Create train and test data
set.seed(2105, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = somedata$HomeTeam, times = 1, 
                                  p = 0.1, list = FALSE)
train <- somedata[-test_index,]
test <- somedata[test_index,]
rm(test_index)

## Naive Bayes Model
naive_bayes_model<-naiveBayes(FTR ~ ., data = train)
naive_bayes_predictions<-predict(naive_bayes_model, newdata=test)
naive_bayes_accuracy=round(mean(naive_bayes_predictions==test$FTR),2)*100
table(naive_bayes_predictions,test[,5])
# Accuracy of our model
result <- mean(naive_bayes_predictions==test[,5])
# Add to the Table
all_rmse <- data.frame()
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "Naive Bayes Model", 
                                 Model_Accuracy = round(result*100,2)))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## SVM Radial Model
svm_model_radial <- svm(FTR ~ ., kernel = "radial", data=train)
summary(svm_model_radial)
prediction_radial <- predict(svm_model_radial, newdata = test)
# Create confusion Matrix
cm <- confusionMatrix(prediction_radial, test$FTR )
# Accuracy of our model
result <- cm$overall[[1]]
# Add to the Table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "SVM_Model_Radial", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## SVM Linear Model
svm_model_Linear <- svm(FTR ~ .,kernel = "linear", data=train)
summary(svm_model_Linear)
prediction_linear <- predict(svm_model_Linear, newdata = test)
# Create confusion Matrix
cm <- confusionMatrix(prediction_linear, test$FTR )
# Accuracy of our model
result <- cm$overall[[1]]
# Add to the Table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "SVM_Model_Linear", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))



## SVM Polynomial Model
svm_model_poli <- svm(FTR ~ .,kernel = "polynomial",degree = 3, data=train)
summary(svm_model_poli)
prediction_poli <- predict(svm_model_poli, newdata = test)
# Create confusion Matrix
cm <- confusionMatrix(prediction_poli, test$FTR )
# Accuracy of our model
result <- cm$overall[[1]]
# Add to the Table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "SVM_Model_Polinominal", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## SVM Polynomial 5th Degree Model
svm_model_poli_5 <- svm(FTR ~ .,kernel = "polynomial",degree = 5, data=train)
summary(svm_model_poli_5)
prediction_poli_5 <- predict(svm_model_poli_5, newdata = test)
# Create confusion Matrix
cm <- confusionMatrix(prediction_poli_5, test$FTR )
# Accuracy of our model
result <- cm$overall[[1]]
# Add to the Table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "SVM_Model_Polinominal_5th", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## QDA Model - Prediction Model
test = data.frame(lapply(test, function(x) as.numeric(x)))
train = data.frame(lapply(train, function(x) as.numeric(x)))
qda.fit = qda(FTR ~.,data=train)
summary(qda.fit)
qda.class=predict(qda.fit,test)$class 
table(qda.class,test[,5]) # 5th column FTR
# Accuracy of our model
result <- mean(qda.class==test[,5]) # 5th column FTR (Accuracy)

all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "QDA_Model", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## LDA - Prediction Model
lda.fit = lda(FTR ~ .,data=train)
summary(lda.fit)
lda.pred=predict(lda.fit, test[-5,])
names(lda.pred)
lda.class=predict(lda.fit,test)$class
table(lda.class,test[,5]) # 5th column FTR
# Accuracy of our model
result <- mean(lda.class==test[,5]) # 5th column FTR (Accuracy)

all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "LDA_Model", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## GLM - Probability Model with poisson Regression
glm.fit = glm(FTR ~ .,data=train, family=poisson(link=log))
summary(glm.fit)
glm.prob=predict(glm.fit,test,type="response")
glm.prob
glm.prob = ifelse(glm.prob>0.5,1,0)
table(glm.prob,test[,5])
# Accuracy of our model
result <- mean(glm.prob==test[,5])

all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "GLM_Poisson_Regression_Model", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

## Neural Networks
train = data.Normalization (train,type="n4",normalization="column")
test = data.Normalization (test,type="n4",normalization="column")

set.seed(2105)
# 5 neurons hidden layer
neural_networks_model = neuralnet(FTR ~ HomeTeam+AwayTeam+FTHG+FTAG+HTR,
              data = train,hidden = 5,linear.output = FALSE)
output <- compute(neural_networks_model, train[,-5])
p1 <- output$net.result
prediction_neural_networks_train <- ifelse(p1>0.5, 1, 0)
table_train <- table(prediction_neural_networks_train, train$FTR)
table_train
# Accuracy of our model
sum(diag(table_train))/sum(table_train)
# Confusion Matrix - Testing data
output <- compute(neural_networks_model, test[,-5])
p2 <- output$net.result
prediction_neural_networks_test <- ifelse(p2>0.5, 1, 0)
table_test <- table(prediction_neural_networks_test, test$FTR)
table_test
result <- sum(diag(table_test))/sum(table_test)

all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "Neural Networks", 
                                 Model_Accuracy = round(result*100,2)))
# Write the result on the table
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

# Results 
## Discussion
# Conclusion 
