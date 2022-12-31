library(dplyr)
library(ggbiplot)
library(knitr)
library(kableExtra)
library(ggpubr)
library(janitor)
library(MASS)
library(ggplot2)
library(tidyverse)
library(caret)
library(factoextra)


#PCA

data=heart_failure_clinical_records_dataset library("dplyr")
age_group <- data %>% mutate(case_when(data$age >= 85 ~ '85+',
                                       data$age >= 40  
                                       & age < 55 ~ '[40,55)')) 
age_group

df=data.frame(data$creatinine_phosphokinase, data$serum_creatinine,
              
              data$serum_sodium, data$platelets, data$ejection_fraction) df

new_data <- df %>% rename("Platelets" = "data.platelets",
                          "Serum Creatinine"="data.serum_creatinine",
                          "Serum Sodium"="data.serum_sodium",
                          "Ejection Fraction"="data.ejection_fraction",
                          "Creatinine Phosphokinase"="data.creatinine_phosphokinase") new_data
cor(df)
datapca <- prcomp(new_data,sc=TRUE) datapca
summary(datapca)
ggbiplot(datapca, groups=age_group$`case_when(...)`) screeplot(datapca, type="lines")

#CLUSTERING
Heart <- heart_failure_clinical_records_dataset 
Heart$smoking [Heart$smoking == '1'] <- "Smoker" 
Heart$smoking [Heart$smoking == '0'] <- "Non-Smoker" 
km <- kmeans(scale(Heart[, c(3,5,7:9)]), 2, nstart = 25) 
km


cluster <- function(data, cat){
  pca <- prcomp(data, scale = TRUE)
  Cluster.coord <- as.data.frame(get_pca_ind(pca)$coord)
  Cluster.coord$cluster <- factor(km$cluster)
  Cluster.coord$Class <- cat
  eigenvalue <- round(get_eigenvalue(pca), 1)
  variance.percent <- eigenvalue$variance.percent
  x <- table(Cluster.coord$Class,Cluster.coord$cluster)/length(Cluster.coord$Class)
  plot <- ggscatter(
    Cluster.coord, x = "Dim.1", y = "Dim.2", main = "Cluster Plot", 
    
    xlab = paste0("Component 1 (", variance.percent[1], "% )" ),
    
  )
  mylist <- list(x, plot)
  
}


##Smoking plot/Distribution table
cluster(Heart[, c(3,5,7:9)], Heart[, 11])	


##Gender plot/Distribution table
Heart$sex [Heart$sex == '1'] <- "Male" Heart$sex [Heart$sex == '0'] <- "Female"

cluster(Heart[, c(3,5,7:9)], Heart[, 10])		


#LOGISTIC REGRESSION
data <- read.csv("heart_failure.csv")

full.model <- glm(DEATH_EVENT ~ ., data = data, family = "binomial") summary(full.model)

step.model <- stepAIC(full.model, direction = "both", trace = F) summary(step.model)

test.data <- rbind(data[data$DEATH_EVENT == 1,][1:20,], data[data$DEATH_EVENT == 0,][1:20,])

train.data <- rbind(data[data$DEATH_EVENT == 1,][21:96,], data[data$DEATH_EVENT == 0,][21:203,])

train.model <- glm(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium + time, data = train.data, family = "binomial")

test.data$DEATH_EVENT <- factor(test.data$DEATH_EVENT)

pred <- factor(as.integer(ifelse(predict(train.model, newdata = test.data, type = "response")>.5, '1', '0')) 

confusion <- confusionMatrix(test.data$DEATH_EVENT, pred)
confusion
               