rm(list=ls())
library(dplyr)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
source("cleanAndManipulate.R")
predictors <- cleanAndManipulate(train)$predictors
Survived <- cleanAndManipulate(train)$Survived[[1]]

library(randomForest)
set.seed(1)
rf <- randomForest(predictors,Survived,importance=T,mtry=3)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
p
rf
# ggsave("2_feature_importance.png", p)