

sample_data_2 <- read_delim("~/Desktop/DS_Modeling_Project/Fantasy_NBA/combined_nba_data.csv", 
                          ",", escape_double = FALSE, trim_ws = TRUE)
#View(train_data)

train_data_2 <- na.omit(sample_data_2)

library(dplyr)
library(GGally)
library(ggplot2)
library(car)
library(leaps)
library(tictoc)
library(MASS)
library(xgboost)
library(caret)

#create a new dataframe removing the features below... 
f.c.2 <- within(train_data_2, rm(`Proj Mins`, `My Proj`, Exposure, `FC Proj`, Value))

f.c.2 <- filter(f.c.2, f.c.2$Score >= 10)
f.c.2 <- f.c.2[, -(1:6)]
#f.c.2 <- f.c.2[, -(2)]

ohe_feats = c('Def v Pos')

#### for training
dummies <- dummyVars(~ `Def v Pos`, data = f.c.2)
df_all_ohe <- as.data.frame(predict(dummies, newdata = f.c.2))
df_all_combined <- cbind(f.c.2[,-c(which(colnames(f.c.2) %in% ohe_feats))], df_all_ohe)


# split the data up randomly into training and test data
# how often shuld we re-train? 
require(caTools)
set.seed(1131) #set a random see to be able to re-generate the split sections
sample = sample.split(df_all_combined, SplitRatio = .80)
train = subset(df_all_combined, sample == TRUE)
test  = subset(df_all_combined, sample == FALSE)

### Still going to remive player name
name = test$Player

model_xgb1 <- xgboost(data.matrix(train[,-12]),
                      label=data.matrix(train[,12]),
                      booster = "dart",
                      objective="reg:linear",
                      nrounds=1000,
                      max.depth=3,
                      eta=0.05,
                      colsample_bytree=0.8,
                      seed=235,
                      eval_metric="rmse",
                      eval_metric = "error", 
                      alpha=0.1,
                      verbose = 1,
                      print_every_n = 100)


test_xgb1 <- predict(model_xgb1, data.matrix(test))



actual <- test$Score
plot(test_xgb1 ~ actual)
abline(lm(test_xgb1 ~ actual))





