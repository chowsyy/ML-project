#Boston Airbnb Price predition (find out the influential factors that are affecting the listing price)
#by Jerry Chow 

# open dataset from Kaggle 

working_dir = "D:/dataset/listings.csv"

#The following is the list of packages I use:
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(leaflet)
library(dplyr)
library(stringr)
library(lattice)
library(caret)
library(modelr)
library(ranger)
library(magrittr)
library(corrplot)

#read csv
listings = read.csv("D:/dataset/listings.csv")
df <- listings

#Data manipulation
# check for missing values
missing_airbnb <- df %>% summarise_all(~(sum(is.na(.))/n()))
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")
missing_airbnb <- missing_airbnb[missing_airbnb$percent_missing > 0.0, ] 
ggplot(missing_airbnb, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip() + 
  theme()  +
  ggtitle("Missing Data") +
  xlab("Column name") +
  ylab("Percentage missing")

# data visulization
#Price by room type
ggplot(df, aes(x = room_type, y = price)) +
  geom_violin() +
  scale_y_log10()


ggplot(df, aes(x = neighbourhood_cleansed, y = price)) +
  geom_violin() +
  scale_y_log10()

#drop rows with missing values, drop columns
df<- df %>% select(neighbourhood_cleansed, room_type, accommodates, bathrooms, price, cancellation_policy, review_scores_rating)
df2 <- listings %>% select(amenities)
# Change type of price
df$price <- as.character(df$price)
# Remove non-numeric characters of price
df$price <- parse_number(df$price)
df$review_scores_rating[is.na(df$review_scores_rating)]<-0
df$price[is.na(df$price)]<-0

  

parsed_amenities <-
  df2 %>% 
  .$amenities %>% 
  sub("^\\{(.*)\\}$", "\\1\n", x = .) %>% 
  lapply(function(x) names(read_csv(x)))
df1 <-
  unique(unlist(parsed_amenities)) %>% 
  .[!grepl("translation missing", .)] %>% 
  setNames(., .) %>% 
  lapply(function(x) vapply(parsed_amenities, "%in%", logical(1), x = x)) %>% 
  as_data_frame()

df1 %<>% mutate_if(is.logical,as.numeric) 

df <- merge(df, df1, by="row.names", all.x=TRUE)

# Change type of price
df$price <- as.character(df$price)
# Remove non-numeric characters of price
df$price <- parse_number(df$price)
df$review_scores_rating[is.na(df$review_scores_rating)]<-0
df$price[is.na(df$price)]<-0

colnames(df) <- make.names(colnames(df))
df[is.na(df)] <- 0
df$Row.names<- NULL

#the correlation between price and other features
df_cor <- df[, sapply(df, is.numeric)]
correlation_matrix <- cor(df_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")
#### 



#machine learning 
# create a testing and training set
set.seed(1111)
train_index <- sample(1:nrow(df), 0.9 * nrow(df))
df_train <- df[train_index, ]
df_test <- df[-train_index, ]
train_index <- createDataPartition(df$price,
                                   p = 0.8,
                                   list = FALSE,
                                   times = 10)


#K-cross valdidaton
df_grouped <- cbind(df_train[1:50, ], group = rep(1:10, each = 5))

group_folds <- groupKFold(df_grouped$group, k = 10)
group_folds

set.seed(1111)
in_training <- createDataPartition(df_train$price, p = .75, list = FALSE)
training <- df_train[ in_training,]
testing  <- df_train[-in_training,]

fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

#### 3. ML experiment and results
#Regression Trees
set.seed(1111)
dt_fit <- train(
  price ~ .,
  data = training,
  method = "treebag",
  metric= "RMSE",
  trControl = fit_control,
  importance = TRUE
)
dt_y_pred <- predict(dt_fit, testing)
RMSE(dt_y_pred, testing$price)

dtImp <- varImp(dt_fit)
dtImp
dtImp_polt<-plot(varImp(dt_fit), 20)

#randonforest
set.seed(1111)
rf_fit <- train((price) ~ .,
                data = training,
                method = "rf",
                trControl = fit_control)
rf_fit


rf_y_pred <- predict(rf_fit, testing)
RMSE(y_pred, testing$price)

rfImp <- varImp(rf_fit)
rfImp
rfImp_polt<-plot(varImp(rf_fit), 20)


#xgboost

set.seed(1141)
xgb_fit <- train(
  +   price ~ .,
  +   data = training,
  +   method = "xgbTree",
  +   trControl = fit_control,
  +   importance = TRUE
 )


xgb_y_pred <- predict(xgb_fit, testing)
RMSE(xgb_y_pred, testing$price)
xgbImp <- varImp(xgb_fit)
xgbImp
xgbImp_polt<-plot(varImp(xgb_fit), 20)

models <- list( dt_fit, xgb_fit,rf_fit)
result <- data.frame(model=c('dt_fit','xgb_fit', 'rf_fit' ), RMSE=0, MAE=0, R2=0)

for (i in 1:nrow(result)) {
  model <- models[[i]]
  y_pred <- predict(model, testing)
  result[i, 2] <- RMSE(y_pred, testing[,'price'])
  result[i, 3] <- MAE(y_pred, testing[,'price'])
  result[i, 4] <- R2(y_pred, testing[,'price'])
}
result



# statistical statements about the model performance differences
resamps <- resamples(list(Desion_Tree = dt_fit,
                          Randon_Forest = rf_fit,
                          XGB_regressor = xgb_fit))
resamps
summary(resamps)

#plot the resample summary 
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

#differnce in MAE
difValues <- diff(resamps)
difValues

trellis.par.set(caretTheme())
dotplot(difValues)


trellis.par.set(caretTheme())
plot(xgb_fit)

