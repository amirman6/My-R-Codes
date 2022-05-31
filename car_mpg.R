library(mlbench)
library(caret)
library(e1071)
library(lime)

# car MPG prediction
df = read.csv(choose.files()) # load vehicle_details in 'data' folder, it has about 38k data 
head(df)
str(df)
apply(is.na(df),2,sum)
# nan filling
df$Engine.Cylinders[is.na(df$Engine.Cylinders)]<-mean(df$Engine.Cylinders,na.rm=TRUE)
df$Engine.Displacement[is.na(df$Engine.Displacement)]<-mean(df$Engine.Displacement,na.rm=TRUE)
df = df[-c(3,5,8,9)]
colnames(df)[6] = "mpg"

df$Make = as.factor(df$Make)
df$Class = as.factor(df$Class)
df$Transmission = as.factor(df$Transmission)

# label encoding, does not need this, keep them as factors, which is faster to run the model
#df['Make'] = lapply(df['Make'],unclass) # may not need to convert into numbers using label encoding, factor will work
#df['Class'] = lapply(df['Class'],unclass)
#df['Transmission'] = lapply(df['Transmission'],unclass)
head(df)

# normalizing
normFunc <- function(x){
  (x-min(x))/(max(x)-min(x))
}

df[,-6] = lapply(df[,-6],normFunc) # except the price column
head(df)
summary(df)




set.seed(1234) 
ind <- sample(2, nrow(df), replace = T, prob = c(0.7, 0.3))
train <- df[ind == 1,]
test <- df[ind == 2,]


set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
    
                          
                                                
# RF, took about 3.5 hrs to run whereas kaggle GPU takes less than one hour even with repeated cvcontrol. So run gpu and save the 
# model file on pc
set.seed(1234)
forest <- train(mpg ~ ., 
                data=train,
                method="rf",
                #trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest))


# saving the model for later use
saveRDS(forest,"rf_CarMpg_model.rds") # check which directory it is saved

print(rf)
# Plot, RMSE, R-square
rf <-  predict(forest,  test)
# if you use the saved file, first read it
rf_read = readRDS("rf_CarMpg_model.rds")
rf = predict(rf_read, test) # if you have saved the model for later use

plot(rf ~ test$mpg, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$mpg - rf)^2)) # RMSE = 0.35
cor(test$mpg, rf) ^2 # R-square, about 99.7 for car mpg prediction

# Explain predictions
explainer <- lime(test[1:3,], forest, n_bins = 5)
explanation <- explain( x = test[1:3,], 
                        explainer = explainer, 
                        n_features = 5)
plot_features(explanation)








# Boosting
set.seed(1234)
boo <- train(mpg ~ ., 
             data=train,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = 3,
                                    eta = 0.2,
                                    gamma = 2.1,
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))
plot(varImp(boo))

# Plot, RMSE, R-square
bo <-  predict(boo,  test)
plot(bo ~ test$mpg, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$mpg - bo)^2))
cor(test$mpg, bo) ^2







# DATA = mpg_data

# car MPG prediction with little data
df = read.csv(choose.files()) # load mpg_data file in 'data' folder, it has  399 data 
head(df)
str(df)
# label encoding, does not need to convert into number
df = df[,-9]
#df$model.year = as.factor(df$model.year)
df$origin = as.factor(df$origin)
df$cylinders = as.factor(df$cylinders)
#df['car.name'] = lapply(df['car.name'],unclass) # don't need to label encode, factor will work in R

apply(is.na(df),2,sum)
df$horsepower[is.na(df$horsepower)]<-mean(df$horsepower,na.rm=TRUE)

#df = df[-c(7,8,9)]


# check the metrics with and without normalizing, normalizing has no effects in improving the accuracy
normFunc <- function(x){
  (x-min(x))/(max(x)-min(x))
}

df[,-1] = lapply(df[,-1],normFunc) # except the mpg column
head(df)
summary(df)


# some EDA
library(psych)
pairs.panels(df)
hist(df$mpg)
boxplot(df)



set.seed(1234) 
ind <- sample(2, nrow(df), replace = T, prob = c(0.7, 0.3))
train <- df[ind == 1,]
test <- df[ind == 2,]

set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
# RF, took less than a minute, takes slightly more than a minute keeping the car.name as a factor variable
set.seed(1234)
forest <- train(mpg ~ ., 
                data=train,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest))

# Plot, RMSE, R-square
rf <-  predict(forest,  test)
plot(rf ~ test$mpg, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$mpg - rf)^2)) # RMSE = 4.2 with the first 6 variables and 2.8 with all the variables
cor(test$mpg, rf) ^2 # R-square, about 72.5% with the the first 6 variables and 87.8% with all the variables


# Explain predictions
explainer <- lime(test[1:3,], forest, n_bins = 5)
explanation <- explain( x = test[1:3,], 
                        explainer = explainer, 
                        n_features = 5)
plot_features(explanation)




# testing with a out of bag independent/custom data
d_new = data.frame(cylinders='8',displacement=350,horsepower=160,weight=3700,acceleration=11.5,
                   model.year = 70,origin = '1', car.name = 'buick skylark 320')
p = predict(forest,d_new)
print(paste("mpg is :",p,"miles/gallon")) # looks predicting within 95% error





# Boosting

set.seed(1234)# takes about 14 min 
boo <- train(mpg ~ ., 
             data=train,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 350,  # best parameters so far
                                    max_depth = 4,
                                    eta = 0.1,
                                    gamma = 4,
                                    colsample_bytree = 1,
                                    min_child_weight = 0.5,
                                    subsample = 0.45))

# saving the model for later use
saveRDS(boo,"boo_CarMpgSmallData.rds") # check which directory it is saved


# or for AUTO tuning of all the parameters
# XG Boosting
set.seed(1234)
boo <- train(mpg ~ ., 
             data=train,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneLength = 3) # will tune 3 parameters for each



boo
plot(varImp(boo))

# Plot, RMSE, R-square
boo_read = readRDS("boo_CarMpgSmallData.rds")

bo <-  predict(boo,  test)
plot(bo ~ test$mpg, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$mpg - bo)^2)) # RMSE =  2.76 with all the variables, with fine tune
cor(test$mpg, bo) ^2   # R-square, about 88% for the first 6 variables




d_new = data.frame(cylinders=6,displacement=350,horsepower=200,weight=3700,acceleration=10,
                   model.year = 80,origin = '1')
boo_read = readRDS("boo_CarMpgSmallData.rds")
p = predict(boo_read,d_new)
print(paste("mpg is :",p,"miles/gallon")) # looks predicting within 95% error



