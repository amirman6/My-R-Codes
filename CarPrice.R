library(mlbench)
library(caret)
library(e1071)
library(lime)

# carprice data analysis

df = read.csv(choose.files()) # load all_cardataset in 'data' folder under 'usedcardataset'
head(df)
str(df)
apply(is.na(df),2,sum) # finding the sum of the nan values for each columns
# Replacing nan values with the mean of the particular column
# df$x[is.na(df$x)]<-mean(df$x,na.rm=TRUE)

# data pre-preparation
# label encoding for the model, as it has lots of model, one hot encoding will create lots of columns
#df$model = as.factor(df$model) # label encoding only works for factor variables not char variable

df = df[,-1]
df$transmission = as.factor(df$transmission)
df$make = as.factor(df$make)
df$fuelType = as.factor(df$fuelType)

# to recognize how levels are assigned for each, levels are created as seperate columns
df['model_n'] = lapply(df['model'],unclass)
df$model = as.character(df$model) # for paste function, it needs a chr
df['model_level'] = paste(df$model,df$model_n)
df['make_n'] = lapply(df['make'],unclass)
df$make = as.character(df$make) # for paste function, it needs a chr
df['make_level'] = paste(df$make,df$make_n)
df['year'] = 2021-df$year  # how old is it
df['transmission_n'] = lapply(df['transmission'],unclass)
df$transmission = as.character(df$transmission) # for paste function, it needs a chr
df['transmission_level'] = paste(df$transmission , df$transmission_n)
df['fuelType_n'] = lapply(df['fuelType'],unclass)
df$fuelType = as.character(df$fuelType) # for paste function, it needs a chr
df['fuelType_level'] = paste(df$fuelType , df$fuelType_n)

str(df)





# scaling between 0 to 1, may not need to scale them
normFunc <- function(x){
  (x-min(x))/(max(x)-min(x))
  }

df[,-4] = lapply(df[,-4],normFunc) # except the price column
head(df)
summary(df)
# writing the new file for later use
write.csv(df,file = "carprice.csv")



# data partition
df_car = df[,c('model_n','make_n','year','price','transmission_n','mileage','fuelType_n','tax','mpg','engineSize')]
set.seed(1234) 
ind <- sample(2, nrow(df_car), replace = T, prob = c(0.7, 0.3))
train <- df_car[ind == 1,]
test <- df_car[ind == 2,]

set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)


# RF, takes about 7 hrs with cv=5 and repeats 1
set.seed(1234)
forest <- train(price ~ model_n + make_n + year + transmission_n + mileage + fuelType_n + tax + mpg + engineSize, 
                data=train,
                method="rf",
                #trControl=cvcontrol,
                importance=TRUE)

# saving the model for later use
saveRDS(forest,"rf_model.rds") # check which directory it is saved


plot(varImp(forest))


# Plot, RMSE, R-square
rf <-  predict(forest,  test)
# if you use the saved file
rf_read = readRDS("rf_model.rds")
rf <-  predict(rf_read,  test) # if you use the saved file

plot(rf ~ test$price, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$price - rf)^2)) # RSME, about 1793
cor(test$price, rf) ^2 #R-Square, about 96.7%

# Explain predictions
explainer <- lime(test[1:3,], forest, n_bins = 5)
explanation <- explain( x = test[1:3,], 
                        explainer = explainer, 
                        n_features = 5)
plot_features(explanation)



# finiding which no are level encoded run this
unique(df$make_level)
unique(df$transmission_level)
unique(df$fuelType_level)
# model for each make  finder
modelfinder = function (x){
  df = df[df$make == x,]
  name = unique(df$model_level)
  return(name)

# finding the corresponding value to the other column for loop
n=length(df[,1])
finder = function(x){
    
    for (i in 1:n){
      if(df$model[i]== x){
        break
      }
    }
    return(df$tax[i])
  }
}


# prediction function
car_price = function(){
  model = readline(prompt = 'model:')
  make =readline(prompt = 'make:')
  year =readline(prompt = 'year(old):')
  transmission = readline(prompt = 'transmission:')
  mileage = readline(prompt = 'mileage(km):')
  fuelType = readline(prompt = 'FuelType:')
  tax = readline(prompt = 'Tax:')
  mpg = readline(prompt = 'mpg(km/l):')
  EngineSize = readline(prompt = 'EngineSize:')
  
  model = as.numeric(model)
  make = as.numeric(make)
  year = as.numeric(year)
  transmission = as.numeric(transmission)
  mileage = as.numeric(mileage)
  fuelType = as.numeric(fuelType)
  tax = as.numeric(tax)
  mpg = as.numeric(mpg)
  EngineSize = as.numeric(EngineSize)
  
  p = data.frame(model_n=model, make_n=make, year=year,transmission_n=transmission,mileage=mileage,fuelType_n=fuelType,
                 tax=tax,mpg=mpg,engineSize=EngineSize)
  
  fit = readRDS("rf_model.rds") # first make sure this saved file is on the directory
  pr = predict(fit,p) # does not return the confidence band with rpart model
  return(pr)
}
car_price()




# XG Boosting
set.seed(1234)
boo <- train(price ~ model_n + make_n + year + transmission_n + mileage + fuelType_n + tax + mpg + engineSize, 
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

# saving the model for later use
saveRDS(boo,"boo_model.rds") # check which directory it is saved

plot(varImp(boo))

# Plot, RMSE, R-square
bo <-  predict(boo,  test)
# if you read the saved file, first read it
boo_read = readRDS("boo_model.rds")
rf <-  predict(boo_read,  test) # if you use the saved file
# if you use the saved file, first read it

plot(bo ~ test$price, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$price - bo)^2))
cor(test$price, bo) ^2









