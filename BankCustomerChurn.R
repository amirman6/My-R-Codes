# bank customer churn
library(mlbench)
library(caret)
library(e1071)
library(lime)

# carprice data analysis

df = read.csv(choose.files()) # load bankcustomer_churn data
head(df)
str(df)
apply(is.na(df),2,sum)

df$Geography = as.factor(df$Geography) # needs as a factor variable for one hot encoding which R will do automatically
df$Gender = as.factor(df$Gender)
df$Exited = as.factor(df$Exited) # As it is a classification problem
df = df[,-c(1,2)]
table(df$Exited) # checking the class imbalance problem


# one hot encoding on factor variables, it needs caret package, R will automatically does hot coding for factor variables
#dummy <- dummyVars(" ~ .", data=df)
#df <- data.frame(predict(dummy, newdata=df))

# normalizing
# scaling between 0 to 1, may not need to scale them
normFunc <- function(x){
  (x-min(x))/(max(x)-min(x))
}

df[,-c(2,3,11)] = lapply(df[,-c(2,3,11)],normFunc)


# data partition
set.seed(1234) 
ind <- sample(2, nrow(df), replace = T, prob = c(0.7, 0.3))
train <- df[ind == 1,]
test <- df[ind == 2,]

# work on class imbalance problem
table(train$Exited)
prob.table(train$Exited)

library(ROSE) 
# under sampling
over <- ovun.sample(Exited~., data = train, method = "over", N = 11212)$data  # N = 2* the higher sample, so the total of 0 adn 1 = 446
table(over$Exited)
summary(over)

# or undersampling
under <- ovun.sample(Exited~., data=train, method = "under", N = 2870)$data  #N = 2* lower sample 
table(under$Exited)



set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)


# RF model
set.seed(1234)
forest <- train(Exited ~ ., 
                data=over, # or train in general
                method="rf",
                #trControl=cvcontrol,
                importance=TRUE)

plot(varImp(forest))

# saving the model for later use
saveRDS(forest,"rf_model_churn.rds") # check which directory it is saved
# if you use the saved file
rf_read = readRDS("rf_model_churn.rds")
rf <-  predict(rf_read,test) # if you use the saved file

# model evaluation
rf = predict(forest,test)
confusionMatrix(rf,test$Exited)



# boost model
set.seed(1234)
boo <- train(price ~ ., 
             data=train,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = c(1,2,3,4),
                                    eta = c(0.1,0.2,0.3,0.4),
                                    gamma = c(1,2,3,4),
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))

# saving the model for later use
saveRDS(boo,"boo_model.rds") # check which directory it is saved
boo_read = readRDS("rf_model.rds")
bo <-  predict(boo_read,  test) # if you use the saved file

# model evaluation
bo <-  predict(boo,  test) # if you use the saved file
confusionMatrix(bo,test$Exited)




# custom, out of bag data testing
rf_read = readRDS("rf_model_churn.rds") # see where it is saved, run getwd()
my_test = data.frame(CreditScore=706,Geography='Spain',Gender='Male',Age=57,Tenure=7,Balance=0,
                     NumOfProducts =1, HasCrCard=1,IsActiveMember=0,EstimatedSalary=17941)
predict(rf_read,my_test)














