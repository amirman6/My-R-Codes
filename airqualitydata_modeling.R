
# built in data
air = airquality

#filling na values with mean
air$Ozone[is.na(air$Ozone)]=mean(airquality$Ozone,na.rm = TRUE)
air$Solar.R[is.na(air$Solar.R)]=mean(airquality$Solar.R,na.rm = TRUE)

# normalizing function

#normalize = function(x){
  #return((x-min(x))/(max(x)-min(x)))}

#nor_air = normalize(air)
X1 = air$Solar.R
Y1 = air$Ozone

#linear model and plotting
model1 = lm(Y1 ~ X1)
plot(X1,Y1,xlab = 'Solar Radiation', ylab = 'Ozone')
abline(model1,lwd = 3,col = 'red')

plot(model1) #see the multiple plots, keep pressing mouse on the graph 
summery(model1)


X2 = air$Wind
Y2 = air$Ozone

model2 = lm(Y2 ~ X2)
plot(X2,Y2,xlab = 'wind', ylab =  'Ozone')
abline(model2,lwd = 3,col = 'blue')

# see the effect of wind on ozone vs solar.R
coplot(Ozone ~ Solar.R|Wind, panel = panel.smooth, airquality)
# or ggplot
library(ggplot2)
# how ozone varies with solar radiation for particular wind
ggplot(air, aes(x=Solar.R, y=Ozone,col=Wind,size=4))+
  geom_point()

# how ozone varies with wind for particular solar radiation
ggplot(air, aes(x=Wind, y=Ozone,col=Solar.R,size=4))+
  geom_point()


# new linear model with interaction terms between wind and solar radiation
model3 = lm(Ozone ~ Solar.R*Wind, air)
#model3 = lm(Ozone ~ Solar.R + Wind, air) # try this too
plot(model3) #see the multiple plots, keep pressing mouse on the graph 
summary(model3)
termplot(model3)


solar1 = mean(X1,na.rm = TRUE)
solar2 = 100 # about the fist quantile
solar3 = 300 # about the 3rd quantile


p1 = predict(model3, data.frame(Solar.R = solar1, Wind = 1:20))
p2 = predict(model3, data.frame(Solar.R = solar2, Wind = 1:20))
p3 = predict(model3, data.frame(Solar.R = solar3, Wind = 1:20))

plot(Ozone ~ Wind, air, main = 'Ozone vs Wind speed linear model @ 3 different solar radiation')
lines(1:20,p1,col='blue',lwd =3) # ozone vs wind for mean solar radiation ~185
lines(1:20,p2,col='red',lwd = 3)  # ozone vs wind for  solar radiation  = 100
lines(1:20,p3,col='black',lwd = 3) # ozone vs wind for  solar radiation  = 300

# now try Ozone ~ Solar.R at three different wind speeds
wind1 = mean(airquality$Wind,na.rm = TRUE)
wind2 = 7 # about the fist quantile
wind3 = 12 # about the 3rd quantile


p1 = predict(model3, data.frame(Wind = wind1, Solar.R = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)))
p2 = predict(model3, data.frame(Wind = wind2, Solar.R = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)))
p3 = predict(model3, data.frame(Wind = wind3, Solar.R = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)))

a = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)
b = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)
c = c(5,10,50,75,100,125,150,175,200,225,250,275,300,325,350)


plot(Ozone ~ Solar.R, airquality, main = 'Ozone vs Solar Ratiation linear model @ 3 different wind speeds')
lines(a,p1,col='blue',lwd =3) # ozone vs solar radiation for mean wind speed ~ 9.9
lines(b,p2,col='red',lwd = 3)  # ozone vs solar radiation for mean wind speed =  7
lines(c,p3,col='black',lwd = 3) # ozone vs solar radiation for mean wind speed  = 12


# work on glm and gls models, check the video
model = glm(Ozone ~ Wind, airquality, family = poisson)
# glm model fits as follows
# logy = ax + b + error
#y = exp(ax+b) + error = exp(ax) .exp(b)
coef(model) # run this 

ozone1.glm = exp(coef(model)[1] + coef(model)[2]*19)
ozone2.glm = exp(coef(model)[1] + coef(model)[2]*20)

ozone2.glm/ozone1.glm # run this, you get 0.86167

exp(coef(model)[2]) # its a exp(slope), also returns 0.86167

# in glm, an individual slope gives an estimate of the multiplicative change in the response variable
# for a one unit change in the corresponding explanatory variable
# means for a one unit change in wind speed, Ozone concentration decreases by exp(-0.1488) fold


# do the predictive modeling, first train and test the model prediction for Ozen concentration for the dataset without
# Nan's. AND use that model to fill the Nan for Ozone as a test data.

library(caret)
air=airquality

str(air)
df=air[!is.na(air$Ozone),]
head(df)
apply(is.na(df),2,sum)
df$Solar.R[is.na(df$Solar.R)] = mean(df$Solar.R,na.rm=TRUE)
str(df)

df = data.frame(df[,-6]) # day column removed

# Data Partition
set.seed(123)
ind <- sample(2, nrow(df), replace = T, prob = c(0.70, 0.30))
train <- df[ind==1,]
test <- df[ind==2,]

set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)

set.seed(1234)
forest <- train(Ozone ~ ., 
                data=train,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)

forest 
plot(varImp(forest))

rf <-  predict(forest,  test)
plot(rf ~ test$Ozone, main = 'Predicted Vs Actual Ozone - Test data')
sqrt(mean((test$Ozone - rf)^2)) # RMSE = 14.2
cor(test$Ozone, rf) ^2 # 79%


# Boosting
set.seed(1234)# takes about 18 min 
boo <- train(Ozone ~ ., 
             data=train,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = c(400,500,600),
                                    max_depth = c(1,2,3,4,5),
                                    eta = c(0.02,0.03,0.04,0.05,0.08,0.1),
                                    gamma = c(3.1,3.5,4.1,4.5,5.1,6.1),
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))


boo
plot(varImp(boo)) 

bo <-  predict(boo,  test)
plot(bo ~ test$Ozone, main = 'Predicted Vs Actual Ozone - Test data')
sqrt(mean((test$Ozone - bo)^2)) # RMSE = 16.2
cor(test$Ozone, bo) ^2 # 74.6%

# now use this trained model to fill the nan values on the original data
df_test = air[is.na(air$Ozone),] # 37 data
head(df_test)
df_test = df_test[,-c(1,6)]
df_test$Solar.R[is.na(df_test$Solar.R)] = mean(df_test$Solar.R,na.rm=TRUE)
apply(is.na(df_test),2,sum)


Ozone = predict(forest,df_test)

df_test = data.frame(Ozone,df_test)

# binding df (whthout nan, original data) and df_test
air_new = rbind(df, df_test)
head(air_new)
str(air_new)
apply(is.na(air_new),2,sum)

# now use this new data to do the beginning analysis







