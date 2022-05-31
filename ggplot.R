library(ggplot2)
library(tidyverse)

#View(mpg)
#help(mpg)
# simple quick qplot examples
qplot(displ,hwy,data = mpg,color = manufacturer) # catagoriging by colors
qplot(displ,hwy,data = mpg,shape = drv) # catagoriging by shaping
qplot(displ,hwy,data = mpg,color = drv,facets = .~drv) #separate charts tilted vertically
qplot(displ,hwy,data = mpg,color = drv,facets = drv~.) #separate charts tilted horizontally
qplot(displ,hwy,data = mpg,geom = c('point','smooth')) # draw smooth line with 95% CI
qplot(displ,hwy,data = mpg,geom = c('point','smooth'),method = 'lm') # draw smooth line with linear model
qplot(displ,hwy,data = mpg,geom = c('point','smooth'),color = drv,method = 'lm',facets = .~drv) # draw smooth line with linear model

# ggplot examples

ggplot(mpg,aes(displ,hwy)) + geom_point() # aes is aesthetics
          # OR
ggplot(mpg)+aes(displ,hwy,col=drv) + geom_point(size=4) # aes is aesthetics

ggplot(mpg,aes(displ,hwy)) + geom_point(color='blue')
ggplot(mpg,aes(displ,hwy)) + geom_point(color='blue',size=4)
ggplot(mpg,aes(displ,hwy)) + geom_point(color='blue',size=4,alpha = 0.5) # alpha is tranperancy
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+ labs(title='mpg plot') 
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+ labs(title='mpg plot')+ labs(x='disp of cars',y='hwy of cars') 
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+theme_bw() # theme changes the background theme
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+geom_smooth()
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+geom_smooth(method='lm')
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+geom_smooth(method='lm',size=3,linetype=2,se=FALSE)
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+geom_smooth(method='lm')+facet_grid(.~drv)# splits vertically
ggplot(mpg,aes(displ,hwy)) + geom_point(aes(color=drv))+geom_smooth(method='lm')+facet_grid(drv~.)# splits vertically


# from mtcars data
ggplot(mtcars,aes(disp,mpg,color=as.character(cyl))) + geom_point(size=4)
# since the cyl is numrical data, it's needs to be in catagorical first, so 
# either do cyl=as.factor(cyl) first or use as.character(cyl) in the color
ggplot(mtcars,aes(disp,mpg)) + geom_point(aes(color=cyl),size=4)+geom_smooth()
ggplot(mtcars,aes(log(disp),log(mpg))) + geom_point(aes(color=cyl),size=4)+geom_smooth(method='lm')
ggplot(mtcars,aes(log(disp),log(mpg))) + geom_point(aes(color=cyl),size=4)+geom_smooth(method='lm')+facet_grid(.~cyl)

# Histogram with density plot

ggplot(mtcars, aes(x=mpg)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.1) + geom_vline(aes(xintercept=mean(mtcars$mpg, na.rm=T)),   # Ignore NA values for mean
                                      color="red", linetype="dashed", size=1) 



# histogram plot examples
qplot(hwy,data=mpg)
qplot(hwy,data=mpg,fill= drv)
qplot(hwy,data=mpg,binwidth = 1,fill= drv)# controls the bins
qplot(log(hwy),data=mpg,fill= drv)
qplot(hwy,data=mpg,fill=manufacturer)
qplot(hwy,data=mpg,facets = .~drv,fill=drv)
qplot(hwy,data=mpg,facets = .~drv,fill=manufacturer)
qplot(hwy,data=mpg,geom ='density',fill= drv)
qplot(log(hwy),data=mpg,geom ='density',fill= drv)
qplot(hwy,data=mpg,geom ='density',fill= drv,facets = .~drv)

# histogram with ggplot
set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)
head(df)

ggplot(df, aes(x=weight, fill=sex)) +
  geom_histogram(color='blue')



