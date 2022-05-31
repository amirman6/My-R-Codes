library(dplyr)

bike = read.csv(choose.files()) #bike_buyers file

colnames(bike) # to see the name of each columns
head(bike)
str(bike)  # structure of data
summary(bike)

bike = select(bike,-ï..ID) # since ID has no meaningful data, it is excluded using dplyr
bike_purchased = filter(bike,bike$Purchased.Bike == 'Yes')
bike_notpurchased = filter(bike,bike$Purchased.Bike == 'No')

# examine the univariate data
# how many bought the bike and not purchased it summary
summary(bike$Purchased.Bike)
plot(bike$Purchased.Bike)
# about income
summary(bike_purchased$Income)
boxplot(bike_purchased$Income)
hist(bike_purchased$Income)
plot(density(bike_purchased$Income))
# we can study the same who did not buy the bicycle
# ?density # if you want to know more attributes about any function
# about education
summary(bike_purchased$Education)
plot(bike_purchased$Education)

# about marital status who purchased the bike
bike_purchased = filter(bike,bike$Purchased.Bike == 'Yes')
summary(bike_purchased$Marital.Status)
plot(bike_purchased$Marital.Status)
pie(table(bike_purchased$Marital.Status),main = 'married vs single buyers')

# about children
summary(bike_purchased$Children)
bike_purchased$Children = factor(bike_purchased$Children) # as factor for plotting a bar graph
plot(bike_purchased$Children, main = 'bike purchased yes, children no')

#commute distance
summary(bike_purchased$Commute.Distance)
plot(bike_purchased$Commute.Distance)

#examine the multivariate data
# relationship between education and income
?by
by(bike$Income, bike$Education,summary)
by(bike$Income,bike$Education,mean)
#boxplot
boxplot(bike$Income ~ bike$Education,col=rainbow(7),main='education and income distribution')


?sm
library(sm)
?sm.density.compare
sm.density.compare(bike$Income,bike$Education,xlab = 'Income')
levels(bike$Education) # checking the levels and it should be in the same order below when you create a levels in the legend
education_legend = factor(bike$Education,labels = c('bachelors','graduate degree','high school','partial college','partialHighschool'))
#colfill = c(1:5) # because it has 5 levels of catagories
#colfill = c(2:(2+length(levels(education_legend))))
legend(locator(1),levels(education_legend),fill = rainbow(5))
# it will prompt you put your cursor on the plot where you want your legends

#bought a bike vs education
?xtabs
xtabs(~Education+Purchased.Bike, bike)
a = xtabs(~Education+Purchased.Bike, bike)
plot(a)

# bought a bike vs occupation
xtabs(~Occupation+Purchased.Bike, bike)
plot(xtabs(~Occupation+Purchased.Bike, bike)) 

# bought a bike vs commute distance
xtabs(~Commute.Distance+Purchased.Bike, bike)
plot(xtabs(~Commute.Distance+Purchased.Bike, bike))

# bought a bike vs region
xtabs(~Region+Purchased.Bike, bike)
plot(xtabs(~Region+Purchased.Bike, bike))

# drill down on the details with ChiAquare test
library(gmodels)
?CrossTable
CrossTable(bike$Education,bike$Purchased.Bike,chisq = TRUE,prop.t = F) # read more about chi-square and prop-t
CrossTable(bike$Occupation,bike$Purchased.Bike,chisq = TRUE,prop.t = F)
CrossTable(bike$Commute.Distance,bike$Purchased.Bike,chisq = TRUE,prop.t = F)






