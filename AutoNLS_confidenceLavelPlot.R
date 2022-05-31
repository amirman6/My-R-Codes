require(propagate)

x = seq(0,50,1) # generates the numbers from 0 to 50 with an increment of 1
# model equation with some normal noise
y = ((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)

model = nls(y ~ A*(1-exp(-x/tau)),start = list(A=40,tau = 150) )
pred_model = predictNLS(model,newdata = data.frame(x))
conf_model = pred_model$summary
#conf_model # prints the summary of the fitted data

plot(x,y,pch=15,col='red')
lines(x,conf_model$Sim.Mean,lwd = 3)
lines(x,conf_model$`Sim.2.5%`,lty=2,col='red')
lines(x,conf_model$`Sim.97.5%`,lty=2,col='blue')

# single value prediction
predictNLS(model, newdata = data.frame(x=16.5))
