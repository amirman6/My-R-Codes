# my own techniques to test the central limit theorem


# or for BETA original distribution
df = rbeta(10000,5,2) # beta distribution, LEFT skewed
hist(df)

d = numeric() # creating an emtpy list to initiate
for(i in 1:1000){
  n = 10000
  s = sample(n,0.1*n,replace=T) # provides 10% of the total n
  df = rbeta(n,5,2) # generates the left skewed data using beta function
  d[i] = mean(df[s]) # mean of left skewed 10% randomly choosen sample 
}
hist(d) # mean of the data sample is almost normal regardless of the original population data


# RIGHT skewed
df = rbeta(10000,2,5) # beta distribution, RIGHT skewed
hist(df)

d = numeric() # creating an emtpy list to initiate
for(i in 1:1000){
  n = 10000
  s = sample(n,0.1*n,replace=T) # provides 10% of the total n
  df = rbeta(n,2,5) # generates the RIGHT skewed data using beta function
  d[i] = mean(df[s]) # mean of  10% randomly choosen sample 
}
hist(d) # mean of the data sample is almost normal regardless of the original population data


# uniform random variable
df = runif(1000, -1, 1)
hist(df)

d = numeric() # creating an emtpy list to initiate
for(i in 1:1000){
  n = 10000
  s = sample(n,0.1*n,replace=T) # provides 10% of the total n
  df = runif(n, -1, 1) # generates the uniform random data using beta function
  d[i] = mean(df[s]) # mean of 10% randomly choosen sample 
}
hist(d)



# Gamma distribution
#generate 1,000 random values that follow gamma distribution
x <- rgamma(n=1000, shape=3, rate=5)
#create histogram to view distribution of values
hist(x)


d = numeric() # creating an emtpy list to initiate
for(i in 1:1000){
  n = 10000
  s = sample(n,0.1*n,replace=T) # provides 10% of the total n
  df = rgamma(n, shape=3, rate=5) # generates the uniform random data using beta function
  d[i] = mean(df[s]) # mean of 10% randomly choosen sample 
}
hist(d)
















