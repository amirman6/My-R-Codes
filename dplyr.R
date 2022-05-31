
library(gapminder)
library(dplyr)

df1 = tbl_df(gapminder) # this makes it easy to see the data frame
df1

df=gapminder %>% 
  
  na.omit() %>% 
  filter(year == '2007') %>% 
  arrange(desc(gdpPercap))

df[1:20,]
#plotting to see the corrleration between the gdp and lifeexpectancy
x = log(df$gdpPercap)
y = df$lifeExp
plot(x,y,pch=15,col='blue')


# analysis by continents
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_lifeExp = mean(lifeExp),mean_gdp = mean(gdpPercap),mean_pop=mean(pop),no_of_country=n()/12)

# checking different paramenters by country

life=gapminder %>% 
   
  na.omit() %>% 
  group_by(country) %>% 
  summarise(avg_lifeExp = mean(lifeExp))

life
  
  
pop=gapminder %>% 
  
  na.omit() %>% 
  group_by(country) %>% 
  summarise(avg_pop = mean(pop))
pop
  
 
gdp=gapminder %>% 
  
  na.omit() %>% 
  group_by(country) %>% 
  summarise(avg_gdpPercap = mean(gdpPercap)) 
gdp
# now combining all together
df = data.frame(life$country,pop$avg_pop,gdp$avg_gdpPercap,life$avg_lifeExp)

df1 = df %>% 
  arrange(desc(df[,3]))# sorting by gdp
# OR arrange(desc(gdp$avg_gdpPercap))# sorting by gdp

df1[1:20,]

#Plotting
library(ggplot2)
df1[1:10,] %>% 
  rename(GDP = gdp.avg_gdpPercap) %>% 
  rename(Country = life.country) %>% 
  ggplot(aes(Country,GDP))+geom_bar(stat='identity',fill='lightgreen')

df1[1:10,] %>% 
  rename(LifeExp = life.avg_lifeExp) %>% 
  rename(Country = life.country) %>% 
  ggplot(aes(Country,LifeExp))+geom_bar(stat='identity',fill='lightgreen')

df1[1:10,] %>% 
  rename(Pop = pop.avg_pop) %>% 
  rename(Country = life.country) %>% 
  ggplot(aes(Country,Pop))+geom_bar(stat='identity',fill='lightgreen')
  

# other example
iris %>%
  group_by(Sepal.Length) %>%
  summarise(avg_slength = mean(Sepal.Length),
            min_slength = min(Sepal.Length),
            max_slength = max(Sepal.Length),
            total = n())
iris %>%
  group_by(Sepal.Width) %>%
  summarise(avg_slength = mean(Sepal.Width),
            min_slength = min(Sepal.Width),
            max_slength = max(Sepal.Width),
            total = n())


  
  

