library("dplyr")
library("tidyr")
suicide_rate <- read.csv("master.csv")
head(suicide_rate)
suicide <-  suicide_rate %>% 
rename(Country = `ï..Country`) %>%
as.data.frame()
View(suicide)
str(suicide)
# 1.What are the strong predictors to determine or predict number of suicides in a country?
suicide_prdict<- lm(suicides.100k.pop ~ Country + year + sex + age + suicides_no + population + suicides_no + country.year + HDI.for.year + gdp_for_year
+ gdp_per_capita+ generation, data = suicide)
summary(suicide_prdict)

lm1 <- lm(suicides.100k.pop ~ population,data = suicide)
lm1
summary(lm1)
population_data <- data.frame(population=c(3129000,3080,2897000,2187600))
predict(lm1,newdata=population_data)
#2. What is the relationship between a countries gdp and suicides?

library("ggplot2")
ggplot(suicide,aes(gdp_for_year,suicides.100k.pop))+
  geom_point(aes(alpha=0.01))+
  geom_jitter()+
  geom_smooth(method = "lm",color = "red")


#3.	For which sex (Male or Female) are the suicides more in a particular  year or generation.

#Year
ggplot(suicide, aes(x = year, y = suicides.100k.pop, colour = sex)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1985, 2016, by = 2)) +
  facet_grid( ~ sex)
#Generation
ggplot(suicide, aes(x = generation, y = suicides.100k.pop, colour = sex)) +
  geom_line() +
  facet_grid( ~ sex)

#4. What is the year in which suicides occurred more globally?
suicides_by_year <- suicide %>% 
  group_by(year) %>% 
  summarise(sum_suicide_no = sum(suicides_no))
View(suicides_by_year)
max1 <- max(suicides_by_year$sum_suicide_no)
max1
which(suicides_by_year$sum_suicide_no==256119)

   #5. At what age in a particular country has the highest suicides.
ggplot(suicide, aes(x = suicides_no, y = Country, colour = age)) +
  geom_line() +
  facet_grid( ~ age)



#6. Is there any decrease or increase in number of suicides in the years?
suicides_in_years<-suicide %>% 
  group_by(year,sex) %>%
  
  summarise(
    population = sum(population), 
    suicides = sum(suicides_no), 
    suicides_per_100k = (suicides / population) * 100000
  )

ggplot(suicides_in_years,aes(year,suicides_per_100k,color=sex),)+
  geom_line(size=1)+
  geom_point(size=2)+
  facet_wrap(~sex,nrow = 2,scales = 'free')+
  labs(title = "Changes in suicides", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  scale_x_continuous(breaks = seq(1985, 2016, 2))




ggplot(suicide,aes(year,suicides.100k.pop,color=age),)+
  geom_line(size=1)+
  geom_point(size=2)+
  facet_wrap(~age,nrow = 2,scales = 'free')+
  labs(title = "Changes in suicides", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "age") + 
  scale_x_continuous(breaks = seq(1985, 2016, 10))










