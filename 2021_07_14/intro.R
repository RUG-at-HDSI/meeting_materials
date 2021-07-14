
# Introduction to R 

x <- 1 
y <- 2 

str <- 'hello'
str2 <- "world"

print(str)
print(str2)

a <- TRUE
b <- FALSE

!b 

a | b 
a & b 

c <- NaN
d <- NA
e <- NULL 

class(x)
class(c)

typeof(x)
typeof(c)

?class 

?`<-` 
?`+`

# simple arithmetic

x + y 

x * y 

# vectors 

x <- c(1,2,3,4)
y <- c(5,6,7,8)

?c

x - y 
x * y 

length(x)

x[4]

?`[`

x[4] <- 5

str_vector <- c('a','b', 'c')


# lists

l <- list(a,b,c,d,e)
length(l)

l[1]
class(l[1])

l[[1]]
class(l[[1]])

# factors 

f <- factor(c('a', 'a', 'b', 'c', 'b', 'a'))
f
levels(f)

# simple vector functions 

sum(x)

mean(x)

sd(x)

var(x)

abs(x)

min(x)

max(x)

?sum

# declaring functions 

avg_absolute_diff <- function(x, y) { 
  mean(abs(x-y)) 
}

avg_absolute_diff(x, y)
avg_absolute_diff(c(1,2), c(5, 7))

say_hi <- function(str = 'hi') {
  print(str)
}

say_hi()
say_hi('bye')
say_hi(str = 'gotta go!')

# if-else 

if (min(x) == 1) {
  print('hi')
} else {
  print('bye')
}

# loops 

for (i in 1:10) {
  print(i)
}

# functional programming 

z <- sapply(1:10, function(x) { x * 2 }) 


# using real data 

?iris 
?mtcars
?rivers
?volcano

View(iris)
View(mtcars)

?volcano


# access data from inside a data.frame

iris[,'Sepal.Length']
iris[1,]

iris[1,'Sepal.Width']



# plotting & visualizing  -- base R 

plot(rivers)
hist(rivers)


# example plot volcanos 

require(grDevices); require(graphics)
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")

# introducing ggplot2

library(ggplot2)

ggplot(mtcars, aes(x = cyl, y = hp)) + 
  geom_point(position = position_jitter(width=0.25)) + 
  geom_boxplot(aes(group = cyl), alpha=0.5)


# using linear models 

model <- lm(hp ~ cyl, mtcars)

summary(model)

# using broom 
library(broom)

tidy(model)

ggplot(
  tidy(model),
  aes(
    y = term,
    x = estimate,
    yend = term,
    xend = estimate + 1.96 * std.error)) +
  geom_segment(aes(x = estimate - 1.96 * std.error)) + 
  geom_point() + 
  ggtitle("Regression Coefficient Estimates") 


# using a CSV dataset 

df <- read.csv('women_owned_businesses.csv')
dim(df)


# clean column names
library(janitor)
df <- clean_names(df)


library(dplyr)

x %>% min() 

df <- df %>% filter(business_name != 'Not yet')

df <- df %>% mutate(
  business_zipcode = ifelse(nchar(business_zipcode) == 4,
                            paste0('0', business_zipcode),
                            business_zipcode))

zip_counts <- df %>% 
  group_by(business_zipcode) %>% 
  summarize(value = n()) %>% 
  arrange(-value)


# https://ctesta.com/articles/2018-05/getting-started-in-r

# readxl   - readxl.tidyverse.org
# haven    - haven.tidyverse.org
# dplyr    - dplyr.tidyverse.org


