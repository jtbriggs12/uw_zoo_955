# dplyr/tidyverse homework
library(tidyverse)

# Outline
# 1. filter
# 2. mutate
# 3. summarize
# 4. group_by
# 5. pipes
# 6. arrange/select/count
# 7. joins


# 1. filter
# filter iris data by each species 
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

# 2. mutate
# create 2 new columns in iris data that is petal/sepal length x width
iris_area <- mutate(
  iris, 
  petal.area = Petal.Length * Petal.Width,
  sepal.area = Sepal.Length * Sepal.Width
)

# plot petal area ~ length - is the relationship linear? Why?
ggplot(data = iris_area, aes(x = Petal.Length, y = petal.area)) + geom_point()
#Not a linear relationship, 0-2 petal length has one relationship, 3-7 has another. Straight line does not describe this relationship well.


# 3. summarize
# compute the mean petal length of each species dataset from above
setosa_mean <- mean(setosa$Petal.Length)
versicolor_mean <- mean(versicolor$Petal.Length)
virginica_mean <- mean(virginica$Petal.Length)

# now do it using summarize
setosa_summarize_mean <- summarize(setosa, mean.petal.length = mean(Petal.Length))
versicolor_summarize_mean <- summarize(versicolor, mean.petal.length = mean(Petal.Length))
virginica_summarize_mean <- summarize(virginica, mean.petal.length = mean(Petal.Length))

# 4. group by
# we can do the above summarize so much easier when combined with group_by
iris_means <- summarize(group_by(iris, Species), mn.petal.length = mean(Petal.Length))

# 5. pipes
# the above can get unwieldy - rearrange iris_means from 4 using pipes
iris_means_pipe <- iris %>%
  group_by(Species) %>%
  summarize(mn.petal.length = mean(Petal.Length))

## On Your Own #1 
# now compute mean petal area for each species - how would you go about it using dplyr
petal.area <- iris %>%
  mutate(petal.area = Petal.Length * Petal.Width) %>%
  group_by(Species) %>%
  summarize(mn.petal.area = mean(petal.area))

# Q: What is the mean petal area for each species
#setosa = 0.36
#versicolor = 5.72
#virginica = 11.29

# 6. arrange/select/count
# determine which species has the longest mean petal length
iris_size <- 
  iris %>% 
  select(Species, Petal.Length) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.petal.length = mean(Petal.Length)) %>%
  arrange(desc(mn.petal.length))

# On Your Own #2
# do the same for the other measurements (i.e. petal.width, sepal.length, etc)
iris_means <- iris %>%
  group_by(Species) %>%
  summarize(mn.petal.length = mean(Petal.Length),
            mn.petal.width = mean(Petal.Width),
            mn.sepal.length = mean(Sepal.Length),
            mn.sepal.width = mean(Sepal.Width)) %>%
  arrange(desc(mn.petal.length))
# Q: What is the mean petal and sepal lengths and widths for each species
iris_means

# count the number of records for each species
(iris_spp_n <- count(iris, Species))

# On Your Own #3
# count the number of samples that are >= mean.petal.length for each species
virginica.means <- iris_means[1,]
iris %>% 
  filter(Species == 'virginica',
         Petal.Length >= virginica.means$mn.petal.length) %>%
  count()

# Q: How many samples where Petal.Length >= mean.petal.length does each species have
#setosa = 26
#versicolor = 27
#virginica = 25


# 7. joins
set.seed(123)
ht <- data.frame(level = LETTERS[1:5], height = sample(40:80, 5, replace = TRUE))
wt <- data.frame(level = LETTERS[1:6], weight = sample(50:500, 6, replace = TRUE))

# join together height and weight by level
# what happens when you reverse ht and wt (i.e. put wt first, then ht)
ht_wt <- left_join(ht, wt, by = "level")
#left_join only includes the rows in the second listed dataframe

# On Your Own #4 - Extra Credit
# work with the nycflights13 data set to determine what airport had the 
#     most departing flights in 2013
# must use combination of dplyr verbs
# data.frames are airports, flights, planes and weather
# HINT: faa column in airports links to origin column in flights
library(nycflights13)
data(airports)
data(flights)
data(planes)
data(weather)

# Q: Which airport (name) had the greatest number of arriving flights in 2013? 
flights %>%
  filter(year == '2013') %>%
  group_by(dest) %>%
  count() %>%
  arrange(desc(n))

airports %>%
  filter(faa == "ORD")

#Chicago O'Hare

# Q: Which airport (name) had the greatest number of delayed arriving flights?
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  count() %>%
  arrange(desc(n))

airports %>%
  filter(faa == "ATL")
#Hartsfield Jackson Atlanta

# Q: What is the manufacturer, model, year, and type of airplane that flew the 
#    most flights in 2013 (only include planes with all relevant information)?
#    How many flights was it?

planeflights <- left_join(flights, planes, level = 'tailnum')

planeflights %>% 
  filter(!is.na(manufacturer),
         !is.na(model),
         !is.na(year),
         !is.na(type)) %>%
  group_by(tailnum) %>%
  count() %>%
  arrange(desc(n))

planes %>%
  filter(tailnum == 'N354JB')
