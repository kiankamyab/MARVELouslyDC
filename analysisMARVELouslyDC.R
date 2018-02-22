##
#
# kian kamyab
#
# taking comic and movie sales data for some descriptive analyses
#
##

library(tidyverse)
library(ggthemes)



perCapMovieViews <- movieSales %>%
  group_by(publisher, year) %>%
  summarise(totalGross = sum(gross),
            perCapViews = totalGross/mean(population))

ggplot(perCapMovieViews, aes(x = year, y = perCapViews, group = publisher, color = publisher)) +
  geom_line() +
  theme_dark()

boxoffice(dates = as.Date('2000-01-01'), site = 'numbers')
boxoffice(dates = as.Date('1999-12-01'), site = c('mojo', 'numbers'))



# some analyses -----------------------------------------------------------

head(comicSalesMonthly)
comicSalesMonthly %>%
  summarise(publishers = n_distinct(publisher),
            title = n_distinct(title)) 

comicSalesMonthly %>%
  filter(publisher %in% c('Marvel', 'DC')) %>%
  summarise(publishers = n_distinct(publisher),
            title = n_distinct(title))

head(comicSalesMonthly)