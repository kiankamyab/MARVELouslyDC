##
#
# kian kamyab
#
# taking comic and movie sales data for some descriptive analyses
#
##


# loading libraries and data ----------------------------------------------

library(tidyverse)
library(ggthemes)
library(scales)

movies <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/movieSalesPop.csv', stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date, '%Y-%m-%d'))
str(movies)

comicsMonthly <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly.csv', stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date, '%Y-%m-%d'))

comicsAnnual <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesAnnual.csv', stringsAsFactors = FALSE)


# some data prep ----------------------------------------------------------

# rolling monthly comic sales up to publisher - quarter level

comicSales <- comicsMonthly %>%
  filter(publisher %in% c('Marvel', 'DC')) %>%
  mutate(sales = estUnits*price,
         quarter = quarter(date)) %>%
  group_by(publisher, year, quarter) %>%
  summarise(units = sum(estUnits),
            sales = sum(sales),
            avgPrice = round(mean(price), 2),
            nProducts = n_distinct(title)) %>%
  mutate(medium = 'comics')


# rolling movie sales up to publisher - quarter level
head(movies)
movieSales <- movies %>% 
  mutate(year = year(date),
         quarter = quarter(date),
         units = gross/avgTicketPrice) %>%
  group_by(publisher, year, quarter) %>%
  summarise(sales = sum(gross),
            units = sum(units),
            avgPrice = mean(avgTicketPrice),
            nProducts = n_distinct(title)) %>%
  mutate(medium = 'movies')


# joining comic and movie sales data to the publisher-product-quarter level

sales <- bind_rows(movieSales, comicSales)

head(sales)

# descriptives ------------------------------------------------------------

# total comics sold and gross sales

comicsMonthly %>%
  filter(publisher %in% c('Marvel', 'DC')) %>%
  group_by(publisher) %>%
  summarise(totalUnits = sum(estUnits, na.rm = TRUE),
            totalSales = sum(estUnits*price, na.rm = TRUE),
            totalTitles = n_distinct(title)) 

comicsMonthly %>%
  filter(publisher %in% c('Marvel', 'DC')) %>%
  group_by(publisher) %>%
  summarise(totalUnits = sum(estUnits, na.rm = TRUE),
            totalSales = sum(estUnits*price, na.rm = TRUE)) %>%
  ggplot(aes(x = publisher, y = totalUnits, fill = publisher)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = 'bottom') +
  labs(x = 'Year', y = 'Units Sold', fill = 'Publisher') +
  scale_y_continuous(name = 'Units Sold', 
                     breaks = seq(400000000, 1000000000, by = 200000000), 
#                     limits = c(400000000, 1000000000), 
                     expand = c(.1, .1),
                     labels = scales::comma)
  
# total movie tickets sold and gross sales

movies %>%
  mutate(ticketsSold = gross/avgTicketPrice) %>%
  group_by(publisher) %>%
  summarise(totalGross = sum(as.numeric(gross)),
            titles = n_distinct(title),
            totalTickets = sum(ticketsSold))

movies %>%
  summarise(gross = sum(gross, na.rm = TRUE))

# sales trends

sales %>%
  filter(medium == 'movies', year > 2000) %>%
  ggplot(aes(x = year, y = units, group = publisher, color = publisher)) +
    geom_line() +
    stat_summary(fun.y = sum)
    
sales %>%
  filter(medium == 'comics', year > 2000) %>%
  ggplot(aes(x = year, y = units, group = publisher, color = publisher, fill = publisher)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  stat_summary(fun.y = sum) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_y_discrete(name = 'Units Sold', limits = c(5000000, 20000000))

comicsMonthly %>%
  filter(publisher %in% c('Marvel', 'DC')) %>%
  group_by(publisher, date) %>%
  summarise(unitsSold = sum(estUnits)) %>%
  ggplot(aes(x = date, y = unitsSold, color = publisher)) +
  geom_smooth() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
                       plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = 'bottom') +
  labs(x = 'Year', y = 'Units Sold', color = 'Publisher')

# some analyses -----------------------------------------------------------
