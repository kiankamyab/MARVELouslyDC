##
#
# kian kamyab
#
# we scrape some comic book sales data, clean it up a bit, and harmonize it
# with movie sales data in order to do some analyses for a 10 minute presentation.
#
# note: the plyr package ends up not playing very well with some of dplyr's group_by
#       functions during the aggregation of monthly sales data. it seems to depend on 
#       your R setup. one may have to detach AND unload plyr if group_by functions
#       yield oddball results.
#
##


# loading libraries -------------------------------------------------------

library(tidyverse)
library(rvest)
library(stringr)
library(boxoffice)
library(purrr)
library(VIM)
library(plyr)
library(ggthemes)
library(lubridate)


# scraping annual sales ---------------------------------------------------

comicSalesAnnualList <- list()
comicSalesAnnualURL <- 'http://www.comichron.com/monthlycomicssales/'

# this loops through web pages with tables of annual comic book sales by publisher
# from 1991 to 2017, creating a list of data frames

for (salesYear in 1991:2017) {
  comicSalesAnnualTbl <- paste0(comicSalesAnnualURL, salesYear, '.html') %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="Top300Comics"]') %>%
    html_table()
  listNameTmp <- paste('Sales in: ', salesYear)
  comicSalesAnnualList[[listNameTmp]] <- as.data.frame(comicSalesAnnualTbl[1]) %>%
    mutate(year = salesYear)
}


# flattening the list into a single data frame

comicSalesAnnual <- rbind.fill(comicSalesAnnualList)


# saving a csv of annual comic book sales

write.csv(comicSalesAnnual, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesAnnual.csv')


# scraping monthly sales -------------------------------------------------

comicSalesMonthlyURL <- 'http://www.comichron.com/monthlycomicssales/'

salesMonths <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')


# scraping 1999-2002: using the same template as for annual sales
# however breaking up years to scrap owing to the fact that the tables
# storing the data aren't consistent across web pages on comichron's web site
#

comicSalesMonthlyList9902 <- list()

for (salesYear in 1999:2002) {
  for (salesMonth in salesMonths) {
  comicSalesMonthlyTbl <- paste0(comicSalesMonthlyURL, '/', salesYear, '/', salesYear, '-', salesMonth, '.html') %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="content"]/div[2]/table') %>%
    html_table()
  listNameTmp <- paste('Sales in: ', salesYear, '-', salesMonth)
  comicSalesMonthlyList9902[[listNameTmp]] <- as.data.frame(comicSalesMonthlyTbl[1]) %>%
    mutate(year = salesYear, month = as.numeric(salesMonth))
}
}

comicSalesMonthly9902 <- rbind.fill(comicSalesMonthlyList9902)
head(comicSalesMonthly9902)

comicSalesMonthly9902 <- comicSalesMonthly9902 %>%
  dplyr::rename(units = X1, title = X2, issue = X3, price = X4, publisher = X5, estUnits = X6) %>%
  slice(-1)
  
write.csv(comicSalesMonthly9902, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly9902.csv')


## 2005 through 2017

comicSalesMonthlyList0517 <- list()

for (salesYear in 2005:2017) {
  for (salesMonth in salesMonths) {
    comicSalesMonthlyTbl <- paste0(comicSalesMonthlyURL, '/', salesYear, '/', salesYear, '-', salesMonth, '.html') %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="Top300Comics"]') %>%
      html_table()
    listNameTmp <- paste('Sales in: ', salesYear, '-', salesMonth)
    comicSalesMonthlyList0517[[listNameTmp]] <- as.data.frame(comicSalesMonthlyTbl[1]) %>%
      mutate(year = salesYear, month = as.numeric(salesMonth))
  }
}

comicSalesMonthly0517 <- rbind.fill(comicSalesMonthlyList0517)


# one month (2017 - 04) of sales data had discrepant column names
# so i'm harmonizing those names with all other months
#

comicSalesMonthly0517$Units <-
  ifelse(is.na(comicSalesMonthly0517$Units), comicSalesMonthly0517$Unit.sales, comicSalesMonthly0517$Units)

comicSalesMonthly0517$Dollars <-
  ifelse(is.na(comicSalesMonthly0517$Dollars), comicSalesMonthly0517$Dollar.sales, comicSalesMonthly0517$Dollars)

comicSalesMonthly0517 <- comicSalesMonthly0517 %>%
  select(-Unit.sales, -Dollar.sales)

write.csv(comicSalesMonthly0517, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly0517.csv')


# 2003 and 2004 sales data are needlessly annoying in format
#

salesYear03 <- 2003
salesMonth03 <- '01'
salesMonths032 <- c('02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

comicSalesMonthlyList0301 <- list()

comicSalesMonthlyTbl <- paste0(comicSalesMonthlyURL, '/', salesYear03, '/', salesYear03, '-', salesMonth03, '.html') %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Top300Comics"]') %>%
  html_table()
listNameTmp <- paste('Sales in: ', salesYear03, '-', salesMonth03)
comicSalesMonthlyList0301[[listNameTmp]] <- as.data.frame(comicSalesMonthlyTbl[1]) %>%
  mutate(year = salesYear03, month = as.numeric(salesMonth03))

comicSalesMonthly0301 <- rbind.fill(comicSalesMonthlyList0301)


# 2003: february through december

comicSalesMonthlyList030212 <- list()

for (salesMonth in salesMonths032) {
  comicSalesMonthlyTbl <- paste0(comicSalesMonthlyURL, '/', salesYear03, '/', salesYear03, '-', salesMonth, '.html') %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="content"]/div[2]/table') %>%
    html_table()
  listNameTmp <- paste('Sales in: ', salesYear03, '-', salesMonth)
  comicSalesMonthlyList030212[[listNameTmp]] <- as.data.frame(comicSalesMonthlyTbl[1]) %>%
    mutate(year = salesYear03, month = as.numeric(salesMonth))
}

comicSalesMonthly030212 <- rbind.fill(comicSalesMonthlyList030212)

comicSalesMonthly030212 <- comicSalesMonthly030212 %>%
  dplyr::rename(units = X1, title = X2, issue = X3, price = X4, publisher = X5, estUnits = X6) %>%
  slice(-1)

write.csv(comicSalesMonthly030212, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly030212.csv')


# consolidating 2003

head(comicSalesMonthly030212)
head(comicSalesMonthly0301)

comicSalesMonthly0301 <- comicSalesMonthly0301 %>%
  dplyr::rename(units = Units, title = Comic.book.Title, issue = Issue, price = Price,
         publisher = Publisher, estUnits = Est..units)

comicSalesMonthly03 <- rbind.fill(comicSalesMonthly0301, comicSalesMonthly030212)

tail(comicSalesMonthly03)

write.csv(comicSalesMonthly03, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly03.csv')


## 2004 --  may's table just will not cooperate (suspecting java has something to do with it), 
#           so for expediency's sake, i manually copy and paste it into a csv
#           and incorporate into the analytic dataset manually

salesMonths04 <- c('01', '02', '03', '04', '06', '07', '08', '09', '10', '11', '12')
comicSalesMonthlyList04 <- list()

for (salesYear in 2004) {
  for (salesMonth in salesMonths04) {
    comicSalesMonthlyTbl <- paste0(comicSalesMonthlyURL, '/', salesYear, '/', salesYear, '-', salesMonth, '.html') %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="Top300Comics"]') %>%
      html_table()
    listNameTmp <- paste('Sales in: ', salesYear, '-', salesMonth)
    comicSalesMonthlyList04[[listNameTmp]] <- as.data.frame(comicSalesMonthlyTbl[1]) %>%
      mutate(year = salesYear, month = as.numeric(salesMonth))
  }
}

comicSalesMonthly0405 <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly0405.csv') %>%
  mutate(year = 2004, month = 5) %>%
  dplyr::rename(Est..units = Est..sales, Units = X)

comicSalesMonthly04 <- rbind.fill(comicSalesMonthly04, comicSalesMonthly0405)

comicSalesMonthly04 <- comicSalesMonthly04 %>%
  dplyr::rename(units = Units, title = Comic.book.Title, issue = Issue, price = Price,
                publisher = Publisher, estUnits = Est..units)

write.csv(comicSalesMonthly04, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly04.csv')


# harmonizing monthly sales data 1999-2002 ------------------------------------------

# trade paperback titles are included in these sales data. let's incorporate them, but
# if we wanted to slice them out, playing around with ways of doing so:

# comicSalesMonthly9902 %>%
# filter(title == 'Trade Paperback title')
# paperbackIndex <- which(comicSalesMonthly9902$title == 'Trade Paperback title')
# comicIndex <- which(comicSalesMonthly9902$title == 'Comic-book Title')
# 
# pbIndex <- c()
# for (i in paperbackIndex) {
#   pbIndexTmp <- seq(from = i, to = i + 50, by = 1)
#   pbIndex <- c(pbIndex, pbIndexTmp)
# }
# 
# pbIndex
# test <- comicSalesMonthly9902[pbIndex, ]


# 1999-2002 requires some extra cleaning

head(comicSalesMonthly9902)
tail(comicSalesMonthly9902)
  
comicSalesMonthly9902 %>%
  mutate(estPreOrders = gsub(',', '', comicSalesMonthly9902$estPreOrders),
         price = gsub('$', '', comicSalesMonthly9902$price)) 
  filter(!(estPreOrders == '' & unitsSoldRank == '')) %>%
  slice(which(is.na(as.numeric(comicSalesMonthly9902$estPreOrders))))

comicSalesMonthly9902 <- comicSalesMonthly9902 %>%
  rename(units = unitsSoldRank, estUnits = estPreOrders)


# check and harmonize column names

head(comicSalesMonthly9902)
head(comicSalesMonthly03)
head(comicSalesMonthly04)
head(comicSalesMonthly0517)

comicSalesMonthly0517 <- comicSalesMonthly0517 %>%
  dplyr::rename(units = Units, title = Comic.book.Title,
                issue = Issue, price = Price, publisher = Publisher, estUnits = Est..units,
                onSale = On.sale)


# binding all years and cleaning up

comicSalesMonthly <- rbind.fill(comicSalesMonthly9902, comicSalesMonthly03, comicSalesMonthly04, comicSalesMonthly0517)
head(comicSalesMonthly)
str(comicSalesMonthly)


# coercing measures into the right format and dropping some unneccessary 

comicSalesMonthly <- comicSalesMonthly %>%
  mutate(price = as.numeric(gsub("\\$", "", comicSalesMonthly$price)),
         estUnits = as.numeric(gsub("\\,", "", comicSalesMonthly$estUnits)),
         date = as.Date(paste0(comicSalesMonthly$year,'-',comicSalesMonthly$month,'-1'), '%Y-%m-%d')) %>%
  select(-onSale, -Dollars)

comicSalesMonthly %>%
  arrange(date, desc(estUnits)) %>%
  write.csv('/Users/leotrozvii/Desktop/MARVELouslyDC/data/comicSalesMonthly.csv')


# box office mojo ---------------------------------------------------------

ticketPrices <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/ticketPrices.csv')
usPop <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/popUS.csv', stringsAsFactors = FALSE)
movieSales <- read.csv(file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/movieSales.csv', stringsAsFactors = FALSE)

movieSales <- movieSales %>%
  mutate(date = as.Date(movieSales$date, '%m/%d/%y'))

usPop <- usPop %>%
  mutate(date = as.Date(usPop$date, '%d-%b-%y'),
         year = year(date)) %>%
  select(-date)

movieSales <- movieSales %>%
  mutate(year = year(date)) %>%
  left_join(ticketPrices, by = c('year')) %>%
  left_join(usPop, by = c('year'))

head(movieSales)

write.csv(movieSales, file = '/Users/leotrozvii/Desktop/MARVELouslyDC/data/movieSalesPop.csv')



