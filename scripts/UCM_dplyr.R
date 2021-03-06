## UCM Bioinformatics: using dplyr

## Two examples, one with R-ecology and one with lubridate temperature data


# QUICK INTRO TO READR ----------------------------------------------------

# what's different here?
library(readr)
d = read.csv("./data/species.csv")
dd = read_csv('./data/species.csv')
d
head(d)
summary(d)

# What are the differences in data types of columns when using `read.csv` vs `read_csv`? Especially compare character or factor data types. For an intriguing read into the perils of using factors, check out level 8.2 of the (http://www.burns-stat.com/pages/Tutor/R_inferno.pdf) 9 levels of hell in R.

# OKAY NOW LET"S PROCESS DATA ---------------------------------------------

## STEPS TO PROCESSING
# 1. read in csv
# 2. view data
# 3. limit columns to species and year
# 4. limit rows to just species "NL"
# 5. get count per year
# 6. write out csv

# read in csv
surveys = read.csv('./data/surveys.csv') 

# view data
head(surveys)
summary(surveys)
str(surveys)

# limit columns to species and year
surveys_2 = surveys[,c('species_id', 'year')]

# limit rows to just species "NL"
surveys_3 = surveys_2[surveys_2$species_id  == 'NL',]

# get count per year
surveys_4 = aggregate(species_id ~ year, data=surveys_3, FUN='length')

# write to csv
write.csv(surveys_4, 'data/surveys_rapeek.csv', row.names = FALSE)


# NESTED FUNCTIONS --------------------------------------------------------

# read in data
surveys = read.csv('./data/surveys.csv') 

# view data
head(surveys)
summary(surveys)

# limit data with [], aggregate to count, write to csv
write.csv(
  aggregate(
    species_id ~ year, 
    data = surveys[surveys_2$species_id  == 'NL', c('species_id', 'year')], 
    FUN = 'length'), 
  'data/surveys_rapeek.csv',
  row.names = FALSE)

library(dplyr)




# TIDYR -------------------------------------------------------------------

### EDAWR

# install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
library(EDAWR)
# help(package='EDAWR')
# ?storms    # wind speed data for 6 hurricanes
# ?cases     # subset of WHO tuberculosis
# ?pollution # pollution data from WHO Ambient Air Pollution, 2014
# ?tb        # tuberculosis data
head(storms)
View(cases)
View(pollution)

# slicing data: done with base R

## storms
storms$storm
storms$wind
storms$pressure
storms$date

## cases
cases$country
names(cases)[-1]
unlist(cases[1:3, 2:4])
 
## pollution
pollution$city[c(1,3,5)]
pollution$amount[c(1,3,5)]
pollution$amount[c(2,4,6)]
 
## ratio
storms$pressure / storms$wind


# DPLYR 1 -----------------------------------------------------------------

# load libraries
library(readr)
library(dplyr)
library(magrittr) # for %T>%

pollution %>%
  filter(city != 'New York') %>%
  group_by(size) %>% 
  summarize(
    mean_amount = mean(amount),
    min = min(amount))

# read co2
library(readxl) # install.packages('readxl')

# xls downloaded from http://edgar.jrc.ec.europa.eu/news_docs/CO2_1970-2014_dataset_of_CO2_report_2015.xls

xls = './data/co2_europa.xls'

print(getwd())
co2 = read_excel(xls, skip=12) # why skip 12?
co2

# read in csv
surveys = read_csv('./data/surveys.csv') 

# dplyr elegance
surveys %>%                          # note tee operator %T>% for glimpse
  select(species_id, year) %>%        # limit columns
  filter(species_id  == 'NL') %>%     # limit rows
  group_by(year) %>%                  # get count by first grouping
  summarize(n = n()) %T>%              #   then summarize
  glimpse() %>%                       # view data
  write_csv('data/surveys_rpeek2.csv') # write out csv

# The "tee" operator `%T>%` is similar to the "then" operator `%>%` in that the left side is passed to the right, 
# but is then also teed off as the output of the right side. This is useful in this case for `glimpse` since its 
# output is simply printed to the Console and does not otherwise return the data frame needed to continue the sequence of operations. So the "tee" operator `%T>%` is most useful for injecting intermediate operations like printing or plotting that wouldn't otherwise output a return object for continuing operations.

# note %<>% operator writing back to same name


surveys2 = read_csv('./data/surveys_rpeek.csv') 


# DPLYR 2 -----------------------------------------------------------------

library(dplyr)
?select
?filter
?arrange
?mutate
?group_by
?summarise

# - Subset Variables (Columns), eg `select()`
# - Subset Observations (Rows), eg `filter()`
# - Reshaping Data - Change the layout of a data set, eg `arrange()`
# - Make New Variables, eg `mutate()`
# - Group Data, eg `group_by()` and `summarise()`
 
## `select`
 
storms
select(storms, storm, pressure)
storms %>% select(storm, pressure)

## `filter`
storms
filter(storms, wind >= 50)
storms %>% filter(wind >= 50)

storms %>%
  filter(wind >= 50) %>%
  select(storm, pressure)

## `mutate`
storms %>%
  mutate(ratio = pressure / wind) %>%
  select(storm, ratio)

## `group_by`
pollution
pollution %>% group_by(city)

## `summarise`

# by city
pollution %>%
  group_by(city) %>%
  summarise(
    mean = mean(amount),
    sum = sum(amount),
    n = n())

# by size
pollution %>%
  group_by(size) %>%
  summarise(
    mean = mean(amount),
    sum = sum(amount),
    n = n())

# note that `summarize` synonymously works
## `ungroup`

pollution %>%
  group_by(size)

pollution %>%
  group_by(size) %>%
  ungroup()

# Recap: dplyr
# - Extract columns with `select()` and rows with `filter()`
# - Sort rows by column with `arrange()`
# - Make new columns with `mutate()`
# - Group rows by column with `group_by()` and `summarise()`
# See sections in the [data-wrangling-cheatsheet.pdf](./refs/cheatsheets/data-wrangling-cheatsheet.pdf):

# - Subset Variables (Columns), eg `select()`
# - Subset Observations (Rows), eg `filter()`
# - Reshaping Data - Change the layout of a data set, eg `arrange()`
# - Make New Variables, eg `mutate()`
# - Group Data, eg `group_by()` and `summarise()`
