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

# What are the differences in data types of columns when using `read.csv` vs `read_csv`? Especially compare character or factor data types. For an intriguing read into the perils of using factors, check out level 8.2 of the [R_inferno.pdf](http://www.burns-stat.com/pages/Tutor/R_inferno.pdf) 9 levels of hell in R (yes, a [Dante reference](https://en.wikipedia.org/wiki/Inferno_(Dante))).

#```{r pseudocode, eval=F}
# read in csv
# view data
# limit columns to species and year
# limit rows to just species "NL"
# get count per year
# write out csv
#```

# read in csv
surveys = read.csv('../data/r-ecology/surveys.csv') 

# view data
head(surveys)
summary(surveys)

# limit columns to species and year
surveys_2 = surveys[,c('species_id', 'year')]

# limit rows to just species "NL"
surveys_3 = surveys_2[surveys_2$species_id  == 'NL',]

# get count per year
surveys_4 = aggregate(species_id ~ year, data=surveys_3, FUN='length')

# write to csv
write.csv(surveys_4, 'data/surveys_bbest.csv', row.names = FALSE)


# NESTED FUNCTIONS --------------------------------------------------------

# read in data
surveys = read.csv('../data/r-ecology/surveys.csv') 

# view data
head(surveys)
summary(surveys)

# limit data with [], aggregate to count, write to csv
write.csv(
  aggregate(
    species_id ~ year, 
    data = surveys[surveys_2$species_id  == 'NL', c('species_id', 'year')], 
    FUN = 'length'), 
  'data/surveys_bbest.csv',
  row.names = FALSE)


# DPLYR 1 -----------------------------------------------------------------

# load libraries
library(readr)
library(dplyr)
library(magrittr) # for %T>%

# read in csv
surveys = read_csv('./data/surveys.csv') 

# dplyr elegance
surveys %T>%                          # note tee operator %T>% for glimpse
  glimpse() %>%                       # view data
  select(species_id, year) %>%        # limit columns
  filter(species_id  == 'NL') %>%     # limit rows
  group_by(year) %>%                  # get count by first grouping
  summarize(n = n()) %>%              #   then summarize
  write_csv('data/surveys_rpeek.csv') # write out csv

# The "tee" operator `%T>%` is similar to the "then" operator `%>%` in that the left side is passed to the right, but is then also teed off as the output of the right side. This is useful in this case for `glimpse` since its output is simply printed to the Console and does not otherwise return the data frame needed to continue the sequence of operations. So the "tee" operator `%T>%` is most useful for injecting intermediate operations like printing or plotting that wouldn't otherwise output a return object for continuing operations.

# note %<>% operator writing back to same name


surveys2 = read_csv('./data/surveys_rpeek.csv') 
# TIDYR -------------------------------------------------------------------

### EDAWR

# ```{r EDAWR}
# # install.packages("devtools")
# # devtools::install_github("rstudio/EDAWR")
# library(EDAWR)
# help(package='EDAWR')
# ?storms    # wind speed data for 6 hurricanes
# ?cases     # subset of WHO tuberculosis
# ?pollution # pollution data from WHO Ambient Air Pollution, 2014
# ?tb        # tuberculosis data
# View(storms)
# View(cases)
# View(pollution)
# ```
# 
# ### slicing
# 
# ```{r traditional R slicing}
# # storms
# storms$storm
# storms$wind
# storms$pressure
# storms$date
# 
# # cases
# cases$country
# names(cases)[-1]
# unlist(cases[1:3, 2:4])
# 
# # pollution
# pollution$city[c(1,3,5)]
# pollution$amount[c(1,3,5)]
# pollution$amount[c(2,4,6)]
# 
# # ratio
# storms$pressure / storms$wind
# ```
# 
# ```{r dplyr on storms}
# # better yet
# library(dplyr)
# 
# pollution %>%
#   filter(city != 'New York') %>%
#   mutate(
#     ratio = pressure / wind)




# read co2
library(dplyr)
library(readxl) # install.packages('readxl')

# xls downloaded from http://edgar.jrc.ec.europa.eu/news_docs/CO2_1970-2014_dataset_of_CO2_report_2015.xls
xls = './data/co2_europa.xls'

print(getwd())
co2 = read_excel(xls, skip=12) # why skip 12?
co2



# DPLYR -------------------------------------------------------------------

# install.packages("dplyr")
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
# 
# ### `select`
# 
# ```{r select}
# storms
# select(storms, storm, pressure)
# storms %>% select(storm, pressure)
# ```
# 
# ### `filter`
# 
# ```{r filter}
# storms
# filter(storms, wind >= 50)
# storms %>% filter(wind >= 50)
# 
# storms %>%
#   filter(wind >= 50) %>%
#   select(storm, pressure)
# ```
# 
# ### `mutate`
# 
# ```{r mutate}
# storms %>%
#   mutate(ratio = pressure / wind) %>%
#   select(storm, ratio)
# ```
# 
# ### `group_by`
# 
# ```{r group_by}
# pollution
# pollution %>% group_by(city)
# ```
# 
# ### `summarise`
# 
# ```{r summarise}
# # by city
# pollution %>% 
#   group_by(city) %>%
#   summarise(
#     mean = mean(amount), 
#     sum = sum(amount), 
#     n = n())
# 
# # by size
# pollution %>% 
#   group_by(size) %>%
#   summarise(
#     mean = mean(amount), 
#     sum = sum(amount), 
#     n = n())
# ```
# 
# note that `summarize` synonymously works
# 
# ### `ungroup`
# 
# ```{r ungroup}
# pollution %>% 
#   group_by(size)
# 
# pollution %>% 
#   group_by(size) %>%
#   ungroup()
# ```
# 
# ### multiple groups
# 
# ```{r multiple groups}
# tb %>%
#   group_by(country, year) %>%
#   summarise(cases = sum(cases))
# summarise(cases = sum(cases))
# ```
# 
# **Recap: dplyr**:
#   
#   - Extract columns with `select()` and rows with `filter()`
# 
# - Sort rows by column with `arrange()`
# 
# - Make new columns with `mutate()`
# 
# - Group rows by column with `group_by()` and `summarise()`
# 
# See sections in the [data-wrangling-cheatsheet.pdf](./refs/cheatsheets/data-wrangling-cheatsheet.pdf):
#   
#   - Subset Variables (Columns), eg `select()`
# 
# - Subset Observations (Rows), eg `filter()`
# 
# - Reshaping Data - Change the layout of a data set, eg `arrange()`
# 
# - Make New Variables, eg `mutate()`
# 
# - Group Data, eg `group_by()` and `summarise()`
