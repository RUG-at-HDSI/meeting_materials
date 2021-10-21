#' ---
#' title: "Analyzing USAFacts COVID-19 Data With dplyr"
#' author: "Christian Testa"
#' date: "October 21st, 2021"
#' ---
#' 
#' Using dplyr, we will do some data reshaping and render visualizations of
#' the results with the USAFacts datasets.
#' 
#' USAFacts provides data on cases, deaths, and population sizes for download
#' at the county level at the following URL:
#' https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
#'
#' Code Organization
#' 
#'     0. Dependencies
#'     1a. Load data
#'     1b. Cleaning data
#'     2a. Reshape data
#'     2b. De-cumulate data
#'     3. Merge data
#'     4. Summarize into weekly case and death counts and per capita incidence
#'     5. Examine trends within a state
#'         - Plot county trends
#'         - Plot summary county trends
#' 
#' Our data analysis goals include the following: 
#' 
#'   - Pivot the wide formatted cases and deaths data into a long-and-lean
#'     format
#'   - Merge the cases, deaths, and population counts data together
#'   - Calculate weekly cases and deaths per capita (per 100k residents)
#'   
#' Our pedagogical goals include the following:
#' 
#'   - Understanding of the functions %>%, pivot_longer, group_by,
#'     summarize, filter, left_join, mutate
#'   - Why we want "long" formatted data
#'   - How and why %>% is at the core of tidyverse
#'
#' Some caveats that must be given: 
#' 
#'   - These data provided from USAFacts are scraped from public health 
#'   websites (such as state and county dashboards) and reflect cumulative
#'   totals.  Cumulative totals of cases and deaths have, on occasion, been 
#'   significantly adjusted upward or downward based on decisions applied
#'   retroactively (such as to include or exclude "probable" cases). 
#'   
#'   - Some states' data are irregular, such as Nebraska and Florida, which 
#'   respectively appear to stop reporting data at different times. 
#'   
#' References:
#' 
#' 1. https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
#' 2. https://usafacts.org/articles/detailed-methodology-covid-19-data/
#' 3. https://en.m.wikipedia.org/wiki/Dplyr
#' 4. https://dplyr.tidyverse.org/
#' 5. https://r4ds.had.co.nz/transform.html


# 0. Dependencies ---------------------------------------------------------
library(tidyverse)
library(janitor)
library(magrittr)

# 1a. Load data ------------------------------------------------------------

cases <- readr::read_csv("covid_confirmed_usafacts.csv")
deaths <- readr::read_csv("covid_deaths_usafacts.csv")
popsize <- readr::read_csv("covid_county_population_usafacts.csv")


# 1b. Clean data ----------------------------------------------------------

# an aside on how %>% and %<>% work:

# f(x) and x %>% f() are equivalent expressions (meaning they will do the same
# things).
# similarly, f(x,y) and x %>% f(y) are equivalent expressions. 

# f <- f(x) and x %<>% f() are equivalent expressions.
# similarly, x <- f(x,y) and x %<>% f(y) are equivalent expressions.

# example 
# head(mtcars)
# mtcars %>% head()

# head(mtcars, 3)
# mtcars %>% head(3) 

# example of copying mtcars and then using head to replace it with the 
# first 3 rows 
# example_df <- mtcars
# example_df %<>% head(3)

# if you need to refer to the "left hand variable", you can do so 
# by referring to it with a period (as in .)
# example:
# iris %>% subset(1:nrow(.) %% 2 == 0)

# you can access the help pages via ?`%>%` or ?`%<>%`


# clean column names 
cases %<>% janitor::clean_names()
deaths %<>% janitor::clean_names()
popsize %<>% janitor::clean_names()


# 2a. Reshape data ---------------------------------------------------------

# pivot into a long format so that each row contains a unique observation
cases %<>% pivot_longer(
  cols = colnames(.)[5:ncol(.)],
  names_to = 'date',
  values_to = 'cases')

deaths %<>% pivot_longer(
  cols = colnames(.)[5:ncol(.)],
  names_to = 'date',
  values_to = 'deaths')
  
# now that the data are in long format, let's clean the date field
cases$date %<>% 
  stringr::str_remove_all("x") %>% 
  lubridate::ymd()

deaths$date %<>% 
  stringr::str_remove_all("x") %>% 
  lubridate::ymd()

# 2b. De-cumulate data ----------------------------------------------------

# create a second dataset that has dates for one week ahead 
# and rename variables appropriately
cases_1week_behind <- cases %>% 
  mutate(one_week_ahead_date = date + 7) %>% 
  rename(cases_one_week_lagged = cases) %>% 
  select(-date) 

# merge in cases from 1 week ahead
cases %<>% left_join(
  cases_1week_behind,
  by = c(
    'date' = 'one_week_ahead_date', 
    'county_fips', 
    'county_name', 
    'state', 
    'state_fips'))

# calculate last 7 days of cases from cumulative totals
cases %<>% mutate(
  last_7days_of_cases = cases - cases_one_week_lagged)

# filter because I only want one day per week of observations
cases %<>% mutate(
  weekday = lubridate::wday(date)) %>% 
  filter(weekday == 1) # e.g. sundays


# repeat steps for deaths, starting with creating the lagged dataset:
deaths_1week_behind <- deaths %>% 
  mutate(one_week_ahead_date = date + 7) %>% 
  rename(deaths_one_week_lagged = deaths) %>% 
  select(-date) 

# merge in lagged dataset
deaths %<>% left_join(
  deaths_1week_behind,
  by = c(
    'date' = 'one_week_ahead_date', 
    'county_fips', 
    'county_name', 
    'state', 
    'state_fips'))

# calculate last 7 days of deaths from cumulative totals
deaths %<>% mutate(
  last_7days_of_deaths = deaths - deaths_one_week_lagged)

# filter because I only want one day per week of observations
deaths %<>% mutate(
  weekday = lubridate::wday(date)) %>% 
  filter(weekday == 1) # e.g. sundays

# for both datasets, drop the columns of data we won't use (the cumulative
# totals) now that we've made our 7 day counts
cases %<>% select(-c(cases, cases_one_week_lagged))
deaths %<>% select(-c(deaths, deaths_one_week_lagged))

# 3. Merge data -----------------------------------------------------------

# we'll drop state_fips here since we won't be using it and it isn't 
# present in the population size dataset -- 
# we also won't be using weekday anymore, so we'll drop that too

cases %<>% select(-c(state_fips, weekday))
deaths %<>% select(-c(state_fips, weekday))

# merge population sizes into the cases data
df <- 
  left_join(
    cases,
    popsize,
    by = c('county_fips', 'county_name', 'state')) 

df %<>% 
  left_join(
    deaths,
    by = c('date', 'county_fips', 'county_name', 'state')) 

# 4. Summarize into weekly cumulative incidence proportions ---------------

df %<>% mutate(
  weekly_cumulative_cases_per_100k = last_7days_of_cases / population * 1e5,
  weekly_cumulative_deaths_per_100k = last_7days_of_deaths / population * 1e5,
)

# 5. Examine trends within a state ----------------------------------------

# weekly cases plot
df %>% 
  filter(state == 'MA') %>% 
  filter(county_name != 'Statewide Unallocated') %>% 
  ggplot(
    aes(
      x = date, 
      y = weekly_cumulative_cases_per_100k, 
      color = county_name)) + 
  geom_line() + 
  ylab("Weekly Cases per 100,000 Residents") + 
  xlab("Date") + 
  labs(color = 'County Name') + 
  ggtitle("Weekly Cases per 100,000 Residents, Massachusetts")

# weekly deaths plot
df %>% 
  filter(state == 'MA') %>% 
  filter(county_name != 'Statewide Unallocated') %>% 
  ggplot(
    aes(
      x = date, 
      y = weekly_cumulative_deaths_per_100k, 
      color = county_name)) + 
  geom_line() + 
  ylab("Weekly Deaths per 100,000 Residents") + 
  xlab("Date") + 
  labs(color = 'County Name') + 
  ggtitle("Weekly Deaths per 100,000 Residents, Massachusetts")


# a function for plotting weekly cases per 100k by state
plot_weekly_cases_per_100k_by_state <- function(state_abbrev) { 
  
  df %>% 
    filter(state == state_abbrev) %>% 
    filter(county_name != 'Statewide Unallocated') %>% 
    ggplot(
      aes(
        x = date, 
        y = weekly_cumulative_cases_per_100k, 
        color = county_name)) + 
    geom_line() + 
    ylab("Weekly Cases per 100,000 Residents") + 
    xlab("Date") + 
    labs(color = 'County Name') + 
    ggtitle(str_c("Weekly Cases per 100,000 Residents, ", state_abbrev)) + 
    theme(legend.position = 'bottom')
}

#+ fig.height = 7, fig.width = 12
plot_weekly_cases_per_100k_by_state("NY")


# a new version of the prior function, now with averages and confidence
# intervals by week
plot_weekly_cases_per_100k_smoothed <- function(state_abbrev) {
  df %>% 
    filter(state == state_abbrev) %>% 
    filter(county_name != 'Statewide Unallocated') %>% 
    group_by(date) %>% 
    summarize(
      mean_cases_per_100k = mean(weekly_cumulative_cases_per_100k, na.rm=T),
      ci_high_cases_per_100k = quantile(weekly_cumulative_cases_per_100k, 0.975, na.rm=T),
      ci_low_cases_per_100k = quantile(weekly_cumulative_cases_per_100k, 0.025, na.rm=T)
    ) %>% 
    ggplot(
      aes(
        x = date, 
        y = mean_cases_per_100k, 
        ymax = ci_high_cases_per_100k,
        ymin = ci_low_cases_per_100k)) + 
    geom_ribbon(size = 0, fill = 'cadetblue', alpha = 0.7) + 
    geom_line() + 
    ylab("Weekly Cases per 100,000 Residents") + 
    xlab("Date") + 
    ggtitle(str_c("Weekly Cases per 100,000 Residents, ", state_abbrev),
            subtitle = "Average and 95% Quantile Range Shown Among Weekly County Cases per 100,000")
}

#+ fig.height = 5
plot_weekly_cases_per_100k_smoothed('MA')
plot_weekly_cases_per_100k_smoothed('NY')
plot_weekly_cases_per_100k_smoothed('TX')
plot_weekly_cases_per_100k_smoothed('FL')
