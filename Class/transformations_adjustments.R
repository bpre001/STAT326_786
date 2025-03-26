library(tidyverse)
library(fpp3)
library(fma)

#####
# Calendar adjustments (impact of number of days per month)
#####

# Read in monthly milk production per cow data
milk <- fma::milk %>% 
  as_tsibble()
milk %>%
  autoplot(value) +
  ylab("Milk production per cow (in pounds)")  # Can you notice a second pattern?
milk %>%
  gg_subseries(value)  # What do you notice about February?

# How can we adjust for the number of days per month?
milk <- milk %>%
  mutate(days = days_in_month(index)) %>%
  mutate(value_per_day = value / days) 

milk %>%
  autoplot(value_per_day) +
  ylab("Milk production per cow (in pounds)")  # What is different here?
milk %>%
  gg_subseries(value_per_day)  # What is different here?


#####
# Population adjustments (per capita)
#####

global_economy %>%
  filter(Country == "New Zealand") %>%
  autoplot(GDP)  # GDP

global_economy %>%
  filter(Country == "New Zealand") %>%
  autoplot(GDP / Population)  # GDP per capita

#####
# Exercise: Compare NZ, Australia, UK, USA GDP and GDP per capita
#####
global_economy %>%
  filter(Country %in% c("New Zealand", "Australia",
                        "United Kingdom", "United States")) %>%
  autoplot(GDP)  # GDP
global_economy %>%
  filter(Country %in% c("New Zealand", "Australia",
                        "United Kingdom", "United States")) %>%
  autoplot(GDP / Population)  # GDP per capita




#####
# Inflation (price change) adjustments
#####

print_media <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

print_media %>%
  autoplot(Turnover)  # This is in "nominal" values. Can we inflation-adjust?

# Recall we have tonnes of economic data for Australia, including the CPI
aus_economy <- global_economy %>%
  filter(Country == "Australia") %>%
  select(Year, CPI)

# We have two data sets; one with turnover, one with economic indicators
# We can use SQL type language from dplyr to join data sets together
glimpse(print_media)
glimpse(aus_economy)
# What variable do they have in common?

# Join the data sets together
pm <- print_media %>%
  left_join(aus_economy, by = "Year") 
pm
# Why left join as opposed to another join?
# We are keeping from 1982 onwards
# Why NAs in 2018?

#####
# Exercise: What would happen if you did a right join instead?
#####
pm.right <- print_media %>%
  right_join(aus_economy, by = "Year")  # What is different?
# Why NAs before 1982?
#####
# Exercise: What would happen if you did an inner join instead?
#####
pm.inner <- print_media %>%
  inner_join(aus_economy, by = "Year")  # What is different?
# Where did 2018 go?
#####

# Calculate turnover in "real" terms (inflation-adjusted)
pm <- pm %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) 
pm
# The adjusted turnover is in 2010 Australian dollars, as CPI is 100 in 2010 in this data set.

# Manipulate the data set to make plotting easier
# We can use the the pivot_longer function from tidyr
pm <- pm %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               names_to = "Type", values_to = "Turnover") 
pm  # What is this function doing?

# Now plot the nominal vs real turnover
ggplot(data = pm, 
       mapping = aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(x = "Year", y = NULL)


#####
# Box-Cox transformations
#####

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))
food %>%
  autoplot(Turnover) + 
  lab("Turnover (Millions of AUD)")
# Notice the multiplicative seasonality and nonlinear trend

# What happens if we transform the data?

# Square-root
food %>%
  autoplot(sqrt(Turnover)) + 
  ylab("Square root turnover")
# What has changed?

# Cube-root
food %>%
  autoplot(Turnover^(1/3)) + 
  ylab("Cube root turnover")
# What has changed?

# Logarithm
food %>%
  autoplot(log(Turnover)) + 
  ylab("Log turnover")

# Box-Cox
food %>%
  features(Turnover, 
           features = guerrero)

# Find optimal Box-Cox parameter
lambda <- food %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)  # What is the pull function?

# Plot data with optimal Box-Cox parameter
food %>% 
  autoplot(box_cox(Turnover, lambda))
