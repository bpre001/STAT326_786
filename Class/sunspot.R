library(tidyverse)
library(fpp3)

# Read in monthly sunspot data
data <- as_tsibble(sunspot.month)

# Visualise
data %>%
  autoplot(value)

# Annual seasonality?
data %>%
  gg_season(value) + 
  ylab("Monthly Sunspot Count") +
  xlab("Month")

data %>%
  gg_subseries(value) + 
  ylab("Monthly Sunspot Count") +
  xlab("Year")

# Doesn't appear to have annual seasonality
# Why would the sun have repeating patterns based on Earth's orbit?


# Look at annual sunspots instead
data <- data %>%
  mutate(year = year(index)) %>%
  index_by(year) %>%
  summarise(total = sum(value))

# Is there seasonality or a cycle?
data %>%
  autoplot(total, colour = "pink") + 
  theme_minimal() + 
  ylab("Annual Sunspot Count") + 
  xlab("Year")

# Apply a transformation to reduce variation
data <- data %>%
  mutate(sqrt.total = sqrt(total))

data %>%
  autoplot(sqrt.total, colour = "pink") + 
  theme_minimal() + 
  ylab(expression(sqrt("Annual Sunspot Count"))) + 
  xlab("Year")


#####
# Extra: Spectral analysis of sunspots - not assessed
#####

# Which frequencies to keep?
n = length(data$sqrt.total)
if (n %% 2 == 0) {  # Even
  keep = 1:(n / 2 - 1)
} else {  # Odd
  keep = 1:((n - 1) / 2)
}

# Compute periodogram
pdgrm = abs(fft(data$sqrt.total)[keep]) ^ 2
# The Fourier transform converts a time series into a frequency series
# Assumes signals can be represented as infinite sum of sines and cosines
# Periodogram is an empirical spectral density
# This tells you how much power is present in the signal at each frequency

# Define frequencies (1 measurement per year)


# Create data
data = tibble(pdgrm = pdgrm,
              f = f)

# Visualise
ggplot(data = data,
       mapping = aes(x = f, y = log(pdgrm))) + 
  geom_line() +
  labs(x = "Frequency (Cycles per Year)",
       y = "log(Peridogram)") + 
  geom_vline(xintercept = f[25], linetype = "dashed")  # Max occurs at 25th freq

# Best guess of cycle duration
1 / f[25]  # Every 11ish years

# Note this is not a statistically valid approach
# Usually do peridogram smoothing to get a "consistent" estimator for spectrum
# Outside scope of this course, but interesting nonetheless
