################################################################################
# tidyverse setup
# Install the packages if needed. (This takes a while)
install.packages(c('tidyverse', 'nycflights13'), repos = 'https://cran.rstudio.com', dependencies  = TRUE)

# Load the core tidyverse packages
library(tidyverse)

# View all tidyverse packages
tidyverse_packages()


################################################################################
# dplyr
# library(dplyr)
# Note that we will use pipeline extensively from now on
glimpse(starwars)

## filter
# filter using logical
starwars |> 
  filter( 
    species == "Human",
    height >= 190
  )

# base R version:
starbool = starwars$species == "Human" & starwars$height >= 190
starbool

# Note that we need to remove the NA's as well
# You can use replace_na in dplyr!
starwars[replace_na(starbool, F),]


# Regular expression to keep rows
starwars |> 
  filter(grepl("Skywalker", name))

# Filter out missing data
starwars |> 
  filter(is.na(height))



## arrange
# arrange items in ascending order
starwars |> 
  arrange(birth_year)
# arrange items in descending order
starwars |> 
  arrange(desc(birth_year))



## select
# select with : and -
starwars |> 
  select(name:skin_color,species,-height)

# select while change names
starwars |> 
  select(alias=name, crib=homeworld, sex=gender)
starwars |> 
  select(name, homeworld, gender) |>
  rename(alias=name, crib=homeworld, sex=gender) 

# You can also use character vectors
starwars |> 
  select(c("name", "homeworld", "gender"))


# Use select containing some words
starwars |> 
  select(name, contains("color"))

# Use select to rearrange variables
starwars |> 
  select(species, homeworld, everything()) |>
  head(5)



## mutate
# Use mutate to create a num variable dog_years and character variable comment
starwars |> 
  select(name, birth_year) |>
  mutate(dog_years = birth_year * 7) |>
  mutate(comment = paste0(name, " is ", dog_years, " in dog years."))


# mutate is order aware
starwars |> 
  select(name, birth_year) |>
  mutate(
    dog_years = birth_year * 7, ## Separate with a comma
    comment = paste0(name, " is ", dog_years, " in dog years.")
  )


# The first five rows of starwars (for comparison)
starwars |> 
  head(5)

# mutate all character variables (to upper case)
starwars |> 
  select(name:eye_color) |> 
  mutate(across(where(is.character), toupper)) |> #<< 
  head(5)

# mutate all numerical variables with customized functions (from x to x+1)
starwars |> 
  mutate(
    across(
      where(is.numeric),
      function(x){
        return(x+1)
      }
    )
  ) |>
  head(5)

# You can also predefine the function and use it in mutate
add_one = function(x) x+1
# add_one = function(x) {return(x+1)}
starwars |> 
  mutate(across(where(is.numeric), add_one)) |>
  head(5)



## summarize
# summarize with each group of species/gender
starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE), .groups = "drop")

# Without na.rm=T
starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height), .groups = "drop")

# Without .groups = "drop"
starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE)) |>
  ungroup()

# multiple summarized values 
starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE),
            min_height = min(height, na.rm=TRUE),
            max_height = max(height, na.rm=TRUE))

# summarize with across
starwars |> 
  group_by(species) |> 
  summarize(across(where(is.numeric), function(x) mean(x, na.rm=T))) |> #<<
  head(5)


## Other useful dplyr functions
starwars |>
  slice(c(1, 5))

starwars |> 
  filter(gender=="feminine") |> 
  pull(height)

unique(starwars$gender)
n_distinct(starwars$gender)

starwars |>
  distinct(gender)

starwars |>
  count(gender)



################################################################################
# Join with dplyr
# Load the package for the data demonstration
library(nycflights13)

# Load data
data(flights)
data(planes)
data(airlines)
data(airports)

# Check data
flights |>
  distinct(dest)

head(flights)
head(planes)
head(airlines)
head(airports)


# first combine flights and planes using a left join
firstjoin = left_join(flights, planes)

# Something wrong, we can check where engines does not exist
firstjoin |>
  filter(is.na(engines)) |>
  nrow()
nrow(firstjoin)

# The year variables in flights and planes have different meanings!
# Check https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf
# We should not join by year
left_join(flights,
          planes,
          by="tailnum") |>
  filter(is.na(engines)) |>
  nrow()

# Alternative ways to do it
left_join(flights,
          planes,
          by=join_by(tailnum)) |>
  filter(is.na(engines)) |>
  nrow()

# Rename year to avoid confusion, better practice
left_join(
  flights |> rename(flight_year = year), 
  planes |> rename(year_built = year),
  by=join_by(tailnum)
  ) |>
  filter(is.na(engines)) |>
  nrow()


# When joining by one variable but with different names in different data
Tampa_flights = left_join(
  flights, 
  airports,
  by=join_by(dest == faa)
  ) |>
  filter(name == "Tampa Intl")

# The alternative is to rename before joining
Tampa_flights = left_join(
  flights, 
  airports |> rename(dest = faa),
  by=join_by(dest)
) |>
  filter(name == "Tampa Intl")



# For Tampa Intl airport (Tampa Intl) on Jan 1, 2013, figure out: 
# Q1. Which carrier flew the most flights into Tampa?
# Q2. How many Airbus (AIRBUS) planes did JetBlue (JetBlue Airways) fly into Tampa? 
# Q3. What manufacturers did Delta (Delta Air Lines Inc.) fly into Tampa?



# answer: 
# Create the data for Tampa on Jan 1, 2013
Tampa_2013_01_01 = flights |>
  # join flights with planes
  left_join(
    planes |> select(-year),
    by="tailnum") |>
  # join with airports
  left_join(airports,
            by = join_by(dest==faa)) |> 
  # join with airlines
  left_join(airlines |> rename(carrier_name = name),
            by = "carrier") |>
  # subset to airport and day of interest
  filter(year==2013,
         day == 1,
         month == 1,
         name == "Tampa Intl")

# Q1: carrier flew the most flights into tampa? 
Tampa_2013_01_01 |>
  group_by(carrier_name) |>
  count() |>
  arrange(desc(n))

# This also works
# Tampa_2013_01_01 |>
#   count(carrier_name) |>
#   arrange(desc(n))

# Q2. How many Airbus (AIRBUS) planes did JetBlue (JetBlue Airways) fly into Tampa Intl? 
Tampa_2013_01_01 |>
  filter(carrier_name == "JetBlue Airways",
         manufacturer == "AIRBUS") |>
  nrow()

# Q3. What manufacturers did Delta (Delta Air Lines Inc.) fly into Tampa?
Tampa_2013_01_01 |>
  filter(carrier_name == "Delta Air Lines Inc.") |>
  distinct(manufacturer)



################################################################################
# tidyr


## pivot_longer
# Create some random stock data with X Y Z for pivoting
stocks = data.frame( ## Could use "tibble" instead of "data.frame" if you prefer
  time = as.Date('2009-01-01') + 0:1,
  X = rnorm(2, 0, 1),
  Y = rnorm(2, 0, 2),
  Z = rnorm(2, 0, 4)
)

stock_vol = data.frame(
  X_vol = rep(1,2),
  Y_vol = rep(2,2),
  Z_vol = rep(4,2)
)

# Combine and rename
stocks2 = cbind(stocks,
                stock_vol) |> 
  rename(X_price = X,
         Y_price = Y,
         Z_price = Z)

# Make the data longer by converting all stocks into one column
tidy_stocks = stocks |> 
  pivot_longer(X:Z,
               names_to = "stock",
               values_to = "returns")

# Another way is to specify the columns to keep using "-"
tidy_stocks = stocks |> 
  pivot_longer(-time,
               names_to = "stock",
               values_to = "returns")

# pivot_longer and separating
# more info: https://stackoverflow.com/questions/69798752/pivot-longer-for-multiple-sets-having-the-same-names-to
stocks2_long = stocks2 |> 
  pivot_longer(-time, 
               names_sep = "_",
               names_to = c("stock",".value"))



## pivot_wider
# Make the data wider by make every stock its own column
tidy_stocks |> 
  pivot_wider(names_from = "stock",
              values_from = "returns")

# Also can make the data wider by make every date its own column
tidy_stocks |> 
  pivot_wider(names_from = "time",
              values_from = "returns")



## separate
economists = data.frame(name = c("Adam.David.Smith", "Paul.David.Samuelson", "Milton.David.Friedman"))
economists

# There are three parts of each name here
economists |>
  separate(name, c("first_name","middle_name", "last_name"))
# gives warning
economists |> 
  separate(name, c("first_name", "last_name"))


## unite
# Create gdp data
gdp = data.frame(
  yr = rep(2016, times = 4),
  mnth = rep(1, times = 4),
  dy = 1:4,
  gdp = rnorm(4, mean = 100, sd = 2)
)

# unite year,month,day into date
gdp |>
  unite(date,
        c("yr", "mnth", "dy"),
        sep = "-") |>
  as_tibble() |>
  # convert to date
  mutate(date = ymd(date))



## Others: crossing
crossing(grade = c("Freshmen","Sophomore","Junior","Senior"),
         subject = c("Econ","Math","English"))

# Similar to expand.grid (in lec5)
expand.grid(c("Freshmen","Sophomore","Junior","Senior"),
            c("Econ","Math","English"))


