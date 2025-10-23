################################################################################
# data.table setup
# Install the packages if needed and load them
if (!require(pacman)) install.packages('pacman', 
                                       repos = 'https://cran.rstudio.com')
pacman::p_load(dplyr, data.table, dtplyr, tidyfast, 
               microbenchmark, ggplot2, nycflights13)


# speed comparison: tidyverse vs. data.table
collapse_dplyr = function() {
  storms |>
    group_by(name, year, month, day) |> 
    summarize(wind = mean(wind), pressure = mean(pressure), category = dplyr::first(category))
}

storms_dt = as.data.table(storms)
collapse_dt = function() {
  storms_dt[, .(wind = mean(wind), pressure = mean(pressure), category = first(category)),
            by = .(name, year, month, day)]
}

microbenchmark(collapse_dplyr(), collapse_dt(), times = 1)




################################################################################
# data.table basics
# data(starwars, package = "dplyr") ## Optional to bring the DF into the global env
# Summarizing by group with dplyr
starwars |> 
  filter(species=="Human") |> 
  group_by(homeworld) |> 
  summarise(mean_height=mean(height))


# convert to data.table and save it separately
starwars_dt = as.data.table(starwars)
# Summarizing by group with data.table
starwars_dt[species=="Human", mean(height), by=gender]



################################################################################
## Row manipulation
# Subset by rows (filter)
starwars_dt[height>190 & species=='Human']


# Order by rows (arrange)
starwars_dt[order(birth_year)]  ## (temporarily) sort by youngest to oldest
starwars_dt[order(-birth_year)] ## (temporarily) sort by oldest to youngest
setorder(starwars_dt, birth_year, na.last = TRUE)
starwars_dt[1:5, name:birth_year] ## Only print subset



################################################################################
## Column manipulation
# Modifying columns := (mutate)
# create a new data.table
DT = data.table(x = 1:2)
# mutate new column (x_sq) and display the data.table
DT[, x_sq := x^2][]

# Shallow copy!
DT_copy = DT
# remove a column of the copy by assigning to NULL
DT_copy[, x_sq := NULL]
# Check what happened to the original DT!
DT

# mutate new column (x_sq) again
DT[, x_sq := x^2]
# Deep copy!
DT_copy = copy(DT)
# remove a column of the copy by assigning to NULL
DT_copy[, x_sq := NULL]
# Check what happened to the original DT!
DT ## x_sq is still there


# create a new data.table
DT2 = data.table(a = -2:2, b = LETTERS[1:5])
# In dplyr, you cannot do subassign, but rather assign a new vector 
DT2 |>
  mutate(b = ifelse(a < 0, NA, b))
# In data.table, you can subassign and it is faster
DT2[a < 0, b := NA][]


# Mutate multiple variables with mutate
DT |>
  mutate(y = 3:4,
         y_name = c("three", "four"))
# Is DT changed by mutate?
DT

# Mutate multiple variables with :=
DT[, ':=' (y = 3:4,
           y_name = c("three", "four"))]
# Is DT changed by :=?
DT


# Another way to create multiple variables
DT2[, c("newvar1", "newvar2") := .(a+3,a*2)][]


# How about dynamic assignment?
DT[, ':=' (z = 5:6, z_sq = z^2)][]
# What can we do?
# Chain the operations instead
DT[, z := 5:6][, z_sq := z^2][] 
# Pipeline with |> and _ as placeholder
DT |>
  _[, xyz := x+y+z] |>
  _[, xyz_sq := xyz^2] |>
  _[] 



## Subsetting on columns (select)
starwars_dt[1:2, c(1:3, 10)]
# starwars_dt[, c("name", "height", "mass", "homeworld")] ## Also works
# starwars_dt[, list(name, height, mass, homeworld)] ## So does this
starwars_dt[1:2, .(name, height, mass, homeworld)]

# keep all except a few columns
starwars_dt[, !c("name", "height")]
starwars_dt[, -c("name", "height")]


# Change names with setnames (rename)
setnames(starwars_dt, old = c("name", "homeworld"), 
         new = c("alias", "crib"))[1:2,]
# Better change it back, in case we use "name" or "homeworld" on a later slide
setnames(starwars_dt, old = c("alias", "crib"), 
         new = c("name", "homeworld"))[1:2,]

# (temporally) subsetting and renaming at the same time
starwars_dt[1:2, .(alias = name, crib = homeworld)]
# Is the original data modified?
starwars_dt[1:2,]

# data.table and dplyr used together
starwars_dt[1:5, ] |> 
  select(crib = homeworld, everything())




## Aggregating (summarize)
# Note that this is copy and modify, the original starwars_dt is not modified
starwars_dt[, mean(height, na.rm=T), by = species]
starwars_dt[, .(species_height = mean(height, na.rm=T)), by = species]

# Create a new column in original starwars_dt storing the mean_height
starwars_dt[, mean_height := mean(height, na.rm=T)] |> ## Add mean height as column
  _[1:5, .(name, height, mean_height)] ## Just to keep everything on the slide

# Check the numbers of observations
starwars_dt[, .N]



## Group (group_by)
# Note that in aggregating, we already used by argument
starwars_dt[, .(mean_height = mean(height, na.rm=T)), by = .(species, homeworld)] |>
  head(4)

# Efficient subsetting with .SD
starwars_dt[, 
            lapply(.SD, mean, na.rm=T),
            .SDcols = height:mass, #c("height", "mass", "birth_year"),
            by = species] |> 
  head(2) ## Just keep everything on the slide

# Use lapply to modify all columns
DT <- DT[, x:x_sq]
DT[, lapply(.SD, mean)]



## Key
## First create a keyed version of the storms data.table.
## Note that key variables match the 'by' grouping variables below.
storms_dt_key = as.data.table(storms, key = c("name", "year", "month", "day"))
# Same dplyr function as before
collapse_dplyr = function() {
  storms |>
    group_by(name, year, month, day) |> 
    summarize(wind = mean(wind), pressure = mean(pressure), category = dplyr::first(category))
}
## Collapse function for this keyed data.table. Everything else stays the same.
collapse_dt_key = function() {
  storms_dt_key[, .(wind = mean(wind), pressure = mean(pressure), category = first(category)), 
                by = .(name, year, month, day)]
}
## Run the benchmark on all three functions.
microbenchmark(collapse_dplyr(), collapse_dt(), collapse_dt_key(), times = 1)




################################################################################
# Merging datasets
# library(nycflights13) ## Already loaded
flights_dt = as.data.table(flights) 
planes_dt = as.data.table(planes)
airlines_dt = as.data.table(airlines)
airports_dt = as.data.table(airports)


# dplyr with left_join
left_join(
  flights, 
  planes, 
  by = "tailnum"
)

# data.table with merge
merge(
  flights_dt, 
  planes_dt, 
  all.x = TRUE, ## omit for inner join
  by = "tailnum")


# We are encountering the name conflicts again
# Need to change variable name "year" from at least one dataset
setnames(planes_dt, old = "year", new = "year_built")
merge(
  flights_dt,
  planes_dt,
  all.x = TRUE, 
  by = "tailnum")


# There is a slight difference between left_join and merge
# Merge will reorder the left data by the key
# Left_join will not reorder data
# If you want to achieve merge but no reordering, you need option "sort = F"
# setnames(planes_dt, old = "year", new = "year_built")
merge(
  flights_dt,
  planes_dt,
  all.x = TRUE, 
  by = "tailnum",
  sort = F)


# For Tampa Intl airport (Tampa Intl) on Jan 1, 2013, figure out: 
# Q1. Which carrier flew the most flights into Tampa?
# Q2. How many Airbus (AIRBUS) planes did JetBlue (JetBlue Airways) fly into Tampa? 
# Q3. What manufacturers did Delta (Delta Air Lines Inc.) fly into Tampa?



# answer: 
# Create the data for Tampa on Jan 1, 2013
setnames(airlines_dt, old = "name", new = "carrier_name")
Tampa_dt_2013_01_01 = flights_dt |>
  # join flights with planes
  merge(
    planes_dt[, !"year_built"],
    all.x = TRUE,
    by="tailnum",
    sort = F) |>
  # join with airports
  merge(airports_dt,
        all.x = TRUE,
        by.x = "dest",
        by.y = "faa",
        sort = F) |> 
  # join with airlines
  merge(airlines_dt,
        all.x = TRUE,
        by = "carrier",
        sort = F) |>
  # subset to airport and day of interest
  _[(year==2013 & day == 1 & month == 1 & name == "Tampa Intl")]

# Q1: carrier flew the most flights into tampa? 
Tampa_dt_2013_01_01[, .N, by="carrier_name"][order(-N),]

# Q2. How many Airbus (AIRBUS) planes did JetBlue (JetBlue Airways) fly into Tampa Intl? 
Tampa_dt_2013_01_01[carrier_name == "JetBlue Airways" & manufacturer == "AIRBUS", .N]

# Q3. What manufacturers did Delta (Delta Air Lines Inc.) fly into Tampa?
Tampa_dt_2013_01_01[carrier_name == "Delta Air Lines Inc.", .N, by = manufacturer]



################################################################################
# reshaping data
# Create some random stock data with X Y Z for pivoting
stocks = data.table(time = as.Date('2009-01-01') + 0:1,
                    X = rnorm(2, 0, 1),
                    Y = rnorm(2, 0, 2),
                    Z = rnorm(2, 0, 4))

## Reshape from wide to long
melt(stocks, id.vars ="time")

## Reshape from wide to long with defined names
# Use melt to pivot_wider
stocks_long = melt(stocks, id.vars ="time", 
                   variable.name = "stock", value.name = "price")
stocks_long

# Use dt_pivot_longer to pivot_wider
stocks |> 
  dt_pivot_longer(X:Z, names_to="stock", values_to="price")

## Reshape from long to wide
# Use dcast to pivot_longer
dcast(stocks_long, 
      time ~ stock, 
      value.var = "price")
# Use dt_pivot_wider to pivot_longer
stocks_long |> 
  dt_pivot_wider(names_from=stock, 
                 values_from=price)


################################################################################
# use dtplyr package lazy_dt
# library(dtplyr)
# convert data to lazy_dt and then use dplyr syntax
lazy_dt(starwars) |>
  filter(species=="Human") |>
  group_by(gender) |>
  summarise(height = mean(height, na.rm=TRUE))


