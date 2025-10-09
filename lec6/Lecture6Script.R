################################################################################
# Pipeline

## These next two lines of code do exactly the same thing.
# subset cars from the manufacturer audi and calculate the mean hwy for every model
mpg |> subset(manufacturer=="audi") |> aggregate(hwy ~ model, FUN = mean)
aggregate(subset(mpg, manufacturer=="audi"), hwy ~ model, FUN = mean)

# pipeline with a vertical view
mpg |>
  subset(manufacturer=="audi") |>
  aggregate(hwy ~ model, FUN = mean)


################################################################################
# Dates and Times

## Convert to date with base function
as.Date("2021-09-14")
Sys.Date()

# Different formats
as.Date("09/14/2021",format="%m/%d/%Y")
as.Date("2021/09/14",format="%Y/%m/%d")
as.Date("2021.09.14",format="%Y.%m.%d")


## Lubridate
library(lubridate)

# Convert to date using lubridate functions
ymd("2021-09-14")
mdy("09/14/2021")
today()

# Extract a specific feature of the date
year(today())
month(today())
week(today())

# More detailed date
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd("2021-09-14") > ymd("2021-08-12")
ymd("2021-09-14") < "2021-08-12"

# R can understand comparison between characters formatted as Date
"2021-09-14" > "2021-08-12"
"2021-09-14" < "2021-08-12"

################################################################################
# Read/Write Files
# Get current working directory
getwd()

# Check files and folders in a new working directory
file_vec <- list.files()
file_vec

# Use the two lines below to load the data from my Github page and save it locally
# OTC_data = read.csv("https://github.com/JiaxiLi1995/ECON370/raw/refs/heads/main/lec6/blp_data.csv")
# write.csv(OTC_data, file = "blp_data.csv", row.names = F)
# Read from your local file
OTC_data = read.csv("blp_data.csv")
write.csv(OTC_data, file = "blp_data2.csv", row.names = F)

# rds files
saveRDS(OTC_data, file = "blp_data.rds")
OTC_data_rds = readRDS("blp_data.rds")

# feather files
library(arrow)
write_feather(OTC_data, "OTC_data.feather", compression = T)
OTC_data_feather = read_feather("OTC_data.feather")

# fst files
library(fst)
write_fst(OTC_data, "OTC_data.fst")
OTC_data_fst = read_fst("OTC_data.fst")


# Check the speed of saving and reading
library(microbenchmark)
# Benchmark reading speed
read_bench = microbenchmark(
  rds_read = readRDS("blp_data.rds"),
  csv_read = read.csv("blp_data.csv"),
  feather_read = read_feather("OTC_data.feather"),
  fst_read = read_fst("OTC_data.fst"),
  times = 10
)

print(read_bench)

# Benchmark saving speed
save_bench = microbenchmark(
  rds_save  = saveRDS(OTC_data, "blp_data.rds"),
  csv_save = write.csv(OTC_data, "blp_data.csv", row.names = F),
  feather_save = write_feather(OTC_data, "OTC_data.feather"),
  fst_save = write_fst(OTC_data, "OTC_data.fst"),
  times = 10
)

print(save_bench)


################################################################################
# Web Scraping
library(rvest)
library(dplyr)
library(janitor)

# read in the web page from wikipedia
raw_wiki <- read_html("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression")
raw_wiki
class(raw_wiki)

# Only take the tables
raw_wiki |> html_elements("table")
# Only take the table.wikitable
raw_wiki |> html_elements("table.wikitable")

# Use html_table to view
table_dfs <- raw_wiki |>
  html_elements("table.wikitable") |> 
  html_table()
table_dfs[[1]]

# Clean up the table
table_dfs_int <- raw_wiki |>
  html_elements("table.wikitable") |> 
  html_table()

table_dfs <- lapply(table_dfs_int[c(1,3,4)], # drop unwanted tables
                    function(x) x |>
                      clean_names() |> ## fix colnames, from the janitor package #<<
                      mutate(date = mdy(date))) ## from lubridate
table_dfs[[1]]

# Combine the three table
wr100 <- rbind(
  table_dfs[[1]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Pre-IAAF"),
  table_dfs[[2]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Pre-automatic"),
  table_dfs[[3]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Modern")
)
head(wr100)



################################################################################
# Regular Expressions
statecountry_names = c('Florida','Germany','Georgia','Geniva',
                       'Istanbul','NewZealand','Australia')

# Find the location where character elements in a vector contain a specific letters/string
grep("A",statecountry_names)
grep("A",statecountry_names, ignore.case=T)

# Logical comparison whether each character elements in a vector contain a specific letters/string
grepl("G",statecountry_names)
grepl("G",statecountry_names,  ignore.case=T)

# Replace the first occurrence
sub("G","A",statecountry_names)
# Replace all occurrence
gsub("G","A",statecountry_names)


# Regular expression with grepl
test_char = c("The beginning.","The end.","The middle.","Other.")
grepl("^The",test_char)
grepl("end.$",test_char)
grepl("[aeiou]",letters)

# When the special character is in the string, use \\ before the special character
test_char2 = c("^$The beginning.","in ^The end.","The middle.","Other.")
grepl("^\\^\\$The", test_char2)

# Subsetting with grepl
c(letters,1:9)[grepl("[aeiou0-9]",c(letters,1:9))]

# Match several strings using |
grepl("beginning|end",test_char)

# Numbers in a string
grepl("[0-9]",c(1:5,"I had 2 coffees today","Hello World!"))


# Use regular expressions to get only one type of files
list.files(
  path = "D:/R/ECON370/lec6",
  pattern = ".*\\.csv$",
  full.names = TRUE
)



################################################################################
# Misc Functions
# Find unique elements
letter_samp = sample(letters,12,replace=T)
letter_samp
unique(letter_samp)

# sort and order the nunbers, sum and create sequence
norm_draws = rnorm(10)
norm_draws
sort(norm_draws)
order(norm_draws)
sum(norm_draws)
seq(1,2.4,by=0.1)

my_string = "Hello, This is Econ 370, and my name is Jiaxi"
# Use substr to subset elements in a string
substr(my_string,1,10)
# Use strsplit to split a string using a character
strsplit(my_string,",")


