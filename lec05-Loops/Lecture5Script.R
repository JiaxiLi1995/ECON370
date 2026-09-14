################################################################################
# Loops

## For loop
# First for loop, adding from 1 to 10
# Preset the variable
sum_val = 0
# Use for loop to add one number at a time
for(i in 1:10){
  sum_val = sum_val + i
}
sum_val

# Compare with built in R function sum
sum_val == sum(1:10)

# Loop over character vectors
# Here is the vector name list
Names_vec = c("James", "Julia", "John", "Jasmine", "Jack")

# Loop over character vectors
for (Name in Names_vec) {
  # Use cat to combine and print
  cat("UNC-", Name, "\n", sep = "")
}

# We can also loop over list...
# Create a simple list
my_list <- list(
  numbers = c(1, 2, 3),
  letters = c("A", "B", "C"),
  mixed = list(name = "Jiaxi", number = 1:4)
)

# Loop over each element in the list
for (item in my_list) {
  print(item)
}

# What does it do?
# Answer: this is similar to loop over the names of the list

# Advanced sum with for loop
# Preset the variable
val = 0
# Loop over a
for(A in 1:20){
  # Loop over b
  for(B in 1:15){
    val = val + (exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B))
  }
}
val


# Preallocation
# calculating squares of 1 to 1000 with and without preallocation
N          = 10000 # number of iterations
my_vec     = c(NA) # No Preallocation
my_vec_pre = rep(NA,N) # Preallocation

# Use for loop to assign new values to my_vec without preallocation
# Each iteration increases the size of my_vec.
for(i in 1:N){
  my_vec[i] = i^2
}

# We can also use 1:N here, but length() is more flexible in some situations
for(i in 1:length(my_vec_pre)){
  my_vec_pre[i] = i^2
}

all.equal(my_vec, my_vec_pre) # compare results

# Use microbenchmark to test the speed of no preallocation vs. preallocation
library(microbenchmark) 
mbm = microbenchmark(
  "no_preal"={
    my_vec     = c(NA)
    for(i in 1:N){
      my_vec[i] = i^2
    }},
  "preal"={
    my_vec_pre = rep(NA,N)
    for(i in 1:length(my_vec_pre)){
      my_vec_pre[i] = i^2
    }},times=1000)
mbm






## while loop
# Add number from 1 to 10 using while loop
# Preset the variable
sum_val = 0
# Set a counter
i = 1

# Use while loop, keep adding while n is less or equals 10
while(i <= 10){
  sum_val = sum_val + i
  i   = i + 1 #update the counter
}

# Compare with built in R function sum
sum_val == sum(1:10)


# Example: Fixed Point
eps   = .Machine$double.eps #set tolerance 
x_n   = 5000                #starting guess

# Keep looping until x_n is sufficiently close to sqrt(x_n)
while(abs(x_n - sqrt(x_n)) > eps){
  x_n = sqrt(x_n) #update guess
}
x_n


# A different starting guess
x_n   = 0.00001             #starting guess

# Keep looping until x_n is sufficiently close to sqrt(x_n)
while(abs(x_n - sqrt(x_n)) > eps){
  x_n = sqrt(x_n) #update guess
}
x_n


# Finance Application: Stock Returns
# use tidyquant to extract stock from the end of 2017
library(tidyquant)

# Get daily S&P 500 data (or Global Standard & Poor's Composite, symbol: ^GSPC)
SP500_monthly = tq_get("^GSPC",
                       from = "2017-12-29",
                       to = "2025-08-29",
                       get  = "stock.prices") |>
  # only keep the data at the end of each month
  tq_transmute(select = adjusted,         # use adjusted closing price
               mutate_fun = to.monthly,   # collapse to monthly
               indexAt = "lastof",        # use last day of month for date
               col_rename = "price") |>   # create new name for the column
  as.data.frame() # change to data frame since tq_get would produce a tibble

head(SP500_monthly)

# Calculate the return as a new column in the data.frame
# Preallocate as a new variable in data.frame, set value as NA
# Try not to set value at 0 if 0 has meanings
SP500_monthly$Return = NA

# Use for loops from the second index to the last
for (i in 2:nrow(SP500_monthly)) {
  SP500_monthly$Return[i] = SP500_monthly$price[i]/SP500_monthly$price[i-1] - 1
}

head(SP500_monthly, n=3)

# Why there is an NA at the beginning?
# What would happen if you preallocated using 0?




# Print the progress every 20th iteration
for (i in 1:100) {
  # Make the system stop for 0.05 second
  Sys.sleep(0.05)
  # Print the progress
  if (i %% 20 == 0) cat("Completed", i, "iterations out of 100\n")
}

# Let's try auto print
for (i in 1:100) {
  # Make the system stop for 0.05 second
  Sys.sleep(0.05)
  # Print the progress
  if (i %% 20 == 0) paste("Completed", i, "iterations out of 100")
}


# For the previous fixed-point example, stop if we reach MaxIt numbers of iterations
x_n   = 50000  #starting guess
i     = 1      #initialize counter
MaxIt = 100000 # max iteration

while(abs(x_n - sqrt(x_n)) > eps){
  x_n = sqrt(x_n)   #update x_n
  i     = i + 1     #increase counter
  
  # Stop the code if it takes too many iterations
  if(i >= MaxIt) stop("Did not find fixed point!")
  # Have a message if the loop stops due to condition met
  if(abs(x_n - sqrt(x_n)) <= eps) message(paste("The estimated fix point is approximately", x_n, "with", i, "iterations."))
}


# Finding fixed-point for f(x)=2x, stop if we reach MaxIt numbers of iterations
x_n   = 0.0001 #starting guess
i     = 1      #initialize counter
MaxIt = 1000   #fix max iterations

while(abs(x_n - 2*x_n) > eps){
  x_n = 2*x_n   #update x_n
  i     = i + 1     #increase counter
  
  # Stop the code if it takes too many iterations
  if(i >= MaxIt) stop("Did not find fixed point!")
  # Have a message if the loop stops due to condition met
  if(abs(x_n - 2*x_n) <= eps) message(paste("The estimated fix point is approximately", x_n, "with", i, "iterations."))
}


# Skip some lines
# An alternative to the previous approach by stepping to a random direction and only keep the favorable steps.
x_n   = 0.0001 #starting guess
i     = 1      #initialize counter
MaxIt = 1000   #fix max iterations
UpdateStep = 0.000002   #make the UpdateStep relatively small

while(abs(x_n - 2*x_n) > eps){
  x_temp = x_n + UpdateStep*sample(c(-1,1), 1) # have the temporary x that is x updated with a random step
  i = i + 1 #increase counter
  
  # Stop the code if it takes too many iterations
  if(i >= MaxIt) stop("Did not find fixed point!")
  if(abs(x_temp - 2*x_temp) <= eps) message(paste("The estimated fix point is", x_temp, "with", i, "iterations."))
  
  # skip updating x if it not in favorable direction
  if(abs(x_temp - 2*x_temp) >= abs(x_n - 2*x_n)) next
  
  x_n = x_temp # update x_n
}


# tryCatch
# Use tryCatch to suppress error and print a message instead
# We can save the index and come back later
Warning_ind = NULL
for (i in 1:5) {
  tryCatch({
    if (i == 2) stop("Error at i=2")
    if (i %in% 3:4) warning("Warning at i=3 and i=4")
  }, error = function(e) {
    message("Skipping error at i=", i)
  }, warning = function(e) {
    Warning_ind <<- c(Warning_ind, i) # Append index to Warning_ind
  })
}

Warning_ind


################################################################################
# Apply

# Create a random matrix with rnorm. Remember to use set.seed.
set.seed(42)
rand_mat = matrix(rnorm(3*2),ncol=3)
rand_mat

# Use apply function to operate the matrix with margin
apply(rand_mat,1,sum)
apply(rand_mat,2,sum)


# Create a list to apply functions
my_list = list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
my_list

# calculate the mean of each object in the list with lapply
lapply(my_list, mean)

# sapply default returns a vector or matrix
sapply(my_list, mean)


# We can use a function whose output is a vector
# lapply quantile function and `probs = (1:3)/4` is the argument for quantile
lapply(my_list, quantile, probs = (1:3)/4)

# sapply default returns a vector or matrix
sapply(my_list, quantile)

# For data.frame or list, we do not need margin
data(mtcars)
sapply(mtcars,summary)


# Draw 10 random numbers with rnorm. Repeat this 6 times and save into a matrix. Do not forget to set seed
set.seed(42)
Rep_matrix = replicate(4, rnorm(6))
Rep_matrix

# Draw 6*4 random numbers with rnorm and then reshape it into a matrix. Do not forget to set seed
set.seed(42)
Reshaped_matrix = matrix(rnorm(6*4), ncol = 4)

all.equal(Rep_matrix, Reshaped_matrix)


################################################################################
# Vectorization

# Compare the for loop and vector operation
mbm = microbenchmark(
  "loop"={
    N   = 10000    #set size of vector
    out = rep(0, N) #preallocate vector
    for(i in 1:N){
      out[i] = i^2  #fill in vector with square of index
    }},
  "vectorized"={
    N   = 10000    #set size of vector
    out = 1:N       #preallocate vector
    out = out^2     #square each index
  },times=1000)
mbm


# "Vectorize a matrix". Now, aANDb contains all pairs of a and b
aANDb = expand.grid(A=1:20,B=1:15)
A     = aANDb$A
B     = aANDb$B
# Run the sum on all pairs of a and b
sum((exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B)))

# Test the speed of loop vs. vectorized advanced sum
mbm = microbenchmark(
  "loop"={
    val = 0
    for(A in 1:20){
      for(B in 1:15){
        val = val + (exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B))}}},
  "vectorized"={
    aANDb = expand.grid(A=1:20,B=1:15)
    A     = aANDb$A
    B     = aANDb$B
    sum((exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B)))
  },times=1000)
mbm
mean(mbm[mbm$expr=="loop","time"])/mean(mbm[mbm$expr=="vectorized","time"])


# Create a data.frame with three variables
# Set seed due to randomness
set.seed(2025)
# x is from uniform distribution, y is from normal distribution and z is from chi squared distribution
d = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))

# We can use apply to demonstrate here since it is basically a loop
apply(d, 2, mean)
# colMeans is a function for matrix
colMeans(d)


# Track speed
# Create a large numeric matrix
set.seed(123)
big_mat = matrix(rnorm(1e7), nrow = 1000, ncol = 10000)  # 1000 x 10000

benchmark_result = microbenchmark(
  base_colMeans = colMeans(big_mat),   # Optimized base R function
  apply_mean = apply(big_mat, 2, mean),# General-purpose apply
  times = 10                           # Repeat each test 10 times
)

# Print detailed timing results
print(benchmark_result)


################################################################################
# Parallelization with future.apply

# The library is called future.apply
library(future.apply)

# Detect the number of cores on your laptop
parallel::detectCores()  # from base R's parallel package

plan(multisession, workers = 4)  # use 4 cores

my_list = list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))

# future_lapply(my_list, mean)

all.equal(unlist(lapply(my_list, mean)), unlist(future_lapply(my_list, mean)))



# Compare speed between loop, apply, and future.apply
library(future.apply)
library(microbenchmark)

# Use 4 cores for parallelization (adjust as needed)
plan(multisession, workers = 4)

# Easier heavy task: generate 1 million random numbers and sort them
heavy_task = function(n = 1e6) {
  x = runif(n)     # generate random numbers between 0 and 1
  sort(x)           # sort them in increasing order
  return(max(x))    # return the maximum (just to have an output)
}

B = 20  # number of replications (increase for bigger tests)

# 1. For loop
for_loop = function() {
  out = numeric(B)
  for (i in 1:B) {
    out[i] = heavy_task()
  }
  out
}

# 2. Apply (sequential)
apply_seq = function() {
  sapply(1:B, function(i) heavy_task())
}

# 3. Future apply (parallel)
apply_par = function() {
  future_sapply(1:B, function(i) heavy_task(), future.seed = 123)
}

# Benchmark them
microbenchmark(
  for_loop(),
  apply_seq(),
  apply_par(),
  times = 3
)


################################################################################
# Dynamic Tasks

# Simulate values for an AR(1) model
# Set parameters, number of period N, set seed for randomness
rho       = 0.5
delta     = 2
N         = 1000
set.seed(42)

# Preallocate and set first value at 0
AR1_ts    = rep(NA,N)
AR1_ts[1] = 0
# Use for loop updating remaining values
for(i in 2:N){
  AR1_ts[i] = delta + rho*AR1_ts[i-1] + rnorm(1)
}

plot_data = data.table(y=AR1_ts,x=1:N)
ggplot(plot_data,aes(x=x,y=y))+geom_line()


# AR(3) example, but only want the final value after 1000 iterations
# Set parameters, number of period N, set seed for randomness
rho1      = 0.3
rho2      = 0.2
rho3      = 0.1
delta     = 2
N         = 1000
set.seed(42)

# set pre_value at 0 for the three most recent history (t-1, t-2, t-3)
AR1_ts_pre = rep(0,3)
# Use for loop updating N times
for(i in 1:N){
  AR1_ts = delta + rho1*AR1_ts_pre[1] + rho2*AR1_ts_pre[2] + rho3*AR1_ts_pre[3] + rnorm(1)
  # Then, we need to update the most recent 3 values
  AR1_ts_pre = c(AR1_ts, AR1_ts_pre[-length(AR1_ts_pre)])
}
AR1_ts


# Inner loop dependency, hard to do without loop
# Preset val
val = 0
# loop over a
for(A in 1:20){
  # loop over b, notice the inner loop is from 1 to a (depends on the value of a)
  for(B in 1:A){
    val = val + (exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B))
  }
}
val

# "Vectorize a matrix". Now, aANDb contains all pairs of a and b
aANDb = expand.grid(A=1:20,B=1:15)
# Use logical index to select b<=a
aANDb_depend = aANDb[aANDb$B<=aANDb$A, ]
A     = aANDb_depend$A
B     = aANDb_depend$B
# Run the sum on all pairs of a and b
sum((exp(sqrt(A))*log(A^5))/(5+cos(A)*sin(B)))
