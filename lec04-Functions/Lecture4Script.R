################################################################################
# Motivation
# Create a data.frame with three variables
# x is from uniform distribution, y is from normal distribution and z is from chi squared distribution
d = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))

# Now, let's try to scale each variable according to x = (x-min(x))/(max(x)-min(x))
d$x = (d$x - min(d$x,na.rm=TRUE))/(max(d$x, na.rm=TRUE) - min(d$x, na.rm=TRUE))

# Do the same for all x, y and z, but oops, something went wrong
d$x = (d$x - min(d$x,na.rm=TRUE))/(max(d$x, na.rm=TRUE) - min(d$x, na.rm=TRUE))
d$y = (d$y - min(d$x,na.rm=TRUE))/(max(d$y, na.rm=TRUE) - min(d$y, na.rm=TRUE))
d$z = (d$z - min(d$z,na.rm=TRUE))/(max(d$z, na.rm=TRUE) - min(d$z, na.rm=TRUE))







################################################################################
# Basic Function
# Create a function where the output is the same as input
# Remember to use ? to make sure it is not a defined function
?return_input
return_input = function (x) {
  x #return the input as output
}

# I would recommend use return to specify what is returned
# This will be required for this class
return_input = function (x) {
  return(x) #return the input as output
}

# Create a function for Pythagorean Theorem hypotenuse calculation
# The inputs are base and height
# Check function name
?Hypotenuse
Hypotenuse = function(a,b){
  # a^2 + b^2 = c^2
  # sqrt(a^c+b^2) = c
  return(sqrt(a^2+b^2))
}

# Try the calculation
Hypotenuse(3, 4)
# Try calculation with vectors
Hypotenuse(1:5,2:6)
Hypotenuse(3,1:5)

# What is wrong with this?
Hypotenuse(3:5,1:5) #don't do this


# Create the weighted average function: x is the value, w is the weight
wt_mean = function(x,w){
  return(sum(x*w)/sum(w))
}

# Try calculation
wts = runif(20)
wt_mean(1:20,wts)

# If you change the order of argument, you must tell computer what they are
wt_mean(wts, 1:20)
wt_mean(w = wts, x=1:20)


# Use weighted mean for simple mean
wt_mean(1:20, rep(1, length(1:20)))
# Check whether the result are the same
mean(1:20) == wt_mean(1:20, rep(1, length(1:20)))


# Simple addition but have y with default value 2
test_fun = function(x, y=2){
  return(x+y)
}
test_fun(3)
test_fun(3, 5)

# Default in default
# ? normalize
# normalize data by subtracting mean and divide by standard deviation
normalize = function(x, m = mean(x),s = sd(x)){
  return((x - m)/s)
}
normalize(c(1:10,NA))

# want to make mean and standard deviation dealing with NA
normalize = function(x, m = mean(x,na.rm=na.rm),s = sd(x,na.rm=na.rm),na.rm=FALSE){
  return((x - m)/s)
}
normalize(c(1:10,NA))
# Why it still returns all NA?

normalize(c(1:10,NA),na.rm=TRUE)




# Add the default argument for the weights
wt_mean = function(x,w=rep(1,length(x))){
  # Description: Takes the weighted average of x using weights w
  # Default w is a vector of 1s the same length as x.
  sum(x*w)/sum(w)
}

# Check whether the result are the same
wt_mean(1:20) == mean(1:20)


# Try different length of x and w
wt_mean(1:20,wts[-1])
wt_mean(1:20,c(0.1,0.2,0.3,0.3))
# What are the problems?


################################################################################
### Discussion of if-else logic flow
# We can change the value of x and rerun the code to see what it does
x = -2
# Print only if x is negative
if(x < 0){
  print("x is less than 0")
}


# Generate a random x (from uniform distribution)
x = runif(1,-1,1)
x
# Print different message for x with different signs
if(x > 0){
  print("x is positive!")
} else if(x < 0){
  print("x is negative!")
} else{
  print("x is 0!")
}

# Generate grade around 85
grade = 85 + rnorm(1,sd=5)
# Use if condition to find corresponding letter grade
# Put all if else conditions together
if (grade >= 90) {
  cat("A",round(grade),"is an A")
} else if (grade >= 80) {
  cat("A",round(grade),"is a B")
} else if (grade >= 70) {
  cat("A",round(grade),"is a C")
} else if (grade >= 60) {
  cat("A",round(grade),"is a D")
} else{
  cat("A",round(grade),"is an F")
}

# # Problem code
# statement = F
# if(statement)
# {print("It's True!")
# }
# else
# {print("It's False!")}

statement = F
# Condition statements
if(statement)
{print("It's True!")
}else
{print("It's False!")}

vals = c(F,T)
# Check how R deal with vectors in if
if(vals){
  print("TRUE!")
}

# mod 2 with ifelse
x = 1:10
# Return even/odd for corresponding numbers
ifelse(x %% 2 == 0, "Even","Odd")

# Exercise: print out "the remainder is r" for the integers 1 to 10 where r is the remainder when dividing by 3.
# For remainder 0, print "divisible".
x = 1:10
r = x %% 3 # calculate remainders
ifelse(r == 0 , "divisible", paste("remainder is", r)) # print differently for 0 and others


# Exercise: a vector from 1 to 100, only prints "value is x" when divisible by 10.
x = 1:100
ifelse(x%%10 == 0 , paste("value is", x), "")


# Stop will stop the code and provide an error message
# The rmd document cannot be knitted if an error occurs (unless set error=T)
x = 0
if(x>0){
  print(1/x)
}else if(x<0){
  print(-1/x)
}else{stop("You cannot divide by 0!")}

# This is an warning message
warning("This is a warning!")
# This is a regular message
message("This is a message!")

# Example of stop stopping all code below
stop("I will stop all code below!")
print("Not Stopped!")


# weighted mean but x and w must have same length
wt_mean = function(x,w=rep(1,length(x))){
  if(length(x)!=length(w)){
    stop("x and w must be the same length")
  }
  sum(x*w)/sum(w)
}



################################################################################
# Advanced Function Features
# Return vs. Print
plus_delta = function(x,delta=1){
  # Show what we are doing
  print(paste0("We are adding ", delta, " to ", x, "!"))
  # Return the numerical value
  return(x + delta)
}

plus_delta(5)
plus_delta(4.5,0.75)

# Things in function after return will not run
plus_delta = function(x,delta=1){
  # Return the numerical value
  return(x + delta)
  # Show what we are doing
  print(paste0("We are adding ", delta, " to ", x, "!"))
}

plus_delta(5)
plus_delta(4.5,0.75)


# Purely updating function
x = 0
# Updating function, no input or output
Add1_to_x = function(){
  x <<- x+1
} #notice, nothing is being returned either!!

Add1_to_x()
x
Add1_to_x()
x

# print vs. cat
# Using cat to print but nothing is returned as output
x = cat("Hello cat!")
x

# Two cat printed, but in same line
cat("Hello cat!")
cat("Hello cat!")

# Two cat printed, but in different line
cat("Hello cat1!\n")
cat("Hello cat2!\n")

# Using print to print and the input itself is returned as output
x = print("Hello print!")
x

# Two print printed, always in different line
print("Hello print!")
print("Hello print!")


# Define a function to calculate tax based on income
# 1. 10% below 10k
# 2. 20% between 10k and 40k
# 3. 30% above 40k
tax_bracket = function(income) {
  if (income <= 10000) {
    return(0.1 * income) # 10% below 10k
  } else if (income <= 40000) {
    return(0.1 * 10000 + 0.2 * (income - 10000)) # 20% between 10k and 40k
  } else {
    return(0.1 * 10000 + 0.2 * 30000 + 0.3 * (income - 40000)) # 30% above 40k
  }
}

# Test the tax_bracket function
tax_bracket(8000)
# tax_bracket(25000)
# tax_bracket(60000)

# Define a function to calculate tax based on income, also effective tax rate
tax_bracket <- function(income) {
  if (income <= 10000) {
    amount = 0.1 * income # 10% below 10k
  } else if (income <= 40000) {
    amount = 0.1 * 10000 + 0.2 * (income - 10000) # 20% between 10k and 40k
  } else {
    amount = 0.1 * 10000 + 0.2 * 30000 + 0.3 * (income - 40000) # 30% above 40k
  }
  return(list(amount = amount, effective_rate = amount/income)) 
}

# Test the tax_bracket function
tax_bracket(8000)
# tax_bracket(25000)
# tax_bracket(60000)

################################################################################
# Functions which Generate Randomness
# Use sample() to draw from a vector
# Draw 10 letters with replacement
sample(letters, 10, replace = T)

# Draw 10 numbers from 1 to 10 with replacement
sample(1:10, 10, replace = T)

# Draw 10 numbers from 1 to 10 without replacement
sample(1:10, 10, replace = F)

# Draw 3 rows from cars dataset without replacement
cars[sample(1:nrow(cars), 3, replace = F),]


# Check whether the data are the same without set.seed
d1 = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))
d2 = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))

d1 == d2

# Check whether the data are the same with set.seed
set.seed(2025)
d1 = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))
set.seed(2025)
d2 = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))

d1 == d2

