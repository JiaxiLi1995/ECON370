################################################################################
# A simple data frame
d = data.frame(x = 1:2, y = 3:4) 
class(d)

# Convert to matrix
m = as.matrix(d)
# Our previous factor
fact_groups = sample(letters,10,replace=T)
# Covert to other types. Do they work?
as.integer(fact_groups)
as.character(d)

# Different ways to conduct lm
lm(y~x,data=d)
lm(d$y~d$x)

# A new data frame!
d2 = data.frame(x = rnorm(10), y = runif(10))

# We can do lm for both
lm(y~x,data=d)
lm(y~x,data=d2)

# Summary here shows more details of lm result
# Do you remember what we get from summary with `summary(mtcars$mpg)`?
summary(lm(y~x,data=d2))
# Try ?summary to found out

################################################################################
# Some naming problems
#TRUE = 1

pi = function(q,p,mc){
  q*p - q*mc
}

rm(pi)

c(1,2,3)
c = 2

library(dplyr)
library(data.table)
library(ggplot2)

filter = stats::filter


################################################################################
# Cleaning up
A = 1
B = 2
D = 3
# I do not use C and D here, you can check why by ?C and ?D

# rm(list = setdiff(ls(), c("keep_this", "and_this")))  # remove all but a few key objects
rm(list = setdiff(ls(), c("A")))  # remove all but A
gc()                                                  # free unused RAM

