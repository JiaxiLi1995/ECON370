################################################################################
# Basic Operations
# Here is out first vector
first_vec = 1:5
first_vec

# Try using : to create vector
0:10
# concatenate numbers to make a vector
c(0,3,2)
# Add single number to a vector, what does that do?
# How about adding a vector to a number?
first_vec + 0.5

# Now, add a vector to a vector of same length
first_vec
6:10
first_vec + 6:10

# What happens if two vectors added have different length?
first_vec
6:9
first_vec + 6:9

# What would be the result of this one?
1.4:3.5

# `c` can be used to create other type of vectors. More in Lecture 2.
c(1,2,5,12,1.4)
c("Blue", "Beige", "Black", "Brown")

# other ways to work with vector
2^(1:4)
1 + 2:4

110:130 %% 60
120 %% 15:20


################################################################################
# Data Types: Character
my_name    = "Jiaxi Li"
first_name = "Jiaxi"
last_name  = "Li"

# `class` can check the data type
class(my_name)

class(first_vec)

# Now, check the length of the numeric vector
length(first_vec)
length(6:9)

# Same length function can work for character (vector) too
# What does it do?
my_name
length(my_name)

# How about nchar
nchar(my_name)
my_name

# Now concatenate two char (vectors)
c(first_name,last_name)
nchar(c(first_name,last_name))

# cat is to concatenate and print
cat(first_name,last_name)

# `==` can be used to compare things, more about this in logic data type section
also_my_name = 'Jiaxi Li' # We are using ' instead of " here
my_name == also_my_name

Lowercase_my_name = 'jiaxi li'
my_name == Lowercase_my_name

# pasting characters
first_name
last_name
paste(first_name,last_name)
paste(first_name,last_name,sep="-")
paste0(first_name,last_name)
paste(first_name,last_name,sep="\'")
c(first_name,last_name)
c(first_name,last_name) == paste(first_name,last_name)
first_name + " " + last_name

# Auto printing vs. forced printing
my_name
print(my_name)

# Things inside of functions/loop are considered non-interactive contexts
nchar(my_name)
nchar(print(my_name))
for (j in 1) {my_name}
for (j in 1) {print(my_name)}

# Change to upper case and lower case
toupper(c(first_name,last_name))
tolower(c(first_name,last_name))


################################################################################
# Data Types: Numeric
tolower(2)

# Some special numeric values
class(Inf)
class(-Inf)
class(NaN)

################################################################################
# Data Types: Logical
R_is_fun  = TRUE
R_is_hard = FALSE
R_is_fun  == T
R_is_fun  == true

# Math comparison
2 > 1
2 > 2
2 >= 2

# `& |` operations
1 > 2
1 > 1/2
(1 > 2) & (1 > 1/2)
(2 > 1) & (1 > 1/2)
(1 > 2) | (1 > 1/2)

# `!` for negation
0.5 == 1/2
3 != 2


!TRUE
!FALSE


# `%in%` checks whether a single element is in a vector
2 %in% c(1,2,3,4)
5 %in% c(1,2,3,4)    # test if 5 is in the vector c(1,2,3,4)
!(5 %in% c(1,2,3,4)) # test if 5 is NOT in the vector c(1,2,3,4)

# NA vs. NaN
class(NA)
NA+1
is.na(NA)
is.nan(NA)
is.na(NaN) # This is a little strange, but good to know

################################################################################
# Data Types: Complex
# We will rarely use it
i
3i
1+2i
# i is the imaginary i, if you put a number in front of it

################################################################################
# Data Structures: Vector
# Now, formally working with vector using `c`
numeric_grades = c(90,75,95,85,100,60,76)
letter_grades  = c("A-","C","A","B","A","D","C")
mixed_grades   = c("A", 95,"B",85,"C",75)

# Add names to vector, I rarely use this
names(numeric_grades) = c("Student 1","Student 2","Student 3",
                          "Student 4","Student 5","Student 6",
                          "Student 7")

# Indexing
some_numbers = c(27,22,94)
some_numbers[1]

################################################################################
# Data Structures: List
# What is the difference between vector and list?
TestList = list(c("Jiaxi","Li"),1:5,sample(c(TRUE,FALSE),20,replace=T))

# Indexing for list
TestList[[1]]
TestList

# some examples we worked through in class:
names(TestList) = c("my_name","numbers","logicals")
TestList[1][1]
TestList[1][[1]]
big_list <- list(list(c("Jiaxi","Li"),1:5,sample(c(TRUE,FALSE),20,replace=T)),c(1,2))
big_list[[1]]
big_list[1]
# Sequential indexing
big_list[[1]][2]

names(letter_grades) = paste("Student",1:length(letter_grades))
grade_list = list(names(numeric_grades),numeric_grades,letter_grades)
grade_list

# names for list
names(grade_list) = c("studentNames","numericGrade","letterGrade")
# names can be used as index!
grade_list[["letterGrade"]]

grade_list["letterGrade"]


################################################################################
# Data Structures: Matrix
# Do you notice how R make a vector into matrix?
num_mat = matrix(1:9,ncol=3)
num_mat2 = matrix(1:9,ncol=3,byrow = T)
# Names for row and col
colnames(num_mat) = paste("Col",1:ncol(num_mat))
rownames(num_mat) = paste("Row",1:nrow(num_mat))
num_mat
num_mat2

################################################################################
# Data Structures: Data.frame
class(mtcars)

head(mtcars)

names(mtcars)

# Usually, we do not really want a row name.
# Instead, we want a separate column storing the names
# add make/model to the data frame as a column
mtcars$makemodel = rownames(mtcars)

# Remove the row names
rownames(mtcars) = NULL

# Five number summary
summary(mtcars$mpg)

mpgvec = mtcars$mpg
# Row selecting and indexing with data frame
testvec <- mtcars[mpgvec > median(mpgvec),3]

################################################################################
# Data Structures: Factors
fact_groups = letters[sample(1:26,10,replace=T)]
fact_groups2 = sample(letters,10,replace=T) # two ways of writing the same command
# Now, convert as factors
fact_groups = factor(fact_groups,levels=letters)
fact_groups = factor(fact_groups,levels=rev(letters))
# Factor with specified levels
fact_groups = factor(fact_groups, levels=c("a","e","i","o","u","y"))
# why are we getting NAs? 


