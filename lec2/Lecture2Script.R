################################################################################
# Basic Operations
first_vec = 1:5
first_vec

0:10
c(0,3,2)
first_vec + 0.5

first_vec
6:10
first_vec + 6:10


first_vec
6:9
first_vec + 6:9

1.4:3.5

c(1,2,5,12,1.4)
c("Blue", "Beige", "Black", "Brown")

2^(1:4)
1 + 2:4

110:130 %% 60
120 %% 15:20


################################################################################
# Data Types: Character
my_name    = "Jiaxi Li"
first_name = "Jiaxi"
last_name  = "Li"

class(my_name)

class(first_vec)

length(first_vec)
length(6:9)

my_name
length(my_name)

nchar(my_name)
my_name

c(first_name,last_name)
nchar(c(first_name,last_name))

also_my_name = 'Jiaxi Li' # We are using ' instead of " here
my_name == also_my_name

Lowercase_my_name = 'jiaxi li'
my_name == Lowercase_my_name

first_name
last_name
paste(first_name,last_name)
paste(first_name,last_name,sep="-")
paste0(first_name,last_name)
paste(first_name,last_name,sep="\'")
c(first_name,last_name)
c(first_name,last_name) == paste(first_name,last_name)
first_name + " " + last_name

my_name
print(my_name)

# Things inside of functions/loop are considered non-interactive contexts
nchar(my_name)
nchar(print(my_name))
for (i in 1) {my_name}
for (i in 1) {print(my_name)}

toupper(c(first_name,last_name))
tolower(c(first_name,last_name))


################################################################################
# Data Types: Numeric
tolower(2)

class(Inf)
class(-Inf)
class(NaN)

################################################################################
# Data Types: Logical
R_is_fun  = TRUE
R_is_hard = FALSE
R_is_fun  == T
R_is_fun  == true

2 > 1
2 > 2
2 >= 2

1 > 2
1 > 1/2
(1 > 2) & (1 > 1/2)
(2 > 1) & (1 > 1/2)
(1 > 2) | (1 > 1/2)

0.5 == 1/2
3 != 2


!TRUE
!FALSE

2 %in% c(1,2,3,4)
5 %in% c(1,2,3,4)    # test if 5 is in the vector c(1,2,3,4)
!(5 %in% c(1,2,3,4)) # test if 5 is NOT in the vector c(1,2,3,4)


class(NA)
NA+1
is.na(NA)
is.nan(NA)
is.na(NaN) # This is a little strange, but good to know

################################################################################
# Data Types: Complex
i
3i


################################################################################
# Data Structures: Vector
numeric_grades = c(90,75,95,85,100,60,76)
letter_grades  = c("A-","C","A","B","A","D","C")
mixed_grades   = c("A", 95,"B",85,"C",75)

names(numeric_grades) = c("Student 1","Student 2","Student 3",
                          "Student 4","Student 5","Student 6",
                          "Student 7")


some_numbers = c(27,22,94)
some_numbers[1]

################################################################################
# Data Structures: List
TestList = list(c("Jiaxi","Li"),1:5,sample(c(TRUE,FALSE),20,replace=T))

TestList[[1]]
TestList

# some examples we worked through in class:
names(TestList) = c("my_name","numbers","logicals")
TestList[1][1]
TestList[1][[1]]
big_list <- list(list(c("Jiaxi","Li"),1:5,sample(c(TRUE,FALSE),20,replace=T)),c(1,2))
big_list[[1]]
big_list[1]
big_list[[1]][2]
#

names(letter_grades) = paste("Student",1:length(letter_grades))
grade_list = list(names(numeric_grades),numeric_grades,letter_grades)
grade_list

names(grade_list) = c("studentNames","numericGrade","letterGrade")

grade_list[["letterGrade"]]

grade_list["letterGrade"]


################################################################################
# Data Structures: Matrix
num_mat = matrix(1:9,ncol=3)
num_mat2 = matrix(1:9,ncol=3,byrow = T)
colnames(num_mat) = paste("Col",1:ncol(num_mat))
rownames(num_mat) = paste("Row",1:nrow(num_mat))
num_mat
num_mat2

################################################################################
# Data Structures: Data.frame
class(mtcars)

head(mtcars)

names(mtcars)

# add make/model to the data frame as a column

mtcars$makemodel = rownames(mtcars)

rownames(mtcars) = NULL

summary(mtcars$mpg)

mpgvec = mtcars$mpg
testvec <- mtcars[mpgvec > median(mpgvec),]

################################################################################
# Data Structures: Factors
fact_groups = letters[sample(1:26,10,replace=T)]
fact_groups2 = sample(letters,10,replace=T) # two ways of writing the same command
fact_groups = factor(fact_groups,levels=letters)
fact_groups = factor(fact_groups,levels=rev(letters))
fact_groups = factor(fact_groups, levels=c("a","e","i","o","u","y"))
# why are we getting NAs? 


