# Reproducible Research Fundamentals 
# Learning and Playing with data table


#Load package 

install.packages("data.table")

# latest development version (only if newer available)
data.table::update_dev_pkg()

# latest development version (force install)
install.packages("data.table", repos="https://rdatatable.gitlab.io/data.table")

# load the package
library(data.table)

#Mock data for the exercise

# load a built-in dataset
data("iris")

# print the first 5 rows
head(iris)

# transform the data.frame into a tibble
data_table <- data.table::as.data.table(iris)

# print the first 5 rows
head(data_table)

#A data.table is a data.frame 

# show the class of the object
class(data_table)

# 1 create a data.table from scratch
data <- data.table(x = 1:100)

# 2 load a data.table from a file (csv, txt, zip and URLs)
data <- fread("data.csv")

# 3 coerce an existing data.frame into a data.table
data_table <- as.data.table(data_frame)

# 4 coerce an existing data.frame into a data.table by "reference"; i.e. you don't have to (re)assing it 
setDT(data_frame)

data_table[1:3, ] # select the first 3 rows
# select rows where Species equals setosa
data_table[Species == "setosa", ]

# we can also avoid to type the comma when we only have a subsetting
data_table[Species == "setosa"  ] 

# select rows where Species equals setosa and Sepal.Length > 5
data_table[Species == "setosa" &
               Sepal.Length >= 5, ] 

data_table[order(Sepal.Width), ]          # order according to the Sepal.Width column, in ascending order

setorder(data_table, Sepal.Width, -Species) # order according to the Sepal.Width and Species columns, in ascending and descending order, respectively

data_table[sample(nrow(data_table), 3)] # randomly sample three rows

data_table[sample(.N, 3), ]             # randomly sample three rows
# select three columns 
data_table <- data_table[, list(Sepal.Length, Sepal.Width, Species)] # add a "-" for dropping instead

# We can also use the alias `.` which is the equivalent of `list()` in datatable
data_table <- data_table[, .(Sepal.Length, Sepal.Width, Species)] # add a "-" for dropping instead

# list of names of columns to be selected
cols <- c("Sepal.Length", "Sepal.Width", "Species")

# select three columns 
data_table <- data_table[, ..cols] # "!.." instead for dropping 

# select three columns 
data_table <- data_table[, c(1:2, 4)] # add a "-" in front for dropping instead

data_table[, 
           .(Sepal.Length.mean   =   mean(Sepal.Length, na.rm = TRUE), 
             Sepal.Length.median = median(Sepal.Length, na.rm = TRUE),
             Sepal.Length.min    =    min(Sepal.Length, na.rm = TRUE),
             Sepal.Length.max    =    max(Sepal.Length, na.rm = TRUE))
]

# add a column with the mean of Sepal.Length
data_table[, Sepal.Length.mean := mean(Sepal.Length, na.rm = TRUE)]

cols <- c(
    "Sepal.Length.mean",
    "Sepal.Length.median",
    "Sepal.Length.min",
    "Sepal.Length.max"
)

data_table[, (cols) := .(
    mean(Sepal.Length, na.rm = TRUE),
    median(Sepal.Length, na.rm = TRUE),
    min(Sepal.Length, na.rm = TRUE),
    max(Sepal.Length, na.rm = TRUE))
][
    1:3, c(6:9)
]


data_table[Species == "setosa", Treatment := TRUE][sample(.N, 5), c(5, 10)]

data_table[, .N, by =    Species ] # one column
data_table[, .N, by = .( Species ,  Treatment ) ] # multiple columns
data_table[, .N, by = c("Species", "Treatment") ] # multiple columns

data_table[, .N, by = is.na(Treatment)]
