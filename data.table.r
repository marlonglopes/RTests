DT[i, j, by]

Take DT, subset rows using i, then calculate j grouped by by

# DT and the data.table package are pre-loaded

# Print the second to last row of DT using .N
DT[.N - 1]

# Print the column names of DT
colnames(DT)

# Print the number or rows and columns of DT
dim(DT)

# Select row 2 twice and row 3, returning a data.table with three rows where row 2 is a duplicate of row 1.
DT[c(2, 2, 3)]


# iris is already available in your workspace

# Convert iris to a data.table: DT
DT <- as.data.table(iris)

# For each Species, print the mean Sepal.Length
DT[, mean(Sepal.Length), by = Species]

# Print mean Sepal.Length, grouping by first letter of Species
DT[, mean(Sepal.Length), by = substr(Species, 1, 1)]


# data.table version of iris: DT
DT <- as.data.table(iris)

# Group the specimens by Sepal area (to the nearest 10 cm2) and count how many occur in each group.
DT[, .N, by = 10 * round(Sepal.Length * Sepal.Width / 10)]

# Now name the output columns `Area` and `Count`
DT[, .(Count = .N), by = .(Area = 10 * round(Sepal.Length * Sepal.Width / 10))]  

# Create the data.table DT
set.seed(1L)
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8))

# Create the new data.table, DT2
DT2 <- DT[, .(C = cumsum(C)), by = .(A, B)]

# Select from DT2 the last two values from C while you group by A
DT2[, .(C = tail(C, 2)), by = A]

# The data.table DT is loaded in your workspace

# Perform chained operations on DT

    DT[, .(Sepal.Length = median(Sepal.Length), Sepal.Width=  median(Sepal.Width), Petal.Length =  median(Petal.Length), Petal.Width= median(Petal.Width)) , by = Species][order(-Species)]

    # Print out the new data.table DT
DT

# Calculate the sum of the Q columns
DT[, lapply(.SD, sum), .SDcols = 2:4]

# Calculate the sum of columns H1 and H2 
DT[, lapply(.SD, sum), .SDcols = paste0("H", 1:2)]

# Select all but the first row of groups 1 and 2, returning only the grp column and the Q columns
DT[, .SD[-1], by = grp, .SDcols = paste0("Q", 1:3)]

# DT is pre-loaded

# Sum of all columns and the number of rows
DT[, c(lapply(.SD, sum), .N), by=x, .SDcols=c("x", "y", "z")]

# Cumulative sum of column x and y while grouping by x and z > 8
DT[, lapply(.SD, cumsum), by = .(by1 = x, by2 = z > 8), 
   .SDcols = c("x", "y")]

# Chaining
DT[, lapply(.SD, cumsum), by = .(by1 = x, by2 = z > 8), 
   .SDcols = 1:2][, lapply(.SD, max), by = by1, .SDcols = c("x", "y")]


# The data.table DT
DT <- data.table(A = letters[c(1, 1, 1, 2, 2)], B = 1:5)

# Add column by reference: Total
DT[, Total := sum(B), by = A]

# Add 1 to column B
DT[c(2, 4), B := B + 1L]

# Add a new column Total2
DT[2:4, Total2 := sum(B), by = A]

# Remove the Total column
DT[, Total := NULL]

# Select the third column using `[[`
DT[[3]]
DT[[3L]] ## Also works

# A data.table DT has been created for you
DT <- data.table(A = c(1, 1, 1, 2, 2), B = 1:5)

# Update B, add C and D
DT[, `:=`(B = B + 1, C = A + B, D = 2)]

# Delete my_cols
my_cols <- c("B", "C")
DT[, (my_cols) := NULL]

# Delete column 2 by number
DT[, 2 := NULL]


# Set the seed
set.seed(1)

# Check the DT that is made available to you
DT

# For loop with set
for (i in 2:4) set(DT, sample(10, 3), i, NA)

# Change the column names to lowercase
setnames(DT, tolower(names(DT)))

# Print the resulting DT to the console
DT

# Define DT
DT <- data.table(a = letters[c(1, 1, 1, 2, 2)], b = 1)

# Add the suffix "_2" to all column names
setnames(DT, paste0(names(DT), "_2"))

# Change column name a_2 to A2
setnames(DT, "a_2", "A2")

# Reverse the order of the columns
setcolorder(DT, 2:1)


# The 'keyed' data.table DT
DT <- data.table(A = letters[c(2, 1, 2, 3, 1, 2, 3)], 
                 B = c(5, 4, 1, 9, 8, 8, 6), 
                 C = 6:12)
setkey(DT, A, B)

# Select the "b" group
DT["b"]

# "b" and "c" groups
DT[c("b", "c")]

# The first row of the "b" and "c" groups
DT[c("b", "c"), mult = "first"]

# First and last row of the "b" and "c" groups
DT[c("b", "c"), .SD[c(1, .N)], by = .EACHI]

# Copy and extend code for instruction 4: add printout
DT[c("b", "c"), { print(.SD); .SD[c(1, .N)] }, by = .EACHI]


# Keyed data.table DT
DT <- data.table(A = letters[c(2, 1, 2, 3, 1, 2, 3)], 
                 B = c(5, 4, 1, 9, 8, 8, 6), 
                 C = 6:12, 
                 key = "A,B")

# Get the key of DT

key(DT)

# Row where A == "b" and B == 6

DT[.("b", "6")]

# Return the prevailing row

DT[.("b", "6"), roll=TRUE]


# Return the nearest row

DT[.("b", "6"), roll = "nearest"]

# Keyed data.table DT
DT <- data.table(A = letters[c(2, 1, 2, 3, 1, 2, 3)], 
                 B = c(5, 4, 1, 9, 8, 8, 6), 
                 C = 6:12, 
                 key = "A,B")

# Print the sequence (-2):10 for the "b" group
DT[.("b", (-2):10)]

# Add code: carry the prevailing values forwards
DT[.("b", (-2):10), roll = TRUE]

# Add code: carry the first observation backwards
DT[.("b", (-2):10), roll = TRUE, rollends = TRUE]
DT[.("b", (-2):10), roll = TRUE, rollends = c(TRUE, TRUE)] # also OK
