#Question 1
#Set your directory by following Session --> Set working directory...

data <- readLines("assignment3-stats380.html")

#Answer
head(data)


#Comment: To represent the data as what we wanted, I used readLines() to bring HTML file into R
#And the result creates character vectors.






#Question 2

#returns the line which contains "<table" pattern in our data
tablelines <- grep("<table", data)

#returns the line which contains "/table" patter in our data
endtablelines <- grep("/table", data)

#binds two elements by columns and as a result, it creats a matrix
tables <- cbind(tablelines, endtablelines)

#Answer
tables






#Question 3

#'spann' finds lines where it contains the pattern we want to extract.
#The pattern searchs the string where it has "<span>" and any characters one or more after that. 
#And the string should be finished by "</span".
spann <- grep("<span>.+</span", data)

#After finding the indices of lines, we want to find the data where it contains such information
new <- data[spann]

#getting rid of any lines before the data values, which is any characters plus "<span>" itself.
first <- gsub(".+<span>", "", new)

#Replace "Missing" values by a space
second <- gsub("Missing.+", " ", first)

#getting rid of any lines after the data values, which is "</span>" itself and any characters afterwards.
third <- gsub("</span>.+$", "", second)

#In case the lines contain unneccesry information (i.e. the lines starting by "<tr." or "<table", etc.), we get rid of them 
values <- gsub("<tr.+|</tr.+|<table.+|<table.+", "", third)

#Answer
head(values)






#Question 4
#Creating a matrix based on what we have. It lines up the values by row and we have 4 columns
mat <- matrix(values, byrow = TRUE, ncol = 4)

#Changing into data frame
matform <- data.frame(mat)

#Giving the name of columns
colnames(matform) <- c("Class","Age","Sex","Status")

#Changing the type of 2nd column into numeric. To make it easy R to identify the columns, 
#I initially converted 2nd column as a character and then changed them into numerics.
matform$Age <- as.numeric(as.character(matform$Age))

#Answer
head(matform)
class(matform$Age)





#Question 5 

#split() function splits the matrix by the categories of 'Class' columns
new_mat <- split(matform, matform$Class)

#grepl() checks if each row of 'Status' in Class='1st' group contains "died" or not.
#The result of grepl() is a logical vector (i.e. True or False) and the sum of its results
#returns the sum of number of True (i.e. each True worth 1, otherwise 0).
#This represents the number of death in '1st' group
#The total number of rows represents the total population.  
death_one <- sum(grepl("died", new_mat$`1st`$Status))/nrow(matform)

#death_two and death three were applied by the same logics.
death_two <- sum(grepl("died", new_mat$`2nd`$Status))/nrow(matform)
death_three <- sum(grepl("died", new_mat$`3rd`$Status))/nrow(matform)

#death rate of '1st' group
death_one
#death rate of '2nd' group
death_two
#death rate of '3rd' group
death_three