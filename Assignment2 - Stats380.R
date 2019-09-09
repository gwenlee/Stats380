#Question 1(a)(i)

x<-c(1,2,3,4)
y<-c(4,1,2,3)

C_fun <- function(x,y,u,v){
  n <- length(x) #Set the variable n
  r <- rank(x)   #Set the variable r
  s <- rank(y)   #Set the variable s
  
  #if... condition checks if u and v are in the range between 0 and 1
  if (u <= 1 & u >= 0 & v <= 1 & v >= 0)      
    I <- (r/(n+1) <= u) & (s/(n+1) <= v)  #Checking the given condition
  else
    print("Wrong input for either u or v")
  
  c = (1/n)*sum(I)    
  #Computing the summation, it didn't need to create to input I as 1 because 
  #the default function gives True=1, False=0
  print(c)
}

C_fun(x,y,0.5,0.5)



#Question 1(a)(ii)

C_fun(x,y,c(0.4,0.5,0.3,0.2),c(0.4,0.5,0.3,0.2))

#Comment: If we bring a vector for u and v, the function is not working
#Instead, we can write down a new function

#My inputs x,y,u,v
x<-c(1,2,3,4)
y <-c(4,1,2,3)
u <- c(0.5,0.5,0.5,0.5)
v <- c(0.5,0.5,0.5,0.5)


C_fun2 <-function(x,y,u,v){
  
  n <- length(x)
  r <- rank(x)
  s <- rank(y)
  
  fraction_r <- outer(r, n+1, FUN="/") #Created fraction for r/(n+1) by using 
                                       #outer function() (i.e. matrix)
  fraction_s <- outer(s, n+1, FUN="/") #Created fraction for s/(n+1) by using 
                                       #outer function() (i.e. matrix)
  
  I <- (fraction_r <= u & fraction_s <= v)
  c = (1/n)*sum(I)  
  #Computing the summation, it didn't need to create to input I as 1 because 
  #the default function gives True=1, False=0
  print(c)
  
}

C_fun2(x,y,u,v)




#Question 1(b)
n = 5

delta = 1:n
phi = 0.5


new_fun <- function(phi,delta){
  apply(outer(cumsum(c(0,delta)), cumsum(c(0,delta)), 
              Vectorize(function(a,b){round(phi^abs(a-b),4)})), 1, 
        function(phi){(1/(1-phi^2))})}

new_fun(phi,delta)



#Comment:
#We have two main arguments as phi and delta, phi is a number between 0 and 1
#And delta takes the value of n and creates the sequence.
#there are two main parts in this code, which is apply() and outer() function.
#I used apply() to multiply the part of  (1/(1-phi^2)) fraction to each of matrices. 

#Outer() function was necessary to create a matrix that we have so I created (n+1)x(n+1)matrix in here

#Also, I found a rule that the cumulated sum of delta can be powers of phi, starting from 0 to n 
#The power of phi's is changing by each iterations by using the absolute difference in each values of 
#cumulative sum for row and columns 
#For example, at 1 by 3 point in matrix, it has phi^2 because it is result in the difference in 
#absolute value of cumulated sum of row which is 0, and the cumulated sum of column which is 0+1+2=2.

#And I rounded this by 4 for its conveinience. 
#Outer function required vector to use function() so I used Vectorize()





#Question 2(a)

#Import the data
library(datasets)
data(iris)
x <- iris$Sepal.Length
y <- iris$Species

#Set gdens() function
gdens <-function(x,y){
  min <- min(x)
  max <- max(x)
   
  #Creating density function for y variable by listing them. split() allows to divide 
  #Sepal.Length into three groups, follwed by Species. We set the limit as minimum 
  #value of all x and maximum value of all x.
  data_frame = lapply(split(x,y), FUN = density, 
                      from=min(x), to=max(x)). 
  
  #Converting to data frame was necessary to use sapply
  df <- as.data.frame(do.call(rbind, data_frame)) 
  max_den = sapply(df$y, FUN=max) #Creating maximum of density function for Sepal.Length 
                                  #for each species
  
  max_of_all = max(max_den)       #Between 3 different species, we wanted to get the maximum of it
  
  min_den = sapply(df$y, FUN=min) #Creating minimum of density function for Sepal.Length 
                                  #for each species
  
  min_of_all = min(min_den)       #Between 3 different species, we wanted to get the minimum of it
  
  #Create the new plot for plotting our density function
  plot.new()
  plot.window(xlim=c(min,max), ylim=c(min_of_all,max_of_all)) #limit is set for each x and y
  axis(1) #axis for x
  axis(2) #axis for y
  title("Density plot of multiple groups", xlab="Sepal.Length", ylab="Density")
  box()   #outlining the box
  #legend() guides three different species visually
  legend("topright", legend=levels(iris$Species),pch=15,col=c("pink","lightgreen","lightblue"))
  
  #Iteration goes through each species
  for(i in 1:length(data_frame)){
    min_x = min(data_frame[[i]]$x) #Minimum value of Sepal.length for each species
    max_y = max(data_frame[[i]]$x)
    
    #Polygon function is used to plot the density function
    polygon(c(min_x,data_frame[[i]]$x,max_y), 
            #This is for x axis: Strating from min x -> 
            #iterates each value of x for ith species -> Ends to max of x
            c(0,data_frame[[i]]$y,0), 
            #This is for y axis: Starting from 0 -> iterates each value of y 
            #for ith species -> Ends to 0 
            col=hcl((i-1)*120)) #hcl function() can use different colours to 
                                #show 3 species independently
  }
  
}

gdens(x,y)





#Question 2(b)
#comment: There are overlapping between each species that we may want to make the diagram as transparent.
#In that case, we may want to use other function instead of polygon because the polygon function itself 
#overrides the next iterations.






#Question 2(c)
x <- iris$Sepal.Length
y <- iris$Species


x_min <- min(iris$Sepal.Length) 
x_max <- max(iris$Sepal.Length)
y_min <- min(iris$Sepal.Width)
y_max <- max(iris$Sepal.Width)


layout(matrix(c(1,1,1,1,1,1,
                2,2,3,3,4,4,
                5,5,5,6,6,6), nc=6, byrow = TRUE), heights=c(lcm(1),1,1))
#The figures in matrix indicates the number of figures, 
#Figure 1 = title, Figure 2 = Sepal.Length vs Sepal.Width for setosa, 
#Figure 3 = Same as Figure 2 but this is for versicolour
#Figure 4 = Same as Figure 2 but this is for virginica, Figure 5= 2(a), Figure 6 = 
#2(a) with Sepal.Width as x inputs.
#This is 3*6 matrix that I used nc=6 but started to align them by rows
#Also, I adjusted heights, to show the tile
layout.show(6)
par("mar"=c(1,1,1,1))
#I tried to avoid par() function since it didn't allow me to have xlab and ylab for each plot.
#The problem that xlab and ylab does not showing didn't happen until I used par() function

#Creating a new plot for title 
plot.new()

#Setting up coordinates for title
plot.window(xlim=c(0,1), ylim=c(0,1))

#Labelling the title (Figure 1)
text(x=0.5,y=0.5,"scatter and density plots for Sepal and Length and Sepal Width" 
     ,font=2, cex=1.5)

#Figure2
plot(iris$Sepal.Length[which(iris$Species=="setosa")],
     iris$Sepal.Width[which(iris$Species=="setosa")], 
     pch=19, xlab="Sepal.Length", ylab="Sepal.Width",col="pink",
     xlim = c(x_min,x_max), ylim= c(y_min,y_max))

#Figure3
plot(iris$Sepal.Length[which(iris$Species=="versicolor")],
     iris$Sepal.Width[which(iris$Species=="versicolor")], 
     pch=19, xlab="Sepal.Length", ylab="Sepal.Width",col="lightgreen", 
     xlim = c(x_min,x_max), ylim= c(y_min,y_max))

#Figure 4
plot(iris$Sepal.Length[which(iris$Species=="virginica")],
     iris$Sepal.Width[which(iris$Species=="virginica")], 
     pch=19, xlab="Sepal.Length", ylab="Sepal.Width",col="lightblue", 
     xlim = c(x_min,x_max), ylim= c(y_min,y_max))

#Figure 5
gdens(x,y)

#Figrue 6
gdens(iris$Sepal.Width,y)


