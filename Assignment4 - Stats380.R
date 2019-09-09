#Please set your working directory 
#Question 1

#I will create names for files from "file1.html" to "file4.html"
num <- 1:4

#Obtaining the names for the file
fileNum <- paste("file", num, ".html", sep="")

#There is another file but without numbers. So there are total 5 html files
fileList <- c("file.html", fileNum)


extract <- function(x){
  #Bringing html into my computer drive
  filehtml <- readLines(x)

  #The infromation that we want to extract is between "<span>" and "</span>"
  #I will extract the indices where it contains "<span>" first
  where <- grep("<span>", filehtml)
  want <- filehtml[where]
  
  #Now we extracted the lines where it contains "<span>"
  #we will extract the information for data frame by getting rid of any line before "<span>" or after "</span>"
  infor <- gsub(".+<span>", "", want)
  another <- gsub("</span>.+$", "", infor )
  
  #Tranforming these information into matrix which has 5 columns
  mat <- matrix(another[6:length(another)], byrow = TRUE, ncol = 5)
  
  #Extracting columns but transform any characters that contain numbers into numeric types
  col1 <- as.numeric(mat[,1])
  col2 <- mat[,2]
  col3 <- as.numeric(mat[,3])
  col4 <- as.numeric(mat[,4])
  col5 <- as.numeric(mat[,5])
  
  #Converting into data frame
  matform <- data.frame(col1, col2, col3, col4, col5)
  #Providing column names into data frame
  colnames(matform) <- c(another[1],another[2],another[3],another[4],another[5])
  matform

}  

#Since we want to repeat the function 'extract' for 5 of html files, we use lapply()
allFiles <- lapply(fileList, extract)
#combine all 5 of results into one data frame
allData <- do.call("rbind", allFiles)
head(allData)



#Question 2

#Bring airport html
intro <- readLines("airport.html")

extract2 <- function(y){
  
  #Extracting the line where it contains "<span>" because it contains the information we want
  where <- grep("<span>", y)
  want <- intro[where]
  
  #Removing any characters before "<span>" or after "</span>"
  infor <- gsub(".+<span>", "", want)
  another <- gsub("</span>.+$", "", infor)
  
  #Converting the information into matrix, we have 7 columns in total
  mat <- matrix(another[8:length(another)], byrow = TRUE, ncol = 7)
  
  #Extracting columns but changing any characters that contain numeric values into numeric types
  col1 <- mat[,1]
  col2 <- mat[,2]
  col3 <- mat[,3]
  col4 <- mat[,4]
  col5 <- mat[,5]
  col6 <- as.numeric(mat[,6])
  col7 <- as.numeric(mat[,7])
  
  #Converting into data frame
  matform <- data.frame(col1, col2, col3, col4, col5, col6, col7)
  #Providing column names
  colnames(matform) <- c(another[1],another[2],another[3],another[4],another[5],another[6],another[7])
  matform

}

airport <- extract2(intro)
head(airport)

#Question 3

#Since some of data contains NA value in its longitude or latitude, we omit them
new_allData <- na.omit(allData)

#Split the 'airport' data frame by 'Airport' file names
b <- split(airport,airport$Airport)
#I confirmed that some of airport contains multiple of each airport, 
#but we don't need more than one for each airport

#extracting unique name of airport (no replicates)
x <- unique(airport$Airport)
x
#Since we have 41 different airport, it suppose to have 41 rows in our new 
length(x)


#Creating new airport files without any duplicates 
new_airport <- rbind(b$Alexandra[1,], b$Ardmore[1,], b$`Auckland intl`[1,], b$`Chatham islands`[1,], 
             b$`Christchurch intl`[1,], b$Dunedin[1,], b$Gisborne[1,], b$Glentanner[1,], b$Hamilton[1,]
             , b$Hastings[1,], b$Hokitika[1,], b$Invercargill[1,], b$Kaitaia[1,], b$Kerikeri[1,]
             , b$Manapouri[1,], b$Masterton[1,], b$`Mount cook`[1,], b$Napier[1,], b$Nelson[1,]
             , b$`New plymouth`[1,], b$Oamaru[1,], b$Ohakea[1,], b$`Palmerston north`[1,], b$Paraparaumu[1,]
             , b$Pukaki[1,], b$Queenstown[1,], b$Rotorua[1,], b$Taupo[1,], b$Tauranga[1,], b$Timaru[1,]
             , b$Waiouru[1,], b$Wairoa[1,], b$Wanaka[1,], b$Wanganui[1,], b$`Wellington intl`[1,]
             , b$Westport[1,], b$Whakatane[1,], b$Whangarei[1,], b$Whenuapai[1,], b$Wigram[1,]
             , b$Woodbourne[1,])
#Omit any rows it contains NA in its row
new_airport <- na.omit(new_airport)
#We can check the number of row is the same as 41 as we checked from length(x)
nrow(new_airport)


#Bring 'geosphere' library to use harvesine distance function
library(geosphere)

#Adding a new column called "Airport" into new_allData
#this is the final data frame structure we want to use
result <- cbind(new_allData, "Airport")
head(result)


get_min_dist <- function(new_allData, new_airport){
  
  #Going through 1 to the end of new_alldata row
  for(i in 1:nrow(new_allData)){
    #our first longtitude and latitude
    longi1 <- new_allData[i,]$`Longitude (DD)`
    lati1 <- new_allData[i,]$`Latitude (DD)`
    
    #create an empty list to store the value of distances
    #this was necessary because each row in new_allData wants to find the minimum distance between each row in new_airport 
    list_dist <- list()
    
    #Going throught 1 to the end of new_airport row
    for(j in 1:nrow(new_airport)){
      #our second longitutde and latitude
      longi2 <- new_airport[j,]$`Longitude (DD)`
      lati2 <- new_airport[j,]$`Latitude (DD)`
      
      #Getting the harvesine distance 
      new_dist <- distHaversine(c(longi1,lati1),c(longi2,lati2))
      #store the distance into the list
      list_dist <- append(list_dist, new_dist)
    }
    
    #find the minimum distance
    find_min <- min(unlist(list_dist))
    #find the index in the distance so that we can extract 
    find_index <- grep(find_min, list_dist)
    
    print(find_min)
    print(find_index)
    print(new_airport[find_index,]$Airport)
    
    #As a last step, I tried to store the name of Airport to the "result" data frame but it didn't work
    #result[i,]$"Airport" <- new_airport[j,]$Airport
  }
}

get_min_dist(new_allData,new_airport)


