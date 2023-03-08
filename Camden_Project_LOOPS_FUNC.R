############### FUNCTIONS AND LOOPS ###########
Census.Data <-read.csv("practical_data.csv")

# load the spatial libraries
library("sp")
library("rgdal")
library("rgeos")
library("tmap")

# Load the output area shapefiles
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")
Output.Areas <- readOGR(".", "Camden_oa11")

# There are two ways of automating codes. One means is by implementing user-written functions, another is by running for loops
# User-written Func
# Rounding of data using round() func e.g round data to one decimal place
round(Census.Data$Qualification,1)

myfunction <- function(x){
  z <- round(x,1)
  return (z)
}

# lets create a new data frame so we don't overwrite our original data object
newdata <- Census.Data
newdata$Qualification_2 <- myfunction(Census.Data$Qualification)


# to create a func that do logarithm and round off data
myfunction <- function(x){
  y <- log(x)
  z <- round(y,4)
  return(z)
}

newdata$Qualification_2 <- myfunction(Census.Data$Qualification)
newdata$Unemployed_2 <- myfunction(Census.Data$Unemployed)

# Or
myfunction <- function(x){
  z <- log(x) 
  z <- round(z,4)
  return(z)}

# map function with 3 arguments
map <- function(x,y,z){
  
  tm_shape(x) + tm_fill(y, palette = z, style = "quantile") + tm_borders(alpha=.4) + 
    tm_compass(size = 1.8, fontsize = 0.5) + 
    tm_layout(title = "Camden", legend.title.size = 1.1, frame = FALSE) 
  
}


# runs map function, remember we need to include all 3 arguments of the function
map(OA.Census, "Unemployed", "Blues")

############# Loops #############3
# looping round fnc for census data column 2-5
newdata<- Census.Data
for (i in 2:5) {
  newdata[,i]<- round(Census.Data[,i], 1)
  
}
view(newdata)


# using ncol to determine the no of columns
for (i in 2:ncol(Census.Data)) {
  newdata[,i] <- Census.Data[,i]/100
  
}


########## If. else statements
# creates a new newdata object so we have it saved somewhere
newdata1 <- newdata
for (i in 1:nrow(newdata) ){
  if (newdata$White_British[i] < 0.5){
    newdata$White_British[i] <-'low';
  } else {
    newdata$White_British[i] <- 'high';
    
  }
  
}

#inputing multiple else statement

# copies the numbers back to newdata so we can start again
newdata <- newdata1

for(i in 1:nrow(newdata)){
  
  if (newdata$White_British[i] < 0.25) {
    newdata$White_British[i] <- "Very Low";
    
  } else if (newdata$White_British[i] < 0.50){
    newdata$White_British[i] <- "Low";
    
  } else if (newdata$White_British[i] < 0.75){
    newdata$White_British[i] <- "High";
    
  } else {
    newdata$White_British[i] <- "Very High";
    
  }
}



# copies the numbers back to newdata so we can start again
newdata <- newdata1


for(j in 2: ncol (newdata)){
  
  for(i in 1:nrow(newdata)){
    
    if (newdata[i,j] < 0.25) {
      newdata[i,j] <- "Very Low";
      
    } else if (newdata[i,j] < 0.50){
      newdata[i,j] <- "Low";
      
    } else if (newdata[i,j] < 0.75){
      newdata[i,j] <- "High";
      
    } else {
      newdata[i,j] <- "Very High";
      
    }
  }
}

view(newdata)


# merge our new formatted data with the output areas shapefile
shapefile <- merge(Output.Areas, newdata, by.x = "OA11CD", by.y = "OA")

# runs our predefined map function
map(shapefile, "Qualification", "Set2")
