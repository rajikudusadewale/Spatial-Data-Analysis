################# GEOGRAPHICALLY WEIGHTED REGRESSION IN R ##############
# GWR is a multivariate model which can indicate where non-stationarity may take place across space; it can be used to 
# identify how locally weighted regression coefficients may vary across the study area. We will first explore the residuals 
# of a linear model to understand its limitations. Next, we will run a GWR and observe its parameters across space.


# Load the data. You may need to alter the file directory
Census.Data <-read.csv("practical_data.csv")
# load the spatial libraries
library("sp")
library("rgdal")
library("rgeos")
library("tmap")

# Load the output area shapefiles
Output.Areas <- readOGR(".", "Camden_oa11")
# join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

## Run a Linear Model ###########
model <- lm(OA.Census$Qualification ~ OA.Census$Unemployed+OA.Census$White_British)
summary(model)

# Plotting the model will give 4 plots. 1. Residuals vs fitted
# 2. Normal Q-Q, 3 Scale Location, Residuals vs leverage

plot(model)

# we can use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
plot(model)

# Mapping the residuals
We can also map the residuals to see if there is a spatial distribution of them across Camden.

resids<-residuals(model)

map.resids <- cbind(OA.Census, resids) 
# we need to rename the column header from the resids file - in this case its the 6th column of map.resids
names(map.resids)[6] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")

Prior to running the GWR model we need to calculate a kernel bandwidth. This will determine now the GWR subsets the data when its test multiple models across space.

library("spgwr")

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.Census$Qualification ~ OA.Census$Unemployed+OA.Census$White_British, data=OA.Census,adapt=T)


#run the gwr model
gwr.model = gwr(OA.Census$Qualification ~ OA.Census$Unemployed+OA.Census$White_British, data = OA.Census, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#print the results of the model
gwr.model


results <-as.data.frame(gwr.model$SDF)

names(results)

gwr.map <- cbind(OA.Census, as.matrix(results))

# The variable names followed by the name of our original data frame (i.e. OA.Census.Unemployed) are the coefficients of the model.

qtm(gwr.map, fill = "localR2")

## USING gridExtra ########

# We will now consider some of the other outputs. We will create four maps in one image to show the original distributions of our unemployed and White British variables and their coefficients in the GWR model.
# 
# To facet four maps in tmap we can use functions from the grid and gridExtra packages which allow us to split the output window into segments. We will divide the output into four and print a map in each window.
# 
# Firstly, we will create four map objects using tmap. Instead of printing them directly as we have done usually, we all assign each map an object ID so it can be called later.

# create tmap objects
map1 <- tm_shape(gwr.map) + tm_fill("White_British", n = 5, style = "quantile")  + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map2 <- tm_shape(gwr.map) + tm_fill("OA.Census.White_British", n = 5, style = "quantile", title = "WB Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map3 <- tm_shape(gwr.map) + tm_fill("Unemployed", n = 5, style = "quantile") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map4 <- tm_shape(gwr.map) + tm_fill("OA.Census.Unemployed", n = 5, style = "quantile", title = "Ue Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)

# With the four maps ready to be printed, we will now create a grid to print them into. From now on everytime we wish to recreate the maps we will need to run the grid.newpage() function to clear the existing grid window.

library(grid)
library(gridExtra)
# creates a clear grid
grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(2,2)))

# prints a map object into a defined cell   
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))