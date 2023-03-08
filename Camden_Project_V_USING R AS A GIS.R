Census.Data <-read.csv("practical_data.csv")
# load the spatial libraries
library("sp")
library("rgdal")
library("rgeos")

# Load the output area shapefiles
Output.Areas <- readOGR("Camden_oa11")

# join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# load the houses point files
House.Points <- readOGR(dsn='House_Points_Shapefile',layer =  "Camden_house_sales")

########### POINTS IN POLYGON ##############
# To aggregate the point data and polygon data together
# We have to project the two to same coord sys

proj4string(OA.Census) <- CRS("+init=EPSG:27700")
proj4string(House.Points) <- CRS("+init=EPSG:27700")

# point in polygon. Gives the points the attributes of the polygons that they are in
pip <- over(House.Points, OA.Census) #Over function :spatial overlay for points,
# grids and polygons: at the spatial locations of object x retrieves the indexes


# need to bind the census data to our original points
House.Points@data <- cbind(House.Points@data, pip)
View(House.Points@data)

# it is now possible to plot the house prices and local unemployment rates
plot(log(House.Points@data$Price), House.Points@data$Unemployed)


# first we aggregate the house prices by the OA11CD (OA names) column, we ask for the mean for each OA
OA <- aggregate(House.Points@data$Price, by = list(House.Points@data$OA11CD), mean)

# change the column names of the aggregated data
names(OA) <- c("OA11CD", "Price")

# join the aggregated data back to the OA.Census polygon
OA.Census@data <- merge(OA.Census@data, OA, by = "OA11CD", all.x = TRUE)


# We can now map the data using tmap. We will have missing data where there are output areas where no houses were sold in 2015.
library(tmap)

tm_shape(OA.Census) + tm_fill(col = "Price", style = "quantile", title = "Mean House Price (£)")


# run a linear model between our unemployment variable from the 2011 Census and our new average house price variable.
model <- lm(OA.Census@data$Price ~ OA.Census@data$Unemployed)
summary(model)

################ BUFFERS qBuffer() func ###############
###################################################

# create 200m buffers for each house point
house_buffers <- gBuffer(House.Points, width = 200, byid = TRUE)

# map in tmap
tm_shape(OA.Census) + tm_borders() +
  tm_shape(house_buffers) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red") 

################## UNION ######################
#We can merge all of the buffers together using a process known as a union. This process will join all intersecting geometries.

# merges the buffers 
union.buffers <- gUnaryUnion(house_buffers)

# map in tmap
tm_shape(OA.Census) + tm_borders() +
  tm_shape(union.buffers) + tm_fill(col = "blue", alpha = .4) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red") 

################# CREATING INTERACTIVE MAPS ######################
# The first thing we need to do is to tell R to switch our tmap mode from plot to view

# interactive maps in tmap
library(leaflet)

# switch tmap mode from plot to view
tmap_mode("view")

#we will map our house price data (note we do not need to include any attributes on layout anymore)
tm_shape(House.Points) + tm_dots(title = "House Prices (£)", border.col = "black", border.lwd = 0.1, border.alpha = 0.2, col = "Price", style = "quantile", palette = "Reds")  

# The Interactive version of tmap works with all of the tmap func
# Interactive map using bubble plot
tm_shape(House.Points) +
  tm_bubbles(size = "Price", 
  title.size = "House Prices (£)", border.col = "black", 
                                    border.lwd = 0.1, border.alpha = 0.4, legend.size.show = TRUE)


### WE can also map polygons
tm_shape(OA.Census) + 
  tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4) 

# To turn tmap back to the plot view simply run tmap_mode("plot") again.
