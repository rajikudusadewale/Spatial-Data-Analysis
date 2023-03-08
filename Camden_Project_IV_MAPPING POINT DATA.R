########### MAPPING POINT DATA IN R ##########
Census.Data <-read.csv("practical_data.csv")
library("rgdal")
library("rgeos")

# Load the output area shapefiles
Output.Areas <- readOGR("Camden_oa11")
# join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")
# Loading point data #House price data
houses <- read.csv('camdenhousesales15.csv')

# we only need a few columns for this practical

houses <- houses[,c(1,2,8,9)]
# 2D scatter plot
plot(houses$oseast1m, houses$osnrth1m) # or plot(houses[[3]],houses[[4]])
## CONVERTING THE CSV INTO SPATIAL POINT DATAFRAME using sp pkg
# To do this we will need to set what the data is to be included, what columns contain the
# x and y coordinates, and what projection system we are using.

library(sp)
# create a House.Points SpatialPointsDataFrame
House.Points <- SpatialPointsDataFrame(houses[,3:4], houses, proj4string = CRS('+init=EPSG:27700'))

# Before we map the points, we will create a base map using the output area boundaries.

library(tmap)
# This plots a blank base map, we have set the transparency of the borders to 0.4

tm_shape(OA.Census) + tm_borders(alpha=.4) # this as base map, then we can add tm_shape for the point data
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", palette = "Reds", style = "quantile") 

###### ADding more arguments within the tm_dots() func just like tm_fill()
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds", style = "quantile", title = "Price Paid (£)") 

# We can also add tm_layout() and tm_compass() as we did in the previous practical.
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Purples", style = "quantile", title = "Price Paid (£)")  +
  tm_compass() +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)


############ PROPORTIONAL SYMBOLS ############
# we replace the tm_dots() function with the tm_bubbles() function. 

# creates a proportional symbol map
tm_shape(OA.Census) + tm_borders(alpha=.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "Blues", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)") +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)


### TO MAKE THE POLYGON SHAPEFILE DISPLAY OF THE CENSUS VARIABLES AS A CHOROPLETH MAP
# In this example, we have also added some more parameters within the tm_bubbles() function to create thin borders around the bubbles

# creates a proportional symbol map
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% Qualification") + 
  tm_borders(alpha=.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "Blues", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)", border.col = "black", border.lwd = 0.1, border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)



############## Saving shapefile #############
writeOGR(House.Points, dsn = 'House_Points_Shapefile', layer = 'Camden_house_sales', driver = 'ESRI Shapefile')










