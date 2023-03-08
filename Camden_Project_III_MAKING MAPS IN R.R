######### MAKING MAPS IN R ###########
# Focusing on using TMAP package due to its relative simplictiy
Census.Data <- read.csv('practical_data.csv')

##### LOADING SHAPEFILES INTO R ######
#A GIS shapefile is a file format for storing the location, shape, 
# and attributes of geographic features.
## LOad the required libaries
library(rgdal)
library(rgeos)
# Load the output area shapefile
Output.Area <- readOGR('Camden_oa11')
# Plot the shapefile
plot(Output.Area)

##### JOINING DATA ######
# JOINING Census.Data with Shapefile Output.Area
OA.Census <- merge(Output.Area, Census.Data, by.x = 'OA11CD',by.y='OA')

######### SETTING A CORRDINATE SYSTEM #######
# It's important to set a coord sys, esp if you want to map multi diff files.
# The proj4string() & CRS() func  allows us to set the coord sys of a shapefile to a 
# predefined sys of our choice.  MOst data from the UK is prokected using the 
# British National Grid (EPSG:27700) produced by the ordnance survey
# sets the coord sys to the Bristish National Grid
proj4string(OA.Census)<- CRS('+init=EPSG:27700')

######## MAPPING DATA IN R ##########
#GGPLOT2 can be used to map spatial data, but we gonna explore tmap lib

library(tmap)
library(leaflet)

# Cteating a quick map using qtm() func
qtm(OA.Census, fill = "Qualification")

# Creating more Advanced Maps with tmap
# Creating maps in tmap involves you binding together several functions
# that comprise different aspects of the graphic. For instance:
#   polygon + polygon's symbology +borders +layout

# Creating a simple choropleth map of Qualification var
tm_shape(OA.Census)+ tm_fill('Qualification')
# the result almost with map output using quick map, qtm(). JUst for more customization options
# we used tmap()
# setting color palette
library(RColorBrewer)
display.brewer.all()

# This presents a range of previously defined colour ramps (above). The continuous ramps at the top are all appropriate for our data. If you enter a minus sign before the name of the ramp within the brackets
# (i.e. -Greens), you will invert the order of the colour ramp.

# setting a colour palette
tm_shape(OA.Census) + tm_fill("Qualification", palette = "-Greens") 
# adding style= will give intervals to the palette style parameter
# changing the intervals 
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", palette = "Reds")

# setting interval no in the legend base on color, its just n = n
# number of levels
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", n = 7, palette = "Reds") 

#Adding hist in the legend, legend.hist = T
# includes a histogram in the legend
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", n = 5, palette = "Reds", legend.hist = TRUE) 

#adding borders # tm_borders() range from 0-1
# add borders
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds") + 
  tm_borders(alpha=.4)

# Addding a north Arrow #tm_compass()
# north arrow
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds") + 
  tm_borders(alpha=.4) +
  tm_compass()

# Editing the layout of the map # tm_layout(), also inside tm_fill()
# adds in layout, gets rid of frame
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + 
  tm_borders(alpha=.4) + 
  tm_compass() + 
  tm_layout(title = "Camden, London", legend.text.size = 1.1, legend.title.size = 1.4, legend.position = c("right", "top"), frame = FALSE) 

################ SAVING THE SHAPEFILE ###########
writeOGR(OA.Census, dsn = 'OA.Census Shapefile', layer = "Census_OA_Shapefile", driver="ESRI Shapefile")


