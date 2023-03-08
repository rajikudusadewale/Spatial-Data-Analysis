# ############## REPRESENTING DENSITIES IN R ##############
# # Kernel density estimation is a commonly used means of 
# representing densities of spatial data points. The technique produces a smooth 
# and continuous surface where each pixel represents a density value based on 
# the number of points within a given distance bandwidth.


# load the spatial libraries
library("sp")
library("rgdal")
library("rgeos")

# Load the output area shapefiles, we won't join it to any data this time
Output.Areas <- readOGR("Camden_oa11")
# load the houses point files
House.Points <- readOGR(dsn = 'House_Points_Shapefile',layer =  "Camden_house_sales")

############### POINT DENSITIES ###########
# Whilst it is straight-forward to determine the frequency of phenomena across space with polygon data
# we gonna run a kernel density estimation for the land registry house price data
# imploring this using func avial from the adehabitatHR pkg

library(raster)
library(adehabitatHR)

# this code run the kde
kde.output <- kernelUD(House.Points, h='href', grid = 1000)
plot(kde.output)

# can also create a simple contour plots # contour(kde) into R
# to map raster in R, first need to ensure it has been projected correctly

# convert kde.output to raster
kde <- raster(kde.output)
# sets projection to British National Grid
proj4string(kde) <- CRS('+init=EPSG:27700')

library(tmap)
# maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")


library(tmaptools) # provides a set of tools for processing spatial data

# creates a bounding box based on the extents of the Output.Areas polygon
bounding_box <- bb(Output.Areas)

# maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")

# We can also mask (or clip) the raster by the output areas polygon and tidy up the graphic. This operation only preserves the parts of the raster which 
# are within the spatial extent of the masking polygon.

# mask the raster by the output area polygon
masked_kde <- mask(kde, Output.Areas)

# maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile", n = 100, legend.show = FALSE, palette = "YlGnBu") +
  tm_shape(Output.Areas) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = F)

# compute homeranges for 75%, 50%, 25% of points, objects are returned as spatial polygon data frames
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)

# the code below creates a map of several layers using tmap
tm_shape(Output.Areas) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "white") +
  tm_shape(House.Points) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_layout(frame = FALSE)

# Whilst mapping the densities of house sales is reasonably interesting, this technique can be applied to all 
# sorts of point data. You could, for example, get two sets of data and create two separate ranges to compare their distributions - i.e. two species of animals.

writeRaster(masked_kde, filename = "kernel_density.grd")
