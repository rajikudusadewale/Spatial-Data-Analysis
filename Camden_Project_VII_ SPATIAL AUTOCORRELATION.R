############ MEASURING SPATIAL AUTOCORRELATION IN R ###########
Census.Data <-read.csv("practical_data.csv")
library("sp")
library("rgdal")
library("rgeos")

# Load the output area shapefiles
Output.Areas <- readOGR("Camden_oa11")
# join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# load the houses point files
House.Points <- readOGR(".", "Camden_house_sales")

library(tmap)
tm_shape(OA.Census)+ tm_fill('Qualification', palette = 'Reds', style = 'quantile', title = '% with a Qualification')+
  tm_borders(alpha = .4)

####### RUNNING A SPATIAL AUTOCORRELATION ############
# A spatial autocorrelation measures how distance influences a particular variable. In other words, it quantifies the degree 
# of which objects are similar to nearby objects. Variables are said to have a positive spatial autocorrelation when similar 
# values tend to be nearer together than dissimilar values.

# Waldo Tober's first law of geography is that "Everything is related to everything else, but near things are more related than distant things

#We will be using the spatial autocorrelation functions available from the spdep package
library(spdep)

########  FINDING NEIGHBOURS #########
# Calculate neighbours
neighbours <- poly2nb(OA.Census)
neighbours

#We can plot the links between neighbours to visualise their distribution across space.
plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='red')

# Calculate the Rook's case neighbours
neighbours2 <- poly2nb(OA.Census, queen = FALSE)
neighbours2   # this one has fewer links btw neighbours.
# by plotting both neighbours, we can interpret the diff
compares different types of neighbours

# We can represent spatial autocorrelation in two ways; globally or locally. Global models will create a single measure which
# represents the entire data whilst local models let us explore spatial clustering across space.
# 

# ########## RUNNING A GLOBAL SPATIAL AUTOCORRELATION############
# With the neighbours defined. We can now run a model. First, we need to convert the data types of the neighbours object. This file will be used to determine how the neighbours are weighted

# Convert the neighbour data to a listw object
listw <- nb2listw(neighbours2)
listw

# ## We can now run the model. This type of model is known as a Moran's test. This will create a correlation score between 
# -1 and 1. Much like a correlation coefficient, 1 determines perfect positive spatial autocorrelation (so our data is clustered), 0 identifies the data is randomly distributed and -1
# represents negative spatial autocorrelation (so dissimilar values are next to each other).

# global spatial autocorrelation
moran.test(OA.Census$Qualification, listw)
# if the Moran I stat is close to 1, the qualification var is positively autocorrelated in Camden. 
# In other words, the data does spatially cluster. can also consider the p_value as a measure of the stat sign of the model

################ CREATE A MORAN PLOT ########
moran <- moran.plot(OA.Census$Qualification, listw = nb2listw(neighbours2, style = 'W'))

# creates a local moran output
local <- localmoran(x = OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))


# binds results to our polygon shapefile
moran.map <- cbind(OA.Census, local)


# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 


### to create LISA cluster map ### 
quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- OA.Census$Qualification - mean(OA.Census$Qualification)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(OA.Census,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")


# It is apparent that there is a statistically significant geographic pattern to the clustering of our qualification variable in Camden

############# HOT-SPOT ANALYSIS ############
# However, here a search radius of just 250 metres fails to define nearest neighbours for some areas so we will need to set the radius as 800 metres or more for our model in Camden
# creates centroid and joins neighbours within 0 and 800 units
nb <- dnearneigh(coordinates(OA.Census),0,800)
# creates listw
nb_lw <- nb2listw(nb, style = 'B')

# plot the data and neighbours
plot(OA.Census, border = 'lightgrey')
plot(nb, coordinates(OA.Census), add=TRUE, col = 'red')

# compute Getis-Ord Gi statistic
local_g <- localG(OA.Census$Qualification, nb_lw)
local_g <- cbind(OA.Census, as.matrix(local_g))
names(local_g)[6] <- "gstat"

# map the results
tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") + tm_borders(alpha=.4)


