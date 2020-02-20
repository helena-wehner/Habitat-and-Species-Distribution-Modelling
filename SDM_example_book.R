### Modelling species distribution ###
### Book Chapter 13; Wegmann       ###

########################################################################################
setwd("C://Users/Lenovo/Desktop/Master/")
getwd()

# all libraries
library(raster)
library(sp)
library(rgdal)

# combining species and environmental information

p224r63_2011 <- raster("data_book_RS and GIS for ecologists/raster_data/final/p224r63_2011.gri")
head(p224r63_2011)

# import Landsat imagery


# load the presence-absence points of our simulated species
pa_points <- readOGR("data_book_RS and GIS for ecologists/vector_data/occurrence.shp", layer="occurrence")
plot(pa_points)

# now we can extract the environmental information for each of our observation locations
env <- extract(p224r63_2011, pa_points)

# then we combine the environmental data and the information on the presence and absence of the species
pa_data <- data.frame(env, occurrence=pa_points$occurrence)
head(pa_data)
# resulting data.frame with location values, that is presence, absence and the corresponding environmental attributes

#### Visualization
plotRGB(p224r63_2011, stretch="lin")
# scale to non-zero size and add some size contrast
plot(p224r63_2011, strecht ="lin")
pointSize <- pa_data$occurrence*5+1
points(pa_points, cex=pointSize, pch=20)

### Distribution
boxplot(pa_data[,1:6])

boxplot(subset(data.frame(pa_data), occurrence == 0, select = c(1:6)))
