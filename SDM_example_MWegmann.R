########################################################
### SDM Example M.Wegmann                           ####
########################################################

library(sdm)
library(rgdal)
library(raster)

# read the species shapefile using the function shapefile:

occ <- readOGR("data_wegmann/occurence.gpkg")
class(occ) # it is a SpatialPointsDataFrame
summary(occ)
head(occ)
plot(occ)

bui <- readOGR("data_wegmann/campus_buildings.gpkg")
summary(bui)
plot(bui)

plot(bui)
plot(occ[occ$students == 1,],col='blue',pch=16,add=T)

plot(occ[occ$students == 0,],col='red',pch=16, add=T)

# ###############
# first run
# ###############


# stack is a function in the raster package, to read/create a
multi-layers raster dataset
# preds <- raster("predictor_sdm_start.grd")

r <- raster(bui, ncols = 100, nrows = 100)

rr.0 <- rasterize(bui, r,  progress = "text")
plot(rr.0)
rr.0.d <- distance(rr.0)
plot(rr.0.d)

preds <- rr.0.d

# preds # see the specification of the raster layers (e.g., cell size,extent, etc.)

library(sdm)

d <- sdmData(formula=students~layer, train=occ, predictors=preds)

d

m1 <- sdm(students~.,data=d,methods=c('glm','svm'))

p1 <- predict(m1,newdata=preds,filename='sdm_preds_1.grd',overwrite=T) 
# many commonly used raster format is supported (through the package rgdal)

plot(p1)
