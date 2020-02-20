### sdm package ###
### example     ###
### http://www.biogeoinformatics.org/
##############################################################################

install.packages("sdm")
library(sp)
library(sdm)
library(raster)
library(rgdal)

# installs all packages which are required for methods and modells inside the sdm package
installAll()

# three main function of using/developing species distribution models
# data preperation; model fitting/evaluation; prediction
# functions used for these steps: sdmData; sdm; predict

################################################################################
### exmaple dataset provided by sdm package

### read the data ###
#####################

file <- system.file("external/species.shp", package="sdm") 
# get the location of the species shapefile
# so, file is simply a filename (with path):
file

# read the species shapefile using the function shapefil:
species <- shapefile(file)
# we can take a look at the head of attribute table in the species dataset
head(species)
class(species)
# it is a SpatialPointsDataFrame

plot(species)

# you can see (head()) that there is a column containing presence-absence records (i.e. Occurence) 
# we can plot presence and absence seperatly with different colours

plot(species[species$Occurrence == 1,], col = "blue", pch = 16)
points(species[species$Occurrence == 0,], col ="red", pch = 16)

# lets read predictor variables (raster datasets)
# we have 4 Ascii-Grids, so, lets just take the name of all files ending to ".asc" to
# be able to read them together. list.files function can be used to get a list of files in a given path

path <- system.file("external",package = "sdm") # path to the folder containing the data
# list the name of files in the specified path, match the pattern 
# (means all files with a name ending .asc)
# we asked to generate full names (i.e., names with the path)
lst <- list.files(path = path, pattern = "asc$", full.names = T)
lst # this is the name of the raster files we want to use as predictor variables

# stack is a fucntion in the raster package, to read/create a multi-layers raster dataset
preds <- stack(lst)
preds # see the specification of the raster layers (e.g. cell size, extent, etc. )

plot(preds)

plot(preds[[4]]) # only plot the 4th layer
plot(species, add=T) # lets add the species points

### data preparation ###
########################

library(sdm)

d <- sdmData(formula=Occurrence~., train = species, predictors = preds)
d

# we didn´t really need the formula in this example, as it would be easy for the function to guess which
# dataset is species, and which are predictors. 
# So it could be like this.
d <- sdmData(train=species, predictors=preds)
d

# however, formula makes it so flexible to handle the variables, specifally if there are several other information
# (i.e. time). If you have multiple species, you can have their names in the left hand side (e.g., sp1+sp2+sp3~.)

# you may also want ot take a part of the variables
d <- sdmData(formula = Occurrence~precipitation+temperature, train=species, predictors=preds)
d

#
d <- sdmData(formula=~., train = species, predictors = preds)
d

### model fitting and evaluation ###
####################################

# in the following example we use 3 different methods to fit the models
m1 <- sdm(Occurrence~., data=d, methods=c("glm","gam","brt"))
m1

# as you can see, a report is generated shows how many percent of the model were successful, and their performance


# in the above example, the performance statistics where calculated based on the training dataset
# (the data that were used to fit the model). It is a better idea to have an independent dataset
# (if so, we would specify the test argument of sdmData). However, for most of the cases, there is no such data
# avaible, therefore we can split the dataset as an alternative solution. Splitting (partitioning) can be one
# time or several times (several replicates). There are also several methods to do that (i.e., subsampling, 
# cross-validation, bootstrapping)

# here we are going to fit 5 models and evaluate them through 2 runs of subsampling, each draw 30 percent of
# training data as test dataset

m2 <- sdm(Occurrence~., data=d, methods=c("rf", "tree","fda","mars","svm"), replicatin="sub",test.percent=30, n=2)
m2

getModelInfo(m2) # info on runs including modelID, whether they are successfully fitted and evaluated, etc.

# we can generate the roc (receiver operating characteristics) curve and compare results for all modells
# empfindlichkeit vs (1-spezifität) Plot
roc(m2)

# the plots can be smoothed
roc(m2, smooth = T)

### prediction ###
##################

# we can use the output of fitting to predict into the study area, or project into a new location or a new time
# the predict function can be used for this purpose

# in the following we just predict the habitat suitability into the whole study area
# since the newdata is a raster object, the output is also a raster object

p1 <- predict(m1, newdata=preds, filename="p1.img")
# many commomly used raster format is supported (through the package rgdal)
p1
plot(p1)

p2 <- predict(m2, newdata=preds, filename="p2.img")
p2

nlayers(p2)

plot(p2[[1:4]]) # plot the first 4 rasters

# we can take the mean of raster over different runs for each method and species
p2m <- predict(m2,newdata=preds,filename="p2m.img", mean=T)
p2m

plot(p2m)

# full names of rasters
getZ(p2m)

### ensemble forecasting ###
############################

# in the following we predict the habitat suitability using the ensemble function
# since the newdata is a raster object, the output is also a raster object

# ensemble based on a Weighted averaging that is weighted using AUC statistic
e1 <- ensemble(m1, newdata = preds, filename="e1.img", setting = list(method="weighted", stat="AUC"))
plot(e1)

# ensemble based on weighted averaging that is weighted TSS (sum of squared total deviations) statistic
# with treshold criterion number 2 which is max(sensivity+specifity)
e2 <- ensemble(m2, newdata = preds, filename="e2.img", setting=list(method="weighted",stat="TSS", opt=2))
e2
plot(e2)

# ensemble based on unweighted averaging
e3 <- ensemble(m2,newdata = preds,filename = "e3.img", setting = list(method="unweighted"))
plot(e3)

# there are other options in the setting argument that can be specified by a user for example one may  define
# a numeric vector as weight, or specify the id of some models that should be incorporated into the ensemble procedure

### sdm can also be used via a GUI (graphical user interface)

gui(m2)
