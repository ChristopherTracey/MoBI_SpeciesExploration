#---------------------------------------------------------------------------------------------
# Name: 4_iNat_getter.r
# Purpose: Download species records for the MoBI project from inaturalist.org. Results are
#          limited to research grade observations.
# Author: Christopher Tracey
# Created: 2018-08-03
# Updated: 2018-09-25
#---------------------------------------------------------------------------------------------
if (!requireNamespace("plyr", quietly=TRUE)) install.packages("plyr")
require(plyr) # "plyr" needs be loaded before 'here'
if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)
if (!requireNamespace("rinat", quietly=TRUE)) install.packages("rinat")
require(rinat)
if (!requireNamespace("sp", quietly=TRUE)) install.packages("sp")
require(sp)
if (!requireNamespace("rgdal", quietly=TRUE)) install.packages("rgdal")
require(rgdal)
if (!requireNamespace("rgeos", quietly=TRUE)) install.packages("rgeos")
require(rgeos)
if (!requireNamespace("arcgisbinding", quietly=TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)

arc.check_product()

setwd(here())

################################################################################################
# variables for rinat

recordlimit <- 10000

# bounding box. The coordinates are for CONUS.
top <- 49.3457868 # north lat
left <- -124.7844079 # west long
right <- -66.9513812 # east long
bottom <- 24.7433195 # south lat
boundbx <- c(bottom,left,top,right)

# check to see if the species query list exists
if(!exists("SpeciesQuery")) {
  print("Please go back to script O_MoBI_SpeciesList.r to create the Species Query")
} else {
  print("Query for species list found, you're good to go!")
}


################################################################################################
# gets the iNat data for each species

# make an empty list
inatlist <- list() 
# query inat
for(i in 1:length(SpeciesQuery)){
  # error handling
  possibleError <- tryCatch(
    x <- get_inat_obs(query=NULL, taxon_name=SpeciesQuery[i], quality="research", geo=TRUE, bounds=boundbx, maxresults=recordlimit, meta=FALSE),
    error=function(e) e
  )  
  if(inherits(possibleError, "error")) next
  #REAL WORK
  x <- get_inat_obs(query=NULL, taxon_name=SpeciesQuery[i], quality="research", geo=TRUE, bounds=boundbx, maxresults=recordlimit, meta=FALSE)
  inatlist[[i]] <- x # subset as the tool can report back a list of data frames
  print(SpeciesQuery[i]) #just for tracking progress
}

# convert to a data frame
inatdf <- ldply(inatlist)

################################################################################################
# cleans up the inat data



################################################################################################
# create a shapefile
# note that the easting and northing columns are in columns 5 and 6; based on http://neondataskills.org/R/csv-to-shapefile-R/
MoBI_iNat <- SpatialPointsDataFrame(inatdf[c("longitude","latitude")],inatdf,,proj4string <- CRS("+init=epsg:4326"))   # assign a CRS, proj4string = utm18nCR  #https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf; the two commas in a row are important due to the slots feature
plot(MoBI_iNat,main="Map of MoBI Species Locations from iNaturalist")
# write a shapefile
writeOGR(MoBI_iNat, getwd(),"MoBI_iNat", driver="ESRI Shapefile")

################################################################################################
# spatial join to states to get the state for each species

# get the state's feature class
bnd_states <- arc.open(here(gdb_boundaries,states))
bnd_states <- arc.select(bnd_states)
bnd_states_sp <- arc.data2sp(bnd_states)
rm(bnd_states)
# should put an alternative approach for not using the bridge

# reproject the gbif data from geographic to those to the MoBI boundaries
MoBI_iNat_projected <- spTransform(MoBI_iNat, bnd_states_sp@proj4string) 

# spatial join to assign a state to each record
MoBI_iNat_States <- over(MoBI_iNat_projected, bnd_states_sp[c("STATE")])
MoBI_iNat1 <- cbind(MoBI_iNat_projected@data,MoBI_iNat_States) 

#get rid of records with NA values in states.  Is this due to bad coordinate data?
MoBI_iNat1 <- MoBI_iNat1[which(!is.na(MoBI_iNat1$STATE)),]
MoBI_iNat1$STATE <- as.character(MoBI_iNat1$STATE)

#make a backup  for reference
write.csv(MoBI_iNat1, here("data/inaturalist","backup_inat.csv"))


################################################################################################
# clean up everything except MoBI_Species, synomony, and results from previous scripts
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states","gbifdata","MoBI_bison1","MoBI_iNat1")))