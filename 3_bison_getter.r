#---------------------------------------------------------------------------------------------
# Name: 3_bison_getter.r
# Purpose: Download species records for the MoBI project from BISON
# Author: Christopher Tracey
# Created: 2018-08-03
# Updated: 2018-09-25
#---------------------------------------------------------------------------------------------
if (!requireNamespace("plyr", quietly=TRUE)) install.packages("plyr")
require(plyr) # "plyr" needs be loaded before 'here'
if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)
if (!requireNamespace("rbison", quietly=TRUE)) install.packages("rbison")
require(rbison)
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
# variables for rbison
recordlimit <- 10000

prj <- CRS("+proj=longlat +datum=WGS84")
states <- readOGR(getwd(),"subnational_boundaries")
st_prj <- states@proj4string

# check to see if the query list exists
if(!exists("SpeciesQuery")) {
  print("Please go back to script O_MoBI_SpeciesList.r to create the Species Query")
} else {
  print("Query for species list found, you're good to go!")
}

################################################################################################
# gets the bison data for each species

# make an empty list
bisonlist <- list() 
# run the bison Solr query
for(i in 1:1){#length(SpeciesQuery)) { # 
  x <- bison_solr(scientificName=SpeciesQuery[i], rows=recordlimit, callopts=list(verbose=FALSE, timeout_ms=40000, buffersize=1000000))
  bisonlist[[i]] <- x$points # subset as the tool can report back a list of data frames
}
# the above can error out fairly easily. If so, replace the '1' in "1:length" with whatever number it errored. Look at the bisonlist and see how many elements are completed to figure this out.
# convert to a data frame
bisondf <- ldply(bisonlist)

# check for species whose length of records is equal to the record limit (default is 10000) and rerun this query for those species. Append.
bison_redospecies <- as.data.frame(table(bisondf$scientificName))
bison_redospecies$Var1 <- as.character(bison_redospecies$Var1)
bison_redospecies <- bison_redospecies[which(bison_redospecies$Freq==recordlimit),]
bison_redospecies <- bison_redospecies$Var1 # turn it into a vector
print("The following species had more than the record limit returned, the next step will go and download all the records for them:")
bison_redospecies

recordlimit2=recordlimit*2

# make an empty list
bisonlist_extra <- list()
# run the bison Solr query with no record limit
for(i in 1:length(bison_redospecies)) { # 
  x <- bison_solr(scientificName=bison_redospecies[i], rows=recordlimit2, callopts=list(verbose=FALSE, timeout_ms=40000, buffersize=1000000))
  bisonlist_extra[[i]] <- x$points # subset as the tool can report back a list of data frames
}
bisondf_extra <- ldply(bisonlist_extra)

#remove the records that reached their limit
bisondf <- bisondf[which(!bisondf$scientificName %in% bison_redospecies),] #CHECK THIS
# sort the extra data frame columns by that of the first
bisondf_extra <- bisondf_extra[names(bisondf)] 
# add in the species that were rerun
bisondf <- rbind(bisondf,bisondf_extra) # load the replacements in.


#make a backup  for reference
write.csv(bisondf, here("data/bison","backup_bisondf.csv"))
################################################################################################
# cleans up the bison data

# remove data that doesn't have complete coordinates
bisondf <- bisondf[which(!is.na(bisondf$decimalLatitude)|!is.na(bisondf$decimalLongitude)),]
# remove the NatureServe provided data, if any is left after the above steps
bisondf <- bisondf[which(bisondf$provider!="NatureServe"),]
# remove anything that doesn't have a year
bisondf <- bisondf[which(!is.na(bisondf$year)),]


################################################################################################
# create a shapefile
# note that the easting and northing columns are in columns 5 and 6; based on http://neondataskills.org/R/csv-to-shapefile-R/
MoBI_bison <- SpatialPointsDataFrame(bisondf[c("decimalLongitude","decimalLatitude")],bisondf,,proj4string <- CRS("+init=epsg:4326"))   # assign a CRS, proj4string = utm18nCR  #https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf; the two commas in a row are important due to the slots feature
plot(MoBI_bison,main="Map of MoBI Species Locations from BISON")
# write a shapefile
writeOGR(MoBI_bison, getwd(),"MoBI_FromGBIF", driver="ESRI Shapefile")

################################################################################################
# spatial join to states

# get the state's feature class
bnd_states <- arc.open(here(gdb_boundaries,states))
bnd_states <- arc.select(bnd_states)
bnd_states_sp <- arc.data2sp(bnd_states)
rm(bnd_states)

# reproject the gbif data from geographic to those to the MoBI boundaries
MoBI_bisonprojected <- spTransform(MoBI_bison, bnd_states_sp@proj4string) 

# spatial join to assign a state to each record
MoBI_bisonStates <- over(MoBI_bisonprojected, bnd_states_sp[c("STATE")])
MoBI_bison1 <- cbind(MoBI_bisonprojected@data,MoBI_bisonStates) 

#get rid of records with NA values in states.  Is this due to bad coordinate data?
MoBI_bison1 <- MoBI_bison1[which(!is.na(MoBI_bison1$STATE)),]
MoBI_bison1$STATE <- as.character(MoBI_bison1$STATE)

#make a backup  for reference
write.csv(MoBI_bison1, here("data/bison","backup_bison.csv"))
################################################################################################
# clean up everything except MoBI_Species, synomony, and results from previous scripts
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states","gbifdata","MoBI_bison1")))
