#---------------------------------------------------------------------------------------------
# Name: 2_gbif_getter.r
# Purpose: 
# Author: Christopher Tracey
# Created: 2018-08-03
# Updated: 2018-09-19
#---------------------------------------------------------------------------------------------

if (!requireNamespace("plyr", quietly=TRUE)) install.packages("plyr")
require(plyr) # "plyr" needs be loaded before 'here'
if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)
if (!requireNamespace("rgbif", quietly=TRUE)) install.packages("rgbif")
require(rgbif)
if (!requireNamespace("sp", quietly=TRUE)) install.packages("sp")
require(sp)
if (!requireNamespace("rgdal", quietly=TRUE)) install.packages("rgdal")
require(rgdal)
if (!requireNamespace("rgeos", quietly=TRUE)) install.packages("rgeos")
require(rgeos)
if (!requireNamespace("arcgisbinding", quietly=TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)

arc.check_product() # for the arcgis-r bridge

setwd(here())

################################################################################################
# variables for rgbif
year_begin <- 1900 #set this to whatever year you want the observations to begin
year_end <- as.numeric(format(Sys.Date(), format="%Y")) # get the current year
time_period <- paste(year_begin,year_end,sep=",")
recordlimit <- 10000 # modify if needed, fewer will make testing go faster
CoordinateUncertainty <- 10000 # in meters
rm("year_begin","year_end") # keeping it clean

# check to see if the query list exists
if(!exists("SpeciesQuery")) {
  print("Please go back to script O_MoBI_SpeciesList.r to create the Species Query")
}

################################################################################################
# gets the  keys for each species name, based on GBIF
keys <- sapply(SpeciesQuery, function(x) name_backbone(name=x), USE.NAMES=FALSE) #$speciesKey
# convert to a data frame
keysdf <- as.data.frame(do.call(rbind.fill.matrix, # i'm not sure why I made this conversion so complicated, but it works...
                                lapply(keys, function(l) {
                                  res <- unlist(l)
                                  t(res)
                                })
)
)

# export the keys to a csv just to have a backup
write.csv(keysdf, here("data/gbif","backup_gbifkeys.csv"), row.names=FALSE)
# remove the keys to keep it clean
rm("keys")

################################################################################################

gbif_keys <- keysdf
#gbif_keys <- gbif_keys[which(gbif_keys$status=="SYNONYM"),]
gbif_keys <- gbif_keys[c("usageKey","acceptedUsageKey","canonicalName","species")]
gbif_keys <- gbif_keys[which(!is.na(gbif_keys$species)),] # get rid of the screw ups
gbif_keys$usageKey <- as.character(gbif_keys$usageKey) 
gbif_keys$acceptedUsageKey <- as.character(gbif_keys$acceptedUsageKey)
gbif_keys$canonicalName <- as.character(gbif_keys$canonicalName) 
gbif_keys$species <- as.character(gbif_keys$species) 
colnames(gbif_keys)[colnames(gbif_keys)=="canonicalName"] <- "GNAME"
colnames(gbif_keys)[colnames(gbif_keys)=="species"] <- "gbifNAME"

# find species that are lumped
lumped_species <- aggregate(GNAME~gbifNAME, data=gbif_keys, FUN=length)
colnames(lumped_species)[colnames(lumped_species)=="GNAME"] <- "length"
lumped_species <- lumped_species[which(lumped_species$length>1),]
print("The following taxa are dropped from the GBIF dataset as they are broader in concept then the MoBI species\n")
print(lumped_species$gbifNAME)

gbif_keys$lumped <- lumped_species$gbifNAME[match(gbif_keys$gbifNAME, lumped_species$gbifNAME)]
gbif_keys <- gbif_keys[which(is.na(gbif_keys$lumped)),] # deletes the broader concept species
gbif_keys$lumped <- NULL
# replace the usage key with the accepted usage key
gbif_keys$acceptedUsageKey[is.na(gbif_keys$acceptedUsageKey)] <-gbif_keys$usageKey[is.na(gbif_keys$acceptedUsageKey)]

#make a backup of the lumped species for reference
write.csv(lumped_species, here("data/gbif","backup_lumpedSpecies.csv"))
rm("lumped_species")

# - Note: could possibly add more taxonomic control here

# make keys for querying gbif for occurence data
keys2 <- as.numeric(as.character(gbif_keys$acceptedUsageKey))
keys2 <- keys2[!is.na(keys2)]  # just in case

################################################################################################
#searches for occurrences
dat <- occ_search(
  taxonKey=keys2, 
  limit=recordlimit,
  return='data', 
  hasCoordinate=TRUE,
  hasGeospatialIssue=FALSE,
  year=time_period,
  country="US"
)

dat <-dat[dat!="no data found, try a different search"] # deletes the items from the list where no occurences were found. doesn't work for one species
datdf <- ldply(dat) # turns the list to a data frame

# subsets records to under a specified coordinate uncertainty (eg. 300m)
datdf <- datdf[which(datdf$coordinateUncertaintyInMeters<CoordinateUncertainty | is.na(datdf$coordinateUncertaintyInMeters)),]

gbifdata <- datdf # just changing the name so it backs up

# export the keys to a csv just to have a backup
write.csv(gbifdata, here("data/gbif","backup_gbifdata.csv"), row.names=FALSE)

# replace gbif names with NatureServe names
###gbif <- read.csv("gbif_download_backup.csv", stringsAsFactors=FALSE)
gbifdata$NSname <- GBIF2NS$GNAME[match(gbifdata$name,  GBIF2NS$gbifNAME)]
gbifdata$name <- with(gbifdata, ifelse(!is.na(NSname),NSname,name) )

################################################################################################
# clean up gbif dataset to get rid of fields we don't need
gbifdata <- gbifdata[c(".id","name","key","decimalLatitude","decimalLongitude","issues","coordinateUncertaintyInMeters","basisOfRecord","species","taxonRank","infraspecificEpithet","year","month","day","habitat","locality","gbifID","footprintWKT") ]


# export the keys to a csv just to have a backup
write.csv(gbifdata, here("data/gbif","backup_gbifdata_clean.csv"), row.names=FALSE)
################################################################################################
# create a shapefile
# note that the easting and northing columns are in columns 5 and 6; based on http://neondataskills.org/R/csv-to-shapefile-R/
MoBIgbif <- SpatialPointsDataFrame(gbifdata[,5:4],gbifdata,,proj4string <- CRS("+init=epsg:4326"))   # assign a CRS, proj4string = utm18nCR  #https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf; the two commas in a row are important due to the slots feature
plot(MoBIgbif,main="Map of MoBI Species Locations from GBIF")
# write a shapefile
writeOGR(MoBIgbif, getwd(),"MoBI_FromGBIF", driver="ESRI Shapefile")

# create polygons for data that has a wkt entry
gbifdata_poly <- gbifdata[which(!is.na(gbifdata$footprintWKT)),]
#### NOTE - more to do here

#######################################################################################################
# spatial join to states

# get the state's feature class
bnd_states <- arc.open(here(gdb_boundaries,states))
bnd_states <- arc.select(bnd_states)
bnd_states_sp <- arc.data2sp(bnd_states)

# reproject the gbif data from geographic to those to the MoBI boundaries
MoBIgbifprojected <- spTransform(MoBIgbif, bnd_states_sp@proj4string) 

# spatial join to assign a state to each record
MoBIgbifStates <- over(MoBIgbifprojected, bnd_states_sp[c("STATE")])
MoBIgbif1 <- cbind(MoBIgbifprojected@data,MoBIgbifStates) 

#get rid of records with NA values in states.  Is this due to bad coordinate data?
MoBIgbif1 <- MoBIgbif1[which(!is.na(MoBIgbif1$STATE)),]
MoBIgbif1$STATE <- as.character(MoBIgbif1$STATE)
MoBIgbif1$STATE2 <- MoBIgbif1$STATE



# write out a copy
write.csv(MoBIgbif1, "gbifdata_ObsData_20180822.csv")
#save.image(file='gbif20180822.RData')

################################################################################################
# clean up everything except MoBI_Species, synomony, and results from previous scripts
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states","gbifdata","MoBIgbif")))
