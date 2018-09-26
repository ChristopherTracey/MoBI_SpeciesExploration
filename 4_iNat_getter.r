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
if (!requireNamespace("mgsub", quietly=TRUE)) install.packages("mgsub")
require(mgsub)
if (!requireNamespace("taxize", quietly=TRUE)) install.packages("taxize")
require(taxize)

arc.check_product()

setwd(here())

################################################################################################
# variables for rinat

recordlimit <- 10000 # max number of records to return
cutoffyear <- 1980  # only accept years after this
pos_accuracy <- 1000 # positional accuracy in meters

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
# taxonomic cleanup the inat data

iNat_species <- unique(inatdf$scientific_name)
iNat_species <- as.data.frame(iNat_species)
names(iNat_species) <- "iNatName"
iNat_species$iNatName <- as.character(iNat_species$iNatName)

iNat_species$GNAME <- match(iNat_species$iNatName,MoBI_species$GNAME)
iNat_species <- iNat_species[which(!is.na(iNat_species$iNatName)),]
iNat_species <- iNat_species[which(is.na(iNat_species$GNAME)),]

MoBI_syn$trinomial <- NA
MoBI_syn$trinomial <- mgsub(MoBI_syn$SYNONYMS,c("var. ","ssp. "), c("",""))
#MoBI_syn <- MoBI_syn[which(!is.na(MoBI_syn$trinomial)),]
iNat_species1 <- merge(iNat_species,MoBI_syn[c("trinomial","GNAME")],by.x="iNatName",by.y="trinomial",all.x=TRUE)

# taxize 
# get the vector of names without matches
unmatched_names <- iNat_species1[which(is.na(iNat_species1$GNAME.y)),]
unmatched_names <- unmatched_names$iNatName

itismatch <- tax_name(unmatched_names, get="species", db="itis") # this is interactive
try_again <- itismatch[which(is.na(itismatch$species)),]
itismatch <- itismatch[which(!is.na(itismatch$species)),]

iNat_species2 <- merge(iNat_species1,itismatch[c("query","species")],by.x="iNatName",by.y="query",all.x=TRUE)
iNat_species2$bestname <- ifelse(is.na(iNat_species2$species),iNat_species2$GNAME.y,iNat_species2$species)

# replace the inat
inat_replace <- iNat_species2[c("iNatName","bestname")]
inat_replace <- inat_replace[which(!is.na(inat_replace$bestname)),]

inatdf_new <- merge(inatdf,inat_replace, by.x="scientific_name", by.y="iNatName", all.x=TRUE)
inatdf_new$scientific_name <- ifelse(!is.na(inatdf_new$bestname),inatdf_new$bestname, inatdf_new$scientific_name)
inatdf_new$X <- NULL

# subset by the MoBI list
inatdf_new1 <- inatdf_new[which(inatdf_new$scientific_name %in% SpeciesQuery),]


################################################################################################
# create a shapefile
# note that the easting and northing columns are in columns 5 and 6; based on http://neondataskills.org/R/csv-to-shapefile-R/
MoBI_iNat <- SpatialPointsDataFrame(inatdf_new1[c("longitude","latitude")],inatdf_new1,,proj4string <- CRS("+init=epsg:4326"))   # assign a CRS, proj4string = utm18nCR  #https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf; the two commas in a row are important due to the slots feature
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
# quality control standardize the dataset with other layers

#first pass to clear out irrelevant fields
MoBI_iNat1 <- MoBI_iNat1[c("scientific_name","species_guess","latitude","longitude","coordinates_obscured","positional_accuracy","observed_on","geoprivacy","out_of_range","place_guess","private_place_guess","STATE","url")]

# flag observations as MoBI suitable or not.
# geoprivacy or obscured coordinates
MoBI_iNat1$MoBI_suitable <- ifelse(MoBI_iNat1$coordinates_obscured=="true"|MoBI_iNat1$geoprivacy=="obscured"|MoBI_iNat1$geoprivacy=="private","no",NA)
# remove observations that are before the cutoff year
MoBI_iNat1 <- MoBI_iNat1[which(as.numeric(substr(MoBI_iNat1$observed_on,1,4))>=cutoffyear),]
# remove super large coordinate accucary issues
MoBI_iNat1 <- MoBI_iNat1[which(MoBI_iNat1$positional_accuracy<=pos_accuracy),]

#make a backup  for reference
write.csv(MoBI_iNat_FINAL, here("data/inaturalist","FINAL_inat.csv"))
################################################################################################
# clean up everything except MoBI_Species, synomony, and results from previous scripts
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states","gbifdata","MoBI_bison1","MoBI_iNat1")))