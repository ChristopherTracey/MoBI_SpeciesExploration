#---------------------------------------------------------------------------------------------
# Name: 0_MoBI_SpeciesList.r
# Purpose: Processes the list of MoBI species to guide the collection of species datasets for
#          the MoBI project.
# Author: Christopher Tracey
# Created: 2018-08-03
# Updated: 2018-09-19
#---------------------------------------------------------------------------------------------

if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)
if (!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx")
require(openxlsx)
if (!requireNamespace("reshape2", quietly=TRUE)) install.packages("reshape2")
require(reshape2)
if (!requireNamespace("tidyr", quietly=TRUE)) install.packages("tidyr")
require(tidyr)


################################################################################################
#get mobi species list file
MoBI_files <- list.files(path=here("data/NatureServe"), pattern=".xlsx$")  # --- make sure your excel file is not open.
MoBI_files
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1
MoBI_file <- here("data/NatureServe",MoBI_files[n])

#get a list of the sheets in the file
MoBI_file_sheets <- getSheetNames(MoBI_file)
MoBI_file_sheets
#look at the output and choose which excel sheet you want to load
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

# geodatabase name
gdb_boundaries <- "MoBI_BoundaryData.gdb"
states <- "US_States"


################################################################################################
# create the MoBI Species List
MoBI_species <- read.xlsx(xlsxFile=MoBI_file, sheet=MoBI_file_sheets[n], skipEmptyRows = FALSE)
# change column names
colnames(MoBI_species)[colnames(MoBI_species)=="Scientific.Name"] <- "GNAME"
colnames(MoBI_species)[colnames(MoBI_species)=="Common.Name"] <- "GCOMNAME"
colnames(MoBI_species)[colnames(MoBI_species)=="ELEMENT_GLOBAL_ID"] <- "EGT.ID"
#colnames(MoBI_species)[colnames(MoBI_species)=="old_name"] <- "new_name"

#extract the species by state, excluding AK and HI  #extract the states to create a master list from the states list
MoBI_Sp_x_St <- MoBI_species[c(4,41:50,52:90)] # ---> THIS IS USED A FEW STEPS DOWN
# delete these columns from the MoBI_species data frame
MoBI_species <- MoBI_species[c(-40:-90)]

# write out the data for a backup
write.csv(MoBI_species, here("data/NatureServe","backup_MoBI_species.csv"))

################################################################################################
# creat a species query list
SpeciesQuery <- MoBI_species$GNAME

################################################################################################
# create the master list of tracking status by state -- other tables will be joined to this in subsequent steps
MoBI_Sp_x_St <- melt(MoBI_Sp_x_St,id.vars=c("GNAME"),variable.name="STATE",value.name="SRANK") # reformat from wide to long format
MoBI_Sp_x_St <- MoBI_Sp_x_St[!is.na(MoBI_Sp_x_St$SRANK),] # drop the NA's
MoBI_Sp_x_St$STATE <- gsub("_", "", MoBI_Sp_x_St$STATE) # get rid the underscores that appear after "OR_" and some other states in the orignal dataset
MoBI_Sp_x_St <- merge(MoBI_species[c("EGT.ID","GNAME")],MoBI_Sp_x_St, by="GNAME",all.y=TRUE) # note: does this add two more records -- check Icaricia and Lycaena
MoBI_Sp_x_St <- MoBI_Sp_x_St[order(MoBI_Sp_x_St$GNAME,MoBI_Sp_x_St$STATE),]

# --Note-- Use this dataset in the next script to join up the MJD data and the subsequent scripts as well.

# write out the data for a backup
write.csv(MoBI_Sp_x_St, here("data/NatureServe","backup_MoBI_Sp_x_St.csv"))
################################################################################################
# create the MoBI synomony for plants   # are there other taxa groups we need to deal with
MoBI_syn <- read.xlsx(xlsxFile=MoBI_file, sheet=MoBI_file_sheets[2], skipEmptyRows = FALSE)
# change column names
colnames(MoBI_syn)[colnames(MoBI_syn)=="EGT.ID_Scientific.Name"] <- "GNAME"
colnames(MoBI_syn)[colnames(MoBI_syn)=="Related_SCIENTIFIC_NAME"] <- "SYNONYMS"
MoBI_syn <- MoBI_syn[c("GNAME","SYNONYMS")]
MoBI_syn$type <- "EGT_ID"

# get synomyns from the main worksheet tab
tmp_synonomy <- MoBI_species[c("GNAME","SYNONYMS")]
tmp_synonomy <- tmp_synonomy[which(!is.na(tmp_synonomy$SYNONYMS)),]
tmp_synonomy <- separate_rows(tmp_synonomy, "SYNONYMS",sep=",")
tmp_synonomy$type <- "synonym"

# get RELATED_INFRAS_EO_NEW
tmp_infrataxa <- MoBI_species[c("GNAME","RELATED_INFRAS_EO_NEW")]
tmp_infrataxa <- tmp_infrataxa[which(!is.na(tmp_infrataxa$RELATED_INFRAS_EO_NEW)),]

tmp_infrataxa <- separate_rows(tmp_infrataxa, "RELATED_INFRAS_EO_NEW",sep="\\|")
tmp_infrataxa$RELATED_INFRAS_EO_NEW <- gsub("\\s*\\([^\\)]+\\)","",as.character(tmp_infrataxa$RELATED_INFRAS_EO_NEW))
tmp_infrataxa$type <- "infrataxa"
colnames(tmp_infrataxa)[colnames(tmp_infrataxa) == 'RELATED_INFRAS_EO_NEW'] <- 'SYNONYMS'

# join the three tables together
MoBI_syn <- rbind(MoBI_syn,tmp_synonomy,tmp_infrataxa)
MoBI_syn$SYNONYMS <- trimws(MoBI_syn$SYNONYMS, which="both")

# write out the data for a backup
write.csv(MoBI_syn, here("data/NatureServe","backup_MoBI_syn.csv"))
################################################################################################
# clean up everything except MoBI_Species, synomony
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states")))
