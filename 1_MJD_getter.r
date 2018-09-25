#---------------------------------------------------------------------------------------------
# Name: 1_MJD_getter.r
# Purpose: Processes a NatureServe Multijuristidictional Dataset (MJD) for use in the MOBI project.
# Author: Christopher Tracey
# Created: 2018-08-03
# Updated: 2018-09-24
#---------------------------------------------------------------------------------------------


#load packages
if (!requireNamespace("reshape2", quietly=TRUE)) install.packages("reshape2")
require(reshape2)
if (!requireNamespace("rgdal", quietly=TRUE)) install.packages("rgdal")
require(rgdal)
if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)

# read in the MoBI species list
MoBI_species <- read.csv(here("JulyMoBI_list.csv"), stringsAsFactors=FALSE)

#extract the species by state, excluding AK and HI; this keeps the TV and NN values for replacement below
MoBI_states <- MoBI_species[c(4,39:48,50:88)]
# reformat from wide to long format
MoBI_states <- melt(MoBI_states,id.vars=c("GNAME"),variable.name="STATE",value.name="SRANK")
MoBI_states <- MoBI_states[MoBI_states$SRANK!="",]
MoBI_states <- MoBI_states[order(MoBI_states$GNAME,MoBI_states$STATE),]

# read in the MoBI EO data
MoBI_EO <- read.csv(here("MoBI_EO_export_from_WL_072018.csv"), stringsAsFactors=FALSE) 
# get rid of non MoBI states
MoBI_EO <- MoBI_EO[which(MoBI_EO$STATE!="AB" & MoBI_EO$STATE!="NS" & MoBI_EO$STATE!="BC" & MoBI_EO$STATE!="SK" & MoBI_EO$STATE!="ON" & MoBI_EO$STATE!="YT" & MoBI_EO$STATE!="NF" & MoBI_EO$STATE!="NT" & MoBI_EO$STATE!="PE" & MoBI_EO$STATE!="AK" & MoBI_EO$STATE!="HI"& MoBI_EO$STATE!="QC" & MoBI_EO$STATE!="NB" & MoBI_EO$STATE!="MB"),] 
# fix the Tennesee Valley Authority and Navajo Nation EOs
TVA_NN <- read.csv(here("MoBI_TVA_NN_EO_State_Lookup.csv"), stringsAsFactors=FALSE)
TVA_NN <- TVA_NN[which(TVA_NN$TVA_DUPLICATE!="Y"),]
TVA_NN <- TVA_NN[which(TVA_NN$STATE_CENTRUM!="na"),]
TVA_NN$MOBI_SPP <- NULL
TVA_NN$MOBI_REL_INFRA <- NULL
TVA_NN$TVA_DUPLICATE <- NULL
MoBI_EO$STATE1 <- TVA_NN$STATE_CENTRUM[match(MoBI_EO$EO_ID, TVA_NN$EO_ID)]
MoBI_EO$STATE <- with(MoBI_EO, ifelse( STATE == "TV", STATE1, STATE ) )
MoBI_EO$STATE <- with(MoBI_EO, ifelse( STATE == "NN", STATE1, STATE ) )
MoBI_EO$STATE1 <-NULL
TVA_NN <- NULL # delete this dataframe as its not needed any more.

#write out the data
write.csv(MoBI_EO, "MJD_data_201808.csv")

# sort out extant vs nonextant records
MoBI_EO$extant <- ifelse(MoBI_EO$EORANK!="H" & MoBI_EO$EORANK!="H?" & MoBI_EO$EORANK!="X" & MoBI_EO$EORANK!="X?", "yes","no")
MoBI_EOsum <- MoBI_EO[which(MoBI_EO$extant=="yes"),] 
MoBI_EOsum <- aggregate(extant~STATE+GNAME,data=MoBI_EOsum,FUN=length) # counts the number of extant records
# get EOs with no extant records
MoBI_EOsumNone <- MoBI_EO[which(MoBI_EO$extant!="yes"),] 
MoBI_EOsumNone <- aggregate(extant~STATE+GNAME,data=MoBI_EOsumNone,FUN=length) # counts the number of E/X records
MoBI_EOsumNone$extant <- 0

MoBI_EOsum$code <- paste(MoBI_EOsum$STATE,MoBI_EOsum$GNAME,sep="_")
MoBI_EOsumNone$code <- paste(MoBI_EOsumNone$STATE,MoBI_EOsumNone$GNAME,sep="_")
MoBI_EOsumNone <- MoBI_EOsumNone[!(MoBI_EOsumNone$code %in% MoBI_EOsum$code),]

MoBI_EOsum <- rbind(MoBI_EOsum,MoBI_EOsumNone)
MoBI_EOsum$code <- NULL




# write output
write.csv(MoBI_EOsum,"MoBI_MJD.csv", row.names=FALSE)



# get common names and join them into the MoBI_states df
commonnames <- unique(MoBI_EO[c("GNAME","GCOMNAME")])

################################################################################################
# clean up everything except MoBI_Species, synomony, and results from previous scripts
rm(list=setdiff(ls(), c("MoBI_species","MoBI_syn","MoBI_Sp_x_St","SpeciesQuery","gdb_boundaries","states","gbifdata")))
