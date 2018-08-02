#---------------------------------------------------------------------------------------------
# Name:     explore_and_map.R
# Purpose:  takes Heritage data from the MJD and makes some tables and maps for data 
#           exploration
# Author:   Christopher Tracey
# Created:  2018-08-01
# Updates:  
#
# To Do List/Future Ideas:
#           - incorporate TVA data
#           - 
#---------------------------------------------------------------------------------------------

#load packages
library(reshape2)
library(rgdal)
library(here) # using instead of setwd

# read in the MoBI species list
MoBI_species <- read.csv(here("JulyMoBI_list.csv"), stringsAsFactors=FALSE)

# create a table of taxa groups by grank
tab_TaxGrp_x_grank <- as.data.frame.matrix(table(MoBI_species$TAX_GROUP,MoBI_species$RND_GRANK))

#extract the species by state, excluding AK and HI
MoBI_states <- MoBI_species[c(4,39:48,50:88)]
# reformat from wide to long format
MoBI_states <- melt(MoBI_states,id.vars=c("GNAME"),variable.name="STATE",value.name="SRANK")
MoBI_states <- MoBI_states[MoBI_states$SRANK!="",]
MoBI_states <- MoBI_states[order(MoBI_states$GNAME,MoBI_states$STATE),]

# read in the MoBI EO data
MoBI_EO <- read.csv(here("MoBI_EO_export_from_WL_072018.csv"), stringsAsFactors=FALSE) 
# get rid of non MoBI states
MoBI_EO <- MoBI_EO[which(MoBI_EO$STATE!="AB" & MoBI_EO$STATE!="NS" & MoBI_EO$STATE!="BC" & MoBI_EO$STATE!="SK" & MoBI_EO$STATE!="ON" & MoBI_EO$STATE!="YT" & MoBI_EO$STATE!="NF" & MoBI_EO$STATE!="NT" & MoBI_EO$STATE!="PE" & MoBI_EO$STATE!="AK" & MoBI_EO$STATE!="HI"& MoBI_EO$STATE!="QC" & MoBI_EO$STATE!="NB" & MoBI_EO$STATE!="MB" & MoBI_EO$STATE!="NN"),] 
# sort out extant vs nonextant records
MoBI_EO$extant <- ifelse(MoBI_EO$EORANK!="H" & MoBI_EO$EORANK!="H?" & MoBI_EO$EORANK!="X" & MoBI_EO$EORANK!="X?", "yes","no")
MoBI_EOsum <- MoBI_EO[which(MoBI_EO$extant=="yes"),] 
MoBI_EOsum <- aggregate(extant~STATE+GNAME,data=MoBI_EOsum,FUN=length) # counts the number of extant records

#######################################################################################################
# make the maps
#######################################################################################################

# load the state shapefile
states <- readOGR(here(),"subnational_boundaries")
states_center <- readOGR(here(),"subnational_boundaries_centroid")

#get a list of snames to run the loop
gnames <-MoBI_states[c("GNAME")]
gnames <- unique(gnames)
gnames <- gnames[order(gnames$GNAME),]

# replace rounded ranks
srank_rnd <- read.csv(here("rounded_srank.csv"), stringsAsFactors=FALSE)

# series of merges to make all the fields available
MoBI_states <- merge(MoBI_states,srank_rnd,by.x="SRANK", by.y="S.RANK", all.x=TRUE) # ,all.x=TRUE
MoBI_states <- merge(MoBI_states,MoBI_species[c("GNAME","TAX_GROUP","G_RANK")],by="GNAME")
MoBI_states <- merge(MoBI_states,MoBI_EOsum, by=c("GNAME","STATE"), all.x=TRUE)

# EO presence symbols
MoBI_states$EOpres <- as.numeric(ifelse(!is.na(MoBI_states$extant), "16","1"))

# assign colors to the ranks
MoBI_states$color[MoBI_states$Rounded.S.RANK=="SX"]  <- "#003593"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="SH"]  <- "#6F97F3"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="S1"]  <- "#D30200"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="S2"]  <- "#FF914B"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="S3"]  <- "#FFFF2D"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="S4"]  <- "#64FE3A"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="S5"]  <- "#039B2E"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="SNR"] <- "#9B656C"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="SU"]  <- "#9B656C"
MoBI_states$color[MoBI_states$Rounded.S.RANK=="SNA"] <- "#FFC8FF"

# loop to make the maps
for (i in 1:length(gnames)) {
  MoBI_sp <- MoBI_states[MoBI_states$GNAME==gnames[i],]
  state_status <- merge(states, MoBI_sp, by.x="subnation", by.y="STATE",duplicateGeoms=TRUE)
  state_eo <- merge(states_center,MoBI_sp[c("STATE","EOpres")],by.x="subnation",by.y="STATE",duplicateGeoms=TRUE)
  # plot
  png(file=paste("maps/",gnames[i],".png",sep=""), width=2100, height=1200,res=300)
  plot(state_status, col=(state_status@data$color))
  plot(state_eo, pch=state_eo$EOpres, cex=0.6, add=TRUE)
  #text(x=12,y=NULL,labels="open circles = no extant EO data; closed circles = extant EO data")
  title(main=paste(MoBI_sp$GNAME[1], MoBI_sp$G_RANK[1], sep=" - " ),sub=MoBI_sp$TAX_GROUP[1] )
  legend(x="left",legend=c("SX","SH","S1","S2","S3","S4","S5","SNR","SU","SNA"),
         fill=c("#003593","#6F97F3","#D30200","#FF914B","#FFFF2D","#64FE3A","#039B2E","#9B656C","#9B656C","#FFC8FF"), bty="n" )
  dev.off() # turns off the plotteMor writing to pngs
}
