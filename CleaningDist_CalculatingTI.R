
## CLEAN poll dist data: 
library(countrycode)
library(CoordinateCleaner)
##First get your species names the same as your tree names: 
POLLDist2$poll<- paste(word(POLLDist2$scientificName, 1, sep=" "), word(POLLDist2$scientificName, 2, sep=" "), sep="_")

#now get rid of any observations with NA in lat or long: 
POLLDist2<- POLLDist2 %>% filter(!(is.na(decimalLatitude))& !(is.na(decimalLongitude)))

#cc_dupl Identify Duplicated Records
dup1DF <- cc_dupl(POLLDist2, lon = "decimalLongitude", lat = "decimalLatitude", species = "poll")

#cc_inst Identify Records in the Vicinity of Biodiversity Institutions
inst_DF <- cc_inst(dup1DF, lon = "decimalLongitude", lat = "decimalLatitude", species = "poll")

#cc_cap Identify Coordinates in Vicinity of Country Capitals.
capDF <- cc_cap(inst_DF, lon = "decimalLongitude", lat = "decimalLatitude", species = "poll")

#cc_cen Identify Coordinates in Vicinity of Country and Province Centroids
cenDF <- cc_cen(capDF, lon = "decimalLongitude", lat = "decimalLatitude", species = "poll")

#cc_equ Identify Records with Identical lat/lon
eqDF <- cc_equ(cenDF, lon = "decimalLongitude", lat = "decimalLatitude")

#cc_val Identify Invalid lat/lon Coordinates
valDF <- cc_val(eqDF, lon = "decimalLongitude", lat = "decimalLatitude")

#cc_zero Identify Zero Coordinates
#zeroDF <- cc_zero(valDF, lon = "lon", lat = "lat")


#cc_gbif Identify Records Assigned to GBIF Headquarters
gbifDF <- cc_gbif(valDF, lon = "decimalLongitude", lat = "decimalLatitude")

#cc_sea Identify Non-terrestrial Coordinates
library(rgdal)
ref <- readOGR('~/Documents/PollinatorCollab/Countries_WGS84.shp')
#seaDF_1 <- cc_sea(gbifDF, lon = "lon", lat = "lat", ref = ref)
seaDF <- cc_sea(gbifDF, lon = "decimalLongitude", lat = "decimalLatitude",  ref = ref)

  # manually look at all of your orchid occurrences (in something like QGIS) and get rid of any outliers or erroneous records.

## reproject your points of orchid occurrences, and then create an equal area grid cell shapefile:
repro <- OrchDist_MostlyClean[, c("decimalLongitude", "decimalLatitude")] %>% SpatialPoints(proj4string = wgs1984) %>% 
  spTransform(behr)
# create a aqual area template in behrman projection
extent(repro)

be <- raster(ncol = 180, nrow = 142/2, ext = extent(repro), crs = behr)
extent(be)
extent(repro) == extent(be)

#create a richness grid: 
pts <- data.frame(OrchDist_MostlyClean$Acc_name, coordinates(repro))

# Equal area Occurrence number
eq_occgri <- RichnessGrid(x = pts, ras = be, type = "spnum")
#write a raster containing this info: 
writeRaster(eq_occgri, "equal_OccGrid.orch.NEW", format = "raster")

#write a shapefile containing this info:
polys1 = rasterToPolygons(eq_occgri, na.rm=T)
polys3 <- polys1
str(polys3@data[["layer"]])
polys3@data[["layer"]] <- 1:length(polys3@data[["layer"]])
str(polys3@data[["layer"]])
raster::shapefile(polys3, "equal_OccgridNEW.shp", overwrite=F)

plot(polys3)
text(polys3, labels =polys3@data$layer, cex = 0.3, pos = 3, col = "black")

plot(polys1)

text(polys1, labels=polys1@polygons@ID)

## now crop your thing to only include North America: 
ref <- readOGR('~/Documents/PollinatorCollab/Countries_WGS84.shp')




b <- as(extent(-170,-20,7,75), 'SpatialPolygons')
crs(b)<-crs(ref)
NA.shape<- raster::crop(ref, b)
NArepro<-NA.shape %>% spTransform(behr)
plot(NArepro)
crs(NArepro)<-crs(polys3)
#write a raster containing this info:
raster::shapefile(NArepro, "NorthAmericaNEW.shp", overwrite=F)


#crop cells to match this new spdf: 
NAcellsNEW<- raster::crop(polys3, NArepro)
plot(NAcellsNEW)
text(NAcellsNEW, label=NAcellsNEW@data$layer, cex=.4)

#write a shapefile containing this info:
raster::shapefile(NAcellsNEW, "NAcellsNEWUSE.shp", overwrite=T)

## Now, you need to reproject your points over this shapefile: 
reproFINAL <- OrchDist_MostlyClean[, c("decimalLongitude", "decimalLatitude")] %>% SpatialPoints(proj4string = wgs1984) %>% 
  spTransform(behr)
pointsSPDFINAL <- SpatialPointsDataFrame(coords=reproFINAL,data=OrchDist_MostlyClean)

crs(pointsSPDFINAL)<-crs(NAcellsNEW)

NewOrchCells<- over(pointsSPDFINAL, NAcellsNEW)
NewOrchCells$orchid<- pointsSPDFINAL@data$Acc_name


## CREATE A COMMUNITY DATA MATRIX: 
overP2$value<- 1
overP3<- overP2 %>% filter(poll %in% speciesLongForm$species)
pollCommNA<-reshape2::dcast(overP3, layer~poll)
write.csv(pollCommNA, "PollcommNA.OrchMatch.csv")


## CALCULATE BETA DIVERSITY METRICS (INCLUDING TURNOVER IMPORTANCE)

## NOW Recalculate the beta diversity metrics for the orchids: 
## Create the dataframes that you'll use to get pollinator beta diversity: 
Polcells<- PollCells %>% filter(!is.na(PollCells$layer))
Polcells<- Polcells %>% filter(poll %in% PollRel$species)
Polcells<-Polcells %>% filter(!(poll=="SELF_NA"))
orchids<-PollRel %>% filter(species %in% Polcells$poll)
orchids<-orchids %>% filter(!(name=="NOTIN_NA"))
orchids2<-unique(orchids$name)

#dataframes you need to have: 
OrchidBeta<- as.data.frame(orchids2)
a<-1
Core_list<-list()
# Now create a betapart.core object for each orchid in your dataframe: 
for(i in 41:length(orchids2)){
  print(i)
  ORCH<-paste(orchids2[i])
  ocells<-OrchCellsFinal %>% filter(orchid %in% ORCH)
  opolls<- PollRel %>% filter(name %in% ORCH)
  pol<-Polcells %>% filter(poll %in% opolls$species)
  pol<-pol %>% filter(layer %in% ocells$layer)
  pol<- pol %>% distinct()
  pol$value<-1
  pcomMat<- reshape2::dcast(pol, layer~poll)
  pcomMat[is.na(pcomMat)]<-0
  rownames(pcomMat)<-pcomMat$layer
  pcomMat<-pcomMat[-1]
  pcomMat<-as.matrix(pcomMat)
  print("MAtMade")
  
  numSites<-nrow(pcomMat)
  Gamma<-ncol(pcomMat)
  OrchidBeta$NumSites[i]<-paste(numSites)
  OrchidBeta$Gamma[i]<-paste(Gamma)
  
  core<-betapart.core(pcomMat)
  Core_list[[a]] <- core
  names(Core_list)[[a]]<-ORCH
  a <- a + 1
}




## re calculate beta diversity metrics for the new cells: 
for(i in 41:nrow(OrchidBeta)){
  print(i)
  query<-OrchidBeta$orchid[i]
  test<-beta.multi(Core_list[[query]])
  OrchidBeta$total_sor[i]<- paste(test$beta.SOR)
  OrchidBeta$nested[i]<-paste(test$beta.SNE)
  OrchidBeta$turnover[i]<-paste(test$beta.SIM)
}

## CALCULATE Turnover Importance: 
OrchidBeta$TurnoverImportance<- (OrchidBeta$turnover-OrchidBeta$nested)/OrchidBeta$total_sor

