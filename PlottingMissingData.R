## Getting stuff for missing data: 
rm(list=ls())
  #read the tree
otree<-read.tree("~/Documents/PollinatorCollab/RealProj/OrchsIQcon")
  #read your map
Nacells<-readOGR("naCellsNEWUSE.shp")
  #get tip names: 
tips<-otree$tip.label
tips<-as.data.frame(tips)
  #get coverage data for orchids: 
HaveCoverage<-read.csv("Orchid.Clim.Range.PolDynamics.Coverage.csv")
HaveCoverage<- HaveCoverage %>% select(orchid)

  #get dist data for all orchids: 
OrchDist_ALL<- read.csv("OrchidDist_Cleaned.csv")
TaxKey<-read.csv("taxonomyKey.OrchDist.csv")

OrchDist_full<-full_join(OrchDist_ALL, TaxKey)
  #get the ones that you've already cleaned: 
OrchCells_clean<- read.csv("NAorchCells.FinalClean.NoSelfing.csv")
OrchCells_self<-read.csv("NAorchCells.SELFING.raw.csv")

  #Now just get the orchids that you need to clean the dist data for: 
OrchDist_notIncluded<- OrchDist_full %>% filter(!(Acc_name%in%OrchCells_clean$orchid | Acc_name %in% OrchCells_self$orchid))

OrchDist_notIncluded <-OrchDist_notIncluded %>% filter(!(Acc_name=="NOTIN_NA"))


  #now automatically get rid of outlier records: 
temp_occurrences <- OrchDist_notIncluded
temp_species <- unique(temp_occurrences$Acc_name)
cleaned_occurrences <- NULL
for(i in 1:length(temp_species)){
  print(i)
  temp <- temp_occurrences[which(temp_occurrences$Acc_name==temp_species[i]),]
  if(length(which(temp$decimalLongitude>(mean(temp$decimalLongitude)+(3*sd(temp$decimalLongitude)))))>0){
    temp <- temp[-which(temp$decimalLongitude>(mean(temp$decimalLongitude)+(3*sd(temp$decimalLongitude)))),]}
  if(length(which(temp$decimalLongitude<(mean(temp$decimalLongitude)-(3*sd(temp$decimalLongitude)))))>0){
    temp <- temp[-which(temp$decimalLongitude<(mean(temp$decimalLongitude)-(3*sd(temp$decimalLongitude)))),]}
  if(length(which(temp$decimalLatitude>(mean(temp$decimalLatitude)+(3*sd(temp$decimalLatitude)))))>0){
    temp <- temp[-which(temp$decimalLatitude>(mean(temp$decimalLatitude)+(3*sd(temp$decimalLatitude)))),]}
  if(length(which(temp$decimalLatitude<(mean(temp$decimalLatitude)-(3*sd(temp$decimalLatitude)))))>0){
    temp <- temp[-which(temp$decimalLatitude<(mean(temp$decimalLatitude)-(3*sd(temp$decimalLatitude)))),]}
  cleaned_occurrences <- rbind(cleaned_occurrences,temp)
}

##### #### ### ### ### now plot these and look at them: 

# Define projections for orch dist data: 
wgs1984 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
behr <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")

repro <- cleaned_occurrences[, c("decimalLongitude", "decimalLatitude")] %>% SpatialPoints(proj4string = wgs1984) %>% 
  spTransform(behr)
pointsSPDF <- SpatialPointsDataFrame(coords=repro,data=cleaned_occurrences)

crs(pointsSPDF)<-crs(Nacells)

newDistOCells<- over(pointsSPDF, Nacells)
newDistOCells$orchid<- pointsSPDF@data$Acc_name
newDistOCells<- newDistOCells %>% distinct(orchid, layer)
orch<- unique(newDistOCells$orchid)


w<-list()
i<-1
for (g in (orch)){
  print(g)
  query<-g
  spcells<- newDistOCells %>% filter(orchid==query)
  temp<-spcells %>% dplyr::select(layer)
  temp$count<-1
  temp<-temp %>% filter(!(is.na(layer)))
  temp<-temp %>% distinct()
  if(nrow(temp)>=1){
  tempNAcells<-Nacells
  tempNAcells@data<-full_join(tempNAcells@data, temp)
  
  pl<-spplot(tempNAcells,"count", main = query, 
             
             col = "gray")
  w[[i]] <- pl
  i <- i + 1
  }
}



#print all plots to a pdf
pdf('OrchDist.NotIncluded.pdf')#Create platmaps.pdf as a blank pdf and then R loads data onto it.
pdf.options(width = 9, height = 7)
for (i in 1:length(w)){
  print(w[[i]])
}
dev.off()

plot(Nacells)
text(Nacells, labels = Nacells@data$layer, cex = 0.3, col = "black")

## now get rid of the ones you don't want. (I'm just gonna do this manually bc its easier I think)

NewDistOcells2<- newDistOCells %>% filter(!(orchid=="Cypripedium_calceolus"|
                                              orchid=="Habenaria_rotundifolia"|orchid=="Sarcoglottis_schaffneri"|
                                              orchid=="Dichromaanthus_yucundaa"
                                              ))
NewDistOcells3<- NewDistOcells2 %>% filter(!(orchid=="Spiranthes_laciniata" & layer=="1009")
                                           |(orchid=="Malaxis_unifolia" & layer=="636"))
NewDistOcells3$X<-1:39156


 ###### for orchids with data, for each cell count whether there is an overlap with a known pollinator:
PolRel<-read.csv("orchidAndpoll_rangeoverlapDynamics.csv")
PollCells<-read.csv("PollCells.FINAL.csv")
    #1) just do the ones with Pollinator Data: (NOT SELFERS)
OrchCellsWithPollDat<-OrchCells_clean
OrchCellsWithPollDat <- OrchCellsWithPollDat %>% select(orchid, layer)
OrchCellsWithPollDat<- OrchCellsWithPollDat %>% distinct()

for(i in 1:nrow(OrchCellsWithPollDat)){
  query<-OrchCellsWithPollDat$orchid[i]
  lay<-OrchCellsWithPollDat$layer[i]
  Rels<- PolRel %>% filter(orchid==query)
  pcell<-PollCells %>% filter(poll %in% Rels$poll)
  if(lay %in% pcell$layer){
    OrchCellsWithPollDat$Present[i]<-1
  }
  if(!(lay %in% pcell$layer)){
    OrchCellsWithPollDat$Present[i]<-0
  }
  
  
}

    #2) Now get some info for the selfers: 
SelfDat<- OrchCells_self %>% filter(!(orchid %in% OrchCellsWithPollDat$orchid))
SelfDat<-SelfDat %>% select(orchid, layer)
SelfDat<-SelfDat %>% distinct()
SelfDat$Present<-1

  #3) Now get the ones with NO data at all: 
NoDatOrchs<- NewDistOcells3
NoDatOrchs<- NoDatOrchs %>% dplyr::select(orchid, layer)
NoDatOrchs<-NoDatOrchs %>% distinct()
NoDatOrchs$Present<-0
  #4) Now join everything together: 
rm(ALLocells)
ALLocells<- rbind(OrchCellsWithPollDat, SelfDat, NoDatOrchs)

ALLocells<- ALLocells %>% filter(!(is.na(layer)))

write.csv(ALLocells, "orchCells.ALLORCHS_selfOutNoDat.csv")

ALLocells <-ALLocells %>% distinct(orchid, layer, .keep_all = TRUE)
##now do some counting: 
OrchidDataCoverage <- ALLocells %>% group_by (layer) %>% summarize(Num=n(), HaveDat=sum(Present==1))
Test<-OrchidDataCoverage %>%filter(!(layer %in% Nacells@data$layer))

OrchidDataCoverage$percentCoverage<- OrchidDataCoverage$HaveDat/OrchidDataCoverage$Num
write.csv(OrchidDataCoverage, "Cells.DataCoverage.NAorchs.csv")

DatCov<- OrchidDataCoverage %>% filter(layer %in% Nacells@data$layer)

## now make your na cells map and plot it: 
  # make sure you read in the right thing: 
      #this is the coverage for cells that you made above. 
orchCov<-read.csv("Cells.DataCoverage.NAorchs.csv", row.names = 1)
        #if you're using the right NAcells, this shouldn't get rid of any cells: ( YOU USED NACELLSNEWUSE)
DatCov<-orchCov %>% filter(layer %in% Nacells@data$layer)

#now combine to plot them: 
CoverageNAcells<-Nacells

CoverageNAcells@data<- full_join(CoverageNAcells@data, orchCov )


##plotting coverage: (do the thing and see if you can change the label name)
CovPlot<-spplot(CoverageNAcells,"percentCoverage", 
           col = "gray", par.settings = list(axis.line = list(col = "transparent")))
CovPlot

#Plotting Richness: #florida is undersampled, bc of a lot of rare species, but floridian orchid pollinators are also undersampled! 
RichPlot<-spplot(CoverageNAcells, "Num", main= "Orchid Species Richness", col="gray")
RichPlot



    ### PLOT COVERAGE IN GGPLOT: #### #### ### ### 

### TRY GGPLOT ##### ##### ##### ##### ##### ##### ##### ##### ##### 
CoverageNAcells2<-CoverageNAcells

## 1: add an "id" column to your sp dataframe
CoverageNAcells2@data$id <- rownames(CoverageNAcells2@data)

## 2:  create a data.frame from our spatial object
CoverageRD <- fortify(CoverageNAcells2, region = "id")

# merge the "fortified" data with the data from our spatial object
CoverageRDF <- merge(CoverageRD, CoverageNAcells2@data,
                  by = "id")

# now plot it: 
coverageGG<- ggplot(data = CoverageRDF, aes(x = long, y = lat, group = group, fill = percentCoverage)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_c(name="Proportion of orchids with known pollinators", na.value="grey")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Pollinator Observation Completeness")+ 
  theme(plot.title = element_text(hjust = 0.5, size=20))


coverageGG

##**##**##**##**##**#plotting Data Completeness on a Tree: ##**##**##**##**##**
##*
##*
Taxa<-otree$tip.label
taxa<-as.data.frame(Taxa)
for (i in 1:nrow(taxa)){
  query<-taxa$Taxa[i]
  if(query %in% NoDatOrchs$orchid){
    taxa$haveData[i]<-0
  }
  if(query %in% SelfDat$orchid | query %in% OrchCellsWithPollDat$orchid){
    taxa$haveData[i]<-1
  }
}


##now do a trait plot: (with DotTree)
  #set up the named Trait: 
AnyDat<-setNames(as.factor(taxa$haveData), taxa$Taxa)
  #set up the Cols
cols<-setNames(c("purple", "yellow"),levels(AnyDat))
  #plot it on the tree
dotTree(otree, AnyDat, fsize=.5, colors=cols, lwd=.4)

#######Now also get some info about pollinator coverage, and distribution data:
AnyDatdf<-as.data.frame(AnyDat)
#1): orchs with Dist Dat:
AnyDatdf$DistDat<- rownames(AnyDatdf) %in% OrchsWithDist
        ##now convert to 1 and 0 (keep as numeric for heatmap!!) 
AnyDatdf$DistDat<-as.integer(AnyDatdf$DistDat)

#2): Now get only orchids with >50% coverage of polliantors: 
Beta_coverage<- read.csv("~/Documents/PollinatorCollab/PolPape_Final/Orchid.Clim.Range.PolDynamics.Coverage.csv", header=TRUE)

AnyDatdf$CoverageDat<- rownames(AnyDatdf) %in% Beta_coverage$orchid
    ## now convert to integer: 
AnyDatdf$CoverageDat<-as.integer(AnyDatdf$CoverageDat)

    ##convert AnyDat to integer: 
AnyDatdf$AnyDat<-as.character(AnyDatdf$AnyDat)
AnyDatdf$AnyDat<-as.numeric(AnyDatdf$AnyDat)

    ##now change it back to one and zero: 
AnyDatdf$AnyDat[1]<-0
AnyDatdf[2]<-1

## maybe try to read it in without branch lengths?? (okay, this works!! )

#in order to have a cladogram, you have to transform the branches in figtree first:
oclad<-ape::read.tree("~/Documents/PollinatorCollab/RealProj/Oclad.IQ")



##### NOW DO A HEATMAP OF THE THING
phylo.heatmap(oclad, AnyDatdf, standardize=FALSE, cex=.3, split=c(.9,.1), fsize=.3, pts=F, type="phylogram")
    # this is fine, but it wont get rid of the branch lengths....

#### TRY TRAITPLOT WITH diversitree: 
trait.plot(oclad, AnyDatdf, type="p", cols=list(AnyDat =c("white", "yellow"), DistDat =c("white", "green"), CoverageDat= c("white", "blue")))


##### Coloring the tree by genus name (in phytools):
  #1: get a list of genera in your tree:
genera<-sapply(oclad$tip.label,function(x) strsplit(x,"_")[[1]][1])
genera<-sort(unique(genera))
genera
  #2: find the MRCA for each genus: 
for(i in 1:length(genera)){
  ii<-grep(genera[i],oclad$tip.label)
  ca<-if(length(ii)>1) 
    findMRCA(oclad,oclad$tip.label[ii]) else ii
  oclad<-paintSubTree(oclad,ca,state=as.character(i),
                             anc.state="0",stem=TRUE)
}
  #3: get rid of segments with zero length: 
tol<-max(nodeHeights(oclad))*1e-12
oclad$maps<-lapply(oclad$maps, function(x,tol) 
  if(length(x)>1) x[-which(x<tol)] else x,tol=tol)
  #4: map your colors, and then plot the tree: 
cols<-setNames(c("grey",rainbow(length(genera))),
               0:length(genera))
plot(oclad,fsize=0.7,colors=cols,ftype="i",split.vertical=FALSE,
     lwd=3,xlim=c(-24,120))
par(font=3)
add.simmap.legend(colors=setNames(cols[2:length(cols)],genera),fsize=0.3,
                  prompt=TRUE)



## Do some other coloring (by like subfamily and subtribe/tribe)
TaxDat<-AnyDatdf

TaxDat$Species<- rownames(TaxDat)

TaxDat$Genus<- word(TaxDat$Species, 1, sep="_")

TaxDat<-full_join(TaxDat, genera)

##now make a dataframe with color/factors: 
subfam<-setNames(as.factor(TaxDat$Subfamily), TaxDat$Species)
subtribe<-setNames(as.factor(TaxDat$Subtribe), TaxDat$Species)

## now, try to do it with maybe heatmap?? Or dot tree idk. 

dotTree(oclad, subfam, fsize=.5, colors=cols, lwd=.4)

### ### ### ## ### ### ### ### ### ### ### ### ### ### 
#### GGTREE  ####################
### ### ### ### ### ### ### ### 


##GGTREE Group info: (making the grouping list is kind of hard?)
species<-oclad$tip.label
subfam<-setNames(as.factor(TaxDat$Subfamily), TaxDat$Species)
#you take a single vector of species names, and then you split it by whatever grouping variable you want (which is a named vector)


subfamlist<-split(species, subfam)

#now groupOTU: 
ocladGroup<-groupOTU(oclad, subfamlist)

## Now plot it: 
subfamTree<-ggtree(ocladGroup, aes(color=group), show.legend=FALSE)+
  scale_color_manual(values=c("#90d743", "#31688e", "#35b779", "#443983", "#fde725"))
  


######now try to do clade labels:
    ## 1: first you need to find the nodes: 
ggtree(ocladGroup, aes(color=group))+
  scale_color_viridis_d(name="Subfamily")+
  geom_text(aes(label=node), hjust=-.3)

      ##2: now make your dataframe: 
CladeNodez <- data.frame(
  node = c(208, 293,199,186,362),
  name = c("Orchidoideae", "Epidendroideae", "Vanilloideae", "Cypripedioideae", "Outgroups")
)
      

#now add the colored tree to a heatmap of data completeness: 
##Change the data completeness back to factor: 

AnyDatFac<-AnyDatdf  
AnyDatFac<-as.data.frame(lapply(AnyDatFac, as.factor))
rownames(AnyDatFac)<-rownames(AnyDatdf)



## now make a heatmap with your labeled clades: 
gheatmap(subfamTree, AnyDatFac)+
  geom_cladelab(
    data = CladeNodez,
    mapping = aes(
      node = node, 
      label = name),
    fontsize = 3,
    align = TRUE,
    angle=45,
    show.legend = FALSE)
library(aplot)
#now add the heatmap to the already colored tree: (with these options, and add tip labels)
gheatmap(subfamTree, AnyDatFac, offset=2.5, width=.2, colnames_position='top', font.size=1.6, custom_column_labels = c("Any Data", "Poll Dist Data", ">50% Pollinator Coverage"), hjust=c(.5, .5, .2), colnames_offset_y=1) +
  scale_fill_manual(values=c("#fcfdbf", "#fc8961"), name="Data Presence") +
              geom_cladelab(
              data = CladeNodez,
              mapping = aes(
              node = node, 
              label = name),
              fontsize = 3,
              align = TRUE,
              show.legend = FALSE)




%>% insert_right (geom_cladelab(
    data = CladeNodez,
    mapping = aes(
      node = node, 
      label = name),
    fontsize = 3,
    align = TRUE,
    angle=45,
    show.legend = FALSE))


## I Think that's it for missing data! 

## Get some counts for specific info about taxonomic sampling: 

AnyDatdf$Species<-rownames(AnyDatdf)

SubFamCounts<- full_join(subfamdf, AnyDatdf)

SubFamCounts2 <- SubFamCounts %>% group_by (Subfamily) %>% summarize(num=n(),countAnyDat=sum(AnyDat==1), DistDat=sum(DistDat==1), CoverageDat=sum(CoverageDat==1))

## how many species have turnover importance of zero: 
