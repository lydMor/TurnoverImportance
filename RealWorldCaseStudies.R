### REAL WORLD EXAMPLES::: 
rm(list=ls())
library(raster)
library(rgdal)
##**##**##**##**##**##**##**##** LOWAD YOUR DATASETS) ##**##**##**##**##**
##* 1): Pollinator Relationship DF: 
PollRel<- read.csv("SpeciesLevelPollRelationships_long.csv")
##* 2): NAcells: 
Nacells<-readOGR("naCellsNEWREALUSE.shp")
##* 3): PollNACells: 
PCells<-read.csv("PollCells.FINAL.csv")
##* 4):OrchNACells: 
OCells<- read.csv("orchCells.ALLORCHS_selfOutNoDat.csv")


##**##**##**##**##**##**##**##**## LOW TI (HIGH NESTEDNESS**##**##**##**##
##* Spirantehs casei: 
    #filter pollRel for just casei obs:
S_Casei<- PollRel %>% filter(name=="Spiranthes_casei")

  #filter Ocells to only include Casei Range: 
cells_Casei <- OCells %>% filter(orchid=="Spiranthes_casei")
cells_CaseiTest<- cells_Casei %>% filter(layer %in% layer)

  #filter Pcells for just those species that pollinate casei:
pols_Casei<- PCells %>% filter(poll %in% S_Casei$species)

  #filter pols_casei to only include casei Range: 
pols_Casei<- pols_Casei %>% filter(layer %in% cells_Casei$layer )

  #now get only one observation per cell: 
pols_Casei<- pols_Casei %>% distinct(poll, layer)

###### 1: Plot Richness Gradients: 
NaCellsCasei<-Nacells
  #get richness summary:
Casei_richness<- pols_Casei %>% group_by(layer) %>% summarize(Richness=n())
  #Join your richness summary with nacellscasei data: 
NaCellsCasei@data<- full_join (NaCellsCasei@data, Casei_richness)


  #plot it: (USING SSPLOT)
Casei_rPlot<-spplot(NaCellsCasei, "Richness", main=expression(italic("Spiranthes Casei")),
                    sub="Beta Diversity=.719 | Turnover Importance= -1", par.settings = list(axis.line = list(col = "transparent")),
                    col = "gray")
Casei_rPlot

### TRY GGPLOT ##### ##### ##### ##### ##### ##### ##### ##### ##### 

NaCellsCasei2<-NaCellsCasei

## 1: add an "id" column to your sp dataframe
NaCellsCasei2@data$id <- rownames(NaCellsCasei2@data)

## 2:  create a data.frame from our spatial object
CaseiRD <- fortify(NaCellsCasei2, region = "id")

# merge the "fortified" data with the data from our spatial object
CaseiRDF <- merge(CaseiRD, NaCellsCasei2@data,
                   by = "id")

# now plot it: 
caseiRichGG<- ggplot(data = CaseiRDF, aes(x = long, y = lat, group = group, fill = Richness)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_c(name="Pollinator Richness", na.value="grey", option="A")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Spirathes casei")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=20))
 
   
caseiRichGG

################## 2: TAXONOMIC SIMILARITY PLOT: ########

    #A: create a community matrix of your things:
pols_Casei$value<-1
matrix_Casei<- reshape2::dcast(pols_Casei, layer~poll)
matrix_Casei[is.na(matrix_Casei)]<-0
rownames(matrix_Casei)<-matrix_Casei$layer
matrix_Casei<-matrix_Casei[-1]
matrix_Casei<-as.matrix(matrix_Casei)

  #B: now create a distance matrix:
casei_dist<-vegdist(matrix_Casei, method= "jaccard")
casei_dist2<-as.matrix(casei_dist)
clust.spec<-hclust(casei_dist, method = "average")
plot(clust.spec, cex=.4)

  #C: use simprof to get significant clusters:
library(clustsig)
          #data is the distnce matrix you made above, cluster method is UPGMA, distnace method is binary:
Caseiclust<-simprof(data = casei_dist2, method.cluster = "average", method.distance="binary")

 
  #D: get the groups out of your cluster list: 

      #create an empty dataframe and extract the significant clusters list
caseiGDF<-data.frame()
groupsCasei<-Caseiclust$significantclusters
      #turn the list into a dataframe
for(i in 1:length(groupsCasei)){
  temp<-as.data.frame(groupsCasei[[i]])
  names(temp)[1]<-"layer"
  temp$group<-paste(i)
  caseiGDF<-rbind(caseiGDF, temp)
}
    #fix the classes of your variables
caseiGDF[1==caseiGDF]<-"A"
caseiGDF[2==caseiGDF]<-"B"
caseiGDF$group<-as.factor(caseiGDF$group)
caseiGDF$layer<-as.integer(caseiGDF$layer)

  #D: Now map it:
naCaseiClust<-Nacells

naCaseiClust@data<-full_join(naCaseiClust@data, caseiGDF)


#Plot it: 
CaseiClustPlot<- spplot(naCaseiClust,"group", main = expression(italic("Spiranthes Casei Pollinator Compositional clusters")), 
                        sub = "UPGMA significant clusters", par.settings = list(axis.line = list(col = "transparent")),
                        col = "grey")
CaseiClustPlot

    #### GGPLOT IT: ######
naCaseiClust2<-naCaseiClust

## 1: add an "id" column to your sp dataframe
naCaseiClust2@data$id <- rownames(naCaseiClust2@data)

## 2:  create a data.frame from our spatial object
CaseiC <- fortify(naCaseiClust2, region = "id")

# merge the "fortified" data with the data from our spatial object
CaseiCDF <- merge(CaseiC, naCaseiClust2@data,
                  by = "id")

# now plot it: 
caseiClustGG<- ggplot(data = CaseiCDF, aes(x = long, y = lat, group = group.x, fill = group.y)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_d(option="A", name="Pollinator Taxonomic Clusters", breaks=c("A", "B"), na.value="grey")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Spirathes casei", caption= "Beta diversity=.719 | Turnover importance= -1")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=22), plot.caption = element_text(size=18))


caseiClustGG


##**##**##**##**##**##**##** HIGH TI (HIGH TURNOVER IMPORTANCE) **##**##**##**##**##
##*Spiranthes romanzoffiana: 


##### SPECIES RICHNESS GRADIENTS: 

#filter pollRel for just romanzoffiana obs:
S_roma<- PollRel %>% filter(name=="Spiranthes_romanzoffiana")

#filter Ocells to only include romanzoffiana Range: 
cells_roma <- OCells %>% filter(orchid=="Spiranthes_romanzoffiana")


#filter Pcells for just those species that pollinate romanzoffiana:
pols_roma<- PCells %>% filter(poll %in% S_roma$species)

#filter pols_casei to only include romanzoffiana Range: 
pols_roma<- pols_roma %>% filter(layer %in% cells_roma$layer )

#now get only one observation per cell: 
pols_roma<- pols_roma %>% distinct(poll, layer)

###### 1: Plot Richness Gradients: 
NaCellsRoma<-Nacells
#get richness summary:
roma_richness<- pols_roma %>% group_by(layer) %>% summarize(Richness=n())
#Join your richness summary with nacellscasei data: 
NaCellsRoma@data<- full_join (NaCellsRoma@data, roma_richness)


#plot it: 
Roma_rPlot<-spplot(NaCellsRoma,"Richness", main=expression( italic("Spiranthes romanzoffiana Pollinator Richness Gradient")),
                    sub="Beta Diversity=.979 | Turnover Importance= .938",
                    col = "gray", par.settings = list(axis.line = list(col = "transparent")))
Roma_rPlot

## ### ### ### ### ### GGPLOT IT: ### ### ### ### ### 
NaCellsRoma2<-NaCellsRoma

## 1: add an "id" column to your sp dataframe
NaCellsRoma2@data$id <- rownames(NaCellsRoma2@data)

## 2:  create a data.frame from our spatial object
romaRD <- fortify(NaCellsRoma2, region = "id")

# merge the "fortified" data with the data from our spatial object
romaRDF <- merge(romaRD, NaCellsRoma2@data,
                  by = "id")

# now plot it: 
romaRichGG<- ggplot(data = romaRDF, aes(x = long, y = lat, group = group, fill = Richness)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_c(name="Pollinator Richness", na.value="grey", option="A")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Spirathes romanzoffiana")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=20))

romaRichGG

######## COMPOSITIONAL TURNOVER: #############

#A: create a community matrix of your things:
pols_roma$value<-1
matrix_Roma<- reshape2::dcast(pols_roma, layer~poll)
matrix_Roma[is.na(matrix_Roma)]<-0
rownames(matrix_Roma)<-matrix_Roma$layer
matrix_Roma<-matrix_Roma[-1]
matrix_Roma<-as.matrix(matrix_Roma)

#B: now create a distance matrix:
roma_dist<-vegdist(matrix_Roma, method= "jaccard")
roma_dist2<-as.matrix(roma_dist)
clust.spec<-hclust(roma_dist, method = "average")
plot(clust.spec, cex=.4)

#C: use simprof to get significant clusters:
library(clustsig)
#data is the distnce matrix you made above, cluster method is UPGMA, distnace method is binary:
Romaclust<-simprof(data = roma_dist2, method.cluster = "average", method.distance="binary")


#D: get the groups out of your cluster list: 

#create an empty dataframe and extract the significant clusters list
romaGDF<-data.frame()
groupsRoma<-Romaclust$significantclusters
#turn the list into a dataframe
for(i in 1:length(groupsRoma)){
  temp<-as.data.frame(groupsRoma[[i]])
  names(temp)[1]<-"layer"
  temp$group<-paste(i)
  romaGDF<-rbind(romaGDF, temp)
}
#fix the classes of your variables
romaGDF$group<-as.characterromaGDF$group
romaGDF$group<-chartr(c("1,2,3,4,5,6,7,8,9"), c("A,B,C,D,E,F,G,H,I"), romaGDF$group)
romaGDF['A0'==romaGDF]<-"J"
romaGDF$group<-as.factor(romaGDF$group)
romaGDF$layer<-as.integer(romaGDF$layer)

#D: Now map it:
naRomaClust<-Nacells

naRomaClust@data<-full_join(naRomaClust@data, romaGDF)


#Plot it: 
pallete<-brewer.pal(name="Spectral", n=10)
romaClustPlot<- spplot(naRomaClust,"group", main = expression(italic("Spiranthes romanzoffiana Pollinator Compositional Clusters")), 
                        sub = "UPGMA significant clusters", col.regions=pallete,
                        col = "grey", par.settings = list(axis.line = list(col = "transparent")))
romaClustPlot

    ## #### ##### #### GGPLOT IT ### #### #### #### #### #### #### 
"Beta Diversity=.979 | Turnover Importance= .938"

naRomaClust2<-naRomaClust

## 1: add an "id" column to your sp dataframe
naRomaClust2@data$id <- rownames(naRomaClust2@data)

## 2:  create a data.frame from our spatial object
romaC <- fortify(naRomaClust2, region = "id")

# merge the "fortified" data with the data from our spatial object
romaCDF <- merge(romaC, naRomaClust2@data,
                  by = "id")

# now plot it: 
romaClustGG<- ggplot(data = romaCDF, aes(x = long, y = lat, group = group.x, fill = group.y)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_d(option="A", name="Pollinator Taxonomic Clusters", breaks=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), na.value="grey")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Spirathes romanzoffiana", caption= "Beta diversity=.979 | Turnover importance= .938")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=22), plot.caption = element_text(size=18))


romaClustGG



##**##**##**##**##**##**##**## LOW TI (EQUAL NESTEDNESS AND TURNOVER IMPORTANCE) **##**##**##**##**##**##
##* Isotria verticillata: 

##### SPECIES RICHNESS GRADIENTS: 

#filter pollRel for just romanzoffiana obs:
I_vert<- PollRel %>% filter(name=="Isotria_verticillata")

#filter Ocells to only include romanzoffiana Range: 
cells_vert <- OCells %>% filter(orchid=="Isotria_verticillata")


#filter Pcells for just those species that pollinate romanzoffiana:
pols_vert<- PCells %>% filter(poll %in% I_vert$species)

#filter pols_casei to only include romanzoffiana Range: 
pols_vert<- pols_vert %>% filter(layer %in% cells_vert$layer )

#now get only one observation per cell: 
pols_vert<- pols_vert %>% distinct(poll, layer)

###### 1: Plot Richness Gradients: 
NaCellsVert<-Nacells
#get richness summary:
vert_richness<- pols_vert %>% group_by(layer) %>% summarize(Richness=n())
#Join your richness summary with nacellscasei data: 
NaCellsVert@data<- full_join (NaCellsVert@data, vert_richness)


#plot it: 
Vert_rPlot<-spplot(NaCellsVert,"Richness", main=expression( italic("Isotria verticillata Pollinator Richness Gradient")),
                   sub="Beta Diversity=..886 | Turnover Importance= .01", 
                   col = "gray", par.settings = list(axis.line = list(col = "transparent")))
Vert_rPlot

### #### ### GGPLOT IT: ### ### 
## ### ### ### ### ### GGPLOT IT: ### ### ### ### ### 
NaCellsVert2<-NaCellsVert

## 1: add an "id" column to your sp dataframe
NaCellsVert2@data$id <- rownames(NaCellsVert2@data)

## 2:  create a data.frame from our spatial object
vertRD <- fortify(NaCellsVert2, region = "id")

# merge the "fortified" data with the data from our spatial object
vertRDF <- merge(vertRD, NaCellsVert2@data,
                 by = "id")

# now plot it: 
vertRichGG<- ggplot(data = vertRDF, aes(x = long, y = lat, group = group, fill = Richness)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_c(name="Pollinator Richness", na.value="grey", option="A")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Isotria verticillata")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=20))

vertRichGG
######## COMPOSITIONAL TURNOVER: #############

#A: create a community matrix of your things:
pols_vert$value<-1
matrix_Vert<- reshape2::dcast(pols_vert, layer~poll)
matrix_Vert[is.na(matrix_Vert)]<-0
rownames(matrix_Vert)<-matrix_Vert$layer
matrix_Vert<-matrix_Vert[-1]
matrix_Vert<-as.matrix(matrix_Vert)

#B: now create a distance matrix:
vert_dist<-vegdist(matrix_Vert, method= "jaccard")
vert_dist2<-as.matrix(vert_dist)
clust.spec<-hclust(roma_dist, method = "average")
plot(clust.spec, cex=.4)

#C: use simprof to get significant clusters:
library(clustsig)
#data is the distnce matrix you made above, cluster method is UPGMA, distnace method is binary:
Vertclust<-simprof(data = vert_dist2, method.cluster = "average", method.distance="binary")


#D: get the groups out of your cluster list: 

#create an empty dataframe and extract the significant clusters list
vertGDF<-data.frame()
groupsVert<-Vertclust$significantclusters
#turn the list into a dataframe
for(i in 1:length(groupsVert)){
  temp<-as.data.frame(groupsVert[[i]])
  names(temp)[1]<-"layer"
  temp$group<-paste(i)
  vertGDF<-rbind(vertGDF, temp)
}
#fix the classes of your variables
vertGDF$group<-chartr(c("1,2,3,4"), c("A,B,C,D"), vertGDF$group)

vertGDF$group<-as.factor(vertGDF$group)
vertGDF$layer<-as.integer(vertGDF$layer)

#D: Now map it:
naVertClust<-Nacells

naVertClust@data<-full_join(naVertClust@data, vertGDF)


#Plot it: 
    #change your pallette for larger numbers of groups ( n= number of significant clusters)
pallete<-brewer.pal(name="Spectral", n=4)

vertClustPlot<- spplot(naVertClust,"group", main = expression(italic("Isotria verticillata Pollinator Compositional Turnover")), 
                       sub = "UPGMA significant clusters", col.regions=pallete,
                       col = "grey", par.settings = list(axis.line = list(col = "transparent")), )
vertClustPlot


### #### #### ### GGPLOT IT: ### ### ### ### 
## #### ##### #### GGPLOT IT ### #### #### #### #### #### #### 
"Beta Diversity=..886 | Turnover Importance= .01"

naVertClust2<-naVertClust

## 1: add an "id" column to your sp dataframe
naVertClust2@data$id <- rownames(naVertClust2@data)

## 2:  create a data.frame from our spatial object
vertC <- fortify(naVertClust2, region = "id")

# merge the "fortified" data with the data from our spatial object
vertCDF <- merge(vertC, naVertClust2@data,
                 by = "id")

# now plot it: 
vertClustGG<- ggplot(data = vertCDF, aes(x = long, y = lat, group = group.x, fill = group.y)) +
  geom_polygon() +
  geom_path(color = "grey", size = 0.2) +
  scale_fill_viridis_d(option="A", name="Pollinator Taxonomic Clusters", breaks=c("A", "B", "C", "D"), na.value="grey")+
  coord_equal()+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black"))+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Isotria verticillata", caption= "Beta diversity=.886 | Turnover importance= .01")+ 
  theme(plot.title = element_text(hjust = 0.5, face="italic", size=22), plot.caption = element_text(size=18))


vertClustGG



