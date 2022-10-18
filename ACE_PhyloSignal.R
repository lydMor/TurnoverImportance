## Calculating phylogenetic signal, doing ACE, and plotting pretty trees: 

Pol_order<- read.csv("~/Documents/PollinatorCollab/RealProj/species_Order.LongWithNonNA.csv")

Out<-Pol_order %>% filter(!(order=="self"))
Self<-Pol_order %>% filter(order=="self")

PolStrat<- Pol_order$name
PoLStrat<-as.data.frame(PolStrat)
names(PoLStrat)[1]<-"orchid"
PoLStrat<- PoLStrat %>% distinct(orchid)

for (i in 1:nrow(PoLStrat)){
  query<-PoLStrat$orchid[i]
  if(query %in% Out$name & query %in% Self$name){
    PoLStrat$Strategy[i]<-paste("Both")
  }
  if(query %in% Out$name & !(query %in% Self$name)){
    PoLStrat$Strategy[i]<- paste("Outcrossing")
  }
  if(query %in% Self$name & !(query %in% Out$name)){
    PoLStrat$Strategy[i]<-paste("Selfing")
  }
}

write.csv(PoLStrat, "PollinatorStrategy.OrderLevel.WithNonNA.csv")

### Turnover Importance: (we're looking for a phylogenetic signal)
TurnoverImportance<- ObetaCoverage %>% select(orchid, turnoverImportance)
write.csv(TurnoverImportance, "turnoverImportance.ForTree.csv")


####**##**##**##**##** now do the plots and make the trees ######################
rm(list=ls())
library(ape)
library(phytools)
library(diversitree)
#read in the data you want to map on your tree: 
TurnoverImportance<-read.csv("turnoverImportance.ForTree.csv")
PolStrat<-read.csv("PollinatorStrategy.OrderLevel.WithNonNA.csv")

#now get your tree
otree<-read.tree("~/Documents/PollinatorCollab/RealProj/OrchsIQcon")


###### SELFING ###################

#prune the tree
notkeep<-otree$tip.label
notkeep<-as.data.frame(notkeep)
notkeep<- notkeep %>% filter(!(notkeep %in% PolStrat$orchid))
pruned.Self<-ape::drop.tip(otree, notkeep$notkeep)
#prune your character dataset
OSTree<- PolStrat %>% filter(orchid %in% pruned.Self$tip.label)

#transform your character dataset so that it is a named vector: 
sex<-setNames(as.factor(OSTree$Strategy), OSTree$orchid)

#now make a simulated stochastic map: (for model: #ARD= all rates different, ER=Equal rates, SYM=Symmetrical rates) 
mtrees_ARD<-make.simmap(pruned.Self,sex,model="ARD",nsim=500)

#randomly sample and view some of your trees: (this makes a weird plot idk)
par(mfrow=c(10,10))
null<-sapply(mtrees,plotSimmap,colors=c("green, yellow"),lwd=1,ftype="off")

#summarize your simulations: 
pd_ARD<-summary(mtrees_ARD)
pd_ARD
cols<-setNames(c("orange","red", "yellow"),levels(sex))

##PLOT: 
plot(pd,fsize=0.5,ftype="i", type="phylogram", cex=.3, colors=cols, lwd=.4, use.edge.length=F, tip.labels=F )
add.simmap.legend(colors=cols[2:1],prompt=TRUE)

## OTHER METHOD FOR PLOTTING: 
#plot tips and values at tips first
dotTree(pruned.Self, sex, fsize=.3, colors=cols, lwd=.4, show.tip.label=FALSE, outline=FALSE, )

scale_fill_viridis_d(name="Genus", breaks=c('Platanthera', 'Galearis', 'Spiranthes', 'Dichromanthus', 'Sacolia', 'Goodyera', 'Calopogon',
                                            'Arethusa', 'Tipularia', 'Calypso', 'Cyrtopodium', 'Pogonia', 'Isotria', 'Cleistesiopsis', 'Cypripedium'))
#then add node labels
nodelabels(pie=pd_ARD$ace,piecol=cols,cex=0.2, outline=FALSE)




##### ACE WITH GGTREE ####### ####### ####### ####### ####### ####### ####### 

## maybe try with GGtree?? 
##FIRST: SET YOUR COLORS THAT YOU WANT TO USE: 
cols<-setNames(c("#009E73","#56B4E9", "#F0E442"),levels(sex))
## 1: Make the node label dataframe

#extract the probabilities at each node using your tree that you made in phytools: 
check<-pd_ARD$ace
#turn it in to a dataframe:
check<-as.data.frame(check)
#create a column that says node
check$node<-rownames(check)
#now get only the nodes, and NOT the tips: 
rownames(check)<- 1:nrow(check)
check<- check[1:105,]

#create the plotting image: 
pies <- nodepie(check, cols=1:3, color=cols, alpha=.8)


##2:  prepare your tip dots: 
GGsex<-as.data.frame(sex)
GGsex$tip_label<-rownames(GGsex)
GGsex<- GGsex %>% select(tip_label, sex)
names(GGsex)[2]<-"Pollinator_Strategy"

## combine your tree with your tip dots: 
p <- ggtree(pruned.Self, lwd=.4, branch.length="none") %<+% GGsex

## create the plot with tip labels, tip dots, and stuff
selfGG<-p + geom_tippoint(aes(color=Pollinator_Strategy), alpha=.8)+
  scale_color_manual(values=cols, name="Pollinator Strategy", breaks=c("Outcrossing", "Selfing", "Both"))+
  geom_tiplab(size=1.5, align=TRUE, linesize=.3)+
  hexpand(.4) ## this is what gets your plot to show all of the tip labels! 

selfGG

## 3:  now add your node labels
inset(selfGG, pies, width=.045, height=.045)
##or: 
selfGG + geom_inset(pies) 
##4: Now try to save it?? :IDK THE DOTS DON'T SHOW UP ITS KINDA ANNOYING. 

ggsave("test.pdf")
##### ##### ##### #### #### #### ### ##### ##### ##### ##### ##### ##### ##### ##### 

### SELFING BUT with MCMC: ############ (it was the same??)
mtrees_MCMC<-make.simmap(pruned.Self,sex,Q="mcmc", model="ARD", nsim=500)

#randomly sample and view some of your trees: (this makes a weird plot idk)
par(mfrow=c(10,10))
null<-sapply(mtrees,plotSimmap,colors=c("green, yellow"),lwd=1,ftype="off")

#summarize your simulations: 
pd_MCMC<-summary(mtrees)
pd
cols<-setNames(c("cornflowerblue","indianred1", "yellow"),levels(sex))

##PLOT: 
plot(pd,fsize=0.5,ftype="i", type="phylogram", cex=.3, colors=cols, lwd=.4, use.edge.length=F )
add.simmap.legend(colors=cols[2:1],prompt=TRUE)

## OTHER METHOD FOR PLOTTING: 
#plot tips and values at tips first
dotTree(pruned.Self, sex, fsize=.4, colors=cols, lwd=.4)
#then add node labels
nodelabels(pie=pd$ace,piecol=cols,cex=0.2, lwd=.0)

## let's try the other models: 
#equal rates:
mtrees_ER<-make.simmap(pruned.Self,sex, model="ER", nsim=500)
pd_ER<- summary(mtrees_ER)
pd_ER

#symmetric rates:
mtrees_SYM<-make.simmap(pruned.Self, sex, model="SYM", nsim=500)
pd_SYM<-summary(mtrees_SYM)
pd_SYM


###### TURNOVER IMPORTANCE ##########
## ## ## ## now plot turnover importance on the tree: 
#prune the tree: 
oclad<-ape::read.tree("~/Documents/PollinatorCollab/RealProj/Oclad.IQ")

notkeep<-oclad$tip.label
notkeep<-as.data.frame(notkeep)
notkeep<- notkeep %>% filter(!(notkeep %in% TurnoverImportance$orchid))
pruned.Turnover<-ape::drop.tip(oclad, notkeep$notkeep)
#prune your character dataset
TurnTree<-TurnoverImportance %>% filter(orchid %in% pruned.Turnover$tip.label)

#transform your character dataset so that it is a named vector: 
TurnTree$turnoverImportance<- as.numeric(TurnTree$turnoverImportance)
Turnover<-as.data.frame(TurnTree$turnoverImportance)
rownames(Turnover)<- TurnTree$orchid
names(Turnover)[1]<-"Turnover_Importance"
Turnover<-as.matrix(Turnover)

## plot the tree: 
#phytools: 
plotTree.barplot(pruned.Turnover, Turnover, scale=NULL, width=NULL, type="phylogram", 
                 method="plotTree", tip.labels=FALSE, col="pink", border=NULL)





## Do it in ggtree: (maybe more complicated IDK)

## 1: Color the things by genus name: 
##now make a dataframe with color/factors: 

TurnTax<- as.data.frame(rownames(Turnover))
names(TurnTax)[1]<-"species"
TurnTax$genus<- paste(word(TurnTax$species, 1, sep="_"))

Genus<-setNames(as.factor(TurnTax$genus), TurnTax$species)



##GGTREE Group info: (making the grouping list is kind of hard?)
species<-TurnTax$species

#you take a single vector of species names, and then you split it by whatever grouping variable you want (which is a named vector)
genuslist<-split(species, Genus)

#now groupOTU: 
Group_Turn<-groupOTU(pruned.Turnover, genuslist)

## Now plot it: 
turnTree_gen<-ggtree(Group_Turn, aes(fill=group))+
  scale_color_viridis_d(name="Genus")

turnTree_gen+geom_tiplab()

plot(Group_Turn)

## ###### now align a barchart to the tree: 

## first, get it in the order that it's supposed to be: 
TurnBar<-TurnTree %>% select(orchid, turnoverImportance)
TurnBar

#the basic barplot function (where I had above labeled the fill colors in turnTree_gen)
turnTree_gen +
  geom_facet(panel = "Turnover Importance", data = TurnBar, geom = geom_col, 
             aes(x = turnoverImportance), orientation = 'y', width = .6)+
  scale_fill_viridis_d(name="Genus")



#try to change the order of plotting for the fuckin legend.
turnTree_gen +
  geom_facet(panel = "Turnover Importance", data = TurnBar, geom = geom_col, 
             aes(x = turnoverImportance), orientation = 'y', width = .6)+
  scale_fill_viridis_d(name="Genus", breaks=c('Platanthera', 'Galearis', 'Spiranthes', 'Dichromanthus', 'Sacolia', 'Goodyera', 'Calopogon',
                                              'Arethusa', 'Tipularia', 'Calypso', 'Cyrtopodium', 'Pogonia', 'Isotria', 'Cleistesiopsis', 'Cypripedium'))




## get the info for phylogenetic signal for turnover importance: !! USE THE FULL TREE WITH BRANCH LENGTHS:
phylosig (otree, Turnover, method="lambda", test=TRUE)