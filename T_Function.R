###### T function ############

# 1: Caclulating T from two community matrices and interaction information: 

  #A: required files:
#1: Sp1= community matrix for species group A (ex: orchids; rows=site, cols=species)
#2: Sp2= community matrix for species group B (ex: pollinators; rows=site, cols=species)
      
#3: SpRel= information about the associations between species group A & B 
      #(long form- WITH THESE NAMES: col1="spA", col2="SpB", col3="value")

###### Download example files from: 
    
T_comp<- function(Sp1, Sp2, SpRel){
  require(betapart)
  require(reshape2)
  require(tidyverse)
SpA<<- melt(Sp1) 
names(SpA)<<-c("site", "spA", "value")
SpA<<- SpA %>% filter(value==1)
SpB<<-melt(Sp2)
names(SpB)<<-c("site", "spB", "value")
SpB<<-SpB %>% filter(value==1)
  print("creating Betapart objects")
  
  SpA2<<-unique(SpRel[1])
  names(SpA2)[1]<-"spA"
  #get your things: 
  a<<-1
  Core_list<<-list()
  # Now create a betapart.core object for each orchid in your dataframe: 
  for(i in 1:nrow(SpA2)){
    print(i)
    A<<-paste(SpA2[i,1])
    Acells<<-SpA %>% filter(SpA$spA %in% A)
    ABrel<<- SpRel %>% filter(SpRel$spA %in% A)
    B<<-SpB %>% filter(SpB$spB %in% ABrel$spB)
    B<<-B %>% filter(site %in% Acells$site)
    B<<- B %>% distinct()
    BcomMat<<- reshape2::dcast(B, site~spB)
    BcomMat[is.na(BcomMat)]<<-0
    rownames(BcomMat)<<-BcomMat$site
    BcomMat<<-BcomMat[-1]
    BcomMat<<-as.matrix(BcomMat)
    
      #make a betapart.core object
    core<-betapart.core(BcomMat)
    Core_list[[a]] <<- core
    names(Core_list)[[a]]<<-A
    a <<- a + 1
  }

print("Betapart.core made")



SpA_Beta<<-data.frame(spA=c(SpA2))
for(i in 1:nrow(SpA_Beta)){
  print(i)
  query<<-SpA_Beta$spA[i]
  test<<-beta.multi(Core_list[[query]])
  SpA_Beta$total_sor[i]<<- paste(test$beta.SOR)
  SpA_Beta$nested[i]<<-paste(test$beta.SNE)
  SpA_Beta$simpson[i]<<-paste(test$beta.SIM)
  }
SpA_Beta$total_sor<<-as.numeric(SpA_Beta$total_sor)
SpA_Beta$nested<<-as.numeric(SpA_Beta$nested)
SpA_Beta$simpson<<-as.numeric(SpA_Beta$simpson)
## CALCULATE Turnover Importance: 
SpA_Beta$TurnoverImportance<<- (SpA_Beta$simpson-SpA_Beta$nested)/SpA_Beta$total_sor

print("T calculated")
print(SpA_Beta)
return(SpA_Beta)
}



# example Run: 
Sp1<- read.csv("Sp1.csv")
Sp2<-read.csv("Sp2.csv")
SpRel<- read.csv("SpRel.csv")
T_comp(Sp1=Sp1, Sp2=Sp2, SpRel=SpRel)


