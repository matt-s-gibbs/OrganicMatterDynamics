library(tidyverse)
library(sensitivity)
library(SWTools)

RunNo<<-1

RunModel<-function(X)
{

storagenodes<-list.files("../NodeVegetationRelationships/",".csv")

#read in Veg data
vegdata<-lapply(storagenodes,function(x) read.csv(paste0("../NodeVegetationRelationships/",x)))
storagenodes<-sapply(storagenodes,function(x) substr(x,1,nchar(x)-4))

#median values from litter data, from values generated for uncertainty analysis
#FractionDegradable<-0.368
# accumulationrates<-tibble(black.box.woodland=0.694419717,
#                           lignum.shrubland=0.391040699,
#                           mixed.woodland=0.960329277,
#                           red.gum.woodland=0.698576778,
#                           shrublands=0.480732932)*10

  DOCDecomposition<-X[1]
  DOCReleaseReadily<-X[2]
  DOCReleaseNonReadily<-X[3]
  MaxDOCReleaseReadily<-X[4]
  MaxDOCReleaseNonReadily<-X[5]
  Reaeration<-X[6]
  WaterTemp<-X[7]
  FractionDegradable<-0.3
  initReadily=X[8]*10 #convert from g/m2 to kg/ha
  initNonReadily=X[9]*10
  accumulation=0
  DecayRateReadily<-0.0
  DecayRateNonReadily<-0.0
  
con<-file("../Model/InputSet.txt","w")
writeLines("Functions.Variables.$Storages.Pike.pw_PikeOps.Relationship=[[1 14.55][152 14.55][252 16.4][292 16.4][329 14.55][366 14.55]]",con)

for(i in 1:length(vegdata))
{
  areas<-vegdata[[i]][,-1]
  levels<-vegdata[[i]][,1]
  
  areas2<-areas
  
  for(j in 2:nrow(areas)) areas2[j,]<-areas[j,]-areas[j-1,]
  areas<-areas2
  areas[areas<0]<-0
  
  
  #accumulation<-apply(areas,1,function(x) sum(mapply("*", x[intersect(names(x), names(accumulationrates))],
  #                                                   accumulationrates[intersect(names(x), names(accumulationrates))]))/sum(x))  #fix any divide by 0
  #accumulation[is.nan(accumulation)]<-0
  #sep<-c(rep("][",length(accumulation)-1),"]]")
  
  NodeType<-"Nodes."
  if(storagenodes[i]=="Lower Pike") NodeType<-"Links."

  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf A=[[",levels[1]," ",accumulation,"][",levels[length(levels)]," ",accumulation,"]]"),con)
  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf Accumulation Constant=",round(FractionDegradable,2)),con)
  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Initial Leaf Dry Matter Non Readily Degradable=[[",levels[1]," ",initNonReadily,"][",levels[length(levels)]," ",initNonReadily,"]]"),con)
  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Initial Leaf Dry Matter Readily Degradable=[[",levels[1]," ",initReadily,"][",levels[length(levels)]," ",initReadily,"]]"),con)
     writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.DOC Decay Constant At 20 Degree C=",DOCDecomposition),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.First Order DOC Release Rate At 20 Degree C=",DOCReleaseReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.First Order DOC Release Rate At 20 Degree C Non Readily=",DOCReleaseNonReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Max DOC Released From Component Of Litter At 20 Degree C=",MaxDOCReleaseReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Max DOC Released From Component Of Litter At 20 Degree C Non Readily=",MaxDOCReleaseNonReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Reaeration Coefficient=",Reaeration),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Water Temperature=",WaterTemp),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf K1=",DecayRateReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf K2=",DecayRateNonReadily),con)
}

close(con)

#source is set to auto export runs, so just need to run the model.
system("../Model/RunModel.bat")

res<-read_res.csv("D:\\Source\\BlackwaterUncertainty\\Model\\Results\\Uncertainty SA Murray 4.8.2 Operation - Scenario 1.res.csv","t")
res<-res %>% 
  select(Date,DO="Pike Minimum Flow.Constituents@DO@Downstream Flow Concentration") %>% 
  mutate(DO=DO*1000) %>%
  filter(Date>="2014-06-01" & Date<="2017-11-25") %>% 
  summarise(mean=mean(DO,na.rm=TRUE))

x<-res %>% pull(mean)
if(is.na(x)|is.nan(x)) x<-8

file.copy("D:\\Source\\BlackwaterUncertainty\\Model\\Results\\Uncertainty SA Murray 4.8.2 Operation - Scenario 1.res.csv",
          paste0("D:\\Source\\BlackwaterUncertainty\\Sensitivity_Aug2020\\Sensivity SA Murray 4.8.2 Operation",RunNo,".csv"))
RunNo<<-RunNo+1

#set mean close to 0
return(x)
}

ApplyModel<-function(vals)
{
  # return(apply(X,1,function(x) RunModel(x))) DOESN"T WORK??? 
  out<-array(0,nrow(vals))
  for(i in 1:nrow(vals))
  {
    out[i]<-RunModel(vals[i,])
  }
  return(out)
}

n=1000
X1<-data.frame(DOCDecomposition=runif(n,0.02,0.3),
               DOCReleaseReadily=runif(n,0.4,0.9),
               DOCReleaseNonReadily=runif(n,0.05,0.2),
               MaxDOCReleaseReadily=runif(n,45,125),
               MaxDOCReleaseNonReadily=runif(n,4.5,12.5),
               Reaeration=runif(n,0.01,0.08),
               WaterTemp=runif(n,10,25),
               InitialReadily=runif(n,0,1000),
               InitialReadily=runif(n,0,3000))

X2<-data.frame(DOCDecomposition=runif(n,0.02,0.3),
               DOCReleaseReadily=runif(n,0.4,0.9),
               DOCReleaseNonReadily=runif(n,0.05,0.2),
               MaxDOCReleaseReadily=runif(n,45,125),
               MaxDOCReleaseNonReadily=runif(n,4.5,12.5),
               Reaeration=runif(n,0.01,0.08),
               WaterTemp=runif(n,10,25),
               InitialReadily=runif(n,0,1000), #g/m2, converted to kg/ha in the function.
               InitialReadily=runif(n,0,3000))
write_csv(X1,"X1.csv")
write_csv(X2,"X2.csv")

#SobolOut <- sobolEff(model = ApplyModel, X1, X2)
SobolOut <- soboltouati(model = ApplyModel, X1, X2)
save(SobolOut,file="SobolTouati_Aug2020Mean.Rdata")
