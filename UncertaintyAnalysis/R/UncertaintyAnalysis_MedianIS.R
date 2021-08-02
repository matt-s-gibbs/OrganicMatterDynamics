library(tidyverse)
library(SWTools)

load("SAVeg_Readily_Pars.Rdata")

storagenodes<-list.files("../NodeVegetationRelationships/",".csv")

#read in Veg data
vegdata<-lapply(storagenodes,function(x) read_csv(paste0("../NodeVegetationRelationships/",x)))
#remove .csv from storagenodes
storagenodes<-sapply(storagenodes,function(x) substr(x,1,nchar(x)-4))

#generate parameters from distributions
#load("TestGeneraterFunction.R")

#Output<-"HighFlow"
# write.csv(ReadilyAvailableSampled,paste0("UncertaintyParameterValues-Readily-",Output,".csv"))
# write.csv(NonReadilyAvailableSampled,paste0("UncertaintyParameterValues-NonReadily-",Output,".csv"))
# write.csv(Readily,paste0("UncertaintyParameterValues-ReadilyFraction-",Output,".csv"))

load("AccumulationRateskgha-1.Rdata")
X<-t(FallRates$median) #need to transpose out of Box-cox
X<-data.frame(X)
names(X)<-FallRates$Vegetation
FallRates<-X

X<-t(BCPars$med_orig) #need to transpose out of Box-cox
X<-data.frame(X)
names(X)<-BCPars$Vegetation
ReadilyAvailableMedian<-X*10

DecayRateReadily=0.0006
DecayRateNonReadily=0.00006
RunNo<-0
for(iter in 1)
{
  
  FractionDegradable<-0.4

con<-file("InputSet-Median.txt","w")
#writeLines(operation,con)

for(i in 1:length(vegdata))
{
  areas<-vegdata[[i]][,-1]
  levels<-vegdata[[i]][,1]
  
  areas2<-areas
  for(j in 2:nrow(areas)) areas2[j,]<-areas[j,]-areas[j-1,]
  areas<-areas2

  accumulationrates<-ReadilyAvailableMedian
  initReadily<-mean(apply(areas,1,function(x) sum(mapply("*", x[intersect(names(x), names(accumulationrates))],
                                                     accumulationrates[intersect(names(x), names(accumulationrates))]))/sum(x)),na.rm=TRUE)
  accumulationrates<-ReadilyAvailableMedian/FractionDegradable
  initNonReadily<-mean(apply(areas,1,function(x) sum(mapply("*", x[intersect(names(x), names(accumulationrates))],
                                                               accumulationrates[intersect(names(x), names(accumulationrates))]))/sum(x)),na.rm=TRUE)
  
  #fix any divide by 0
 # sep<-c(rep("][",length(levels)-1),"]]")
 # accumulation<-array(0,nrow(levels))
  accumulationrates<-FallRates
  accumulation<-apply(areas,1,function(x) sum(mapply("*", x[intersect(names(x), names(accumulationrates))],
                                                     accumulationrates[intersect(names(x), names(accumulationrates))]))/sum(x))
  #fix any divide by 0
  accumulation[is.nan(accumulation)]<-0
  sep<-c(rep("][",length(accumulation)-1),"]]")
  
  NodeType<-"Nodes."
  if(storagenodes[i]=="Lower Pike") NodeType<-"Links."
  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf A=[[",paste0(levels$Grouping," ",round(accumulation,2),sep,collapse="")),con)
  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf Accumulation Constant=",round(FractionDegradable,2)),con)
  # if(storagenodes[i]=="Upper Pike"|storagenodes[i]=="Mundic")
  # {
   writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Initial Leaf Dry Matter Non Readily Degradable=[[",levels$Grouping[1]," ",initNonReadily,"][",levels$Grouping[length(levels$Grouping)]," ",initNonReadily,"]]"),con)
    writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Initial Leaf Dry Matter Readily Degradable=[[",levels$Grouping[1]," ",initReadily,"][",levels$Grouping[length(levels$Grouping)]," ",initReadily,"]]"),con)
  # }
  
     # writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.DOC Decay Constant At 20 Degree C=",DOCDecomposition),con)
    #  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.First Order DOC Release Rate At 20 Degree C=",DOCReleaseReadily),con)
    #  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.First Order DOC Release Rate At 20 Degree C Non Readily=",DOCReleaseNonReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf K1=",DecayRateReadily),con)
      writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Leaf K2=",DecayRateNonReadily),con)
    #  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Max DOC Released From Component Of Litter At 20 Degree C=",MaxDOCReleaseReadily),con)
    #  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Max DOC Released From Component Of Litter At 20 Degree C Non Readily=",MaxDOCReleaseNonReadily),con)
    #  writeLines(paste0(NodeType,storagenodes[i],".Constituent Data.DOC.Processing Model.Reaeration Coefficient=",Reaeration),con)
  
}

close(con)
# system("C:\\Source\\Source_4.9.0.9485\\RiverSystem.CommandLine.exe -m Client -a \"net.tcp://localhost:6787/eWater/Services/RiverSystemService\"",ignore.stdout=TRUE)
# 
# 
# file.copy("D:\\Source\\BlackwaterUncertainty\\GoyderRuns\\Analysis\\River Murray Model 4.9_Nov2019_ScHighFlow - Scenario 1.res.csv",
#           paste0("D:\\Source\\BlackwaterUncertainty\\GoyderRuns\\",Output,"\\",RunNo,".csv"))
# RunNo<-RunNo+1
}

