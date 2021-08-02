library(tidyverse)
library(sensitivity)
library(sensobol) #need 0.2.0 version. A recent update (not sure exact version) doesnt work with this code.
#require(devtools)
#install_version("sensobol", version = "0.2.0", repos = "http://cran.us.r-project.org")

load("SobolTouati_Aug2020Mean.Rdata")

plot(SobolOut)

N<-11000
k<-9
A <- SobolOut$X
Y<-SobolOut$y


params <- c("DOC\nconsumption rate",
            "DOC release rate\nreadily",
            "DOC release rate\nnon-readily",
              "Max. DOC release\nreadily",
              "Max. DOC release\nnon-readily",
              "Reaeration rate",
              "Water temperature",
            "Initial load\nreadily",
            "Initial load\nnon-Readily")

p<-plot_scatter(x = A, 
             Y = Y, 
             n = N, 
             params = params)+
  scale_fill_gradientn(colours = terrain.colors(10))+
  ylab(bquote(Average~dissolved~oxygen~(mgL^-1)))+
  xlab("Parameter value")

ggsave("SensitvityScatter.tiff",width=18,height=18,units="cm",dpi=600,compression = "lzw")

Sens<-SobolOut$T
Sens$`min. c.i.`[Sens$`min. c.i.`<0]<-0

Sens$names<-factor(params,levels=params[order(Sens$original,decreasing = TRUE)])


p<-ggplot(Sens)+geom_bar(aes(names,original),stat="identity")+theme_bw()+
  geom_errorbar(aes(ymin=`min. c.i.`,ymax=`max. c.i.`,x=names),width=0.2)+
  #coord_cartesian(ylim=c(0,1))+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+
  xlab(NULL)+ylab("Sobol' total sensitivity indices")


ggsave("SensitvitySobol.tiff",width=18,height=10,units="cm",dpi=600,compression = "lzw")
