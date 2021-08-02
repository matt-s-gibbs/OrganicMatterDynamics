library(tidyverse)
library(SWTools)
library(readxl)

Inputs<-read_excel("../GoyderRuns/OutputMappingPaper.xlsx")

Outputs<-c("ScA","ScL","HighFlow")
OutputNames<-Outputs
Names<-c("No operations","Site operations","High flow")
names(Names)<-Outputs


startdate<-as.Date("2017-07-01")
enddate<-as.Date("2017-12-31")

Metrics<-NULL
TS<-NULL

for(Output in Outputs)
{

files<-list.files(paste0("D:\\Source\\BlackwaterUncertainty\\GoyderRuns\\",Output),full.names = TRUE)

A<-lapply(files,function(x) read_res.csv(x,"t")) #make this bigger for plotting.

res<-array(0,nrow(Inputs))
for(i in 1:nrow(Inputs))
{
  dat<-A %>% map(paste0(Inputs$Site[i],".",Inputs$DO)[i])%>% 
    set_names(paste0("Run",1:length(A))) %>% 
    bind_rows() %>% 
    bind_cols(Date=A[[1]]$Date) %>% 
    filter(Date>=startdate&Date<enddate) %>% 
    select(-Date) %>% 
    mutate_all(~.*1000)
  
  res[i]<-sum(dat<Inputs$DO_lim[i])/(nrow(dat)*ncol(dat))
  
  
  dat<-dat%>% 
    pmap_df(~as.list(quantile(c(...), c(0.025,0.05,0.1,0.25, .5,0.75,0.9,0.95,0.975)))) %>%
    bind_cols(Date=as.Date(startdate)+0:(nrow(dat)-1)) %>% 
    select(Date,`2.5%`,`50%`,`97.5%`) %>% 
    mutate(Scenario=Output,
           Site=Inputs$Site[i])
  
  TS<-TS %>% bind_rows(dat)
}

#Metrics<- tibble(Value=res,Site=Inputs$Site) %>% mutate(Scenario=Output) %>% bind_rows(Metrics)

}

TS<-TS %>% mutate(Scenario=recode(Scenario,!!!Names),
                  Site=gsub(" Regulator 254","",Site),
                  Site=gsub("Lower ","",Site),
                  Site=factor(Site,levels=c("Lock 6","Chowilla","Lock 5","Pike","Lock 4","Katarapko","Lock 3")))

p1<-ggplot(TS %>% filter(Scenario!="Extreme"),aes(x=Date,colour=Scenario,fill=Scenario))+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.5)+
#  geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill="red",alpha=0.9)+
  geom_line(aes(y=`50%`))+
  facet_grid(vars(Site), 
             scales = "free_y", switch = "y")+
  #  geom_line(data=operation,aes(date,level/2),lty="dashed",colour="grey")+
  #  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Water Level (m AHD)"))+
  ylab(bquote("Dissolved oxygen"~(mg~L^-1)))+xlab("Date")+theme_bw()+ylim(c(0,11))+
  theme_bw()+theme(legend.position = "top", 
                   strip.background = ggplot2::element_blank(), 
                   strip.placement = "outside") + 
  scale_x_date(date_breaks="months",date_labels="%d-%b")
ggsave(paste0("DO_Locks.tiff"),p1,width=18,height=18,units="cm",dpi=600)
ggsave(paste0("DO_Locks.eps"),p1,width=18,height=18,units="cm")
ggsave(paste0("DO_Locks.png"),p1,width=18,height=18,units="cm",dpi=600)

# 
library(lubridate)
TS1<-TS %>% mutate(`50%`=ifelse(Scenario=="Extreme"&month(Date)==9&Site=="Chowilla Regulator 254",10.1,`50%`),
                   `2.5%`=ifelse(Scenario=="Extreme"&month(Date)==9&Site=="Chowilla Regulator 254",10.1,`2.5%`),
                   `97.5%`=ifelse(Scenario=="Extreme"&month(Date)==9&Site=="Chowilla Regulator 254",10.1,`97.5%`))

p2<-ggplot(TS1%>% filter(!grepl("Lock",Site)),aes(x=Date,colour=Scenario,fill=Scenario))+
            geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.5)+
            #  geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill="red",alpha=0.9)+
            geom_line(aes(y=`50%`))+
            facet_grid(vars(Site),
                       scales = "free_y", switch = "y")+
            #  geom_line(data=operation,aes(date,level/2),lty="dashed",colour="grey")+
            #  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Water Level (m AHD)"))+
  ylab(bquote("Dissolved oxygen"~(mg.L^-1)))+xlab("Date")+theme_bw()+#ylim(c(0,11))+
            theme_bw()+
  theme(legend.position = "top",
        strip.background = ggplot2::element_blank(),
        strip.placement = "outside") +
            scale_x_date(date_breaks="months",date_labels="%d-%b")

p2<-ggplot(TS1 %>% filter(Site!="Lock 4" & Site!="Lock 5"),aes(x=Date,colour=Scenario,fill=Scenario))+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.5)+
  #  geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill="red",alpha=0.9)+
  geom_line(aes(y=`50%`))+
  facet_grid(vars(Site),
             #scales = "free_y",
             switch = "y")+
  #  geom_line(data=operation,aes(date,level/2),lty="dashed",colour="grey")+
  #  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Water Level (m AHD)"))+
  ylab(bquote("Dissolved oxygen"~(mg.L^-1)))+xlab("Date")+theme_bw()+#ylim(c(0,11))+
  theme_bw()+
  theme(legend.position = "top",
        strip.background = ggplot2::element_blank(),
        strip.placement = "outside") +
  scale_x_date(date_breaks="months",date_labels="%d-%b")


ggsave(paste0("DO_Other.pdf"),p2,width=15,height=22,units="cm")

          