library(tidyverse)
library(SWTools)
library(readxl)

load("../UncertaintyAnalysis/TS1ForModelResult.RData")


# p2<-ggplot(TS1%>% filter(!grepl("Lock",Site)),aes(x=Date,colour=Scenario,fill=Scenario))+
#             geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.5)+
#             #  geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill="red",alpha=0.9)+
#             geom_line(aes(y=`50%`))+
#             facet_grid(vars(Site),
#                        scales = "free_y", switch = "y")+
#             #  geom_line(data=operation,aes(date,level/2),lty="dashed",colour="grey")+
#             #  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Water Level (m AHD)"))+
#   ylab(bquote("Dissolved oxygen"~(mg.L^-1)))+xlab("Date")+theme_bw()+#ylim(c(0,11))+
#             theme_bw()+
#   theme(legend.position = "top",
#         strip.background = ggplot2::element_blank(),
#         strip.placement = "outside") +
#             scale_x_date(date_breaks="months",date_labels="%d-%b")

p2<-ggplot(TS1 %>% filter(Site!="Lock 4" & Site!="Lock 5"),aes(x=Date,colour=Scenario,fill=Scenario))+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.5)+
  #  geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill="red",alpha=0.9)+
  geom_line(aes(y=`50%`))+
  facet_grid(vars(Site),
             #scales = "free_y",
             switch = "y")+
  #  geom_line(data=operation,aes(date,level/2),lty="dashed",colour="grey")+
  #  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Water Level (m AHD)"))+
  ylab(bquote("Dissolved oxygen"~(mg~L^-1)))+xlab("Date")+theme_bw()+#ylim(c(0,11))+
  theme_bw()+
  theme(legend.position = "top",
        strip.background = ggplot2::element_blank(),
        strip.placement = "outside") +
  scale_x_date(date_breaks="months",date_labels="%d-%b")


ggsave(paste0("DO_Other.tiff"),p2,width=15,height=22,units="cm",dpi=600,compression = "lzw")

          