remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)

scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
resp=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "responses")
quest=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                         sheet = "questions")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
coding= read_csv("C:/github/extreme_weather_risk/data/coding_report.csv")
names(coding)[5]='id_sub'
styles= read_csv("C:/github/extreme_weather_risk/data/styles_desc.csv")
f.style=coding[coding$id_q==0,c('id_I','value')]
names(f.style)[2]='fishing_style'
cod.unc= read_csv("C:/github/extreme_weather_risk/data/coding_report_unc.csv")


###
event.vals=cod.unc[!is.na(cod.unc$id_sub) & is.na(cod.unc$unit_range),]
event.vals$fishing_style=as.factor(event.vals$fishing_style)
event.vals=event.vals[event.vals$value>=0, ]
event.vals=event.vals[event.vals$value<=5, ]
event.vals=event.vals[!is.na(event.vals$value), ]
event.vals=event.vals[event.vals$short_description %in% c('go_out','catchability','catch_condition','personal_safety','damage','avoidance', 'cope_condition', 'catch_assemblage'), ]

event.vals$uncertainty=ifelse(event.vals$uncertainty<0,NA,event.vals$uncertainty)
event.vals$description=factor(event.vals$description, levels = c("Heatwave in the winter season" ,   
                                                                 "Severe icing (in the winter season)"  ,   
                                                                 "Gale" ,
                                                                 "Storm" ,
                                                                 "Heatwave in the summer season" ,  
                                                                 "Algal bloom (in the summer season)"    ,   
                                                                 "Extreme cloudburst (in the summer season)"
                                                                ))


ch=event.vals[event.vals$short_description=='go_out',]
ch[is.na(ch$target),]$target='not_specified'
pl=ggplot(data=ch)+
  geom_col(aes(x=id_I, y=value, group=(target), fill=target), position = 'dodge', color='black')+
  facet_wrap(~description)+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Interviewee ID')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d()
ggsave(plot=pl, 'results/images/goout.jpeg', width = 20, height =12, units='cm')


ch=event.vals[event.vals$short_description=='catch_condition',]
ch[is.na(ch$target),]$target='not_specified'
pl=ggplot(data=ch)+
  geom_col(aes(x=id_I, y=value, group=(target), fill=target), position = 'dodge', color='black')+
  facet_wrap(~description)+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d()
ggsave(plot=pl, 'results/images/condition.jpeg', width = 20, height =12, units='cm')


ch=event.vals[event.vals$short_description=='damage',]
ggplot(data=ch)+
  geom_col(aes(x=id_I, y=value, group=(target), fill=fishing_style), position = 'dodge', color='black')+
  facet_wrap(~description)+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d()


ch=event.vals[event.vals$short_description=='personal_safety',]
ggplot(data=ch)+
  geom_col(aes(x=id_I, y=value, group=(target), fill=fishing_style), position = 'dodge', color='black')+
  facet_wrap(~description)+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d()





ch=event.vals[event.vals$short_description=='catchability',]
ch[is.na(ch$target),]$target='not_specified'
ggplot(data=ch)+
  geom_col(aes(x=description, y=value, group=target, fill=target, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_wrap(~fishing_style)+
  theme_bw()+
  labs(alpha='Certainty', fill='Target')+
  xlab('Type of event')+
  ylab('Answer')+
  ggtitle('Is there a difference in catchability?')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d()

