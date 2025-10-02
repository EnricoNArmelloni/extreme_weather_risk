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

## formatting
cod.form=coding%>%
  dplyr::filter(!is.na(id_sub))%>%
  left_join(resp, by = join_by(id_q, id_sub, id_I))%>%
  left_join(evts)
cod.form2=coding%>%
  dplyr::filter(is.na(id_sub))%>%
  left_join(resp[,-which(colnames(resp)=='id_sub')], by = join_by(id_q,  id_I))%>%
  dplyr::mutate(description=NA,event_code=NA)
cod.form=rbind(cod.form, cod.form2[,names(cod.form)])%>%
  left_join(f.style)


## adding uncertainty
cod.form$value=as.numeric(cod.form$value)
cod.form$uncertainty=NA
int.i=unique(cod.form$id_I)
cod.unc=NULL
for(j in 1:length(int.i)){
  j.cod=cod.form[cod.form$id_I== int.i[j],]
  uncertainty.vals=j.cod[grep(j.cod$short_description, pattern='unc'),]
  uncertainty.id=str_split(uncertainty.vals$short_description, '_')
  for(i in 1:nrow(uncertainty.vals)){
    i.id=as.numeric(uncertainty.id[[i]][-1])
    i.unc=uncertainty.vals[i,]$value/10
    if(length(i.id) ==2){
      j.cod[j.cod$id_q %in% i.id[1]:i.id[2],]$uncertainty=i.unc
    }
    if(length(i.id)==1){
      j.cod[grep(j.cod$id_q ,pattern= i.id[1]),]$uncertainty=i.unc
    }
  }
  # clean
  j.cod=j.cod[-which(j.cod$id_q %in% uncertainty.vals$id_q),]
  cod.unc=rbind(cod.unc, j.cod)
}


## some plotting ####
# some plots
hc.vals=cod.unc[is.na(cod.unc$id_sub) & is.na(cod.unc$unit_range),]
hc.vals$fishing_style=as.factor(hc.vals$fishing_style)
hc.vals=hc.vals[hc.vals$value>=0, ]
hc.vals=hc.vals[hc.vals$value<=5, ]
hc.vals=hc.vals[!is.na(hc.vals$value), ]

hc.vals$uncertainty=ifelse(hc.vals$uncertainty<0,NA,hc.vals$uncertainty)

plot.demo=ggplot(data=hc.vals)+
  geom_col(aes(x=id_I, y=value, fill=fishing_style,group=fishing_style, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_wrap(~short_description, scales='free_x')+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom');plot.demo


ggsave(plot=plot.demo, 'results/images/human_community.jpeg', width = 28,height = 15, units='cm', dpi=500)

event.vals=cod.unc[!is.na(cod.unc$id_sub) & is.na(cod.unc$unit_range),]
event.vals$fishing_style=as.factor(event.vals$fishing_style)
event.vals=event.vals[event.vals$value>=0, ]
event.vals=event.vals[event.vals$value<=5, ]
event.vals=event.vals[!is.na(event.vals$value), ]
event.vals=event.vals[event.vals$short_description %in% c('go_out','catchability','catch_condition','personal_safety','damage','avoidance'), ]

event.vals$uncertainty=ifelse(event.vals$uncertainty<0,NA,event.vals$uncertainty)
event.vals$description=factor(event.vals$description, levels = c("Heatwave in the winter season" ,         
                                                                 "Heatwave in the summer season" ,           
                                                                 "Severe icing (in the winter season)"  ,    
                                                                 "Algal bloom (in the summer season)"    ,   
                                                                 "Extreme cloudburst (in the summer season)",
                                                                 "Gale" ,
                                                                 "Storm" ))
plot.demo=ggplot(data=event.vals)+
  geom_col(aes(x=id_I, y=value, group=fishing_style, fill=fishing_style, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_grid(cols=vars(description), rows=vars(short_description), scales='free_x')+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom');plot.demo

 
