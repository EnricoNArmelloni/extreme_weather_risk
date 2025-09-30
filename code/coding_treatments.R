library(readxl)
library(tidyverse)
rm(list = ls())
summary.path="C:/github/extreme_weather_risk/data/questionnaires_coded.xlsx"
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
target.sheets=readxl::excel_sheets(summary.path)

store.results=NULL
i=7
for(i in 1:length(target.sheets)){
  #template=template[,c(3,7:18)]
  template=readxl::read_excel("C:/github/extreme_weather_risk/data/questionnaires_coded.xlsx",
                              sheet = target.sheets[i])
  temp.quest.1=template[!is.na(template$broad),c('id_q', 'short_description','broad',"range.lwr","range.upr","unit_range")]
  temp.quest.costs=template[!is.na(template$unit_range),c('id_q', 'short_description','broad',"range.lwr","range.upr","unit_range")]
  temp.quest.mult=template[-which(template$id_q %in% c(temp.quest.1$id_q, temp.quest.costs$id_q)),]
  temp.quest.mult=temp.quest.mult[,c(3:6,8:14)]
  temp.quest.mult[,5:11]=apply(temp.quest.mult[,5:11],2,
                                    function(x)as.numeric(str_trim(as.character(x),'both')))
  
  temp.quest.mult=temp.quest.mult%>%
    pivot_longer(-c('id_q', 'gear','area','target'))
  temp.quest.mult$sub=substr(temp.quest.mult$name,1,1)
  temp.quest.mult=temp.quest.mult[,c('id_q','sub','gear','area', 'target', 'value')]
  temp.quest.mult$range.lwr=temp.quest.mult$range.upr=temp.quest.mult$unit_range=NA
  temp.quest.mult$broad=temp.quest.mult$value
  stressquest=temp.quest.1[temp.quest.1$id_q %in% c('21a','21b','21c'),]
  stressquest$sub=substr(stressquest$id_q,3,3)
  stressquest$id_q=substr(stressquest$id_q,1,2)
  stressquest$value=stressquest$broad
  stressquest$gear=stressquest$area=stressquest$target=NA
  temp.quest.1=temp.quest.1[-which(temp.quest.1$id_q %in% c('21a','21b','21c')),]
  quest1=rbind(temp.quest.1, temp.quest.costs)
  quest1$sub=quest1$gear=quest1$area=quest1$target=NA
  quest1$value=quest1$broad
  quest1=quest1[,c('gear','area','target', 'id_q','sub','value',"range.lwr","range.upr","unit_range")]
  quest1=rbind(quest1, temp.quest.mult[,c('gear','area','target', 'id_q','sub','value',"range.lwr","range.upr","unit_range")])
  quest1=rbind(quest1, stressquest[,c('gear','area','target','id_q','sub','value',"range.lwr","range.upr","unit_range")])
  quest1$id_q=as.numeric(quest1$id_q)
  quest1=quest1%>%arrange(id_q,sub)
  quest1$id_I=substr(target.sheets[i],3,3)
  
  if( i==7){
    quest1[quest1$id_q==16,]$target=c('not_specified','salmon')
  }
  store.results=rbind(store.results, quest1)
  
}
names(store.results)[5]='id_sub'
write.csv(store.results, file.path(scriptDir, '../data/coding_report.csv'), row.names = F )

coding=store.results

## addressing things
### removing those that does not apply
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
resp=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "responses")
resp$id_I=as.character(resp$id_I)

names(evts)[2]='desc'
f.style=coding[coding$id_q==0,c('id_I','value')]
names(f.style)[2]='style'
quest=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                         sheet = "questions")
evt.resp=coding[!is.na(coding$id_sub) & coding$id_q!=21  & !is.na(coding$value),]
evt.resp$type=ifelse(evt.resp$value=='-999' , 'NO' ,'YES')
evts.style=evt.resp%>%
  dplyr::group_by(id_I, id_sub, type)%>%
  tally()%>%
  dplyr::group_by(id_I, id_sub)%>%
  dplyr::mutate(prop=n/sum(n))%>%
  slice_max(prop)%>%
  dplyr::filter(type=='NO') 

### treating the 999s
interpret=store.results[store.results$value=='999' & !is.na(store.results$value),]
interpret=interpret[-which(interpret$id_q%in% c(16, 18,19)),] # exclude 18 and 19
interpret.llm=interpret%>%
  left_join(f.style)%>%
  left_join(evts, by='id_sub')%>%
  left_join(evts.style, by=c('id_sub','id_I'))%>%
  left_join(quest)%>%
  dplyr::mutate(id_sub=ifelse(is.na(id_sub), 'NA', id_sub))%>%
  left_join(resp)

#%>%
 # dplyr::select(id_I, id_q, id_sub,description, LIKERT)




