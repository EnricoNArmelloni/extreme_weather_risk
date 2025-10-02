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


###

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

write.csv(cod.unc, file.path(scriptDir, '../data/coding_report_unc.csv'), row.names = F )


