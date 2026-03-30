# Version 2. February 2026
# Description
# this code extracts numbers that are included into the excel file and prepares a formatted input file for cpt compilation. The excel file already contains revised responses

library(readxl)
library(tidyverse)
rm(list = ls())
summary.path="C:/github/extreme_weather_risk/data/questionnaires_coded.xlsx"
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
target.sheets=readxl::excel_sheets(summary.path)
store.results=NULL


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
write.csv(store.results, file.path(scriptDir, '../data/read_only/coding_report.csv'), row.names = F )


###
remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)

scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
resp=readxl::read_excel(file.path(scriptDir, '..', 'data', 'dialogues_raw.xlsx'), 
                        sheet = "responses")

quest=readxl::read_excel(file.path(scriptDir, '..', 'data', 'dialogues_raw.xlsx'), 
                         sheet = "questions")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'dialogues_raw.xlsx'), 
                        sheet = "events")
coding= read_csv("C:/github/extreme_weather_risk/data/read_only/coding_report.csv")


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
  left_join(quest[,c('id_q','short_description')])


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
    i.unc=uncertainty.vals[i,]$value
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

## bringing values on the 0-1 scale
questionnaire=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "questions")
questionnaire=questionnaire[!is.na(questionnaire$max),c('id_q', 'min', 'max')]

cod.unc=cod.unc%>%left_join(questionnaire, by='id_q')
cod.unc$value.norm=ifelse(cod.unc$value>=0 & cod.unc$value<=5, round((cod.unc$value-cod.unc$min)/(cod.unc$max -cod.unc$min), digits=2),NA)
cod.unc$unc.norm=ifelse(cod.unc$value>=0 & cod.unc$value<=5, round((cod.unc$uncertainty-1)/(5 -1), digits=2),NA)
  
## fix missing uncertainty: i borrowed fishing guide unc 2_7 from 9_13 and 15_19; borrowed archiplego uncertainty on personal values from avg of all coastal and recreational

## lowest uncertainty means that the answer is 100% pertaining to the declared rank. On the opposite, highest uncertainty let the rank to go all over the place. Then it should be a gradient of betweens
# when answer is 1 to 5, min = 0.25 and max = 1.5 looks reasonable7

source('code/supporting_r1.R')
unique(cod.unc$value.norm)
x.answs=seq(0,1,0.1)
x.unc=0.2*unique(cod.unc$unc.norm)[!is.na(unique(cod.unc$unc.norm))]
i=j=3
x.store=NULL
for(i in 1:length(x.answs)){
  for(j in 1:length(x.unc)){
    x.res=answer.to.cpt(x.ans=x.answs[i], 
              unc=x.unc[j],
              dim.name = 'test',
              x.range=c(0, 1))
    x.res=as.data.frame(x.res)
    x.res$lev=rownames(x.res)
    x.res$val=x.answs[i]
    x.res$unc=x.unc[j]
    x.store=rbind(x.store, x.res)
  }
}

ggplot(data=x.store, aes(x=val, y=x.res, fill=lev))+
  geom_col()+
  facet_wrap(~unc)

write.csv(cod.unc, file.path(scriptDir, '../data/read_only/coding_report_unc.csv'), row.names = F )






