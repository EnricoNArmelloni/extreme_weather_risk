remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)

fish.style=read_excel("data/lists_fishing_styles.xlsx", 
           sheet = "fishing_style")
events=read_excel("data/lists_fishing_styles.xlsx", 
           sheet = "events")

summary.path="data/questionnaires_coded.xlsx"

sheet.list=readxl::excel_sheets(summary.path)
int.summary=NULL
i=2

events.results=NULL
for(i in 1:length(sheet.list)){
  #
  i.dat=readxl:: read_excel(summary.path,sheet.list[i])
  i.type=i.dat$broad[1]
  i.id=as.numeric(substr(sheet.list[i], (nchar(sheet.list[i])), nchar(sheet.list[i])))
  i.dat$broad=as.numeric(i.dat$broad)
  i.dat$domain=str_remove(i.dat$category, 'hc_')
  i.dat$id_I=i.id
  i.dat$style=i.type
  #i.style=fish.style[fish.style$id==i.id,]
  
  # uncertainty
  i.dat$uncertainty=NA
  uncertainty.vals=i.dat[grep(i.dat$short_description, pattern='unc'),]
  uncertainty.id=str_split(uncertainty.vals$short_description, '_')
  for(j in 1:nrow(uncertainty.vals)){
    j.id=as.numeric(uncertainty.id[[j]][-1])
    i.unc=uncertainty.vals[j,]$broad/10
    if(length(j.id) ==2){
      i.dat[i.dat$id_q %in% j.id[1]:j.id[2],]$uncertainty=i.unc
    }
    if(length(j.id)==1){
      i.dat[grep(i.dat$id_q ,pattern= j.id[1]),]$uncertainty=i.unc
    }
  }
  i.dat=i.dat[-which(i.dat$id_q %in% uncertainty.vals$id_q),]
  
  # event related
  i.events=events[events$id==i.id,4:10]
  i.events=i.events[apply(i.events,2,function(x)x==1)]
  df.events=i.dat[,c( 'id_I', 'style', 'gear', 'area','target','id_q','uncertainty',names(i.events))]
  exclude.rows=apply(df.events[,8:ncol(df.events)],1,function(x)sum(ifelse(unique(is.na(x))==FALSE,1,0)))
  df.events=df.events[exclude.rows==1,]
  
  df.events[,names(i.events)]=apply(df.events[,names(i.events)],2,
                                    function(x)as.numeric(str_trim(as.character(x),'both'))) # make sure to remove whitespaces and make it numeric
  
  df.events=df.events%>%
    pivot_longer(cols = names(i.events), names_to = 'event', values_to = 'score')
  events.results=rbind(events.results, df.events)
    
  # prices
  df.costs=i.dat[!is.na(i.dat$unit_range),c( 'id_I', 'style', 'gear', 'area','target','id_q','short_description','uncertainty','range.lwr','range.upr','unit_range')]
  
  # other
  df.personal=i.dat[!is.na(i.dat$broad),c( 'id_I', 'style', 'gear', 'area','target','id_q','short_description','uncertainty','broad')]
  
}

# check for relevant missing data
question.type=data.frame(id_q=c("2","3","4","5","6","7","9","10","13","15", '35'),
           hum.dom=c('human', 'metier', 'fish','fish','human','fish','human','human','desc','human', 'social'))

events.results.qt=left_join(events.results[!is.na(events.results$score),], question.type, by='id_q')
events.missing=events.results.qt[events.results.qt$score==-998,]

missing.human=events.missing[events.missing$hum.dom=='human',]
missing.metier=events.missing[events.missing$hum.dom=='metier',]
missing.fish=events.missing[events.missing$hum.dom=='fish',]





read_csv("data/coding_report.csv")





