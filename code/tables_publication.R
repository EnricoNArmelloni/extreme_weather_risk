remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_r1.R')

## economic values mat & method
aer.se=read_csv("data/fisheries_statistics/AER_SWE.csv")
aer.se=aer.2[aer.2$country_code=='SWE',]

aer.ssf=aer.se[aer.se$vessel_length %in% c('VL0010', 'VL0812') 
        & aer.se$fishing_tech %in% c('FPO','DFN'),]
ch=aer.ssf[aer.ssf$variable_name=="Number of vessels",]
ssf.vess=ch%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(nvess=sum(value))
mean(tail(ssf.vess$nvess,5))

aer.ssf=aer.ssf[aer.ssf$variable_group=='Income',]

ssf.mln=aer.ssf%>%
  dplyr::filter(!is.na(value))%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(mln.eur=sum(value)/1000)

mean(tail(ssf.mln$mln.eur,5))/300


unique(aer.ssf$variable_name)
unique(aer.ssf$variable_group)


aer.se[ aer.se$fishing_tech %in% c('TM'),]

aer.trw=aer.se[aer.se$vessel_length %in% c('VL2440'),]
ch=aer.trw[aer.trw$variable_name=="Number of vessels",]


aer.trw=aer.trw[aer.trw$variable_group=='Income',]
trw.mln=aer.trw%>%
  dplyr::filter(!is.na(value))%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(mln.eur=sum(value)/1000000)
mean(tail(trw.mln$mln.eur,5))


unique(aer.trw$variable_name)

## Table 4
node.text=read_excel(file.path(scriptDir, '..','data/nodes_text.xlsx'))
net=read.net(file.path(scriptDir, '..','data/networks/BEWARE_learn_pt2.net'), debug = T)
x.nodes=nodes(net)


table.format=NULL
i=5
for(i in 1:length(x.nodes)){
  
  extra.info=node.text[node.text$node==x.nodes[i],]
  x.var=x.nodes[i]
  if(x.var %in% c('Node5', 'Node1')){next}
  array.var=net[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  if(ncol(df.var)==2){
    x.lev=levels(df.var$Var1)
    x.parent=NA
  }else{
    x.lev=levels(df.var[,x.var])
    x.parent=names(df.var)[-which(names(df.var)%in% c(x.var, 'Freq'))]
  }
  x.lev=paste(x.lev, collapse=', ')
  x.parent=paste(x.parent, collapse=', ')
  
  result=data.frame(name=x.nodes[i], group= extra.info$group, specification=extra.info$short_text, levels= x.lev, parents= x.parent, cycle=extra.info$cycle)
  table.format=rbind(table.format, result)
}

write.csv(table.format, file.path(scriptDir, '..','results/tables/tab4_cpt_description.csv'), row.names = F)



