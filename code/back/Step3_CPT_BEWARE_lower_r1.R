remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
source('code/Step2_CPT_BEWARE_r1.R')
remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)

library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_r1.R')

# Load data ####
# data from interviews
questionnaire=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "questions")

int.dat=read_csv("data/read_only/coding_report_unc.csv")

evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'dialogues_raw.xlsx'), 
                        sheet = "events")

style.dataset=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "fishers")
names(style.dataset)[3]='fishing_style'

bn.desc=read_excel("data/nodes_text.xlsx")

# data from literature
cost.dat=read_csv("data/read_only/cost_df_v2.csv")

# BN
net=read.net('data/networks/BEWARE_r1_learn_pt1.net', debug = T)

# format data ####
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

styles=net[['fishing_style']]$prob
styles=data.frame(styles)
names(styles)[1]='f.style'

# root nodes ####
# select nodes
root.nodes=bn.desc[bn.desc$group %in% c('economic', 'societal','individual'),]
root.nodes=root.nodes[root.nodes$cycle==2,]
root.nodes=root.nodes[-(grep('import', root.nodes$node)),]
x.nodes=nodes(net)
x.nodes=x.nodes[x.nodes %in% root.nodes$node]
x.nodes=x.nodes[x.nodes %in% nodes(net)]


hc.dat=int.dat[int.dat$short_description %in% x.nodes,]

## temporary solution: this needs to be fixed
hc.dat[hc.dat$uncertainty==-99.8 & !is.na(hc.dat$uncertainty),]$uncertainty=3/5 # this is the archipelago fisherman that does not know what to say
#hc.dat[hc.dat$value==999 & !is.na(hc.dat$uncertainty),]$value=2 # recreational fisherman question about benefit to community. Need better interpretation

hc.dat=left_join(hc.dat,style.dataset[,c('id_I', "fishing_style")])

i=4
for(i in 1:length(x.nodes)){
    
  i.node=x.nodes[i]
  array.var=net[[i.node]]$prob
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,i.node])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]
  i.dat=hc.dat[hc.dat$short_description==i.node,]
  
  for(z in 1:length(styles$f.style)){
    dat=i.dat[i.dat$fishing_style==styles[z,]$f.style,]
    if(nrow(dat)>1){
     dat=uncertainty.function(dat) 
    }
    if(abs(dat$value)>5){
      df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=c(1,0,0)
      next
    }
    i.cpt=answer.to.cpt(x.ans=dat$value.norm, 
                  unc=(dat$unc.norm+0.01)*0.5,
                  dim.name = i.node,
                  dim.labs = x.lev)
    
    df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=i.cpt
  }
  
  net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=sense_of_home))

## substitution capacity ####
i.node='substitution_capacity'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
#x.lev=rev(x.lev)
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

i.dat=int.dat[int.dat$short_description==i.node,]%>%
  left_join(., style.dataset[,c('id_I', "fishing_style")])

for(z in 1:length(styles$f.style)){
  
  j.answ=i.dat[i.dat$fishing_style==styles[z,]$f.style,]
  
  if(nrow(j.answ)>1){
    j.answ=uncertainty.function(j.answ)
  }
  
  i.cpt=answer.to.cpt(x.ans=j.answ$value.norm, 
                        unc=(j.answ$unc.norm+0.001)*0.25,
                        dim.name = i.node,
                        dim.labs = x.lev)
  
  df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=(i.cpt)
}

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=substitution_capacity))+
  theme(legend.position = 'bottom')

# Importance ####
## Comments/to do: also here the order of states matters a lot: negative goes first!!! this has to be check

imp.vars=c('societal_importance', 'individual_importance' ,'economic_buffers' )
i=3
for(i in 1:length(imp.vars)){
    
    x.var=imp.vars[i]
    array.var=net[[x.var]][['prob']]
    xdim=dim(array.var)
    xnam=dimnames(array.var)
    df.var=as.data.frame(array.var)
    x.lev=levels(df.var[,x.var])
    target.dims=names(df.var)
    target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
    link.w=c(rep(1, length(xdim)))
    if(x.var=='economic_buffers'){
      link.w=c(0.5,1,1)
    }
    supp.data=data.frame(node=c(target.dims,x.var), 
                         link.w=link.w,
                         states=c(xdim[2:length(xdim)], xdim[1]),
                         type=c(rep('parent', length(target.dims)) , 'child'),
                         direction=c(rep('pos', length(xdim))),
                         id=1:length(xdim))
    x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'max', xnam=xnam)
    net[[x.var]]=x.cpt$bn.cpt
}

x.cpt$cpt.long%>%
  ggplot(aes(x=job_mobility, y=prob, fill=economic_buffers))+
  geom_col()+
  facet_wrap(~credit)


# health ####
x.var='health'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
direction=c('neg', 'neg','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,3,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
net[[x.var]]=x.cpt$bn.cpt

x.cpt[[1]]%>%
  pivot_longer(cols=c('high','medium', 'low'), names_to = 'health')%>%
  dplyr::mutate(health=factor(health, levels=c('low','medium','high')))%>%
  ggplot(aes(x=health, y=personal_safety, fill=value))+
  facet_wrap(~stress)+
  geom_tile()+
  scale_fill_viridis_c()



# non monetary value ####
x.var='non_monetary_value'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[1:3]
xnam=dimnames(array.var)[1:3]
df.var=as.data.frame(array.var)

x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_fishing', 'substitution_capacity'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,1,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 

x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('high','medium', 'low'), names_to = 'nonmonval')%>%
  dplyr::mutate(nonmonval=factor(nonmonval, levels=c('low','medium','high')))%>%
  arrange(satisfaction, health,(nonmonval))
df.var[df.var$go_fishing%in% c('yes'),]$Freq=x.cpt$value

df.var[df.var$go_fishing%in% c('no', 'not_relevant') & df.var$substitution_capacity=='no',]$Freq=c(1,0,0)
df.var[df.var$go_fishing%in% c('no', 'not_relevant') & df.var$substitution_capacity=='yes',]$Freq=c(0,1,0)

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=dimnames(array.var))

p1=df.var%>%
  dplyr::filter(go_fishing=='no')%>%
  ggplot()+
  geom_col(aes(x=health, y=Freq, fill=non_monetary_value))+
  facet_grid(cols=vars(satisfaction), rows=vars(substitution_capacity))+
  theme(legend.position='bottom')

p2=df.var%>%
  dplyr::filter(go_fishing=='yes')%>%
  ggplot()+
  geom_col(aes(x=health, y=Freq, fill=non_monetary_value))+
  facet_grid(cols=vars(satisfaction), rows=vars(substitution_capacity))+
  theme(legend.position='bottom')
ggpubr::ggarrange(p1,p2, common.legend = T, labels=c('a: no fishing', 'b: fishing'))

# realised catches ####
x.var='catches'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[c(1,3,4)]
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
df.var[df.var$go_fishing%in% c('no', 'not_relevant'),]$Freq=c(1,0,0,0)

x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_fishing'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,1,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam[c(1,3,4)])

x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('no','poor', 'average','good'), names_to = 'catches')%>%
  dplyr::mutate(catches=factor(catches, levels=c('no','poor', 'average','good')))%>%
  arrange(catchability , stock_status ,(catches))
df.var[df.var$go_fishing%in% c('yes'),]$Freq=x.cpt$value

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)

## cost ####
x.var='costs'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[-c(4)]
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
i.gear=levels(df.var$fishing_style)

cost.dat.bck=cost.dat
cost.dat=cost.dat[-grep('miss', cost.dat$parent),]
parent.vec=strsplit(cost.dat$parent, '.', fixed = TRUE)
parent.vec=plyr::ldply(parent.vec)
cost.dat$damage=parent.vec[,1]
cost.dat$additional=parent.vec[,2]

## common
df.var[df.var$damage=='destroy',]$Freq=c(0,0,0,1)
df.var[df.var$damage=='no' & df.var$additional_mitigation!='travel_further',]$Freq=c(1,0,0,0)
base.grid=expand.grid(damage=c('no', 'minor','major'), additional=c('no', 'travel'), state=c('negligible','low', 'medium', 'high'))

## gear
for(i in 1:length(i.gear)){
  i.fisher=i.gear[i]
  dat=cost.dat[cost.dat$fishing_style%in%i.fisher,]
  dat=dat[,c('damage','additional','state', 'prob')]
  dat=full_join(base.grid, dat, by=c('damage', 'additional', 'state'))%>%
    replace(is.na(.),0)
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' & 
           df.var$damage=='no',]$Freq=dat[dat$damage=='no' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' &
           df.var$damage=='minor',]$Freq= dat[dat$damage=='minor' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' & 
           df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation!='travel_further' & 
           df.var$damage=='minor',]$Freq=dat[dat$damage=='minor' & dat$additional=='no',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation!='travel_further' & 
           df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='no',]$prob
}

df.var%>%
  ggplot(aes(x=fishing_style, y=Freq, fill=costs))+
  facet_grid(cols=vars(additional_mitigation), rows=vars(damage))+
  geom_col()
net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)

# monetary loss ####
x.var='monetary_loss'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

# set loss equal to cost
df.var[df.var$monetary_loss== df.var$costs,]$Freq=1
df.var[df.var$monetary_loss!= df.var$costs,]$Freq=0

# now adjust the cases when there is no fishing
df.var[df.var$go_fishing=='no' & df.var$fishing_style%in% c('recreational','household'),]$Freq=c(1,0,0,0)
df.var[df.var$go_fishing=='no' & df.var$substitution_capacity== 'yes',]$Freq=c(1,0,0,0)

i.gear=levels(df.var$fishing_style)
cost.dat=cost.dat.bck[grep('miss', cost.dat.bck$parent),]
parent.vec=strsplit(cost.dat$parent, '.', fixed = TRUE)
parent.vec=plyr::ldply(parent.vec)
cost.dat$damage=parent.vec[,1]
cost.dat$travel=parent.vec[,2]
base.prob=data.frame(state=c('negligible','low','medium','high'))

for(i in 1:length(i.gear)){
  
  i.fisher=i.gear[i]
  if(i.fisher %in% c('recreational','household')){next}
  dat=cost.dat[cost.dat$fishing_style%in%i.fisher,]
  dat=dat[dat$travel=='no' & dat$damage=='no',]
  dat$state=factor(dat$state, levels=c('negligible', 'low', 'medium', 'high'))
  dat=dat[order(dat$state),]
  dat=left_join(base.prob, dat)%>%
    replace(is.na(.),0)
  
  
  
  df.var[df.var$fishing_style==i.fisher & 
           df.var$go_fishing=='no' & 
           df.var$substitution_capacity=='no',]$Freq=dat$prob
  
}



net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=dimnames(array.var))

df.var%>%
  dplyr::filter(fishing_style=='small_scale')%>%
  ggplot()+
  geom_col(aes(x=costs, y=Freq, fill=monetary_loss))+
  facet_grid(rows=vars(substitution_capacity), cols=vars(go_fishing))+
  theme(legend.position='bottom')


# satisfaction #### 
x.var='satisfaction'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_out'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,1,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)

x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('low','medium', 'high'), names_to = 'satisfaction')%>%
  dplyr::mutate(satisfaction=factor(satisfaction, levels=c('low','medium', 'high')))%>%
  arrange(catch_condition , catches ,(satisfaction))
df.var$Freq=x.cpt$value
df.var[df.var$catches=='no',]$Freq=c(1,0,0)

net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


# risks ####
risk.vars=c('societal_risk', 'individual_risk')

for(i in 1:length(risk.vars)){
  
  x.var=risk.vars[i]
  array.var=net[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  
  # convert to numeric
  x.val=df.var[,grep('monet', names(df.var))]  
  x.val=ifelse(x.val=='low',1,ifelse(x.val=='medium',0.5,0))
  
  x.imp=df.var[,grep('impor', names(df.var))]  
  x.imp=ifelse(x.imp=='high',1,ifelse(x.imp=='medium',0.5,0))
  
  x.risk=x.val*x.imp
  x.risk=cut(x.risk, c(-Inf,0.1,0.6,Inf), labels=c('low','medium','high'))
  
  df.var$risk2=x.risk
  x.risk.v=ifelse(as.character(df.var[,grep('_ri', names(df.var))])==as.character(df.var[,grep('k2', names(df.var))]),1,0)
  df.var$Freq=x.risk.v
  
  # plug back in CPT
  df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
  net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

#df.var%>%
#  ggplot(aes(x=societal_importance, y=Freq, fill=societal_risk))+
#  geom_col()+
#  facet_grid(cols=vars(non_monetary_value), rows=vars(strategy)) 

# economic risk
risk.vars=c('economic_risk')
x.var=risk.vars
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.val=df.var[,grep('monet', names(df.var))]  
x.val=ifelse(x.val=='high',1,ifelse(x.val=='medium',0.66,ifelse(x.val=='low',0.33,0)))
x.imp=df.var[,grep('buff', names(df.var))]  
x.imp=ifelse(x.imp=='high',1,ifelse(x.imp=='medium',0.5,0))
x.risk=x.val*(1-(x.imp*0.7))
x.risk=cut(x.risk, c(-Inf,0.25,0.5,Inf), labels=c('low','medium','high'))
df.var$risk2=x.risk
x.risk.v=ifelse(as.character(df.var[,grep('_ri', names(df.var))])==as.character(df.var[,grep('k2', names(df.var))]),1,0)
df.var$Freq=x.risk.v
df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

df.var%>%
       ggplot(aes(x=economic_buffers, y=Freq, fill=economic_risk))+
       geom_col()+
       facet_grid(cols=vars(monetary_loss), rows=vars(strategy_to_change)) 

#

bnlearn::write.net( 'data/networks/BEWARE_r1_learn_pt2.net', net)
#save(net, file='data/networks/BEWARE_v3_learn_pt2.rdata')






