remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
source('code/CPT_BEWARE_v3.R')
remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)

library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_r1.R')

# data from interviews
questionnaire=read_excel("data/values.xlsx", 
                         sheet = "questions")
int.dat=read_csv("data/read_only/coding_report_unc.csv")
cost.dat=read_csv("data/read_only/cost_df_v2.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

# Human community - Variables ####
net=read.net('data/networks/BEWARE_v3_learn_pt1.net', debug = T)
net.hc=read.net('data/networks/BN_lower_end_2.net', debug = T)
graphviz.plot(net, layout = "dot", fontsize = 50)
graphviz.chart(net)

styles=net[['fishing_style']]$prob
styles=data.frame(styles)
names(styles)[1]='f.style'
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('coast', styles$f.style),]$f.style.2='coastal'
styles[grep('trawl', styles$f.style),]$f.style.2='pelagic_trawler'
styles[grep('archi', styles$f.style),]$f.style.2='subsistence'
styles[grep('guid', styles$f.style),]$f.style.2='fishing guide'
styles$season=c('all','summer','summer','winter','all')

# check missing data
x.nodes=nodes(net.hc)
x.nodes=x.nodes[x.nodes %in% int.dat$short_description]

hc.dat=int.dat[int.dat$short_description %in% c('fishing_style', x.nodes),]
## temporary solution: this needs to be fixed
hc.dat[hc.dat$uncertainty==-99.8 & !is.na(hc.dat$uncertainty),]$uncertainty=3/5 # this is the archipelago fisherman that does not know what to say
#hc.dat[hc.dat$value==999 & !is.na(hc.dat$uncertainty),]$value=2 # recreational fisherman question about benefit to community. Need better interpretation

hc.light=hc.dat[,c('fishing_style', 'short_description','value', 'uncertainty')]
#hc.light$text
hc.light=hc.light%>%
  dplyr::group_by(fishing_style, short_description)%>%
  dplyr::summarise(value=mean(value),uncertainty=mean(uncertainty))


# root nodes
# here is very important that the node definition in GeNie makes sense. Especially the order.- This needs to be revised
x.nodes=nodes(net.hc)
x.nodes=x.nodes[x.nodes %in% hc.dat$short_description]
x.nodes=x.nodes[x.nodes %in% nodes(net)]


for(i in 1:length(x.nodes)){
    
  i.node=x.nodes[i]
  array.var=net[[i.node]]$prob
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,i.node])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]
  i.dat=hc.light[hc.light$short_description==i.node,]
  
  for(z in 1:length(styles$f.style)){
    answ.range=data.frame(min=1, max=5)
    dat=i.dat[i.dat$fishing_style==styles[z,]$f.style.2,]
    dat$uncertainty=ifelse(dat$uncertainty==0,0.1,dat$uncertainty)
    i.cpt=answer.to.cpt(x.ans=dat$value, 
                  unc=dat$uncertainty,
                  dim.name = i.node,
                  x.range=c(answ.range$min, answ.range$max),
                  dim.labs = x.lev)
    
    df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=i.cpt
  }
  
  net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=home))

  
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
    #if(x.var=='innovative_capacity'){
    #  link.w=c(0.8,1,1)
    #}
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
  ggplot(aes(x=credit, y=prob, fill=economic_buffers))+
  geom_col()+
  facet_wrap(~job_mobility)


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
  pivot_longer(cols=c('High','Medium', 'Low'), names_to = 'health')%>%
  dplyr::mutate(health=factor(health, levels=c('Low','Medium','High')))%>%
  ggplot(aes(x=health, y=personal_safety, fill=value))+
  facet_wrap(~stress)+
  geom_tile()+
  scale_fill_viridis_c()

## objectives type ####
i.node='objectives_type'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

df.var[df.var$fishing_style=='trawler',]$Freq=answer.to.cpt(x.ans=1, unc=.0001, dim.name = i.node,
                                                            x.range=c(1, 5),
                                                            dim.labs = c('distal','progress'))

df.var[df.var$fishing_style%in% c('recreational'),]$Freq=answer.to.cpt(x.ans=4, unc=.0001, dim.name = i.node,
                                                                       x.range=c(1, 5),
                                                                       dim.labs = c('distal','progress'))

df.var[df.var$fishing_style %in% c('guide'),]$Freq=answer.to.cpt(x.ans=5, unc=.0001, dim.name = i.node,
                                                                 x.range=c(1, 5),
                                                                 dim.labs = c('distal','progress'))

df.var[df.var$fishing_style%in% c( 'archipelago'),]$Freq=answer.to.cpt(x.ans=2, unc=.0001, dim.name = i.node,
                                                                       x.range=c(1, 5),
                                                                       dim.labs = c('distal','progress'))

df.var[df.var$fishing_style %in% c('coastal'),]$Freq=answer.to.cpt(x.ans=3, unc=.0001, dim.name = i.node,
                                                                   x.range=c(1, 5),
                                                                   dim.labs = c('distal','progress'))


df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=objectives_type))+
  theme(legend.position = 'bottom')

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


# non monetary value ####
x.var='non_monetary_value'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[1:3]
xnam=dimnames(array.var)[1:3]
df.var=as.data.frame(array.var)

x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_out', 'objectives_type'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,1,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 

x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('High','Medium', 'Low'), names_to = 'nonmonval')%>%
  dplyr::mutate(nonmonval=factor(nonmonval, levels=c('Low','Medium','High')))%>%
  arrange(satisfaction, health,(nonmonval))
df.var[df.var$go_out%in% c('yes'),]$Freq=x.cpt$value


df.var[df.var$go_out%in% c('no', 'not_relevant') & df.var$objectives_type=='progress_rate',]$Freq=c(1,0,0)
df.var[df.var$go_out%in% c('no', 'not_relevant') & df.var$objectives_type=='distal_outcomes',]$Freq=c(0,1,0)

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=dimnames(array.var))

p1=df.var%>%
  dplyr::filter(go_out=='no')%>%
  ggplot()+
  geom_col(aes(x=health, y=Freq, fill=non_monetary_value))+
  facet_grid(cols=vars(satisfaction), rows=vars(objectives_type))+
  theme(legend.position='bottom')

p2=df.var%>%
  dplyr::filter(go_out=='yes')%>%
  ggplot()+
  geom_col(aes(x=health, y=Freq, fill=non_monetary_value))+
  facet_grid(cols=vars(satisfaction), rows=vars(objectives_type))+
  theme(legend.position='bottom')

ggpubr::ggarrange(p1,p2, common.legend = T, labels=c('a: no fishing', 'b: fishing'))


# realised catches ####
x.var='Catches'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[c(1,3,4)]
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
df.var[df.var$go_out%in% c('no', 'not_relevant'),]$Freq=c(1,0,0,0)

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
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam[c(1,3,4)])

x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('no','poor', 'average','good'), names_to = 'catches')%>%
  dplyr::mutate(catches=factor(catches, levels=c('no','poor', 'average','good')))%>%
  arrange(catchability , stock_status ,(catches))
df.var[df.var$go_out%in% c('yes'),]$Freq=x.cpt$value

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)

## cost ####
style.dataset=read_excel("data/lists_fishing_styles.xlsx", 
                         sheet = "fishing_style")

x.var='cost'
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
  df.var[df.var$fishing_style==i.fisher & df.var$additional_mitigation=='travel_further' & df.var$damage=='no',]$Freq=dat[dat$damage=='no' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & df.var$additional_mitigation=='travel_further' & df.var$damage=='minor',]$Freq= dat[dat$damage=='minor' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & df.var$additional_mitigation=='travel_further' & df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & df.var$additional_mitigation!='travel_further' & df.var$damage=='minor',]$Freq=dat[dat$damage=='minor' & dat$additional=='no',]$prob
  df.var[df.var$fishing_style==i.fisher & df.var$additional_mitigation!='travel_further' & df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='no',]$prob
}

#df.var[df.var$go_out=='no',]$Freq=c(1,0,0,0)

df.var%>%
  ggplot(aes(x=fishing_style, y=Freq, fill=cost))+
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
df.var[df.var$monetary_loss== df.var$cost,]$Freq=1
df.var[df.var$monetary_loss!= df.var$cost,]$Freq=0

# now adjust the cases when there is no fishing
df.var[df.var$go_out=='no' & df.var$fishing_style%in% c('recreational','archipelago'),]$Freq=c(1,0,0,0)
df.var[df.var$go_out=='no' & df.var$objectives_type== 'distal_outcomes',]$Freq=c(1,0,0,0)

i.gear=levels(df.var$fishing_style)
cost.dat=cost.dat.bck[grep('miss', cost.dat.bck$parent),]
parent.vec=strsplit(cost.dat$parent, '.', fixed = TRUE)
parent.vec=plyr::ldply(parent.vec)
cost.dat$damage=parent.vec[,1]
cost.dat$travel=parent.vec[,2]
base.prob=data.frame(state=c('negligible','low','medium','high'))

for(i in 1:length(i.gear)){
  
  i.fisher=i.gear[i]
  if(i.fisher %in% c('recreational','archipelago')){next}
  dat=cost.dat[cost.dat$fishing_style%in%i.fisher,]
  dat=dat[dat$travel=='no' & dat$damage=='no',]
  dat$state=factor(dat$state, levels=c('negligible', 'low', 'medium', 'high'))
  dat=dat[order(dat$state),]
  dat=left_join(base.prob, dat)%>%
    replace(is.na(.),0)
  
  df.var[df.var$fishing_style==i.fisher & 
           df.var$go_out=='no' & 
           df.var$objectives_type=='progress_rate',]$Freq=dat$prob
  
}



net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=dimnames(array.var))

df.var%>%
  dplyr::filter(fishing_style=='coastal')%>%
  ggplot()+
  geom_col(aes(x=cost, y=Freq, fill=monetary_loss))+
  facet_grid(rows=vars(objectives_type), cols=vars(go_out))+
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
  pivot_longer(cols=c('Low','Medium', 'High'), names_to = 'satisfaction')%>%
  dplyr::mutate(satisfaction=factor(satisfaction, levels=c('Low','Medium', 'High')))%>%
  arrange(catch_condition , Catches ,(satisfaction))
df.var$Freq=x.cpt$value
df.var[df.var$Catches=='no',]$Freq=c(1,0,0)

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
  x.val=ifelse(x.val=='Low',1,ifelse(x.val=='Medium',0.5,0))
  
  x.imp=df.var[,grep('impor', names(df.var))]  
  x.imp=ifelse(x.imp=='Very_important',1,ifelse(x.imp=='Somehow_important',0.5,0))
  
  x.risk=x.val*x.imp
  x.risk=cut(x.risk, c(-Inf,0.1,0.6,Inf), labels=c('Low','Medium','High'))
  
  df.var$risk2=x.risk
  df.var$Freq=ifelse(as.character(df.var$societal_risk) == as.character(df.var$risk2),1,0)
  
  # plug back in CPT
  
  if(i==1){
    df.var[df.var$societal_risk=='not_relevant',]$Freq=0
    df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
    net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
  }
  if(i==2){
    df.var[df.var$individual_risk=='not_relevant',]$Freq=0
    df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
    df.var[df.var$strategy!='not_relevant' & df.var$individual_risk!='not_relevant',]$Freq=z.cpt$value
    net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
    
  }
  
}

df.var%>%
  ggplot(aes(x=societal_importance, y=Freq, fill=societal_risk))+
  geom_col()+
  facet_grid(cols=vars(non_monetary_value), rows=vars(strategy)) 



# economic risk ####
x.var='economic_risk'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'strategy'))]
direction=c('pos', 'neg','neg')
x.lev=levels(df.var[,x.var])[c(1,2,3)]
l.w=c(1,3,1)
xdim2=dim(array.var)[c(1,2,3)]
xnam2=dimnames(array.var)[c(1,2,3)]
xnam2[[1]]=c( "Low","Medium","High")
supp.data=data.frame(node=c(target.dims,x.var), 
                     #link.w=c(rep(1, length(xdim))),
                     link.w=l.w,
                     states=c(xdim[2:length(xdim2)], 3),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim2)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.0001, algorithm = 'max', xnam=xnam2)
x.cpt[[3]]%>%
  ggplot(aes(x=economic_buffers, y=prob, fill=economic_risk))+
  geom_col()+
  facet_grid(cols=vars(monetary_loss))  

z.cpt=x.cpt[[1]]%>%
    pivot_longer(cols=c('Low','Medium','High'), names_to = 'economic_risk')%>%
    arrange(desc(monetary_loss), (economic_buffers))
df.var[df.var$economic_risk=='not_relevant',]$Freq=0
df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)

df.var[df.var$strategy=='cope' & df.var$economic_risk!='not_relevant',]$Freq=z.cpt$value
df.var[df.var$strategy=='adapt' & df.var$economic_risk!='not_relevant',]$Freq=z.cpt$value
df.var[df.var$strategy=='react' & df.var$economic_risk!='not_relevant',]$Freq=z.cpt$value

net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

df.var%>%
    ggplot(aes(x=economic_buffers, y=Freq, fill=economic_risk))+
    geom_col()+
    facet_grid(cols=vars(monetary_loss), rows=vars(strategy)) 


bnlearn::write.net( 'data/networks/BEWARE_v3_learn_pt2.net', net)
save(net, file='data/networks/BEWARE_v3_learn_pt2.rdata')






