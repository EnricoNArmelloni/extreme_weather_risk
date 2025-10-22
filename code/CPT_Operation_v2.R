remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_functions.R')


## functions confirmed ####
answer.to.cpt_vH=function(x.ans, dim.name, div.factor=5, 
                          dim.labs = c('L','M','H'), unc=0.05){
  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=0,b=1,n=100, mean=x.ans/div.factor, sd=unc)
  min_bin=1/div.factor
  x.breaks=seq(min_bin,1,(1-min_bin)/length(dim.labs))
  x.breaks[1]=-Inf
  x.breaks[length(x.breaks)]=Inf
  base.dist=cut(base.dist, breaks=x.breaks, labels=dim.labs)
  prob.tab=array(table(base.dist)/100, dim = length(dim.labs), dimnames = dim.list)
  return(prob.tab)
}

rank.cpt=function(nodes.df, uncertainty, algorithm = 'equal', meaning='abs', xnam){
  
  # identify nodes
  x.parents=nodes.df[nodes.df$type=='parent',]
  x.child=nodes.df[nodes.df$type=='child',]
  
  # expand combination of parent state
  state.list=list()
  for(i in 1:nrow(x.parents)){
    i.parent=x.parents[i,]
    i.states=seq(0,1,1/(i.parent$states-1))
    state.list[[i]]=i.states
    names(state.list)[i]=i.parent$node
  }
  base.cpt=do.call(expand.grid, state.list)
  
  # apply algorithm
  cpt.default=NULL
  for(i in 1:nrow(base.cpt)){
    if(algorithm == 'equal'){
      target.state.cpt=p.avg(n=100,
                             x.w = x.parents, 
                             x.state = as.numeric(base.cpt[i,]),
                             x.sd=uncertainty)  
    }else if(algorithm == 'min'){
      target.state.cpt=p.wmin(n=100,
                              x.w = x.parents, 
                              x.state = as.numeric(base.cpt[i,]),
                              x.sd=uncertainty)  
    }else if(algorithm == 'max'){
      target.state.cpt=p.wmax(n=100,
                              x.w = x.parents, 
                              x.state = as.numeric(base.cpt[i,]),
                              x.sd=uncertainty)  
    }
    
    i.cpt=data.frame(base.cpt[i,],out=round(target.state.cpt, digits = 2), row.names = NULL)
    names(i.cpt)[ncol(i.cpt)]=x.child$node
    cpt.default=rbind(cpt.default, i.cpt)
  }
  
  # discretise
  disc.cpt=cpt.default
  for(i in 1:nrow(nodes.df)){
    x.labs=xnam[[nodes.df[i,]$node]]
    if(nodes.df[i,]$direction=='neg'){
      x.labs=rev(x.labs)
    }
    i.disc=cut(cpt.default[,i], include.lowest = T,
               breaks = seq(0,1,1/length(x.labs)), labels = factor(x.labs))
    disc.cpt[,i]=i.disc
  }
  
  # probabilities
  cpt.long=disc.cpt%>%
    dplyr::group_by_all()%>%
    tally()%>%
    dplyr::mutate(prob=n/100)%>%
    dplyr::select(-n)
  
  
  # BN cpt
  nodes.df=nodes.df%>%arrange(type, desc(id))
  var.desc=list()
  var.dim=NULL
  for(i in 1:nrow(nodes.df)){
    i.nm=nodes.df[i,]$node
    i.st=nodes.df[i,]$states
    i.lv=cpt.long[,which(colnames(cpt.long)==i.nm)]
    i.lv=levels(as.data.frame(i.lv)[,1])
    var.dim=c(var.dim, i.st)
    var.desc[[i]]=i.lv
    names(var.desc)[i]=i.nm
  }
  cpt.wide=cpt.long%>%
    pivot_wider(names_from = x.child$node,
                values_from = prob)%>%
    replace(is.na(.),0)
  cpt.long2=cpt.wide%>%
    pivot_longer(-x.parents$node)
  E.prob <- array(cpt.long2$value, 
                  dim = var.dim, 
                  dimnames = var.desc)
  
  return(list(report.cpt=cpt.wide, bn.cpt=E.prob, cpt.long=cpt.long))
}


# data from interviews
int.dat=read_csv("data/coding_report_unc.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

style.dataset=read_excel("data/lists_fishing_styles.xlsx", 
           sheet = "fishing_style")
# DAG
net=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/type1_v5_general_simple.net', debug = T)
graphviz.plot(net, layout = "dot", fontsize = 18)
graphviz.chart(net)

f.styles=unique(int.dat[int.dat$id_q==0,]$text)


stress.prob=cpdist(net, nodes = c('stress'), 
       evidence = (event == 'gal'))
table(stress.prob)

# check missing data
x.nodes=nodes(net)
x.nodes=x.nodes[x.nodes %in% int.dat$short_description]
int.dat[abs(int.dat$uncertainty)>10 & !is.na(int.dat$uncertainty),]$uncertainty=1/5
xx=1


# start
x.nodes=nodes(net)

## Set even event probability
array.var=net[['event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['event']]=array.var

## set fisher probability
array.var=net[['fishing_style']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['fishing_style']]=array.var

## fishing style
i.node='gear'
# gear in this moment is set manually. COnsider if there is the need to create two different traps. The mani issue here is to properly catch how fisher can switch between gears, especially between nets and traps for coastal and between rod and ice fishing for recreational

## fishing area
i.node='area' # same as fishing style


## infrastructure ####
i.node='infrastructure'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
z=3
for(z in 1:length(styles$f.style)){
  
  if(i.node=='infrastructure'){
    answ.range=0:2
  }else if(i.node=='damage'){
    answ.range=0:3
  }else{
    answ.range=1:5
  }
  adj.factor=ifelse(min(answ.range)==0,0.5,0)
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  x.answ=dat[grep(i.node, dat$short_description),]%>%
    arrange(short_description)
  
  if(nrow(x.answ)>1){
    x.answ=x.answ%>%
      dplyr::group_by(short_description)%>%
      dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
    x.answ$uncertainty=ifelse(is.na(x.answ$uncertainty),0,x.answ$uncertainty)
  }
  
  i.cpt=answer.to.cpt_vH(x.ans=x.answ$value+adj.factor, 
                         unc=x.answ$uncertainty,
                         dim.name = i.node,
                         div.factor = length(answ.range), # this is not always 3!
                         dim.labs = x.lev)
  df.var[df.var$fishing_style==z.style$f.style ,]$Freq=i.cpt  
}
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


## management ####
i.node='management'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
z=1
for(z in 1:length(styles$f.style)){
  
  if(i.node=='infrastructure'){
    answ.range=0:2
  }else if(i.node=='damage'){
    answ.range=0:3
  }else{
    answ.range=1:5
  }
  adj.factor=ifelse(min(answ.range)==0,0.5,0)
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  x.answ=dat[grep(i.node, dat$short_description),]%>%
    arrange(short_description)
  x.answ=x.answ[abs(x.answ$value)<=5,]
  
  if(nrow(x.answ)>1){
    x.answ=x.answ%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
    x.answ$uncertainty=ifelse(is.na(x.answ$uncertainty),0,x.answ$uncertainty)
  }
  
  i.cpt=answer.to.cpt_vH(x.ans=x.answ$value+adj.factor, 
                         unc=x.answ$uncertainty,
                         dim.name = i.node,
                         div.factor = length(answ.range), # this is not always 3!
                         dim.labs = x.lev)
  df.var[df.var$fishing_style==z.style$f.style ,]$Freq=i.cpt  
}
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


## flexibility ####

i.node='flexibility'
array.var=net[[i.node]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

# if management is not flexible and/or there is no infrastructure, then all is a no
df.var[df.var$management=='Rigid'|df.var$infrastructure=='no',]$Freq=c(1,0)

# this should be re4 considered. flexibility is somehow implicit in the definition of adaptive strategy. Sure it is interesting




## Potential consequences ####
p.consequences=c('personal_safety', 'damage' , 'catch_condition', 'catchability')

for(z in 1:length(p.consequences)){
  
  # settings for the node
  i.node=p.consequences[z]
  if(i.node=='personal_safety'){
    answ.range=0:2
  }else if(i.node=='damage'){
    answ.range=0:3
  }else{
    answ.range=1:5
  }
  
  adj.factor=ifelse(min(answ.range)==0,0.5,0)
  array.var=net[[i.node]]$prob
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,i.node])
  i.gear=levels(df.var$gear)
  
  for(i in 1:length(i.gear)){
    i.fisher=style.dataset[style.dataset$gear==i.gear[i],]
    i.fisher=unique(i.fisher$id)
    dat=int.dat[int.dat$id_I%in%i.fisher,]
    x.answ=dat[grep(i.node, dat$short_description),]
    x.answ=x.answ[is.na(x.answ$unit_range),]
    
    # revise the following and make sure to exclude any gear that is different
    x.answ$target=ifelse(is.na(x.answ$target), 'not_specified', x.answ$target)
    x.answ$area=ifelse(is.na(x.answ$area), 'not_specified', x.answ$area)
    x.answ=x.answ[x.answ$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]
    x.answ$gear=ifelse(is.na(x.answ$gear), 'not_specified', x.answ$gear)
    x.answ=x.answ[x.answ$gear %in% c('not_specified', i.gear[i]), ]
    x.answ=x.answ[!is.na(x.answ$value),]
    
    # prob for each event
    i.evts=unique(df.var$event)
    for(j in 1:length(i.evts)){
    j.event=i.evts[j]
    j.answ=x.answ[x.answ$event_code==j.event,]  
    j.answ=j.answ[abs(j.answ$value)<=5,]
    if(nrow(j.answ)>1){
     j.answ=j.answ%>%
       dplyr::group_by(gear,area,target)%>%
       dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
     j.answ$uncertainty=ifelse(is.na(j.answ$uncertainty),0,j.answ$uncertainty)
    }
    # single option
    if(nrow(j.answ)==1){
      i.cpt=answer.to.cpt_vH(x.ans=j.answ$value+adj.factor, 
                             unc=j.answ$uncertainty,
                             dim.name = i.node,
                             div.factor = length(answ.range), # this is not always 3!
                             dim.labs = x.lev)
      df.var[df.var$event==j.event & df.var$gear== i.gear[i],]$Freq=i.cpt  
    }
    # multiple options
    if(nrow(j.answ)>1){
      check.multi=data.frame(Freq=apply(j.answ[,1:3], 2, function(x)length(unique(x))))
      check.multi$type=rownames(check.multi)
      check.multi=check.multi[check.multi$Freq>1,]
    
      if(nrow(check.multi)==1){
        multi.options=j.answ[,check.multi$type]
        
        # TO DO make sure to have a generic baseline based on answers. 
        # FOr instance, in case only some of the species mentioned have a specific value. 
        # ALl the other should be assimilated to the not_specified
        
       for(k in 1:nrow(multi.options)){
          k.opt=multi.options[k,]
          k.feature=names(k.opt)
          if(k.opt %in% df.var[[k.feature]]==F){next}
          k.answ=j.answ[k,]
          i.cpt=answer.to.cpt_vH(x.ans=k.answ$value+adj.factor, 
                                 unc=k.answ$uncertainty,
                                 dim.name = i.node,
                                 div.factor = length(answ.range), # this is not always 3!
                                 dim.labs = x.lev)
          df.var[df.var$event==j.event & df.var$gear== i.gear[i] & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
        }
    }
   }
  }
  }
  net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}


# stress
i.node='stress'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
for(z in 1:length(styles$f.style)){
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  x.answ=dat[grep(i.node, dat$short_description),]%>%
    arrange(short_description)
 
  if(nrow(x.answ)>3){
    x.answ=x.answ%>%
      dplyr::group_by(short_description)%>%
      dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
    x.answ$uncertainty=ifelse(is.na(x.answ$uncertainty),0,x.answ$uncertainty)
  }
  
  supp.data=data.frame(node=c('catch_condition',  'damage', 'personal_safety','stress'), 
                       link.w=c(x.answ$value, 0),
                       states=c(3,4,3,2),
                       type=c('parent','parent','parent','child'),
                       direction=c('neg', 'pos','pos', 'pos'),
                       id=1:4)
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = unique(x.answ$uncertainty), algorithm = 'min', xnam=xnam)
  x.cpt[[1]]%>%
    pivot_longer(cols=c('yes','no'), names_to = 'stress')%>%
    dplyr::filter(stress=='yes')%>%
    ggplot(aes(x= personal_safety, y=damage, fill=value))+
    geom_tile()+
    facet_wrap(~catch_condition)+
    scale_fill_viridis_c()
  
  z.cpt=x.cpt[[1]]%>%
    pivot_longer(cols=c('no','yes'), names_to = 'stress')%>%
    arrange(desc(catch_condition))
  
  df.var[df.var$fishing_style==styles[z,]$f.style,]$Freq=z.cpt$value
  
  
}
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)




# go out
i.node='go_out'
array.var=net[[i.node]]$prob
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'

z=6

for(z in 1:length(styles$f.style)){
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  dat=dat[is.na(dat$unit_range),]
  dat$target=ifelse(is.na(dat$target), 'not_specified', dat$target)
  dat$area=ifelse(is.na(dat$area), 'not_specified', dat$area)
  dat=dat[dat$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]
  dat$gear=ifelse(is.na(dat$gear), 'not_specified', dat$gear)
  dat=dat[!is.na(dat$value),]
  dat=dat[abs(dat$value)<=5,]
  
  
  # go fishing
  x.answ=dat[grep(i.node, dat$short_description),]
  # adapt
  x.answ.2=dat[grep('avoid', dat$short_description),]
  
  i.evts=unique(df.var$event)
  j=7
  for(j in 1:length(i.evts)){
    j.event=i.evts[j]
    j.answ=x.answ[x.answ$event_code==j.event,]  
    j.answ=j.answ[abs(j.answ$value)<=5,]
    if(nrow(j.answ)>1){
      j.answ=j.answ%>%
        dplyr::group_by(gear,area,target)%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
      j.answ$uncertainty=ifelse(is.na(j.answ$uncertainty),0,j.answ$uncertainty)
    }
    
    j.answ.2=x.answ.2[x.answ.2$event_code==j.event,]  
    j.answ.2=j.answ.2[abs(j.answ.2$value)<=5,]
    if(nrow(j.answ.2)>1){
      j.answ.2=j.answ.2%>%
        dplyr::group_by(gear,area,target)%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
      j.answ.2$uncertainty=ifelse(is.na(j.answ.2$uncertainty),0,j.answ.2$uncertainty)
    }
    
    
    # without an adaptive strategy
    i.cpt=answer.to.cpt_vH(x.ans=j.answ$value+adj.factor, 
                     unc=j.answ$uncertainty,
                     dim.name = i.node,
                     div.factor = length(answ.range), # this is not always 3!
                     dim.labs = x.lev)
    df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style & df.var$strategy=='no',]$Freq=i.cpt
    
    # with an adaptive strategy
    if(j.answ.2$value>j.answ$value){
      i.cpt=answer.to.cpt_vH(x.ans=j.answ.2$value+adj.factor, 
                              unc=j.answ.2$uncertainty,
                              dim.name = i.node,
                              div.factor = length(answ.range), # this is not always 3!
                              dim.labs = x.lev)
    }
    df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style & df.var$strategy=='yes',]$Freq=i.cpt
    
    
    df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]
    
    # single option
    if(nrow(j.answ)==1){
      i.cpt=answer.to.cpt_vH(x.ans=j.answ$value+adj.factor, 
                             unc=j.answ$uncertainty,
                             dim.name = i.node,
                             div.factor = length(answ.range), # this is not always 3!
                             dim.labs = x.lev)
      df.var[df.var$event==j.event & df.var$gear== i.gear[i],]$Freq=i.cpt  
    }
    
    # multiple options
    if(nrow(j.answ)>1){
      check.multi=data.frame(Freq=apply(j.answ[,1:3], 2, function(x)length(unique(x))))
      check.multi$type=rownames(check.multi)
      check.multi=check.multi[check.multi$Freq>1,]
      
      if(nrow(check.multi)==1){
        multi.options=j.answ[,check.multi$type]
        
        # TO DO make sure to have a generic baseline based on answers. 
        # FOr instance, in case only some of the species mentioned have a specific value. 
        # ALl the other should be assimilated to the not_specified
        
        for(k in 1:nrow(multi.options)){
          k.opt=multi.options[k,]
          k.feature=names(k.opt)
          if(k.opt %in% df.var[[k.feature]]==F){next}
          k.answ=j.answ[k,]
          i.cpt=answer.to.cpt_vH(x.ans=k.answ$value+adj.factor, 
                                 unc=k.answ$uncertainty,
                                 dim.name = i.node,
                                 div.factor = length(answ.range), # this is not always 3!
                                 dim.labs = x.lev)
          df.var[df.var$event==j.event & df.var$gear== i.gear[i] & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
        }
      }
    }
  }
  
  
  
  
  supp.data=data.frame(node=c('catch_condition',  'damage', 'personal_safety','stress'), 
                       link.w=c(x.answ$value, 0),
                       states=c(3,4,3,2),
                       type=c('parent','parent','parent','child'),
                       direction=c('neg', 'pos','pos', 'pos'),
                       id=1:4)
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = unique(x.answ$uncertainty), algorithm = 'min', xnam=xnam)
  x.cpt[[1]]%>%
    pivot_longer(cols=c('yes','no'), names_to = 'stress')%>%
    dplyr::filter(stress=='yes')%>%
    ggplot(aes(x= personal_safety, y=damage, fill=value))+
    geom_tile()+
    facet_wrap(~catch_condition)+
    scale_fill_viridis_c()
  
  z.cpt=x.cpt[[1]]%>%
    pivot_longer(cols=c('no','yes'), names_to = 'stress')%>%
    arrange(desc(catch_condition))
  
  df.var[df.var$fishing_style==styles[z,]$f.style,]$Freq=z.cpt$value
  
  
}
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)



# injury




stress.prob=cpdist(net, nodes = c('stress'), 
                   evidence = (event == 'sto'))
table(stress.prob)

graphviz.chart(net)












## old part ####

x.answ=dat[grep(i.node, dat$short_description),]

x.answ$target=ifelse(is.na(x.answ$target), 'not_specified', x.answ$target)
x.answ$area=ifelse(is.na(x.answ$area), 'not_specified', x.answ$area)
x.answ$gear=ifelse(is.na(x.answ$gear), 'not_specified', x.answ$gear)
x.answ=x.answ[!is.na(x.answ$value),]



store.res=NULL
for(xx in 1:length(f.styles)){
  
  #dat=int.dat[int.dat$fishing_style==f.styles[xx],]
  dat=int.dat[int.dat$id_I==1,]
  
  # root nodes
  # here is very important that the node definition in GeNie makes sense. Especially the order.- This needs to be revised
  x.nodes=nodes(net)
  x.nodes=x.nodes[x.nodes %in% dat$short_description]
  for(i in 1:length(x.nodes)){
    i.node=x.nodes[i]
    array.var=net[[i.node]]$prob
    xdim=dim(array.var)
    xnam=dimnames(array.var)
    df.var=as.data.frame(array.var)
    x.lev=levels(df.var[,i.node])
    x.answ=dat[grep(i.node, dat$short_description),]
    
    x.answ$target=ifelse(is.na(x.answ$target), 'not_specified', x.answ$target)
    x.answ$area=ifelse(is.na(x.answ$area), 'not_specified', x.answ$area)
    x.answ$gear=ifelse(is.na(x.answ$gear), 'not_specified', x.answ$gear)
    x.answ=x.answ[!is.na(x.answ$value),]
    
    # loop for events
    x.events=xnam$event
    for(j in 1:length(x.events)){
      j.event=x.events[j]
      j.answ=x.answ[x.answ$event_code == j.event,]
      
      # no multiple options mentioned: we replicate everything
      if(nrow(j.answ)==1){
        i.cpt=answer.to.cpt_vH(x.ans=j.answ$value, 
                               unc=j.answ$uncertainty,
                               dim.name = i.node,
                               div.factor = 5,
                               dim.labs = x.lev)
        df.var[df.var$event==j.event,]$Freq=i.cpt
      }
      # multiple answers detected
      if(nrow(j.answ)>1){
        check.multi=data.frame(Freq=apply(j.answ[,1:3], 2, function(x)length(unique(x))))
        check.multi$type=rownames(check.multi)
        check.multi=check.multi[check.multi$Freq>1,]
        
        # only one multiple options, therefore no interactions
        if(nrow(check.multi)==1){
          multi.options=j.answ[,check.multi$type]
          
          for(k in 1:nrow(multi.options)){
            k.opt=multi.options[k,]
            k.answ=j.answ[k,]
            
            i.cpt=answer.to.cpt_vH(x.ans=k.answ$value, 
                                   unc=k.answ$uncertainty,
                                   dim.name = i.node,
                                   div.factor = 5,
                                   dim.labs = x.lev)
            
            df.var[df.var$event==j.event & df.var[['gear']]=='Rod',]
            
            df.var[df.var$event==j.event,]$Freq=i.cpt
          }
          
        }
        
        i.cpt=answer.to.cpt_vH(x.ans=j.answ$value, 
                               unc=j.answ$uncertainty,
                               dim.name = i.node,
                               div.factor = 5,
                               dim.labs = x.lev)
        df.var[df.var$event==j.event,]$Freq=i.cpt
      }
      
      
      supp.data=data.frame(node=c(target.dims[2:length(target.dims)],x.var), 
                          link.w=c(x.answ[x.answ$event_code== target.dims[2:length(target.dims)],]$value, 0),
                          states=c(trg.size,3),
                          type=c(rep('parent', length(target.dims)-1) , 'child'),
                          direction=c(rep('neg', length(target.dims))),
                          id=1:length(target.dims))
    
      x.cpt=get.cpt(nodes.df = supp.data, uncertainty = unique(x.answ$uncertainty), algorithm = 'min', xnam=xnam)
    }
    
    
    target.dims=names(df.var)
    target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]
    
    trg.size=dimnames(array.var)[target.dims[2:length(target.dims)]]
    trg.size=as.numeric(lapply(trg.size, function(x)length(x)))
    

    net[[x.var]]=x.cpt$bn.cpt
    
    
    
    
    
    i.dat=dat[dat$short_description==i.node,]
    i.cpt=answer.to.cpt_vH(x.ans=i.dat$value, 
                           unc=i.dat$uncertainty,
                           dim.name = i.node,
                           div.factor = 5,
                           dim.labs = i.lev)
    net.h[[i.node]]=i.cpt
  }
  
  i.node='professional'
  i.lev=names(net.h[[i.node]]$prob)
  x.pro=ifelse(f.styles[xx] %in% c('recreational', 'archipelago'),1,5)
  i.cpt=answer.to.cpt_vH(x.ans=x.pro, 
                         unc=0.001,
                         dim.name = i.node,
                         div.factor = 5,
                         dim.labs = i.lev)
  net.h[[i.node]]=i.cpt
  
  
  
  
}


x.var='catchability'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]

x.answ=dat[grep(x.var, dat$short_description),]
x.answ=x.answ[x.answ$event_code %in% target.dims,] 
if(is.na(unique(x.answ$target))){
  x.answ$target='not_specified'
}

trg.size=dimnames(array.var)[target.dims[2:length(target.dims)]]
trg.size=as.numeric(lapply(trg.size, function(x)length(x)))

supp.data=data.frame(node=c(target.dims[2:length(target.dims)],x.var), 
                     link.w=c(x.answ[x.answ$event_code== target.dims[2:length(target.dims)],]$value, 0),
                     states=c(trg.size,3),
                     type=c(rep('parent', length(target.dims)-1) , 'child'),
                     direction=c(rep('neg', length(target.dims))),
                     id=1:length(target.dims))

x.cpt=get.cpt(nodes.df = supp.data, uncertainty = unique(x.answ$uncertainty), algorithm = 'min', xnam=xnam)
net[[x.var]]=x.cpt$bn.cpt















# Human community - Variables ####
net.h=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/BN_lower_end_2.net', debug = T)
graphviz.plot(net.h, layout = "dot", fontsize = 50)

# root nodes
# here is very important that the node definition in GeNie makes sense. Especially the order.- This needs to be revised
x.nodes=nodes(net.h)
x.nodes=x.nodes[x.nodes %in% dat$short_description]
i=5
for(i in 1:length(x.nodes)){
  i.node=x.nodes[i]
  i.lev=names(net.h[[i.node]]$prob)
  i.dat=dat[dat$short_description==i.node,]
  i.cpt=answer.to.cpt_vH(x.ans=i.dat$value, 
                unc=i.dat$uncertainty,
                dim.name = i.node,
                div.factor = 5,
                dim.labs = i.lev)
  net.h[[i.node]]=i.cpt
}



# Importance ####
## Comments/to do: also here the order of states matters a lot: negative goes first!!! this has to be check
imp.vars=c('societal_importance', 'individual_importance' ,'economic_importance','innovative_capacity' )
for(i in 1:length(imp.vars)){
  
  x.var=imp.vars[i]
  array.var=net.h[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,x.var])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
  link.w=c(rep(1, length(xdim)))
  if(x.var=='economic_importance'){
    link.w=c(0.5,1,1)
  }
  if(x.var=='innovative_capacity'){
    link.w=c(0.8,1,1)
  }
  supp.data=data.frame(node=c(target.dims,x.var), 
                       link.w=link.w,
                       states=c(xdim[2:length(xdim)], xdim[1]),
                       type=c(rep('parent', length(target.dims)) , 'child'),
                       direction=c(rep('pos', length(xdim))),
                       id=1:length(xdim))
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'max', xnam=xnam)
  net.h[[x.var]]=x.cpt$bn.cpt
}




# risks ####
risk.vars=c('societal_risk', 'individual_risk', 'economic_risk')

for(i in 1:length(risk.vars)){
  x.var=risk.vars[i]
  array.var=net.h[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,x.var])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
  direction=c('neg', 'pos','neg')
  if(x.var=='economic_risk'){
    direction=c('neg', 'neg','pos','pos')
  }
  supp.data=data.frame(node=c(target.dims,x.var), 
                       link.w=c(rep(1, length(xdim))),
                       states=c(xdim[2:length(xdim)], xdim[1]),
                       type=c(rep('parent', length(target.dims)) , 'child'),
                       direction=direction,
                       id=1:length(xdim)) 
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
  net.h[[x.var]]=x.cpt$bn.cpt
}



x.var='economic_risk'
array.var=net.h[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(rep(1, length(xdim))),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=c('neg','pos', 'pos','neg'),
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)

net.h[[x.var]]=x.cpt$bn.cpt










econrisk.cpt=get.cpt(data.frame(node=c('profit','innov', 'econ_imp','econ_risk'), 
                                link.w=c(8,6,10,NA),
                                states=c(3,3,3,3),
                                type=c('parent','parent','parent','child'),
                                direction=c('neg', 'neg','pos','pos'),
                                id=1:4), uncertainty = 0.1, algorithm = 'min')

# seasonal results second layer ####
health.cpt=get.cpt(data.frame(node=c('injury','stress', 'health'), 
                              link.w=c(1,0.8,0),
                              states=c(3,3,3),
                              type=c('parent','parent','child'),
                              direction=c('neg', 'neg','pos'),
                              id=1:3), algorithm = 'min', uncertainty = 0.1)

satisf.cpt=get.cpt(data.frame(node=c('cquality','catches', 'csize','satisfaction'), 
                              link.w=c(1,1,1,0),
                              states=c(3,3,3,3),
                              type=c('parent','parent','parent','child'),
                              direction=c('pos', 'pos','pos','pos'),
                              id=1:4), uncertainty = 0.1, algorithm = 'min')

non.mon.cpt=get.cpt(data.frame(node=c('ftime','health', 'satisfaction','nonmon_val'), 
                               link.w=c(1,0.8,0.5,0),
                               states=c(3,3,3,3),
                               type=c('parent','parent','parent','child'),
                               direction=c('pos', 'pos','pos','pos'),
                               id=1:4), uncertainty = 0.15, algorithm = 'min')

## initial values ####
csize.prob= answer.to.cpt(x.ans=2.5, dim.name='csize', unc = 0.5) # dummy
catches.prob= answer.to.cpt(x.ans=2.5, dim.name='catches', unc = 0.5) # dummy
cquality.prob= answer.to.cpt(x.ans=2.5, dim.name='cquality', unc = 0.5) # dummy
stress.prob= answer.to.cpt(x.ans=2.5, dim.name='stress', unc = 0.5) # dummy
ftime.prob= answer.to.cpt(x.ans=2.5, dim.name='ftime', unc = 0.5) # dummy
event.prob= cpt.bin(x.ans=2, dim.name='event', dim.labs = c('y','n'))
event.prob[1:2]=c(0.5,0.5)
manag.prob=cpt.bin(x.ans=2, dim.name='management', dim.labs = c('rigid','flexible'))
manag.prob[1:2]=c(0.5,0.5)
handl.prob=cpt.bin(x.ans=2, dim.name='handling', dim.labs = c('y','n'))
handl.prob[1:2]=c(0.5,0.5)

# Events ####
# characteristics
gear.prob=answer.to.cpt(x.ans=gear.ans, div.factor = 4, unc=.001, dim.name='gear',dim.labs = c('not_mobile','mobile'), x.breaks=c(-Inf,0.5,Inf))
infrastr.prob=answer.to.cpt(x.ans=infr.ans, div.factor = 4, unc=.001, dim.name='infrastr',dim.labs = c('bad','good'), x.breaks=c(-Inf,0.5,Inf))


## spatial mobility
p1=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(0,1), c(0,1)))
p2=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(1,0), c(.3,.7)))
p3=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(0,1), c(0,1)))
p4=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(.5,.5), c(.1,.9)))
mobility.cpt=expand.probs(x.levels=list(c('y','n'),c('good','bad'),c('rigid','flexible') , c('mobile','not_mobile')) ,
                          x.names=c( 'mobility','infrastr','management','gear'),
                          x.probs=list(p1,p2,p3,p4))

# being aware that the event is coming
alert.base=answer.to.cpt(x.ans=surprise.ans, dim.name='alert', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
alert.cpt=expand.probs(x.levels=list(c('y','n'),c('n','y')) ,
                       x.names=c( 'alert','event'),
                       x.probs=list(c(0,1),alert.base)) ;alert.cpt

## potential consequences 
# good idea to have some probabilities beyond yes/no

# which kind of extra maintenance (if any)?
maint.cpt=answer.to.cpt(x.ans=maint.ans, dim.name='maintenance', dim.labs = c('no','small','large'))
maint.cpt=expand.probs(x.levels=list(c('no','small','large'),c('y','n')) ,
                       x.names=c( 'mainten','event'),
                       x.probs=list(maint.cpt,c(1,0,0))) ## on this you can also consider a different distribution to answer something like: not so likely, but if it happens is large



# how likely does your gear break?
break.cpt=answer.to.cpt(x.ans=break.ans, dim.name='gbreak', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
break.cpt=expand.probs(x.levels=list(c('n','y'),c('y','n')) ,
                       x.names=c( 'gbreak','event'),
                       x.probs=list(break.cpt,c(0,1))) 

# how likely the fish does not look good? 
chealth.cpt=answer.to.cpt(x.ans=quality.ans, dim.name='health', dim.labs = c('good','bad'), x.breaks = c(-Inf, 0.5,Inf))
chealth.cpt=expand.probs(x.levels=list(c('good','bad'),c('y','n')) ,
                         x.names=c( 'health','event'),
                         x.probs=list(chealth.cpt,c(1,0))) 

# how likely you can go to an area that is NOT affected by the event?
area.cpt=answer.to.cpt(x.ans=area.ans, dim.name='area', dim.labs = c('usual','distant'), x.breaks = c(-Inf, 0.5,Inf))
area.cpt=expand.probs(x.levels=list(c('usual','distant'), c('n','y'), c('n','y')) ,
                      x.names=c( 'area','alert', 'mobility'),
                      x.probs=list(c(1,0),
                                   c(1,0),
                                   c(1,0),
                                   area.cpt)) 

## realised consequences
q.cpt=answer.to.cpt(x.ans=q.ans, dim.name='catchability', dim.labs = c('less','same','more'))
q.cpt=expand.probs(x.levels=list(c('less','same','more'),c('y','n'),c('y','n'),c('usual','distant')) ,
                   x.names=c( 'catchability','event','handling', 'area'),
                   x.probs=list(as.numeric(table(cut((q.ans.bis+q.ans)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                as.numeric(table(cut((q.ans.bis+0)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                as.numeric(table(cut((0+q.ans)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                c(0,1,0),
                                # distant area
                                c(0,1,0),c(0,1,0),
                                c(0,1,0),c(0,1,0)
                                )) ;q.cpt # this should be improved
## catch assemblage (expected)
assembl.cpt=expand.probs(x.levels=list(c('usual','different'), c('usual','distant')) ,
                         x.names=c( 'assemblage','area'),
                         x.probs=list(c(1,0),
                                      cpt.bin(x.ans=ass.ans, dim.name = 'x', dim.labs = c('y','n'))))

# catch quality accounting for the effect of handling
handl.effect=answer.to.cpt(x.ans=handl.ans, dim.name='handling', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
c.quality.cpt=expand.probs(x.levels=list(c('bad','good'),c('good','bad'),c('y','n'),c('usual','distant')) ,
                           x.names=c( 'quality','health', 'handling','area'),
                           x.probs=list(c(0,1), 
                                        handl.effect,
                                        c(0,1), 
                                        c(1,0), 
                                        c(0,1), 
                                        c(0,1), 
                                        c(0,1), 
                                        c(0,1)))

# injury
injury.cpt=expand.probs(x.levels=list(c('no','minor','life'),c('no','minor','life'),c('usual','distant')) ,
                        x.names=c( 'injury','safety','area'),
                        x.probs=list(c(1,0,0),c(0,1,0),c(0,0,1), # mirroring the safety
                                     c(1,0,0),
                                     c(1,0,0),
                                     c(1,0,0)))

# stress
stress.base=get.cpt(data.frame(node=c('gbreak', 'health', 'safety','stress'), 
                   link.w=c(stress.1.ans,stress.2.ans,stress.3.ans, 0),
                   states=c(2,2,3,2),
                   type=c('parent','parent','parent','child'),
                   direction=c('pos', 'neg','pos', 'pos'),
                   id=1:4), uncertainty = 0.15, algorithm = 'max')

stress.cpt=expand.probs(x.levels=list(c('n','y'),c('no','minor', 'life'),c('y','n'),c('good','bad'), c('usual','distant')) ,
                        x.names=c( 'stress','safety','gbreak','health','area'),
                        x.probs=list(as.numeric(stress.base$report.cpt[7,4:5]),
                                     as.numeric(stress.base$report.cpt[8,4:5]),
                                     as.numeric(stress.base$report.cpt[9,4:5]),
                                     as.numeric(stress.base$report.cpt[1,4:5]),
                                     as.numeric(stress.base$report.cpt[2,4:5]),
                                     as.numeric(stress.base$report.cpt[3,4:5]),
                                     as.numeric(stress.base$report.cpt[10,4:5]),
                                     as.numeric(stress.base$report.cpt[11,4:5]),
                                     as.numeric(stress.base$report.cpt[12,4:5]),
                                     as.numeric(stress.base$report.cpt[4,4:5]),
                                     as.numeric(stress.base$report.cpt[5,4:5]),
                                     as.numeric(stress.base$report.cpt[6,4:5]),
                                     ## area distant
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0)
                        )) ;stress.cpt


# expected costs. This needs to be further elaborated based on the answers!!!
costs.cpt=expand.probs(x.levels=list(c('n','l','m','h'),c('no','small','large'),
                                     c('y','n'),c('usual','distant')) ,
                       x.names=c( 'cost','mainten','gbreak','area'),
                       x.probs=list(# if you break the gear the cost is high
                                    c(0,0,0,1),c(0,0,0,1), c(0,0,0,1), 
                                    # if you do not break the gear, the cost mirrors the maintenance
                                    c(1,0,0,0),c(0,1,0,0), c(0,0,1,0), 
                                    # when you travel far, the cost is always low
                                    c(0,1,0,0),c(0,1,0,0), c(0,1,0,0), 
                                    c(0,1,0,0),c(0,1,0,0), c(0,1,0,0))) ; costs.cpt

## decisions
allow.cpt=expand.probs(x.levels=list(c('y','n'),c('usual','different'),c('rigid','flexible')) ,
                       x.names=c( 'permit','assemblage','management'),
                       x.probs=list(c(1,0), 
                                    c(0,1),  
                                    c(1,0), 
                                    c(1,0))) 

fish.cpt=expand.probs(x.levels=list(c('n','y'),c('y','n'),c('y','n'),c('usual','distant')) ,
                      x.names=c( 'fishing','permit','alert', 'area'),
                      x.probs=list(answer.to.cpt(x.ans=fish.ans, dim.name='area', dim.labs = c('n','y'), 
                                                 x.breaks = c(-Inf, 0.5,Inf), unc=0.4), 
                                   c(1,0), c(0,1), c(1,0), c(0,1), c(1,0), c(0,1), c(1,0))) ; fish.cpt




        