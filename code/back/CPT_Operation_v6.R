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
int.dat=read_csv("data/read_only/coding_report_unc.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
evts=evts[!is.na(evts$event_code),]
evts$season=c('winter','summer','winter','summer','summer','all','all')
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')
f.styles=unique(int.dat[int.dat$id_q==0,]$text)

style.dataset=read_excel("data/lists_fishing_styles.xlsx", 
           sheet = "fishing_style")

strategy.dataset=read.csv("data/adaptive_revised_v2.csv")
#strategy.unique=strategy.dataset[nchar(strategy.dataset$strategy)>0,]
strategy.unique=strategy.dataset[nchar(strategy.dataset$strategy)>0&
                                   !is.na(strategy.dataset$event_code),]%>%
  distinct(id_I, event_code, strategy, solution)%>%
  dplyr::filter(!is.na(strategy))

# DAG
net=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/type1_v6_general_simple.net', debug = T)
graphviz.plot(net, layout = "dot", fontsize = 18)
graphviz.chart(net)

# check missing data
x.nodes=nodes(net)
x.nodes=x.nodes[x.nodes %in% int.dat$short_description]
int.dat[abs(int.dat$uncertainty)>10 & !is.na(int.dat$uncertainty),]$uncertainty=1/5

# start ####
x.nodes=nodes(net)

## event probability ####
array.var=net[['event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['event']]=array.var

## fisher probability ####
array.var=net[['fishing_style']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['fishing_style']]=array.var

## strategy ####
i.node='strategy'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles$season=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
styles[grep('guid', styles$f.style),]$f.style.2='guide'
styles$season=c('winter','summer','summer','all','all','all','winter','summer')

for(z in 1:length(styles$f.style)){

  answ.range=1:5
  adj.factor=ifelse(min(answ.range)==0,0.5,0)
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  
  i.strategy=strategy.unique[strategy.unique$id_I %in% i.fisher,]
  i.strategy=i.strategy%>%distinct(event_code,id_I, strategy)
  
  str.selection=strategy.dataset[strategy.dataset$id_I%in%i.fisher,]
  
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  dat=dat[is.na(dat$unit_range),]
  dat$target=ifelse(is.na(dat$target), 'not_specified', dat$target)
  dat$area=ifelse(is.na(dat$area), 'not_specified', dat$area)
  dat=dat[dat$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]
  dat$gear=ifelse(is.na(dat$gear), 'not_specified', dat$gear)
  dat=dat[!is.na(dat$value),]
  
  
  # strategy
  nodes.selection=str.selection[!is.na(str.selection$strategy_node),]
  nodes.selection=nodes.selection[,c('id_I', 'id_q', 'event_code', 'strategy')]
  x.answ=left_join(nodes.selection, dat)
 
  i.evts=unique(df.var$event)
  for(j in 1:length(i.evts)){
    
    j.event=i.evts[j]
    
    # is the event possible given the seasonality?
    if(z.style$season!='all'){
      evt.season=evts[evts$event_code==j.event,]$season
      if(z.style$season!=evt.season){
        if(evt.season!='all'){
          df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(0,0,0,1)
        next 
        }
      }
    }
    
    # if so, go ahead
    j.strategy=i.strategy[i.strategy$event_code==i.evts[j],]
    j.strategy.unique=unique(j.strategy$strategy)
    j.answ=x.answ[x.answ$event_code==j.event,]  
    j.answ=j.answ[abs(j.answ$value)<=5,]
    j.answ$value=ifelse(j.answ$id_q %in% c(13),round(((j.answ$value+1)/3)*5), j.answ$value )
    
    
    if(nrow(j.answ)==0){
      df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(1,0,0,0)
      next
    }
    
    if(length(j.strategy.unique)>1){
      
      if('A' %in% j.strategy.unique){
        j.adapt=j.answ[j.answ$strategy=='A',]
      if(nrow(j.adapt)>1){
        # this implies there is just one respondent
        if(unique(j.adapt$id_q) %in% 1:2){
          j.adapt=j.adapt[sort(j.adapt$value),]
          j.adapt=j.adapt[1,]  
          j.adapt$weight=1
        }else{
          j.adapt=j.adapt%>%
            dplyr::group_by(gear,area,target)%>%
            dplyr::summarise(uncertainty=sd(value), value=mean(value), weight=n(), .groups = "keep") # adjust the uncertainty
          j.adapt$uncertainty=ifelse(is.na(j.adapt$uncertainty),0,j.adapt$uncertainty) 
        }
      }else{
        j.adapt$weight=1
      }
        p=answer.to.cpt_vH(x.ans=mean(j.adapt$value), 
                            unc=mean(j.adapt$uncertainty),
                            dim.name = i.node,
                            div.factor = length(answ.range), # this is not always 3!
                            dim.labs = c('cope', 'adapt'))
        pA=data.frame(p)
        pA$lab=rownames(pA)
        pA$w=j.adapt$weight
      }else{
        pA=data.frame(p=0, lab='adapt', w=0)
      }
      
      
      if('R' %in% j.strategy.unique){
        j.react=j.answ[j.answ$strategy=='R',]
      if(nrow(j.react)>1){
        if(unique(j.react$id_q) %in% 1:2){
          j.react=j.react[sort(j.react$value),]
          j.react=j.react[1,]   
        }else{
          j.react=j.react%>%
            dplyr::group_by(gear,area,target)%>%
            dplyr::summarise(uncertainty=sd(value), value=mean(value), weight=n(), .groups = "keep") # adjust the uncertainty
          j.react$uncertainty=ifelse(is.na(j.react$uncertainty),0,j.react$uncertainty) 
        }
      }else{
        j.react$weight=1
      }
        p=answer.to.cpt_vH(x.ans=mean(j.react$value), 
                            unc=mean(j.react$uncertainty),
                            dim.name = i.node,
                            div.factor = length(answ.range), # this is not always 3!
                            dim.labs = c('cope', 'react'))
        pR=data.frame(p)
        pR$lab=rownames(pR)
        pR$w=j.react$weight
      }else{
        pR=data.frame(p=0, lab='react', w=0)
      }
      
      if('C' %in% j.strategy.unique){
        pC=data.frame(p=1, lab='cope', w=nrow(j.strategy[j.strategy$strategy=='C',]))
      }else{
        pC=data.frame(p=0, lab='react', w=0)
      }
      
      pARC=rbind(pA,pR,pC)%>%
        dplyr::mutate(p.w=p*w)%>%
        dplyr::mutate(p=p.w/sum(p.w))%>%
        dplyr::group_by(lab)%>%
        dplyr::summarise(p=sum(p))
      pARC=pARC[match(c('cope','react','adapt'),pARC$lab ),]
      df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(pARC$p,0)
      next  
    }
    
    
    if(nrow(j.answ)>1){
      if(z %in% c(1,2,7)){
           j.answ=j.answ[sort(j.answ$value),]
      j.answ=j.answ[1,]   
      }else{
        j.answ=j.answ%>%
        dplyr::group_by(gear,area,target)%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
      j.answ$uncertainty=ifelse(is.na(j.answ$uncertainty),0,j.answ$uncertainty) 
      }
    }
    
    if(j.strategy.unique=='A'){
      # adapt or react? 
      i.cpt=answer.to.cpt_vH(x.ans=j.answ$value, 
                             unc=j.answ$uncertainty,
                             dim.name = i.node,
                             div.factor = length(answ.range), # this is not always 3!
                             dim.labs = c('cope', 'adapt'))
      df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(i.cpt[1],0,i.cpt[2],0)
      next 
    }
    if(j.strategy.unique =='R'){
      # cope or react? 
      i.cpt=answer.to.cpt_vH(x.ans=j.answ$value, 
                             unc=j.answ$uncertainty,
                             dim.name = i.node,
                             div.factor = length(answ.range), # this is not always 3!
                             dim.labs = c('cope', 'react'))
      df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(i.cpt,0,0)
      next
    }
    if(j.strategy.unique =='C'){
      df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style,]$Freq=c(1,0,0,0)
      next
    }
  }
}

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  ggplot(aes(x=event, y=Freq, fill=strategy))+
  geom_col()+
  facet_wrap(~fishing_style);pl

ggsave(plot=pl, 'results/images/strategy.jpeg', width = 18, height = 8, units='cm', dpi=150)

## go fishing ####
i.node='go_out'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles$season=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
styles[grep('guid', styles$f.style),]$f.style.2='guide'
styles$season=c('winter','summer','summer','all','all','all','winter','summer')

df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,1)
df.var[df.var$strategy=='adapt',]$Freq=c(0,1,0)
df.var[df.var$strategy=='react',]$Freq=c(0,1,0)
df.var[df.var$strategy=='cope',]$Freq=c(1,0,0)

for(z in 1:length(styles$f.style)){
  answ.range=0:1
  adj.factor=ifelse(min(answ.range)==0,0.5,0)
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$style==z.style$f.style.2,]
  i.fisher=unique(i.fisher$id)
  i.strategy=strategy.dataset[strategy.dataset$id_I %in% i.fisher,]
  i.strategy$strategy=ifelse(i.strategy$strategy=='A','A','RC')
  #i.strategy=i.strategy%>%distinct(event_code, strategy)
  
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  dat=dat[is.na(dat$unit_range),]
  dat$target=ifelse(is.na(dat$target), 'not_specified', dat$target)
  dat$area=ifelse(is.na(dat$area), 'not_specified', dat$area)
  dat=dat[dat$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]
  dat$gear=ifelse(is.na(dat$gear), 'not_specified', dat$gear)
  dat=dat[!is.na(dat$value),]
  
  # go fishing
  x.answ=dat[grep('go_out', dat$short_description),]
  i.evts=unique(df.var$event)
  for(j in 1:length(i.evts)){
    
  j.event=i.evts[j]
    
  j.answ=x.answ[x.answ$event_code==j.event,]  
  j.answ=j.answ[abs(j.answ$value)<=5,]
  
  if(nrow(j.answ)==0){
    df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style & df.var$strategy=='cope',]$Freq=c(1,0,0)
    next
  }
  
  if(nrow(j.answ)>1){
    j.answ=j.answ%>%
      dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
    j.answ$uncertainty=ifelse(is.na(j.answ$uncertainty),0,j.answ$uncertainty)
  }

  # cope (accept passively)
  i.cpt=answer.to.cpt_vH(x.ans=j.answ$value, 
                       unc=j.answ$uncertainty,
                       dim.name = i.node,
                       div.factor = length(answ.range), # this is not always 3!
                       dim.labs = c('no','yes'))
  df.var[df.var$event==j.event & df.var$fishing_style==styles[z,]$f.style & df.var$strategy=='cope',]$Freq=c(i.cpt,0)

  }
}
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(strategy=='cope')%>%
  ggplot(aes(x=event, y=Freq, fill=go_out))+
  geom_col()+
  facet_wrap(~fishing_style);pl

ggsave(plot=pl, 'results/images/go_fishing.jpeg', width = 18, height = 8, units='cm', dpi=150)

# technical solutions
i.node='additional_mitigation'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

pl=df.var%>%
  dplyr::filter(strategy=='react')%>%
  ggplot(aes(x=event, y=Freq, fill=additional_mitigation))+
  geom_col()+
  facet_wrap(~fishing_style);pl

ggsave(plot=pl, 'results/images/mitigation.jpeg', width = 18, height = 8, units='cm', dpi=150)

## gear ####
i.node='gear'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

styles=data.frame(f.style=unique(df.var$fishing_style))
styles$f.style.2=NA
styles$season=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='trawler'
styles[grep('archi', styles$f.style),]$f.style.2='archipelago'
styles[grep('guid', styles$f.style),]$f.style.2='guide'
styles$season=c('winter','summer','summer','all','all','all','winter','summer')

df.var[df.var$strategy=='not_relevant',]$Freq=c(rep(0,6),1)

# those that never change gear
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='industrial',]$Freq=c(1, rep(0,6))
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='archipelago',]$Freq=c(0,1, rep(0,5))
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='recreational_winter',]$Freq=c(0,0,0,0,1,0,0)
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='artisanal_salmon',]$Freq=c(0,0,1,0,0,0,0)

# those that changes
## recreational in summer
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='recreational_summer',]$Freq=c(0,0,0,1,0,0,0)
df.var[df.var$strategy=='adapt' & 
         df.var$fishing_style=='recreational_summer'&
         df.var$event=='abl',]$Freq=c(0,0,0,0,0,1,0)

## coastal
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='artisanal',]$Freq=c(0,0.7,0.2,0,0.1,0,0)


df.var[df.var$strategy=='adapt' & 
         df.var$fishing_style=='artisanal'&
         df.var$event=='hww',]$Freq=c(0,1,0,0,0,0,0)
df.var[df.var$strategy=='adapt' & 
         df.var$fishing_style=='artisanal'&
         df.var$event=='hws',]$Freq=c(0,1,0,0,0,0,0)

## fishing guide
df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='guide_winter',]$Freq=c(0,0,0,0.2,0.8,0,0)

df.var[df.var$strategy=='adapt' & df.var$fishing_style=='guide_winter'&
         df.var$event=='hww',]$Freq=c(0,0,0,1,0,0,0)

df.var[df.var$strategy!='not_relevant' & df.var$fishing_style=='guide_summer',]$Freq=c(0,0,0,1,0,0,0)

df.var[df.var$strategy=='adapt' & df.var$fishing_style=='guide_summer'&
         df.var$event=='abl',]$Freq=c(0,0,0,0,0,1,0)

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(strategy=='adapt')%>%
  ggplot(aes(x=event, y=Freq, fill=gear))+
  geom_col()+
  facet_wrap(~fishing_style);pl

ggsave(plot=pl, 'results/images/gear.jpeg', width = 18, height = 8, units='cm', dpi=150)

## target ####
i.node='target'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

df.var$Freq=c(rep(0,6),1) # the baseline is not better specified for all
# baseline target per gear
# species are perch, pike, zander, herring, salmon, whitefish, not specified
df.var[df.var$strategy!='not_relevant' & df.var$gear=='trawl',]$Freq=c(0,0,0,1,0,0,0)
df.var[df.var$strategy!='not_relevant' & df.var$gear=='gillnets',]$Freq=c(0.2,0,0,0.5,0,0.3,0)
df.var[df.var$strategy!='not_relevant' & df.var$gear=='trap',]$Freq=c(0,0,0,0,1,0,0)
df.var[df.var$strategy!='not_relevant' & df.var$gear=='rod',]$Freq=c(0.4,0.3,0.2,0,0.1,0,0)
df.var[df.var$strategy!='not_relevant' & df.var$gear=='icefishing',]$Freq=c(0.5,0.5,0,0,0,0,0)

# those that changes
df.var[df.var$strategy=='adapt' & 
         df.var$gear=='gillnets' &
         df.var$event=='hws',]$Freq=c(1,0,0,0,0,0,0)

df.var[df.var$strategy=='adapt' & 
         df.var$gear=='rod' &
         df.var$event=='hws',]$Freq=c(0.5,0,0.5,0,0,0,0)

df.var[df.var$strategy=='adapt' & 
         df.var$gear=='rod' &
         df.var$event=='hww',]$Freq=c(0,0,0,0,1,0,0)


net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(strategy=='adapt')%>%
  ggplot(aes(x=event, y=Freq, fill=target))+
  geom_col()+
  facet_wrap(~gear);pl

ggsave(plot=pl, 'results/images/target.jpeg', width = 18, height = 8, units='cm', dpi=150)

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
styles[grep('guid', styles$f.style),]$f.style.2='guide'
z=7
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
styles[grep('guid', styles$f.style),]$f.style.2='guide'
z=8
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

## personal_safety ####

i.node='personal_safety'
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
  
  # baseline answer: no mitigations
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
        df.var[df.var$event==j.event & df.var$gear== i.gear[i] & df.var$additional_mitigation!='travel_further',]$Freq=i.cpt  
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
            df.var[df.var$event==j.event & df.var$gear== i.gear[i] & 
                     df.var$additional_mitigation!='travel_further'
                   & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
          }
        }
      }
    }
  }
  
  # mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
  df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0)
  
  df.var[df.var$event %in% c('sto','gal', 'hww') & 
           df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0)
  
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


pl=df.var%>%
  dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=event, y=Freq, fill=personal_safety))+
  geom_col()+
  facet_wrap(~gear);pl

ggsave(plot=pl, 'results/images/safety.jpeg', width = 18, height = 8, units='cm', dpi=150)



## damage ####
i.node='damage'
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

# baseline answer: no mitigations
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
      df.var[df.var$event==j.event & df.var$gear== i.gear[i] & df.var$additional_mitigation!='travel_further',]$Freq=i.cpt  
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
          df.var[df.var$event==j.event & df.var$gear== i.gear[i] & 
                   df.var$additional_mitigation!='travel_further'
                 & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
        }
      }
    }
  }
}

# mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0,0)

df.var[df.var$event %in% c('sto','gal', 'hww') & 
         df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0,0)

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=event, y=Freq, fill=damage))+
  geom_col()+
  facet_wrap(~gear);pl

ggsave(plot=pl, 'results/images/damage.jpeg', width = 18, height = 8, units='cm', dpi=150)



## catch_condition ####
i.node='catch_condition'
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



  dat=int.dat
  x.answ=dat[grep(i.node, dat$short_description),]
  x.answ=x.answ[is.na(x.answ$unit_range),]
  
  # revise the following and make sure to exclude any gear that is different
  x.answ$target=ifelse(is.na(x.answ$target), 'not_specified', x.answ$target)
  x.answ$area=ifelse(is.na(x.answ$area), 'not_specified', x.answ$area)
  x.answ=x.answ[x.answ$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]
  x.answ$gear=ifelse(is.na(x.answ$gear), 'not_specified', x.answ$gear)
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
      df.var[df.var$event==j.event,]$Freq=i.cpt  
    }
    # multiple options
    if(nrow(j.answ)>1){
      check.multi=data.frame(Freq=apply(j.answ[,1:3], 2, function(x)length(unique(x))))
      check.multi$type=rownames(check.multi)
      check.multi=check.multi[check.multi$Freq>1,]
      
      if(nrow(check.multi)==1){
        multi.options=j.answ[,check.multi$type]
        
        # generic baseline based on answers. 
        generic.cpt=answer.to.cpt_vH(x.ans=mean(j.answ$value)+adj.factor, 
                         unc=mean(j.answ$uncertainty),
                         dim.name = i.node,
                         div.factor = length(answ.range), # this is not always 3!
                         dim.labs = x.lev)
        
        df.var[df.var$event==j.event ,]$Freq=generic.cpt
          
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
          df.var[df.var$event==j.event
                 & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
        }
      }
    }
}

# mitigations for catch conditions are many
solutions=strategy.dataset[!is.na(strategy.dataset$solution) & 
                             abs(strategy.dataset$value)<=5 & strategy.dataset$short_description=='cope_condition',]
solutions=solutions[nchar(solutions$solution)>4&!is.na(solutions$solution),]

df.var[df.var$target == 'herring' & df.var$event=='hws' & df.var$additional_mitigation=='other_technical',]$Freq=c(0,1,0) # ice machine 
df.var[df.var$event=='hws' & df.var$additional_mitigation=='short_sets',]$Freq=c(0,1,0) # ice machine 
df.var[df.var$event=='hws' & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1,0) # ice machine 
df.var[df.var$event=='abl' & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1,0) # ice machine 

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


pl=df.var%>%
  dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=event, y=Freq, fill=catch_condition))+
  geom_col()+
  facet_wrap(~target);pl

ggsave(plot=pl, 'results/images/catch_condition.jpeg', width = 18, height = 8, units='cm', dpi=150)



## catchability ####
i.node='catchability'
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
      df.var[df.var$event==j.event & df.var$gear== i.gear[i] ,]$Freq=i.cpt  
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
          df.var[df.var$event==j.event & df.var$gear== i.gear[i] 
                 & tolower(df.var[[k.feature]])==k.opt[[1]],]$Freq=i.cpt
        }
      }
    }
  }
}

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

df.var%>%
  ggplot(aes(x=event, y=Freq, fill=catchability))+
  geom_col()+
  facet_wrap(~target+gear)




# stress ####
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
styles[grep('guid', styles$f.style),]$f.style.2='guide'
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

df.var%>%
  ggplot(aes(x=event, y=Freq, fill=catchability))+
  geom_col()+
  facet_wrap(~target+gear)



bnlearn::write.net( '~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/v6_general_simple_leanrt.net', net)



