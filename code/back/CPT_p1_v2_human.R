remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
source('code/CPT_p1_v2.R')
remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)

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
  
  return(list(report.cpt=cpt.wide, bn.cpt=E.prob))
}

# data from interviews
int.dat=read_csv("data/read_only/coding_report_unc.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

# Human community - Variables ####
net=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/BN_p1_v2_learn_pt1.net', debug = T)
net.hc=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/BN_lower_end_2.net', debug = T)
graphviz.plot(net, layout = "dot", fontsize = 50)
graphviz.chart(net)

styles=net[['fishing_style']]$prob
styles=data.frame(styles)
names(styles)[1]='f.style'
styles$f.style.2=NA
styles[grep('recreat', styles$f.style),]$f.style.2='recreational'
styles[grep('artis', styles$f.style),]$f.style.2='coastal'
styles[grep('indust', styles$f.style),]$f.style.2='pelagic_trawler'
styles[grep('archi', styles$f.style),]$f.style.2='subsistence'
styles[grep('guid', styles$f.style),]$f.style.2='fishing guide'

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
    answ.range=1:5
    adj.factor=ifelse(min(answ.range)==0,0.5,0)
    dat=i.dat[i.dat$fishing_style==styles[z,]$f.style.2,]
    
    i.cpt=answer.to.cpt_vH(x.ans=dat$value, 
                  unc=dat$uncertainty,
                  dim.name = i.node,
                  div.factor = 5,
                  dim.labs = x.lev)
    
    df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=i.cpt
  }
  
  net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=home))

  
i.node='professional'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

df.var%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=Freq, fill=professional)) 
  # Importance ####
  ## Comments/to do: also here the order of states matters a lot: negative goes first!!! this has to be check

imp.vars=c('societal_importance', 'individual_importance' ,'economic_importance','innovative_capacity' )
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
    net[[x.var]]=x.cpt$bn.cpt
}


# risks ####
risk.vars=c('societal_risk', 'individual_risk', 'economic_risk')
  
for(i in 1:length(risk.vars)){
    x.var=risk.vars[i]
    array.var=net[[x.var]][['prob']]
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
    net[[x.var]]=x.cpt$bn.cpt
}
  
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


# non monetary value ####
x.var='non_monetary_value'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[-1]
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
df.var[df.var$go_out%in% c('no', 'not_relevant'),]$Freq=c(1,0,0)


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
  pivot_longer(cols=c('High','Medium', 'Low'), names_to = 'nonmonval')%>%
  dplyr::mutate(nonmonval=factor(nonmonval, levels=c('Low','Medium','High')))%>%
  arrange(satisfaction, health,(nonmonval))
df.var[df.var$go_out%in% c('yes'),]$Freq=x.cpt$value

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)


# satisfaction ####




# realised catches ####
x.var='Catches'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[-4]
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
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)

x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('no','poor', 'average','good'), names_to = 'catches')%>%
  dplyr::mutate(catches=factor(catches, levels=c('no','poor', 'average','good')))%>%
  arrange(catchability , stock_status ,(catches))
df.var[df.var$go_out%in% c('yes'),]$Freq=x.cpt$value

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)



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
  

bnlearn::write.net( '~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/BN_p1_v2_learn_pt2.net', net)

summary(net)


### let'### let'### let's checks what have we been doinnn
  jpeg(paste0('results/images/CPT_id', f.styles[xx], '.jpeg'), width=35, height=10, units='cm', res=500)
  graphviz.chart(net, scale=c(2,2))
  dev.off()
  
  junction <- compile(as.grain(net.h))
  est.cpt=querygrain(junction)
  
  est.cpt.long=lapply(est.cpt, function(x){
    ch=as.data.frame(x)
    ch$levels=rownames(ch)
    names(ch)[1]='value'
    return(ch)
  })
  est.cpt.long=plyr::ldply(est.cpt.long)
  names(est.cpt.long)[1]='node'
  est.cpt.long$id_I=as.character(f.styles[xx])
  store.res=rbind(store.res, est.cpt.long)
    

  
  

# some plotting ####
ggplot(data = store.res[store.res$node%in% c('traditional_food' ,'knowledge_transmission','benefit_local_community', 'societal_importance'),])+
  geom_col(aes(x=levels, y=value, fill=id_I), position='dodge')+
  facet_wrap(~node)

ggplot(data = store.res[store.res$node%in% c('identity' ,'home','outdoor_benefit', 'individual_importance'),])+
  geom_col(aes(x=levels, y=value, fill=id_I), position='dodge')+
  facet_wrap(~node)


ggplot(data = store.res[store.res$node%in% c('economic_importance','societal_importance', 'individual_importance'),])+
  geom_col(aes(x=levels, y=value, fill=id_I), position='dodge')+
  facet_wrap(~node)


store.res$id_I=factor(store.res$id_I, levels=c('Fishing guide', 'coastal', 'pelagic trawler', 'recreational','archipelago'))
store.res$levels=str_remove(store.res$levels, '_important')
store.res$node=str_remove(store.res$node, '_importance')
p=ggplot(data = store.res[store.res$node%in% c('economic','societal', 'individual'),])+
  geom_col(aes(x=levels, y=value, fill=node), position='dodge')+
  facet_wrap(~id_I)+
  theme_bw()+
  labs(fill='Importance type')+
  xlab('How important is it?')+
  ylab('Frequency')
library(lemon)
shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}


p=shift_legend2(p)
ggsave(plot=p, paste0('results/images/importances.jpeg'), width = 20, height = 
         12, units='cm')
