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
  
  return(list(report.cpt=cpt.wide, bn.cpt=E.prob))
}

# data from interviews
int.dat=read_csv("data/coding_report_unc.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

# Human community - Variables ####
net.h=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/BN_lower_end_2.net', debug = T)
graphviz.plot(net.h, layout = "dot", fontsize = 50)
graphviz.chart(net.h)

f.styles=unique(int.dat[int.dat$id_q==0,]$text)

# check missing data
x.nodes=nodes(net.h)
x.nodes=x.nodes[x.nodes %in% int.dat$short_description]


hc.dat=int.dat[int.dat$short_description %in% c('fishing_style', x.nodes),]
## temporary solution: this needs to be fixed
hc.dat[hc.dat$uncertainty==-99.8 & !is.na(hc.dat$uncertainty),]$uncertainty=3/5 # this is the archipelago fisherman that does not know what to say
hc.dat[hc.dat$value==999 & !is.na(hc.dat$uncertainty),]$value=2 # recreational fisherman question about benefit to community. Need better interpretation

resps=unique(hc.dat$id_I)
hc.dat$fishing_style=NA
for(i in 1:length(resps)){
  hc.dat[hc.dat$id_I==resps[i],]$fishing_style=hc.dat[hc.dat$id_I==resps[i],]$text[1]  
}

hc.light=hc.dat[,c('fishing_style', 'short_description','value', 'uncertainty')]
hc.light$text
hc.light=hc.light%>%
  dplyr::group_by(fishing_style, short_description)%>%
  dplyr::summarise(value=mean(value),uncertainty=mean(uncertainty))

store.res=NULL
for(xx in 1:length(f.styles)){
  
  dat=hc.light[hc.light$fishing_style==f.styles[xx],]
  
  # root nodes
  # here is very important that the node definition in GeNie makes sense. Especially the order.- This needs to be revised
  x.nodes=nodes(net.h)
  x.nodes=x.nodes[x.nodes %in% dat$short_description]
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
  
  i.node='professional'
  i.lev=names(net.h[[i.node]]$prob)
  x.pro=ifelse(f.styles[xx] %in% c('recreational', 'archipelago'),1,5)
  i.cpt=answer.to.cpt_vH(x.ans=x.pro, 
                         unc=0.001,
                         dim.name = i.node,
                         div.factor = 5,
                         dim.labs = i.lev)
  net.h[[i.node]]=i.cpt
  
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
  
  ### let's checks what have we been doinnn
  jpeg(paste0('results/images/CPT_id', f.styles[xx], '.jpeg'), width=35, height=10, units='cm', res=500)
  graphviz.chart(net.h, scale=c(2,2))
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
    
}

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
