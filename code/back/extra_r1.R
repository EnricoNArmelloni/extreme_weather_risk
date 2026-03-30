# scope: to create conditional probabilities for variables that are excluded from the final version of the BN. It follows the same procedure adopted in other scripts.

remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(gRain)
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())
net=read.net('data/networks/BEWARE_r1_learn_pt2.net', debug = T)

## awareness
array.var=net[['aware']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(1,0)
net[['aware']]=array.var


### scenario settings
net.s1=net

# event
array.var=net.s1[['event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/(xdim), (xdim))
#array.var[c(3,7)]=0
net.s1[['event']]=array.var

### fisher
array.var=net[['fishing_style']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/(xdim), xdim)
#array.var[c(1,7)]=0
net.s1[['fishing_style']]=array.var

# stock status
array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0,1,0)
net.s1[['stock_status']]=array.var

## S1 ####
array.var=net.s1[['fishing_style']]$prob
fi.list=names(array.var[array.var>0])
scen.grid2=expand.grid(fishing_style=fi.list)
ev.store=NULL
i=1
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s1, nodes = c('strategy_to_change', 
                                 'additional_mitigation','substitution_capacity',
                                 'go_fishing', 
                                 'economic_risk','societal_risk','individual_risk'), 
               evidence = list(fishing_style = as.character(scen.grid2[i,])), 
               n=10^5, method='lw')
  
  i.res$fishing_style=as.character(scen.grid2[i,])
  i.res=i.res%>%
    pivot_longer(-c(fishing_style), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style,  node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
}

strat.store=ev.store[ev.store$node=='strategy_to_change',]
p2=strat.store[c('fishing_style','est','prob')]%>%
  pivot_wider(names_from = est, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(-fishing_style, names_to = 'est', values_to = 'prob')

strat.plot=ev.store[ev.store$node=='strategy_to_change' ,]
strat.plot$est=factor(strat.plot$est, levels=c('adapt', 'react','cope','not_relevant'))

pl.ada2=strat.plot%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='small_scale', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='household' ,'hhf',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  ggplot()+
  geom_col(aes(x=fishing_style, y=prob, fill=est), position='fill')+
  #facet_wrap(~node)+
  scale_fill_viridis_d()+
  ylab('')+
  labs(fill='strategy_to_change')+
  scale_y_reverse()+
  scale_fill_manual(values=c('lightgreen','yellow','red' ,  "grey98"  ))+
  xlab('Probability')+
  theme(legend.position = 'bottom');pl.ada2



obj.store=ev.store[ev.store$node%in%c('substitution_capacity'),]
#obj.store$est=ifelse(obj.store$est=='progress_rate','no','yes')
obj.store$node='substitution_capacity'

obj.store=obj.store%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(cols=c('no','yes'), names_to = 'est', values_to = 'prob')
obj.store=obj.store[obj.store$est=='yes',]


strat.df=p2%>%
  dplyr::mutate(est=ifelse(est%in%c('react','adapt'),'act',est))%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::group_by(fishing_style, strategy_to_change=est)%>%
  dplyr::summarise(st_prob=sum(prob))%>%
  dplyr::group_by(fishing_style)%>%
  dplyr::mutate(st_prob=st_prob/sum(st_prob))

extr2=left_join(obj.store, strat.df)
library(ggrepel)
pl.kobe=ggplot(data=extr2[extr2$strategy_to_change=='act',])+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=-Inf, ymax=0.25, fill='red')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=-Inf, ymax=0.25, fill='yellow')+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=0.25, ymax=Inf, fill='yellow')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=0.25, ymax=Inf, fill='green')+
  geom_label_repel(aes(x=prob, y=st_prob, label=fishing_style))+
  geom_point(aes(x=prob, y=st_prob))+
  xlim(c(0,1))+
  ylim(c(0,0.5))+
  xlab('Substitution Capacity')+
  ylab('Proactive response')
  
ggsave(plot=pl.kobe, 'results/scenarios/skobe.png', width = 120, height = 120, units='mm', dpi=500)

pl.comb=ggpubr::ggarrange(pl.ada2, pl.kobe, labels=c('a)','b)'))

ggsave(plot=pl.comb, 'results/scenarios/skobe2.png', width = 250, height = 120, units='mm', dpi=500)


# s2
array.var=net.s1[['extreme_event']]$prob
fi.list=names(array.var[array.var>0])
scen.grid2=expand.grid(extreme_event=fi.list)
ev.store=NULL
i=1
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s1, nodes = c('strategy_to_change', 'additional_mitigation','substitution_capacity', 'go_fishing', 'economic_risk','societal_risk','individual_risk'), 
               evidence = list(extreme_event = as.character(scen.grid2[i,])), 
               n=10^5, method='lw')
  
  i.res$extreme_event=as.character(scen.grid2[i,])
  i.res=i.res%>%
    pivot_longer(-c(extreme_event), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(extreme_event,  node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
}

strat.store=ev.store[ev.store$node=='strategy_to_change',]
p2=strat.store[c('extreme_event','est','prob')]%>%
  pivot_wider(names_from = est, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(-extreme_event, names_to = 'est', values_to = 'prob')

strat.plot=ev.store[ev.store$node=='strategy_to_change' ,]
strat.plot$est=factor(strat.plot$est, levels=c('adapt', 'react','cope','not_relevant'))

pl.ada3=strat.plot%>%
  ggplot()+
  geom_col(aes(x=extreme_event, y=prob, fill=est), position='fill')+
  #facet_wrap(~node)+
  scale_fill_viridis_d()+
  ylab('')+
  labs(fill='strategy_to_change')+
  scale_y_reverse()+
  scale_fill_manual(values=c('lightgreen','yellow','red' ,  "grey98"  ))+
  xlab('Probability')+
  theme(legend.position = 'bottom');pl.ada3



obj.store=ev.store[ev.store$node%in%c('substitution_capacity'),]
obj.store$est=ifelse(obj.store$est=='progress_rate','no','yes')
obj.store$node='substitution_capacity'
obj.store=obj.store[obj.store$est=='yes',]

strat.df=p2%>%
  dplyr::mutate(est=ifelse(est%in%c('react','adapt'),'act',est))%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::group_by(event, strategy_to_change=est)%>%
  dplyr::summarise(st_prob=sum(prob))%>%
  dplyr::group_by(event)%>%
  dplyr::mutate(st_prob=st_prob/sum(st_prob))

extr2=left_join(obj.store, strat.df)
library(ggrepel)
pl.kobe2=ggplot(data=extr2[extr2$strategy_to_change=='act',])+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=-Inf, ymax=0.25, fill='red')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=-Inf, ymax=0.25, fill='yellow')+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=0.25, ymax=Inf, fill='yellow')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=0.25, ymax=Inf, fill='green')+
  geom_label_repel(aes(x=prob, y=st_prob, label=event))+
  geom_point(aes(x=prob, y=st_prob))+
  xlim(c(0,1))+
  ylim(c(0,0.5))+
  xlab('Substitution Capacity')+
  ylab('Proactive response')

?ggarrange
pl.comb=ggpubr::ggarrange(pl.ada2, pl.ada3, labels=c('a)','b)'), nrow=1, common.legend = T, legend='bottom');pl.comb
pl.comb2=ggpubr::ggarrange(pl.comb, pl.kobe,labels=c('','c)'), nrow=1, common.legend = T, widths = c(2,1));pl.comb2


ggsave(plot=pl.comb2, 'results/scenarios/pic2.png',  width = 30, height = 10, units='cm', dpi=500)




