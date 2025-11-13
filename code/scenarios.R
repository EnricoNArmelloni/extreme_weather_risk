remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())

# load ####
net=read.net('data/networks/BN_p1_v2_learn_pt2.net', debug = T)


cpdist(net, nodes = c('go_out', 'economic_risk', 'societal_risk','individual_risk'), 
       evidence = (event == 'hws'))



### scenario settings
net.s1=net

# event
array.var=net.s1[['event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/(xdim-2), (xdim))
array.var[c(3,7)]=0
net.s1[['event']]=array.var

### fisher
array.var=net[['fishing_style']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/(xdim-2), xdim)
array.var[c(1,7)]=0
net.s1[['fishing_style']]=array.var

# management
array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0,1,0)
net.s1[['management']]=array.var

# stock status
array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0,1,0)
net.s1[['stock_status']]=array.var

##
array.var=net.s1[['event']]$prob
ev.list=names(array.var[array.var>0])
ev.store=NULL
for(i in 1:length(ev.list)){
  i.res=cpdist(net.s1, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
         evidence = (event == ev.list[i]))
  i.res$event=ev.list[i]
  i.res=i.res%>%
    pivot_longer(-event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(event, node,est)%>%
    tally()%>%
    dplyr::group_by(event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}

p.f1=ev.store%>%
  ggplot(aes(x=event, y=prob, fill=est))+
  geom_col()+
  facet_wrap(~node)+
  scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('MEW')
ggsave(plot=p.f1, 'results/scenarios/event.png', width = 18, height = 8, units='cm', dpi=300)


##
array.var=net.s1[['fishing_style']]$prob
fi.list=names(array.var[array.var>0])
ev.store=NULL
for(i in 1:length(fi.list)){
  i.res=cpdist(net.s1, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == fi.list[i]))
  i.res$fishing_style=fi.list[i]
  i.res=i.res%>%
    pivot_longer(-fishing_style, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}
p.f2=ev.store%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational_summer',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                           ifelse(fishing_style=='archipelago' ,'arc',
                                                                  ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  ggplot(aes(x=fishing_style, y=prob, fill=est))+
  geom_col()+
  facet_wrap(~node)+
  scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('Fishing Style')
ggsave(plot=p.f2, 'results/scenarios/fstyle.png', width = 18, height = 8, units='cm', dpi=300)

p.f3=ggpubr::ggarrange(p.f1,p.f2, nrow=2, common.legend = T, legend = 'bottom')
ggsave(plot=p.f3, 'results/scenarios/arrange_event_fstyle.png', width = 18, height = 12, units='cm', dpi=300)


unique(ev.store$fishing_style)


##
scen.grid1=expand.grid(fishing_style=fi.list, event=ev.list)
ev.store=NULL
for(i in 1:nrow(scen.grid1)){
  
  i.res=cpdist(net.s1, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == as.character(scen.grid1[i,]$fishing_style) & event==as.character(scen.grid1[i,]$event)))
  
  i.res$fishing_style=as.character(scen.grid1[i,]$fishing_style)
  i.res$event=as.character(scen.grid1[i,]$event)
  
  i.res=i.res%>%
    pivot_longer(-c(fishing_style, event), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, event,node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}
ev.store%>%
  dplyr::filter(est=='High')%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile()+
  facet_wrap(~node)+
  scale_fill_gradient2(midpoint=0.5, low='lightgreen', high='red', mid='yellow')+
  theme(legend.position = 'bottom')+
  labs(fill='Probability of High Risk')+
  ylab('MEW Type')+
  xlab('Fishing Style')


## scen grid 2
array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s2=net.s1
net.s2[['management']]=array.var
ma.list=names(net[['management']]$prob)
scen.grid2=expand.grid(fishing_style=fi.list, event=ev.list, management=ma.list)
ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s2, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == as.character(scen.grid2[i,]$fishing_style) &
                             event==as.character(scen.grid2[i,]$event) &
                             management==as.character(scen.grid2[i,]$management)))
  
  i.res$fishing_style=as.character(scen.grid2[i,]$fishing_style)
  i.res$event=as.character(scen.grid2[i,]$event)
  i.res$management=as.character(scen.grid2[i,]$management)
  
  i.res=i.res%>%
    pivot_longer(-c(fishing_style, event, management), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, event, management, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, event, management, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}

p.f4=ev.store%>%
  dplyr::filter(est=='High', management!='rigid')%>%
  na.omit()%>%
  dplyr::select(fishing_style, event, management,prob)%>%
  pivot_wider(names_from = event, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(-c(node, fishing_style, management), names_to = 'event', values_to = 'prob')%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational_summer',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='archipelago' ,'arc',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(management=paste('Management:', management))%>%
  dplyr::mutate(management=factor(management, levels=paste('Management:',c('rigid', 'BAU','FLEX'))))%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile()+
  facet_grid(cols=vars(node), rows=vars(management))+
  scale_fill_gradient2(midpoint=0.5, low='lightgreen', high='red', mid='yellow',na.value = "lightgreen")+
  theme(legend.position = 'bottom')+
  labs(fill='Probability of High Risk')+
  ylab('MEW')+
  xlab('Fishing Style');p.f4

ggsave(plot=p.f4, 'results/scenarios/mgmt.png', width = 18, height = 12, units='cm', dpi=300)


## scen grid 3
array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s3=net.s1
net.s3[['stock_status']]=array.var

ma.list=names(net[['stock_status']]$prob)
scen.grid2=expand.grid(fishing_style=fi.list, event=ev.list, stock_status=ma.list)
ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s3, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == as.character(scen.grid2[i,]$fishing_style) &
                             event==as.character(scen.grid2[i,]$event) &
                             stock_status==as.character(scen.grid2[i,]$stock_status)))
  
  i.res$fishing_style=as.character(scen.grid2[i,]$fishing_style)
  i.res$event=as.character(scen.grid2[i,]$event)
  i.res$stock_status=as.character(scen.grid2[i,]$stock_status)
  
  i.res=i.res%>%
    pivot_longer(-c(fishing_style, event, stock_status), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, event, stock_status, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, event, stock_status, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}
ev.store%>%
  dplyr::filter(est=='High')%>%
  na.omit()%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile()+
  facet_grid(cols=vars(node), rows=vars(stock_status))+
  scale_fill_gradient2(midpoint=0.5, low='lightgreen', high='red', mid='yellow',na.value = "lightgreen")




### super multi
net.s4=net.s1

array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['management']]=array.var
ma.list=names(net[['management']]$prob)

array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['stock_status']]=array.var
ss.list=names(net[['stock_status']]$prob)

scen.grid2=expand.grid(fishing_style=fi.list, event=ev.list, stock_status=ss.list, management=ma.list)

ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s4, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == as.character(scen.grid2[i,]$fishing_style) &
                             event==as.character(scen.grid2[i,]$event) &
                             stock_status==as.character(scen.grid2[i,]$stock_status)&
                             management==as.character(scen.grid2[i,]$management)))
  
  i.res$fishing_style=as.character(scen.grid2[i,]$fishing_style)
  i.res$event=as.character(scen.grid2[i,]$event)
  i.res$stock_status=as.character(scen.grid2[i,]$stock_status)
  i.res$management=as.character(scen.grid2[i,]$management)
  
  i.res=i.res%>%
    pivot_longer(-c(fishing_style, event, stock_status, management), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, event, stock_status, management, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, event, stock_status, management, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}
ev.store%>%
  dplyr::filter(management%in%c('status_quo','flexible'))%>%
  dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(management=paste('Management:', management))%>%
  dplyr::mutate(management=factor(management, levels=paste('Management:',c('rigid', 'BAU','FLEX'))))%>%
  dplyr::filter(est=='High')%>%
  na.omit()%>%
  ggplot(aes(x=event, y=prob, color=stock_status, pch=node))+
  geom_point(size=5)+
  facet_grid(rows=vars(fishing_style), cols=vars(management))+
  scale_color_manual(values=c('lightgreen', 'yellow', 'red'))


### super multi
net.s4=net.s1

array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['management']]=array.var
ma.list=names(net[['management']]$prob)

array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['stock_status']]=array.var
ss.list=names(net[['stock_status']]$prob)

scen.grid2=expand.grid( event=ev.list, stock_status=ss.list, management=ma.list)

ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s4, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (
                             event==as.character(scen.grid2[i,]$event) &
                             stock_status==as.character(scen.grid2[i,]$stock_status)&
                             management==as.character(scen.grid2[i,]$management)))
  
  i.res$event=as.character(scen.grid2[i,]$event)
  i.res$stock_status=as.character(scen.grid2[i,]$stock_status)
  i.res$management=as.character(scen.grid2[i,]$management)
  
  i.res=i.res%>%
    pivot_longer(-c(event, stock_status, management), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by( event, stock_status, management, node,est)%>%
    tally()%>%
    dplyr::group_by( event, stock_status, management, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}
ev.store%>%
  dplyr::filter(management%in%c('status_quo','flexible'))%>%
  dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(management=paste('Management:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Management:',c('rigid', 'BAU','FLEX'))))%>%
  dplyr::filter(est=='High')%>%
  na.omit()%>%
  ggplot(aes(x=event, y=prob, fill=management))+
  geom_col(position='dodge')+
  facet_grid(rows=vars(node), cols=vars(stock_status))
  geom_hline(yintercept=0.5, linetype=2)


### super multi
net.s5=net.s1

array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['management']]=array.var
ma.list=names(net[['management']]$prob)

array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s4[['stock_status']]=array.var
ss.list=names(net[['stock_status']]$prob)

scen.grid2=expand.grid(stock_status=ss.list, management=ma.list)

ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s4, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (  stock_status==as.character(scen.grid2[i,]$stock_status)&
                             management==as.character(scen.grid2[i,]$management)))
  
 
  i.res$stock_status=as.character(scen.grid2[i,]$stock_status)
  i.res$management=as.character(scen.grid2[i,]$management)
  
  i.res=i.res%>%
    pivot_longer(-c( stock_status, management), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by( stock_status, management, node,est)%>%
    tally()%>%
    dplyr::group_by( stock_status, management, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}

p.f5=ev.store%>%
  dplyr::filter(management%in%c('status_quo','flexible'))%>%
  dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(management=paste('Manag:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Manag:',c('rigid', 'BAU','FLEX'))))%>%
ggplot(aes(x=management, y=prob, fill=est))+
  geom_col()+
  facet_grid(rows=vars(node), cols=vars(stock_status))+
  scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('Fishing Style')

ggsave(plot=p.f5, 'results/scenarios/mgmt_ss.png', width = 18, height = 12, units='cm', dpi=300)




