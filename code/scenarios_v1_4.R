remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())

# load ####
net=read.net('data/networks/BEWARE_learn_pt2.net', debug = T)



cpdist(net, nodes = c('go_out', 'economic_risk', 'societal_risk','individual_risk'), 
       evidence = (event == 'hws'))



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
array.var=net.s1[['event']]$prob
ev.list=names(array.var[array.var>0])
ev.store=NULL
for(i in 1:length(ev.list)){
  i.res=cpdist(net.s1, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
         evidence = (event == ev.list[i])&
            stock_status=='status_quo')
  i.res$event=ev.list[i]
  i.res=i.res%>%
    pivot_longer(-event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(event, node,est)%>%
    tally()%>%
    dplyr::group_by(event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}

x.rel=ev.store%>%
  dplyr::filter(est=='not_relevant')%>%
  ungroup()%>%
  distinct(event, notrel=prob)


p.f1=ev.store%>%
  #dplyr::filter(est!='not_relevant')%>%
  left_join(x.rel)%>%
  dplyr::mutate(est=ifelse(as.character(est)== 'not_relevant', 'Not Relevant', as.character(est)))%>%
  #dplyr::filter(est!='not_relevant')%>%
  dplyr::mutate(est=factor(est, levels=c('Not Relevant', 'Low', 'Medium',  'High')))%>%
  replace(is.na(.),0)%>%
  #dplyr::mutate(prob=prob*(1-notrel))%>%
  ggplot(aes(x=event, y=prob, fill=est))+
  geom_col(position='fill', color='black')+
  facet_wrap(~node)+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  theme(legend.position = 'bottom', strip.text = element_blank())+
  labs(fill='Risk')+
  ylab('Frequency')+
  xlab('MEW');p.f1
#ggsave(plot=p.f1, 'results/scenarios/event.png', width = 18, height = 8, units='cm', dpi=300)


## S2 ####
array.var=net.s1[['fishing_style']]$prob
fi.list=names(array.var[array.var>0])
ev.store=NULL
for(i in 1:length(fi.list)){
  i.res=cpdist(net.s1, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
               evidence = (fishing_style == fi.list[i])& stock_status=='status_quo')
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
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                           ifelse(fishing_style=='archipelago' ,'arc',
                                                                  ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  dplyr::mutate(est=ifelse(as.character(est)== 'not_relevant', 'Not Relevant', as.character(est)))%>%
  #dplyr::filter(est!='not_relevant')%>%
  dplyr::mutate(est=factor(est, levels=c('Not Relevant', 'Low', 'Medium',  'High')))%>%
  ggplot(aes(x=fishing_style, y=prob, fill=est))+
  geom_col(position='fill', color='black')+
  facet_wrap(~node)+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  #scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom', strip.text = element_blank())+
  labs(fill='Risk')+
  ylab('Frequency')+
  xlab('Fishing style');p.f2
#ggsave(plot=p.f2, 'results/scenarios/fstyle.png', width = 18, height = 8, units='cm', dpi=300)

p.f3=ggpubr::ggarrange(p.f1,p.f2, nrow=2, common.legend = T, legend = 'bottom', labels=c('a)','b)'))
ggsave(plot=p.f3, 'results/scenarios/arrange_event_fstyle.png', width = 180, height = 120, units='mm', dpi=500)


## S3 ####
array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s2=net.s1
net.s2[['stock_status']]=array.var
ma.list=names(net[['stock_status']]$prob)
scen.grid2=expand.grid(fishing_style=fi.list, event=ev.list, stock_status=ma.list)
ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s2, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
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

excl=ev.store[ev.store$est =='not_relevant',]%>%
  ungroup()%>%
  distinct(fishing_style, event)
plot.event=ev.store[ev.store$est!='not_relevant',]

plot.event=plot.event%>%
  dplyr::group_by(event, fishing_style, stock_status, node)%>%
  dplyr::mutate(prob=prob/sum(prob))


base.grid=expand.grid(fishing_style=unique(plot.event$fishing_style), 
                      event=unique(plot.event$event), 
                      stock_status=unique(plot.event$stock_status),
                      node=unique(plot.event$node))

plot.event=plot.event%>%
  dplyr::filter(est=='High')%>%
  right_join(base.grid)%>%
  dplyr::mutate(est='High')%>%
  replace(is.na(.),0)

plot.event[which(paste0(plot.event$fishing_style, plot.event$event) %in% paste0(excl$fishing_style, excl$event)),]$prob=-999
plot.event[plot.event$prob<0,]$prob=NA
ev.val=mean(plot.event[!is.na(plot.event$prob),]$prob)

unique(plot.event$stock_status)

p.f4=plot.event%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='archipelago' ,'arc',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  #dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(stock_status=paste('Stock status:', stock_status))%>%
  dplyr::mutate(stock_status=factor(stock_status, levels=paste('Stock status:',c('worst', 'status_quo','better'))))%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile(color='black')+
  facet_grid(cols=vars(node), rows=vars(stock_status))+
  scale_fill_gradient2(midpoint=0.5, low='lightgreen', high='red', mid='yellow',na.value = "grey98")+
  theme(legend.position = 'bottom')+
  labs(fill='Probability of High Risk')+
  ylab('MEW')+
  xlab('Fishing Style');p.f4


ggsave(plot=p.f4, 'results/scenarios/mgmt.png', width = 18, height = 12, units='cm', dpi=300)


plot.event%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = stock_status, values_from = prob)%>%
  dplyr::mutate(worst=worst-status_quo, 
                better=better-status_quo)%>%
  pivot_longer(cols = c(worst, status_quo, better), values_to = 'prob', names_to = 'stock_status')%>%
  dplyr::filter(stock_status!='status_quo')%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='archipelago' ,'arc',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  #dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(stock_status=paste('Stock status:', stock_status))%>%
  dplyr::mutate(stock_status=factor(stock_status, levels=paste('Stock status:',c('worst', 'status_quo','better'))))%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile(color='black')+
  facet_grid(cols=vars(node), rows=vars(stock_status))+
  scale_fill_gradient2(midpoint=0, low='lightgreen', high='red', mid='white',na.value = "grey98")+
  theme(legend.position = 'bottom')+
  labs(fill='Probability of High Risk')+
  ylab('MEW')+
  xlab('Fishing Style')


## s3bis ####
array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s2=net.s1
net.s2[['stock_status']]=array.var
ma.list=names(net[['stock_status']]$prob)
scen.grid2=expand.grid(fishing_style=fi.list, event=ev.list, stock_status=ma.list)
ev.store=NULL
ev.supstore=NULL
for (j in 1:100){
  cat(j)
  for(i in 1:nrow(scen.grid2)){
    
    i.res=cpdist(net.s2, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
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
  ev.store$iteration=j
  ev.supstore=rbind(ev.supstore,ev.store)
}

plot.event=ev.supstore%>%
  dplyr::group_by(event, fishing_style, stock_status,est, node)%>%
  dplyr::summarise(prob.mu=mean(prob),prob.sd=sd(prob))


plot.event%>%
  dplyr::filter(est=='High')%>%
  dplyr::select(-prob.sd)%>%
  pivot_wider(names_from = stock_status, values_from = prob.mu)%>%
  dplyr::mutate(worst=worst-status_quo, 
                better=better-status_quo)%>%
  pivot_longer(cols = c(worst, status_quo, better), values_to = 'prob', names_to = 'stock_status')%>%
  dplyr::filter(stock_status!='status_quo')%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='archipelago' ,'arc',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  #dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(stock_status=paste('Stock status:', stock_status))%>%
  dplyr::mutate(stock_status=factor(stock_status, levels=paste('Stock status:',c('worst', 'status_quo','better'))))%>%
  ggplot(aes(x=fishing_style, y=event, fill=prob))+
  geom_tile(color='black')+
  facet_grid(cols=vars(node), rows=vars(stock_status))+
  scale_fill_gradient2(midpoint=0, low='lightgreen', high='red', mid='white',na.value = "grey98")+
  theme(legend.position = 'bottom')+
  labs(fill='Probability of High Risk')+
  ylab('MEW')+
  xlab('Fishing Style')



plot.event%>%
  dplyr::filter(est=='High', node!='economic_risk')%>%
  dplyr::select(-prob.sd)%>%
  pivot_wider(names_from = stock_status, values_from = prob.mu)%>%
  dplyr::mutate(worst=worst-status_quo, 
                better=better-status_quo)%>%
  pivot_longer(cols = c(worst, status_quo, better), values_to = 'prob', names_to = 'stock_status')%>%
  dplyr::filter(stock_status!='status_quo')%>%
  dplyr::mutate(fishing_style=ifelse(fishing_style=='recreational',
                                     'rcf', ifelse(fishing_style=='coastal', 'ssf',
                                                   ifelse(fishing_style=='coastal_salmon' , 'sal' ,
                                                          ifelse(fishing_style=='archipelago' ,'arc',
                                                                 ifelse(fishing_style=='trawler' ,'trw','fgu'))))))%>%
  #dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(stock_status=paste('Stock status:', stock_status))%>%
  dplyr::mutate(stock_status=factor(stock_status, levels=paste('Stock status:',c('worst', 'status_quo','better'))))%>%
  ggplot(aes(x=prob, y=event, color=stock_status))+
  geom_point()+
  geom_vline(xintercept=0)+
  facet_grid(cols=vars(fishing_style), rows=vars(node))+
  xlim(c(-0.3,0.3))





## S 4 ####
net.s5=net.s1

array.var=net[['management']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.5,0.5)
net.s5[['management']]=array.var
ma.list=names(net[['management']]$prob)

array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=c(0.33,0.33,0.34)
net.s5[['stock_status']]=array.var
ss.list=names(net[['stock_status']]$prob)

scen.grid2=expand.grid(stock_status=ss.list, management=ma.list)

ev.store=NULL
for(i in 1:nrow(scen.grid2)){
  
  i.res=cpdist(net.s5, nodes = c('economic_risk', 'societal_risk','individual_risk'), 
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

ev.store=ev.store[ev.store$est!='not_relevant',]
ev.store=ev.store%>%
  dplyr::group_by(stock_status, management, node)%>%
  dplyr::mutate(prob=prob/sum(prob))

ev.prob=ev.store%>%
  dplyr::filter(est=='High')
ev.val=mean(ev.prob$prob)

p.f5=ev.store%>%
  dplyr::filter(est!='not_relevant')%>%
  #dplyr::filter(management%in%c('status_quo','flexible'))%>%
  #dplyr::mutate(management=ifelse(management=='status_quo', 'BAU', 'FLEX'))%>%
  dplyr::mutate(management=paste('Manag:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Manag:',c('rigid', 'BAU','FLEX'))))%>%
ggplot(aes(x=management, y=prob, fill=est))+
  geom_col(position='fill')+
  facet_grid(rows=vars(node), cols=vars(stock_status))+
  scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('Fishing Style');p.f5

ggsave(plot=p.f5, 'results/scenarios/mgmt_ss.png', width = 18, height = 12, units='cm', dpi=300)

p.f5=ev.store%>%
  dplyr::filter(est=='High')%>%
  dplyr::mutate(management=paste('Manag:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Manag:',c('rigid', 'BAU','FLEX'))))%>%
  ggplot(aes(x=management, y=stock_status, fill=prob))+
  geom_tile()+
  facet_grid(rows=vars(node))+
  #scale_color_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  scale_fill_gradient2(midpoint=ev.val, low='lightgreen', high='red', mid='yellow',na.value = "lightgreen")+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('Fishing Style');p.f5

ggsave(plot=p.f5, 'results/scenarios/mgmt_ss_opt2.png', width = 18, height = 12, units='cm', dpi=300)

p.f5=ev.store%>%
  dplyr::filter(est=='High')%>%
  dplyr::mutate(management=paste('Manag:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Manag:',c('rigid', 'BAU','FLEX'))))%>%
  ggplot(aes(x=management, y=1, fill=prob))+
  geom_tile()+
  facet_grid(rows=vars(node), cols=vars(stock_status))+
  #scale_color_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  scale_fill_gradient2(midpoint=ev.val, low='lightgreen', high='red', mid='yellow',na.value = "lightgreen")+
  labs(fill='Risk Level')+
  ylab('Probability')+
  xlab('Fishing Style');p.f5

ggsave(plot=p.f5, 'results/scenarios/mgmt_ss_opt3.png', width = 18, height = 12, units='cm', dpi=300)




ev.store%>%
  dplyr::filter(est=='High')%>%
  dplyr::mutate(management=paste('Manag:', management))%>%
  dplyr::mutate(stock_status=paste('Stock:', stock_status))%>%
  dplyr::mutate(management=factor(management, levels=paste('Manag:',c('rigid', 'BAU','FLEX'))))%>%
  ggplot(aes(x=management, y=stock_status, color=prob, size=exp(prob)))+
  geom_point()+
  facet_grid(rows=vars(node))+
  #scale_color_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk Level')+
  scale_color_gradient2(midpoint=ev.val, low='lightgreen', high='red', mid='yellow',na.value = "lightgreen")+
  ylab('Probability')+
  xlab('Fishing Style') ## work on this with sizes and jitter on the x axis











