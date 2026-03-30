remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())

# load ####
x.mod=list.files(path='results/iterations')
x.st=x.st.2=x.st.3=NULL
for(xx in 1:length(x.mod)){
  cat(xx)
  
  net=read.net(paste0('results/iterations/',x.mod[xx]))
  
  # S2
  array.var=net[['extreme_event']]$prob
  ev.list=names(array.var[array.var>0])
  ev.store=NULL
  for(i in 1:length(ev.list)){
  i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk',
                              'go_fishing', 'strategy_to_change'), 
                evidence = list(extreme_event = ev.list[i],
                                stock_status='status_quo',
                                aware_of_event='yes'), n=10^5, method='lw')
  i.res$extreme_event=ev.list[i]
  i.res=i.res%>%
    pivot_longer(-extreme_event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(extreme_event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
  }
  ev.store$iter=xx
  x.st=rbind(x.st, ev.store)
  
  # S3
  array.var=net[['fishing_style']]$prob
  fi.list=names(array.var[array.var>0])
  fi.store=NULL
  for(i in 1:length(fi.list)){
    i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk', 
                                'strategy_to_change', 'go_fishing', 'substitution_capacity', 'economic_buffers', 'societal_importance', 'individual_importance'), 
                 evidence = list(fishing_style = fi.list[i], stock_status='status_quo',
                                 aware_of_event='yes'), n=10^5, method='lw')
    i.res$fishing_style=fi.list[i]
    i.res=i.res%>%
      pivot_longer(-fishing_style, names_to = 'node', values_to = 'est')%>%
      dplyr::group_by(fishing_style, node,est)%>%
      tally()%>%
      dplyr::group_by(fishing_style, node)%>%
      dplyr::mutate(prob=n/sum(n))
    fi.store=rbind(fi.store, i.res)
    
  }
  fi.store$iter=xx
  x.st.2=rbind(x.st.2, fi.store)
  
  
  # s4
  fi.ev.store=NULL
  fi.ev.list=expand.grid(ev.list, fi.list)
  
  for(i in 1:nrow(fi.ev.list)){
    
    i.res=cpdist(net, nodes = c('fishing_style', 'extreme_event', 'economic_risk', 'societal_risk','individual_risk', 
                                'strategy_to_change', 'go_fishing', 'substitution_capacity', 'economic_buffers', 'societal_importance', 'individual_importance'), 
                 evidence = list(fishing_style = fi.ev.list[i,]$Var2, 
                                 extreme_event = fi.ev.list[i,]$Var1,
                                 stock_status='status_quo',
                                 aware_of_event='yes'), n=10^5, method='lw')
    
    
    i.res=i.res%>%
      pivot_longer(-c(fishing_style, extreme_event), names_to = 'node', values_to = 'est')%>%
      dplyr::group_by(fishing_style, extreme_event, node,est)%>%
      tally()%>%
      dplyr::group_by(fishing_style, node)%>%
      dplyr::mutate(prob=n/sum(n))
    fi.ev.store=rbind(fi.ev.store, i.res)
    
  }
  fi.ev.store$iter=xx
  x.st.3=rbind(x.st.3, fi.ev.store)
  
}

rq1=read_csv("results/scenarios/riskRQ1.csv")
rq1=rq1[rq1$est=='high',]
rq2=read_csv("results/scenarios/riskRQ2.csv")
rq2=rq2[rq2$est=='high',]

p.a=x.st%>%
  dplyr::filter(est=='high')%>%
  ggplot()+
  xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  facet_grid( rows=vars(extreme_event), scales='free_y')+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ));p.a

x.st.2%>%
  #dplyr::filter(est=='high')%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, node, iter)%>%
  #dplyr::summarise(risk=sum(prob.2))%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::filter( node %in% c('strategy_to_change'))%>%
  dplyr::filter(est!='not_relevant')%>%
  ggplot()+
  geom_density(aes(x=prob, fill=est), alpha=0.5)+
  xlab('Probability of High risk')+
  facet_grid( rows=vars(fishing_style), scales='free_y')
  
scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ));p.c


po1=ggpubr::ggarrange(p.a,p.c, common.legend = T, ncol=2)
ggsave(plot=po1, 'results/scenarios/s4_c.png', width = 220, height = 120, units='mm', dpi=500)


x.st.3%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  dplyr::summarise(prob=sum(prob.2))%>%
  dplyr::filter(prob>0)%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  #facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free')+
  facet_wrap(~fishing_style+extreme_event, scales='free_y')+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ))


pz1=x.st.3%>%
  #dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  #dplyr::summarise(prob=sum(prob.2))%>%
  #dplyr::filter(prob>0)%>%
  dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  #facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free')+
  facet_wrap(~extreme_event+fishing_style, scales='free_y')+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ))

ggsave(plot=pz1, 'results/scenarios/risk_comb.png', width = 250, height = 200, units='mm', dpi=500)


pz2=x.st.3%>%
  dplyr::filter(node %in% c('strategy_to_change'))%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  #dplyr::summarise(prob=sum(prob.2))%>%
  #dplyr::filter(prob>0)%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=est), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  #facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free')+
  facet_wrap(~fishing_style+extreme_event, scales='free_y')
ggsave(plot=pz2, 'results/scenarios/strategy_comb.png', width = 250, height = 200, units='mm', dpi=500)

pz3=x.st.3%>%
  dplyr::filter(node %in% c('go_fishing'))%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  #dplyr::summarise(prob=sum(prob.2))%>%
  #dplyr::filter(prob>0)%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=est), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  #facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free')+
  facet_wrap(~fishing_style+extreme_event, scales='free_y')
ggsave(plot=pz3, 'results/scenarios/fishing_comb.png', width = 250, height = 200, units='mm', dpi=500)



x.st.3%>%
  dplyr::filter(node %in% c('individual_importance'))%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  #dplyr::summarise(prob=sum(prob.2))%>%
  #dplyr::filter(prob>0)%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=est), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  #facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free')+
  facet_wrap(~fishing_style+extreme_event, scales='free_y')














x.st.2%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, node, iter)%>%
  #dplyr::summarise(risk=sum(prob.2))%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::filter( node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  xlim(c(0,1))+
  geom_vline(data=rq2, aes(xintercept =prob, color=node))+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  xlab('Probability of High risk')+
  facet_grid( rows=vars(fishing_style), scales='free_y')+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ));p.c




p.b=x.st%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::select(-n)%>%
  ungroup()%>%
  dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  dplyr::group_by(extreme_event, node, iter)%>%
  dplyr::summarise(risk=sum(prob.2))%>%
  pivot_wider(names_from = node, values_from = risk)%>%
  ggplot()+
  xlim(c(0,1))+
  geom_point(aes(x=economic_risk, y=individual_risk, color=extreme_event), alpha=0.5)+
  facet_wrap(~extreme_event,  ncol=1)+
  scale_fill_manual(values=c('blue', 'white', 'black' ))






p.d=x.st.2%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::select(-n)%>%
  ungroup()%>%
  dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  dplyr::group_by(fishing_style, node, iter)%>%
  dplyr::summarise(risk=sum(prob.2))%>%
  pivot_wider(names_from = node, values_from = risk)%>%
  ggplot()+
  xlim(c(0,1))+
  ylim(c(0,1))+
  geom_point(aes(x=economic_risk, y=individual_risk, color=fishing_style), alpha=0.5)+
  geom_density_2d(aes(x=economic_risk, y=individual_risk, color=fishing_style), alpha=0.5)+
  #facet_wrap(~fishing_style, scales='free_y', ncol=1)+
  scale_fill_manual(values=c('blue', 'white', 'black' ));p.d

p.d=x.st.2%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::select(-n)%>%
  ungroup()%>%
  dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  dplyr::group_by(fishing_style, node, iter)%>%
  dplyr::summarise(risk=sum(prob.2))%>%
  pivot_wider(names_from = node, values_from = risk)%>%
  ggplot()+
  #xlim(c(0,1))+
  #ylim(c(0,1))+
  geom_point(aes(x=economic_risk, y=individual_risk), alpha=0.5)+
  geom_smooth(aes(x=economic_risk, y=individual_risk), alpha=0.5, method='lm')+
  facet_wrap(~fishing_style, scales='free', ncol=1)+
  scale_fill_manual(values=c('blue', 'white', 'black' ));p.d

?geom_smooth

po1=ggpubr::ggarrange(p.a,p.c, common.legend = T, ncol=2)
ggsave(plot=po1, 'results/scenarios/s4_c.png', width = 220, height = 120, units='mm', dpi=500)

po2=ggpubr::ggarrange(p.b,p.d, common.legend = T, ncol=2)

ggpubr::ggarrange(po1,po2, ncol=1)


