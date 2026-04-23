remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())

# load ####
x.mod=list.files(path='data/read_only/networks/iterations')
x.st=x.st.2=x.st.3=NULL
for(xx in 1:length(x.mod)){
  cat(xx)
  
  net=read.net(paste0('data/read_only/networks/iterations/',x.mod[xx]))
  
  # S2
  array.var=net[['extreme_event']]$prob
  ev.list=names(array.var[array.var>0])
  ev.store=NULL
  for(i in 1:length(ev.list)){
  i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk',
                              'go_fishing', 'strategy_to_change'), 
                evidence = list(extreme_event = ev.list[i],
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
                 evidence = list(fishing_style = fi.list[i], 
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
                                 aware_of_event='yes'), n=10^5, method='lw')
    
    
    i.res=i.res%>%
      pivot_longer(-c(fishing_style, extreme_event), names_to = 'node', values_to = 'est')%>%
      dplyr::group_by(fishing_style, extreme_event, node,est)%>%
      tally()
    fi.ev.store=rbind(fi.ev.store, i.res)
    
  }
  fi.ev.store$iter=xx
  x.st.3=rbind(x.st.3, fi.ev.store)
  
}
write.csv(x.st,"results/scenarios/S4_1.csv" )
write.csv(x.st.2,"results/scenarios/S4_2.csv" )
write.csv(x.st.3,"results/scenarios/S4_3.csv" )



###
x.st=read_csv("results/scenarios/S4_1.csv")
x.st.2=read_csv("results/scenarios/S4_2.csv")
x.st.3=read_csv("results/scenarios/S4_3.csv")
rq1=read_csv("results/scenarios/S2.csv")
rq1=rq1[rq1$est=='high',]
rq2=read_csv("results/scenarios/S3.csv")
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

p.c=x.st.2%>%
  dplyr::filter(est=='high')%>%
  #dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  #dplyr::group_by(fishing_style, node, iter)%>%
  #dplyr::summarise(risk=sum(prob.2))%>%
  dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::filter(est=='high')%>%
  ggplot()+
  xlim(c(0,1))+
  xlab('Probability of High risk')+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  xlab('Probability of High risk')+
  facet_grid( rows=vars(fishing_style), scales='free_y')+
  geom_vline(data=rq2[rq2$node %in% c('economic_risk', 'individual_risk', 'societal_risk'), ], aes(xintercept =prob, color=node))+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ))


po1=ggpubr::ggarrange(p.a,p.c, common.legend = T, ncol=2, labels=c('a)', 'b)'))
ggsave(plot=po1, 'results/scenarios/s4_a.png', width = 220, height = 120, units='mm', dpi=500)


p4c=x.st.3%>%
  dplyr::group_by(fishing_style, extreme_event,node, iter)%>%
  dplyr::mutate(prob=n/sum(n))%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::mutate(prob.2=ifelse(est=='high', prob, ifelse(est=='medium', prob/2,0)))%>%
  dplyr::group_by(fishing_style, extreme_event, node, iter)%>%
  dplyr::summarise(prob=sum(prob.2))%>%
  dplyr::filter(prob>0)%>%
  dplyr::mutate(prob=round(prob, digits=3))%>%
  #dplyr::filter(est=='high', node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  ggplot()+
  #xlim(c(0,1))+
  xlab('Probability of risk')+
  geom_density(aes(x=prob, fill=node), alpha=0.5)+
  #geom_vline(data=rq1, aes(xintercept =prob, color=node))+
  facet_grid( rows=vars(extreme_event),cols=vars(fishing_style), scales='free_y')+
  #facet_grid(rows=vars(fishing_style), cols=vars(extreme), scales='free_y')+
  scale_fill_manual(values=c('blue', 'chocolate', 'black' ))+
  scale_color_manual(values=c('blue', 'chocolate', 'black' ))

ggsave(plot=p4c, 'results/scenarios/s4_c.png', width = 220, height = 220, units='mm', dpi=500)




