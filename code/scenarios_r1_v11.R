remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
library(ggrepel)
library(readxl)
library(gRain)
theme_set(theme_bw())
net=read.net('data/networks/BEWARE_r1_learn_pt2.net', debug = T)
source('code/supporting_r1.R')

sim.uncertainty=1

# Load data ####
# data from interviews
style.dataset=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "fishers")
styles=style.dataset%>%distinct(f.style=code, short_description)

# RQ1 ####
# how does MEWs produce risks?
# plot 1: potential consequences
array.var=net[['extreme_event']]$prob
ev.list=names(array.var[array.var>0])
store.rq1.1=NULL
for(i in 1:length(ev.list)){
  # query
  i.res=cpdist(net, nodes = c('catch_condition', 'personal_safety', 'damage', 'catchability'), 
               evidence = list(extreme_event = ev.list[i],
                               stock_status='status_quo',
                               aware_of_event='no', strategy_to_change='cope'), n=10^5, method='lw')
  i.res$extreme_event=ev.list[i]
  
  # results are many observations. I summerise directly here
  i.res=i.res%>%
    pivot_longer(-extreme_event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(extreme_event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  store.rq1.1=rbind(store.rq1.1, i.res)
}

# plot 2: risks and adaptation
array.var=net[['extreme_event']]$prob
ev.list=names(array.var[array.var>0])
store.rq1.2=NULL
store.rq1.3=NULL
for(i in 1:length(ev.list)){
  # query
  i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk','go_fishing', 'strategy_to_change'), 
               evidence = list(extreme_event = ev.list[i],
                               stock_status='status_quo',
                               aware_of_event='yes'), n=10^5, method='lw')
  i.res$extreme_event=ev.list[i]
  
  # results are many observations. I summerise directly here
  i.res.fishing=i.res[i.res$go_fishing=='yes',]%>%
    pivot_longer(-extreme_event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(extreme_event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  store.rq1.3=rbind(store.rq1.3, i.res.fishing)
  
  i.res=i.res%>%
    pivot_longer(-extreme_event, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(extreme_event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  
  store.rq1.2=rbind(store.rq1.2, i.res)
}

# effects plot
effects.df=store.rq1.1
effects.df$category.format='bad'
effects.df[effects.df$est=='same',]$category.format='good'
effects.df[effects.df$est=='worse',]$category.format='bad'
effects.df[effects.df$est=='better',]$category.format='very_good'
effects.df[ effects.df$est=='no',]$category.format='good'
effects.df[effects.df$est=='minor',]$category.format='moderate'
effects.df[ effects.df$est=='major',]$category.format='bad'
effects.df[effects.df$est=='destroy',]$category.format='very_bad'
effects.df$category.format=factor(effects.df$category.format, levels=c('very_good', 'good', 'moderate', 'bad','very_bad'))
effects.df$node=factor(effects.df$node, levels=c('go_fishing','strategy_to_change',  'personal_safety', 'damage','catch_condition', 'catchability'))
effects.df$lab=paste(effects.df$node, effects.df$est, sep='_')

p.RQ1.c=effects.df%>%
  replace(is.na(.),0)%>%
  ggplot(aes(x=node, y=prob, fill=as.factor(category.format)))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_wrap(~extreme_event, ncol=2)+
  theme(legend.position = 'bottom')+
  labs(fill='Category')+
  ylab('Frequency')+
  xlab('Node')+
  coord_flip()+
  scale_fill_manual(values=c('green', 'lightgreen', 'yellow', 'red', 'brown' ))
ggsave(plot=p.RQ1.c, 'results/scenarios/s1_effect.png', width = 210, height = 120, units='mm', dpi=500)

new.dat=effects.df%>%
  dplyr::filter(node %in% c('catch_condition', 'catchability', 'damage', 'personal_safety'))%>%
  dplyr::mutate(type=paste(node, est, sep='_'))%>%
  ungroup()%>%
  dplyr::select(extreme_event, type, prob)%>%
  pivot_wider(names_from = type, values_from = prob)%>%
  replace(is.na(.),0)
new.pc=data.frame(new.dat[,2:ncol(new.dat)])
rownames(new.pc)=new.dat$extreme_event
my_pca <- prcomp(new.pc, scale. = TRUE, center = TRUE, retx = TRUE)
scores <- as.data.frame(my_pca$x)
scores$lab=rownames(scores)

col.palette=effects.df%>%
  ungroup%>%
  distinct(node,lab,category.format)

loadings <- as.data.frame(my_pca$rotation)
loadings$contr=abs(loadings$PC1)+abs(loadings$PC2)
loadings=loadings[order(abs(loadings$contr), decreasing = T),]
loadings$cum.cont=cumsum(loadings$contr)/sum(loadings$contr)
#loadings=loadings[loadings$cum.cont<=0.75,]
loadings$lab=rownames(loadings)
loadings=loadings%>%left_join(col.palette)

pca.summ=(summary(my_pca))
comp1=pca.summ$importance[2,1]
comp2=pca.summ$importance[2,2]
pca_scores <- as.data.frame(my_pca$x)
kmeans_result <- kmeans(pca_scores[,1:2], centers = 3)
scores$cluster <- as.factor(kmeans_result$cluster)

pca.plot=ggplot(scores, aes(PC1, PC2)) +
  geom_segment(data = loadings, 
               aes(x = 0, y = 0,xend = PC1*10,yend = PC2*10,color = category.format),
               arrow = arrow(length = unit(0.2,"cm")), linewidth=2) +
  geom_text(data = loadings,
            aes(x = PC1*10,y = PC2*10,label = node),
            color = "black",vjust = 1.5) +
  geom_vline(xintercept=0, linetype=2)+
  geom_hline(yintercept=0, linetype=2)+
  geom_label(aes(label=paste(lab, "^(Cluster", cluster, ")", sep = "")), parse=T)+
  theme_classic()+
  labs(color='Effect type')+
  scale_color_manual(values=c('darkgreen', 'lightgreen', 'yellow', 'red', 'brown' ))+
  xlab(paste('PC1 (', round(comp1, digits=3)*100 ,'%)'))+
  ylab(paste('PC2 (', round(comp2, digits=3)*100 ,'%)'));pca.plot

ggsave(plot=pca.plot, 'results/scenarios/s1_pca.png', width = 120, height = 120, units='mm', dpi=500)

# risks plot
x.rel=store.rq1.2%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::filter(est=='not_relevant')%>%
  ungroup()%>%
  distinct(extreme_event, notrel=prob)
risk.df.R1=store.rq1.2%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  left_join(x.rel)%>%
  dplyr::mutate(est=ifelse(as.character(est)== 'not_relevant', 'Not Relevant', as.character(est)))%>%
  dplyr::mutate(est=factor(est, levels=c('Not Relevant', 'low', 'medium',  'high')))%>%
  replace(is.na(.),0)
p.RQ1.a=risk.df.R1%>%
  dplyr::mutate(node=str_remove(node, '_risk'))%>%
  ggplot(aes(x=node, y=prob, fill=est))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_wrap(~extreme_event, nrow=1)+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  theme(legend.position = 'bottom')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(fill='Risk')+
  ylab('Frequency')+
  xlab('Risk category')
write.csv(risk.df.R1, 'results/scenarios/riskRQ1.csv', row.names = F)
ggsave(plot=p.RQ1.a, 'results/scenarios/s1_risk.png', width = 120, height = 200, units='mm', dpi=500)


pv2=rbind(store.rq1.2[which(store.rq1.2$node %in% c('go_fishing')),],
          store.rq1.3[which(store.rq1.3$node %in% c('strategy_to_change')),])%>%
  dplyr::filter(est!='no')%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::mutate(est=as.character(est))%>%
  dplyr::mutate(est=ifelse(est=='yes', 'fishing', est))%>%
  dplyr::mutate(est=factor(est, levels=c('fishing', 'cope','adapt','react')))%>%
  #replace(is.na(.),0)%>%
  ggplot(aes(x=est, y=prob))+
  geom_col(position='dodge', color='black', linewidth=0.1)+
  facet_wrap(~extreme_event, nrow=1)+
  theme(legend.position = 'bottom')+
  labs(fill='Category')+
  ylab('Frequency')+
  xlab('Node')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

pr1.b=ggpubr::ggarrange(pv2,p.RQ1.a,  ncol=1, labels=c('c)','d)'))
pr1c=ggpubr::ggarrange(pca.plot, pr1.b, ncol=1, heights = c(1,1.2))
ggsave(plot=pr1c, 'results/scenarios/s1_c.png', width = 230, height = 250, units='mm', dpi=500)

### table numbers
store.rq1.2%>%
  dplyr::filter(node=='go_fishing')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)

store.rq1.3%>%
  dplyr::filter(node=='strategy_to_change')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)

# RQ2 ####
# what are the fishing styles more at risk?
array.var=net[['fishing_style']]$prob
fi.list=names(array.var[array.var>0])
ev.store=NULL
ev.store.2=NULL
for(i in 1:length(fi.list)){
  i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk', 
                                 'strategy_to_change', 'go_fishing', 'substitution_capacity', 'economic_buffers', 'societal_importance', 'individual_importance'), 
               evidence = list(fishing_style = fi.list[i], stock_status='status_quo',
                               aware_of_event='yes'), n=10^5, method='lw')
    i.res$fishing_style=fi.list[i]
    
  i.res.fishing=i.res[i.res$go_fishing=='yes',]%>%
    pivot_longer(-fishing_style, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store.2=rbind(ev.store.2, i.res.fishing)
    
    
  i.res=i.res%>%
    pivot_longer(-fishing_style, names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, node)%>%
    dplyr::mutate(prob=n/sum(n))
  ev.store=rbind(ev.store, i.res)
  
}

names(styles)[2]='fishing_style'
ev.store=ev.store%>%left_join(styles)

risk.df.R2=ev.store%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::mutate(est=ifelse(as.character(est)== 'not_relevant', 'Not Relevant', as.character(est)))%>%
  dplyr::mutate(est=factor(est, levels=c('Not Relevant', 'low', 'medium',  'high')))%>%
  replace(is.na(.),0)

p.f2=risk.df.R2%>%
  dplyr::mutate(node=str_remove(node, '_risk'))%>%
  ggplot(aes(x=node, y=prob, fill=est))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_wrap(~f.style, nrow=1)+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  #scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylab('Frequency')+
  xlab('Fishing style');p.f2

write.csv(risk.df.R2, 'results/scenarios/riskRQ2.csv', row.names = F)
ggsave(plot=p.f2, 'results/scenarios/s2_risk.png', width = 120, height = 200, units='mm', dpi=500)


# effects plot
effects.df=ev.store[-which(ev.store$node %in% c('economic_risk', 'individual_risk', 'societal_risk','substitution_capacity')),]
unique(effects.df$node)

effects.df$category.format='not_relevant'
effects.df[effects.df$node=='go_fishing' & effects.df$est=='no',]$category.format='negative'
effects.df[effects.df$node=='go_fishing' & effects.df$est=='yes',]$category.format='positive'
effects.df[effects.df$node%in% c('societal_importance', 'individual_importance') & effects.df$est=='low',]$category.format='strong'
effects.df[effects.df$node%in% c('societal_importance', 'individual_importance') & effects.df$est=='medium',]$category.format='moderate'
effects.df[effects.df$node%in% c('societal_importance', 'individual_importance') & effects.df$est=='high',]$category.format='no'
effects.df[effects.df$node%in% c('economic_buffers') & effects.df$est=='high',]$category.format='strong'
effects.df[effects.df$node%in% c('economic_buffers') & effects.df$est=='medium',]$category.format='moderate'
effects.df[effects.df$node%in% c('economic_buffers') & effects.df$est=='low',]$category.format='no'


effects.df$category.format=paste(effects.df$category.format, 'buffer', sep='_')
effects.df$category.format=factor(effects.df$category.format, levels=c('strong_buffer', 'moderate_buffer', 'no_buffer'))


effects.df$node=factor(effects.df$node, levels=c('go_fishing','strategy_to_change',  'economic_buffers', 'societal_importance','individual_importance'))


p.RQ2.c=effects.df%>%
  dplyr::filter(node %in% c('economic_buffers', 'societal_importance','individual_importance'))%>%
  #replace(is.na(.),0)%>%
  dplyr::mutate(node=str_remove(node, '_importance'))%>%
  ggplot(aes(x=node, y=prob, fill=as.factor(category.format)))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_wrap(~fishing_style, nrow=1)+
  theme(legend.position = 'bottom')+
  labs(fill='Risk')+
  ylab('Frequency')+
  xlab('MEW')+
  scale_fill_grey();p.RQ2.c
  #coord_flip()+
  scale_fill_manual(values=c( 'lightgreen', 'yellow', 'red', 'white'));p.RQ2.c

rbind(ev.store[which(ev.store$node %in% c('go_fishing')),],
      ev.store.2[which(ev.store.2$node %in% c('strategy_to_change')),])%>%
  dplyr::filter(est!='no')%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::mutate(est=as.character(est))%>%
  dplyr::mutate(est=ifelse(est=='yes', 'fishing', est))%>%
  dplyr::mutate(est=factor(est, levels=c('fishing', 'cope','adapt','react')))%>%
  #replace(is.na(.),0)%>%
  ggplot(aes(x=est, y=prob))+
  geom_col(position='dodge', color='black', linewidth=0.1)+
  facet_wrap(~fishing_style, nrow=1)+
  theme(legend.position = 'bottom')+
  labs(fill='Category')+
  ylab('Frequency')+
  xlab('Node')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggsave(plot=p.RQ2.c, 'results/scenarios/s2_effect.png', width = 210, height = 120, units='mm', dpi=500)
ggsave(plot=p.f3, 'results/scenarios/s1_2.png', width = 180, height = 120, units='mm', dpi=500)

new.buffer=effects.df%>%
  dplyr::filter(node %in% c('economic_buffers', 'societal_importance','individual_importance'))%>%
  #replace(is.na(.),0)%>%
  dplyr::mutate(node=str_remove(node, '_importance'))%>%
  dplyr::mutate(weight=ifelse(category.format=='no_buffer',0,
                              ifelse(category.format=='moderate_buffer',0.5,1)),
                prob.w=prob*weight)%>%
  dplyr::group_by(fishing_style, node)%>%
  dplyr::summarise(buffer_strenght=sum(prob.w))


strat.store=ev.store[ev.store$node=='strategy_to_change',]
obj.store=ev.store[ev.store$node%in%c('substitution_capacity'),]

p2=strat.store[c('fishing_style','est','prob')]%>%
  pivot_wider(names_from = est, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(-fishing_style, names_to = 'est', values_to = 'prob')

strat.plot=ev.store[ev.store$node=='strategy_to_change' ,]
strat.plot$est=factor(strat.plot$est, levels=c('adapt', 'react','cope','not_relevant'))
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
pl.kobe=ggplot(data=extr2[extr2$strategy_to_change=='act',])+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=-Inf, ymax=0.25, fill='#FFFFFF')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=-Inf, ymax=0.25, fill='#808080')+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=0.25, ymax=Inf, fill='#808080')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=0.25, ymax=Inf, fill='#000000')+
  geom_label_repel(aes(x=prob, y=st_prob, label=fishing_style))+
  geom_point(aes(x=prob, y=st_prob))+
  xlim(c(0,1))+
  ylim(c(0,0.5))+
  xlab('Substitution Capacity')+
  ylab('Proactive response');pl.kobe

grey(seq(0,1,0.5))000000

ggsave(plot=pl.kobe, 'results/scenarios/s2_kobe.png', width = 120, height = 120, units='mm', dpi=500)


prq2.1=ggpubr::ggarrange( pl.kobe,p.RQ2.c, labels=(c('a)','b)')), nrow=2);prq2.1

p.rq2.2=ggpubr::ggarrange(p.f2,p.f1,  ncol=1, labels=(c('c)','d)')))
pr2c=ggpubr::ggarrange(prq2.1, p.rq2.2, ncol=1)

ggsave(plot=pr2c, 'results/scenarios/s3_c.png', width = 220, height = 250, units='mm', dpi=500)


# radar plot
r.buffer=new.buffer%>%
  pivot_wider(names_from = node, values_from = buffer_strenght)%>%
  left_join(extr2[extr2$strategy_to_change=='act',c('fishing_style', "prob", "st_prob")])
names(r.buffer)[1]='Group'

ggradar(r.buffer,
        background.circle.colour = "white")

df.2=data.frame(t(names(r.buffer)))
names(df.2)=df.2[1,]
df.2=df.2[rep(row.names(df.2), times = 2), ]
df.2[1,]=0
df.2[2,]=1

df.3=rbind(df.2,r.buffer)


install.packages("fmsb")
library(fmsb)
student1_data <- df[c("Max", "Min", "Student.1"), ]


radarchart(df.3[c(1,2,5),2:6])


library(ggradar)
ggradar(df)
set.seed(4)
df <- data.frame(matrix(runif(30), ncol = 10))
df[, 1] <- paste0("G", 1:3)
colnames(df) <- c("Group", paste("Var", 1:9))

ggradar(df,
        background.circle.colour = "white")







ev.store%>%
  dplyr::filter(node=='go_fishing')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)

ev.store.2%>%
  dplyr::filter(node=='strategy_to_change')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)





