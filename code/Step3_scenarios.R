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
net=read.net('data/read_only/networks/BEWARE_learnt_r1_0_0.net', debug = T)
source('code/supporting_r1.R')
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
sim.uncertainty=1

# Load data ####
# data from interviews
style.dataset=read_excel("data/editable_files/dialogues_raw.xlsx", 
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
               aes(x = 0, y = 0,xend = PC1*7,yend = PC2*7,color = category.format),
               arrow = arrow(length = unit(0.2,"cm")), linewidth=2) +
  geom_text(data = loadings,
            aes(x = PC1*7,y = PC2*7,label = node),
            color = "black",vjust = 1.5) +
  geom_vline(xintercept=0, linetype=2)+
  geom_hline(yintercept=0, linetype=2)+
  geom_label(aes(label=paste(lab, "^(Cluster", cluster, ")", sep = "")), parse=T)+
  theme_classic()+
  xlim(c(-5,4))+
  ylim(c(-4,3))+
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
  dplyr::mutate(extreme_event=factor(extreme_event, levels=c('hws', 'abl','hww','erf', 'gal', 'sto','ici')))%>%
  ggplot(aes(x=node, y=prob, fill=est))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_wrap(~extreme_event, nrow=1)+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  theme(legend.position = 'bottom')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(fill='Risk')+
  ylab('Frequency')+
  xlab('Risk category')
write.csv(risk.df.R1, 'results/scenarios/S2.csv', row.names = F)
ggsave(plot=p.RQ1.a, 'results/scenarios/s1_risk.png', width = 120, height = 200, units='mm', dpi=500)


pv2=rbind(store.rq1.2[which(store.rq1.2$node %in% c('go_fishing')),],
          store.rq1.3[which(store.rq1.3$node %in% c('strategy_to_change')),])%>%
  dplyr::filter(est!='no')%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::mutate(est=as.character(est))%>%
  dplyr::mutate(est=ifelse(est=='yes', 'Fishing', firstup(est)))%>%
  dplyr::mutate(est=factor(est, levels=c('Fishing', 'Cope','Adapt','React')))%>%
  dplyr::mutate(extreme_event=factor(extreme_event, levels=c('hws', 'abl','hww','erf', 'gal', 'sto','ici')))%>%
  #replace(is.na(.),0)%>%
  ggplot(aes(x=est, y=prob))+
  geom_col(position='dodge', color='black', linewidth=0.1)+
  facet_wrap(~extreme_event, nrow=1)+
  theme(legend.position = 'bottom')+
  labs(fill='Category')+
  ylab('Frequency')+
  xlab('Decision')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

pr1.b=ggpubr::ggarrange(pv2,p.RQ1.a,  ncol=1, labels=c('b)','c)'))
pr1c=ggpubr::ggarrange(pca.plot, pr1.b, ncol=1, heights = c(1,1.2), labels=c('a)', 'b)','c)'))
ggsave(plot=pr1c, 'results/scenarios/s1_c.png', width = 250, height = 250, units='mm', dpi=500)

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
                                 'strategy_to_change', 'go_fishing', 'substitution_capacity', 'economic_buffers', 'societal_importance', 'individual_importance', 'non_monetary_value', 'monetary_loss'), 
               evidence = list(fishing_style = fi.list[i], 
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

write.csv(risk.df.R2, 'results/scenarios/S3.csv', row.names = F)
ggsave(plot=p.f2, 'results/scenarios/s2_risk.png', width = 200, height = 70, units='mm', dpi=500)


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

ss1=rbind(ev.store[which(ev.store$node %in% c('go_fishing')),],
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
  xlab('Decision')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

ggsave(plot=ss1, 'results/scenarios/ss_effect.png', width = 200, height = 60, units='mm', dpi=500)

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


# radar plot
library("fmsb")

# Put the line labels in rownames
r.buffer=new.buffer%>%
  pivot_wider(names_from = node, values_from = buffer_strenght)%>%
  left_join(extr2[extr2$strategy_to_change=='act',c('fishing_style', "prob", "st_prob")])
names(r.buffer)[1]='Group'
r.buffer=as.data.frame(r.buffer)
rownames(r.buffer)=firstup(r.buffer$Group)
rownames(r.buffer)=c('Fishing guide', 'Household fishing', 'Recreational fishing', 'Small-scale coastal fishing',
                     'Pelagic trawling')
r.buffer$Group=NULL

data=r.buffer
names(data)=c('Econ. buffers', 'Ind. buffers', 'Soc. buffers', 'Substitution capacity','Proactive resp.')
data=data[,c(4,1:3,5)]

# Fetch minima and maxima of every column of the data-set ---- 
colMax <- function (x) { apply(x, MARGIN=c(2), max) }
colMin <- function (x) { apply(x, MARGIN=c(2), min) }
maxmin <- data.frame(max=colMax(data),min=colMin(data))

# Calculate the average profile ----
average <- data.frame(rbind(maxmin$max,maxmin$min,t(colMeans(data))))
colnames(average) <- colnames(data)
radarchart(average)

# Produce multiple plots ----
opar <- par() # save standard page layout settings for later restoration
# Define settings for plotting in a 3x4 grid, with appropriate margins:

# Iterate through the data, producing a radar-chart for each line
library("fmsb")
png('results/scenarios/sradar.png',width=32,height=20, units = 'cm', res=500)
#par(mar=rep(0.8,2))
par(mfrow=c(2,3))
for (i in 1:nrow(data)) {
  toplot <- rbind(
    1,
    0,
    average[3,],
    data[i,]
  )
  radarchart(
    toplot,
    pfcol = c("#99999980",NA),
    pcol= c(NA,1),
    pty = 12, 
    plty = 1,
    plwd = 2,
    seg=2,
    vlcex = 1.5,
    cex.main = 2,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    title = row.names(data[i,])
  )
}
radarchart(rbind(
  1,
  0,
  average[3,]
), title='Average',
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           plty = 1,
           vlcex = 1.5,
seg = 2,
           cex.main = 2,
           caxislabels = c("0", "20"),  
           calcex = 1.2,
           plwd = 2,
           pfcol = c("#99999980",NA),pcol=1)
dev.off()
par <- par(opar) # restore standard par settings


## tables

ev.store%>%
  dplyr::filter(node=='monetary_loss')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)

ev.store%>%
  dplyr::filter(node=='non_monetary_value')%>%
  dplyr::mutate(prob=round(prob, digits=2))%>%
  dplyr::select(-n)%>%
  pivot_wider(names_from = est, values_from = prob)



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

# RQ extra ####
# RQ2 ####
# what are the fishing styles more at risk?
array.var=net[['fishing_style']]$prob
fi.list=names(array.var[array.var>0])
array.var=net[['extreme_event']]$prob
ev.list=names(array.var[array.var>0])

extra.store=NULL
for(i in 1:length(fi.list)){
  for(j in 1:length(ev.list)){
    
      i.res=cpdist(net, nodes = c('economic_risk', 'societal_risk','individual_risk' ), 
               evidence = list(fishing_style = fi.list[i],
                               extreme_event = ev.list[j],
                                                    aware_of_event='yes'), n=10^5, method='lw')
  i.res$fishing_style=fi.list[i]
  i.res$extreme_event=ev.list[j]
  
  i.res=i.res%>%
    pivot_longer(-c(fishing_style, extreme_event), names_to = 'node', values_to = 'est')%>%
    dplyr::group_by(fishing_style,extreme_event, node,est)%>%
    tally()%>%
    dplyr::group_by(fishing_style, extreme_event, node)%>%
    dplyr::mutate(prob=n/sum(n))
  extra.store=rbind(extra.store, i.res)
  }
}

extra.store=extra.store%>%left_join(styles)

risk.df.R2=extra.store%>%
  dplyr::filter(node %in% c('economic_risk', 'individual_risk', 'societal_risk'))%>%
  dplyr::mutate(est=ifelse(as.character(est)== 'not_relevant', 'Not Relevant', as.character(est)))%>%
  dplyr::mutate(est=factor(est, levels=c('Not Relevant', 'low', 'medium',  'high')))%>%
  replace(is.na(.),0)

p.f2=risk.df.R2%>%
  dplyr::mutate(node=str_remove(node, '_risk'))%>%
  ggplot(aes(x=node, y=prob, fill=est))+
  geom_col(position='fill', color='black', linewidth=0.1)+
  facet_grid(rows=vars(f.style), cols=vars(extreme_event))+
  scale_fill_manual(values=c('grey98', 'lightgreen', 'yellow', 'red' ))+
  #scale_fill_manual(values=c('lightgreen', 'yellow', 'red'))+
  theme(legend.position = 'bottom')+
  labs(fill='Risk')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylab('Frequency')+
  xlab('Fishing style');p.f2

write.csv(risk.df.R2, 'results/scenarios/S5.csv', row.names = F)
ggsave(plot=p.f2, 'results/scenarios/sS_risk.png', width = 200, height = 200, units='mm', dpi=500)



## Table 4
node.text=read_excel(file.path(scriptDir, '..','data/editable_files/nodes_text.xlsx'))
net=read.net(file.path(scriptDir, '..','data/editable_files/networks/BEWARE_release_v1_0_0.net'), debug = T)
x.nodes=nodes(net)


#table.format=NULL
#i=5
#for(i in 1:length(x.nodes)){
#  
#  extra.info=node.text[node.text$node==x.nodes[i],]
#  x.var=x.nodes[i]
#  if(x.var %in% c('Node5', 'Node1')){next}
#  array.var=net[[x.var]][['prob']]
#  xdim=dim(array.var)
#  xnam=dimnames(array.var)
#  df.var=as.data.frame(array.var)
#  if(ncol(df.var)==2){
#    x.lev=levels(df.var$Var1)
#    x.parent=NA
#  }else{
#    x.lev=levels(df.var[,x.var])
#    x.parent=names(df.var)[-which(names(df.var)%in% c(x.var, 'Freq'))]
#  }
#  x.lev=paste(x.lev, collapse=', ')
#  x.parent=paste(x.parent, collapse=', ')
#  
#  result=data.frame(name=x.nodes[i], group= extra.info$group, specification=extra.info$short_text, levels= x.lev, parents= x.parent, cycle=extra.info$cycle)
#  table.format=rbind(table.format, result)
#}
#
#write.csv(table.format, file.path(scriptDir, '..','results/scenarios/tab4_cpt_description.csv'), row.names = F)
#


