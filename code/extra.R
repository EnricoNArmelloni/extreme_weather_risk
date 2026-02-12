# scope: to create conditional probabilities for variables that are excluded from the final version of the BN. It follows the same procedure adopted in other scripts.

remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_r1.R')

# data from interviews
questionnaire=read_excel("data/values.xlsx", 
                         sheet = "questions")
int.dat=read_csv("data/read_only/coding_report_unc.csv")
cost.dat=read_csv("data/read_only/cost_df_v2.csv")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'values.xlsx'), 
                        sheet = "events")
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')

style.dataset=read_excel("data/lists_fishing_styles.xlsx", 
                         sheet = "fishing_style")

style.df=style.dataset%>%distinct(id,style)
styles=data.frame(f.style=unique(style.df$style))

## cpts ####
answer.to.dist=function(x.ans, x.range=c(1,5),dim.name, dim.labs = c('L','M','H'), unc=0.05, k.thr=2, x.breaks=NULL){
  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=x.range[1],b=x.range[2],n=100, mean=x.ans, sd=unc)
  return(base.dist)
}


extra.nodes=c('infrastructure', 'management_space', 'management_species' , 'entrepeneurship', 'credit')
store=NULL
for(xx in 1:length(extra.nodes)){
  i.node=extra.nodes[xx]
  x.lev=c('low', 'medium','high')
  for(z in 1:length(styles$f.style)){
    z.style=styles[z,]
    i.fisher=style.dataset[style.dataset$style==z.style,]
    i.fisher=unique(i.fisher$id)
    dat=int.dat[int.dat$id_I%in%i.fisher,]
    x.answ=dat[grep(i.node, dat$short_description),]%>%
      arrange(short_description)
    answ.range=questionnaire[questionnaire$short_description==i.node & !is.na(questionnaire$short_description),]
    if(nrow(x.answ)>1){
      x.answ=x.answ%>%
        dplyr::group_by(short_description)%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
      x.answ$uncertainty=ifelse(is.na(x.answ$uncertainty),0.1,x.answ$uncertainty)
      x.answ$uncertainty=ifelse(x.answ$uncertainty==0,0.1,x.answ$uncertainty)
    }
    i.cpt=answer.to.cpt(x.ans=x.answ$value, 
                        unc=x.answ$uncertainty,
                        dim.name = i.node,
                        x.range=c(answ.range$min, answ.range$max),
                        dim.labs = x.lev)
    i.res=as.data.frame(i.cpt)
    names(i.res)='prob'
    i.res$est=rownames(i.res)
    i.res$fishing_style=z.style
    i.res$node=i.node
    store=rbind(store, i.res)
  }
}

store$est=factor(store$est, levels=c('low', 'medium','high'))

ggplot(data=store)+
  geom_col(aes(x=fishing_style, y=prob, fill=factor(est)))+
  facet_wrap(~node)

write.csv(store, 'data/read_only/extra_nodes_cpt.csv', row.names = F)

answer.to.dist=function(x.ans, x.range=c(1,5),dim.name, dim.labs = c('L','M','H'), unc=0.05, k.thr=2, x.breaks=NULL){
  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=x.range[1],b=x.range[2],n=100, mean=x.ans, sd=unc)
  base.dist=log.reg(l=1,k=k.thr, x=base.dist, x0=(x.range[2]+x.range[1])/2) 
  return(base.dist)
}


extra.nodes=c('infrastructure', 'management_space', 'management_species' , 'entrepeneurship',  'credit')
store=NULL
for(xx in 1:length(extra.nodes)){
  i.node=extra.nodes[xx]
  x.lev=c('low', 'medium','high')
  for(z in 1:length(styles$f.style)){
    z.style=styles[z,]
    i.fisher=style.dataset[style.dataset$style==z.style,]
    i.fisher=unique(i.fisher$id)
    dat=int.dat[int.dat$id_I%in%i.fisher,]
    x.answ=dat[grep(i.node, dat$short_description),]%>%
      arrange(short_description)
    answ.range=questionnaire[questionnaire$short_description==i.node & !is.na(questionnaire$short_description),]
    if(nrow(x.answ)>1){
      x.answ=x.answ%>%
        dplyr::group_by(short_description)%>%
        dplyr::summarise(uncertainty=sd(value), value=mean(value), .groups = "keep") # adjust the uncertainty
      x.answ$uncertainty=ifelse(is.na(x.answ$uncertainty),0.1,x.answ$uncertainty)
      x.answ$uncertainty=ifelse(x.answ$uncertainty==0,0.1,x.answ$uncertainty)
    }
    i.cpt=answer.to.dist(x.ans=x.answ$value, 
                        unc=x.answ$uncertainty,
                        dim.name = i.node,
                        x.range=c(answ.range$min, answ.range$max),
                        dim.labs = x.lev)
    i.res=as.data.frame(i.cpt)
    names(i.res)='prob'
    #i.res$est=rownames(i.res)
    i.res$fishing_style=z.style
    i.res$node=i.node
    store=rbind(store, i.res)
  }
}

store=store%>%
  dplyr::group_by(fishing_style, node)%>%
  dplyr::summarise(mu=mean(prob), sd=sd(prob))

ggplot(data=store)+
  geom_point(aes(x=fishing_style, y=mu), size=2)+
  geom_errorbar(aes(x=fishing_style, ymin=mu-sd, ymax=mu+sd))+
  facet_wrap(~node)

ggplot(data=store)+
  geom_tile(aes(x=fishing_style, y=node, fill=mu))+
  scale_fill_viridis_c()


#write.csv(store, 'data/read_only/extra_nodes_cpt.csv', row.names = F)
# load ####
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
theme_set(theme_bw())


net=read.net('data/networks/BEWARE_v3_learn_pt2.net', debug = T)

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
  
  i.res=cpdist(net.s1, nodes = c('strategy', 'additional_mitigation','objectives_type', 'go_out', 'economic_risk','societal_risk','individual_risk'), 
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

ggplot(data=ev.store[ev.store$node=='strategy' ,])+
  geom_col(aes(x=fishing_style, y=prob, fill=est))+
  facet_wrap(~node)


#### put all togheter
#all.cpt=rbind(ev.store[,names(store)],store)

strat.store=ev.store[ev.store$node=='strategy',]

p1=store[,c('fishing_style','node','mu')]
p2=strat.store[c('fishing_style','est','prob')]%>%
  pivot_wider(names_from = est, values_from = prob)%>%
  replace(is.na(.),0)%>%
  pivot_longer(-fishing_style, names_to = 'est', values_to = 'prob')
p3=left_join(p1,p2, by='fishing_style')


ggplot(data=p3, aes(x=mu, y=prob))+
  geom_point(aes(color=node), size=2)+
  facet_wrap(~est, scales='free_y')


pl.ada1=ggplot(data=store)+
  geom_tile(aes(y=fishing_style, x=node, fill=mu))+
  scale_fill_viridis_c()+
  xlab('')+
  labs(fill='Availability OR Flexibility')+
  theme(legend.position = 'bottom')+
  scale_fill_gradient2(midpoint=0.5, high='lightgreen', low='red', mid='yellow',na.value = "grey98")

strat.plot=ev.store[ev.store$node=='strategy' ,]
strat.plot$est=factor(strat.plot$est, levels=c('adapt', 'react','cope','not_relevant'))
#strat.plot$prob=-strat.plot$prob

pl.ada2=ggplot(data=strat.plot)+
  geom_col(aes(y=fishing_style, x=prob, fill=est), position='fill')+
  #facet_wrap(~node)+
  scale_fill_viridis_d()+
  ylab('')+
  labs(fill='Strategy')+
  scale_x_reverse()+
  scale_fill_manual(values=c('lightgreen','yellow','red' ,  "grey98"  ))+
  xlab('Probability')+
  theme(legend.position = 'bottom');pl.ada2

pl.ada3=ggpubr::ggarrange(pl.ada1, pl.ada2, nrow=1)

ggsave(plot=pl.ada3, 'results/scenarios/sextra.png', width = 350, height = 120, units='mm', dpi=500)


obj.store=ev.store[ev.store$node%in%c('objectives_type'),]
obj.store$est=ifelse(obj.store$est=='progress_rate','no','yes')
obj.store$node='substitution_capacity'
obj.store=obj.store[obj.store$est=='yes',]

strat.df=p2%>%
  dplyr::mutate(est=ifelse(est%in%c('react','adapt'),'act',est))%>%
  dplyr::filter(est!='not_relevant')%>%
  dplyr::group_by(fishing_style, strategy=est)%>%
  dplyr::summarise(st_prob=sum(prob))%>%
  dplyr::group_by(fishing_style)%>%
  dplyr::mutate(st_prob=st_prob/sum(st_prob))

extr2=left_join(obj.store, strat.df)

pl.kobe=ggplot(data=extr2[extr2$strategy=='act',])+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=-Inf, ymax=0.25, fill='red')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=-Inf, ymax=0.25, fill='yellow')+
  annotate('rect', xmin=-Inf,xmax=0.5, ymin=0.25, ymax=Inf, fill='yellow')+
  annotate('rect', xmin=0.5,xmax=Inf, ymin=0.25, ymax=Inf, fill='green')+
  geom_label(aes(x=prob, y=st_prob, label=fishing_style))+
  xlim(c(0,1))+
  ylim(c(0,0.5))+
  xlab('Substitution Capacity')+
  ylab('Proactive response')
  
ggsave(plot=pl.kobe, 'results/scenarios/skobe.png', width = 120, height = 120, units='mm', dpi=500)



library(randomForest)

p.rf=p3[p3$est%in%c('cope'),]%>%
  dplyr::group_by(fishing_style, node, mu)%>%
  dplyr::summarise(prob=sum(prob))%>%
  pivot_wider(names_from = node, values_from = mu)


rf_model=randomForest(
  prob ~ .,
  data = p.rf[,2:ncol(p.rf)],
  ntree = 25,        
  mtry = 1,     
  importance = TRUE)

print(rf_model)

preds=predict(rf_model, p.rf)
obs=p.rf$prob


plot(obs, preds)
ch=importance(rf_model)
data.frame(ch)%>%
  arrange(desc(IncNodePurity))


ch=varImpPlot(rf_model)

library(rpart)
library(rpart.plot)
tree <- rpart(prob ~ ., data =  p.rf[,2:ncol(p.rf)], cp = 0)
print(tree)
rpart.plot(tree)


pairs(p.rf[, -which(names(p.rf) %in%c( 'fishing_style'))])

cor(p.rf[, -which(names(p.rf) %in%c( 'fishing_style'))])















