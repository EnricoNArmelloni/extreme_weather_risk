#remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
source('code/supporting_r1.R')
sim.uncertainty=0

# Load data ####
# data from interviews
questionnaire=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "questions")
style.dataset=read_excel("data/dialogues_raw.xlsx", 
                         sheet = "fishers")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'dialogues_raw.xlsx'), 
                        sheet = "events")
int.dat=read_csv("data/read_only/coding_report_unc.csv")
strategy.dataset=read.csv("data/adaptive_revised_v2.csv")
strategy.dataset$gear=ifelse(is.na(strategy.dataset$gear), 'not_specified', strategy.dataset$gear)
strategy.dataset$target=ifelse(is.na(strategy.dataset$target), 'not_specified', strategy.dataset$target)


evt.relevance=read_csv("data/read_only/relevance.csv")

# data from literature
coast.fdays=read_csv("data/read_only/coastal_fdays.csv")
coast.fdays=coast.fdays[1:3,]
coast.fdays$cpt=coast.fdays$fdays/sum(coast.fdays$fdays)
coast.catch=read_csv("data/read_only/coastal_catch.csv")
rec.catch=read_excel("data/read_only/recf_catch.xlsx", 
                     sheet = "tab2")
rec.catch=rec.catch[rec.catch$name %in% c('Perch', 'Pike', 'Salmon', 'Zander', 'Whitefish', 'Trout', 'Herring','Cod',
                                'Not specified'),]
rec.practics=read_excel("data/read_only/recf_catch.xlsx", 
           sheet = "tab1")
rec.catch=left_join(rec.catch, rec.practics)
rec.catch$prop=rec.catch$practitioners/sum(rec.catch$practitioners)

# DAG
net=read.net('data/networks/BEWARE_r1_2.net', debug = F)
#graphviz.chart(net)

# format data ####
evts=evts[!is.na(evts$event_code),]
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')
strategy.unique=strategy.dataset[nchar(strategy.dataset$strategy)>0&
                                   !is.na(strategy.dataset$event_code),]%>%
  distinct(id_I, event_code, strategy, solution)%>%
  dplyr::filter(!is.na(strategy))
evt.relevance=evt.relevance[evt.relevance$value>0.5,]
names(evt.relevance)[2]='f.style'
styles=style.dataset%>%distinct(f.style=code, short_description)

int.dat$target=ifelse(is.na(int.dat$target), 'not_specified', int.dat$target)
int.dat$area=ifelse(is.na(int.dat$area), 'not_specified', int.dat$area)
int.dat$gear=ifelse(is.na(int.dat$gear), 'not_specified', int.dat$gear)
int.dat=int.dat[is.na(int.dat$unit_range),]
int.dat=int.dat[!is.na(int.dat$value.norm),]
int.dat=int.dat[int.dat$area %in% c('not_specified', 'Baltic', 'deep','shallow'),]

fish.simple=style.dataset[,c('short_description', 'id_I')]
names(fish.simple)[1]='fishing_style'
evt.relevance=evt.relevance%>%left_join(styles, by='f.style')

# CPT filling ####
x.nodes=nodes(net)

# decision nodes ####
# set defaults as equal probabilities for all the levels
array.var=net[['extreme_event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['extreme_event']]=array.var

array.var=net[['fishing_style']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['fishing_style']]=array.var

array.var=net[['stock_status']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['stock_status']]=array.var

array.var=net[['aware_of_event']]$prob
xdim=dim(array.var)
array.var[1:xdim]=rep(1/xdim, xdim)
net[['aware_of_event']]=array.var

# user####
## gear ####
i.node='gear'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]
df.var[df.var$strategy_to_change=='not_relevant',]$Freq=c(rep(1/7,7))

## Define here fishing gear by style. Those that never change gear
df.var[df.var$strategy_to_change!='not_relevant' & df.var$fishing_style=='trawler',]$Freq=c(1, rep(0,6))
df.var[df.var$strategy_to_change!='not_relevant' & df.var$fishing_style=='household',]$Freq=c(0,0.5,0.5, rep(0,4))

# those that changes, set baseline first. 
df.var[df.var$strategy_to_change!='not_relevant' & 
         df.var$fishing_style=='recreational',]$Freq=c(0,0,0,0,0.42,0,0.58) # tabell 2. FIshing days inland = 5814; Kust = 4303. Kust accounts for 42% of rec fishing
df.var[df.var$strategy_to_change!='not_relevant' & 
         df.var$fishing_style=='guide',]$Freq=c(0,0,0,0,0.42,0,0.58) # tabell 2. FIshing days inland = 5814; Kust = 4303. Kust accounts for 42% of rec fishing

# small scale from DCF data
coast.fdays$metier=tolower(coast.fdays$metier)
coast.met.cpt=data.frame(metier=unique(df.var$gear))%>%
  left_join(coast.fdays[,c('metier', 'cpt')], by='metier')%>%
  replace(is.na(.),0)%>%
  dplyr::mutate(cpt=round(cpt, digits=3))
df.var[df.var$strategy_to_change!='not_relevant' & df.var$fishing_style=='small_scale',]$Freq=coast.met.cpt$cpt

pl=df.var%>%
  dplyr::filter(strategy_to_change=='cope')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=gear))+
  geom_col()+
  facet_wrap(~fishing_style);pl

gear.prob=df.var%>%
  dplyr::filter(strategy_to_change=='cope')%>%
  distinct(fishing_style, gear, Freq)%>%
  dplyr::filter(Freq>0)

## summer
df.var[df.var$strategy_to_change=='adapt' & 
         df.var$fishing_style=='recreational'&
         df.var$extreme_event=='abl',]$Freq=c(0,0,0,0,0,0,1) # goes inland

df.var[df.var$strategy_to_change=='adapt' & 
         df.var$fishing_style=='guide'&
         df.var$extreme_event=='abl',]$Freq=c(0,0,0,0,0,0,1) # goes inland

# one small scale fisher mention that he stop fishing for herring and keep doing it for perch, the other two change strategy but not target
change.gnx=data.frame(metier=c(rep('gns_fws',3), rep('gns_spf',3)), 
                      val=c(1,1,1,0,1,1))%>%
  dplyr::group_by(metier)%>%
  dplyr::summarise(val=mean(val))%>%
  dplyr::mutate(change=val-1)
coast.gnx=coast.met.cpt[coast.met.cpt$metier %in% c('gns_spf', 'gns_fws'),]
coast.gnx$prop=coast.gnx$cpt/sum(coast.gnx$cpt)
change.gnx=left_join(coast.gnx, change.gnx)
new.prop.spf=change.gnx[change.gnx$metier=='gns_spf', ]$prop*(1+change.gnx[change.gnx$metier=='gns_spf', ]$change)
new.prop.fws=change.gnx[change.gnx$metier=='gns_fws', ]$prop+(change.gnx[change.gnx$metier=='gns_spf', ]$prop-new.prop.spf)
new.prop.spf*sum(change.gnx$cpt)
new.prop.fws*sum(change.gnx$cpt)

df.var[df.var$strategy_to_change=='adapt' & 
         df.var$fishing_style=='small_scale'&
         df.var$extreme_event=='hws' &
         df.var$gear %in% c('gns_spf'),]$Freq=new.prop.spf*sum(change.gnx$cpt)
df.var[df.var$strategy_to_change=='adapt' & 
         df.var$fishing_style=='small_scale'&
         df.var$extreme_event=='hws' &
         df.var$gear %in% c('gns_fws'),]$Freq=new.prop.fws*sum(change.gnx$cpt)

## winter
df.var[df.var$strategy_to_change!='not_relevant' & 
         df.var$fishing_style%in%c('guide', 'recreational', 'small_scale')&
         df.var$extreme_event%in%c('hww'),]$Freq=c(0,0,0,0,0,1,0)

df.var[df.var$strategy_to_change!='not_relevant' & 
         df.var$fishing_style%in%c('guide', 'recreational', 'small_scale')&
         df.var$extreme_event%in%c('hww'),]$Freq=c(0,0,0,0,0,1,0)

df.var[df.var$strategy_to_change=='adapt' & df.var$fishing_style%in%c('guide', 'recreational')&
         df.var$extreme_event=='hww',]$Freq=c(0,0,0,0,1,0,0) # rod

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
pl=df.var%>%
  dplyr::filter(strategy_to_change=='adapt')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=gear))+
  geom_col()+
  facet_wrap(~fishing_style);pl
ggsave(plot=pl, 'results/images/gear.jpeg', width = 18, height = 8, units='cm', dpi=150)

gear.dataset=df.var%>%
  dplyr::filter(Freq >0, strategy_to_change!='not_relevant')%>%
  dplyr::distinct(gear, fishing_style)

df.var%>%
  #dplyr::filter(strategy_to_change=='adapt')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=gear))+
  geom_col()+
  facet_grid(rows=vars(fishing_style), cols=vars(strategy_to_change))

## target ####
i.node='target'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

df.var$Freq=c(rep(0,6),1) # the baseline is not better specified for all

# baseline target per gear
# species are  herring,perch, pike,  salmon, seatrout, whitefish, not specified
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='otm',]$Freq=c(1,0,0,0,0,0,0)
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='icefishing',]$Freq=c(0,0.5,0.5,0,0,0,0)
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='gns_spf',]$Freq=c(1,0,0,0,0,0,0) # only herring

round(coast.catch[coast.catch$metier=='GNS_FWS',]$prop, digits=2)
sum(round(coast.catch[coast.catch$metier=='GNS_FWS',]$prop, digits=2))
0.33+0.09# whitefish
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='gns_fws',]$Freq=c(0,0.45,0,0,0,0.42,0.13)

round(coast.catch[coast.catch$metier=='FPN_ANA',]$prop, digits=2)
sum(round(coast.catch[coast.catch$metier=='FPN_ANA',]$prop, digits=2))
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='fpn_ana',]$Freq=c(0,0,0,0.66,0.06,0.23,0.05)

rec.catch
df.var[df.var$strategy_to_change!='not_relevant' & df.var$gear=='rod',]$Freq=c(0.07,0.29,0.18,0.05,0.13,0.03,0.25)

pl=df.var%>%
  dplyr::filter(strategy_to_change=='cope')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=target))+
  geom_col()+
  facet_wrap(~gear);pl

# change for recreationals
change.rec=data.frame(target=unique(df.var$target), 
                      val=c(NA,(5-1)/(5-1),(2-1)/(5-1),(1-1)/(5-1),(5-1)/(5-1),NA,NA))%>%
  dplyr::group_by(target)%>%
  dplyr::summarise(val=mean(val))%>%
  dplyr::mutate(change=val-1)
rec.catch=df.var[df.var$strategy_to_change=='adapt' & 
                   df.var$gear=='rod' &
                   df.var$extreme_event=='hws',]

change.rec=left_join( rec.catch, change.rec)
rec.change=change.rec[!is.na(change.rec$change),]
rec.change$prop=rec.change$Freq/sum(rec.change$Freq)
rec.neg=rec.change[rec.change$change <0 ,]
rec.pos=rec.change[rec.change$change >=0,]
rec.neg$new.prop=rec.neg$prop*(1+rec.neg$change)
rec.pos$new.prop=rec.pos$prop+(sum(rec.neg$prop)-sum(rec.neg$new.prop))*(rec.pos$prop/sum(rec.pos$prop))
change.rec=rbind(rec.pos, rec.neg)
change.rec$new.freq=change.rec$new.prop*sum(rec.change$Freq)
change.rec=change.rec[,c('target', 'new.freq')]
change.rec=left_join( rec.catch, change.rec)
change.rec$new.freq=ifelse(is.na(change.rec$new.freq),change.rec$Freq ,change.rec$new.freq)



df.var[df.var$strategy_to_change=='adapt' & 
         df.var$gear=='rod' &
         df.var$extreme_event=='hws',]$Freq=change.rec$new.freq # goes more for perch

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(strategy_to_change=='adapt')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=target))+
  geom_col()+
  facet_wrap(~gear);pl

ggsave(plot=pl, 'results/images/target.jpeg', width = 18, height = 8, units='cm', dpi=150)

target.prob=df.var%>%
  dplyr::filter(strategy_to_change=='cope')%>%
  distinct(target, gear, Freq)%>%
  dplyr::filter(Freq>0)

gear.target.prob=full_join(gear.prob, target.prob, by='gear')%>%
  dplyr::mutate(Freq=Freq.x * Freq.y)%>%
  dplyr::group_by(fishing_style, target)%>%
  dplyr::summarise(Freq=sum(Freq))%>%
  arrange(target)%>%
  dplyr::filter(!is.na(fishing_style))%>%
  dplyr::group_by(target)%>%
  dplyr::mutate(importance=Freq/sum(Freq))

## strategy ####
i.node='strategy_to_change'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
df.var$Freq=c(1,0,0,0)

x.strat=strategy.dataset[!is.na(strategy.dataset$strategy_node),c('id_I', 'id_q', 'target', 'gear', 'event_code', 'strategy_value')]

x.strat=left_join(x.strat, int.dat[,c('id_I', 'id_q', 'target', 'gear', 'event_code', 'value','value.norm', 'unc.norm')],
          by=c('id_I', 'id_q', 'event_code','target', 'gear'))
x.strat=x.strat[x.strat$strategy_value!='C',]
x.strat[is.na(x.strat$value),c('value', 'value.norm', 'unc.norm')]=c(2,0.125,0.25)

x.strat=x.strat%>%
  dplyr::group_by(id_I, id_q , strategy_value, event_code)%>%
  slice_max(value.norm,n=1)%>%
  dplyr::summarise(value.norm=mean(value.norm), unc.norm=mean(unc.norm))# if there are multiple species per gear, pick the most probable
x.strat=x.strat%>%left_join(fish.simple, by='id_I')

x.strat=uncertainty.function.2(x.strat, col.selection = c('fishing_style', 'event_code', 'strategy_value'))
x.strat[x.strat$fishing_style=='small_scale',]$value.norm=x.strat[x.strat$fishing_style=='small_scale',]$value.norm*(x.strat[x.strat$fishing_style=='small_scale',]$weight)/3

x.strat=x.strat%>%
  dplyr::group_by(fishing_style, event_code)%>%
  dplyr::mutate(unc.norm=mean(unc.norm))%>%
  select(-c(uncertainty.obs, uncertainty.decl, weight))%>%
  pivot_wider(names_from = strategy_value, values_from = value.norm)%>%
  replace(is.na(.),0)

S <- x.strat$A + x.strat$R
idx <- S > 1
x.strat[idx, c("A", "R")] <- x.strat[idx, c("A", "R")] / S[idx]
if(nrow(x.strat[-which((x.strat$A+x.strat$R)==0),])>0){
  x.strat=x.strat[-which((x.strat$A+x.strat$R)==0),]
}


x.strat=x.strat%>%
  dplyr::mutate(C=1-(A+R), NR=0, extreme_event=event_code)

if(sim.uncertainty==1){
  prior = rep(0.5, 3)
  x.strat[,c('A','R','C')]=t(mapply(function(u, A, R,C){
    dir.draws(kappa = 20*(1-u)+prior,p = c(A, R,C), n = 1)}, 
    x.strat$unc.norm, x.strat$A, x.strat$R, x.strat$C))
}

df.cpt=df.var%>%
  pivot_wider(names_from = i.node, values_from = 'Freq')
df.cpt=df.cpt%>%
  left_join(x.strat[, c('fishing_style', 'extreme_event' ,'A','R','C')], by=c('fishing_style', 'extreme_event'))

df.cpt$adapt=ifelse(!is.na(df.cpt$A), df.cpt$A, df.cpt$adapt)
df.cpt$react=ifelse(!is.na(df.cpt$R), df.cpt$R, df.cpt$react)
df.cpt$cope=ifelse(!is.na(df.cpt$C), df.cpt$C, df.cpt$cope)
not.rel.events=paste(evt.relevance$short_description,evt.relevance$event_code)
df.cpt[-which(paste(df.cpt$fishing_style, df.cpt$extreme_event) %in% not.rel.events),]$not_relevant=1
df.cpt[-which(paste(df.cpt$fishing_style, df.cpt$extreme_event) %in% not.rel.events),]$cope=0
df.cpt[-which(paste(df.cpt$fishing_style, df.cpt$extreme_event) %in% not.rel.events),]$adapt=0
df.cpt[-which(paste(df.cpt$fishing_style, df.cpt$extreme_event) %in% not.rel.events),]$react=0

df.cpt=df.cpt%>%
  pivot_longer(cols=c('adapt', 'react','cope','not_relevant'),values_to = 'Freq', names_to = i.node)

df.cpt=re.format.cpt(df.cpt , df.var)
df.var$Freq=df.cpt$Freq

pl=df.var%>%
  ggplot(aes(x=fishing_style, y=Freq, fill=strategy_to_change))+
  geom_col()+
  facet_grid(cols=vars(extreme_event));pl
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
ggsave(plot=pl, 'results/images/strategy.jpeg', width = 18, height = 8, units='cm', dpi=150)


## go fishing ####
#sim.uncertainty=0
i.node='go_fishing'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
ini=Sys.time()
exam=int.dat[grep(i.node, int.dat$short_description), c('gear','target','event_code', 'id_I','value.norm', 'unc.norm')]

exam=left_join(exam, fish.simple, by='id_I')
names(exam)[3]='extreme_event'

exam%>%
  #distinct(id_I, extreme_event)%>%
  dplyr::group_by(id_I, extreme_event)%>%
  tally()%>%
  arrange(desc(n))

## decisions to take
# for the guide we use value for ice fishing
exam=exam[-which(exam$fishing_style=='guide'& exam$extreme_event=='hww' & exam$gear=='rod'),]

# for other we should balance the value for the frequency they use 
ssf.selection=exam[which(exam$id_I%in% c(5,7)& exam$extreme_event=='hws'),]%>%
  left_join(gear.target.prob, by=c('target', 'fishing_style'))%>%
  na.omit()%>%
  dplyr::group_by(id_I)%>%
  dplyr::mutate(importance=((importance)/sum(importance)))%>%
  mutate(value.norm=sum(value.norm*importance),
         unc.norm=sum(unc.norm*importance))%>%
  select(names(exam))%>%
  distinct(id_I, .keep_all = T)

exam=rbind(exam[-which(exam$id_I%in% c(5,7)& exam$extreme_event=='hws'),],ssf.selection)

# move target and gear at the beginning, we ight need it
exam=uncertainty.function.2(exam, col.selection = c('extreme_event', 'fishing_style'))

df.cpt=df.var%>%
  pivot_wider(names_from = i.node, values_from = 'Freq')%>%
  left_join(exam, by=c('fishing_style', 'extreme_event'))%>%
  replace(is.na(.),0)

vec.decisions=data.frame(t(sapply(df.cpt$value.norm, answ.cpt.det, categories = c('no','yes'))))
df.cpt[,c('no','yes')]=vec.decisions

if(sim.uncertainty==1){
  prior = rep(0.1, length(x.lev))
  df.cpt[,c('no','yes')]=t(mapply(function(u, no, yes){
  dir.draws(kappa = 20*(1-u)+prior,p = c(no, yes), n = 1)}, 
  df.cpt$unc.norm, df.cpt$no, df.cpt$yes))
  }

df.cpt=df.cpt%>%
  pivot_longer(cols=c('no', 'yes'),
               values_to = 'Freq',
               names_to = i.node)

# re-format
df.cpt=re.format.cpt(df.cpt , df.var)
# assign and aet other categories
df.var$Freq=df.cpt$Freq
df.var[df.var$strategy_to_change=='not_relevant',]$Freq=c(0.5,0.5)
df.var[df.var$strategy_to_change=='adapt',]$Freq=c(0,1)
df.var[df.var$strategy_to_change=='react',]$Freq=c(0,1)
df.var[df.var$aware_of_event=='no',]$Freq=c(0,1)
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


pl=df.var%>%
  dplyr::filter(strategy_to_change=='cope' & aware_of_event=='yes')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=go_fishing))+
  geom_col()+
  facet_wrap(~fishing_style);pl
ggsave(plot=pl, 'results/images/go_out.jpeg', width = 18, height = 8, units='cm', dpi=150)

# technical solutions ####
i.node='additional_mitigation'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
tech.sol=strategy.dataset[!is.na(strategy.dataset$solution) & nchar(strategy.dataset$solution)>1,]%>%
  distinct(style, id_I, id_q, solution, event_code, value)
df.var$Freq=c(1,0,0,0)
df.var[df.var$fishing_style=='trawler'&
         df.var$extreme_event %in% c('hws','hww')&
         df.var$strategy_to_change=='react',]$Freq=c(0,0,0,1)

# define the solutions for the baseline case
for(z in 1:length(styles$f.style)){
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$code==z.style$f.style,]
  i.fisher=unique(i.fisher$id_I)
  i.strategy=tech.sol[tech.sol$id_I %in% i.fisher,]
  i.strategy=i.strategy%>%distinct(event_code,id_I, id_q, solution, value)
  i.evts=unique(df.var$extreme_event)
  j=2
  for(j in 1:length(i.evts)){
    j.event=i.evts[j]
    j.strategy=i.strategy[i.strategy$event_code==i.evts[j],]
    
    if(nrow(j.strategy)==0){
      df.var[df.var$extreme_event==j.event & 
               df.var$fishing_style==styles[z,]$short_description &
               df.var$strategy_to_change=='react',]$Freq=c(1,0,0,0)
      next
    }
    
    # weight multiple strategies
    answ.range=questionnaire[questionnaire$id_q%in% j.strategy$id_q & !is.na(questionnaire$short_description),]
    j.strategy=j.strategy%>%
      left_join(answ.range)
    j.strategy=j.strategy[!is.na(j.strategy$value),]
    j.strategy$normalised=(j.strategy$value-j.strategy$min)/(j.strategy$max -j.strategy$min)
    j.strategy=j.strategy%>%
      dplyr::group_by(id_I, solution)%>%
      dplyr::summarise(normalised=mean(normalised))%>%
      dplyr::group_by(solution)%>%
      dplyr::summarise(normalised=sum(normalised))%>%
      dplyr::mutate(normalised=round(normalised/sum(normalised), digits=3))%>%
      arrange(desc(solution))
    mit=data.frame(mit=c('no','travel','short','other'), Freq=0)
    mit[mit$mit%in%j.strategy$solution,]$Freq=j.strategy$normalised
    
    df.var[df.var$extreme_event==j.event & 
             df.var$fishing_style==styles[z,]$short_description &
             df.var$strategy_to_change=='react',]$Freq=mit$Freq
  }
}
pl=df.var%>%
  ggplot(aes(x=strategy_to_change, y=Freq, fill=additional_mitigation))+
  geom_col()+
  facet_grid(cols=vars(fishing_style),
             rows=vars(extreme_event));pl
ggsave(plot=pl, 'results/images/mitigation.jpeg', width = 18, height = 8, units='cm', dpi=150)
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


## personal_safety ####
i.node='personal_safety'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])

i.evts=levels(df.var$extreme_event)
for(i in 1:length(i.evts)){
  j.event=i.evts[i]
  dat=int.dat[int.dat$event_code==j.event,]
  x.answ=dat[grep(i.node, dat$short_description),]
  x.answ=x.answ[!is.na(x.answ$value.norm),]
  # distribution across answers
  x.answ.complete=x.answ$value.norm
  # prob for each event
  i.gear=unique(df.var$gear)
  for(j in 1:length(i.gear)){
    i.fisher=gear.dataset[gear.dataset$gear==i.gear[j],] 
    i.fisher=unique(style.dataset[style.dataset$short_description %in% i.fisher$fishing_style,]$id_I)
    j.answ=x.answ[x.answ$id_I%in%i.fisher,]  
    
    if(nrow(j.answ)>1){j.answ=uncertainty.function(j.answ)}
    
    #i.cpt=answer.to.cpt(x.ans=j.answ$value.norm, 
    #                    unc=0.2*j.answ$unc.norm+0.001,
    #                    dim.name = i.node,
    #                    dim.labs = x.lev,
    #                    priors = x.answ.complete)
    
    i.cpt=answ.cpt.det(j.answ$value.norm, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(xnam))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1-j.answ$unc.norm)+prior)
    }
    df.var[df.var$extreme_event==j.event & df.var$gear== i.gear[j],]$Freq=i.cpt  
  }
}

# mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0)
df.var[df.var$extreme_event %in% c('sto','gal', 'hww') & 
           df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0)

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  #dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=personal_safety))+
  geom_col()+
  facet_grid(rows=vars(additional_mitigation), cols=vars(gear));pl

ggsave(plot=pl, 'results/images/safety.jpeg', width = 18, height = 8, units='cm', dpi=150)

## damage ####
i.node='damage'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])

i.evts=levels(df.var$extreme_event)
for(i in 1:length(i.evts)){
  j.event=i.evts[i]
  dat=int.dat[int.dat$event_code==j.event,]
  x.answ=dat[grep(i.node, dat$short_description),]
  x.answ=x.answ[!is.na(x.answ$value.norm),]
  # distribution across answers
  x.answ.complete=x.answ$value.norm
  # prob for each event
  i.gear=unique(df.var$gear)
  for(j in 1:length(i.gear)){
    i.fisher=gear.dataset[gear.dataset$gear==i.gear[j],] 
    i.fisher=unique(style.dataset[style.dataset$short_description %in% i.fisher$fishing_style,]$id_I)
    j.answ=x.answ[x.answ$id_I%in%i.fisher,]  
    
    if(nrow(j.answ)>1){j.answ=uncertainty.function(j.answ)}
    
    #i.cpt=answer.to.cpt(x.ans=j.answ$value.norm, 
    #                    unc=0.2*j.answ$unc.norm+0.001,
    #                    dim.name = i.node,
    #                    dim.labs = x.lev,
    #                    priors = x.answ.complete)
    i.cpt=answ.cpt.det(j.answ$value.norm, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(xnam))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-j.answ$unc.norm)+prior)
    }
    
    df.var[df.var$extreme_event==j.event & df.var$gear== i.gear[j],]$Freq=i.cpt  
  }
}

# mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0,0)
df.var[df.var$extreme_event %in% c('sto','gal', 'hww') & 
         df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0,0)
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

pl=df.var%>%
  dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=extreme_event, y=Freq, fill=damage))+
  geom_col()+
  facet_wrap(~gear);pl
ggsave(plot=pl, 'results/images/damage.jpeg', width = 18, height = 8, units='cm', dpi=150)


## catch_condition ####
# nobody answered anything better for catch condition. (max is 3)
i.node='catch_condition'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
answ.range=questionnaire[questionnaire$short_description==i.node & !is.na(questionnaire$short_description),]
dat=int.dat
x.answ=dat[grep(i.node, dat$short_description),]

# prob for each event
i.evts=unique(df.var$extreme_event)
i.target=unique(df.var$target)

gear.style.prob=style.dataset%>%
  dplyr::mutate(fishing_style=short_description)%>%
  dplyr::select(fishing_style, id_I)%>%
  right_join(gear.target.prob)

j=4
for(j in 1:length(i.evts)){
  
  j.event=i.evts[j]
  j.answ=x.answ[x.answ$event_code==j.event,]  
  j.answ=j.answ[abs(j.answ$value)<=5,c('gear', 'area', 'target', 'id_I', 'value.norm', 'unc.norm')]
  j.answ=j.answ%>%right_join(gear.style.prob, by='id_I')%>%
    dplyr::filter(!is.na(value.norm))
  
  ## keep working on revision from here
  for(i in 1:length(i.target)){
    i.answ=j.answ[j.answ$target.y==i.target[i],]
    ## if some fisher mention specific for a species, then exclude the not specified for the same fisher
    specific.answers=i.answ[i.answ$target.x==i.answ$target.y,]
    if(nrow(specific.answers)>0){
    i.answ=rbind(specific.answers,
          i.answ[-which(i.answ$id_I %in% specific.answers$id_I),])
    }
    i.val=(sum(i.answ$value.norm*i.answ$importance))/sum(i.answ$importance)
    i.unc.obs=abs(sum((i.answ$value.norm-sd(i.answ$value.norm))*i.answ$importance))/sum(i.answ$importance)
    i.unc.obs=ifelse(is.na(i.unc.obs), 0.01,i.unc.obs)
    i.unc.decl=(sum(i.answ$unc.norm*i.answ$importance))/sum(i.answ$importance)
    i.unc=min(c(max(c(i.unc.obs, i.unc.decl))))
    
    #i.cpt=answer.to.cpt(x.ans=i.val/0.5, 
    #                    unc=i.unc*0.25,
    #                    dim.name = i.node,
    #                    dim.labs = x.lev,
    #                    priors = unique(i.answ$value.norm))
    i.cpt=answ.cpt.det(i.val/0.5, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(xnam))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-i.unc)+prior)
    }
    
    df.var[df.var$extreme_event==j.event & df.var$target == i.target[i],]$Freq=i.cpt  
  }
}

pl=df.var%>%
  #dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=target, y=Freq, fill=catch_condition))+
  geom_col()+
  facet_grid(rows=vars(additional_mitigation), cols=vars(extreme_event));pl

# mitigations for catch conditions are many
solutions=strategy.dataset[!is.na(strategy.dataset$solution) & 
                             abs(strategy.dataset$value)<=5 & strategy.dataset$short_description=='cope_condition',]
solutions=solutions[nchar(solutions$solution)>4&!is.na(solutions$solution),]

df.var[df.var$extreme_event=='hws' & df.var$additional_mitigation=='other_technical',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event=='hws' & df.var$additional_mitigation=='short_sets',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event%in%c('hws', 'sto', 'gal') & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event=='abl' & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1) # ice machine 

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
pl=df.var%>%
  #dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=target, y=Freq, fill=catch_condition))+
  geom_col()+
  facet_grid(rows=vars(additional_mitigation), cols=vars(extreme_event));pl
ggsave(plot=pl, 'results/images/catch_condition.jpeg', width = 18, height = 8, units='cm', dpi=150)


## catchability ####
i.node='catchability'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
answ.range=questionnaire[questionnaire$short_description==i.node & !is.na(questionnaire$short_description),]
dat=int.dat
x.answ=dat[grep(i.node, dat$short_description),]
x.answ=x.answ[is.na(x.answ$unit_range),]
# prob for each event
i.evts=unique(df.var$extreme_event)
i.target=unique(df.var$target)
j=1

style.df2=style.dataset[,c('id_I','short_description')]
names(style.df2)[2]='fishing_style'
style.df2=style.df2%>%left_join(gear.prob)
df.var$Freq=c(0,1,0)

j=5
for(j in 1:length(i.evts)){
  
  j.event=as.character(i.evts[j])
  j.answ=x.answ[x.answ$event_code==j.event,]  
  j.answ=j.answ[abs(j.answ$value)<=5,c('gear', 'area', 'target', 'id_I', 'value.norm', 'unc.norm')]
  j.answ=j.answ%>%right_join(gear.style.prob, by='id_I')%>%
    dplyr::filter(!is.na(value.norm))
  
  i.target=as.character(unique(j.answ$target.y))
  
  ## keep working on revision from here
  i=3
  for(i in 1:length(i.target)){
    i.answ=j.answ[j.answ$target.y==i.target[i],]
    i.answ=i.answ%>%left_join(style.df2, by='id_I')
    i.gear=as.character(unique(i.answ$gear.y))
 
    for(k in 1:length(i.gear)){
      k.answ=i.answ[i.answ$gear.y==i.gear[k],]
    ## if some fisher mention specific for a species, then exclude the not specified for the same fisher
      specific.answers=k.answ[k.answ$target.x==k.answ$target.y,]
      if(nrow(specific.answers)>0){
        specific.answers$importance=specific.answers$importance*5
        k.answ=rbind(specific.answers,
                     k.answ[-which(k.answ$id_I %in% specific.answers$id_I),])
      }
      
      i.val=(sum(k.answ$value.norm*k.answ$importance))/sum(k.answ$importance)
      i.unc.obs=abs(sum((k.answ$value.norm-sd(k.answ$value.norm))*k.answ$importance))/sum(k.answ$importance)
      i.unc.obs=ifelse(is.na(i.unc.obs), 0.01,i.unc.obs)
      i.unc.decl=(sum(k.answ$unc.norm*k.answ$importance))/sum(k.answ$importance)
      i.unc=min(1,c(max(c(i.unc.obs, i.unc.decl))))
      
      #i.cpt=answer.to.cpt(x.ans=i.val, 
      #                    unc=i.unc*0.25,
      #                    dim.name = i.node,
      #                    dim.labs = x.lev,
      #                    priors = unique(i.answ$value.norm))
      i.cpt=answ.cpt.det(i.val, x.lev)
      if(sim.uncertainty==1){
        prior = rep(0.1, length(xnam))
        i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-i.unc)+prior)
      }
 
    df.var[df.var$extreme_event==as.character(j.event) & 
             df.var$target == as.character(i.target[i])&
             df.var$gear ==  as.character(i.gear[k]),]$Freq=i.cpt  

  }
 }
}

pl=df.var%>%
  #dplyr::filter(additional_mitigation=='no')%>%
  ggplot(aes(x=target, y=Freq, fill=catchability))+
  geom_col()+
  facet_grid( cols=vars(extreme_event), rows=vars(gear));pl

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

# stress ####
i.node='stress'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

for(z in 1:length(styles$f.style)){
  
  z.style=styles[z,]
  i.fisher=style.dataset[style.dataset$code==z.style$f.style,]
  i.fisher=unique(i.fisher$id_I)
  dat=int.dat[int.dat$id_I%in%i.fisher,]
  
  x.answ=dat[grep(i.node, dat$short_description),]%>%
    arrange(desc(id_sub))
  x.answ$short_description=x.answ$id_sub
  
  if(nrow(x.answ)>3){
    x.answ=uncertainty.function(x.answ)
  }
  
  supp.data=data.frame(node=c('catch_condition',  'damage', 'personal_safety','stress'), 
                       link.w=c(x.answ$value.norm, 0),
                       states=c(2,4,3,2),
                       type=c('parent','parent','parent','child'),
                       direction=c('neg', 'pos','pos', 'pos'),
                       id=1:4)
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = unique(x.answ$unc.norm), algorithm = 'min', xnam=xnam)
  x.cpt[[1]]%>%
    pivot_longer(cols=c('yes','no'), names_to = 'stress')%>%
    dplyr::filter(stress=='yes')%>%
    ggplot(aes(x= personal_safety, y=damage, fill=value))+
    geom_tile()+
    facet_wrap(~catch_condition)+
    scale_fill_viridis_c()
  
  z.cpt=x.cpt[[1]]%>%
    pivot_longer(cols=c('no','yes'), names_to = 'stress')%>%
    arrange(desc(catch_condition))
  
  df.var[df.var$fishing_style==styles[z,]$short_description,]$Freq=z.cpt$value
  
  
}

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
df.var%>%
  #dplyr::filter(fishing_style=='small_scale')%>%
  ggplot(aes(x=paste(catch_condition, personal_safety), y=Freq, fill=stress))+
  geom_col()+
  theme(axis.text.x = element_text(angle=45))+
  facet_grid(cols=vars(fishing_style), rows=vars(damage))
bnlearn::write.net( 'data/networks/BEWARE_r1_learn_pt1.net', net)





















