remove(list=ls())
ini=Sys.time()
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)
library(bnlearn)
library(gRain)
library(profvis)
source('code/supporting_r1.R')
sim.uncertainty=0
show.plots=0

# Load data ####
# data from interviews
questionnaire=read_excel("data/editable_files/dialogues_raw.xlsx", 
                         sheet = "questions")
style.dataset=read_excel("data/editable_files/dialogues_raw.xlsx", 
                         sheet = "fishers")
evts=readxl::read_excel(file.path(scriptDir, '..', 'data', 'editable_files/dialogues_raw.xlsx'), 
                        sheet = "events")
int.dat=read_csv("data/read_only/coding_report_unc.csv")
strategy.dataset=read.csv("data/editable_files/adaptive_revised_v2.csv")
strategy.dataset$gear=ifelse(is.na(strategy.dataset$gear), 'not_specified', strategy.dataset$gear)
strategy.dataset$target=ifelse(is.na(strategy.dataset$target), 'not_specified', strategy.dataset$target)
evt.relevance=read_csv("data/read_only/relevance.csv")

# data from literature
baseline_gear=read_csv("data/read_only/baseline_gear.csv")
baseline_target=read_csv("data/read_only/baseline_target.csv")
coast.catch=read_csv("data/read_only/coastal_catch.csv")
rec.catch=read.csv("C:/github/extreme_weather_risk/data/fisheries_statistics/rec_catch_SWE.csv")
rec.catch=rec.catch[rec.catch$name %in% c('Perch', 'Pike', 'Salmon', 'Zander', 'Whitefish', 'Trout', 'Herring','Cod',
                                          'Not specified'),]
rec.catch$prop=rec.catch$practitioners/sum(rec.catch$practitioners)

# DAG
net=read.net('data/editable_files/networks/BEWARE_release_v1_0_0.net', debug = F)


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

profvis({
# CPT filling ####
x.nodes=nodes(net)

# decision nodes ####
net <- initialize_equal_probabilities(net, c('extreme_event', 'fishing_style', 'aware_of_event')) # set defaults as equal probabilities for all the levels

# user####
## gear ####
i.node='gear'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

## assign baseline probabilities
df.var=df.var%>%
  left_join(baseline_gear, by =c('gear', 'fishing_style'))%>%
  dplyr::mutate(Freq=prob)%>%
  dplyr::select(-prob)

# save this for later
gear.prob=df.var%>%
  dplyr::filter(strategy_to_change=='cope')%>%
  distinct(fishing_style, gear, Freq)%>%
  dplyr::filter(Freq>0)

## winter: going icefishing
df.var[df.var$fishing_style%in%c('guide', 'recreational', 'small_scale')&
         df.var$extreme_event%in%c('hww'),]$Freq=c(0,0,0,0,0,1,0)

# hard assignation of probabilities when adaptive strategies are applied
## recreational and guides goes inland when abl happen
df.var[df.var$strategy_to_change=='adapt' & 
         df.var$fishing_style%in% c('recreational', 'guide')&
         df.var$extreme_event=='abl',]$Freq=c(0,0,0,0,0,0,1) # goes inland

## recreational and guides switch to fishing in open waters (lhp) when hww happen
df.var[df.var$strategy_to_change=='adapt' & df.var$fishing_style%in%c('guide', 'recreational')&
         df.var$extreme_event=='hww',]$Freq=c(0,0,0,0,1,0,0) # lhp

# one small scale fisher mention that he stop fishing for herring and keep doing it for perch, the other two change strategy but not target
change.gnx=baseline_gear[baseline_gear$gear %in% c('gns_spf', 'gns_fws') & baseline_gear$fishing_style=='small_scale',]
new.prop.spf=change.gnx[change.gnx$gear=='gns_spf', ]$prob*2/3
new.prop.fws=change.gnx[change.gnx$gear=='gns_fws', ]$prob+(change.gnx[change.gnx$gear=='gns_spf', ]$prob-new.prop.spf)

df.var[df.var$strategy_to_change=='adapt' &  df.var$fishing_style=='small_scale'& df.var$extreme_event=='hws' &
         df.var$gear %in% c('gns_spf', 'gns_fws'),]$Freq=c(new.prop.spf,new.prop.fws)

# save this for later
gear.dataset=df.var%>%
  dplyr::filter(Freq >0, strategy_to_change!='not_relevant')%>%
  dplyr::distinct(gear, fishing_style)

#¨write into the net
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)


## target ####
i.node='target'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

## assign baseline probabilities
df.var=df.var%>%
  left_join(baseline_target, by =c('gear', 'target'))%>%
  dplyr::mutate(Freq=prob)%>%
  dplyr::select(-prob)

# hard assignation of probabilities when adaptive strategies are applied
# change for recreationals
change.rec=df.var[df.var$strategy_to_change=='adapt' & 
                    df.var$gear=='lhp' &
                    df.var$extreme_event=='hws',]
change.val=int.dat[int.dat$id_q==2 & int.dat$id_sub=='b' & int.dat$id_I==5,c('target', 'value')]
change.val[change.val$target=='zander',]$target='not_specified'
change.rec=left_join(change.rec, change.val)
change.rec$value=(change.rec$value-1)/(5-1) # re scale to 0-1
change.rec[is.na(change.rec$value),]$value=0
change.rec$value=change.rec$value/sum(change.rec$value)
df.var[df.var$strategy_to_change=='adapt' & 
         df.var$gear=='lhp' &
         df.var$extreme_event=='hws',]$Freq=change.rec$value# goes more for perch

# save this for later
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

#¨write into the net
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

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
  dplyr::summarise(value.norm=mean(value.norm), unc.norm=mean(unc.norm)) # if there are multiple species per gear, pick the most probable
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

## go fishing ####
i.node='go_fishing'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])

## hard coding decisions in case there are multiple declarations
go.fish.duplicate=int.dat[grep(i.node, int.dat$short_description), 
                          c('gear','target','event_code', 'id_I','value.norm', 'unc.norm')]
go.fish.duplicate=left_join(go.fish.duplicate, fish.simple, by='id_I')
names(go.fish.duplicate)[3]='extreme_event'

# for the guide we use value for ice fishing
go.fish.duplicate=go.fish.duplicate[-which(go.fish.duplicate$fishing_style=='guide'& 
                                             go.fish.duplicate$extreme_event=='hww' & 
                                             go.fish.duplicate$gear=='rod'),]

# for other we should balance the value for the frequency they use 
ssf.selection=go.fish.duplicate[which(go.fish.duplicate$id_I%in% c(5,7)& go.fish.duplicate$extreme_event=='hws'),]%>%
  left_join(gear.target.prob, by=c('target', 'fishing_style'))%>%
  dplyr::filter(!is.na(value.norm))%>%
  dplyr::filter(!is.na(importance))%>%
  dplyr::group_by(id_I)%>%
  dplyr::mutate(importance=((importance)/sum(importance)))%>%
  mutate(value.norm=sum(value.norm*importance),
         unc.norm=sum(unc.norm*importance))%>%
  select(names(go.fish.duplicate))%>%
  distinct(id_I, .keep_all = T)

go.fish=rbind(go.fish.duplicate[-which(go.fish.duplicate$id_I%in% c(5,7) & go.fish.duplicate$extreme_event=='hws'),],
              ssf.selection)

# move target and gear at the beginning, we ight need it
go.fish=uncertainty.function.2(go.fish[!is.na(go.fish$value.norm),], col.selection = c('extreme_event', 'fishing_style'))

df.cpt=df.var%>%
  pivot_wider(names_from = i.node, values_from = 'Freq')%>%
  left_join(go.fish, by=c('fishing_style', 'extreme_event'))%>%
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
df.var$Freq=df.cpt$Freq
df.var[df.var$strategy_to_change=='not_relevant',]$Freq=c(0.5,0.5)
df.var[df.var$strategy_to_change=='adapt',]$Freq=c(0,1)
df.var[df.var$strategy_to_change=='react',]$Freq=c(0,1)
df.var[df.var$aware_of_event=='no',]$Freq=c(0,1)
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

# technical solutions ####
i.node='additional_mitigation'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
df.var$Freq=c(1,0,0,0)# set baseline

# fill strategies when react
tech.sol=strategy.dataset[!is.na(strategy.dataset$solution) & nchar(strategy.dataset$solution)>1,]%>%
  distinct(style, id_I, id_q, solution, event_code, value)%>%
  left_join(questionnaire[,c('id_q', 'min','max')], by='id_q')%>%
  dplyr::filter(!is.na(value))
tech.sol$normalised=(tech.sol$value-tech.sol$min)/(tech.sol$max -tech.sol$min)
tech.sol=tech.sol%>%
  dplyr::group_by(id_I, style, event_code, solution)%>%
  dplyr::summarise(normalised=mean(normalised)) # average when multiple answers by fishers available

tech.sol=tech.sol%>%
  dplyr::group_by(extreme_event=event_code, fishing_style=style, additional_mitigation=solution)%>%
  dplyr::summarise(normalised=mean(normalised))%>%
  dplyr::mutate(normalised=round(normalised/sum(normalised), digits=3))%>%
  arrange(desc(additional_mitigation))

react.strategy=df.var[df.var$strategy_to_change=='react',]%>%
  left_join(tech.sol)%>%
  replace(is.na(.),0)%>%
  dplyr::group_by(fishing_style, extreme_event)%>%
  dplyr::mutate(prob=ifelse(additional_mitigation=='no',1-sum(normalised), normalised))
react.strategy=as.data.frame(react.strategy)
react.strategy[react.strategy$fishing_style=='trawler'&
                 react.strategy$extreme_event %in% c('hws','hww')&
                 react.strategy$strategy_to_change=='react',]$prob=c(0,0,0,1)
df.var[df.var$strategy_to_change=='react',]$Freq=react.strategy$prob

#¨write into the net
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
    i.cpt=answ.cpt.det(j.answ$value.norm, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(x.lev))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1-j.answ$unc.norm)+prior)
    }
    df.var[df.var$extreme_event==j.event & df.var$gear== i.gear[j],]$Freq=as.numeric(i.cpt)  
  }
}

# mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0)
df.var[df.var$extreme_event %in% c('sto','gal', 'hww') & 
         df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0)

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

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
    i.cpt=answ.cpt.det(j.answ$value.norm, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(x.lev))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-j.answ$unc.norm)+prior)
    }
    df.var[df.var$extreme_event==j.event & df.var$gear== i.gear[j],]$Freq=as.numeric(i.cpt)  
  }
}

# mitigations for safety are to travel further. Also, inland and not relevant equal no safety concerns
df.var[df.var$gear %in% c('not_relevant', 'inland'),]$Freq=c(1,0,0,0)
df.var[df.var$extreme_event %in% c('sto','gal', 'hww') & 
         df.var$additional_mitigation=='travel_further',]$Freq=c(1,0,0,0)
net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

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
    
    i.cpt=answ.cpt.det(i.val/0.5, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(x.lev))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-i.unc)+prior)
    }
    
    df.var[df.var$extreme_event==j.event & df.var$target == i.target[i],]$Freq=as.numeric(i.cpt)  
  }
}

# mitigations for catch conditions are many
solutions=strategy.dataset[!is.na(strategy.dataset$solution) & 
                             abs(strategy.dataset$value)<=5 & strategy.dataset$short_description=='cope_condition',]
solutions=solutions[nchar(solutions$solution)>4&!is.na(solutions$solution),]

df.var[df.var$extreme_event=='hws' & df.var$additional_mitigation=='other_technical',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event=='hws' & df.var$additional_mitigation=='short_sets',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event%in%c('hws', 'sto', 'gal') & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1) # ice machine 
df.var[df.var$extreme_event=='abl' & df.var$additional_mitigation=='travel_further',]$Freq=c(0,1) # ice machine 

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

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

style.df2=style.dataset[,c('id_I','short_description')]
names(style.df2)[2]='fishing_style'
style.df2=style.df2%>%left_join(gear.prob)
df.var$Freq=c(0,1,0)

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
      
      i.cpt=answ.cpt.det(i.val, x.lev)
      if(sim.uncertainty==1){
        prior = rep(0.1, length(x.lev))
        i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-i.unc)+prior)
      }
      
      df.var[df.var$extreme_event==as.character(j.event) & 
               df.var$target == as.character(i.target[i])&
               df.var$gear ==  as.character(i.gear[k]),]$Freq=as.numeric(i.cpt)  
      
    }
  }
}

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

# Lower half ####
# data from interviews
names(style.dataset)[3]='fishing_style'
int.dat=read_csv("data/read_only/coding_report_unc.csv")
bn.desc=read_excel("data/editable_files/nodes_text.xlsx")
# data from literature
cost.dat=read_csv("data/read_only/cost_df_v2.csv")

# format data ####
int.dat=left_join(int.dat[,-which(colnames(int.dat)=='event_code')], evts, by='id_sub')
styles=net[['fishing_style']]$prob
styles=data.frame(styles)
names(styles)[1]='f.style'

# root nodes ####
root.nodes=bn.desc[bn.desc$group %in% c('economic', 'societal','individual'),]
root.nodes=root.nodes[root.nodes$cycle==2,]
root.nodes=root.nodes[-(grep('import', root.nodes$node)),]
x.nodes=nodes(net)
x.nodes=x.nodes[x.nodes %in% root.nodes$node]
x.nodes=x.nodes[x.nodes %in% nodes(net)]
hc.dat=int.dat[int.dat$short_description %in% x.nodes,]

hc.dat[hc.dat$uncertainty==-99.8 & !is.na(hc.dat$uncertainty),]$uncertainty=3/5 # this is the archipelago fisherman that does not know what to say
hc.dat=left_join(hc.dat,style.dataset[,c('id_I', "fishing_style")])

for(i in 1:length(x.nodes)){
  
  i.node=x.nodes[i]
  array.var=net[[i.node]]$prob
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,i.node])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]
  i.dat=hc.dat[hc.dat$short_description==i.node,]
  
  for(z in 1:length(styles$f.style)){
    dat=i.dat[i.dat$fishing_style==styles[z,]$f.style,]
    if(nrow(dat)>1){
      dat=uncertainty.function(dat) 
    }
    if(abs(dat$value)>5){
      df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=c(1,0,0)
      next
    }
    i.cpt=answ.cpt.det(dat$value.norm, x.lev)
    if(sim.uncertainty==1){
      prior = rep(0.1, length(x.lev))
      i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-dat$unc.norm)+prior)
    }
    
    
    df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=as.numeric(i.cpt)
  }
  
  net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

## substitution capacity ####
i.node='substitution_capacity'
array.var=net[[i.node]]$prob
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,i.node])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(i.node,'Freq'))]

i.dat=int.dat[int.dat$short_description==i.node,]%>%
  left_join(., style.dataset[,c('id_I', "fishing_style")])

for(z in 1:length(styles$f.style)){
  
  j.answ=i.dat[i.dat$fishing_style==styles[z,]$f.style,]
  
  if(nrow(j.answ)>1){
    j.answ=uncertainty.function(j.answ)
  }
  
  i.cpt=answ.cpt.det(j.answ$value.norm, x.lev)
  if(sim.uncertainty==1){
    i.cpt=dir.draws(p=i.cpt, kappa=20*(1.001-j.answ$unc.norm))
  }
  
  df.var[df.var$fishing_style==styles[z,]$f.style ,]$Freq=as.numeric(i.cpt)
}

net[[i.node]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

# Importance ####
imp.vars=c('societal_importance', 'individual_importance' ,'economic_buffers' )

for(i in 1:length(imp.vars)){
  
  x.var=imp.vars[i]
  array.var=net[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  x.lev=levels(df.var[,x.var])
  target.dims=names(df.var)
  target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
  link.w=c(rep(1, length(xdim)))
  if(x.var=='economic_buffers'){
    link.w=c(0.5,1,1)
  }
  supp.data=data.frame(node=c(target.dims,x.var), 
                       link.w=link.w,
                       states=c(xdim[2:length(xdim)], xdim[1]),
                       type=c(rep('parent', length(target.dims)) , 'child'),
                       direction=c(rep('pos', length(xdim))),
                       id=1:length(xdim))
  x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.05, algorithm = 'max', xnam=xnam)
  net[[x.var]]=x.cpt$bn.cpt
}

# health ####
x.var='health'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq'))]
direction=c('neg', 'neg','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,3,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
net[[x.var]]=x.cpt$bn.cpt

# non monetary value ####
x.var='non_monetary_value'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[1:3]
xnam=dimnames(array.var)[1:3]
df.var=as.data.frame(array.var)
x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_fishing', 'substitution_capacity'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,1,1),
                     states=c(xdim[2:length(xdim)], xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 

x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam)
x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('high','medium', 'low'), names_to = 'nonmonval')%>%
  dplyr::mutate(nonmonval=factor(nonmonval, levels=c('low','medium','high')))%>%
  arrange(satisfaction, health,(nonmonval))
df.var[df.var$go_fishing%in% c('yes'),]$Freq=x.cpt$value
df.var[df.var$go_fishing%in% c('no', 'not_relevant') & df.var$substitution_capacity=='no',]$Freq=c(1,0,0)
df.var[df.var$go_fishing%in% c('no', 'not_relevant') & df.var$substitution_capacity=='yes',]$Freq=c(0,1,0)

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=dimnames(array.var))

# realised catches ####
x.var='catches'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
df.var[df.var$go_fishing%in% c('no', 'not_relevant'),]$Freq=c(1,0,0,0)
df.var[df.var$go_fishing == 'yes' & df.var$catchability =='same',]$Freq=c(0,0,1,0)
df.var[df.var$go_fishing == 'yes' & df.var$catchability =='worse',]$Freq=c(0,1,0,0)
df.var[df.var$go_fishing == 'yes' & df.var$catchability =='better',]$Freq=c(0,0,0,1)
net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

## cost ####
x.var='costs'
array.var=net[[x.var]][['prob']]
xdim0=dim(array.var)
xdim=dim(array.var)[-c(4)]
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
i.gear=levels(df.var$fishing_style)

cost.dat.bck=cost.dat
cost.dat=cost.dat[-grep('miss', cost.dat$parent),]
parent.vec=strsplit(cost.dat$parent, '.', fixed = TRUE)
parent.vec=plyr::ldply(parent.vec)
cost.dat$damage=parent.vec[,1]
cost.dat$additional=parent.vec[,2]

## common
df.var[df.var$damage=='destroy',]$Freq=c(0,0,0,1)
df.var[df.var$damage=='no' & df.var$additional_mitigation!='travel_further',]$Freq=c(1,0,0,0)
base.grid=expand.grid(damage=c('no', 'minor','major'), additional=c('no', 'travel'), state=c('negligible','low', 'medium', 'high'))

## gear
for(i in 1:length(i.gear)){
  i.fisher=i.gear[i]
  dat=cost.dat[cost.dat$fishing_style%in%i.fisher,]
  dat=dat[,c('damage','additional','state', 'prob')]
  dat=full_join(base.grid, dat, by=c('damage', 'additional', 'state'))%>%
    replace(is.na(.),0)
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' & 
           df.var$damage=='no',]$Freq=dat[dat$damage=='no' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' &
           df.var$damage=='minor',]$Freq= dat[dat$damage=='minor' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation=='travel_further' & 
           df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='travel',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation!='travel_further' & 
           df.var$damage=='minor',]$Freq=dat[dat$damage=='minor' & dat$additional=='no',]$prob
  df.var[df.var$fishing_style==i.fisher & 
           df.var$additional_mitigation!='travel_further' & 
           df.var$damage=='major',]$Freq=dat[dat$damage=='major' & dat$additional=='no',]$prob
}

net[[x.var]]=array(df.var$Freq, dim=xdim0, dimnames=xnam)

# monetary loss ####
x.var='monetary_loss'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)

# set loss equal to cost
df.var[df.var$monetary_loss== df.var$costs,]$Freq=1
df.var[df.var$monetary_loss!= df.var$costs,]$Freq=0

# now adjust the cases when there is no fishing
df.var[df.var$go_fishing=='no' & df.var$fishing_style%in% c('recreational','household'),]$Freq=c(1,0,0,0)
df.var[df.var$go_fishing=='no' & df.var$substitution_capacity== 'yes',]$Freq=c(1,0,0,0)

i.gear=levels(df.var$fishing_style)
cost.dat=cost.dat.bck[grep('miss', cost.dat.bck$parent),]
parent.vec=strsplit(cost.dat$parent, '.', fixed = TRUE)
parent.vec=plyr::ldply(parent.vec)
cost.dat$damage=parent.vec[,1]
cost.dat$travel=parent.vec[,2]
base.prob=data.frame(state=c('negligible','low','medium','high'))

for(i in 1:length(i.gear)){
  
  i.fisher=i.gear[i]
  if(i.fisher %in% c('recreational','household')){next}
  dat=cost.dat[cost.dat$fishing_style%in%i.fisher,]
  dat=dat[dat$travel=='no' & dat$damage=='no',]
  dat$state=factor(dat$state, levels=c('negligible', 'low', 'medium', 'high'))
  dat=dat[order(dat$state),]
  dat=left_join(base.prob, dat)%>%
    replace(is.na(.),0)
  
  
  
  df.var[df.var$fishing_style==i.fisher & 
           df.var$go_fishing=='no' & 
           df.var$substitution_capacity=='no',]$Freq=dat$prob
  
}



net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=dimnames(array.var))

# satisfaction #### 
x.var='satisfaction'
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
xnam2=xnam
xnam2$catches=c("poor","average","good" )

df.var=as.data.frame(array.var)

x.lev=levels(df.var[,x.var])
target.dims=names(df.var)
target.dims=target.dims[-which(target.dims %in% c(x.var,'Freq', 'go_out'))]
direction=c('pos', 'pos','pos')

supp.data=data.frame(node=c(target.dims,x.var), 
                     link.w=c(1,0.5,1),
                     states=c(3,2, xdim[1]),
                     type=c(rep('parent', length(target.dims)) , 'child'),
                     direction=direction,
                     id=1:length(xdim)) 
x.cpt=rank.cpt(nodes.df = supp.data, uncertainty = 0.1, algorithm = 'min', xnam=xnam2)

x.cpt=x.cpt[[1]]%>%
  pivot_longer(cols=c('low','medium', 'high'), names_to = 'satisfaction')%>%
  dplyr::mutate(satisfaction=factor(satisfaction, levels=c('low','medium', 'high')))%>%
  arrange(catch_condition , catches ,(satisfaction))
df.var[df.var$catches!='no',]$Freq=x.cpt$value
df.var[df.var$catches=='no',]$Freq=c(1,0,0)

net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

# risks ####
risk.vars=c('societal_risk', 'individual_risk')

for(i in 1:length(risk.vars)){
  
  x.var=risk.vars[i]
  array.var=net[[x.var]][['prob']]
  xdim=dim(array.var)
  xnam=dimnames(array.var)
  df.var=as.data.frame(array.var)
  
  # convert to numeric
  x.val=df.var[,grep('monet', names(df.var))]  
  x.val=ifelse(x.val=='low',1,ifelse(x.val=='medium',0.5,0))
  
  x.imp=df.var[,grep('impor', names(df.var))]  
  x.imp=ifelse(x.imp=='high',1,ifelse(x.imp=='medium',0.5,0))
  
  x.risk=x.val*x.imp
  x.risk=cut(x.risk, c(-Inf,0.1,0.6,Inf), labels=c('low','medium','high'))
  
  df.var$risk2=x.risk
  x.risk.v=ifelse(as.character(df.var[,grep('_ri', names(df.var))])==as.character(df.var[,grep('k2', names(df.var))]),1,0)
  df.var$Freq=x.risk.v
  
  # plug back in CPT
  df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
  net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)
}

# economic risk
risk.vars=c('economic_risk')
x.var=risk.vars
array.var=net[[x.var]][['prob']]
xdim=dim(array.var)
xnam=dimnames(array.var)
df.var=as.data.frame(array.var)
x.val=df.var[,grep('monet', names(df.var))]  
x.val=ifelse(x.val=='high',1,ifelse(x.val=='medium',0.66,ifelse(x.val=='low',0.33,0)))
x.imp=df.var[,grep('buff', names(df.var))]  
x.imp=ifelse(x.imp=='high',1,ifelse(x.imp=='medium',0.5,0))
x.risk=x.val*(1-(x.imp*0.7))
x.risk=cut(x.risk, c(-Inf,0.25,0.5,Inf), labels=c('low','medium','high'))
df.var$risk2=x.risk
x.risk.v=ifelse(as.character(df.var[,grep('_ri', names(df.var))])==as.character(df.var[,grep('k2', names(df.var))]),1,0)
df.var$Freq=x.risk.v
df.var[df.var$strategy=='not_relevant',]$Freq=c(0,0,0,1)
net[[x.var]]=array(df.var$Freq, dim=xdim, dimnames=xnam)

#
if(sim.uncertainty==0){
  bnlearn::write.net( 'data/read_only/networks/BEWARE_learnt_r1_0_0.net', net)
}else{
  bnlearn::write.net( 'data/read_only/networks/iterations/BEWARE_learnt_r1_0_0_iter.net', net)
}

#save(net, file='data/networks/BEWARE_v3_learn_pt2.rdata')

})
fin=Sys.time()
fin-ini






















