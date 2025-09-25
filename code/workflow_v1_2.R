#setwd("~/ARMELLONI_SLU/RiskAnalysis/code")
rm(list = ls())
setwd("~/ARMELLONI_SLU/RiskAnalysis")
library(tidyverse)
library(Rgraphviz)
library(bnlearn)
source('code/CPT_algorithms/supporting_functions.R')


# scenario
manag='flexible'
n.events=15
surprise.ans= 3
crew.cost=5.76 # Q4
summary.path="~/ARMELLONI_SLU/RiskAnalysis/LEK/interviews_aug25/questionnaires_filled/questionnaires_coded.xlsx"

# answers ####
read.res=function(df, sheet){
  # read in
  int.results=readxl:: read_excel(df,sheet)
  # some format
  int.results$fishing_style=int.results[int.results$short_description=='fishing_style',]$broad
  int.results$broad=as.numeric(int.results$broad)
  int.results$domain=str_remove(int.results$category, 'hc_')
  int.results$uncertainty=NA
  # assign uncertainty
  ## previous version with manual code on human community
  #int.results[int.results$domain=='econ',]$uncertainty=as.numeric(int.results[int.results$id_q==26,]$broad)/10
  #int.results[int.results$domain=='personal',]$uncertainty=as.numeric(int.results[int.results$id_q==35,]$broad)/10
  #int.results[int.results$domain=='social',]$uncertainty=as.numeric(int.results[int.results$id_q==31,]$broad)/10
  ## new automatic version
  uncertainty.vals=int.results[grep(int.results$short_description, pattern='unc'),]
  uncertainty.id=str_split(uncertainty.vals$short_description, '_')
  for(i in 1:nrow(uncertainty.vals)){
    i.id=as.numeric(uncertainty.id[[i]][-1])
    i.unc=uncertainty.vals[i,]$broad/10
    if(length(i.id) ==2){
      int.results[int.results$id_q %in% i.id[1]:i.id[2],]$uncertainty=i.unc
    }
    if(length(i.id)==1){
      int.results[grep(int.results$id_q ,pattern= i.id[1]),]$uncertainty=i.unc
    }
  }
    # clean
  int.results[-which(int.results$id_q %in% uncertainty.vals$id_q),]
  return(int.results)
}

sheet.list=readxl::excel_sheets(summary.path)
int.summary=NULL
for(i in 1:length(sheet.list)){
  i.dat=read.res(df=summary.path, sheet=sheet.list[i])
  i.dat$fish_id=i
  int.summary=rbind(int.summary, i.dat)
}

## little uncertainty exploration
pgp=int.summary[int.summary$fishing_style=='coastal',]
pgeco=pgp[substr(pgp$category,1,2)=='hc',c('id_q', 'category', 'short_description','broad','uncertainty','fish_id')]%>%
  dplyr::filter(!is.na(uncertainty))%>%
  arrange(as.numeric(id_q), fish_id)%>%
  dplyr::mutate(uncertainty=uncertainty*10)
pgeco=pgeco[-which(substr(pgeco$short_description,1,2)=='un'),]

pgeco%>%
  dplyr::group_by(id_q,category)%>%
  dplyr::mutate(mu.val=mean(broad), sd.obs=sd(broad))%>%
  dplyr::group_by(category)%>%
  dplyr::summarise(sd.obs=mean(sd.obs), sd.declared=(mean(uncertainty)-1)/1.41) 
# why 1.74: the maximum variability that can be observed (two people) is 1 and 5. sd(c(1,5))~2.82. So that the maximum uncertainty calculated can be 2.82, and the minimum is 0. we want to anchor the uncertainty declared (max=5, min =1) to what can be observed. 1.41 is the conversion, or (5-1)/2.82
# this is what I mean
test=expand.grid(q1=1:5, q2=1:5)
unique(apply(test,1, sd))
4/2.82


# some plots
hc.vals=int.summary[substr(int.summary$category,1,2)=='hc',]
hc.vals$fishing_style=as.factor(hc.vals$fishing_style)
plot.demo=ggplot(data=hc.vals)+
  geom_col(aes(x=short_description, y=broad, group=fishing_style, fill=fishing_style, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_wrap(~domain, scales='free_x')+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom');plot.demo
ggsave(plot=plot.demo, 'results/images/human_community.jpeg', width = 28,height = 15, units='cm', dpi=500)

event.vals=int.summary[int.summary$category%in%c('event','mobility'),]
event.vals=event.vals%>%
  pivot_longer(-c('id','broad', 'category','id_q', 'range.upr', 'range.lwr','unit_range','short_description','long_description','fishing_style', 'domain','uncertainty'))%>%
  dplyr::filter(!is.na(value))
ggplot(data=event.vals)+
  geom_col(aes(x=short_description, y=value, group=fishing_style, fill=fishing_style, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_wrap(~name, scales='free_x')+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  theme(legend.position = 'bottom')
ggplot(data=event.vals)+
  geom_col(aes(x=name, y=value, group=fishing_style, fill=fishing_style, alpha=6-(uncertainty*10)), position = 'dodge', color='black')+
  facet_wrap(~short_description, scales='free_x')+
  theme_bw()+
  labs(alpha='Certainty', fill='Fisherman')+
  xlab('Question')+
  ylab('Answer')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = 'bottom')


#int.event=int.results[,c('Q','storm_h')]
event='hw_w'
x.res=int.summary
import.interview.res <- function(x.res, type='pro',event) {
  
  pro.ans=ifelse(type=='pro',2, ifelse(type=='side',1,0)) # Q20 
  x.res$Q=paste0('q',x.res$id_q)
  
  # event
  x.event=x.res[,c('Q', event, 'uncertainty')]
  #
  fish.ans=as.numeric(x.event[x.event$Q=='q2',2:3]) # how likely are you to go out fishing
  q.ans =as.numeric(x.event[x.event$Q=='q3',2:3]) # catchability
  ass.ans=as.numeric(x.event[x.event$Q=='q4',2:3]) # catch different fish species
  quality.ans= as.numeric(x.event[x.event$Q=='q5',2:3]) # poor health condition
  handl.ans = as.numeric(x.event[x.event$Q=='q6',2:3]) # handling
  long.term=as.numeric(x.event[x.event$Q=='q7',2:3])
  safe.ans=as.numeric(x.event[x.event$Q=='q9',2:3])  # physical safety
  damage.ans=as.numeric(x.event[x.event$Q=='q10',2:3])  # applying maintenance
  area.ans=as.numeric(x.event[x.event$Q=='q15',2:3]) # How easy is to avoid 
  
  # uncertainty
  #unc_1=as.numeric(x.res[x.res$Q=='q8',]$broad) # q 2-7
  #unc_2=as.numeric(x.res[x.res$Q=='q14',]$broad) # q 9-13 
  #unc_3=as.numeric(x.res[x.res$Q=='q20',]$broad) # q 16 - 19
  #unc_4=as.numeric(x.res[x.res$Q=='q22',]$broad) # q 21
  #unc_5=as.numeric(x.res[x.res$Q=='q26',]$broad) # ecopnomics
  #unc_6=as.numeric(x.res[x.res$Q=='q31',]$broad) # social
  #unc_7=as.numeric(x.res[x.res$Q=='q35',]$broad) # personal
  #unc_8=as.numeric(x.res[x.res$Q=='q44',]$broad) # costs
  
  # mobility
  gear.ans=as.numeric(x.res[x.res$Q=='q16',c('broad', 'uncertainty')])
  infr.ans=as.numeric(x.res[x.res$Q=='q17',c('broad', 'uncertainty')])
  management=(x.res[x.res$Q=='q18',c('broad', 'uncertainty')])
  permits=(x.res[x.res$Q=='q19',c('broad', 'uncertainty')])
  
  # stress
  stress.1.ans= as.numeric(x.res[x.res$Q=='q21a',c('broad', 'uncertainty')]) # Q9
  stress.2.ans= as.numeric(x.res[x.res$Q=='q21b',c('broad', 'uncertainty')]) # Q9 bis
  stress.3.ans= as.numeric(x.res[x.res$Q=='q21c',c('broad', 'uncertainty')]) # Q9 tris
  
  # Human community
  # economics
  jobmob.ans=as.numeric(x.res[x.res$Q=='q23',c('broad', 'uncertainty')]) # easiness to change job
  entrep.ans=as.numeric(x.res[x.res$Q=='q24',c('broad', 'uncertainty')]) # entrepeneurship
  credit.ans=as.numeric(x.res[x.res$Q=='q25',c('broad', 'uncertainty')]) # easiness to access credits
  # social
  gen.ans=as.numeric(x.res[x.res$Q=='q27',c('broad', 'uncertainty')])# transgenerational exchange
  comm.ans=as.numeric(x.res[x.res$Q=='q28',c('broad', 'uncertainty')])# community building
  comm.ans.2=as.numeric(x.res[x.res$Q=='q29',c('broad', 'uncertainty')])# community building
  food.ans=as.numeric(x.res[x.res$Q=='q30',c('broad', 'uncertainty')])# traditional food
  # personal
  out.ans=as.numeric(x.res[x.res$Q=='q32',c('broad', 'uncertainty')])# outdoor enjoyment
  self.ans=as.numeric(x.res[x.res$Q=='q33',c('broad', 'uncertainty')])# self-identity
  loc.ans=as.numeric(x.res[x.res$Q=='q34',c('broad', 'uncertainty')])# location attachment
  
  # fishing activity and costs
  fd0=as.numeric(x.res[x.res$Q=='q36',c('broad', 'uncertainty')])# fishing days
  income.day.lo=as.numeric(x.res[x.res$Q=='q37',]$range.lwr) # daily revenue
  income.day.hi=as.numeric(x.res[x.res$Q=='q37',]$range.upr) # daily revenue
  income.unit=(x.res[x.res$Q=='q37',]$unit_range) # daily revenue
  #income0=fd0*income.day ## this depends so much on the units
  
  maint_C1.lo=as.numeric(x.res[x.res$Q=='q11',]$range.lwr) 
  maint_C1.hi=as.numeric(x.res[x.res$Q=='q11',]$range.upr)
  maint_C1.unit=(x.res[x.res$Q=='q11',]$unit_range) 
  
  maint_C2.lo=as.numeric(x.res[x.res$Q=='q12',]$range.lwr) 
  maint_C2.hi=as.numeric(x.res[x.res$Q=='q12',]$range.upr) 
  maint_C2.unit=(x.res[x.res$Q=='q12',]$unit_range) 
  
  ord.maint.lo=as.numeric(x.res[x.res$Q=='38',]$range.lwr) 
  ord.maint.hi=as.numeric(x.res[x.res$Q=='38',]$range.upr) 
  ord.maint.unit=(x.res[x.res$Q=='38',]$unit_range)
  
  trip.cost.lo=as.numeric(x.res[x.res$Q=='39',]$range.lwr)
  trip.cost.hi=as.numeric(x.res[x.res$Q=='39',]$range.upr)
  trip.cost.unit=(x.res[x.res$Q=='39',]$broad)
  
  extra.fuel.lo=as.numeric(x.res[x.res$Q=='q40',]$range.lwr)
  extra.fuel.hi=as.numeric(x.res[x.res$Q=='q40',]$range.upr)
  extra.fuel.unit=(x.res[x.res$Q=='q40',]$unit_range)
  
  extra.crew.lo=as.numeric(x.res[x.res$Q=='q41',]$range.lwr)
  extra.crew.hi=as.numeric(x.res[x.res$Q=='q41',]$range.upr)
  extra.crew.unit=(x.res[x.res$Q=='q41',]$unit_range)
  
  extra.work.lo=as.numeric(x.res[x.res$Q=='q42',]$range.lwr)
  extra.work.hi=as.numeric(x.res[x.res$Q=='q42',]$range.upr)
  extra.work.unit=(x.res[x.res$Q=='q42',]$unit_range)
  
  xx.res=mget(ls())
  return(Filter(is.vector, xx.res))
}

x.answers=import.interview.res(x.res=int.summary[int.summary$fishing_style=='fishing guide',],
                     event='hw_s')

list2env(x.answers, envir = .GlobalEnv)

## do some math
cost0=(crew.cost+maint.cost+trip.cost) ## reflect the base cost of a fishing trip
profit0=income0-cost0
p0=0.2 # i made up this
costs=data.frame(level=c('no','low','medium','high'),
                 value=c(0,low.maint,large.maint,large.maint*5))

# find base f: this should come from models
model.res=pop.dyn(T=2000, K=50000,s_A = 0.75,fmort=0.25,regime=0.5,init=50,sigma=0.05)
(apply(model.res, 2, mean))
catch0=mean(model.res$catch.a + model.res$catch.j);catch0
fref=0.25
size0=mean(model.res$size )

## load models
source('code/network_release/CPTs_v1_2.R')
source('code/network_release/DAGs.R')

## fit BN 1. Move this elsewhere to not bother ####
#cpt.t1=list(management=manag.prob,
#            event=event.prob,
#            infrastr=infrastr.prob,
#            gear=gear.prob,
#            mobility=mobility.cpt,
#            area=area.cpt,
#            safety=safety.cpt,
#            gbreak=break.cpt,
#            mainten= maint.cpt,
#            assemblage= assembl.cpt,
#            injury=injury.cpt,
#            stress=stress.cpt,
#            cost=costs.cpt,
#            permit=allow.cpt,
#            fishing=fish.cpt)
#bn.t1 <- custom.fit(dag.t1, cpt.t1)
#jpeg('results/images/bn_t1.jpeg', width=15, height = 15, units='cm', res=500)
#graphviz.chart(bn.t1, scale=c(1,2))
#dev.off()
#
#cpt.t2=list(management=manag.prob,
#            event=event.prob,
#            infrastr=infrastr.prob,
#            gear=gear.prob,
#            mobility=mobility.cpt,
#            handling=handl.prob,
#            area=area.cpt,
#            health=chealth.cpt,
#            gbreak=break.cpt,
#            catchability=q.cpt,
#            mainten= maint2.cpt,
#            assemblage= assembl.cpt,
#            quality=c.quality.cpt,
#            stress=stress.cpt2,
#            cost=costs.cpt,
#            permit=allow.cpt,
#            fishing=fish.cpt)
#bn.t2 <- custom.fit(dag.t2, cpt.t2)
#jpeg('results/images/bn_t2.jpeg', width=15, height = 15, units='cm', res=500)
#graphviz.chart(bn.t2, scale=c(1,2))
#dev.off()

cpt.tc=list(management=manag.prob,
            event=event.prob,
            alert=alert.cpt,
            infrastr=infrastr.prob,
            gear=gear.prob,
            mobility=mobility.cpt,
            handling=handl.prob,
            area=area.cpt,
            health=chealth.cpt,
            gbreak=break.cpt,
            catchability=q.cpt,
            safety=safety.cpt,
            injury=injury.cpt,
            mainten= maint.cpt,
            assemblage= assembl.cpt,
            quality=c.quality.cpt,
            stress=stress.cpt,
            cost=costs.cpt,
            permit=allow.cpt,
            fishing=fish.cpt)

bn.tc <- custom.fit(dag.tc, cpt.tc)

#jpeg('results/images/bn_tc.jpeg', width=15, height = 15, units='cm', res=500)
#graphviz.chart(bn.tc, scale=c(1,2))
#dev.off()

# describe scenario
# this is a storms only scenario
event.bn.probs=cpdist(bn.tc, nodes = c('fishing', 'stress', 'injury', 'assemblage', 'safety', 'gbreak', 'cost', 'area'), 
                      evidence = (event == 'y' & management==manag))
bn.fish.prob=nrow(event.bn.probs[event.bn.probs$fishing=='y',])/nrow(event.bn.probs)
bn.safety=nrow(event.bn.probs[event.bn.probs$safety!='no',])/nrow(event.bn.probs)
bn.fishevent=nrow(event.bn.probs[event.bn.probs$area=='usual' & event.bn.probs$fishing=='y',])/nrow(event.bn.probs)
bn.stress=nrow(event.bn.probs[event.bn.probs$stress=='y',])/nrow(event.bn.probs)
bn.break=nrow(event.bn.probs[event.bn.probs$gbreak=='y',])/nrow(event.bn.probs)
bn.costs=table(event.bn.probs$cost)/nrow(event.bn.probs)

seas.scen=seasonal.figures(fishing = bn.fish.prob, stress = bn.stress, injury = bn.safety, inv.cost = 0, # values from BN submodule
                           fishing.event = bn.fishevent, gc=1, brek.gear = bn.break, # values from BN submodule
                           investment=0,
                           n=n.events, # number of events
                           costs = data.frame(level=c('no','low','medium','high'),value=c(0,low.maint,large.maint,large.maint*10), probs=bn.costs), # reference values for the fishery
                           fd0=fd0, catch0 = catch0, p0=0.2, fref=fref # reference values for the fishery
)

profit.scen=seas.scen[[4]]
profit.prob=table(profit.scen)/sum(table(profit.scen))
injury.prob=as.array(seas.scen[[3]])
cpt=list(csize=csize.prob,
         catches=catches.prob,
         cquality=cquality.prob,
         stress=stress.prob,
         injury=injury.prob,
         health=health.cpt$bn.cpt,
         satisfaction=satisf.cpt$bn.cpt,
         ftime=ftime.prob,
         transgeneration=gen.prob,
         community=comm.prob,
         food=food.prob,
         outdoor=out.prob,
         self=self.prob,
         location=loc.prob,
         pro=pro.prob,
         jobmob=jobmob.prob,
         entrep=entrep.prob,
         credit=credit.prob,
         nonmon_val=non.mon.cpt$bn.cpt,
         soc_imp= soc.imp.cpt$bn.cpt,
         ind_imp=  ind.imp.cpt$bn.cpt, 
         econ_imp=econimp.cpt$bn.cpt,
         innov=innov.cpt$bn.cpt,
         profit=profit.prob,
         ind_risk=ind.risk.cpt$bn.cpt,
         econ_risk=econrisk.cpt$bn.cpt,
         soc_risk=soc.risk.cpt$bn.cpt)
bn <- custom.fit(dag.le, cpt)
approx=cpdist(bn, nodes = c('soc_risk', 'ind_risk', 'econ_risk', 'nonmon_val','profit', 'soc_imp','ind_imp','econ_imp'), 
              evidence = (stress == seas.scen[[1]]$stress &
                            ftime== seas.scen[[1]]$fishing.time &
                            csize== seas.scen[[1]]$catch.size &
                            catches== seas.scen[[1]]$catch &
                            cquality == seas.scen[[1]]$catch.quality))

jpeg('results/images/bn_le.jpeg', width=15, height = 15, units='cm', res=500)
graphviz.chart(bn, scale=c(4,4))
dev.off()

## see outcomes ####
summary.tab=approx%>%
  summarise_all(table)%>%
  dplyr::mutate(label=factor(c('L','M','H')))
summary.tab$biomass=table(seas.scen[[1]]$biom)*max(summary.tab[1:3,1:5])
summary.tidy=summary.tab%>%
  pivot_longer(-label)
summary.tidy$name=factor(summary.tidy$name, levels=c('econ_risk', 'econ_imp','profit','soc_risk','soc_imp','nonmon_val','ind_risk','ind_imp','biomass'))
pl=ggplot(data=summary.tidy)+
  geom_col(aes(x=factor(label, levels=c('L','M','H')), y=value/100))+
  facet_wrap(~name)+
  xlab('State')+
  ylab('Probability')
print(pl)


