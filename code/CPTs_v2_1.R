remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 
library(readxl)
library(tidyverse)
source('code/supporting_functions.R')


# load an exmaple of the DAG
library(bnlearn)
net=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/type1_v3_general_simple.net', debug = T)
graphviz.plot(net, layout = "dot", fontsize = 18)

test=read.net('~/ARMELLONI_SLU/RiskAnalysis/networks/single_events/test.net', debug = T)
cpdist(test, nodes = c('c'), 
       evidence = (a == 'state0'))





# data from interviews
dat=read_csv("data/coding_report_unc.csv")
dat=dat[dat$id_I==2,]


# in this example we are only dealing with a reduced dataset


# what degree of danger does thee event pose to you?

# simplest case
ch=net$personal_safety$prob
class(ch)
ch2=as.data.frame(ch)
table(ch2)


safe.ans=dat[dat$short_description=='personal_safety' & dat$id_sub=='g',]$value
safe.unc=dat[dat$short_description=='personal_safety' & dat$id_sub=='g',]$uncertainty
safety.cpt=answer.to.cpt(x.ans=safe.ans, unc = safe.unc, div.factor = 3,
                         dim.name='personal_safety', dim.labs = c('no','Minor','Major'))
safety.cpt=expand.probs(x.levels=list(c('no','minor','life'), c('n','y')) ,
                        x.names=c( 'personal_safety','gale'),
                        x.probs=list(c(1,0,0),safety.cpt)) 


#
dat[dat$short_description=='catchability' & dat$id_sub=='g',]

net$catchability

class(net)















# Human community - Variables ####

### Economic 
pro.prob=answer.to.cpt(x.ans=pro.ans, div.factor = 4, unc=.001, dim.name='pro')
jobmob.prob=answer.to.cpt(x.ans=jobmob.ans, dim.name='jobmob')
entrep.prob=answer.to.cpt(x.ans=entrep.ans, dim.name='entrep')
credit.prob=answer.to.cpt(x.ans=credit.ans, dim.name='credit')
innov.cpt=get.cpt(data.frame(node=c('credit','entrep', 'innov'), 
                             link.w=c(0.8,1,0),
                             states=c(3,3,3),
                             type=c('parent','parent','child'),
                             direction=c('pos', 'pos','pos'),
                             id=1:3), uncertainty = 0.2, algorithm = 'min')

### Societal
comm.prob=answer.to.cpt(x.ans=comm.ans, dim.name='community')
food.prob=answer.to.cpt(x.ans=food.ans, dim.name='food')
gen.prob= answer.to.cpt(x.ans=gen.ans, dim.name='transgeneration')

### Economic
loc.prob  = answer.to.cpt(x.ans=loc.ans, dim.name='location')
out.prob  = answer.to.cpt(x.ans=out.ans, dim.name='outdoor')
self.prob = answer.to.cpt(x.ans=self.ans, dim.name='self')

# Human community - Importances ####
ind.imp.cpt=get.cpt(data.frame(node=c('outdoor','self', 'location', 'ind_imp'), 
                               link.w=c(out.ans,self.ans,loc.ans,3),
                               states=c(3,3,3,3),
                               type=c('parent','parent','parent','child'),
                               direction=c('pos','pos', 'pos','pos'),
                               id=1:4), uncertainty = 0.1, algorithm = 'max') # individual

soc.imp.cpt=get.cpt(data.frame(node=c('transgeneration','community', 'food', 'soc_imp'), 
                               link.w=c(gen.ans,comm.ans,food.ans,3),
                               states=c(3,3,3,3),
                               type=c('parent','parent','parent','child'),
                               direction=c('pos','pos', 'pos','pos'),
                               id=1:4), uncertainty = 0.1, algorithm = 'max') # societal

econimp.cpt=get.cpt(data.frame(node=c('jobmob','pro', 'econ_imp'), 
                   link.w=c(2,5,3),
                   states=c(3,3,3),
                   type=c('parent','parent','child'),
                   direction=c('neg', 'pos','pos'),
                   id=1:3), uncertainty = 0.1, algorithm = 'min') # societal

# risks ####
ind.risk.cpt=get.cpt(data.frame(node=c('nonmon_val','ind_imp', 'ind_risk'), 
                                link.w=c(10,10,NA),
                                states=c(3,3,3),
                                type=c('parent','parent','child'),
                                direction=c('neg', 'pos','pos'),
                                id=1:3), uncertainty=0.1, algorithm = 'min')

soc.risk.cpt=get.cpt(data.frame(node=c('nonmon_val','soc_imp', 'soc_risk'), 
                                link.w=c(10,10,NA),
                                states=c(3,3,3),
                                type=c('parent','parent','child'),
                                direction=c('neg', 'pos','pos'),
                                id=1:3), uncertainty=0.1, algorithm = 'min')

econrisk.cpt=get.cpt(data.frame(node=c('profit','innov', 'econ_imp','econ_risk'), 
                                link.w=c(8,6,10,NA),
                                states=c(3,3,3,3),
                                type=c('parent','parent','parent','child'),
                                direction=c('neg', 'neg','pos','pos'),
                                id=1:4), uncertainty = 0.1, algorithm = 'min')

# seasonal results second layer ####
health.cpt=get.cpt(data.frame(node=c('injury','stress', 'health'), 
                              link.w=c(1,0.8,0),
                              states=c(3,3,3),
                              type=c('parent','parent','child'),
                              direction=c('neg', 'neg','pos'),
                              id=1:3), algorithm = 'min', uncertainty = 0.1)

satisf.cpt=get.cpt(data.frame(node=c('cquality','catches', 'csize','satisfaction'), 
                              link.w=c(1,1,1,0),
                              states=c(3,3,3,3),
                              type=c('parent','parent','parent','child'),
                              direction=c('pos', 'pos','pos','pos'),
                              id=1:4), uncertainty = 0.1, algorithm = 'min')

non.mon.cpt=get.cpt(data.frame(node=c('ftime','health', 'satisfaction','nonmon_val'), 
                               link.w=c(1,0.8,0.5,0),
                               states=c(3,3,3,3),
                               type=c('parent','parent','parent','child'),
                               direction=c('pos', 'pos','pos','pos'),
                               id=1:4), uncertainty = 0.15, algorithm = 'min')

## initial values ####
csize.prob= answer.to.cpt(x.ans=2.5, dim.name='csize', unc = 0.5) # dummy
catches.prob= answer.to.cpt(x.ans=2.5, dim.name='catches', unc = 0.5) # dummy
cquality.prob= answer.to.cpt(x.ans=2.5, dim.name='cquality', unc = 0.5) # dummy
stress.prob= answer.to.cpt(x.ans=2.5, dim.name='stress', unc = 0.5) # dummy
ftime.prob= answer.to.cpt(x.ans=2.5, dim.name='ftime', unc = 0.5) # dummy
event.prob= cpt.bin(x.ans=2, dim.name='event', dim.labs = c('y','n'))
event.prob[1:2]=c(0.5,0.5)
manag.prob=cpt.bin(x.ans=2, dim.name='management', dim.labs = c('rigid','flexible'))
manag.prob[1:2]=c(0.5,0.5)
handl.prob=cpt.bin(x.ans=2, dim.name='handling', dim.labs = c('y','n'))
handl.prob[1:2]=c(0.5,0.5)

# Events ####
# characteristics
gear.prob=answer.to.cpt(x.ans=gear.ans, div.factor = 4, unc=.001, dim.name='gear',dim.labs = c('not_mobile','mobile'), x.breaks=c(-Inf,0.5,Inf))
infrastr.prob=answer.to.cpt(x.ans=infr.ans, div.factor = 4, unc=.001, dim.name='infrastr',dim.labs = c('bad','good'), x.breaks=c(-Inf,0.5,Inf))


## spatial mobility
p1=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(0,1), c(0,1)))
p2=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(1,0), c(.3,.7)))
p3=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(0,1), c(0,1)))
p4=expand.probs(x.levels=list(c('z1','z2'),c('x1','x2') ) ,
                x.names=c('y1','y2'),
                x.probs=list(c(.5,.5), c(.1,.9)))
mobility.cpt=expand.probs(x.levels=list(c('y','n'),c('good','bad'),c('rigid','flexible') , c('mobile','not_mobile')) ,
                          x.names=c( 'mobility','infrastr','management','gear'),
                          x.probs=list(p1,p2,p3,p4))

# being aware that the event is coming
alert.base=answer.to.cpt(x.ans=surprise.ans, dim.name='alert', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
alert.cpt=expand.probs(x.levels=list(c('y','n'),c('n','y')) ,
                       x.names=c( 'alert','event'),
                       x.probs=list(c(0,1),alert.base)) ;alert.cpt

## potential consequences 
# good idea to have some probabilities beyond yes/no

# which kind of extra maintenance (if any)?
maint.cpt=answer.to.cpt(x.ans=maint.ans, dim.name='maintenance', dim.labs = c('no','small','large'))
maint.cpt=expand.probs(x.levels=list(c('no','small','large'),c('y','n')) ,
                       x.names=c( 'mainten','event'),
                       x.probs=list(maint.cpt,c(1,0,0))) ## on this you can also consider a different distribution to answer something like: not so likely, but if it happens is large



# how likely does your gear break?
break.cpt=answer.to.cpt(x.ans=break.ans, dim.name='gbreak', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
break.cpt=expand.probs(x.levels=list(c('n','y'),c('y','n')) ,
                       x.names=c( 'gbreak','event'),
                       x.probs=list(break.cpt,c(0,1))) 

# how likely the fish does not look good? 
chealth.cpt=answer.to.cpt(x.ans=quality.ans, dim.name='health', dim.labs = c('good','bad'), x.breaks = c(-Inf, 0.5,Inf))
chealth.cpt=expand.probs(x.levels=list(c('good','bad'),c('y','n')) ,
                         x.names=c( 'health','event'),
                         x.probs=list(chealth.cpt,c(1,0))) 

# how likely you can go to an area that is NOT affected by the event?
area.cpt=answer.to.cpt(x.ans=area.ans, dim.name='area', dim.labs = c('usual','distant'), x.breaks = c(-Inf, 0.5,Inf))
area.cpt=expand.probs(x.levels=list(c('usual','distant'), c('n','y'), c('n','y')) ,
                      x.names=c( 'area','alert', 'mobility'),
                      x.probs=list(c(1,0),
                                   c(1,0),
                                   c(1,0),
                                   area.cpt)) 

## realised consequences
q.cpt=answer.to.cpt(x.ans=q.ans, dim.name='catchability', dim.labs = c('less','same','more'))
q.cpt=expand.probs(x.levels=list(c('less','same','more'),c('y','n'),c('y','n'),c('usual','distant')) ,
                   x.names=c( 'catchability','event','handling', 'area'),
                   x.probs=list(as.numeric(table(cut((q.ans.bis+q.ans)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                as.numeric(table(cut((q.ans.bis+0)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                as.numeric(table(cut((0+q.ans)/5, breaks = c(-Inf,-0.1,0.1,Inf), labels=c('less','same','more')))),
                                c(0,1,0),
                                # distant area
                                c(0,1,0),c(0,1,0),
                                c(0,1,0),c(0,1,0)
                                )) ;q.cpt # this should be improved
## catch assemblage (expected)
assembl.cpt=expand.probs(x.levels=list(c('usual','different'), c('usual','distant')) ,
                         x.names=c( 'assemblage','area'),
                         x.probs=list(c(1,0),
                                      cpt.bin(x.ans=ass.ans, dim.name = 'x', dim.labs = c('y','n'))))

# catch quality accounting for the effect of handling
handl.effect=answer.to.cpt(x.ans=handl.ans, dim.name='handling', dim.labs = c('n','y'), x.breaks = c(-Inf, 0.5,Inf))
c.quality.cpt=expand.probs(x.levels=list(c('bad','good'),c('good','bad'),c('y','n'),c('usual','distant')) ,
                           x.names=c( 'quality','health', 'handling','area'),
                           x.probs=list(c(0,1), 
                                        handl.effect,
                                        c(0,1), 
                                        c(1,0), 
                                        c(0,1), 
                                        c(0,1), 
                                        c(0,1), 
                                        c(0,1)))

# injury
injury.cpt=expand.probs(x.levels=list(c('no','minor','life'),c('no','minor','life'),c('usual','distant')) ,
                        x.names=c( 'injury','safety','area'),
                        x.probs=list(c(1,0,0),c(0,1,0),c(0,0,1), # mirroring the safety
                                     c(1,0,0),
                                     c(1,0,0),
                                     c(1,0,0)))

# stress
stress.base=get.cpt(data.frame(node=c('gbreak', 'health', 'safety','stress'), 
                   link.w=c(stress.1.ans,stress.2.ans,stress.3.ans, 0),
                   states=c(2,2,3,2),
                   type=c('parent','parent','parent','child'),
                   direction=c('pos', 'neg','pos', 'pos'),
                   id=1:4), uncertainty = 0.15, algorithm = 'max')

stress.cpt=expand.probs(x.levels=list(c('n','y'),c('no','minor', 'life'),c('y','n'),c('good','bad'), c('usual','distant')) ,
                        x.names=c( 'stress','safety','gbreak','health','area'),
                        x.probs=list(as.numeric(stress.base$report.cpt[7,4:5]),
                                     as.numeric(stress.base$report.cpt[8,4:5]),
                                     as.numeric(stress.base$report.cpt[9,4:5]),
                                     as.numeric(stress.base$report.cpt[1,4:5]),
                                     as.numeric(stress.base$report.cpt[2,4:5]),
                                     as.numeric(stress.base$report.cpt[3,4:5]),
                                     as.numeric(stress.base$report.cpt[10,4:5]),
                                     as.numeric(stress.base$report.cpt[11,4:5]),
                                     as.numeric(stress.base$report.cpt[12,4:5]),
                                     as.numeric(stress.base$report.cpt[4,4:5]),
                                     as.numeric(stress.base$report.cpt[5,4:5]),
                                     as.numeric(stress.base$report.cpt[6,4:5]),
                                     ## area distant
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0), 
                                     c(1,0),  c(1,0),  c(1,0)
                        )) ;stress.cpt


# expected costs. This needs to be further elaborated based on the answers!!!
costs.cpt=expand.probs(x.levels=list(c('n','l','m','h'),c('no','small','large'),
                                     c('y','n'),c('usual','distant')) ,
                       x.names=c( 'cost','mainten','gbreak','area'),
                       x.probs=list(# if you break the gear the cost is high
                                    c(0,0,0,1),c(0,0,0,1), c(0,0,0,1), 
                                    # if you do not break the gear, the cost mirrors the maintenance
                                    c(1,0,0,0),c(0,1,0,0), c(0,0,1,0), 
                                    # when you travel far, the cost is always low
                                    c(0,1,0,0),c(0,1,0,0), c(0,1,0,0), 
                                    c(0,1,0,0),c(0,1,0,0), c(0,1,0,0))) ; costs.cpt

## decisions
allow.cpt=expand.probs(x.levels=list(c('y','n'),c('usual','different'),c('rigid','flexible')) ,
                       x.names=c( 'permit','assemblage','management'),
                       x.probs=list(c(1,0), 
                                    c(0,1),  
                                    c(1,0), 
                                    c(1,0))) 

fish.cpt=expand.probs(x.levels=list(c('n','y'),c('y','n'),c('y','n'),c('usual','distant')) ,
                      x.names=c( 'fishing','permit','alert', 'area'),
                      x.probs=list(answer.to.cpt(x.ans=fish.ans, dim.name='area', dim.labs = c('n','y'), 
                                                 x.breaks = c(-Inf, 0.5,Inf), unc=0.4), 
                                   c(1,0), c(0,1), c(1,0), c(0,1), c(1,0), c(0,1), c(1,0))) ; fish.cpt




        