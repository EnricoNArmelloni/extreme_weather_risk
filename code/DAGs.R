library(tidyverse)
library(Rgraphviz)
library(bnlearn)

# BN lower end ####
ind.dag='[outdoor][self][location][ind_imp|outdoor:self:location][ind_risk|nonmon_val:ind_imp]'
soc.dag='[transgeneration][community][food][soc_imp|transgeneration:community:food][soc_risk|nonmon_val:soc_imp]'
econ.dag='[credit][entrep][jobmob][pro][econ_imp|pro:jobmob][econ_risk|profit:econ_imp:innov][innov|entrep:credit]'
seas.dag='[profit][csize][catches][cquality][ftime][stress][injury][nonmon_val|health:satisfaction:ftime][health|stress:injury][satisfaction|cquality:catches:csize]'
dag.string=paste0(seas.dag,ind.dag, soc.dag, econ.dag)
dag.le=model2network(dag.string)
#graphviz.plot(dag.le, layout = "dot", fontsize = 18)

# BN events ####
## Type 1
operation='[event][gbreak|event][mainten|event][safety|event]'
trait='[management][infrastr][gear][mobility|management:infrastr:gear][area|event:mobility]'
human='[stress|gbreak:safety:area][injury|safety:area][cost|area:mainten:gbreak]'
outcome='[fishing|event:permit:area][assemblage|area][permit|assemblage:management]'
dag.string=paste0(operation, trait, human, outcome)
dag.t1=model2network(dag.string)
#graphviz.plot(dag.t1, layout = "dot", fontsize = 18)

## Type 2
operation='[event][handling][gbreak|event][mainten|event:handling][health|event]'
trait='[management][infrastr][gear][mobility|management:infrastr:gear][area|event:mobility]'
human='[stress|gbreak:health:area][quality|health:handling:area][cost|area:mainten:gbreak][catchability|event]'
outcome='[fishing|event:permit:area][assemblage|area][permit|assemblage:management]'
dag.string=paste0(operation, trait, human, outcome)
dag.t2=model2network(dag.string)
#graphviz.plot(dag.t2, layout = "dot", fontsize = 18)

## type combined
operation='[event][handling][alert|event][gbreak|event][mainten|event][health|event][catchability|event:handling:area][safety|event]'
trait='[management][infrastr][gear][mobility|management:infrastr:gear][area|alert:mobility]'
human='[stress|gbreak:health:area:safety][quality|health:area:handling][cost|area:mainten:gbreak][injury|safety:area]'
outcome='[fishing|alert:permit:area][assemblage|area][permit|assemblage:management]'
dag.string=paste0(operation, trait, human, outcome)
dag.tc=model2network(dag.string)
#graphviz.plot(dag.tc, layout = "dot", fontsize = 18)















