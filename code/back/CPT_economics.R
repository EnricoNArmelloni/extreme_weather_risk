rm(list=ls())
library(tidyverse)
library(readxl)

setwd("C:/github/extreme_weather_risk")
xdat=read_excel("~/ARMELLONI_SLU/miscellaneous_datasets/STECF_24-07_EU Fleet Economic and Transversal data/STECF 24-07 - EU Fleet Economic and Transversal data_fleet segment level.xlsx", 
                sheet = "FS data")
xdat.sw=xdat[xdat$country_code=='SWE',]
write.csv(xdat.sw,'data/AER_SE.csv', row.names = F)



store.cpt=NULL

## coastal fisheries ####
xdat.sw=xdat.sw[xdat.sw$vessel_length %in% c('VL0010', 'VL0812') & xdat.sw$fishing_tech %in% c('FPO','DFN'),]
options(scipen=999)
costs1=xdat.sw[xdat.sw$variable_name %in%c('Number of vessels', "Number of fishing trips", 'Repair & maintenance costs', 'Energy costs'), 
           c('year', 'vessel_length' ,'fishing_tech', 'variable_name' , 'value')]%>%
  na.omit()%>%
  dplyr::filter(year>=2018)%>%
  dplyr::group_by(vessel_length, fishing_tech, variable_name)%>%
  dplyr::summarise(value=mean(value))%>%
  dplyr::mutate(variable_name=str_remove_all(variable_name, ' '))%>%
  pivot_wider(names_from = variable_name, values_from = value)%>%
  na.omit()

x.spending=xdat.sw[xdat.sw$variable_group=='Expenditure',]
spend.tot=x.spending%>%
  dplyr::group_by(vessel_length, fishing_tech,year)%>%
  dplyr::summarise(value=sum(value))%>%
  na.omit()%>%
  dplyr::filter(year>=2018)%>%
  dplyr::group_by(vessel_length, fishing_tech)%>%
  dplyr::summarise(totaal_exp=mean(value))
monetary=left_join(costs1, spend.tot)
monetary=as.data.frame(t(apply(monetary[,3:7],2,FUN=sum)))

monetary$trip_fuel=monetary$Energycosts/monetary$Numberoffishingtrips
monetary$annual_spend=monetary$totaal_exp/monetary$Numberofvessels

###
int.dat=read_csv("data/read_only/coding_report_unc.csv")
coast.dat=int.dat[int.dat$fishing_style=='coastal',]
sek.eur=0.091

# costs are made of maintenance and fuel
# profit is catches value - costs
coast.dat[coast.dat$short_description=='fuel_trip',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='extra_fuel',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='season_maint',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='income',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='fdays',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='minor_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='major_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]

# sketch
cost.lo=rtruncnorm(a=1000, b=50000, mean=(50000-1000)/2, sd=1000000, n=1000) # minor cost
cost.hi=rtruncnorm(a=50000, b=100000, mean=(100000-50000)/2, sd=10000000, n=1000) # major cost
fuel.lo=rtruncnorm(a=1500, b=2500, mean=(2000)/2, sd=1000, n=10000)
fuel.hi=rtruncnorm(a=3000, b=5000, mean=(2000)/2, sd=1000, n=10000)
no.travel=(fuel.hi/fuel.lo)*monetary$trip_fuel/sek.eur  ### fuel price for the trip 
cost.df=data.frame(minor.no=cost.lo, major.no=cost.hi, no.travel=no.travel)
cost.df$minor.travel=cost.df$minor.no+cost.df$no.travel
cost.df$major.travel=cost.df$major.no+cost.df$no.travel
cost.annual=monetary$annual_spend/sek.eur
cost.df[,]=apply(cost.df[,],2,function(x) x/cost.annual)
hist(unlist(cost.df, use.names=FALSE))
quantile(unlist(cost.df, use.names=FALSE))
cost.df.disc=cost.df
cost.df.disc[,]=apply(cost.df.disc[,],2,function(x) cut(x, c(-Inf, 0.005,0.025,0.05,Inf), labels=c('negligible','low','medium','high')))
cost.df.disc$type='coastal'

cost.cpt=cost.df.disc%>%
  pivot_longer(-type, names_to = 'parent', values_to = 'state')%>%
  dplyr::group_by(parent, state)%>%
  tally()%>%
  dplyr::mutate(prob=n/sum(n))

cost.cpt%>%
  ggplot(aes(x=parent, y=prob, fill=state))+
  geom_col()
cost.cpt$fishing_style='coastal'
store.cpt=rbind(store.cpt, cost.cpt)

## trawlers ####
xdat.sw=xdat[xdat$country_code=='SWE' ,]
xdat.sw=xdat.sw[xdat.sw$vessel_length %in% c('VL2440'),]
#xdat.sw=xdat.sw[xdat.sw$fishing_tech %in% c('TM'),]
options(scipen=999)
costs1=xdat.sw[xdat.sw$variable_name %in%c('Number of vessels', "Number of fishing trips", 'Repair & maintenance costs', 'Energy costs'), 
               c('year', 'vessel_length' ,'fishing_tech', 'variable_name' , 'value')]%>%
  na.omit()%>%
  dplyr::filter(year>=2018)%>%
  dplyr::group_by(vessel_length, fishing_tech, variable_name)%>%
  dplyr::summarise(value=mean(value))%>%
  dplyr::mutate(variable_name=str_remove_all(variable_name, ' '))%>%
  pivot_wider(names_from = variable_name, values_from = value)%>%
  na.omit()

x.spending=xdat.sw[xdat.sw$variable_group=='Expenditure',]
spend.tot=x.spending%>%
  dplyr::group_by(vessel_length, fishing_tech,year)%>%
  dplyr::summarise(value=sum(value))%>%
  na.omit()%>%
  dplyr::filter(year>=2018)%>%
  dplyr::group_by(vessel_length, fishing_tech)%>%
  dplyr::summarise(totaal_exp=mean(value))
monetary=left_join(costs1, spend.tot)
monetary=as.data.frame(t(apply(monetary[,3:7],2,FUN=sum)))

monetary$trip_fuel=monetary$Energycosts/monetary$Numberoffishingtrips
monetary$annual_spend=monetary$totaal_exp/monetary$Numberofvessels

int.dat=read_csv("data/read_only/coding_report_unc.csv")
coast.dat=int.dat[int.dat$fishing_style=='pelagic_trawler',]
sek.eur=0.091
# costs are made of maintenance and fuel
# profit is catches value - costs
coast.dat[coast.dat$short_description=='fuel_trip',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='extra_fuel',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='season_maint',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='income',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='fdays',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='minor_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='major_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]

# sketch
cost.lo=rtruncnorm(a=10000, b=50000, mean=(50000-10000)/2, sd=1000000, n=1000) # minor cost
cost.hi=rtruncnorm(a=50000, b=500000, mean=(500000-50000)/2, sd=10000000, n=1000) # major cost

fuel.lo=rtruncnorm(a=0.75, b=1.25, mean=(1)/2, sd=10, n=10000)
fuel.hi=rtruncnorm(a=1, b=1.5, mean=(2)/2, sd=10, n=10000)

no.travel=(fuel.hi/fuel.lo)*monetary$trip_fuel/sek.eur  ### fuel price for the trip 
cost.df=data.frame(minor.no=cost.lo, major.no=cost.hi, no.travel=no.travel)
cost.df$minor.travel=cost.df$minor.no+cost.df$no.travel
cost.df$major.travel=cost.df$major.no+cost.df$no.travel
cost.annual=monetary$annual_spend/sek.eur
cost.df[,]=apply(cost.df[,],2,function(x) x/cost.annual)
hist(unlist(cost.df, use.names=FALSE))
quantile(unlist(cost.df, use.names=FALSE))
cost.df.disc=cost.df
cost.df.disc[,]=apply(cost.df.disc[,],2,function(x) cut(x, c(-Inf, 0.005,0.025,0.05,Inf), labels=c('negligible','low','medium','high')))
cost.df.disc$type='coastal'

cost.cpt=cost.df.disc%>%
  pivot_longer(-type, names_to = 'parent', values_to = 'state')%>%
  dplyr::group_by(parent, state)%>%
  tally()%>%
  dplyr::mutate(prob=n/sum(n))

cost.cpt%>%
  ggplot(aes(x=parent, y=prob, fill=state))+
  geom_col()

cost.cpt$fishing_style='trawler'
store.cpt=rbind(store.cpt, cost.cpt)

## fishing guide ####
int.dat=read_csv("data/read_only/coding_report_unc.csv")
coast.dat=int.dat[int.dat$fishing_style=='fishing guide',]

coast.dat[coast.dat$short_description=='fuel_trip',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='extra_fuel',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='season_maint',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='income',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='fdays',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='minor_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='major_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]

# sketch
cost.lo=rtruncnorm(a=10000, b=20000, mean=(20000-1000)/2, sd=10000, n=1000) # minor cost
cost.hi=rtruncnorm(a=20000, b=500000, mean=(500000-50000)/2, sd=10000, n=1000) # major cost
cost.seas=rtruncnorm(a=100000, b=200000, mean=(100000-50000)/2, sd=100000, n=1000) 
cost.seas=cost.seas+(1000*100)
fuel.lo=rtruncnorm(a=0.9, b=1.1, mean=(1)/2, sd=10, n=10000)
fuel.hi=rtruncnorm(a=1.3, b=1.5, mean=(2)/2, sd=10, n=10000)
no.travel=(fuel.hi/fuel.lo)*1000  ### fuel price for the trip 
cost.df=data.frame(minor.no=cost.lo, major.no=cost.hi, no.travel=no.travel, cost.annual=cost.seas)
cost.df$minor.travel=cost.df$minor.no+cost.df$no.travel
cost.df$major.travel=cost.df$major.no+cost.df$no.travel
cost.df=cost.df[,c(1:3,5,6)]/cost.df[,c(4)]
cost.df.disc=cost.df
cost.df.disc[,]=apply(cost.df.disc[,],2,function(x) cut(x, c(-Inf, 0.005,0.025,0.05,Inf), 
                                                        labels=c('negligible','low','medium','high')))
cost.df.disc$type='coastal'

cost.cpt=cost.df.disc%>%
  pivot_longer(-type, names_to = 'parent', values_to = 'state')%>%
  dplyr::group_by(parent, state)%>%
  tally()%>%
  dplyr::mutate(prob=n/sum(n))

cost.cpt%>%
  ggplot(aes(x=parent, y=prob, fill=state))+
  geom_col()
cost.cpt$fishing_style='guide'
store.cpt=rbind(store.cpt, cost.cpt)

## recreational ####

int.dat=read_csv("data/read_only/coding_report_unc.csv")
coast.dat=int.dat[int.dat$fishing_style=='recreational',]
recf.numb=read_excel("data/recf_catch.xlsx", 
                     sheet = "tab4")
recf.exp=read_excel("data/recf_catch.xlsx", 
                    sheet = "tab3")
recf.exp$sek_person_mean=(recf.exp$mean*1000000)/(recf.numb[1,]$mean_value*1000)
recf.exp$sek_person_error=(recf.exp$error*1000000)/(recf.numb[1,]$mean_value*1000)
recf.exp$sek_day_mean=(recf.exp$mean*1000000)/(recf.numb[2,]$mean_value*1000)
recf.exp$sek_day_error=(recf.exp$error*1000000)/(recf.numb[2,]$mean_value*1000)
recf.exp$sek_day_boat_mean=(recf.exp$mean*1000000)/(recf.numb[3,]$mean_value*1000)
recf.exp$sek_day_boat_error=(recf.exp$error*1000000)/(recf.numb[3,]$mean_value*1000)

## get mean cost fro trips
mean.rf.travel=mean(c(recf.exp[recf.exp$category %in% c('Expenses for travel'),]$sek_day_mean,
                     recf.exp[recf.exp$category %in% c('Expenses for boat fuel'),]$sek_day_boat_mean ))

coast.dat[coast.dat$short_description=='fuel_trip',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='extra_fuel',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='season_maint',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='income',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='fdays',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='minor_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]
coast.dat[coast.dat$short_description=='major_damage_cost',c('value', 'range.lwr','range.upr','unit_range')]

# sketch
cost.lo=rtruncnorm(a=10000, b=50000, mean=(20000-1000)/2, sd=10000, n=1000) # minor cost
cost.hi=rtruncnorm(a=50000, b=500000, mean=(500000-50000)/2, sd=10000, n=1000) # major cost

#cost.seas=rtruncnorm(a=5000, b=10000, mean=(100000-50000)/2, sd=100000, n=1000) 
cost.seas=rtruncnorm(a=0, b =Inf,mean=recf.exp[recf.exp$category=='Totalt',]$sek_person_mean, 
                sd=recf.exp[recf.exp$category=='Totalt',]$sek_person_error, n=10000)
fuel.lo=rtruncnorm(a=30, b=50, mean=40, sd=100, n=10000)
fuel.hi=rtruncnorm(a=50, b=100, mean=70, sd=100,n=10000)

no.travel=(fuel.hi/fuel.lo)*mean.rf.travel  ### fuel price for the trip 


cost.df=data.frame(minor.no=cost.lo, major.no=cost.hi, no.travel=no.travel, cost.annual=cost.seas)
cost.df$minor.travel=cost.df$minor.no+cost.df$no.travel
cost.df$major.travel=cost.df$major.no+cost.df$no.travel
cost.df=cost.df[,c(1:3,5,6)]/cost.df[,c(4)]
cost.df.disc=cost.df
cost.df.disc[,]=apply(cost.df.disc[,],2,function(x) cut(x, c(-Inf, 0.005,0.025,0.05,Inf), 
                                                        labels=c('negligible','low','medium','high')))
cost.df.disc$type='coastal'

cost.cpt=cost.df.disc%>%
  pivot_longer(-type, names_to = 'parent', values_to = 'state')%>%
  dplyr::group_by(parent, state)%>%
  tally()%>%
  dplyr::mutate(prob=n/sum(n))

cost.cpt%>%
  ggplot(aes(x=parent, y=prob, fill=state))+
  geom_col()

cost.cpt$fishing_style='recreational'
store.cpt=rbind(store.cpt, cost.cpt)

####
store.cpt%>%
  ggplot(aes(x=parent, y=prob, fill=state))+
  geom_col()+
  facet_wrap(~fishing_style)

write.csv(store.cpt, 'data/read_only/cost_df.csv', row.names = F)














