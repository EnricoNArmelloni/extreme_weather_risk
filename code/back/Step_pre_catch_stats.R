library(tidyverse)
library(readxl)
setwd('~/ARMELLONI_SLU/miscellaneous_datasets')


# effort ####
# FDI 
# Question: which are the fishing gear mostöly used by coastal fishers?
x.eff=read_csv("~/ARMELLONI_SLU/miscellaneous_datasets/2024_fdi_effort/Effort/FDI Effort by country.csv")
names(x.eff)=str_replace(names(x.eff), ' ', '_')
names(x.eff)=str_replace(names(x.eff), ' ', '_')
names(x.eff)=str_replace(names(x.eff), '-', '_')
names(x.eff)=tolower(names(x.eff))

eff.30=x.eff[x.eff$sub_region=='27.3.D.30' &x.eff$country=='Sweden',]
coastal.eff=eff.30[eff.30$vessel_length_category %in% c('VL0812', 'VL0008'),]

#eff.plot=coastal.eff%>%
#  dplyr::mutate(total_fishing_days=as.numeric(total_fishing_days))%>%
#  dplyr::group_by(fishing_technique, gear_type,target_assemblage,year)%>%
#  dplyr::summarise(fdays=sum(total_fishing_days))%>%
#  dplyr::group_by(fishing_technique, gear_type, target_assemblage)%>%
#  dplyr::summarise(fdays=mean(fdays))
#
#eff.plot%>%
#  ggplot(aes(x=gear_type, y=fdays, fill=target_assemblage))+
#  geom_col()+
#  facet_wrap(~fishing_technique)


eff.plot=coastal.eff%>%
  dplyr::mutate(total_fishing_days=as.numeric(total_fishing_days))%>%
  dplyr::group_by(gear_type, vessel_length_category, target_assemblage)%>%
  dplyr::summarise(fdays=sum(total_fishing_days))

eff.plot%>%
  ggplot(aes(x=target_assemblage, y=fdays, fill=vessel_length_category))+
  geom_col()+
  facet_wrap(~gear_type, scales='free_x')

## Decision
coastal_fdays=coastal.eff%>%
  dplyr::mutate(total_fishing_days=as.numeric(total_fishing_days))%>%
  dplyr::group_by(fishing_technique,gear_type,target_assemblage,year)%>%
  dplyr::summarise(fdays=sum(total_fishing_days))%>%
  dplyr::group_by(fishing_technique,gear_type, target_assemblage)%>%
  dplyr::summarise(fdays=mean(fdays))%>%
  dplyr::group_by(gear_type, target_assemblage)%>%
  dplyr::summarise(fdays=sum(fdays))%>%
  arrange(desc(fdays))%>%
  ungroup()%>%
  dplyr::mutate(cum_val=cumsum(fdays)/sum(fdays), prop=fdays/sum(fdays)) ## CPT for fishing_style -> gear

coastal_fdays$metier=paste(coastal_fdays$gear_type, coastal_fdays$target_assemblage, sep='_')
write.csv(coastal_fdays, "C:/github/extreme_weather_risk/data/read_only/coastal_fdays.csv", row.names = F)


## Catches ####
# FDI provides data by country and sub region, with fine detail on fishing tech etc.
asfis= read_csv("~/ARMELLONI_SLU/miscellaneous_datasets/ASFIS_2020.csv")
names(asfis)[3]='species'
asfis=asfis[,3:5]
fdi.folder='~/ARMELLONI_SLU/miscellaneous_datasets/2024_fdi_catches/Catches'
fdi.dats=list.files(fdi.folder)
fdi.dats=fdi.dats[nchar(fdi.dats)<40]
fdi.yrs=length(fdi.dats)

fdi.store=NULL
for(i in 1:fdi.yrs){
  fdi=read_csv(file.path(fdi.folder, fdi.dats[i]))
  fdi.30=fdi[fdi$sub_region=='27.3.D.30' &fdi$country=='Sweden' & fdi$vessel_length %in% c('VL0812', 'VL0008'),]
  fdi.store=rbind(fdi.store, fdi.30)
}


fdi.30=fdi.store%>%
  dplyr::mutate(tons=as.numeric(total_live_weight_landed))%>%
  dplyr::filter(!is.na(tons))%>%
  dplyr::group_by(fishing_tech, gear_type, target_assemblage, species, year)%>%
  dplyr::summarise(ton=sum(tons))%>%
  dplyr::group_by(fishing_tech, gear_type, target_assemblage, species)%>%
  dplyr::summarise(ton=mean(ton))

fdi.30.short=fdi.30%>%
  dplyr::mutate(metier=paste(fishing_tech,gear_type, sep='_' ))%>%
  dplyr::group_by(metier, target_assemblage,)%>%
  dplyr::mutate(thr=quantile(ton, probs=0.5))%>%
  dplyr::filter(ton>thr)

plfdi=ggplot(data=fdi.30.short)+
  geom_col(aes(x=species, y=ton, fill=target_assemblage))+
  facet_wrap( ~metier);plfdi

ggsave(plot=plfdi, '~/ARMELLONI_SLU/miscellaneous_images/SSF_FDI_catches.jpeg', width = 25, height = 15, units='cm')



fdi.30$metier=paste(fdi.30$gear_type, fdi.30$target_assemblage, sep='_')

coastal.catch=fdi.30[fdi.30$metier %in% coastal_fdays[1:4,]$metier ,]%>%
  dplyr::group_by(metier, species)%>%
  dplyr::summarise(ton=sum(ton))%>%
  dplyr::arrange(metier, desc(ton))%>%
  dplyr::mutate(cum_val=round(cumsum(ton)/sum(ton), digits=2), prop=ton/sum(ton))

coastal.catch$keep=1
for(i in 2:(nrow(coastal.catch))){
   if(coastal.catch[i,]$metier==coastal.catch[i-1,]$metier){
     if(coastal.catch[i,]$cum_val<=0.95){
       next
     }else{
       coastal.catch[i,]$keep=0
     }
   }else{
     coastal.catch[i,]$keep=1
   }
}

coastal.catch=coastal.catch%>%
  dplyr::filter(keep==1)%>%
  left_join(asfis)

coastal.catch%>%
  ggplot(aes(x=English_name, y=ton))+
  geom_col()+
  facet_wrap(~metier, scales='free_x')

write.csv(coastal.catch, "C:/github/extreme_weather_risk/data/read_only/coastal_catch.csv", row.names = F)





