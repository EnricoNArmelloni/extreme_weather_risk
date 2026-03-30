library(tidyverse)
library(readxl)
# this code extract information for the case study from large fisheries statistics datasets available online on the EU website

## FDI fishing effort
# available at...
setwd('~/ARMELLONI_SLU/miscellaneous_datasets')
x.eff=read_csv("~/ARMELLONI_SLU/miscellaneous_datasets/2025_Effort-landings-catches-capacity-biological/Effort/FDI Effort by country.csv")
names(x.eff)=str_replace(names(x.eff), ' ', '_')
names(x.eff)=str_replace(names(x.eff), ' ', '_')
names(x.eff)=str_replace(names(x.eff), '-', '_')
names(x.eff)=tolower(names(x.eff))
#eff.30=x.eff[x.eff$sub_region=='27.3.D.30' &x.eff$country=='Sweden',]
#write.csv(eff.30,'C:/github/extreme_weather_risk/data/fisheries_statistics/fdi_effort_SWE_D30.csv', row.names = F)
eff.30=x.eff[x.eff$country=='Sweden',]
write.csv(eff.30,'C:/github/extreme_weather_risk/data/fisheries_statistics/fdi_effort_SWE.csv', row.names = F)


## FDI catches
# available at
fdi.folder='~/ARMELLONI_SLU/miscellaneous_datasets/2025_Effort-landings-catches-capacity-biological/Catches'
fdi.dats=list.files(fdi.folder)
fdi.dats=fdi.dats[nchar(fdi.dats)<40]
fdi.yrs=length(fdi.dats)
fdi.store=NULL
for(i in 1:fdi.yrs){
  fdi=read_csv(file.path(fdi.folder, fdi.dats[i]))
  fdi.30=fdi[fdi$country=='Sweden',]
  fdi.store=rbind(fdi.store, fdi.30)
}
write.csv(fdi.store,'C:/github/extreme_weather_risk/data/fisheries_statistics/fdi_catch_SWE.csv', row.names = F)

## AER
# available at
aer=read_excel("~/ARMELLONI_SLU/miscellaneous_datasets/STECF_25-07_AER_data/STECF_25 07_EU Fleet Economic and Transversal data_fleet segment level.xlsx", 
               sheet = "FS data")
aer.se=aer[aer$country_code=='SWE',]
write.csv(aer.se,'C:/github/extreme_weather_risk/data/fisheries_statistics/AER_SWE.csv', row.names = F)

