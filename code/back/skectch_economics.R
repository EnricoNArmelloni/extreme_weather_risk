library(tidyverse)


store=NULL
for(i in 1:1000){
  minor=rnorm(n=1, mean=15000, sd=5000)
  major=rnorm(n=1, mean=100000, sd=10000) # this should be lognormal
  fdays=100
  income_day=rnorm(n=1, mean=7000, sd=250)
  season.base=rnorm(n=1, mean=75000, sd=1500)
  fuel.base=rnorm(n=1, mean=1000, sd=250)
  fuel.large=rnorm(n=1, mean=500, sd=250)
  
  ###
  profit=((income_day-fuel.base)*fdays)-season.base # baseline profit
  ###
  est.costs=data.frame(expense.pct=c(minor/profit,major/profit,fuel.large/profit),
             label=c('damage.s', 'damage.l', 'fuel'))
  store=rbind(store, est.costs)
}

plot(density(store$expense.pct))

thr=quantile(store$expense.pct, probs=c(0.33,0.66))
store$category=cut(store$expense.pct, breaks = c(-Inf, thr, Inf), labels = c('minor', 'medium','high'))

cpt=store%>%
  dplyr::group_by(label, category)%>%
  tally()%>%
  dplyr::group_by(label)%>%
  dplyr::mutate(n=n/sum(n))

ggplot(data=cpt)+
  geom_col(aes(x=category, y=n))+
  facet_wrap(~label)
