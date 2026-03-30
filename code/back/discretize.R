

f.df=function(vx){
  low.b=(f.norm(vx)+dplyr::lag(f.norm(vx),1))/2
  hi.b=(f.norm(vx)+dplyr::lead(f.norm(vx),1))/2
  c1=data.frame( lo=low.b,mu=f.norm(vx), hi=hi.b)
  c1[is.na(c1)]=c(0,1)
  return(c1)  
}


f.norm=function(x){
  (x-min(x))/(max(x)-min(x))  
}
# answers: 
va=seq(1,5,1)
vb=seq(0,3,1)
v1=seq(0,2,1)

#categories
r1=c('l','m','h')
r2=c('l','h')

f.df(va)
f.df(vb)
f.df(v1)

cut(f.norm(va),f.df(va)$mu)


rbinom(n=10,size=c(2), prob=0.8)    
?rbinom

map_to_cat = function(x, categories){
  x_norm = f.norm(x)
  cuts = seq(0, 1, length.out = length(categories) + 1)
  categories[findInterval(x_norm, cuts, all.inside = TRUE)]
}

map_to_cat(va, r1)


f.norm = function(x){
  (x - min(x)) / (max(x) - min(x))  
}






prob_from_single_value = function(x, min_val, max_val, categories){
  
  # normalizzazione
  x_norm = (x - min_val) / (max_val - min_val)
  x_norm = max(0, min(1, x_norm))
  k = length(categories)
  # posizioni equidistanti delle categorie
  pos = seq(0, 1, length.out = k)
  probs = rep(0, k)
  # casi estremi
  if(x_norm <= pos[1]){
    probs[1] = 1
  } else if(x_norm >= pos[k]){
    probs[k] = 1
  } else {
    j = max(which(pos <= x_norm))
    w = (x_norm - pos[j]) / (pos[j+1] - pos[j])
    probs[j]   = 1 - w
    probs[j+1] = w
  }
  names(probs) = categories
  return(probs)
}


# applicare senza incertezza versione base, poi simulare iterazioni con dirichlet

p=prob_from_single_value(0.75, 0,1,r1);p

kappa = 20
alpha = p * kappa
gv=gtools::rdirichlet(10, alpha)
gv
apply(gv,2, FUN=quantile)








x=rnorm(n=100,mean=0.75,sd=0.5)
x[x>1]=1

y=t(sapply(x, prob_from_single_value,
                  min_val = 0,
                  max_val = 1,
                  categories = r1))
y
apply(y,2, FUN=mean)



p = c(l=0, m=0, h=1)
kappa = 2
prior = rep(0.1, 3)
alpha = p * kappa + prior
gtools::rdirichlet(1, alpha)




prob_from_single_value.2 = function(x, min_val, max_val, categories, lambda = 5){
  
  # normalizzazione
  x_norm = (x - min_val) / (max_val - min_val)
  x_norm = max(0, min(1, x_norm))
  
  # posizioni categorie
  k = length(categories)
  pos = seq(0, 1, length.out = k)
  
  # peso basato su distanza quadratica
  w = exp(-lambda * (x_norm - pos)^2)
  
  # normalizzazione
  p = w / sum(w)
  
  names(p) = categories
  return(p)
}
