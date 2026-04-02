library(truncnorm)

#answer.to.cpt=function(x.ans, x.range=c(1,5),dim.name, dim.labs = c('L','M','H'), unc=0.05){
#  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
#  dim.list <- list(dim.labs)
#  names(dim.list) <- dim.name
#  base.dist=rtruncnorm(a=x.range[1],b=x.range[2],n=100, mean=x.ans, sd=unc)
#  x.breaks=seq(x.range[1],x.range[2],(x.range[2]-x.range[1])/length(dim.labs))
#  x.breaks[1]=-Inf
#  x.breaks[length(x.breaks)]=Inf
#  base.dist=cut(base.dist, breaks=x.breaks, labels=dim.labs)
#  prob.tab=array(table(base.dist)/100, dim = length(dim.labs), dimnames = dim.list)
#  return(prob.tab)
#}
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
answer.to.cpt=function(x.ans, x.range=c(0,1),dim.name, dim.labs = c('L','M','H'), unc=0.05, k.thr=2, x.breaks=NULL, priors=NULL){
  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=x.range[1],b=x.range[2],n=100, mean=x.ans, sd=unc)
  if(is.null(x.breaks)){
   x.breaks=seq(x.range[1],x.range[2],(x.range[2]-x.range[1])/length(dim.labs)) 
  }
  x.breaks[1]=-Inf
  x.breaks[length(x.breaks)]=Inf
  if(length(dim.labs)==2){
    #base.dist=log.reg(l=1,k=k.thr, x=base.dist, x0=(x.range[2]+x.range[1])/2) 
    #prob.tab=array(c(1-mean(base.dist),mean(base.dist)), dim = length(dim.labs), dimnames = dim.list) 
    #p = (sample(base.dist,10) - x.range[1]) / (x.range[2] - x.range[1])
    #prob.tab = array(c(1-mean(p), mean(p)), dim=2, dimnames=dim.list)
    #base.dist=rtruncnorm(a=0,b=1,n=100, mean=0.75, sd=0.1)
    base.dist=cut(base.dist, breaks=c(-Inf,0.33,0.66,Inf), labels=c('L','M','H'))
    prob.tab=array(table(base.dist)/100, dim = 3, dimnames = list(c('L','M','H'))) 
    prob.tab=array(c(prob.tab[1]+(prob.tab[2]/2), prob.tab[3]+(prob.tab[2]/2)), dim=2, dimnames=dim.list)
    
    if(!is.null(priors)){
      prior.dist=cut(priors, breaks=c(-Inf,0.33,0.66,Inf), labels=c('L','M','H'))
      prior.tab=array(table(prior.dist)/100, dim = 3, dimnames = list(c('L','M','H'))) 
      prior.tab=array(c(prior.tab[1]+(prior.tab[2]+0.001/2), prior.tab[3]+(prior.tab[2]+0.001/2)), dim=2, dimnames=dim.list)
      post=(2*prob.tab)*(prior.tab+0.001)
      prob.tab= post / sum(post)
    }
    
    }else{
    
    base.dist=cut(base.dist, breaks=x.breaks, labels=dim.labs)
    prob.tab=array(table(base.dist)/100, dim = length(dim.labs), dimnames = dim.list) 
    
    if(!is.null(priors)){
      prior.dist=cut(priors, breaks=x.breaks, labels=dim.labs)
      prior.tab=array(table(prior.dist)/length(priors), dim = length(dim.labs), dimnames = dim.list) 
      post=(2*prob.tab)*(prior.tab+0.001)
      prob.tab= post / sum(post)
    }
  }
  #mean(base.dist)

  
  return(prob.tab)
}

dir.draws=function(kappa=20, p, n=1){
  alpha = as.numeric(p) * kappa
  gtools::rdirichlet(n, alpha)
}

f.norm=function(x){
  (x-min(x))/(max(x)-min(x))  
}

log.reg=function(l,k,x0=0,x){
  l/(1+exp(-k*(x-x0)))
}

answ.cpt.det = function(x.norm, categories){
  
  # normalizzazione
  #x_norm = (x - min_val) / (max_val - min_val)
  x.norm = max(0, min(1, x.norm))
  k = length(categories)
  # posizioni equidistanti delle categorie
  pos = seq(0, 1, length.out = k)
  probs = rep(0, k)
  # casi estremi
  if(x.norm <= pos[1]){
    probs[1] = 1
  } else if(x.norm >= pos[k]){
    probs[k] = 1
  } else {
    j = max(which(pos <= x.norm))
    w = (x.norm - pos[j]) / (pos[j+1] - pos[j])
    probs[j]   = 1 - w
    probs[j+1] = w
  }
  names(probs) = categories
  return(probs)
}

uncertainty.function=function(unc.dat){
  
  unc.dat=unc.dat%>%
    dplyr::group_by(gear,area,target,short_description)%>%
    dplyr::summarise(uncertainty.obs=sd(value.norm), 
                     uncertainty.decl=mean(unc.norm), 
                     value=mean(value.norm), 
                     weight=n(), .groups = "keep") # adjust the uncertainty
  unc.dat$unc.norm=NA
  
  for(uu in 1:nrow(unc.dat)){
    if(is.na(unc.dat[uu,]$uncertainty.obs)){
      i.unc=0.01
    }else{
      if(unc.dat[uu,]$uncertainty.obs > unc.dat[uu,]$uncertainty.decl){
        i.unc=min(c(1,unc.dat[uu,]$uncertainty.obs))
      }
      if(unc.dat[uu,]$uncertainty.obs < unc.dat[uu,]$uncertainty.decl){
        i.unc=unc.dat[uu,]$uncertainty.decl
      }
    }
    unc.dat[uu,]$unc.norm=i.unc
    
  }
  unc.dat$value.norm=unc.dat$value
  return(unc.dat)
}      

uncertainty.function.2=function(unc.dat, col.selection){
  
  unc.dat=unc.dat%>%
    dplyr::group_by_at(col.selection)%>%
    dplyr::summarise(uncertainty.obs=sd(value.norm), 
                     uncertainty.decl=mean(unc.norm), 
                     value.norm=mean(value.norm), 
                     weight=n(), .groups = "keep") # adjust the uncertainty
  unc.dat$unc.norm=NA
  unc.dat$uncertainty.obs=ifelse(is.na(unc.dat$uncertainty.obs), 0.01, unc.dat$uncertainty.obs)
  unc.dat$unc.norm=apply(unc.dat[,c('uncertainty.obs', 'uncertainty.decl')], 1, max)
  unc.dat$unc.norm=ifelse(unc.dat$unc.norm>1,1,unc.dat$unc.norm)
  return(unc.dat)
}  

re.format.cpt=function(df.cpt, df.ref){
  df.cpt=df.cpt[, names(df.ref)]
  cols <- setdiff(names(df.ref), "Freq")
  key1 <- do.call(paste, c(df.ref[cols], sep = "\r"))
  key2 <- do.call(paste, c(df.cpt[cols], sep = "\r"))
  df.cpt <- df.cpt[match(key1, key2), ]
  return(df.cpt)
}

rank.cpt=function(nodes.df, uncertainty, algorithm = 'equal', meaning='abs', xnam){
  
  # identify nodes
  x.parents=nodes.df[nodes.df$type=='parent',]
  x.child=nodes.df[nodes.df$type=='child',]
  
  # expand combination of parent state
  state.list=list()
  for(i in 1:nrow(x.parents)){
    i.parent=x.parents[i,]
    i.states=seq(0,1,1/(i.parent$states-1))
    state.list[[i]]=i.states
    names(state.list)[i]=i.parent$node
  }
  base.cpt=do.call(expand.grid, state.list)
  
  # apply algorithm
  cpt.default=NULL
  for(i in 1:nrow(base.cpt)){
    if(algorithm == 'equal'){
      target.state.cpt=p.avg(n=100,
                             x.w = x.parents, 
                             x.state = as.numeric(base.cpt[i,]),
                             x.sd=uncertainty)  
    }else if(algorithm == 'min'){
      target.state.cpt=p.wmin(n=100,
                              x.w = x.parents, 
                              x.state = as.numeric(base.cpt[i,]),
                              x.sd=uncertainty)  
    }else if(algorithm == 'max'){
      target.state.cpt=p.wmax(n=100,
                              x.w = x.parents, 
                              x.state = as.numeric(base.cpt[i,]),
                              x.sd=uncertainty)  
    }
    
    i.cpt=data.frame(base.cpt[i,],out=round(target.state.cpt, digits = 2), row.names = NULL)
    names(i.cpt)[ncol(i.cpt)]=x.child$node
    cpt.default=rbind(cpt.default, i.cpt)
  }
  
  # discretise
  disc.cpt=cpt.default
  for(i in 1:nrow(nodes.df)){
    x.labs=xnam[[nodes.df[i,]$node]]
    if(nodes.df[i,]$direction=='neg'){
      x.labs=rev(x.labs)
    }
    i.disc=cut(cpt.default[,i], include.lowest = T,
               breaks = seq(0,1,1/length(x.labs)), labels = factor(x.labs))
    disc.cpt[,i]=i.disc
  }
  
  # probabilities
  cpt.long=disc.cpt%>%
    dplyr::group_by_all()%>%
    tally()%>%
    dplyr::mutate(prob=n/100)%>%
    dplyr::select(-n)
  
  
  # BN cpt
  nodes.df=nodes.df%>%arrange(type, desc(id))
  var.desc=list()
  var.dim=NULL
  for(i in 1:nrow(nodes.df)){
    i.nm=nodes.df[i,]$node
    i.st=nodes.df[i,]$states
    i.lv=cpt.long[,which(colnames(cpt.long)==i.nm)]
    i.lv=levels(as.data.frame(i.lv)[,1])
    var.dim=c(var.dim, i.st)
    var.desc[[i]]=i.lv
    names(var.desc)[i]=i.nm
  }
  cpt.wide=cpt.long%>%
    pivot_wider(names_from = x.child$node,
                values_from = prob)%>%
    replace(is.na(.),0)
  cpt.long2=cpt.wide%>%
    pivot_longer(-x.parents$node)
  E.prob <- array(cpt.long2$value, 
                  dim = var.dim, 
                  dimnames = var.desc)
  
  return(list(report.cpt=cpt.wide, bn.cpt=E.prob, cpt.long=cpt.long))
}


# base functions from Fenton and Neil 2007 ####
# weighted min
p.wmin=function(n=100,x.w, x.state, x.sd){
  x.w$X=x.state
  x.w$Xiwi=x.w$link.w*x.w$X
  n.i=nrow(x.w)
  x.w$mu.y=NA
  for(i in 1:nrow(x.w)){
    i.val=x.w[i,]
    j.val=x.w[-i,]
    x.w[i,]$mu.y=(i.val$Xiwi + sum(j.val$X))/(i.val$link.w + (n.i-1))
  }
  mu.y=min(x.w$mu.y)
  #sd.y=1/sum(x.w$link.w)
  p.y=rtruncnorm(n=n, a=0,b=1, mean=mu.y, sd=x.sd)
  return(p.y) 
}
# weighted max
p.wmax=function(n=100,x.w, x.state, x.sd){
  x.w$X=x.state
  x.w$Xiwi=x.w$link.w*x.w$X
  n.i=nrow(x.w)
  x.w$mu.y=NA
  for(i in 1:nrow(x.w)){
    i.val=x.w[i,]
    j.val=x.w[-i,]
    x.w[i,]$mu.y=(i.val$Xiwi + sum(j.val$X))/(i.val$link.w + (n.i-1))
  }
  mu.y=max(x.w$mu.y)
  #sd.y=1/sum(x.w$link.w)
  p.y=rtruncnorm(n=n, a=0,b=1, mean=mu.y, sd=x.sd)
  return(p.y) 
}
# weighted average
p.avg=function(n=100,x.w, x.state, x.sd){
  x.w$X=x.state
  mu.y=sum((x.w$X*x.w$link.w))/sum(x.w$link.w)
  # sd.y=1/sum(x.w$link.w)
  p.y=rtruncnorm(n=n, a=0,b=1, mean=mu.y, sd=x.sd)
  return(p.y)
}
# min max
p.mixminmax=function(n=100,x.w, x.state, x.sd){
  x.w$X=x.state
  x.w$Xiwi=x.w$link.w*x.w$X
  n.i=nrow(x.w)
  x.w$mu.y=NA
  for(i in 1:nrow(x.w)){
    i.val=x.w[i,]
    j.val=x.w[-i,]
    x.w[i,]$mu.y=(i.val$Xiwi + sum(j.val$X))/(i.val$link.w + (n.i-1))
  }
  wmin=min(x.w$mu.y)
  wmax=max(x.w$mu.y)
  mu.y=(min(x.state)*wmin+max(x.state)*wmax)/(wmin+wmax)
  #sd.y=1/sum(x.w$link.w)
  p.y=rtruncnorm(n=n, a=0,b=1, mean=mu.y, sd=x.sd)
  return(p.y)
}

initialize_equal_probabilities <- function(net, node_names) {
  for (node in node_names) {
    array.var <- net[[node]]$prob
    xdim <- dim(array.var)
    array.var[1:xdim] <- rep(1/prod(xdim), xdim)
    net[[node]] <- array.var
  }
  return(net)
}
