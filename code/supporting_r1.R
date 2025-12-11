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

answer.to.cpt=function(x.ans, x.range=c(1,5),dim.name, dim.labs = c('L','M','H'), unc=0.05, k.thr=2, x.breaks=NULL){
  # this is tested and working to import interviewee answers in CPT which configuration is defined in GeNie
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=x.range[1],b=x.range[2],n=100, mean=x.ans, sd=unc)
  if(is.null(x.breaks)){
   x.breaks=seq(x.range[1],x.range[2],(x.range[2]-x.range[1])/length(dim.labs)) 
  }
  x.breaks[1]=-Inf
  x.breaks[length(x.breaks)]=Inf
  if(length(dim.labs)==2 & x.range[2]-x.range[1] >1){
    base.dist=log.reg(l=1,k=k.thr, x=base.dist, x0=(x.range[2]+x.range[1])/2) 
    prob.tab=array(c(1-mean(base.dist),mean(base.dist)), dim = length(dim.labs), dimnames = dim.list) 
  }else{
    base.dist=cut(base.dist, breaks=x.breaks, labels=dim.labs)
    prob.tab=array(table(base.dist)/100, dim = length(dim.labs), dimnames = dim.list) 
  }
  #mean(base.dist)
  
  return(prob.tab)
}

log.reg=function(l,k,x0=0,x){
  l/(1+exp(-k*(x-x0)))
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