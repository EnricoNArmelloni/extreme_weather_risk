library(truncnorm)

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
# random testing
#weights.df=data.frame(node.from=c('A','B','C'), 
#                      node.to=rep('D',3),
#                      link.w=c(100,150,180)); weights.df
#states.v=c(0.1,0.5,0.8)
#x.w=weights.df
#hist(p.avg(x.w = weights.df, x.state = c(0.3,0.5,0.8)))
#hist(p.wmin(x.w = weights.df, x.state = c(0.3,0.5,0.8)))
#hist(p.wmax(x.w = weights.df, x.state = c(0.3,0.5,0.8)))


get.cpt=function(nodes.df, uncertainty, algorithm = 'equal', meaning='abs'){
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
    
    if(meaning=='abs'){
      if(nodes.df[i,]$states==2){
         x.labs=c('L','H')
       }else if(nodes.df[i,]$states==3){
         x.labs=c('L','M','H')
       }else if(nodes.df[i,]$states==4){
         x.labs=c('L','ML', 'MH', 'H')
       }else if(nodes.df[i,]$states==5){
         x.labs=c('VL', 'L','M','H','VH')
       }
      
    }else if(meaning=='rel'){
      if(nodes.df[i,]$states==2){
        x.labs=c('N','P')
      }else if(nodes.df[i,]$states==3){
        x.labs=c('N','A','P')
      }else if(nodes.df[i,]$states==4){
        x.labs=c('N','AN', 'AP', 'P')
      }else if(nodes.df[i,]$states==5){
        x.labs=c('VN', 'N','A','P','VP')
      }
    }

    if(nodes.df[i,]$direction=='neg'){
      x.labs=rev(x.labs)
    }
    i.disc=cut(cpt.default[,i], include.lowest = T, breaks = seq(0,1,1/(nodes.df[i,]$states)), labels = factor(x.labs))
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
  
  return(list(report.cpt=cpt.wide, bn.cpt=E.prob))
}

answer.to.cpt=function(x.ans, dim.name, div.factor=5, 
                       x.breaks=c(-Inf, 0.33,0.66,Inf),
                       dim.labs = c('L','M','H'), unc=0.05){
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rtruncnorm(a=0,b=1,n=100, mean=x.ans/div.factor, sd=unc)
  base.dist=cut(base.dist, breaks=x.breaks, labels=dim.labs)
  prob.tab=array(table(base.dist)/100, dim = length(dim.labs), dimnames = dim.list)
  return(prob.tab)
}



cpt.2=function(x.ans, dim.name, dim.labs = c('L','M','H'), unc=0.05){
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rnorm(n=100, mean=x.ans/5, sd=unc)
  base.dist=cut(base.dist, breaks=c(-Inf, 0.33,0.66,Inf), labels=dim.labs)
  prob.tab=array(table(base.dist)/100, dim = 3, dimnames = dim.list)
  return(prob.tab)
}

expand.probs=function(x.prob, x.lab){
  dim.list <- list(c('L','M','H'),c('coastal','trawlers', 'recr') )
  names(dim.list) <- c(x.lab,'fishery')
  array(cbind(x.prob, x.prob,x.prob), 
        dim = c(3,3), 
        dimnames = dim.list)
  
}

expand.probs=function(x.levels,x.names,x.probs){
  dim.list <- x.levels
  names(dim.list) <-   x.names
  array(do.call(cbind, x.probs), 
        dim = lengths(x.levels), 
        dimnames = dim.list)
}


cpt.bin=function(x.ans, dim.name, dim.labs = c('Y','N')){
  dim.list <- list(dim.labs)
  names(dim.list) <- dim.name
  base.dist=rnorm(n=100, mean=x.ans/5, sd=0.05)
  base.dist=cut(base.dist, breaks=c(-Inf, 0.5,Inf), labels=dim.labs)
  prob.tab=array(table(base.dist)/100, dim = 2, dimnames = dim.list)
  return(prob.tab)
}

seasonal.figures=function(fishing, stress,injury,inv.cost, fishing.cost, fishing.event, gc, 
                          brek.gear,n,fd0,costs, catch0,
                          p0, fref, investment, management=0){
  
  fdref=fd0
  fref=fref+(fref*management)
  fd0=fd0+(fd0*management)
  
  # fishing time
  s.ft=(fd0-(n*(1-fishing)))/fd0 # fishing
  ft.variation.ref=(fd0-(n*(1-fishing)))/fdref
  f.event=n*fishing*fishing.event # fishing in the event
  
  # getting injuried
  s.inj=(n*fishing*fishing.event*injury)/p0 ## to be revised
  injuries.vec=numeric(100)
  for(k in 1:100){
    injuries=rbinom(n=round(f.event), size=1, prob=injury)
    number.injuries=length(injuries[injuries==1])
    #if(number.injuries>1){number.injuries=2}
    injuries.vec[k]=number.injuries
  }
  d.inj=cut(injuries.vec, breaks = c(-Inf,0.8,1.2,Inf), labels = c('L','M','H'))
  injuries.prob=table(d.inj)/100
  
  s.str=(n*stress)/fd0
  s.gc=1-((n*fishing*(1-gc)))/(fd0*s.ft)
  
  cost.by.trip=sample(costs$value, size=f.event, prob=costs$probs.Freq, replace=T)
  s.cost=sum(cost.by.trip)+investment# miss the prob
  s.lc=((n*fishing*(brek.gear)))/(fd0*s.ft) # losing the gear with the catches
  
  # model values
  pop.scen=pop.dyn(T=2000, K=50000,s_A = 0.75,fmort=fref*s.ft,regime=0.5,init=50,sigma=0.05)
  catch=mean(pop.scen$catch.a + pop.scen$catch.j)
  daily.exp=(catch/(fd0*s.ft))
  fish.size=mean(pop.scen$size)
  stock.status=mean(pop.scen$B_t)
  size.variation=(size0-fish.size)*100
  
  # p of loosing the catches when there is an event
  p.loose=fishing.event*fishing*brek.gear
  realised.catch=daily.exp* ((fd0*s.ft) - n*(p.loose))
  catch.variation=((realised.catch-catch0)/catch0)*100
  
  # economics
  realised.income=(realised.catch/catch0)*income0
  realised.cost=s.cost+(cost0*s.ft)
  realised.profit=(realised.income-realised.cost)
  profit.variation=((realised.profit-profit0)/profit0)*100
  
  
  # discretise
  d.stress=cut(s.str, breaks = c(-Inf,0.05,0.2,Inf),labels = c('L','M','H'))
  #d.ft=cut(((ft.variation.ref-1))*100, breaks = c(-Inf, -50,-10,10,50,Inf), labels=c('VN', 'N', 'A', 'P','VP')) # Very negative, negative, average (or negligible), positive, very positive
  
  d.ft=cut(((ft.variation.ref-1))*100, breaks = c(-Inf, -10,10,Inf), labels=c('L', 'M', 'H')) # Very negative, negative, average (or negligible), positive, very positive
  
  #d.cost=cut(s.cost, breaks = c(-Inf,0.1,0.25,0.75,1.5,Inf), labels = c('VL', 'L','M','H','VH'))
  d.stock=cut(stock.status, breaks = c(-Inf,0.3,0.5,Inf), labels = c('below','between','above'))
  d.profit=cut(profit.variation, breaks = c(-Inf, -50,-10,10,50,Inf), labels=c('VN', 'N', 'A', 'P','VP'))
  
  d.catch=cut(catch.variation, breaks = c(-Inf, -10,10,Inf), labels=c('L', 'M', 'H')) # it means worse, same, better
  d.size=cut(size.variation, breaks = c(-Inf, -10,10,Inf), labels=c('L', 'M', 'H')) # or smaller, average, bigger
  d.gc=cut(s.gc, breaks = c(-Inf,0.8,0.95,Inf), labels = c('L', 'M', 'H')) 
  
  
  d.profit.bn=cut(profit.variation, breaks = c(-Inf, -50,-10,Inf), labels=c('L', 'M', 'H'))
  
  res=data.frame(
    stress=d.stress,
    fishing.time=d.ft,
    catch.quality=d.gc,
    catch.size=d.size,
    catch=d.catch,
    biom=d.stock) 
  return(list(res, d.profit, injuries.prob, d.profit.bn)) 
}

pop.dyn=function(T,K,s_A,fmort,regime,fpart=c(0.3,0.7),init,sigma){
  
  # Storage
  g_rate <-2*regime
  r=2*(1-regime)
  J <- A <- B <- B_t <-catch.j <- catch.a <-numeric(T)
  J[1] <- K/3
  A[1] <- K/2
  recruit <- function(A, B) {
    r * A * (1 - B / K)         # density-dependent recruitment
  }
  grow <- function(J) {
    g_rate * J
  }
  # Simulation loop
  for (t in 1:(T-1)) {
    #B_t <- J[t] + A[t]
    
    # apply fishery current year
    if(t> init){
      #catch=fmort*(J[t]+A[t])
      catch.j[t]=fpart[1]*fmort*(J[t])
      catch.a[t]=fpart[2]*fmort*(A[t])
    }else{
      catch.j[t]=0
      catch.a[t]=0
    }
    A[t]=A[t]-catch.a[t]
    J[t]=J[t]-catch.j[t]
    
    # generate recruit next year
    R <- recruit(A[t], A[t]+J[t])
    J[t+1] <-max(0, R * exp(rnorm(1, 0, sigma)))
    # generate adults next year
    G <- grow(J[t])
    A[t+1] <- s_A * A[t] + G
    
    # Prevent negative values
    J[t+1] <- max(J[t+1], 0)
    A[t+1] <- max(A[t+1], 0)
    
    # register results
    B_t[t] <- (J[t] + A[t])/K
    
  }
  
  res=data.frame(A=A/1000,J=J/1000,catch.a=catch.a/1000, catch.j=catch.j/1000, B_t)
  res$size=res$catch.a/(res$catch.a+res$catch.j)
  res=res[(nrow(res)-10):nrow(res)-1,]
  return(res)
  
  
}