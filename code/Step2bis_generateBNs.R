remove(list=ls())
scriptPath <- rstudioapi::getSourceEditorContext()$path
scriptDir <- dirname(scriptPath)
setwd(file.path(scriptDir, '..')) 

for(iter in 1:100){
  source('code/Step3_BEWARE_r0_0.R')
  x.files=list.files('results/iterations')
  bnlearn::write.net( paste0('results/iterations/BEWARE_simuliter_',length(x.files)+1,'.net'), net)
}
