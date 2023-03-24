#require(devtools)
#install_version("exactmeta", version = "1.0.2", repos = "http://cran.us.r-project.org")
library(metafor)
library(exactmeta)

sample_size = matrix(c(98,16,23,137,
                       108,24,30,162,
                       14,10,13,37,
                       13,4,8,25,
                       30,18,6,54,
                       113,23,7,143,
                       4,14,13,31),ncol=4,byrow=TRUE)
sample_size = data.frame(sample_size)
colnames(sample_size) = c("alpha","delta","omicron","total")
sample_size$site = c("BCH","CHOP","FRBDX","H12O","PittCHP","RP401ped","UMICH")
site_list = c("BCH","CHOP","FRBDX","H12O","PittCHP","RP401ped","UMICH")

setwd("/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")


################################
################################
##### binary outcome ###########
################################
################################
ft = function(x){
  0.5*log((1+x)/(1-x))
}

ft.inverse = function(x){
  (exp(2*x)-1)/(exp(2*x)+1)
}

data_allsite = vector("list",length(site_list))
for (i in 1:length(site_list)){
  
  filename = paste(site_list[i], "/replace_earlier/",site_list[i],"_table1Categorical.RData",sep="")
#filename = paste("table1_diagnosis/",site_list[i],"_table1Categorical.RData",sep="")
  load(filename)
  table1_categorical = data.frame(table1_categorical)
  count_wide = reshape(table1_categorical,direction="wide",timevar="variant_misc",idvar="categories")
  rownames(count_wide) = NULL
  count_wide[is.na(count_wide)] = 0
  if (i == 6){
    count_wide[,2:4] = round(count_wide[,2:4] + 0.0001)
  }
  data_allsite[[i]] = count_wide
}
names(data_allsite) = site_list

diag_list = c("CARDIOVASCULAR SYMPTOMS","COVID-19","GI SYMPTOMS",
              "LIVER DYSFUNCTION","NEUROLOGIC SYMPTOMS","RENAL DYSFUNCTION",
              "RESPIRATORY SYMPTOMS","SHOCK/SIRS")
#diag_list = data_allsite[[1]]$categories


data_alldiag = vector("list",length(diag_list))
for (i in 1:length(diag_list)){
  allsite = NULL
  for (j in 1:length(site_list)){
    index = which(data_allsite[[j]]$categories == diag_list[i])
    if (length(index) == 0){
      print(paste("no patient with",diag_list[i],"in site",site_list[j],sep=" "))
      allsite = rbind(allsite,c(0,0,0,0))
    } else {
      allsite = rbind(allsite,data_allsite[[j]][index,-1])
    }
  }
  allsite$site = site_list
  data_alldiag[[i]] = allsite
}

#for (i in 1:length(diag_list)){
#print(apply(data_alldiag[[i]][,1:4],2,sum)) 
#}
names(data_alldiag) = diag_list

####################
### exact method ###
####################

res.exact.delta = vector("list",length(diag_list))
for (i in 1:length(diag_list)){
  print(paste0("#####", i))
  dat.exact = cbind(data_alldiag[[i]][,1:2],sample_size[,1:2])
  res.exact = meta.exact(dat.exact, type="risk difference")
  res.exact.delta[[i]] = res.exact
}

res.exact.omicron = vector("list",length(diag_list))
for (i in 1:length(diag_list)){
  print(paste0("#####", i))
  dat.exact = cbind(data_alldiag[[i]][,c(1,3)],sample_size[,c(1,3)])
  res.exact = meta.exact(dat.exact, type="risk difference")
  res.exact.omicron[[i]] = res.exact
}

#save(res.exact.delta,res.exact.omicron,file="exactresult_all.RData")
save(res.exact.delta,res.exact.omicron,file="exactresult.RData")

#############################
### approach II: exact CI ###
#############################
load("exactresult_all.RData")
load("exactresult.RData")
library(dplyr)

delta.exact = NULL
for (i in 1:length(res.exact.delta)){
  delta.exact = rbind(delta.exact,res.exact.delta[[i]]$ci.fixed[,2])
}
delta.exact = data.frame(delta.exact)
delta.exact$outcome = diag_list
print( delta.exact %>% filter( p < 0.01 ))

omicron.exact = NULL
for (i in 1:length(res.exact.omicron)){
  omicron.exact = rbind(omicron.exact,res.exact.omicron[[i]]$ci.fixed[,2])
}
omicron.exact = data.frame(omicron.exact)
omicron.exact$outcome = diag_list

### stat significant
print( omicron.exact %>% filter( p < 0.01 ))

##################################################
### approach III: exact site CI, standard meta ###
##################################################
delta.hybrid = NULL
transform = FALSE

#for (i in 1:length(res.exact.delta)){
for (i in c(1:15,17:length(res.exact.delta))){
  print(i)
  yi = NULL
  yi.alt = NULL
  vi = NULL
  CIs = res.exact.delta[[i]]$study.ci
  if (transform){
    for (j in 1:nrow(CIs)){
      yi = c(yi, (ft(CIs[j,2])+ft(CIs[j,3]))/2)
      yi.alt = c(yi.alt, ft(CIs[j,1]))
      vi = c(vi,(ft(CIs[j,3]) - ft(CIs[j,2]))/2/qnorm(0.975))
    }
    vi = vi^2
    mod = rma.uni(yi,vi)
    delta.hybrid = rbind(delta.hybrid, c(ft.inverse(mod$beta),ft.inverse(mod$ci.lb), ft.inverse(mod$ci.ub), mod$pval))
  } else {
    for (j in 1:nrow(CIs)){
      yi = c(yi, (CIs[j,2]+CIs[j,3])/2)
      yi.alt = c(yi.alt, CIs[j,1])
      vi = c(vi,(CIs[j,3] - CIs[j,2])/2/qnorm(0.975))
    }
    vi = vi^2
    mod = rma.uni(yi,vi)
    delta.hybrid = rbind(delta.hybrid, c(mod$beta,mod$ci.lb, mod$ci.ub, mod$pval))
  }
}
delta.hybrid = data.frame(delta.hybrid)
colnames(delta.hybrid) = c("est","ci.lb","ci.ub","pval")
delta.hybrid$outcome = diag_list[-16]

print( delta.hybrid %>% filter( pval < 0.05))


omicron.hybrid = NULL
#for (i in 1:length(res.exact.omicron)){
for (i in c(1:67,69:length(res.exact.omicron))){
  
  print(i)
  yi = NULL
  yi.alt = NULL
  vi = NULL
  CIs = res.exact.omicron[[i]]$study.ci
  if (transform){
    for (j in 1:nrow(CIs)){
      yi = c(yi, (ft(CIs[j,2])+ft(CIs[j,3]))/2)
      yi.alt = c(yi.alt, ft(CIs[j,1]))
      vi = c(vi,(ft(CIs[j,3]) - ft(CIs[j,2]))/2/qnorm(0.975))
    }
    vi = vi^2
    mod = rma.uni(yi,vi)
    omicron.hybrid = rbind(omicron.hybrid, c(ft.inverse(mod$beta),ft.inverse(mod$ci.lb), ft.inverse(mod$ci.ub), mod$pval))
  } else {
    for (j in 1:nrow(CIs)){
      yi = c(yi, (CIs[j,2]+CIs[j,3])/2)
      yi.alt = c(yi.alt, CIs[j,1])
      vi = c(vi,(CIs[j,3] - CIs[j,2])/2/qnorm(0.975))
    }
    vi = vi^2
    mod = rma.uni(yi,vi)
    omicron.hybrid = rbind(omicron.hybrid, c(mod$beta,mod$ci.lb, mod$ci.ub, mod$pval))
  }
}
omicron.hybrid = data.frame(omicron.hybrid)
colnames(omicron.hybrid) = c("est","ci.lb","ci.ub","pval")
omicron.hybrid$outcome = diag_list[-68]
print( omicron.hybrid %>% filter( pval < 0.05))

################################
################################
##### continuous outcome #######
################################
################################
# meta analysis of two-sample t-test
lab_bysite = vector("list",length(site_list))
for (i in 1:length(site_list)){
  print(i)
  filename = paste(site_list[i], "/replace_earlier/",site_list[i],"_table2AtAdmission.RData",sep="")
  #filename = paste0("table2/",site_list[i],"_table2AtAdmission.RData")
  load(filename)
  
  table2_admission = table2_admission %>% filter( ! is.na( median_value ))
  mean_sd = data.frame(table2_admission[,c("variant_misc","variableName","mean_value","sd_value","n_patients")])
  mean_sd_reshape = reshape(mean_sd,direction="wide",idvar="variableName",timevar="variant_misc")
  lab_bysite[[i]] = mean_sd_reshape
}
for (i in 1:length(site_list)){
  print(dim(lab_bysite[[i]]))
}

lab_names_list <- lab_bysite[[1]]$variableName
#this_lab = "albumin"
#labs_results <- as.data.frame( matrix( ncol = 5, nrow = length(lab_names_list)))
#colnames( labs_results) <- c("lab", "alpha_delta_pval", "alpha_omicron_pval",  "alpha_delta_pval_ee", "alpha_omicron_pval_ee")
labs_results <- as.data.frame( matrix( ncol = 3, nrow = length(lab_names_list)))
colnames( labs_results) <- c("lab", "alpha_delta_pval", "alpha_omicron_pval")

outputs_to_plot <- list()

for( j in 1:length(lab_names_list)){
  
  this_lab = lab_names_list[j]
  print(this_lab)
  
  # all sites measured albumin, we take albumin as an example
  diff_alpha_delta = rep(0,length(site_list))
  var.diff_alpha_delta = rep(0,length(site_list))
  
  diff_alpha_omicron = rep(0,length(site_list))
  var.diff_alpha_omicron = rep(0,length(site_list))
  
  thres = 0
  sites.exclude = NULL
  for (i in 1:length(site_list)){
    
    #print(i)
    index = which(lab_bysite[[i]]$variableName == this_lab)
    
    if( !any(index) ){
      sites.exclude = c(sites.exclude,i)
    }else{
      min.size = min(lab_bysite[[i]]$n_patients.Alpha[index],lab_bysite[[i]]$n_patients.Delta[index],lab_bysite[[i]]$n_patients.Omicron[index])
      if (min.size < thres | is.na(min.size)){
        sites.exclude = c(sites.exclude,i)
      }
      diff_alpha_delta[i] = lab_bysite[[i]]$mean_value.Delta[index] - lab_bysite[[i]]$mean_value.Alpha[index]
      var.diff_alpha_delta[i] = lab_bysite[[i]]$sd_value.Delta[index]^2/sample_size[i,2] + lab_bysite[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
      
      diff_alpha_omicron[i] = lab_bysite[[i]]$mean_value.Omicron[index] - lab_bysite[[i]]$mean_value.Alpha[index]
      var.diff_alpha_omicron[i] = lab_bysite[[i]]$sd_value.Omicron[index]^2/sample_size[i,3] + lab_bysite[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
      
    }
     }
  
  set.seed(123)
  #rma(diff[-sites.exclude], var.diff[-sites.exclude], method="EE")
  if( is.null(sites.exclude )){
    stats_output_alpha_delta <- rma(diff_alpha_delta, var.diff_alpha_delta)
    stats_output_alpha_omicron <- rma(diff_alpha_omicron, var.diff_alpha_omicron)
   
    #stats_output_alpha_delta_ee <- rma(diff_alpha_delta, var.diff_alpha_delta, method="EE")
    #stats_output_alpha_omicron_ee <- rma(diff_alpha_omicron, var.diff_alpha_omicron, method="EE")
    
    #stats_output <- rma(diff, var.diff, method="EE")
  }else{
    stats_output_alpha_delta  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude])
    stats_output_alpha_omicron  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude])
    
    #stats_output_alpha_delta_ee  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude], method="EE")
    #stats_output_alpha_omicron_ee  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude], method="EE")
    
  }

  labs_results$lab[j] <- this_lab
  labs_results$alpha_delta_pval[j] <- stats_output_alpha_delta$pval
  labs_results$alpha_omicron_pval[j] <- stats_output_alpha_omicron$pval
  
  if( stats_output_alpha_delta$pval < 0.05){
    outputs_to_plot[[this_lab]] <- stats_output_alpha_delta
  }
  if( stats_output_alpha_omicron$pval < 0.05){
    outputs_to_plot[[this_lab]] <- stats_output_alpha_omicron
  }
  #labs_results$alpha_delta_pval_ee[j] <- stats_output_alpha_delta_ee$pval
  #labs_results$alpha_omicron_pval_ee[j] <- stats_output_alpha_omicron_ee$pval
}

forest( outputs_to_plot[[1]])
forest( outputs_to_plot[[2]])

###### During admission
# meta analysis of two-sample t-test
lab_bysite = vector("list",length(site_list))
for (i in 1:length(site_list)){
  print(i)
  filename = paste(site_list[i], "/replace_earlier/",site_list[i],"_table2DuringAdmission.RData",sep="")
  load(filename)
  
  table2_during = table2_during %>% filter( ! is.na( median_value ))
  mean_sd = data.frame(table2_during[,c("variant_misc","variableName","mean_value","sd_value","n_patients")])
  mean_sd_reshape = reshape(mean_sd,direction="wide",idvar="variableName",timevar="variant_misc")
  lab_bysite[[i]] = mean_sd_reshape
}
for (i in 1:length(site_list)){
  print(dim(lab_bysite[[i]]))
}

lab_names_list <- lab_bysite[[1]]$variableName
#this_lab = "albumin"
#labs_results <- as.data.frame( matrix( ncol = 5, nrow = length(lab_names_list)))
#colnames( labs_results) <- c("lab", "alpha_delta_pval", "alpha_omicron_pval",  "alpha_delta_pval_ee", "alpha_omicron_pval_ee")
labs_results <- as.data.frame( matrix( ncol = 3, nrow = length(lab_names_list)))
colnames( labs_results) <- c("lab", "alpha_delta_pval", "alpha_omicron_pval")

outputs_to_plot <- list()

for( j in 1:length(lab_names_list)){
  
  this_lab = lab_names_list[j]
  print(this_lab)
  
  # all sites measured albumin, we take albumin as an example
  diff_alpha_delta = rep(0,length(site_list))
  var.diff_alpha_delta = rep(0,length(site_list))
  
  diff_alpha_omicron = rep(0,length(site_list))
  var.diff_alpha_omicron = rep(0,length(site_list))
  
  thres = 0
  sites.exclude = NULL
  for (i in 1:length(site_list)){
    
    #print(i)
    index = which(lab_bysite[[i]]$variableName == this_lab)
    
    if( !any(index) ){
      sites.exclude = c(sites.exclude,i)
    }else{
      min.size = min(lab_bysite[[i]]$n_patients.Alpha[index],lab_bysite[[i]]$n_patients.Delta[index],lab_bysite[[i]]$n_patients.Omicron[index])
      if (min.size < thres | is.na(min.size)){
        sites.exclude = c(sites.exclude,i)
      }
      diff_alpha_delta[i] = lab_bysite[[i]]$mean_value.Delta[index] - lab_bysite[[i]]$mean_value.Alpha[index]
      var.diff_alpha_delta[i] = lab_bysite[[i]]$sd_value.Delta[index]^2/sample_size[i,2] + lab_bysite[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
      
      diff_alpha_omicron[i] = lab_bysite[[i]]$mean_value.Omicron[index] - lab_bysite[[i]]$mean_value.Alpha[index]
      var.diff_alpha_omicron[i] = lab_bysite[[i]]$sd_value.Omicron[index]^2/sample_size[i,3] + lab_bysite[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
      
    }
  }
  
  set.seed(123)
  #rma(diff[-sites.exclude], var.diff[-sites.exclude], method="EE")
  if( is.null(sites.exclude )){
    stats_output_alpha_delta <- rma(diff_alpha_delta, var.diff_alpha_delta)
    stats_output_alpha_omicron <- rma(diff_alpha_omicron, var.diff_alpha_omicron)
    
    #stats_output_alpha_delta_ee <- rma(diff_alpha_delta, var.diff_alpha_delta, method="EE")
    #stats_output_alpha_omicron_ee <- rma(diff_alpha_omicron, var.diff_alpha_omicron, method="EE")
    
    #stats_output <- rma(diff, var.diff, method="EE")
  }else{
    stats_output_alpha_delta  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude])
    stats_output_alpha_omicron  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude])
    
    #stats_output_alpha_delta_ee  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude], method="EE")
    #stats_output_alpha_omicron_ee  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude], method="EE")
    
  }
  
  labs_results$lab[j] <- this_lab
  labs_results$alpha_delta_pval[j] <- stats_output_alpha_delta$pval
  labs_results$alpha_omicron_pval[j] <- stats_output_alpha_omicron$pval
  
  if( stats_output_alpha_delta$pval < 0.05){
    outputs_to_plot[[this_lab]] <- stats_output_alpha_delta
  }
  if( stats_output_alpha_omicron$pval < 0.05){
    outputs_to_plot[[this_lab]] <- stats_output_alpha_omicron
  }
  #labs_results$alpha_delta_pval_ee[j] <- stats_output_alpha_delta_ee$pval
  #labs_results$alpha_omicron_pval_ee[j] <- stats_output_alpha_omicron_ee$pval
}

forest( outputs_to_plot[[1]])
forest( outputs_to_plot[[2]])

###### if everything else fails then try this

####################################
### approach one: metafor for RD ###
####################################
res.delta = NULL
res.omicron = NULL
for (i in 1:length(data_alldiag)){
  ai = NULL
  bi = NULL
  ci = NULL
  di = NULL
  for (j in 1:nrow(data_alldiag[[i]])){
    ci = c(ci,data_alldiag[[i]][j,"n.Alpha"])
    di = c(di,sample_size[which(sample_size$site == data_alldiag[[i]][j,"site"]),"alpha"]-data_alldiag[[i]][j,"n.Alpha"])
    ai = c(ai,data_alldiag[[i]][j,"n.Delta"])
    bi = c(bi,sample_size[which(sample_size$site == data_alldiag[[i]][j,"site"]),"delta"]-data_alldiag[[i]][j,"n.Delta"])
  }
  delta.random = rma(measure="RD",ai=ai,bi=bi,ci=ci,di=di)
  delta.fix = rma(measure="RD",ai=ai,bi=bi,ci=ci,di=di,method="EE")
  res.delta = rbind(res.delta,c(delta.random$ci.lb,delta.random$ci.ub,delta.random$pval,
                                delta.fix$ci.lb,delta.fix$ci.ub,delta.fix$pval))
  
  ai = NULL
  bi = NULL
  ci = NULL
  di = NULL
  for (j in 1:nrow(data_alldiag[[i]])){
    ci = c(ci,data_alldiag[[i]][j,"n.Alpha"])
    di = c(di,sample_size[which(sample_size$site == data_alldiag[[i]][j,"site"]),"alpha"]-data_alldiag[[i]][j,"n.Alpha"])
    ai = c(ai,data_alldiag[[i]][j,"n.Omicron"])
    bi = c(bi,sample_size[which(sample_size$site == data_alldiag[[i]][j,"site"]),"omicron"]-data_alldiag[[i]][j,"n.Omicron"])
  }
  omicron.random = rma(measure="RD",ai=ai,bi=bi,ci=ci,di=di)
  omicron.fix = rma(measure="RD",ai=ai,bi=bi,ci=ci,di=di,method="EE")
  res.omicron = rbind(res.omicron,c(omicron.random$ci.lb,omicron.random$ci.ub,omicron.random$pval,
                                    omicron.fix$ci.lb,omicron.fix$ci.ub,omicron.fix$pval))
}
colnames(res.delta) = c("low.random","up.random","pval.random","low.fix","up.fix","pval.fix")
colnames(res.omicron) = colnames(res.delta)

