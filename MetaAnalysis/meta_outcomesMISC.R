#require(devtools)
#install_version("exactmeta", version = "1.0.2", repos = "http://cran.us.r-project.org")
library(metafor)
library(exactmeta)

sample_size = matrix(c(98,16,23,137,
                       14,10,13,37,
                       13,4,8,25,
                       30,18,6,54,
                       113,23,7,143,
                       4,14,13,31),ncol=4,byrow=TRUE)
sample_size = data.frame(sample_size)
colnames(sample_size) = c("alpha","delta","omicron","total")
sample_size$site = c("BCH","FRBDX","H12O","PittCHP","RP401ped","UMICH")
site_list = c("BCH","FRBDX","H12O","PittCHP","RP401ped","UMICH")

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

outcome_allsite = vector("list",length(site_list))
for (i in 1:length(site_list)){
  
  filename = paste(site_list[i], "/replace_earlier/",site_list[i],"_table3.RData",sep="")
  #filename = paste("table1_diagnosis/",site_list[i],"_table1Categorical.RData",sep="")
  load(filename)
  table3 = data.frame(table3)
  count_wide = reshape(table3,direction="wide",timevar="variant_misc",idvar="categories")
  rownames(count_wide) = NULL
  count_wide[is.na(count_wide)] = 0
  if (i == 6){
    count_wide[,2:4] = round(count_wide[,2:4] + 0.0001)
  }
  outcome_allsite[[i]] = count_wide
}
names(outcome_allsite) = site_list

outcome_list = outcome_allsite[[1]]$categories


data_alloutcome = vector("list",length(outcome_list))
for (i in 1:length(outcome_list)){
  allsite = NULL
  for (j in 1:length(site_list)){
    index = which(outcome_allsite[[j]]$categories == outcome_list[i])
    if (length(index) == 0){
      print(paste("no patient with",outcome_list[i],"in site",site_list[j],sep=" "))
      allsite = rbind(allsite,c(0,0,0,0))
    } else {
      allsite = rbind(allsite,outcome_allsite[[j]][index,-1])
    }
  }
  allsite$site = site_list
  data_alloutcome[[i]] = allsite
}

names(data_alloutcome) = outcome_list

####################
### exact method ###
####################

res.exact.delta = vector("list",length(outcome_list))
for (i in 1:length(outcome_list)){
  print(paste0("#####", i))
  dat.exact = cbind(data_alloutcome[[i]][,1:2],sample_size[,1:2])
  res.exact = meta.exact(dat.exact, type="risk difference")
  res.exact.delta[[i]] = res.exact
}

res.exact.omicron = vector("list",length(outcome_list))
for (i in 1:length(outcome_list)){
  print(paste0("#####", i))
  dat.exact = cbind(data_alloutcome[[i]][,c(1,3)],sample_size[,c(1,3)])
  res.exact = meta.exact(dat.exact, type="risk difference")
  res.exact.omicron[[i]] = res.exact
}

save(res.exact.delta,res.exact.omicron,file="exactresult_outcomes.RData")

#############################
### approach II: exact CI ###
#############################
load("exactresult_outcomes.RData")
library(dplyr)

delta.exact = NULL
for (i in 1:length(res.exact.delta)){
  delta.exact = rbind(delta.exact,res.exact.delta[[i]]$ci.fixed[,2])
}
delta.exact = data.frame(delta.exact)
delta.exact$outcome = outcome_list
print( delta.exact %>% filter( p < 0.05 ))

omicron.exact = NULL
for (i in 1:length(res.exact.omicron)){
  omicron.exact = rbind(omicron.exact,res.exact.omicron[[i]]$ci.fixed[,2])
}
omicron.exact = data.frame(omicron.exact)
omicron.exact$outcome = outcome_list

### stat significant
print( omicron.exact %>% filter( p < 0.05 ))

##################################################
### approach III: exact site CI, standard meta ###
##################################################
delta.hybrid = NULL
transform = FALSE

#for (i in 1:length(res.exact.delta)){
for (i in c(1:length(res.exact.delta))){
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
delta.hybrid$outcome = outcome_list

print( delta.hybrid %>% filter( pval < 0.05))


omicron.hybrid = NULL
#for (i in 1:length(res.exact.omicron)){
for (i in c(1:length(res.exact.omicron))){
  
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
omicron.hybrid$outcome = outcome_list
print( omicron.hybrid %>% filter( pval < 0.05))


### put the results together
delta_outcome <- delta.hybrid %>%
  select( outcome, delta_pval = pval )

omicron_outcome <- omicron.hybrid %>%
  select( outcome, omicron_pval = pval )

outcome_results <- merge( delta_outcome, omicron_outcome)
