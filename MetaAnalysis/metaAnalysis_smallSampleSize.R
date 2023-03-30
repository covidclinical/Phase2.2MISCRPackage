##############################################
###### Meta-analysis: small sample size ######
######Code developped by: Xiudi Li      ######
# Adapted by: Alba Gutierrez & Simran Makwana #
##############################################

## Libraries installation
#require(devtools)
#install_version("exactmeta", version = "1.0.2", repos = "http://cran.us.r-project.org")
library(metafor)
library(exactmeta)
library(dplyr)

## Read the file with the sample size for alpha, delta and omicron for each site
sample_size <- read.delim("/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/sample_size.txt")
sample_size <- sample_size[ sample_size$site != "CHOP", ]

#####################
##### Functions #####
#####################
ft = function(x){
  0.5*log((1+x)/(1-x))
}

ft.inverse = function(x){
  (exp(2*x)-1)/(exp(2*x)+1)
}

# binary outcome read and format the data
# this function formats the data to have as output a list with a data.frame 
# that contains one data.frame per site with the counts for all the characteristics
# each row represents a characteristic (diagnosis, outcome, etc) 
binary_outcome_data <- function( site_df, rdata_fileName, files_path ){
  
  #we do not have outcome data for chop, so for table 3 we exclude it from the df
  #if( rdata_fileName == "table3.RData" ){
  #  site_df <- site_df[-which(site_df$site == "CHOP"), ]
  #}
  
  #create the list of sites
  site_list <- site_df$site
  output_allsite = vector("list",length(site_list))
  
  for (i in 1:length(site_list)){
    #automatic load of the file
    filename = paste(files_path, site_list[i], "/replace_earlier/",site_list[i],"_", rdata_fileName,sep="")
    load(filename)
    
    if(rdata_fileName == "table3.RData"){
      dataFile <- table3
    }else if( rdata_fileName == "table1Categorical.RData"){
      dataFile <- table1_categorical
    }

    dataFile = data.frame(dataFile)
    count_wide = reshape(dataFile,direction="wide",timevar="variant_misc",idvar="categories")
    rownames(count_wide) = NULL
    count_wide[is.na(count_wide)] = 0
    
    ### if the site has obfuscation round the values
    if (site_df$obfuscation[i] == TRUE){
      count_wide[,2:4] = round(count_wide[,2:4] + 0.0001)
    }
    output_allsite[[i]] = count_wide
  }
  names(output_allsite) = site_list
  
  return( output_allsite)
}

# this function formats the data to have as output a list with a data.frame 
# that contains the counts one data frame with characteristic and 
# each row represents a site 
format_data_to_meta_analysis <- function( input_char_to_evaluate, input_list_df ){
  
  # create a vector with all the sites 
  site_list <- names( input_list_df )
  
  # create an empty list with the characteristics for meta-analysis
  data_all_variables = vector("list",length(input_char_to_evaluate))
  
  # for each diag/outcome in the input list
  for (i in 1:length(input_char_to_evaluate)){
    
    allsite_output = NULL
    
    # for each site in the list
    for (j in 1:length(site_list)){
      
      # find in which row we had the information for the variable of interest
      index = which(input_list_df[[j]]$categories == input_char_to_evaluate[i])
      
      # if is not present (means that this site has no patient with this characteristic)
      if (length(index) == 0){
        # we print it to make it clear
        print(paste("no patient with",input_char_to_evaluate[i],"in site",site_list[j],sep=" "))
        
        # we save all 0s vector to our output
        allsite_output = rbind(allsite_output,c(0,0,0,0))
      } else {
        # if there are patients, we add the actual value 
        allsite_output = rbind(allsite_output,input_list_df[[j]][index,-1])
      }
    }
    allsite_output$site = site_list
    data_all_variables[[i]] = allsite_output
  }
  names(data_all_variables) = input_char_to_evaluate
  
  return(data_all_variables)
}

# exact method function 
exact_method <- function(input_char_to_evaluate, sample_size_df, formated_meta_analysis_list, outputPath ){
  
  # create a vector to save the results
  res.exact.delta = vector("list",length(input_char_to_evaluate))
  
  # for each characteristic in the list we perform the meta.exact analysis
  for (i in 1:length(input_char_to_evaluate)){
    print(paste0("#####", i))
    
    #since we are comparing alpha and delta we get need a data frame with 4 cols
    # the alpha and delta counts per site and the total alpha and delta per site
    dat.exact <- formated_meta_analysis_list[[i]] %>%
      left_join( sample_size_df) %>%
      select(  n.Alpha, n.Delta, alpha, delta )
    
    res.exact = meta.exact(dat.exact, type="risk difference")
    res.exact.delta[[i]] = res.exact
  }
  
  res.exact.omicron = vector("list",length(input_char_to_evaluate))
  for (i in 1:length(input_char_to_evaluate)){
    print(paste0("#####", i))
    dat.exact <- formated_meta_analysis_list[[i]] %>%
      left_join( sample_size_df) %>%
      select(  n.Alpha, n.Omicron, alpha, omicron )
    res.exact = meta.exact(dat.exact, type="risk difference")
    res.exact.omicron[[i]] = res.exact
  }

  save(res.exact.delta,res.exact.omicron,file=outputPath)
  return( list( res.exact.delta,res.exact.omicron ))
}

### function to extract the results for approach II: exact CI ###
exact_method_format_results <- function( exact_results, p_value, input_char_to_evaluate, filter_p_val){

  output.exact = NULL
  for (i in 1:length(exact_results)){
    output.exact = rbind(output.exact,exact_results[[i]]$ci.fixed[,2])
  }
  output.exact = data.frame(output.exact)
  output.exact$outcome = input_char_to_evaluate
  
  if( filter_p_val == TRUE ){
    output <- output.exact %>% filter( p < p_value)
    print( output )
    return( output )
  }else{
    return( output.exact )
  }
}

### function to extract the results with method III: exact site CI, standard meta
exact_site_standardMeta <- function( exact_results, p_value, input_char_to_evaluate, filter_p_val){
  output.hybrid = NULL
  transform = FALSE
  
  for (i in 1:length(exact_results)){
    #print(i)
    yi = NULL
    yi.alt = NULL
    vi = NULL
    CIs = exact_results[[i]]$study.ci
    if (transform){
      for (j in 1:nrow(CIs)){
        yi = c(yi, (ft(CIs[j,2])+ft(CIs[j,3]))/2)
        yi.alt = c(yi.alt, ft(CIs[j,1]))
        vi = c(vi,(ft(CIs[j,3]) - ft(CIs[j,2]))/2/qnorm(0.975))
      }
      vi = vi^2
      tryCatch({mod <- rma.uni(yi, vi, method = "EE")
      }, error = function(e) mod <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      output.hybrid = rbind(output.hybrid, c(ft.inverse(mod$beta),ft.inverse(mod$ci.lb), ft.inverse(mod$ci.ub), mod$pval))
    } else {
      for (j in 1:nrow(CIs)){
        yi = c(yi, (CIs[j,2]+CIs[j,3])/2)
        yi.alt = c(yi.alt, CIs[j,1])
        vi = c(vi,(CIs[j,3] - CIs[j,2])/2/qnorm(0.975))
      }
      vi = vi^2
      tryCatch({mod <- rma.uni(yi, vi, method = "EE")
      }, error = function(e) mod <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      output.hybrid = rbind(output.hybrid, c(mod$beta,mod$ci.lb, mod$ci.ub, mod$pval))
    }
  }
  output.hybrid = data.frame(output.hybrid)
  colnames(output.hybrid) = c("est","ci.lb","ci.ub","pval")
  output.hybrid$var = input_char_to_evaluate
  
  if( filter_p_val == TRUE ){
    output <- output.hybrid %>% filter( pval < p_value)
    print( output )
    return( output )
  }else{
    return( output.hybrid )
  }
  
}

## function to read and format the lab data
continuous_outcome_data <- function(site_df, rdata_fileName, files_path){
  #create the list of sites
  site_list <- site_df$site
  lab_bysite = vector("list",length(site_list))
  
  for (i in 1:length(site_list)){
    
    # load the file
    filename = paste(files_path, site_list[i], "/replace_earlier/",site_list[i],"_", rdata_fileName,sep="")
    load(filename)
    
    if( rdata_fileName == "table2AtAdmission.RData"){
      lab_data <- table2_admission
    }else if( rdata_fileName == "table2DuringAdmission.RData"){
      lab_data <- table2_during
    }
    
    # remove any NAs and select the columns of interest (mean and sd values)
    mean_sd = lab_data %>% filter( ! is.na( median_value )) %>%
      group_by( variableName ) %>%
      select( variant_misc, variableName, mean_value, sd_value, n_patients )
    
    # pivot wider
    mean_sd_reshape = reshape(as.data.frame(mean_sd),direction="wide",idvar="variableName",timevar="variant_misc")
    lab_bysite[[i]] = mean_sd_reshape
  }
  names(lab_bysite) = site_list
  
  return( lab_bysite )
}


continous_var_test <- function( lab_list, lab_data, p_value ){
  
  # create a vector with the sites
  site_list <- names( lab_data )
  
  # create an empty df to save the results
  labs_results <- as.data.frame( matrix( ncol = 3, nrow = length(lab_list)))
  colnames( labs_results) <- c("lab", "alpha_delta_pval", "alpha_omicron_pval")
  
  # create a list to save stat results to plot forest plot
  outputs_to_plot <- list()
  
  for( j in 1:length(lab_list)){
    
    this_lab = lab_list[j]
    print(this_lab)
    
    # create empty vectors 
    diff_alpha_delta = rep(0,length(site_list))
    var.diff_alpha_delta = rep(0,length(site_list))
    
    diff_alpha_omicron = rep(0,length(site_list))
    var.diff_alpha_omicron = rep(0,length(site_list))
    
    thres = 0
    sites.exclude = NULL
    for (i in 1:length(site_list)){
      
      # determine if the lab is collected in the site
      index = which(lab_data[[i]]$variableName == this_lab)
      
      # if not collected, we will exclude the site (adding it to the sites.exclude)
      if( !any(index) ){
        sites.exclude = c(sites.exclude,i)
      }else{
        #determine which is the minimum number of patients with this lab for this site
        min.size = min(lab_data[[i]]$n_patients.Alpha[index],lab_data[[i]]$n_patients.Delta[index],lab_data[[i]]$n_patients.Omicron[index])
        # if the min.size is NA or less than the threshold we add to site to excluded sites
        if (min.size < thres | is.na(min.size)){
          sites.exclude = c(sites.exclude,i)
        }
        
        # estimate the difference on the means in delta and alpha and omicron and alpha
        # and save the value in the empty vector created 
        diff_alpha_delta[i] = lab_data[[i]]$mean_value.Delta[index] - lab_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_delta[i] = lab_data[[i]]$sd_value.Delta[index]^2/sample_size[i,2] + lab_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
        diff_alpha_omicron[i] = lab_data[[i]]$mean_value.Omicron[index] - lab_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_omicron[i] = lab_data[[i]]$sd_value.Omicron[index]^2/sample_size[i,3] + lab_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
      }
    }
    
    set.seed(123)
    
    # run the meta-analysis, using the rma function
    if( is.null(sites.exclude )){
      stats_output_alpha_delta <- rma(diff_alpha_delta, var.diff_alpha_delta, method = "EE")
      stats_output_alpha_omicron <- rma(diff_alpha_omicron, var.diff_alpha_omicron, method = "EE")
    }else{
      stats_output_alpha_delta  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude], method = "EE")
      stats_output_alpha_omicron  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude], method = "EE")
    }
    
    labs_results$lab[j] <- this_lab
    labs_results$alpha_delta_pval[j] <- stats_output_alpha_delta$pval
    labs_results$alpha_omicron_pval[j] <- stats_output_alpha_omicron$pval
    
    if( stats_output_alpha_delta$pval < p_value){
      outputs_to_plot[[this_lab]] <- stats_output_alpha_delta
    }
    if( stats_output_alpha_omicron$pval < p_value){
      outputs_to_plot[[this_lab]] <- stats_output_alpha_omicron
    }
  }
  return( list( labs_results, outputs_to_plot) )
  
}



###### Run the code 

## use the binary_outcome data function to generate a list with
# 1. all the clinical characteristics per site
clinical_char_allsite <- binary_outcome_data( site_df = sample_size, 
                     rdata_fileName =  "table1Categorical.RData", 
                     files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/"  )

# 2. all the outcomes per site
outcome_allsite <- binary_outcome_data( site_df = sample_size, 
                                  rdata_fileName =  "table3.RData", 
                                  files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/"  )

# Load the different variables to evaluate (outcomes, categories and all clinical variables)
load( "/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/listOfVariablesForMetaAnalysis.RData")


# For the category level, format the data for the meta-analysis
data_allcategories <- format_data_to_meta_analysis( input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories, 
                                                    input_list_df = clinical_char_allsite)

data_alldiag <- format_data_to_meta_analysis( input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all, 
                                                    input_list_df = clinical_char_allsite)

data_alloutcomes <- format_data_to_meta_analysis( input_char_to_evaluate = list_to_evaluate$Outcomes_all, 
                                              input_list_df = outcome_allsite)


# run the exact method (the most conservative one)
exact_method_allcategories <- exact_method(
  input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories,
  formated_meta_analysis_list = data_allcategories,
  outputPath = "/Users/alba/Desktop/exactMethod_categories.RData",
  sample_size_df = sample_size
)


exact_method_alloutcomes <- exact_method(
  input_char_to_evaluate = list_to_evaluate$Outcomes_all,
  formated_meta_analysis_list = data_alloutcomes,
  outputPath = "/Users/alba/Desktop/exactMethod_outcomes.RData",
  sample_size_df = sample_size
)

### this one takes a while
exact_method_alldiag <- exact_method(
  input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all,
  formated_meta_analysis_list = data_alldiag,
  outputPath = "/Users/alba/Desktop/replace_earlier/exactMethod_allDiagnosis.RData",
  sample_size_df = sample_size
)




# get the statistically significant results 
load("/Users/alba/Desktop/replace_earlier/exactMethod_outcomes.RData")
exact_method_alloutcomes <- list( res.exact.delta,res.exact.omicron )

stat_significant_outcomes <- list()
stat_significant_outcomes[["alphavsdelta"]] <-exact_method_format_results( exact_results = exact_method_alloutcomes[[1]], 
                                                             p_value = 0.05, 
                                                             input_char_to_evaluate = list_to_evaluate$Outcomes_all, 
                                                             filter_p_val = FALSE)

stat_significant_outcomes[["alphavsomicron"]] <-exact_method_format_results( exact_results = exact_method_alloutcomes[[2]], 
                                                                        p_value = 0.05, 
                                                                        input_char_to_evaluate = list_to_evaluate$Outcomes_all, 
                                                                        filter_p_val = FALSE)
### based on the most restrictive statistical method we find stat significant
### results on alpha vs. omicron for anti-coagulation therapy

# for the categories
load("/Users/alba/Desktop/replace_earlier/exactMethod_categories.RData")
exact_method_allcategories <- list( res.exact.delta,res.exact.omicron )

stat_significant_categories <- list()
stat_significant_categories[["alphavsdelta"]] <-exact_method_format_results( exact_results = exact_method_allcategories[[1]], 
                                                                           p_value = 0.05, 
                                                                           input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories, 
                                                                           filter_p_val = TRUE)

stat_significant_categories[["alphavsomicron"]] <-exact_method_format_results( exact_results = exact_method_allcategories[[2]], 
                                                                             p_value = 0.05, 
                                                                             input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories, 
                                                                             filter_p_val = TRUE)
### no statistical results for the categories with the restrictive method

# for all diagnosis
load("/Users/alba/Desktop/replace_earlier/exactMethod_allDiagnosis.RData")
exact_method_alldiag <- list( res.exact.delta,res.exact.omicron )

stat_significant_categories <- list()
stat_significant_categories[["alphavsdelta"]] <-exact_method_format_results( exact_results = exact_method_alldiag[[1]], 
                                                                             p_value = 0.05, 
                                                                             input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all, 
                                                                             filter_p_val = TRUE)

stat_significant_categories[["alphavsomicron"]] <-exact_method_format_results( exact_results = exact_method_alldiag[[2]], 
                                                                               p_value = 0.05, 
                                                                               input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all, 
                                                                               filter_p_val = TRUE)



### run method 3, less restrictive
stat_significant_categories_standardMeta <- list()
stat_significant_categories_standardMeta[["alphavsdelta"]] <- exact_site_standardMeta( exact_results = exact_method_allcategories[[1]], 
                         p_value = 0.05, 
                         input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories, 
                         filter_p_val = TRUE)

# in alpha vs. delta we find neurological symptoms and shock/irs with sig p-value

stat_significant_categories_standardMeta[["alphavsomicron"]] <- exact_site_standardMeta( exact_results = exact_method_allcategories[[2]], 
                         p_value = 0.05, 
                         input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_categories, 
                         filter_p_val = TRUE)

# no stat. significant results in alpha vs. omicron

### run method 3, for outcomes
stat_significant_outcomes_standardMeta <- list()
stat_significant_outcomes_standardMeta[["alphavsdelta"]] <- exact_site_standardMeta( exact_results = exact_method_alloutcomes[[1]], 
                                                                                       p_value = 0.05, 
                                                                                       input_char_to_evaluate = list_to_evaluate$Outcomes_all, 
                                                                                       filter_p_val = TRUE)

# composite adverse cardiovascular outcome is significant 

stat_significant_outcomes_standardMeta[["alphavsomicron"]] <- exact_site_standardMeta( exact_results = exact_method_alloutcomes[[2]], 
                                                                                         p_value = 0.05, 
                                                                                         input_char_to_evaluate = list_to_evaluate$Outcomes_all, 
                                                                                         filter_p_val = FALSE)

# with this method anticoagulation therapy is not stat significant

### run method 3, for allDiagnosis
stat_significant_allDiag_standardMeta <- list()
stat_significant_allDiag_standardMeta[["alphavsdelta"]] <- exact_site_standardMeta( exact_results = exact_method_alldiag[[1]], 
                                                                                     p_value = 0.05, 
                                                                                     input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all, 
                                                                                     filter_p_val = TRUE)

# history of covid, NEUROLOGY SYMPTOMS and SHOCK/IRS

stat_significant_allDiag_standardMeta[["alphavsomicron"]] <- exact_site_standardMeta( exact_results = exact_method_alldiag[[2]], 
                                                                                       p_value = 0.05, 
                                                                                       input_char_to_evaluate = list_to_evaluate$ClinicalCharacteristic_all, 
                                                                                       filter_p_val = TRUE)




##################################################################
## Labs; Continuous outputs: meta analysis of two-sample t-test ##
##################################################################

labs_at_admission <- continuous_outcome_data( site_df = sample_size, 
                                              rdata_fileName =  "table2AtAdmission.RData", 
                                              files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")

labs_during_admission <- continuous_outcome_data( site_df = sample_size, 
                                              rdata_fileName =  "table2DuringAdmission.RData", 
                                              files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")


# extract all the lab names 
lab_names_list <- labs_at_admission[[1]]$variableName

# lab at admission meta-analysis results
labs_at_admission_metaAnalysis_output <- continous_var_test( lab_list = lab_names_list,
                            lab_data = labs_at_admission, 
                            p_value = 0.05) 

labs_at_admission_metaAnalysis_output_df <- labs_at_admission_metaAnalysis_output[[1]]
labs_at_admission_outputs_to_plot <- labs_at_admission_metaAnalysis_output[[2]]

names(labs_at_admission_outputs_to_plot)
forest( labs_at_admission_outputs_to_plot[[1]])
forest( labs_at_admission_outputs_to_plot[[2]])
# troponin normal sensitivity and lymphocyte stat significant

# lab during admission meta-analysis results
labs_during_admission_metaAnalysis_output <- continous_var_test( lab_list = lab_names_list,
                                                             lab_data = labs_during_admission, 
                                                             p_value = 0.05) 

labs_during_admission_metaAnalysis_output_df <- labs_during_admission_metaAnalysis_output[[1]]
labs_during_admission_outputs_to_plot <- labs_during_admission_metaAnalysis_output[[2]]

names(labs_during_admission_outputs_to_plot)
forest( labs_during_admission_outputs_to_plot[[1]])
forest( labs_during_admission_outputs_to_plot[[2]])
