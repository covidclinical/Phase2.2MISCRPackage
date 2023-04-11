### create a new RData file with the age/hospitalization reformatted 
sample_size <- read.delim("/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/sample_size.txt")
site_list <- sample_size$site
files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/" 

for (i in 1:length(site_list)){

    filename = paste(files_path, site_list[i], "/replace_earlier/",site_list[i],"_table1Continuous.RData",sep="")
    load(filename)
  
    age <- table1_continuous %>%
      mutate( variable_name = "age") %>%
      select( variant_misc, 
              variable_name, 
              median_value = age_median, 
              iqr_value = age_iqr, 
              mean_value = age_mean, 
              sd_value = age_sd, 
              min_value = age_min, 
              max_value = age_max )
    
    hospitalization <- table1_continuous %>%
      mutate( variable_name = "length_hospitalization" ) %>%
      select( variant_misc, variable_name, 
              median_value = length_hospitalization_median, 
              iqr_value = length_hospitalization_iqr, 
              mean_value = length_hospitalization_mean, 
              sd_value = length_hospitalization_sd, 
              min_value = length_hospitalization_min, 
              max_value = length_hospitalization_max )
    
    new_table1_continuous <- rbind( age, hospitalization)
    new_file = paste(files_path, site_list[i], "/replace_earlier/",site_list[i],"_reformatted_table1Continuous.RData",sep="")
    save( new_table1_continuous, file = new_file)
  
}
###
continuous_data <- function(site_df, rdata_fileName, files_path){
  #create the list of sites
  site_list <- site_df$site
  var_bysite = vector("list",length(site_list))
  
  for (i in 1:length(site_list)){
    
    # load the file
    print(i)
    filename = paste(files_path, site_list[i], "/replace_earlier/",site_list[i],"_", rdata_fileName,sep="")
    load(filename)
    
   
    # remove any NAs and select the columns of interest (mean and sd values)
    mean_sd = new_table1_continuous %>% filter( ! is.na( median_value )) %>%
      group_by( variable_name ) %>%
      select( variant_misc, variableName = variable_name, mean_value, sd_value )
    
    # pivot wider
    mean_sd_reshape = reshape(as.data.frame(mean_sd),direction="wide",idvar="variableName",timevar="variant_misc")
    var_bysite[[i]] = mean_sd_reshape
  }
  names(var_bysite) = site_list
  
  return( var_bysite )
}


continous_var_test <- function( var_list, var_data, p_value ){
  
  # create a vector with the sites
  site_list <- names( var_data )
  
  # create an empty df to save the results
  
  var_results <- as.data.frame( matrix( ncol = 7, nrow = length(var_list)))
  colnames( var_results) <- c("var", "alpha_delta_pval", "alpha_delta_est","alpha_delta_CI", 
                               "alpha_omicron_pval", "alpha_omicron_est","alpha_omicron_CI")

  
  # create a list to save stat results to plot forest plot
  outputs_to_plot <- list()
  
  for( j in 1:length(var_list)){
    print(i)
    this_var = var_list[j]
    print(this_var)
    
    # create empty vectors 
    diff_alpha_delta = rep(0,length(site_list))
    var.diff_alpha_delta = rep(0,length(site_list))
    
    diff_alpha_omicron = rep(0,length(site_list))
    var.diff_alpha_omicron = rep(0,length(site_list))
    
    thres = 0
    sites.exclude = NULL
    for (i in 1:length(site_list)){
      
      # determine if the lab is collected in the site
      index = which(var_data[[i]]$variableName == this_var)
      
      # if not collected, we will exclude the site (adding it to the sites.exclude)
      if( !any(index) ){
        sites.exclude = c(sites.exclude,i)
      }else{
        #determine which is the minimum number of patients with this lab for this site
        min.size = min(var_data[[i]]$n_patients.Alpha[index],var_data[[i]]$n_patients.Delta[index],var_data[[i]]$n_patients.Omicron[index])
        # if the min.size is NA or less than the threshold we add to site to excluded sites
        if (min.size < thres | is.na(min.size)){
          sites.exclude = c(sites.exclude,i)
        }
        
        # estimate the difference on the means in delta and alpha and omicron and alpha
        # and save the value in the empty vector created 
        diff_alpha_delta[i] = var_data[[i]]$mean_value.Delta[index] - var_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_delta[i] = var_data[[i]]$sd_value.Delta[index]^2/sample_size[i,2] + var_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
        diff_alpha_omicron[i] = var_data[[i]]$mean_value.Omicron[index] - var_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_omicron[i] = var_data[[i]]$sd_value.Omicron[index]^2/sample_size[i,3] + var_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
      }
    }
    
    set.seed(123)
    # run the meta-analysis, using the rma function
    if( is.null(sites.exclude )){
      
      tryCatch({stats_output_alpha_delta <- rma(diff_alpha_delta, var.diff_alpha_delta, method = "EE")
      }, error = function(e) stats_output_alpha_delta <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      tryCatch({stats_output_alpha_omicron <- rma(diff_alpha_omicron, var.diff_alpha_omicron, method = "EE")
      }, error = function(e) stats_output_alpha_omicron <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      
    }else{
      
      tryCatch({stats_output_alpha_delta  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude], method = "EE")
      }, error = function(e) stats_output_alpha_delta <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      tryCatch({stats_output_alpha_omicron  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude], method = "EE")
      }, error = function(e) stats_output_alpha_omicron <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      
    }
    
    var_results$var[j] <- this_var

    var_results$alpha_delta_pval[j] <- round(stats_output_alpha_delta$pval,3)
    var_results$alpha_delta_est[j] <- round(stats_output_alpha_delta$b, 3)
    var_results$alpha_delta_CI[j] <- paste0("[", round(stats_output_alpha_delta$ci.lb, 3), ",", 
                                             round(stats_output_alpha_delta$ci.ub, 3), "]")
    
    var_results$alpha_omicron_pval[j] <- round(stats_output_alpha_omicron$pval,3)
    var_results$alpha_omicron_est[j] <- round(stats_output_alpha_omicron$b, 3)
    var_results$alpha_omicron_CI[j] <- paste0("[", round(stats_output_alpha_omicron$ci.lb, 3), ",", 
                                               round(stats_output_alpha_omicron$ci.ub, 3), "]")
    
    
    
    
    if( !is.na(stats_output_alpha_delta$pval)){
      if (stats_output_alpha_delta$pval < p_value){
        var<- paste0(this_var, "_alphaDelta")
        outputs_to_plot[[var]] <- stats_output_alpha_delta
      }}
    if( !is.na(stats_output_alpha_omicron$pval)){
      if(stats_output_alpha_omicron$pval < p_value){
        var<- paste0(this_var, "_alphaOmicron")
        outputs_to_plot[[var]] <- stats_output_alpha_omicron
      }}    
    
  }
  return( list( var_results, outputs_to_plot) )
  
}


t1_continuous <- continuous_data( site_df = sample_size, 
                                              rdata_fileName =  "reformatted_table1Continuous.RData", 
                                              files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")

# extract all the lab names 
var <- c("age", "length_hospitalization")
# meta-analysis results
outputs <- continous_var_test( var_list = var,
                    var_data = t1_continuous, 
                    p_value = 0.05) 


outputs_df <- outputs[[1]]
outputs_to_plot <- outputs[[2]]

names(outputs_to_plot)
forest( outputs_to_plot[[1]])
forest( outputs_to_plot[[2]])

outputs_df %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "/Users/alba/Desktop/age_lengthHosp_meta_analysis.docx")



#########################################################
#########################################################
###### Re-run it for the original cut-off dates #########
#########################################################
#########################################################
rm(list=ls())


### create a new RData file with the age/hospitalization reformatted 
sample_size <- read.delim("/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/sample_size_original.txt")
site_list <- sample_size$site
files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/" 

for (i in 1:length(site_list)){
  
  filename = paste(files_path, site_list[i], "/original/",site_list[i],"_table1Continuous.RData",sep="")
  load(filename)
  
  age <- table1_continuous %>%
    mutate( variable_name = "age") %>%
    select( variant_misc, 
            variable_name, 
            median_value = age_median, 
            iqr_value = age_iqr, 
            mean_value = age_mean, 
            sd_value = age_sd, 
            min_value = age_min, 
            max_value = age_max )
  
  hospitalization <- table1_continuous %>%
    mutate( variable_name = "length_hospitalization" ) %>%
    select( variant_misc, variable_name, 
            median_value = length_hospitalization_median, 
            iqr_value = length_hospitalization_iqr, 
            mean_value = length_hospitalization_mean, 
            sd_value = length_hospitalization_sd, 
            min_value = length_hospitalization_min, 
            max_value = length_hospitalization_max )
  
  new_table1_continuous <- rbind( age, hospitalization)
  new_file = paste(files_path, site_list[i], "/original/",site_list[i],"_reformatted_table1Continuous.RData",sep="")
  save( new_table1_continuous, file = new_file)
  
}
###
continuous_data <- function(site_df, rdata_fileName, files_path){
  #create the list of sites
  site_list <- site_df$site
  var_bysite = vector("list",length(site_list))
  
  for (i in 1:length(site_list)){
    
    # load the file
    print(i)
    filename = paste(files_path, site_list[i], "/original/",site_list[i],"_", rdata_fileName,sep="")
    load(filename)
    
    
    # remove any NAs and select the columns of interest (mean and sd values)
    mean_sd = new_table1_continuous %>% filter( ! is.na( median_value )) %>%
      group_by( variable_name ) %>%
      select( variant_misc, variableName = variable_name, mean_value, sd_value )
    
    # pivot wider
    mean_sd_reshape = reshape(as.data.frame(mean_sd),direction="wide",idvar="variableName",timevar="variant_misc")
    var_bysite[[i]] = mean_sd_reshape
  }
  names(var_bysite) = site_list
  
  return( var_bysite )
}


continous_var_test <- function( var_list, var_data, p_value ){
  
  # create a vector with the sites
  site_list <- names( var_data )
  
  # create an empty df to save the results
  var_results <- as.data.frame( matrix( ncol = 7, nrow = length(var_list)))
  colnames( var_results) <- c("var", "alpha_delta_pval", "alpha_delta_est","alpha_delta_CI", 
                              "alpha_omicron_pval", "alpha_omicron_est","alpha_omicron_CI")
  
  # create a list to save stat results to plot forest plot
  outputs_to_plot <- list()
  
  for( j in 1:length(var_list)){
    print(i)
    this_var = var_list[j]
    print(this_var)
    
    # create empty vectors 
    diff_alpha_delta = rep(0,length(site_list))
    var.diff_alpha_delta = rep(0,length(site_list))
    
    diff_alpha_omicron = rep(0,length(site_list))
    var.diff_alpha_omicron = rep(0,length(site_list))
    
    thres = 0
    sites.exclude = NULL
    for (i in 1:length(site_list)){
      
      # determine if the lab is collected in the site
      index = which(var_data[[i]]$variableName == this_var)
      
      # if not collected, we will exclude the site (adding it to the sites.exclude)
      if( !any(index) ){
        sites.exclude = c(sites.exclude,i)
      }else{
        #determine which is the minimum number of patients with this lab for this site
        min.size = min(var_data[[i]]$n_patients.Alpha[index],var_data[[i]]$n_patients.Delta[index],var_data[[i]]$n_patients.Omicron[index])
        # if the min.size is NA or less than the threshold we add to site to excluded sites
        if (min.size < thres | is.na(min.size)){
          sites.exclude = c(sites.exclude,i)
        }
        
        # estimate the difference on the means in delta and alpha and omicron and alpha
        # and save the value in the empty vector created 
        diff_alpha_delta[i] = var_data[[i]]$mean_value.Delta[index] - var_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_delta[i] = var_data[[i]]$sd_value.Delta[index]^2/sample_size[i,2] + var_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
        diff_alpha_omicron[i] = var_data[[i]]$mean_value.Omicron[index] - var_data[[i]]$mean_value.Alpha[index]
        var.diff_alpha_omicron[i] = var_data[[i]]$sd_value.Omicron[index]^2/sample_size[i,3] + var_data[[i]]$sd_value.Alpha[index]^2/sample_size[i,1]
        
      }
    }
    
    set.seed(123)
    # run the meta-analysis, using the rma function
    if( is.null(sites.exclude )){
      
      tryCatch({stats_output_alpha_delta <- rma(diff_alpha_delta, var.diff_alpha_delta, method = "EE")
      }, error = function(e) stats_output_alpha_delta <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      tryCatch({stats_output_alpha_omicron <- rma(diff_alpha_omicron, var.diff_alpha_omicron, method = "EE")
      }, error = function(e) stats_output_alpha_omicron <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      
    }else{
      
      tryCatch({stats_output_alpha_delta  <- rma(diff_alpha_delta[-sites.exclude], var.diff_alpha_delta[-sites.exclude], method = "EE")
      }, error = function(e) stats_output_alpha_delta <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      tryCatch({stats_output_alpha_omicron  <- rma(diff_alpha_omicron[-sites.exclude], var.diff_alpha_omicron[-sites.exclude], method = "EE")
      }, error = function(e) stats_output_alpha_omicron <<- list('beta' = NA, 'ci.lib' = NA, 'ci.ub' = NA, 'pval' = NA))
      
      
    }
    
    var_results$var[j] <- this_var
    var_results$alpha_delta_pval[j] <- round(stats_output_alpha_delta$pval,3)
    var_results$alpha_delta_est[j] <- round(stats_output_alpha_delta$b, 3)
    var_results$alpha_delta_CI[j] <- paste0("[", round(stats_output_alpha_delta$ci.lb, 3), ",", 
                                            round(stats_output_alpha_delta$ci.ub, 3), "]")
    
    var_results$alpha_omicron_pval[j] <- round(stats_output_alpha_omicron$pval,3)
    var_results$alpha_omicron_est[j] <- round(stats_output_alpha_omicron$b, 3)
    var_results$alpha_omicron_CI[j] <- paste0("[", round(stats_output_alpha_omicron$ci.lb, 3), ",", 
                                              round(stats_output_alpha_omicron$ci.ub, 3), "]")
    
    
    
    
    if( !is.na(stats_output_alpha_delta$pval)){
      if (stats_output_alpha_delta$pval < p_value){
        var<- paste0(this_var, "_alphaDelta")
        outputs_to_plot[[var]] <- stats_output_alpha_delta
      }}
    if( !is.na(stats_output_alpha_omicron$pval)){
      if(stats_output_alpha_omicron$pval < p_value){
        var<- paste0(this_var, "_alphaOmicron")
        outputs_to_plot[[var]] <- stats_output_alpha_omicron
      }}    
    
  }
  return( list( var_results, outputs_to_plot) )
  
}


t1_continuous <- continuous_data( site_df = sample_size, 
                                  rdata_fileName =  "reformatted_table1Continuous.RData", 
                                  files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")

# extract all the lab names 
var <- c("age", "length_hospitalization")
# meta-analysis results
outputs <- continous_var_test( var_list = var,
                               var_data = t1_continuous, 
                               p_value = 0.05) 


outputs_df <- outputs[[1]]
outputs_to_plot <- outputs[[2]]

names(outputs_to_plot)
forest( outputs_to_plot[[1]])
forest( outputs_to_plot[[2]])

outputs_df %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "/Users/alba/Desktop/age_lengthHosp_meta_analysis_original.docx")

