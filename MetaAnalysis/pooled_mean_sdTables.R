rm(list=ls())
####### packages
library(dplyr)
library(tidyr)

####### set up directory 
setwd("/Users/alba/Desktop/finalTablesTestWithP-val/4CE_MISC_outputs/")
sites <- list.files("./")

###############################
########## Functions ##########
###############################

### calculate pooled mean from site specific means and site sample size
### can allow site sample size =1, in which case site mean = that single observation
### if for privacy concern, when site sample size = 1, that single obs 
###    is not reported, just set site mean for that site to NA
pooled_mean = function(site_mean, site_size){
  exclude = which(is.na(site_mean) == TRUE)
  if (length(exclude)>0){
    means = site_mean[-exclude]
    sizes = site_size[-exclude]
    print(paste0("sites",exclude,"excluded"))
  } else {
    means = site_mean
    sizes = site_size
  }
  pool_mean = sum(means*sizes)/sum(sizes)
  return(pool_mean)
}

### by default, when calculating variance, R uses sample size - 1 in the
###.  denomimator
### which means if some site has sample size one, site var/sd is na
### in which case just set site_sd for that site to NA, site_size for that to 1

### the function computes variance, for sd just take square root

pooled_var = function(site_mean,site_sd,site_size){
  exclude = which(is.na(site_mean) == TRUE)
  if (length(exclude)>0){
    means = site_mean[-exclude]
    sds = site_sd[-exclude]
    sizes = site_size[-exclude]
    print(paste0("sites",exclude,"excluded"))
  } else {
    means = site_mean
    sds = site_sd
    sizes = site_size
  }
  
  
  fractions = sizes/sum(sizes)
  var_cond_mean = sum((means - sum(means*fractions))^2*fractions)
  # if size variance is computed with sample size in the denominator
  # change sizes-1 to sizes
  vars = ifelse(is.na(sds)|sizes==1,0,sds^2*(sizes-1)/sizes)
  mean_cond_var = sum(vars*fractions)
  return((var_cond_mean + mean_cond_var)*sum(sizes)/(sum(sizes)-1))
}


####################################################
########## Age and length hospitalization ##########
####################################################
sample_size <- read.delim("/Users/alba/Desktop/Phase2.2MISCRPackage/MetaAnalysis/sample_size.txt")


continuous_data <- function(site_df, rdata_fileName, files_path){
  
  for (i in 1:nrow(site_df)){
    
    # load the file
    print(i)
    filename = paste(files_path, site_df$site[i], "/replace_earlier/",site_df$site[i],"_", rdata_fileName,sep="")
    load(filename)
    
    if( i == 1){
      aggValues <- new_table1_continuous %>%
        mutate( site = site_df$site[i]) %>%
        select(variant_misc, variable_name, mean_value, sd_value, site )
    }else{
      int_aggValues <-new_table1_continuous %>%
        mutate( site = site_df$site[i]) %>%
        select(variant_misc, variable_name, mean_value, sd_value, site)
      aggValues <- rbind( aggValues, int_aggValues)
    }
  }
  return( aggValues )
}

t1_continuous <- continuous_data( site_df = sample_size, 
                                  rdata_fileName =  "reformatted_table1Continuous.RData", 
                                  files_path     =  "/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")

samples <- sample_size %>%
  select( -obfuscation) %>%
  pivot_longer( cols = c("alpha", "delta", "omicron", "total"), 
                names_to = "variant_misc", 
                values_to = "n")
t1_continuous$variant_misc <- tolower( t1_continuous$variant_misc)

output <- merge( t1_continuous, samples, by = c("site", "variant_misc"))

age_hospitalization <- output %>%
  group_by(variant_misc, variable_name) %>%
  summarise(pooled_means = pooled_mean(mean_value, n),
            pooled_vars = pooled_var( site_mean = mean_value, site_sd= sd_value, site_size= n),
            pooled_sd = sqrt( pooled_vars ),
            total = sum(n))

age_hospitalization_formatted <- age_hospitalization %>%
  mutate( value = paste0( round( pooled_means,3), " (", round(pooled_sd,3), ")")) %>%
  select( variant_misc, variable_name, value ) %>%
  pivot_wider( names_from = variant_misc, 
               values_from =  value ) %>%
  select( variable_name, total, alpha, delta, omicron )

age_hospitalization_formatted %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "/Users/alba/Desktop/age_lengthHosp_summary.docx")


########################################
########## Laboratory values ##########
#######################################

##### combine continuous data from sites
combineContinuousDataFromSites <- function( fileList, time_point){
  
  for( i in 1:length( fileList )){
    
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    print(site_id)
    
    #load the file
    load( paste0("./", site_id, "/replace_earlier/", fileList[i]))
    
    if( time_point =="admission"){
      inputData <- table2_admission
      rm(table2_admission)
    }else if( time_point == "during"){
      inputData <- table2_during
      rm( table2_during)
    }
    
    #add column with the site and with the time, and select the columns of interest
    table_output <- inputData %>%
      mutate( site = site_id,
              time = time_point) %>%
      select( variant_misc, variableName, mean_value, sd_value, n_patients, site, time ) 
    
    
    #remove the original df
    rm(inputData)
    
    if( i ==1 ){
      output <- table_output
    }else{
      output <- rbind( table_output, output)
    }
  }
  return( output )
  
}



#####################
###### TABLE 2 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    labFiles <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table2")
  }else{
    labFilesInt <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table2")
    labFiles <- c( labFiles, labFilesInt)
  }
}


### split the files
atAdmission <- labFiles[ grepl( "AtAdmission.RData", labFiles)] 
duringAdmission <- labFiles[ grepl( "DuringAdmission.RData", labFiles)] 

labsAllAdm <- combineContinuousDataFromSites( fileList = atAdmission, time_point = "admission")
labsAllDuring <- combineContinuousDataFromSites( fileList = duringAdmission, time_point = "during")


### final table 2 at admission
labsAllAdm_results <- labsAllAdm %>%
  group_by(variant_misc, variableName, time) %>%
  summarise(pooled_means = pooled_mean(mean_value, n_patients),
            pooled_vars = pooled_var( mean_value, sd_value, n_patients),
            pooled_sd = sqrt( pooled_vars ), 
            total = sum(n_patients))

labsAllDuring_results <- labsAllDuring %>%
  group_by(variant_misc, variableName, time) %>%
  summarise(pooled_means = pooled_mean(mean_value, n_patients),
            pooled_vars = pooled_var( mean_value, sd_value, n_patients),
            pooled_sd = sqrt( pooled_vars ), 
            total = sum(n_patients))


labsAll_results <- rbind( labsAllAdm_results, labsAllDuring_results)

# transform to follow the format of the table 2
labsAll_results_formatted <- labsAll_results %>%
  mutate( value = paste0( round( pooled_means,3), " (", round(pooled_sd,3), ")")) %>%
  select( variant_misc, variableName, time, value ) %>%
  pivot_wider( names_from = variant_misc, 
               values_from =  value ) %>%
  select( variableName, time, Total = total_n, Alpha, Delta, Omicron ) %>%
  arrange( variableName )

labsAll_results_formatted %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "/Users/alba/Desktop/labs_summary.docx")