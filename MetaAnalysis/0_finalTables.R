library(dplyr)
library(tidyr)
library(meta)
library(stringr)
rm(list=ls())
#setwd('/Users/smakwana/Desktop/finalTablesTest/4CE_MISC_outputs/')
setwd("/Users/alba/Desktop/finalTablesTest/4CE_MISC_outputs/")
sites <- list.files("./")

########################
###### FUNCTIONS ######
#######################
combineCategoricalDataFromSites <- function( fileList, table_num ){
  
  for( i in 1:length( fileList )){
    
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    
    #load the file
    load( paste0("./", site_id, "/replace_earlier/", fileList[i]))
    
    if( table_num == 1){
      inputData <- table1_categorical %>% 
        mutate( site = site_id )
      rm(table1_categorical)
      
      total_Values <- read.delim(paste0("./", site_id, "/replace_earlier/", site_id, "_table1.txt" ))
      
    }else if( table_num == 3){
      inputData <- table3 %>% 
        mutate( site = site_id )
      rm(table3)
      
      total_Values <- read.delim(paste0("./", site_id, "/replace_earlier/", site_id, "_table3.txt" ))
      
    }
    
    #extract total values from column names
    variantsN <- as.data.frame(matrix(ncol=2, nrow=4))
    colnames(variantsN) <- c("variant_misc", "total")
    variantsN$variant_misc <- c("Alpha", "Delta", "Omicron", "total")
    
    variantsN$total[1] <- as.numeric(sapply(strsplit( colnames(total_Values)[2], "[.]"), tail, 1))
    variantsN$total[2] <- as.numeric(sapply(strsplit( colnames(total_Values)[3], "[.]"), tail, 1))
    variantsN$total[3] <- as.numeric(sapply(strsplit( colnames(total_Values)[4], "[.]"), tail, 1))
    variantsN$total[4] <- as.numeric(sapply(strsplit( colnames(total_Values)[5], "[.]"), tail, 1))
    variantsN$site <- site_id
    
    # merge both
    output <- inputData %>%
      left_join( variantsN, by = c("variant_misc", "site"))
    
    # remove tables
    rm(total_Values)
    rm(variantsN)
    
    #remove the original df
    rm(inputData)
    
    if( i ==1 ){
      table_output <- output
    }else{
      table_output <- rbind( table_output, output)
    }
    
  }
  return( table_output )
}

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


# run categorical meta-analysis
categoricalToMetaAnalysis <- function(n, total, site){
  
  tryCatch({
    mtprop <- metaprop(event = n, n = total, studlab = site, method = 'GLMM', sm = 'PLOGIT', hakn = TRUE)
    random.est_values <- c(mtprop$TE.random,mtprop$lower.random,mtprop$upper.random)
    values_forTable <- meta:::backtransf(random.est_values, sm="PLOGIT")
    r <- paste0( round(values_forTable[1], 2), " [", round(values_forTable[2], 2), ", ",round( values_forTable[3], 2),  "] ;", sum(mtprop$event))
    return(r)
  }, error = function(e) return(NA))
  
}

## run continous meta-analysis 
continuousToMetaAnalysis <- function(n_patients, mean_value, sd_value, site){
  
  tryCatch({
    mtmean <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, sm="MRAW", method.ci= "z",
                        comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
    r <- paste0( round( mtmean$TE.random, 2), " [", round(mtmean$lower.random, 2), ", ",round( mtmean$upper.random, 2),  "]")
    return(r)
  }, error = function(e) return(NA))
  
}



#####################
###### TABLE 1 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    table1_files <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table1")
  }else{
    table1_filesInt <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table1")
    table1_files <- c( table1_files, table1_filesInt)
  }
}

continuousTable1 <- table1_files[ grepl( "Continuous.RData", table1_files)] 
categoricalTable1 <- table1_files[ grepl( "Categorical.RData", table1_files)] 


table1_categorical <- combineCategoricalDataFromSites( fileList = categoricalTable1, table_num = 1 )


table1_categorical_results <- table1_categorical %>% 
  group_by(categories, variant_misc) %>%
  summarise(res = categoricalToMetaAnalysis(n, total, site),
            total = sapply( strsplit(res, " ;"), '[', 2), 
            res = sapply( strsplit(res, " ;"), '[', 1)
            )

rm(table1_categorical)
colnames(table1_categorical_results) <- c("Variable", "variant", "value", "total")

# the continuous function does not work to combine table 1 variables because they are special
# we do this instead

table1_continuous_combined <- data.frame(variant_misc = '', 
                                variableName = '', 
                                mean_value = NA, 
                                sd_value = NA, 
                                n_patients = NA, 
                                site = '')
# save thsi for later
total_n <- data.frame()

for (s in sites) {
  
  load(paste0('./', s, '/replace_earlier/', s, '_table1Continuous.RData'))
  t1 <- read.table(paste0('./', s, '/replace_earlier/', s, '_table1.txt'), sep = '\t')
  
  nvec <- t1[1,]
  ndf <- data.frame(variant_misc = c('Alpha', 'Delta', 'Omicron', 'total'),
                    n_patients = c(str_extract(nvec$V2, '\\d+'),
                                   str_extract(nvec$V3, '\\d+'),
                                   str_extract(nvec$V4, '\\d+'),
                                   str_extract(nvec$V5, '\\d+'))) %>%
    mutate(n_patients = as.numeric(n_patients))
  
  total_n <- rbind(total_n, ndf %>% mutate(site = s))
  
  df <- table1_continuous
  df2 <- df %>%
    pivot_longer(cols = colnames(df)[-1]) %>%
    mutate(name = gsub('length_hospitalization', 'lengthhospitalization', name)) %>%
    separate(name, into = c('variableName', 'stat'), sep = '_') %>%
    pivot_wider(id_cols = c(variant_misc, variableName), names_from = stat, values_from = value) %>%
    mutate(site = s) %>%
    rename('mean_value' = mean,
           'sd_value' = sd) %>%
    left_join(ndf) %>%
    select(variant_misc, variableName, mean_value, sd_value, n_patients, site)
  
  table1_continuous_combined <- rbind(table1_continuous_combined, df2)

}

table1_continuous_combined <- table1_continuous_combined[-1,]


table1_continuous_results <- table1_continuous_combined %>%
  group_by(variant_misc, variableName) %>%
  summarise(res = continuousToMetaAnalysis(n_patients, mean_value, sd_value, site),
            total = sum(n_patients))

colnames(table1_continuous_results) <- c("variant", "variable", "value", "total")

rm(df, df2, ndf, nvec, t1, table1_continuous, table1_continuous_combined)

#####################
###### TABLE 3 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    table3_files <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table3.RData")
  }else{
    table3_filesInt <-  list.files(paste0("./", sites[i], "/replace_earlier/"), pattern = "table3.RData")
    table3_files <- c( table3_files, table3_filesInt)
  }
}

table3_all <- combineCategoricalDataFromSites( fileList = table3_files, table_num = 3 )

### final table 3

table3_categorical_results <- table3_all %>% 
  group_by(categories, variant_misc) %>%
  summarise(res = categoricalToMetaAnalysis(n, total, site),
            total = sapply( strsplit(res, " ;"), '[', 2), 
            res = sapply( strsplit(res, " ;"), '[', 1)
  )

rm(table3_all)
colnames(table3_categorical_results) <- c("Variable", "variant", "value", "total")



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
  summarise(res = continuousToMetaAnalysis(n_patients, mean_value, sd_value, site),
            total = sum(n_patients))

labsAllDuring_results <- labsAllDuring %>%
  group_by(variant_misc, variableName, time) %>%
  summarise(res = continuousToMetaAnalysis(n_patients, mean_value, sd_value, site),
            total = sum(n_patients))


labsAll_results <- rbind( labsAllAdm_results, labsAllDuring_results)

colnames(labsAll_results) <- c("variant", "variable", "time", "value", "total")

rm(labsAllAdm, labsAllAdm_results, labsAllDuring, labsAllDuring_results)



# transform to follow the format of the table 2

total_n_dfx <- total_n %>% 
  group_by(variant_misc) %>% 
  summarise(total_n = sum(n_patients)) %>%
  mutate(names = paste0(variant_misc, ' (N = ', total_n, ')')) %>%
  select(-total_n)

df <- labsAll_results

df2 <- df %>%
  pivot_wider(id_cols = c(variable, time), 
              names_from = variant, values_from = c(value, total)) %>%
  mutate(combined_n = paste0('(', total_Alpha, ', ', total_Delta, ', ', total_Omicron, ', ', total_total_n, ')')) %>%
  select(-total_Alpha, -total_Delta, -total_Omicron, -total_total_n) %>%
  arrange(variable) %>%
  select(variable, combined_n, time, value_Alpha, value_Delta, value_Omicron, value_total_n)

colnames(df2) <- c('Variable', 'Patients per variant', 'Timepoint', total_n_dfx$names)

table2 <- df2

rm(df, df2, labsAll_results)


##########
# save rdata files
save(table1_categorical_results, table1_continuous_results, table2, table3_categorical_results, total_n_dfx, 
     file = '../output/results.Rdata')


###########
# transform and write pretty
library(flextable)

table1_categorical_results2 <- table1_categorical_results %>%
  pivot_wider(id_cols = Variable, 
              names_from = variant, values_from = c(value, total)) %>%
  mutate(combined_n = paste0('(', total_Alpha, ', ', total_Delta, ', ', total_Omicron, ', ', total_total, ')')) %>%
  select(-total_Alpha, -total_Delta, -total_Omicron, -total_total) %>%
  arrange(Variable) %>%
  select(Variable, combined_n, value_Alpha, value_Delta, value_Omicron, value_total)

colnames(table1_categorical_results2) <- c('Variable', 'Patients per variant', total_n_dfx$names)

table1_categorical_results2 %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../output/final_table1_categorical.docx")


##

table1_continuous_results2 <- table1_continuous_results %>%
  pivot_wider(id_cols = variable, 
              names_from = variant, values_from = c(value, total)) %>%
  mutate(combined_n = paste0('(', total_Alpha, ', ', total_Delta, ', ', total_Omicron, ', ', total_total, ')')) %>%
  select(-total_Alpha, -total_Delta, -total_Omicron, -total_total) %>%
  arrange(variable) %>%
  select(variable, combined_n, value_Alpha, value_Delta, value_Omicron, value_total)

colnames(table1_continuous_results2) <- c('Variable', 'Patients per variant', total_n_dfx$names)

table1_continuous_results2 %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../output/final_table1_continuous.docx")

##

table2 %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../output/final_table2.docx")

##

table3 <- table3_categorical_results %>%
  pivot_wider(id_cols = Variable, 
              names_from = variant, values_from = c(value, total)) %>%
  mutate(combined_n = paste0('(', total_Alpha, ', ', total_Delta, ', ', total_Omicron, ', ', total_total, ')')) %>%
  select(-total_Alpha, -total_Delta, -total_Omicron, -total_total) %>%
  arrange(Variable) %>%
  select(Variable, combined_n, value_Alpha, value_Delta, value_Omicron, value_total)

colnames(table3) <- c('Variable', 'Patients per variant', total_n_dfx$names)

table3 %>%
  flextable::flextable() %>%
  flextable::save_as_docx(path = "../output/final_table3.docx")






