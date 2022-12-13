library(dplyr)
library(tidyr)
library(meta)
rm(list=ls())
setwd('/Users/alba/Desktop/4CE_MISC_outputs/')
sites <- list.files("./")

########################
###### FUNCTIONS ######
#######################
combineCategoricalDataFromSites <- function( fileList, table_num ){
  
  for( i in 1:length( fileList )){
    
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    
    #load the file
    load( paste0("./", site_id, "/original/", fileList[i]))
    
    if( table_num == 1){
      inputData <- table1_categorical %>% 
        mutate( site = site_id )
      rm(table1_categorical)
      
      total_Values <- read.delim(paste0("./", site_id, "/original/", site_id, "_table1.txt" ))
      
    }else if( table_num == 3){
      inputData <- table3 %>% 
        mutate( site = site_id )
      rm(table3)
      
      total_Values <- read.delim(paste0("./", site_id, "/original/", site_id, "_table3.txt" ))
      
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

##### combine categorical data from sites
combineContinuousDataFromSites <- function( fileList, time_point, table_num ){
  
  for( i in 1:length( fileList )){
    
    #extract the site id
    site_id <- sapply( strsplit(fileList[i], "[_]"), '[', 1)
    
    #load the file
    load( paste0("./", site_id, "/original/", fileList[i]))
    
    if( table_num == 2){
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
    }
    
    if( table_num == 1){
      inputData <- table1_continuous
      rm( table1_continuous )
      
      table_output <- inputData %>%
        mutate( site = site_id ) %>%
        select( variant_misc, variableName, mean_value, sd_value, n_patients, site) 
      
    }
    
    #remove the original df
    rm(inputData)
    
    if( i ==1 ){
      output <- table_output
    }else{
      table_output <- rbind( table_output, output)
    }
    
  }
  return( table_output )
}


# run categorical meta-analysis
categoricalToMetaAnalysis <- function( df, variant ){
  df_selection <- df %>% 
    filter( variant_misc == variant )
  
  output <- as.data.frame( matrix( ncol=4, nrow=length(unique(df_selection$categories ))))
  output$V1 <- unique( df_selection$categories)
  output$V3 <- variant
  
  for(i in 1:nrow( output )){
    
    variable <- output$V1[i]
    print(variable)
    print(i)
    
    toForestOutput <- df_selection %>%
      filter( categories == variable )
    
    ## this needs to be checked. it is failing if not
    if( 1 %in% unique(toForestOutput$n) & nrow(toForestOutput) == 2 ){
      toForestOutput <- toForestOutput %>%
       filter(n > 1)

     if(nrow( toForestOutput) == 0){
       next()
     }
    }
    
    mtprop <- metaprop(event=n, n=total, studlab=site, data=toForestOutput, method = "GLMM", sm = "PLOGIT",
                       comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE, verbose = TRUE)
    
    
    output$V2[i] <- paste0( round( mtprop$TE.random, 2), " [", round(mtprop$lower.random, 2), ", ",round( mtprop$upper.random, 2),  "]")
    output$V4[i] <- sum(mtprop$n)
  }
  return(output)
}

## run continous meta-analysis 
continuousToMetaAnalysis <- function( df, time, variant ){
  df_selection <- df %>% 
    filter( time == time & variant_misc == variant )
  
  output <- as.data.frame( matrix( ncol=5, nrow=length(unique(df_selection$variableName ))))
  output$V1 <- unique( df_selection$variableName)
  output$V3 <- time
  output$V5 <- variant
  
  for(i in 1:nrow( output )){
    
    variable <- output$V1[i]
    
    toForestOutput <- df_selection %>%
      filter( variableName == variable )
    
    mtmean <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestOutput, sm="MRAW", method.ci= "z",
                        comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
    output$V2[i] <- paste0( round( mtmean$TE.random, 2), " [", round(mtmean$lower.random, 2), ", ",round( mtmean$upper.random, 2),  "]")
    output$V4[i] <- sum(mtmean$n)
  }
  return(output)
}


#####################
###### TABLE 1 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    table1_files <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table1")
  }else{
    table1_filesInt <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table1")
    table1_files <- c( table1_files, table1_filesInt)
  }
}

continuousTable1 <- table1_files[ grepl( "Continuous.RData", table1_files)] 
categoricalTable1 <- table1_files[ grepl( "Categorical.RData", table1_files)] 


table1_categorical <- combineCategoricalDataFromSites( fileList = categoricalTable1, table_num = 1 )

variants <- c("Alpha", "Delta", "Omicron", "total")

for( v in 1:length(variants ) ){
  print(paste0("Variant: ", variants[v]))
  if( v == 1 ){
    t1_categorical_output <- categoricalToMetaAnalysis(df = table1_categorical, variant = variants[v])
  }else{
    t1_categorical_outputInt <- categoricalToMetaAnalysis(df = table1_categorical, variant = variants[v])
    t1_categorical_output <- rbind( t1_categorical_output, t1_categorical_outputInt)
  }
}

rm(t1_categorical_outputInt)
rm(table1_categorical)
colnames(t1_categorical_output) <- c("Variable", "value", "variant", "total")

# the continuous function does not work for table 1 variables because they are special
#table1_continuous <- combineContinuousDataFromSites( fileList = continuousTable1, table_num = 1)


#####################
###### TABLE 3 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    table3_files <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table3.RData")
  }else{
    table3_filesInt <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table3.RData")
    table3_files <- c( table3_files, table3_filesInt)
  }
}

table3_all <- combineCategoricalDataFromSites( fileList = table3_files, table_num = 3 )

### final table 3

variants <- c("Alpha", "Delta", "Omicron", "total")

for( v in 1:length(variants ) ){
  print(paste0("Variant: ", variants[v]))
  if( v == 1 ){
    t3_output <- categoricalToMetaAnalysis(df = table3_all, variant = variants[v])
    }else{
      t3_outputInt <- categoricalToMetaAnalysis(df = table3_all, variant = variants[v])
      t3_output <- rbind( t3_output, t3_outputInt)
    }
}

colnames(t3_output) <- c("Variable", "value", "variant", "total")



#####################
###### TABLE 2 ######
#####################
for( i in 1:length(sites)){
  if(i == 1){
    labFiles <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table2")
  }else{
    labFilesInt <-  list.files(paste0("./", sites[i], "/original/"), pattern = "table2")
    labFiles <- c( labFiles, labFilesInt)
  }
}



### split the files
atAdmission <- labFiles[ grepl( "AtAdmission.RData", labFiles)] 
duringAdmission <- labFiles[ grepl( "DuringAdmission.RData", labFiles)] 

labsAllAdm <- combineContinuousDataFromSites( fileList = atAdmission, time_point = "admission", table_num =  2)
labsAllDuring <- combineContinuousDataFromSites( fileList = duringAdmission, time_point = "during", table_num = 2)

  
### final table 2 at admission
times <- c("admission", "during")
variants <- c("Alpha", "Delta", "Omicron", "total_n")
labsAll <- rbind( labsAllAdm, labsAllDuring)

for( j in 1:length(times)){
  time_selection <- times[j]
  print(paste0("Time: ", time_selection ) )
  
  for( h in 1:length(variants ) ){
    print(paste0("Variant: ", variants[h]))
    if( j == 1 & h == 1){
      t2_output <- continuousToMetaAnalysis(df = labsAll, time = times[j], variant = variants[h])
    }else{
      t2_outputInt <- continuousToMetaAnalysis(df = labsAll, time = times[j], variant = variants[h])
      t2_output <- rbind( t2_output, t2_outputInt)
    }
  }
}
colnames(t2_output) <- c("Variable", "value", "time", "total", "variant")

# transform to follow the format of the table 2
#t2_output_pivot <- t2_output %>%
#  pivot_wider(names_from = variant, values_from = value )


#colnames( finalTable2 ) <- c("Variable", "Alpha", "Delta", "Omicron", "Total", "Time Point")

#finalTable2 <- finalTable2 %>%
#  select( Variable, Total, Alpha, Delta, Omicron, `Time Point`) %>%
#  arrange( Variable ) %>%
#  flextable::flextable() %>% 
#  flextable::save_as_docx(path = "/Users/alba/Desktop/tentative_final_table2.docx")

