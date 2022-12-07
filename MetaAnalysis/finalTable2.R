setwd('/Users/alba/Desktop/a1l1nOn3/')
at_admission_files <- list.files("./atAdmission/")
during_admission_files <- list.files("./duringAdmission/")
totalN <- read.delim("./sites_totalPatients.txt")

for( i in 1:length( at_admission_files )){
  site_id <- sapply( strsplit(at_admission_files[i], "[_]"), '[', 1)
  
  load( paste0("./atAdmission/", at_admission_files[i]))
  n_total <- totalN %>% filter( site == site_id )
  
  if( i ==1 ){
    
    labs_adm <- table2_admission %>%
      mutate( percs = round(n_patients / n_total$total_n *100, 2),
              label = paste0( variableName, " (", units, ")"), 
              site = paste0( site_id, "\n (", n_total$total_n, ")"),
              site_label =  paste0( site_id, "\n (", n_patients, ")"),
              time = "admission")
    
    rm(table2_admission)
  }else{
    int_labs_adm <- table2_admission %>%
      mutate( percs = round(n_patients / n_total$total_n *100, 2),
              label = paste0( variableName, " (", units, ")"), 
              site = paste0( site_id, "\n (", n_total$total_n, ")"), 
              site_label =  paste0( site_id, "\n (", n_patients, ")"),
              time = "admission" )
    labs_adm <- rbind( labs_adm, int_labs_adm)
  }
}

for( i in 1:length( during_admission_files )){
  site_id <- sapply( strsplit(during_admission_files[i], "[_]"), '[', 1)
  load( paste0("./duringAdmission/", during_admission_files[i]))
  n_total <- totalN %>% filter( site == site_id )
  
  if( i ==1 ){
    
    labs_during <- table2_during %>%
      mutate( percs = round(n_patients / n_total$total_n *100, 2),
              label = paste0( variableName, " (", units, ")"), 
              site = paste0( site_id, "\n (", n_total$total_n, ")"), 
              site_label =  paste0( site_id, "\n (", n_patients, ")"),
              time = "during")
    
    rm(table2_during)
  }else{
    int_labs_during <- table2_during %>%
      mutate( percs = round(n_patients / n_total$total_n *100, 2),
              label = paste0( variableName, " (", units, ")"), 
              site = paste0( site_id, "\n (", n_total$total_n, ")"), 
              site_label =  paste0( site_id, "\n (", n_patients, ")"),
              time = "during" )
    labs_during <- rbind( labs_during, int_labs_during)
  }
}

labs <- rbind( labs_adm, labs_during)

labsToMetaAnalysis <- labs %>%
  select( variant_misc, variableName, mean_value, sd_value, n_patients, site, time ) %>%
  mutate( site = sapply( strsplit(site, " "), '[', 1), 
          site = gsub("\n", "", site ))

### final table 2 at admission
labsToMetaAnalysisAtAdmission <- labsToMetaAnalysis %>%
  filter( time == "admission")
finalTable2AtAdmission <- as.data.frame( matrix( ncol= 6, nrow=length(unique(labsToMetaAnalysisAtAdmission$variableName ))))
finalTable2AtAdmission$V1 <- unique( labsToMetaAnalysisAtAdmission$variableName)
finalTable2AtAdmission$V6 <- 'admission'

for(i in 1:nrow( finalTable2AtAdmission )){
  
  variable <- finalTable2AtAdmission$V1[i]
  
  toForestAlpha <- labsToMetaAnalysisAtAdmission %>%
    filter( variableName == variable &
              variant_misc == "Alpha")
  
  mtmeanAlpha <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestAlpha, sm="MRAW", method.ci= "z",
                      comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2AtAdmission$V2[i] <- paste0( round( mtmeanAlpha$TE.random, 2), " [", round(mtmeanAlpha$lower.random, 2), ", ",round( mtmeanAlpha$upper.random, 2),  "]")
  
  toForestDelta <- labsToMetaAnalysisAtAdmission %>%
    filter( variableName == variable &
              variant_misc == "Delta")
  
  mtmeanDelta <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestDelta, sm="MRAW", method.ci= "z",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2AtAdmission$V3[i] <- paste0( round( mtmeanDelta$TE.random, 2), " [", round(mtmeanDelta$lower.random, 2), ", ",round( mtmeanDelta$upper.random, 2),  "]")
  
  toForestOmicron <- labsToMetaAnalysisAtAdmission %>%
    filter( variableName == variable &
              variant_misc == "Omicron")
  
  mtmeanOmicron <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestOmicron, sm="MRAW", method.ci= "z",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2AtAdmission$V4[i] <- paste0( round( mtmeanOmicron$TE.random, 2), " [", round(mtmeanOmicron$lower.random, 2), ", ",round( mtmeanOmicron$upper.random, 2),  "]")
  
  toForestTotal <- labsToMetaAnalysisAtAdmission %>%
    filter( variableName == variable &
              variant_misc == "total_n")
  
  mtmeanTotal <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestTotal, sm="MRAW", method.ci= "z",
                             comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2AtAdmission$V5[i] <- paste0( round( mtmeanTotal$TE.random, 2), " [", round(mtmeanTotal$lower.random, 2), ", ",round( mtmeanTotal$upper.random, 2),  "]")
  
  
}


### final table 2 during admission
labsToMetaAnalysisDuringAdmission <- labsToMetaAnalysis %>%
  filter( time == "during")
finalTable2DuringAdmission <- as.data.frame( matrix( ncol= 6, nrow=length(unique(labsToMetaAnalysisDuringAdmission$variableName ))))
finalTable2DuringAdmission$V1 <- unique( labsToMetaAnalysisDuringAdmission$variableName)
finalTable2DuringAdmission$V6 <- 'during'

for(i in 1:nrow( finalTable2DuringAdmission )){
  
  variable <- finalTable2DuringAdmission$V1[i]
  
  toForestAlpha <- labsToMetaAnalysisDuringAdmission %>%
    filter( variableName == variable &
              variant_misc == "Alpha")
  
  mtmeanAlpha <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestAlpha, sm="MRAW", method.ci= "z",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2DuringAdmission$V2[i] <- paste0( round( mtmeanAlpha$TE.random, 2), " [", round(mtmeanAlpha$lower.random, 2), ", ",round( mtmeanAlpha$upper.random, 2),  "]")
  
  toForestDelta <- labsToMetaAnalysisDuringAdmission %>%
    filter( variableName == variable &
              variant_misc == "Delta")
  
  mtmeanDelta <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestDelta, sm="MRAW", method.ci= "z",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2DuringAdmission$V3[i] <- paste0( round( mtmeanDelta$TE.random, 2), " [", round(mtmeanDelta$lower.random, 2), ", ",round( mtmeanDelta$upper.random, 2),  "]")
  
  toForestOmicron <- labsToMetaAnalysisDuringAdmission %>%
    filter( variableName == variable &
              variant_misc == "Omicron")
  
  mtmeanOmicron <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestOmicron, sm="MRAW", method.ci= "z",
                             comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2DuringAdmission$V4[i] <- paste0( round( mtmeanOmicron$TE.random, 2), " [", round(mtmeanOmicron$lower.random, 2), ", ",round( mtmeanOmicron$upper.random, 2),  "]")
  
  toForestTotal <- labsToMetaAnalysisDuringAdmission %>%
    filter( variableName == variable &
              variant_misc == "total_n")
  
  mtmeanTotal <-  metamean(n=n_patients, mean=mean_value, sd=sd_value, studlab=site, data=toForestTotal, sm="MRAW", method.ci= "z",
                           comb.fixed = TRUE, comb.random = TRUE, hakn = TRUE)
  finalTable2DuringAdmission$V5[i] <- paste0( round( mtmeanTotal$TE.random, 2), " [", round(mtmeanTotal$lower.random, 2), ", ",round( mtmeanTotal$upper.random, 2),  "]")
  
  
}

finalTable2 <- rbind( finalTable2AtAdmission, finalTable2DuringAdmission )
colnames( finalTable2 ) <- c("Variable", "Alpha", "Delta", "Omicron", "Total", "Time Point")

finalTable2 <- finalTable2 %>%
  arrange( Variable ) %>%
  flextable::flextable() %>% 
  flextable::save_as_docx(path = "/Users/alba/Desktop/tentative_final_table2.docx")

