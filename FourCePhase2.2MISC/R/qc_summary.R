#' Function to create a summary table of the MISC Phase 2.2 data
#'
#' Given the \code{df} LocalPatientClinicalCourse data use the in_hospital column
#' that generates two new columns, \code{n_hospitalisation} which represents which hospitalization,
#' and the \code{len_hospitalisation} which represents the number of consecutive days
#' the patient spent at the hospital for that hospitalization.
#'
#' @param complete_df The combined dataframe generated from the allFilesInOne function
#' @param obfuscation_threshold FALSE if no obfuscation is needed, the numeric value when there is obfuscation
#' @param during_misc_hosp By default \code{TRUE}, meaning that the summary will only be generated for data during the first MISC hospitalization.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{data.frame} containing the summaries of labs, meds, and procedures described in the SRC
#' @examples
#'
#' qc_summary(
#'               df      = "./" )
#' @export qc_summary
#'
#'

# testing code
#complete_df <- misc_complete

qc_summary <- function(complete_df, obfuscation_threshold, currSiteId, during_misc_hosp = TRUE, dir.output){

  print("Starting QC summary")

  if(during_misc_hosp){
    complete_df <- complete_df %>% filter(n_hospitalisation == 1)
  }

  outcome_summary <- complete_df %>%
    group_by(patient_num) %>%
    summarise(ever_in_hospital = max(in_hospital),
              ever_severe = max(severe),
              ever_in_icu = max(in_icu),
              ever_dead = max(dead))
  print(paste0("Total number of patients ever hospitalized: ", sum(outcome_summary$ever_in_hospital)))
  print(paste0("Total number of patients ever severe: ", sum(outcome_summary$ever_severe)))
  print(paste0("Total number of patients ever in icu: ", sum(outcome_summary$ever_in_icu)))
  print(paste0("Total number of patients ever dead: ", sum(outcome_summary$ever_dead)))

  total_n <- length(unique(complete_df$patient_num))

  labs_of_interest <- read.delim(system.file(paste0("extdata", .Platform$file.sep,
                                "laboratoryCharacteristics.txt"), package = "FourCePhase2.2MISC"), stringsAsFactors = FALSE)

  meds_of_interest <- read.delim(system.file(paste0("extdata", .Platform$file.sep,
                                                    "medicationCharacteristics.txt"), package = "FourCePhase2.2MISC"), stringsAsFactors = FALSE)

  procs_of_interest <- c()
  diag_of_interest <- c()

  ### create laboratory value summary
  # include: max, min, and mean values
  # include: number of patients and %
  # calculate neutrophil ratio?

  lab_sum <- complete_df %>%
    filter(concept_code %in% labs_of_interest$concept_code) %>%
    select(siteid, cohort, patient_num, concept_code, value) %>%
    left_join(labs_of_interest, by = 'concept_code') %>%
    filter( value != -999,
            ! is.na( value ))
  print(paste0("There are ", n_distinct(lab_sum$concept_code), " lab codes of interest reported in the data"))


  # lab summary table
  if(n_distinct(lab_sum$concept_code) != 0 ){
    lab_sum <- lab_sum %>%
      group_by(variableName) %>%
      summarise(units = first(units),
                min_value = min(value, na.rm = TRUE),
                max_value = max(value, na.rm = TRUE),
                mean_value = mean(value, na.rm = TRUE),
                sd_value = sd(value, na.rm = TRUE),
                n_patients = n_distinct(patient_num)) %>%
      mutate( n_patients = ifelse( n_patients > obfuscation_threshold | isFALSE( obfuscation_threshold), n_patients, 0.5),
              perc_patients = (n_patients / total_n) * 100,
              siteid = currSiteId)

    #write.table(lab_sum, paste0(dir.output,'/QC/', currSiteId, '_MISC', 'lab_summary.txt'), quote = FALSE, row.names = FALSE)
    print(lab_sum)
  }


  ### create medication value summary
  # include: number of patients and %
  meds <- complete_df %>%
    filter(concept_type == 'MED-CLASS') %>%
    pull(concept_code) %>%
    unique()
  print("These are the meds that are present for MISC patients: ")
  print(meds)

  med_sum <- complete_df %>%
    filter(concept_code %in% meds_of_interest$concept_code) %>%
    select(siteid, cohort, patient_num, concept_code, value) %>%
    left_join(meds_of_interest, by = 'concept_code')
  print(paste0("There are ", n_distinct(med_sum$concept_code), " medication codes of interest reported in the data"))

  if( n_distinct(med_sum$concept_code) != 0){
    # medication summary table
    med_sum <- med_sum %>%
      group_by(variableName) %>%
      summarise( n_patients = n_distinct(patient_num)) %>%
      mutate(n_patients = ifelse( n_patients > obfuscation_threshold | isFALSE( obfuscation_threshold), n_patients, 0.5),
             perc_patients = (n_patients / total_n) * 100,
             siteid = currSiteId)

    #write.table(med_sum, paste0(dir.output, '/QC/', currSiteId, '_MISC', 'medication_summary.txt'), quote = FALSE, row.names = FALSE)
    print(med_sum)
  }

  if( 'PROC-GROUP' %in% complete_df$concept_type){
    ### create procedures value summary
    # include: number of patients and %
    proc_sum <- complete_df %>%
      filter(concept_type == 'PROC-GROUP') %>%
      group_by(concept_code) %>%
      summarise(n_patients = n_distinct(patient_num)) %>%
      mutate( n_patients = ifelse( n_patients > obfuscation_threshold | isFALSE( obfuscation_threshold), n_patients, 0.5),
              perc_patients = (n_patients / total_n) * 100,
              siteid = currSiteId)
    print(paste0("There are ", n_distinct(proc_sum$concept_code), " procedural codes reported in the data"))


    print(proc_sum)

    #write.table(proc_sum, paste0(dir.output, '/QC/', currSiteId, '_MISC', 'procedure_summary.txt'), quote = FALSE, row.names = FALSE)
  }

  if( 'DIAG-ICD10' %in% complete_df$concept_type){
    # add a summary of the ICD codes for QC
    diag_sum <- complete_df %>%
      filter( concept_type == 'DIAG-ICD10') %>%
      group_by( concept_code ) %>%
      summarise( n_patients = n_distinct( patient_num ) ) %>%
      mutate( n_patients = ifelse( n_patients > obfuscation_threshold | isFALSE( obfuscation_threshold), n_patients, 0.5)) %>%
      arrange( desc(n_patients))

    save( diag_sum, file=paste0( dir.output, "/QC/", currSiteId, "_ICDdiagnosisCodes.RData"))

    print(diag_sum[1:10,])
  }

}



