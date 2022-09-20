#' Function to create a summary table of the MISC Phase 2.2 data
#'
#' Given the \code{df} LocalPatientClinicalCourse data use the in_hospital column
#' that generates two new columns, \code{n_hospitalisation} which represents which hospitalization,
#' and the \code{len_hospitalisation} which represents the number of consecutive days
#' the patient spent at the hospital for that hospitalization.
#'
#' @param complete_df The combined dataframe generated from the allFilesInOne function
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

qc_summary <- function(complete_df, during_misc_hosp = TRUE){

  if(during_misc_hosp){
    complete_df <- complete_df %>% filter(n_hospitalisation == 1)
  }

  total_n <- length(unique(complete_df$patient_num))

  labs_of_interest <- read.table('inst/extdata/laboratoryCharacteristics.txt', header = TRUE)
  meds_of_interest <-  read.table('inst/extdata/medicationCharacteristics.txt', header = TRUE, colClasses = 'character')
  procs_of_interest <- c()
  diag_of_interest <- c()

  ### create laboratory value summary
  # include: max, min, and mean values
  # include: number of patients and %
  # calculate neutrophil ratio?

  lab_sum <- complete_df %>%
    filter(concept_code %in% labs_of_interest$concept_code) %>%
    select(siteid, cohort, patient_num, concept_code, value) %>%
    left_join(labs_of_interest, by = 'concept_code')

  # lab summary table
  lab_sum <- lab_sum %>%
    group_by(variableName) %>%
    summarise(units = first(units),
              min_value = min(value),
              max_value = max(value),
              mean_value = mean(value),
              n_patients = n_distinct(patient_num)) %>%
    mutate(perc_patients = (n_patients / total_n) * 100)

  ### create medication value summary
  # include: number of patients and %
  med_sum <- complete_df %>%
    filter(concept_code %in% meds_of_interest$concept_code) %>%
    select(siteid, cohort, patient_num, concept_code, value) %>%
    left_join(meds_of_interest, by = 'concept_code')

  # lab summary table
  med_sum <- med_sum %>%
    group_by(variableName) %>%
    summarise(n_patients = n_distinct(patient_num)) %>%
    mutate(perc_patients = (n_patients / total_n) * 100)

  ### create procedures value summary
  # include: number of patients and %
  proc_sum <- complete_df %>%
    filter(concept_type == 'PROC-GROUP') %>%
    group_by(concept_code) %>%
    summarise(n_patients = n_distinct(patient_num)) %>%
    mutate(perc_patients = (n_patients / total_n) * 100)

  proc_sum

  ## how to export / show summary table?
  print(lab_sum)
  print(med_sum)
  print(proc_sum)

}


