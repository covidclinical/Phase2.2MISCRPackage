#' Function to run the 4CE MISC pediatric project using as input the 2.2 4CE misc cohort dataset
#'
#' Information about how to create the 2.2 data can be found here: https://github.com/covidclinical/PhaseX.2SqlDataExtraction
#' The additional script required to create the MISC cohort can be found here: https://github.com/covidclinical/PhaseX.2SqlDataExtraction/blob/main/Extras/4CE_PhaseX.2_CustomCohorts_MISCPatients_mssql.sql
#'
#' @param dir.input The path where the 2.2 MISC cohort files are located.
#' @param dir.output The path to the output directory where the aggregate counts and plots will be saved.
#' @param obfuscation Determine the obfuscation threshold (FALSE if no obfuscation, or the numeric value of the obfuscation threshold if any). By default \code{FALSE}
#' @param raceAvailable set as TRUE or FALSE depending on whether the variable is being collected at your site. By default \code{TRUE}
#' @param dateFormat Specify the format of the date at your site (e.g., for "03-AUG-20", the format would be "%d-%b-%y", see documentation](https://www.stat.berkeley.edu/~s133/dates.html))
#' @param data_update_date date at which the data has been updated in the local data warehouse. Used to estimate patient age at time of visit, since patients age in the 4CE demographic file is expected the age at data update.
#' @param country Specify the country of origin or your data (e.g, US, France, London, Spain)
#' @param cbPalette Color palette to use for the plots (set up by default)
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{list} with the \code{data.frames}.
#' @examples
#'
#' FourCePhase2.2MISC::runAnalysis()
#' @export runAnalysis


runAnalysis <- function( dir.input, dir.output, obfuscation, raceAvailable, dateFormat, data_update_date, country, cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), verbose ) {

  ## Create the output folder if it doesn't exist
  if(verbose == TRUE){ print("Creating the output folder if it doesn't exist")}

  if (! dir.output %in% list.dirs()) dir.create(dir.output)

  dir.output.qc <- paste0(dir.output, "/QC/")
  if (! dir.output.qc %in% list.dirs()) dir.create(dir.output.qc)

  dir.output.figures <- paste0(dir.output, "/figures/")
  if (! dir.output.figures %in% list.dirs()) dir.create(dir.output.figures)

  sink(paste0( dir.output, "/QC/MISC_logs_QC.txt"))


  ## Read the 2.2 files
  if(verbose == TRUE){ print("Reading input files")}

  files <- FourCePhase2.2MISC::readInputFiles( path = dir.input,
                                      separator = ",",
                                      skip      = 0,
                                      verbose   = verbose )


  ### Extract the patient summary and observation information.
  demo_raw <- files[["patientSummary"]]
  obs_raw <- files[["patientObservations"]]
  clinical_raw <- files[["patientClinicalCourse"]] %>% filter(cohort == 'MISC')

  ### Read the file containing race information for those sites recording this variable
  if(verbose == TRUE & raceAvailable == TRUE){ print("Reading the LocalPatientRace.csv")}

  if( raceAvailable == TRUE ){
    race_raw <- read.delim(file.path(dir.input, "/LocalPatientRace.csv"), sep = ",", skip = 0)
  }

  ### Read the file containing the variants dates per country
  if(verbose == TRUE){ print("Loading the internal file with the variant date")}
  if(verbose == TRUE){ print("Checking that the country is on the right format")}

  if( tolower(country) %in% c("us", "france", "spain", "london")){
    variantsDates <- read.delim(system.file(paste0("extdata", .Platform$file.sep,
                                          "variantsDates.txt"), package = "FourCePhase2.2MISC"), stringsAsFactors = FALSE)
    variantsDates <- variantsDates %>%
      filter(  tolower( Country ) == tolower( country ) )
  }

  #### estimate hospitalization count and length
  hospitalisations_seq_df <- clinical_raw %>%
    distinct(patient_num, days_since_admission, in_hospital) %>%
    group_by(patient_num) %>%
    group_modify(count_sequences_hospitalisation)

  clinical_raw <- left_join(clinical_raw,
                            hospitalisations_seq_df,
                            by = c("patient_num", "days_since_admission"))

  #### Integrate it with the misc_complete df
  ## merge all the files as one data frame for the analysis
  misc_complete <- allFilesInOne(obs_df = obs_raw, demo_df = demo_raw, clinical_df = clinical_raw, variants_df = variantsDates, dateFormat = dateFormat, verbose = verbose )
  site <- unique( misc_complete$siteid )

  ### check how many of the patients got hospitalized in day_since_admission = 0
  ### filter
  totalMISCpatients <- length( unique( misc_complete$patient_num ) )
  print( paste0( "Total MISC patients: ", totalMISCpatients ))

  misc_hospitalization_flag <- misc_complete %>%
    dplyr::mutate( misc_hospitalized = ifelse( days_since_admission == 0 & in_hospital == 1, 1, 0)) %>%
    dplyr::group_by( patient_num ) %>%
    dplyr::summarise( misc_hospitalized = max( misc_hospitalized ))

  misc_no_hosp <- misc_hospitalization_flag %>%
    dplyr::filter( misc_hospitalized == 0 )
  print( paste0( "MISC patients not hospitalized during the MISC admission date provided: ", length(unique( misc_no_hosp$patient_num)) ))

  misc_complete <- left_join( misc_complete, misc_hospitalization_flag )

  ## filter to focus only on those that were hospitalized
  misc_complete <- misc_complete %>% filter( misc_hospitalized == 1)


  ### QC
  qc_summary( complete_df =  misc_complete, obfuscation_threshold = obfuscation, during_misc_hosp = TRUE, dir.output=dir.output, site_id = site)


  ## estimate the number of MISC patients per period
  misc_cases_perTimePeriod(integrated_df =  misc_complete, period = "month", output_plot = TRUE, output_df = FALSE, dir.output = dir.output, verbose = verbose)


  ## sex and age distribution overview
  misc_overview( integrated_df =  misc_complete, obfuscation_threshold = obfuscation, output_plot = TRUE, output_df = FALSE, dir.output = dir.output,cbPalette = cbPalette, verbose= verbose )

  ## table 1
  t1_misc <- misc_table1( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, raceAvailable, dir.input = dir.input, dir.output = dir.output,verbose)
  print("Table 1 successfully generated")

  # table 2
  t2_misc <- misc_table2( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, dir.output = dir.output, verbose )
  print("Table 2 successfully generated")

  ## table 3
  t3_misc <- misc_table3( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, raceAvailable, dir.input = dir.input, dir.output = dir.output, verbose )
  print("Table 3 successfully generated")

  sink()

}

