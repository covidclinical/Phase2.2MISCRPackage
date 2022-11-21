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
#' @param country Specify the country of origin or your data (e.g, US, France, UK, Spain)
#' @param cbPalette Color palette to use for the plots (set up by default)
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{list} with the \code{data.frames}.
#' @examples
#'
#' FourCePhase2.2MISC::runAnalysis()
#' @export runAnalysis


runAnalysis <- function( dir.input, dir.output, obfuscation, raceAvailable, dateFormat, data_update_date, country, cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), verbose ) {

  tryCatch({

    ## Create the output folder if it doesn't exist

    if(verbose == TRUE){ print("Creating the output folders if they don't exist")}
    if (! dir.output %in% list.dirs()) {dir.create(dir.output)}
    dir.output.qc <- paste0(dir.output, "/QC")
    if (! dir.output.qc %in% list.dirs()) {dir.create(dir.output.qc)}

    sink(file = paste0( dir.output, "/QC/MISC_logs_QC.txt"))
    print('##########')
    print(timestamp())
    print('##########')
    print(paste0( 'FourCePhase2.2MISC Package version: ', packageVersion("FourCePhase2.2MISC")))
    print('##########')

    dir.output.figures <- paste0(dir.output, "/figures")
    if (! dir.output.figures %in% list.dirs()) {dir.create(dir.output.figures)}

    if(verbose == TRUE){ print("Output folders successfully created")}

  }, error = function(e) sink())

  ## Read the 2.2 files
  tryCatch({
    if(verbose == TRUE){ print("Reading input files")}

    files <- FourCePhase2.2MISC::readInputFiles( path = dir.input,
                                        separator = ",",
                                        skip      = 0,
                                        verbose   = verbose )


    ### Extract the patient summary and observation information.
    demo_raw <- files[["patientSummary"]]
    obs_raw <- files[["patientObservations"]]
    clinical_raw <- files[["patientClinicalCourse"]] %>% filter(cohort == 'MISC')
    if(verbose == TRUE){print('Input files successfully read, patient data extracted')}

  }, error = function(e) sink())

  ### Read the file containing race information for those sites recording this variable
  tryCatch({
    if(verbose == TRUE & raceAvailable == TRUE){ print("Reading the LocalPatientRace.csv")}

    if( raceAvailable == TRUE ){
      race_raw <- read.delim(file.path(dir.input, "/LocalPatientRace.csv"), sep = ",", skip = 0)
      raceCategories <- unique( race_raw$race_4ce )
      print( paste0( "Race categories present in this dataset: ", paste( raceCategories, collapse = "; ")))
    }
  }, error = function(e) {
    print('error reading race info')
    sink()
    })

  ### Read the file containing the variants dates per country
  tryCatch({
    if(verbose == TRUE){ print("Loading the internal file with the variant date")}
    if(verbose == TRUE){ print("Checking that the country is on the right format")}

    if( tolower(country) %in% c("us", "france", "spain", "uk")){
      variantsDates <- read.delim(system.file(paste0("extdata", .Platform$file.sep,
                                            "variantsDates.txt"), package = "FourCePhase2.2MISC"), stringsAsFactors = FALSE)
      variantsDates <- variantsDates %>%
        filter(  tolower( Country ) == tolower( country ) )
    }
  }, error = function(e) {
    print('error reading files')
    sink()
  })


  #### estimate hospitalization count and length
  tryCatch({
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
    if( is.null( site)){
      print("siteid column missing. A temporary site id, 'TBD' will be used")
      site = "TBD"
    }
    if(length(site) == 1){
      print(paste0("Site ID is: ", site ))
    }else{
      print("Site id has more than one value, the first one will be used as site id")
      site <- site[1]
      print(paste0("Site ID is: ", site ))
    }

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

  }, error = function(e) {
    print('error estimating hospitalization count and length')
    sink()
  })



  ### QC
  tryCatch({
    qc_summary( complete_df =  misc_complete, obfuscation_threshold = obfuscation,  currSiteId = site,during_misc_hosp = TRUE, dir.output=dir.output)
  }, error = function(e) {
    print('error generating qc summary')
    sink()
  })


  ## estimate the number of MISC patients per period
  tryCatch({
    misc_cases_perTimePeriod(integrated_df =  misc_complete, period = "month", obfuscation_threshold = obfuscation, output_plot = TRUE, output_df = FALSE, dir.output = dir.output, verbose = verbose)
  }, error = function(e) {
    print('error estimating number of MISC patients per period')
    sink()
  })

  ## sex and age distribution overview
  tryCatch({
    misc_overview( integrated_df =  misc_complete, obfuscation_threshold = obfuscation, output_plot = TRUE, output_df = FALSE, dir.output = dir.output,cbPalette = cbPalette, verbose= verbose )
  }, error = function(e) {
    print('error generating sex and age distribution overview')
    sink()
  })

  ## table 1
  tryCatch({
    t1_misc <- misc_table1( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, raceAvailable, dir.input = dir.input, dir.output = dir.output,verbose)
    print("Table 1 successfully generated")
  }, error = function(e) {
    print('error generating table 1')
    sink()
  })

  # table 2
  tryCatch({
    t2_misc <- misc_table2( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, dir.output = dir.output, verbose )
    print("Table 2 successfully generated")
  }, error = function(e) {
    print('error generating table 2')
    sink()
  })

  ## table 3
  tryCatch({
    t3_misc <- misc_table3( complete_df = misc_complete, currSiteId = site, obfuscation_threshold = obfuscation, raceAvailable, dir.input = dir.input, dir.output = dir.output, verbose )
    print("Table 3 successfully generated")
  }, error = function(e) {
    print('error generating table 3')
    sink()
    }
  )

  print('done')
  sink()
  print('sink closed')

}

