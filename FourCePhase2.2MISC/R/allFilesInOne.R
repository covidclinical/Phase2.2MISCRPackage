#' Function to aggregate all the files (obs_raw, demo_raw, clincal_raw) in one file
#' and add additional information of interest, as the variant_misc assesed based on
#' the date and the country
#'
#' Given the three files (obs_raw, demo_raw and clinical_raw) readed with the
#' \code{readInput} file function, merge all in one and create as output a
#' \code{data.frame} object with all the data integrated, and adding the variant as
#' well as the specific week, month and year that will allow us to stratify the
#' analysis as required.
#'
#' @param obs_raw Data frame with all the observations for the MISC patients
#' @param demo_raw Data frame with the MISC demographic information.
#' @param clincal_raw Data frame with all the clinical course details for MISC patients, starting with day 0 as the MISC admission date.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{data.frame}.
#' @examples
#'
#' misc_complete <- allFilesInOne(
#'                     obs_raw,
#'                     demo_raw,
#'                     clinical_raw,
#'                     dateFormat,
#'                     verbose
#'              )
#' @export allFilesInOne

allFilesInOne <- function(obs_df, demo_df, clinical_df, dateFormat, verbose = FALSE ){

  if(verbose == TRUE){
    print("merging observation data with clinical data by patient_num and days_since_admission")
  }

  misc_all <- left_join( obs_df,
                         clinical_df[, c("patient_num", "days_since_admission", "calendar_date", "in_hospital", "severe", "in_icu", "dead", "n_hospitalisation", "len_hospitalisation")],
                         by=c("patient_num", "days_since_admission"))

  if(verbose == TRUE){
    print("merging it with the demographic data by patient_num")
    print("adding the variant type")
  }
  misc_all <- left_join( misc_all, demo_df[, c("patient_num", "age", "sex", "admission_date")], by= "patient_num") %>%
    mutate( date = as.Date( admission_date, format = dateFormat ),
            variant_misc = ifelse( date >= variantsDates$Omicron, "Omicron", ifelse( date <= variantsDates$Alpha, "Alpha", "Delta")))

  if(verbose == TRUE){
    print("Add 3 columns, with the week, month and year of each admission_date")
  }
  output <- misc_all %>%
    filter( !is.na( calendar_date )) %>%
    dplyr::mutate( weeks = as.Date(cut( date, breaks = "week")),
                   month = as.Date(cut( date, breaks = "month")),
                   year = format( date, "%Y"))
  return( output )

}
