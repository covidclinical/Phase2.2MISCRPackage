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
#' @param variants_df Data frame with the variant dates filtered by country in run_analysis()
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

allFilesInOne <- function(obs_df, demo_df, clinical_df, variants_df, dateFormat, washout_days = 0, washout_opt, verbose = FALSE ){

  if(verbose == TRUE){
    print("merging observation data with clinical data by patient_num and days_since_admission")
  }

  print( paste0("Number of MISC patients in obs_df: ", length(unique( obs_df$patient_num ))))
  print( paste0("Number of MISC patients in clinical_df: ", length(unique( clinical_df$patient_num ))))
  print( paste0("Number of MISC patients in demo_df: ", length(unique( demo_df$patient_num ))))

  misc_all <- left_join( obs_df,
                         clinical_df[, c("patient_num", "days_since_admission", "calendar_date", "in_hospital", "severe", "in_icu", "dead", "n_hospitalisation", "len_hospitalisation")],
                         by=c("patient_num", "days_since_admission"))

  print( paste0("Number of MISC patients after left join obs_df and clinical_df ", length(unique( misc_all$patient_num ))))

  if(verbose == TRUE){
    print("merging it with the demographic data by patient_num")
    print("adding the variant type")
  }
  #misc_all <- left_join( misc_all, demo_df[, c("patient_num", "age", "sex", "admission_date")], by= "patient_num") %>%
  #  mutate( date = as.Date( admission_date, format = dateFormat ),
  #          variant_misc = ifelse( date >= variants_df$Omicron, "Omicron", ifelse( date <= variants_df$Alpha, "Alpha", "Delta")))

  misc_all <- left_join( misc_all, demo_df[, c("patient_num", "age", "sex", "admission_date")], by= "patient_num") %>%
    mutate( date = as.Date( admission_date, format = dateFormat ))

  print( paste0("Number of MISC patients after left join misc_all and demo_df ", length(unique( misc_all$patient_num ))))

  variants_df$a <- as.Date( variants_df$Alpha ) - washout_days
  variants_df$b <- as.Date( variants_df$Alpha ) + washout_days

  variants_df$c <- as.Date( variants_df$Omicron ) - washout_days
  variants_df$d <- as.Date( variants_df$Omicron ) + washout_days

  if( washout_opt == "remove"){
    print(paste0("Removing patients in ", washout_days, " days before and after the cut-off date of each variant"))
    print("Washout option is: remove")
    misc_all <- misc_all %>%
      mutate( variant_misc = ifelse( date <= variants_df$a, "Alpha",
                                     ifelse( date >= variants_df$b & date <= variants_df$c, "Delta",
                                             ifelse( date >= variants_df$d, "Omicron", "washout"))) )
  }else if( washout_opt == "replace_earlier"){
    print("Washout option is: replace assuming the variant arrived earlier than expected")
    misc_all <- misc_all %>%
      mutate( variant_misc = ifelse( date <= variants_df$a, "Alpha",
                                     ifelse( date > variants_df$a & date <= variants_df$c, "Delta",
                                             ifelse( date > variants_df$c, "Omicron", "uncategorized"))) )
  }else if( washout_opt == "replace_late"){
    print("Washout option is: replace assuming the variant arrived later than expected")
    misc_all <- misc_all %>%
      mutate( variant_misc = ifelse( date <= variants_df$b, "Alpha",
                                     ifelse( date > variants_df$b & date <= variants_df$d, "Delta",
                                             ifelse( date > variants_df$d, "Omicron", "uncategorized"))) )

  }

  toCheck <- misc_all %>% filter( n_hospitalisation == 1 ) %>%
    select( patient_num, date, variant_misc  ) %>%
    unique()
  print( table( toCheck$variant_misc))


  if(verbose == TRUE){
    print("Add 3 columns, with the week, month and year of each admission_date")
  }
  output <- misc_all %>%
    filter( !is.na( calendar_date )) %>%
    filter( ! variant_misc %in% c( "washout", "uncategorized") ) %>%
    dplyr::mutate( weeks = as.Date(cut( date, breaks = "week")),
                   month = as.Date(cut( date, breaks = "month")),
                   year = format( date, "%Y"))

  print( paste0("Number of MISC patients after filter by !is.na(calendar_date) ", length(unique( output$patient_num ))))

  if( length( unique( output$patient_num)) != length( unique(misc_all$patient_num))){

    print( "entries to review")

    to_review <- misc_all %>%
      filter( is.na( calendar_date ) & days_since_admission > 0) %>%
      select(-patient_num)
    print( to_review )
  }


  return( output )

}

