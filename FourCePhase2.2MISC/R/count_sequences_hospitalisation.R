#' Function to estimate the sequences of hospitalization in Phase 2.2 data
#'
#' Given the \code{df} LocalPatientClinicalCourse data use the in_hospital column
#' that generates two new columns, \code{n_hospitalisation} which represents which hospitalization,
#' and the \code{len_hospitalisation} which represents the number of consecutive days
#' the patient spent at the hospital for that hospitalization.
#'
#' @param df The clinical_raw data frame generated when applying the readInputFiles function.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{data.frame} with two additional columns, the  \code{len_hospitalisation}
#' and \code{n_hospitalisation}.
#' @examples
#'
#' count_sequences_hospitalisation(
#'               df      = "./" )
#' @export count_sequences_hospitalisation

#####
# using clinical_raw as input df
# df for testing. use patient 104511172,
#df <- clinical_raw %>%
#  filter(patient_num == 104511172)

#####


count_sequences_hospitalisation <- function(df, verbose = FALSE) {

  if(verbose == TRUE){
    print('Creating the sequence data')
  }

  # not sure what this is for
  seq_hospitalisation_df <- data.frame(total_span = seq(min(df$days_since_admission),
                                                        max(df$days_since_admission))) %>%
    left_join(df, by = c("total_span" = "days_since_admission")) %>%
    replace_na(list(in_hospital = 0))

  if(verbose == TRUE){
    print('Estimating the length of hospitalization')
  }

  rle_res <- rle(df$in_hospital)
  rle_lengths <- rle_res$lengths
  rle_values <- rle_res$values

  # this loop generates the sequential numbering of the hospitalizations
  c <- 1
  for (x in c(1:length(rle_values))){
    if (rle_values[x] == 1){
      rle_values[x] <- c
      c <- c + 1
    }
  }

  sequences     <- rep.int( rle_values, rle_lengths )
  sequences_len <- rep.int( rle_lengths, rle_lengths )

  if(verbose == TRUE){
    print('Checking that the length of the df$days_since_admission is the same than the length(sequences)')
  }
  stopifnot(length(df$days_since_admission) == length(sequences))

  res <- data.frame(days_since_admission = df$days_since_admission,
                    n_hospitalisation = sequences,
                    len_hospitalisation = sequences_len)

  res
}







