#' Function to estimate the sequences of hospitalization in Phase 2.2 data
#'
#' Given the \code{df} LocalPatientClinicalCourse data use the in_hospital column
#' that can get the values 1 when the patient is hospitalized, and 0 when the patient
#' is not hospitalized and generates two new columns, the \code{n_hospitalisation} one
#' that represents the number of distinct hospitalizations, and the \code{len_hospitalisation}
#' with the actual number of consecutive days the patient spent at the hospital.
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


count_sequences_hospitalisation <- function(df, ...) {

  if(verbose == TRUE){
    print('Creating the sequence data')
  }

  seq_hospitalisation_df <- data.frame(total_span = seq(min(df$days_since_admission),
                                                        max(df$days_since_admission))) %>%
    left_join(df, by = c("total_span" = "days_since_admission")) %>%
    replace_na(list(in_hospital = 0))

  count_sequences   <- rle( seq_hospitalisation_df$in_hospital )
  count_sequences_1 <- lapply( count_sequences, function(x) x[ count_sequences$values == 1 ] )
  n_sequences       <- seq_along( count_sequences_1$lengths )

  if(verbose == TRUE){
    print('Estimating the length of hospitalization')
  }

  sequences     <- rep.int( n_sequences, count_sequences_1$lengths )
  sequences_len <- rep.int( count_sequences_1$lengths, count_sequences_1$lengths )

  if(verbose == TRUE){
    print('Checking that the length of the df$days_since_admission is the same than the length(sequences)')
  }
  stopifnot(length(df$days_since_admission) == length(sequences))

  data.frame(days_since_admission = df$days_since_admission,
             n_hospitalisation = sequences,
             len_hospitalisation = sequences_len)
}
