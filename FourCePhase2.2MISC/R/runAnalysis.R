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
#' @param washout_opt Specify the option for the washout. Values: remove or replace_earlier or replace_late
#' @param cbPalette Color palette to use for the plots (set up by default)
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{list} with the \code{data.frames}.
#' @examples
#'
#' FourCePhase2.2MISC::runAnalysis()
#' @export runAnalysis


runAnalysis <- function( dir.input,
                         dir.output,
                         obfuscation,
                         raceAvailable,
                         dateFormat,
                         data_update_date,
                         country,
                         cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                         verbose ) {

  if(verbose == TRUE){ print("Creating the main output folder if it doesn't exist")}
  if (! dir.output %in% list.dirs()) {dir.create(dir.output)}

  # original analysis
  original_outdir <- paste0(dir.output, '/original')
  runAnalysis_internal( dir.input = dir.input, dir.output = original_outdir,
               obfuscation = obfuscation, raceAvailable= raceAvailable, country = country,
               verbose = verbose, dateFormat = dateFormat, data_update_date = data_update_date, cbPalette = cbPalette,
               washout_days = 0, washout_opt = "none")

  # remove washout
  remove_outdir <- paste0(dir.output, '/remove')
  runAnalysis_internal( dir.input = dir.input, dir.output = remove_outdir,
               obfuscation = obfuscation, raceAvailable= raceAvailable, country = country,
               verbose = verbose, dateFormat = dateFormat, data_update_date = data_update_date, cbPalette = cbPalette,
               washout_days = 14, washout_opt = "remove")

  # washout 'replace_earlier'
  replace_earlier_outdir <- paste0(dir.output, '/replace_earlier')
  runAnalysis_internal( dir.input = dir.input, dir.output = replace_earlier_outdir,
               obfuscation = obfuscation, raceAvailable= raceAvailable, country = country,
               verbose = verbose, dateFormat = dateFormat, data_update_date = data_update_date, cbPalette = cbPalette,
               washout_days = 14, washout_opt = "replace_earlier")

  # washout 'replace_late'
  replace_late_outdir <- paste0(dir.output, '/replace_late')
  runAnalysis_internal( dir.input = dir.input, dir.output = replace_late_outdir,
               obfuscation = obfuscation, raceAvailable= raceAvailable, country = country,
               verbose = verbose, dateFormat = dateFormat, data_update_date = data_update_date, cbPalette = cbPalette,
               washout_days = 14, washout_opt = "replace_late")
}