% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_overview.R
\name{misc_overview}
\alias{misc_overview}
\title{Function to get an overview of the MISC patients characteristics
(summary of age and sex ratio, summary of ICU cases and length of
hospitalization during the MISC admission)}
\usage{
misc_overview(
  integrated_df,
  obfuscation_threshold,
  dir.output,
  output_plot = FALSE,
  output_df = TRUE,
  cbPalette = cbPalette,
  verbose
)
}
\arguments{
\item{integrated_df}{The data.frame generated by the \code{allFilesInOne} function.}

\item{obfuscation_threshold}{FALSE if no obfuscation is needed, the numeric value when there is obfuscation}

\item{output_plot}{By default \code{FALSE}. Change it to \code{TRUE} to get a barplot output.}

\item{output_df}{By default \code{TRUE}. Change it to \code{FALSE} to not to get the counts as a data.frame.}

\item{cbPalette}{Color palette to use for the plots (set up by default)}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.}
}
\value{
Multiple objects of class \code{data.frame} with the counts, and the plots when output_plot is set to TRUE.
}
\description{
Given the \code{data.frame} output generated by the \code{allFilesInOne} function
generates as output multiple \code{data.frame} and plots with the number of MISC
cases by wave and: age and sex ratio per wave, ICU cases and length of hospitalization
}
\examples{

casesCounts <- misc_overview(
              integrated_df = misc_complete,
              output_plot = FALSE,
              output_df   = TRUE
             )
}
