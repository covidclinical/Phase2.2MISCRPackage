% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_cases_perTimePeriod.R
\name{misc_cases_perTimePeriod}
\alias{misc_cases_perTimePeriod}
\title{Function to estimate number of MISC patients by period}
\usage{
misc_cases_perTimePeriod(
  integrated_df,
  period = "month",
  output_plot = FALSE,
  output_df = TRUE,
  dir.output,
  verbose
)
}
\arguments{
\item{integrated_df}{The data.frame generated by the \code{allFilesInOne} function.}

\item{period}{The counts can be done by weeks, months or year. By default it is done by month.}

\item{output_plot}{By default \code{FALSE}. Change it to \code{TRUE} to get a barplot output.}

\item{output_df}{By default \code{TRUE}. Change it to \code{FALSE} to not to get the counts as a data.frame.}

\item{dir.output}{The path to the output directory where the aggregate counts and plots will be saved.}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.}
}
\value{
An object of class \code{data.frame} with the counts, and the plot when output_plot is set to TRUE.
}
\description{
Given the \code{data.frame} output generated by the \code{allFilesInOne} function
and given a specific period, generates as output a \code{data.frame} with MISC
cases.
}
\examples{

casesCounts <- misc_cases_perTimePeriod(
              integrated_df = misc_complete,
              period = "month",
              output_plot = FALSE,
              output_df   = TRUE,
              dir.output
             )
}
