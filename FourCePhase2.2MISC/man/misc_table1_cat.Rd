% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_table1_cat.R
\name{misc_table1_cat}
\alias{misc_table1_cat}
\title{Function to get table 1 categorical variables
Given the \code{data.frame} output generated by the \code{allFilesInOne} function
generates as output the table 1 for the manuscript for categorical variables}
\usage{
misc_table1_cat(
  complete_df,
  obfuscation_threshold,
  currSiteId,
  dir.input,
  dir.output,
  raceAvailable,
  verbose
)
}
\arguments{
\item{complete_df}{The data.frame generated by the \code{allFilesInOne} function.}

\item{obfuscation_threshold}{FALSE if no obfuscation is needed, the numeric value when there is obfuscation}

\item{currSiteId}{Site ID}

\item{dir.input}{The path where the 2.2 MISC cohort files are located.}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.}
}
\value{
Table 1 categorical variables for the manuscript.
}
\description{
Function to get table 1 categorical variables
Given the \code{data.frame} output generated by the \code{allFilesInOne} function
generates as output the table 1 for the manuscript for categorical variables
}
\examples{

t1_cat <- misc_table1_cat(
              complete_df,
              currSiteId,
              obfuscation_threshold,
              verbose
             )
}
