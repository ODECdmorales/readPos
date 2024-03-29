% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readPos.R
\name{scanSTA}
\alias{scanSTA}
\title{Function to scan a STA definition file and create a map with the same parameters for each variable}
\usage{
scanSTA(stafile, map)
}
\arguments{
\item{stafile}{Path of the STA definition file taht will be scanned and being the base to generate de map.csv}

\item{map}{Name of the CSV that will be generated}
}
\value{
A csv with many rows as variable defined in STA file.
}
\description{
Function to scan a STA definition file and create a map with the same parameters for each variable
}
\examples{
scanSTA(STA = "C:/Desktop/definition.sta", map = "output.csv")

}
