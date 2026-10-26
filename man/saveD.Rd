% Generated from roxygen comments in R/organizeScripts.R
\name{saveD}
\alias{saveD}
\title{Save a data file}
\usage{
saveD(x, name, sep = ",", format = "auto")
}
\arguments{
\item{x}{The object to be saved.}

\item{name}{The name of the file (without extension).}

\item{sep}{The field separator for CSV files. Defaults to a comma.}

\item{format}{Output format: auto, csv, rds, shp, or tiff.}
}
\value{
The saved file path invisibly.
}
\description{
This function saves a data file to the folder.
}
