% Generated from roxygen comments in R/organizeScripts.R
\name{classing}
\alias{classing}
\title{Class and Save Object}
\usage{
classing(x, dir, name, sep = ",", format = "auto")
}
\arguments{
\item{x}{The object to be saved.}

\item{dir}{The directory where the object will be saved.}

\item{name}{The name of the file (without extension).}

\item{sep}{The field separator for CSV files. Defaults to a comma.}

\item{format}{Output format: auto, csv, rds, shp, or tiff.}
}
\value{
The saved file path invisibly.
}
\description{
This function saves an object based on its class.
}
