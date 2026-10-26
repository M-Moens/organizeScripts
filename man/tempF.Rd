% Generated from roxygen comments in R/organizeScripts.R
\name{tempF}
\alias{tempF}
\title{Save or import temporary files}
\usage{
tempF(x = NULL, name, sep = ",", format = "auto")
}
\arguments{
\item{x}{The object to be saved. Ignore for importing data.}

\item{name}{Saving: the name of the file (without extension) and importing: the name of the file (with extension)}

\item{sep}{The field separator for CSV files. Defaults to a comma.}

\item{format}{Output format: auto, csv, rds, shp, or tiff.}
}
\value{
The imported object, or the saved file path invisibly.
}
\description{
This function saves an object to the temporary data directory or 
returns an imported temporary object.
}
