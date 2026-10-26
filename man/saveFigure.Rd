% Generated from roxygen comments in R/organizeScripts.R
\name{saveFigure}
\alias{saveFigure}
\title{Save a figure file in pdf, tiff and png}
\usage{
saveFigure(x,name,mywidth,myheight,mydpi=300)
}
\arguments{
\item{x}{The gg object to be saved.}

\item{name}{The name of the object to save}

\item{mywidth}{The width of the image to be saved.}

\item{myheight}{The height of the image to be saved.}

\item{mydpi}{The dpi of the image to be saved.}
}
\value{
The saved figure paths invisibly.
}
\description{
This function saves a figure with the targeted size in mm.
}
