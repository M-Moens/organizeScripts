% Generated from roxygen comments in R/organizeScripts.R
\name{finalFigure}
\alias{finalFigure}
\title{Save a final figure file in pdf, tiff and png}
\usage{
finalFigure(x,name,mywidth=89,myheight=60,mydpi=300)
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
This function saves a final figure with the targeted size in mm.
}
