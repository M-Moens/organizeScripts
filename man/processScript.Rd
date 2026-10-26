% Generated from roxygen comments in R/organizeScripts.R
\name{processScript}
\alias{processScript}
\title{Process a Script File}
\usage{
processScript(name)
}
\arguments{
\item{name}{The name of the script file (without extension).}
}
\value{
None
}
\description{
This function processes a script file within the project directory.
Opens the file in RStudio when its API is available, otherwise in the
configured R editor.
}
