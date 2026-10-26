#' Create a New Project Directory Structure
#'
#' This function creates a new project directory with a specified structure.
#'
#' @param dir The base directory where the project will be created.
#' @param projectName The name of the project.
#' @return The project directory invisibly.
#' @export
newProject <- function(dir, projectName) {
  .check_name(projectName)
  root <- file.path(normalizePath(dir, winslash = "/", mustWork = FALSE), projectName)
  record <- file.path(.settings_dir(), "projects", paste0(projectName, ".csv"))
  if (file.exists(record) && .read_path(record, "y") != root)
    stop("Project name is already registered to another directory: ", projectName)
  folders <- c("data", "scripts", "output", "manuscript", "temp",
    "output/figures", "output/finalFigures", "output/models", "data/final",
    "scripts/source", "scripts/final", "scripts/backup", "data/backup",
    "data/extra", "data/original")
  project_folders <- c(root, file.path(root, folders))
  added <- sum(!dir.exists(project_folders))
  .ensure_dir(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  for (folder in folders) .ensure_dir(file.path(root, folder))
  utils::write.csv(data.frame(x = projectName, y = root), record, row.names = FALSE)
  setProject(projectName)
  if (added > 0L) {
    message("Project '", projectName, "': created ", added, " new folder",
            if (added == 1L) "." else "s.")
  } else {
    message("Project '", projectName, "': all folders already exist; no new folders added.")
  }
  invisible(root)
}

#' Set Project Directory
#'
#' This function sets the default project directory.
#'
#' @param projectName The name of the project.
#' @return The active project directory invisibly.
#' @export
setProject <- function(projectName) {
  .check_name(projectName)
  settings <- .settings_dir()
  record <- file.path(settings, "projects", paste0(projectName, ".csv"))
  path <- .read_path(record, "y")
  if (!dir.exists(path)) stop("Project directory does not exist: ", path)
  .copy_file(record, file.path(settings, "projects", "cur1234.csv"))
  invisible(path)
}

#' Set Project Directory
#'
#' This function gets the current directory
#'
#' @return The active project directory.
#' @export
getCurrentDir <- function() {
  record <- file.path(.settings_dir(), "projects", "cur1234.csv")
  if (!file.exists(record)) stop("No active project. Use newProject() or setProject() first.")
  path <- .read_path(record, "y")
  if (!dir.exists(path)) stop("Project directory does not exist: ", path)
  path
}

#' Process a Script File
#'
#' This function processes a script file within the project directory.
#' Opens the file in RStudio when its API is available, otherwise in the
#' configured R editor.
#'
#' @param name The name of the script file (without extension).
#' @return None
#' @export
processScript <- function(name) {
  mydefaultdir = getCurrentDir()
  if (!file.exists(paste0(mydefaultdir, "/scripts/", name, ".R"))) {
    writeLines(paste0("## ",name, " script:\n"), paste0(mydefaultdir, "/scripts/", name, ".R"))
  }
  path <- file.path(mydefaultdir, "scripts", paste0(name, ".R"))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(path)
  } else {
    utils::file.edit(path)
  }
}

#' Import a data file
#'
#' This function opens a datafile
#'
#' @param name The name of the data file (with extension).
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @return The imported object.
#' @export
importD <- function(name, sep = ",") {
  classingImport(file.path(getCurrentDir(), "data", name), sep = sep)
}

#' Save a data file
#'
#' This function saves a data file to the folder.
#'
#' @param x The object to be saved.
#' @param name The name of the file (without extension).
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @return The saved file path invisibly.
#' @export
#' @param format Output format: auto, csv, rds, shp, or tiff.
saveD <- function(x, name, sep = ",", format = "auto") {
  classing(x, file.path(getCurrentDir(), "data"), name, sep = sep, format = format)
}

#' Finalize a data file
#'
#' This function finalizes a data file by copying it to the final and backup directories.
#'
#' @param name The name of the data file (with extension).
#' @return The final and backup paths invisibly.
#' @export
finalD <- function(name) {
  root <- file.path(getCurrentDir(), "data")
  .finalize(file.path(root, name), root)
}

#' Finalize a Script File
#'
#' This function finalizes a script file by copying it to the final and backup directories.
#'
#' @param name The name of the script file (without extension).
#' @return The final and backup paths invisibly.
#' @export
finalScript <- function(name) {
  root <- file.path(getCurrentDir(), "scripts")
  .finalize(file.path(root, paste0(name, ".R")), root)
}

#' Class and Save Object
#'
#' This function saves an object based on its class.
#'
#' @param x The object to be saved.
#' @param dir The directory where the object will be saved.
#' @param name The name of the file (without extension).
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @return The saved file path invisibly.
#' @export
#' @param format Output format: auto, csv, rds, shp, or tiff.
classing <- function(x, dir, name, sep = ",", format = "auto") {
  format <- match.arg(format, c("auto", "csv", "rds", "shp", "tiff"))
  spatial <- inherits(x, "sf") || inherits(x, "Spatial")
  raster <- inherits(x, c("RasterLayer", "RasterStack", "RasterBrick"))
  if (format == "auto") {
    format <- if (spatial) "shp" else if (inherits(x, "data.frame")) "csv" else if (raster) "tiff" else
      stop("Unsupported object type. Use format = 'rds' to save any R object.")
  }
  path <- file.path(dir, paste0(name, ".", format))
  .ensure_dir(dirname(path))
  if (format == "rds") {
    saveRDS(x, path)
  } else if (format == "csv") {
    if (!inherits(x, "data.frame")) stop("CSV output requires a data frame.")
    utils::write.table(x, path, sep = sep, row.names = FALSE,
                       col.names = TRUE, qmethod = "double")
  } else if (format == "shp") {
    if (!spatial) stop("Shapefile output requires an sf or Spatial object.")
    .need_package("sf")
    sf::st_write(sf::st_as_sf(x), path, delete_dsn = TRUE, quiet = TRUE)
  } else {
    if (!raster) stop("TIFF output requires a RasterLayer, RasterStack, or RasterBrick.")
    .need_package("raster")
    raster::writeRaster(x, path, overwrite = TRUE)
    if (raster::nlayers(x) > 1)
      utils::write.table(data.frame(x = names(x)), file.path(dir, paste0(name, ".csv")),
                         sep = sep, row.names = FALSE, qmethod = "double")
  }
  invisible(path)
}

#' Class and import Object
#'
#' This function imports an object based on its class.
#'
#' @param dir The directory where the object is saved.
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @return The imported object.
#' @export
classingImport <- function(dir, sep = ",") {
  .require_file(dir)
  extension <- tolower(tools::file_ext(dir))
  if (extension == "csv") return(utils::read.csv(dir, sep = sep))
  if (extension == "rds") return(readRDS(dir))
  if (extension == "shp") {
    .need_package("sf")
    return(sf::st_read(dir, quiet = TRUE))
  }
  if (extension %in% c("tiff", "tif", "grd")) {
    .need_package("raster")
    return(raster::stack(dir))
  }
  stop("Unsupported file extension: ", extension)
}

#' Save or import temporary files
#'
#' This function saves an object to the temporary data directory or 
#' returns an imported temporary object.
#' 
#'
#' @param x The object to be saved. Ignore for importing data.
#' @param name Saving: the name of the file (without extension) and
#' importing: the name of the file (with extension)
#' @return The imported object, or the saved file path invisibly.
#' @export
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @param format Output format: auto, csv, rds, shp, or tiff.
tempF <- function(x = NULL, name, sep = ",", format = "auto") {
  dir <- file.path(getCurrentDir(), "temp")
  if (missing(x) || (is.null(x) && format != "rds"))
    return(classingImport(file.path(dir, name), sep = sep))
  classing(x, dir, name, sep = sep, format = format)
}

#' Save Final Object
#'
#' This function saves an object to a specified directory within the project.
#'
#' @param x The object to be saved.
#' @param dir The subdirectory within the project where the object will be saved.
#' @param name The name of the file (without extension).
#' @return The saved file path invisibly.
#' @export
#' @param sep The field separator for CSV files. Defaults to a comma.
#' @param format Output format: auto, csv, rds, shp, or tiff.
saveF <- function(x, dir, name, sep = ",", format = "auto") {
  classing(x, file.path(getCurrentDir(), dir), name, sep = sep, format = format)
}

#' Run the source script
#'
#' This function runs the script name in the source directory.
#'
#' @param name The name of the source script (without extension).
#' @return None
#' @export
runSource <- function(name) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir, "/scripts/source/",name,".R")
  .require_file(mydir)
  source(mydir)
  
}

#' Create/edit a source script in the source directory
#'
#' This function creates or edits a source script in the source directory.
#'
#' @param name The name of the source script (without extension).
#' @return None
#' @export
saveSource <- function(name) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir, "/scripts/source/",name,".R")
  if (file.exists(mydir)) {rstudioapi::navigateToFile(mydir)} else {
    writeLines(paste0("## ",name, " source script:\n",
                      "library(organizeScripts)\n"), mydir)
    rstudioapi::navigateToFile(mydir)
  }
}

#' Save a figure file in pdf, tiff and png
#'
#' This function saves a figure with the targeted size in mm.
#'
#' @param x The gg object to be saved.
#' @param name The name of the object to save
#' @param mywidth The width of the image to be saved.
#' @param myheight The height of the image to be saved.
#' @param mydpi The dpi of the image to be saved.
#' @return The saved figure paths invisibly.
#' @export
saveFigure <- function(x,name,mywidth,myheight,mydpi=300) {
  .need_package("ggplot2")
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir,"/output/figures/",name)
  .ensure_dir(dirname(mydir))
  ggplot2::ggsave(paste0(mydir,".pdf"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "pdf")
  ggplot2::ggsave(paste0(mydir,".png"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "png")
  ggplot2::ggsave(paste0(mydir,".tiff"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "tiff")
  invisible(paste0(mydir, c(".pdf", ".png", ".tiff")))
}

#' Save a final figure file in pdf, tiff and png
#'
#' This function saves a final figure with the targeted size in mm.
#'
#' @param x The gg object to be saved.
#' @param name The name of the object to save
#' @param mywidth The width of the image to be saved.
#' @param myheight The height of the image to be saved.
#' @param mydpi The dpi of the image to be saved.
#' @return The saved figure paths invisibly.
#' @export
finalFigure <- function(x,name,mywidth=89,myheight=60,mydpi=300) {
  .need_package("ggplot2")
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir,"/output/finalFigures/",name)
  .ensure_dir(dirname(mydir))
  ggplot2::ggsave(paste0(mydir,".pdf"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "pdf")
  ggplot2::ggsave(paste0(mydir,".png"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "png")
  ggplot2::ggsave(paste0(mydir,".tiff"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "tiff")
  invisible(paste0(mydir, c(".pdf", ".png", ".tiff")))
}

#' Set or get common R library
#'
#' This function sets the common R library or gets it when it already exists.
#'
#' @param dir The directory for the common library.
#' @param overwrite Do you want to overwrite the current directory?
#' @return The common library base directory.
#' @export
setCommonLib <- function(dir = "", overwrite = FALSE) {
  record <- file.path(.settings_dir(), "commonLibPath.csv")
  if (file.exists(record) && !overwrite) {
    dir <- .read_path(record, "commonlibpath")
  } else {
    if (length(dir) != 1L || is.na(dir) || !nzchar(dir))
      stop("Supply a directory to configure the common library.")
    .ensure_dir(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  }
  for (folder in c("", "final", "backup"))
    .ensure_dir(file.path(dir, "commonLibraryR", folder))
  utils::write.csv(data.frame(commonlibpath = dir), record, row.names = FALSE)
  dir
}

#' Process a common library script file.
#'
#' This function processes a common library script file.
#'
#' @param name The name of the library script file (without extension).
#' @return None
#' @export
processLib <- function(name) {
  mydefaultdir = paste0(setCommonLib(),"/commonLibraryR")
  liB = paste0(mydefaultdir, "/", name, ".R")
  if (!file.exists(liB)) {
    writeLines(paste0("## ",name, " library script:\n"), liB)
  }
  rstudioapi::navigateToFile(liB)
}

#' Finalize a library script
#'
#' This function finalizes a library script by copying it to the final and backup directories.
#'
#' @param name The name of the library script file (without extension).
#' @return The final and backup paths invisibly.
#' @export
finalLib <- function(name) {
  root <- file.path(setCommonLib(), "commonLibraryR")
  .finalize(file.path(root, paste0(name, ".R")), root)
}

