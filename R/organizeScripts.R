#' Create a New Project Directory Structure
#'
#' This function creates a new project directory with a specified structure.
#'
#' @param dir The base directory where the project will be created.
#' @param projectName The name of the project.
#' @return A message indicating the status of the project creation.
#' @export
newProject <- function(dir, projectName) {
  if (substring(dir,nchar(dir)) == "/") {dirname = paste0(dir, projectName)} else {
  dirname = paste0(dir, "/", projectName)}
  save_path = system.file(package = "organizeScripts")
  save_pathd = paste0(save_path, "/projects")
 
  
  if (!dir.exists(save_pathd)) {
    dir.create(save_pathd)
  }
  save_pathdd = paste0(save_pathd, "/", projectName, ".csv")
  if (file.exists(save_pathdd)) {
    rr = read.csv(save_pathdd)
    if (rr$y != dirname) {
      return(message("Project name with different directory exists! Exiting function."))
    }
  }
  write.csv(data.frame(x = projectName, y = dirname), save_pathdd)
  write.csv(data.frame(x = projectName, y = dirname),  paste0(save_pathd,"/cur1234.csv"))
  
  
  if (dir.exists(dirname)) {
    message("Project exists \n")
  } else {
    dir.create(dirname)
  }
  
  dirnames = c("data", "scripts", "output", "manuscript","temp")
  recdirs = c("output_figures", "output_finalFigures","output_models", "data_final",
              "scripts_source", "scripts_final", "scripts_backup", "data_backup",
              "data_extra", "data_original")
  
  recdirs1 = data.frame(x = sapply(recdirs, FUN = function(x) strsplit(x, "_")[[1]][1]),
                        y = sapply(recdirs, FUN = function(x) strsplit(x, "_")[[1]][2]))
  
  for (i in 1:length(dirnames)) {
    dirn = paste0(dirname, "/", dirnames[i])
    if (!dir.exists(dirn)) {
      dir.create(dirn)
    }
    
    if (dirnames[i] %in% recdirs1$x) {
      for (j in recdirs1$y[which(recdirs1$x == dirnames[i])]) {
        plusdir = paste0(dirn, "/", j)
        if (!dir.exists(plusdir)) {
          dir.create(plusdir)
        } else {
          message(paste0(plusdir, " already exists \n"))
        }
      }
    } else {
      message(paste0(dirn, " already exists! \n"))
    }
  }
  
  return(message("Finished editing/creating the project."))
}

#' Set Project Directory
#'
#' This function sets the default project directory.
#'
#' @param projectName The name of the project.
#' @return None
#' @export
setProject <- function(projectName) {
  save_path = system.file(package = "organizeScripts")
  save_pathdd = paste0(save_path, "/projects/", projectName, ".csv")
  if (file.exists(save_pathdd)) {
    dirname = read.csv(save_pathdd)$y
    write.csv(data.frame(x = projectName, y = dirname),  paste0(save_path,"/projects/cur1234.csv"))
    } else {
      message("Project doesn't exist yet!")
    }
}

#' Set Project Directory
#'
#' This function gets the current directory
#'
#' @return None
#' @export
getCurrentDir = function() {
  save_path = system.file(package = "organizeScripts")
  save_pathd = paste0(save_path, "/projects")
  rr = paste0(save_pathd,"/cur1234.csv")
  rr =read.csv(rr)
  mydir = rr$y
  return(mydir)
}

#' Process a Script File
#'
#' This function processes a script file within the project directory.
#'
#' @param name The name of the script file (without extension).
#' @return None
#' @export
processScript <- function(name) {
  mydefaultdir = getCurrentDir()
  if (!file.exists(paste0(mydefaultdir, "/scripts/", name, ".R"))) {
    writeLines(paste0("## ",name, " script:\n"), paste0(mydefaultdir, "/scripts/", name, ".R"))
  }
  file.edit(paste0(mydefaultdir, "/scripts/", name, ".R"))
}

#' Import a data file
#'
#' This function opens a datafile
#'
#' @param name The name of the data file (with extension).
#' @return None
#' @export
importD <- function(name) {
  mydefaultdir = getCurrentDir()
  xx= paste0(mydefaultdir, "/data/", name)
  if (file.exists(xx)) {
    classingImport(xx)
  }
}

#' Save a data file
#'
#' This function saves a data file to the folder.
#'
#' @param x The object to be saved.
#' @param name The name of the file (without extension).
#' @return None
#' @export
saveD <- function(x,name) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir, "/data/")
  classing(x, dir = mydir, name)
}

#' Finalize a data file
#'
#' This function finalizes a data file by copying it to the final and backup directories.
#'
#' @param name The name of the data file (with extension).
#' @return None
#' @export
finalD <- function(name) {
  mydefaultdir = getCurrentDir()
  name2 = gsub("\\..*","",name)
  if (!grepl("\\.",name)) { message("Did you add the extension?")}
  
  
  name3 = paste0(".",gsub(".*\\.","",name))
  P = paste0(mydefaultdir, "/data/",name)
  if (file.exists(P)) {
  P2 = paste0(mydefaultdir, "/data/final/", name)
  P4 = gsub(":", "_", gsub("\\.", "_", gsub(" ", "_", Sys.time())))

  if (!dir.exists(paste0(mydefaultdir, "/data/backup/", name2))) {
    dir.create(paste0(mydefaultdir, "/data/backup/", name2))
  }
  name2
  
  P3 = paste0(mydefaultdir, "/data/backup/", name2, "/", name2, "_", P4, name3)
  file.copy(from = P, to = P2, overwrite = TRUE)
  if (dir.exists(gsub(paste0("/", gsub(".*\\/", "", P3)), "", P3))) {
    file.copy(from = P2, to = P3)
  } else {
    message("Directory doesn't exist\n")
  }
  } else {
    message("You need to save the data file first (saveD)")
  }
}

#' Finalize a Script File
#'
#' This function finalizes a script file by copying it to the final and backup directories.
#'
#' @param name The name of the script file (without extension).
#' @return None
#' @export
finalScript <- function(name) {
  mydefaultdir = getCurrentDir()
  P = paste0(mydefaultdir, "/scripts/", name, ".R")
  P2 = paste0(mydefaultdir, "/scripts/final/", name, ".R")
  P4 = gsub(":", "_", gsub("\\.", "_", gsub(" ", "_", Sys.time())))
  if (!dir.exists(paste0(mydefaultdir, "/scripts/backup/", name))) {
    dir.create(paste0(mydefaultdir, "/scripts/backup/", name))
  }
  
  P3 = paste0(mydefaultdir, "/scripts/backup/", name, "/", name, "_", P4, ".R")
  file.copy(from = P, to = P2, overwrite = TRUE)
  if (dir.exists(gsub(paste0("/", gsub(".*\\/", "", P3)), "", P3))) {
    file.copy(from = P2, to = P3)
  } else {
    message("Directory doesn't exist\n")
  }
}

#' Class and Save Object
#'
#' This function saves an object based on its class.
#'
#' @param x The object to be saved.
#' @param dir The directory where the object will be saved.
#' @param name The name of the file (without extension).
#' @return None
#' @export
classing <- function(x, dir, name) {
  if (class(x) == "sp") {
    st_write(x, paste0(dir, name, ".shp"),delete_dsn = TRUE)
  } else if (class(x) == "data.frame") {
    write.csv(x, paste0(dir, name, ".csv"), row.names = FALSE)
  } else if (class(x) == "raster") {
    writeRaster(x, paste0(dir, name, ".tiff"),overwrite = T)
    if (nlayers(x) > 1) {
      write.csv(data.frame(x = names(raster)), paste0(dir, name, ".csv"))
    }
  }
}

#' Class and import Object
#'
#' This function imports an object based on its class.
#'
#' @param dir The directory where the object is saved.
#' @param name The name of the file (with extension).
#' @return None
#' @export
classingImport <- function(dir) {
  name2 = gsub(".*\\.","",dir)
  l=NA
  
  if (name2 == "shp") {
    l= st_read(dir)
  } else if (name2 == "csv") {
    l = read.csv(dir)
  } else if (name2 %in%  c("tiff","tif","grd")) {
    l = stack(dir)
  }
  
  return(l)
}

#' Save or import temporary files
#'
#' This function saves an object to the temporary data directory or 
#' imports a temporary file into the global environment.
#' 
#'
#' @param x The object to be saved. Ignore for importing data.
#' @param name Saving: the name of the file (without extension) and
#' importing: the name of the file (with extension)
#' @return None
#' @export
tempF <- function(x=NULL, name) {
  if (!is.null(x)) {
  mydefaultdir = getCurrentDir()
  tempfo = paste0(mydefaultdir, "/temp/")
  classing(x, dir = tempfo, name)} else {
    mydefaultdir = getCurrentDir()
    xx= paste0(mydefaultdir, "/temp/", name)
    if (file.exists(xx)) {
      classingImport(xx)
    }
  }
}

#' Save Final Object
#'
#' This function saves an object to a specified directory within the project.
#'
#' @param x The object to be saved.
#' @param dir The subdirectory within the project where the object will be saved.
#' @param name The name of the file (without extension).
#' @return None
#' @export
saveF <- function(x, dir, name) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir, "/", dir, "/")
  classing(x, mydir, name)
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
  if (file.exists(mydir)) {source(mydir)} else {
    message("Source script file doesn't exist.")
  }
  
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
  if (file.exists(mydir)) {file.edit(mydir)} else {
    writeLines(paste0("## ",name, " source script:\n",
                      "library(organizeScripts)\n"), mydir)
    file.edit(mydir)
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
#' @return None
#' @export
saveFigure <- function(x,name,mywidth,myheight,mydpi=300) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir,"/output/figures/",name)
  ggsave(paste0(mydir,".pdf"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "pdf")
  ggsave(paste0(mydir,".png"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "png")
  ggsave(paste0(mydir,".tiff"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "tiff")
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
#' @return None
#' @export
finalFigure <- function(x,name,mywidth=89,myheight=60,mydpi=300) {
  mydefaultdir = getCurrentDir()
  mydir = paste0(mydefaultdir,"/output/finalFigures/",name)
  ggsave(paste0(mydir,".pdf"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "pdf")
  ggsave(paste0(mydir,".png"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "png")
  ggsave(paste0(mydir,".tiff"), plot = x, width = mywidth, height = myheight, units = "mm",dpi=mydpi,device = "tiff")
}

#' Set or get common R library
#'
#' This function sets the common R library or gets it when it already exists.
#'
#' @param dir The directory for the common library.
#' @param overwrite Do you want to overwrite the current directory?
#' @return None
#' @export
setCommonLib = function(dir ="",overwrite = F) {
  save_path = system.file(package = "organizeScripts")
  myfile = paste0(save_path,"/commonLibPath.csv")
  p =1
  if (file.exists(myfile)) {
    myp = read.csv(myfile)$commonlibpath
    message(paste0("Common library path already exists in :\n",myp))
    return(myp)
    p=0
  } 
  if (overwrite) { p=1}
  if (p == 1) { 
    if (substring(dir,nchar(dir)) == "/") {dir = substr(dir, 1, nchar(dir) - 1)} 
    write.csv(data.frame(commonlibpath = dir),myfile)
    dir2 = paste0(dir,"/commonLibraryR") 
    if (!dir.exists(dir2)) dir.create(dir2)
    mydir = paste0(dir2,"/final")
    mydir2 = paste0(dir2,"/backup")
    if (!dir.exists(mydir)) dir.create(mydir)
    if (!dir.exists(mydir)) dir.create(mydir)
    message("Common R library changed.")
    return(dir)
  }
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
  file.edit(liB)
}

#' Finalize a library script
#'
#' This function finalizes a library script by copying it to the final and backup directories.
#'
#' @param name The name of the library script file (without extension).
#' @return None
#' @export
finalLib <- function(name) {
  mydefaultdir = paste0(setCommonLib(),"/commonLibraryR")
  P = paste0(mydefaultdir, "/", name, ".R")
  P2 = paste0(mydefaultdir, "/final/", name, ".R")
  P4 = gsub(":", "_", gsub("\\.", "_", gsub(" ", "_", Sys.time())))
  if (!dir.exists(paste0(mydefaultdir, "/backup"))) {
    dir.create(paste0(mydefaultdir, "/backup"))
  }
  if (!dir.exists(paste0(mydefaultdir, "/backup/",name))) {
    dir.create(paste0(mydefaultdir, "/backup/",name))
  }
  
  P3 = paste0(mydefaultdir, "/backup/", name, "/", name, "_", P4, ".R")
  file.copy(from = P, to = P2, overwrite = TRUE)
  if (dir.exists(gsub(paste0("/", gsub(".*\\/", "", P3)), "", P3))) {
    file.copy(from = P2, to = P3)
  } else {
    message("Directory doesn't exist\n")
  }
}

