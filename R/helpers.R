.ensure_dir <- function(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE))
    stop("Could not create directory: ", path)
  invisible(path)
}

.require_file <- function(path) {
  if (!file.exists(path) || dir.exists(path)) stop("File does not exist: ", path)
}

.copy_file <- function(from, to, overwrite = TRUE) {
  .require_file(from)
  .ensure_dir(dirname(to))
  if (!file.copy(from, to, overwrite = overwrite))
    stop("Could not copy ", from, " to ", to)
  invisible(to)
}

.check_name <- function(name) {
  if (length(name) != 1L || is.na(name) || !nzchar(name) ||
      name %in% c(".", "..", "cur1234") || grepl("[/\\\\]", name))
    stop("Supply a nonempty project name without path separators; cur1234 is reserved.")
}

.read_path <- function(record, column) {
  .require_file(record)
  value <- utils::read.csv(record, stringsAsFactors = FALSE)[[column]]
  if (length(value) != 1L || is.na(value) || !nzchar(value))
    stop("Invalid settings file: ", record)
  value
}

.settings_dir <- function() {
  path <- getOption("organizeScripts.config_dir",
                    tools::R_user_dir("organizeScripts", "config"))
  .ensure_dir(path)
  .ensure_dir(file.path(path, "projects"))
  marker <- file.path(path, ".migrated")
  if (!file.exists(marker)) {
    .migrate_settings(system.file(package = "organizeScripts"), path)
    if (!file.create(marker)) stop("Could not record settings migration: ", marker)
  }
  path
}

# Copy legacy settings without replacing settings already configured by the user.
.migrate_settings <- function(legacy, target) {
  if (!nzchar(legacy) || !dir.exists(legacy)) return(invisible(NULL))
  .ensure_dir(file.path(target, "projects"))
  records <- list.files(file.path(legacy, "projects"), pattern = "[.]csv$",
                        full.names = TRUE)
  for (record in records) {
    destination <- file.path(target, "projects", basename(record))
    if (!file.exists(destination)) .copy_file(record, destination, overwrite = FALSE)
  }
  record <- file.path(legacy, "commonLibPath.csv")
  destination <- file.path(target, "commonLibPath.csv")
  if (file.exists(record) && !file.exists(destination))
    .copy_file(record, destination, overwrite = FALSE)
  invisible(target)
}

.need_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    stop("This operation requires package '", package,
         "'. Install it with install.packages('", package, "').")
}

.finalize <- function(source, root) {
  .require_file(source)
  name <- basename(source)
  stem <- tools::file_path_sans_ext(name)
  extension <- tools::file_ext(name)
  backup_dir <- file.path(root, "backup", stem)
  .ensure_dir(backup_dir)
  backup <- tempfile(paste0(stem, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"),
                     tmpdir = backup_dir,
                     fileext = if (nzchar(extension)) paste0(".", extension) else "")
  final <- file.path(root, "final", name)
  .copy_file(source, backup, overwrite = FALSE)
  .copy_file(source, final)
  invisible(c(final = final, backup = backup))
}
