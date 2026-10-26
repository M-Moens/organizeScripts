if (file.exists(file.path("R", "organizeScripts.R"))) {
  source(file.path("R", "helpers.R"))
  source(file.path("R", "organizeScripts.R"))
} else {
  library(organizeScripts)
  .migrate_settings <- getFromNamespace(".migrate_settings", "organizeScripts")
}

local({
  sandbox <- tempfile("organizeScripts-tests-")
  dir.create(sandbox)
  old <- options(organizeScripts.config_dir = file.path(sandbox, "settings"))
  dir.create(file.path(sandbox, "settings"))
  file.create(file.path(sandbox, "settings", ".migrated"))
  on.exit(options(old))
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  fails <- function(expr, pattern) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
  }
  fails(getCurrentDir(), "No active project")
  first <- newProject(sandbox, "first")
  second <- newProject(sandbox, "second")
  stopifnot(identical(getCurrentDir(), second))
  setProject("first")
  stopifnot(identical(getCurrentDir(), first))
  stopifnot(dir.exists(file.path(first, "scripts", "backup")))
  fails(setProject("missing"), "File does not exist")
  fails(newProject(sandbox, "../bad"), "project name")
  fails(newProject(sandbox, "cur1234"), "reserved")
  fails(newProject(file.path(sandbox, "elsewhere"), "first"), "already registered")

  data <- data.frame(text = c("a;b", 'say "hello"', "a,b"),
                     value = c(1.5, NA, 3))
  for (separator in c(",", ";", "\t", "|")) {
    saved <- withVisible(saveD(data, "sample", sep = separator))
    stopifnot(!saved$visible, file.exists(saved$value))
    stopifnot(identical(importD("sample.csv", sep = separator), data))
    path <- saveF(data, "output/models", "sample", sep = separator)
    stopifnot(identical(classingImport(path, sep = separator), data))
    tempF(data, "sample", sep = separator)
    stopifnot(identical(tempF(name = "sample.csv", sep = separator), data))
  }
  saveD(data, "default")
  utils::write.csv(data, file.path(sandbox, "expected.csv"), row.names = FALSE)
  stopifnot(identical(readLines(file.path(first, "data/default.csv")),
                      readLines(file.path(sandbox, "expected.csv"))))
  subclass <- data
  class(subclass) <- c("custom_data", "data.frame")
  saveD(subclass, "subclass")
  stopifnot(identical(importD("subclass.csv"), data))
  fails(saveD(list(a = 1), "unsupported"), "Unsupported object type")
  fails(importD("missing.csv"), "File does not exist")
  writeLines("x", file.path(first, "data", "unknown.xyz"))
  fails(importD("unknown.xyz"), "Unsupported file extension")
  object <- list(data = data, model = stats::lm(mpg ~ wt, data = mtcars))
  saveD(object, "object", format = "rds")
  restored <- importD("object.rds")
  stopifnot(identical(restored$data, object$data),
            identical(stats::coef(restored$model), stats::coef(object$model)))
  tempF(NULL, "null", format = "rds")
  stopifnot(is.null(tempF(name = "null.rds")))
  path <- saveF(object, "output/models", "object", format = "rds")
  stopifnot(identical(readRDS(path)$data, object$data))

  saveD(data, "version.1")
  a <- finalD("version.1.csv")
  data$value[1] <- 9
  saveD(data, "version.1")
  b <- finalD("version.1.csv")
  stopifnot(a[["backup"]] != b[["backup"]], all(file.exists(c(a, b))))
  stopifnot(utils::read.csv(a[["backup"]])$value[1] == 1.5,
            utils::read.csv(b[["final"]])$value[1] == 9)
  fails(finalD("missing.csv"), "File does not exist")
  writeLines("x <- 1", file.path(first, "scripts", "analysis.R"))
  stopifnot(all(file.exists(finalScript("analysis"))))
  fails(runSource("missing"), "File does not exist")

  common <- setCommonLib(file.path(sandbox, "common"))
  stopifnot(dir.exists(file.path(common, "commonLibraryR", "backup")))
  stopifnot(identical(setCommonLib("ignored"), common))
  replacement <- setCommonLib(file.path(sandbox, "replacement"), overwrite = TRUE)
  stopifnot(identical(setCommonLib(), replacement), replacement != common)
  writeLines("x <- 2", file.path(replacement, "commonLibraryR", "shared.R"))
  stopifnot(all(file.exists(finalLib("shared"))))

  legacy <- file.path(sandbox, "legacy")
  target <- file.path(sandbox, "migrated")
  dir.create(file.path(legacy, "projects"), recursive = TRUE)
  utils::write.csv(data.frame(x = "old", y = first),
                   file.path(legacy, "projects", "old.csv"), row.names = FALSE)
  utils::write.csv(data.frame(commonlibpath = common),
                   file.path(legacy, "commonLibPath.csv"), row.names = FALSE)
  .migrate_settings(legacy, target)
  stopifnot(file.exists(file.path(target, "projects", "old.csv")))
  writeLines("keep", file.path(target, "commonLibPath.csv"))
  .migrate_settings(legacy, target)
  stopifnot(identical(readLines(file.path(target, "commonLibPath.csv")), "keep"))
  newProject(sandbox, "first")
  stopifnot(file.exists(file.path(first, "data", "default.csv")))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
      ggplot2::geom_point()
    paths <- saveFigure(plot, "test", 89, 60)
    stopifnot(length(paths) == 3L, all(file.exists(paths)))
    stopifnot(all(file.exists(finalFigure(plot, "test"))))
  }
})
cat("All regression checks passed.\n")
