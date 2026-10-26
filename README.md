# organizeScripts

Create a consistent folder structure for research projects, save data and figures,
and keep final copies with separate backups.

```r
devtools::install_github("M-Moens/organizeScripts")
library(organizeScripts)

newProject(tempdir(), "example")
saveD(iris, "iris", sep = ";")
data <- importD("iris.csv", sep = ";")
finalD("iris.csv")

saveF(iris, "output/models", "iris", sep = ";")
tempF(iris, name = "working", sep = ";")
working <- tempF(name = "working.csv", sep = ";")

model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
saveD(model, "model", format = "rds")
model <- importD("model.rds")
setProject("example")
getCurrentDir()
```

Saving functions take names **without extensions**. Importing functions take
names **with extensions**. CSV separators default to commas. Use
`format = "rds"` with `saveD()`, `saveF()`, or `tempF()` to preserve any R
object, including lists and models. Saving returns the file path invisibly.
Missing files and unsupported object types raise errors.

Project and common-library settings live in
`tools::R_user_dir("organizeScripts", "config")`, outside the installation.
Set `options(organizeScripts.config_dir = "/your/settings/folder")` to override
this location. Existing settings still present in an installed package are
copied on first use without replacing new settings. If an earlier reinstall
already removed them, register existing folders again with `newProject()`.
Project files are retained when registering an existing folder.

Spatial operations require `sf` (sf/Spatial objects) or `raster`
(RasterLayer/Stack/Brick). Figure export requires `ggplot2`. Install these
optional packages only if you use those features.

`setCommonLib(path)` configures shared scripts; use
`setCommonLib(new_path, overwrite = TRUE)` to change it.
Finalizing data or scripts returns final and backup paths invisibly.

Run the dependency-free regression checks with
`Rscript tests/regression.R` from the repository, or run `R CMD check`.
