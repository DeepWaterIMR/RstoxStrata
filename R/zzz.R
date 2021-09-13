.onLoad <- function(libname, pkgname) {
  options("rgdal_show_exportToProj4_warnings"="none")
}

# Define global variables
utils::globalVariables(c("Elevation.relative.to.sea.level", "geom", "area.km2", "nautical_mile", "."))


