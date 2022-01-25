#' @title Plot a geostrata data frame on a map
#' @description Plots geostrata argument from the \code{\link[=strataPolygon]{strataPolygon}} on a \code{\link[ggOceanMaps]{basemap}} together with bathymetry
#' @param geostrata A data frame. See the same argument in \code{\link[=strataPolygon]{strataPolygon}}
#' @param plot Logical indicating whether ggOceanMaps plot should be returned. If \code{FALSE}, the geostrata polygons are returned as an \link[sf]{sf} objected instead.
#' @param ... Additional parameters passed to \code{\link[ggOceanMaps]{basemap}}.
#' @details This function is useful for fitting geostrata visually on a map before running the time-consuming \code{\link[=strataPolygon]{strataPolygon}} step. Can also be used to acquire \code{geostrata} as \link[sf]{sf} polygons
#' @return If \code{plot = TRUE} a \link[ggplot2]{ggplot} map, which can be assigned to an object and modified as any ggplot object. Otherwise an \link[sf]{sfc}.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

plotGeostrata <- function(geostrata, plot = TRUE, ...) {

  ## Turn off s2

  s2_mode <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})

  ## Checks

  if(!(is.data.frame(geostrata) & ncol(geostrata) %in% 4:5)) {
    stop("The geostrata argument has to be a data.frame with 4 or 5 columns.")
  }

  if(ncol(geostrata) == 5) {
    name.col <- which(sapply(geostrata, class) %in% c("character", "factor"))

    geostrata.names <- geostrata[, name.col]
    geostrata <- geostrata[, -name.col]
  } else{
    geostrata.names <- LETTERS[1:nrow(geostrata)]
  }

  ## Formulate geostrata as polygons

  geopols <- lapply(1:nrow(geostrata), function(i) {

    tmp <- unname(unlist(geostrata[i,]))
    names(tmp) <- c("xmin", "xmax", "ymin", "ymax")

    sf::st_bbox(tmp) %>%
      sf::st_as_sfc() %>%
      sf::st_set_crs(value = 4326) %>%
      sf::st_sf() %>%
      dplyr::mutate(geostrata.name = geostrata.names[i]) %>%
      cbind(geostrata[i,]) %>%
      smoothr::densify()
  }) %>% dplyr::bind_rows()

  if(!all(sf::st_is_valid(geopols))) {
    geopols <- sf::st_make_valid(geopols)
  }

  if(plot) {
    ggOceanMaps::basemap(data = geopols, bathymetry = TRUE, ...) +
      ggplot2::geom_sf(data = geopols,
                       fill = NA, color = "black") +
      ggplot2::geom_sf_text(data = suppressWarnings(sf::st_centroid(geopols)),
                            ggplot2::aes(label = geostrata.name),
                            fontface = 2)

  } else {
    geopols
  }
}

