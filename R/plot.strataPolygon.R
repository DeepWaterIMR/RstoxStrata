##' @title Plot a strata polygon object on a map
##' @description Plot method for \code{\link[=strataPolygon]{strataPolygon}} objects.
##' @param x \code{strataPolygon} object from the \code{\link{strataPolygon}} function.
##' @param basemap Logical. Should the polygons be plotted on a \code{\link[ggOceanMaps]{basemap}} from the ggOceanMaps package.
##' @param fill_color Character specifying the fill color of strata polygons if \code{facetted = TRUE}.
##' @param fill_alpha Numeric between 0 and 1 specifying the tranparency of strata polygon fill.
##' @param facetted Logical indicating whether a facetted map should be returned instead of colored polygons overlaid on the same map.
##' @param ... Additional parameters passed to \code{\link[ggOceanMaps]{basemap}}.
##' @method plot strataPolygon
##' @seealso \code{\link{strataPolygon}} \code{\link[ggOceanMaps]{basemap}}
##' @author Mikko Vihtakari
##' @export

## Test parameters

plot.strataPolygon <- function(x, basemap = TRUE, facetted = FALSE, fill_color = "red", fill_alpha = 0.8, ...) {

  if(basemap) {
    if(facetted) {
      ggOceanMaps::basemap(data = x$geostrata, ...) +
        ggplot2::geom_sf(data = x$strata, color = fill_color, fill = fill_color,
                         size = ggOceanMaps::LS(0.1), alpha = fill_alpha) +
        ggplot2::geom_sf(data = x$geostrata,
                         fill = NA, color = "black") +
        ggplot2::geom_sf_text(data = suppressWarnings(sf::st_centroid(x$geostrata)),
                              ggplot2::aes(label = geostrata.name),
                              fontface = 2) +
        ggplot2::facet_wrap(~ interval)
    } else {
      ggOceanMaps::basemap(data = x$geostrata, ...) +
        ggplot2::geom_sf(data = x$strata,
                         ggplot2::aes(fill = interval, color = interval),
                         alpha = fill_alpha, size = ggOceanMaps::LS(0.1)) +
        ggplot2::geom_sf(data = x$geostrata,
                         fill = NA, color = "black") +
        ggplot2::geom_sf_text(data = suppressWarnings(sf::st_centroid(x$geostrata)),
                              ggplot2::aes(label = geostrata.name),
                              fontface = 2) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(fill = "Interval", color = "Interval")
    }

  } else {
    if(facetted) {

      ggplot2::ggplot() +
        ggplot2::geom_sf(data = x$strata, color = fill_color, fill = fill_color,
                         size = ggOceanMaps::LS(0.1), alpha = fill_alpha) +
        ggplot2::geom_sf(data = x$geostrata,
                         fill = NA, color = "black") +
        ggplot2::geom_sf_text(data = suppressWarnings(sf::st_centroid(x$geostrata)),
                              ggplot2::aes(label = geostrata.name),
                              fontface = 2) +
        ggplot2::facet_wrap(~ interval) +
        ggplot2::labs(x = "Longitude", y = "Latitude") +
        ggplot2::theme_bw() %>% suppressWarnings()

    } else {
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = x$strata,
                         ggplot2::aes(fill = interval, color = interval),
                         size = ggOceanMaps::LS(0.1), alpha = fill_alpha) +
        ggplot2::geom_sf(data = x$geostrata,
                         fill = NA, color = "black") +
        ggplot2::geom_sf_text(data = suppressWarnings(sf::st_centroid(x$geostrata)),
                              ggplot2::aes(label = geostrata.name),
                              fontface = 2) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(fill = "Interval", color = "Interval", x = "Longitude",
                      y = "Latitude") +
        ggplot2::theme_bw() %>% suppressWarnings()
    }
  }
}
