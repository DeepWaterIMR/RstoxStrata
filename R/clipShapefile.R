#' @title Clip a shape file using a bounding area
#' @description Clips an area from a larger shape file (\link[sf]{st_polygon}).
#' @param x Original shape file to be clipped. Required. Must contain \code{\link[sp]{proj4string}} information.
#' @param limits The constraining area used to clip \code{x}. Required. Either a numeric vector of length 4 or a \link[sp]{SpatialPolygons} object. The first element of the numeric vector defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The \link[sp]{SpatialPolygons} object must contain \code{\link[sp]{proj4string}} information. See details.
#' @param proj.limits The \code{\link[sp]{proj4string}} projection attributes for \code{limits}. Defaults to decimal degrees (see **Usage**).
#' @param simplify Should the \code{x} geometry be simplified before clipping? Useful to make the function faster for large shape files. Uses \code{\link[rgeos]{gSimplify}} function.
#' @param tol Numerical tolerance value to be used for simplification. See \code{\link[rgeos]{gSimplify}}.
#' @param return.boundary logical. If \code{TRUE} returns the clip boundary together with the shapefile
#' @details The function uses the \code{\link[rgeos]{gIntersection}} function to clip smaller \link[sp]{SpatialPolygons} from larger ones. The clip area is constrained by either a numeric vector or \link[sp]{SpatialPolygons} object in the \code{limits} argument. One of these arguments must be given. Defining \code{limits} by a \link[sp]{SpatialPolygons} object gives greater freedom for the clip area as the area does not have to be rectangular.
#' @author Mikko Vihtakari with a solution from \href{https://stackoverflow.com/questions/15881455/how-to-clip-worldmap-with-polygon-in-r}{Simon O'Hanlon, Roger Bivand/SO community}
#' @export

# Test parameters
# x = pol; limits = tmp; proj.limits = 4326; simplify = FALSE; tol = 60; return.boundary = FALSE
clipShapefile <- function(x, limits = NULL, proj.limits = 4326, simplify = FALSE, tol = 60, return.boundary = FALSE) {

  ## Checks

  if(is.null(x)) stop("x, the original shape file must be supplied")
  if(is.null(limits)) stop("Either limits or limiting.polygon must be supplied")

  ## Projection

  x_proj <- sf::st_crs(x)

  if(is.na(x_proj)) stop("CRS for x is missing. Define the projection attributes and try again.")

  ## Clip boundary

  if(grepl("sf", class(limits))) {
    proj.limits <- sf::st_crs(limits)
    clip_boundary <- limits
  } else {
    if(!is.numeric(limits)) stop("limits have to be numeric, SpatialPolygonsDataFrame or SpatialPolygons object")
    if(length(limits) == 1) {

      stop("Limits with length of 1 has not been implemented properly yet")
      bd <- data.frame(lon = seq(-180, 180, by = 0.5), lat = limits)
      bd <- ggOceanMaps::transform_coord(bd, proj.out = x_proj)
      ch <- grDevices::chull(bd$lat, bd$lon)
      coords <- as.matrix(bd[c(ch, ch[1]), 1:2])
      clip_boundary <- sf::st_sfc(sf::st_polygon(list(coords)), crs = x_proj)

    } else if(length(limits) != 4) {
      stop("the length of limits vector has to be 4. See limits argument")
    } else {

      coords <- as.matrix(data.frame(
        lon = c(limits[[1]], limits[[2]], limits[[2]], limits[[1]], limits[[1]]),
        lat = c(limits[[3]], limits[[3]], limits[[4]], limits[[4]], limits[[3]])))
      clip_boundary <- sf::st_sfc(sf::st_polygon(list(coords)), crs = sf::st_crs(proj.limits))
      # clip_boundary <- sf::st_segmentize(clip_boundary, units::set_units(0.1, degree))

    }
  }

  ### Validate the clip boundary

  if(!all(sf::st_is_valid(clip_boundary))) {
    clip_boundary <- sf::st_make_valid(clip_boundary)
  }

  ## Check that the projections match

  if(sf::st_crs(proj.limits) != sf::st_crs(x_proj)) {
    clip_boundary <- sf::st_transform(clip_boundary, x_proj)
  }

  ## Simplify bathymetry. Not used when run through basemap

  if(!all(sf::st_is_valid(x))) {
    x <- suppressWarnings(sf::st_as_sf(rgeos::gBuffer(sf::as_Spatial(x), byid = TRUE, width = 0))) # Because the sf way under doesn't work:
    # x <- sf::st_make_valid(x, oriented = TRUE)
    # x <- sf::st_buffer(x, dist = 1)

  }

  if(simplify) {
    x <- sf::st_simplify(x, dTolerance = tol)
  }

  ## Cropping

  shapefile <- suppressWarnings(sf::st_intersection(x, clip_boundary))

  ## Clipping the bathymetry (using a bypass scavenged from here: https://stackoverflow.com/questions/15881455/how-to-clip-worldmap-with-polygon-in-r)
  ## Sometimes rgeos::gIntersection gets confused when the resulting clipped SpatialPolygon contains other shapes than polygons. The bypass fixes this problem, but takes a longer time to complete than the regular method. Therefore two methods
  # error_test <- quiet(try(rgeos::gIntersection(x, clip_boundary, byid = TRUE), silent = TRUE))
  #
  # if(class(error_test) == "try-error") {
  #  shapefile <- rgeos::gIntersection(x, clip_boundary, byid = TRUE, drop_lower_td = TRUE, checkValidity = 0L)
  #   } else {
  #     shapefile <- error_test
  #   }
  #
  # if(class(x) == "SpatialPolygonsDataFrame") {
  #   ids <- sapply(slot(shapefile, "polygons"), function(x) slot(x, "ID"))
  #   ids <- gsub("\\D", "", ids)
  #
  #   if(ncol(x@data) == 1) {
  #     tmp.df <- data.frame(x@data[ids,])
  #     names(tmp.df) <- names(x@data)
  #   } else {
  #     tmp.df <- x@data[ids,]
  #   }
  #
  #   shapefile <- SpatialPolygonsDataFrame(shapefile, tmp.df, match.ID = FALSE)
  #
  # }


  if(return.boundary) {
    list(shapefile = shapefile, boundary = clip_boundary)
  } else {
    shapefile
  }

}
