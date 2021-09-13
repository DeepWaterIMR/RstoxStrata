#' @title Create polygon shapefiles for depth strata required by StoX
#' @description A helper function to define strata for stock assesment from GEBCO and ETOPO bathymetry grids.
#' @param bathy String giving the path to the bathymetry NetCDF file.
#' @param depths Numeric vector giving the cut points for depth strata (see \code{\link[base]{cut}}. Data outside the cut range will be dropped. Use limits of length two exceeding the depths of the region to avoid depth categorization (\code{c(0, 1000)} for instance).
#' @param boundary A \link[sf]{st_polygon} object, text string defining the file path to a spatial polygon or a numeric vector of length 4 giving the boundaries for the overall region. Should be given as decimal degrees. If numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @param geostrata A data frame defining the minimum and maximum longitude and latitude for geographically bounded strata. The data frame columns must be ordered as \code{lon.min, lon.max, lat.min, lat.max}. Column names do not matter. Each row in the data frame will be interpreted as separate geographically bounded strata. Use \code{NULL} to ignore geostrata.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for disconnected polygons which should be removed from the strata. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function.
#' @param remove.holes Single numeric value specifying a threshold (area in km2) for holes which should be removed from the strata. Set to \code{NULL} to bypass the removal.
#' @param precise Logical indicating whether a more precise \link[smoothr]{fill_holes} function should be used over less precise but a lot quicker \link[nngeo]{st_remove_holes}.
#' @details Uses \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO} or \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids to define the depth strata. Download the desired grid from the links. The bathymetry grids must be in NetCDF format and defined using decimal degrees.
#' @return An \link[sf]{sf} object containing the estimated strata and information for them including areas. The strata are returned as decimal degrees (EPGS:4326).
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#'
#' ETOPO1 1 Arc-Minute Global Relief Model. \url{https://doi.org/10.7289/V5C8276M}.
#' @author Mikko Vihtakari
#' @export

## Developmental code
# bathy = link; depths = depths.vec; boundary = boundary.vec; geostrata = geostrata.df; drop.crumbs = 10; remove.holes = 100; precise = FALSE
strataPolygon <- function(bathy, depths, boundary, geostrata = NULL, drop.crumbs = NULL, remove.holes = NULL, precise = FALSE) {

  ## General checks ####

  sf::sf_use_s2(FALSE) # A temporary hack to bypass problems: https://github.com/r-spatial/sf/issues/1780

  ### Bathy argument

  if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")

  ### The depths argument

  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")}

  ### The boundary argument

  if(grepl("spatialpolygons|sf", class(boundary), ignore.case = TRUE)) {

    if(is.na(sf::st_crs(boundary))) {
      stop("boundary misses proj4string argument.")
    } else if(!sf::st_is_longlat(boundary)) {
      stop("boundary has to be defined as decimal degrees")
    }

  } else if(class(boundary) == "character" & length(boundary) == 1) {
    if(!file.exists(boundary)) stop("Boundary shapefile not found. Check your path")

    boundary <- sf::st_read(boundary, quiet = TRUE)

    if(is.na(sf::st_crs(boundary))) {
      stop("boundary misses proj4string argument.")
    } else if(!sf::st_is_longlat(boundary)) {
      stop("boundary has to be defined as decimal degrees")
    }

  } else if(!(is.vector(boundary) & class(boundary) %in% c("numeric", "integer") & length(boundary) == 4)) {
    stop("The boundary parameter has to be a numeric/integer vector of length 4 giving the decimal degree longitude and latitude limits for the strata region OR a character argument giving the location of the shapefile polygon.")
  }

  ### The geostrata argument

  if(is.null(geostrata)) {
    if(class(boundary) == "numeric") {
      geostrata <- data.frame(lon.min = boundary[1],
                              lon.max = boundary[2],
                              lat.min = boundary[3],
                              lat.max = boundary[4])
    } else {
      tmp <- raster::extent(boundary)
      geostrata <- data.frame(lon.min = tmp@xmin,
                              lon.max = tmp@xmax,
                              lat.min = tmp@ymin,
                              lat.max = tmp@ymax)
    }
  } else {
    if(!(is.data.frame(geostrata) & ncol(geostrata) == 4)) {
      stop("The geostrata argument has to be a data.frame with 4 columns.")
    }
  }

  ### The drop.crumbs argument

  if(!is.null(drop.crumbs)) {
    if(!(is.vector(drop.crumbs) & class(drop.crumbs) %in% c("numeric", "integer") & length(drop.crumbs) == 1)) {
      stop("The drop.crumbs parameter has to be a single value.")
    }
  }

  ## Set the counter

  pb <- utils::txtProgressBar(min = 1, max = nrow(geostrata) + 10, style = 3)
  utils::setTxtProgressBar(pb, 1)

  ## Open raster ####

  ras <- raster::raster(bathy)
  # ras <- stars::read_stars(bathy) %>% st_set_crs(4326)

  if(is.na(sf::st_crs(ras))) stop("bathy does not contain coordinate reference information")

  if(!sf::st_is_longlat(ras)) {
    ras <- ras %>% sf::st_transform(4326)
  }

  utils::setTxtProgressBar(pb, 2)

  ras <- raster::crop(ras, raster::extent(boundary))

  # sf::st_crop(ras, boundary)

  if(grepl("spatialpolygons|sf", class(boundary), ignore.case = TRUE)) {
    ras <- raster::mask(ras, boundary)
  }

  utils::setTxtProgressBar(pb, 3)

  ## Reclassify raster ####

  if(all(depths >= 0)) depths <- sort(-1 * depths)

  depths <- c(-Inf, depths, Inf)

  cut_int <- paste(abs(depths[-1]), abs(depths[-length(depths)]), sep = "-")

  cut_df <- data.frame(from = depths[-length(depths)],
                       to = depths[-1],
                       average = sapply(strsplit(cut_int, "-"), function(k) mean(as.numeric(k))),
                       interval = factor(cut_int, levels = cut_int),
                       stringsAsFactors = FALSE)

  cut_matrix <- as.matrix(cut_df[-ncol(cut_df)])

  r <- raster::reclassify(ras, rcl = cut_matrix, right = NA)

  ## Polygonization ####

  pol <- sf::st_as_sf(stars::st_as_stars(r), as_points = FALSE, merge = TRUE) %>%
    sf::st_set_crs(sf::st_crs(ras))

  utils::setTxtProgressBar(pb, 4)

  ### Validate the polygon

  # pol <- sf::st_transform(pol, sf::st_crs(crop.proj))

  if(!all(sf::st_is_valid(pol))) {
    pol <- suppressWarnings(sf::st_as_sf(rgeos::gBuffer(sf::as_Spatial(pol), byid = TRUE, width = 0))) # Because the sf way under doesn't work:
    # pol <- sf::st_make_valid(pol, oriented = TRUE)
    # pol <- sf::st_buffer(pol, dist = 1)
  }

  utils::setTxtProgressBar(pb, 5)

  if(!is.null(drop.crumbs)) {

    pol <- pol[sf::st_area(pol) >=
                 units::set_units(drop.crumbs, "km^2", mode = "standard"),]

    # out <- suppressWarnings(suppressMessages(
    #   smoothr::drop_crumbs(
    #     st_geometry(out),
    #     units::set_units(drop.crumbs, "km^2", mode = "standard")
    #   )
    # )) # old
  }

  utils::setTxtProgressBar(pb, 6)

  if(!is.null(remove.holes)) {

    if(!all(sf::st_is(pol, "POLYGON") | sf::st_is(pol, "MULTIPOLYGON"))) {
      pol <- sf::st_collection_extract(pol, "POLYGON")
      # message("Removed non-polygons from geostrata ", LETTERS[i], ". This may have influenced the result.")
    }

    # if(remove.holes == 0) {
    #   pol <- sfheaders::sf_remove_holes(pol) # sfheaders::sf_remove_holes is very fast but you cannot decide the hole area
    #
    # } else

    if(precise) {
      pol <- smoothr::fill_holes(pol, units::set_units(remove.holes, "km^2", mode = "standard")) # smoothr is slow at the time of testing
    } else {
      pol <- nngeo::st_remove_holes(pol, remove.holes/1e4)
    }

    ## To find polygons with holes (length > 1)
    # sapply(1:length(sf::st_geometry(pol)), function(i) {
    #   tmp <- sf::st_cast(geom[i], "POLYGON")
    #   length(tmp)
    #   }) > 1
  }

  utils::setTxtProgressBar(pb, 7)

  # Validate once more

  if(!all(sf::st_is_valid(pol))) {
    pol <- sf::st_make_valid(pol)
  }

  ## Combine polygons with same depth

  suppressMessages(suppressWarnings(
    pol <- pol %>% dplyr::group_by(Elevation.relative.to.sea.level) %>%
      dplyr::summarise(geometry = sf::st_union(
        .data[[!!grep("geom", names(pol), ignore.case = TRUE, value = TRUE)]])) %>%
      dplyr::rename("average" = "Elevation.relative.to.sea.level") %>%
      dplyr::left_join(., cut_df, by = "average")
  ))

  if(!"geometry" %in% names(pol)) {
    names(pol)[grep("geom", names(pol), ignore.case = TRUE)] <- "geometry"
  }

  utils::setTxtProgressBar(pb, 8)

  ## Geostrata ####

  # i = 1
  out.pols <- lapply(1:nrow(geostrata), function(i) {

    ## ###
    utils::setTxtProgressBar(pb, 8 + i)
    # print(i)

    tmp <- unname(unlist(geostrata[i,]))

    out <- suppressMessages(suppressWarnings(clipShapefile(pol, limits = tmp)))

    # out <- suppressWarnings(
    #   sf::st_crop(pol,
    #               st_bbox(c(xmin = tmp[1], xmax = tmp[2], ymin = tmp[3], ymax = tmp[4]), crs = 4326))
    # )

    if(!all(sf::st_is_valid(out))) {
      out <- sf::st_make_valid(out)
    }

    if(!all(sf::st_is_valid(out))) stop("The geometry validation within the geostrata loop did not work. Adjust something.")

    ## Combine polygons with same depth

    out %>% cbind(., geostrata[i,], geostrata.name = LETTERS[i])

  })

  ## Combine the list####

  if(length(out.pols) == 1) {
    pol <- out.pols[[1]]
  } else {
    pol <- do.call(rbind, out.pols)
  }

  ## Remove other geometries than polygons

  utils::setTxtProgressBar(pb, 9 + nrow(geostrata))

  if(!all(sf::st_is(pol, "POLYGON") | sf::st_is(pol, "MULTIPOLYGON"))) {
    suppressMessages(suppressWarnings(
      pol <- sf::st_collection_extract(pol, "POLYGON") %>%
        dplyr::group_by(dplyr::across(-geometry)) %>%
        dplyr::summarise(geometry = sf::st_union(
          .data[[!!grep("geom", names(pol), ignore.case = TRUE, value = TRUE)]])) %>%
        dplyr::arrange(geostrata.name, average)
    ))
  }

  ## Validate
  if(!all(sf::st_is_valid(pol))) {
    pol <- sf::st_make_valid(pol)

    if(!all(sf::st_is_valid(pol))) {
      pol <- suppressWarnings(sf::st_as_sf(rgeos::gBuffer(sf::as_Spatial(pol), byid = TRUE, width = 0))) # Because the sf way under doesn't work:
      # pol <- sf::st_make_valid(pol, oriented = TRUE)
      # pol <- sf::st_buffer(pol, dist = 1)
    }
  }

  ## Calculate area ####

  pol <- pol %>%
    tibble::add_column(area.km2 = units::set_units(sf::st_area(pol), "km^2"),
                       area.nm2 = units::set_units(area.km2, nautical_mile^2),
                       .before = "geometry")


  sf::sf_use_s2(TRUE) # A temporary hack to bypass problems: https://github.com/r-spatial/sf/issues/1780
  utils::setTxtProgressBar(pb, 10 + nrow(geostrata))

  ## Return

  pol

}
