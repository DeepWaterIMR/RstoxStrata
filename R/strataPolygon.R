#' @title Create polygon shapefiles for depth strata required by StoX
#' @description A helper function to define strata for stock assessment from GEBCO and ETOPO bathymetry grids.
#' @param bathy String giving the path to the bathymetry NetCDF file.
#' @param depths Numeric vector giving the cut points for depth strata (see \code{\link[base]{cut}}. Data outside the cut range will be dropped. Use limits of length two exceeding the depths of the region to avoid depth categorization (\code{c(0, 1000)} for instance).
#' @param boundary A \link[sf]{st_polygon} object, text string defining the file path to a spatial polygon or a numeric vector of length 4 giving the boundaries for the overall region. Should be given as decimal degrees. If numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @param geostrata A data frame defining the minimum and maximum longitude and latitude for geographically bounded strata. The data frame has to have four numeric columns and optionally one character or factor column. The character or factor column defines the names for geostrata, the default being \code{LETTERS[1:nrow(geostrata)]}. The numeric columns must be ordered as \code{lon.min, lon.max, lat.min, lat.max}. Column names do not matter. Each row in the data frame will be interpreted as separate geographically bounded strata. Use \code{NULL} to ignore geostrata.
#' @param fragment.area Single numeric value specifying a threshold (area in km2) for disconnected polygons and holes which should be removed from the strata. Set to \code{NULL} to bypass the removal.
#' @param bathy.crs The \link[sf:st_crs]{coordinate reference system} for the \code{bathy} raster. Defaults to decimal degrees.
#' @param silent A logical indicating whether the function should be run without returning messages from the operations.
#' @details Uses \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO} or \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids to define the depth strata. Download the desired grid from the links. The bathymetry grids must be in NetCDF format and defined using decimal degrees.
#' @return A list of length two containing \link[sf]{sf} objects of the estimated strata with information for them, including areas, and geostrata polygons.
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#'
#' ETOPO1 1 Arc-Minute Global Relief Model. \url{https://doi.org/10.7289/V5C8276M}.
#' @author Mikko Vihtakari
#' @export

## Developmental code
# bathy = link; depths = depths.vec; boundary = boundary.vec; geostrata = geostrata.df; fragment.area = 100; bathy.crs = 4326; silent = FALSE
strataPolygon <- function(bathy, depths, boundary, geostrata = NULL, fragment.area = NULL, bathy.crs = 4326, silent = FALSE) {

  ## General checks ####

  raster = TRUE # This is developmental switch to make change to the stars package easier
  s2_mode <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})

  ## Set the counter

  pb <- utils::txtProgressBar(min = 1, max = 14 + nrow(geostrata) + length(depths), style = 3)
  utils::setTxtProgressBar(pb, 1)

  ### Bathy argument

  if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")

  ### The depths argument

  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")}

  ### The boundary argument

  if(any(grepl("spatialpolygons|sf", class(boundary), ignore.case = TRUE))) {

    if(is.na(sf::st_crs(boundary))) {
      stop("boundary misses proj4string argument.")
    } else if(!sf::st_is_longlat(boundary)) {
      stop("boundary has to be defined as decimal degrees")
    }

  } else if("character" %in% class(boundary) & length(boundary) == 1) {
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

  if(is.vector(boundary) & any(class(boundary) %in% c("numeric", "integer")) &
     length(boundary) == 4 & is.null(names(boundary))) {
    if(boundary[1] > boundary[2]) boundary <- boundary[c(2,1,3,4)] # correct wrong lon order
    if(boundary[3] > boundary[4]) boundary <- boundary[c(1,2,4,3)] # correct wrong lat order
    names(boundary) <- c("xmin", "xmax", "ymin", "ymax")
  }

  ### The geostrata argument

  if(is.null(geostrata)) {
    if(class(boundary) == "numeric") {
      geostrata <- data.frame(lon.min = boundary[1],
                              lon.max = boundary[2],
                              lat.min = boundary[3],
                              lat.max = boundary[4])
    } else {
      tmp <- sf::st_bbox(boundary)
      geostrata <- data.frame(lon.min = tmp["xmin"],
                              lon.max = tmp["xmax"],
                              lat.min = tmp["ymin"],
                              lat.max = tmp["ymax"])
    }
  } else {
    if(is.data.frame(geostrata) & ncol(geostrata) == 5) {
      name.col <- which(sapply(geostrata, class) %in% c("character", "factor"))

      geostrata.names <- geostrata[, name.col]
      geostrata <- geostrata[, -name.col]
    } else if(is.data.frame(geostrata) & ncol(geostrata) == 4) {
      geostrata.names <- LETTERS[1:nrow(geostrata)]
    } else {
      stop("The geostrata argument has to be a data.frame with 4 or 5 columns.")
    }
  }

  ### The fragment.area argument

  if(!is.null(fragment.area)) {
    if(!(is.vector(fragment.area) & class(fragment.area) %in% c("numeric", "integer") & length(fragment.area) == 1)) {
      stop("The fragment.area parameter has to be a single value.")
    }
  }

  ## Open raster ###

  utils::setTxtProgressBar(pb, 2)

  if(raster) {
    ras <- suppressMessages(raster::raster(bathy))
  } else {
    ras <- stars::read_stars(bathy)
  }

  if(is.na(sf::st_crs(ras))) {
    if(!silent) message("bathy crs set to ", bathy.crs)

    if(raster) {
      raster::crs(ras) <- paste0("EPSG:", bathy.crs)
    } else {
      ras <- ras %>% sf::st_set_crs(bathy.crs)
    }
  }

  if(!sf::st_is_longlat(ras)) {
    ras <- ras %>% sf::st_transform(4326)
  }

  utils::setTxtProgressBar(pb, 3)

  if(any(grepl("spatialpolygons|sf", class(boundary), ignore.case = TRUE))) {
    if(raster) {
      ras <- raster::crop(ras, boundary)
      ras <- raster::mask(x = ras, mask = boundary)
    } else {
      ras <- sf::st_crop(ras, boundary)
    }
  } else {
    if(raster) {
      ras <- raster::crop(ras, raster::extent(boundary))
    } else {
      ras <- sf::st_crop(ras, sf::st_bbox(boundary, crs = sf::st_crs(4326)))
    }
  }

  utils::setTxtProgressBar(pb, 4)

  ## Cut data frame ###

  if(!raster) ras <- stars::st_as_stars(ras) # This step is time and memory consuming. Optimally done after reclassification

  utils::setTxtProgressBar(pb, 5)

  if(all(depths >= 0)) depths <- sort(-1 * depths)

  depths <- c(-1e5, depths, 1e5)

  # if(raster::minValue(ras) > min(depths)) {
  #   depths <- c(depths, 1e5)
  # } else {
  #
  # }

  cut_int <- paste(abs(depths[-1]), abs(depths[-length(depths)]), sep = "-")

  cut_df <- data.frame(from = depths[-length(depths)], to = depths[-1]) %>%
    dplyr::mutate(
      average = -1*rowMeans(dplyr::across(from:to)),
      interval = factor(cut_int, levels = cut_int))

  # Polygonization ####

  ## Deep-sea (-Inf)

  rout <- raster::reclassify(
    ras,
    rbind(as.matrix(cut_df[1,1:3]), cbind(cut_df[2,1], Inf, NA)),
    right = FALSE)

  names(rout) <- "average"

  pol <- sf::st_as_sf(stars::st_as_stars(rout), as_points = FALSE, merge = TRUE) %>%
    sf::st_set_crs(sf::st_crs(ras))

  pol$area <- sf::st_area(pol)

  if(!is.null(fragment.area)) {
    pol <- pol[sf::st_area(pol) >= units::set_units(fragment.area, "km^2", mode = "standard"),]
    pol <- smoothr::fill_holes(pol, units::set_units(fragment.area, "km^2", mode = "standard"))
  }

  pold <- pol

  utils::setTxtProgressBar(pb, 6)

  ## Depth strata

  pols <- lapply(2:(nrow(cut_df)-2), function(i) {
    utils::setTxtProgressBar(pb, 4 + i) # because i starts from 2

    rout <- raster::reclassify(
      ras,
      rbind(cbind(cut_df[1,1], cut_df[i,2], cut_df[i,3]),
            cbind(cut_df[i,2], Inf, NA)),
      right = FALSE)

    names(rout) <- "average"

    pol <- sf::st_as_sf(stars::st_as_stars(rout), as_points = FALSE, merge = TRUE) %>%
      sf::st_set_crs(sf::st_crs(ras))

    pol$area <- sf::st_area(pol)

    if(!is.null(fragment.area)) {
      pol <- pol[sf::st_area(pol) >= units::set_units(fragment.area, "km^2", mode = "standard"),]
      pol <- smoothr::fill_holes(pol, units::set_units(fragment.area, "km^2", mode = "standard"))
    }

    pol
  })

  ## Land (Inf)

  utils::setTxtProgressBar(pb, 4 + nrow(cut_df))

  j <- nrow(cut_df)-1

  rout <- raster::reclassify(
    ras,
    rbind(cbind(cut_df[1,1], cut_df[j,2], cut_df[j,3]),
          cbind(cut_df[j,2], Inf, NA)),
    right = FALSE)

  names(rout) <- "average"

  pol <- sf::st_as_sf(stars::st_as_stars(rout), as_points = FALSE, merge = TRUE) %>%
    sf::st_set_crs(sf::st_crs(ras))

  pol$area <- sf::st_area(pol)

  if(!is.null(fragment.area)) {
    pol <- pol[sf::st_area(pol) >= units::set_units(fragment.area, "km^2", mode = "standard"),]
    # pol <- smoothr::fill_holes(pol, units::set_units(1, "km^2", mode = "standard"))
  }

  poll <- pol

  ## Combine the polygons

  utils::setTxtProgressBar(pb, 5 + nrow(cut_df))

  pol <- dplyr::bind_rows(poll, pols, pold) %>% dplyr::select(-area)

  pol <- pol %>%
    dplyr::group_by(average) %>%
    dplyr::summarise(geometry = sf::st_union(
      .data[[!!grep("geom", names(pol), ignore.case = TRUE, value = TRUE)]])) %>%
    suppressMessages()

  ## Remove overlapping regions

  utils::setTxtProgressBar(pb, 6 + nrow(cut_df))

  pol2 <- lapply(1:(nrow(pol)-1), function(i) {
    rmapshaper::ms_erase(pol[i,], pol[i+1,])
  }) %>% dplyr::bind_rows()

  if(!all(cut_df[cut_df$average < 1.2e4 & cut_df$average > - 9e3, "average"] %in% pol2$average)) {
    pol <- dplyr::bind_rows(pol2,
                            pol %>%
                              dplyr::filter(average < 1.2e4, average > - 9e3, !average %in% pol2$average)
    ) %>% dplyr::arrange(average)
  } else {
    pol <- pol2
  }

  utils::setTxtProgressBar(pb, 7 + nrow(cut_df))

  if(!all(sf::st_is_valid(pol))) {
    pol <- sf::st_make_valid(pol, oriented = TRUE)
  }

  ## Add depth data

  utils::setTxtProgressBar(pb, 8 + nrow(cut_df))

  pol <- pol %>% dplyr::left_join(., cut_df, by = "average")

  if(!"geometry" %in% names(pol)) {
    names(pol)[grep("geom", names(pol), ignore.case = TRUE)] <- "geometry"
  }

  ## Geostrata ####

  # i = 3
  out.pols <- lapply(1:nrow(geostrata), function(i) {

    utils::setTxtProgressBar(pb, 9 + nrow(cut_df) + i)
    # print(i)

    tmp <- unname(unlist(geostrata[i,]))
    names(tmp) <- c("xmin", "xmax", "ymin", "ymax")

    geopol <- sf::st_bbox(tmp) %>%
      sf::st_as_sfc() %>%
      sf::st_set_crs(value = 4326) %>%
      sf::st_sf() %>%
      dplyr::mutate(geostrata.name = geostrata.names[i]) %>%
      cbind(geostrata[i,]) %>%
      smoothr::densify()

    out <- sf::st_intersection(pol, geopol) %>% suppressMessages() %>% suppressWarnings()

    if(!all(sf::st_is_valid(out))) {
      out <- sf::st_make_valid(out)
    }

    if(!all(sf::st_is_valid(out))) stop("The geometry validation within the geostrata loop did not work. Adjust something.")

    list(strata = out, geostrata = geopol)
  })

  ## Combine the list####

  pol <- dplyr::bind_rows(lapply(out.pols, function(x) x$strata))
  geopols <- dplyr::bind_rows(lapply(out.pols, function(x) x$geostrata))

  ## Remove other geometries than polygons

  utils::setTxtProgressBar(pb, 11 + nrow(geostrata) + nrow(cut_df))

  if(!all(sf::st_is(pol, "POLYGON") | sf::st_is(pol, "MULTIPOLYGON"))) {
    pol <- sf::st_collection_extract(pol, "POLYGON") %>%
      dplyr::group_by(dplyr::across(-geometry)) %>%
      dplyr::summarise(geometry = sf::st_union(
        .data[[!!grep("geom", names(pol), ignore.case = TRUE, value = TRUE)]])) %>%
      dplyr::arrange(geostrata.name, average) %>%
      suppressMessages()
  }

  ## Validate

  utils::setTxtProgressBar(pb, 12 + nrow(geostrata) + nrow(cut_df))

  if(!all(sf::st_is_valid(pol))) {
    pol <- sf::st_make_valid(pol)
  }

  ## Column classes

  pol <- pol %>%
    mutate(interval =
             factor(as.character(interval),
                    levels = unique(as.character(pol$interval)))
           )

  ## Calculate area ####

  pol <- pol %>%
    tibble::add_column(area.km2 = units::set_units(sf::st_area(pol), "km^2"),
                       area.nm2 = units::set_units(area.km2, nautical_mile^2),
                       .before = "geometry")

  utils::setTxtProgressBar(pb, 13 + nrow(geostrata) + nrow(cut_df))

  ## Return

  out <- list(strata = pol, geostrata = geopols)
  class(out) <- append("strataPolygon", class(out))
  out

}
