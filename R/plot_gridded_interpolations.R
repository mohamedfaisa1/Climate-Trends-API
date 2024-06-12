#' Creates a Plot of Interpolated Values for Spatial Data in the Mainland
#' United States
#'
#' Given an R object containing interpolated data (output of
#' \code{\link{interpolate_points}}), and a data frame
#' containing spatial data (longitudinal and latitudinal coordinates)
#' within the contiguous (48 States) United States a ggplot2 plot object
#' be returned.
#'
#' @param x R object containing interpolations for grid points
#' @param g Data frame containing grid points with first column containing
#' longitudes (-180,180) and second column representing latitudes (-90,90)
#' @param t Plot title
#' @param f Legend title
#' @return ggplot2 plot object
#' @examples
#' # Plot interpolated values of T_DAILY_AVG for mainland USA
#' g <- create_grid_points()
#' w <- weather_data[,c(3,4)]
#' Y <- weather_data$T_DAILY_AVG
#' locs_interp <- create_grid_points()
#' X_interp <- model.matrix(~1, data=locs_interp)
#' x <- interpolate_points(w, Y, NULL, locs_interp, X_interp)
#' g.p <- plot_gridded_interpolations(x, g)
#' g.p # to see plot
#' @export
plot_gridded_interpolations <- function(x, g, t=NULL, f=NULL){
  # set default title if no title is provided
  if(is.null(t)){
      t <- "Average Temperature (°C) in the Mainland United States"
  }
  if(is.null(f)){
    f <- "Average Temperature (°C)"
  }

  # make plotting data frame
  img.mat <- cbind.data.frame(g, x)
  colnames(img.mat) <- c("lon", "lat", "interps")

  # get contiguous United States data
  xx <- maps::map("usa", "main", exact=T, plot=FALSE)
  bound.df <- cbind.data.frame(lon=xx$x, lat=xx$y)
  usa_bounds <- sf::st_as_sf(xx)

  # plot data
  g <- ggplot2::ggplot() +
    ggplot2::geom_raster(data=img.mat, ggplot2::aes(x=lon, y=lat,
                                                    fill=interps)) +
    ggplot2::geom_sf(data=usa_bounds, color="black", fill=NA) +
    ggplot2::coord_sf() +
    ggplot2::scale_fill_viridis_c(option = "magma") +
    ggplot2::labs(title=t, x="Longitude", y="Latitude",
                  fill=f) +
    ggplot2::geom_polygon(data=bound.df, ggplot2::aes(x=lon,y=lat),
                          fill=NA, linewidth=1.2, color="black") +
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5),
                   legend.title=ggplot2::element_text(hjust=0.5))

  # return plot object
  return(g)
}
