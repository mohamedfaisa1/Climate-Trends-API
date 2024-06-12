#' Creates Grid Points that Lie within the Contiguous United States
#'
#' Given a certain resolution, a data frame containing longitudinal and
#' latitudinal coordinates within the contiguous (48 States) United States will
#' be returned.
#'
#' @param resolution The resolution of the grid points
#' @return A data frame containing grid points within the contiguous United
#' States with the first column being longitude (-180,180) and the second column
#' containing latitude (-90,90).
#' @examples
#' # Get a grid of points with resolution 100
#' pt.grid <- create_grid_points(resolution=100)
#' @export
create_grid_points <- function(resolution=50){
  # make longitude and latitude sequences for grid points
  xx <- maps::map("usa", "main", exact=T, plot=FALSE)

  long <- seq(xx$range[1],  xx$range[2], length.out=resolution)
  lat <- seq(xx$range[3],  xx$range[4], length.out=resolution)

  # make grid
  g <- expand.grid(long, lat)
  colnames(g) <- c("Longitude", "Latitude")

  # get US grid points (bounds)
  x_map <- maps::map("usa", regions="main", exact=TRUE, plot=FALSE)

  # keep only grid points in polygon (within contigous US bounds)
  pts_poly <- sp::point.in.polygon(g$Longitude, g$Latitude, x_map$x, x_map$y)
  keep <- which(pts_poly == 1)
  g <- g[keep, ]

  # return grid points in contiguous US
  return(g)
}
