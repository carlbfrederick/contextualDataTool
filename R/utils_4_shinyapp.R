#' Expand the bounding box of the polygon to show some context
#'
#' @param bbox coordinates given by \code{\link[sf]{st_bbox}}
#' @param zoom_factor numeric, multiplier ... default is 1.2
#'
#' @return zoomed coordinates
#' @export
expand_bbox <- function(bbox, zoom_factor = 1.2) {
  orig_coords <- purrr::map(list("^x", "^y"), ~bbox[grep(.x, names(bbox))])
  new_coords <- purrr::map(orig_coords, ~zoom_factor * (.x - mean(.x)) + .x)
  names(new_coords) <- c("x", "y")
  return(new_coords)
}

