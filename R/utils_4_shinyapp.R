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

#' Create the leaflet color palette function and choose the
#' optimal* number of breaks.
#'
#' *In this case the optimal value is the value that allows
#' leaflet to work.
#'
#' @param vals vector of values to create palette. Usually the
#'             variable being mapped.
#' @param palette colors or color function that values will be
#'                mapped to. This is passed directly to the
#'                palette argument of \code{\link[leaflet:colorNumeric]{colorQuantile}}
#'
#' @return a palette function
#' @export
#'
#' @importFrom leaflet colorQuantile
myPal <- function(vals, palette = "viridis") {
  #Cycle through to find the max number of breaks we can have
  ngrps <- NA_integer_
  for (i in 7:2) {
    lvls <- quantile(vals, probs = seq(0,1, length = (i + 1)), na.rm=TRUE)
    if (i + 1 == length(unique(lvls))) {
      ngrps <- i
      break()
    }
  }
  #Return palette function
  leaflet::colorQuantile(palette, vals, n = ngrps)
}

#' Create Custom Legend for Map
#'
#' @inheritParams myPal
#'
#' @return a ggplot object acting as a legend
#' @export
#'
#' @import ggplot2
#' @importFrom toOrdinal toOrdinal
#' @importFrom scales percent
myLeg <- function(vals, palette = "viridis") {
  #define palette and get unique number of levels
  cols <- unique(myPal(vals, palette)(sort(vals, na.last = TRUE)))

  ncols <- length(cols)
  nalab <- NULL

  if (length(setdiff(cols, "#808080")) != ncols) {
    ncols <- ncols - 1
    nalab <- "No Data"
  }

  vals <- quantile(vals, probs = seq(0,1, length = (ncols+1)), na.rm = TRUE)
  if (max(vals) <= 1) {
    vals <- scales::percent(vals, accuracy = .1)
  } else {
    vals <- round(vals, 1)
  }


  nlabs <- ncols - 1

  quantity <- c("half", "third", "fourth", "fifth", "sixth", "seventh")

  which_quantity <- purrr::map_chr(1:ncols, toOrdinal::toOrdinal)
  which_quantity[1] <- "Lowest"
  which_quantity[ncols] <- "Highest"

  labs <- paste(which_quantity, " ",
                quantity[nlabs], "\n(",
                vals[1:ncols], " to ",
                vals[2:(ncols + 1)], ")", sep = "")

  labs <- c(labs, nalab)

  #Create data.frame to graph
  df <- data.frame(x = 3.2*rep(1:4, 2),
                   y = rep(rep(2:1, each = 4)))

  df <- df[1:length(cols),]

  df$colors <- factor(cols, levels = cols)
  df$labs <- as.factor(labs)
  df$x_labs <- df$x + 1.56

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, label = labs)) +
    ggplot2::geom_tile(ggplot2::aes(fill = colors), width = 1, height = .8, alpha = .5) +
    ggplot2::geom_text(ggplot2::aes(x = x_labs), size = 3.4) +
    ggplot2::scale_fill_manual(breaks = df$colors, values = as.character(df$colors), ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() +
    ggplot2::xlim(c(2.5,15))
}
