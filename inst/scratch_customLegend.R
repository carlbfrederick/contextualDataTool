#create legend

myLeg <- function(vals, palette = "viridis") {
  #define palette and get unique number of levels
  cols <- unique(myPal(vals, palette)(sort(vals, na.last = TRUE)))

  ncols <- length(cols)
  nalab <- NULL

  if (length(setdiff(cols, "#808080")) != ncols) {
    ncols <- ncols - 1
    nalab <- "No Data"
  }

  vals <- quantile(wiTract$fs_singlepar, probs = seq(0,1, length = (ncols+1)), na.rm = TRUE)

  nlabs <- ncols - 1

  quantity <- c("half", "third", "fourth", "fifth", "sixth", "seventh")

  which_quantity <- purrr::map_chr(1:ncols, toOrdinal::toOrdinal)
  which_quantity[1] <- "Lowest"
  which_quantity[ncols] <- "Highest"

  labs <- paste(which_quantity, " ",
                quantity[nlabs], "\n(",
                scales::percent(vals, accuracy = .1)[1:ncols], " to ",
                scales::percent(vals, accuracy = .1)[2:(ncols + 1)], ")", sep = "")

  labs <- c(labs, nalab)

  #Create data.frame to graph
  df <- data.frame(x = rep(1:4, 2),
                   y = rep(rep(2:1, each = 4)))

  df <- df[1:length(cols),]

  df$colors <- factor(cols, levels = cols)
  df$labs <- as.factor(labs)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, label = labs)) +
    ggplot2::geom_tile(ggplot2::aes(fill = colors), width = .9, height = .8) +
    ggplot2::geom_label(size = 3) +
    ggplot2::scale_fill_manual(breaks = df$colors, values = as.character(df$colors), ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed()
}

myLeg(wiBlkGrp$lf_nilf)

# rm(testvar, cols, ncols, nalab, vals, nlabs, quantity, which_quantity, labs, df)
# testvar <- wiBlkGrp$la_notwellnoteng
#
# #define palette and get unique number of levels
# cols <- unique(myPal(testvar)(sort(testvar, na.last = TRUE)))
#
# ncols <- length(cols)
# nalab <- NULL
#
# if (length(setdiff(cols, "#808080")) != ncols) {
#   ncols <- ncols - 1
#   nalab <- "No Data"
# }
#
# vals <- quantile(wiTract$fs_singlepar, probs = seq(0,1, length = (ncols+1)), na.rm = TRUE)
#
# nlabs <- ncols - 1
#
# quantity <- c("half", "third", "fourth", "fifth", "sixth", "seventh")
#
# which_quantity <- purrr::map_chr(1:ncols, toOrdinal::toOrdinal)
# which_quantity[1] <- "Lowest"
# which_quantity[ncols] <- "Highest"
#
# labs <- paste(which_quantity, " ",
#               quantity[nlabs], "\n(",
#               scales::percent(vals, accuracy = .1)[1:ncols], " to ",
#               scales::percent(vals, accuracy = .1)[2:(ncols + 1)], ")", sep = "")
#
# labs <- c(labs, nalab)
#
# labs
#
# #Create data.frame to graph
# df <- data.frame(x = rep(1:4, 2),
#                  y = rep(rep(2:1, each = 4)))
#
# df <- df[1:length(cols),]
#
# df$colors <- factor(cols, levels = cols)
# df$labs <- as.factor(labs)
#
# ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, label = labs)) +
#   ggplot2::geom_tile(ggplot2::aes(fill = colors), width = .8, height = .8) +
#   ggplot2::geom_label() +
#   ggplot2::scale_fill_manual(breaks = df$colors, values = as.character(df$colors), ) +
#   ggplot2::theme_void() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::coord_fixed()
