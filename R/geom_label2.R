#' NST Choropleth map

#Function to customize labels for callout boxes
#'
#' @import tidyverse

#' @export
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
#' @export
geom_label2 <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = grid::unit(0.25, "lines"),
                        label.r = grid::unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}
#' @export
GeomLabel2 <- ggplot2::ggproto("GeomLabel2", ggplot2::Geom,
                               required_aes = c("x", "y", "label"),

                               default_aes = ggplot2::aes(
                                 colour = "black", fill = "white", size = 3.88, angle = 0,
                                 hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                                 lineheight = 1.2
                               ),

                               draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                     na.rm = FALSE,
                                                     label.padding = grid::unit(0.25, "lines"),
                                                     label.r = grid::unit(0.15, "lines"),
                                                     label.size = 0.25) {
                                 lab <- data$label
                                 if (parse) {
                                   lab <- parse(text = as.character(lab))
                                 }

                                 data <- coord$transform(data, panel_params)
                                 if (is.character(data$vjust)) {
                                   data$vjust <- compute_just(data$vjust, data$y)
                                 }
                                 if (is.character(data$hjust)) {
                                   data$hjust <- compute_just(data$hjust, data$x)
                                 }

                                 grobs <- lapply(1:nrow(data), function(i) {
                                   row <- data[i, , drop = FALSE]
                                   labelGrob2(lab[i],
                                              x = grid::unit(row$x, "native"),
                                              y = grid::unit(row$y, "native"),
                                              just = "center",
                                              padding = label.padding,
                                              r = label.r,
                                              text.gp =
                                                grid::gpar(
                                                  col = row$colour,
                                                  fontsize = row$size * ggplot2::.pt,
                                                  fontfamily = row$family,
                                                  fontface = row$fontface,
                                                  lineheight = row$lineheight,
                                                  alpha = 0
                                                ),
                                              rect.gp = grid::gpar(
                                                col = row$colour,
                                                fill = scales::alpha(row$fill, row$alpha),
                                                lwd = label.size * ggplot2::.pt
                                              )
                                   )
                                 })
                                 class(grobs) <- "gList"

                                 ggname("geom_label2", grid::grobTree(children = grobs))
                               },

                               draw_key = ggplot2::draw_key_label
)
#' @export
labelGrob2 <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                       just = "center", padding = grid::unit(0.25, "lines"), r = grid::unit(0.1, "snpc"),
                       default.units = "npc", name = NULL,
                       text.gp = grid::gpar(), rect.gp = grid::gpar(fill = "white"), vp = NULL) {

  stopifnot(length(label) == 1)

  if (!grid::is.unit(x))
    x <- grid::unit(x, default.units)
  if (!grid::is.unit(y))
    y <- grid::unit(y, default.units)

  grid::gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
              name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob2")
}
#' @export
makeContent.labelgrob2 <- function(x) {
  hj <- grid::resolveHJust(x$just, NULL)
  vj <- grid::resolveVJust(x$just, NULL)

  t <- grid::textGrob(
    x$label,
    x$x + 2 * (0.55 - hj) * x$padding,
    x$y + 2 * (0.55 - vj) * x$padding,
    just = "center",
    gp = x$text.gp,
    name = "text"
  )

  r <- grid::roundrectGrob(x$x, x$y, default.units = "native",
                           width =  grid::unit(6, "mm"),
                           height = grid::grobHeight(t) + 1.5 * x$padding,
                           just = c(hj, vj),
                           r = x$r,
                           gp = x$rect.gp,
                           name = "box"
  )

  grid::setChildren(x, grid::gList(r, t))
}
