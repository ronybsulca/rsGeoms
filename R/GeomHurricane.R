#' GeomHurricane
#'
#' This Geom returns ggproto object made of a gTree of polygonGrobs. Each Grob is a circle with different radii in each circle depending on the quadrant.
#' The grobs are colored based on the \code{colour} argument. There will be one grob per wind_speed, aka \code{colour} variable. This
#' is a ggproto object, and thus it requires no Arguments once Created (those are found in the calling function).
#'
#'
#' @importFrom dplyr mutate
#' @importFrom geosphere destPoint
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom grid gList
#' @importFrom dplyr filter
#' @importFrom grid polygonGrob
#' @importFrom grid gTree
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom ggplot2 Geom
#'
#'
#' @return This function returns a Data Frame object, with row names given based on the \code{row_names} argument.
#'         It includes all the columns of the given dataset whose elements are all numeric or have
#'         a meaningful numerical equivalent.
#' @section Errors: \code{GeomHurricane} won't be able to display the Hurricane grobs if there isn't a current ggmap
#'          active for which this Geom will be apllied onto.
#'
#' @examples
#'
#' \dontrun{
#' ## Example of Creating a calling function with this Grob
#' geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
#'                            position = "identity", na.rm = FALSE,
#'                            show.legend = NA, inherit.aes = TRUE, ...) {
#'                                  ggplot2::layer(geom = GeomHurricane, mapping = mapping,
#'                                                 data = data, stat = stat, position = position,
#'                                                 show.legend = show.legend, inherit.aes = inherit.aes,
#'                                                 params = list(na.rm = na.rm, ...))
#'                            }
#'
#' }
#'
#' @export
#'

GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
                                  required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw"),
                                  default_aes = ggplot2::aes(scale_radii = 1, fill = 1, colour = 1),
                                  draw_key = ggplot2::draw_key_polygon,
                                  draw_panel = function(data, panel_scales, coord){
                                      print(data)

                                      ## Changing Radii from Nautical Miles to Meters (and Scaling)
                                      data <- data %>%
                                          dplyr::mutate(r_ne = r_ne * 1852 * scale_radii, r_se = r_se * 1852* scale_radii,
                                                        r_sw = r_sw * 1852 * scale_radii, r_nw = r_nw * 1852* scale_radii)

                                      ## Obtaining the Locations of the Edges
                                      all_locations <- data.frame() # Main Dataframe

                                      # For each wind type
                                      for (i in 1:nrow(data)) {
                                          quadrants <- data.frame() # All quadrant

                                          # for each of the 4 quadrants
                                          for (angles in list(list(0:90, "r_ne"),
                                                              list(90:180, "r_se"),
                                                              list(180:270, "r_sw"),
                                                              list(270:360, "r_nw"))){
                                              current_quadrant <-
                                                  data.frame(geosphere::destPoint(c(data[i,]$x, data[i,]$y),
                                                                                  b = angles[[1]],
                                                                                  d = data[i, angles[[2]]]),
                                                             colour = data[i,]$colour,
                                                             fill = data[i,]$fill,
                                                             group = data[i,]$group,
                                                             alpha = data[i,]$alpha,
                                                             panel = data[i,]$PANEL)
                                              ## Append the current quadrant to our list
                                              quadrants <- dplyr::bind_rows(list(quadrants, current_quadrant))
                                          }

                                          ## Append all the new quadrants to our main DataFrame
                                          all_locations <- dplyr::bind_rows(list(all_locations, quadrants))
                                      }


                                      ## Keeping the names as X and Y, for Grob consistency
                                      all_locations <- all_locations %>%
                                          dplyr::rename(x = lon, y = lat)

                                      ## transform data points
                                      coords <- coord$transform(all_locations, panel_scales)

                                      ## Construct grid polygon
                                      grobs_list = grid::gList()

                                      # Alphas will be distributed increasingly
                                      alpha_list <- data.frame(
                                          colour = data$colour,
                                          alpha = seq(.5, .8, length.out = length(data$colour))
                                      )

                                      for (elem in alpha_list$colour) { # For each wind_speed

                                          alpha = alpha_list[alpha_list$colour == elem, "alpha"]

                                          ## Filter for current wind_speed points (locations)
                                          c_df = coords %>% dplyr::filter(colour == elem)

                                          ## Create The Grob Oject
                                          current_grob = grid::polygonGrob(
                                              x= c_df$x,
                                              y = c_df$y,
                                              gp = grid::gpar(fill = c_df$fill,
                                                              alpha = alpha,
                                                              col = c_df$colour,
                                                              group = c_df$group,
                                                              lwd = 4,
                                                              lty = 1)
                                          )
                                          ## Add the Grob t the final image
                                          grobs_list = grid::gList(grobs_list, current_grob)

                                      }

                                      ## Return A Tree of Grobs for each wind_speed
                                      grid::gTree(children = grobs_list)

                                  })

