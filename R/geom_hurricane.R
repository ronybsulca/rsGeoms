#' geom_hurricane
#'
#' This function return a Grob object to be displayed. The image will characterize a hurricane depited by the edges of each circle's
#' quadrant (possibly representing wind_raius), and the color of the circles (possibly depicting wind_speeds).
#'
#' @param data A Dataframe Object containing the data to plot.
#' @param x Column of `data` depicting the x-locations of the Storm's Center (LONGITUDE for maps).
#' @param y Column of `data` depicting the y-locations of the Storm's Center (LATITUDE for maps).
#' @param r_ne Column of `data` depicting the wind_raius of the North East Quadrant.
#' @param r_se Column of `data` depicting the wind_raius of the South East Quadrant.
#' @param r_nw Column of `data` depicting the wind_raius of the North West Quadrant.
#' @param r_sw Column of `data` depicting the wind_raius of the South West Quadrant.
#' @param fill OPTIONAL. Column of `data` which will be use to color the circles.
#' @param color OPTIONAL. Column of `data` which will be use to color the circle outlines.
#' @param scale_radii OPTIONAL. Numeric. Size scale of the drawn Hurricane.
#'
#'
#' @importFrom ggplot2 layer
#'
#' @return This function adds the hurricane image (Grob) into the current graphics device.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Data
#' ext_tracks <- reading_tropical_cyclone(filename = "data/ebtrk_atlc_1988_2015.txt")
#' ext_tracks <- tc_geom_format(ext_tracks)
#'
#' ## STORMS
#'
#' katrina <- ext_tracks %>%
#'     dplyr::filter(storm_id == "KATRINA-2005", date == "2005-08-29 12:00:00") %>%
#'     dplyr::mutate(longitude = longitude * -1) %>%
#'     as.data.frame()
#'
#' ## Maps
#'
#' ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
#'     ggmap::ggmap(extent = "device") +
#'     geom_hurricane(data = katrina,
#'                    aes(x = longitude, y = latitude,
#'                        r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                        fill = wind_speed, color = wind_speed)) +
#'     ggplot2::scale_color_manual(name = "Wind speed (kts)",
#'                                values = c("red", "orange", "yellow")) +
#'     ggplot2::scale_fill_manual(name = "Wind speed (kts)",
#'                                values = c("red", "orange", "yellow"))
#'
#' }
#'
#' @export
#'

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomHurricane, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
