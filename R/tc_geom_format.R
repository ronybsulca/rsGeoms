#' tc_geom_format
#'
#' This function format, subsets, and mutates The Tropical Cyclone Extended Best Track Dataset that has been read
#' using the \code{reading_tropical_cyclone} function. It returns one row per wind_speed, per Storm Observation.
#' Individual Storm observations ae characterized by their wind_radius (per direction), and storm metadata.
#'
#' @param dataset A dataframe object. Must be read using the \code{reading_tropical_cyclone} function, or share the same format
#'                as data read that way.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#'
#' @return This function returns a Data Frame object of The Tropical Cyclone Extended Best Track Dataset, formatted
#'         down to N rows per Storm Observation (Where N represents each available wind_speed).
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Reading Dataset
#' data_tc <- reading_tropical_cyclone(filename = "data/ebtrk_atlc_1988_2015.txt")
#' ext_tracks <- tc_geom_format(data_tc) # Formatting
#'
#' }
#'
#' @export
#'


tc_geom_format <- function(dataset){
    dataset <- dataset %>%
        dplyr::mutate(storm_id = paste0(storm_name, '-', year), # Creating Unique Observation ID
                      longitude = ifelse(longitude > 180, -360 + longitude, longitude), # Changing Units
                      date = paste(paste(year, month, day, sep = "-"), # Combining Date Parts
                                   paste(hour, "00", "00", sep = ":") )) %>%
        dplyr::select(storm_id,date, latitude, longitude, 15:26) %>%
        tidyr::gather(key, value, -c(storm_id, date, latitude, longitude)) %>% # Gathering all of the wind_radii
        tidyr::separate(key, into = c("radius", "wind_speed", "direction")) %>% # Separating Wind_radii into multiple sections
        dplyr::select(-radius) %>%
        tidyr::spread(direction, value) # Spreading wind_radii back as columns
    return(dataset)

}
