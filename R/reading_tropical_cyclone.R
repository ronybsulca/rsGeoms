#' reading_tropical_cyclone
#'
#' This function correctly reads the The Tropical Cyclone Extended Best Track Dataset.
#'  **NOTE: This function was Created by the team behind COursera's Building Data Visualization Tools course.
#'
#' @param filename Either a path to a file, or a connection.
#'                 Files starting with http://, https://, ftp://, or ftps:// will be automatically downloaded.
#'
#' @importFrom readr read_fwf
#'
#' @return This function returns the raw Tropical Cyclone Data, in \code{tbl_df} format.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ext_tracks <-reading_tropical_cyclone(filename = "data/ebtrk_atlc_1988_2015.txt")
#'
#' dataset_url <- "http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt"
#' ext_tracks <- reading_tropical_cyclone(filename = dataset_url)
#'
#' }
#'
#' @export
#'

reading_tropical_cyclone <- function(filename){

    ## Fixed Widths for each column
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

    ## Names of Each column
    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                             "hour", "year", "latitude", "longitude",
                             "max_wind", "min_pressure", "rad_max_wind",
                             "eye_diameter", "pressure_1", "pressure_2",
                             paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final")

    ext_tracks <- readr::read_fwf(filename,
                                  fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                  na = "-99")

}
