#' @importFrom dplyr %>%
NULL

#' Get source for database
#'
#' @param db_name The name of the database for putio
#'
#' @export
get_db_src <- function(db_name) {
  credentials <- readr::read_lines(system.file("secret", package = "putdatio"))
  pool <- arutil::cache_computation(db_name, {
    pool::dbPool(
      RMySQL::MySQL(),
      dbname = db_name,
      host = credentials[1],
      username = "root",
      password = credentials[2])
  })

  pool::src_pool(pool)
}
