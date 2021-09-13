

#' Get a list of Citations
#'
#' Creates a list of citations included in the database.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing citations for all references included the database.
#' @export
#'
#' @examples
#' get.citations()
#'
get.citations <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  ref_sql <-
     glue::glue_sql(
       "SELECT *
       FROM ref")
  ref_sql <- DBI::dbSendQuery(conn, ref_sql)
  ref_table <- DBI::dbFetch(ref_sql)
  DBI::dbClearResult(ref_sql)
  DBI::dbDisconnect(conn)
  result <- unique(dplyr::select(ref_table, Citation))
  return(result)
}



#' Get a list of Groups
#'
#' Creates a list of research groups included in the database.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing research groups included the database.
#' @export
#'
#' @examples
#' get.groups()
#'
get.groups <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  ref_sql <-
    glue::glue_sql(
      "SELECT *
       FROM ref")
  ref_sql <- DBI::dbSendQuery(conn, ref_sql)
  ref_table <- DBI::dbFetch(ref_sql)
  DBI::dbClearResult(ref_sql)
  DBI::dbDisconnect(conn)
  result <- unique(dplyr::select(ref_table, Team))
  return(result)
}

#' Get a list of Chemical Categories
#'
#' Creates a list of chemical categories included in the database. Note this is not comprehensive.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing the categories for all chemicals included the database.
#' @export
#'
#' @examples
#' get.categories()
#'
get.categories <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  chem_sql <-
    glue::glue_sql(
      "SELECT *
       FROM chem")
  chem_sql <- DBI::dbSendQuery(conn, chem_sql)
  chem_table <- DBI::dbFetch(chem_sql)
  DBI::dbClearResult(chem_sql)
  DBI::dbDisconnect(conn)
  result <- unique(dplyr::select(chem_table, Category))
  return(result)
}



