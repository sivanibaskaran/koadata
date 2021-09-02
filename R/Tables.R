


#' Import Chemical Table
#'
#' Imports the chemical table from the database. The table can be subset to include general categories of chemicals.
#'
#' @param query Group name, either a single input or a list. Default is NA.
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing identifying data for chemicals in the database.
#' @export
#'
#' @examples
#' chem.table()
#' chem.table("PCB")
chem.table <- function(query = NA, ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (is.na(query)) {
    chemical_sql <-
      glue::glue_sql(
        "SELECT *
         FROM chem")
    chemical_sql <- DBI::dbSendQuery(conn, chemical_sql)
  } else {
    chemical_sql <-
      glue::glue_sql(
        "SELECT *
         FROM chem
        WHERE chem.Category = ?")
    chemical_sql <- DBI::dbSendQuery(conn, chemical_sql)
    chemical_sql <- DBI::dbBind(chemical_sql, list(query))
  }
  result <- DBI::dbFetch(chemical_sql)
  DBI::dbClearResult(chemical_sql)
  DBI::dbDisconnect(conn)
  return(result)
}

#' Import the Reference Table
#'
#' Imports the Reference table from the database.
#'
#' @param query Group name, either a single input or a list. Default is NA.
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data included the database.
#' @export
#'
#' @examples
#' ref.table()
#' ref.table("Harner")
#' ref.table("Carr", "upd")
#'
ref.table <- function(query = NA, ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
     if (is.na(query)) {
      ref_sql <-
        glue::glue_sql(
          "SELECT *
         FROM ref")
      ref_sql <- DBI::dbSendQuery(conn, ref_sql)
    } else {
      ref_sql <-
        glue::glue_sql(
          "SELECT *
         FROM ref
        WHERE ref.Team = ?")
      ref_sql <- DBI::dbSendQuery(conn, ref_sql)
      ref_sql <- DBI::dbBind(ref_sql, list(query))
    }
  result <- DBI::dbFetch(ref_sql)
  DBI::dbClearResult(ref_sql)
  DBI::dbDisconnect(conn)
  return(result)
}


#' Import the Methods Table
#'
#' Imports the Methods table from the database.
#'
#' @param query Method Type, either a single input or a list. Default is NA. Must be either: Dynamic, Static, Indirect, or EST.
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing information on the different methods used to obtain the KOA values in the database.
#' @export
#'
#' @examples
#' meth.table()
#' meth.table("Dynamic")
meth.table <- function(query = NA, ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db",sep = ""), package = "koadata"))
  if (is.na(query)) {
    meth_sql <-
      glue::glue_sql(
        "SELECT *
         FROM meth")
    meth_sql <- DBI::dbSendQuery(conn, meth_sql)
  } else {
    meth_sql <-
      glue::glue_sql(
        "SELECT *
         FROM meth
        WHERE meth.Method_Type = ?")
    meth_sql <- DBI::dbSendQuery(conn, meth_sql)
    meth_sql <- DBI::dbBind(meth_sql, list(query))
  }
  result <- DBI::dbFetch(meth_sql)
  DBI::dbClearResult(meth_sql)
  DBI::dbDisconnect(conn)
  return(result)

}



#' Import the KOA Table
#'
#' Imports the KOA table from the database.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data included the database.
#' @export
#'
#' @examples
#' koa.table()
#' koa.table(upd)
koa.table <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db",sep = ""), package = "koadata"))
  koa_sql <-
    glue::glue_sql(
      "SELECT *
         FROM koa")
  koa_sql <- DBI::dbSendQuery(conn, koa_sql)
  result <- DBI::dbFetch(koa_sql)
  DBI::dbClearResult(koa_sql)
  DBI::dbDisconnect(conn)
  return(result)
}





#' Import the Author Table
#'
#' Imports the Author table from the database.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing a list of Authors whose work has been included the database.
#' @export
#'
#' @examples
#' au.table()

au.table <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db",sep = ""), package = "koadata"))
  au_sql <-
    glue::glue_sql(
      "SELECT *
         FROM au")
  au_sql <- DBI::dbSendQuery(conn, au_sql)
  result <- DBI::dbFetch(au_sql)
  DBI::dbClearResult(au_sql)
  DBI::dbDisconnect(conn)
  return(result)
}

