

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
#' table.chem()
#' table.chem("PCB")
table.chem <- function(query = NA, ver = "upd") {
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
#' table.ref()
#' table.ref("Harner")
#' table.ref("Carr", "upd")
#'
table.ref <- function(query = NA, ver = "upd") {
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
#' table.meth()
#' table.meth("Dynamic")
table.meth <- function(query = NA, ver = "upd") {
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
#' table.koa()
#' table.koa(upd)
table.koa <- function(ver = "upd") {
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
#' table.au()

table.au <- function(ver = "upd") {
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





#' Import the Property Table
#'
#' Imports the Property table from the database.
#'
#' @param query Property type, either a single input or a list. Default is NA. Must be either: "log_KOW", "log_KAW", "Sw", "VP", "inf_act", "dG".
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing information on the different methods used to obtain the KOA values in the database.
#' @export
#'
#' @examples
#' table.prop()
#' table.prop("log_KOW")
#' table.prop(c("log_KOW", "log_KAW"))
table.prop <- function(query = NA, ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db",sep = ""), package = "koadata"))
  if (is.na(query)) {
    prop_sql <-
      glue::glue_sql(
        "SELECT *
         FROM prop")
    prop_sql <- DBI::dbSendQuery(conn, prop_sql)
  } else {
    prop_sql <-
      glue::glue_sql(
        "SELECT *
         FROM prop
        WHERE prop.Property = ?")
    prop_sql <- DBI::dbSendQuery(conn, prop_sql)
    prop_sql <- DBI::dbBind(prop_sql, list(query))
  }
  result <- DBI::dbFetch(prop_sql)
  DBI::dbClearResult(prop_sql)
  DBI::dbDisconnect(conn)
  return(result)

}

