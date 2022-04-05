

#' Open the KOA database
#'
#' Opens the KOA database using RSQLite. The database connection must be closed before other functions in the package can be used.
#'
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A connection to the KOA database.
#' @export
#'
#' @examples
#' conn <- koa.database("upd")
#' DBI::dbDisconnect(conn)
#'
koa.database <- function(ver = "upd") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("DB", paste("koa-", ver, ".db",sep = ""), package = "koadata"))
  result <- conn
return(result)
}


#' Search by CAS number
#'
#'Create a data frame with all estimated and experimental KOA values which match the specified CAS number(s).
#'
#' @param query CAS number, with or without dashes, either a single input or a vector.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals with CAS No. matching the query.
#' @export
#' @examples
#' query.cas("50-29-3")
#' query.cas("50293")
#' query.cas("50293", more.info = TRUE)
#' query.cas(c("50-29-3","118-74-1"))
#' query.cas("50293", ver = "upd")
#' @family Queries
query.cas <- function(query,
                      more.info = F,
                      ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    CAS_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA, meth.Ranking, meth.Type,  meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
  FROM koa
  JOIN chem ON koa.chemID = chem.id
  JOIN meth ON koa.methID = meth.id
  JOIN ref ON meth.refID = ref.id
  WHERE ? in (chem.Cas_No, chem.CAS)"
      )
    CAS <- DBI::dbSendQuery(conn, CAS_sql)
    CAS <- DBI::dbBind(CAS, list(query))
  } else {
    CAS_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
        FROM koa
        JOIN chem ON koa.chemID = chem.id
        JOIN meth ON koa.methID = meth.id
        JOIN ref ON meth.refID = ref.id
        WHERE ? in (chem.Cas_No, chem.CAS)"
      )
    CAS <- DBI::dbSendQuery(conn, CAS_sql)
    CAS <- DBI::dbBind(CAS, list(query))

  }
  result <- DBI::dbFetch(CAS)
  DBI::dbClearResult(CAS)
  DBI::dbDisconnect(conn)
  return(result)
}



#' Search by method type
#'
#'Create a data frame with all KOA values obtained using a specific type of method. Method types include: "Dynamic", "Static", "Indirect", or "EST".
#'
#' @param query The type of method.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals with CAS No. matching the query.
#' @export
#' @examples
#' query.meth.type("Dynamic")
#' query.meth.type("Dynamic", more.info = TRUE)
#' @family Queries
query.meth.type <- function(query,
                      more.info = F,
                      ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    meth_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA, meth.Ranking, meth.Type,  meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
  FROM koa
  JOIN chem ON koa.chemID = chem.id
  JOIN meth ON koa.methID = meth.id
  JOIN ref ON meth.refID = ref.id
  WHERE ? in (meth.Method_Type)"
      )
    meth <- DBI::dbSendQuery(conn, meth_sql)
    meth <- DBI::dbBind(meth, list(query))
  } else {
    meth_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
        FROM koa
        JOIN chem ON koa.chemID = chem.id
        JOIN meth ON koa.methID = meth.id
        JOIN ref ON meth.refID = ref.id
        WHERE ? in (meth.Method_Type)"
      )
    meth <- DBI::dbSendQuery(conn, meth_sql)
    meth <- DBI::dbBind(meth, list(query))

  }
  result <- DBI::dbFetch(meth)
  DBI::dbClearResult(meth)
  DBI::dbDisconnect(conn)
  return(result)
}



#' Search by method
#'
#'Create a data frame with all KOA values obtained using a specific method. Method types include: "FM", "QSPR", "Dy-GLC-RT", "HS", "RT", "droplet", "Eqbm", "Solvation", "VP", "GasSol", or "GS".
#'
#' @param query The specific method.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals with CAS No. matching the query.
#' @export
#' @examples
#' query.meth("FM")
#' query.meth("Eqbm", more.info = TRUE)
#' @family Queries
query.meth <- function(query,
                            more.info = F,
                            ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    meth_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA, meth.Ranking, meth.Type,  meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
  FROM koa
  JOIN chem ON koa.chemID = chem.id
  JOIN meth ON koa.methID = meth.id
  JOIN ref ON meth.refID = ref.id
  WHERE ? in (meth.Method_Bin)"
      )
    meth <- DBI::dbSendQuery(conn, meth_sql)
    meth <- DBI::dbBind(meth, list(query))
  } else {
    meth_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
        FROM koa
        JOIN chem ON koa.chemID = chem.id
        JOIN meth ON koa.methID = meth.id
        JOIN ref ON meth.refID = ref.id
        WHERE ? in (meth.Method_Bin)"
      )
    meth <- DBI::dbSendQuery(conn, meth_sql)
    meth <- DBI::dbBind(meth, list(query))

  }
  result <- DBI::dbFetch(meth)
  DBI::dbClearResult(meth)
  DBI::dbDisconnect(conn)
  return(result)
}



#' Search by chemical name
#'
#'Create a data frame with all estimated and experimental KOA values which exactly match the specified chemical name(s). All entries are case sensitive.
#'
#' @param query Chemical name, either a single input or a vector. Searches against multiple synonymns included in the chemical table.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals with names exactly matching the query.
#' @export
#' @examples
#' query.name("8:2 FTOH")
#' query.name("HCB", more.info = TRUE)
#' query.name(c("8:2 FTOH", "Hexachlorobenzene"))
#' query.name("8:2 FTOH", ver = "upd")
#' @family Queries
query.name <- function(query,
                       more.info = F,
                       ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    name_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA,
                                     meth.Ranking, meth.Type, meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE ? IN (chem.Chemical_Name, chem.IUPAC_name, chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym)"
      )
    name <- DBI::dbSendQuery(conn, name_sql)
    name <- DBI::dbBind(name, list(query))
  } else {
    name_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
        FROM koa
        JOIN chem ON koa.chemID = chem.id
        JOIN meth ON koa.methID = meth.id
        JOIN ref ON meth.refID = ref.id
        WHERE ? IN (chem.Chemical_Name, chem.IUPAC_name, chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym)"
      )
    name <- DBI::dbSendQuery(conn, name_sql)
    name <- DBI::dbBind(name, list(query))
  }
  result <- DBI::dbFetch(name)
  DBI::dbClearResult(name)
  DBI::dbDisconnect(conn)
  return(result)
}




#' Search by chemical category
#'
#'Create a data frame with all estimated and experimental KOA values for chemicals which belong to the specified category. All entries are case sensitive.
#'
#' @param query Category name, either a single input or a vector.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals which belong to the category or categories matching the query.
#' @export
#'
#' @examples
#' query.category("PCB")
#' query.category(c("Terpene", "PBDE"))
#' query.category(c("Terpene", "PBDE"), more.info = TRUE)
#' query.category("PCB", ver = "upd")
#' @family Queries

query.category <- function(query,
                           more.info = F,
                           ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    cat_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA, meth.Ranking,
                meth.Type, meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE chem.Category = ?"
      )
    cat <- DBI::dbSendQuery(conn, cat_sql)
    cat <- DBI::dbBind(cat, list(query))
  } else {
    cat_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE chem.Category = ?"
      )
    cat <- DBI::dbSendQuery(conn, cat_sql)
    cat <- DBI::dbBind(cat, list(query))

  }
  result <- DBI::dbFetch(cat)
  DBI::dbClearResult(cat)
  DBI::dbDisconnect(conn)
  return(result)
}


#' Search by chemical mass
#'
#'Create a data frame with all estimated and experimental KOA values for chemicals which have a molar mass between the two values in the query.
#'
#' @param lower_limit Lower mass limit.
#' @param upper_limit Upper mass limit.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data for chemicals which have a mass between the range specified in the query.
#' @export
#'
#' @examples
#' query.mass(350, 400)
#' query.mass(200, 350, more.info = TRUE)
#' query.mass(350, 400, ver = "upd")
#' @family Queries

query.mass <- function(lower_limit, upper_limit, ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    mass_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE chem.Molar_Mass BETWEEN {lower_limit} AND {upper_limit}"
      )
    mass <- DBI::dbSendQuery(conn, mass_sql)
  } else {
    mass_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE chem.Molar_Mass BETWEEN {lower_limit} AND {upper_limit}"
      )
    mass <- DBI::dbSendQuery(conn, mass_sql)
  }
  result <- DBI::dbFetch(mass)
  DBI::dbClearResult(mass)
  DBI::dbDisconnect(conn)
  return(result)
}



#' Search by citation
#'
#' Create a data frame with all estimated and experimental KOA values from the citations in the query.
#'
#' @param query  Citation, either a single input or a vector. Use function ref.table() to get a list of citations in the database.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data from the citations included in the query.
#' @export
#'
#' @examples
#' query.citation("Lei et al. 2019")
#' query.citation(c("Hussam and Carr 1985", "Lei et al. 2019"))
#' query.citation("Lei et al. 2019", more.info = TRUE, ver = "upd")
#' @family Queries

query.citation <- function(query,
                           more.info = F,
                           ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    citation_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE ref.Citation = ?"
      )
    citation <- DBI::dbSendQuery(conn, citation_sql)
    citation <- DBI::dbBind(citation, list(query))
  } else {
    citation_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE ref.Citation = ?"
      )
    citation <- DBI::dbSendQuery(conn, citation_sql)
    citation <- DBI::dbBind(citation, list(query))
  }
  result <- DBI::dbFetch(citation)
  DBI::dbClearResult(citation)
  DBI::dbDisconnect(conn)
  return(result)
}

#' Search by group
#'
#' Create a data frame with all estimated and experimental KOA values from the research groups in the query.
#'
#' @param query Group name, either a single input or a vector.
#' @param more.info When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).
#' @param ver Version of the database. Default is upd, for the latest version.
#'
#' @return A data frame containing KOA data from the groups included in the query.
#' @export
#'
#' @examples
#' query.group("Harner")
#' query.group(c("Harner", "Chen", "Odabasi"))
#' query.group(c("Harner", "Chen", "Odabasi"), more.info = TRUE)
#' query.group("Harner", ver = "upd")
#' @family Queries
query.group <- function(query,
                        more.info = F,
                        ver = "upd") {
  conn <-
    DBI::dbConnect(RSQLite::SQLite(),
                   system.file("DB", paste("koa-", ver, ".db", sep = ""), package = "koadata"))
  if (more.info == F) {
    group_sql <-
      glue::glue_sql(
        "SELECT chem.Cas_No, chem.Chemical_Name, koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Bin, ref.Citation, koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE ref.Team = ?"
      )
    group <- DBI::dbSendQuery(conn, group_sql)
    group <- DBI::dbBind(group, list(query))
  } else {
    group_sql <-
      glue::glue_sql(
        "SELECT chem.Category, chem.Cas_No, chem.Chemical_Name, chem.IUPAC_name,
                chem.Alt_Chem_Name, chem.Acronym, chem.Alt_Acronym, chem.Molar_Mass,
                koa.Temp, koa.log_KOA,
                meth.Ranking, meth.Type, meth.Method_Type, meth.Gen_Method, meth.Method_Bin,
                meth.wet_dry_Octanol, meth.Value_Reported_As,
                ref.Citation, ref.Team,
                koa.Reliability, koa.Comment
         FROM koa
         JOIN chem ON koa.chemID = chem.id
         JOIN meth ON koa.methID = meth.id
         JOIN ref ON meth.refID = ref.id
         WHERE ref.Team = ?"
      )
    group <- DBI::dbSendQuery(conn, group_sql)
    group <- DBI::dbBind(group, list(query))
  }
  result <- DBI::dbFetch(group)
  DBI::dbClearResult(group)
  DBI::dbDisconnect(conn)
  return(result)
}


