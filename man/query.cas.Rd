% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchdatabase.R
\name{query.cas}
\alias{query.cas}
\title{Search by CAS number}
\usage{
query.cas(query, more.info = F, ver = "upd")
}
\arguments{
\item{query}{CAS number, with or without dashes, either a single input or a vector.}

\item{more.info}{When FALSE (default), returns fewer columns from the chemical, methods, and reference tables. When TRUE, additional details on the chemical (category, synonyms, molar mass), methods (wet/dry octanol, type of value reported, type of method), and reference (group/team).}

\item{ver}{Version of the database. Default is upd, for the latest version.}
}
\value{
A data frame containing KOA data for chemicals with CAS No. matching the query.
}
\description{
Create a data frame with all estimated and experimental KOA values which match the specified CAS number(s).
}
\examples{
query.cas("50-29-3")
query.cas("50293")
query.cas("50293", more.info = TRUE)
query.cas(c("50-29-3","118-74-1"))
query.cas("50293", ver = "upd")
}
\seealso{
Other Queries: 
\code{\link{query.category}()},
\code{\link{query.citation}()},
\code{\link{query.group}()},
\code{\link{query.mass}()},
\code{\link{query.meth.type}()},
\code{\link{query.meth}()},
\code{\link{query.name}()}
}
\concept{Queries}
