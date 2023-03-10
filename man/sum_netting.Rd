% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kostra_regnskap.R
\name{sum_netting}
\alias{sum_netting}
\title{Sett av matrisekomponenter slås sammen}
\usage{
sum_netting(aij)
}
\arguments{
\item{aij}{To deler av output fra \code{\link{get_a1234}}. Dvs. enten a2 og a3 sammen eller a1 og a4 sammen.}
}
\value{
matrisekomponenter
}
\description{
Når det bare er summen av hierarki og nettinghierarki for samme datainput som skal brukes til slutt, er det fordelaktig å slå disse sammen.
}
\keyword{internal}
