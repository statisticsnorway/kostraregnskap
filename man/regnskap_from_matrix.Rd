% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kostra_regnskap.R
\name{regnskap_from_matrix}
\alias{regnskap_from_matrix}
\title{Sammenstilling av output til en data-frame}
\usage{
regnskap_from_matrix(
  matA,
  matB,
  periode,
  regnskapsomfang = NULL,
  value_var = "belop",
  kombinasjoner,
  storkOrder = NULL,
  storkombinasjoner = NULL,
  colVar = "region",
  integerInOutput,
  bidragA,
  bidragB,
  bidrag_var = "source",
  id_var = "UUID",
  fun_generer_id
)
}
\arguments{
\item{matA}{matrise med belop-output for regnskapsomfang A}

\item{matB}{matrise med belop-output for regnskapsomfang B}

\item{periode}{\code{NULL} eller periode som skal med i output}

\item{regnskapsomfang}{\code{regnskapsomfang}}

\item{value_var}{\verb{= "belop"}}

\item{kombinasjoner}{\code{kombinasjoner}}

\item{storkOrder}{Indekser som brukes når endelig output skal lages for å passe med \code{storkombinasjoner}}

\item{storkombinasjoner}{\code{storkombinasjoner}}

\item{colVar}{\verb{= "region"}}

\item{integerInOutput}{\code{TRUE} når \code{belop} i output skal omgøres til integer}

\item{bidragA}{\code{NULL} eller matrise med generert tekst for regnskapsomfang A}

\item{bidragB}{\code{NULL} eller matrise med generert tekst for regnskapsomfang A}

\item{bidrag_var}{Navn på variabel i output som genereres når \code{bidrag = TRUE}.}

\item{id_var}{Navn på id-variabel i input/output.}

\item{fun_generer_id}{\code{\link{uuid_generate_time}} eller en tilsvarende funksjon.}
}
\value{
Som output fra \code{\link{kostra_regnskap}}
}
\description{
Sammenstilling av output til en data-frame og generering av UUID
}
\details{
Utgangspunktet for funksjonen er gammel kode fra \code{\link{KostraRegnskap}}.
Koden ble skrevet for å være gjerrig på minnebruk.
Dette har gått på bekostning av enkelhet.
Egentlig er ikke det som foregår i denne funksjonen særlig avansert.
Det settes bare sammen tall/tekst fra ulike typer input. I tillegg genereres UUID.
}
\keyword{internal}
