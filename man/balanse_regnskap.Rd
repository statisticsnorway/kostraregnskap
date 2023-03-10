% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balanse_regnskap.R
\encoding{UTF8}
\name{balanse_regnskap}
\alias{balanse_regnskap}
\title{Balanseregnskapet}
\usage{
balanse_regnskap(
  data,
  kapittelhierarki,
  kombinasjoner = NULL,
  regioner = NULL,
  storkombinasjoner = NULL,
  kapitler = NULL,
  omfang = NULL,
  output = "standard",
  omfang_fra_input = output == "standard",
  printData = FALSE,
  lag0300 = FALSE,
  fixRegionkode = TRUE,
  handleDuplicated = "stop",
  bidrag = TRUE,
  generer_id = TRUE,
  bidrag_var = "source",
  id_var = "UUID",
  fun_id_bidrag = id_bidrag,
  fun_generer_id = uuid_generate_time
)
}
\arguments{
\item{data}{inputdata}

\item{kapittelhierarki}{kapittelhierarki}

\item{kombinasjoner}{kombinasjoner kapitler og regnskapsomfang som skal med i output}

\item{regioner}{regioner som skal med i output. Kan også gjøre bruk av *, ?, ! og –}

\item{storkombinasjoner}{kombinasjoner kapitler,regnskapsomfang og region som skal med i output}

\item{kapitler}{kapitler som skal med i output}

\item{omfang}{regnskapsomfang som skal med i output}

\item{output}{Dersom annet enn \code{"standard"} spesifiseres vil resultatet som returneres
være som resultatet fra den gamle \code{\link{BalanseRegnskapEnPeriode}}.}

\item{omfang_fra_input}{Ved \code{FALSE} benyttes kodene \code{"B"}, \code{"sbedr"} og \code{"lanefond"}.
Ved \code{TRUE} (default) benyttes alle kodene for regnskapsomfang som finnes i inputdata.
Disse kodene kan etterspørres med parameteren \code{omfang}.
I output blir \code{"A"} summen av alle inputkoder. I utgangspunktet er dette laget for å bruke \code{"C"} istedenfor \code{"B"}.}

\item{printData}{Ved TRUE printes to første og to siste rader av alle inputdataene}

\item{lag0300}{Ved TRUE kopieres region 0301 til 0300 i inputdata}

\item{fixRegionkode}{Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved endring av input)}

\item{handleDuplicated}{Parameter til \code{\link{HierarchyCompute}}.
Bare \code{"stop"} mulig  når \code{bidrag = TRUE}.}

\item{bidrag}{Ved \code{TRUE} vil det genereres tekststreng
med funksjonen spesifisert med parameteren \code{fun_id_bidrag}.}

\item{generer_id}{Ved \code{TRUE} vil det genereres nye id-er i output-data
med funksjonen spesifisert med parameteren \code{fun_generer_id}.}

\item{bidrag_var}{Navn på variabel i output som genereres når \code{bidrag = TRUE}.}

\item{id_var}{Navn på id-variabel i input/output.}

\item{fun_id_bidrag}{\code{\link{id_bidrag}} eller en tilsvarende funksjon.}

\item{fun_generer_id}{\code{\link{uuid_generate_time}} eller en tilsvarende funksjon.}
}
\value{
Data frame med samme variabler som data i input og med kapitler/regnskapsomfang i henhold til annen input
}
\description{
Fornyet versjon av  \code{\link{BalanseRegnskap}} med nye muligheter.
}
\details{
Denne funksjonen er utvidelse av den gamle funksjonen \code{\link{BalanseRegnskapEnPeriode}}.
Faktisk er nå \code{BalanseRegnskapEnPeriode} kun en wrapper til denne funksjonen.

I motsetning til \code{\link{BalanseRegnskap}} så kjører denne funksjonen bare på data for én periode.
Dersom variabelen \code{"periode"} finnes i input, blir \code{"periode"} også med i output.
Dersom \code{periode} i input ikke er unik, blir det feilmelding.

Dersom \code{fun_generer_id} settes til \code{NULL} blir output det samme som ved \code{bidrag = FALSE},
men beregningene foregår med den litt mer tungvinte bidrag-TRUE-metoden.
}
\examples{

# Her brukes data som ligger i pakken der bare noen regioner er med.

inputdata <- kr_data("balansedata")  
hierarki <- kr_data("kapittelhierarki")  

inputdata$UUID <- paste0("ed1716d4-a869-11ed-8000-", 
                          SSBtools::Number(seq_len(nrow(inputdata)), 13))

z1 <- balanse_regnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301"))
z1[37:41, ]

z2 <- balanse_regnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301"),
                       fun_id_bidrag = id_bidrag2, generer_id = FALSE)
z2[37:41, ]

z3 <- balanse_regnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301"),
                       bidrag = FALSE)
z3[37:41, ] 

z4 <- balanse_regnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301"),
                       omfang = c("B", "A" ), kapitler = c("KG3", "KG2"),
                       bidrag = FALSE, generer_id = FALSE)
z4 

dataC <- inputdata 
dataC$regnskapsomfang[dataC$regnskapsomfang == "B"] <- "C"
z5 <- balanse_regnskap(dataC, hierarki, regioner = c("2021", "2022", "0301"),
                       omfang = c("C", "A" ), kapitler = c("KG3", "KG2"),
                       bidrag = FALSE, generer_id = FALSE)
z5 
}
\seealso{
\code{\link{kostra_regnskap}}, \code{\link{HierarchyCompute}}, \code{\link{HierarchicalWildcardGlobbing}}
}
