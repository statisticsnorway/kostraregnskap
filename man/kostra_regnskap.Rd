% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kostra_regnskap.R
\name{kostra_regnskap}
\alias{kostra_regnskap}
\title{Bevilgningsregnskapet}
\usage{
kostra_regnskap(
  data,
  funksjonshierarki,
  artshierarki,
  data_saer = NULL,
  artshierarki_nettinger = NULL,
  artshierarki_nettinger_kasse = NULL,
  kombinasjoner = NULL,
  regioner = NULL,
  storkombinasjoner = NULL,
  stjernetabell = NULL,
  funksjoner = NULL,
  arter = NULL,
  kontoklasser = NULL,
  formler = NULL,
  regnskapsomfang = NULL,
  useC = any(c(grepl("C", regnskapsomfang))),
  handleDuplicated = "stop",
  ...,
  output = "standard",
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

\item{funksjonshierarki}{funksjonshierarki}

\item{artshierarki}{artshierarki}

\item{data_saer}{inputdata for særbedrifter}

\item{artshierarki_nettinger}{artshierarki for særbedrift-nettinger}

\item{artshierarki_nettinger_kasse}{artshierarki for kasse-nettinger}

\item{kombinasjoner}{kombinasjoner av art, funksjon, kontoklasse som skal med i output}

\item{regioner}{regioner som skal med i output. Kan også settet til et tall eller gjøre bruk av *, ?, ! og – (se nedenfor).}

\item{storkombinasjoner}{kombinasjoner av art, funksjon, kontoklasse og region}

\item{stjernetabell}{tabell med *  og ? som definerer koder som skal med i output}

\item{funksjoner}{funksjoner som skal med i output. Man kan også bruke  *, ?, ! og – tilsvarende som for regioner (se nedenfor).}

\item{arter}{arter som skal med i output. Man kan også bruke  *, ?, ! og – tilsvarende som for regioner (se nedenfor).}

\item{kontoklasser}{kontoklasser som skal med i output. Det er mulig å bruke * istedenfor NULL til å velge alle kontoklosser.}

\item{formler}{data frame med formler som brukes til korreksjoner}

\item{regnskapsomfang}{regnskapsomfang som skal med i output. Det er mulig å bruke * og ? istedenfor NULL til å velge begge regnskapsomfang.
Man kan også spesifisere \code{"C"} istedenfor \code{"B"} istedenfor.}

\item{useC}{Ved TRUE: \code{"C"} istedenfor \code{"B"} i output.
Default er \code{FALSE} med mindre \code{C} er med i inputparameter regnskapsomfang.
Men man trenger altså ikke å bruke \code{useC}-parameteren hvis man bruker
parameteren \code{regnskapsomfang} og tar med \code{"C"} der (se eksempler).}

\item{handleDuplicated}{Parameter til \code{\link{HierarchyCompute}}.
Bare \code{"stop"} mulig  når \code{bidrag = TRUE}.}

\item{...}{Flere parametere til \code{\link{KostraRegnskapEnPeriode}}.}

\item{output}{Dersom annet enn \code{"standard"} spesifiseres vil resultatet fra
\code{\link{KostraRegnskapEnPeriode}} returneres.}

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
En data frame
}
\description{
Fornyet versjon av  \code{\link{KostraRegnskap}} med nye muligheter.
}
\details{
Denne funksjonen gjenbruker den gamle funksjonen \code{\link{KostraRegnskapEnPeriode}}
til å forbehandle input slik som i \code{\link{beredt}}.
Selve hovedberegningene foregår utenfor den gamle funksjonen.

I motsetning til \code{\link{KostraRegnskap}} så kjører denne funksjonen bare på data for én periode.
Dersom variabelen \code{"periode"} finnes i input, blir \code{"periode"} også med i output.
Dersom \code{periode} i input ikke er unik, blir det feilmelding.

Dersom \code{fun_generer_id} settes til \code{NULL} blir output det samme som ved \code{bidrag = FALSE},
men beregningene foregår med den litt mer tungvinte bidrag-TRUE-metoden.
}
\examples{
aar <- 2015  # 2016 kan også velges 
inputdata <- kr_data("data", aar)
funksjonshierarki <- kr_data("funksjonshierarki", aar)
artshierarki <- kr_data("artshierarki", aar)
inputdata_saer <- kr_data("data_saer", aar)
artshierarki_nettinger <- kr_data("artshierarki_nettinger", aar)
artshierarki_nettinger_kasse <- kr_data("artshierarki_nettinger_kasse", aar)
stjerne <- kr_data("stjernetabell", aar)
formler <- kr_data("formler", aar)


inputdata$UUID <- paste0("ce1f9682-data-data-8000-", 
                          SSBtools::Number(seq_len(nrow(inputdata)), 13))
inputdata_saer$UUID <- paste0("ca3679e0-data-saer-8000-", 
                               SSBtools::Number(seq_len(nrow(inputdata_saer)), 13))

z1 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"))
z1[1:10, 1:7]
z1[5, ]
z1[7:10, ]


# Samme, men bruker id_bidrag2 som ikke tar med belop i "source"
z2 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"), fun_id_bidrag = id_bidrag2)
z2[5, ]

# Samme uten generering av bidrag
z3 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"), bidrag = FALSE)
z3[c(3:7, 51:55),  ]


# Samme med C istedenfor B
z4 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"), bidrag = FALSE,
                      useC = TRUE)
z4[c(3:7, 51:55),  ]


# Samme med C istedenfor B på annen måte. Samt ikke generering av id. 
z5 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"), bidrag = FALSE,
                      regnskapsomfang = c("A", "C"), generer_id = FALSE)
z5[c(3:7, 51:55),  ]


# Bare C i output 
z5 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
                      artshierarki = artshierarki, data_saer = inputdata_saer, 
                      artshierarki_nettinger = artshierarki_nettinger, 
                      artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                      stjernetabell = stjerne, formler = formler, 
                      arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
                      funksjoner = c("FG2", "FG1"), bidrag = FALSE,
                      regnskapsomfang = "C", generer_id = FALSE)
z5[1:10,  ]


}
