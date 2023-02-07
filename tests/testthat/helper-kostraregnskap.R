

kostra_regnskap_aar <- function(aar = 2015, 
  data = kr_data("data", aar),
  funksjonshierarki = kr_data("funksjonshierarki", aar),
  artshierarki = kr_data("artshierarki", aar),
  data_saer = kr_data("data_saer", aar),
  artshierarki_nettinger = kr_data("artshierarki_nettinger", aar),
  artshierarki_nettinger_kasse = kr_data("artshierarki_nettinger_kasse", aar),
  stjernetabell = kr_data("stjernetabell", aar),
  formler = kr_data("formler", aar),
  ...){
  kostra_regnskap(data = data,  
                  funksjonshierarki =  funksjonshierarki,
                  artshierarki = artshierarki,
                  data_saer = data_saer,
                  artshierarki_nettinger = artshierarki_nettinger,
                  artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                  stjernetabell = stjernetabell,
                  formler = formler,
                  ...)
  
}

