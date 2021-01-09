library(tidyverse)
library(RSelenium)
# library(stringr)
library(xfun) 
library(xml2)
library(jsonlite)
#library(magrittr)
library(jsonlite)
library(XML)
library(httr)
library(rvest)
# library(dplyr)
# library(V8)
library(readxl)

#ANDALUCIA----

# source: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/informe/anual?CodOper=b3_2314&idNode=42348

almeria_munis <- read_xls("Andalucía/almeria.xls", skip = 13)
cadiz_munis <- read_xls("Andalucía/cadiz.xls", skip = 13)
cordoba_munis <- read_xls("Andalucía/cordoba.xls", skip = 13)
granada_munis <- read_xls("Andalucía/granada.xls", skip = 13)
huelva_munis <- read_xls("Andalucía/huelva.xls", skip = 13)
jaen_munis <- read_xls("Andalucía/jaen.xls", skip = 13)
malaga_munis <- read_xls("Andalucía/malaga.xls", skip = 13)
sevilla_munis <- read_xls("Andalucía/sevilla.xls", skip = 13)

andalucia_munis <- almeria_munis %>% 
  rbind(cadiz_munis, cordoba_munis, granada_munis, huelva_munis,
        jaen_munis, malaga_munis, sevilla_munis) %>% 
  select(municipio = `Lugar de residencia`, poblacion = Población,
         cases = `Total Confirmados`, deaths = Fallecidos, recovered = Curados) %>% 
  mutate(ccaa = "Andalucia", codmun = NA, download_Date = Sys.Date())


#2.2 ARAGON----

url <- "https://datacovid.salud.aragon.es/covid/"


mybrowser <- rsDriver(browser = "firefox")

remDr <- mybrowser[["client"]]

remDr$navigate(url)

Sys.sleep(9)


i=1

categorykey_prov <-remDr$findElement('xpath', '/html/body/div[1]/div/ul/li[3]/a')

categorykey_prov$clickElement()

aragon_munis <- tibble(municipio = NA, cases = NA, deaths = NA, recovered = NA, download_Date = Sys.Date())

for (i in 1:731){



Sys.sleep(2)

categorykey_zaragoza <-remDr$findElement('xpath', '/html/body/div[1]/div/div/div[3]/div[1]/div[3]/div[2]/div/div/div/div[1]')

categorykey_zaragoza$clickElement()

categorykey_zaragoza <-remDr$findElement('xpath', paste("/html/body/div[1]/div/div/div[3]/div[1]/div[3]/div[2]/div/div/div/div[2]/div/div[",i,"]", sep = ""))

categorykey_zaragoza$clickElement()

Sys.sleep(5)

parsed_pagesource_zaragoza<- remDr$getPageSource()[[1]]


muni_name <- xml2::read_html(parsed_pagesource_zaragoza) %>%
  rvest::html_nodes(xpath = paste("/html/body/div[1]/div/div/div[3]/div[1]/div[3]/div[2]/div/div/div/div[1]", sep = "")) %>% 
  html_text()

muni_cases <- xml2::read_html(parsed_pagesource_zaragoza) %>%
  rvest::html_nodes(css = "#totalMuni") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\\.","",.)


muni_deaths <- xml2::read_html(parsed_pagesource_zaragoza) %>%
  rvest::html_nodes(css = "#muertesMuni") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\\.","",.)



muni_recovered <- xml2::read_html(parsed_pagesource_zaragoza) %>%
  rvest::html_nodes(css = "#recuperadosMuni") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\\.","",.)

aa <- tibble(municipio = muni_name, cases = muni_cases, deaths = muni_deaths, recovered = muni_recovered, download_Date = Sys.Date())

aragon_munis <- rbind(aragon_munis, aa)

}

aragon_munis <- aragon_munis %>% 
  mutate(ccaa = "Aragón", poblacion = NA, codmun = NA, download_Date = Sys.Date())

remDr$close()

mybrowser$server$stop()

remDr$close()
rm(mybrowser)
gc()

#BAleArES----





url <- "https://covid19ib.maps.arcgis.com/apps/opsdashboard/index.html#/c1cde29b86bd431287fd4225bb4193a4"


mybrowser <- rsDriver(browser = "firefox")

remDr <- mybrowser[["client"]]

remDr$navigate(url)



parsed_pagesource_bal<- remDr$getPageSource()[[1]]


remDr$close()

mybrowser$server$stop()

remDr$close()
rm(mybrowser)
gc()


muni_bal <- xml2::read_html(parsed_pagesource_bal) %>%
  rvest::html_nodes(css = "#ember75") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\\.","",.)

baleares_munis <- str_split_fixed(muni_bal, "\n\n\n", 68) %>% 
  as.data.frame(t(.)) %>% 
  gather(key, full_label, 1:68) %>% 
  select(-key) %>% 
  mutate(full_label = trimws(full_label)) %>% 
  filter(full_label != "Seleccioni un municipi per filtrar") %>% 
  mutate(cases = gsub(" .*", "",full_label)) %>% 
  mutate(cases = gsub(",*", "",cases)) %>% 
  mutate(municipio = gsub("[0-9]","",full_label)) %>% 
  mutate(municipio = gsub(",","",municipio)) %>% 
  mutate(municipio = str_sub(municipio, 1, -7)) %>% 
  mutate(municipio = trimws(municipio)) %>% 
  mutate(recodeath = gsub(".*? ", "", full_label)) %>% 
  mutate(recodeath = gsub("\\)", "", recodeath)) %>% 
  mutate(recodeath = gsub("\\(", "", recodeath)) %>% 
  mutate(recodeath = gsub("[^[:alnum:][:space:]]", "", recodeath)) %>% 
  mutate(deaths = gsub(".*\\s+", "", recodeath)) %>% 
  mutate(recovered = gsub("\\s+.*", "", recodeath)) %>% 
  mutate(ccaa = "Baleares") %>% 
  select(ccaa, municipio, cases, recovered, deaths)

baleares_munis <- baleares_munis %>% 
  mutate(cases = as.numeric(cases), recovered = as.numeric(recovered), deaths = as.numeric(deaths), poblacion = NA, codmun = NA, download_Date = Sys.Date())


#CANARIAS----


url <- "https://www.arcgis.com/home/webmap/viewer.html?url=https://services9.arcgis.com/CgZpnNiCwFObjaOT/ArcGIS/rest/services/CVCanarias/FeatureServer&source=sd"


mybrowser <- rsDriver(browser = "firefox")

remDr <- mybrowser[["client"]]

remDr$navigate(url)


categorykey_zaragoza <-remDr$findElement('xpath', '/html/body/div[1]/div[1]/div[3]/div[3]/div/div/div[3]/div[2]/div/div[1]/div[2]/div[1]/div[1]/div/div[2]')

categorykey_zaragoza$clickElement()


parsed_pagesource_can<- remDr$getPageSource()[[1]]
parsed_pagesource_can1<- remDr$getPageSource()[1]
parsed_pagesource_can2<- remDr$getPageSource()


remDr$close()

mybrowser$server$stop()

remDr$close()
rm(mybrowser)
gc()


canarias_munis <- tibble(codmun = NA, municipio = NA, isla = NA, cases = NA, poblacion = NA, deaths = NA, recovered = NA, download_Date = Sys.Date())


for (i in 1:88) {

munis_can <- xml2::read_html(parsed_pagesource_can) %>%
  rvest::html_nodes(css = paste("#dgrid_0-row-",i, sep = ""))

codmun <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 1))
municipio <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 2))
isla <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 3))
cases <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 4))
poblacion <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 5))
deaths <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 7))
recovered <- html_text(xml_child(xml_child(xml_child(munis_can[[1]], 1), 1), 8))


aa <- tibble(codmun = codmun, municipio = municipio, isla = isla, cases = cases,
             poblacion = poblacion, deaths = deaths, recovered = recovered, download_Date = Sys.Date())

canarias_munis <- rbind(canarias_munis, aa)

}

canarias_munis1 <- canarias_munis %>% 
  select(codmun, municipio, cases, deaths, recovered, poblacion) %>% 
  mutate(cases = gsub(",", "", cases)) %>% 
  mutate(cases = as.numeric(cases)) %>% 
  mutate(deaths = gsub(",", "", deaths)) %>% 
  mutate(deaths = as.numeric(deaths)) %>% 
  mutate(recovered = gsub(",", "", recovered)) %>% 
  mutate(recovered = as.numeric(recovered)) %>% 
  mutate(poblacion = gsub(",", "", poblacion)) %>% 
  mutate(poblacion = as.numeric(poblacion), ccaa ="Canarias", download_Date = Sys.Date())


#CANTABTIA----

#fuente: https://www.icane.es/covid19/dashboard/mun-cases/home

cant_falle <- read_csv("Cantabria/Fallecidos por municipio.csv") %>% 
  mutate(deaths = Valor) %>% 
  select(municipios, deaths)
cant_casos <- read_csv("Cantabria/Casos por municipio.csv") %>% 
  mutate(cases = Valor) %>% 
  select(municipios, cases)
cant_reco <- read_csv("Cantabria/Altas por municipio.csv") %>% 
  mutate(recovered = Valor) %>% 
  select(municipios, recovered)


cantabria_munis <- cant_falle %>% 
  left_join(cant_casos) %>% 
  left_join(cant_reco) %>% 
  mutate(codmun = str_sub(municipios,1,5)) %>% 
  mutate(municipio = str_sub(municipios,9,40)) %>% 
  select(municipio, codmun,cases, deaths, recovered) %>% 
  mutate(poblacion = NA, ccaa = "Cantabria", download_Date = Sys.Date())


#C. VALENCIANA----

#fuente: https://dadesobertes.gva.es/va/dataset/covid-19-casos-confirmats-pcr-casos-pcr-en-els-ultims-14-dies-i-persones-mortes-per-municipi-2020

valen_munis <- read.csv("Valencia/covid-19-casos-confirmados-por-pcr-casos-pcr-en-los-ultimos-14-dias-y-personas-fallecidas-por-mu.csv",
                        sep = ";")


cvalenciana_munis <- valen_munis %>% 
  mutate(codmun = CodMunicipio, municipio = Municipi, cases = `Casos.PCR.`, deaths = Defuncions) %>% 
  select(codmun, municipio, cases, deaths) %>% 
  mutate(poblacion = NA, ccaa = "Comunidad de Valencia", recovered = NA, download_Date = Sys.Date())





#CATALUÑA----

catalabl <- read_xls("catalunya_codmun.xls") %>% 
  mutate(urlcod = paste(CPRO, CMUN, sep = "")) %>% 
  mutate(urlcod = str_sub(urlcod,1,5))

#url <- "https://datacovid.salud.aragon.es/covid/"


mybrowser <- rsDriver(browser = "firefox")

remDr <- mybrowser[["client"]]

#remDr$navigate(url)

#Sys.sleep(9)


#i = "08111"
catalunya_munis <- tibble(municipio = NA, cases = NA, deaths = NA, recovered = NA, codmun = NA, download_Date = Sys.Date())

for (i in catalabl$urlcod){
  url <- paste("https://dadescovid.cat/?tipus=municipi&codi=", i, sep = "")
  remDr$navigate(url)

  
  Sys.sleep(3)
  
  parsed_pagesource_catal<- remDr$getPageSource()[[1]]
  
  
  muni_name <- xml2::read_html(parsed_pagesource_catal) %>%
    rvest::html_nodes(xpath = paste("/html/body/div/div[1]/div[1]/div/div[2]/a", sep = "")) %>% 
    html_text()
  
  muni_cases <- xml2::read_html(parsed_pagesource_catal) %>%
    rvest::html_nodes(xpath = "/html/body/div/div[1]/div[2]/div[3]/div[4]/table/tbody/tr/td") %>% 
    html_text() %>% 
    gsub("\n", "", .) %>% 
    gsub("\\.","",.)
  
  
  muni_deaths <- xml2::read_html(parsed_pagesource_catal) %>%
    rvest::html_nodes(xpath = "/html/body/div/div[1]/div[2]/div[3]/div[6]/table/tbody/tr/td") %>% 
    html_text() %>% 
    gsub("\n", "", .) %>% 
    gsub("\\.","",.)
  
  codmun <- catalabl %>% 
    filter(urlcod == i) %>% 
    mutate(codmun = paste(CPRO, CMUN, sep = "")) %>% 
    select(codmun) %>% 
    pull()

  
  aa <- tibble(municipio = muni_name, cases = muni_cases, deaths = muni_deaths, recovered = NA, codmun = codmun, download_Date = Sys.Date())
  
  catalunya_munis <- rbind(catalunya_munis, aa)
  
}




catalunya_munis <- catalunya_munis %>% 
  mutate(ccaa = "Cataluña", poblacion = NA) 
  

remDr$close()

mybrowser$server$stop()

remDr$close()
rm(mybrowser)
gc()





#EUSKADI----

url <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/generated/covid19-bymunicipality.json"

tmp <- tempfile()

library(jsonlite)
library(RCurl) 

download.file(url, destfile =tmp,quiet = FALSE)
wb <- fromJSON(getURL("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/generated/covid19-bymunicipality.json", 
                      .encoding = "ISO Latin 1"))


muni_name_eus <- wb[["byMunicipalityByDate"]][["populationByMunicipalityByDate"]][["dimension"]][["officialName"]]
cod1 <- wb[["byMunicipalityByDate"]][["populationByMunicipalityByDate"]][["dimension"]][["oid"]]
cod2 <- wb[["byMunicipalityByDate"]][["populationByMunicipalityByDate"]][["dimension"]][["countyId"]]
cod3 <- wb[["byMunicipalityByDate"]][["populationByMunicipalityByDate"]][["dimension"]][["regionId"]] 
poblacion <- wb[["byMunicipalityByDate"]][["populationByMunicipalityByDate"]][["values"]] %>% 
  unlist()
cases <- wb[["byMunicipalityByDate"]][["positiveCountByMunicipalityByDate"]][["values"]] %>% 
  unlist()
deaths <- wb[["byMunicipalityByDate"]][["deceasedCountByMunicipalityByDate"]][["values"]] %>% 
  unlist()
euskadi_munis <- tibble(municipio = muni_name_eus, poblacion = poblacion, cases = cases, 
                        deaths = deaths, codmun = paste(cod2, cod1, cod3, sep = ""),
                        recovered = NA, ccaa = "Euskadi", download_Date = Sys.Date())



#TOTAL NACIONAL----

total_munis <- aragon_munis %>% 
  rbind(andalucia_munis, baleares_munis, canarias_munis1, cantabria_munis, cvalenciana_munis,
        euskadi_munis, catalunya_munis)


write_csv(total_munis, "datos_covid_municipios.csv")


#save.image("datos_diciembre.Rdata")


