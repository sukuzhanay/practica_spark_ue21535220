
# a -----------------------------------------------------------------------

# i -----------------------------------------------------------------------
url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
## Load libs
install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite, leaflet, geosphere, mapsapi,xml2, mapsapi)
data <- fromJSON(url_)
## crtl + shift + m   = pipe
## option / alt + -   = assign
ds_raw <- data$ListaEESSPrecio
ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble()

# ii ----------------------------------------------------------------------
## Hecho en clase

# iii ---------------------------------------------------------------------
ds_f %>% count(rotulo) %>% view()
ds_f %>% distinct(rotulo) %>% view()
no_low_cost <- c('REPSOL','CEPSA', 'GALP','SHELL','BP','PETRONOR','AVIA','Q8', 'CAMPSA','BONAREA')
ds_low_cost <- ds_f %>% mutate(low_cost = !rotulo %in% no_low_cost)
ds_low_cost %>% view()
ds_low_cost %>% summary(precio_gasoleo_a)
precio_promedio_ccaa <- ds_low_cost %>% select(precio_gasoleo_a,precio_gasolina_95_e5,idccaa,rotulo) %>% group_by(idccaa) %>% 
  summarise(gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),  gasolina_95= mean(precio_gasolina_95_e5, na.rm = TRUE),
            gasolina_91= mean(precio_gasolina_95_e5, na.rm = TRUE),gasolina_93= mean(precio_gasolina_95_e5, na.rm = TRUE))
write_excel_csv(precio_promedio_ccaa,"promedios_por_ccaa.xls")
ds_low_cost %>% count(horario, sort = TRUE)
no_24h <- ds_low_cost %>% filter(horario == 'L-D: 24H') %>% select(!horario)



# PRediccion de estaciones que cerraran -----------------------------------

pob2000 <- read_excel("pobmun/pobmun00.xls", skip = 1)
pob2000


pob <- readxl::read_excel('pobmun21.xlsx', skip = 1)
pob_def <- pob %>% select(NOMBRE,POB21) %>% clean_names() %>% rename(municipio=nombre)
ds_w_pob <- inner_join(ds_low_cost,pob_def,by="municipio")
top_ten <-no_24h %>% select(latitud,longitud_wgs84,municipio, rotulo) %>% filter(municipio == 'Alcobendas')
top_ten
top_ten %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)

ds_w_pob %>% select(pob21,municipio) %>% filter(pob21>=15000) %>%count(pob21,municipio, sort = TRUE) %>%  view()
uem <- c(-3.919257897378161, 40.373942679873714)

gasos_villa <-ds_w_pob %>%  filter(municipio=="Villaviciosa de Odón")

distancias_villa <- ds_w_pob %>% filter(municipio=="Villaviciosa de Odón") %>% 
  select(longitud_wgs84,latitud) %>% distGeo(uni) %>% view()

data_set_villa <-  gasos_villa %>%  
  mutate(distancias = round(distancias_villa/1000, digits = 2)) %>% view()

direccion <- "Calle de Goya, 88, 28009 Madrid"
key= "AIzaSyBwZmpm5vyvU7lKhHH7iCpXkVq3cy_C8Jc"

mp_geocode(direccion)













                                            















