
# a -----------------------------------------------------------------------


# i -----------------------------------------------------------------------
url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
## Load libs
install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite, leaflet)
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



pob <- readxl::read_excel('pobmun21.xlsx', skip = 1)

pob_def <- pob %>% select(NOMBRE,POB21) %>% clean_names() %>% rename(municipio=nombre)

ds_w_pob <- inner_join(ds_low_cost,pob_def,by="municipio")

top_ten <-no_24h %>% select(latitud,longitud_wgs84,municipio, rotulo) %>% filter(municipio == 'Alcobendas')

top_ten

top_ten %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)


















