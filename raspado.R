                                #RASPADO
# Este script realiza un raspado de la página https://coronavirus.asturias.es/ que es el
# Portal de Transparencia en Datos de Asturias, crea un csv con el nombre 
# asturias.csv. Se ejecuta con tarea programada todos los días salvo los fines de semana y fiestas de guardar (jua jua)
# Se imputa el día actual al anterior y se calculan los casos nuevos. 

if (!("rvest" %in% installed.packages())) {
        install.packages("rvest")
}
if (!("tidyverse" %in% installed.packages())) {
        install.packages("tidyverse")
}
if (!("zoo" %in% installed.packages())) {
        install.packages("zoo")
}

library(rvest)
library(tidyverse)


#==============  Raspado =========== 
pagina <-  "https://coronavirus.asturias.es/"
html <- read_html(pagina)

html %>% 
        html_nodes(xpath = '//*[@class="numero"]') %>% 
        html_text()  -> value

value <- as.numeric(gsub("[.]", "", value)) #quita separador de miles

html %>% 
        html_nodes(xpath = '//*[@class="titulo"]') %>% 
        html_text() -> key

#=============== data.frame ========
df <- data.frame(key = key, value = value) %>% 
        spread(key, value) %>% 
        mutate(date = Sys.Date() - 1, # Los datos de hoy son los de ayer 
               province = "Asturias",
               ccaa = "Asturias, Principado de") %>% 
        select(-`Casos confirmados en personal sanitario`)

#nombres de las variables
names(df) <- c("cases_accumulated_pcr", 
               "recovered",
               "deceased",
               "hospitalized",
               "intensive_care",
               "muestras.pcr",
               "muestras.testac",
               "date", "province", "ccaa")

#Datos del drive
# datos <- read.csv("data/asturias.csv", header = T)
# names(datos) <- tolower(names(datos))
# datos$date <- lubridate::as_date(datos$date)
# datos <- datos %>% select(names(df))
# 
# datos <- full_join(datos, df) 

#======= Datos del output por actualizar ========= 
# datos <- read.csv("data/output.csv", header = T)

#Solo tomar el último valor no NA 
datos <- datos %>% mutate(new_cases = zoo::na.locf0(cases_accumulated_pcr),
                          new_cases = new_cases - lag(new_cases),
                          new_cases = if_else(is.na(cases_accumulated_pcr), cases_accumulated_pcr, new_cases))

# Campos en el mismo orden
datos <- datos %>% 
        select(date, province, ccaa, new_cases, muestras.testac, 
               hospitalized, intensive_care, deceased, cases_accumulated_pcr, 
               muestras.pcr) %>% 
        mutate(muestras_totales = muestras.pcr + muestras.testac,
               source_name = "Gobierno de Asturias", 
               source = "https://coronavirus.asturias.es/")

# write.csv(datos, "data/output.csv", row.names = F)

#TO-DO Poner el git aquí con el cron (después del anterior TO-DO)