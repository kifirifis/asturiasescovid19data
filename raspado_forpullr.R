#========= Append by JKniffki 
# Method 1 Source: https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/ @DadosdeLaplace
# Method 2 Source since 13-10-2020 from:
# https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/_w_0ecb484e/DATOS/TABLAS_RESUMEN/asturias_resumen.csv

# Read data from main source
asturias2 <- read.csv("https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/_w_0ecb484e/DATOS/TABLAS_RESUMEN/asturias_resumen.csv", header = T) %>% 
        #Select and rename 18 variables
        select(
                date = fecha,
                #Casos
                new_cases = casos_diarios,
                cases_accumulated = casos_acum,
                PCR = PCR_diarias,
                TestAc = TestAg_diarios,
                #Pruebas
                cases_accumulated_PCR = PCR_acum,
                Muestras.TestAc = TestAg_acum,
                muestras_totales = LAB.n_test_acum,
                #Indicadores
                IA14,
                #Fallecidos
                deceased = fallec_acum,
                fallec_diarios,
                #Capacidad hospitalaria
                intensive_care = pacientes_UCI_covid,
                hospitalized = pacientes_hospi_covid,
                ocu_UCI_covid,
                ocu_hospi_covid,
                ocu_UCI_total,
                ocu_hospi_total,
                altas24h_total_covid
        ) %>% 
        #Add 4 variables
        mutate(
                ccaa = "Asturias, Principado de",
                province = "Asturias",
                source = "https://dgspasturias.shinyapps.io/panel_de_indicadores_asturias/",
                source_name = "Dados de Laplace"
        ) %>%
        select(date, ccaa, province, everything()) %>% #Set order (David R. Advice)
        arrange(date) %>% 
        mutate(date = as.Date( date )) %>% #Into date
        
        # load data
        mutate(
                activos = NA,
                recovered = NA,
                source = "https://github.com/kifirifis/asturiasescovid19data/blob/main/data/output2.csv",
                hospitalized_new = NA,
                #TODO
                hospitalized_accumulated = NA,
                #TODO
        ) %>%  select(names(data_cases_sp_provinces))
#========= End JKniffki
