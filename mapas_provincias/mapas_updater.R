#========= Dependencias =======
if (!("rgdal" %in% installed.packages())) {
        install.packages("rgdal")
}
if (!("ggplot2" %in% installed.packages())) {
        install.packages("ggplot2")
}
if (!("sf" %in% installed.packages())) {
        install.packages("sf")
}
if (!("rgeos" %in% installed.packages())) {
        install.packages("rgeos")
}
if (!("broom" %in% installed.packages())) {
        install.packages("broom")
}
if (!("raster" %in% installed.packages())) {
        install.packages("raster")
}
if (!("dplyr" %in% installed.packages())) {
        install.packages("dplyr")
}
`%>%` <- dplyr::`%>%` #definición del pipe 

#====== Cartografia y datos ========== 

# shp península
penin_shp <- rgdal::readOGR("mapas_provincias/cartografia/se89_10_admin_prov_a_x.shp", encoding = "UTF-8") 
# shp canarias
canarias_shp <- rgdal::readOGR("mapas_provincias/cartografia/se89_10_admin_prov_a_y.shp", encoding = "UTF-8") 

shp <- raster::union(penin_shp, canarias_shp) #Unir shapes
shp@data$id <- row.names(shp@data) #hacer explícito el id

#Deja solo ine_code en datos
shp@data <- shp@data %>% dplyr::select(id_prov, id)
names(shp)[1] <- "ine_code"

# pasar shp a data.frame 
shp_df <- broom::tidy(shp)

# id unir por id
shp_df <- dplyr::left_join(shp_df, shp@data, by = "id")

# Leer datos escovid19
df <- read.csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv", header = T, encoding = "UTF-8") #TODO Archivo grande utilizar spark 

#======= Preparación de los datos ======== 

# TODO Hacer función bucle con varias variables
fecha <- Sys.Date() - 4 # 3 días antes que suelen estar completos los datos

# Último día completo y var ia14
datos <- df %>% # TODO Renombrar variables para hacer el bucle
        dplyr::select(date, province, ine_code, ia14) %>% 
        dplyr::filter(date == fecha)

shp_df$ine_code <- as.numeric(shp_df$ine_code) #númerico como en los datos

#Union 

ifelse( 
        dim(dplyr::anti_join(datos, shp_df))[1] == 0,
        unido <- dplyr::left_join(shp_df, datos, by = "ine_code"),
        print("¡Cuidado! No se está uniéndo bien. Revisa los códigos de provincia.")
) 

#========= "acercar" Canarias 
cerca <- unido %>% 
        dplyr::mutate(lat = ifelse(lat < 30, lat + 5 + 3, lat),
                      long = ifelse(long < (-10), (long + 3), long))

# Dibujar linea para separar Canarias
lineas_canarias <- data.frame(long = c(-15, -9.5, -9.5),
                              lat =  c(34.5 + 3,  34.5 + 3, 32.5 + 3))

#Cargar tema personalizado 
# TODO ¿Cambiar el color de fondo?
tema <- function(...) {
        ggplot2::theme_minimal() +
                ggplot2::theme(
                        text = ggplot2::element_text(color = "#22211d"),
                        axis.line = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
                        # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
                        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
                        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                        panel.border = ggplot2::element_blank(),
                        legend.title = ggplot2::element_blank(),
                        legend.position = "bottom",
                        ...
                )
}

# Dibujar gráfica
# TODO Función todas las variables de interes en bucle
ggplot2::ggplot(cerca, ggplot2::aes(long, lat, group = group)) +
        ggplot2::geom_polygon(ggplot2::aes(fill = ia14), 
                              color = "gray70", size = 0.09) +
        
        # Linea de separación de Canarias
        ggplot2::geom_path(data = lineas_canarias, 
                           ggplot2::aes(long, lat, group = NULL),
                           colour = "white", size = 1.3, alpha = 0.5) +
        
        # Escala de colores viridis
        ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
        
        # Tema personalizado
        tema() +
        
        # Etiquetas 
        ggplot2::labs(title = "Incidencia Acumulada 14 días",
                      subtitle = paste("Actualización:", fecha),
                      caption = "Datos: @escovid19data | Geometría: IGN | Elaboración: @JKniffki - KStats®") -> p

ggplot2::ggsave("mapas_provincias/plots/ia14.png", p, 
                width = 39, height = 26, units = "cm") # TODO Renombrar con bucle 

