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
if (!("patchwork" %in% installed.packages())) {
        install.packages("patchwork")
}
require(patchwork)
`%>%` <- dplyr::`%>%` # Definición del pipe 

#====== Cartografía y datos ========== 

# shp Península
penin_shp <- rgdal::readOGR("mapas_provincias/cartografia/se89_10_admin_prov_a_x.shp", encoding = "UTF-8") 
# shp Canarias
canarias_shp <- rgdal::readOGR("mapas_provincias/cartografia/se89_10_admin_prov_a_y.shp", encoding = "UTF-8") 

shp <- raster::union(penin_shp, canarias_shp) #Unir shapes
shp@data$id <- row.names(shp@data) # Hace  explícito el id

# Deja sólo ine_code en datos
shp@data <- shp@data %>% dplyr::select(id_prov, id)
names(shp)[1] <- "ine_code"

# Pasar shp a data.frame 
shp_df <- broom::tidy(shp)

# Unir por id
shp_df <- dplyr::left_join(shp_df, shp@data, by = "id")

# Leer datos scovid19data
df <- read.csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv", header = T, encoding = "UTF-8")  #TODO Probar cargar con fread

#======= Preparación de los datos ======== 

# TODO Hacer función bucle con varias variables
fecha <- Sys.Date() - 6 # 3 días antes que suelen estar completos los datos

# Último día completo y var ia14
datos <- df %>% # TODO Renombrar variables para hacer el bucle
        dplyr::select(date, province, ine_code, ia14, deceassed_per_100000) %>% 
        dplyr::filter(date == fecha)

shp_df$ine_code <- as.numeric(shp_df$ine_code) # Numérico como en los datos

#Unión shp_df-datos

ifelse( 
        dim(dplyr::anti_join(datos, shp_df))[1] == 0,
        unido <- dplyr::left_join(shp_df, datos, by = "ine_code"),
        print("¡Cuidado! No se está uniéndo bien. Revisa los códigos de provincia.")
) 

#========= "acercar" Canarias 
cerca <- unido %>% 
        dplyr::mutate(lat = ifelse(lat < 30, lat + 7, lat),
                      long = ifelse(long < (-10), (long + 6.3), long))

# Dibujar línea para separar Canarias
lineas_canarias <- data.frame(long = c(-14.5 + 2.7, -9.5 + 2.7, -9.5 + 2.7),
                              lat =  c(34.5 + 2, 34.5 + 2, 32.5 + 2))

#Cargar tema personalizado 
tema <- function(...) {
        ggplot2::theme_minimal() +
                ggplot2::theme(
                        text = ggplot2::element_text(color = "#22211d", size = 15),
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
                        # TODO ¿Cambiar el color de fondo?
                        plot.background = ggplot2::element_rect(fill =   "#e6e6e1", color = NA), 
                        panel.background = ggplot2::element_rect(fill =  "#e6e6e1", color = NA), 
                        legend.background = ggplot2::element_rect(fill = "#e6e6e1", color = NA),
                        panel.border = ggplot2::element_blank(),
                        legend.title = ggplot2::element_blank(),
                        legend.position = "bottom",
                        ...
                )
}

# Dibujar gráfica
# TODO Función todas las variables de interés en bucle

#===== ia14 Incidencia acumulada 14 días =====
ggplot2::ggplot(cerca, ggplot2::aes(long, lat, group = group)) +
        ggplot2::geom_polygon(ggplot2::aes(fill = ia14), 
                              color = "gray50", size = 0.3) +
        
        # Línea de separación de Canarias
        ggplot2::geom_path(data = lineas_canarias,
                           ggplot2::aes(long, lat, group = NULL),
                           colour = "gray50", size = 1.3, alpha = 0.5) +
        
        # Escala de colores viridis
        ggplot2::scale_fill_fermenter(palette = "Blues", direction = 1) +
        ggplot2::coord_equal() +
        
        # Tema personalizado
        tema(legend.key.size = ggplot2::unit(1,"line"),
             legend.key.width = ggplot2::unit(2, "cm" )) +
        
        # Etiquetas 
        ggplot2::labs(title = "Covid-19 Incidencia Acumulada 14 días",
                      subtitle = paste("Actualización:", fecha),
                      caption = "Datos: @escovid19data | Geometría: IGN | Elaboración: @JKniffki - KStats®") -> p

ggplot2::ggsave("mapas_provincias/plots/ia14.png", p, 
                width = 39, height = 26, units = "cm") # TODO Renombrar con bucle 


#====== deceassed_per_100000 Esto es sólo el mapa por el momento no se actualiza =======
# ggplot2::ggplot(cerca, ggplot2::aes(long, lat, group = group)) +
#         ggplot2::geom_polygon(ggplot2::aes(fill = deceassed_per_100000), 
#                               color = "gray50", size = 0.3) +
#         
#         # Línea de separación de Canarias
#         ggplot2::geom_path(data = lineas_canarias, 
#                            ggplot2::aes(long, lat, group = NULL),
#                            colour = "gray50", size = 1.3, alpha = 0.5) +
#         
#         # Escala de colores viridis
#         # ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
#         ggplot2::scale_fill_fermenter(direction = 1, palette = "Blues") +
#         
#         # Tema personalizado
#         tema(legend.key.size = ggplot2::unit(1,"line"),
#              legend.key.width = ggplot2::unit(2, "cm" )) +
#         
#         # Etiquetas 
#         ggplot2::labs(title = "Mortalidad COVID-19 (Fallecidos por 100 mil habitantes)",
#                       subtitle = paste("Actualización:", fecha),
#                       caption = "Datos: @escovid19data | Geometría: IGN | Elaboración: @JKniffki - KStats®") -> p2
# 
# ggplot2::ggsave("mapas_provincias/plots/fallecidos100mil.png", p2, 
#                 width = 39, height = 26, units = "cm") # TODO Renombrar con bucle 


#=============================================================
#============ Mapa con barras a un lado ======================
#=============================================================

#Mapa sin leyenda
ggplot2::ggplot(cerca, ggplot2::aes(long, lat, group = group)) +
        ggplot2::geom_polygon(ggplot2::aes(fill = deceassed_per_100000), 
                              color = "gray50", size = 0.3) +
        # Línea de separación de Canarias
        ggplot2::geom_path(data = lineas_canarias,
                           ggplot2::aes(long, lat, group = NULL),
                           colour = "gray50", size = 1, alpha = 0.5) +
        # Paleta         
        ggplot2::scale_fill_fermenter(direction = 1, palette = "Blues") +
        # aspect.ratio 
        ggplot2::coord_cartesian(xlim = c(-11.3, 3)) +
        # Barra más grande
        tema(legend.key.size = ggplot2::unit(0.9,"line"),
             legend.key.width = ggplot2::unit(2, "cm" )) -> p3

#barras
datos %>% 
        ggplot2::ggplot(ggplot2::aes(reorder(province, deceassed_per_100000), 
                   deceassed_per_100000, fill = deceassed_per_100000)) +
        ggplot2::geom_bar(stat = "identity", 
                          colour = c("#8CAEC2"),
                          position = "identity") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_fermenter(palette = "Blues", direction = 1) +
        ggplot2::geom_text(ggplot2::aes(label = deceassed_per_100000, fontface = 2), 
                           hjust = -0.05, size = 3) +
        ggplot2::theme(
                       text = ggplot2::element_text(size = 15),
                       legend.key.size = ggplot2::unit(0.9,"line"),
                       legend.key.width = ggplot2::unit(2, "cm" ),
                       legend.position = "bottom",
                       plot.background = ggplot2::element_rect(fill =   "#e6e6e1", color = NA), 
                       panel.background = ggplot2::element_rect(fill =  "#e6e6e1", color = NA), 
                       legend.background = ggplot2::element_rect(fill = "#e6e6e1", color = NA),
                       legend.title = ggplot2::element_blank()) + 
        ggplot2::scale_y_continuous(limits = c(0, max(datos$deceassed_per_100000, na.rm = T) + 7),
                                    expand = c(0.01, 0.01)) +
        ggplot2::labs(x = "", y = "") -> p4

#Composición 
p5 <- p3 | p4  
p5 <- p5 + 
        plot_annotation(title = "Mortalidad COVID-19 (Ratio fallecidos por cada 100 mil habitantes)",
                        subtitle = paste("Actualización: ", fecha, "| ", "Datos: @escovid19data"),
                        caption = "Geometría: IGN | Elaboración: @JKniffki - KStats®") +
        plot_layout(nrow = 1, widths = c(3, 2) ) & ggplot2::theme(title = ggplot2::element_text(size = 25))
        
# Guardar 
ggplot2::ggsave("mapas_provincias/plots/fallecidos.png", p5,
                width = 68, height = 33, units = "cm")
