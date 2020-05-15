## Cargar librerías ====
library(tidyverse)
library(httr)
library(jsonlite)

## Cargar datos ====
load("fixtures_goals_de.Rdata")
### Cada fila corresponde a un gol

## Etiquetar rango de tiempo según minuto ====
fixtures_goals <- fixtures_goals %>% 
  mutate(time_range = case_when(
    elapsed <= 15 ~ "1-15", 
    elapsed > 15 & elapsed <= 30 ~ "16-30", 
    elapsed > 30 & elapsed <= 45 ~ "31-45", 
    elapsed > 45 & elapsed <= 60 ~ "46-60", 
    elapsed > 60 & elapsed <= 75 ~ "61-75", 
    elapsed > 75 ~ "76-90"
  ))

## Indicar tipo de letra ====
windowsFonts(SegoeUI = windowsFont("Segoe UI"))

## Indicar información de texto y colores ====
title_text <- "Goals per 15-min time range"
subtitle_text <- "Bundesliga 19/20. Up to GW25"
legend_name <- "Time range"
caption_text <- "Source: api-football.com. @tavo_cera"
background <- "#0A0920"
text_color <- "whitesmoke"
lines_color <- "#004845"

## Crear gráfico ====
fixtures_goals %>% 
  ## Recortar nombre para que se ajuste a las dimensiones del gráfico
  mutate(teamName = ifelse(teamName == "Borussia Monchengladbach", 
                           "B. Monchengladbach", teamName)) %>% 
  ggplot() + 
  ## La variable de tiempo se usa tanto para las barras como para los colores
  aes(time_range, fill = time_range) + 
  ## Seleccionar gráfico de barras
  geom_bar() + 
  ## Remover etiquetas del eje x
  scale_x_discrete(labels = NULL) + 
  ## indicar paleta de colores para las barras
  scale_fill_brewer(palette = "PuBu", name = legend_name) + 
  ## Indicar coordenadas polares para dibujar las barras
  coord_polar() + 
  ## Replicar el gráfico para cada equipo
  facet_wrap(~teamName) + 
  ## Agregar etiquetas de texto
  labs(title = title_text, subtitle = subtitle_text, x = "", y = "", 
       caption = caption_text) + 
  ## Seleccionar tema básico
  theme_minimal() + 
  theme(
    ## Ajustar aspecto, color y tamaño del texto
    text = element_text(family = "SegoeUI", face = "bold", color = text_color), 
    axis.text = element_text(family = "SegoeUI", color = text_color, size = 7), 
    strip.text = element_text(family = "SegoeUI", face = "bold", color = text_color), 
    plot.subtitle = element_text(family = "SegoeUI", size = 10), 
    ## Cambiar color de líneas
    panel.grid = element_line(color = lines_color), 
    ## Cambiar color de fondo
    plot.background = element_rect(fill = background), 
    ## Colocar fuente y firma en el borde inferior derecho
    plot.caption.position = "plot", 
    ## Ajustar tamaño de la leyenda
    legend.key.height = unit(1, "cm")
  )

## Guardar gráfico en formato png ====
ggsave("goal_time_bundes.png")


