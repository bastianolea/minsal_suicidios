library(dplyr)
library(ggplot2)
library(ggforce)
library(ggblend)
library(scales)
library(tidyr)

source("funciones.R")

# datos ----
datos <- arrow::read_parquet("datos/minsal_suicidios.parquet")

conteo <- datos |> 
  summarize(valor = sum(valor), 
            .by = c(genero, condicion)) |> 
  mutate(porcentaje = valor / sum(valor),
         .by = genero) |> 
  mutate(maximo = max(valor),
         .by = genero)


# colores ----
color_base <- "#CA1F7B"

color <- list(femenino = color_base,
              masculino = color_base |> col_darker(5) |> col_saturate(-30) |> col_shift(-30),
              fondo = color_base |> col_darker(38) |> col_saturate(-45),
              texto = color_base |> col_lighter(35) |> col_saturate(-50),
              detalle = color_base |> col_darker(20) |> col_saturate(-40),
              tenue = color_base |> col_darker(30) |> col_saturate(-45))

# previsualizar paleta
# show_col(unlist(color))


# temas ----

# tema
theme_set(
  theme_minimal(ink = color$texto,
                paper = color$fondo,
                base_family = "Rubik") +
    theme(legend.position = "none") +
    theme(panel.grid = element_line(linetype = "dashed", color = color$tenue),
          panel.grid.minor = element_blank())
)

# tipografía desde google fonts
library(showtext)
font_add_google("Rubik", "Rubik")
showtext_auto()
showtext_opts(dpi = 200)

# opciones de formato de números
number_options(big.mark = ".",
               decimal.mark = ",")



# gráficos ----

## intentos ----
conteo |> 
  filter(condicion == "intento") |>
  grafico_circulos_encima()


## consumados ----
conteo |> 
  filter(condicion == "consumado") |>
  grafico_circulos_encima()


## unidos ----
datos_conteo <- conteo |> 
  mutate(horizontal = case_when(
    condicion == "intento" & genero == "femenino" ~ 1-max(valor)-200,
    condicion == "intento" & genero == "masculino" ~ 1+maximo+200,
    condicion == "consumado" & genero == "femenino" ~ 1-200,
    condicion == "consumado" & genero == "masculino" ~ 1+valor+50)) |>
  mutate(condicion = forcats::fct_relevel(condicion, "consumado", "intento", )) |>
  mutate(vertical = case_match(
    genero,
    "femenino" ~ 1,
    "masculino" ~ 1)) 


datos_conteo |> 
  grafico_circulos_unidos()

diferencia_consumados <- datos_conteo |> 
  filter(condicion == "consumado") |> 
  select(genero, valor) |> 
  pivot_wider(names_from = genero, values_from = valor) |> 
  mutate(diferencia = masculino - femenino) |> 
  pull(diferencia)

diferencia_intentos <- datos_conteo |> 
  filter(condicion == "intento") |> 
  select(genero, valor) |> 
  pivot_wider(names_from = genero, values_from = valor) |> 
  mutate(diferencia = femenino - masculino) |> 
  pull(diferencia)

posicion_intentos <- datos_conteo |> 
  filter(genero == "femenino",
         condicion == "intento") |> 
  pull(horizontal)

# grafico_unidos +
#   annotate("text", x = I(0), y = I(0.6), 
#            hjust = 0,
#            family = "Rubik",
#            label = "Intentos de suicidio:")
#   annotate("text", x = 0, y = -diferencia_intentos/2, label = "Diferencia en intentos de suicidio", size = 4, fontface = "bold", color = color$texto)



## años ----
# devtools::install_github("hrbrmstr/ggalt")
# library(ggalt)

datos_año <- datos |> 
  group_by(año, genero, condicion) |> 
  summarize(valor = sum(valor), .groups = "drop") |> 
  mutate(condicion = recode_values(condicion,
                                   "intento" ~ "Intentos de suicidio",
                                   "consumado" ~ "Suicidios consumados"))

# intentos
datos_año |> 
  filter(condicion == "Intentos de suicidio") |>
  grafico_lineas() +
  geom_text(data = ~filter(.x, año %in% c(2022)),
            aes(label = number(valor)),
            nudge_y = 180,
            size = 4, fontface = "bold") |> 
  ggblend::copy_over(color = color$texto, alpha = 0.4)

# consumados
datos_año |> 
  filter(condicion == "Suicidios consumados") |>
  grafico_lineas() +
  scale_y_continuous(labels = number,
                     breaks = pretty_breaks(n = 8),
                     expand = expansion(c(0.07, 0.07))) +
  geom_text(data = ~filter(.x, año %in% c(2022, 2023)),
            aes(label = number(valor)),
            nudge_y = 1,
            size = 4, fontface = "bold") |> 
  ggblend::copy_over(color = color$texto, alpha = 0.4)




# causas ----
datos_diag <- arrow::read_parquet("datos/minsal_suicidios_diagnostico.parquet")

# datos_diag |> 
#   count(glosa_categoria, sort = T) |> 
#   print(n=Inf)

# procesar_



datos_diag <- datos_diag |> 
  limpiar_causas()

datos_diag |> 
  count(glosa, sort = T)

datos_diag |> 
  filter(condicion_egreso == "consumado") |>
  count(genero, glosa, sort = T) |> 
  pivot_wider(names_from = genero, values_from = n, values_fill = 0) |> 
  print(n=Inf)

datos_diag |> 
  filter(condicion_egreso == "consumado") |>
  count(genero, glosa, sort = T) |> 
  group_by(genero) |> 
  mutate(p = n / sum(n)) |> 
  select(-n) |> 
  pivot_wider(names_from = genero, values_from = p, 
              values_fill = 0) |> 
  filter(masculino + femenino > 0.05) |> 
  print(n=Inf)

datos_diag_conteo <- datos_diag |> 
  filter(condicion_egreso == "consumado") |>
  count(genero, glosa, sort = T) |> 
  group_by(genero) |> 
  mutate(p = n / sum(n)) |> 
  group_by(glosa) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  filter(total > 4) |> 
  mutate(glosa = forcats::fct_reorder(glosa, n))

datos_diag_wide <- datos_diag_conteo |>
  select(-total, -p) |> 
  pivot_wider(names_from = genero, values_from = n, 
              values_fill = 0) |> 
  mutate(mayor = if_else(femenino > masculino, "femenino", "masculino")) |> 
  group_by(glosa) |> 
  mutate(max = max(femenino, masculino),
         min = min(femenino, masculino))


datos_diag_conteo |> 
  grafico_causas(datos_diag_wide)
