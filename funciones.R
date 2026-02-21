# funciones de temas
escala_genero <- function() {
  scale_color_manual(values = c("femenino" = color$femenino, 
                                "masculino" = color$masculino),
                     aesthetics = c("fill", "color"))
}

tema_sin_fondo <- function() {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = color$fondo, linewidth = 0),
        plot.background = element_rect(fill = color$fondo, linewidth = 0))
}

tema_sin_ejes <- \() {
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0))
}

grafico_circulos_encima <- function(datos) {
  # browser()
  datos |> 
    mutate(horizontal = case_match(genero,
                                   "femenino" ~ 1,
                                   "masculino" ~ max(valor))) |>
    mutate(vertical = case_match(genero,
                                 "femenino" ~ 1,
                                 "masculino" ~ 1)) |> 
    ggplot() +
    geom_circle(
      aes(x0 = horizontal, y0 = vertical, 
          r = valor,
          fill = genero, color = genero),
      linewidth = .3, alpha = 0.9
    ) |> blend("multiply") +
    geom_text(
      aes(x = horizontal, y = vertical, label = number(valor)),
      size = 5, fontface = "bold", color = color$fondo, alpha = 0.8
    ) +
    geom_text(
      aes(x = horizontal, y = vertical-(max(horizontal)*0.08), label = genero),
      size = 3, fontface = "plain", color = color$fondo, alpha = 0.8
    ) +
    coord_fixed() +
    escala_genero() +
    tema_sin_fondo() +
    tema_sin_ejes()
}


grafico_circulos_unidos <- function(datos) {
  datos |> 
    ggplot() +
    list(
      # círculos
      list(
        geom_circle(
          data = ~filter(.x, condicion == "intento"),
          aes(x0 = horizontal, y0 = vertical, r = valor,
              fill = genero, color = genero,
              alpha = condicion),
          linewidth = 0
        ),
        geom_circle(
          data = ~filter(.x, condicion != "intento"),
          aes(x0 = horizontal, y0 = vertical, r = valor,
              fill = genero, color = genero,
              alpha = condicion),
          linewidth = 0.4
        ) 
      ) |> blend("multiply"),
      # líneas horizontales
      geom_hline(
        aes(yintercept = valor, color = genero),
        alpha = 0.5),
      geom_hline(
        aes(yintercept = -valor, color = genero),
        alpha = 0.5)
    ) |> blend("overlay") +
    # bordes
    geom_circle(
      data = ~filter(.x, condicion != "intento"),
      aes(x0 = horizontal, y0 = vertical, r = valor,
          fill = genero, color = genero,
          alpha = condicion),
      fill = NA, linewidth = 0.3
    ) |> blend(alpha = 0.4) +
    # recorte
    coord_fixed(xlim = c(-4400, 4000),
                ylim = c(-3000, 3000),
                expand = FALSE) +
    escala_genero() +
    scale_alpha_manual(values = c("intento" = 0.5, "consumado" = 1)) +
    tema_sin_fondo() +
    tema_sin_ejes()
}


grafico_lineas <- function(datos) {
  datos |> 
    ggplot() +
    aes(x = año, y = valor, color = genero) +
    scale_x_continuous(breaks = seq(min(datos$año), max(datos$año), 1),
                       expand = expansion(c(0.02, 0.1))) +
    scale_y_continuous(labels = number,
                       expand = expansion(c(0.07, 0.07))) +
    # facet_wrap(~condicion, scales = "free", ncol = 1) +
    geom_smooth(se = FALSE, linetype = "solid", linewidth = 1.5, lineend = "round") +
    geom_point(data = ~filter(.x, año == max(año)), size = 6, color = color$fondo) +
    geom_point(data = ~filter(.x, año == max(año)), size = 4) +
    geom_text(data = ~filter(.x, año == max(año)),
              aes(label = number(valor)),
              nudge_x = 0.08,
              size = 4, fontface = "bold", hjust = 0) |> 
    ggblend::copy_over(color = color$texto, alpha = 0.4) +
    theme(legend.position = "none") +
    escala_genero() +
    coord_cartesian(clip = "off") +
    labs(y = "Casos", x = NULL) +
    theme(axis.text.x = element_text(face = "bold", color = color$texto, size = 11, vjust = 0),
          axis.title.y = element_text(vjust = 1, margin = margin(r = 4))) +
    theme(axis.line = element_line(color = color$detalle, lineend = "round", linewidth = 1))
}

limpiar_causas <- function(datos) {
  datos |> 
    mutate(glosa = str_replace(glosa_categoria, "autoinfligido intencionalmente por, y exposición (al|a)", "por")) |> 
    mutate(glosa = str_remove(glosa, "autoinfligida intencionalmente ")) |> 
    mutate(glosa = str_remove_all(glosa, "Lesión por|Envenenamiento por "),
           glosa = str_remove_all(glosa, ", y (los|las) no especificad(a|o)s|, no clasificadas en otra parte"),
           glosa = str_remove_all(glosa, "antiepilépticas,|, antiparkinsonianas"),
           glosa = str_remove_all(glosa, "^otr(a|o)s"),
           glosa = str_replace(glosa, "otras armas", "armas"),
           glosa = str_replace(glosa, "medios especificados", "medios"),
           glosa = str_squish(glosa) |> str_trim()) |> 
    mutate(glosa = str_to_sentence(glosa))
}

grafico_causas <- function(datos_diag_conteo, datos_diag_wide) {
  datos_diag_conteo |> 
    ggplot() +
    aes(x = n, y = glosa, color = genero) +
    # segmentos uniendo los puntos
    geom_segment(data = datos_diag_wide,
                 aes(x = min, xend = max, 
                     y = glosa, yend = glosa,
                     color = mayor), 
                 inherit.aes = F, size = 1.3, alpha = .5) +
    # textos
    geom_text(data = datos_diag_wide,
              aes(x = max, y = glosa, color = mayor, label = number(max)),
              nudge_x = 3, hjust = 0,
              size = 3, fontface = "bold") |> 
    ggblend::copy_over(color = color$texto, alpha = 0.4) +
    geom_text(data = datos_diag_wide,
              aes(x = min, color = menor, y = glosa, label = number(min)),
              nudge_x = -3, hjust = 1,
              size = 3, fontface = "bold") |> 
    ggblend::copy_over(color = color$texto, alpha = 0.4) +
    # punto transparente
    geom_point(size = 8, alpha = 0.3) +
    # punto normal
    geom_point(size = 4) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 36)) +
    scale_x_continuous(labels = number, 
                       breaks = seq(0, 50, by = 10)) +
    theme(axis.text.y = element_text(color = color$texto,
                                     lineheight = 1),
          axis.title.y = element_text(color = "#9F8B97"),
          axis.title.x = element_text(vjust = 0, margin = margin(t = 4)),
          panel.grid.major.x = element_line(linetype = "solid")) +
    theme(legend.position = "none") +
    labs(y = "Lesiones autoinflingidas intencionalmente",
         x = "Cantidad de víctimas letales por género")
}
