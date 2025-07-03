#### Paleta de colors
paleta_ivalua <- c("#002C4B", "#40617a", "#4A5C83", "#6782a1",
                   "#74b2c0", "#81c6d0","#bcdfe4", "#e5e4e4",
                   "#fac5c5", "#d72132", "#C0001B")


#### Theme pels gràfics

# Nota: perquè funcioni la tipografia "Roboto" cal utilitzar el paquet 'extrafont'.

# El primer cop que l'utilitzis, fes servir la funció font_import() per carregar
# totes els tipografies que tens instal·lades a l'ordinador (assegura't que tens la Roboto!)
# library(extrafont)
# font_import()

# Després de la primera vegada, utilitza load_fonts() per carregar les tipografies
# cada cop que obris R. 
library(extrafont)
loadfonts(quiet = TRUE)

theme_ivalua <- function(base_size = 11,
                         base_family = "Roboto",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22,
                         axis_text_x = TRUE,
                         axis_text_y = FALSE,
                         axis_titles = FALSE) {
  t <- ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) + # need to eventually change to replace()
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = base_family,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = base_family,
        face = "plain"
      ),
      plot.caption = ggplot2::element_text(
        family = base_family,
        size = base_size + 1,
        hjust = 0,
        vjust = 1.2,
        face = "plain"
      ),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text.x = ggplot2::element_text(
        face = "bold",
        size = base_size,
        angle = 0
      ),
      strip.text.y.left = ggplot2::element_text(
        face = "bold",
        size = base_size,
        angle = 0
      ),
      strip.text.y.right = ggplot2::element_text(
        face = "bold",
        size = base_size,
        angle = 0
      ),
      axis.text.x = ggplot2::element_text(
        family = base_family,
        face = "plain",
        size = base_size
      ),
      axis.text.y = ggplot2::element_text(
        family = base_family,
        face = "plain",
        size = base_size
      ),
      axis.title.x = ggplot2::element_text(
        family = base_family,
        face = "plain"
      ),
      axis.title.y = ggplot2::element_text(
        family = base_family,
        face = "plain"
      )
    )
  if(!axis_titles){
    t <- t +
      theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
  }
  if(!axis_text_y){
    t <- t +
      theme(axis.text.y = ggplot2::element_blank())
  }
  if(!axis_text_x){
    t <- t +
      theme(axis.text.x = ggplot2::element_blank())
  }
  return(t)
}



especialitats_primaria <- c(
  "PRI", "INF", "PAN", "EES", "PEF", "PMU", "AAP", "ALL",
  "UEE")


