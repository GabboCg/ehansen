#' Grafica Portafolios
#'
#' @import ggplot2
#' @importFrom scales percent
#'
#' @param stats_port Promedio, desviacion estandar y varianza-covarianza
#' @param min_port Portafolio minima varianza
#' @param eff_front Frontera efciente
#' @param tang_port Portafolio tangente
#' @param rf Tasa libre de riesgo
#' @param title Titulo
#' @param subtitle Subtitulo
#' @param caption Nota al pie
#'
#' @return
#' @export
#'
#' @examples
portfolio_ggplot <- function(stats_port, min_port, eff_front, tang_port, rf, title, subtitle, caption) {

  risk_free <- rf

  sharpe_ratio <- (tang_port$er - risk_free) / tang_port$sd

  gg <- ggplot() +
    geom_point(mapping = aes(eff_front$sd, eff_front$er),
               color = "#008b8b", size = 3) +
    geom_path(mapping = aes(eff_front$sd, eff_front$er),
              color = "#008b8b") +
    geom_point(mapping = aes(min_port$sd, min_port$er, color = "a"),
               fill = "#cc6633", color = "black",  size = 5, shape = 23) +
    geom_point(mapping = aes(tang_port$sd, tang_port$er, color = "b"),
               fill ="#FFCC00", color = "black", size = 5, shape = 23) +
    geom_point(mapping = aes(stats_port$Std,
                             stats_port$Mean),
               shape = 23, fill = "#cc528d", color = "black", size = 5) +
    annotate(geom = "text",
             x = stats_port$Std,
             y = stats_port$Mean,
             label = stats_port$Stocks,
             color = "Black",
             hjust = -0.25,
             vjust = -0.25) +
    geom_abline(intercept = risk_free, slope = sharpe_ratio,
                colour = "darkgreen") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#FFF1E0"),
          plot.title = element_text(color = "black", size = 20, face = "bold"),
          plot.subtitle = element_text(color = "#808080",
                                       margin = margin(0, 0, 0, b = 0.25, "cm"), size = 16),
          panel.border = element_rect(linetype = "solid", color = "grey", fill = NA),
          axis.text.x = element_text(face = "bold", color = "#a89a90", size = 12),
          axis.text.y = element_text(face = "bold", color = "#a89a90", size = 12),
          axis.title.x = element_text(face = "bold", color = "#a89a90", size = 16),
          axis.title.y = element_text(face = "bold", color = "#a89a90", size = 16),
          plot.margin = margin(1, 1.5, 0.5, 0.75, "cm")) +
    labs(title = title,
         subtitle = subtitle,
         y = expression("E(r)"),
         x = expression({sigma}),
         caption = caption)

  return(gg)

}
