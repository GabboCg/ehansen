#' Grafica Portafolios Markowitz
#'
#' @import ggplot2
#' @importFrom scales percent
#'
#' @param stats_port Promedio, desviacion estandar y varianza-covarianza
#' @param min_port Portafolio minima varianza
#' @param eff_port Portafolio eficiente sujeto a un retorno
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
portfolio_ggplot <- function(stats_port, min_port, eff_port, eff_front, tang_port, rf, title, subtitle, caption) {

  risk_free <- rf

  sharpe_ratio <- (tang_port$er - risk_free) / tang_port$sd

  gg <- ggplot() +
    geom_point(mapping = aes(eff_front$sd, eff_front$er),
               color = "#008b8b", size = 3) +
    geom_path(mapping = aes(eff_front$sd, eff_front$er),
              color = "#008b8b") +
    geom_point(mapping = aes(eff_port$sd, eff_port$er),
               fill = "#808080", color = "black",  size = 5, shape = 23) +
    geom_point(mapping = aes(min_port$sd, min_port$er),
               fill = "#cc6633", color = "black",  size = 5, shape = 23) +
    geom_abline(intercept = risk_free, slope = sharpe_ratio,
                colour = "darkgreen") +
    geom_point(mapping = aes(tang_port$sd, tang_port$er),
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

#' Grafica Portafolios
#'
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom rlang .data
#'
#' @param returns_tbl Retornos
#' @param title Titulo
#' @param subtitle Subtitulo
#' @param caption Nota al pie
#'
#' @return
#' @export
#'
#' @examples
cumulative_ggplot <- function(returns_tbl, title, subtitle, caption) {

  returns_tbl %>%
    group_by(.data$symbol) %>%
    mutate(return_cumulative = cumsum(.data$return),
           date = as.Date(.data$date)) %>%
    ungroup() %>%
    ggplot(aes(y = .data$return_cumulative,
               x = .data$date,
               color = .data$symbol)) +
    geom_line(size = 0.75) +
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
          legend.position='bottom',
          plot.margin = margin(1, 1.5, 0.5, 0.75, "cm")) +
    guides(color = guide_legend(title = "Activos:")) +
    labs(title = title,
         subtitle = subtitle,
         y = "",
         x = "",
         caption = caption)

}

#' Grafica Portafolios Black-Litterman
#' @import ggplot2
#' @importFrom scales percent
#'
#' @param stats_port Promedio, desviacion estandar y varianza-covarianza
#' @param min_port Portafolio minima varianza
#' @param eff_port Portafolio eficiente sujeto a un retorno
#' @param eff_front Frontera efciente
#' @param tang_port Portafolio tangente
#' @param bl_port Portafolio BL
#' @param rf Tasa libre de riesgo
#' @param title Titulo
#' @param subtitle Subtitulo
#' @param caption Nota al pie
#'
#' @return
#' @export
#'
#' @examples
portfolio_bl_ggplot <- function(stats_port, min_port, eff_port, eff_front,
                                tang_port, bl_port, rf, title, subtitle, caption) {

  risk_free <- rf

  sharpe_ratio <- (tang_port$er - risk_free) / tang_port$sd

  gg <- ggplot() +
    geom_point(mapping = aes(eff_front$sd, eff_front$er),
               color = "#008b8b", size = 3) +
    geom_path(mapping = aes(eff_front$sd, eff_front$er),
              color = "#008b8b") +
    geom_point(mapping = aes(eff_port$sd, eff_port$er),
               fill = "#808080", color = "black",  size = 5, shape = 23) +
    geom_point(mapping = aes(min_port$sd, min_port$er),
               fill = "#cc6633", color = "black",  size = 5, shape = 23) +
    geom_abline(intercept = risk_free, slope = sharpe_ratio,
                colour = "darkgreen") +
    geom_point(mapping = aes(tang_port$sd, tang_port$er),
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
    geom_point(mapping = aes(bl_port$sd,
                             bl_port$er),
               shape = 23, fill = "#B53737", color = "black", size = 5) +
    annotate(geom = "text",
             x = bl_port$sd,
             y = bl_port$er,
             label = "BL",
             color = "Black",
             hjust = -0.35,
             vjust = -0.35) +
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

#' Grafica Portafolios Markowitz & Black-Litterman
#' @import ggplot2
#' @importFrom scales percent
#'
#' @param log_returns_tbl Retornos ln
#' @param bl_port Portafolio BL
#' @param rf Tasa libre de riesgo
#' @param title Titulo
#' @param subtitle Subtitulo
#' @param caption Nota al pie
#'
#' @return
#' @export
#'
#' @examples
portfolio_all_ggplot <- function(log_returns_tbl, bl_port, rf, title, subtitle, caption) {

  # estadística descriptiva
  descriptive_stats <- descriptive_stats(return_tbl = log_returns_tbl)

  # preliminares portafolios
  preliminares <- preliminary_portfolio(log_returns_tbl)

  # portafolio minima varianza
  min_port <- globalMin_portfolio(er = preliminares$Mean,
                                  cov.mat = preliminares$VarCov,
                                  shorts = TRUE)

  # portafolio tangente
  tang_port <- tangency_portfolio(er = preliminares$Mean,
                                  cov.mat = preliminares$VarCov,
                                  risk.free = rf,
                                  shorts = TRUE)

  # portafolio eficiente
  eff_port <- efficient_portfolio(er = preliminares$Mean,
                                  cov.mat = preliminares$VarCov,
                                  target.return = max(preliminares$Mean),
                                  shorts = TRUE)

  # frontera eficiente
  eff_front <- efficient_frontier(er = preliminares$Mean,
                                  cov.mat = preliminares$VarCov,
                                  nport = 75,
                                  alpha.min = -21.5,
                                  alpha.max = 20.5,
                                  shorts = TRUE)

  risk_free <- rf

  sharpe_ratio <- (tang_port$er - risk_free) / tang_port$sd

  gg <- portfolio_bl_ggplot(stats_port = descriptive_stats,
                            min_port = min_port,
                            eff_port = eff_port,
                            eff_front = eff_front,
                            tang_port = tang_port,
                            bl_port = bl_port,
                            rf = rf,
                            title = title,
                            subtitle = subtitle,
                            caption = caption)


  return(gg)

}

