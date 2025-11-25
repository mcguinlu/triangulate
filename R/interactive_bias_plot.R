#' Interactive bias-adjusted plot (horizontal layout)
#'
#' Launches a Shiny app that allows users to adjust additive and proportional bias priors
#' and visualizes both original and adjusted estimates horizontally.
#'
#' @param dat A data.frame with columns: result_id, yi, vi, study, type
#' @return A Shiny app object
#' @export
interactive_bias_plot <- function(dat) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages 'shiny', 'ggplot2', and 'dplyr' are required.")
  }

  required_cols <- c("result_id", "yi", "vi", "study", "type")
  missing <- setdiff(required_cols, names(dat))
  if (length(missing) > 0) stop("Missing columns in input data: ", paste(missing, collapse = ", "))

  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive Sensitivity Analysis with Bias Priors"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("bias_add", "Additive Bias (mean)", min = -1, max = 1, value = 0.1, step = 0.01),
        shiny::sliderInput("bias_add_var", "Additive Bias (variance)", min = 0, max = 1, value = 0.05, step = 0.01),
        shiny::sliderInput("bias_prop", "Proportional Bias (mean)", min = -1, max = 1, value = 0.1, step = 0.01),
        shiny::sliderInput("bias_prop_var", "Proportional Bias (variance)", min = 0, max = 1, value = 0.02, step = 0.01)
      ),

      shiny::mainPanel(
        shiny::plotOutput("biasPlot")
      )
    )
  )

  server <- function(input, output) {
    output$biasPlot <- shiny::renderPlot({
      shiny::req(input$bias_add, input$bias_add_var, input$bias_prop, input$bias_prop_var)

      bias_priors <- dplyr::mutate(dat,
                                   domain = "all",
                                   j = "custom",
                                   bias_m_add = input$bias_add,
                                   bias_v_add = input$bias_add_var,
                                   bias_m_prop = input$bias_prop,
                                   bias_v_prop = input$bias_prop_var
      )

      indirectness_priors <- dplyr::select(dat, result_id) %>%
        dplyr::mutate(
          domain = "all",
          j = "neutral",
          ind_m_add = 0,
          ind_v_add = 0,
          ind_m_prop = 0,
          ind_v_prop = 0
        )

      dat_adj <- tri_prep_data(bias_priors, indirectness_priors)
      dat_adj <- tri_calculate_adjusted_estimates(dat_adj)

      dat_combined <- dplyr::bind_rows(
        dplyr::mutate(dat, est_type = "Original", x = yi, se = sqrt(vi)),
        dplyr::mutate(dat_adj, est_type = "Adjusted", x = yi_adj, se = sqrt(vi_adj))
      ) %>%
        dplyr::mutate(result_id = factor(result_id, levels = rev(sort(unique(result_id)))))

      ggplot2::ggplot(dat_combined, ggplot2::aes(y = result_id, x = x, color = est_type)) +
        ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.4), size = 3) +
        ggplot2::geom_errorbarh(
          ggplot2::aes(xmin = x - se, xmax = x + se),
          position = ggplot2::position_dodge(width = 0.4),
          height = 0.2
        ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
        ggplot2::labs(
          title = "Original vs. Bias-Adjusted Estimates",
          x = "Bias-Adjusted Effect",
          y = "Study",
          color = "Estimate Type"
        ) +
        ggplot2::theme_minimal()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
