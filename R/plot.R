# Plotting ----------------------------------------------------------------

#' Plots the distribution of the TAGFA composition.
#'
#' @param data Project data.
#'
#' @export
plot_tagfa_distribution <- function() {
    project_data %>%
        dplyr::filter(VN == 0) %>%
        dplyr::select(SID, dplyr::matches('^tg\\d+')) %>%
        tidyr::gather(Measure, Value, -SID) %>%
        stats::na.omit() %>%
        dplyr::mutate(Measure = renaming_fats(Measure)) %>%
        dplyr::mutate(
            order1 = substr(Measure, nchar(Measure), nchar(Measure)),
            order1 = ifelse(order1 == 0, 10, order1),
            order1 = ifelse(order1 == 'l', 20, order1),
            order1 = as.integer(order1)
        ) %>%
        dplyr::arrange(dplyr::desc(order1)) %>%
        dplyr::select(-order1) %>%
        dplyr::mutate(Measure = forcats::fct_inorder(Measure)) %>%
        seer::view_boxplots(
            'Measure', 'Value',
            dot.colour = "palegreen4",
            xlab = 'Concentration (nmol/mL)',
            ylab = 'Triacylglyceride fatty acids') +
        graph_theme(ticks = FALSE)
}

#' Plot of MetS components over time.
#'
plot_mets_over_time <- function() {
    project_data %>%
        dplyr::select(VN, Waist, HDL, TAG, Glucose0, MeanArtPressure) %>%
        tidyr::gather(Measure, Value,-VN) %>%
        dplyr::mutate(
            Measure = forcats::fct_recode(
                Measure,
                "FG (mmol/L)" = "Glucose0",
                "HDL (mmol/L)" = "HDL",
                "MAP (mmHg)" = "MeanArtPressure",
                "Tg (mmol/L)" = "TAG",
                "WC (cm)" = "Waist"
                )
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = VN, y = Value)) +
        ggplot2::geom_smooth(method = "loess", colour = "palegreen4") +
        ggplot2::facet_wrap( ~ Measure, ncol = 1, scale = "free") +
        ggplot2::scale_x_continuous(breaks = c(0, 1, 2),
                                    labels = c("0-yr", "3-yr", "6-yr")) +
        ggplot2::labs(y = "", x = "Follow-up year") +
        graph_theme()
}
