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
            dot_colour = "#D9853B") +
        ggplot2::labs(
            x = 'Concentration (nmol/mL)',
            y = 'Triacylglyceride fatty acids') +
        graph_theme(ticks = FALSE)
}

#' Plot of MetS components over time.
#'
plot_mets_over_time <- function() {

    pval_data <- project_data %>%
        mason::design('gee') %>%
        mason::add_settings(cluster.id = "SID", corstr = "ar1") %>%
        mason::add_variables("yvars", outcomes) %>%
        mason::add_variables("xvars", "VN") %>%
        mason::construct() %>%
        mason::scrub() %>%
        dplyr::filter(term == "<-Xterm") %>%
        dplyr::mutate(p.value = aide::format_p(p.value)) %>%
        dplyr::select(Yterms, p.value)

    pre_data <- project_data %>%
        dplyr::select_(.dots = c("VN", outcomes)) %>%
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
        )

    relabel_fun <- function(x, pval) {
        pval_equal_sign <- gsub("^([01]\\.)", "=\\1", pval)
        paste(x, pval_equal_sign, sep = "; p")
    }

    pre_data %>%
        dplyr::mutate(Measure = forcats::fct_relabel(pre_data$Measure, relabel_fun, pval = pval_data$p.value)) %>%
        ggplot2::ggplot(ggplot2::aes(x = VN, y = Value)) +
        ggplot2::geom_smooth(method = "loess", colour = "#D9853B", fill = "#ECECEA") +
        ggplot2::facet_wrap( ~ Measure, ncol = 1, scale = "free") +
        ggplot2::scale_x_continuous(breaks = c(0, 1, 2),
                                    labels = c("0-yr", "3-yr", "6-yr")) +
        ggplot2::labs(y = "", x = "Follow-up year") +
        graph_theme()
}
