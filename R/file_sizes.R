#' Grouped file size query
#'
#' Memoised because it will be called repeatedly from \code{\link{file_sizes}}
#' This value is cached in this repository as file_sizes_log
#'
#' @export
file_sizes_db <- function() {
  get_db_src("mogilefs") %>%
    dplyr::tbl("file") %>%

    # Getting log2 / 10 of file size so that 1->kb, 2->mb, 3->gb etc
    dplyr::transmute(size_log = round(log2(length) / 10, 3)) %>%

    # Grouping to reduce the network bandwidth resulting from query
    dplyr::group_by(size_log) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::collect() %>%

    # Regenerating original data from groups
    dplyr::filter(!is.na(size_log)) %>% {
      rep(.$size_log, times = .$count)
    }
}

#' Human readable file size from log2 bytes
#'
#' @export
size_log_si <- function(size_log) {
  2 ^ (size_log * 10) %>%
    purrr::map_chr(~ format(structure(.x, class = "object_size"),
                            units = "auto"))
}

#' File size kernel density estimate
#'
#' @export
file_size_density <- memoise::memoise(function(file_sizes = sizes_log) {
  KernSmooth::bkde(file_sizes) %>%
    dplyr::as_data_frame()
})

#' Get the percentage ratio of a file size range
#'
#' @param file_sizes All file sizes
#' @param range Numeric vector of two, giving a range for file sizes.
#'
#' @return The percentage of file sizes that are between \code{range}.
#'
#' @export
range_size_percentage <- function(file_sizes, range) {
  size_count <- length(file_sizes)
  sum(file_sizes >= range[1] & file_sizes <= range[2]) / size_count
}

#' Plot density
#'
#' @export
file_size_density_plot <- function(density_data, range = NULL, point = NULL) {
  breaks <- 1:3

  if (!is.null(range)) {
    assertthat::assert_that(is.numeric(range))
    breaks <- c(breaks, range)
  }

  if (!is.null(point)) {
    assertthat::assert_that(assertthat::is.number(point))
    breaks <- c(breaks, point)
  }

  plot_breaks <- sort(unique(breaks))
  plot <- ggplot2::ggplot(density_data, ggplot2::aes(x, y)) +
    ggplot2::geom_path() +
    ggplot2::ylab("Density") +
    ggplot2::xlab("File Size") +
    ggplot2::ggtitle("Putio MogileFS File Size Density Plot") +
    ggplot2::coord_cartesian(xlim = c(0.5, 3.5)) +
    ggplot2::geom_area(
      data = dplyr::filter(density_data, x > range[1], x < range[2]),
      alpha = 0.3) +
    ggplot2::theme_set(ggplot2::theme_gray(base_size = 14)) +
    ggplot2::scale_x_continuous(breaks = plot_breaks, labels = size_log_si,
                                minor_breaks = seq(0.5, 3.5, by = 0.5))

  if (is.numeric(point)) {
    plot <- plot +
      ggplot2::geom_vline(xintercept = point, color = "red")
  }

  plot
}

#' File size density plot shiny ui
#'
#' @export
file_size_shiny_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::titlePanel("Putio File Sizes"),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::wellPanel(
          shiny::sliderInput(
            ns("size_range"), step = 0.1,
            label = shiny::h3("File Size Range"),
            min = 0.5, max = 3.5, value = c(0.5, 2.5)),
          shiny::textOutput(ns("scale_info"))
        ),
        shiny::textOutput(ns("range_info"))
      ),
      shiny::column(
        6,
        shiny::plotOutput(
          ns("density_plot"),
          click = ns("plot_click")),
        shiny::textOutput(ns("files_info"))
      )
    )
  )
}

#' File size density plot server function
#'
#' @export
file_size_shiny <- function(input, output, session, file_sizes = sizes_log) {
  r_val <- shiny::reactiveValues(clicked_point = 1)

  shiny::observe({
    shiny::req(input$plot_click)
    r_val$clicked_point <- input$plot_click$x
  })

  output$density_plot <- shiny::renderPlot({
    density_data <- file_size_density(file_sizes)
    file_size_density_plot(density_data,
                           range = input$size_range,
                           point = r_val$clicked_point)
  })

  output$files_info <- shiny::renderText({
    sprintf("As of this analysis, there are %s files in MogileFS. Scale of x
            axis is logarithmic and if you are curious about a peak or a valley,
            just click on it to reveal its value on the x axis with a vertical
            line.", format(length(file_sizes), big.mark = ",")
    )
  })

  output$scale_info <- shiny::renderText({
    "You can select a range from the slider above to
    get the percentage of files inside that range. The
    slider is scaled to log1024 so that 1 -> 1Kb,
    2 -> 1Mb and 3 -> 1Gb."
  })

  output$range_info <- shiny::renderText({
    sprintf("%.01f %% percent of files are between selected range %s and %s.",
            range_size_percentage(file_sizes, input$size_range) * 100,
            size_log_si(input$size_range[1]),
            size_log_si(input$size_range[2]))
  })
}
