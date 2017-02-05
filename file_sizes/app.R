ui <- shiny::fluidPage(putdatio::file_size_shiny_ui("mogilefs"))
server <- function(...) shiny::callModule(putdatio::file_size_shiny,
                                          "mogilefs")
shiny::shinyApp(ui, server)
