#' Interactively select intensity data using a lasso gate
#'
#' @param data data
#' @param channel_1 channel 1
#' @param channel_2 channel 2
#'
#' @return Coordinates for gate
#' @export
interactive_data_select <-
  function(data, channel_1, channel_2) {

    ui = miniUI::miniPage(
      miniUI::gadgetTitleBar("Daniel's interactive data select"),
      # Render plot
      plotly::plotlyOutput("plot"))


    server = function(input, output) {

      output$plot <- plotly::renderPlotly({
        p = plot_intensity_2d(data, !!rlang::ensym(channel_1),!!rlang::ensym(channel_2))
        plotly::ggplotly(p) %>% plotly::layout(dragmode = 'lasso') %>% plotly::event_register("plotly_brushed")
      })

      brushed_values = shiny::reactive(plotly::event_data("plotly_brushed"))

      shiny::observe({
        if(input$done){
          shiny::stopApp(brushed_values())}
      })

      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })


    }

    shiny::runGadget(ui, server)

  }

