#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sampling distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("diff",
                        "Difference between groups",
                        min = -2,
                        max = 2,
                        step = 0.1,
                        value = 0.1),
            sliderInput("sd",
                        "Pooled standard deviation",
                        min = 1,
                        max = 5,
                        value = 2),
            sliderInput("n",
                        "Sample size",
                        min = 10,
                        max = 200,
                        value = 50,
                        step = 1),
            sliderInput("alpha",
                        "Significance level",
                        min = 0.001,
                        max = 0.5,
                        value = 0.05,
                        step = 0.001)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      nulldist <- \(x, n, sd) dt(x*sqrt(n)/sd, n - 2) / (sd/sqrt(n))
      altdist  <- \(x, n, sd, diff) dt((x - diff)*sqrt(n)/sd, n-2) / (sd/sqrt(n))

      p <- ggplot2::ggplot() +
        ggplot2::stat_function(fun = nulldist, args = list(n = input$n, sd = input$sd),
                               color = "navy", linewidth = 2, xlim = c(-6, 6), n = 10001) +
        ggplot2::stat_function(fun = altdist, args = list(n = input$n, sd = input$sd, diff = input$diff),
                               color = "salmon", linewidth = 2, linetype = 2, xlim = c(-6, 6),
                               n = 10001) +
        ggplot2::theme_minimal()

      if (input$diff == 0) {
        tval <- qt(1 - input$alpha/2, input$n-2) * input$sd/sqrt(input$n)
        alpha <- 1 - integrate(nulldist, -tval, tval, n = input$n, sd = input$sd)$value
        p <- p +
          ggplot2::geom_vline(xintercept = c(-1, 1) * tval) +
          ggplot2::stat_function(fun = nulldist, args = list(n = input$n, sd = input$sd),
                                 geom = "area",
                                 col = "transparent", fill = "lightblue3", alpha = 0.7, n = 10001,
                                 xlim = c(-6, -tval)) +
          ggplot2::stat_function(fun = nulldist, args = list(n = input$n, sd = input$sd),
                                 geom = "area",
                                 col = "transparent", fill = "lightblue3", alpha = 0.7, n = 10001,
                                 xlim = c(tval, 6)) +
          ggplot2::geom_label(ggplot2::aes(x = 4, y = nulldist(0, input$n, input$sd),
                                           label = paste0("Type-I error rate: ", round(alpha, 3))),
                              size = 8, col = "lightblue3")
      } else {
        tval <- qt(1 - input$alpha/2, input$n-2) * input$sd/sqrt(input$n)
        power <- 1 - integrate(altdist, -tval, tval, diff = input$diff, n = input$n, sd = input$sd)$value
        p <- p +
          ggplot2::geom_vline(xintercept = c(-1, 1) * tval) +
          ggplot2::stat_function(fun = altdist, args = list(n = input$n, sd = input$sd, diff = input$diff),
                                 geom = "area",
                                 col = "transparent", fill = "lightcoral", alpha = 0.7, n = 10001,
                                 xlim = c(-6, -tval)) +
          ggplot2::stat_function(fun = altdist, args = list(n = input$n, sd = input$sd, diff = input$diff),
                                 geom = "area",
                                 col = "transparent", fill = "lightcoral", alpha = 0.7, n = 10001,
                                 xlim = c(tval, 6)) +
          ggplot2::geom_label(ggplot2::aes(x = 4, y = nulldist(0, input$n, input$sd),
                             label = paste0("Power: ", round(power, 3))),
                             size = 8, col = "lightcoral") +
          ggplot2::ylab(NULL)
      }
      p
    })
}

# Run the application
shinyApp(ui = ui, server = server)
