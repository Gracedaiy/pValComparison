library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(

    # # Application title
    # titlePanel("Old Faithful Geyser Data"),
    # 
    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
  # Application title
  titlePanel("p-value comparison between exact-test and asymptotic-test"),
  sidebarLayout(
    sidebarPanel(
      numericInput("hyP", "Probability you want to test in hypothesis:",
                   value = 0.5,min = 0, max = 1),
      numericInput("n", "Sample size:",
                   value = 10,min = 1),
      numericInput("obsucc", "Counts of success trails:",
                   value = 1, min = 0),
      verbatimTextOutput("pValRes"),
      verbatimTextOutput("asympvalRes")
    ),
    mainPanel(
      plotOutput("refDistPlot")
    )
  )
  
)

server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
  hypoProb <- reactive(input$hyP)
  sampSize <- reactive(input$n)
  obSucc <- reactive(input$obsucc)
  num_succ <- reactive(seq(0, sampSize(), 1))
  prob <- reactive(dbinom(num_succ(), sampSize(), hypoProb()))
  df <- reactive(data.frame(x=num_succ(), y=prob()))
  exp_x <- reactive(hypoProb()*sampSize())
  # Exact p-value
  exc_p <- reactive(1-sum(prob()[c(which(prob() > prob()[obSucc()+1]))]))
  # Asy p-value
  asy_p <- reactive(2*(1-pnorm(abs((obSucc()-exp_x())/sqrt(exp_x()*(1-hypoProb()))))))
  
  output$refDistPlot <- renderPlot({
    ggplot(df(), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "pink", fill = "pink")+
      stat_function(fun = dnorm, args = list(mean = exp_x(), sd = sqrt(exp_x()*(1-hypoProb()))))+
      scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") +
      labs(title = "dbinom(x, 20, 0.5)") + theme_bw(16, "serif") +
      theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))
    #ggplotly(plt)
  }, res = 96)
  
  output$pValRes <- renderPrint({
    print("Exact p-value");
    exc_p()
  })
  
  output$asympvalRes <- renderPrint({
    print("asymptotic p-value");
    asy_p()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
