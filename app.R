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
      selectInput("sideTest", "Side of your test:",
                  choices = c("Two-sided", "One-sided Greater", "One-sided Less"),
                  selected = "Two-sided"),
      textOutput("res"),
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
  ## Two-sided
  exc_p <- reactive(max(1-sum(prob()[c(which(prob() > prob()[obSucc()+1]))]),0))
  ## One-sided Greater
  exc_p_one_g <- reactive(max(sum(prob()[seq(obSucc()+1, length(prob()),1)]),0))
  ## One-sided Less
  exc_p_one_l <- reactive(max(sum(prob()[seq(1, obSucc()+1,1)]),0))
  # Asy p-value
  ## Two-sided
  asy_p <- reactive(2*(1-pnorm(abs((obSucc()-exp_x())/sqrt(exp_x()*(1-hypoProb()))))))
  ## One-sided Greater
  asy_p_one_g <- reactive(1-(pnorm((obSucc()-exp_x())/sqrt(exp_x()*(1-hypoProb())))))
  ## One-sided Less
  asy_p_one_l <- reactive((pnorm((obSucc()-exp_x())/sqrt(exp_x()*(1-hypoProb())))))
  
  output$refDistPlot <- renderPlot({
    ggplot(df(), aes(x = x, y = y)) + 
      geom_bar(aes(fill = "Binomal"), stat = "identity" )+
      stat_function(fun = dnorm, args = list(mean = exp_x(), sd = sqrt(exp_x()*(1-hypoProb()))), 
                    mapping = aes(col = "Normal"), size = 1)+
      scale_y_continuous(expand = c(0.01, 0)) +
      scale_fill_manual("", values = c("Binomal" = "pink")) +
      scale_colour_manual("", values = c("Normal" = "aquamarine4"))+
      xlab("Counts of success trials(X)") + 
      ylab("Density") +
      labs(title = paste0("Binomal(",sampSize(),",", hypoProb(),")")) + 
      theme_bw(16, "serif") +
      theme(plot.title = element_text(size = rel(1.2), vjust = 1.5),
            legend.key = element_blank(),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.position = "top")
    #ggplotly(plt)
  }, res = 96)
  
  output$res <- renderText("Comparison of p-value")
  
  output$pValRes <- renderPrint({
    print("Exact p-value");
    if (input$sideTest == "Two-sided"){
      exc_p()
    }
    else if (input$sideTest == "One-sided Greater"){
      exc_p_one_g()
    }
    else {
      exc_p_one_l()
    }
  })
  
  output$asympvalRes <- renderPrint({
    print("asymptotic p-value");
    if (input$sideTest == "Two-sided"){
      asy_p()
    }
    else if (input$sideTest == "One-sided Greater"){
      asy_p_one_g()
    }
    else {
      asy_p_one_l()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
