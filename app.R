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
      plotOutput("refDistPlot"),
      tableOutput("CITable"),
      plotOutput("powerPlot")
      # fluidRow(
      #   column(12,plotOutput("refDistPlot"))
      # ),
      # fluidRow(
      #   column(12,tableOutput("powerCITable"))
      # )
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
  
  
  #CITable <- reactive({
  output$CITable <- renderTable({
    sum <- 0
    i <- 1
    j <- sampSize()
    tp <- 0
    pHat <- obSucc()/sampSize()
    prob <- dbinom(num_succ(), sampSize(), prob = pHat)
    alpha <- 0.05
    
    if (input$sideTest == "Two-sided"){
      while (sum <= alpha & i != j) {
        if(prob[i] == prob[j]){
          sum <- sum + prob[i]*2
          i <- i + 1
          j <- j - 1
          tp <- 1
        }else if(prob[i] <  prob[j]){
          sum <- sum + prob()[i]
          i <- i + 1
          tp <- 2
        }
        else if(prob[i] >  prob[j]){
          sum <- sum + prob[j]
          j <- j - 1
          tp <- 3
        }
      }
      
      if(tp == 1){
        i <- i - 1
        j <- j + 1
      }else if(tp == 2){
        i <- i - 1 
      }else{
        j <- j + 1
      }
      
      
      asy_i <- max(pHat - abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize()),0)
      asy_j <- min(pHat + abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize()),1)
      lwb <- c((i-1)/sampSize(), asy_i)
      upb <- c((j-1)/sampSize(), asy_j)
      CI_table <- data.frame(Type = c("Exact CI", "Asymptotic CI"), lowerbound = lwb, upperbound = upb)
      CI_table
    } else if (input$sideTest == "One-sided Greater"){
      while (sum <= alpha) {
        sum <- sum + prob[j]
        j <- j - 1
      }
      j <- j + 1
      
      asy_j <- min(pHat + abs(qnorm(alpha))*sqrt(pHat*(1-pHat)/sampSize()),1)
      lwb <- c(0, 0)
      upb <- c((j-1)/sampSize(), asy_j)
      
      CI_table <- data.frame(Type = c("Exact CI", "Asymptotic CI"), lowerbound = lwb, upperbound = upb)
      CI_table
    } else {
      while (sum <= alpha) {
        sum <- sum + prob[i]
        i <- i + 1
      }
      i <- i - 1
      
      asy_i <- max(pHat - abs(qnorm(alpha))*sqrt(pHat*(1-pHat)/sampSize()),0)
      lwb <- c((i-1)/sampSize(), asy_i)
      upb <- c(1, 1)
      
      CI_table <- data.frame(Type = c("Exact CI", "Asymptotic CI"), lowerbound = lwb, upperbound = upb)
      CI_table
    }
    
    # while (sum < alpha & i != j) {
    #   if(prob[i] == prob[j]){
    #     sum <- sum + prob[i]*2
    #     i <- i + 1
    #     j <- j - 1
    #     tp <- 1
    #   }else if(prob[i] <  prob[j]){
    #     sum <- sum + prob()[i]
    #     i <- i + 1
    #     tp <- 2
    #   }
    #   else if(prob[i] >  prob[j]){
    #     sum <- sum + prob[j]
    #     j <- j - 1
    #     tp <- 3
    #   }
    # }
    # 
    # if(tp == 1){
    #   i <- i - 1
    #   j <- j + 1
    # }else if(tp == 2){
    #   i <- i - 1 
    # }else{
    #   j <- j + 1
    # }
    # 
    # 
    # asy_i <- max(pHat - abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize()),0)
    # asy_j <- pHat + abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize())
    # lwb <- c(i/sampSize(), asy_i)
    # upb <- c(j/sampSize(), asy_j)
    # CI_table <- data.frame(Type = c("Exact CI", "Asymptotic CI"), lowerbound = lwb, upperbound = upb)
    # CI_table
  })

  #output$powerCITable <- renderDataTable({pwTable()})
  output$powerPlot <- renderPlot({
    theta <- seq(0.01, 0.99, 0.01)
    exc_pw <- rep(0, length(theta))
    asy_pw <- rep(0, length(theta))
    pHat <- obSucc()/sampSize()
    prob_hypo <- dbinom(num_succ(), sampSize(), prob = pHat)
    
    
    for (k in 1:length(theta)) {
      prob <- dbinom(num_succ(), sampSize(), prob = theta[k])
      sum <- 0
      i <- 1
      j <- sampSize()+1
      tp <- 0 
      alpha <- 0.05
      if (input$sideTest == "Two-sided"){
        while (sum <= alpha & i != j) {
          if(prob_hypo[i] == prob_hypo[j]){
            sum <- sum + prob_hypo[i]*2
            i <- i + 1
            j <- j - 1
            tp <- 1
          }else if(prob_hypo[i] <  prob_hypo[j]){
            sum <- sum + prob_hypo[i]
            i <- i + 1
            tp <- 2
          }
          else if(prob_hypo[i] >  prob_hypo[j]){
            sum <- sum + prob_hypo[j]
            j <- j - 1
            tp <- 3
          }
        }
        
        if(tp == 1){
          i <- i - 1
          j <- j + 1
        }else if(tp == 2){
          i <- i - 1 
        }else{
          j <- j + 1
        }
        asy_i <- max(pHat - abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize()),0)
        asy_j <- min(pHat + abs(qnorm(alpha/2))*sqrt(pHat*(1-pHat)/sampSize()),1)
        exc_pw[k] <- 1 - sum(prob[i:j])
        asy_pw[k] <- 1 - (pnorm(asy_j, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize())) - pnorm(asy_i, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize())))
      } else if (input$sideTest == "One-sided Greater"){
        while (sum <= alpha) {
          sum <- sum + prob_hypo[j]
          j <- j - 1
        }
        j <- j + 1
        
        asy_j <- min(pHat + abs(qnorm(alpha))*sqrt(pHat*(1-pHat)/sampSize()),1)
        exc_pw[k] <- 1 - sum(prob[1:j])
        asy_pw[k] <- 1 - (pnorm(asy_j, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize())) - pnorm(0, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize())))
      } else {
        while (sum <= alpha) {
          sum <- sum + prob_hypo[i]
          i <- i + 1
        }
        i <- i - 1
        
        asy_i <- max(pHat - abs(qnorm(alpha))*sqrt(pHat*(1-pHat)/sampSize()),0)
        exc_pw[k] <- 1 - sum(prob[i: sampSize()+1])
        asy_pw[k] <- 1 - (pnorm(1, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize()))- pnorm(asy_i, mean = theta[k], sd = sqrt(theta[k]*(1-theta[k])/sampSize())))
      }
    }
    
    pw_table <- data.frame(theta = theta, exact_pw = exc_pw, asymptotic_pw = asy_pw)
    #colnames(pw_table) <- c("theta", "Exact Power", "Asymptotic Power")
    ggplot(pw_table, aes(x = theta)) +
      geom_line(aes(y = exact_pw, color = "Exact Power"), size = 1)+  
      geom_line(aes(y = asymptotic_pw, color = "asymptotic Power"), size = 1) +
      theme_bw(16, "serif") +
      theme(plot.title = element_text(size = rel(1.2), vjust = 1.5),
            legend.key = element_blank(),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.position = "top") +
      xlab("p") +
      ylab("power") +
      labs(title = "Power Comparison")
      
    # ggplot(df(), aes(x = x, y = y)) + 
    #   geom_bar(aes(fill = "Binomal"), stat = "identity" )+
    #   stat_function(fun = dnorm, args = list(mean = exp_x(), sd = sqrt(exp_x()*(1-hypoProb()))), 
    #                 mapping = aes(col = "Normal"), size = 1)+
    #   scale_y_continuous(expand = c(0.01, 0)) +
    #   scale_fill_manual("", values = c("Binomal" = "pink")) +
    #   scale_colour_manual("", values = c("Normal" = "aquamarine4"))+
    #   xlab("Counts of success trials(X)") + 
    #   ylab("Density") +
    #   labs(title = paste0("Binomal(",sampSize(),",", hypoProb(),")")) + 
    #   theme_bw(16, "serif") +
    #   theme(plot.title = element_text(size = rel(1.2), vjust = 1.5),
    #         legend.key = element_blank(),
    #         legend.title = element_blank(),
    #         legend.box = "horizontal",
    #         legend.position = "top")
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
