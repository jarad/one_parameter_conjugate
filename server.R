library(shiny)

shinyServer(function(input,output) {
  output$plot <- renderPlot({
    mode = (input$alpha+input$y-1)/(input$alpha+input$beta+input$n-2)
    ymax = dbeta(mode, input$alpha+input$y, input$beta+input$n-input$y)
  
    curve(dbeta(x, input$alpha, input$beta), 
          from = 0, 
          to = 1, 
          n = 1001, 
          col="red", lwd=2, 
          ylim=c(0,ymax), 
          xlab = expression(theta),
          ylab = '',
          main = 'Prior x Likelihood = Posterior')
    curve(dbeta(x, input$y, input$n-input$y), 
          n = 1001, 
          col="blue", lwd=2, add=TRUE)
    curve(dbeta(x, input$alpha+input$y, input$beta+input$n-input$y),
          n = 1001, 
          col="purple", lwd=2, add=TRUE)
    legend("topright", 
           c("Prior","Likelihood","Posterior"), 
           col=c("red","blue","purple"), lwd=2)
  })
})
