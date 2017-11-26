#############################################
##### minimal example for HTML - server.R ####
##############################################

library(shiny)
library(polynom)
library(DT)

shinyServer(function(input,output){
        
        xx <- seq(from = -2*pi, to = 2*pi, length.out=500)
        p0 <- function(x){1}
        p2 <- function(x){1-x^2/2}
        p4 <- function(x){1-x^2/2+x^4/24}
        p6 <- function(x){1-x^2/2+x^4/24-x^6/720}
        p8 <- function(x){1-x^2/2+x^4/24-x^6/720+x^8/factorial(8)}
        
        df <- data.frame(xx,cos(xx),p0(xx),p2(xx),p4(xx),p6(xx),p8(xx))
        
        names(df) <- c("xx", "cos", "p0", "p2","p4", "p6", "p8")
        
        output$textDisplay <- renderText({
                n <- input$graph
                paste0("Approximation of cosine by ", 
                       switch(n,
                              '0'="a constant ",
                              '2'="a second degree",
                              '4'='a fourth degree',
                              '6'='a sixth degree',
                              '8'='an eighth degree'), 
                        " polynomial.")
        })
        output$textDisplay <- renderUI({
                h4(HTML(paste0("cos(",input$sliders,") = ", cos(as.numeric(input$sliders)),
                       " and its approximation is p", input$graph, "(", input$sliders, ") = ",
                       switch(input$graph,
                              '0'=p0(as.numeric(input$sliders)),
                              '2'=p2(as.numeric(input$sliders)),
                              '4'=p4(as.numeric(input$sliders)),
                              '6'=p6(as.numeric(input$sliders)),
                              '8'=p8(as.numeric(input$sliders))),"."), 
                             paste0("The absolute error in this approximation is ",
                     abs(cos(as.numeric(input$sliders)) - 
                                 switch(input$graph,
                                        '0'=p0(as.numeric(input$sliders)),
                                        '2'=p2(as.numeric(input$sliders)),
                                        '4'=p4(as.numeric(input$sliders)),
                                        '6'=p6(as.numeric(input$sliders)),
                                        '8'=p8(as.numeric(input$sliders)))), ".")))

        })
        output$absoluteError <- DT::renderDataTable({
                datf <- read.csv("datf.csv", colClasses = "character" )
                datatable(datf)
        })
        output$plotDisplay <- renderPlot({
                
                par(bg="darkolivegreen1") #ecf1ef
              
                curve(cos, -2*pi, 2*pi, type="l",ylim=c(-2,2),
                     ylab="y", xlab="x", col="steelblue", lwd=2)
                abline(h=0, lty=2, lwd=2, col="violet")
                abline(v=0, lty=2, lwd=2, col="violet")
              if(input$plotPoly){  
                switch(input$graph,
                       '0'=lines(df$xx, df$p0, col="coral", lwd=2),
                       '2'=lines(df$xx, df$p2, col="coral", lwd=2),
                       '4'=lines(df$xx, df$p4, col="coral", lwd=2),
                       '6'=lines(df$xx, df$p6, col="coral", lwd=2),
                       '8'=lines(df$xx, df$p8, col="coral", lwd=2))
              
                pt2 <- switch(input$graph,
                              '0'=p0(as.numeric(input$sliderp)),
                              '2'=p2(as.numeric(input$sliderp)),
                              '4'=p4(as.numeric(input$sliderp)),
                              '6'=p6(as.numeric(input$sliderp)),
                              '8'=p8(as.numeric(input$sliderp)))
                
                xsl <- as.numeric(input$sliderp)
                pt1 <- cos(xsl)
                
                
               points(c(xsl,xsl), c(pt1,pt2), col=c("blue","coral"), pch=19, cex=3)
              }
                grid(lty=3, col="violet")
        })
        output$someText <- renderUI({
                h3(HTML(paste("You said '", input$comment, "' -- Thank you!")))
        })
})