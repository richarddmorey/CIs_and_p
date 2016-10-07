#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('settings.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot <- renderPlot({
 
    xbar = input$xbar
    mu = input$mu
    N = as.integer(input$N)
    sigma = 15
    CC = as.numeric(input$CC)
    
    rejectCol = rgb(255,192,203, maxColorValue = 255)
    retainCol = rgb(173,216,230, maxColorValue = 255)
    
    alpha = 1-CC
    zstar = qnorm(1-alpha/2)
    
    ci = xbar + c(-1,1)*zstar*sigma/sqrt(N)
    
    pp = seq(.0001,.9999,len=400)
    xx = sort(c(65,qnorm(pp, mu, sigma/sqrt(N)),135))

    layout(matrix(c(1,1,1,2),ncol=1))
    
    do.call(par,par.list)
    par(cex.lab=1.5,cex.axis=1.5)
    
    plot(xx, dnorm(xx, mu, sigma/sqrt(N)), ty='l', axes=FALSE, ylab="Density", xlab="Observed mean IQ", main = "Sampling distribution")
    axis(1)
    
    
    # left side
    xx1 = c(xx[xx<xbar],xbar)
    myCol = ifelse(mu>ci[2],rejectCol, retainCol)
    polygon(c(xx1,rev(xx1)), c(dnorm(xx1, mu, sigma/sqrt(N)), 0*xx1),col=myCol,border=NA)
    pval = pnorm(xbar, mu, sigma/sqrt(N))
    text(par()$usr[2], par()$usr[4], substitute(paste(H[0]," ",mu>mu0),list(mu0=mu)), 
         cex=2, adj=c(1,1.2), col=myCol)
    text(par()$usr[2], par()$usr[4], substitute(paste(p==p0),list(p0=round(pval,4))), 
         cex=2, adj=c(1,2.2), col=myCol)
    
    # right side
    xx1 = c(xbar,xx[xx>xbar])
    pval = pnorm(xbar, mu, sigma/sqrt(N))
    myCol = ifelse(mu<ci[1],rejectCol, retainCol)
    polygon(c(xx1,rev(xx1)), c(dnorm(xx1, mu, sigma/sqrt(N)), 0*xx1),col=myCol,border=NA)
    pval = 1-pnorm(xbar, mu, sigma/sqrt(N))
    text(par()$usr[1], par()$usr[4], substitute(paste(H[0]," ",mu<mu0),list(mu0=mu)), 
         cex=2, adj=c(0,1.2), col=myCol)
    text(par()$usr[1], par()$usr[4], substitute(paste(p==p0),list(p0=round(pval,4))), 
         cex=2, adj=c(0,2.2), col=myCol)
    
    
    abline(v = xbar, col="darkgray")
    text(xbar,par()$usr[4],"Observed mean",srt=90,adj=c(1.2,1.2), col="darkgray", cex = 1.5)
    
    abline(v = mu, col="purple")
    text(mu,par()$usr[4],expression("True mean ",mu),srt=90,adj=c(1.2,1.2), col="purple", cex = 1.5)
    
    
    
    plot(xx, 0*xx, ty='n', axes=FALSE, ylab="", xlab=expression(paste("True mean ",mu)), ylim=c(-1,1), main="Confidence interval")
    axis(1)
    rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col=rejectCol,border=NA)
    rect(ci[1],par()$usr[3],ci[2],par()$usr[4], col=retainCol, border=NA)
    segments(ci[1],0,ci[2],0)
    points(xbar,0,pch=19)
    
    abline(v=mu, col="purple")
    
    
  })
  
})
