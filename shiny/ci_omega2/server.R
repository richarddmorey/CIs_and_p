#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('steiger.utility.R')
source('settings.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot <- renderPlot({
 
    Fstat = input$Fstat
    omega.sq = input$omega2
    N = as.integer(input$N)
    J = as.integer(input$J)
    
    CC = as.numeric(input$CC)
    xmax = 30
    
    
    rejectCol = rgb(255,192,203, maxColorValue = 255)
    retainCol = rgb(173,216,230, maxColorValue = 255)
    
    alpha = 1-CC

    df1 = J - 1
    df2 = J * (N - 1)
    ci = steigerCI.omega2(Fstat,df1,df2,conf.level=CC)
    
    
    FF = seq(0,xmax*1.1,len=300)
    lambda.sq = N*J*omega.sq/(1-omega.sq)
    
    layout(matrix(c(1,1,1,2),ncol=1))
    
    do.call(par, par.list)
    par(cex.lab=1.5,cex.axis=1.5)
 
    plot(FF, df(FF, df1,df2,ncp = lambda.sq),ty='l', lwd=2, ylab="Density", xlab="F statistic", axes=FALSE, xlim=c(0,xmax))
    axis(1)
    
    
    # left side
    xx1 = c(FF[FF<Fstat],Fstat)
    myCol = ifelse(omega.sq>ci[2],rejectCol, retainCol)
    polygon(c(xx1,rev(xx1)), c(df(xx1, df1,df2,ncp = lambda.sq), 0*xx1),col=myCol,border=NA)
    
    # right side
    xx1 = c(Fstat,FF[FF>Fstat])
    myCol = ifelse(omega.sq<ci[1],rejectCol, retainCol)
    polygon(c(xx1,rev(xx1)), c(df(xx1, df1,df2,ncp = lambda.sq), 0*xx1),col=myCol,border=NA)
    
    # left side
    xx1 = c(FF[FF<Fstat],Fstat)
    myCol = ifelse(omega.sq>ci[2],rejectCol, retainCol)
    pval = pf(Fstat,df1,df2,ncp = lambda.sq)
    text(par()$usr[2], par()$usr[4], substitute(paste(H[0]," ",omega^2>om0),list(om0=omega.sq)), 
         cex=2, adj=c(1,1.2), col=myCol)
    text(par()$usr[2], par()$usr[4], substitute(paste(p==p0),list(p0=round(pval,4))), 
         cex=2, adj=c(1,2.7), col=myCol)
  
    # right side
    xx1 = c(Fstat,FF[FF>Fstat])
    myCol = ifelse(omega.sq<ci[1],rejectCol, retainCol)
    pval = 1-pf(Fstat,df1,df2,ncp = lambda.sq)
    text(par()$usr[1], par()$usr[4], substitute(paste(H[0]," ",omega^2<om0),list(om0=omega.sq)), 
         cex=2, adj=c(0,1.2), col=myCol)
    text(par()$usr[1], par()$usr[4], substitute(paste(p==p0),list(p0=round(pval,4))), 
         cex=2, adj=c(0,2.7), col=myCol)
    
    
    abline(v = Fstat, col="darkgray")
    text(Fstat,par()$usr[4],"Observed F",srt=90,adj=c(1.2,1.2), col="darkgray", cex = 1.5)
    
    plot(0, 0, ty='n', axes=FALSE, ylab="", xlab=expression(paste("True ",omega^2)), 
         xlim=c(0,1),ylim=c(-1,1), main="Confidence interval")
    axis(1)
    rect(0,par()$usr[3],par()$usr[2],par()$usr[4], col=rejectCol,border=NA)
    rect(ci[1],par()$usr[3],ci[2],par()$usr[4], col=retainCol, border=NA)
    segments(ci[1],0,ci[2],0)
    
    abline(v=omega.sq, col="purple")
    
    
  })
  
})
