library(shiny)


function(input, output){


  readData <- reactive({
   
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(read.csv(file.path("./Data/SK CLOT LYSIS REPS.csv"))) 
    else(read.table(inputFile$datapath, sep = input$sep, header = input$header))
    
   
  })

  var<- reactive({
    mycols<-colnames(readData())
 })
  
  

  output$whatx<-renderUI({
   
    selectInput("colmnamesx",
                label= h4("Select x axis data"),
                choices = var(), selected = colnames(readData()[1]))
  })

  output$whaty<-renderUI({
   
    selectInput("colmnamesy",
                label= h4("Select y axis data"),
                choices = var(), selected = colnames(readData()[2]))
                                 
  })

  
  
  
  
  output$myplot<-renderPlot({
    if(is.null(input$colmnamesx)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   
    S<-readData()[,input$colmnamesx]  
    V<-readData()[,input$colmnamesy]
    
    #Hanes
    X.h<-S
    Y.h<-S/V
    HModl<-lm(Y.h~X.h)
    slope.h<- coef(HModl)[2]
    int.h<- coef(HModl)[1]
    r.h<- signif(cor(X.h,Y.h), digits=4)
    Kmlm.h<- signif(int.h/slope.h,digits=4)
    Vmaxlm.h<-signif(1/slope.h, digits=4)
    
    #Lineweaver-Burke
    X.l<-1/S
    Y.l<-1/V
    LModl<-lm(Y.l~X.l)
    slope.l<- coef(LModl)[2]
    int.l<- coef(LModl)[1]
    r.l<- signif(cor(X.l,Y.l), digits=4)
    Kmlm.l<- signif(slope.l/int.l,digits=4)
    Vmaxlm.l<-signif(1/int.l, digits=4)
    
    #Eadie-Hofstee
    X.e<-V/S
    Y.e<-V
    EModl<-lm(Y.e~X.e)
    slope.e<- coef(EModl)[2]
    int.e<- coef(EModl)[1]
    r.e<- signif(cor(X.e,Y.e), digits=4)
    Kmlm.e<- signif(-slope.e,digits=4)
    Vmaxlm.e<-signif(int.e, digits=4)
    
    #fitMM<-nls(V~Vmax*S/(Km+S), start=list(Vmax=Vmaxlm.h, Km=Kmlm.h))
    fitMM<- nls(V~SSmicmen(S,Vmaxlm.h,Kmlm.h))
    xpred<- seq(0, max(S), length.out=50)
    ypred<- predict(fitMM, list(S=xpred))
    Vmax<- signif(coef(fitMM)[1], digits=4)
    Km<- signif(coef(fitMM)[2], digits=4)
    crcNls<-signif(cor(V, predict(fitMM)), digits = 4)
    
      switch(input$raw,
             "Raw data"=plot(S,V, main="Raw data", pch=5, xlab=input$colmnamesx, ylab=input$colmnamesy, xlim= c(0, max(S)), ylim = c(0, Vmax*1.1), lines(xpred,ypred, col="red", lwd=2),lwd=2),
            
             "Hanes"=plot(X.h,Y.h, main="Hanes plot", pch=17, xlab=input$colmnamesy, ylab=paste(input$colmnamesx,"/", input$colmnamesy), xlim= c(0, max(X.h)), ylim = c(0, max(Y.h)), abline(lm(Y.h~X.h), col="red", lwd=2)),
             "Lineweaver-Burk"=plot(X.l,Y.l, main="Lineweaver-Burk plot", pch=17, xlab=paste("1/", input$colmnamesx), ylab=paste("1/", input$colmnamesy), xlim= c(0, max(X.l)), ylim = c(0, max(Y.l)), abline(lm(Y.l~X.l), col="red", lwd=2)),
             "Eadie-Hofstee"=plot(X.e,Y.e, main="Eadie-Hofstee", pch=17, xlab=paste(input$colmnamesy,"/", input$colmnamesx), ylab= input$colmnamesy, xlim= c(0, max(X.e)), ylim = c(0, max(Y.e)), abline(lm(Y.e~X.e), col="red", lwd=2)),
             "Residual"=plot(S, resid(fitMM), xlab=input$colmnamesx, ylab= "Residual", pch=4, col="blue", lwd=2)) 
      
  })
  
  output$resultsTable<-renderTable({
    if(is.null(input$colmnamesx)){return(NULL)} # To stop this section running and producing an error before the data has uploaded

    S<-readData()[,input$colmnamesx]  
    V<-readData()[,input$colmnamesy]
   
    #Hanes
    X.h<-S
    Y.h<-S/V
    HModl<-lm(Y.h~X.h)
    slope.h<- coef(HModl)[2]
    int.h<- coef(HModl)[1]
    r.h<- signif(cor(X.h,Y.h), digits=4)
    Kmlm.h<- signif(int.h/slope.h,digits=4)
    Vmaxlm.h<-signif(1/slope.h, digits=4)
    
    #Lineweaver-Burke
    X.l<-1/S
    Y.l<-1/V
    LModl<-lm(Y.l~X.l)
    slope.l<- coef(LModl)[2]
    int.l<- coef(LModl)[1]
    r.l<- signif(cor(X.l,Y.l), digits=4)
    Kmlm.l<- signif(slope.l/int.l,digits=4)
    Vmaxlm.l<-signif(1/int.l, digits=4)
    
    #Eadie-Hofstee
    X.e<-V/S
    Y.e<-V
    EModl<-lm(Y.e~X.e)
    slope.e<- coef(EModl)[2]
    int.e<- coef(EModl)[1]
    r.e<- signif(cor(X.e,Y.e), digits=4)
    Kmlm.e<- signif(-slope.e,digits=4)
    Vmaxlm.e<-signif(int.e, digits=4)
    
    #fitMM<-nls(V~Vmax*S/(Km+S), start=list(Vmax=Vmaxlm.h, Km=Kmlm.h))
    fitMM<- nls(V~SSmicmen(S,Vmaxlm.h,Kmlm.h))
    fitted<- predict(fitMM)
    Vmax<- signif(coef(fitMM)[1], digits=4)
    Km<- signif(coef(fitMM)[2], digits=4)
    crcNls<-signif(cor(V, predict(fitMM)), digits = 4)
    
    
    tabData<-matrix(c("Non-linear fit", Vmax, Km, crcNls,
                      "Linear fit Hanes", Vmaxlm.h, Kmlm.h, r.h,
                      "Linear fit Eadie-Hofstee", Vmaxlm.e, Kmlm.e, r.e,
                      "Linear fit Lineweaver-Burk", Vmaxlm.l, Kmlm.l, r.l
                      ), byrow=TRUE, nrow=4)
    colnames(tabData)<-c("Fit", "Vmax", "Km", "Correlation")
    
    #write.table(tabData, "clipboard", sep="\t", col.names=F, row.names=F) 
    
    tabData
    
  })

  output$text1<-renderText({
    paste( "Fitting for ", input$colmnamesy, "versus", input$colmnamesx)
    
  })
  
  output$contents<-renderDataTable({
    readData()

  })

  
}