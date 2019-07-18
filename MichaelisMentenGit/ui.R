library(shiny)


fluidPage(
  
  titlePanel (h2("Michaelis Menten curves and linear transformations",  align = "right")),
 
 
                
  sidebarLayout(
    
    sidebarPanel(

      
      fileInput("data", label = "Select dataset:"),
     
      radioButtons(inputId = "sep", label = "File type", choices = c(csv = ",", txt ="\t"), selected = ","),
      
      checkboxInput(inputId= "header", label = "Columns have header text", value = TRUE),
      
      
      
      
      
      uiOutput("whatx"), 
      
      uiOutput("whaty"),
    
     
      
      radioButtons(inputId="raw", label = h5("Select plot"), choices = c("Raw data", "Hanes", "Eadie-Hofstee","Lineweaver-Burk", "Residual"
                                                                              ), selected = "Raw data"), 
      
      
      helpText("Please cite this page if you find it useful, Longstaff C, 2016, Shiny App for analysing enzyme kinetics, version 0.55,
               URL address, last accessed", Sys.Date())
   
    
    
  ),
  mainPanel( 
    tabsetPanel(type="tab",
                tabPanel("Plot", 
                         
                         plotOutput(outputId = "myplot"),
                         
                         h4(textOutput("text1")),
                         
                         h4("Results Table"), tableOutput("resultsTable"), align = "center"),
                        
                       
                
                
                tabPanel("Raw data", dataTableOutput("contents")),
                
                tabPanel("Help",
                         
                  tags$blockquote(h5("►Load your own data file in csv or txt fomat (tab separator)",
                                  tags$br(),
                                  "►Avoid unusual characters such as % and ' in names",
                                  tags$br(),
                                  "►Three sets of data are provided for plasminogen activation rates, A, B and C",
                                  tags$br(),
                                  "►The supplied data includes replicates",
                                  tags$br(), 
                                  "►Alternative sets of data can be fitted for the means",
                                  tags$br(),
                                  "►Or for 3 points, below Km, around Km and greater than Km",
                                  tags$br(), 
                                  "►You can choose non-linear regression or select a linear transformation",
                                  tags$br(),
                                  "►Investigate how the linear transformations match the non-linear fitting for each data set",
                                  tags$br(),
                                  "►The residual plot show the residuals for the non-linear fitting only"
                                 
                                 )
  
  ),
  
                
                  
                  
                  
                  tags$img(src="Table1.png", width=700, height=400)
                  
                )
                
               
                
    )
  )
  
)   
)
