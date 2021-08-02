#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny)
#install.packages("shinythemes")
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

  theme= shinytheme("darkly"),
 
    # Application title
    titlePanel("UNIVERSITY ADMISSION CHANCES PREDICTION"),

  fluidRow(
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
                
                textInput("CGPA","Enter the CGPA Score (Out of 10):- ", ""),
                textInput("GRE","Enter the GRE Score (Out of 340):- ", ""),
                textInput("TOEFL","Enter the TOEFL Score (Out of 120):- " ,""),
                textInput("LOR","Enter the LOR Score (Out of 5 ):- " ,""),
                actionButton('go',"Predict"),
                
                
                titlePanel("To plot graph kindly choose the X AND Y variable respectively"),
                selectInput(inputId = "var1", label = "Select the X variable", choices = c("ChancesOfAdmit" = 9, "GRE" = 2, "CGPA" = 7, "LOR" = 6 , "SOP"= 5 ,"Research"= 8, "TOEFL" = 3)),
                selectInput(inputId = "var2", label = "Select the Y variable", choices = c("ChancesOfAdmit" = 9, "GRE" = 2, "CGPA" = 7, "LOR" = 6 , "Research"= 8, "TOEFL" = 3), selected = 2),
                radioButtons(inputId = "var3", label = "Select the file type", choices = list("png", "pdf"))
      
                
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            sidebarPanel( Width = 10,
                         
                          headerPanel(" The chances of you getting admitted is: "),
                         textOutput("value"),
                         br(),
                         br(),
                         
                         titlePanel("To Download the dataset click on download dataset button"),
                         downloadButton(outputId = "downloadData", label = "Download the dataset"),
                         br(),
                         br(),
                   
                         titlePanel("Data Visualization"),
                         plotOutput("plot"),
                      
                         
                         titlePanel("To download the graph plotted click on the download the graph button"),
                         downloadButton(outputId = "down", label = "Download the plot"),
                    
            )
        )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) 
    {
  
  Admission<- read.csv("Admission_Predict.csv", head=TRUE, sep=",")
     View(Admission)
  
  x <- reactive({
    Admission[,as.numeric(input$var1)]
  })
  # x contains all the observations of the y variable selected by the user. Y is a reactive function
  y <- reactive({
    Admission[,as.numeric(input$var2)]
    
  })
  # xl contains the x variable or column name of the  dataset selected by the user
  xl <- reactive({
    names( Admission[as.numeric(input$var1)])
  })
  
  # yl contains the y variable or column name of the  dataset selected by the user
  yl <- reactive({
    names( Admission[as.numeric(input$var2)])
  })
  
  
  output$downloadData <- downloadHandler(
    
       filename = function(){
         paste("Admission", "csv", sep =".")
         
       },
         content = function(file){
           write.csv(Admission,file)
         }
  )
         
  
  output$plot <- renderPlot({
    plot(x=x(), y=y(), main = "Admission Prediction dataset plot", xlab = xl(), ylab = yl())
    
    
  })
  
 
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("Admission", input$var3, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      plot(x=x(), y=y(), main = "Admission Prediction dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      dev.off()  # turn the device off
      
    } 
  )
  
  
        data2 <- reactiveValues()
        observeEvent(input$go,{
            
       
        Admission <- subset(Admission, select = -1 )
      
        
        colnames(Admission)[1] <- "GRE"
        colnames(Admission)[2] <- "TOEFL"
        colnames(Admission)[3] <- "UniversityRating"
        colnames(Admission)[8] <- "ChancesOfAdmit"
        
        
        data2$mycgpascore <- as.numeric(input$CGPA)
        data2$mygrescore <- as.numeric(input$GRE)
        data2$mytoeflscore <- as.numeric(input$TOEFL)
        data2$mylorscore <- as.numeric(input$LOR)
        
        
        newPredict = data.frame(CGPA = data2$mycgpascore , GRE = data2$mygrescore,  TOEFL = data2$mytoeflscore, LOR =  data2$mylorscore )
        
        model <- lm(ChancesOfAdmit~ CGPA + GRE +TOEFL+LOR, data = Admission)
        
        data2$op = predict(model , newPredict)
    
        })
  
output$value<- renderText((data2$op*100))


}


# Run the application 
shinyApp(ui = ui, server = server)



