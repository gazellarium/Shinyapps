
# loading libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)



# dataset<-read_csv("tidyresult_wordfinal.csv")
# dataset$PID<-as.numeric(dataset$PID)
# dataset$Attestedness<-as.factor(dataset$Attestedness)

# Load the dataset 
load("data/dataset-ssp.RData")

# Create vectors for the inputs 
PID<-pull(dataset, PID)
IVS <- pull(dataset, IVS)

# ui -------------------

# This will change the interface of the front end 
ui <- fluidPage(
      # Chose a theme from the shinythemes package
      theme = shinytheme("flatly"),
      
      # Title of the page
      fluidRow(
        column(3, ), column(8, titlePanel("Sonority Projection Effects in Persian Dataset")), column(1,)),
     
      # Descriptions about what the app is
      fluidRow(
        column(2, ),column(8, h3 (  style="text-align:center","This is an interactive app for an Acceptability Judgement Experiment in a project about Sonority Projection Effects in Persian"), column(2,))
      ),
  br(),
  br(),
      
      # This sets the layout into two panels of side and man
      sidebarLayout(
        # Things here will go on the left side of the page
        sidebarPanel(
       
          # Descriptions about what the app does   
          h4 ("You can choose a participant from our 112 particiapnts pool to see how they rated each level of our conditions, visually, in a plot and compare it to the avarage reatings on the whole."),   
      
          # Picking a participant with numeric IDs 
          sliderInput(inputId="pid", label="Pick a participant:", min= 1, max= 112, value=20, step=1),
      
          # A guide of our variables and ratings
          h4("Variable guide:"),
          p ("In this experiment, there are two main conditions as independent variables. There is the *Sonority Distance* of the clusters and *Attestedness* of these clusters in Persian."), 
          p("Sonority is measured from -3 to +3. Attestedness is either attested (1) or unattested (0)."),
          p ("For example, here, these clusters are final clusters so a -3 sonority distance shows wellformedness and +3 cluster shows illformedness. Therefore a downslope trend in participants ratings shows the Sonority Projection Effects."),
          h4("Ratings guide:"),
          p(" The participants rated the stimuli on a 5 level likert scale from 1= illformed to 5 = wellformed."),
          br(),
      
          # Pressing this buttons will refresh the plot
          submitButton("Plot it!", icon("sync")),
    ),
    
        # Things here will go on the right main panel
        mainPanel(
          # The plot goes here
          h4("Here's an interaction plot between Sonority Distance and Ratings:"),
          plotOutput("plot"),
          
          fluidRow(
          # The tables go on the bottom
          column(6,h4("Here's how this participants rated things:"),
          tableOutput("meanall")),
          
          column(6,h4("Here's how all participant rated things:"),
          tableOutput("mean")),       
          )
          
          
    )),
  
     # Pointing to the source code
          h4 (style =" ", "You can find the souce code of this app here: ", tags$a(href="https://github.com/gazellarium/Shinyapps/tree/main/SSP-EXP/app.R", "Github Source code")),
          
          br(),
          br(),
)

#  server ----------------

# This will be the server logic in the backend of the app
server <- function(input, output, session) {
  
            # The reactive variable for the input slider
            set<- reactive (dataset %>% filter (PID== input$pid, type== "Stimuli"))
  
  
            # Here is how the plot is rendered with ggplot
            output$plot <- renderPlot({
                  
                ggplot(set(), aes(x=Sonority, y=Value, shape=Attestedness, color=Attestedness)) +
                geom_jitter ()+
                geom_smooth(method= glm)+
                scale_x_continuous(breaks=seq(-3, +3, 1))+
                scale_y_continuous(breaks=seq(1, 5, 0.5))+
                theme_minimal()+
                scale_colour_manual(values=c ("darkred","darkblue")) +
                ylab("Average Rating") + xlab (" Sonority Distance")
            } , res = 96)
            
            
            # Here is how the table with the means is calculated
            output$meanall<- renderTable({
              set() %>% group_by(Sonority, Attestedness) %>% summarize(Mean.Rating= mean(Value))
              })
            
            # Here is how the table with the means is calculated
            output$mean<- renderTable({
              dataset %>% filter (type== "Stimuli") %>% group_by(Sonority, Attestedness) %>% summarize(Mean.Rating= mean(Value))
    
  })
  
}
shinyApp(ui, server)
