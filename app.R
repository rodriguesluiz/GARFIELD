#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(nnet)

#mdl <- NULL
load(file = "recmodel.rda")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data-driven Recommender System for Personalizing Gamified Educational Systems"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderIM","IM", min=1, max=7, step=0.25, value=1),
            sliderInput("sliderAge","Age", min=18, max=80, step=1, value=1),
            selectInput("Gender","Gender", choices = c("Female", "Male")),
            selectInput("Edu","Highest degree", choices = c("High school", "Technical", "Undergraduation")),
            selectInput("PGG","Preferred Game genre", choices = c("Action", "Adventure", "RPG", "Strategy")),
            selectInput("PPS","Preferred Playing Setting", choices = c("Multiplayer", "Singleplayer")),
            sliderInput("sliderWPT","Weekly Playing Time (hours)", min=0, max=112, step=1, value=1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(column(6,plotOutput('bplot')))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bplot <- renderPlot({
        # transforms input into a dataframe with one row
        row <- data.frame(intrinsic_motivation = c(0))
        #row$motivated <- ifelse(input$sliderIM > 0, "TRUE", "FALSE")
        row$intrinsic_motivation <- input$sliderIM
        row$age <- input$sliderAge
        row$gender <- input$Gender
        row$educationlevel <- input$Edu
        row$prefgamegenre <- input$PGG
        row$prefplayingsetting <- input$PPS
        row$weeklyplayingtime <- input$sliderWPT
        # passes to 'newdata'
        preds <- predict(mdl, newdata = row, "probs")
        df <- data.frame(prob = preds, design = c("PBL", "AOP", "AOS", "CCT"))
        p <- ggplot(data=df, aes(x=design, y=prob, fill = design)) + 
            geom_bar(stat="identity", color="black") +
            theme_minimal()
        p + theme(legend.position="top")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
