library(shiny)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

#UI Layout

ui <- fluidPage(
  
  titlePanel(title = "EasyPlot",windowTitle = "EasyPlot"),
  
  #Input and Selections Layout
  
  sidebarLayout( 
    sidebarPanel(fluidRow(
      column(width = 6,fileInput(inputId = "file", label = "Upload Delmited file", accept = c(".csv"))),
      column(width = 6,textInput(inputId = "sep", "Specify delimiter", value = ","))),
      selectInput(inputId = "pltype", label = "Select plot type", 
                                      choices = c("Scatter" = "geom_point(size=4)",
                                                  "Line" = "geom_line()",
                                                  "Bar" = "geom_bar(stat = 'identity')",
                                                  "Box" = "if(input$needgroup){geom_boxplot(aes(group = eval(parse(text = input$X))))}else{geom_boxplot()}")),
      uiOutput(outputId = "setup"),
      uiOutput(outputId = "facet")
    
    ),
  
  #Output - Data display and Visualization Layout
  
    mainPanel (
    
      tabsetPanel(
        tabPanel("Summary",verbatimTextOutput(outputId = "summary")),
        tabPanel("Data",dataTableOutput(outputId = "table")),
        tabPanel("Plot",plotOutput(outputId = "plot",height = "650px"))
      )
    )
  )
)


#R object processing

server <- function(input,output){
  datafile <- reactive({
    infile <- input$file
    req(infile)
    t <- read.csv(infile$datapath, header = TRUE, as.is = TRUE, sep = input$sep)
    if("Date" %in% names(t)){t$Date <- mdy(t$Date)}
    t
    })
  
  observeEvent(input$file,output$setup <- renderUI({
    dataframe <- datafile()
    wellPanel(selectInput(inputId = "Y", label = "Y Axis", choices = names(dataframe),selected = names(dataframe)[2]),
              selectInput(inputId = "X", label = "X Axis", choices = names(dataframe)),
              selectInput(inputId = "color", label = "Color by", choices = c("None",names(dataframe)),selected = "None"),
              tags$strong("Additional Elements"),
              fluidRow(column(width = 4,checkboxInput(inputId = "needreg", label = "Trend Line", value = FALSE)),
              column(width = 4,checkboxInput(inputId = "needgroup", label = "Groups", value = FALSE)),
              column(width = 4,checkboxInput(inputId = "needfacet", label = "Facet/Panel", value = FALSE)))
    )
  
  }))
  
  observeEvent(input$needfacet, output$facet <- renderUI({
    conditionalPanel(condition = "input.needfacet", selectInput(inputId = "facet", label = "Facet/Panel By", choices = names(datafile()), 
                                                                  selected = names(datafile()[length(names(datafile()))])),
                                                    radioButtons(inputId = "freescale", label = "Y Axis Scale", choices = c("Fixed" = "fixed", "Free" = "free"),
                                                                  selected = "Fixed",inline = TRUE))
  }))
  
  observeEvent(input$needgroup,
    output$plot <- renderPlot(
      (if(input$color == "None"){ggplot(data=datafile(), aes(x=eval(parse(text=input$X)), y=eval(parse(text=input$Y))))}
      else{ggplot(data=datafile(), aes(x=eval(parse(text=input$X)), y=eval(parse(text=input$Y)), col = eval(parse(text = input$color))))})
      + eval(parse(text =input$pltype))
      + scale_color_gradientn(colors = brewer.pal(length(unique(eval(parse(text=paste0("datafile()$",input$color))))),name = "Dark2"),name = input$color)
      + labs(x = input$X, y = input$Y)
      + (if (input$needfacet){facet_wrap(eval(parse(text = paste0("~",input$facet))),scales = input$freescale)}
      else{theme_gray()})
      + (if (input$needreg){geom_smooth(method=lm,se = FALSE, color = "lightslategray")}
      else{theme_gray()})
  ))

  output$summary <- renderPrint(print(str(datafile())))
  
  output$table <- renderDataTable(
    datafile(), options = list(pageLength = 10)
  )
  
}

shinyApp(ui,server)