library(plotly)
library(ggplot2)
library(dplyr)
library(lazyeval)
devtools::load_all(".")


#Indlæs initialiseringsobjekt
pca_df <<- readRDS(file = "pca2.rds")
doc_labs <<- readRDS(file = "labels2.rds")


ui <- navbarPage("visApp",id="nav",
                 tabPanel("Plot",
                          uiOutput("variabela"),
                          uiOutput("variabelb"),
                          actionButton(inputId="clear",label="Clear"),
                          fluidRow(plotlyOutput("p",height="600px")),
                          fluidRow(textOutput("mes"))),
                 tabPanel("Valg data",
                          downloadButton('downloadData', 'Hent data som csv'),
                          fluidRow(tableOutput("tab"))))

server <- function(input, output) {


  #####################################
  output$variabela <- renderUI({
    selectInput(inputId = "variabelx",label="Vælg variabel X",choices=unique(colnames(pca_df)),selected="PC1",selectize = TRUE)
  })
  output$variabelb <- renderUI({
    selectInput(inputId = "variabely",label="Vælg variabel Y",choices=unique(colnames(pca_df)), selected="PC2",selectize = TRUE)
  })

  #Datasættet
  mdldata <- pca_df
  mdldata$labs <- as.factor(doc_labs)
  #Datamanipulation

  dat <- reactive({
    #De 2 variable
    var1 <- input$variabelx
    var2 <- input$variabely
    #Datasæt
    dat <- mdldata[!is.na(mdldata[[var1]]) & is.finite(mdldata[[var1]]) &
                     !is.na(mdldata[[var2]]) & is.finite(mdldata[[var2]]),]
    dat <- dat %>% group_by(labs) %>% mutate(id=paste0(1:n(),"-",as.numeric(labs)))
    dat
  })


  output$p <- renderPlotly({
    var_1 <- input$variabelx
    var_2 <- input$variabely
    p <- ggplot2::ggplot(data=dat(), aes_string(x = var_1, y = var_2, col = "labs")) + #,col="Antal"
      geom_point(alpha=0.5) #+
#      scale_color_manual("labs",breaks=c("Contracts", "Non-Contracts"),values=c("blue","#FF0000"),na.value=NA)

    ggplotly(p,source="select")
  })


  id <- reactiveValues(id=NULL, mes=NULL, grp=NULL)

  event.data1 <- reactive({
    event.data1 <- as.data.frame(event_data("plotly_click", source = "select"))
    event.data1
  })
  event.data2  <- reactive({
    event.data2 <- as.data.frame(event_data("plotly_selected",source= "select"))
    event.data2
  })

  observeEvent(event.data1(),{
    id$id <- c(id$id,as.vector(paste0(round(event.data1()$pointNumber+1,0),"-",round(event.data1()$curveNumber+1,0))))
  })
  observeEvent(event.data2(),{
    id$id <- c(id$id,as.vector(paste0(round(event.data2()$pointNumber+1,0),"-",round(event.data2()$curveNumber+1,0))))

  })
  observeEvent(input$clear,{
    id$id <- NULL
    id$grp <- NULL
  })

  output$tab <- renderTable({
    tmpdata <- dat()
    tmpdata <- tmpdata[tmpdata$id %in% id$id,]
    tmpdata[,1:10]
  }, include.rownames=TRUE)


  observeEvent(event.data1(),{
    id$mes <- as.vector(round(event.data1()$pointNumber+1,0))
  })
  observeEvent(event.data2(),{
    id$mes <- as.vector(round(event.data2()$pointNumber+1,0))
  })


  output$mes <- renderText({
    tmptext <- as.character(id$mes)
    if(length(tmptext)!=0){
      paste("Observation(er) med ID: ", paste(tmptext,",",collapse=""), "er tilføjet")}
  })

  finaltab <- reactive({

    tmpdata <- dat()
    tmpdata <- tmpdata[tmpdata$id %in% id$id,]
    tmpdata[,1:10]

  })


  output$downloadData <- downloadHandler(
    filename =paste0("Outputdata", '.csv', sep=''),
    content = function(file) {
      write.csv2(finaltab(), file) })

}
# Run the application
shinyApp(ui = ui, server = server)



