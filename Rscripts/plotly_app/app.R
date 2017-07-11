library(plotly)
library(ggplot2)
library(dplyr)
library(lazyeval)

Sys.setlocale("LC_ALL", 'en_DK.utf-8')

#Indlæs initialiseringsobjekt
pca_df <<- readRDS(file = "pca.rds")[,1:10]
doc_labs <<- readRDS(file = "labels.rds")
doc_labs <- lapply(doc_labs, function(x) {
    if (is.character(x)) {as.factor(x)} else x}) %>% as.data.frame
doc_labs$path <- enc2utf8(doc_labs$path)
#validUTF8(doc_labs$path) %>% sum


ui <- navbarPage("visApp",id="nav",
                 tabPanel("Plot",
                          uiOutput("variabela"),
                          uiOutput("variabelb"),
                          uiOutput("tier"),
                          actionButton(inputId="clear",label="Clear"),
                          fluidRow(plotlyOutput("p",height="1200px", width = "1600px")),
                          # fluidRow(tableOutput("rotation_x")),
                          # fluidRow(tableOutput("rotation_y")),
                          fluidRow(textOutput("mes"))),
                 tabPanel("Valgt data",
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

  output$tier <- renderUI({
    selectInput(inputId = "tier",label="Vælg et kategoriserings-niveau",choices=c("tier_0", "tier_1", "tier_2", "tier_3"), selected="tier_0",selectize = TRUE)
  })


  #Datasættet
  mdldata <- pca_df
  mdldata <- cbind(pca_df, doc_labs)

  #Datamanipulation

  dat <- reactive({
    #De 2 variable
    var1 <- input$variabelx
    var2 <- input$variabely
    tier <- input$tier
    #Datasæt
    dat <- mdldata[!is.na(mdldata[[var1]]) & is.finite(mdldata[[var1]]) &
                     !is.na(mdldata[[var2]]) & is.finite(mdldata[[var2]]),]
    dat <- dat[!is.na(dat[[tier]]),]
    #mutate_ skal bruge en lazyeval for at kunne tage input$tier i stedet for tier_0
    dat <- dat %>% group_by_(tier) %>%
      mutate_("index" = lazyeval::interp(~paste0(1:n(),"-",as.numeric(x)), x = as.name(tier)))
    dat
  })

  # rotation_a <- reactive({
  #   var1 <- input$variabelx
  #   out <- rotations[[var1]]
  #   out
  # })
  # rotation_b <- reactive({
  #   var2 <- input$variabely
  #   out <- rotations[[var2]]
  #   out
  # })


  output$p <- renderPlotly({
    var_1 <- input$variabelx
    var_2 <- input$variabely
    tier <- input$tier
    text <- "PC10"
    p <- ggplot2::ggplot(data=dat(), aes_string(x = var_1, y = var_2, col = tier, text = text)) +
      geom_point(alpha=0.5)

    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    ggplotly(p,source="select") %>% layout(autosize = F, width = 1000, height = 600, margin = m)
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
    tmpdata <- tmpdata[tmpdata$index %in% id$id,]
    tmpdata[,c("id", "path", "tier_0", "tier_1", "tier_2", "tier_3")]
  }, include.rownames=TRUE)

  # output$rotation_x <- renderTable({
  #   rotation_a()
  # })
  # output$rotation_y <- renderTable({
  #   rotation_b()
  # })

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
    tmpdata <- tmpdata[tmpdata$index %in% id$id,]
    tmpdata[,c("id", "path", "tier_0", "tier_1", "tier_2", "tier_3")]

  })


  output$downloadData <- downloadHandler(
    filename =paste0("Outputdata", '.csv', sep=''),
    content = function(file) {
      write.csv2(finaltab(), file) })

}
# Run the application
shinyApp(ui = ui, server = server)



