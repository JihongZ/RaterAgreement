library(shiny)
library(DT)
library(psy)
# Define UI
ui <- shinyUI(fluidPage(

  fileInput('target_upload', '点击上传数据',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  radioButtons("separator","CSV格式数值间隔为: ",choices = c(",",";",":"), selected=",",inline=TRUE),
  mainPanel(
    textOutput("wkappa")
  ),
  mainPanel(
    tableOutput("ICC")
  )
  ,
  DT::dataTableOutput("sample_table"),
  mainPanel(
    textOutput("copyright")
  )
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  output$copyright<- renderText({"App made by Zhehan Jiang"})
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$wkappa<- renderText({
    KappaRes<-c('数据还未上传')
    df <- df_products_upload()
    if(!is.null(df)){
      WKAPPAcoef<-psy::wkappa(df)
      KappaRes<-c('Kappa系数是',round(WKAPPAcoef$kappa,3))
    }
    KappaRes
  })
  output$ICC<- renderTable({
    ICCres<-c('数据还未上传')
    df <- df_products_upload()
    if(!is.null(df)){
      ICCcoef<-psy::icc(df)
      ICCres<-cbind(names(ICCcoef),
                    unlist(ICCcoef))
      colnames(ICCres)<-c('指标','估计结果')
      ICCres
    }
  }) 
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)