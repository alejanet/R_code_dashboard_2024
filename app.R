
####
library(shiny)
library(DT)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(tidyverse)

#### get working directory
getwd()
setwd("/Users/Usuario/OneDrive - Grupo de Análisis para el Desarrollo/Documentos/Proyecto Creer/Fase 2/CREER_monitoreo/0_git/creer-db1")

#### Google credentials
gs4_deauth()
googlesheets4::gs4_auth(path = './token/turing-lane-413803-27f19c8788ec.json')

#### Read google sheets data into R
#(1) share with spreadsheet-user-creer-1@turing-lane-413803.iam.gserviceaccount.com
df <- read_sheet('https://docs.google.com/spreadsheets/d/17ViTAMf0aJnfDL271QkO6wVeH5htwKirz7Ek9h0id8g/edit#gid=0', sheet = "Reporte")

#### Cleaning data
head(df)
names(df)

## Renaming variables
new_df <- df %>%
  rename(marca = "Marca temporal", prov = "Indique su provincia", ap = "Nombre del AP", ie="Indique la Institución educativa que programó visitar", 
         nomdocente="Indique el nombre del docente con el que programó trabajar", implementa="¿Pudo implementar la sesión que programó en la IE?", fecha_visita="¿Para qué fecha programó su visita a la IE?",
         nsesion="Indique la sesión implementada", grados_atendidos="Indique el o los grados atendidos",
         area="Indique el área de la sesión implementada", lidera="¿Quién lideró la sesión implementada?",
         ndocentes="Indique el número de docentes que permanecen en la sesión", cumple="¿En qué medida se cumplió con las actividades planificadas?"
         )

## Drop NA observations(missing)
#new_df <- new_df[!is.na(new_df$prov),]
#new_df <- new_df[!is.na(new_df$ap),]

## Keep a group of variables
new_df <- select(new_df, marca, prov, ap, ie, nomdocente, implementa, fecha_visita, nsesion, grados_atendidos, area, lidera,
             ndocentes, cumple)
## Me quedo con obs nuevas
#new_df <- subset(new_df, !grepl("Opción", ap))

# Clear a data frame from memory
#rm(new_df)

#turn into a factor variable
v_ie <- unlist(new_df$ie)
new_df$v_ie <- v_ie
new_df <- new_df[order(new_df$v_ie), ]
f_ie <- as.factor(new_df$v_ie)
new_df$f_ie <- f_ie

#table(new_df$f_ie)
new_df$Date <- as.Date(new_df$fecha_visita)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = span(img(src="imagotipo CREER horizontal PNG.png",height=35), "CREER 1"),
    titleWidth = 300     
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cobertura", tabName = "cobertura"),
      menuItem("Sesiones", tabName = "sesiones"),
      menuItem("Tabla de datos", tabName = "t_datos")
    ) 
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cobertura",
      ),
      tabItem(tabName = "sesiones",
              h4("Descripcion tab 2"),
              fluidRow(
                box(
                  dateRangeInput(
                    'dateRange',
                    label= "Selecciona rango de fechas:",
                    start = Sys.Date() - 10, end = Sys.Date() + 2
                  )
                ),
                box(
                  title = "Avance acumulativo en la implementacion de sesiones",
                  width=8,
                  plotOutput("barplot1")
                )
              )
      ),
      tabItem(tabName = "t_datos",
              h4("Descripcion tab 3"),
              fluidRow(
                box(
                  checkboxGroupInput(
                    "prov",
                    "Selecciona la provincia:",
                    choices=c("Jaén", "Cajabamba"),
                    selected = "Cajabamba"
                  )
                ),
                box(
                  title = "table 1", width = 12,
                  DT::dataTableOutput("table")
                )              
                
              )      
      )
    )
  )
)

#server
server <- function(input, output, session) {
  
  gg_data1 <- reactive({
    subset(new_df, Date >=input$dateRange[1] & Date<= input$dateRange[2])
  })
  
  output$barplot1 <- renderPlot({
    g1 <- ggplot(gg_data1()) + stat_count(mapping = aes(x=fct_rev(f_ie)), fill="green3") + labs(x ="IE", y = "Numero total de sesiones") + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(f_ie), label=..count..), hjust=2, size=4)
    print(g1)
  })
  
  #read data, invalidate every second
  rt_data <- reactive({
    invalidateLater(1000)
    new_df
    req(input$prov)
    filter(new_df, prov %in% input$prov)
    
  })
  
  #render dataTable, use server mode
  #isolate the reactive data so the table doesn't re-render
  output$table <- DT::renderDataTable({
    isolate({rt_data()})}, options = list(paging=F, searching=F, processing=F))
  
  #declare proxy to control dataTable
  proxy <- dataTableProxy("table")
  
  #when data changes, replace data using server-side proxy
  #set resetPaging/clearSelection to FALSE to avoid loosing interactivity mechanics
  #https://rdrr.io/cran/DT/man/replaceData.html
  observe({
    replaceData(
      proxy,
      rt_data(),
      resetPaging=F,
      clearSelection=F
    )
  })
}

shinyApp(ui, server)


