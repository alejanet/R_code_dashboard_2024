###
library(shiny)
library(DT)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(tidyverse)
library(shinycssloaders)
#library(rsconnect)

### connect to posit

### get working directory
getwd()

### Google credentials
gs4_deauth()

#### Read google sheets data into R
#(1) share with XXXXXXXXXX@WWWWWWWWWWWW.iam.gserviceaccount.com
df <- read_sheet('URL', sheet = "Reporte")

# CLEANING DATA ------------------------------------------

head(df)
names(df)

### Renaming variables
new_df <- df %>%
  rename(marca = "Marca temporal", prov = "Indique su provincia", ap = "Indique su nombre", ie="Indique la Institución educativa que programó visitar", 
         nomdocente="Indique el nombre del docente con el que programó trabajar", programa="¿Programó una visita a alguna IE?", implementa="¿Pudo implementar la sesión que programó en la IE?",
         motivo_pro="Explique por qué no programó una visita", motivo_im="Explique por qué no pudo implementar la sesión", fecha_visita="Indique la fecha sobre la que desea reportar", 
         nsesion="Indique la sesión implementada", grados_atendidos="Indique el o los grados atendidos",
         area="Indique el área de la sesión que programó", lidera="¿Quién lideró la sesión implementada?",
         ndocentes="Indique el número de docentes de la IE que permanecen en la sesión", cumple="¿En qué medida se cumplió con las actividades planificadas?",
         motivo_no_lid="Explique de manera breve por qué el docente no lideró la sesión", aspec_1_1="Apecto1_1", aspec_1_2="Apecto1_2", aspec_1_3="Apecto1_3",
         aspec_1_4="Apecto1_4", aspec_1_5="Apecto1_5", aspec_2_1="Aspecto2_1", aspec_2_2="Aspecto2_2", aspec_2_3="Aspecto2_3", aspec_2_4="Aspecto2_4", aspec_3_1="Aspecto3_1", aspec_3_2="Aspecto3_2", aspec_3_3="Aspecto3_3",
         aspec_4_1="Aspecto4_1", aspec_4_2="Aspecto4_2", aspec_4_3="Aspecto4_3"
  )

### Keep a group of variables
new_df <- select(new_df, marca, prov, ap, ie, nomdocente, programa, implementa, motivo_pro, motivo_im, fecha_visita, nsesion, grados_atendidos, area, lidera,
                 ndocentes, cumple, motivo_no_lid, aspec_1_1, aspec_1_2, aspec_1_3, aspec_1_4, aspec_1_5, aspec_2_1, aspec_2_2, aspec_2_3, aspec_2_4, aspec_3_1, aspec_3_2, aspec_3_3,
                 aspec_4_1, aspec_4_2, aspec_4_3)

### creating a factor variable
v_ie <- unlist(new_df$ie)
new_df$v_ie <- v_ie
new_df <- new_df[order(new_df$v_ie), ]
f_ie <- as.factor(new_df$v_ie)
new_df$f_ie <- f_ie

### gen var Date simplified
new_df$Date <- as.Date(new_df$fecha_visita)
new_df$sesion_numeric <- as.numeric(gsub("Sesión ", "", new_df$nsesion))

# UI ------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = span(img(src="imagotipo CREER horizontal PNG.png",height=35)),
    titleWidth = 300     
  ),
  dashboardSidebar(
    sidebarMenu(
      #      menuItem("Cobertura", tabName = "cobertura"),
      menuItem("Sesiones", tabName = "sesiones"),
      menuItem("Desempeño docente", tabName = "desempenio"),
      menuItem("Tabla de datos", tabName = "t_datos")
    ) 
    
  ),
  dashboardBody(
    tabItems(
      #      tabItem(tabName = "cobertura",
      #              h4("En construccion ..."),
      #              fluidRow(
      #                box(title = tags$div("IE atendidas", style = "text-align: center;"), width = 2),
      #                box(title = tags$div("Estudiantes atendidos", style = "text-align: center;"), width = 2),
      #                box(title = tags$div("3°", style = "text-align: center;"), width = 2),
      #                box(title = tags$div("4°", style = "text-align: center;"), width = 2),
      #                box(title = tags$div("5°", style = "text-align: center;"), width = 2),
      #                box(title = tags$div("6°", style = "text-align: center;"), width = 2)
      #              ),
      #              fluidRow(
      #                box(title = "Estudiantes atendidos x acompañante", width = 6, plotOutput("graphC1")
      #                ),
      #                box(title = "Docentes atendidos x acompañante", width = 6, plotOutput("graphC2"))
      #              ),
      #              fluidRow(
      #                box(title = "Lista de IE atendidas x acompañante", width = 6, plotOutput("graphC3")
      #                ),
      #                box(title = "Lista de docentes atendidos x acompañante", width = 6, plotOutput("graphC4"))
      #              )
      #      ),
      tabItem(tabName = "sesiones",
              fluidRow(
                column(4,
                       box(width = 12,
                           dateRangeInput(
                             'dateRange',
                             label= "Selecciona rango de fechas:",
                             start = Sys.Date() - 10, end = Sys.Date() + 2
                           ))),
                column(4,
                       box(width = 12,
                           selectInput("selectAP", label = "Selecciona nombre de AP:",choices = c("Todos",unique(new_df$ap)),
                                       selected = "Gabriela R.")
                       )),
                column(4,
                       box(width = 12,
                           selectInput("selectArea", label = "Selecciona el Area:",choices = c("Todos", setdiff(na.omit(unique(new_df$area)), "NA")),
                                       selected = "Comunicación")
                       )
                )),
              fluidRow(
                box(title = "Sesiones no efectuadas", solidHeader = T, width = 4,collapsible = T,
                    div(DT::DTOutput("ses_no_efe"), style="font-size: 70%;")
                ),
                box(
                  title = "Avance acumulativo en la implementacion de sesiones",
                  width=8,
                  plotOutput("barplot1")
                )                
              )
      ),
      tabItem(tabName = "desempenio",
              h4("Las gráficas muestran el puntaje promedio que alcanza cada docente en el acumulativo de visitas en una escala del 1 al 5. "),
              fluidRow(
                column(7,
                       box(width = 12,
                           checkboxGroupInput("ap_d", "Selecciona nombre de AP:", choices=c("Gabriela R.", "Nicole A.", "Sandra P.", "Oscar S.", "Ronal M.", "Segundo A.", "Socorro H."), selected="Gabriela R.", inline=TRUE)
                       ))
              ),
              fluidRow(
                div(
                  id= "desempenio_2",
                  column(
                    width=6,
                    uiOutput("a1_graphs")
                  ),
                  column(
                    width=6,
                    uiOutput("a2_graphs")
                  ),
                  column(
                    width=6,
                    uiOutput("a3_graphs")
                  ),
                  column(
                    width = 6,
                    uiOutput("a4_graphs")
                  )
                )
              )),
      tabItem(tabName = "t_datos",
              fluidRow(
                box(
                  selectInput("ap", label = "Selecciona nombre de AP:",choices = c("Todos",unique(new_df$ap)),
                              selected = "Gabriela R.")),
                box(
                  div(
                    "Descarga de respuestas:", 
                    style = "font-weight: bold; font-size: 14px; color: #333; margin-top: 6px; margin-left: 6px;"
                  ),
                  downloadButton("downloadData", "Excel" )),
                box(
                  title = "Histórico de respuestas", width = 12,
                  DT::dataTableOutput("table")
                )              
                
              )      
      )
    )
  )
)

# SERVER ------------------------------------------

server <- function(input, output, session) {
  
  # tab: sesiones __________________
  
  gg_data1 <- reactive({
    filtered <- new_df
    filtered <- subset(filtered, Date >=input$dateRange[1] & Date<= input$dateRange[2])
    if(input$selectAP != "Todos") {
      filtered <- subset(filtered, ap == input$selectAP)
    }
    if(input$selectArea != "Todos") {
      filtered <- subset(filtered, area == input$selectArea) 
    }
    
    return(filtered)
  })
  
  gg_data2 <- reactive({
    subset(gg_data1(), programa == "Sí" & implementa =="Sí")
  })
  
  output$barplot1 <- renderPlot({
    g1 <- ggplot(gg_data2()) + stat_count(mapping = aes(x=fct_rev(f_ie)), fill="green3") + labs(x ="IE", y = "Numero total de sesiones") + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(f_ie), label=..count..), hjust=2, size=4)
    print(g1)
  })
  
  ## ses_no_efectuadas
  filtered_data_table <- reactive({
    subset(gg_data1(), implementa == "No", select=c("marca","ap","fecha_visita","ie","motivo_im"))
  })
  
  # Render datatable
  output$ses_no_efe <- renderDT({
    datatable(filtered_data_table(),
              options = list(pageLength=10),
              selection = "none",
              colnames = c("marca","ap","fecha_visita","ie","motivo_im"))
  })  
  
  #tab: desempenio __________________
  #aspec_1_1
  
  output$a1_graphs <- renderUI ({
    div(
      style="position: relative",
      tabBox(
        id = "a1_graphs",
        width = NULL,
        height = 400,
        tabPanel(
          title= "Aspecto 1.1",
          withSpinner(
            uiOutput("g_1_1"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 1.2",
          withSpinner(
            uiOutput("g_1_2"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 1.3",
          withSpinner(
            uiOutput("g_1_3"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 1.4",
          withSpinner(
            uiOutput("g_1_4"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 1.5",
          withSpinner(
            uiOutput("g_1_5"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        )       
      )) 
  })
  
  output$g_1_1 <- renderUI({
    plotOutput("g_1_1_x", height = 300)
  })
  
  output$g_1_1_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a1_1 <- aggregate(aspec_1_1 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a1_1, aes(x = aspec_1_1, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_1_1, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 1.1. El docente muestra disposición para comprender la lógica de las sesiones enfocada \nen la atención simultánea y diferenciada de los estudiantes") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  }) 
  
  output$g_1_2 <- renderUI({
    plotOutput("g_1_2_x", height = 300)
  })
  
  output$g_1_2_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a1_2 <- aggregate(aspec_1_2 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a1_2, aes(x = aspec_1_2, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_1_2, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 1.2. Muestra conocimiento y familiaridad de la sesión que implementará \n(lo que denota que ha leído con anticipación y comprendido)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
  })   
  
  output$g_1_3 <- renderUI({
    plotOutput("g_1_3_x", height = 300)
  })
  
  output$g_1_3_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a1_3 <- aggregate(aspec_1_3 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a1_3, aes(x = aspec_1_3, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_1_3, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 1.3. El docente dispone oportunamente de los recursos y espacios  requeridos \npara el desarrollo de la sesión, según la atención diferenciada que se requiera") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
  })  
  
  output$g_1_4 <- renderUI({
    plotOutput("g_1_4_x", height = 300)
  })
  
  output$g_1_4_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a1_4 <- aggregate(aspec_1_4 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a1_4, aes(x = aspec_1_4, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_1_4, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 1.4. El docente realiza ajustes a las sesiones en coherencia con los propósitos \nde aprendizaje y criterios de evaluación, asi como a las características de los estudiantes y su contexto, con la intención de desafiarlos y/o motivarlos") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
  })    
  
  output$g_1_5 <- renderUI({
    plotOutput("g_1_5_x", height = 300)
  })
  
  output$g_1_5_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a1_5 <- aggregate(aspec_1_5 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a1_5, aes(x = aspec_1_5, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_1_5, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 1.5. El docente incorpora con autonomía recursos/actividades/estrategias/sesiones \nen coherencia con la lógica de las sesiones del proyecto CREER en su trabajo o planificación regular") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
  })
  
  output$a2_graphs <- renderUI ({
    div(
      style="position: relative",
      tabBox(
        id = "a2_graphs",
        width = NULL,
        height = 400,
        tabPanel(
          title= "Aspecto 2.1",
          withSpinner(
            uiOutput("g_2_1"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 2.2",
          withSpinner(
            uiOutput("g_2_2"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 2.3",
          withSpinner(
            uiOutput("g_2_3"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 2.4",
          withSpinner(
            uiOutput("g_2_4"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        )       
      )) 
  })  
  
  output$g_2_1 <- renderUI({
    plotOutput("g_2_1_x", height = 300)
  })
  
  output$g_2_1_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a2_1 <- aggregate(aspec_2_1 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a2_1, aes(x = aspec_2_1, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_2_1, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 2.1. La disposición física de los objetos en el aula es pertinente a la atención \nsimultánea y diferenciada de grupos de estudiantes y en relación al \nespacio disponible.  Promueve la autonomía de los y las estudiantes") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })  
  
  output$g_2_2 <- renderUI({
    plotOutput("g_2_2_x", height = 300)
  })
  
  output$g_2_2_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a2_2 <- aggregate(aspec_2_2 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a2_2, aes(x = aspec_2_2, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_2_2, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 2.2. Los estudiantes realizan automáticamente (como hábito) rutinas o procedimientos \nen el aula para realizar diferentes tareas como, por ejemplo: ordenar, regular \nel ruido, salir del salón, limpiar, participar en clase, comer la lonchera, entre otros") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })    
  
  output$g_2_3 <- renderUI({
    plotOutput("g_2_3_x", height = 300)
  })
  
  output$g_2_3_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a2_3 <- aggregate(aspec_2_3 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a2_3, aes(x = aspec_2_3, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_2_3, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 2.3. Las normas de convivencia democrática son usadas tanto por docentes como \nestudiantes en diferentes situaciones pertinentes para la autorregulación del comportamiento") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })
  
  output$g_2_4 <- renderUI({
    plotOutput("g_2_4_x", height = 300)
  })
  
  output$g_2_4_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a2_4 <- aggregate(aspec_2_4 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a2_4, aes(x = aspec_2_4, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_2_4, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 2.4. Existe una relación de confianza entre docente y estudiantes \nque motiva un intercambio y comunicación entre todos, así como, el \ninvolucramiento de los estudiantes en las actividades planteadas o acordadas. \nEl docente valora a sus estudiantes y muestra tener altas \nexpectativas de cada uno de ellos") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })
  
  output$a3_graphs <- renderUI ({
    div(
      style="position: relative",
      tabBox(
        id = "a3_graphs",
        width = NULL,
        height = 400,
        tabPanel(
          title= "Aspecto 3.1",
          withSpinner(
            uiOutput("g_3_1"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 3.2",
          withSpinner(
            uiOutput("g_3_2"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 3.3",
          withSpinner(
            uiOutput("g_3_3"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        )       
      )) 
  })
  
  output$g_3_1 <- renderUI({
    plotOutput("g_3_1_x", height = 300)
  })
  
  output$g_3_1_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a3_1 <- aggregate(aspec_3_1 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a3_1, aes(x = aspec_3_1, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_3_1, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 3.1. El docente formula tareas o preguntas abiertas que invita a \nlos estudiantes a reflexionar, crear o indagar en relación a las actividades que \nrealiza de la sesión. Los estudiantes elaboran sus ideas con calma, por ejemplo: \nexplican sus formas de pensar o sus acciones, comparan o contrastan sus ideas, \nargumentan una postura, hacen predicciones o hipótesis, etc") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })  
  
  output$g_3_2 <- renderUI({
    plotOutput("g_3_2_x", height = 300)
  })
  
  output$g_3_2_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a3_2 <- aggregate(aspec_3_2 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a3_2, aes(x = aspec_3_2, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_3_2, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 3.2. El docente hace uso de los recursos y materiales concretos \n(cuadernos de autoaprendizaje, fichas de trabajo diferenciados, material estructurado y no estructurado, \nentre otros) propuestos en las sesiones, en función de las necesidades de aprendizaje \ncomunes y diferenciadas de los estudiantes") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  }) 
  
  output$g_3_3 <- renderUI({
    plotOutput("g_3_3_x", height = 300)
  })
  
  output$g_3_3_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a3_3 <- aggregate(aspec_3_3 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a3_3, aes(x = aspec_3_3, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_3_3, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 3.3. El docente concatena las actividades de la sesión con los propósitos \nde la sesión y las ideas que tienen o van construyendo los y las \nestudiantes o las necesidades que identifica") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  }) 
  
  output$a4_graphs <- renderUI ({
    div(
      style="position: relative",
      tabBox(
        id = "a4_graphs",
        width = NULL,
        height = 400,
        tabPanel(
          title= "Aspecto 4.1",
          withSpinner(
            uiOutput("g_4_1"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 4.2",
          withSpinner(
            uiOutput("g_4_2"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        ),
        tabPanel(
          title= "Aspecto 4.3",
          withSpinner(
            uiOutput("g_4_3"),
            type=4,
            color="#90EE90",
            size = 0.7
          )
        )       
      )) 
  })
  
  output$g_4_1 <- renderUI({
    plotOutput("g_4_1_x", height = 300)
  })
  
  output$g_4_1_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a4_1 <- aggregate(aspec_4_1 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a4_1, aes(x = aspec_4_1, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_4_1, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 4.1. El docente identifica las fortalezas y aspectos de mejora del \naprendizaje de los estudiantes a través del análisis de productos escritos, \npreguntas orales de ida y vuelta, así como, repreguntas") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })  
  
  output$g_4_2 <- renderUI({
    plotOutput("g_4_2_x", height = 300)
  })
  
  output$g_4_2_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a4_2 <- aggregate(aspec_4_2 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a4_2, aes(x = aspec_4_2, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_4_2, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 4.2. El docente brinda retroalimentaciones oportunas y enfocadas \nen aspectos relevante de la tarea que los estudiantes pueden mejorar, por medio \nde preguntas, sugerencias y ejemplos y contraejemplos que genere la \nreflexión sobre lo que han logrado y lo que necesitan mejorar. Los estudiantes tienen \ntiempos y espacios para mejorar su trabajo") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })  
  
  output$g_4_3 <- renderUI({
    plotOutput("g_4_3_x", height = 300)
  })
  
  output$g_4_3_x <- renderPlot({
    filtered_data_d <- subset(new_df, ap %in% input$ap_d)
    #calcula promedio
    avg_a4_3 <- aggregate(aspec_4_3 ~ nomdocente, data = filtered_data_d, FUN = function(x) mean(x, na.rm = TRUE))
    
    # Create ggplot object for horizontal bar plot
    ggplot(avg_a4_3, aes(x = aspec_4_3, y = nomdocente)) +
      geom_bar(stat = "identity", fill = "blue") +  
      geom_text(aes(label = round(aspec_4_3, 1)), hjust = -0.2, vjust = 0.5, size = 3) +        
      labs(x = NULL, y = "Docente", caption ="Aspecto 4.3. El docente usa el error como una oportunidad de aprendizaje para \nlos estudiantes, ayudándolos con preguntas a analizar cómo llegó al error y \ncómo lo resolvería de otras maneras") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.caption = element_text(size = 12))  
    
  })
  
  #tab: tabla de datos __________________
  
  #read data, invalidate every second
  rt_data <- reactive({
    invalidateLater(1000)
    filtered2 <- new_df
    req(input$ap)
    #filter(new_df, ap %in% input$ap)
    if(input$ap != "Todos") {
      filtered2 <- subset(new_df, ap == input$ap)
    }
    return(filtered2)    
  })
  
  #render dataTable, use server mode
  #isolate the reactive data so the table doesn't re-render
  output$table <- DT::renderDataTable({
    isolate({rt_data()})}, options = list(paging=F, searching=F, processing=F, scrollX = TRUE))
  
  #download data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rt_data(), file, row.names = FALSE)
    }
  )
  
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