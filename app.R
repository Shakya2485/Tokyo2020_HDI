library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(plyr)
library(Rcpp)
library(ggcorrplot)
ui <- dashboardPage(
  dashboardHeader(title= " Tokyo Olympics 2020 versus HDI Index ",
                  titleWidth = 400,
                  tags$li(class = "dropdown", tags$a(href="https://github.com/Shakya2485",icon("github"), "My Github", target ="_blank" ))
                  
                  ),
  
  dashboardSidebar(
    #sidebarmenu
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Dataset",tabName="data",icon = icon("database")),
      menuItem(text = "Visualisation", tabName= "visual", icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'visual' && input.tab2 == 'bliss'",selectInput(inputId = "var1",label ="Select Variable" , choices = select(mydata,4:9) %>% names())),
      conditionalPanel("input.sidebar == 'visual' && input.tab2 == 'pass'",selectInput(inputId = "var2",label ="Select Variable" , choices = select(mydata,5:9) %>% names()))
      
    )
  ),
  dashboardBody(
    tabItems(
      #first tab item
      tabItem(tabName="data",
              tabBox(id ="tab1",width =12,
                     tabPanel("Subject",icon = icon("address-card"),fluidRow(
                       column(width=8, tags$img(src = "https://img.olympicchannel.com/images/image/private/f_auto/t_s_pog_staticContent_hero_xl_2x/primary/tyf8qnedqshwpc01qlfh",width = 600,height=300),
                              tags$br() ,
                              tags$a(" logo designed by Asao Tokolo (official logo of Tokyo Olympics 2020) ", align = "center"),
                       column(width = 12,tags$br() ,
                               tags$p("The report examined the medal tally of developed and developing countries participating in the Tokyo
Olympics 2020. Based on their(countries) HDI index, this report aims to show a relationship between the
quality wise (Gold, Silver, and Bronze) or quantity wise (total no. of medals) of medals in the Tokyo
20’olympics for nations.
",align="Topright"))
                              )
                     )),
                     tabPanel("Data",icon = icon("address-card"),dataTableOutput("dataTable")),
                     tabPanel("Structure",icon = icon("address-card"),verbatimTextOutput("structure")),
                     tabPanel("Summary",icon = icon("address-card"),verbatimTextOutput("summary")),
                     
              )
              ),
      #second tab item
      
      tabItem(tabName = "visual",
              tabBox(id="tab2",width =12,
                     tabPanel(title = " Tokyo Olympics 2020 versus HDI Index ", value = "pass", plotlyOutput("bar")),
                     tabPanel(title = " Distribution ", value = "bliss",plotlyOutput("chartplot")),
                     tabPanel(title = " Distribution compare", value = "pass",plotlyOutput("histplot")),
                     tabPanel(title = " Spread ", value = "bliss", plotlyOutput("jitter")),
                     tabPanel(title = " Density ", value = "bliss",plotlyOutput("density")),
                     tabPanel(title = " Correlation ", plotlyOutput("cor")),
                     tabPanel(title = " Relation ",radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                              withSpinner(plotlyOutput("scatter")), value="bliss"),
                     side = "left"
                     
                     
                     
                     
                     ),
              
              )
     
      
      
    )
    
  )
)

server <- function(input, output) { 
  
  #Structure
  output$structure <- renderPrint(
    str(mydata)
  )
  
  #Summary
  output$summary <- renderPrint(
    summary(mydata)
  )
  
  #DataTable
  output$dataTable <- renderDataTable(
    mydata
  )
  #stacked hist
  library(ggplot2)
  library(plotly)
  output$chartplot <- renderPlotly({
   
    hi_plot <- plot_ly(data=mydata,x = ~get(input$var1),type= "histogram") %>%
      layout(xaxis=list(title=input$var1))
    
    box_plot <- plot_ly(data=mydata,x = ~get(input$var1),type= "box") %>%
      layout(yaxis=list(showticklabels= T))
    
    subplot(hi_plot,box_plot,nrows = 2) %>% 
      layout(title = "Distribution chart",
             yaxis = list(title="Frequency"))
    
  })
  #Histogram by medals of Developed and Developing Countries
  #D= Developed and N = Developing Countries
  
  output$histplot <- renderPlotly({
    
    his2 <- ggplot(mydata,aes(get(input$var2),color=HDI))+geom_histogram(fill="white", alpha=0.5, position="identity")+
      labs(title="Histogram by medals(or Variable) of Developed and Developing Countries\n D= Developed and N = Developing Countries",x = input$var2) +
      theme(plot.title = element_text(color="black",size = 8,face = "bold"))
    
    ggplotly(his2)
  })
  
  #jitter and box for developing and developed
  
  output$jitter <- renderPlotly({
    
    jit <- ggplot(mydata,aes(HDI,get(input$var1),color=HDI))+geom_jitter()+geom_boxplot()+
      labs(title="Jitter and Box plot by medals(or Variable) of Developed and Developing Countries\n D= Developed and N = Developing Countries",y = input$var1,x ="HDI")+
      theme(plot.title = element_text(color="black",size = 8,face = "bold"))
    
    ggplotly(jit)
    
  })
  
  output$density <- renderPlotly({
    
    mu <- ddply(mydata, "HDI", summarise, grp.mean=mean(Total))
    den<-ggplot(mydata, aes(get(input$var1), color=HDI)) +geom_density()+geom_vline(data=mu, aes(xintercept=grp.mean, color=HDI),
                                                                               linetype="dashed")+
      labs(title='Density plot for Developed and Developing countries by total medals\n D= Developed and N = Developing Countries ',x =input$var1,y="density")+
      theme(plot.title = element_text(color="black",size = 8,face = "bold"))
    
    ggplotly(den)
  })
  
  #scatter
  output$scatter <- renderPlotly({
    
    scat <- ggplot(mydata,aes(HDI.index,get(input$var1),color=HDI,shape=HDI))+geom_point()+geom_smooth(method = get(input$fit))+
      labs(title="Scatter plot of medals(or variable) against HDI indices",x = "HDI index",y = input$var1)+
      theme(plot.title = element_text(color="black",size = 8,face = "bold"))
    
    ggplotly(scat)
  })
  
  #corrplot
  output$cor <- renderPlotly({

    df <- mydata %>%
      select(select(mydata,5:9) %>% names())

    # Compute a correlation matrix
    corr <- round(cor(df),5)

    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(df)

    corr.plot <- ggcorrplot(
      corr,
      hc.order = TRUE,
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )

    ggplotly(corr.plot)

  })
  #bar
  
  output$bar <- renderPlotly({
    mydata %>% 
      plot_ly() %>% 
      add_bars(x=~HDI.index, y=~get(input$var2)) %>% 
      layout(title = paste("HDI wise", input$var2),
             xaxis = list(title = "HDI Index"),
             yaxis = list(title = paste(input$var2) ))
  })
  
  }

shinyApp(ui, server)
