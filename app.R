setwd("C:/Users/wuxx1/Downloads/2019 Research")
directory<- getwd()
location <- paste0(directory, "/agesx4Di.csv")
sample.data <- read.csv(location)



library(shiny)
library(shinydashboard)
library(lubridate)

#fluidPage
#dashboardPage
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title="Twin & Family"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filter Data", tabName="fd", icon=icon("dashboard")),
      menuItem("Age Distribution", tabName="p2", icon=icon("th"))
    )
  ),
  dashboardBody(
      
      tags$head(tags$style(HTML('
                                .main-header .logo {
                                font-family: "Georgia", Times, "Times New Roman", serif;
                                font-weight: bold;
                                font-size: 20px;
                                }'))
      ),
      tabItems(
        tabItem(tabName = "fd",
              fluidRow(
                tags$head(
                  tags$style(HTML("
                                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                  
                                  h1 {
                                  font-family: 'Georgia', cursive;
                                  font-weight: 500;
                                  line-height: 1.1;
                                  color: #FFF00;
                                  }
                                  
                                  "))
                ),

                headerPanel("All information table"),
                column(2,sliderInput("date", 
                                     "Birthday(BDAY): ",
                                     min(dmy(sample.data$BDAY)),
                                     max(dmy(sample.data$BDAY)),
                                     value = c(min(dmy(sample.data$BDAY)),
                                               max(dmy(sample.data$BDAY))))),
                column(2,selectInput("Sex",
                                     "Project sex(IDPR): ",
                                     c("All", "Male", "Female"))),
                column(2,selectInput("Scode",
                                     "Sibling code(IDSC): ",
                                     c("All", "Twin a", "Twin b", "Triplet"))),
                column(2,selectInput("Tsex",
                                     "Twin pair sex(IDFAMSEX): ",
                                     c("All", "Male", "Female"))),

                column(2, checkboxGroupInput("Time",
                                             "The ith time attend the test: ",
                                             selected = NULL,
                                             c("Intake",
                                               "Follow-up1",
                                               "Follow-up2",
                                               "Follow-up3",
                                               "Follow-up4",
                                               "Follow-up5",
                                               "Follow-up6"))),
                

                column(12, DT::dataTableOutput('table'), style = "overflow-x: scroll; background-color: orange; border: 5px solid orange;")
              )
        ),
        tabItem(tabName = "p2",
                fluidRow(
                  tags$head(
                    tags$style(HTML("
                                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                    
                                    h1 {
                                    font-family: 'Georgia', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #FFF00;
                                    }
                                    
                                    "))
                    ),
                  
                  headerPanel("Statistics of age"),
                  column(2,selectInput("Scode2",
                                       "Sibling Code(IDSC): ",
                                       c("All", "Twin a", "Twin b", "Triplet"))),
                  column(2,selectInput("Sex2",
                                       "Project sex(IDPR): ",
                                       c("All", "Male", "Female"))),
                  column(2, selectInput("Time2",
                                        "The ith time attend the test: ",
                                        c("Intake",
                                          "Follow-up1",
                                          "Follow-up2",
                                          "Follow-up3",
                                          "Follow-up4",
                                          "Follow-up5",
                                          "Follow-up6"))),
                  column(2, sliderInput(inputId = "Bins",
                                        "Number of bins: ",
                                        min = 1,
                                        max = 50,
                                        value = 30)),
                  column(12, plotOutput('age_hist')),
                  column(12, verbatimTextOutput("age_summary"))
                  )
                
                )
        
        
        
        
      )
    
  )
)







server <- function(input, output) {
  output$age_hist <- renderPlot({
     data2<- sample.data
     if(input$Scode2!="All") {
       if(input$Scode2 == "Twin a") {
         data2 <- data2[data2$IDSC == 0, ]
       }
       else if(input$Scode2 == "Twin b") {
         data2 <- data2[data2$IDSC == 1, ]
       }
       else {
         data2 <- data2[data2$IDSC == 2, ]
       }
     }
     if(input$Sex2!="All") {
       if(input$Sex2 == "Female") {
         data2 <- data2[data2$IDPR == 2, ]
       }
       else {
         data2 <- data2[data2$IDPR == 1, ]
       }
     }
     #print(input$Time2)
     if(input$Time2 == "Intake") {
       data2 <- data2$AGE_IN
     }
     else if(input$Time2 == "Follow-up1") {
       data2 <- data2$AGE_FU1
     }
     else if(input$Time2 == "Follow-up2") {
       data2 <- data2$AGE_FU2
     }
     else if(input$Time2 == "Follow-up3") {
       data2 <- data2$AGE_FU3
     }
     else if(input$Time2 == "Follow-up4") {
       data2 <- data2$AGE_FU4
     }
     else if(input$Time2 == "Follow-up5") {
       data2 <- data2$AGE_FU5
     }
     else if(input$Time2 == "Follow-up6"){
       data2 <- data2$AGE_FU6
     }
     ## omit nas
     data2 <- data2[!is.na(data2)]
   
     bins <- seq(min(data2), max(data2), length.out = input$Bins+1)
     hist(data2, breaks = bins, col = "#75AADB", border = "white",
          xlab = "Age",
          main = "Histogram of age distribution in ith visit")
  }
  )
  
  output$age_summary <- renderPrint({
    data2<- sample.data
    if(input$Scode2!="All") {
      if(input$Scode2 == "Twin a") {
        data2 <- data2[data2$IDSC == 0, ]
      }
      else if(input$Scode2 == "Twin b") {
        data2 <- data2[data2$IDSC == 1, ]
      }
      else {
        data2 <- data2[data2$IDSC == 2, ]
      }
    }
    if(input$Sex2!="All") {
      if(input$Sex2 == "Female") {
        data2 <- data2[data2$IDPR == 2, ]
      }
      else {
        data2 <- data2[data2$IDPR == 1, ]
      }
    }

    
    #print(input$Time2)
    if(input$Time2 == "Intake") {
      data2 <- data2$AGE_IN
    }
    else if(input$Time2 == "Follow-up1") {
      data2 <- data2$AGE_FU1
    }
    else if(input$Time2 == "Follow-up2") {
      data2 <- data2$AGE_FU2
    }
    else if(input$Time2 == "Follow-up3") {
      data2 <- data2$AGE_FU3
    }
    else if(input$Time2 == "Follow-up4") {
      data2 <- data2$AGE_FU4
    }
    else if(input$Time2 == "Follow-up5") {
      data2 <- data2$AGE_FU5
    }
    else if(input$Time2 == "Follow-up6"){
      data2 <- data2$AGE_FU6
    }
    ## omit nas
    data2 <- data2[!is.na(data2)]
    summary(data2)
    
  })
  output$table <- DT::renderDataTable(DT::datatable({
    data <-sample.data
    mindate <- input$date[1]
    maxdate <- input$date[2]
    cols.dont.want <- c("PID1", "PID2", "PID3", "PID4", "PID5", "PID6")
    data <- data[, ! names(data) %in% cols.dont.want, drop = F]
    
    data <- data[dmy(data$BDAY) >= as.Date(mindate) &
                 dmy(data$BDAY) <= as.Date(maxdate), ]
    
    if (input$Sex != "All") {
      if (input$Sex == "Female") {
        data <- data[data$IDPR == 2, ]
      }
      else {
        data <- data[data$IDPR == 1, ]
      }
    }
    if (input$Scode != "All"){
      if (input$Scode == "Twin a") {
        data <- data[data$IDSC == 0, ]
      }
      else if(input$Scode == "Twin b") {
        data <- data[data$IDSC == 1, ]
      }
      else {
        data <- data[data$IDSC==2, ]
      }
    }
    if ( input$Tsex != "All") {
      if (input$Tsex == "Female") {
        data <- data[data$IDFAMSEX == 2, ]
      }
      else {
        data <- data[data$IDFAMSEX == 1, ]
      }
    }

    intake.col <- c("AGE_IN", "VT_IN", "VSTAGE_IN",	"VDATE_IN",	"VINTBY_IN")
    f1.col <- c("AGE_FU1", "VT_FU1",	"VSTAGE_FU1",	"VDATE_FU1",	"VINTBY_FU1")
    f2.col <- c("AGE_FU2", "VT_FU2",	"VSTAGE_FU2",	"VDATE_FU2",	"VINTBY_FU2")
    f3.col <- c("AGE_FU3", "VT_FU3",	"VSTAGE_FU3",	"VDATE_FU3",	"VINTBY_FU3")
    f4.col <- c("AGE_FU4", "VT_FU4",	"VSTAGE_FU4",	"VDATE_FU4",	"VINTBY_FU4")
    f5.col <- c("AGE_FU5", "VT_FU5",	"VSTAGE_FU5",	"VDATE_FU5",	"VINTBY_FU5")
    f6.col <- c("AGE_FU6", "VT_FU6",	"VSTAGE_FU6",	"VDATE_FU6",	"VINTBY_FU6")
    
    if(!("Intake" %in% input$Time)) {
      data <- data[, ! names(data) %in% intake.col, drop = F]
    }
    if(!("Follow-up1" %in% input$Time)) {
      data <- data[, ! names(data) %in% f1.col, drop = F]
    }
    if(!("Follow-up2" %in% input$Time)) {
      data <- data[, ! names(data) %in% f2.col, drop = F]
    }
    if(!("Follow-up3" %in% input$Time)) {
      data <- data[, ! names(data) %in% f3.col, drop = F]
    }
    if(!("Follow-up4" %in% input$Time)) {
      data <- data[, ! names(data) %in% f4.col, drop = F]
    }
    if(!("Follow-up5" %in% input$Time)) {
      data <- data[, ! names(data) %in% f5.col, drop = F]
    }
    if(!("Follow-up6" %in% input$Time)) {
      data <- data[, ! names(data) %in% f6.col, drop = F]
    }
    
    date.col <- c("BDAY", "VDATE_IN", "VDATE_FU1", "VDATE_FU2", "VDATE_FU3", "VDATE_FU4", "VDATE_FU5", "VDATE_FU6")
    data.date.index <- which(names(data)%in%date.col)
    for (i in 1:length(data.date.index)) {
      col <- data.date.index[i]
      data[, col] <- dmy(data[, col])
    }
    
    
    data <- data[!apply(data, 1, function(x) any(is.na(x))), ]
    
  }))
  
}

shinyApp(ui = ui, server = server)