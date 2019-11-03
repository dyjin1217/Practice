library(shinythemes)
library(shiny)
library(DT)
library(readxl)

dat <- NULL
dat2 <- NULL

ui <- navbarPage(
  "미세먼지 Viewer",
   tabPanel("데이터 및 기간선택 ",
      sidebarPanel(
       h4("데이터 선택"),
       checkboxInput("KA","대한민국"),
       checkboxInput("CN","중국"),
       checkboxInput("UP","업로드"),
       actionButton("dlBtn", "데이터 로드"),
       width=3
      ),
      mainPanel(
       dataTableOutput("dat")
      )
   ),
   tabPanel("전처리",
      sidebarPanel(
        dateRangeInput("dtRange", "데이터 기간 선택",
                       start  = "2010-01-01",
                       end    = "2030-12-31",
                       min    = "2010-01-01",
                       max    = "2030-12-21",
                       format = "mm-dd-yy",
                       separator = " - "),
        # 시간열 선택
        
        radioButtons("inputSP", "기준 공간",
                     c("CATEGORY1" = "CATEGORY1",
                       "CATEGORY2" = "CATEGORY2",
                       "CATEGORY3" = "CATEGORY3"
                     )),
        
        radioButtons("inputDT", "입력날 형태",
                     c("년월" = "ym",
                       "년월일" = "ymd",
                       "년월일시" = "ydmh"
                     )),
      
        
        radioButtons("outputDT", "출력날 형태",
                     c("년월" = "ym",
                       "년월일" = "ymd"
                     )),
        
        radioButtons("aggMethod", "통합방법",
                     c("최대값" = "max",
                       "최소" = "min",
                       "평균" = "mean",
                       "중앙값" = "median"
                     )),
        
  
        actionButton("ptBtn", "전처리실행"),
        width=3
      ),
      mainPanel(
          dataTableOutput("dat2")
      )
   ),
   tabPanel("공간전처리"),
   tabPanel("시각화"),
   theme = shinytheme("united")
)

mean2 <- function(x)
{
  x <- mean(x)
  return (round(x,2))
}
server <- function(input, output,session){
  
  observeEvent(input$dlBtn, {
    output$dat <-  renderDataTable({
      if(input$KA == TRUE){
        dat <- read_excel("./data/20182.xlsx")
      }
      print('시발')
      dat <<- dat
      return (datatable(dat))
    })
    
    output$dat2 <-  renderDataTable({
      return (datatable(dat))
    })
    
  })
  
  observeEvent(input$ptBtn, {
    output$dat2 <-  renderDataTable({

      dat2 <- dat
      dt <- ymd_h(dat$DATE)
      dt <- date(dt)
      dat2$DATE <- dt
 

      if(input$inputSP == "CATEGORY1"){
        dat2 <- subset(dat2,select=-c(CATEGORY2,CATEGORY3))
        print("시발~~")
        dat2 <- aggregate(.~DATE+CATEGORY1, dat2, mean2)
        dat2 <<- dat2
      }
      
      if(input$inputSP == "CATEGORY2"){
        dat2 <- subset(dat2,select=-c(CATEGORY3))
        dat2 <- aggregate(.~DATE+CATEGORY1+CATEGORY2, dat2, mean2)
        dat2 <<- dat2
      }
      
      if(input$inputSP == "CATEGORY3"){
        dat2 <- aggregate(.~DATE+CATEGORY1+CATEGORY2+CATEGORY3, dat2, mean2)
        dat2 <<- dat2
      }
      
      
      return (datatable(dat2))
    })
  })
}

shinyApp(ui = ui, server = server)