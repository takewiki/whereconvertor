#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# m00

library(shiny)
library(shinydashboard)
#library(recharts)
# 1.01 add the 1st msg notification Menu----






shinyUI(dashboardPage(skin = "blue",
                    
                    # add the themes selector
                    
                    #ui.header ----
                    
                    dashboardHeader(title = "SQL-Where转换器"
                                                        
                    ),
                    
                    #ui.sideBar----
                    dashboardSidebar(
                        sidebarMenu(
                        # add the item
                       
                        menuItem(text = "工作区",tabName = "rdCostingTopic")
                        
                        
                      )
                    ),
                    
                    #ui.body----
                    dashboardBody(
                      tabItems(
                        # First tab content
                        
                        tabItem(tabName = "rdCostingTopic",
                                tabsetPanel(
                                  
                                  tabPanel("上传数据", 
                                           fluidRow(
                                             box(title = "选择数据源",width = 3, status = "primary",
                                                 radioButtons("fileType","文件类型",choices = c("Excel","CSV"),selected = "Excel"),
                                                 conditionalPanel("input.fileType =='Excel'",
                                                                  fileInput("fileExcel", "请选择Excel文件位置:",buttonLabel = "浏览",
                                                                            accept = c(
                                                                              ".xls",
                                                                              ".xlsx")
                                                                  ),
                                                                  checkboxInput("header", "首行包含标题?", TRUE),
                                                                  
                                                                  checkboxInput("firstUse1","首次使用，请下载文件模板"),
                                                                  tags$hr(),
                                                                  conditionalPanel("input.firstUse1 == true",
                                                                                   downloadButton("fileTemlate1","下载文件模板"),tags$hr())),
                                                 conditionalPanel("input.fileType == 'CSV'",
                                                                  fileInput("file1", "请选择一下CSV文件.",buttonLabel = "浏览",
                                                                            accept = c(
                                                                              "text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv")
                                                                  ), checkboxInput("firstUse2","首次使用，请下载文件模板"),
                                                                  tags$hr(),
                                                                  conditionalPanel("input.firstUse2 == true",
                                                                                   downloadButton("fileTemlate2","下载文件模板"),tags$hr()))
                                                 
                                                                                                 #tags$hr(),
                                                 
                                      
                                                 ),
                                             box(title = "预览上传数据:",width = 9, status = "primary",
                                                 HTML('友情提示:仅显示前6行记录，用于标题及内容确认。'),
                                                 dataTableOutput("content_table"))
                                             
                                             
                                                                                        )
                                          
                                           
                                  ),
                                 
                                  tabPanel("下载数据", 
                                           fluidRow(
                                                    box(title = "下载区域",width = 3, status = "primary",
                                                         downloadButton("report_xlsx", "下载xlsx格式数据")),
                                                    box(title = "预览结果",width = 9, status = "primary",
                                                         dataTableOutput("dataResult"))
                                                    )
                                           )
                                  
                                  
                                  
                             
                                )
                                )  
                      )
                    )
)
)


