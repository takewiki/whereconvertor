#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#m00


library(shiny)
library(shinydashboard)
library(readxl)
library(openxlsx);
library(stringr);

templateData <-data.frame(barcode=paste('SQ2018123-010-0',1:100,sep = ""));

#' 获取字符赂量中每个元素的长度，而不是向量的长度
#'
#' @param x  字符型数据
#'
#' @return   数值型向量
#' @export
#' @import stringr
#' @examples len(letters);
#

len <- function (x)
{
  if (class(x) !='character')
  {
    stop("'x'参数必须是字符型向量!")
  }else{
    str_length(x)
  }
}

#' 从左边截取n个字符
#'
#' @param x 字符串向量
#' @param num_char  字符个数
#'
#' @return 返回字符向量
#' @export
#' @import stringr
#' @examples left(letters,1);
left <- function (x,num_char=1){
  nmax <- max(len(x));
  if (num_char >nmax)
  {
    stop("输入的字符截取位数超过了字符的最大位度！")
  }else{
    str_sub(x, 1, num_char);
  }
  
}

#' 从右边截取指定位数的字符串
#'
#' @param x 签字文本
#' @param num_char  签字位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples right(letters,3);
right <- function (x,num_char){
  nstart <- len(x)-num_char+1;
  ncount <-length(x);
  res <- list(ncount);
  for (i in 1:ncount){
    res[[i]]=str_sub(x[i],nstart[i],-1L);
  }
  res <-unlist(res);
  res;
  
}

#' 从字符中间进行测试
#'
#' @param x 字符串
#' @param start 开始位置
#' @param num_char 位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples mid('sdfdsfdsf',2,4);
mid  <- function (x,startIndex,num_char){
  endIndex <- startIndex+num_char-1;
  str_sub(x,startIndex,endIndex);
}

#' 将字符串进行拆分开
#'
#' @param x 字符串
#' @param pattern 可以使用字符串或正则表达式[]
#'
#' @return 返回一个列表
#' @export
#' @import stringr
#' @examples split.str('afsdsdf,bbbfsdfds,sdfds',',');
splitStr <- function (x,pattern){
  str_split(x,pattern);
}
getDataFromExcel <- function(file,sheet=1)
{
   res <- read_excel(file,sheet)
   return(res)
};
Excel2sqlstring <- function (data)
{
  paste0("'",data,"',");
}

writeDataToExcel <- function (data,fileName,sheetName)
{
  
  #write.xlsx(x = data,file = fileName,sheetName = sheetName,row.names = FALSE,append = T,showNA = T);
  write.xlsx(x = data,file = fileName,sheetName=sheetName);
  
};

 shinyServer(function(input, output) {
  
     #GET THE INTIIAL DATA
    csvdata <- reactive({
      if (input$fileType =='Excel')
      {
        inFile <- input$fileExcel
        if (is.null(inFile))
          return(NULL)
        res <-openxlsx::read.xlsx(inFile$datapath,sheet=1,colNames=input$header)
      }
      if (input$fileType == 'CSV')
      {
        inFile <- input$file1
        
        if (is.null(inFile))
          return(NULL)
        
        res <-read.csv(inFile$datapath, header = input$header)    
      }
      res;
       
    })
    
   
  output$content_table <- renderDataTable({
    
 
   
    head(csvdata()[,1,drop=F])
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(6,10,50,100), 
                   pageLength = 6))
  
  res  <- reactive({
    d1 <-Excel2sqlstring(csvdata()[,1]);
    d1 <- as.character(d1);
    nlen <-length(d1)
    lastChar <- d1[nlen];
    ndigit <- len(lastChar);
    lastChar2 <- left(lastChar,num_char = ndigit-1);
    res <-c(d1[1:nlen-1],lastChar2);
    res <-as.data.frame(res);
    names(res) <- names(csvdata())[1];
    return(res);
  })
  output$dataResult <- renderDataTable({
    
    
    res()
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(10,50,100,1000), 
                   pageLength = 10))
    
  
  
  output$fileTemlate1<- downloadHandler(
    filename = 'template.xlsx',
    content = function(file) {
      writeDataToExcel(templateData, file, sheetName = 'sheet1');
    }
  )
  output$fileTemlate2<- downloadHandler(
    filename = 'template.xlsx',
    content = function(file) {
      writeDataToExcel(templateData, file, sheetName = 'sheet1');
    }
  )

  output$report_xlsx<- downloadHandler(
    filename = 'Result.xlsx',
    content = function(file) {
      writeDataToExcel(res(), file, sheetName = 'sheet1');
    }
  )
   
 }
)
