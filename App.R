library(shiny)
# library(rstudioapi)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("R/230606_SGI_shiny_module.R")
source("R/230506_SPI분석_shiny_module.R")
source("R/230730_SPI_SGI분석_shiny_module_spi미입력.R")
source("R/230603_SPI_SGI분석_shiny_module_spi입력.R")

ui <- fluidPage(
  titlePanel(
    h1("영섬유역 가뭄분석 서비스", align = "center"),
    windowTitle = "영섬유역 가뭄분석 서비스"),
  tabsetPanel(id = "tabset",
    tabPanel(value = "sgi", title = "지하수가뭄지수(SGI)", SGI_ui("sgi")),
    tabPanel(value = "spi", title = "기상가뭄지수(SPI)", SPI_ui("spi")),
    tabPanel(value = "spi_sgi", title = "SPI로 SGI 예측(SPI 미입력)", 
             SPI_SGI_ui("spi_sgi")),
    tabPanel(value = "input_spi_sgi", title = "SPI로 SGI 예측(미래 SPI 입력)", 
             input_SPI_SGI_ui("input_spi_sgi"))
  )
)

server <- function(input, output, session) {
  SGI_server("sgi")
  SPI_server("spi")
  SPI_SGI_server("spi_sgi")
  input_SPI_SGI_server("input_spi_sgi")
}

shinyApp(ui, server)
