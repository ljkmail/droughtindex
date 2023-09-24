
# 라이브러리 불러오기--------------------------------------------------------------

library(shiny)
library(tidyverse)
library(rstudioapi)
library(lubridate)
library(vroom)
library(progress)
library(xts)
library(timetk)
library(plotly)
library(forecast)
library(keras)
library(tensorflow)
library(sf)

# 자료불러오기-------------------------------------------------------------

df_spi <- read_rds("R/input/df_asos_spi_recent_temp.rds")
# df_sgi <- read_rds("R/input/df_sgi.rds") %>% bind_rows() # SGI_shiny_module에서 자료를 읽으므로 비활성화
# 기상관측소 인근 지하수관측소 위치자료를 만들기 위한 자료 불러오기
# 기상(ASOS)관측위치도 불러오기
# df_asos_list <- read.csv(file = "R/input/230217_META_관측지점정보_asos.csv",
#                          fileEncoding = "euc-kr")
# 지하수관측소 위치정보 불러오기
df_obs_list <- read.csv("R/input/230216_염섬관측정현황.csv",
                        fileEncoding = "EUC-KR",
                        row.names = 1)

# 기상자료 인근에 위치한 지하수관측소 찾기 --------------------------------------------------

sf_asos <- st_as_sf(df_asos_list %>% 
                        filter(종료일 == "") %>% 
                        filter(지점 %in% (df_spi %>% pull(stn) %>% 
                                            unique())) %>% 
                        filter(!지점명 %in% c("제주", "서귀포")), 
                    coords = c("경도", "위도"), crs = 4326)
sf_ngw <- st_as_sf(df_obs_list %>% 
                       drop_na(GENNUM, 경도, 위도) %>% 
                       filter(구분 %in% c("천부", "충적")), 
                   coords = c("경도", "위도"), crs = 4326)

df_location <- sf_asos %>% st_join(sf_ngw, join = st_nearest_feature) %>% 
    select(지점, 지점명, GENNUM, 관측소명) %>% 
    rename(기상지점코드_stn = 1, 기상지점명 = 2, 
           지하수지점코드_gennum = 3, 지하수지점명 = 4)

# SGI, SPI의 관측소 자료가 있는지 확인
# (df_location %>% pull(지하수지점코드_gennum)) %in% (df_obs_list %>% pull(GENNUM) %>% unique())
# (df_location %>% pull(기상지점코드_stn)) %in% (df_spi %>% pull(stn) %>% unique())

# LSTM_ui -----------------------------------------------------------------

SPI_SGI_ui <- function(id) {
    ns <- NS(id) # renderUI사용을 위해 강제로 id 설정함수 생성
    fluidPage(
        sidebarLayout(
            sidebarPanel(
                selectInput(NS(id, "stn_num"), label = "기상관측소 선택", 
                            choices = setNames(nm = df_location$기상지점명, 
                                               object = df_location$기상지점코드_stn), 
                            selected = 1),
                tags$strong(textOutput(NS(id, "text"))),
                br(),
                tags$strong("미래 SPI값을 입력하지 않고 1개월 후의 SGI값을 예측합니다."),
                br(),
                br(),
                numericInput(NS(id, "epochs"), "LSTM 학습횟수(epoch) 설정", 
                             value = 100, min = 10, max = 200),
                actionButton(NS(id, "run_button"), "SGI 예측 실행",
                             class = "btn-success",
                             icon = icon("magnifying-glass"))
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("시계열 그래프", plotlyOutput(NS(id, "time_series_plot"))),
                    tabPanel("산점도", plotOutput(NS(id, "scatter_plot"))),
                    tabPanel("SGI 예측 결과(LSTM)", plotlyOutput(NS(id, "prediction_plot")),
                             div(style = "text-align: center;",
                                 "LSTM 오차(Mean Squared Error)"), 
                             plotOutput(NS(id, "hist")),
                             div(style = "text-align: center;",
                                 "SGI계측값과 SGI예측값의 비교"), 
                             plotOutput(NS(id, "predictions_val_plot")))
                )
            )
        )
    )
}

SPI_SGI_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        df_sgi_temp <- reactive({
            df_sgi %>% bind_rows() %>% 
                filter(gennum == df_location %>%
                           filter(기상지점코드_stn %in% input$stn_num) %>%
                           pull(지하수지점코드_gennum)) %>%
                tk_xts(order.by = .$YMD) %>%
                apply.monthly(1, FUN = mean) %>%
                ggplot2::fortify() %>%
                mutate(gennum = df_location %>%
                           filter(기상지점코드_stn %in% input$stn_num) %>%
                           pull(지하수지점코드_gennum))
        })
        # df_spi에 Inf가 있는 경우 아래열의 값으로 대체
        fn_replace_inf <- function(x) {
            for (i in 1:nrow(x - 1)) {
                if (is.infinite(x$spi[i])) {
                    x$spi[i] <- x$spi[i+1]
                }
            }
            return(x)
        }
        
        df_spi_filtered <- reactive({
            df_spi %>% 
                filter(stn == input$stn_num)
        })
        
        df_sgi_spi <- reactive({
            df_sgi_temp() %>%
                mutate(ym = substr(Index, 1, 7)) %>%
                left_join(df_spi %>% fn_replace_inf() %>% 
                              filter(stn %in% input$stn_num) %>%
                              mutate(ym_ = substr(ym, 1, 7)),
                          by = c("ym" = "ym_")) %>%
                select(Index, sgi, spi) %>%
                rename(ymd = Index) %>%
                na.omit()
        })
        
        
        # 예측하고자 하는 지하수관측소 설명 ------------------------------------------------------
        
        output$text <- renderText(
            paste0(df_location %>% 
                       filter(기상지점코드_stn == input$stn_num) %>% 
                       pull(기상지점명) %>% as.character(),
                   " 기상관측소 SPI값으로 ",
                   df_location %>% 
                       filter(기상지점코드_stn == input$stn_num) %>% 
                       pull(지하수지점명) %>% as.character(),
                   " 국가지하수관측소 SGI값을 예측합니다.")
        )
        
        # Generate time series plot-------------------------------------------------------------
        
        output$time_series_plot <- renderPlotly({
            
            ggplotly(ggplot() + theme_minimal() + 
                         geom_line(data = df_spi_filtered(), 
                                   aes(x = ym, y = spi, color = "SPI"), size = 0.8) +
                         geom_line(data = df_sgi_temp(), 
                                   aes(x = Index, y = sgi, color = "SGI"), size = 0.8) +
                         scale_x_date(limits = c(max(df_sgi_temp()$Index[1],  
                                                     df_spi_filtered()$ym[1]),
                                                 tail(df_sgi_temp()$Index, 1))) +
                         scale_color_manual(values = c("SPI" = "steelblue", "SGI" = "orangered")))
        })
        
        # Generate scatter plot--------------------------------------------------------------------
        
        output$scatter_plot <- renderPlot({
            
            ggplot() + theme_minimal() +
                geom_point(data = df_sgi_spi() %>%
                               mutate(ym = substr(ymd, 1, 7)) %>%
                               left_join(df_spi %>%
                                             filter(stn %in% input$stn_num) %>%
                                             mutate(ym_ = substr(ym, 1, 7)),
                                         by = c("ym" = "ym_")),
                           aes(x = spi.y, y = sgi), color = "steelblue") +
                geom_abline(slope = 1, linetype = "dashed", color = "orange") +
                coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(-3, 3)) + 
                labs(x = "SPI", y = "SGI")
        }, res = 96)
        
        # LSTM prediction-----------------------------------------------------------------------
       
        observeEvent(input$run_button, {
            
            # 프로그래스 바 생성
            progress <- Progress$new(session, min = 0, max = 100)
            progress$set(message = "딥러닝(LSTM) 분석중입니다",
                         detail = "분석 완료후 'SGI예측결과(LSTM)' 탭을 눌러주세요")
            
            on_epoch_end <- function(epoch, logs) {
                progress$set(value = epoch / input$epochs * 100)
            }
            
            # 예측을 위하여 SGI값을 설정한 개월수로 이동
            df_sgi_shift_spi <- reactive({
                df_sgi_spi() %>% 
                    mutate(sgi_shift = lead(sgi, 1))
            })
            
            # 훈련 및 테스트 데이터 분할
            x_train1 <- reactive({
                df_sgi_shift_spi() %>%
                    head((nrow(.) - 2)) %>%
                    select(spi) %>%
                    as.matrix()
            })
            
            y_train1 <- reactive({
                df_sgi_shift_spi() %>%
                    head((nrow(.) - 2)) %>%
                    select(sgi_shift) %>%
                    as.matrix()
            })
            
            x_test1 <- reactive({
                df_sgi_shift_spi() %>%
                    tail(2) %>%
                    drop_na() %>% 
                    select(spi) %>%
                    as.matrix()
            })
            
            y_test1 <- reactive({
                df_sgi_shift_spi() %>%
                    tail(2) %>%
                    drop_na() %>% 
                    select(sgi_shift) %>%
                    as.matrix()
            })
            
            
            x_train2 <- reactive({
                array(x_train1(), dim = c(dim(x_train1())[1], dim(x_train1())[2], 1))
            })
            y_train2 <- reactive({
                array(y_train1(), dim = c(dim(y_train1())[1], 1))
            })
            x_test2 <- reactive({
                array(x_test1(), dim = c(dim(x_test1())[1], dim(x_test1())[2], 1))
            })
            y_test2 <- reactive({
                array(y_test1(), dim = c(dim(y_test1())[1], 1))
            })
            
            model <- keras_model_sequential() %>%
                layer_lstm(units = 50,
                           return_sequences = TRUE) %>% 
                layer_dense(units = 1) %>% compile(
                    loss = 'mean_squared_error',
                    optimizer = optimizer_adam(learning_rate = 0.01)
                )
            
            history <- model %>% fit(
                x_train2(), y_train2(),
                batch_size = 32,
                epochs = input$epochs,
                validation_data = list(x_test2(), y_test2()),
                shuffle = FALSE, 
                callbacks = list(keras::callback_lambda(on_epoch_end))
            )
            
            # 프로그래스바 닫기
            progress$close()
            
            # 1개월 예측
            x_predic1 <- reactive({
                df_sgi_shift_spi() %>% 
                    tail(1) %>% 
                    select(spi) %>% 
                    as.matrix()
            })
            
            x_predic2 <- reactive({
                array(x_predic1(), dim = c(dim(x_predic1())[1], dim(x_predic1())[2], 1))
            })
            
            predictions <- reactive({
                model %>% predict(x_predic2())
            })
            
            df_sgi_predic <- reactive({
                data.frame(sgi_predic = predictions()) %>%
                    bind_cols(data.frame(ym = df_sgi_temp()$Index + days(30)) %>%
                                  tail(length(1)) %>% mutate_at(vars(ym), ymd)) %>%
                    select(ym, sgi_predic)
            })
            # 검증그래프 작성
            x_train1_val <- reactive({
                df_sgi_shift_spi() %>%
                    select(spi) %>%
                    as.matrix()
            })
            
            x_train2_val <- reactive({
                array(x_train1_val(), dim = c(dim(x_train1_val())[1], dim(x_train1_val())[2], 1))
            })
            
            predictions_val <- reactive({
                # 모델을 사용하여 예측값 계산
                model %>% predict(x_train2_val())
            })
            
            df_sgi_predic_val <- reactive({
                # 예측값과 해당 날짜를 데이터프레임으로 조합
                data.frame(sgi_predic_val = predictions_val(), 
                           ym = df_sgi_spi()$ymd,
                           sgi = df_sgi_spi()$sgi)
            })
            
            # 검증값 출력
            output$predictions_val_plot <- renderPlot({
                ggplot(df_sgi_predic_val(), aes(x = ym)) + theme_minimal() +
                    geom_line(aes(y = sgi, color = 'sgi'), size = 1.2) +
                    geom_line(aes(y = sgi_predic_val, color = 'sgi_predict'),
                              size = 1.2) +
                    scale_color_manual(values = c('sgi' = 'steelblue', 
                                                  'sgi_predict' = 'orangered')) +
                    theme(axis.title = element_text(size = 20),
                          axis.text = element_text(size = 20),
                          axis.title.x = element_blank())
            })
            
            model %>% predict(x_predic2())
            
            output$prediction_plot <- renderPlotly({
                ggplotly(
                    ggplot() +  geom_line(data = df_spi_filtered(), 
                                          aes(x = ym, y = spi, color = "SPI"), 
                                          size = 0.8) +
                        geom_line(data = df_sgi_temp(), aes(x = Index, y = sgi, color = "SGI"), 
                                  size = 0.8) +
                        geom_line(data = df_sgi_predic() %>%
                                      bind_rows(df_sgi_temp() %>%
                                                    select(Index, sgi) %>%
                                                    tail(1) %>%
                                                    rename(ym = Index, sgi_predic = sgi)),
                                  aes(x = ym, y = sgi_predic, color = "SGI_predic"), size = 0.8) +
                        scale_color_manual(values = c("SPI" = "steelblue",
                                                      "SGI" = "orangered",
                                                      "SGI_predic" = "orange")) +
                        xlab("Date") +
                        ylab("SGI") +
                        theme_minimal() +
                        scale_x_date(limits = c(max(df_sgi_temp()$Index[1], 
                                                    df_spi_filtered()$ym[1]),
                                                tail(df_sgi_predic()$ym, 1)))
                )
            })
            
            output$hist <- renderPlot({
                plot(history) + theme_minimal()
            }, res = 96)
        })  # run 종료
        
    }) # server 닫음
}