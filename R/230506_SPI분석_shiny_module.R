
# 라이브러리 불러오기 및 경로설정 -------------------------------------------------------

library(rstudioapi)
library(sf)
library(progress)
library(SPEI)
library(xts)
library(lubridate)
library(zoo)
library(shiny)
library(leaflet)
library(gstat)
library(raster)
library(tidyverse)

# 자료불러오기 ------------------------------------------------------------------

# 유역지도 불러오기
sf_watershed <- st_read("R/input/수자원단위지도(최종,GRS80)/WKMBBSN.shp")
# 기상(ASOS)관측위치도 불러오기
df_asos_list <- read.csv(file = "R/input/230217_META_관측지점정보_asos.csv",
                         fileEncoding = "euc-kr")
# asos 위치
sf_asos_list <- st_as_sf(x = df_asos_list, coords = c("경도", "위도"), 
                         crs = 4326, remove = FALSE)
# asos 자료 불러오기
df_asos <- read_rds("R/input/df_asos.rds")
# 최근 spi 자료 불러오기
df_asos_spi_recent_temp <- read_rds("R/input/df_asos_spi_recent_temp.rds")

# 자료 정리 -------------------------------------------------------------------

sf_watershed <- st_transform(sf_watershed, crs = 4326)

sf_asos_list_intersect <- st_intersection(sf_asos_list, sf_watershed) |> 
    st_as_sf()

# shiny -------------------------------------------------------------------

# selectinput 인수 
select_var_spi <- sf_asos_list_intersect %>% 
    filter(종료일 == "") %>% 
    distinct(지점명, 지점)

SPI_ui <- function(id) {
    fluidPage(
        fluidRow(
            column(9,
                   bootstrapPage(
                       div(class = "outer", tags$style(type = "text/css", 
                                                       ".outer {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                           leafletOutput(NS(id,"map"), width = "100%", height = "100%")))),
            column(3, 
                   absolutePanel(top = 10, right = 10,
                                 selectInput(NS(id, "stn"), label = "기상 관측소를 선택하세요", 
                                             choices = setNames(nm = select_var_spi$지점명, 
                                                                object = select_var_spi$지점), 
                                             selected = 1), 
                                 numericInput(NS(id, "spi_scale"), label = "SPI 누적강우개월을 선택하세요", 
                                              value = 6, min = 1, max = 12, step = 1),
                                 selectInput(NS(id, "month"), label = "지도의 분석월을 선택하세요", 
                                             choices = setNames(nm = str_sub(ym(str_sub(Sys.Date(), 
                                                                                        start = 1, end = 7)) - 
                                                                                 months(0:11), start = 1, end = 7), 
                                                                object =  Sys.Date() - months(0:11))),
                                 actionButton(NS(id, "detect"), label = "기상자료 업데이트",                    
                                              class = "btn-success",
                                              icon = icon("magnifying-glass")),
                                 plotlyOutput(NS(id, "plot"), width = "600px")
                   )
            )
        )
    )
}

SPI_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # 기상자료 OpenAPI불러오기 --------------------------------------------------------
        
        observeEvent(input$detect, {
            
            shiny::withProgress(message = "기상자료 DB를 불러오고 있습니다.", {
                
                authKey <- "_NL4EYPlSCOS-BGD5VgjvQ"                                              # 인증키
                obs <- "RN"                                                                      # 관측항목
                
                df_asos <- list()
                
                for (i in sf_asos_list_intersect$지점 %>% unique()) {
                    
                    tryCatch({
                        
                        shiny::incProgress(1/length(sf_asos_list_intersect %>% unique()))
                        
                        stn <- i
                        tm1 <- 19000101
                        tm2 <- Sys.Date() |>
                            str_replace_all("-", "")
                        df_asos_temp <- read.table(paste0("https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd3.php?tm1=",
                                                          tm1, "&tm2=", tm2, "&obs=", obs, "&stn=", stn, "&authKey=", authKey))
                        df_asos[[i]] <- df_asos_temp
                    }, error = function(e) {
                        df_asos[[i]] <- NULL
                        warning(paste0("관측소의 자료가 없습니다.: ", i))
                    })
                }
            })
            
            df_asos <- do.call(rbind, df_asos)
            
            names(df_asos)  <- c("TM", "STN", "WS_AVG", "WR_DAY", "WD_MAX", "WS_MAX",
                                 "WS_MAX_TM", "WD_INS", "WS_INS", "WS_INS_TM", "TA_AVG",
                                 "TA_MAX", "TA_MAX_TM", "TA_MIN", "TA_MIN_TM", "TD_AVG",
                                 "TS_AVG", "TG_MIN", "HM_AVG", "HM_MIN", "HM_MIN_TM",
                                 "PV_AVG", "EV_S", "EV_L", "FG_DUR", "PA_AVG", "PS_AVG",
                                 "PS_MAX", "PS_MAX_TM", "PS_MIN", "PS_MIN_TM", "CA_TOT",
                                 "SS_DAY", "SS_DUR", "SS_CMB", "SI_DAY", "SI_60M_MAX",
                                 "SI_60M_MAX_TM", "RN_DAY", "RN_D99", "RN_DUR", "RN_60M_MAX",
                                 "RN_60M_MAX_TM", "RN_10M_MAX", "RN_10M_MAX_TM", "RN_POW_MAX",
                                 "RN_POW_MAX_TM", "SD_NEW", "SD_NEW_TM", "SD_MAX", "SD_MAX_TM",
                                 "TE_05", "TE_10", "TE_15", "TE_30", "TE_50")
            
            df_asos <- df_asos |>
                dplyr::select(TM, STN, RN_DAY) |>
                filter(TM >= 19860000)      # 순창군에서 1985년 자료가 있고 이후 누락 2008년 자료 부터 있어서 1860년 부터 분석함
            
            write_rds(df_asos, "R/input/df_asos.rds")
            df_asos <- read_rds("R/input/df_asos.rds")
            
            # 최근 전체 SPI 구하기 -----------------------------------------------------------
            
            df_asos_stn <- df_asos |> distinct(STN) %>% 
                left_join(sf_asos_list_intersect %>% 
                              filter(종료일 == "") %>% 
                              dplyr::select(지점, 지점명), by = c("STN" = "지점")) %>% 
                drop_na(지점명)
            
            df_asos_temp_spi2 <- list()
            
            shiny::withProgress(message = "전체 SPI를 분석하고 있습니다.", {
                
                for (i in df_asos_stn$STN) {
                    
                    shiny::incProgress(1/length(sf_asos_list_intersect %>% unique()))
                    
                    tryCatch({
                        df_asos_temp2 <- data.frame(
                            YMD =
                                seq(
                                    from = df_asos |> filter(STN == i) |> dplyr::select(TM) |> slice_head() |> ymd(),
                                    to = df_asos |> filter(STN == i) |> dplyr::select(TM) |> slice_tail() |> ymd(),
                                    by  = "days"
                                )
                        ) |>
                            left_join(df_asos |> filter(STN == i) |> mutate_at(vars(TM), ymd),
                                      by = c("YMD" = "TM")) |>
                            mutate(
                                RN_DAY = replace_na(RN_DAY, 0),
                                RN_DAY = replace(RN_DAY, RN_DAY == -9, 0)
                            )
                        
                        df_asos_temp_xts2 <- as.xts(df_asos_temp2[-1], order.by = df_asos_temp2$YMD) 
                        
                        df_asos_temp_xts_rollsum2 <-
                            rollapply(
                                rev.zoo(df_asos_temp_xts2$RN_DAY),
                                width = 30,
                                by = 30,
                                align = "right",
                                FUN = sum
                            ) |> 
                            rev.zoo()
                        
                        df_asos_temp_xts_month2 <- as.xts(
                            apply.monthly(df_asos_temp_xts_rollsum2[, 1], sum))
                        
                        df_asos_temp_spi2[[i]] <-
                            data.frame(ym = rownames(as.data.frame(df_asos_temp_xts_month2))) |>
                            bind_cols(
                                SPEI::spi(
                                    df_asos_temp_xts_month2,
                                    scale = 6,   # 이것 반응성으로 설정할 것(SPI6를 기본으로 할 것)
                                    distribution = "PearsonIII",
                                    verbose = FALSE
                                )$fitted
                            ) |>
                            rename(spi = 2) |> 
                            mutate_at(vars(ym), ymd) %>% 
                            mutate(stn = i) %>% 
                            dplyr::select(stn, everything())
                        
                    }, error = function(e) {
                        df_asos_temp_spi2[[i]] <- NULL
                        warning(paste0("SPI 분석에 실패하였습니다.:", i))
                    })   
                }
            })   # 진행바 종료
            
            # 분석 년월 설정
            
            df_asos_spi_recent_temp <- do.call(bind_rows, df_asos_temp_spi2)
            
            # SPI로부터 SGI를 예측하기 위한 자료 저장
            write_rds(df_asos_spi_recent_temp, file = "R/input/df_asos_spi_recent_temp.rds")
            df_asos_spi_recent_temp <- read_rds("R/input/df_asos_spi_recent_temp.rds")
            
            fn_replace_inf <- function(x) {
                for (i in 1:nrow(x)) {
                    if (is.infinite(x$spi[i])) {
                        x$spi[i] <- x$spi[i+1]
                    }
                }
                return(x)
            }
            
            df_asos_spi_recent_temp <- df_asos_spi_recent_temp %>% fn_replace_inf()
            
            # 월초의 경우 해당월의 SPI자료가 없는 경우가 있어 검색월 1달 안의 SPI 검색
            df_asos_spi_recent <- reactive({df_asos_spi_recent_temp %>%
                    filter(ym <= ymd(input$month) & ym > (ymd(input$month) - days(30))) %>%
                    filter(is.finite(spi)) %>%
                    left_join(sf_asos_list_intersect %>%
                                  filter(종료일 == "") %>%
                                  dplyr::select(지점, 지점명), by = c("stn" = "지점"))
            })
            
            # 기상관측소별 SPI 산정 ------------------------------------------------------------------
            
            df_asos_stn <- df_asos |> distinct(STN) %>%
                left_join(sf_asos_list_intersect[, c("지점", "지점명")], by = c("STN" = "지점"))
            
            df_asos_temp <- reactive({
                data.frame(
                    YMD =
                        seq(
                            from = df_asos |> filter(STN == input$stn) |>
                                dplyr::select(TM) |> slice_head() |> ymd(),
                            to = df_asos |> filter(STN == input$stn) |> dplyr::select(TM) |> slice_tail() |> ymd(),
                            by  = "days"
                        )
                ) |>
                    left_join(df_asos |> filter(STN == input$stn) |> mutate_at(vars(TM), ymd),
                              by = c("YMD" = "TM")) |>
                    mutate(
                        STN = replace_na(STN, as.numeric(input$stn)),
                        RN_DAY = replace_na(RN_DAY, 0),
                        RN_DAY = replace(RN_DAY, RN_DAY == -9, 0)
                    )
            })
            
            df_asos_temp_xts <- reactive({
                as.xts(df_asos_temp()[-1], order.by = df_asos_temp()$YMD)
            })
            
            df_asos_temp_xts_rollsum <- reactive({
                rollapply(
                    rev.zoo(df_asos_temp_xts()$RN_DAY),
                    width = 30,
                    by = 30,
                    align = "right",
                    FUN = sum
                ) |>
                    rev.zoo()
            })
            
            df_asos_temp_xts_month <- reactive({
                as.xts(
                    apply.monthly(df_asos_temp_xts_rollsum()[, 1], sum))
            })
            
            df_asos_temp_spi <- reactive({
                data.frame(ym = rownames(as.data.frame(df_asos_temp_xts_month()))) |>
                    bind_cols(
                        SPEI::spi(
                            df_asos_temp_xts_month(),
                            scale = input$spi_scale,
                            distribution = "PearsonIII"
                        )$fitted
                    ) |>
                    rename(spi = 2) |>
                    mutate_at(vars(ym), ymd)
            })
            
            # 크리깅 ---------------------------------------------------------------------
            
            id <- showNotification("SPI 지도를 그리고 있습니다.", duration = NULL)
            
            # 좌표변환
            sf_watershed_tf <- st_transform(sf_watershed, crs = 5185)
            sf_asos_spi_recent_tf <- reactive({
                st_transform(st_as_sf(df_asos_spi_recent()), crs = 5185)
            })
            
            # variogram
            sp_asos_spi_recent <- reactive({
                sf_asos_spi_recent_tf() %>%
                    as_Spatial()
            })
            
            sp_vario_asos_spi_recent <- reactive({
                gstat::variogram(spi ~ 1, data = sp_asos_spi_recent())
            })
            
            sp_vario_fit_asos_spi_recent <- reactive({
                fit.variogram(sp_vario_asos_spi_recent(),
                              model = vgm(as.character(vgm()[[1]][1:10])),
                              fit.kappa = TRUE)
            })
            
            # plot(sp_vario_asos_spi_recent(), sp_vario_fit_asos_spi_recent())
            
            # 크리깅
            ra_watershed_template <- raster(extent(sf_watershed_tf %>%
                                                       filter(!BBSNNM == "제주도")), resolution = 1000,
                                            crs = 5185)
            ra_watershed <- rasterize(sf_watershed_tf, ra_watershed_template)
            df_grid <- as.data.frame(ra_watershed, xy = TRUE) %>% drop_na()
            gridded(df_grid) <- c("x", "y")
            # 투영좌표계 설정
            projection(df_grid) <- "+proj=tmerc +lat_0=38 +lon_0=125 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
            
            krig_spi <- reactive({
                krige(spi ~ 1, sp_asos_spi_recent(), df_grid, model = sp_vario_fit_asos_spi_recent())
            })
            
            ra_krig_spi <- reactive({
                st_as_sf(krig_spi()) %>%
                    rasterize(ra_watershed_template)
            })
            
            ra_krig_spi_pred <- reactive({
                raster(ra_krig_spi(), layer = 2)
            })
            
            
            # 그래프그리기 ------------------------------------------------------------------
            
            output$plot <- renderPlotly({
                
                ggplotly(ggplot(data = df_asos_temp_spi(), aes(x = ym)) + theme_minimal() +
                             geom_area(aes(y = ifelse(spi > 0,  spi, 0)),
                                       fill = "blue",
                                       alpha = 0.6) +
                             geom_area(aes(y = ifelse(spi < 0,  spi, 0)),
                                       fill = "red",
                                       alpha = 0.6) +
                             geom_line(y = 0, color = "grey") + 
                             geom_line(aes(y = spi), color = "grey") +
                             geom_line(aes(y = spi)) + 
                             theme(axis.title = element_text(size = 20),
                                   axis.text = element_text(size = 18),
                                   axis.title.x = element_blank()) +
                             ylab("SPI"))
            })
            
            #  지도그리기 ------------------------------------------------------------------
            
            rc_pal <- reactive({
                colorNumeric(c("red", "yellow","green", "blue"), 
                             domain = c(-3, 3))
            })
            
            output$map <- renderLeaflet({
                leaflet(options = providerTileOptions(minZoom = 9.1, maxZoom = 15)) %>%
                    addTiles() %>% addPolylines(data = sf_watershed, weight = 2) %>%
                    addCircleMarkers(data = st_transform(sf_asos_spi_recent_tf(),
                                                         crs = 4326),
                                     color = ~rc_pal()(spi)) %>%
                    leaflet::addLegend("bottomleft", pal = rc_pal(),
                                       values = sf_asos_spi_recent_tf()$spi,
                                       title = "가뭄정도", opacity = 1) %>% 
                    addRasterImage(ra_krig_spi_pred(), 
                                   colors = rc_pal(), opacity = 0.4)
            })
            
            on.exit(removeNotification(id))
            
        })  #  input$detect 닫음
        
        # 아래 부분은 input$detect을 실행하지 않아도 진행되는 코드
        # df_asos_spi_recent_temp에 Inf가 있는 경우 아래열의 값으로 대체
        
        fn_replace_inf <- function(x) {
            for (i in 1:nrow(x)) {
                if (is.infinite(x$spi[i])) {
                    x$spi[i] <- x$spi[i+1]
                }
            }
            return(x)
        }
        
        df_asos_spi_recent_temp <- df_asos_spi_recent_temp %>% fn_replace_inf()
        
        # 월초의 경우 해당월의 SPI자료가 없는 경우가 있어 검색월 1달 안의 SPI 검색
        df_asos_spi_recent <- reactive({df_asos_spi_recent_temp %>%
                filter(ym <= ymd(input$month) & ym > (ymd(input$month) - days(30))) %>%
                filter(is.finite(spi)) %>%
                left_join(sf_asos_list_intersect %>%
                              filter(종료일 == "") %>%
                              dplyr::select(지점, 지점명), by = c("stn" = "지점"))
        })
        
        # 기상관측소별 SPI 산정 ------------------------------------------------------------------
        
        df_asos_stn <- df_asos |> distinct(STN) %>%
            left_join(sf_asos_list_intersect[, c("지점", "지점명")], by = c("STN" = "지점"))
        
        df_asos_temp <- reactive({
            data.frame(
                YMD =
                    seq(
                        from = df_asos |> filter(STN == input$stn) |>
                            dplyr::select(TM) |> slice_head() |> ymd(),
                        to = df_asos |> filter(STN == input$stn) |> dplyr::select(TM) |> slice_tail() |> ymd(),
                        by  = "days"
                    )
            ) |>
                left_join(df_asos |> filter(STN == input$stn) |> mutate_at(vars(TM), ymd),
                          by = c("YMD" = "TM")) |>
                mutate(
                    STN = replace_na(STN, as.numeric(input$stn)),
                    RN_DAY = replace_na(RN_DAY, 0),
                    RN_DAY = replace(RN_DAY, RN_DAY == -9, 0)
                )
        })
        
        df_asos_temp_xts <- reactive({
            as.xts(df_asos_temp()[-1], order.by = df_asos_temp()$YMD)
        })
        
        df_asos_temp_xts_rollsum <- reactive({
            rollapply(
                rev.zoo(df_asos_temp_xts()$RN_DAY),
                width = 30,
                by = 30,
                align = "right",
                FUN = sum
            ) |>
                rev.zoo()
        })
        
        df_asos_temp_xts_month <- reactive({
            as.xts(
                apply.monthly(df_asos_temp_xts_rollsum()[, 1], sum))
        })
        
        df_asos_temp_spi <- reactive({
            data.frame(ym = rownames(as.data.frame(df_asos_temp_xts_month()))) |>
                bind_cols(
                    SPEI::spi(
                        df_asos_temp_xts_month(),
                        scale = input$spi_scale,
                        distribution = "PearsonIII"
                    )$fitted
                ) |>
                rename(spi = 2) |>
                mutate_at(vars(ym), ymd)
        })
        
        # 크리깅 ---------------------------------------------------------------------
        
        id <- showNotification("SPI 지도를 그리고 있습니다.", duration = NULL)
        
        # 좌표변환
        sf_watershed_tf <- st_transform(sf_watershed, crs = 5185)
        sf_asos_spi_recent_tf <- reactive({
            st_transform(st_as_sf(df_asos_spi_recent()), crs = 5185)
        })
        
        # variogram
        sp_asos_spi_recent <- reactive({
            sf_asos_spi_recent_tf() %>%
                as_Spatial()
        })
        
        sp_vario_asos_spi_recent <- reactive({
            gstat::variogram(spi ~ 1, data = sp_asos_spi_recent())
        })
        
        sp_vario_fit_asos_spi_recent <- reactive({
            fit.variogram(sp_vario_asos_spi_recent(),
                          model = vgm(as.character(vgm()[[1]][1:10])),
                          fit.kappa = TRUE)
        })
        
        # plot(sp_vario_asos_spi_recent(), sp_vario_fit_asos_spi_recent())
        
        # 크리깅
        ra_watershed_template <- raster(extent(sf_watershed_tf %>%
                                                   filter(!BBSNNM == "제주도")), resolution = 1000,
                                        crs = 5185)
        ra_watershed <- rasterize(sf_watershed_tf, ra_watershed_template)
        df_grid <- as.data.frame(ra_watershed, xy = TRUE) %>% drop_na()
        gridded(df_grid) <- c("x", "y")
        # 투영좌표계 설정
        projection(df_grid) <- "+proj=tmerc +lat_0=38 +lon_0=125 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        
        krig_spi <- reactive({
            krige(spi ~ 1, sp_asos_spi_recent(), df_grid, model = sp_vario_fit_asos_spi_recent())
        })
        
        ra_krig_spi <- reactive({
            st_as_sf(krig_spi()) %>%
                rasterize(ra_watershed_template)
        })
        
        ra_krig_spi_pred <- reactive({
            raster(ra_krig_spi(), layer = 2)
        })
        
        
        # 그래프그리기 ------------------------------------------------------------------
        
        output$plot <- renderPlotly({
            ggplotly(ggplot(data = df_asos_temp_spi(), aes(x = ym)) + theme_minimal() +
                         geom_area(aes(y = ifelse(spi > 0,  spi, 0)),
                                   fill = "blue",
                                   alpha = 0.6) +
                         geom_area(aes(y = ifelse(spi < 0,  spi, 0)),
                                   fill = "red",
                                   alpha = 0.6) +
                         geom_line(y = 0, color = "grey") + 
                         geom_line(aes(y = spi), color = "grey") +
                         geom_line(aes(y = spi)) + 
                         theme(axis.title = element_text(size = 20),
                               axis.text = element_text(size = 18),
                               axis.title.x = element_blank()) +
                         ylab("SPI"))
        })
        
        #  지도그리기 ------------------------------------------------------------------
        
        rc_pal <- reactive({
            colorNumeric(c("red", "yellow","green", "blue"),domain = c(-3, 3))
        })
        
        output$map <- renderLeaflet({
            leaflet(options = providerTileOptions(minZoom = 9.1, maxZoom = 15)) %>%
                addTiles() %>% addPolylines(data = sf_watershed, weight = 2) %>%
                addCircleMarkers(data = st_transform(sf_asos_spi_recent_tf(),
                                                     crs = 4326),
                                 color = ~rc_pal()(spi)) %>%
                leaflet::addLegend("bottomleft", pal = rc_pal(),
                                   values = sf_asos_spi_recent_tf()$spi,
                                   title = "가뭄정도", opacity = 1) %>% 
                addRasterImage(ra_krig_spi_pred(), 
                               colors = rc_pal(), opacity = 0.4)
        })
        
        on.exit(removeNotification(id))
        
        # 지도 클릭시 spi 시계열 그래프 출력
        observeEvent(input$map_marker_click, {
            updateSelectInput(inputId = "stn",
                              selected = as.numeric(st_as_sf(data.frame(lat = round(input$map_marker_click$lat, 4),
                                                                        lng = input$map_marker_click$lng),
                                                             coords = c("lng", "lat"), crs = 4326) %>%
                                                        st_as_sf() %>%
                                                        st_join(st_transform(sf_asos_spi_recent_tf(), crs = 4326),
                                                                join = st_nearest_feature) %>%
                                                        st_drop_geometry() %>%
                                                        dplyr::select(stn))
            )
        })
        # })  #  액션버튼 닫음
    }) # server 닫음
}

