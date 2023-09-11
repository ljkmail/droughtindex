
# 라이브러리 및 경로설정 ------------------------------------------------------------

library(tidyverse)
library(rstudioapi)
library(vroom)
library(ks)
library(progress)
library(sf)
library(leaflet)
library(raster)
library(gstat)
library(shiny)

# 자료불러오기 ------------------------------------------------------------------

df_yunbo <- vroom::vroom("R/input/gw_yunbo.csv")
df_obs_list <- read.csv("R/input/230216_염섬관측정현황.csv",
                        fileEncoding = "EUC-KR",
                        row.names = 1)
df_gennum <- na.omit(unique(df_obs_list$GENNUM))
# 유역지도 불러오기
sf_watershed <- st_read("R/input/수자원단위지도(최종,GRS80)/WKMBBSN.shp") %>%
    st_transform(crs = 4326)

# 지하수관측자료 불러오기
df <- read_rds("R/input/df.rds")

# sgi 분석자료 불러오기
df_sgi <- read_rds("R/input/df_sgi.rds")

# shiny -------------------------------------------------------------------

# selectinput 인수
select_var <- df_obs_list %>% 
    filter(!is.na(GENNUM))

SGI_ui <- function(id) {
    fluidPage(
        fluidRow(
            column(9,
                   bootstrapPage(
                       div(class = "outer", tags$style(type = "text/css", 
                                                       ".outer {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                           leafletOutput(NS(id, "map"), width = "100%", height = "100%")))),
            column(3,
                   absolutePanel(top = 10, right = 10,
                                 selectInput(NS(id, "stn"), label = "지하수 관측소를 선택하세요", 
                                             choices = setNames(nm = paste0(select_var$관측소명, 
                                                                            "_", select_var$구분), 
                                                                object = select_var$GENNUM), 
                                             selected = 1), 
                                 selectInput(NS(id, "month"), label = "지도의 분석월을 선택하세요", 
                                             choices = setNames(nm = str_sub(ym(str_sub(Sys.Date(), 
                                                                                        start = 1, end = 7)) - 
                                                                                 months(0:11), start = 1, end = 7), 
                                                                object =  Sys.Date() - months(0:11))),
                                 actionButton(NS(id, "detect"), label = "지하수자료 업데이트",                   
                                              class = "btn-success",
                                              icon = icon("magnifying-glass")),
                                 plotlyOutput(NS(id, "plot"), width = "600px")
                   )
            )
        )
    )
}

SGI_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # open API 지하수 자료불러오기 -----------------------------------------------------
        
        today_ <- as.character(lubridate::today()) |>
            str_replace_all("-", "")
        
        result <- data.frame()
        
        df_gennum <- as.data.frame(df_gennum)
        
        observeEvent(input$detect, {
            
            shiny::withProgress(message = "지하수자료 DB를 불러오고 있습니다.", {
                
                for(i in 1:nrow(df_gennum)) {
                    
                    shiny::incProgress(1/nrow(df_obs_list %>%
                                                  filter(!is.na(GENNUM))))
                    
                    key <- "L0eM%2BBjtmlHYcF5I5lOvfJ%2F0TBcPs11GMO1P0ENzjkoPaJQMrx3pVqPQ%2FXNBWYBQ"
                    gennum <- df_gennum[i,]
                    startdate <- "20220101"
                    enddate <- today_
                    
                    result_temp <- jsonlite::fromJSON(paste0("http://www.gims.go.kr/api/data/observationStationService/getGroundwaterMonitoringNetwork?KEY=",
                                                             key,
                                                             "&type=JSON&gennum=", gennum,
                                                             "&begindate=", startdate,
                                                             "&enddate=", enddate)) |>
                        as.data.frame()
                    result <- rbind(result, result_temp)
                }
            })
            
            df_real <- result |>
                rename(GENNUM = response.resultData.gennum,
                       ELEV = response.resultData.elev,
                       WTEMP = response.resultData.wtemp,
                       LEV = response.resultData.lev,
                       EC = response.resultData.ec,
                       YMD = response.resultData.ymd) |>
                dplyr::select(GENNUM, YMD, ELEV, LEV, WTEMP, EC) |>
                mutate_at(vars(c("GENNUM", "ELEV", "LEV", "WTEMP", "EC")), as.numeric) |>
                mutate_at(vars("YMD"), ymd)
            
            # 연보자료와 DB자료를 합치기
            
            df_yunbo <- df_yunbo |>
                mutate_at(vars(YMD), ymd) |>
                mutate_if(is.character, as.numeric) |>
                dplyr::select(-WELLNUM)
            
            df <- bind_rows(df_yunbo, df_real)
            
            write_rds(df, file = "R/input/df.rds")
            df <- read_rds("R/input/df.rds")
            
            # SGI 분석 ------------------------------------------------------------------
            
            df_obs <- list()
            df_sgi <- list()
            
            shiny::withProgress(message = "SGI를 분석하고 있습니다.", {
                
                for (i in 1:nrow(df_gennum)) {
                    
                    shiny::incProgress(1/nrow(df_gennum))
                    
                    tryCatch({
                        df_obs[[i]] <- df %>%
                            filter(GENNUM == df_gennum[i, ]) %>%
                            drop_na(ELEV)
                        
                        fhat <- kde(x = df_obs[[i]]$ELEV, binned = FALSE, h = 0.06)
                        df_sgi[[i]] <- data.frame(pkde = pkde(fhat = fhat, df_obs[[i]]$ELEV))
                        ifelse(df_sgi[[i]]$pkde == 0, 0.0001, df_sgi[[i]]$pkde)
                        ifelse(df_sgi[[i]]$pkde == 1, 0.9999, df_sgi[[i]]$pkde)
                        df_sgi[[i]] <- df_sgi[[i]] %>%
                            mutate(gennum = df_obs[[i]]$GENNUM,
                                   YMD = df_obs[[i]]$YMD,
                                   ELEV = df_obs[[i]]$ELEV,
                                   across(pkde, ~ qnorm(., 0, 1))) %>%
                            rename(sgi = pkde)
                    }, error = function(e) {
                        warning(paste0("커널덴시티 분석에 실패하였습니다: ", df_gennum[i, ]))
                    })
                }
                
            }) # SGI 분석 진행바 닫기
            
            names(df_sgi) <- df_gennum$df_gennum
            
            # SGI 분석자료 저장
            write_rds(df_sgi, file = "R/input/df_sgi.rds")
            df_sgi <- read_rds("R/input/df_sgi.rds")
            
            output$map <- renderLeaflet({
                leaflet(options = providerTileOptions(minZoom = 9.1, maxZoom = 15)) %>%
                    addTiles() %>% addPolylines(data = sf_watershed, weight = 2) %>%
                    addCircleMarkers(data = sf_sgi_recent() %>% filter(구분 %in% "천부"),
                                     color = ~rc_pal()(sgi)) %>%
                    leaflet::addLegend("bottomleft", pal = rc_pal(),
                                       values = sf_sgi_recent() %>% filter(구분 %in% "천부") %>% pull(sgi),
                                       title = "가뭄정도", opacity = 1) %>% 
                    addRasterImage(
                        ra_krig_pred(),
                        colors = colorNumeric(c("red", "yellow", "green", "blue"), values(ra_krig_pred())),
                        opacity = 0.4
                    )
            })
            
            output$plot <- renderPlotly({
                ggplotly(ggplot(rc_df_sgi(), aes(x = YMD, y = sgi)) + theme_minimal() +
                             geom_area(aes(y = ifelse(sgi > 0,  sgi, 0)),
                                       fill = "blue",
                                       alpha = 0.6) +
                             geom_area(aes(y = ifelse(sgi < 0,  sgi, 0)),
                                       fill = "red",
                                       alpha = 0.6) +
                             geom_line(y = 0, color = "grey") + geom_line(aes(y = sgi), color = "grey") +
                             geom_line(aes(y = sgi)) + 
                             theme(axis.text = element_text(size = 15),
                                   axis.title = element_text(size = 20),
                                   axis.title.x = element_blank()) +
                             ylab("SGI"))
            })
            
            # SGI분석 ----------------------------------------------------------------
            # 선택한 일짜와 가장 가까운 일자로 검색
            df_sgi_recent <- reactive({
                df_sgi %>%
                    bind_rows() %>% 
                    mutate(diff = abs(YMD - ymd(input$month))) %>% 
                    slice_min(diff) %>% 
                    dplyr::select(gennum, YMD, ELEV, sgi)
            })
            
            # 크리깅 -------------------------------------------------------------------
            
            id <- showNotification("SGI 지도를 그리고 있습니다.", duration = NULL)
            
            sf_sgi_recent <- reactive({
                df_sgi_recent() %>%
                    mutate_at(vars(gennum), as.numeric) %>%
                    left_join(df_obs_list %>%
                                  dplyr::select(관측소명, GENNUM, 구분, 경도, 위도),
                              by = c("gennum" = "GENNUM")) %>%
                    st_as_sf(coords = c("경도", "위도"), crs = 4326)
            })
            
            # 좌표변환
            sf_watershed_tf <- sf_watershed %>% st_transform(crs = 5185)
            sf_sgi_recent_tf <- reactive({
                sf_sgi_recent() %>% st_transform(crs = 5185)
            })
            
            # variogram
            sp_sgi_recent_tf <- reactive({
                sf_sgi_recent_tf() %>%
                    filter(구분 %in% "천부") %>%                # 충적 관측소 만으로 베리오그램 생성
                    as_Spatial()
            })
            sp_vario_sgi_recent_tf <- reactive({
                gstat::variogram(sgi ~ 1,
                                 data = sp_sgi_recent_tf())
            })
            
            sp_vario_fit_sgi_recent_tf <- reactive({
                fit.variogram(sp_vario_sgi_recent_tf(),
                              model = vgm(as.character(vgm()[[1]][1:15])),
                              fit.kappa = TRUE)
            })
            
            # plot(sp_vario_sgi_recent_tf(), sp_vario_fit_sgi_recent_tf())
            
            # 크리깅
            ra_watershed_template <- raster(extent(sf_watershed_tf %>%
                                                       filter(!BBSNNM == "제주도")),
                                            resolution = 1000,
                                            crs = 5185)
            
            ra_watershed <- rasterize(sf_watershed_tf %>%
                                          filter(!BBSNNM == "제주도"), ra_watershed_template)
            df_grid <- as.data.frame(ra_watershed, xy = TRUE) %>% drop_na()
            gridded(df_grid) <- c("x", "y")
            # 투영좌표계 설정
            projection(df_grid) <- "+proj=tmerc +lat_0=38 +lon_0=125 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
            
            krig_sgi <- reactive({
                krige(sgi ~ 1, sp_sgi_recent_tf(), df_grid,
                      model = sp_vario_fit_sgi_recent_tf())
            })
            
            ra_krig <- reactive({
                st_as_sf(krig_sgi()) %>%
                    rasterize(ra_watershed_template)
            })
            
            ra_krig_pred <- reactive({
                raster(ra_krig(), layer = 2)
            })
            
            # 지도그리기 -------------------------------------------------------------------
            
            rc_pal <- reactive({
                colorNumeric(c("red", "yellow","green", "blue"), domain = c(-3.5, 3.5))
            })
            
            output$map <- renderLeaflet({
                leaflet(options = providerTileOptions(minZoom = 9.1, maxZoom = 15)) %>%
                    addTiles() %>% addPolylines(data = sf_watershed, weight = 2) %>%
                    addCircleMarkers(data = sf_sgi_recent() %>% filter(구분 %in% "천부"),
                                     color = ~rc_pal()(sgi)) %>%
                    leaflet::addLegend("bottomleft", pal = rc_pal(),
                                       values = sf_sgi_recent() %>% filter(구분 %in% "천부") %>% pull(sgi),
                                       title = "가뭄정도", opacity = 1) %>% 
                    addRasterImage(
                        ra_krig_pred(),
                        colors = rc_pal(),
                        opacity = 0.4
                    )
            })
            
            # 그래프 그리기 -----------------------------------------------------------------
            
            rc_df_sgi <- reactive({
                df_sgi[[input$stn]]
            })
            
            output$plot <- renderPlotly({
                ggplotly(ggplot(rc_df_sgi(), aes(x = YMD, y = sgi)) + theme_minimal() +
                             geom_area(aes(y = ifelse(sgi > 0,  sgi, 0)),
                                       fill = "blue",
                                       alpha = 0.6) +
                             geom_area(aes(y = ifelse(sgi < 0,  sgi, 0)),
                                       fill = "red",
                                       alpha = 0.6) +
                             geom_line(y = 0, color = "grey") + geom_line(aes(y = sgi), color = "grey") +
                             geom_line(aes(y = sgi)) + 
                             theme(axis.text = element_text(size = 15),
                                   axis.title = element_text(size = 20),
                                   axis.title.x = element_blank()) +
                             ylab("SGI"))
            })
            
            on.exit(removeNotification(id))
            
        }) # input$detect 닫음
        
        # 아래 부분은 input$detect을 실행하지 않아도 진행되는 코드
        # SGI분석 ----------------------------------------------------------------
        # 선택한 일짜와 가장 가까운 일자로 검색
        df_sgi_recent <- reactive({
            df_sgi %>%
                bind_rows() %>% 
                mutate(diff = abs(YMD - ymd(input$month))) %>% 
                slice_min(diff) %>% 
                dplyr::select(gennum, YMD, ELEV, sgi)
        })
        
        # 크리깅 -------------------------------------------------------------------
        
        id <- showNotification("SGI 지도를 그리고 있습니다.", duration = NULL)
        
        sf_sgi_recent <- reactive({
            df_sgi_recent() %>%
                mutate_at(vars(gennum), as.numeric) %>%
                left_join(df_obs_list %>%
                              dplyr::select(관측소명, GENNUM, 구분, 경도, 위도),
                          by = c("gennum" = "GENNUM")) %>%
                st_as_sf(coords = c("경도", "위도"), crs = 4326)
        })
        
        # 좌표변환
        sf_watershed_tf <- sf_watershed %>% st_transform(crs = 5185)
        sf_sgi_recent_tf <- reactive({
            sf_sgi_recent() %>% st_transform(crs = 5185)
        })
        
        # variogram
        sp_sgi_recent_tf <- reactive({
            sf_sgi_recent_tf() %>%
                filter(구분 %in% "천부") %>%                # 충적 관측소 만으로 베리오그램 생성
                as_Spatial()
        })
        sp_vario_sgi_recent_tf <- reactive({
            gstat::variogram(sgi ~ 1,
                             data = sp_sgi_recent_tf())
        })
        
        sp_vario_fit_sgi_recent_tf <- reactive({
            fit.variogram(sp_vario_sgi_recent_tf(),
                          model = vgm(as.character(vgm()[[1]][1:15])),
                          fit.kappa = TRUE)
        })
        
        # plot(sp_vario_sgi_recent_tf(), sp_vario_fit_sgi_recent_tf())
        
        # 크리깅
        ra_watershed_template <- raster(extent(sf_watershed_tf %>%
                                                   filter(!BBSNNM == "제주도")),
                                        resolution = 1000,
                                        crs = 5185)
        
        ra_watershed <- rasterize(sf_watershed_tf %>%
                                      filter(!BBSNNM == "제주도"), ra_watershed_template)
        df_grid <- as.data.frame(ra_watershed, xy = TRUE) %>% drop_na()
        gridded(df_grid) <- c("x", "y")
        # 투영좌표계 설정
        projection(df_grid) <- "+proj=tmerc +lat_0=38 +lon_0=125 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        
        krig_sgi <- reactive({
            krige(sgi ~ 1, sp_sgi_recent_tf(), df_grid,
                  model = sp_vario_fit_sgi_recent_tf())
        })
        
        ra_krig <- reactive({
            st_as_sf(krig_sgi()) %>%
                rasterize(ra_watershed_template)
        })
        
        ra_krig_pred <- reactive({
            raster(ra_krig(), layer = 2)
        })
        
        # 지도그리기 -------------------------------------------------------------------
        
        rc_pal <- reactive({
            colorNumeric(c("red", "yellow","green", "blue"), domain = c(-3.5, 3.5))
        })
        
        output$map <- renderLeaflet({
            leaflet(options = providerTileOptions(minZoom = 9.1, maxZoom = 15)) %>%
                addTiles() %>% addPolylines(data = sf_watershed, weight = 2) %>%
                addCircleMarkers(data = sf_sgi_recent() %>% filter(구분 %in% "천부"),
                                 color = ~rc_pal()(sgi)) %>%
                leaflet::addLegend("bottomleft", pal = rc_pal(),
                                   values = sf_sgi_recent() %>% filter(구분 %in% "천부") %>% pull(sgi),
                                   title = "가뭄정도", opacity = 1) %>% 
                addRasterImage(
                    ra_krig_pred(),
                    colors = rc_pal(),
                    opacity = 0.4
                )
        })
        
        # 그래프 그리기 -----------------------------------------------------------------
        
        rc_df_sgi <- reactive({
            df_sgi[[input$stn]]
        })
        
        output$plot <- renderPlotly({
            ggplotly(ggplot(rc_df_sgi(), aes(x = YMD, y = sgi)) + theme_minimal() +
                         geom_area(aes(y = ifelse(sgi > 0,  sgi, 0)),
                                   fill = "blue",
                                   alpha = 0.6) +
                         geom_area(aes(y = ifelse(sgi < 0,  sgi, 0)),
                                   fill = "red",
                                   alpha = 0.6) +
                         geom_line(y = 0, color = "grey") + geom_line(aes(y = sgi), color = "grey") +
                         geom_line(aes(y = sgi)) + 
                         theme(axis.text = element_text(size = 15),
                               axis.title = element_text(size = 20),
                               axis.title.x = element_blank()) +
                         ylab("SGI"))
        })
        
        on.exit(removeNotification(id))
        
        # 지도 클릭시 sgi 시계열 그래프 출력
        
        sf_obs_list <- select_var %>%
            st_as_sf(., coords = c("경도", "위도"), crs = 4326)
        
        observeEvent(input$map_marker_click, {
            updateSelectInput(inputId = "stn",
                              selected = st_as_sf(
                                  data.frame(lat = round(input$map_marker_click$lat, 4),
                                             lng = input$map_marker_click$lng, 4),
                                  coords = c("lng", "lat"), crs = 4326) %>%
                                  st_as_sf()  %>%
                                  st_join(sf_obs_list,
                                          join = st_nearest_feature) %>%
                                  st_drop_geometry() %>%
                                  dplyr::select(GENNUM)
            )
        })
    })
    # }) # SGI 분석 진행바 닫기(전체)
}
