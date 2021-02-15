
# 첨부파일


#패키지 불러오기
library(yaml); library(knitr)
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(sf)){install.packages("sf"); library(sf)}
if(!require(rgdal)){install.packages("rgdal")}
if(!require(maptools)){install.packages("maptools"); library(maptools)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(plotly)){install.packages("plotly"); library(plotly)}
if(!require(scales)){install.packages("scales"); library(scales)}
if(!require(patchwork)){
  if(!require(devtools)){install.packages("devtools")}
  devtools::install_github("thomasp85/patchwork"); library(plotly)
}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(gridExtra)){install.packages("gridExtra"); library(gridExtra)}
library(tibble)

setwd("C:\\Users\\moonf\\Desktop\\결과물") # 경로를 설정해 줍니다.


####################### 유동인구 데이터 전처리 ###########################

#knit 로 실행하기보다 R 또는 Rstudio에서 직접 실행해 주세요.

{
  map = st_read("./데이터/2020빅콘테스트 문제데이터(혁신아이디어분야)/01_유동인구데이터(SK텔레콤)/4개지역_행정동.shp",
                stringsAsFactors = F)
  map_sp = as_Spatial(map)
  map_sp = spTransform(map_sp,
                       CRS("+proj=longlat"))
  map_df = fortify(map_sp)
  map_meta = cbind(data.frame(map)[as.numeric(map_df$id),1:11], map_df)

  map_meta[,10:11] = sapply(map_meta[,10:11], as.numeric)
  map_meta[map_meta$HDONG_NM == "수성1가동", 11] = 35.856
  map_meta[map_meta$HDONG_NM == "수성2.3가동", 11] = 35.854

  map_meta = map_meta[, c(5, 7, 3, 2, 8, 9, 10:13)]

  gg_map_seoul = map_meta %>% filter(SIDO_NM == "서울특별시") %>%
    select(long, lat, HDONG_CD)
  colnames(gg_map_seoul)[3] = "id"
  gg_map_daegu = map_meta %>% filter(SIDO_NM == "대구광역시") %>%
    select(long, lat, HDONG_CD)
  colnames(gg_map_daegu)[3] = "id"
}


## 나이, 성별 유동인구


{
  year = c("2019", "2020")
  month = paste0("0", 2:5)
  age_flow = NULL
  for(i in 1:2){
    for(j in 1:4){
      name = paste0("./데이터/2020빅콘테스트 문제데이터(혁신아이디어분야)/01_유동인구데이터(SK텔레콤)/4개지역_FLOW_AGE_",
                    year[i], month[j], ".csv")
      # 오류를 없애기 위해 넣은 코드(encoding 문제 예상)
      b = read.table(name, stringsAsFactors = F)
      Encoding(b$V1) = "UTF-8"
      b_mid = matrix(unlist(strsplit(b$V1, "\\|")), ncol = 34, byrow = T)
      colnames(b_mid) = b_mid[1,]
      b_mid = data.frame(b_mid[-1,])

      b_mid[,-4] = sapply(b_mid[,-4], function(x) as.numeric(as.vector(x)))
      b_mid[,1:4] = sapply(b_mid[,1:4], as.character)


      colnames(b_mid)[-(1:4)] = unlist(lapply(strsplit(colnames(b_mid)[-(1:4)], "_FLOW_POP_CNT_"), function(x) paste0(x, collapse = "")))
      colnames(b_mid)[1] = "STD_YM"

      # long type으로 바꿈
      b_midmid = melt(b_mid, id.vars = colnames(b_mid)[1:4], variable.name = "genderage", value.name = "flow_pop")

      # 성별, 나이 분리
      d = unlist(str_split(b_midmid$genderage, "MAN"))
      d[d==""] = "M"
      d_mid = sapply(data.frame(matrix(d, ncol = 2, byrow = T)), as.character)
      colnames(d_mid) = c("gender", "age")

      # 합침
      a = cbind(STD_Y = year[i], b_midmid[,1:4], d_mid, flow_pop = b_midmid[,6])
      age_flow = rbind(age_flow, a)
    }
  }
  age_flow$STD_Y = as.character(age_flow$STD_Y)
  age_flow$STD_YMD = as.Date(age_flow$STD_YMD, format = "%Y%m%d")

  age_flow$weekday = weekdays(age_flow$STD_YMD)
  age_flow$weekend = rep("평일", nrow(age_flow))
  age_flow$weekend[age_flow$weekday %in% c("토요일", "일요일")] = "주말"

  age_flow$SIDO_NM = NA
  age_flow$SGNG_NM = NA
  age_flow[str_detect(age_flow$HDONG_CD, "^27260"), 12] = "수성구"
  age_flow[str_detect(age_flow$HDONG_CD, "^27110"), 12] = "중구"
  age_flow[str_detect(age_flow$HDONG_CD, "^27"), 11] = "대구광역시"
  age_flow[str_detect(age_flow$HDONG_CD, "^11350"), 12] = "노원구"
  age_flow[str_detect(age_flow$HDONG_CD, "^11140"), 12] = "중구"
  age_flow[str_detect(age_flow$HDONG_CD, "^11"), 11] = "서울특별시"


  age_flow = age_flow[,c(1:3, 10, 9, 11, 12, 5, 4, 6, 7, 8)]
}

age_flow$age_n<-NA
age_flow$age_n[age_flow$age<10]="10세 미만"
age_flow$age_n[10 <= age_flow$age & age_flow$age<20]="10대"
age_flow$age_n[20 <= age_flow$age & age_flow$age<30]="20대"
age_flow$age_n[30 <= age_flow$age & age_flow$age<40]="30대"
age_flow$age_n[40 <= age_flow$age & age_flow$age<50]="40대"
age_flow$age_n[50 <= age_flow$age & age_flow$age<60]="50대"
age_flow$age_n[60 <= age_flow$age & age_flow$age<70]="60대"
age_flow$age_n[70 <= age_flow$age]="70세 이상"


## 시간별 유동인구


{
  year = c("2019", "2020")
  month = paste0("0", 2:5)
  time_flow = NULL
  for(i in 1:2){
    for(j in 1:4){
      name = paste0("./데이터/2020빅콘테스트 문제데이터(혁신아이디어분야)/01_유동인구데이터(SK텔레콤)/4개지역_FLOW_TIME_",
                    year[i], month[j], ".csv")
      # 인코딩 때문인지..이대로 해야 오류 없음
      b = read.table(name, stringsAsFactors = F)
      Encoding(b$V1) = "UTF-8"
      b_mid = matrix(unlist(strsplit(b$V1, "\\|")), ncol = 28, byrow = T)
      colnames(b_mid) = b_mid[1,]
      b_mid = data.frame(b_mid[-1,])

      b_mid[,-4] = sapply(b_mid[,-4], function(x) as.numeric(as.vector(x)))
      b_mid[,1:4] = sapply(b_mid[,1:4], as.character)

      colnames(b_mid)[1] = "STD_YM"

      # long type으로
      b_midmid = melt(b_mid, id.vars = colnames(b_mid)[1:4], variable.name = "hour", value.name = "flow_pop")

      b_midmid$hour = str_remove(b_midmid$hour, "TMST_")
      b_midmid$time = paste(b_midmid$STD_YMD, b_midmid$hour)

      b_midmid$STD_Y = year[i]
      time_flow = rbind(time_flow, b_midmid)
    }
  }
  time_flow$STD_YMD = as.Date(time_flow$STD_YMD, format = "%Y%m%d")
  time_flow$time = as.POSIXct(paste0(time_flow$time, "00"), format = "%Y%m%d %H%M")

  time_flow$weekday = weekdays(time_flow$STD_YMD)
  time_flow$weekend = rep("평일", nrow(time_flow))
  time_flow$weekend[time_flow$weekday %in% c("토요일", "일요일")] = "주말"

  time_flow$SIDO_NM = NA
  time_flow$SGNG_NM = NA
  time_flow[str_detect(time_flow$HDONG_CD, "^27260"), 12] = "수성구"
  time_flow[str_detect(time_flow$HDONG_CD, "^27110"), 12] = "중구"
  time_flow[str_detect(time_flow$HDONG_CD, "^27"), 11] = "대구광역시"
  time_flow[str_detect(time_flow$HDONG_CD, "^11350"), 12] = "노원구"
  time_flow[str_detect(time_flow$HDONG_CD, "^11140"), 12] = "중구"
  time_flow[str_detect(time_flow$HDONG_CD, "^11"), 11] = "서울특별시"

  time_flow = time_flow[, c(8, 1, 2, 10, 9, 5, 7, 11, 12, 4, 3, 6)]
}

# 저장
save(map_meta, age_flow, time_flow, file="./코드/유동인구.rdata")

########################### 지도 데이터 전처리 ###################################3


# knit 로 실행하기보다 R 또는 Rstudio에서 직접 실행해 주세요.

{
  empty_theme = theme(legend.position = "right",
                      legend.title = element_blank(),
                      legend.text = element_text(size = 8),
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.background = element_blank())
  # 서울시 전체 지도
  {
    seoulmap = st_read("./데이터/seoul.shp", stringsAsFactors = F)
    Encoding(seoulmap$SIG_KOR_NM) = "CP949"

    seoul = seoulmap %>% select(SIG_KOR_NM)

    seoulmap_sp = as(seoul, "Spatial")

    seoulmap_longlat = spTransform(seoulmap_sp,
                                   CRS("+proj=longlat"))
    seoul_tot = fortify(seoulmap_longlat)

    levels(seoul_tot$group) = seoul$SIG_KOR_NM

    seoul_gu_center = read.csv("./데이터/seoul_gu_center.txt", sep=",",encoding = "CP949")

  }

  seoul_base_map = ggplot(seoul_tot) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", col = "lightgrey") +
    geom_text(data = seoul_gu_center,
              aes(x = long, y = lat, label = group), size = 2.8, col = "lightgrey") +
    empty_theme  +
    coord_cartesian(ylim = c(37.54, 37.7), xlim = c(126.96, 127.12)) +
    theme(legend.position = "top")


  # 대구 지도
  {
    daegumap = st_read("./데이터/daegu.shp", stringsAsFactors = F)
    Encoding(daegumap$SIG_KOR_NM) = "CP949"

    daegu = daegumap %>% select(SIG_KOR_NM)

    daegumap_sp = as(daegu, "Spatial")

    daegumap_longlat = spTransform(daegumap_sp,
                                   CRS("+proj=longlat"))
    daegu_tot = fortify(daegumap_longlat)

    levels(daegu_tot$group) = daegu$SIG_KOR_NM[floor(as.numeric(levels(daegu_tot$group)))]
    daegu_gu_center = daegu_tot %>% select(long, lat, group) %>%
      group_by(group) %>%
      summarize(long = mean(long),
                lat = mean(lat))
    }
daegu_base_map = ggplot() +
coord_cartesian(ylim = c(35.785, 35.885), xlim = c(128.58, 128.73)) +
  empty_theme +
  theme(legend.position = "top")

}

## 변수 및 데이터 저장
save(seoul_base_map, daegu_base_map, gg_map_daegu, gg_map_seoul, file="./코드/지도.rdata")
save(seoul_tot, daegu_tot, seoul_gu_center, daegu_gu_center, label, file="./코드/전체지도.rdata")







