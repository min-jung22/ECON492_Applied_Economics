### 필요한 패키지
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
 
getwd()
setwd("C:/Users/khang/OneDrive/바탕 화면/DATA/")

### 상품군별 소비자물가 상승률
cpi_delta <- read_excel("품목별물가상승률.xlsx")
head(cpi_delta)

cpi_delta <- cpi_delta %>% gather(key = "품목", value = "cpi_delta", 2:9)
cpi_delta <- cpi_delta %>% arrange(시점)
cpi_delta <- as.data.frame(cpi_delta)
CPIdelta <- cpi_delta[-(385:456),]


### 상품군별 소매판매액 대비 온라인판매액 비중 증감
## 상품군별 소매판매액
retailSales <- read_excel("소매판매액.xlsx")
head(retailSales)

retailSales <- retailSales %>%
  gather(key = "품목", value = "retailSales", 2:9) %>% arrange(시점)

head(retailSales)

## 상품군별 온라인판매액
onlineSales <- read_excel("온라인상품군별거래액.xlsx")
head(onlineSales)

onlineSales <- onlineSales %>%
  gather(key = "품목", value = "onlineSales", 2:9) %>% arrange(시점)
head(onlineSales)

## 상품군별 소매판매액 대비 온라인판매액 비중
weightSales <- merge(retailSales, onlineSales)
head(weightSales)
weightSales <- weightSales %>%
  mutate(weight = onlineSales/retailSales) %>% 
  select(시점, 품목, weight)
head(weightSales)

## 작년동월대비 비중 증감
increase = data.frame(rep(0,96))
increase <- t(increase)

for(i in 97:544){
    increase[i] <- (weightSales$weight[i] - weightSales$weight[i-96])
}
increase[97]

ONLINEdelta <- cbind(weightSales, increase)[-c((1:96),(481:544)),-3]


### gdp갭(대체변수로 산업생산지수에 추세를 제거)
gdpgap_1701_2206 <- read_csv("gdpgap__1701_2206.csv")
t(gdpgap_1701_2206)[-1]
gdpgap_ts <- ts(as.numeric(t(gdpgap_1701_2206)[-1]), start = c(2017,1), frequency = 12)
plot(gdpgap_ts)
gdpgap_ts_decompose <- decompose(gdpgap_ts)
plot(gdpgap_ts_decompose)

gdpgap_alter <- gdpgap_ts - gdpgap_ts_decompose$trend
plot(gdpgap_alter)
gdpgap_alter

gdpgap_alter <- as.data.frame(gdpgap_alter)
gdpgap_alter <- gdpgap_alter[-c((1:12),(61:66)),]
gdpgap_alter <- rep(gdpgap_alter,each=8)

GDPGAPalter <- cbind(CPIdelta[1],gdpgap_alter)
head(GDPGAPalter)

### 원/달러환율 변동률(전년동월비)
exr_delta <- read_excel("환율변동률.xlsx")
exr_delta <- rep(t(exr_delta[-58,2]),each=8)
str(exr_delta)
exr_delta <- cbind(cpi_delta[1],exr_delta)

EXRdelta <- as.data.frame(exr_delta[1:384,])


### 하나의 데이터프레임으로 만들기
time <- data.frame(c(ONLINEdelta[1])) #시점 형식 통일(YYYYMM)
oneData <- cbind(time ,CPIdelta[2:3], ONLINEdelta[3],
                 GDPGAPalter[2],EXRdelta[2])
head(oneData)
summary(oneData)

### 최종 데이터 저장하기
library(writexl)
write_xlsx(oneData, "data_ready_to_use.xlsx")