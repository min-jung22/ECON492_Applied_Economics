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
gdpgap_ts <- log(gdpgap_ts)
plot(gdpgap_ts)
gdpgap_ts_decompose <- decompose(gdpgap_ts, type="additive")
plot(gdpgap_ts_decompose)

gdpgap_alter <- gdpgap_ts - gdpgap_ts_decompose$trend
plot(gdpgap_alter)
gdpgap_alter

gdpgap_alter <- as.data.frame(gdpgap_alter)
gdpgap_alter <- gdpgap_alter[-c((1:12),(61:66)),]

gdpgap_alter <- rep(gdpgap_alter,each=8)

GDPGAPalter <- cbind(CPIdelta[1],gdpgap_alter)
head(GDPGAPalter)
# write_xlsx(gdpgap_alter, "gdpgap_alter_1108.xlsx")

### 원/달러환율 변동률(전년동월비)
exr_delta <- read_excel("exr_1108.xlsx")
exr_delta <- rbind(NA, exr_delta)
exr_delta <- exr_delta[-1]
exr_delta <- rep(t(exr_delta),each=8)

exr_delta <- cbind(ONLINEdelta[1],exr_delta)

EXRdelta <- as.data.frame(exr_delta)

### 하나의 데이터프레임으로 만들기
time <- data.frame(c(ONLINEdelta[1])) #시점 형식 통일(YYYYMM)
CPIdelta <- cbind(time, CPIdelta[2:3])
mergeData <- merge(CPIdelta, ONLINEdelta)

oneData <- cbind(mergeData,
                 GDPGAPalter[2],EXRdelta[2])
head(oneData)
summary(oneData)

## 시간ID 추가
timeID <- data.frame(rep((1:48),each=8))
data1 <- cbind(timeID, oneData)
colnames(data1)[1] = "timeID"
#data1$timeID <-as.numeric(data1$timeID)
head(data1)

## 상품군ID 추가
product <- data.frame(rep(c(2,1,8,4,5,6,3,7),48))
data2 <- cbind(data1[1:2],product,data1[3:7])
colnames(data2)[3] = "productID"
#data2$productID <-as.numeric(data2$productID)
head(data2)

data0 <- data2[-c(2,4)]
head(data0)


#### data를 상품군별로 묶어서 시점 주르륵 나오게 해야해
df <- data0 %>% arrange(productID)
df <- cbind(df[2],df[1],df[3:6])
summary(df)
df$productID <- as.factor(df$productID)
df$timeID <- as.factor(df$timeID)

df %>% is.pbalanced()

### 최종 데이터 저장하기
library(writexl)
write_xlsx(df, "data_1108.xlsx")

