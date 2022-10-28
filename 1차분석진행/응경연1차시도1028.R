### 1차 시도 (22.10.28)


### 데이터 불러오기
setwd("C:/Users/khang/OneDrive/바탕 화면/DATA/")

library(readxl)
data <- read_excel("data_ready_to_use.xlsx")
summary(data)

### 시점, 품목을 ID로 변경해 사용함
## 시점ID 추가
timeID <- data.frame(rep((1:48),each=8))
data1 <- cbind(timeID, data)
colnames(data1)[1] = "timeID"
data1$timeID <-as.numeric(data1$timeID)
head(data1)

## 상품군ID 추가
product <- data.frame(rep((1:8),48))
data2 <- cbind(data1[1:2],product,data1[3:7])
colnames(data2)[3] = "productID"
data2$productID <-as.numeric(data2$productID)
head(data2)

data0 <- data2[-c(2,4)]
head(data0)


### 다중선형회귀
## 내생성있는 변수 없이 진행
lm(cpi_delta ~ increase + gdpgap_alter + exr_delta, data0)

lmtry1 <- lm(cpi_delta ~ increase + gdpgap_alter + exr_delta, data0)
summary(lmtry1) ##increase(온라인판매비중증감) 변수 유의함

##내생성있는 변수를 그냥 추가하고 선형회귀 진행한 경우
lmtry2 <- lm(cpi_delta ~ increase +
              lag(cpi_delta, 1) + gdpgap_alter +exr_delta, data0)
summary(lmtry2) ##increase(온라인판매비중증감) 변수 유의함




###동적패널모형 시도
library(plm)
library(stargazer)

## 차분gmm(difference gmm, ab gmm) : 도구변수로 수준변수만 사용

## 원래 원하는 식.. 데이터의 균형이 안맞아서 에러? 상관관계?? 상품군별이 아닌 변수가 있어서 그런가
trial1 <- pgmm(cpi_delta ~ increase +
                 lag(cpi_delta, 1) + gdpgap_alter + exr_delta
               | lag(cpi_delta, 1:99), data=data0, 
               effect = "twoways", model = "twosteps"  ) 
    #역행렬이 안나와서 생기는 오류?? 변수의 크기가 많이 다르면 그럴 수 있다고 해서 수정해봤는데도 안됨..


## 위 식에서 상품군별 아닌 변수 제외하고 돌리면 돌아가네.. 유의하지 않음
trial2 <- pgmm(cpi_delta ~ increase +
                 lag(cpi_delta, 1) 
               | lag(cpi_delta, 1:99), data=data0, 
               effect = "twoways", model = "twosteps"  )
summary(trial2)


##system gmm : 도구변수로 수준변수 뿐 아니라 과거값을 다 집어넣는 거
##수준값이 불안정하더라도 과거값을 다 사용하니까 추정 더 잘할 수 있어
sysTrial1 <- pgmm(cpi_delta ~ increase +
                    lag(cpi_delta, 1)+ gdpgap_alter +exr_delta
                  | lag(cpi_delta, 1:99), data=data0,
                  effect = "twoways", model = "twosteps", transformation = "ld")

summary(sysTrial1) #경고는 있지만 나옴... 전기의 물가상승률만 유의?

