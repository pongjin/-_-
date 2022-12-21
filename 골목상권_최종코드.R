rm(list=ls())
library(dbscan)
library(dplyr)
library(cluster) #shilouette 그래프
library(fpc) #clustering 시각화 package
library(factoextra) #elbow plot 그리는 package
library(ggplot2)
library(ggmap)
library(car)
library(psych)
setwd('C:/Users/kimpu/Desktop/onedrive/OneDrive - dongguk.edu/OneDrive_2022-07-22')
#load data
price <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2021년.csv',encoding='cp949')
store <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_점포)_2021년.csv')
around <- read.csv('서울시 우리마을가게 상권분석서비스(상권배후지-집객시설).csv',encoding='cp949')
around_bus <- read.csv('서울시 우리마을가게 상권분석서비스(상권-집객시설).csv',encoding='cp949')
people <- read.csv('서울시 우리마을가게 상권분석서비스(상권-생활인구).csv',encoding='cp949')
income <- read.csv('서울시 우리마을가게 상권분석서비스(상권배후지-소득소비).csv',encoding='cp949')
living <- read.csv('서울시 우리마을가게 상권분석서비스(상권-상주인구).csv',encoding='cp949')
area<- read.csv('서울시_우리마을가게_상권분석서비스(상권_면적).csv')

###################시각화
setwd('C:/Users/kimpu/Desktop/onedrive/OneDrive - dongguk.edu/골목상권')
survey <- read.csv('2019_연간자료_20220709_13051.csv')
str(survey)
#View(survey)
#창업 어려움정도가 입지선정
nosurvey <- survey[is.na(survey$경영_애로사항1코드)==FALSE,]
survey[survey$경영_애로사항1코드=="*",'경영_애로사항1코드']=NA
survey[survey$경영_애로사항1코드=="**",'경영_애로사항1코드']=NA
survey[survey$경영_애로사항1코드=="***",'경영_애로사항1코드']=NA
survey[survey$경영_애로사항1코드=="",'경영_애로사항1코드']=NA
nosurvey[nosurvey$경영_애로사항1코드=="1",]$경영_애로사항1코드= '상권쇠퇴'
nosurvey[nosurvey$경영_애로사항1코드=="2",'경영_애로사항1코드']='동일 업종 경쟁심화'
nosurvey[nosurvey$경영_애로사항1코드=="3",'경영_애로사항1코드']='원재료비·재료매입비'
nosurvey[nosurvey$경영_애로사항1코드=="4",'경영_애로사항1코드']='최저임금 영향(인건비)'
nosurvey[nosurvey$경영_애로사항1코드=="5",'경영_애로사항1코드']='보증금·월세'
nosurvey[nosurvey$경영_애로사항1코드=="6",'경영_애로사항1코드']='부채상환'
nosurvey[nosurvey$경영_애로사항1코드=="7",'경영_애로사항1코드']='인력관리(채용의 어려움 등)'
nosurvey[nosurvey$경영_애로사항1코드=="8",'경영_애로사항1코드']='기타'

#경엉 애로사항 1순위가 상권쇠퇴이다. 
nosurvey <- survey[is.na(survey$경영_애로사항1코드)==FALSE,]
par(mar=c(8,8,8,8))
barplot(sort(table(nosurvey$경영_애로사항1코드),decreasing=F),main="경영_애로사항",xlab="응답횟수",horiz=T)
barplot(prop.table(sort(table(nosurvey$경영_애로사항1코드),decreasing=F)),main="경영_애로사항",xlab="응답비율",horiz=T,las=1)

survey[survey=="",'창업_어려움정도_입지선정코드']=NA

survey <- survey[is.na(survey$창업_어려움정도_입지선정코드)==FALSE,]
survey[survey$창업_어려움정도_입지선정코드=="3",'창업_어려움정도_입지선정코드']='어려움'
survey[survey$창업_어려움정도_입지선정코드=="4",'창업_어려움정도_입지선정코드']='어려움'
survey[survey$창업_어려움정도_입지선정코드=="5",'창업_어려움정도_입지선정코드']='어려움'
survey[survey$창업_어려움정도_입지선정코드=="2",'창업_어려움정도_입지선정코드']='보통'
survey[survey$창업_어려움정도_입지선정코드=="1",'창업_어려움정도_입지선정코드']='어려움없음'

table(survey$창업_어려움정도_입지선정코드)
par(mar=c(4,4,4,4))
barplot(prop.table(sort(table(survey$창업_어려움정도_입지선정코드),decreasing=F)),main="입지선정",xlab="",ylab='응답 비율',las=1)

#년도별 연령대 매출비율 선그래프
#년도별 20대 매출액비율 증가
#년도별 20대 매출건수 증가

#골목상권 내 연령대 별 매출액 or 매출건수 증가 추이
price <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2021년.csv',encoding='cp949')

#21년 골목상권의 매출비율
data1 <- price %>% group_by(상권_구분_코드_명) %>% summarise(매출액=sum(분기당_매출_금액),
                                                         매출건수=sum(분기당_매출_건수))

data1 <- data.frame(data1)
rownames(data1) <- data1$상권_구분_코드_명
data1$상권_구분_코드_명 <- NULL
str(data1)
data1$매출액 <- data1$매출액/colSums(data1)[1]
data1$매출건수 <- data1$매출건수/colSums(data1)[2]
data1
table(data1$매출액)
data2 <- t(data1)
barplot(data2[,c(3,1,4,2)],beside=TRUE,col=c("red","beige"),main="2021년 서울시 상권 별 매출",xlab="상권명",ylab='비율',las=1)
legend("topright",legend=c("매출액","매출건수"),fill=c("red","beige"),border="white",box.lty=0,cex=1.5)


#데이터불러오기
price2020 <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2020년.csv',encoding='cp949')
price2019 <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2019년.csv',encoding='cp949')
price2018 <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2018년.csv',encoding='cp949')
price2017 <- read.csv('서울시_우리마을가게_상권분석서비스(신_상권_추정매출)_2017년.csv',encoding='cp949')
#골목 상권 내 2030의 매출건수 추이
#17년
price_cnt <- price2017[,c(2,75:79)]
cnt2017 <- price_cnt %>% summarise(건수20=mean(연령대_20_매출_건수),
                                     건수30=mean(연령대_30_매출_건수),
                                     건수40=mean(연령대_40_매출_건수),
                                     건수50=mean(연령대_50_매출_건수),
                                     건수60=mean(연령대_60_이상_매출_건수))
cnt2017
#18년
price_cnt <- price2018[,c(2,75:79)]
cnt2018 <- price_cnt %>% summarise(건수20=mean(연령대_20_매출_건수),
                                     건수30=mean(연령대_30_매출_건수),
                                     건수40=mean(연령대_40_매출_건수),
                                     건수50=mean(연령대_50_매출_건수),
                                     건수60=mean(연령대_60_이상_매출_건수))
cnt2018
#19년
price_cnt <- price2019[,c(2,75:79)]
cnt2019 <- price_cnt %>% summarise(건수20=mean(연령대_20_매출_건수),
                                     건수30=mean(연령대_30_매출_건수),
                                     건수40=mean(연령대_40_매출_건수),
                                     건수50=mean(연령대_50_매출_건수),
                                     건수60=mean(연령대_60_이상_매출_건수))
cnt2019
#20년
price_cnt <- price2020[,c(2,75:79)]
cnt2020 <- price_cnt %>% summarise(건수20=mean(연령대_20_매출_건수),
                                     건수30=mean(연령대_30_매출_건수),
                                     건수40=mean(연령대_40_매출_건수),
                                     건수50=mean(연령대_50_매출_건수),
                                     건수60=mean(연령대_60_이상_매출_건수))
cnt2020
#21년
price_cnt <- price[,c(2,75:79)]
cnt2021 <- price_cnt %>% summarise(건수20=mean(연령대_20_매출_건수),
                                     건수30=mean(연령대_30_매출_건수),
                                     건수40=mean(연령대_40_매출_건수),
                                     건수50=mean(연령대_50_매출_건수),
                                     건수60=mean(연령대_60_이상_매출_건수))
cnt2021
cnt <- rbind(cnt2017,cnt2018)
cnt <- rbind(cnt,cnt2019)
cnt <- rbind(cnt,cnt2020)
cnt <- rbind(cnt,cnt2021)
cnt
rownames(cnt) <- c('2017','2018','2019','2020','2021')
colnames(cnt) <- c('20대','30대','40대','50대','60대이상')
cnt[1,] = cnt[1,]/rowSums(cnt)[1]
cnt[2,] = cnt[2,]/rowSums(cnt)[2]
cnt[3,] = cnt[3,]/rowSums(cnt)[3]
cnt[4,] = cnt[4,]/rowSums(cnt)[4]
cnt[5,] = cnt[5,]/rowSums(cnt)[5]
#그래프
plot(cnt[,1],type='o',lwd=5,xlab='연도',ylab='매출건수',ylim=c(0,0.5),axes=F,col=2,main='5개년 매출비율',las=1)
lines(cnt[,2], type = 'o', lwd=5,col = 3)
lines(cnt[,3], type = 'o', lwd=1,col = 4)
lines(cnt[,4], type = 'o', lwd=1,col = 5)
lines(cnt[,5], type = 'o', lwd=1,col = 6)
axis(1, at=1:5,labels=c('2017','2018','2019','2020','2021'),las=1)
axis(2, ylim = c(0,250000),las=1)
legend('topright', colnames(cnt),cex = 0.8, pch = 1, col = 2:6, lty = 1)

#업종별 매출비율 확인
choose <- cnt_all[c(17,61,50),]
#rownames(choose) <- choose$서비스_업종_코드_명
#choose$서비스_업종_코드_명 <- NULL
#colnames(choose) <- c('20대','30대','40대','50대','60대')
aa <- t(choose)
aa
aa <- aa[2:6,]
aa
colnames(aa) <- choose$서비스_업종_코드_명
aa <- data.frame(aa)
aa$서적 <- as.numeric(aa$서적)
aa$호프.간이주점 <- as.numeric(aa$호프.간이주점)
aa$청과상 <- as.numeric(aa$청과상)
barplot(choose[1,])

data_bar <- aa$서적

names(data_bar) <- c('20대','30대','40대','50대','60대')
barplot(data_bar,col=2:6,main="서점",ylab='비율',las=1)
data_bar <- aa$호프.간이주점
names(data_bar) <- c('20대','30대','40대','50대','60대')
barplot(data_bar,col=2:6,main="호프.간이주점",ylab='비율',las=1)
data_bar <- aa$청과상
names(data_bar) <- c('20대','30대','40대','50대','60대')
barplot(data_bar,col=2:6,main="청과상",ylab='비율',las=1)

##############################20대가 선호하는 업종 찾기
#업종별 군집화해보기
set.seed(101)
set.seed(2021)
str(price)
summary(price[,2])
colnames(price)[c(8,73:79)]
price_cnt <- price[,c(8,73:79)]
gg2 <- group_by(price_cnt,price_cnt$서비스_업종_코드_명)
cnt_all <- summarize(gg2,sum20=sum(연령대_20_매출_건수,na.rm=TRUE),
                     sum30=sum(연령대_30_매출_건수,na.rm=TRUE),
                     sum40=sum(연령대_40_매출_건수,na.rm=TRUE),
                     sum50=sum(연령대_50_매출_건수,na.rm=TRUE),
                     sum60=sum(연령대_60_이상_매출_건수,na.rm=TRUE));cnt_all
cnt_all <- data.frame(cnt_all)
colnames(cnt_all) <- c('서비스_업종_코드_명','sum20','sum30','sum40','sum50','sum60')

price()
#비율로 변환
for (i in 1:nrow(cnt_all)){
  cnt_all[i,2:6]=cnt_all[i,2:6]/sum(cnt_all[i,2:6])
}
head(cnt_all,10)

#군집화 확인
set.seed(2022)
set.seed(2021)
par(mfrow=c(2,2))
#elbow plot
wss <- 0
for(i in 1:10) wss[i]<-sum(kmeans(cnt_all[2:6],centers = i)$withinss)
plot(1:10, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")
#silhouette
km_cnt <- kmeans(cnt_all[2:6],4,nstart=10)  #4개가 제일 잘나옴 
distance <- dist(cnt_all[2:6], method= "euclidean")
sil = silhouette (km_cnt$cluster, distance)
plot(sil)
#hclust
par(mfrow=c(1,1))
hcl <- hclust(dist(cnt_all[2:6]),method='average')
summary(hcl)
plot(hcl)
rect.hclust(hcl,4)
group <- cutree(hcl,4)
cnt_all$cl <- group
fviz_cluster(group,cnt_all[2:6])
sil = silhouette (cnt_all$cl, dist(cnt_all[2:6]))
plot(sil)
#kmeans
km_cnt #1: 골고루 2:405060대 3:40대 4:20+30
km_cnt$centers
km_cnt$size
fviz_cluster(km_cnt,cnt_all[2:6])
cnt_all$cl <- km_cnt$cluster
table(cnt_all$cl) #16개의 20대를 겨냥한 시설이 있다.
cnt_all[cnt_all$cl==1,] #업종 종류 확인
cnt_all[cnt_all$cl==2,] #업종 종류 확인
cnt_all[cnt_all$cl==3,] #업종 종류 확인
cnt_all[cnt_all$cl==4,] #업종 종류 확인
km_cnt$size
########################################################
########################################################store
golmok <- store[store$상권_구분_코드=='A' & store$기준_년_코드==2021,]
golmok$총점포수 <- golmok$유사_업종_점포_수 #최종 점포수
str(golmok)
colnames(golmok)[c(6,8,10)]
golmok_cnt <- golmok[,c(6,8,10)]
golmok_cnt <- golmok_cnt[golmok_cnt$유사_업종_점포_수!=0,]  #0인거 다 정리
golmok_2021 <- golmok_cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(점포수 = mean(유사_업종_점포_수))#년도별 평균값

golmok_2021[golmok_2021$]

# 40대 중심 주 소비 업종
golmok40<- golmok_2021[golmok_2021$서비스_업종_코드_명=='예술학원'|golmok_2021$서비스_업종_코드_명=='외국어학원'|golmok_2021$서비스_업종_코드_명=='일반교습학원'|golmok_2021$서비스_업종_코드_명=='골프연습장'|golmok_2021$서비스_업종_코드_명=='서적'|golmok_2021$서비스_업종_코드_명=='스포츠 강습'|golmok_2021$서비스_업종_코드_명=='자동차미용',]
golmok40

ggg2 <- group_by(golmok40,golmok40$상권_코드_명)
golmok_40_store <- data.frame(summarize(ggg2,점포수=sum(점포수,na.rm=TRUE)))
colnames(golmok_40_store) <- c('상권_코드_명','40대점포수')
head(golmok_40_store)


# 고연령대 주소비업종 (클러스터3)
golmok_high<- golmok_2021[golmok_2021$서비스_업종_코드_명=='가전제품'|golmok_2021$서비스_업종_코드_명=='가전제품수리'|golmok_2021$서비스_업종_코드_명=='당구장'|golmok_2021$서비스_업종_코드_명=='미곡판매'|golmok_2021$서비스_업종_코드_명=='섬유제품'|golmok_2021$서비스_업종_코드_명=='수산물판매'|golmok_2021$서비스_업종_코드_명=='운동/경기용품'|golmok_2021$서비스_업종_코드_명=='육류판매'|golmok_2021$서비스_업종_코드_명=='의료기기'|golmok_2021$서비스_업종_코드_명=='의약품'|golmok_2021$서비스_업종_코드_명=='인테리어'|golmok_2021$서비스_업종_코드_명=='일반의원'|golmok_2021$서비스_업종_코드_명=='자동차수리'|golmok_2021$서비스_업종_코드_명=='조명용품'|golmok_2021$서비스_업종_코드_명=='철물점'|golmok_2021$서비스_업종_코드_명=='청과상'|golmok_2021$서비스_업종_코드_명=='치과의원'|golmok_2021$서비스_업종_코드_명=='한의원'|golmok_2021$서비스_업종_코드_명=='의료기기',]
golmok_high

ggg3 <- group_by(golmok_high,golmok_high$상권_코드_명)
golmok_high_store <- data.frame(summarize(ggg3,점포수=sum(점포수,na.rm=TRUE)))
colnames(golmok_high_store) <- c('상권_코드_명','고연령대_점포수')
dim(golmok_high_store)


# 20대 주 소비 업종
golmok_low<- golmok_2021[golmok_2021$서비스_업종_코드_명=='PC방'|golmok_2021$서비스_업종_코드_명=='네일숍'|golmok_2021$서비스_업종_코드_명=='노래방'|golmok_2021$서비스_업종_코드_명=='스포츠클럽'|golmok_2021$서비스_업종_코드_명=='안경'|golmok_2021$서비스_업종_코드_명=='양식음식점'|golmok_2021$서비스_업종_코드_명=='여관'|golmok_2021$서비스_업종_코드_명=='일식음식점'|golmok_2021$서비스_업종_코드_명=='커피-음료'|golmok_2021$서비스_업종_코드_명=='컴퓨터및주변장치판매'|golmok_2021$서비스_업종_코드_명=='패스트푸드점'|golmok_2021$서비스_업종_코드_명=='피부관리실'|golmok_2021$서비스_업종_코드_명=='호프-간이주점'|golmok_2021$서비스_업종_코드_명=='화장품'|golmok_2021$서비스_업종_코드_명=='화초'|golmok_2021$서비스_업종_코드_명=='편의점',]
golmok_low

ggg4 <- group_by(golmok_low,golmok_low$상권_코드_명)
golmok_low_store <- data.frame(summarize(ggg4,점포수=sum(점포수,na.rm=TRUE)))
colnames(golmok_low_store) <- c('상권_코드_명','20대_점포수')
head(golmok_low_store)

# 30대 주 소비 업종
golmok_low_ex20<- golmok_2021[golmok_2021$서비스_업종_코드_명=='네일숍'|golmok_2021$서비스_업종_코드_명=='완구'|golmok_2021$서비스_업종_코드_명=='고시원'|golmok_2021$서비스_업종_코드_명=='안경'|golmok_2021$서비스_업종_코드_명=='스포츠클럽'|golmok_2021$서비스_업종_코드_명=='양식음식점'|golmok_2021$서비스_업종_코드_명=='일식음식점'|golmok_2021$서비스_업종_코드_명=='커피-음료'|golmok_2021$서비스_업종_코드_명=='컴퓨터및주변장치판매'|golmok_2021$서비스_업종_코드_명=='패스트푸드점'|golmok_2021$서비스_업종_코드_명=='피부관리실'|golmok_2021$서비스_업종_코드_명=='호프-간이주점'|golmok_2021$서비스_업종_코드_명=='화장품'|golmok_2021$서비스_업종_코드_명=='화초'|golmok_2021$서비스_업종_코드_명=='편의점',]
golmok_low_ex20

ggg5 <- group_by(golmok_low_ex20,golmok_low_ex20$상권_코드_명)
golmok_low_ex20_store <- data.frame(summarize(ggg5,점포수=sum(점포수,na.rm=TRUE)))
colnames(golmok_low_ex20_store) <- c('상권_코드_명','30대_점포수')
head(golmok_low_ex20_store)

# 각 연령대 업종별, 상권별 총매출 매출건수 변수생성
######################################price
price2 <- price[price$상권_구분_코드=='A' & price$기준_년_코드==2021,]
str(price2)

# 
price_20cnt <- price2[,c(6,8,52,75)];head(price_cnt)
str(price_20cnt)
sum(is.na(price_20cnt)) # 결측x

price_30cnt <- price2[,c(6,8,53,76)];head(price_30cnt)
str(price_30cnt)
sum(is.na(price_30cnt))

price_40cnt <- price2[,c(6,8,54,77)];head(price_40cnt)
str(price_40cnt)
sum(is.na(price_40cnt))

price_50cnt <- price2[,c(6,8,55,78)];head(price_50cnt)
str(price_50cnt)
sum(is.na(price_50cnt))

price_60cnt <- price2[,c(6,8,56,79)];head(price_60cnt)
str(price_60cnt)
sum(is.na(price_60cnt))

# 
price_20cnt <- price_20cnt[price_20cnt$연령대_20_매출_금액!=0 | price_20cnt$연령대_20_매출_건수!=0,]  # 건수가있는데 매출이0인경우는 환불건수.
price_30cnt <- price_30cnt[price_30cnt$연령대_30_매출_금액!=0 | price_30cnt$연령대_30_매출_건수!=0,]  # 건수가있는데 매출이0인경우는 환불건수.
price_40cnt <- price_40cnt[price_40cnt$연령대_40_매출_금액!=0 | price_40cnt$연령대_40_매출_건수!=0,]
price_50cnt <- price_50cnt[price_50cnt$연령대_50_매출_금액!=0 | price_50cnt$연령대_50_매출_건수!=0,]
price_60cnt <- price_60cnt[price_60cnt$연령대_60_이상_매출_금액!=0 | price_60cnt$연령대_60_이상_매출_건수!=0,]


# 년도별 상권별 업종 매출액, 매출건수 평균
price_2021_20 <- price_20cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(매출액 = mean(연령대_20_매출_금액),
                                                                                매출건수 = mean(연령대_20_매출_건수))
price_2021_30 <- price_30cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(매출액 = mean(연령대_30_매출_금액),
                                                                                매출건수 = mean(연령대_30_매출_건수))

price_2021_40 <- price_40cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(매출액 = mean(연령대_40_매출_금액),
                                                                                매출건수 = mean(연령대_40_매출_건수))

price_2021_50 <- price_50cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(매출액 = mean(연령대_50_매출_금액),
                                                                                매출건수 = mean(연령대_50_매출_건수))
price_2021_60 <- price_60cnt %>% group_by(상권_코드_명,서비스_업종_코드_명) %>% summarise(매출액 = mean(연령대_60_이상_매출_금액),
                                                                                매출건수 = mean(연령대_60_이상_매출_건수))


#년도별 상권 매출액, 매출건수 총합값
price_2021_20_1 <- price_2021_20 %>% group_by(상권_코드_명) %>% summarise(매출액 = sum(매출액),
                                                                        매출건수 = sum(매출건수))
price_2021_20_1 <- data.frame(price_2021_20_1)

price_2021_30_1 <- price_2021_30 %>% group_by(상권_코드_명) %>% summarise(매출액 = sum(매출액),
                                                                        매출건수 = sum(매출건수))
price_2021_30_1 <- data.frame(price_2021_30_1)

price_2021_40_1 <- price_2021_40 %>% group_by(상권_코드_명) %>% summarise(매출액 = sum(매출액),
                                                                        매출건수 = sum(매출건수))
price_2021_40_1 <- data.frame(price_2021_40_1)

price_2021_50_1 <- price_2021_50 %>% group_by(상권_코드_명) %>% summarise(매출액 = sum(매출액),
                                                                        매출건수 = sum(매출건수))
price_2021_50_1 <- data.frame(price_2021_50_1)

price_2021_60_1 <- price_2021_60 %>% group_by(상권_코드_명) %>% summarise(매출액 = sum(매출액),
                                                                        매출건수 = sum(매출건수))
price_2021_60_1 <- data.frame(price_2021_60_1)



str(price_2021_40_1) # 상권의 20대 총 매출액, 매출건수
head(price_2021_20) # 상권의 업종마다 20대 총매출 및 매출건수
#########################################################around : 배후지 집객시설


str(around_20)
around <- around[around$상권_구분_코드=='A' & around$기준_년_코드==2021 & around$기준_분기_코드==4,]
around[is.na(around)]<-0
around_20 <- around[,c(6,25)]
around_20$지하철 = ifelse (around_20$지하철_역_수!=0,1,0)
around_20$대학교 = ifelse (around$대학교_수!=0,1,0)
around_20$백화점 = ifelse (around$백화점_수!=0,1,0)
around_20$의료기관 = rowSums(around[,c(10,11,12)])
around_20$관공서 = around[,8]
str(around_20)
around_20 <- around_20[,c(1,3,4,5,6,7)]
around_20
#########################################################around_bus
str(around_bus)
around_bus <- around_bus[around_bus$상권_구분_코드=='A' & around_bus$기준_년_코드==2021 & around_bus$기준_분기_코드==4,]
around_20_bus <- around_bus[,c(6,25)]
around_20_bus$bus <- around_bus$버스_정거장_수
around_20_bus$bus <- ifelse (!is.na(around_20_bus$bus),around_20_bus$bus,0)
around_20_bus <- around_20_bus[,c(1,3)]
around_20_sum <- left_join(around_20,around_20_bus,by='상권_코드_명')
around_20 <- na.omit(around_20_sum)
str(around_20)

#########################################################living : 상주인구
str(living)
living$상권_코드_명 <- living$상권.코드.명
living <- living[living$상권_구분_코드=='A' & living$기준_년_코드==2021,]
living <- living[living$연령대.20.상주인구.수 !=0,]

living_golmok<- living[,c(31,6)]
living_golmok<-living_golmok %>% group_by(상권_코드_명) %>% summarise(총_상주인구 = mean(총.상주인구.수))
living_golmok <-data.frame(living_golmok) # 총 평균상주인구
str(living_golmok)

# 총상주인구 변수
living_golmok[,"연령대_20_총_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(연령대.20.상주인구.수)))[,2]
living_golmok[,"연령대_30_총_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(연령대.30.상주인구.수)))[,2]
living_golmok[,"연령대_40_총_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(연령대.40.상주인구.수)))[,2]
living_golmok[,"연령대_50_총_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(연령대.50.상주인구.수)))[,2]
living_golmok[,"연령대_60_총_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(연령대.60.이상.상주인구.수)))[,2]

living_golmok[,"남성_20_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(남성연령대.20.상주인구.수)))[,2]
living_golmok[,"여성_20_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(여성연령대.20.상주인구.수)))[,2]
living_golmok[,"남성_30_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(남성연령대.30.상주인구.수)))[,2]
living_golmok[,"여성_30_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(여성연령대.30.상주인구.수)))[,2]
living_golmok[,"남성_40_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(남성연령대.40.상주인구.수)))[,2]
living_golmok[,"여성_40_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(여성연령대.40.상주인구.수)))[,2]
living_golmok[,"남성_50_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(남성연령대.50.상주인구.수)))[,2]
living_golmok[,"여성_50_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(여성연령대.50.상주인구.수)))[,2]
living_golmok[,"남성_60_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(남성연령대.60.이상.상주인구.수)))[,2]
living_golmok[,"여성_60_상주인구"]<-data.frame(living %>% group_by(상권_코드_명) %>% summarise(상주인구 = mean(여성연령대.60.이상.상주인구.수)))[,2]

sum(is.na(living_golmok))
str(living_golmok)

############################################################# work
work1 <- work[work$상권_구분_코드=='A' & work$기준_년월_코드==2021,]
str(work1)
work_golmok <- work1 %>% group_by(상권_코드_명) %>% summarise(연령대_20_직장_인구_수 = mean(연령대_20_직장_인구_수),
                                                             연령대_30_직장_인구_수 = mean(연령대_30_직장_인구_수),
                                                             연령대_40_직장_인구_수 = mean(연령대_40_직장_인구_수),
                                                             연령대_50_직장_인구_수 = mean(연령대_50_직장_인구_수),
                                                             연령대_60_직장_인구_수 = mean(연령대_60_이상_직장_인구_수),
                                                             
                                                             남성연령대_20_직장_인구_수 = mean(연령대_20_직장_인구_수),
                                                             여성연령대_20_직장_인구_수 = mean(연령대_20_직장_인구_수),
                                                             남성연령대_30_직장_인구_수 = mean(연령대_30_직장_인구_수),
                                                             여성연령대_30_직장_인구_수 = mean(연령대_30_직장_인구_수),
                                                             남성연령대_40_직장_인구_수 = mean(연령대_40_직장_인구_수),
                                                             여성연령대_40_직장_인구_수 = mean(연령대_40_직장_인구_수),
                                                             남성연령대_50_직장_인구_수 = mean(연령대_50_직장_인구_수),
                                                             여성연령대_50_직장_인구_수 = mean(연령대_50_직장_인구_수),
                                                             남성연령대_60_직장_인구_수 = mean(연령대_60_이상_직장_인구_수),
                                                             여성연령대_60_직장_인구_수 = mean(연령대_60_이상_직장_인구_수))
work_golmok<-data.frame(work_golmok)
str(work_golmok)
#########################################################people
people <- people[people$상권_구분_코드=='A' & people$기준.년코드==2021,]
str(people)
people_20 <- people[people$연령대_20_생활인구_수!=0,]
people_30 <- people[people$연령대_30_생활인구_수!=0,]
people_40 <- people[people$연령대_40_생활인구_수!=0,]
people_50 <- people[people$연령대_50_생활인구_수!=0,]
people_60 <- people[people$연령대_60_이상_생활인구_수!=0,]


people_tmp<-people[,c(6,7,11:15,71:112,113:154,155:196,197:238,239:280, 323:364, 365:406, 407:448, 449:490, 491:532)]
dim(people_tmp)
str(people_golmok)

tmp<-matrix(nrow=1090,ncol=427)
people_golmok<-data.frame(tmp)
people_golmok[,1]<-unique(around$상권_코드_명)
colnames(people_golmok)<-colnames(people_tmp)


for (place in people_golmok$상권_코드_명){
  for (vari in colnames(people_tmp)[-1]){
    people_golmok[people_golmok$상권_코드_명==place,vari]<-mean(people_tmp[people$상권_코드_명==place,vari])
  }
}
str(people_golmok)

people_golmok[is.na(people_golmok$연령대_20_생활인구_수)==TRUE,"상권_코드_명"]
sum(is.na(people_golmok))

# 연령대별 성별별 생활인구 수
# 남성
i=8
for (age in c(20,30,40,50,60)){
  variname = paste("남성연령대_",age,"대_생활인구_수")
  people_golmok[,variname]<-rowSums(people_golmok[,c(i:(i+41))])
  i=i+42
}
#여성
i=218
for (age in c(20,30,40,50,60)){
  variname = paste("여성연령대_",age,"대_생활인구_수")
  people_golmok[,variname]<-rowSums(people_golmok[,c(i:(i+41))])
  i=i+42
}

colnames(people_golmok)
# 연령대별 시간대별 생활인구 수
i=8
j=218

for (age in c(20,30,40,50,60)){
  time<-c("시간대_1_","시간대_2_","시간대_3_","시간대_4_","시간대_5_","시간대_6_")
  variname = paste(time,rep(age,6),rep("대_생활인구_수",6),sep="")
  
  people_golmok[,variname[1]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+1
  j=j+1
  people_golmok[,variname[2]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+1
  j=j+1
  people_golmok[,variname[3]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+1
  j=j+1
  people_golmok[,variname[4]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+1
  j=j+1
  people_golmok[,variname[5]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+1
  j=j+1
  people_golmok[,variname[6]]<-rowSums(people_golmok[,c(i,i+6,i+6*2,i+6*3,i+6*4,i+6*5,i+6*6,j,j+6,j+6*2,j+6*3,j+6*4,j+6*5,j+6*6)])
  i=i+37
  j=j+37
  
}
dim(people_golmok)
str(people_golmok[,c(438:470)])

# 주중 주말 연령대별 생활인구 수\

i=8
j=218
for (age in c(20,30,40,50,60)){
  variname = paste(c("주중_","주말_"),rep(age,2),rep("대_생활인구_수",),sep="")
  people_golmok[,variname[1]]<-rowSums(people_golmok[,c(i:(i+29),j:(j+29))])
  people_golmok[,variname[2]]<-rowSums(people_golmok[,c((i+30):(i+41),(j+30):(j+41))])
  i=i+42
  j=j+42
}

str(people_golmok)
people_golmok1<-people_golmok[,c(1:7,428:477)]
str(people_golmok1)

# 소득 소비
str(income)
income1 <- income[income$상권_구분_코드=='A' & income$기준.년.코드==2021,]
income1 <- income1[income1$월_평균_소득_금액 !=0,]
income1_golmok<-income1 %>% group_by(상권_코드_명) %>% summarise(월_평균_소득_금액 = mean(월_평균_소득_금액))
income_golmok <-data.frame(income1_golmok)


# 면적별 
str(area)
area1<-area[,2:3]
summary(area1)
# 10000m^2기준으로 진행
area1[,3]<-area[,3]/10000
colnames(area1)[3]<-"면적_10000m^2"
str(area1)

####################데이터 결합
data<-data.frame(matrix(nrow=1090,ncol=2))
data[,1]<-area1$name
colnames(data)[1]<-"name"
str(data)


# 연령대 매출건수비율로 군집화한 업종들의 점포수 변수 추가
for(place in golmok_40_store$상권_코드_명){
  data[data$name==place,"40대점포수"]<-golmok_40_store[golmok_40_store$상권_코드_명==place,"40대점포수"]
}
data<-data[,c(1,3)]

str(golmok_high_store )
for(place in golmok_high_store $상권_코드_명){
  data[data$name==place,"고연령대_점포수"]<-golmok_high_store [golmok_high_store$상권_코드_명==place,"고연령대_점포수"]
}
str(data)
data[is.na(data$연령대_고연령대_중심업종_점포수)==TRUE,"name"]

str(golmok_low_store)
for(place in golmok_low_store$상권_코드_명){
  data[data$name==place,"20대_점포수"]<-golmok_low_store[golmok_low_store $상권_코드_명==place,"20대_점포수"]
}

str(data)
data[is.na(data$연령대_저연령대_중심업종_점포수)==TRUE,"name"]

# 집객시설 변수 
str(around_20)

for(place in around_20$상권_코드_명){
  data[data$name==place,"배후지_지하철_유무"]<-around_20[around_20 $상권_코드_명==place,"지하철"]
  data[data$name==place,"배후지_대학교_유무"]<-around_20[around_20 $상권_코드_명==place,"대학교"]
  data[data$name==place,"배후지_백화점_유무"]<-around_20[around_20 $상권_코드_명==place,"백화점"]
  data[data$name==place,"배후지_의료기관_수"]<-around_20[around_20 $상권_코드_명==place,"의료기관"]
  data[data$name==place,"배후지_관공서_수"]<-around_20[around_20 $상권_코드_명==place,"관공서"]
  data[data$name==place,"상권_버스정류장_수"]<-around_20[around_20 $상권_코드_명==place,"bus"]
}
data$배후지_지하철_유무<-as.factor(data$배후지_지하철_유무)
data$배후지_대학교_유무<-as.factor(data$배후지_대학교_유무)
data$배후지_백화점_유무<-as.factor(data$배후지_백화점_유무)
str(data)

# 상주인구 데이터
str(living_golmok)
for(place in living_golmok$상권_코드_명){
  data[data$name==place,"총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"총_상주인구"]
  data[data$name==place,"연령대_20대_총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"연령대_20_총_상주인구"]
  data[data$name==place,"연령대_30대_총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"연령대_30_총_상주인구"]
  data[data$name==place,"연령대_40대_총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"연령대_40_총_상주인구"]
  data[data$name==place,"연령대_50대_총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"연령대_50_총_상주인구"]
  data[data$name==place,"연령대_60대_이상_총_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"연령대_60_총_상주인구"]
  data[data$name==place,"남성_20대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"남성_20_상주인구"]
  data[data$name==place,"남성_30대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"남성_30_상주인구"]
  data[data$name==place,"남성_40대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"남성_40_상주인구"]
  data[data$name==place,"남성_50대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"남성_50_상주인구"]
  data[data$name==place,"남성_60대_이상_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"남성_60_상주인구"]
  data[data$name==place,"여성_20대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"여성_20_상주인구"]
  data[data$name==place,"여성_30대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"여성_30_상주인구"]
  data[data$name==place,"여성_40대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"여성_40_상주인구"]
  data[data$name==place,"여성_50대_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"여성_50_상주인구"]
  data[data$name==place,"여성_60대_이상_상주인구"]<-living_golmok[living_golmok$상권_코드_명==place,"여성_60_상주인구"]
}
str(data)
data[is.na(data$연령대_30대_총_상주인구)==TRUE,"name"]

# 생활인구 데이터
str(people_golmok1)
for(place in people_golmok1$상권_코드_명){
  data[data$name==place,"연령대_20_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"연령대_20_생활인구_수"]
  data[data$name==place,"연령대_30_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"연령대_30_생활인구_수"]
  data[data$name==place,"연령대_40_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"연령대_40_생활인구_수"]
  data[data$name==place,"연령대_50_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"연령대_50_생활인구_수"]
  data[data$name==place,"연령대_60_이상_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"연령대_60_이상_생활인구_수"]
  
  data[data$name==place,"남성연령대_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"남성연령대_ 20 대_생활인구_수"]
  data[data$name==place,"남성연령대_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"남성연령대_ 30 대_생활인구_수"]
  data[data$name==place,"남성연령대_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"남성연령대_ 40 대_생활인구_수"]
  data[data$name==place,"남성연령대_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"남성연령대_ 50 대_생활인구_수"]
  data[data$name==place,"남성연령대_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"남성연령대_ 60 대_생활인구_수"]
  
  data[data$name==place,"여성연령대_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"여성연령대_ 20 대_생활인구_수"]
  data[data$name==place,"여성연령대_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"여성연령대_ 30 대_생활인구_수"]
  data[data$name==place,"여성연령대_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"여성연령대_ 40 대_생활인구_수"]
  data[data$name==place,"여성연령대_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"여성연령대_ 50 대_생활인구_수"]
  data[data$name==place,"여성연령대_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"여성연령대_ 60 대_생활인구_수"]
  
  data[data$name==place,"시간대_1_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_1_20대_생활인구_수"]
  data[data$name==place,"시간대_2_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_2_20대_생활인구_수"]
  data[data$name==place,"시간대_3_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_3_20대_생활인구_수"]
  data[data$name==place,"시간대_4_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_4_20대_생활인구_수"]
  data[data$name==place,"시간대_5_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_5_20대_생활인구_수"]
  data[data$name==place,"시간대_6_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_6_20대_생활인구_수"]
  
  data[data$name==place,"시간대_1_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_1_30대_생활인구_수"]
  data[data$name==place,"시간대_2_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_2_30대_생활인구_수"]
  data[data$name==place,"시간대_3_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_3_30대_생활인구_수"]
  data[data$name==place,"시간대_4_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_4_30대_생활인구_수"]
  data[data$name==place,"시간대_5_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_5_30대_생활인구_수"]
  data[data$name==place,"시간대_6_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_6_30대_생활인구_수"]
  
  data[data$name==place,"시간대_1_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_1_40대_생활인구_수"]
  data[data$name==place,"시간대_2_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_2_40대_생활인구_수"]
  data[data$name==place,"시간대_3_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_3_40대_생활인구_수"]
  data[data$name==place,"시간대_4_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_4_40대_생활인구_수"]
  data[data$name==place,"시간대_5_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_5_40대_생활인구_수"]
  data[data$name==place,"시간대_6_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_6_40대_생활인구_수"]
  
  data[data$name==place,"시간대_1_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_1_50대_생활인구_수"]
  data[data$name==place,"시간대_2_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_2_50대_생활인구_수"]
  data[data$name==place,"시간대_3_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_3_50대_생활인구_수"]
  data[data$name==place,"시간대_4_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_4_50대_생활인구_수"]
  data[data$name==place,"시간대_5_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_5_50대_생활인구_수"]
  data[data$name==place,"시간대_6_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_6_50대_생활인구_수"]
  
  data[data$name==place,"시간대_1_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_1_60대_생활인구_수"]
  data[data$name==place,"시간대_2_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_2_60대_생활인구_수"]
  data[data$name==place,"시간대_3_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_3_60대_생활인구_수"]
  data[data$name==place,"시간대_4_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_4_60대_생활인구_수"]
  data[data$name==place,"시간대_5_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_5_60대_생활인구_수"]
  data[data$name==place,"시간대_6_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"시간대_6_60대_생활인구_수"]
  
  data[data$name==place,"주중_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주중_20대_생활인구_수"]
  data[data$name==place,"주말_20대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주말_20대_생활인구_수"]
  data[data$name==place,"주중_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주중_30대_생활인구_수"]
  data[data$name==place,"주말_30대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주말_30대_생활인구_수"]
  data[data$name==place,"주중_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주중_40대_생활인구_수"]
  data[data$name==place,"주말_40대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주말_40대_생활인구_수"]
  data[data$name==place,"주중_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주중_50대_생활인구_수"]
  data[data$name==place,"주말_50대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주말_50대_생활인구_수"]
  data[data$name==place,"주중_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주중_60대_생활인구_수"]
  data[data$name==place,"주말_60대_생활인구_수"]<-people_golmok1[people_golmok1$상권_코드_명==place,"주말_60대_생활인구_수"]
  
}
str(data)
summary(area1)
# 면적
str(area1)
for(place in area1$name){
  data[data$name==place,"면적_헥타르"]<-area1[area1$name==place,"면적_10000m^2"]
  data[data$name==place,"면적_제곱미터"]<-area1[area1$name==place,"면적"]
}
str(data)
dim(data)
colnames(data)[c(2:4,11:82)]

# 월평균 소득
str(income_golmok)
for(place in income_golmok$상권_코드_명){
  data[data$name==place,"월_평균_소득금액"]<-income_golmok[income_golmok$상권_코드_명==place,"월_평균_소득_금액"]
}

vari<-paste(rep("면적",length(colnames(data))),colnames(data),sep="_")
for(now_col in c(2:4,11:81)){
  data[,vari[now_col]]<-data[,now_col]/data$면적_제곱미터
  
}
str(data)
dim(data)
dim(data1)




# 종속변수(연령대 매출)
str(price_2021_20_1 )
for(place in price_2021_20_1$상권_코드_명){
  data[data$name==place,"총매출_20대"]<-price_2021_20_1[price_2021_20_1$상권_코드_명==place,"매출액"]
}
str(data[,"총매출_20대"])

for(place in price_2021_30_1 $상권_코드_명){
  data[data$name==place,"총매출_30대"]<-price_2021_30_1 [price_2021_30_1$상권_코드_명==place,"매출액"]
}

for(place in price_2021_40_1$상권_코드_명){
  data[data$name==place,"총매출_40대"]<-price_2021_40_1[price_2021_40_1$상권_코드_명==place,"매출액"]
}

for(place in price_2021_50_1$상권_코드_명){
  data[data$name==place,"총매출_50대"]<-price_2021_50_1[price_2021_50_1$상권_코드_명==place,"매출액"]
}

for(place in price_2021_60_1$상권_코드_명){
  data[data$name==place,"총매출_60대"]<-price_2021_60_1[price_2021_60_1$상권_코드_명==place,"매출액"]
}

# 골목 점포 밀집도
data$점포_밀집도<-rowSums(data[,2:4])/data$면적_제곱미터

# 20제외 저연령대 중심업종 점포수
str(golmok_low_ex20_store)
for(place in golmok_low_ex20_store$상권_코드_명){
  data[data$name==place,"30대_점포수"]<-golmok_low_ex20_store[golmok_low_ex20_store$상권_코드_명==place,"30대_점포수"]
}


# 결측행 제거
data1<-na.omit(data)
summary(data1)
dim(data1)
hist(data1[,2])

par(mfrow=c(6,6))
for(i in colnames(data1)[c(2:4,11:37)]){
  hist(data1[,i])
}
for(i in colnames(data1)[c(38:67)]){
  hist(data1[,i])
}
for(i in colnames(data1)[c(68:97)]){
  hist(data1[,i])
}
for(i in colnames(data1)[c(98:130)]){
  hist(data1[,i])
}
for(i in colnames(data1)[c(131:164)]){
  hist(data1[,i])
}
par(mar=c(1,1,1,1))

dim(data1)
a<-colnames(data1)
write.csv(a,"col.csv")

# 사용변수만 가져오기
data2<-data1[,c(1,165,4,3,2,5:10,27:81,84:87, 104:164)]

##################################################################연령대 데이터분할
str(data2)
data2$총생활인구 <- data2$연령대_20_생활인구_수 + data2$연령대_30_생활인구_수 
data2$남성생활인구 <- data2$남성연령대_20대_생활인구_수+data2$남성연령대_30대_생활인구_수
data2$여성생활인구 <- data2$여성연령대_20대_생활인구_수+data2$여성연령대_30대_생활인구_수
data2$주중생할인구 <- data2$주중_20대_생활인구_수+data2$주중_30대_생활인구_수
data2$주말생할인구 <- data2$주말_20대_생활인구_수+data2$주말_30대_생활인구_수
data2$시간대_1 <- data2$시간대_1_20대_생활인구_수 + data2$시간대_1_30대_생활인구_수
data2$시간대_2 <- data2$시간대_2_20대_생활인구_수 + data2$시간대_2_30대_생활인구_수
data2$시간대_3 <- data2$시간대_3_20대_생활인구_수 + data2$시간대_3_30대_생활인구_수
data2$시간대_4 <- data2$시간대_4_20대_생활인구_수 + data2$시간대_4_30대_생활인구_수
data2$시간대_5 <- data2$시간대_5_20대_생활인구_수 + data2$시간대_5_30대_생활인구_수
data2$시간대_6 <- data2$시간대_6_20대_생활인구_수 + data2$시간대_6_30대_생활인구_수

str(data2[,c(1,12:16,132:142)])
df_2030 <- read.csv('C:/Users/user/Documents/카카오톡 받은 파일/df_2030.csv')

df_2030$name <- df_2030$상권_코드_명
str(data2)
data <- left_join(data2,df_2030,by='name')
df_2030 <- data_2030[,c(1:12,15:21,23,24)]
rm_list=c('부암동주민센터','신내역 2번','방배배수지체육공원','청계산원터골','하나고등학교','홍지문','선사초등학교','청계산입구역 2번(내곡동주민센터)','응암역 3번','충정로역 7번','망리단길','까치산역 3번','화곡역 4번','구립대학경로당(관악산샘말공원)','강동역 4번(강풀만화거리)','관악구 중앙길','샤로수길','사당역 4번')
data=data.frame(data%>%filter(!name%in%rm_list))
str(data)
str(df_2030)
setwd('C:/Users/user/Desktop/OneDrive_2022-07-22')
write.csv(df_2030,'df_2030.csv')

data2$총생활인구 <- data2$연령대_40_생활인구_수 + data2$연령대_50_생활인구_수 +  data2$연령대_60_이상_생활인구_수
data2$남성생활인구 <- data2$남성연령대_40대_생활인구_수+data2$남성연령대_50대_생활인구_수 +  data2$남성연령대_60대_생활인구_수
data2$여성생활인구 <- data2$여성연령대_40대_생활인구_수+data2$여성연령대_50대_생활인구_수 +  data2$여성연령대_60대_생활인구_수
data2$주중생할인구 <- data2$주중_40대_생활인구_수+data2$주중_50대_생활인구_수 +  data2$주중_60대_생활인구_수
data2$주말생할인구 <- data2$주말_40대_생활인구_수+data2$주말_50대_생활인구_수 +  data2$주말_60대_생활인구_수
data2$시간대_1 <- data2$시간대_1_40대_생활인구_수 + data2$시간대_1_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_2 <- data2$시간대_2_40대_생활인구_수 + data2$시간대_2_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_3 <- data2$시간대_3_40대_생활인구_수 + data2$시간대_3_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_4 <- data2$시간대_4_40대_생활인구_수 + data2$시간대_4_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_5 <- data2$시간대_5_40대_생활인구_수 + data2$시간대_5_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_6 <- data2$시간대_6_40대_생활인구_수 + data2$시간대_6_50대_생활인구_수+data2$시간대_1_60대_생활인구_수

str(data2[,c(1,132:142)])
df_4060 <- read.csv('C:/Users/user/Documents/카카오톡 받은 파일/df_4060.csv')
str(df_4060)
df_4060$name <- df_4060$상권_코드_명
df_4060 <- left_join(data2[,c(1,132:142)],df_4060,by='name')
df_4060 <- df_4060[,c(1:12,15:21,23,24)]
rm_list=c('부암동주민센터','신내역 2번','방배배수지체육공원','청계산원터골','하나고등학교','홍지문','선사초등학교','청계산입구역 2번(내곡동주민센터)','응암역 3번','충정로역 7번','망리단길','까치산역 3번','화곡역 4번','구립대학경로당(관악산샘말공원)','강동역 4번(강풀만화거리)','관악구 중앙길','샤로수길','사당역 4번')
df_4060=data.frame(df_4060%>%filter(!name%in%rm_list))
str(df_4060)

write.csv(df_4060,'df_4060.csv')


data2$총생활인구 <- data2$연령대_40_생활인구_수 + data2$연령대_50_생활인구_수 +  data2$연령대_60_이상_생활인구_수
data2$남성생활인구 <- data2$남성연령대_40대_생활인구_수+data2$남성연령대_50대_생활인구_수 +  data2$남성연령대_60대_생활인구_수
data2$여성생활인구 <- data2$여성연령대_40대_생활인구_수+data2$여성연령대_50대_생활인구_수 +  data2$여성연령대_60대_생활인구_수
data2$주중생할인구 <- data2$주중_40대_생활인구_수+data2$주중_50대_생활인구_수 +  data2$주중_60대_생활인구_수
data2$주말생할인구 <- data2$주말_40대_생활인구_수+data2$주말_50대_생활인구_수 +  data2$주말_60대_생활인구_수
data2$시간대_1 <- data2$시간대_1_40대_생활인구_수 + data2$시간대_1_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_2 <- data2$시간대_2_40대_생활인구_수 + data2$시간대_2_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_3 <- data2$시간대_3_40대_생활인구_수 + data2$시간대_3_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_4 <- data2$시간대_4_40대_생활인구_수 + data2$시간대_4_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_5 <- data2$시간대_5_40대_생활인구_수 + data2$시간대_5_50대_생활인구_수+data2$시간대_1_60대_생활인구_수
data2$시간대_6 <- data2$시간대_6_40대_생활인구_수 + data2$시간대_6_50대_생활인구_수+data2$시간대_1_60대_생활인구_수

str(data2[,c(1,132:142)])
df_4060 <- read.csv('C:/Users/user/Documents/카카오톡 받은 파일/df_4060.csv')
str(df_4060)
df_4060$name <- df_4060$상권_코드_명
df_4060 <- left_join(data2[,c(1,132:142)],df_4060,by='name')
df_4060 <- df_4060[,c(1:12,15:21,23,24)]
rm_list=c('부암동주민센터','신내역 2번','방배배수지체육공원','청계산원터골','하나고등학교','홍지문','선사초등학교','청계산입구역 2번(내곡동주민센터)','응암역 3번','충정로역 7번','망리단길','까치산역 3번','화곡역 4번','구립대학경로당(관악산샘말공원)','강동역 4번(강풀만화거리)','관악구 중앙길','샤로수길','사당역 4번')
df_4060=data.frame(df_4060%>%filter(!name%in%rm_list))
str(df_4060)

write.csv(df_4060,'df_4060.csv')

##################################################################20대 모델
df2030<-read.csv('C:/Users/kimpu/Documents/카카오톡 받은 파일/df_2030 (1).csv')
df2030$name <- df2030$상권_코드_명
data2<- left_join(data2,df2030[,c(4,24)],by='name')
str(data2)
data2$X20대_점포수 <- data2$store_2030
#20대
data2 = data2[data2$연령대_20_생활인구_수!=0,]
data2 = data2[data2$연령대_30_생활인구_수!=0,]
data2 = data2[data2$연령대_40_생활인구_수!=0,]
data2 = data2[data2$연령대_50_생활인구_수!=0,]
data2 = data2[data2$시간대_1_20대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_30대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_40대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_50대_생활인구_수!=0,]
data2$배후지_의료기관_수 = data2$배후지_의료기관_수+1
data2$배후지_관공서_수 = data2$배후지_관공서_수+1
data2$상권_버스정류장_수 = data2$상권_버스정류장_수+1

head(data2$연령대_20_생활인구_수)
head(data2$남성연령대_20대_생활인구_수+data2$여성연령대_20대_생활인구_수)
rm_list=c('부암동주민센터','신내역 2번','방배배수지체육공원','청계산원터골','하나고등학교','홍지문','선사초등학교','청계산입구역 2번(내곡동주민센터)','응암역 3번','충정로역 7번','망리단길','까치산역 3번','화곡역 4번','구립대학경로당(관악산샘말공원)','강동역 4번(강풀만화거리)','관악구 중앙길','샤로수길','사당역 4번')
data2=data.frame(data2%>%filter(!name%in%rm_list))
############################################################## 0. 생활인구
lm1_data<-data2[,c(1,3,6:11,12,126,131)]
str(lm1_data)
dim(lm1_data)

## 상관계수
cor(lm1_data[,-c(2:4,8:10)])
summary(lm1_data)
### log변환
for(i in colnames(lm1_data)[-c(1,3:5)]){
  lm1_data[,i]<-log(lm1_data[,i])
}
summary(lm1_data)
#### 회귀
lm0<-lm(총매출_20대~.,data=lm1_data[,-1])
summary(lm0)
str(lm1_data)
par(mfrow=c(2,2))
plot(lm0)
vif(lm00)
lm00<-lm(총매출_20대~.,data=lm1_data[,-c(1,3,5,6,7)])
summary(lm00)
par(mfrow=c(2,2))
plot(lm00)
"
결과:  0.6516
"
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
########################################################################## 최종모델선정
########################################################################## 
################################# dfbeta로 이상치 있는 x확인
par(mfrow=c(1,1))
plot(abs(dfbetas(lm1)[,'연령대_저연령대_중심업종_점포수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'배후지_지하철_유무1']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'배후지_대학교_유무1']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'배후지_관공서_수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'점포_밀집도']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'배후지_의료기관_수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'연령대_20_생활인구_수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas(lm1)[,'상권_버스정류장_수']),pch=23,bg='orange',cex=2)
################################# 군집화
par(mar=c(4,4,4,4))

df<-data2[,c(1,3,7,11,12,131,126)]
str(df)
pc1 <- prcomp(df[,-c(1,3,7)],scale=T)
summary(pc1)
pc1$rotation
par(mfrow=c(1,2))
pc_data <- pc1$x[,1:2]
pc_no <- df[,-c(1,3,7)]
wss <- 0
for(i in 1:10) wss[i]<-sum(kmeans(pc_data,centers = i)$withinss)
plot(1:10, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")
#silhouette
km_cnt <- kmeans(pc_data,4)  #4개가 제일 잘나옴 
distance <- dist(pc_data, method= "euclidean")
sil = silhouette (km_cnt$cluster, distance)
plot(sil)
km_cnt$centers
km_cnt$size
par(mfrow=c(1,1))
df$cl<-km_cnt$cluster
str(df)
write.csv(df,'df20.csv')
boxplot(df[df$cl==1,"총매출_20대"],df[df$cl==2,"총매출_20대"],df[df$cl==3,"총매출_20대"],
        df[df$cl==4,"총매출_20대"])
df[df$cl==1,'name']
df4 <- df[df$cl==1,]
school <- df[df$배후지_대학교_유무==1,]
noschool <- df[df$배후지_대학교_유무==0,]
fviz_cluster(km_cnt,pc_data) 
par(mfrow=c(1,2))
boxplot(school[school$cl==1,"총매출_20대"],school[school$cl==2,"총매출_20대"],school[school$cl==3,"총매출_20대"],
        school[school$cl==4,"총매출_20대"],school[school$cl==5,"총매출_20대"])
boxplot(noschool[noschool$cl==1,"총매출_20대"],noschool[noschool$cl==2,"총매출_20대"],noschool[noschool$cl==3,"총매출_20대"],
        noschool[noschool$cl==4,"총매출_20대"],noschool[noschool$cl==5,"총매출_20대"])

##################################################################30대 모델
data2<-data1[,c(1,165,4,3,2,5:10,27:81,84:87, 104:164)]
df2030<-read.csv('C:/Users/kimpu/Documents/카카오톡 받은 파일/df_2030 (1).csv')
df2030$name <- df2030$상권_코드_명
data2<- left_join(data2,df2030[,c(4,24)],by='name')
str(data2)
data2$X20대_점포수 <- data2$store_2030
data2 = data2[data2$연령대_20_생활인구_수!=0,]
data2 = data2[data2$연령대_30_생활인구_수!=0,]
data2 = data2[data2$연령대_40_생활인구_수!=0,]
data2 = data2[data2$연령대_50_생활인구_수!=0,]
data2 = data2[data2$시간대_1_20대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_30대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_40대_생활인구_수!=0,]
data2 = data2[data2$시간대_1_50대_생활인구_수!=0,]
data2$배후지_의료기관_수 = data2$배후지_의료기관_수+1
data2$배후지_관공서_수 = data2$배후지_관공서_수+1
data2$상권_버스정류장_수 = data2$상권_버스정류장_수+1
data2$X20대_점포수 = data2$X20대_점포수+1
data2$store_2030 <- data2$store_2030+1
############################################################## 0. 총 생활인구
str(data2)
View(data2)
lm3_data<-data2[,c(1,133,6:11,13,127,131)]
rm_list=c('부암동주민센터','신내역 2번','방배배수지체육공원','청계산원터골','하나고등학교','홍지문','선사초등학교','청계산입구역 2번(내곡동주민센터)','응암역 3번','충정로역 7번','망리단길','까치산역 3번','화곡역 4번','구립대학경로당(관악산샘말공원)','강동역 4번(강풀만화거리)','관악구 중앙길','샤로수길','사당역 4번')
lm3_data=data.frame(lm3_data%>%filter(!name%in%rm_list))
str(lm3_data)
summary(lm3_data)
## 상관계수
cor(lm3_data[,c(2,6:11)])
plot(lm3_data[,c(2,6:11)])
(lm0_data)

### log변환
for(i in colnames(lm3_data)[-c(1,3:5)]){
  lm3_data[,i]<-log(lm3_data[,i])
}
summary(lm3_data)
cor(lm3_data[,c(2,6:11)])

# 시각화
par(mfrow=c(3,3))
for(i in colnames(lm3_data)[-c(1,3:5)]){
  hist(lm3_data[,i],main=i)
}

#### 회귀
lm0<-lm(총매출_30대~.,data=lm3_data[,-1])
summary(lm0)
par(mfrow=c(2,2))
plot(lm0)
"
결과:  0.6399
"
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
########################################################################## 최종모델선정
########################################################################## 
################################# dfbeta로 이상치 있는 x확인
par(mfrow=c(4,4))
plot(abs(dfbetas()[,'연령대_저연령대_중심업종_점포수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'배후지_지하철_유무1']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'배후지_대학교_유무1']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'배후지_관공서_수']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'면적_제곱미터']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'점포_밀집도']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'면적_남성_20대_상주인구']),pch=23,bg='orange',cex=2)
plot(abs(dfbetas()[,'면적_여성_20대_상주인구']),pch=23,bg='orange',cex=2)

################################# 군집화
par(mfrow=c(1,1))
par(mar=c(4,4,4,4))
str(lm3_data)
pc1 <- prcomp(lm3_data[,-c(1,5)],scale=T)
summary(pc1)
pc1
pc_data <- pc1$x[,1:2]
wss <- 0
for(i in 1:10) wss[i]<-sum(kmeans(pc_data,centers = i)$withinss)
plot(1:10, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")
#silhouette
km_cnt <- kmeans(pc_data,4)  #4개가 제일 잘나옴 
distance <- dist(pc_data, method= "euclidean")
sil = silhouette (km_cnt$cluster, distance)
plot(sil)
km_cnt$centers
km_cnt$size
par(mfrow=c(1,1))
lm3_data$cl<-km_cnt$cluster
boxplot(lm3_data[lm3_data$cl==1,"총매출_30대"],lm3_data[lm3_data$cl==2,"총매출_30대"],
        lm3_data[lm3_data$cl==3,"총매출_30대"],lm3_data[lm3_data$cl==4,"총매출_30대"])
lm3_data3 <- lm3_data[lm3_data$cl==1,]
dim(lm3_data3)

##################################################################군집해석
dim(df4) #452
dim(lm3_data3) #460

old <- read.csv('C:/Users/kimpu/Documents/카카오톡 받은 파일/sample_df.csv')
old2 <- read.csv('C:/Users/kimpu/Documents/카카오톡 받은 파일/sample_df2.csv')
old2=old2[,2:11]
old=old[,2:11]
old2$name <- old2$상권_코드_명
old$name <- old$상권_코드_명
dim(old) #412
dim(old2) #412
#20대기준
diff2030 <- setdiff(df4['name'],lm3_data3['name'])
diff2040 <- setdiff(df4['name'],old2['name'])
diff20 <- intersect(diff2030['name'],diff2040['name'])
#30대기준
diff3020 <- setdiff(lm3_data3['name'],df4['name'])
diff3040 <- setdiff(lm3_data3['name'],old2['name'])
diff30 <- intersect(diff3020['name'],diff3040['name'])
#40대기준
diff4020 <- setdiff(old2['name'],df4['name'])
diff4030 <- setdiff(old2['name'],lm3_data3['name'])
diff40 <- intersect(diff4020['name'],diff4030['name'])
#교집합
inter2030 <- intersect(df4['name'],lm3_data3['name'])
inter2040 <- intersect(df4['name'],old2['name'])
inter3040 <- intersect(lm3_data3['name'],old2['name'])
inter203040 <- intersect(inter2030['name'],inter2040['name'])

diff20
diff30
diff40
dim(diff40)
dim(inter203040)


dim(intersect(sample['name'],lm3_data3['name'])) #324 412&460 80개빠짐
inter3060 <- intersect(sample['name'],lm3_data3['name'])

#30대와 405060대 하집단의 차집합
dim(setdiff(sample['name'],lm3_data3['name']))# 88개지역
#405060대 30대와 하집단의 차집합
dim(setdiff(lm3_data3['name'],sample['name']))# 136개지역
setdif3060 <-union(setdiff(lm3_data3['name'],sample['name']),setdiff(sample['name'],lm3_data3['name']))
dim(setdif3060)

#학교 0 X
dim(intersect(noschool4['name'],inter3060['name'])) #223 324&322
dim(intersect(school4['name'],inter3060['name'])) #94 130&322

#지하철 0
dim(intersect(subway['name'],lm3_data3['name'])) #153
subway3060 <- intersect(subway['name'],lm3_data3['name'])
#지하철 X
dim(intersect(nosubway['name'],lm3_data3['name'])) #171
nosubway3060 <- intersect(nosubway['name'],lm3_data3['name'])

#지하철0 학교0
dim(intersect(school4['name'],subway3060['name'])) #39
#지하철0 학교X
dim(intersect(noschool4['name'],subway3060['name'])) #108
#지하철X 학교0
dim(intersect(school4['name'],nosubway3060['name'])) #55
#지하철X 학교X
dim(intersect(noschool4['name'],nosubway3060['name'])) #115


#######################################
View(price)
price_age <- price[price$기준_년_코드==2021 ,c(6,75:79)]
str(price_age)

diff20
diff30
write.csv(diff40,'diff40.csv')
dim(diff40)
dim(inter203040)

colSums(price_age[price_age$상권_코드_명=='증산역 1번',2:5])
colSums(price_age[price_age$상권_코드_명=='천호지구대(천호동자전거거리)',2:5])
colSums(price_age[price_age$상권_코드_명=='천호공원(해공체육문화센터)',2:5])
colSums(price_age[price_age$상권_코드_명=='천호초등학교',2:5])

colSums(price_age[price_age$상권_코드_명=='용두희망어린이공원',2:5]) 1
colSums(price_age[price_age$상권_코드_명=='보문역 4번',2:5]) 1
colSums(price_age[price_age$상권_코드_명=='북한산우이역 2번',2:5])
colSums(price_age[price_age$상권_코드_명=='인수동 우편취급국',2:5])
colSums(price_age[price_age$상권_코드_명=='연신내역 5번',2:5])
colSums(price_age[price_age$상권_코드_명=='홍릉시장',2:5])
colSums(price_age[price_age$상권_코드_명=='월드순복음교회',2:5])
colSums(price_age[price_age$상권_코드_명=='하나은행 망우동지점',2:5])
colSums(price_age[price_age$상권_코드_명=='러스크서울병원',2:5])
colSums(price_age[price_age$상권_코드_명=='북한산보국문역 2번',2:5]) 1
colSums(price_age[price_age$상권_코드_명=='보문역 4번',2:5])
colSums(price_age[price_age$상권_코드_명=='가오리역 1번',2:5])
colSums(price_age[price_age$상권_코드_명=='인수동 우편취급국',2:5])
colSums(price_age[price_age$상권_코드_명=='강북중학교',2:5])
colSums(price_age[price_age$상권_코드_명=='대림성모병원',2:5])
colSums(price_age[price_age$상권_코드_명=='석계역 2번',2:5])
colSums(price_age[price_age$상권_코드_명=='호림박물관(성보고등학교)',2:5])
colSums(price_age[price_age$상권_코드_명=='신림우방아파트',2:5])
colSums(price_age[price_age$상권_코드_명=='은곡마을(은곡마을공원)',2:5])

colSums(price_age[price_age$상권_코드_명=='서울숲카페거리',2:5])
colSums(price_age[price_age$상권_코드_명=='서울대입구역 8번',2:5])
colSums(price_age[price_age$상권_코드_명=='서울대병원',2:5])
colSums(price_age[price_age$상권_코드_명=='금천구립가산도서관',2:5])
colSums(price_age[price_age$상권_코드_명=='대광초등학교',2:5])
colSums(price_age[price_age$상권_코드_명=='뚝섬역교차로',2:5])
colSums(price_age[price_age$상권_코드_명=='연희초등학교',2:5])
