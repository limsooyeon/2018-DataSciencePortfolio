#### load data
setwd("C:/Users/xnote/Downloads/death")

# load data from 2009
filelist = choose.files()
datalist =lapply(filelist, FUN=read.table, sep="\t")
from2009 = do.call("rbind", datalist)
names(from2009) = c("신고_년", "신고_월", "신고_일", "주소지", "성별", "사망_년", "사망_월", "사망_일", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "사망원인1", "사망원인2", "사망연령")

# 2010~2013 data
filelist = choose.files()
datalist =lapply(filelist, FUN=read.table, sep="\t")
data10to13 = do.call("rbind", datalist) 
names(data10to13) = c("신고_년", "신고_월", "신고_일", "주소지", "성별", "사망_년", "사망_월", "사망_일", "사망시간", "사망장소", "사망자직업", "혼인상태", "교육정도", "사망원인1", "사망원인2", "사망연령", "국적구분", "이전국적명")

# 2014~2016 data
filelist = choose.files()
datalist =lapply(filelist, FUN=read.table, sep="\t")
data14to16 = do.call("rbind", datalist) 
names(data14to16) = c("신고_년", "신고_월", "신고_일", "주소지", "성별", "사망_년", "사망_월", "사망_일", "사망시간", "사망연령", "사망장소", "사망자직업", "혼인상태", "교육정도", "사망원인1", "사망원인2", "국적구분", "이전국적명", "사망원인 103항목 분류", "사망원인 56항목 분류")

#data set
library(gtools)
death = smartbind(from2009, data10to13)
death = smartbind(death, data14to16)

#사망원인-자살 추출
num<-600:849
num<-paste0("X",num)
suicide<-data.frame()
for(i in 1:length(num)){
  death_cond = death[death$사망원인2==num[i],]
  suicide<-rbind(suicide,death_cond)
}




####데이터 파악
#총 데이터 개수(사망 건수)
str(death)
summary(death)
Dcount <- nrow(death)
#자살 데이터 개수(자살 건수)
str(suicide)
summary(suicide)
Scount <- nrow(suicide)




####분석

##자살률
#자살률 계산
pop <- read.csv("연앙인구.csv", stringsAsFactor=FALSE)
pop1 <- t(pop[,c(-2,-3,-4)])
sumpop <- c(pop1[seq(2,16,2),]+pop1[seq(3,17,2),])
sumpop <- matrix(sumpop,nrow =8, ncol = 15, byrow = F)
sumpop <- rbind(sumpop,colSums(pop1[15:nrow(pop1),]))
colnames(sumpop) <- c(2002:2016)
age = c("10대 미만", "10대", "20대", "30대", "40대", "50대", "60대", "70대", "80대 이상","나이 미상")
rownames(sumpop) <- age[1:9]
sumpop

#연령대별 범주화
suicide$사망연령 = as.numeric(suicide$사망연령)
table(suicide$사망연령)
suicide$group = cut(suicide$사망연령, c(-Inf,9,19,29,39,49,59,69,79,109,Inf),
                    labels=age)


#우리나라 자살률
year = 0
k<-matrix(nrow=10, ncol = 15)
for(i in 1:15){
  year = subset(suicide, suicide$사망_년==2001+i, drop=F)
  year = table(year$group)
  for(j in 1:10)
    k[j,i] <- as.numeric(year[[j]])
}
year_age = k
year_age[9,] = year_age[9,] + year_age[10,]
year_age = year_age[-10,]
colnames(year_age)<-c(2002:2016)
rownames(year_age)<-age[1:9]
for (i in 1:9){
  year_age[i,] = year_age[i,]*100000/sumpop[i, ] 
}

year_sum = colSums(k)
pop2 <- t(pop[,c(-2,-3)])
year_death_per = as.data.frame(year_sum*100000/pop2[2,])
years = c(2002:2016)
year_death_per = cbind(years, year_death_per)
colnames(year_death_per) = c("year", "rate")

library(ggplot2)
ggplot(year_death_per, aes(x=year,y=rate))+
  geom_line()+
  geom_point(size=2) +
  ggtitle("자살률") +labs(list(x="연도", y="자살률"))+
  geom_text(aes(label=round(rate,1)), size=5, vjust=-0.8)


#oecd 자살률과 비교
oecd = read.csv("C:/Users/xnote/Downloads/suicide_oecd.csv", header=TRUE, sep='\t')
oecd_time = oecd[oecd$TIME>=1985,]
oecd_tot = oecd_time[oecd_time$SUBJECT=="TOT",]
oecd_loca = oecd_tot[oecd_tot$LOCATION=="KOR"|
                       oecd_tot$LOCATION=="JPN"|
                       oecd_tot$LOCATION=="HUN"|
                       oecd_tot$LOCATION=="FIN"|
                       oecd_tot$LOCATION=="USA",]

ggplot(oecd_loca, aes(x=TIME, y=Value, colour=LOCATION, group=LOCATION)) +
  geom_line(size=1.1) +
  geom_point(size=1.5) +
  labs(list(title="OECD국가 자살률", x="연도", y="자살률", colour="나라"))+
  geom_text(aes(label=Value), size=3, vjust=-1, show.legend=FALSE)+
  geom_vline(xintercept=2002, linetype='dotted', color='black', size=2)



# 자살 날짜별 빈도
url="https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R" 
source(url)

g2r <- rev(c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") )

suicide$사망날짜 = paste(suicide$사망_년, suicide$사망_월, suicide$사망_일, sep="-")
count = table(suicide$사망날짜)
calendarHeat(dates=rownames(count), values=count, color="g2r", varname="2002~2016 자살건수")


#연령대별 자살률
library("RColorBrewer")
color = brewer.pal(10, "Paired")
plot.new()
par(mfrow=c(1,1),mar=c(1,4.1,0,4.1))
plot.new()
legend("top", ncol=5,inset=.05, legend=rownames(year_age),pch=16, col = color)
par(new=T)
par(mfrow=c(1,1),mar=c(5.1,4.1,5.1,2.1))
matplot(colnames(year_age),t(year_age),lwd=3 , xlim = c(2002,2016),ylim = c(0,100),xlab='Years', ylab='rate',col =color, type = "l",lty = 1 )


#자살 방법 범주화
suicide$사망원인2 = droplevels(suicide$사망원인2)
suicide$사망원인2 = substr(suicide$사망원인2, 1, 3)
suicide$사망원인2 = as.factor(suicide$사망원인2)

levels(suicide$사망원인2) = list("기타 약물"="X60", "수면제"="X61", "마약 및 환각제"="X62", "기타 약물"="X63", "기타 약물"="X64",
                             "알콜"="X65", "휘발물질"="X66", "가스"="X67", "농약"="X68", "기타 독성물질"="X69",
                             "목맴"="X70", "익사"="X71", "권총발사"="X72", "기타 화기발사"="X73", "기타 화기발사"="X74", 
                             "폭발성 물질"="X75", "불"="X76", "증기"="X77", "예리한 물체"="X78", "둔한 물체"="X79",
                             "추락"="X80", "충돌"="X81", "충돌"="X82", "기타"="X83", "상세불명"="X84")

library(forcats)
library(dplyr)

#자살 방법 빈도
sort(table(suicide$사망원인2), decreasing=TRUE)
#상위 10개 항목
sui = c("목맴","농약","추락","가스","기타 독성물질","익사","상세불명", "예리한 물체","불","수면제")
suicide10 = suicide[suicide$사망원인2==sui,]


#시간별
Stime = subset(suicide10, suicide10$사망시간!=99)
Stimefreq = table(Stime$사망시간)
ggplot(Stime, aes(x=사망시간)) +
  geom_line(stat='count') +
  labs(list(title="시간대별 자살 빈도", x="시간", y="빈도"))+
  geom_vline(xintercept=c(9.5,16.5), linetype='dotted', color='red', size=2)


#사망장소 분포
table(death$사망장소)
table(suicide$사망장소)
#99결측치를 11로 수정 99결측치는 장소미상인 것들임.
death$사망장소<-replace(death$사망장소, death$사망장소==99, 11)
suicide$사망장소<-replace(suicide$사망장소, suicide$사망장소==99, 11)

Splace_freq<-table(suicide$사망장소)
Splace_per<-round(Splace_freq/sum(Splace_freq)*100,1)
place<-c("주택","의료기관","사회복지시설","공공시설","도로","상업,서비스시설","산업장","농장","병원이송중","기타","미상")

Splace_lbs<-paste0(Splace_per,"%")

pie(Splace_freq,labels=Splace_lbs,cex.main=3,main="장소",init.angle=90, radius=1,col=rainbow(11))
legend(x="bottom", legend=place, fill=rainbow(11),cex=1, ncol=2)



#자살방법
ggplot(data=subset(suicide10, !is.na(사망원인2)), aes(fct_rev(fct_infreq(사망원인2, ordered=TRUE))))+
  geom_bar(stat='count')+
  labs(list(x="자살 방법", y="빈도")) +
  coord_flip()


#연령대별 데이터 분류
s00 = subset(suicide, group=="10대 미만")
s10 = subset(suicide, group=="10대")
s20 = subset(suicide, group=="20대")
s30 = subset(suicide, group=="30대")
s40 = subset(suicide, group=="40대")
s50 = subset(suicide, group=="50대")
s60 = subset(suicide, group=="60대")
s70 = subset(suicide, group=="70대")
s80 = subset(suicide, group=="80대 이상")
sNA = subset(suicide, group=="나이 미상")

# 연령별 - 성별
sexM <- c(nrow(s00[s00$성별==1,]), nrow(s10[s10$성별==1,]),
          nrow(s20[s20$성별==1,]), nrow(s30[s30$성별==1,]),
          nrow(s40[s40$성별==1,]), nrow(s50[s50$성별==1,]),
          nrow(s60[s60$성별==1,]), nrow(s70[s70$성별==1,]),
          nrow(s80[s80$성별==1,]), nrow(sNA[sNA$성별==1,]))

sexF <- c(nrow(s00[s00$성별==2,]), nrow(s10[s10$성별==2,]),
          nrow(s20[s20$성별==2,]), nrow(s30[s30$성별==2,]),
          nrow(s40[s40$성별==2,]), nrow(s50[s50$성별==2,]),
          nrow(s60[s60$성별==2,]), nrow(s70[s70$성별==2,]),
          nrow(s80[s80$성별==2,]), nrow(sNA[sNA$성별==2,]))

library(pyramid)
library(readxl)

Ssex<- data.frame(sexM,sexF,age)
pyramid::pyramid(Ssex,main="2002~2016년 성별 및 연령대 자살 현황",Laxis=seq(0,30000,len=5),Cstep=1,Cgap = .4)


#성별
suicide$성별 = as.factor(suicide$성별)
levels(suicide$성별) = list("남자"=1, "여자"=2)

ggplot(data=subset(suicide10, !is.na(사망원인2)), aes(fct_infreq(사망원인2), fill=factor(성별), group=성별)) + 
  geom_bar() + ggtitle("성별 자살방법") +labs(list(x="자살 방법", y="빈도", fill="성별"))+
  scale_fill_manual(values = c( "#99CCCC", "#FF9999"))


#연령대별 자살방법
s00_freq<-table(s00$사망원인2)
s00_per<-round(s00_freq*100/sum(s00_freq),1)
s00_lbs<- paste(s00_per, "%", sep="")
s10_freq<-table(s10$사망원인2)
s10_per<-round(s10_freq*100/sum(s10_freq),1)
s10_lbs<- paste(s10_per, "%", sep="")
s20_freq<-table(s20$사망원인2)
s20_per<-round(s20_freq*100/sum(s20_freq),1)
s20_lbs<- paste(s20_per, "%", sep="")
s30_freq<-table(s30$사망원인2)
s30_per<-round(s30_freq*100/sum(s30_freq),1)
s30_lbs<- paste(s30_per, "%", sep="")
s40_freq<-table(s40$사망원인2)
s40_per<-round(s40_freq*100/sum(s40_freq),1)
s40_lbs<- paste(s40_per, "%", sep="")
s50_freq<-table(s50$사망원인2)
s50_per<-round(s50_freq*100/sum(s50_freq),1)
s50_lbs<- paste(s50_per, "%", sep="")
s60_freq<-table(s60$사망원인2)
s60_per<-round(s60_freq*100/sum(s60_freq),1)
s60_lbs<- paste(s60_per, "%", sep="")
s70_freq<-table(s70$사망원인2)
s70_per<-round(s70_freq*100/sum(s70_freq),1)
s70_lbs<- paste(s70_per, "%", sep="")
s80_freq<-table(s80$사망원인2)
s80_per<-round(s80_freq*100/sum(s80_freq),1)
s80_lbs<- paste(s80_per, "%", sep="")

library(randomcoloR)
a<-randomColor(21)
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1),oma=c(0,0,0,0))
plot.new()

par(mfrow=c(3,3),mar=c(5.1, 4.1, 4.1, 2.1),oma=c(0,0,0,15))

freq00 = table(s00$사망원인2)
pie(freq00, labels=s00_lbs, main="10대 미만", radius=1, init.angle=90,col=a)
freq10 = table(s10$사망원인2)
pie(freq10, labels=s10_lbs, main="10대", radius=1, init.angle=90,col=a)
freq20 = table(s20$사망원인2)
pie(freq20, labels=s20_lbs, main="20대", radius=1, init.angle=90,col=a)
freq30 = table(s30$사망원인2)
pie(freq30, labels=s30_lbs, main="30대", radius=1, init.angle=90,col=a)
freq40 = table(s40$사망원인2)
pie(freq40, labels=s40_lbs, main="40대", radius=1, init.angle=90,col=a)
freq50 = table(s50$사망원인2)
pie(freq50, labels=s50_lbs, main="50대", radius=1, init.angle=90,col=a)
freq60 = table(s60$사망원인2)
pie(freq60, labels=s60_lbs, main="60대", radius=1, init.angle=90,col=a)
freq70 = table(s70$사망원인2)
pie(freq70, labels=s70_lbs, main="70대", radius=1, init.angle=90,col=a)
freq80 = table(s80$사망원인2)
pie(freq80, labels=s80_lbs, main="80대 이상", radius=1, init.angle=90,col=a)


par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 0.2),oma=c(0,0,0,0))
par(new=T)
lbsu<-dimnames(freq00)[[1]]
legend("right",lbsu,fill=a)
