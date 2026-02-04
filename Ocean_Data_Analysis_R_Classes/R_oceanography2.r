# svinoume o,ti mporei na yparhei sti mnimi
# poly simantiko os energeia pou prepei na pragmatopoiithei,
# an trehoume polla programata horis na ehoume "kleisei" 
# kai "xana-anoixei" tin R
rm(list=ls())

# orismos thesis arheiou ergasias: pou vriskontai ta dedomena
# (p.h. os thesi arheiou) pou tha anoixo na diavaso kai pou 
# tha grapso an prokeitai na soso ena arheio.
#setwd("D:/Work/Current/Education/Patras/Met_Oceanography_Data_Analysis/day3_data_analysis_in_R/")

# diavasma tou arheiou kai metatropi tou se pinaka
fishdata <- read.csv("Citharus.csv", header = TRUE, sep=",",stringsAsFactors=T)

# emfanisi periehomenon tou pinaka stin othoni
fishdata
View(fishdata)
summary(fishdata)

# mi kai ehoume milisei gia ti vivliothiki 'tidyverse', na mia alli hrisimi 
# entoli apeikonisis ton periehomenon tis klassis fishdata, pou omos gia na
# tin "trexoume" prepei na fortosoume prota ti vivliothiki
library(tidyverse)
glimpse(fishdata)

# anafora se stoiheia tou pinaka, gia paradeigma se stiles
# kathe stili mporei na anaferetai os tmima tou antikeimenou 
# me to dollario
fishdata$TotalLength
fishdata$Sex
fishdata$aa_protocol

# mporo na anafertho me ti dieuthinsi se sygkekrimenes stiles
fishdata$FishingGear
fishdata[,7]

# ...i se seires (p.h. proti i pempti seira)
fishdata[1,]
fishdata[5,]
fishdata[3,]

# ...i kai se sygkekrimena stoiheia (deuteri seira&pempti stili)
fishdata[2,5]

# metatropi stoiheion se anexartites metavlites
aa_protocol   <-  fishdata$aa_protocol               
aa_fish       <-  fishdata$aa_fish                   
day           <-  fishdata$Day
month         <-  fishdata$Month
year          <-  fishdata$Year
gear          <-  fishdata$FishingGear
substrate     <-  fishdata$Substrate
depth         <-  fishdata$Depth
TL            <-  fishdata$TotalLength
SL            <-  fishdata$StandardLength
TW            <-  fishdata$TotalWeight
GW            <-  fishdata$GonadWeight
LW            <-  fishdata$LiverWeight
sex           <-  fishdata$Sex
stage         <-  fishdata$GonadStage
age           <-  fishdata$Age


# pleon ehoume metatrepsei se metavlites...
age
year
substrate
LW

# kai mporoume na kanoume ena aplo diagramma
plot(GW)

# i na doume ta epipeda mias metavlitis
levels(sex)
levels(substrate)

# oi metavlites mporoun na hrisimopoiithoun se aplous ypologismous
# p.h. metrisi tou mikous pou einai metrimeno se mm e metrisi se cm
TL
TLcm  <-  TL/10
TLcm

# ehoume to vathos se orgies, theloume na to metatrepsoume se metra
# i orgia einai enas grigoros tropos metrisis (praktika antistoihei sto anoigma 
# ton herion mas) kai kata symvasi antistoihei se 1.82 metra
depth_m <-  depth/1.82

# mporei na thelo auto to stoiheio stin klasi pou periehei ta dedomena mou
fishdata$Depth_m <- fishdata$Depth/1.82

fishdata$GSI  <- fishdata$GonadWeight/fishdata$TotalWeight*100
fishdata$HSI  <- fishdata$LiverWeight/fishdata$TotalWeight*100

# ston pinaka tis klasis ehei prostethei pleon kai to vathos se metra kai ta GSI, HSI
fishdata

# aploi ypologismoi
min(TL)
min(TL,na.rm=TRUE)
max(TL,na.rm=TRUE)
mean(TL,na.rm=TRUE)
median(TL,na.rm=TRUE)
sd(TL,na.rm=TRUE)

# dimiourgia aplou diagrammatos x~y
plot(TW~TL)

# mporoume na morfopoiisoume ta diagrammata mas
plot(TW~TL, pch=3)
plot(TW~TL, pch=1,col=6)

# se opoiodipote simeio mporoume na psaxoume gia voitheia me ? i me help()
?plot

# mporoume na epilexoume diaforetikous typous diagrammaton
plot(age)
barplot(age)

# ...i na kanoume diagrammata syhnotiton
hist(age)
hist(month)

# sta diagrammata mporoume na kathorisoume idiotites, opos p.h. ton arithmo klaseon
hist(month,nclass=12,col=2)

# mporoume na exetasoume aples ypotheseis...
# p.h. i fishbase (www.fishbase.org) dinei os "Common length" gia to eidos ta 15 cm

commonlength  <-  150
mesomikos     <-  mean(TL,na.rm = TRUE)

# kanoume mia proti ypothesi...
char <-  "The sample mean length is lower than the global common length"

char

# ...kai tin eleghoume
if(mesomikos > commonlength) 
  char <-  "The sample mean length is higher than the global common length"

char

# posa stoiheia apo mia metavliti yparhoun?
length(TL)

# poies einai oi diastaseis tou pinaka mas?
dim(fishdata)

# kai poia einai ta onomata ton stoiheion tou pinaka mas?
dimnames(fishdata)

# poia einai ta epipeda ton poiotikon metavliton?
levels(fishdata$Vessel)

# kai posa einai auta ta epipeda?
length(levels(fishdata$Vessel))


# kai to idio gia tis alles metavlites...
levels(fishdata$Sex)
length(levels(fishdata$Sex))

# mporoume na kanoume kai apla 'statistika' diagrammata
boxplot(TL~sex, data=fishdata)

# panta ehontas ti dynatotita na ta morfopoiisoume
boxplot(TL~sex, data=fishdata, col=3)
boxplot(TW~sex, data=fishdata, col=5)
boxplot(TW~sex, data=fishdata, col=5)

# epistrefontas sti shesi ypsous me varos
plot(varos~ypsos, pch=3)

# giana doume an oi dyo metavlites ehoun grammiki shesi
linmod<-lm(TW~TL)

summary(linmod)

anova(linmod)
plot(linmod)


# mporoume na kanoume aplous (i polyplokous) statistikous eleghous
statcheck <- aov(fishdata$TotalLength~fishdata$Sex)
summary(statcheck)
anova(statcheck)
plot(statcheck)

# to apotelesma...
statcheck

# kai fysika mporoume na kanoume eleghous parametrikotitas
# ta parametrika test opos i ANOVA ehoun paradohes:
#       1. ta dedomena proerhontai apokanoniki katanomi
#       2. ta dedomena emfanizoun omoiogeneia diasporas sta diafora epipeda 
#          poy thelo na do diafores

# eleghos kanonikotitas
prereq1 <-  shapiro.test(fishdata$TotalLength)
prereq1

# eleghos omoiogeneias tis diasporas
prereq2 <- bartlett.test(fishdata$TotalLength ~ fishdata$Sex)
prereq2

# mporei omos na thelo kapoio kalytero statistiko elegho
# gia paradeigma to Levene's test einai pio eurosto 
# ston elegho tis omoiogeneias tis diasporas apo to Bartlett's,
# idiaitera an den eho kanonikotita stin katanomi teinoume na to protimame

# prosohi!!!! kapoia test (kai pio periplokoi statistikoi eleghoi) 
# den yparhoun stin R "by default" (syhna anaferetai os 'R-base') kai prepei na tous egkatastisoume...

# otan egkatastisoume tin antistoihi vivliothiki, ti "fortonoume"
# install.packages("car")
library(car)

prereq2 <- leveneTest(fishdata$TotalLength ~ fishdata$Sex)
prereq2

# merikoi alloi statistikoi eleghoi, analoga me ta erotimata pou ehoume...
statcheck2 <- aov(fishdata$GSI~fishdata$Sex)
summary(statcheck2)
anova(statcheck2)
plot(statcheck2)
boxplot(fishdata$GSI~fishdata$Sex)

statcheck3 <- aov(fishdata$GSI~as.factor(fishdata$Month))
summary(statcheck3)
anova(statcheck3)
plot(statcheck3)
boxplot(fishdata$GSI~as.factor(fishdata$Month))
