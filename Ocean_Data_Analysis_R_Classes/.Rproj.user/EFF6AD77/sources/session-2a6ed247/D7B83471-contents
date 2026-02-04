###########################
######## 1. INTRO #########
###########################


a<- -7

# Svinoume o,tidipote mporei na yparhei sti mnimi. Poly praktiki symvouli kai 
# kali praktiki an pairnoume kommatia kodika apo 'do ki apo 'kei
rm(list=ls())

# Orismos tis dieuthinsis ergasias
setwd("/Users/sarah-isabelpoindexter-ibarra/Downloads/erasmus_greece/Ocean_Data_Analysis")

# Opoiadipote entoli den xeroume i de thymomaste mporoume na doume plirofories 
# gia auti me to erotimatiko prin apo auti i me ti routina help()
#?setwd
#help(setwd)

# Anoigma kai eisagogi dedomenon apo arheio
ctd <- read.table("ODVdata2021.csv", header = TRUE, sep=",")

# Yparhoun diaforoi tropoi na doume ta dedomena mas...
ctd
head(ctd)
#View(ctd)

#metatrepo ta stoixeia se katiforimatikes metablites dld diabazei ti exei mesa
#kai ftiaxnei katigories mono touctd
ctd$CRUISE<-as.factor(ctd$CRUISE)
ctd$STATION<-as.factor(ctd$STATION)
# To antikeimeno 'ctd' einai ousiastika mia klasi, dil. ena syntheto antikeimeno
# pou periehei epimerous stoiheia sta opoia mporo na anafertho hrisimopoiontas 
# to symvolo: $
ctd$CRUISE
ctd$TEMPERATURE
ctd$DO

# I glossa R douleuei me routines (anaferomaste se autes os "function(s)") oi 
# opoies pairnoun parametrous (anaferomaste se autes os "arguments") gia na
# ektelestoun

# Gia paradeigma sti "vasiki"/base R mporo na kano ena aplo diagramma x-y
# kalo tin function "plot" hrisimopoiontas os arguments ta x & y pou thelo na eho
plot(ctd$DEPTH,ctd$TEMPERATURE)
plot(ctd$DEPTH,ctd$SALINITY)
plot(ctd$DEPTH,ctd$DO)


########################################
######## 2. CALCULATE DENSITY ##########
########################################
# Fortonoume mia vivliothiki. Oi vivliothikes ehoun mesa functions
# pou mas epitrepoun na kanoume analyseis (statistikes i alles) alla kai na 
# ftiaxoume i na heiristoume grafika...
library(marelac)

# An i vivliothiki den einai egkatestimeni, prepei na tin egkatastisoume prota
# (edo den tha tin egkatastiso... gi' auto i antistoihi entoli ehei morfi sholiou)
#install.packages("marelac")

# Paradeigma apo to help tis vivliothikis:
# sw_dens(S = 35, t = 25, p = max(0, P-1.013253), P = 1.013253, method=c("Gibbs","UNESCO","Chen"))

# Ypenthimisi tou dataset
ctd

# Ypologismos tis piesis pou einai aparaititi kai
# eisagogi tis os alli mia metavliti stin klasi 'ctd'
ctd$my_pressure <- 1+ctd$DEPTH/10

# I piesi apotelei pleon mia epiprostheti stili tou pinaka
ctd

# Ypologismos tis pyknotitas kai eisagogi tis sto 'ctd' apo tin function 'sw_dens'
ctd$my_density <- sw_dens(S = ctd$SALINITY, t = ctd$TEMPERATURE,P=ctd$mypressure, method="UNESCO")

# Idou...
ctd$my_density

ctd

# Apothikeusi ton dedomenon se arheio
write.table(ctd, file = "ctd_withdensity.csv", sep = ",")

#########################################
######## 3. MAPPING CONVERSION ##########
#########################################

# Metatropi ton dekadikon moiron (DD.decimal) se moires-lepta-deuterolepta/
# degrees-minutes seconds (DD MM SS)
library(celestial)

# Ftiahnoume ena kainourio dataset (tha mporousame na doulepsoume sto 'ctd', 
# apla gia na min to fortosoume me stiles...)
a_ctd         <-  ctd 
a_ctd$DNSlat  <-  deg2dms(ctd$LAT)
a_ctd$DNSlong <-  deg2dms(ctd$LONG)

a_ctd
a_ctd$DNSlat
a_ctd$DNSlat[,1]
a_ctd$DNSlat[,2]
a_ctd$DNSlat[,3]
a_ctd$DNSlat

###############################
######## 4. TIDYVERSE #########
###############################

# Poly hrisimi vivliothiki gia heirismo ton dedomenon mas. Episis me polles 
# dynatotites gia dimiourgia shimaton
library(tidyverse)

# To tidyverse hrisimopoiei enan idiaitero typo syntaxis pou legetai "pipe".
# To "pipe" grafetai patontas: Ctrl+Shft+M 
# ...kai emfanizetai os: %>% 
# Epi tis ousias to "pipe" simainei: "me auto pou proigeitai (pou emfanizetai 
# prin to "pipe") kane auto pou grafei sti syneheia"

# Etsi allazei i synithismeni logiki tis R
# diladi apo tin paradosiaki logiki syntaxis mias entolis stin R, p.h.:
# round(pi,2) 
# (pou leei "stroggylopoiise to "pi" sta dyo dekadika psifia dil. se 3.14) pame se...
# pi %>% round(2) 
# ...pou dinei to idio apotelesma
# Praktika (se shesi me tin paradosiki R) prin to "pipe" proigeitai to proto argument mias function

ctd %>% select(DEPTH,TEMPERATURE,SALINITY) 
ctd %>% select(DEPTH,TEMPERATURE,SALINITY) %>% View()
ctd %>% filter(STATION=="ST1")
ctd %>% filter(STATION=="ST2" & DEPTH>50) 

# Prosohi to antikeimeno 'ctd' den allazei. I klasi 'ctd' synehizei na periehei 
# ola ta stoiheia pou tin apotelousan
ctd
shallow_station2 <- ctd %>% filter(STATION=="ST2" & DEPTH<50)
shallow_station2

#########################################
######## 5. TEMPERATURE PROFILE #########
#########################################

# Poly hrisimi function tou tidyverse einai to 'ggplot'
# To ggplot ousiastika anaferetai sto dataset (mporei na graftei kai os "pipe")
# afou eisagoume to ggplot() ola ta systatika stoiheia eisagontai me ena "+"

# I entoli ggplot hreiazetai:
# 1. ggplot()   [i routina ggplot pou anaferetai sto dataset]
# 2. aes:       to simeio pou tha poume ti paei pou, 'hartografontas' tin eikona
#               p.h. poia parametros paei se poion axona
#               [gi' auto kai anaferetai syhna os "the aesthaetics mapping"]
# 3. geom:      o typos tou diagrammatos
#               p.h. barplot, kampyli, x-y plot
# ...epiprostheta mporei na prosthesete hromata, etikettes klp.

ggplot(ctd)+
  aes(TEMPERATURE,DEPTH,colour=STATION)+ 
  geom_point(size=2)+
  scale_y_reverse()+
  theme(
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_rect(fill = "blue"),
    panel.grid = element_blank())

ctd3 <- ctd  %>% filter(STATION=="ST3")

ggplot(ctd3)+
  aes(TEMPERATURE,DEPTH)+ 
  geom_point(size=1)+
  scale_y_reverse()+
  theme(
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank())

#@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@ Homework: Vertical profiles of salinity & oxygen
#@@@@@@@@@@@@@@@@@@



##################################
######## 6. T-S DIAGRAM ##########
##################################

# Me tin idia proseggisi...
ggplot(ctd)+
  aes(TEMPERATURE,SALINITY,colour=STATION)+ 
  geom_point(size=2)+
  theme(
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank())

# allagi stous axones...
ggplot(ctd)+
  aes(SALINITY,TEMPERATURE,colour=STATION)+ 
  geom_point(size=2)+
  theme(
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank())



##################################
######## 7. SOME EXTRAS ##########
##################################

# Alla hrisima stoiheia apo ti vivliothiki "marelac"

# Global bathymetry
par(mar = c(2,2,2,2))

image(Bathymetry$x, Bathymetry$y, Bathymetry$z, col = femmecol(100),
      asp = TRUE, xlab = "dg", ylab = "dg")
contour(Bathymetry$x, Bathymetry$y, Bathymetry$z, asp = TRUE, add = TRUE)

# Global bathymetry without altitude
zz <- Bathymetry$z
zz[zz>0] <- 0
image(Bathymetry$x, Bathymetry$y, zz, col = c(femmecol(100), "white"),
      asp = TRUE)
contour(Bathymetry$x, Bathymetry$y, zz, asp = TRUE, add = TRUE)

# Coriolis as a function of latitude
plot(-90:90, coriolis(-90:90), xlab = "latitude, dg North",
     ylab = "/s", main = "coriolis factor", type = "l", lwd = 2)

# Global oceans properties
data.frame(cbind(acronym = names(Oceans),
                 matrix(ncol = 3, byrow = TRUE, data = unlist(Oceans),
                        dimnames = list(NULL, c("value", "units", "description")))))

################################
######## 8. DIVERSITY ##########
################################s

# Prosdiorismos tou Shannon diversity index se tria set dedomenon
library(vegan)

# mporo na balo kai = anti gia <- gia kataxorisi
pop1 = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
pop2 = c(100, 500, 200, 150, 0, 400, 700, 900, 50, 0)
pop3 =  c(0, 0, 0, 0, 0, 0, 0, 200, 1800,0)

barplot(pop1)
barplot(pop2)
barplot(pop3)

d1 <- diversity(pop1, "shannon")
d2 <- diversity(pop2, "shannon")
d3 <- diversity(pop3, "shannon")

d1
d2
d3

#@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@ Homework: Vreite ena "diko sas" dataset kai kante to idio
#@@@@@@@@@@@@@@@@@@



##########################################################
######## 9. TEMPERATURE & SALINITY CONCURRENTLY ##########
##########################################################

#rm(list=ls())

library(tidyverse)

#setwd("D:/Work/Current/Education/Patras/Met_Oceanography_Data_Analysis/")
ctd <- read.table("ODVdata2021.csv", header = TRUE, sep=",")

ctd


library(patchwork) # To display 2 charts together
library("gtable")
library("grid")

ctd2 <- ctd  %>% filter(STATION=="ST2")

p1 <- ggplot(ctd2, aes(x=TEMPERATURE, y=DEPTH)) +
  geom_point(color="red", size=1) +
  scale_y_reverse()+
  scale_x_continuous(position = "top")+ 
  theme(
    
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_blank(),
    panel.grid = element_blank())


p1

p2 <- ggplot(ctd2, aes(x=SALINITY, y=DEPTH)) +
  geom_point(color="blue", size=1) +
  scale_y_reverse()+
  scale_x_continuous(position = "top") +
  theme(
    
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_blank(),
    panel.grid = element_blank())

p2

p1+p2

p3 <- ggplot(ctd2, aes(x=SALINITY, y=DEPTH)) +
  geom_point(color="blue", size=1) +
  scale_y_reverse()+
  # scale_x_continuous(position = "top") +
  theme(
    
    axis.title.y=element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(),
    axis.line.x = element_line(),    
    axis.line.y = element_line(),    
    panel.background = element_blank(),
    panel.grid = element_blank())

p3

grid.newpage()

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p3))


g1
g2

# get the location of the panel of p1 
# so that the panel of p2 is positioned correctly on top of it
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# superimpose p2 (the panel) on p1
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# extract the x-axis of p2
ia <- which(g2$layout$name == "axis-b")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax
# extract the x-axis title of p2
ia2 <- which(g2$layout$name == "xlab-b")
ga2 <- g2$grobs[[ia2]]

# add the  x axis pf p2
g <- gtable_add_rows(g, g2$heights[g2$layout[ia, ]$t], length(g$heights)-1)
g <- gtable_add_grob(g, ax, length(g$heights)-0.5,  pp$r)
g <- gtable_add_grob(g, ga2, length(g$heights)-1,  pp$r)

grid.newpage()
grid.draw(g)
