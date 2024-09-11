###Mate Color Choice####
library(ggpubr)
library(cowplot)
theme_set(theme_cowplot())
library(nnet)

#Data files needed: 
#"Trypsin Val Data.csv", to analyze male preference for size when treated with trypsin
#"color Choice Data.csv", to analyze effect of color on clasping and to plot histogram of male clasp time
#"Color Choice Totals.csv", to plot male choice data
#"Trypsin Val Totals.csv", to plot male preference for stage when treated with trypsin
 

#################### STart Plotting of results ###################################
#Plot bar graphs with totals for each group

#subset group totals out from groups organized by male
colortots = read.csv("Color Choice Totals.csv")
colortots$Female = relevel(colortots$Female, ref = "White")
table(colortots$Female)

#All male data in each group
#Leave odd spacing alone in scale_x_discrete code. It matches up when drawn out. 

p1<-ggplot(data=colortots, aes(x=Group, y=percent, fill=Female, label = clasped)) +
  geom_bar(stat="identity")+
  geom_text(size=6, position = position_stack(vjust=0.5), color="white")+
  scale_fill_manual("Female Clasped:", values=c('#999999','#666666', '#000000'))+
  ylab("Percentage of Male Choices") +
  theme(axis.text.x = element_text(size=13))+
  theme(axis.text.y = element_text(size=13))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x = element_blank())+
  theme(legend.text = element_text(size=13))+
  theme(legend.position = c(0.28,0.98), legend.direction = "horizontal")+
  scale_x_discrete(labels=c("Wild-type red vs
  white (trypsin)", "Wild-type red vs 
white (untreated)",
                            "Restored-red vs 
white (trypsin)", "Restored-red vs 
white (untreated)"))
p1

tiff(file = "Fig 4 main groups.tiff", units = "in", width = 7.5, height = 7.5, res = 600) 
p1
dev.off()


tiff(file = "Fig III main groups low res.tiff", units = "in", width = 7.5, height = 7.5, res = 100) 
p1
dev.off()

##Trypsin size choice test
#Import data totals for graphing
trypvaltots = read.csv("Trypsin Val Totals.csv")

#Build plot and export to tiff file
p2<-ggplot(data=trypvaltots, aes(x=Trypsin, y=percent, fill=female, label=clasped)) +
  geom_bar(stat="identity")+
  geom_text(size=6, position = position_stack(vjust=0.5), color="white")+
  scale_fill_manual("Female clasped:",values=c('#999999','#666666', '#000000'))+
  ylab("Percentage of Male Choices") +
  theme(axis.text.x = element_text(size=13))+
  theme(axis.text.y = element_text(size=13))+
  theme(axis.title.y = element_text(size=13))+
  theme(axis.title.x = element_blank())+
  theme(legend.text = element_text(size=11), legend.position = c(0.05,0.99), legend.direction = "horizontal")
p2

tiff(file = "Fig 3 trypsin val totals.tiff", units = "in", width = 5.0, height = 5.5, res = 600) 
p2
dev.off()

tiff(file = "Fig 3 trypsin val totals low res.tiff", units = "in", width = 5.0, height = 5.5, res = 100) 
p2
dev.off()


#histogram of how long it took males to clasp females
p0 <- ggplot(color, aes(x=TimeClasped)) + 
  geom_histogram(binwidth=5, color="black", fill="white")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,60,120))+
  xlab("Time until male clasped in minutes")+
  ylab("Number of males")
p0

tiff(file = "Fig SII.tiff", units = "in", width = 6.8, height = 5.0, res = 600)
p0
dev.off()

tiff(file = "Fig SII low res.tiff", units = "in", width = 6.8, height = 5.0, res = 100)
p0
dev.off()

################# End plotting and start modeling #####################


####Are there significant differences in the number of red and clear females chosen within a treatment group?####
  
#Read in data and check that variables are in correct format
color=read.csv("Color Choice Data.csv")
str(color)

###Subsetting###
#Droplevels used to reset categorical factors to appropriate level values after subset
#Algae data.frame
colorA=color[1:184,]
colorA = droplevels(colorA)

#Zeaxanthin data.frame
colorZ=color[185:335,]
colorZ = droplevels(colorZ)

algaeuntreat = color[1:88,]
algaeuntreat = droplevels(algaeuntreat)

algaetreat = color[89:184,]
algaetreat = droplevels(algaetreat)

zeauntreat = color[185:259,]
zeauntreat = droplevels(zeauntreat)

zeatreat = color[260:335,]
zeatreat = droplevels(zeatreat)

###mULTINOMIAL COLOR CHOICE MODELS

##Wild type red untreated
#Compare wild type red and clear to no choice as reference
algaeuntreat$female = relevel(algaeuntreat$female, ref = "Y")
AUtest <- multinom(female~1, data = algaeuntreat)
summary(AUtest)
exp(coef(AUtest))
exp(confint(AUtest))

##wildtype red treated
#Compare wild type red and clear to no choice as reference
algaetreat$female = relevel(algaetreat$female, ref="Y")
ATtest <- multinom(female~1, data=algaetreat)
summary(ATtest)
exp(coef(ATtest))
exp(confint(ATtest))

##Restored red untreated
#compare restored red and clear to no choice as reference
zeauntreat$female = relevel(zeauntreat$female, ref = "Y")
ZUtest <- multinom(female~1, data=zeauntreat)
summary(ZUtest)
exp(coef(ZUtest))
exp(confint(ZUtest))

##Restored red treated
#compare restored red and clear to no choice as reference
zeatreat$female = relevel(zeatreat$female, ref = "Y")
ZTtest <- multinom(female~1, data=zeatreat)
summary(ZTtest)
exp(coef(ZTtest))
exp(confint(ZTtest))




####################### Analysis of Trypsin Validation Data #################################


#Are there significat differences between the number of large and small pods chosen between a treatment group?       

#Subset trypdatum dataset into treated and untreated groups, then drop levels of data frames
tryptreat = trypdatum[40:80,]
tryptreat = droplevels(tryptreat)
trypuntreat = trypdatum[1:39,]
trypuntreat = droplevels(trypuntreat)

###Untreated group
#Compare the likelihood of clasping large and making no choice to clasping small females
Utest = multinom(female~1, data = trypuntreat)
summary(Utest)

exp(coef(Utest))
exp(confint(Utest))

zUtest <- summary(Utest)$coefficients/summary(Utest)$standard.errors
zUtest

pUtest <- (1 - pnorm(abs(zUtest), 0, 1)) * 2
pUtest

#Compare the likelihood of clasping large or small to making no choice
trypuntreat$female = relevel(trypuntreat$female, ref="0")
Utest2 = multinom(female~1, data = trypuntreat)
summary(Utest2)

exp(coef(Utest2))
exp(confint(Utest2))

zUtest2 <- summary(Utest2)$coefficients/summary(Utest2)$standard.errors
zUtest2

pUtest2 <- (1 - pnorm(abs(zUtest2), 0, 1)) * 2
pUtest2

###Treated group
#Compare the likelihood of clasping large and making no choice to clasping small females
Ttest = multinom(female~1, data = tryptreat)
summary(Ttest)

exp(coef(Ttest))
exp(confint(Ttest))

zTtest <- summary(Ttest)$coefficients/summary(Ttest)$standard.errors
zTtest

pTtest <- (1 - pnorm(abs(zTtest), 0, 1)) * 2
pTtest

#Compare the likelihood of clasping large or small to making no choice
tryptreat$female = relevel(tryptreat$female, ref="0")
Ttest2 = multinom(female~1, data = tryptreat)
summary(Ttest2)

exp(coef(Ttest2))
exp(confint(Ttest2))

zTtest2 <- summary(Ttest2)$coefficients/summary(Ttest2)$standard.errors
zTtest2

pTtest2 <- (1 - pnorm(abs(zTtest2), 0, 1)) * 2
pTtest2







