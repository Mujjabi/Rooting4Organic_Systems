R.version.string ##################CHECK THE CURRENT VERSION OF R #########
install.packages("installr") 
library(installr)
updateR()  
#############TO UPDATE TO A NEWER VERSION OF R#########

rm(list=ls(all=TRUE))
graphics.off()
shell("cls")  ############CLEAR ALL PREVIOUS OBJECT, VALUES, GRAPHICS AND WINDOWS###########

#https://stat.ethz.ch/education/semesters/as2015/anova/08_Split_Plots.pdf
## Replicated Trial Data Analysis
setwd("C:/Users/mujjabi2/OneDrive - University of Illinois - Urbana/General/GitHub_Repositories/OREI_Controlled_Experiment/Phenotypic_Data")

library(agricolae)
library(xtable)

Compiled <- read.csv("Compiled_Data.csv", header = TRUE)
Compiled <- type.convert(replace(Compiled, Compiled == "." & !is.na(Compiled), NA), as.is = TRUE)


## Converting Yield from Bu/Acre to Kg/ha

Compiled$YIELD <-Compiled$YIELD*67.25
min(Compiled$YIELD, na.rm = TRUE)
max(Compiled$YIELD, na.rm = TRUE)


## Settings design and response variables

Compiled$EXP <- as.factor(Compiled$EXP)
Compiled$YEAR <- as.factor(Compiled$YEAR)
Compiled$LOC <- as.factor(Compiled$LOC)
Compiled$REP <- as.factor(Compiled$REP)
Compiled$NITRO <- as.factor(Compiled$NITRO)
Compiled$PED <- as.factor(Compiled$PED)
Compiled$HYB <- as.factor(Compiled$HYB)
Compiled$WEED <- as.factor(Compiled$WEED)
Compiled$STC <- as.numeric(Compiled$STC)
Compiled$DTA <- as.numeric(Compiled$DTA)
Compiled$DTS <- as.numeric(Compiled$DTS)
Compiled$FDT <- as.numeric(Compiled$FDT)
Compiled$FDS <- as.numeric(Compiled$FDS)
Compiled$RTA <- as.numeric(Compiled$RTA)
Compiled$RLA<- as.numeric(Compiled$RLA)
Compiled$SDI <- as.numeric(Compiled$SDI)
Compiled$PHT <- as.numeric(Compiled$PHT)
Compiled$EHT <- as.numeric(Compiled$EHT)
Compiled$SDL <- as.numeric(Compiled$SDL)
Compiled$SDS <- as.numeric(Compiled$SDS)
Compiled$GWT <- as.numeric(Compiled$GWT)
Compiled$MST <- as.numeric(Compiled$MST)
Compiled$TWT <- as.numeric(Compiled$TWT)
Compiled$YIELD <- as.numeric(Compiled$YIELD)
Compiled$PROT <- as.numeric(Compiled$PROT)
Compiled$OIL <- as.numeric(Compiled$OIL)
Compiled$STA <- as.numeric(Compiled$STA)
Compiled$ASH <- as.numeric(Compiled$ASH)
Compiled$DENS <- as.numeric(Compiled$DENS)
Compiled$pH <- as.numeric(Compiled$pH)
Compiled$OM <- as.numeric(Compiled$OM)
Compiled$EstNRel <- as.numeric(Compiled$EstNRel)
Compiled$Su <- as.numeric(Compiled$Su)
Compiled$P <- as.numeric(Compiled$P)
Compiled$Ca <- as.numeric(Compiled$Ca)
Compiled$Mg <- as.numeric(Compiled$Mg)
Compiled$K <- as.numeric(Compiled$K)
Compiled$Na <- as.numeric(Compiled$Na)
Compiled$Bray1P <- as.numeric(Compiled$Bray1P)
Compiled$TOC <- as.numeric(Compiled$TOC)
Compiled$TN <- as.numeric(Compiled$TN)
Compiled$SoilCN <- as.numeric(Compiled$SoilCN)
Compiled$Sand <- as.numeric(Compiled$Sand)
Compiled$Silt <- as.numeric(Compiled$Silt)
Compiled$Clay <- as.numeric(Compiled$Clay)
Compiled$SMC <- as.numeric(Compiled$SMC)
Compiled$InorgN <- as.numeric(Compiled$InorgN)
Compiled$POMN <- as.numeric(Compiled$POMN)
Compiled$POMC <- as.numeric(Compiled$POMC)
Compiled$POMCN <- as.numeric(Compiled$POMCN)
Compiled$PMN <- as.numeric(Compiled$PMN)
Compiled$POXC <- as.numeric(Compiled$POXC)


Compiled2 <- Compiled[c(2,3,7:11,19:21, 24:27,29:34)]
Compiled2 <- Compiled2[complete.cases(Compiled2),]   #removes all missing, including 2020
colSums(is.na(Compiled2))  # Shows number of empty rows per column   

##Imputing missing data using mL
#install.packages("missForest")
library(missForest)
Impdata <- missForest(Compiled2)



## Correlation between root and agronomic traits. 

Data1<-Compiled2[,c(8:12, 15:20)]

Data2 <- Data1[complete.cases(Data1),]  ## Removes all missing values. Best way to deal with them?


## Splitting years
ALL  <- Compiled2
Data1819 <- subset(ALL, YEAR %in% c("2018","2019"))
Data2020 <- subset(ALL, YEAR=="2020")


ALL2 <-ALL[,c(1:12,15:20)]
ALL2 <- ALL2[complete.cases(ALL2)]

Data1819 <- Data1819[complete.cases(Data1819),] 

Data2020 <- Data2020[,c(1:12, 15:20)]
Data2020  <- Data2020[complete.cases(Data2020),] 


library(car)
library(corrplot)
Correlation <- cor(ALL, use = "pairwise.complete.obs")
corrplot(Correlation,method = "color",type="upper", order="hclust", #Type = upper,lower, #method=circle,pie,color,number
         addCoef.col="black", # Add coefficient of correlation
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation


library(car)
library(corrplot)
Correlation <- cor(Data2020[,c(8:18)], use = "pairwise.complete.obs")
corrplot(Correlation,method = "color",type="upper", order="hclust", #Type = upper,lower, #method=circle,pie,color,number
         addCoef.col="black", # Add coefficient of correlation
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation



#### GGPLOT BOXPLOT WITH ERROR BARS AND NO OUTLIERS ####

# 1. Root complexity
### A. Across Nitrogen levels
library(ggplot2)
ggplot(Compiled2, aes(x = NITRO, y = FDT,  fill = NITRO, las=1)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"))+
  labs(x = "Nitrogen Treatment", y = "Root Complexity", fill = "Nitrogen", cex.lab=5) +
  theme(axis.title = element_text(size = rel(1.5))) 


###B.  Across Weed pressure levels

library(ggplot2)
ggplot(Compiled2, aes(x = WEED, y = FDT,  fill = WEED, las=1)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  scale_fill_manual(values = c( "sienna","cyan3")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"))+
  labs(x = "Weed Pressure", y = "Root Complexity",fill = "Weed Treatment", cex.lab=5) +
  theme(axis.title = element_text(size = rel(1.5)))





# 2. Root Angles 
### A. Across Nitrogen levels
library(ggplot2)
ggplot(Compiled2, aes(x = NITRO, y = RTA,  fill = NITRO)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = 0, hjust = 1, size=13,color="black"))+
  labs(x = "Nitrogen Treatment", y = "Root Angle [Degrees]", fill = "Nitrogen") +
  theme(axis.title = element_text(size = rel(1.5)))

###B.  Across Weed pressure levels
library(ggplot2)
ggplot(Compiled2, aes(x = WEED, y = RTA,  fill = WEED)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  scale_fill_manual(values = c( "sienna","cyan3")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"))+
  labs(x = "Weed Pressure", y = "Root Angle [Degrees]", fill = "Weed Treatment") +
  theme(axis.title = element_text(size = rel(1.5)))



# BAR Graphs Hybrid Performance

## Select CHECK, UIUC4 and ORG4 to show differences in hybrid performance
Selected_HYB <- subset(ALL, HYB %in% c("CHECK","ORG4", "UIUC4"))


## 1. Change of Yield across N treatments
library(ggplot2)
library(ggpattern)
ggplot(data = Selected_HYB, aes(x = HYB, y = YIELD, fill = NITRO)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  #scale_fill_manual(values = c( "#009FD4","#1D58A7", "#1E3877")) + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  ylim(0,12000) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 1, size=13,color="black"), axis.title = element_text(size = 18)) +
  labs(x = "Hybrid", y = "Grain Yield [Kg/Ha]",fill = "Nitrogen", cex.lab =10)


## 2. Change of Yield across weed treatments
library(ggplot2)
library(ggpattern)
ggplot(data = Compiled2, aes(x = HYB, y = YIELD, fill = WEED)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  scale_fill_manual(values = c( "sienna","cyan3")) + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  ylim(0,12000) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 1, size=13,color="black"), axis.title = element_text(size = 18)) +
  labs(x = "Location", y = "Grain Yield [Kg/Ha]",fill = "Weed Treatment", cex.lab=10)





## Hybrid Root characteristics

## Root Complexity
#1. Hybrid response to Nitrogen
library(ggplot2)
library(ggpattern)
library(scales)
ggplot(data = Selected_HYB, aes(x = HYB, y = FDT, fill = NITRO)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  #scale_fill_manual(values = c( "grey41","grey41", "grey41","grey41","grey41", "grey41","grey41","grey41", "grey41","grey41","grey41", "grey41" )) + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(ylim = c(1.75, 1.95)) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1.75, 1.95, by = 0.05),labels = scales::number_format(accuracy = 0.01))+
  guides(fill = guide_legend(direction = "vertical")) +
  labs(x = "Hybrids", y = "Root Complexity", fill = "Nitrogen")

#2. Hybrid response to Weed Treatment
library(ggplot2)
library(ggpattern)
library(scales)
ggplot(data = Selected_HYB, aes(x = HYB, y = FDT, fill = WEED)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  scale_fill_manual(values = c( "sienna","cyan3")) +
    theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5,  linetype = 'solid', color = "white")) +
  coord_cartesian(ylim = c(1.75, 1.95)) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1.75, 1.95, by = 0.05),labels = scales::number_format(accuracy = 0.01))+
  guides(fill = guide_legend(direction = "vertical")) +
  labs(x = "Hybrids", y = "Root Complexity", fill = "Weed Treatment")


## Root Angles
#1. Hybrid response to Nitrogen Treatment

library(ggplot2)
library(ggpattern)
ggplot(data = Selected_HYB, aes(x = HYB, y = RTA, fill = NITRO)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  #scale_fill_manual(values = c( "grey41","grey41", "grey41","grey41","grey41", "grey41","grey41","grey41", "grey41","grey41","grey41", "grey41" )) + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(ylim = c(50, 110)) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  guides(fill = guide_legend(direction = "horizontal")) +
  labs(x = "Hybrids", y = "Root Angle [Degrees]", cex.lab=10)


#2. Hybrid response to Weed Treatment

library(ggplot2)
library(ggpattern)
ggplot(data = Selected_HYB, aes(x = HYB, y = RTA, fill = WEED)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  scale_fill_manual(values = c( "sienna","cyan3")) +
    theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(ylim = c(50, 110)) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  guides(fill = guide_legend(direction = "vertical"))+
  labs(x = "Hybrids", y = "Root Angle [Degrees]", cex.lab=10)



## Plotting relationships and correlations

#1. Root complexity with yield

library(ggplot2)
library(ggpubr) 
ggplot(ALL2, aes(x=FDT, y=YIELD, color=NITRO)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Root Complexity", y = "Grain Yield [Kg/Ha]", fill = "Nitrogen") + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(xlim = c(1.8, 2)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1000, 15000, by = 2000)) +
  guides(fill = guide_legend(direction = "vertical")) +
  stat_cor(method = "pearson", label.x = 1.8, label.y = 15000, color="black", size = 5)

#2. Root angles  with yield

library(ggplot2)
library(ggpubr) 
ggplot(ALL2, aes(x=RTA, y=YIELD, color=NITRO)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Root Angle [Degrees]", y = "Grain Yield [Kg/Ha]", fill = "Nitrogen") + 
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(xlim = c(40, 120)) +
  theme(axis.text= element_text(angle = 0, hjust = 0.5, size=13,color="black"), axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1000, 15000, by = 2000)) +
  guides(fill = guide_legend(direction = "vertical")) +
  stat_cor(method = "pearson", label.x = 40, label.y = 15000, color="black", size = 5)+
  theme(legend.direction = "vertical")


## Analysis of variance


## Splitting years
ALL  <- Compiled2
Pana <- subset(ALL, LOC=="Pana")
Macomb <- subset(ALL, LOC=="Macomb")
UIUC2019 <- subset(ALL, LOC=="UIUC2019")
UIUC2020 <- subset(ALL, LOC=="UIUC2020")
Selected_HYB <- subset(ALL, HYB %in% c("CHECK","ORG4", "UIUC4"))

# Combined analysis with all locations

## 1. Root complexity
install.packages("agricolae")
install.packages("rlang")
library(car)
library(agricolae)
Model_FDT <- with(data=ALL2, ssp.plot(YEAR:LOC:REP, NITRO,HYB, WEED, FDT)) ##Model for split split plot 
gla<-Model_FDT$gl.a 
glb<-Model_FDT$gl.b 
glc<-Model_FDT$gl.c

Ea<-Model_FDT$Ea 
Eb<-Model_FDT$Eb 
Ec<-Model_FDT$Ec

out1<-with(data=ALL2,LSD.test(FDT,NITRO,gla,Ea,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))
out2<-with(data=ALL2,LSD.test(FDT,HYB,glb,Eb,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))
out3<-with(data=ALL2,LSD.test(FDT,WEED,glc,Ec,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))


## Root Angles 
install.packages("agricolae")
install.packages("rlang")
library(car)
library(agricolae)
Model_RTA <- with(data=ALL2, ssp.plot(YEAR:LOC:REP, NITRO,HYB, WEED, RTA)) ##Model for split split plot 
gla<-Model_RTA$gl.a 
glb<-Model_RTA$gl.b 
glc<-Model_RTA$gl.c

Ea<-Model_RTA$Ea 
Eb<-Model_RTA$Eb 
Ec<-Model_RTA$Ec

out4<-with(data=ALL2,LSD.test(RTA,NITRO,gla,Ea,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))
out5<-with(data=ALL2,LSD.test(RTA,HYB:LOC,glb,Eb,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))
out6<-with(data=ALL2,LSD.test(RTA,WEED,glc,Ec,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))


# Analysis by selected Hybrids

### 1. Root complexity
library(car)
library(agricolae)
Model_HYB1 <- with(data=Selected_HYB , ssp.plot(YEAR:LOC:REP, NITRO,HYB, WEED, FDT))
gla<-Model_HYB1$gl.a 
glb<-Model_HYB1$gl.b 
glc<-Model_HYB1$gl.c

Ea<-Model_HYB1$Ea 
Eb<-Model_HYB1$Eb 
Ec<-Model_HYB1$Ec

out7<-with(data=Selected_HYB,LSD.test(FDT,NITRO,gla,Ea,console=TRUE,alpha = 0.05,  p.adj = "bonferroni"))
out8<-with(data=Selected_HYB,LSD.test(FDT,HYB:NITRO,glb,Eb,console=TRUE,alpha = 0.05))
out82<-with(data=Selected_HYB,LSD.test(FDT,HYB:WEED,glb,Ec,console=TRUE,alpha = 0.05))
out9<-with(data=Selected_HYB,LSD.test(FDT,NITRO:WEED,glc,Ec,console=TRUE,alpha = 0.05,  p.adj = "bonferroni"))


### 2. Root Angles 

library(car)
library(agricolae)
Model_HYB2<- with(data=Selected_HYB, ssp.plot(YEAR:LOC:REP, NITRO,HYB, WEED, RTA))
gla<-Model_HYB2$gl.a 
glb<-Model_HYB2$gl.b 
glc<-Model_HYB2$gl.c

Ea<-Model_HYB2$Ea 
Eb<-Model_HYB2$Eb 
Ec<-Model_HYB2$Ec

out10<-with(data=Selected_HYB,LSD.test(RTA,NITRO,gla,Ea,console=TRUE,alpha = 0.05, p.adj = "bonferroni"))
out11<-with(data=Selected_HYB,LSD.test(RTA,HYB:NITRO,glb,Eb,console=TRUE,alpha = 0.05,  p.adj = "bonferroni"))
out111<-with(data=Selected_HYB,LSD.test(RTA,HYB:WEED,glb,Ec,console=TRUE,alpha = 0.05,  p.adj = "bonferroni"))
out12<-with(data=Selected_HYB,LSD.test(RTA,WEED,glc,Ec,console=TRUE,alpha = 0.05,  p.adj = "bonferroni"))


## Using AsReml

#Eliminating 2020 dataset

Data1819 <- subset(Compiled, YEAR %in% c("2018","2019"))


# 1. Yield estimates 

library(asreml)

model.asreml.yld <- asreml(fixed = YIELD ~ NITRO + HYB + NITRO:HYB + WEED + NITRO:WEED + HYB:WEED + NITRO:HYB:WEED,
                       random = ~YEAR + LOC + REP + YEAR:LOC:REP,
                       residual = ~units,
                       ai.sing = TRUE,
                       data = Compiled)
plot(model.asreml.yld)
summary(model.asreml.yld)$varcomp
wald(model.asreml.yld)

model.wld.yld <- wald(model.asreml.yld, denDF = "default")
model.wld.yld$Wald
model.wld.yld$stratumVariances

## Predicting Performance

Nitro.pv.yld <- predict(model.asreml.yld, classify = "NITRO", sed = TRUE)
Hyb.pv.yld  <-  predict(model.asreml.yld, classify = "HYB", sed = TRUE)
weed.pv.yld  <- predict(model.asreml.yld, classify = "WEED", sed = TRUE)
all.pv.yld   <- predict(model.asreml.yld, classify = "LOC:NITRO:HYB:WEED", sed = TRUE)

Estimates <- all.pv.yld$pvals
Estimates$YLD <- as.numeric(Estimates$predicted.value)
Estimates$YLD_sd <- as.numeric(Estimates$std.error)



#2.  Root complexity estimates
model.asreml.fdt <- asreml(fixed = FDT ~ NITRO + HYB + NITRO:HYB + WEED + NITRO:WEED + HYB:WEED + NITRO:HYB:WEED,
                       random = ~ YEAR + LOC + REP + YEAR:LOC:REP,
                       residual = ~units,
                       data = Compiled)
plot(model.asreml.fdt)

summary(model.asreml.fdt)$varcomp

wald(model.asreml.fdt)

model.wld.fdt <- wald(model.asreml.fdt, denDF = "default")
model.wld.fdt$Wald
model.wld.fdt$stratumVariances

## Predicting Performance
Nitro.pv.fdt <- predict(model.asreml.fdt, classify = "NITRO", sed = TRUE)
Hyb.pv.fdt <-  predict(model.asreml.fdt, classify = "HYB", sed = TRUE)
weed.pv.fdt <- predict(model.asreml.fdt, classify = "WEED", sed = TRUE)
all.pv.fdt  <- predict(model.asreml.fdt, classify = "LOC:NITRO:HYB:WEED", sed = TRUE)

Estimates.fdt <- all.pv.fdt$pvals
Estimates$FDT <- as.numeric(Estimates.fdt$predicted.value)
Estimates$FDT_sd <- as.numeric(Estimates.fdt$std.error)


# 3.  Root angle estimates  
model.asreml.rta <- asreml(fixed = RTA ~ NITRO + HYB + NITRO:HYB + WEED + NITRO:WEED + HYB:WEED + NITRO:HYB:WEED,
                           random = ~ YEAR + LOC + REP + YEAR:LOC:REP,
                           residual = ~units,
                           data = Compiled)
plot(model.asreml.rta)

summary(model.asreml.rta)$varcomp

wald(model.asreml.rta)

model.wld.rta <- wald(model.asreml.rta, denDF = "default")
model.wld.rta$Wald
model.wld.rta$stratumVariances

## Predicting Performance
Nitro.pv.rta <-predict(model.asreml.rta, classify = "NITRO", sed = TRUE)
Hyb.pv.rta <-  predict(model.asreml.rta, classify = "HYB", sed = TRUE)
weed.pv.rta <- predict(model.asreml.rta, classify = "WEED", sed = TRUE)
all.pv.rta  <- predict(model.asreml.rta, classify = "LOC:NITRO:HYB:WEED", sed = TRUE)

Estimates.rta <- all.pv.rta$pvals

Estimates$RTA <- as.numeric(Estimates.rta$predicted.value)
Estimates$RTA_se <- as.numeric(Estimates.rta$std.error)


## Bargraph for the predicted values
library(ggplot2)
library(ggpattern)
ggplot(data = Estimates2, aes(x = HYB, y = pred, fill = WEED)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, position = position_dodge(width = 0.9), width = 0.5, size =0.7) +
  scale_fill_manual(values = c( "#edf8b1","#7fcdbb", "#2c7fb8")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(ylim = c(0, 16000)) +
  theme(line = element_line(size = 1)) +
  theme(axis.text= element_text(angle = 90, hjust = 0.5, size=10,color="black"), axis.title = element_text(size = 18)) +
  guides(fill = guide_legend(direction = "vertical"))+
  labs(x = "Hybrids", y = "Predicted Yield [Bu/Acre]", cex.lab=5) 

## Correlation between yield yield and root architectural traits
#1. Root complexity with yield

library(ggplot2)
library(ggpubr) 
ggplot(Estimates, aes(x = FDT, y = YLD, color = NITRO)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  labs(x ="Root Complexity", y = "Grain Yield [Kg/Ha]", fill = "Nitrogen") + 
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(xlim = c(1.85, 1.94)) +
  theme(axis.text = element_text(angle = 0, hjust = 0.5, size = 13, color = "black"),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1000, 12000, by = 1000)) +
  scale_x_continuous(breaks = seq(0.2, 1.94, by = 0.02)) +
  guides(fill = guide_legend(direction = "vertical")) +
  stat_cor(aes(group = LOC),method = "pearson",label.x = 1.85,label.y = 12000,color = "black",size = 4) +  # Separate correlation for each Nitro level aes(group = NITRO)
  theme(legend.direction = "vertical")


#2. Root angles  with yield
ggplot(Estimates, aes(x = RTA, y = YLD, color = NITRO)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  labs(x = "Root Angle [Degrees]", y = "Grain Yield [Kg/Ha]", fill = "Nitrogen") + 
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white")) +
  coord_cartesian(xlim = c(63, 85)) +
  theme(axis.text = element_text(angle = 0, hjust = 0.5, size = 13, color = "black"),
    axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(1000, 12000, by = 1000)) +
  scale_x_continuous(breaks = seq(10, 85, by = 5)) +
  guides(fill = guide_legend(direction = "vertical")) +
  stat_cor(aes(group = LOC), method = "pearson",label.x = 63,label.y = 12000,color = "black",size = 4) +  # Separate correlation for each Nitro level aes(group = NITRO)
  theme(legend.direction = "vertical")




















