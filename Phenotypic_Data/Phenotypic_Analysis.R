R.version.string ##################CHECK THE CURRENT VERSION OF R #########
install.packages("installr") 
library(installr)
updateR()  
#############TO UPDATE TO A NEWER VERSION OF R#########

rm(list=ls(all=TRUE))
graphics.off()
shell("cls")  ############CLEAR ALL PREVIOUS OBJECT, VALUES, GRAPHICS AND WINDOWS###########

## Replicated Trial Data Analysis
setwd("C:/Users/mujjabi2/OneDrive - University of Illinois - Urbana/General/GitHub_Repositories/OREI_Controlled_Experiment/Phenotypic_Data")

library(agricolae)
library(xtable)
Compiled <- read.csv("Compiled_Data.csv", header = TRUE)
Compiled <- type.convert(replace(Compiled, Compiled == "." & !is.na(Compiled), NA), as.is = TRUE)

View(Compiled)

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

## Trait distribution
library(ggplot2)
ggplot(Compiled, aes(x = YIELD, fill = LOC)) +
geom_histogram(alpha = 0.8, position = "identity") +
scale_fill_hue(labels = c("Pana", "Macomb", "UIUC2019", "UIUC2020"))





library(ggplot2)
Plot<-ggplot(data=Compiled, aes(x = LOC, y = YIELD, group = HYB)+
               geom_line(size=2, aes(color=HYB))+
               ylab("Yield")+
               xlab("Nitrogen Level")+
               ggtitle("Nitrogen Levelas Predictors of Yield")+
               theme_bw()+ 
               theme(panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank())+
               scale_fill_grey())
Plot


################PLOTTING CORRELATIONG##############
library(car)
library(corrplot)
Correlation <- cor(Compiled[,c(17:34)], use = "pairwise.complete.obs")
corrplot(Correlation,method = "circle",type="upper", order="hclust", #Type = upper,lower, #method=circle,pie,color,number
           addCoef.col="black", # Add coefficient of correlation
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation

Correlation <- cor(Compiled[,c(17:60)], use = "pairwise.complete.obs")
corrplot(Correlation, method = "circle", type="upper", order="original", na.label=" ",#Type = upper,lower, #method=circle,pie,color,number
        #addCoef.col="black", # Add coefficient of correlation
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation
corrplot(Correlation)
