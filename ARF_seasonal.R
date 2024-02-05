#**************************************************
# R code associated with:
# Soil microbial communities and biogeochemistry during human decomposition differs between seasons: evidence from year-long trials
# Lois S. Taylor, Allison R. Mason, Hannah L. Noel, Michael E. Essington, Mary C. Davis, Veronica A. Brown, Dawnie W. Steadman, Jennifer M. DeBruyn
#**************************************************


#*********************
# Biogeochemistry
#***************************************************
  # Temperature, moisture, and conductivity data from probes
  #**************************************************
  library(dplyr)
Decagon <- read.csv(file="decagon_SP.csv",stringsAsFactors = FALSE)
Decagon <- read.csv(file="decagon_WIN.csv",stringsAsFactors = FALSE)
names(Decagon)
str(Decagon)

# Temperature Means plot (by day)
pdf("tempWIN.pdf", width = 7, height = 4.5)
#pdf("tempSP.pdf", width = 7, height = 4.5)
plot(Decagon$Day, Decagon$ambient_.means, xlim=c(0,380),ylim=c(0,45),type="l", lty=1,lwd=1, ylab="Degrees C ", xlab = "Day", col="gray")
lines(Decagon$Day, Decagon$Soiltemp_conmeans, col="black", lty=1, lwd=1)
lines(Decagon$Day, Decagon$internal_means, col="red3", lty=1, lwd=2)
lines(Decagon$Day, Decagon$Soiltemp_means, col="blue3", lty=1, lwd=2)
legend("topright", legend=c("Internal","Soil","Soil control","Ambient air"),lty=c(1,1,1,1),col=c("red3","blue3","black","gray"), lwd=c(3,3,1,1),cex=c(0.6,0.6,0.6,0.6),bty="n")
dev.off()

# EC Means plot
pdf("ECWIN.pdf", width = 7, height = 4.5)
#pdf("ECSP.pdf", width = 7, height = 4.5)
plot(Decagon$Day, Decagon$EC_conmeans, xlim=c(0,380),ylim=c(0,1.5),type="l", lty=1,lwd=1, ylab="mS cm-1 ", xlab = "Day", col="black")
lines(Decagon$Day, Decagon$EC_means, col="blue3", lty=1, lwd=3)
legend("topright", legend=c("Soil","Soil control"),lty=c(1,1),col=c("blue3","black"), lwd=c(3,1),cex=c(0.6,0.6),bty="n")
dev.off()

# Moisture Means plot
#pdf("MoistureWIN.pdf", width = 7, height = 4.5)
#pdf("MoistureSP.pdf", width = 7, height = 4.5)
plot(Decagon$Day,Decagon$Moisture_conmeans, xlim=c(0,380),ylim=c(0,65),type="l", lty=1,lwd=1, ylab="% Volumetric Moisture ", xlab = "Day", col="black")
lines(Decagon$Day, Decagon$Moisture_means, col="blue3", lty=1, lwd=3)
legend("topright", legend=c("Soil","Soil control"),lty=c(1,1),col=c("blue3","black"), lwd=c(3,1),cex=c(0.6,0.6),bty="n")
dev.off()

#******************************************************************
#Day vs ADH plots  and correlations
#*******************************************************************
library(dplyr)
decagon <- read.csv(file="decagon_SP.csv",stringsAsFactors = FALSE)
decagon1 <- read.csv(file="decagon_WIN.csv",stringsAsFactors = FALSE)

pdf("ADHbyDAY_v2.pdf", width = 7, height = 6)
plot(decagon$Day, decagon$ADH_ambient, xlim=c(0,390),ylim=c(0,170000),type="l", lwd=3, ylab="ADH ", xlab = "Day", main= "ADH by day", col="black")
lines(decagon$Day, decagon$ADH_soil, col="red2", type="l", lwd=3)
lines(decagon$Day, decagon$ADH_internal, col="red2", type="l", lwd=3, lty=3)
lines(decagon$Day, decagon$ADH_consoil, col="red2", type="l", lwd=3, lty=2)

lines(decagon1$Day, decagon1$ADH_ambient, col="#828282", type="l", lwd=3)
lines(decagon1$Day, decagon1$ADH_soil, col="blue2", type="l", lwd=3)
lines(decagon1$Day, decagon1$ADH_internal, col="blue2", type="l", lwd=3, lty=3)
lines(decagon1$Day, decagon1$ADH_consoil, col="blue2", type="l", lwd=3, lty=2)

legend("bottomright", legend=c("Spring - ambient air","Spring - soil", "Spring - internal","Spring - control soil","Winter - ambient air","Winter - soil", "Winter - internal","Winter - control soil"),lty=c(1,1,3,2,1,1,3,2),col=c("black","red2","red2","red2","#828282","blue2","blue2", "blue2"), lwd=c(3,3,3,3,3,3,3,3),cex=c(0.9,0.9,0.9,0.9,0.9,0.9, 0.9, 0.9),bty="n")
dev.off()


#***********************************************************************************
# CORE AND INTERFACE COMPOSITE GRAPHS BY ADH CALCULATED FROM SOIL TEMPERATURES
#**********************************************************************************

library (dplyr)
pHECetcSP <- read.csv(file="pH_EC_etc_SP.csv")# stringsAsFactors = FALSE)
names(pHECetcSP)
str(pHECetcSP)

pHECetcWIN <- read.csv(file="pH_EC_etc_WIN.csv")# stringsAsFactors = FALSE)
names(pHECetcWIN)
str(pHECetcWIN)

conintSP <-filter(pHECetcSP,Location == "conint")
con15SP <-filter(pHECetcSP,Location == "con15")
grintSP <-filter(pHECetcSP,Location == "grint")
gr15SP <-filter(pHECetcSP,Location == "gr15")

conintWIN <-filter(pHECetcWIN,Location == "conint")
con15WIN <-filter(pHECetcWIN,Location == "con15")
grintWIN <-filter(pHECetcWIN,Location == "grint")
gr15WIN <-filter(pHECetcWIN,Location == "gr15")

#******************
# CORE COMPOSITES
#******************

sample_ADHSP <-c(0,2703,5362,8360,18289,30771,41581,51922,61203,71557,79553,88547,98759,107850,116954,127477,135944,144538,151096,159517)
sample_ADHWIN<-c(0,3826,7594,12127,22782,33575,43005,52404,60714,71952,79891,87883,96552,104845,115079,123576,134019,142473,151104)

# pH cores
pdf("core_compositeADH_pH.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(5,8), ylab="pH",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(con15SP$pH, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$pH, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(con15WIN$pH, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$pH, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
pH_meansSP <-as.vector(tapply(gr15SP$pH, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$pH, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
#abline(h=0, col="black", lwd=2)

# WIN points and lines
pH_meansWIN <-as.vector(tapply(gr15WIN$pH, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$pH, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# EC cores
pdf("core_compositeADH_EC.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,1050), ylab="EC",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
EC_meansSPcon <-as.vector(tapply(con15SP$EC, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, EC_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$EC, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, EC_meansSPcon, sample_ADHSP, EC_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, EC_meansSPcon, sample_ADHSP, EC_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
EC_meansWINcon <-as.vector(tapply(con15WIN$EC, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, EC_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$EC, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, EC_meansWINcon, sample_ADHWIN, EC_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, EC_meansWINcon, sample_ADHWIN, EC_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
EC_meansSP <-as.vector(tapply(gr15SP$EC, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, EC_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$EC, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, EC_meansSP, sample_ADHSP, EC_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, EC_meansSP, sample_ADHSP, EC_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
#abline(h=0, col="black", lwd=2)

# WIN points and lines
EC_meansWIN <-as.vector(tapply(gr15WIN$EC, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, EC_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$EC, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, EC_meansWIN, sample_ADHWIN, EC_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, EC_meansWIN, sample_ADHWIN, EC_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# CO2 cores
pdf("core_compositeADH_CO2.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,70), ylab="CO2",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
CO2_meansSPcon <-as.vector(tapply(con15SP$CO2.umol.gdw.soil.1.day.1, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, CO2_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$CO2.umol.gdw.soil.1.day.1, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, CO2_meansSPcon, sample_ADHSP, CO2_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, CO2_meansSPcon, sample_ADHSP, CO2_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
CO2_meansWINcon <-as.vector(tapply(con15WIN$CO2.umol.gdw.soil.1.day.1, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, CO2_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$CO2.umol.gdw.soil.1.day.1, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, CO2_meansWINcon, sample_ADHWIN, CO2_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, CO2_meansWINcon, sample_ADHWIN, CO2_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
CO2_meansSP <-as.vector(tapply(gr15SP$CO2.umol.gdw.soil.1.day.1, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, CO2_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$CO2.umol.gdw.soil.1.day.1, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, CO2_meansSP, sample_ADHSP, CO2_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, CO2_meansSP, sample_ADHSP, CO2_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
CO2_meansWIN <-as.vector(tapply(gr15WIN$CO2.umol.gdw.soil.1.day.1, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, CO2_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$CO2.umol.gdw.soil.1.day.1, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, CO2_meansWIN, sample_ADHWIN, CO2_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, CO2_meansWIN, sample_ADHWIN, CO2_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# DO cores
pdf("core_compositeADH_DO.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,100), ylab="O2",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
DO_meansSPcon <-as.vector(tapply(con15SP$DO, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, DO_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$DO, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, DO_meansSPcon, sample_ADHSP, DO_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, DO_meansSPcon, sample_ADHSP, DO_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
DO_meansWINcon <-as.vector(tapply(con15WIN$DO, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, DO_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$DO, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, DO_meansWINcon, sample_ADHWIN, DO_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, DO_meansWINcon, sample_ADHWIN, DO_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
DO_meansSP <-as.vector(tapply(gr15SP$DO, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, DO_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$DO, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, DO_meansSP, sample_ADHSP, DO_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, DO_meansSP, sample_ADHSP, DO_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
#abline(h=0, col="black", lwd=2)

# WIN points and lines
DO_meansWIN <-as.vector(tapply(gr15WIN$DO, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, DO_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$DO, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, DO_meansWIN, sample_ADHWIN, DO_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, DO_meansWIN, sample_ADHWIN, DO_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# NH4 cores
pdf("core_compositeADH_NH4.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,1.1), ylab="NH4",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
NH4_meansSPcon <-as.vector(tapply(con15SP$NH4.mg.per.gdw, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NH4_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$NH4.mg.per.gdw, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NH4_meansSPcon, sample_ADHSP, NH4_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, NH4_meansSPcon, sample_ADHSP, NH4_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
NH4_meansWINcon <-as.vector(tapply(con15WIN$NH4.mg.per.gdw, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NH4_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$NH4.mg.per.gdw, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NH4_meansWINcon, sample_ADHWIN, NH4_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, NH4_meansWINcon, sample_ADHWIN, NH4_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
NH4_meansSP <-as.vector(tapply(gr15SP$NH4.mg.per.gdw, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NH4_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$NH4.mg.per.gdw, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NH4_meansSP, sample_ADHSP, NH4_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, NH4_meansSP, sample_ADHSP, NH4_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
NH4_meansWIN <-as.vector(tapply(gr15WIN$NH4.mg.per.gdw, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NH4_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$NH4.mg.per.gdw, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NH4_meansWIN, sample_ADHWIN, NH4_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, NH4_meansWIN, sample_ADHWIN, NH4_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# NO3 cores
pdf("core_compositeADH_NO3.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,200), ylab="NO3",las=1, cex.axis=1, xlab = "Soil ADH", main= "core", col="white")

# SP points and lines for the controls
NO3_meansSPcon <-as.vector(tapply(con15SP$NO3.ug.per.gdw, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NO3_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$NO3.ug.per.gdw, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NO3_meansSPcon, sample_ADHSP, NO3_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, NO3_meansSPcon, sample_ADHSP, NO3_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
NO3_meansWINcon <-as.vector(tapply(con15WIN$NO3.ug.per.gdw, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NO3_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$NO3.ug.per.gdw, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NO3_meansWINcon, sample_ADHWIN, NO3_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, NO3_meansWINcon, sample_ADHWIN, NO3_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
NO3_meansSP <-as.vector(tapply(gr15SP$NO3.ug.per.gdw, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NO3_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$NO3.ug.per.gdw, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NO3_meansSP, sample_ADHSP, NO3_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, NO3_meansSP, sample_ADHSP, NO3_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
NO3_meansWIN <-as.vector(tapply(gr15WIN$NO3.ug.per.gdw, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NO3_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$NO3.ug.per.gdw, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NO3_meansWIN, sample_ADHWIN, NO3_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, NO3_meansWIN, sample_ADHWIN, NO3_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

#****************************

pdf("composites.legend.pdf", width = 5,height=5)
plot(1, type="n", xlim=c(0,160000),ylim=c(-2,10), ylab="",las=1, cex.axis=1, xlab = "", col="white")
legendtxt <- c("Spring","Winter","Controls")
legend("right", legend = legendtxt, col = c("red2","blue2","black"), lty = c(1,1,1),lwd = c(4,4,1), cex=1.2)
dev.off()

#********************************
#COMPOSITE INTERFACES
#******************************** 

# pH interfaces
pdf("int_compositeADH_pH.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(5,8), ylab="pH",las=1, cex.axis=1, xlab = "Soil ADH", main= "interface", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(conintSP$pH, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$pH, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(conintWIN$pH, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$pH, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
pH_meansSP <-as.vector(tapply(grintSP$pH, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$pH, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(grintWIN$pH, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$pH, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# EC interfaces
pdf("int_compositeADH_EC.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,2500), ylab="EC",las=1, cex.axis=1, xlab = "Soil ADH", main= "interface", col="white")

# SP points and lines for the controls
EC_meansSPcon <-as.vector(tapply(conintSP$EC, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, EC_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$EC, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, EC_meansSPcon, sample_ADHSP, EC_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, EC_meansSPcon, sample_ADHSP, EC_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
EC_meansWINcon <-as.vector(tapply(conintWIN$EC, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, EC_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$EC, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, EC_meansWINcon, sample_ADHWIN, EC_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, EC_meansWINcon, sample_ADHWIN, EC_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
EC_meansSP <-as.vector(tapply(grintSP$EC, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, EC_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$EC, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, EC_meansSP, sample_ADHSP, EC_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, EC_meansSP, sample_ADHSP, EC_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
EC_meansWIN <-as.vector(tapply(grintWIN$EC, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, EC_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$EC, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, EC_meansWIN, sample_ADHWIN, EC_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, EC_meansWIN, sample_ADHWIN, EC_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# CO2 interfaces
pdf("int_compositeADH_CO2.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,120), ylab="CO2",las=1, cex.axis=1, xlab = "Soil ADH", main= "interface", col="white")

# SP points and lines for the controls
CO2_meansSPcon <-as.vector(tapply(conintSP$CO2.umol.gdw.soil.1.day.1, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, CO2_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$CO2.umol.gdw.soil.1.day.1, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, CO2_meansSPcon, sample_ADHSP, CO2_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, CO2_meansSPcon, sample_ADHSP, CO2_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
CO2_meansWINcon <-as.vector(tapply(conintWIN$CO2.umol.gdw.soil.1.day.1, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, CO2_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$CO2.umol.gdw.soil.1.day.1, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, CO2_meansWINcon, sample_ADHWIN, CO2_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, CO2_meansWINcon, sample_ADHWIN, CO2_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
CO2_meansSP <-as.vector(tapply(grintSP$CO2.umol.gdw.soil.1.day.1, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, CO2_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$CO2.umol.gdw.soil.1.day.1, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, CO2_meansSP, sample_ADHSP, CO2_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, CO2_meansSP, sample_ADHSP, CO2_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
CO2_meansWIN <-as.vector(tapply(grintWIN$CO2.umol.gdw.soil.1.day.1, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, CO2_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$CO2.umol.gdw.soil.1.day.1, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, CO2_meansWIN, sample_ADHWIN, CO2_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, CO2_meansWIN, sample_ADHWIN, CO2_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# NH4 interfaces
pdf("int_compositeADH_NH4.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,9), ylab="NH4",las=1, cex.axis=1, xlab = "Soil ADH", main= "interface", col="white")

# SP points and lines for the controls
NH4_meansSPcon <-as.vector(tapply(conintSP$NH4.mg.per.gdw, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NH4_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$NH4.mg.per.gdw, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NH4_meansSPcon, sample_ADHSP, NH4_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, NH4_meansSPcon, sample_ADHSP, NH4_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
NH4_meansWINcon <-as.vector(tapply(conintWIN$NH4.mg.per.gdw, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NH4_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$NH4.mg.per.gdw, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NH4_meansWINcon, sample_ADHWIN, NH4_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, NH4_meansWINcon, sample_ADHWIN, NH4_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
NH4_meansSP <-as.vector(tapply(grintSP$NH4.mg.per.gdw, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NH4_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$NH4.mg.per.gdw, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NH4_meansSP, sample_ADHSP, NH4_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, NH4_meansSP, sample_ADHSP, NH4_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
NH4_meansWIN <-as.vector(tapply(grintWIN$NH4.mg.per.gdw, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NH4_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$NH4.mg.per.gdw, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NH4_meansWIN, sample_ADHWIN, NH4_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, NH4_meansWIN, sample_ADHWIN, NH4_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

# NO3 interfaces
pdf("int_compositeADH_NO3.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,400), ylab="NO3",las=1, cex.axis=1, xlab = "Soil ADH", main= "interface", col="white")

# SP points and lines for the controls
NO3_meansSPcon <-as.vector(tapply(conintSP$NO3.ug.per.gdw, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NO3_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$NO3.ug.per.gdw, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NO3_meansSPcon, sample_ADHSP, NO3_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, NO3_meansSPcon, sample_ADHSP, NO3_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
NO3_meansWINcon <-as.vector(tapply(conintWIN$NO3.ug.per.gdw, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NO3_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$NO3.ug.per.gdw, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NO3_meansWINcon, sample_ADHWIN, NO3_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, NO3_meansWINcon, sample_ADHWIN, NO3_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
NO3_meansSP <-as.vector(tapply(grintSP$NO3.ug.per.gdw, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, NO3_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$NO3.ug.per.gdw, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, NO3_meansSP, sample_ADHSP, NO3_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, NO3_meansSP, sample_ADHSP, NO3_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
NO3_meansWIN <-as.vector(tapply(grintWIN$NO3.ug.per.gdw, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, NO3_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$NO3.ug.per.gdw, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, NO3_meansWIN, sample_ADHWIN, NO3_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, NO3_meansWIN, sample_ADHWIN, NO3_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

dev.off()

#*********************************
#COMPOSITE CORES TC, TN, and C:N
#*********************************

CN_master <- read.csv(file="CN_updatable.csv")# stringsAsFactors = FALSE)
str(CN_master)
CN_SP <- filter(CN_master, Season == "spring")
CN_WIN <- filter(CN_master, Season == "winter")

conintSP <-filter(CN_SP,Location == "conint")
con15SP <-filter(CN_SP,Location == "con15")
grintSP <-filter(CN_SP,Location == "grint")
gr15SP <-filter(CN_SP,Location == "gr15")

conintWIN <-filter(CN_WIN,Location == "conint")
con15WIN <-filter(CN_WIN,Location == "con15")
grintWIN <-filter(CN_WIN,Location == "grint")
gr15WIN <-filter(CN_WIN,Location == "gr15")

# COMPOSITE BY ADH
# CORES TC

pdf("TC_core_abline.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,22), ylab="%C",las=1, cex.axis=1, xlab = "Soil ADH", main= "Total C (%C) - cores", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(con15SP$C..percent, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$C..percent, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(con15WIN$C..percent, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$C..percent, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
pH_meansSP <-as.vector(tapply(gr15SP$C..percent, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$C..percent, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(gr15WIN$C..percent, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$C..percent, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)

dev.off()
#*************************************************************
# TOTAL N - CORES
pdf("TN_core_abline.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,2.2), ylab="%N",las=1, cex.axis=1, xlab = "Soil ADH", main= "Total N (%N) - cores", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(con15SP$N..percent, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$N..percent, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(con15WIN$N..percent, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$N..percent, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
pH_meansSP <-as.vector(tapply(gr15SP$N..percent, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$N..percent, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(gr15WIN$N..percent, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$N..percent, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)
dev.off()

#******************************
# C:N CORES
pdf("CN_core_abline.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(9,21), ylab="C:N",las=1, cex.axis=1, xlab = "Soil ADH", main= "C:N - cores", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(con15SP$CN..ratio, INDEX=con15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$CN..ratio, INDEX=con15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(con15WIN$CN..ratio, INDEX=con15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$CN..ratio, INDEX=con15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
pH_meansSP <-as.vector(tapply(gr15SP$CN..ratio, INDEX=gr15SP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$CN..ratio, INDEX=gr15SP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(gr15WIN$CN..ratio, INDEX=gr15WIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$CN..ratio, INDEX=gr15WIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)
dev.off()

#****************************

# INTERFACES TC
pdf("TC_int_abline_test.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(5,40), ylab="%C",las=1, cex.axis=1, xlab = "Soil ADH", main= "Total C (%C) - interfaces", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(conintSP$C..percent, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$C..percent, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(conintWIN$C..percent, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$C..percent, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
pH_meansSP <-as.vector(tapply(grintSP$C..percent, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
#points (sample_ADHSP, pH_meansSP,bg="red",cex=1.2, pch=21, col="red2")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$C..percent, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(grintWIN$C..percent, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
#points (sample_ADHWIN, pH_meansWIN,bg="blue",cex=1.2, pch=21, col="blue2")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$C..percent, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)

dev.off()
#*************************************************************
# TOTAL N - INTERFACES
pdf("TN_int_abline_test.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0.5,2.5), ylab="%N",las=1, cex.axis=1, xlab = "Soil ADH", main= "Total N (%N) - interfaces", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(conintSP$N..percent, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$N..percent, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(conintWIN$N..percent, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$N..percent, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
pH_meansSP <-as.vector(tapply(grintSP$N..percent, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$N..percent, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(grintWIN$N..percent, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$N..percent, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)

dev.off()
#******************************
# C:N INTERFACES
pdf("CN_int_abline_test.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(9,30), ylab="C:N",las=1, cex.axis=1, xlab = "Soil ADH", main= "C:N - interfaces", col="white")

# SP points and lines for the controls
pH_meansSPcon <-as.vector(tapply(conintSP$CN..ratio, INDEX=conintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$CN..ratio, INDEX=conintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, pH_meansSPcon, sample_ADHSP, pH_meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")

# WIN points and lines for the controls
pH_meansWINcon <-as.vector(tapply(conintWIN$CN..ratio, INDEX=conintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$CN..ratio, INDEX=conintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, pH_meansWINcon, sample_ADHWIN, pH_meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the interface samples
pH_meansSP <-as.vector(tapply(grintSP$CN..ratio, INDEX=grintSP$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, pH_meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$CN..ratio, INDEX=grintSP$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, pH_meansSP, sample_ADHSP, pH_meansSP - errorbarsSP, length=0.03, angle=90,col="red2")

# WIN points and lines
pH_meansWIN <-as.vector(tapply(grintWIN$CN..ratio, INDEX=grintWIN$Study.day, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, pH_meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$CN..ratio, INDEX=grintWIN$Study.day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, pH_meansWIN, sample_ADHWIN, pH_meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")

abline(v=5362, col="red2", lwd=1)
abline(v=98759, col="red2", lwd=1)
abline(v=12127, col="blue2", lwd=1)
abline(v=87883, col="blue2", lwd=1)

dev.off()

#****************************************************************
# Kruskal-Wallace
#****************************************************************
library(dplyr)

pHECetcSP <- read.csv(file="pH_EC_etc_SP.csv")# stringsAsFactors = FALSE)
pHECetcWIN <- read.csv(file="pH_EC_etc_WIN.csv")# stringsAsFactors = FALSE)
CN_master <- read.csv(file="CN_updatable.csv")# stringsAsFactors = FALSE)
CN_SP <- filter(CN_master, Season == "spring")
CN_WIN <- filter(CN_master, Season == "winter")

# Kruskal-Wallace tests - SP cores

con15SP <-filter(pHECetcSP,Location == "con15")
gr15SP <-filter(pHECetcSP,Location == "gr15")
core_data <- rbind(con15SP, gr15SP)

kruskal.test(core_data$pH, core_data$Location)
kruskal.test(core_data$EC, core_data$Location)
kruskal.test(core_data$DO, core_data$Location)
kruskal.test(core_data$CO2.umol.gdw.soil.1.day.1, core_data$Location)
kruskal.test(core_data$NH4.mg.per.gdw, core_data$Location)
kruskal.test(core_data$NO3.ug.per.gdw, core_data$Location)

con15SP <- filter(CN_SP,Location == "con15")
gr15SP <- filter(CN_SP,Location == "gr15")
core_data <- rbind(con15SP,gr15SP)

kruskal.test(core_data$N..percent, core_data$Location)
kruskal.test(core_data$C..percent, core_data$Location)
kruskal.test(core_data$CN..ratio, core_data$Location)

# Kruskal-Wallace tests - SP interfaces

conintSP <-filter(pHECetcSP,Location == "conint")
grintSP <-filter(pHECetcSP,Location == "grint")
int_data <- rbind(conintSP,grintSP)

kruskal.test(int_data$pH, int_data$Location)
kruskal.test(int_data$EC, int_data$Location)
kruskal.test(int_data$CO2.umol.gdw.soil.1.day.1, int_data$Location)
kruskal.test(int_data$NH4.mg.per.gdw, int_data$Location)
kruskal.test(int_data$NO3.ug.per.gdw, int_data$Location)

conintSP <- filter(CN_SP,Location == "conint")
grintSP <- filter(CN_SP,Location == "grint")
int_data <- rbind(conintSP,grintSP)

kruskal.test(int_data$N..percent, int_data$Location)
kruskal.test(int_data$C..percent, int_data$Location)
kruskal.test(int_data$CN..ratio, int_data$Location)

# Kruskal-Wallace tests - WIN cores

con15WIN <-filter(pHECetcWIN,Location == "con15")
gr15WIN <-filter(pHECetcWIN,Location == "gr15")
core_data <- rbind(con15WIN, gr15WIN)

kruskal.test(core_data$pH, core_data$Location)
kruskal.test(core_data$EC, core_data$Location)
kruskal.test(core_data$DO, core_data$Location)
kruskal.test(core_data$CO2.umol.gdw.soil.1.day.1, core_data$Location)
kruskal.test(core_data$NH4.mg.per.gdw, core_data$Location)
kruskal.test(core_data$NO3.ug.per.gdw, core_data$Location)

con15WIN <- filter(CN_WIN,Location == "con15")
gr15WIN <- filter(CN_WIN,Location == "gr15")
core_data <- rbind(con15WIN,gr15WIN)

kruskal.test(core_data$N..percent, core_data$Location)
kruskal.test(core_data$C..percent, core_data$Location)
kruskal.test(core_data$CN..ratio, core_data$Location)

# Kruskal-Wallace tests - WIN interfaces

conintWIN <-filter(pHECetcWIN,Location == "conint")
grintWIN <-filter(pHECetcWIN,Location == "grint")
int_data <- rbind(conintWIN,grintWIN)

kruskal.test(int_data$pH, int_data$Location)
kruskal.test(int_data$EC, int_data$Location)
kruskal.test(int_data$CO2.umol.gdw.soil.1.day.1, int_data$Location)
kruskal.test(int_data$NH4.mg.per.gdw, int_data$Location)
kruskal.test(int_data$NO3.ug.per.gdw, int_data$Location)

conintWIN <- filter(CN_WIN,Location == "conint")
grintWIN <- filter(CN_WIN,Location == "grint")
int_data <- rbind(conintWIN,grintWIN)

kruskal.test(int_data$N..percent, int_data$Location)
kruskal.test(int_data$C..percent, int_data$Location)
kruskal.test(int_data$CN..ratio, int_data$Location)

#****************************************************************
# T tests
#****************************************************************

library(dplyr)

# SP cores and interfaces (pH, EC, CO2, DO, NH4, NO3)
pHECetc <- read.csv(file="pH_EC_etc_SP.csv")# stringsAsFactors = FALSE)
conint <- filter(pHECetc,Location == "conint")
con15 <- filter(pHECetc,Location == "con15")
grint <- filter(pHECetc,Location == "grint")
gr15 <- filter(pHECetc,Location == "gr15")

# to run core data
core_data <- rbind(con15, gr15)

T_day<- filter(core_data, Study.day==376) #adjust study day
T_con <- filter(T_day, Location=="con15") 
T_treat <-filter(T_day, Location=="gr15") 

t.test(T_con$pH,T_treat$pH) # pH
t.test(T_con$EC,T_treat$EC) # EC
t.test(T_con$DO,T_treat$DO) # DO
t.test(T_con$CO2.umol.gdw.soil.1.day.1,T_treat$CO2.umol.gdw.soil.1.day.1) # CO2
t.test(T_con$NH4.mg.per.gdw,T_treat$NH4.mg.per.gdw) # NH4
t.test(T_con$NO3.ug.per.gdw,T_treat$NO3.ug.per.gdw) # NO3

# to run interface data
int_data <- rbind(conint,grint)

T_day<- filter(int_data, Study.day==376) #adjust study day
T_con <- filter(T_day, Location=="conint") 
T_treat <-filter(T_day, Location=="grint") 

t.test(T_con$pH,T_treat$pH) # pH
t.test(T_con$EC,T_treat$EC) # EC
t.test(T_con$CO2.umol.gdw.soil.1.day.1,T_treat$CO2.umol.gdw.soil.1.day.1) # CO2
t.test(T_con$NH4.mg.per.gdw,T_treat$NH4.mg.per.gdw) # NH4
t.test(T_con$NO3.ug.per.gdw,T_treat$NO3.ug.per.gdw) # NO3

#***********************************************************
# WIN cores and interfaces (pH, EC, CO2, DO, NH4, NO3)
pHECetc <- read.csv(file="pH_EC_etc_WIN.csv")# stringsAsFactors = FALSE)
conint <- filter(pHECetc,Location == "conint")
con15 <- filter(pHECetc,Location == "con15")
grint <- filter(pHECetc,Location == "grint")
gr15 <- filter(pHECetc,Location == "gr15")

# to run core data
core_data <- rbind(con15, gr15)

T_day<- filter(core_data, Study.day==384) #adjust study day
T_con <- filter(T_day, Location=="con15") 
T_treat <-filter(T_day, Location=="gr15") 

t.test(T_con$pH,T_treat$pH) # pH
t.test(T_con$EC,T_treat$EC) # EC
t.test(T_con$DO,T_treat$DO) # DO
t.test(T_con$CO2.umol.gdw.soil.1.day.1,T_treat$CO2.umol.gdw.soil.1.day.1) # CO2
t.test(T_con$NH4.mg.per.gdw,T_treat$NH4.mg.per.gdw) # NH4
t.test(T_con$NO3.ug.per.gdw,T_treat$NO3.ug.per.gdw) # NO3

# to run interface data
int_data <- rbind(conint,grint)

T_day<- filter(int_data, Study.day==384) #adjust study day
T_con <- filter(T_day, Location=="conint") 
T_treat <-filter(T_day, Location=="grint") 

t.test(T_con$pH,T_treat$pH) # pH
t.test(T_con$EC,T_treat$EC) # EC
t.test(T_con$CO2.umol.gdw.soil.1.day.1,T_treat$CO2.umol.gdw.soil.1.day.1) # CO2
t.test(T_con$NH4.mg.per.gdw,T_treat$NH4.mg.per.gdw) # NH4
t.test(T_con$NO3.ug.per.gdw,T_treat$NO3.ug.per.gdw) # NO3

#***********************************************************
CN_master <- read.csv(file="CN_updatable.csv")# stringsAsFactors = FALSE)

# SP cores and interfaces (TC, TN, C:N)
CN_SP <- filter(CN_master, Season == "spring")
conint <- filter(CN_SP,Location == "conint")
con15 <- filter(CN_SP,Location == "con15")
grint <- filter(CN_SP,Location == "grint")
gr15 <- filter(CN_SP,Location == "gr15")

# to run core data
core_data <- rbind(con15, gr15)

T_day<- filter(core_data, Study.day==376) #adjust study day
T_con <- filter(T_day, Location=="con15") 
T_treat <-filter(T_day, Location=="gr15")

t.test(T_con$N..percent,T_treat$N..percent) # TN
t.test(T_con$C..percent,T_treat$C..percent) # TC
t.test(T_con$CN..ratio,T_treat$CN..ratio) # CN

# to run interface data
int_data <- rbind(conint,grint)

T_day<- filter(int_data, Study.day==376) #adjust study day
T_con <- filter(T_day, Location=="conint") 
T_treat <-filter(T_day, Location=="grint")

t.test(T_con$N..percent,T_treat$N..percent) # TN
t.test(T_con$C..percent,T_treat$C..percent) # TC
t.test(T_con$CN..ratio,T_treat$CN..ratio) # CN

#**********************
# WIN cores and interfaces (TC, TN, C:N)
CN_WIN <- filter(CN_master, Season == "winter")
conint <- filter(TN_TC_CN_WIN,Location == "conint")
con15 <- filter(TN_TC_CN_WIN,Location == "con15")
grint <- filter(TN_TC_CN_WIN,Location == "grint")
gr15 <- filter(TN_TC_CN_WIN,Location == "gr15")

# to run core data
core_data <- rbind(con15, gr15)

T_day<- filter(core_data, Study.day==384) #adjust study day
T_con <- filter(T_day, Location=="con15") 
T_treat <-filter(T_day, Location=="gr15")

t.test(T_con$N..percent,T_treat$N..percent) # TN
t.test(T_con$C..percent,T_treat$C..percent) # TC
t.test(T_con$CN..ratio,T_treat$CN..ratio) # CN

# to run interface data
int_data <- rbind(conint,grint)

T_day<- filter(int_data, Study.day==384) #adjust study day
T_con <- filter(T_day, Location=="conint") 
T_treat <-filter(T_day, Location=="grint")

t.test(T_con$N..percent,T_treat$N..percent) # TN
t.test(T_con$C..percent,T_treat$C..percent) # TC
t.test(T_con$CN..ratio,T_treat$CN..ratio) # CN


2..mol.per.gdw~ ADH_internal, data=int_data) #CO2
int_mod <- lm(NH4.mg.per.gdw~ ADH_internal, data=int_data)# NH4
int_mod <- lm(NO3.ug.per.gdw~ ADH_internal, data=int_data)# NO3
int_mod <- lm(Ratio~ADH_internal, data=int_data)#Abun
summary(int_mod)

library(dplyr)
TBS <- read.csv(file="TBS_ADD.csv")# stringsAsFactors = FALSE)
TBS_SP <- filter(TBS,Study == "spring")
TBS_WIN <- filter(TBS,Study == "winter")

#************************
# TBS as a function of ADD
#************************
library(dplyr)
TBS <- read.csv(file="TBS_ADD.csv")# stringsAsFactors = FALSE)
TBS_SP <- filter(TBS,Study == "spring")
TBS_WIN <- filter(TBS,Study == "winter")

#pdf("TBS_ADD.pdf", width = 6, height = 4.8, useDingbats = FALSE)
png("TBS_ADD.png", width = 6,height=4.8, units = 'in',res = 600)

plot(1, type="n", xlim=c(0,6000),ylim=c(0,35), ylab="TBS",las=1, cex.axis=1, xlab = "ADD", main= "TBS by ADD", col="white")
# spring scores
points (TBS_SP$ADD, TBS_SP$SP1_WIN1_TBS,bg="pink",cex=1.2, pch=21, col="black")
points (TBS_SP$ADD, TBS_SP$SP2_WIN2_TBS,bg="lightcoral",cex=1.2, pch=21, col="black")
points (TBS_SP$ADD, TBS_SP$SP3_WIN3_TBS,bg="red2",cex=1.2, pch=21, col="black")
# winter scores
points (TBS_WIN$ADD, TBS_WIN$SP1_WIN1_TBS,bg="lightblue1",cex=1.2, pch=21, col="black")
points (TBS_WIN$ADD, TBS_WIN$SP2_WIN2_TBS,bg="royalblue1",cex=1.2, pch=21, col="black")
points (TBS_WIN$ADD, TBS_WIN$SP3_WIN3_TBS,bg="blue3",cex=1.2, pch=21, col="black")
dev.off()

pdf("TBS_legend.pdf", width = 3.25, height = 4, useDingbats = FALSE)
#png("TBS_legend.png", width = 3.25,height=4, units = 'in',res = 600)
plot(-0.5:12.5, -0.5:12.5, type="n", xaxt="n",bty="n", yaxt= "n",  ylab="", xlab = "")
points (1, 12, pch=21, cex=1.6, col="black", bg="pink", lwd=1)
text(2,11.7, "SP1", cex=0.9, adj = c(0,0))
points (1, 11, pch=21, cex=1.6, col="black", bg="lightcoral", lwd=1)
text(2,10.7, "SP2", cex=0.9,adj = c(0,0))
points (1, 10, pch=21, cex=1.6, col="black", bg="red2", lwd=1)
text(2,9.7, "SP3", cex=0.9,adj = c(0,0))
points (1, 9, pch=21, cex=1.6, col="black", bg="lightblue1", lwd=1)
text(2,8.7, "WIN1", cex=0.9,adj = c(0,0))
points (1, 8, pch=21, cex=1.6, col="black", bg="royalblue1", lwd=1)
text(2,7.7, "WIN2", cex=0.9,adj = c(0,0))
points (1, 7, pch=21, cex=1.6, col="black", bg="blue3", lwd=1)
text(2, 6.7, "WIN3", cex=0.9,adj = c(0,0))
dev.off()

#**********************************************************************************
# Microbial data
#**********************************************************************************
# This is the Phyloseq pipeline to process Mothur outputs
# Object names used in the following code derive from output files
# Code adapted from: Berry (2016)  http://deneflab.github.io/MicrobeMiseq/demos/mothur_2_phyloseq.html

# Load phyloseq
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")

library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(RColorBrewer)

#*************************************************************
# IMPORT MOTHUR DATA AND SET UP PHYLOSEQ OBJECT FOR SP SERIES
#*************************************************************
# First, create the variables for the imported data (first 3 are ITS, second 3 are 16S)
sharedfile <- "NIJARFSP.trim.contigs.pcr.good.unique.precluster.pick.agc.shared"
taxfile <- "NIJARFSP.trim.contigs.pcr.good.unique.precluster.pick.agc.0.05.cons.taxonomy"
metadata <- read.csv(file="SP_ITS_metadata.csv")

sharedfile <- "NIJARFSP16S.shared"
taxfile <- "NIJARFSP16S.taxonomy"
metadata <- read.csv(file="SP_16S_metadata.csv")

# Now, import the mothur data
mothur_data <- import_mothur(mothur_shared_file = sharedfile, mothur_constaxonomy_file = taxfile)

# import the metadata file as a phyloseq object
metadata <- sample_data(metadata)
# In the metadata file set Sample_name as the row name
rownames(metadata) <- metadata$Sample_name

# Merge metadata file into phyloseq object created above
mothur_merged <-merge_phyloseq(mothur_data, metadata)
metadata
# Inspect column names of taxonomy file 
colnames(tax_table(mothur_merged))
# Current names are "Rank 1",...through "Rank 7"
# Rename them to something more accessible:
colnames(tax_table(mothur_merged))<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species") #ITS UNITE database
colnames(tax_table(mothur_merged))<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus") #16S Silva database

mothur_merged #This shows taxa and samples
sample_names(mothur_merged) # This shows what our samples are called and allows us to remove items as necessary (blanks, etc)
mothur_merged1a <- subset_samples(mothur_merged, Depth !="blank") #removes the sample blanks
mothur_merged1a # shows how many taxa and OTUs remain

#**************************************
# prune and alpha diversity for SP ITS
#**************************************
#Now let's prune out the singletons SP ITS
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 1}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_SP_ITS_minussingletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_SP_ITS_minussingletons$SampleID = row.names(alphadiv_SP_ITS_minussingletons)
write.csv(alphadiv_SP_ITS_minussingletons, "alphadiv_SP_ITS_minussingletons.csv", row.names = FALSE) # diversity data can now be used for downstream analyses

#Now let's prune the doubletons SP ITS
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 2}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_SP_ITS_minusdoubletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_SP_ITS_minusdoubletons$SampleID = row.names(alphadiv_SP_ITS_minusdoubletons)
write.csv(alphadiv_SP_ITS_minusdoubletons, "alphadiv_SP_ITS_minusdoubletons.csv", row.names = FALSE) #diversity data can now be used for downstream analyses

#**************************************
# prune and alpha diversity for SP 16S
#**************************************
#Now let's prune out the singletons SP 16S
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 1}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_SP_16S_minussingletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_SP_16S_minussingletons$SampleID = row.names(alphadiv_SP_16S_minussingletons)
write.csv(alphadiv_SP_16S_minussingletons, "alphadiv_SP_16S_minussingletons.csv", row.names = FALSE)

#Now let's prune the doubletons SP 16S
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 2}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_SP_16S_minusdoubletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_SP_16S_minusdoubletons$SampleID = row.names(alphadiv_SP_16S_minusdoubletons)
write.csv(alphadiv_SP_16S_minusdoubletons, "alphadiv_SP_16S_minusdoubletons.csv", row.names = FALSE)
#**************************************************************************************************************

# Let's look at the read-count distribution of our samples
#sample_sum_df <- data.frame(sum = sample_sums(mothur_merged))
sample_sum_df <- data.frame(sum = sample_sums(mo_merge_pruned))

# This will show a histogram of the sample read counts
ggplot(sample_sum_df, aes(x=sum)) + 
  geom_histogram(color = "black", fill = "cadetblue", binwidth = 2500) +
  ggtitle("sample seqencing depth distribution") +
  xlab("read counts") +
  theme(axis.title.y = element_blank())

#******************************************************************************
# SP DATA SETUP IS NOW FINISHED -- FROM HERE WE GO TO THE VARIOUS ANALYSES:
# STACKED BAR PLOTS 
# CAP 
#******************************************************************************

#*************************************************************
# IMPORT MOTHUR DATA AND SET UP PHYLOSEQ OBJECT FOR WIN SERIES
#*************************************************************
# First, create the variables for the imported data
sharedfile <- "NIJARFWIN.trim.contigs.pcr.good.unique.precluster.pick.agc.shared"
taxfile <- "NIJARFWIN.trim.contigs.pcr.good.unique.precluster.pick.agc.0.05.cons.taxonomy"
metadata <- read.csv(file="WIN_ITS_metadata.csv")

sharedfile <- "NIJARFWIN16S.shared"
taxfile <- "NIJARFWIN16S.taxonomy"
metadata <- read.csv(file="WIN_16S_metadata.csv")

# Now, import the mothur data
mothur_data <- import_mothur(mothur_shared_file = sharedfile, mothur_constaxonomy_file = taxfile)

# import the metadata file as a phyloseq object
metadata <- sample_data(metadata)
# In the metadata file set Sample_name as the row name
rownames(metadata) <- metadata$Sample_name

# Merge metadata file into phyloseq object created above
mothur_merged <-merge_phyloseq(mothur_data, metadata)

colnames(tax_table(mothur_merged))
# Current names are "Rank 1",...through "Rank 7"
# Rename columns to something more accessible:
colnames(tax_table(mothur_merged))<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species") #ITS UNITE database
colnames(tax_table(mothur_merged))<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus") #16S Silva database

mothur_merged 
sample_names(mothur_merged) 
mothur_merged1a <- subset_samples(mothur_merged, Depth !="blank") #removes the sample blanks
mothur_merged1a 
sample_names(mothur_merged1a)

#**************************************
# prune and alpha diversity for WIN ITS
#**************************************
#Now let's prune out the singletons WIN ITS
mo_merge_pruned <- filter_taxa(mothur_merged1, function (x) {sum(x > 0) > 1}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_WIN_ITS_minussingletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_WIN_ITS_minussingletons$SampleID = row.names(alphadiv_WIN_ITS_minussingletons)
write.csv(alphadiv_WIN_ITS_minussingletons, "alphadiv_WIN_ITS_minussingletons.csv", row.names = FALSE)

#Now let's prune the doubletons WIN ITS
mo_merge_pruned <- filter_taxa(mothur_merged1, function (x) {sum(x > 0) > 2}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_WIN_ITS_minusdoubletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_WIN_ITS_minusdoubletons$SampleID = row.names(alphadiv_WIN_ITS_minusdoubletons)
write.csv(alphadiv_WIN_ITS_minusdoubletons, "alphadiv_WIN_ITS_minusdoubletons.csv", row.names = FALSE)

#**************************************
# prune and alpha diversity for WIN 16S
#**************************************
#Now let's prune out the singletons WIN 16S
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 1}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_WIN_16S_minussingletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_WIN_16S_minussingletons$SampleID = row.names(alphadiv_WIN_16S_minussingletons)
#write.csv(alphadiv_WIN_16S_minussingletons, "alphadiv_WIN_16S_minussingletons.csv", row.names = FALSE) This can be used in place of doubletons

#Now let's prune the doubletons WIN 16S
mo_merge_pruned <- filter_taxa(mothur_merged1a, function (x) {sum(x > 0) > 2}, prune=TRUE)
mo_merge_pruned 
sample_names(mo_merge_pruned)
alphadiv_WIN_16S_minusdoubletons <- data.frame(estimate_richness(mo_merge_pruned, measures=c("Observed", "Shannon", "InvSimpson", "Chao1")))
alphadiv_WIN_16S_minusdoubletons$SampleID = row.names(alphadiv_WIN_16S_minusdoubletons)
write.csv(alphadiv_WIN_16S_minusdoubletons, "alphadiv_WIN_16S_minusdoubletons.csv", row.names = FALSE)
#**************************************************************************************************************

# Read-count distribution of our samples
sample_sum_df <- data.frame(sum = sample_sums(mo_merge_pruned))

# This will show a histogram of the sample read counts
ggplot(sample_sum_df, aes(x=sum)) + 
  geom_histogram(color = "black", fill = "cadetblue", binwidth = 2500) +
  ggtitle("sample seqencing depth distribution") +
  xlab("read counts") +
  theme(axis.title.y = element_blank())

#******************************************************************************
# WIN DATA SETUP IS NOW FINISHED -- FROM HERE WE GO TO THE VARIOUS ANALYSES:
# STACKED BAR PLOTS 
# CAP 
#******************************************************************************
#************************************************
# STACKED BAR PLOTS - PHYLUM
#************************************************
# Colors and themes can be set by the user
# Existing color palettes may be used or individual colors can be selected to create a custom palette

#set ggplot background theme of choice
theme_set(theme_bw())

#Increase range of color palette of choice
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

# transform data to a long format for ggplot (melt)
# Optional: remove data below a given contribution to sample (2% selected)
# Location can be specified by "grave" or "control"
sortby_phylum <- mo_merge_pruned %>%
  tax_glom(taxrank = "Phylum") %>%
  transform_sample_counts(function(x) {x/sum(x)}) %>%
  psmelt() %>%
  filter(Abundance > 0.02) %>%
  filter(Location == "grave") %>%
  arrange(Phylum)
# NOTE: to avoid conflicts with phyloseq plot attributes, Sample variable has been renamed sample_Sample

# Construct the plot
#pdf(file = "16S_WINphylum_control.pdf", height = 5, width = 7)
pdf(file = "16S_WINphylum_grave.pdf", height = 5, width = 7)
#pdf(file = "16S_SPphylum_control.pdf", height = 5, width = 7)
#pdf(file = "16S_SPphylum_grave.pdf", height = 5, width = 7)
#pdf(file = "SPphylum_grave.pdf", height = 5, width = 7)
#pdf(file = "SPphylum_control.pdf", height = 5, width = 7)
#pdf(file = "WINphylum_grave.pdf", height = 5, width = 7)
#pdf(file = "WINphylum_control.pdf", height = 5, width = 7)
ggplot(sortby_phylum, aes(x = Sort1, y = Abundance/3, fill = Phylum)) +
  facet_grid(Depth~.) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  guides (fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Phyla > 2%) \n") +
  ggtitle("Phylum Composition of Winter 16S Donor Series - grave")
dev.off()

#************************************************
# STACKED BAR PLOTS - CLASS
#************************************************
sortby_class <- mo_merge_pruned %>%
  tax_glom(taxrank = "Class") %>%
  transform_sample_counts(function(x) {x/sum(x)}) %>%
  psmelt() %>%
  filter(Abundance > 0.05) %>%
  filter(Location == "grave") %>%
  arrange(Class)

# Set plot colors
mycolors <- c("#A6CEE3","#5B9EC9", "#2D82AF", "#7EBA98", "#000000", "#98D277", "#52AF43", "#6F9E4C", "#828282", "#DD9A88", "#F16667", "#F06C45", "#FE982C", "#F78620", "#D9A295", "#B294C7", "#7D54A5", "#F0EB99", "#DBB466") #This is for SP
# colors for WIN
mycolors <- c("#A6CEE3", "#3385BB", "#84BF96", "#6DBD57", "#7F9D55", "#F57C7C", "#E42622", "#FBB268", "#FE8D19", "#828282", "#000000", "#9D7BBA", "#977899", "#F3E587", "#B15928")

# Or increase range of color palette of choice
theme_set(theme_bw())
nb.cols <- 21 #this is for fungal
nb.cols <- 14 #this is for bacterial
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
print(mycolors)

# Construct the plot
pdf(file = "16S_WINclass_grave_V2.pdf", height = 6, width = 9)
#pdf(file = "16S_WINclass_control.pdf", height = 5, width = 7)
#pdf(file = "16S_SPclass_grave_V2.pdf", height = 6, width = 9)
#pdf(file = "16S_SPclass_control.pdf", height = 5, width = 7)
#pdf(file = "SPclass_grave_V2.pdf", height = 6, width = 9)
#pdf(file = "SPclass_controls.pdf", height = 5, width = 7)
#pdf(file = "WINclass_grave_V2.pdf", height = 6, width = 9)
#pdf(file = "WINclass_controls.pdf", height = 5, width = 7)
ggplot(sortby_class, aes(x = Sort1, y = Abundance/3, fill = Class)) +
  facet_grid(Depth~.) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  guides (fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Class > 5%) \n") +
  ggtitle("Class Composition of WIN 16S")
dev.off()

#************************************************
# STACKED BAR PLOTS - ORDER
#************************************************
sortby_order <- mo_merge_pruned %>%
  tax_glom(taxrank = "Order") %>%
  transform_sample_counts(function(x) {x/sum(x)}) %>%
  psmelt() %>%
  filter(Abundance > 0.05) %>%
  filter(Location == "grave") %>%
  arrange(Order)

#Increase range of color palette of choice
nb.cols <-30
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

# Construct the plot
#pdf(file = "16S_WINorder_grave.pdf", height = 8, width = 7)
#pdf(file = "16S_WINorder_control.pdf", height = 7, width = 7)
pdf(file = "16S_SPorder_grave.pdf", height = 8, width = 7)
#pdf(file = "16S_SPorder_control.pdf", height = 7, width = 7)
#pdf(file = "SPorder_controls.pdf", height = 5, width = 7)
#pdf(file = "SPorder_grave.pdf", height = 7, width = 9.8)
#pdf(file = "WINorder_controls.pdf", height = 5, width = 7)
#pdf(file = "WINorder_grave.pdf", height = 7, width = 9.8)
ggplot(sortby_order, aes(x = Sort1, y = Abundance/3, fill = Order)) +
  facet_grid(Depth~.) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  guides (fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Order > 5%) \n") +
  ggtitle("Order Composition of 16S Spring Donor Series - grave") +guides(fill=guide_legend(ncol=1))
dev.off()

#***********************************************************************
# CONSTRAINED ORDINATION (PCA) 
#***********************************************************************

# First, remove samples that we don't want (SP-ITS)
mo_merge_pruned 
sample_names(mo_merge_pruned) #This show what sample names are present
mo_merge_pruned1a <- subset_samples(mo_merge_pruned, Depth !="blank") #removes the sample blanks
mo_merge_pruned1b <- subset_samples(mo_merge_pruned1a, Sample_name !="P150_CORE1") #removes day 150 gr_15_SP1 (f:B >10)
mo_merge_pruned1c <- subset_samples(mo_merge_pruned1b, Sample_name !="T303_IN3") #removes day 303 gr_int_SP3 (f:B >10)
mo_merge_pruned2 <- subset_samples(mo_merge_pruned1c, Sample_name !="V340_CORE3") #removes day 340 gr_15_SP3 (f:B >10)
mo_merge_pruned2 
sample_names(mo_merge_pruned2) #verifies what samples are present now

# First, remove samples that we don't want (SP-16S)
mo_merge_pruned 
sample_names(mo_merge_pruned) #This show what sample names are present
mo_merge_pruned1a <- subset_samples(mo_merge_pruned, Depth !="blank") #removes the sample blanks
mo_merge_pruned1b <- subset_samples(mo_merge_pruned1a, Sample_name !="N150_CORE1") #removes day 150 gr_15_SP1 (f:B >10)
mo_merge_pruned1c <- subset_samples(mo_merge_pruned1b, Sample_name !="S303_IN3") #removes day 303 gr_int_SP3 (f:B >10)
mo_merge_pruned2 <- subset_samples(mo_merge_pruned1c, Sample_name !="T340_CORE3") #removes day 340 gr_15_SP3 (f:B >10)
mo_merge_pruned2 
sample_names(mo_merge_pruned2) #verifies what samples are present now

# First, remove samples that we don't want (WIN)
mo_merge_pruned 
sample_names(mo_merge_pruned) #This show what sample names are present
mo_merge_pruned2 <- subset_samples(mo_merge_pruned, Depth !="blank")
mo_merge_pruned2 
sample_names(mo_merge_pruned2) #verifies what samples are present now


# We now scale the reads to relative abundance (which appears to be the same formula as code above)
mo_merge_prune_scaled <- mo_merge_pruned2 %>%
  transform_sample_counts(function(x) {x/sum(x)}) 
mo_merge_prune_scaled 
sample_names(mo_merge_prune_scaled) #still the same as above
sample_variables(mo_merge_prune_scaled) #these are the metadata column headings

#*****************************************
# PERMANOVA using Vegan Adonis command
#*****************************************
set.seed(1)

# Calculate the distance matrix
SP_bray <- phyloseq::distance(mo_merge_prune_scaled, method = "bray")
WIN_bray <- phyloseq::distance(mo_merge_prune_scaled, method = "bray")

# Create a dataframe from sample_data within the phyloseq object
SPsampledf <- data.frame(sample_data(mo_merge_pruned2))
WINsampledf <- data.frame(sample_data(mo_merge_pruned2))

# Run the Adonis test to get the Permanova results
adonis(SP_bray~Sort2, data = SPsampledf) #Pr(>F)=0.001 (by Sort 2 (Study day))
#adonis(SP_bray~Stage1, data = SPsampledf) #Pr(>F)=0.001 (by Stage1 (Stage))
adonis(WIN_bray~Sort2, data = WINsampledf) #Pr(>F)=0.001 (by Sort 2 (Study day))
#adonis(WIN_bray~Stage1, data = WINsampledf) #Pr(>F)=0.001 (by Stage1 (Stage))

#************************************************************************************************
# CONSTRAINED ORDINATION -CAP SP or WIN  full data set
# code was originally written for SP, so objects reflect that
# pipeline at this point don't distinguish between SP or WIN
# O2 (DO) can only be used as metadata if missing values are filled in or if samples are removed
#************************************************************************************************

# Create the distance matrix
SP_dist <-phyloseq::distance(physeq = mo_merge_prune_scaled, method = "bray")

# Create the CAP ordination
SP_cap_ord <- ordinate(
  physeq = mo_merge_prune_scaled,
  method = "CAP",
  distance = SP_dist,
  formula = ~ pH + EC + CO2 + DO + NH4 + NO3 + TN + TC + CN_ratio + log.copies.gdw + fungal.bacterial.ratio + Chao1 + Shannon + InvSimpson 
)

# create the color palette
# note that RdYlBu is a colorblind-friendly palette
nb.cols <- 21
ordcolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(nb.cols)

#create the plot
#pdf(file = "SP_ITS_CAP_studyday_updated.pdf", height = 6, width = 7, useDingbats = FALSE)
#pdf(file = "SP_16S_CAP_studyday_updated.pdf", height = 6, width = 7, useDingbats = FALSE)
#pdf(file = "WIN_ITS_CAP_studyday_updated.pdf", height = 6, width = 7, useDingbats = FALSE)
pdf(file = "WIN_16S_CAP_studyday_updated.pdf", height = 6, width = 7, useDingbats = FALSE)

theme_set(theme_classic())
SPcap_plot <- plot_ordination(
  physeq = mo_merge_prune_scaled, 
  ordination = SP_cap_ord) +
  geom_point(aes(fill = Sort2, shape = Depth), size = 4, stroke = 1.5) +   
  scale_shape_manual(values=c(21,22)) +
  scale_fill_manual(values = ordcolors) +
  ggtitle("CAP:\nWIN 16S") +
  theme(plot.title = element_text(size = 20)) +
  theme(                             
    legend.text = element_text(size = 10),                               
    legend.title = element_blank(),                                      
    legend.background = element_rect(fill = "white", color = "black"))+  
  theme(axis.text.y.left = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  guides(fill = guide_legend(override.aes = list(shape = 21))) 

# add metadata as arrows
SP_arrows <- vegan::scores(SP_cap_ord, display = "bp")

#add labels and create dataframe
SP_arrows_df <- data.frame(labels = rownames(SP_arrows), SP_arrows)

# create arrow aesthetics
ar_aesthetics <- aes(xend = CAP1, yend = CAP2, x = 0, y = 0, shape = NULL, color = NULL, label = labels)
lb_aesthetics <- aes (x = 1.5 * CAP1, y = 1.5 * CAP2, shape = NULL, color = NULL, label = labels)
ar_head = arrow(length = unit (0.02, "npc"))

# overlay arrows
SPcap_plot+
  geom_segment(mapping = ar_aesthetics, size = 1.25, data = SP_arrows_df, color = "black", arrow = ar_head) + 
  geom_text(mapping = lb_aesthetics, size = 3, data = SP_arrows_df, show.legend = FALSE)

dev.off()  
anova(SP_cap_ord) #Permutational Anova


#***********************************************************************************
# COMPOSITES - alpha diversity and qPCR data
#**********************************************************************************
# files with the _mo extension simply have the three data points in which f:b > 20 removed

#COMPOSITE CORES
library(dplyr)
SP_16S_aq <- read.csv(file="SP_16S_alpha_qPCR.csv")
SP_16S_aq <- read.csv(file="SP_16S_alpha_qPCR_mo.csv")

str(SP_16S_aq)
WIN_16S_aq <- read.csv(file="WIN_16S_alpha_qPCR.csv")
str(WIN_16S_aq)

conintSP <-filter(SP_16S_aq,Location == "conint")
con15SP <-filter(SP_16S_aq,Location == "con15")
grintSP <-filter(SP_16S_aq,Location == "grint")
gr15SP <-filter(SP_16S_aq,Location == "gr15")

conintWIN <-filter(WIN_16S_aq,Location == "conint")
con15WIN <-filter(WIN_16S_aq,Location == "con15")
grintWIN <-filter(WIN_16S_aq,Location == "grint")
gr15WIN <-filter(WIN_16S_aq,Location == "gr15")

# COMPOSITE BY ADH
# IMPORTANT--these are soil ADH values
sample_ADHSP <-c(0,2703,5362,8360,18289,30771,41581,51922,61203,71557,79553,88547,98759,107850,116954,127477,135944,144538,151096,159517)
sample_ADHWIN<-c(0,3826,7594,12127,22782,33575,43005,52404,60714,71952,79891,87883,96552,104845,115079,123576,134019,142473,151104)
#*******************
# 16S CORES 
#*******************
#Shannon diversity
pdf("16Scomposite_core_Shannon.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(3,7), ylab="Shannon",las=1, cex.axis=1, xlab = "Soil ADH", main= "16SShannon - cores", col="white")

# SP points and lines for the controls
meansSPcon <-as.vector(tapply(con15SP$Shannon, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$Shannon, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$Shannon, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$Shannon, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$Shannon, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$Shannon, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$Shannon, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$Shannon, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# Inverse Simpson
pdf("composite_core_invSimp.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,350), ylab="InvSimpson",las=1, cex.axis=1, xlab = "Soil ADH", main= "InvSimpson - cores", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$InvSimpson, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$InvSimpson, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$InvSimpson, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$InvSimpson, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$InvSimpson, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$InvSimpson, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$InvSimpson, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$InvSimpson, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# Chao1
pdf("composite_core_Chao1.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,7000), ylab="Chao1",las=1, cex.axis=1, xlab = "Soil ADH", main= "Chao1 - cores", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$Chao1, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$Chao1, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$Chao1, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$Chao1, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$Chao1, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$Chao1, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$Chao1, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$Chao1, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# 16S gene abundances
pdf("composite_core_16S_gene_abundances_minus_outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(6,12), ylab="log copy number gdw-1",las=1, cex.axis=1, xlab = "Soil ADH", main= "16S core gene abundances minus outliers f:b>10", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$log.copies.gdw, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$log.copies.gdw, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$log.copies.gdw, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$log.copies.gdw, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$log.copies.gdw, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$log.copies.gdw, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$log.copies.gdw, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$log.copies.gdw, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# fungal:bacterial ratio
pdf("composite_core_16S_fb_ratio_minus_outliers_rescale.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,8), ylab="fungal:bacterial",las=1, cex.axis=1, xlab = "Soil ADH", main= "16S core f:b ratio minus outliers f:b>10", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$fungal.bacterial.ratio, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$fungal.bacterial.ratio, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$fungal.bacterial.ratio, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$fungal.bacterial.ratio, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$fungal.bacterial.ratio, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$fungal.bacterial.ratio, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$fungal.bacterial.ratio, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$fungal.bacterial.ratio, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()


#************************
# 16S interfaces
#*************************
# INT Shannon diversity
pdf("16Scomposite_int_Shannon.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(3,7), ylab="Shannon",las=1, cex.axis=1, xlab = "Soil ADH", main= "Shannon - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$Shannon, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$Shannon, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$Shannon, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$Shannon, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$Shannon, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$Shannon, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$Shannon, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$Shannon, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# INT Inverse Simpson
pdf("16S_composite_int_invSimp.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,350), ylab="InvSimpson",las=1, cex.axis=1, xlab = "Soil ADH", main= "InvSimpson - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$InvSimpson, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$InvSimpson, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$InvSimpson, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$InvSimpson, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$InvSimpson, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$InvSimpson, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$InvSimpson, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$InvSimpson, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

#INT Chao1
#png("16Scomposite_int_Chao1", width = 8,height=3.5, units = 'in',res = 300)
pdf("16Scomposite_int_Chao1.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,7000), ylab="Chao1",las=1, cex.axis=1, xlab = "Soil ADH", main= "Chao1 - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$Chao1, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$Chao1, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$Chao1, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$Chao1, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$Chao1, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$Chao1, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$Chao1, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$Chao1, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# INT 16S gene abundances
pdf("composite_int_16S_gene_abundances_minus outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(6,12), ylab="log copy number gdw-1",las=1, cex.axis=1, xlab = "Soil ADH", main= "16S INT gene abundances minus outliers f:b>10 ", col="white")

meansSPcon <-as.vector(tapply(conintSP$log.copies.gdw, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$log.copies.gdw, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$log.copies.gdw, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$log.copies.gdw, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$log.copies.gdw, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$log.copies.gdw, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$log.copies.gdw, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$log.copies.gdw, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# fungal:bacterial ratio
pdf("composite_int_16S_fb_ratio_minus_outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,8), ylab="fungal:bacterial",las=1, cex.axis=1, xlab = "Soil ADH", main= "16S core f:b ratio minus outliers f:b>10 ", col="white")

meansSPcon <-as.vector(tapply(conintSP$fungal.bacterial.ratio, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$fungal.bacterial.ratio, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$fungal.bacterial.ratio, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$fungal.bacterial.ratio, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$fungal.bacterial.ratio, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$fungal.bacterial.ratio, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$fungal.bacterial.ratio, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$fungal.bacterial.ratio, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

#*****************
# ITS CORES
#*****************

SP_ITS_aq <- read.csv(file="SP_ITS_alpha_qPCR.csv")
SP_ITS_aq <- read.csv(file="SP_ITS_alpha_qPCR_mo.csv")
str(SP_ITS_aq)
WIN_ITS_aq <- read.csv(file="WIN_ITS_alpha_qPCR.csv")
str(WIN_ITS_aq)

conintSP <-filter(SP_ITS_aq,Location == "conint")
con15SP <-filter(SP_ITS_aq,Location == "con15")
grintSP <-filter(SP_ITS_aq,Location == "grint")
gr15SP <-filter(SP_ITS_aq,Location == "gr15")

conintWIN <-filter(WIN_ITS_aq,Location == "conint")
con15WIN <-filter(WIN_ITS_aq,Location == "con15")
grintWIN <-filter(WIN_ITS_aq,Location == "grint")
gr15WIN <-filter(WIN_ITS_aq,Location == "gr15")
#*****
# COMPOSITE BY ADH
# IMPORTANT--these are soil ADH values
sample_ADHSP <-c(0,2703,5362,8360,18289,30771,41581,51922,61203,71557,79553,88547,98759,107850,116954,127477,135944,144538,151096,159517)
sample_ADHWIN<-c(0,3826,7594,12127,22782,33575,43005,52404,60714,71952,79891,87883,96552,104845,115079,123576,134019,142473,151104)


#Shannon diversity
pdf("ITScomposite_core_Shannon.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,6), ylab="Shannon",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITSShannon - cores", col="white")

# SP points and lines for the controls
meansSPcon <-as.vector(tapply(con15SP$Shannon, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$Shannon, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$Shannon, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$Shannon, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$Shannon, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$Shannon, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$Shannon, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$Shannon, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# Inverse Simpson
pdf("ITScomposite_core_invSimp.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,150), ylab="InvSimpson",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITSInvSimpson - cores", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$InvSimpson, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$InvSimpson, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$InvSimpson, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$InvSimpson, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$InvSimpson, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$InvSimpson, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$InvSimpson, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$InvSimpson, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# Chao1
pdf("ITScomposite_core_Chao1.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,3000), ylab="Shannon",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS Chao1 - core", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$Chao1, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$Chao1, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$Chao1, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$Chao1, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$Chao1, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$Chao1, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$Chao1, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$Chao1, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# 16S gene abundances
pdf("composite_core_ITS_gene_abundances_minus_outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(6,12), ylab="log copy number gdw-1",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS core gene abundances minus outliers f:b>10 ", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$log.copies.gdw, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$log.copies.gdw, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$log.copies.gdw, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$log.copies.gdw, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$log.copies.gdw, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$log.copies.gdw, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$log.copies.gdw, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$log.copies.gdw, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# fungal:bacterial ratio
pdf("composite_core_ITS_fb_ratio_minus_outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,8), ylab="fungal:bacterial",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS core f:b ratio minus outliers f:b>10 ", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(con15SP$fungal.bacterial.ratio, INDEX=con15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(con15SP$fungal.bacterial.ratio, INDEX=con15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(con15WIN$fungal.bacterial.ratio, INDEX=con15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(con15WIN$fungal.bacterial.ratio, INDEX=con15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines for the core samples
meansSP <-as.vector(tapply(gr15SP$fungal.bacterial.ratio, INDEX=gr15SP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(gr15SP$fungal.bacterial.ratio, INDEX=gr15SP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(gr15WIN$fungal.bacterial.ratio, INDEX=gr15WIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(gr15WIN$fungal.bacterial.ratio, INDEX=gr15WIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

#************************
# ITS interfaces
#*************************
# INT Shannon diversity
pdf("ITScomposite_int_Shannon.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,6), ylab="Shannon",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS Shannon - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$Shannon, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$Shannon, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$Shannon, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$Shannon, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$Shannon, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$Shannon, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$Shannon, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$Shannon, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# INT Inverse Simpson
pdf("ITS_composite_int_invSimp.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,150), ylab="InvSimpson",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS InvSimpson - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$InvSimpson, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$InvSimpson, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$InvSimpson, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$InvSimpson, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$InvSimpson, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$InvSimpson, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$InvSimpson, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$InvSimpson, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

#INT Chao1
pdf("ITS_composite_int_Chao1.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,3000), ylab="Chao1",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS Chao1 - int", col="white")

# SP points and lines for the controls 
meansSPcon <-as.vector(tapply(conintSP$Chao1, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$Chao1, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$Chao1, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$Chao1, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$Chao1, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$Chao1, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$Chao1, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$Chao1, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# INT ITS gene abundances
pdf("composite_int_ITS_gene_abundances_minus_outliers.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(6,12), ylab="log copy number gdw-1",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS INT gene abundances minus outliers  ", col="white")

meansSPcon <-as.vector(tapply(conintSP$log.copies.gdw, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$log.copies.gdw, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$log.copies.gdw, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$log.copies.gdw, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$log.copies.gdw, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$log.copies.gdw, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$log.copies.gdw, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$log.copies.gdw, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

# fungal:bacterial ratio
pdf("composite_int_ITS_fb_ratio.pdf", width = 8, height =3.5)
plot(1, type="n", xlim=c(0,160000),ylim=c(0,8), ylab="fungal:bacterial",las=1, cex.axis=1, xlab = "Soil ADH", main= "ITS INT f:b ratio  ", col="white")

meansSPcon <-as.vector(tapply(conintSP$fungal.bacterial.ratio, INDEX=conintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSPcon,lty=1, lwd=1, col="black")
errorbarsSPcon <- as.vector(tapply(conintSP$fungal.bacterial.ratio, INDEX=conintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon + errorbarsSPcon, length=0.03, angle=90, col="black")
arrows (sample_ADHSP, meansSPcon, sample_ADHSP, meansSPcon - errorbarsSPcon, length=0.03, angle=90,col="black")
# WIN points and lines for the controls
meansWINcon <-as.vector(tapply(conintWIN$fungal.bacterial.ratio, INDEX=conintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWINcon,lty=1, lwd=1, col="black")
errorbarsWINcon <- as.vector(tapply(conintWIN$fungal.bacterial.ratio, INDEX=conintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon + errorbarsWINcon, length=0.03, angle=90, col="black")
arrows (sample_ADHWIN, meansWINcon, sample_ADHWIN, meansWINcon - errorbarsWINcon, length=0.03, angle=90,col="black")

# SP points and lines 
meansSP <-as.vector(tapply(grintSP$fungal.bacterial.ratio, INDEX=grintSP$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHSP, meansSP,lty=1, lwd=4, col="red2")
errorbarsSP <- as.vector(tapply(grintSP$fungal.bacterial.ratio, INDEX=grintSP$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP + errorbarsSP, length=0.03, angle=90, col="red2")
arrows (sample_ADHSP, meansSP, sample_ADHSP, meansSP - errorbarsSP, length=0.03, angle=90,col="red2")
# WIN points and lines
meansWIN <-as.vector(tapply(grintWIN$fungal.bacterial.ratio, INDEX=grintWIN$ADH_soil, FUN=mean, simplify = TRUE), mode="numeric")
lines (sample_ADHWIN, meansWIN,lty=1, lwd=4, col="blue2")
errorbarsWIN <- as.vector(tapply(grintWIN$fungal.bacterial.ratio, INDEX=grintWIN$ADH_soil, FUN=sd, simplify = TRUE), mode="numeric")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN + errorbarsWIN, length=0.03, angle=90, col="blue2")
arrows (sample_ADHWIN, meansWIN, sample_ADHWIN, meansWIN - errorbarsWIN, length=0.03, angle=90,col="blue2")
dev.off()

#***************************************************
# Stats: Kruskal-Wallace and T-tests
#***************************************************
library(dplyr)

SP_16S_aq <- read.csv(file="SP_16S_alpha_qPCR_mo.csv")

conintSP <-filter(SP_16S_aq,Location == "conint")
con15SP <-filter(SP_16S_aq,Location == "con15")
grintSP <-filter(SP_16S_aq,Location == "grint")
gr15SP <-filter(SP_16S_aq,Location == "gr15")

#****************
# SP 16S cores
#****************
core_data <- rbind(con15SP, gr15SP)

kruskal.test(core_data$Shannon, core_data$Location)
kruskal.test(core_data$InvSimpson, core_data$Location)
kruskal.test(core_data$Chao1, core_data$Location)
kruskal.test(core_data$log.copies.gdw, core_data$Location)
kruskal.test(core_data$fungal.bacterial.ratio, core_data$Location)

# T tests - SP 16S cores 
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(gr15SP, Study.day==376) #adjust study day

t.test(con15SP$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(con15SP$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(con15SP$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(con15SP$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 
t.test(con15SP$fungal.bacterial.ratio,T_treat$fungal.bacterial.ratio, var.equal = FALSE) 

#*******************
# SP 16S interfaces
#*******************
int_data <- rbind(conintSP,grintSP)

kruskal.test(int_data$Shannon, int_data$Location)
kruskal.test(int_data$InvSimpson, int_data$Location)
kruskal.test(int_data$Chao1, int_data$Location)
kruskal.test(int_data$log.copies.gdw, int_data$Location)
kruskal.test(int_data$fungal.bacterial.ratio, int_data$Location)

# T tests - SP 16S interfaces
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(grintSP, Study.day==376) #adjust study day

t.test(conintSP$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(conintSP$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(conintSP$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(conintSP$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 
t.test(conintSP$fungal.bacterial.ratio,T_treat$fungal.bacterial.ratio, var.equal = FALSE) 

#***********************************************************************************************************
SP_ITS_aq <- read.csv(file="SP_ITS_alpha_qPCR_mo.csv")

conintSP <-filter(SP_ITS_aq,Location == "conint")
con15SP <-filter(SP_ITS_aq,Location == "con15")
grintSP <-filter(SP_ITS_aq,Location == "grint")
gr15SP <-filter(SP_ITS_aq,Location == "gr15")

#*************************************
# SP ITS cores
#*************************************
core_data <- rbind(con15SP, gr15SP)

kruskal.test(core_data$Shannon, core_data$Location)
kruskal.test(core_data$InvSimpson, core_data$Location)
kruskal.test(core_data$Chao1, core_data$Location)
kruskal.test(core_data$log.copies.gdw, core_data$Location)
kruskal.test(core_data$fungal.bacterial.ratio, core_data$Location)

# T tests - SP ITS cores 
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(gr15SP, Study.day==376) #adjust study day

t.test(con15SP$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(con15SP$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(con15SP$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(con15SP$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 

#****************************
# SP ITS interfaces
#****************************
int_data <- rbind(conintSP,grintSP)

kruskal.test(int_data$Shannon, int_data$Location)
kruskal.test(int_data$InvSimpson, int_data$Location)
kruskal.test(int_data$Chao1, int_data$Location)
kruskal.test(int_data$log.copies.gdw, int_data$Location)
kruskal.test(int_data$fungal.bacterial.ratio, int_data$Location)

# T tests - SP ITS interfaces
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(grintSP, Study.day==376) #adjust study day

t.test(conintSP$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(conintSP$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(conintSP$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(conintSP$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 

#************************************************************************************************************************
WIN_16S_aq <- read.csv(file="WIN_16S_alpha_qPCR.csv")

conintWIN <-filter(WIN_16S_aq,Location == "conint")
con15WIN <-filter(WIN_16S_aq,Location == "con15")
grintWIN <-filter(WIN_16S_aq,Location == "grint")
gr15WIN <-filter(WIN_16S_aq,Location == "gr15")

#*****************************
# WIN 16S cores
#*****************************
core_data <- rbind(con15WIN, gr15WIN)

kruskal.test(core_data$Shannon, core_data$Location)
kruskal.test(core_data$InvSimpson, core_data$Location)
kruskal.test(core_data$Chao1, core_data$Location)
kruskal.test(core_data$log.copies.gdw, core_data$Location)
kruskal.test(core_data$fungal.bacterial.ratio, core_data$Location)

# T tests - WIN 16S cores 
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 19 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(gr15WIN, Study.day==384) #adjust study day

t.test(con15WIN$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(con15WIN$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(con15WIN$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(con15WIN$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 
t.test(con15WIN$fungal.bacterial.ratio,T_treat$fungal.bacterial.ratio, var.equal = FALSE) 

#*********************
# WIN 16S interfaces
#*********************
int_data <- rbind(conintWIN,grintWIN)

kruskal.test(int_data$Shannon, int_data$Location)
kruskal.test(int_data$InvSimpson, int_data$Location)
kruskal.test(int_data$Chao1, int_data$Location)
kruskal.test(int_data$log.copies.gdw, int_data$Location)
kruskal.test(int_data$fungal.bacterial.ratio, int_data$Location)

# T tests - WIN 16S interfaces
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(grintWIN, Study.day==384) #adjust study day

t.test(conintWIN$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(conintWIN$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(conintWIN$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(conintWIN$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 
t.test(conintWIN$fungal.bacterial.ratio,T_treat$fungal.bacterial.ratio, var.equal = FALSE) 

#*********************************************************************************************************************
WIN_ITS_aq <- read.csv(file="WIN_ITS_alpha_qPCR.csv")

conintWIN <-filter(WIN_ITS_aq,Location == "conint")
con15WIN <-filter(WIN_ITS_aq,Location == "con15")
grintWIN <-filter(WIN_ITS_aq,Location == "grint")
gr15WIN <-filter(WIN_ITS_aq,Location == "gr15")

#***************************
# WIN ITS cores
#***************************
core_data <- rbind(con15WIN, gr15WIN)

kruskal.test(core_data$Shannon, core_data$Location)
kruskal.test(core_data$InvSimpson, core_data$Location)
kruskal.test(core_data$Chao1, core_data$Location)
kruskal.test(core_data$log.copies.gdw, core_data$Location)
kruskal.test(core_data$fungal.bacterial.ratio, core_data$Location)

# T tests - WIN ITS cores 
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 19 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(gr15WIN, Study.day==384) #adjust study day

t.test(con15WIN$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(con15WIN$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(con15WIN$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(con15WIN$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE) 

#**************************************
# WIN ITS interfaces
#**************************************
int_data <- rbind(conintWIN,grintWIN)

kruskal.test(int_data$Shannon, int_data$Location)
kruskal.test(int_data$InvSimpson, int_data$Location)
kruskal.test(int_data$Chao1, int_data$Location)
kruskal.test(int_data$log.copies.gdw, int_data$Location)
kruskal.test(int_data$fungal.bacterial.ratio, int_data$Location)

# T tests - WIN ITS interfaces
# Since control samples are pooled, and thus single samples which are insufficient for t-tests,
# t-tests are run comparing grave samples on a given day (3 samples) against the entire study control of 20 samples.
# granted, this is an artificial comparison and thus a conservative estimate of significance.
T_treat<- filter(grintWIN, Study.day==384) #adjust study day

t.test(conintWIN$Shannon,T_treat$Shannon, var.equal = FALSE) 
t.test(conintWIN$InvSimpson,T_treat$InvSimpson, var.equal = FALSE)
t.test(conintWIN$Chao1,T_treat$Chao1, var.equal = FALSE) 
t.test(conintWIN$log.copies.gdw,T_treat$log.copies.gdw, var.equal = FALSE)

