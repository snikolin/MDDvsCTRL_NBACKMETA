# Load required packages
library(metafor)
library(readxl)
library(RColorBrewer)
library(ggplot2)


###### LOAD DATA ###### 
file <- "/Users/.../WM.MDD.META.DATASET.xlsx"

WM.META.ACC         <- read_excel(file,1)
WM.META.ACC         <- as.data.frame(WM.META.ACC)
WM.META.ACC$diff    <- ordered(WM.META.ACC$load)
WM.META.ACC$diffval <- as.numeric(WM.META.ACC$diff)
WM.META.ACC$meanMDD <- as.numeric(WM.META.ACC$meanMDD)
WM.META.ACC$sdMDD   <- as.numeric(WM.META.ACC$sdMDD)
WM.META.ACC$meanCTRL<- as.numeric(WM.META.ACC$meanCTRL)
WM.META.ACC$sdCTRL  <- as.numeric(WM.META.ACC$sdCTRL)

WM.META.RT          <- read_excel(file,2)
WM.META.RT          <- as.data.frame(WM.META.RT)
WM.META.RT$diff     <- ordered(WM.META.RT$load)
WM.META.RT$diffval  <- as.numeric(WM.META.RT$diff)
WM.META.RT$meanCTRL <- as.numeric(WM.META.RT$meanCTRL)
WM.META.RT$sdCTRL   <- as.numeric(WM.META.RT$sdCTRL)
WM.META.RT$meanMDD  <- as.numeric(WM.META.RT$meanMDD)
WM.META.RT$sdMDD    <- as.numeric(WM.META.RT$sdMDD)


###### ACCURACY ###### 
dat.acc <- escalc(measure="SMD", vtype="UB",
               m1i=meanMDD, m2i=meanCTRL,
               sd1i=sdMDD,sd2i=sdCTRL,
               n1i=nMDD,n2i=nCTRL,
               data=WM.META.ACC)

dat.acc$meanage <- ((dat.acc$nMDD*dat.acc$ageMDD) + (dat.acc$nCTRL*dat.acc$ageCTRL))/(dat.acc$nMDD + dat.acc$nCTRL)
dat.acc$modality <- as.factor(dat.acc$modality)

acc.index.yi <- !is.na(dat.acc$comb.yi)
dat.acc$yi[acc.index.yi] <- dat.acc$comb.yi[acc.index.yi]

acc.index.vi <- !is.na(dat.acc$comb.vi)
dat.acc$vi[acc.index.vi] <- dat.acc$comb.vi[acc.index.vi]

# Remove outlier studies
acc.outlier <- is.na(dat.acc$outlier)
dat.acc <- dat.acc[acc.outlier,]

# Accuracy Data Subgrouping
dat.acc.0back <- dat.acc[dat.acc$diff == "0-back",]
dat.acc.1back <- dat.acc[dat.acc$diff == "1-back",]
dat.acc.2back <- dat.acc[dat.acc$diff == "2-back",]
dat.acc.3back <- dat.acc[dat.acc$diff == "3-back",]

# Accuracy Meta-Analyses
res.acc.0back <- rma(yi, vi, data = dat.acc.0back)
res.acc.1back <- rma(yi, vi, data = dat.acc.1back)
res.acc.2back <- rma(yi, vi, data = dat.acc.2back)
res.acc.3back <- rma(yi, vi, data = dat.acc.3back)


###### RESPONSE TIME ######
dat.rt <- escalc(measure="SMD", vtype="UB",
                 m1i=meanMDD, m2i=meanCTRL,
                 sd1i=sdMDD,sd2i=sdCTRL,
                 n1i=nMDD,n2i=nCTRL,
                 data=WM.META.RT)

dat.rt$meanage <- ((dat.rt$nMDD*dat.rt$ageMDD) + (dat.rt$nCTRL*dat.rt$ageCTRL))/(dat.rt$nMDD + dat.rt$nCTRL)
dat.rt$modality <- as.factor(dat.rt$modality)

rt.index.yi <- !is.na(dat.rt$comb.yi)
dat.rt$yi[rt.index.yi] <- dat.rt$comb.yi[rt.index.yi]

rt.index.vi <- !is.na(dat.rt$comb.vi)
dat.rt$vi[rt.index.vi] <- dat.rt$comb.vi[rt.index.vi]

# Remove outlier studies
rt.outlier <- is.na(dat.rt$outlier)
dat.rt <- dat.rt[rt.outlier,]

# Response Time Data Subgrouping
dat.rt.0back <- dat.rt[dat.rt$diff == "0-back",]
dat.rt.1back <- dat.rt[dat.rt$diff == "1-back",]
dat.rt.2back <- dat.rt[dat.rt$diff == "2-back",]
dat.rt.3back <- dat.rt[dat.rt$diff == "3-back",]

# Response Time Meta-Analyses
res.rt.0back  <- rma(yi, vi, data = dat.rt.0back)
res.rt.1back  <- rma(yi, vi, data = dat.rt.1back)
res.rt.2back  <- rma(yi, vi, data = dat.rt.2back)
res.rt.3back  <- rma(yi, vi, data = dat.rt.3back)


###### FUNNEL PLOTS ###### 
# Funnel plot with contour shading - TIFF: 600 x 400
funnel(res.acc.0back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.acc.0back$outlier), 19, 20),
       col=ifelse(is.na(dat.acc.0back$outlier), "red", "blue"),
       cex = 1.5)
legend(-2.5, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.acc.1back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.acc.1back$outlier), 19, 20),
       col=ifelse(is.na(dat.acc.1back$outlier), "red", "blue"),
       cex = 1.5)
legend(-4, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.acc.2back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.acc.2back$outlier), 19, 20),
       col=ifelse(is.na(dat.acc.2back$outlier), "red", "blue"),
       cex = 1.5)
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.acc.3back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.acc.3back$outlier), 19, 20),
       col=ifelse(is.na(dat.acc.3back$outlier), "red", "blue"),
       cex = 1.5)
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))


funnel(res.rt.0back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.rt.0back$outlier), 19, 20),
       col=ifelse(is.na(dat.rt.0back$outlier), "red", "blue"),
       cex = 1.5)
legend(6, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.rt.1back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.rt.1back$outlier), 19, 20),
       col=ifelse(is.na(dat.rt.1back$outlier), "red", "blue"),
       cex = 1.5)
legend(5, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.rt.2back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.rt.2back$outlier), 19, 20),
       col=ifelse(is.na(dat.rt.2back$outlier), "red", "blue"),
       cex = 1.5)
legend(2.5, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(res.rt.3back,level = c(90,95,99), shade = c("white","gray","darkgray"),
       pch=ifelse(is.na(dat.rt.3back$outlier), 19, 20),
       col=ifelse(is.na(dat.rt.3back$outlier), "red", "blue"),
       cex = 1.5)
legend(0.8, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))


# Trim and Fill
taf.acc.0back <- trimfill(res.acc.0back) 
taf.acc.1back <- trimfill(res.acc.1back) 
taf.acc.2back <- trimfill(res.acc.2back) 
taf.acc.3back <- trimfill(res.acc.3back) 

taf.rt.0back <- trimfill(res.rt.0back) 
taf.rt.1back <- trimfill(res.rt.1back) 
taf.rt.2back <- trimfill(res.rt.2back) 
taf.rt.3back <- trimfill(res.rt.3back) 

# Trim and Fill Funnel Plots - TIFF: 600 x 400
funnel(taf.acc.0back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.acc.1back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.acc.2back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.acc.3back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.2, -0.02, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))


funnel(taf.rt.0back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.8, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.rt.1back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(1.5, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.rt.2back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(1.5, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))

funnel(taf.rt.3back,level = c(90,95,99), shade = c("white","gray","darkgray"))
legend(0.8, 0, c("0.10 < p < 1.00", "0.05 < p < 0.10", "0.01 < p < 0.05"), bty = "n", fill=c("white","gray","darkgray"))


# Egger Statistic - Regression test for funnel plot asymmetry
regtest(res.acc.0back)
regtest(res.acc.1back)
regtest(res.acc.2back) 
regtest(res.acc.3back)

regtest(res.rt.0back)
regtest(res.rt.1back)
regtest(res.rt.2back) 
regtest(res.rt.3back)


###### FOREST PLOTS ######
# Accuracy - 0-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.acc.0back)[1] 

tiff(file = 'forest.acc.0back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.acc.0back, xlim=c(-24, 8), at=seq(-2,1.5,by=0.5),
       ilab=cbind(dat.acc.0back$nMDD,round(dat.acc.0back$meanMDD,1), round(dat.acc.0back$sdMDD,1), 
                  dat.acc.0back$nCTRL,round(dat.acc.0back$meanCTRL,1), round(dat.acc.0back$sdCTRL,1)),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.acc.0back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.acc.0back$QE, digits=2, format="f")), ", df = ", .(res.acc.0back$k - res.acc.0back$p),
                  ", p-value = ", .(formatC(res.acc.0back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.acc.0back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Accuracy - 1-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.acc.1back)[1] 

tiff(file = 'forest.acc.1back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.acc.1back, xlim=c(-24, 8), at=seq(-2,1,by=0.5),
       ilab=cbind(dat.acc.1back$nMDD,round(dat.acc.1back$meanMDD,1), round(dat.acc.1back$sdMDD,1), 
                  dat.acc.1back$nCTRL,round(dat.acc.1back$meanCTRL,1), round(dat.acc.1back$sdCTRL,1)),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.acc.1back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.acc.1back$QE, digits=2, format="f")), ", df = ", .(res.acc.1back$k - res.acc.1back$p),
                  ", p-value = ", .(formatC(res.acc.1back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.acc.1back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Accuracy - 2-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.acc.2back)[1] 

tiff(file = 'forest.acc.2back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.acc.2back, xlim=c(-24, 8), at=seq(-3,1.5,by=0.5),
       ilab=cbind(dat.acc.2back$nMDD,round(dat.acc.2back$meanMDD,1), round(dat.acc.2back$sdMDD,1), 
                  dat.acc.2back$nCTRL,round(dat.acc.2back$meanCTRL,1), round(dat.acc.2back$sdCTRL,1)),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.acc.2back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.acc.2back$QE, digits=2, format="f")), ", df = ", .(res.acc.2back$k - res.acc.2back$p),
                  ", p-value = ", .(formatC(res.acc.2back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.acc.2back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Accuracy - 3-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.acc.3back)[1] 

tiff(file = 'forest.acc.3back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.acc.3back, xlim=c(-24, 8), at=seq(-1.5,1.5,by=0.5),
       ilab=cbind(dat.acc.3back$nMDD,round(dat.acc.3back$meanMDD,1), round(dat.acc.3back$sdMDD,1), 
                  dat.acc.3back$nCTRL,round(dat.acc.3back$meanCTRL,1), round(dat.acc.3back$sdCTRL,1)),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.acc.3back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.acc.3back$QE, digits=2, format="f")), ", df = ", .(res.acc.3back$k - res.acc.3back$p),
                  ", p-value = ", .(formatC(res.acc.3back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.acc.3back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Response Time - 0-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.rt.0back)[1] 

tiff(file = 'forest.rt.0back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.rt.0back, xlim=c(-24, 8), at=seq(-1.5,3,by=0.5),
       ilab=cbind(dat.rt.0back$nMDD, dat.rt.0back$meanMDD, dat.rt.0back$sdMDD, 
                  dat.rt.0back$nCTRL, dat.rt.0back$meanCTRL, dat.rt.0back$sdCTRL),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.rt.0back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.rt.0back$QE, digits=2, format="f")), ", df = ", .(res.rt.0back$k - res.rt.0back$p),
                  ", p-value = ", .(formatC(res.rt.0back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.rt.0back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Response Time - 1-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.rt.1back)[1] 

tiff(file = 'forest.rt.1back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.rt.1back, xlim=c(-24, 10), at=seq(-1.5,4.5,by=1),
       ilab=cbind(dat.rt.1back$nMDD, dat.rt.1back$meanMDD, dat.rt.1back$sdMDD, 
                  dat.rt.1back$nCTRL, dat.rt.1back$meanCTRL, dat.rt.1back$sdCTRL),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.rt.1back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.rt.1back$QE, digits=2, format="f")), ", df = ", .(res.rt.1back$k - res.rt.1back$p),
                  ", p-value = ", .(formatC(res.rt.1back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.rt.1back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(10,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Response Time - 2-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.rt.2back)[1] 

tiff(file = 'forest.rt.2back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.rt.2back, xlim=c(-24, 10), at=seq(-1.5,5.5,by=1),
       ilab=cbind(dat.rt.2back$nMDD, dat.rt.2back$meanMDD, dat.rt.2back$sdMDD, 
                  dat.rt.2back$nCTRL, dat.rt.2back$meanCTRL, dat.rt.2back$sdCTRL),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.rt.2back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.rt.2back$QE, digits=2, format="f")), ", df = ", .(res.rt.2back$k - res.rt.2back$p),
                  ", p-value = ", .(formatC(res.rt.2back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.rt.2back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(10,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

# Response Time - 3-back
par(mar=c(4,4,1,2), cex = 1)
rowsnum <- dim(dat.rt.3back)[1] 

tiff(file = 'forest.rt.3back.tiff', width=16, height = 0.4*(rowsnum+12.85), units='cm', res = 300)

forest(res.rt.3back, xlim=c(-24, 8), at=seq(-1,2,by=0.5),
       ilab=cbind(dat.rt.3back$nMDD, dat.rt.3back$meanMDD, dat.rt.3back$sdMDD, 
                  dat.rt.3back$nCTRL, dat.rt.3back$meanCTRL, dat.rt.3back$sdCTRL),
       ilab.xpos=c(-14,-12,-10,-8,-6,-4), cex=.75, ylim=c(-1, rowsnum + 3),
       xlab="SMD", mlab="",slab=dat.rt.3back$study,order="obs")

text(-24, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                  .(formatC(res.rt.3back$QE, digits=2, format="f")), ", df = ", .(res.rt.3back$k - res.rt.3back$p),
                  ", p-value = ", .(formatC(res.rt.3back$pval, digits=3, format="f")), ", ", I^2, " = ",
                  .(formatC(res.rt.3back$I2, digits=1, format="f")), "%)")))

par(font=2, ps = 12, cex = 0.8, cex.main = 1)
text(c(-14,-12,-10,-8,-6,-4),(rowsnum+2), c("N", "Mean", "SD", "N", "Mean", "SD"))
text(c(-12,-6),              (rowsnum+3), c("MDD", "Control"))
text(-24,                    (rowsnum+2), "Authors and Year",  pos=4)
text(8,                      (rowsnum+2), "SMD [95% CI]", pos=2)

dev.off()

###### BUBBLE PLOTS ######
# Accuracy
summary.file  <- "/Users/stevannikolin/OneDrive - UNSW/Studies/Depression/TBD - Depression vs Healthy N-back Meta-analysis/results/WM.MDD.SUMMARY.xlsx"
summary.acc   <- read_excel(summary.file,1)
summary.acc   <- as.data.frame(summary.acc)
summary.acc$diff <- ordered(summary.acc$load)
summary.acc$mean <- as.numeric(summary.acc$mean)
summary.acc$cilb <- as.numeric(summary.acc$cilb)
summary.acc$ciub <- as.numeric(summary.acc$ciub)

acc.wi    <- 1/sqrt(dat.acc$vi)
acc.size  <- 2 + 5.0 * (acc.wi - min(acc.wi))/(max(acc.wi) - min(acc.wi))

bubble.acc <- ggplot(dat.acc,aes(x=diff,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size=acc.size,alpha=0.5,position=position_jitter(width=0.1,height=0)) +
  geom_point(data=summary.acc, aes(x=diff, y=mean), size=5, shape=18, color="black") +
  geom_errorbar(data=summary.acc, mapping = aes(x=diff, y=mean, ymin=cilb, ymax=ciub), size=1, color="black", width=0.1) +
  labs(x="Difficulty Level",y="SMD: Accuracy") +
  theme(text = element_text(size = 20)) + 
  theme_bw() +
  ggtitle("Accuracy")


# Response time
summary.rt   <- read_excel(summary.file,2)
summary.rt   <- as.data.frame(summary.rt)
summary.rt$diff <- ordered(summary.rt$load)
summary.rt$mean <- as.numeric(summary.rt$mean)
summary.rt$cilb <- as.numeric(summary.rt$cilb)
summary.rt$ciub <- as.numeric(summary.rt$ciub)

rt.wi   <- 1/sqrt(dat.rt$vi)
rt.size <- 2 + 5.0 * (rt.wi - min(rt.wi))/(max(rt.wi) - min(rt.wi))

bubble.rt <- ggplot(dat.rt,aes(x=diff,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size=rt.size,alpha=0.5,position=position_jitter(width=0.1,height=0)) +
  geom_point(data=summary.rt, aes(x=diff, y=mean), size=5, shape=18, color="black") +
  geom_errorbar(data=summary.rt, mapping = aes(x=diff, y=mean, ymin=cilb, ymax=ciub), size=1, color="black", width=0.1) +
  labs(x="Difficulty Level",y="SMD: RT") +
  theme(text = element_text(size = 20)) + 
  theme_bw() +
  ggtitle("Response Times")


##### SAVING PLOTS #####
DIR <- "/Users/stevannikolin/OneDrive - UNSW/Studies/Depression/TBD - Depression vs Healthy N-back Meta-analysis/figures"
mypath <- file.path(DIR,"filename.tiff")
tiff(file = mypath, width=500, height = 500, units='px', res = 300)
bubble.rt
dev.off()


##### CONFIRMATORY ANALYSES ##### 
# Exclude studies of remitted depressed individuals
dat.nonremitt.acc <- dat.acc[dat.acc$remitted=="no",]
dat.nonremitt.acc.0back <- dat.nonremitt.acc[dat.nonremitt.acc$diff == "0-back",]
dat.nonremitt.acc.1back <- dat.nonremitt.acc[dat.nonremitt.acc$diff == "1-back",]
dat.nonremitt.acc.2back <- dat.nonremitt.acc[dat.nonremitt.acc$diff == "2-back",]
dat.nonremitt.acc.3back <- dat.nonremitt.acc[dat.nonremitt.acc$diff == "3-back",]

res.nonremitt.acc.0back <- rma(yi, vi, data = dat.nonremitt.acc.0back)
res.nonremitt.acc.1back <- rma(yi, vi, data = dat.nonremitt.acc.1back)
res.nonremitt.acc.2back <- rma(yi, vi, data = dat.nonremitt.acc.2back)
res.nonremitt.acc.3back <- rma(yi, vi, data = dat.nonremitt.acc.3back)


dat.nonremitt.rt <- dat.rt[dat.rt$remitted=="no",]
dat.nonremitt.rt.0back <- dat.nonremitt.rt[dat.nonremitt.rt$diff == "0-back",]
dat.nonremitt.rt.1back <- dat.nonremitt.rt[dat.nonremitt.rt$diff == "1-back",]
dat.nonremitt.rt.2back <- dat.nonremitt.rt[dat.nonremitt.rt$diff == "2-back",]
dat.nonremitt.rt.3back <- dat.nonremitt.rt[dat.nonremitt.rt$diff == "3-back",]

res.nonremitt.rt.0back  <- rma(yi, vi, data = dat.nonremitt.rt.0back)
res.nonremitt.rt.1back  <- rma(yi, vi, data = dat.nonremitt.rt.1back)
res.nonremitt.rt.2back  <- rma(yi, vi, data = dat.nonremitt.rt.2back)
res.nonremitt.rt.3back  <- rma(yi, vi, data = dat.nonremitt.rt.3back)


###### META REGRESSIONS - AGE ######
BUBBLE.ALPHA    <- 0.7
RIBBON.ALPHA    <- 0.1
PREDS.LINEWIDTH <- 0.5
TEXT.SIZE       <- 20
JITT.WIDTH      <- 0
JITT.HEIGHT     <- 0

res.acc.meanage.0back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.acc.0back)
res.acc.meanage.1back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.acc.1back)
res.acc.meanage.2back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.acc.2back)
res.acc.meanage.3back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.acc.3back)

res.rt.meanage.0back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.rt.0back)
res.rt.meanage.1back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.rt.1back)
res.rt.meanage.2back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.rt.2back)
res.rt.meanage.3back <- rma(yi, vi, mods = ~ scale(meanage), data = dat.rt.3back)

# Visualise effect of age
acc.wi.0back    <- 1/sqrt(dat.acc.0back$vi)
acc.size.0back  <- 2 + 5.0 * (acc.wi.0back - min(acc.wi.0back))/(max(acc.wi.0back) - min(acc.wi.0back))
preds <- predict(res.acc.meanage.0back)

bubble.age.reg.0back <- ggplot(dat.acc.0back,aes(x=meanage,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_line(aes(x=meanage,y=preds$pred),size=PREDS.LINEWIDTH) +
  geom_ribbon(aes(ymin=preds$ci.lb, ymax = preds$ci.ub, x = meanage),alpha = RIBBON.ALPHA) +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size = acc.size.0back, alpha=BUBBLE.ALPHA,position=position_jitter(width=JITT.WIDTH,height=JITT.HEIGHT)) +
  labs(x="Age",y="SMD: Accuracy") +
  theme(text = element_text(size = TEXT.SIZE)) + 
  theme_bw()


acc.wi.1back    <- 1/sqrt(dat.acc.1back$vi)
acc.size.1back  <- 2 + 5.0 * (acc.wi.1back - min(acc.wi.1back))/(max(acc.wi.1back) - min(acc.wi.1back))
preds <- predict(res.acc.meanage.1back)

bubble.age.reg.1back <- ggplot(dat.acc.1back,aes(x=meanage,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_line(aes(x=meanage,y=preds$pred),size=PREDS.LINEWIDTH) +
  geom_ribbon(aes(ymin=preds$ci.lb, ymax = preds$ci.ub, x = meanage),alpha = RIBBON.ALPHA) +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size = acc.size.1back, alpha=BUBBLE.ALPHA,position=position_jitter(width=JITT.WIDTH,height=JITT.HEIGHT)) +
  labs(x="Age",y="SMD: Accuracy") +
  theme(text = element_text(size = TEXT.SIZE)) + 
  theme_bw()

acc.wi.2back    <- 1/sqrt(dat.acc.2back$vi)
acc.size.2back  <- 2 + 5.0 * (acc.wi.2back - min(acc.wi.2back))/(max(acc.wi.2back) - min(acc.wi.2back))
preds <- predict(res.acc.meanage.2back)

bubble.age.reg.2back <- ggplot(dat.acc.2back,aes(x=meanage,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_line(aes(x=meanage,y=preds$pred),size=PREDS.LINEWIDTH) +
  geom_ribbon(aes(ymin=preds$ci.lb, ymax = preds$ci.ub, x = meanage),alpha = RIBBON.ALPHA) +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size = acc.size.2back, alpha=BUBBLE.ALPHA,position=position_jitter(width=JITT.WIDTH,height=JITT.HEIGHT)) +
  labs(x="Age",y="SMD: Accuracy") +
  theme(text = element_text(size = TEXT.SIZE)) + 
  theme_bw()


acc.wi.3back    <- 1/sqrt(dat.acc.3back$vi)
acc.size.3back  <- 2 + 5.0 * (acc.wi.3back - min(acc.wi.3back))/(max(acc.wi.3back) - min(acc.wi.3back))
preds <- predict(res.acc.meanage.3back)

bubble.age.reg.3back <- ggplot(dat.acc.3back,aes(x=meanage,y=yi)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_line(aes(x=meanage,y=preds$pred),size=PREDS.LINEWIDTH) +
  geom_ribbon(aes(ymin=preds$ci.lb, ymax = preds$ci.ub, x = meanage),alpha = RIBBON.ALPHA) +
  geom_point(aes(colour=remitted,fill = remitted),shape=19,size = acc.size.3back, alpha=BUBBLE.ALPHA,position=position_jitter(width=JITT.WIDTH,height=JITT.HEIGHT)) +
  labs(x="Age",y="SMD: Accuracy") +
  theme(text = element_text(size = TEXT.SIZE)) + 
  theme_bw()
