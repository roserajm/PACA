### Code for Rosera and Coleman (in prep) ####
### Note, figures are drafts that were exported
### to Adobe Illustrator for final touch up


##### Written for R version 3.4.3 ###

library(scales)


# Data were collected by measuring the distance of ore deposists from MRDS
# to caldera rims (Nearest Neighbor - ArcGIS).  Caldera rim-distance for
# each deposit was placed in 2.5 km bins and exported as CSV files.  
# Three files were made:
#    ds01 = counts for deposits wherein commodity is listed as primary in MRDS
#    ds02 = counts for deposits wherein commodity is listed as either primary, secondary, or tertiary
#    ds03 = counts for random points (generated in ArcGIS)

## set working directory as appropriate

#Read in data

  # Primary only
  Y = read.csv('ds01.csv', header = TRUE)
  
  # Multi-commodity data
  Y123 = read.csv('ds02.csv', header = TRUE)
  
  #Random data generated in ArcGIS and compiled into 2.5 km distance bins#
  RNT = read.csv('ds03.csv', header = TRUE)
  RNT = RNT[, c(3:102)]
  
  #data tables contain null values that need to be zeroed:
  Y[is.na(Y)] = 0
  Y123[is.na(Y123)] = 0
  RNT[is.na(RNT)] = 0


## set a high frequency cut off
hf = 500

## Select columns with counts and extract names
z = Y[, c(3:19)]
z[is.na(z)] = 0
labZ = colnames(Y[,c(3:19)])

## filtered high frequency mines for primary-only dataset
y = z[,apply(z,2,sum)>hf]
y[is.na(y)] = 0

#define upper limit
ul = dim(y)[2]
labY = colnames(Y[,c(3:(2+ul))])


##repeate for multi-commodity analysis
z123 = Y123[, c(3:30)]
z123[is.na(z123)] = 0
labZ123 = colnames(Y123[,c(3:30)])

# multi-commodity, high-frequency filter
y123 = z123[,apply(z123,2,sum)>hf]
y123[is.na(y123)] = 0
ul123 = dim(y123)[2]
labY123 = colnames(Y123[,c(3:(2+ul123))])


# Use upper bound of bins for plotting and analysis
seq = Y[,2]
seq123 = Y123[,2]


#calculate frequency for each interation of random points
fNT = RNT*0
for(k in c(1:100)){
  fNT[,k] = RNT[,k]/sum(RNT[,k])
}

avgNT = apply(fNT, 1, mean)
sdNT = apply(fNT, 1, sd)

#set up colors
FrPal = colorRampPalette(c('firebrick2', 'orange', 'blue', 'light blue', 'gray40', 'gray70'), alpha = TRUE)

col.Au = rgb(254,153,51, maxColorValue = 255)
col.Cu = rgb(45,175,74, maxColorValue = 255)
col.Pb = rgb(255,65,0, maxColorValue = 255)
col.Ag= rgb(33,113,181, maxColorValue = 255)
col.W= rgb(0,112,255, maxColorValue = 255)
col.F= rgb(152,36,163, maxColorValue = 255)
col.Mn= rgb(240,2,127, maxColorValue = 255)

mCol = c(col.Au, col.Cu, col.Pb, col.Ag, col.W, col.Mn, col.F, "black", "gray40", "orange", "black", "brown","black","red")
mCol123 = c(col.Au, col.Ag, col.Cu, col.Pb, "brown", col.W, "black", "gray40", col.Mn, "red", "orange", col.F)


######### Plots for Fig. 2 ##

par(mfrow=c(3,2),mar = c(0,0,0,0), oma = c(5,4,5,2))


### FIRST ROW: High frequency deposits for Au-Ag-Pb-Zn, or ~ epithermal deposits ###
plot(seq/1000, 100*apply(y,1,sum)/sum(y), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)

lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
for(i in c(c(3,4,1))){
  lines(seq/1000, 100*(z[,i]/sum(z[,i])), col = alpha(mCol[i],.65), lwd=2)
}
lines(seq/1000, 100*(z[,12]/sum(z[,12])), col = alpha(mCol[12],.65), lwd=1, lty = 3)
axis(side =2, tck = 0.01)
box()


plot(seq/1000, 100*apply(z123[c(1:138),],1,sum)/sum(y123), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)

lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
for(i in c(c(1,2,4,5))){
  lines(seq/1000, 100*(z123[c(1:138),i]/sum(z123[,i])), col = alpha(mCol123[i],.65), lwd=2)
}

box()


legend('topright', legend =c(labY123[c(1,2,4,5)], "Random"), col = alpha(c(mCol123[c(1,2,4,5)], 'gray50'), .65), lwd = c(2,2,2,1))


#### SECOND ROW ######
plot(seq/1000, 100*apply(y[,],1,sum)/sum(y), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)
lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )

### Cu-mo-W-Fe
for(i in c(c(9,14))){
  lines(seq/1000, 100*(z[,i]/sum(z[,i])), col = alpha(mCol[i],.65), lwd=1, lty = 3)
}

for(i in c(c(2,5))){
  lines(seq/1000, 100*(z[,i]/sum(z[,i])), col = alpha(mCol[i],.65), lwd=2)
}

axis(side = 2, tck = .01)
box()



plot(seq/1000, 100*apply(z123[c(1:138),],1,sum)/sum(y123), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)

lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
for(i in c(c(3,10,6,8))){
  lines(seq/1000, 100*(z123[c(1:138),i]/sum(z123[,i])), col = alpha(mCol123[i],.65), lwd=2)
}

box()

legend('topright', legend =c(labY123[c(3,10,6,8)], "Random"), col = alpha(c(mCol123[c(3,10,6,8)], 'gray50'), .65), lwd = c(2,2,2,1))




######## row 3 ###############

plot(seq/1000, 100*apply(y[,],1,sum)/sum(y), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)
lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )

### Mn, F, U
for(i in c(c(7,8, 10))){
  lines(seq/1000, 100*(z[,i]/sum(z[,i])), col = alpha(mCol[i],.65), lwd=1, lty = 3)
}

for(i in c(c(6))){
  lines(seq/1000, 100*(z[,i]/sum(z[,i])), col = alpha(mCol[i],.65), lwd=2)
}
axis(side = 2, tck = 0.01)
axis(side = 1, tck = 0.01)
box()



plot(seq/1000, 100*apply(z123[c(1:138),],1,sum)/sum(y123), type = 'n', log = 'x', axes = FALSE, xlab = "Distance from nearest caldera rim (km)", ylab = "Frequency (%)", pch = 15, col = 'black', ylim = c(0,15), cex = 1.5)

lines(seq/1000, 100*avgNT, lty =1, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT+(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
lines(seq/1000, 100*(avgNT-(2*sdNT)), lty =2, cex = 1, col = 'gray50' )
for(i in c(c(7,9,11,12))){
  lines(seq/1000, 100*(z123[c(1:138),i]/sum(z123[,i])), col = alpha(mCol123[i],.65), lwd=2)
}
axis(side = 1, tck = 0.01)
box()

legend('topright', legend =c(labY123[c(7,9,11,12)], "Random"), col = alpha(c(mCol123[c(7,9,11,12)], 'gray50'), .65), lwd = c(2,2,2,1))

mtext("Frequency (%)" , side = 2, outer = TRUE, line = 2, cex = 1.2)
mtext("Distance from nearest caldera rim (km, log scale)", side = 1, outer = TRUE, line = 3, cex = 1.2)


######################################
######  CORRESPONDENCE ANALYSIS ######
######   FOR PRIMARY ONLY ANALYSIS ######


###  Start with the more frequent occurences:
###
#Set up correspondence matrix
P = as.matrix(y/sum(y))

#Row and column masses:
Ri = apply(P, 1, sum)
Ci = apply(P, 2, sum)

#Diagonal matrices of row and column masses
Dr = diag(Ri)
Dc = diag(Ci)

#inverse square root of diagonal matrix, where off-diagonal remains 0:
Drs = diag(1/sqrt(diag(Dr)))
Dcs = diag(1/sqrt(diag(Dc)))
## a few 0's in denominator need to be set back to 0:
Drs[is.infinite(Drs)] = 0
Dcs[is.infinite(Dcs)] = 0

### Calculate the S matrix (A.4 in Greenacre)

S = Drs %*% (P - Ri %*% t(Ci)) %*% Dcs

# Decompose S with SVD:
Ssvd = svd(S)
U = Ssvd$u
V = Ssvd$v
Da = diag(Ssvd$d)
Dl = diag(1/sqrt(Ssvd$d))

#std coordinates, phi, of rows (A.6 Greenacre)
phi = Drs %*% U

# std coordinates gam of columns:
gam = Dcs %*% V

#principal coordinates F of rows:
Fr = phi %*% Da

#principal coordinates Gc of columns (A.9):
Gc = gam %*% Da
G = solve(Dcs) %*% t(P) %*% Fr %*% Dl


#plot of principal intertias, lamk:
par(mfrow=c(1,1))
lamk = (Ssvd$d)^2
#barplot(lamk[1:length(lamk)])
#plot principal intertias as percentages
#barplot(100*lamk/sum(lamk), names.arg = c(1:length(lamk)), xlab = "Principal inertias", ylab = "%")
round(cumsum(lamk[1:length(lamk)])/sum(lamk[1:length(lamk)]), 3) *100

#color given to last column, which is insignificant anyway
breaks= c(0, 10000, seq(20000,100000,20000),350000)
Fr[,ul] = as.numeric(cut(seq,breaks=breaks))
pCol = c("red", "orange", "yellow", "green", "cadetblue", "light blue", "gray50")

###################################################
###  Now include less frequent ore deposit groups:
##################################################
#Set up correspondence matrix
Pz = as.matrix(z/sum(z))

#Row and column masses:
Rzi = apply(Pz, 1, sum)
Czi = apply(Pz, 2, sum)

#Diagonal matrices of row and column masses
Dzr = diag(Rzi)
Dzc = diag(Czi)

#inverse square root of diagonal matrix, where off-diagonal remains 0:
Dzrs = diag(1/sqrt(diag(Dzr)))
Dzcs = diag(1/sqrt(diag(Dzc)))
## a few 0's in denominator need to be set back to 0:
Dzrs[is.infinite(Dzrs)] = 0
Dzcs[is.infinite(Dzcs)] = 0



### Calculate the S matrix (A.4 in Greenacre)

Sz = Dzrs %*% (Pz - Rzi %*% t(Czi)) %*% Dzcs


# Decompose S with SVD:
Szsvd = svd(Sz)
Uz = Szsvd$u
Vz = Szsvd$v
Dza = diag(Szsvd$d)
Dzl = diag(1/sqrt(Szsvd$d))

#std coordinates, phi, of rows (A.6 Greenacre)
phiz = Dzrs %*% Uz

# std coordinates gam of columns:
gamz = Dzcs %*% Vz

#principal coordinates F of rows:
Frz = phiz %*% Dza

#principal coordinates Gc of columns (A.9):
Gcz = gamz %*% Dza
Gz = solve(Dzcs) %*% t(Pz) %*% Frz %*% Dzl


#plot of principal intertias, lamk:
lamkz = (Szsvd$d)^2
#barplot(lamkz[1:length(lamkz)])
#barplot(100*lamkz/sum(lamkz), names.arg = c(1:length(lamkz)), xlab = "Principal inertias", ylab = "%")
round(cumsum(lamkz[1:length(lamkz)])/sum(lamkz[1:length(lamkz)]), 3) *100

#color given to last column, which is insignificant anyway
Frz[,17] = as.numeric(cut(seq,breaks=breaks))

#################################################################
##### Now for multi-commodity ####################
#################################################################

###  Start with the more frequent occurences:
###
#Set up correspondence matrix
P123 = as.matrix(y123/sum(y123))

#Row and column masses:
Ri123 = apply(P123, 1, sum)
Ci123 = apply(P123, 2, sum)

#Diagonal matrices of row and column masses
Dr123 = diag(Ri123)
Dc123 = diag(Ci123)

#inverse square root of diagonal matrix, where off-diagonal remains 0:
Drs123 = diag(1/sqrt(diag(Dr123)))
Dcs123 = diag(1/sqrt(diag(Dc123)))
## a few 0's in denominator need to be set back to 0:
Drs123[is.infinite(Drs123)] = 0
Dcs123[is.infinite(Dcs123)] = 0

### Calculate the S matrix (A.4 in Greenacre)

S123 = Drs123 %*% (P123 - Ri123 %*% t(Ci123)) %*% Dcs123
##H = Rs %*% B %*% Cs

# Decompose S with SVD:
Ssvd123 = svd(S123)
U123 = Ssvd123$u
V123 = Ssvd123$v
Da123 = diag(Ssvd123$d)
Dl123 = diag(1/sqrt(Ssvd123$d))

#std coordinates, phi, of rows (A.6 Greenacre)
phi123 = Drs123 %*% U123

# std coordinates gam of columns:
gam123 = Dcs123 %*% V123

#principal coordinates F of rows:
Fr123 = phi123 %*% Da123

#principal coordinates Gc of columns (A.9):
Gc123 = gam123 %*% Da123
G123 = solve(Dcs123) %*% t(P123) %*% Fr123 %*% Dl123


#plot of principal intertias, lamk:
par(mfrow=c(1,1))
lamk123 = (Ssvd123$d)^2
#barplot(lamk[1:length(lamk)])
#plot principal intertias as percentages
#barplot(100*lamk123/sum(lamk123), names.arg = c(1:length(lamk123)), xlab = "Principal inertias", ylab = "%")
round(cumsum(lamk123[1:length(lamk123)])/sum(lamk123[1:length(lamk123)]), 3) *100

#color given to last column, which is insignificant anyway
breaks= c(0, 10000, seq(20000,100000,20000),350000)
Fr123[,ul123] = as.numeric(cut(seq123,breaks=breaks))
pCol = c("red", "orange", "yellow", "green", "cadetblue", "light blue", "gray50")

###############################################
###  Now for the less frequent ore deposit groups:
###
#Set up correspondence matrix
Pz123 = as.matrix(z123/sum(z123))

#Row and column masses:
Rzi123 = apply(Pz123, 1, sum)
Czi123 = apply(Pz123, 2, sum)

#Diagonal matrices of row and column masses
Dzr123 = diag(Rzi123)
Dzc123 = diag(Czi123)

#inverse square root of diagonal matrix, where off-diagonal remains 0:
Dzrs123 = diag(1/sqrt(diag(Dzr123)))
Dzcs123 = diag(1/sqrt(diag(Dzc123)))
## a few 0's in denominator need to be set back to 0:
Dzrs123[is.infinite(Dzrs123)] = 0
Dzcs123[is.infinite(Dzcs123)] = 0



### Calculate the S matrix (A.4 in Greenacre)

Sz123 = Dzrs123 %*% (Pz123 - Rzi123 %*% t(Czi123)) %*% Dzcs123

# Decompose S with SVD:
Szsvd123 = svd(Sz123)
Uz123 = Szsvd123$u
Vz123 = Szsvd123$v
Dza123 = diag(Szsvd123$d)
Dzl123 = diag(1/sqrt(Szsvd123$d))

#std coordinates, phi, of rows (A.6 Greenacre)
phiz123 = Dzrs123 %*% Uz123

# std coordinates gam of columns:
gamz123 = Dzcs123 %*% Vz123

#principal coordinates F of rows:
Frz123 = phiz123 %*% Dza123

#principal coordinates Gc of columns (A.9):
Gcz123 = gamz123 %*% Dza123
Gz123 = solve(Dzcs123) %*% t(Pz123) %*% Frz123 %*% Dzl123


#plot of principal intertias, lamk:
lamkz123 = (Szsvd123$d)^2
#barplot(lamkz[1:length(lamkz)])
#first component is 27.0%, second is 16.4 percent, 3rd is 11.4
#barplot(100*lamkz123/sum(lamkz123), names.arg = c(1:length(lamkz123)), xlab = "Principal inertias", ylab = "%")
round(cumsum(lamkz123[1:length(lamkz123)])/sum(lamkz123[1:length(lamkz123)]), 3) *100

#color given to last column, which is insignificant anyway
#Frz[,28] = FrPal(10)[as.numeric(cut(seq123,breaks=breaks))]
Frz123[,28] = as.numeric(cut(seq123,breaks=breaks))


###################################################################
#####Figure 3.  CA for HF ################################
op = par(mfrow = c(1,1))
par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))

plot(as.numeric(Fr[,1]), as.numeric(Fr[,2]), axes = FALSE, type = 'n', pch = 20, col = alpha(Fr[,ul],.75), cex = 2, xlab = "Correspondence Axis 1", ylab = "Correspondence Axis 2")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(Gc[,1], Gc[,2], pch = 0, col = 'gray30', cex = .8)
text(Gc[,1], Gc[,2], labels = labY, col = 'gray30', pos = 3, font = 2,  cex = 0.8)
for(i in rev(c(1,c(1:max(Fr[,ul]))))){
  points(as.numeric(Fr[Fr[,ul]==i,1]), as.numeric(Fr[Fr[,ul]==i,2]), pch = 20, col = alpha(pCol[i],.45), cex = 1.2)
}

axis(side = 2, tck = 0.01)
axis(side = 3, tck = 0.01)
box()


par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(1,2), fig = c(0.6,0.95,0.6,0.95))


legend('bottomleft', legend = breaks[c(1:7)]/1000, col = pCol, pch = 20, pt.cex = 1.5)
legend('bottomright', legend = breaks[c(1:7)]/1000, col = pCol, pch = 1, pt.cex = 0.8)



par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(2,1))

plot(as.numeric(Fr123[,1]), as.numeric(Fr123[,2]), axes = FALSE, type = 'n', pch = 20, col = alpha(Fr123[,ul123],.75), cex = 2, xlab = "Correspondence Axis 1", ylab = "Correspondence Axis 2")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(Gc123[,1], Gc123[,2], pch = 0, col = 'gray30', cex = .8)
text(Gc123[,1], Gc123[,2], labels = labY123, col = 'gray30', pos = 3, font = 2, cex = 0.8)
for(i in rev(c(1,c(1:max(Fr123[,ul123]))))){
  points(as.numeric(Fr123[Fr123[,ul123]==i,1]), as.numeric(Fr123[Fr123[,ul123]==i,2]), pch = 1, col = alpha(pCol[i],.45), cex = 0.8)
}
rect(xleft = -0.5, ybottom =-.5, xright = 0.5, ytop = 0.5, border = 'gray80', lty= 3)

par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(2,1))
axis(side = 2, tck = 0.01)
axis(side = 1, tck = 0.01)
box()


par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(2,2))

par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
plot(as.numeric(Fr123[,1]), as.numeric(Fr123[,2]), axes = FALSE, type = 'n', xlim = c(-.5,.5), ylim = c(-.5,.5))
rect(-1,-1,1,1, col = "white", border = "black")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(Gc123[,1], Gc123[,2], pch = 0, col = 'gray40', cex = .8)
text(Gc123[,1], Gc123[,2], labels = labY123, col = 'gray30', pos = 3, font = 2, cex = 0.8)
for(i in rev(c(1,c(1:max(Fr123[,ul123]))))){
  points(as.numeric(Fr123[Fr123[,ul123]==i,1]), as.numeric(Fr123[Fr123[,ul123]==i,2]), pch = 1, col = alpha(pCol[i],.45), cex = 0.8)
}

#rect(-.5,0.1,-0.1,0.5, col = "white", border = "gray30")
axis(side = 4, tck = 0.01)
axis(side = 1, tck = 0.01)
box()




###################################################################
#####END Figure 3:  Note: touched up in Illustrator ###############




########################################################################################
#####FIG 4; CA FOR ALL TYPES; NO FILTER #################################################
op = par(mfrow = c(1,1))
par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2), bg = 'white')

plot(as.numeric(Frz[,1]), as.numeric(Frz[,2]), axes = FALSE,type = 'n', pch = 20, cex = 2)
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(Gcz[,1], Gcz[,2], pch = 0, col = 'gray30', cex = .8)
text(Gcz[,1], Gcz[,2], labels = labZ, col = 'gray30', pos = 3, cex = 0.8, font = 2)
for(i in rev(c(1,c(1:max(Frz[,17]))))){
  points(as.numeric(Frz[Frz[,17]==i,1]), as.numeric(Frz[Frz[,17]==i,2]), pch = 20, col = alpha(pCol[i],.45), cex = 1.2)
}
axis(side = 2, tck = 0.01)
axis(side = 3, tck = 0.01)
box()

rect(xleft = -.5, ybottom =-.7, xright = 0.5, ytop = .5, border = 'gray80', lty= 3)

par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(1,2))

plot(as.numeric(Frz[,1]), as.numeric(Frz[,2]), axes = FALSE, type = 'n', xlim = c(-.5,.5), ylim = c(-.7,.5))
rect(-1,-1,1,1, col = "white", border = "black")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(Gcz[,1], Gcz[,2], pch =0, col = 'gray40', cex = .8)
text(Gcz[,1], Gcz[,2], labels = labZ, col = 'gray30', pos = 3, cex = 0.8, font = 2)
for(i in rev(c(1,c(1:max(Frz[,17]))))){
  points(as.numeric(Frz[Frz[,17]==i,1]), as.numeric(Frz[Frz[,17]==i,2]), pch = 20, col = alpha(pCol[i],.45), cex = 1.2)
}
axis(side = 4, tck = 0.01)
axis(side = 3, tck = 0.01)
box()


par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(2,1))


plot(as.numeric(-1*Frz123[,1]), as.numeric(Frz123[,2]), axes = FALSE, xlim = c(-1,2),ylim=c(-3.5,1),type = 'n', pch = 20, cex = 2, xlab = "Correspondence Axis 1", ylab = "Correspondence Axis 2")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(-1*Gcz123[,1], Gcz123[,2], pch = 0, col = 'gray40', cex = .8)
#text(-1*Gcz123[,1], Gcz123[,2], labels = labZ123, col = 'gray30', pos = 3, cex = 0.8)
for(i in rev(c(1,c(1:max(Frz123[,28]))))){
  points(-1*as.numeric(Frz123[Frz123[,28]==i,1]), as.numeric(Frz123[Frz123[,28]==i,2]), pch = 1, col = alpha(pCol[i],.75), cex = 0.8)
}
axis(side = 2, tck = 0.01)
axis(side = 1, tck = 0.01)
box()

rect(xleft = -.6, ybottom =-.4, xright = 0.4, ytop = .6, border = 'gray80', lty= 3)

par(mfrow = c(2,2),mar = c(0,0,0,0), oma = c(5,4,4,2))
par(mfg = c(2,2))
plot(-1*as.numeric(Frz123[,1]), as.numeric(Frz123[,2]), axes = FALSE, type = 'n', xlim = c(-.4,.6), ylim = c(-.4,.6))
rect(-1,-1,1,1, col = "white", border = "black")
abline(v = 0, h = 0, lty = 2, lwd = 0.5, col = 'gray60')
points(-1*Gcz123[,1], Gcz123[,2], pch = 0, col = 'gray40', cex = .8)
text(-1*Gcz123[,1], Gcz123[,2], labels = labZ123, col = 'gray30', pos = 3, cex = 0.8, font = 2)
for(i in rev(c(1,c(1:max(Frz123[,28]))))){
  points(-1*as.numeric(Frz123[Frz123[,28]==i,1]), as.numeric(Frz123[Frz123[,28]==i,2]), pch = 1, col = alpha(pCol[i],.75), cex = .8)
}
axis(side = 4, tck = 0.01)
axis(side = 1, tck = 0.01)
box()


#####
############
################

######## Principal inertia figures for Supplementary Information ########

par(mfrow = c(2,2),mar = c(2,2,2,2), oma = c(5,4,4,2), bg = 'white')


barplot(100*lamk/sum(lamk), names.arg = c(1:length(lamk)), xlab = "Principal inertias", ylab = "%")
barplot(100*lamkz/sum(lamkz), names.arg = c(1:length(lamkz)), xlab = "Principal inertias", ylab = "%")
barplot(100*lamk123/sum(lamk123), names.arg = c(1:length(lamk123)), xlab = "Principal inertias", ylab = "%")
barplot(100*lamkz123/sum(lamkz123), names.arg = c(1:length(lamkz123)), xlab = "Principal inertias", ylab = "%")

mtext("% of total inertia", side = 2, outer = TRUE)
mtext("Principal inertias", side = 1, outer = TRUE)


### END ###