load('WB_BE_paper_code.RData')

#load('paper_data_2.RData')

## Data names follow the convention:
## Grain size (g1 = GSD1, g2 = GSD2)
## Discharge (q100 = 0.1 l/s, q200 = 0.2 l/s)
## Feed rate (l = 1 g/s, h = 2 g/s)
## Slope (slope values)
## Transport (sediment transport efficiency calculated from Eaton and Church (2011))
## t### (classification of image for Time = ###, i.e. Time = 327 and 150 minutes)
## 
## gs1d/gs2d (percentage of total per class of grain size)
## gs1/gs2 (cumulative percentage of grain size)
## grainsizes (class upper limit)
## gsizes (mean value of upper and lower bounds of classes)

## Grain size distribution plot
## Figure 2 

par(mfrow = c(2,1), mar = c(1.75, 4.1, 1, 0), oma = c(2,0,0.2,0))
plot(gsizes[2:11], gs1d[1:10], ylim = c(0, 55), xlim =c(0.2, 12), col = '#fbc577', type = 'o', pch = 1, log = 'x',
     xlab = '', ylab = 'Proportion of Total Mass (%)', lwd = 2.5)
points(gsizes[2:11], gs2d[1:10], col = '#0000e7', type = 'o', pch = 1, lwd = 2.5)
legend('topleft', col = c(NA, '#fbc577', '#0000e7'), lty = 1, pch = 1, lwd = 2.5,
       legend = c(expression(~~~~~~~~~~~~~~~~~~D[50]~~~D[84]~~~~~sigma),expression(GSD[broad]~~2.03~3.65~1.38), expression(GSD[narrow]~2.02~2.52~0.42)), bty = 'n')
legend('topright', legend = '(a)', bty = 'n')
plot(grainsizes[2:12], gs1[2:12], col = '#fbc577', type = 'o', pch = 1, lwd =3, log = 'x',
     xlab = 'Grain Size (mm)', ylab = 'Cumulative Finer Than (%)', xlim =c(0.2, 12))
points(grainsizes[2:12], gs2[1:11], col = '#0000e7', type = 'o', pch = 1, lwd = 2.5)
legend('topright', legend = '(b)', bty = 'n')
par(mfrow = c(1,1), new = T)
plot(0,0, axes = F, xlab = '',ylab = '', pch = NA)
mtext(as.character('Grain Size (mm)'), side = 1, line = 2.5)

## Example classified rasters
## Figure 4

breakpoints = c(-0.5, 0.5, 1.5, 2.5, 3.5)

colors = c('#fbc577', '#0000e7', '#ffffff', '#f0f0f0')
legColors = c('#ffffff', '#fbc577', '#0000e7', '#dddddd')
e <- as(extent(64, 2011, 275, 601), 'SpatialPolygons')
crs(e) = crs(g1q100l_t327)
g1 = crop(g1q100l_t327,e)
g2 = crop(g2q100l_t150,e)

par(mfrow = c(2,1),mar = c(2.1,2.1,1.1,5.1), oma = c(0,0,0,2))
plot(g1, box = F, legend = F, axes = F, col = colors)
axis(at = seq(64, 1964, 200), labels = seq(0,1900,200),side = 1, pos = 275, lwd = 4)
axis(2, labels = seq(0,300,150), at = seq(275, 575,150), pos = 64, lwd = 4)
segments(68,438,2007,438, col = 'grey40', lwd = 4)
segments(68,415,68,461, col = 'grey40', lwd = 4)
segments(2007,415,2007,461, col = 'grey40', lwd = 4)
segments(1034,275,1034, 600, col = 'grey40', lwd = 4)
segments(1011,596,1057,596, col = 'grey40', lwd = 4)
segments(1011,279,1057,279, col = 'grey40', lwd = 4)
legend(x = 1700, y = 620, legend = c("x = 1.44 m", "y = 0.28 m"), bty = 'n')
legend(x = 50, y = 620, legend = "(a)", bty = 'n')
plot(g2, box = F, legend = F, axes = F, col = colors)
axis(at = seq(64,1964, 200), labels = seq(0,1900,200),side = 1, pos = 275, lwd = 4)
axis(2, labels = seq(0,300,150), at = seq(275,575,150), pos = 64, lwd = 4)
legend(x = 50, y = 620, legend = "(b)", bty = 'n')
par(mfrow=c(1, 1), new=FALSE)
plot(g1q100l_t327, legend.only=TRUE, legend.shrink=1, legend.width=2, zlim=c(0, 3),
     col = legColors, breaks = breakpoints,
     axis.args=list(at= c(0, 1, 2, 3),
                    labels= c('NA', 'Sediment', 'Water', 'Background')),
     smallplot = c(0.87,0.89, 0.1 , 0.95),
     legend.args=list(text='Pixel Class', side=4, line=5))

## Example water surface profiles
## Figure 5

par(mfrow = c(2,2), mar = c(3,2,0,0.5), oma = c(0.5,1.5,0.5,0.5))
plot(r2[[326]]/100, xlim=c(0,1.5), ylim = c(0,0.2), col='grey80', pch = 0, cex = 0.8)
legend('topleft', legend = "(a)", bty = 'n', col = NA)
legend('topright', legend = c("T = 326 min","T = 438 min","T = 549 min"),
       bty = 'n', col = c('grey80', 'grey50', 'grey20'), pch = 0, lty = 1)
abline(lm(y~x, data = r2[[326]]/100),col='grey80' )
points(r2[[438]]/100, col = 'grey50', pch = 0, cex = 0.8)
abline(lm(y~x, data = r2[[428]]/100),col='grey50' )
points(r2[[549]]/100, col = 'grey20', pch = 0, cex = 0.8)
abline(lm(y~x, data = r2[[549]]/100),col='grey20' )
plot(r4[[149]]/100, xlim=c(0,1.5), ylim = c(0,0.2),col='grey80', pch = 0, cex = 0.8)
legend('topleft', legend = "(b)", bty = 'n', col = NA)
legend('topright', legend = c("T = 149 min","T = 223 min","T = 297 min"),
       bty = 'n', col = c('grey80', 'grey50', 'grey20'), pch = 0, lty = 1)
abline(lm(y~x, data = r4[[149]]/100),col='grey80' )
points(r4[[223]]/100, col = 'grey50', pch = 0, cex = 0.8)
abline(lm(y~x, data = r4[[223]]/100),col='grey50' )
points(r4[[297]]/100, col = 'grey20', pch = 0, cex = 0.8)
abline(lm(y~x, data = r4[[297]]/100),col='grey20' )
plot(r7[[149]]/100, xlim=c(0,1.5), ylim = c(0,0.2),col='grey80', pch = 0, cex = 0.8)
legend('topleft', legend = "(c)", bty = 'n', col = NA)
legend('topright', legend = c("T = 149 min","T = 289 min","T = 429 min"),
       bty = 'n', col = c('grey80', 'grey50', 'grey20'), pch = 0, lty = 1)
abline(lm(y~x, data = r7[[149]]/100),col='grey80' )
points(r7[[289]]/100, col = 'grey50', pch = 0, cex = 0.8)
abline(lm(y~x, data = r7[[289]]/100),col='grey50' )
points(r7[[429]]/100, col = 'grey20', pch = 0, cex = 0.8)
abline(lm(y~x, data = r7[[429]]/100),col='grey20' )
plot(r9[[116]]/100, xlim=c(0,1.5), ylim = c(0,0.2),col='grey80', pch = 0, cex = 0.8)
legend('topleft', legend = "(d)", bty = 'n', col = NA)
legend('topright', legend = c("T = 116 min","T = 201 min","T = 286 min"),
       bty = 'n', col = c('grey80', 'grey50', 'grey20'), pch = 0, lty = 1)
abline(lm(y~x, data = r9[[116]]/100),col='grey80' )
points(r9[[201]]/100, col = 'grey50', pch = 0, cex = 0.8)
abline(lm(y~x, data = r9[[201]]/100),col='grey50' )
points(r9[[286]]/100, col = 'grey20', pch = 0, cex = 0.8)
abline(lm(y~x, data = r9[[286]]/100),col='grey20' )
par(mfrow=c(1,1),oma = c(0.5,1.5,0.5,0.5), new = T)
plot(0,0,col=NA, axes = F)
mtext("Distance Downstream (m)",side=1,line=2.5)
mtext("Water Surface Height (m)", side = 2, line = 2.5)



## Boxplots of slope values
## Figure 6


box_plotR = function(x, y, colourfill = 1, width = 2){
  y <- y[!is.na(y)]
  ysum <- summary(y)
  outup = 1.5*IQR(y, na.rm = T) + ysum[5]
  outlo = ysum[2] - 1.5*IQR(y, na.rm = T)
  yup = y > outup
  ylo = y < outlo
  ycom = yup + ylo
  ysum = summary(y[!ycom])
  segments(x - 15, ysum[1],x + 15, ysum[1], col = colourfill,lwd = width)
  segments(x - 15, ysum[6],x + 15, ysum[6], col = colourfill,lwd = width)
  segments(x,ysum[5],x, ysum[6], col = colourfill, lty = 3,lwd = width)
  segments(x,ysum[1],x, ysum[2], col = colourfill, lty = 3,lwd = width)
  polygon(c(x - 25, x - 25,x + 25, x + 25),
          c(ysum[2],ysum[5],ysum[5],ysum[2]),
          border = colourfill,
          lwd = width)
  segments(x - 25, ysum[3], x + 25, ysum[[3]], col = colourfill, lwd = width)
  points(rep(x, sum(yup)), y[yup], pch = 16, col = colourfill)
  points(rep(x, sum(ylo)),y[ylo], pch = 16, col = colourfill)
}

par(mfrow = c(2,1), oma = c(4,4,1,1), mar = c(1.5,0,1.5,0))
plot(100, 50, ylim = c(0.02,0.1), xlim = c(50,250), pch= NA,
     ylab = expression(eta), xlab = '', xaxt = 'n')
axis(1, at = c(100,200), labels = c(0.1, 0.2))
box_plotR(100, g1q100l_slope, colourfill = '#fbc577', width = 2.5)
box_plotR(100, g2q100l_slope, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200h_slope, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200h_slope, colourfill = '#0000e7', width = 2.5)
text(c(100, 200), c(0.024), labels = c('Low Supply', 'High Supply'))
legend('topleft', legend = '(a)', bty = 'n', pch = NA)
legend('topright', legend = c(expression(GSD[broad], GSD[narrow])), bty = 'n', pch = NA, lty = 1,col = c('#fbc577','#0000e7'), lwd = 2.5)
plot(100, 50, ylim = c(0.02,0.1), xlim = c(50,250), pch= NA,
     ylab = expression(eta), xlab ='', xaxt='n')
axis(1, at = c(100,200), labels = c(0.1, 0.2))
box_plotR(100, g2q100h_slope, colourfill = '#0000e7', width = 2.5)
box_plotR(100, g1q100h_slope, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200l_slope, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200l_slope, colourfill = '#fbc577', width = 2.5)
text(c(100, 200), c(0.024), labels = c('High Supply',  'Low Supply'))
legend('topleft', legend = '(b)', bty = 'n', pch = NA)
par(mfrow =c(1,1),new = T)
plot(0,0,pch = NA, yaxt = 'n', xaxt='n', axes = F)
mtext(expression(Discharge~(l~s^-1)), side = 1, line =2.5)
mtext(expression(Slope~(m~m^-1)), side = 2, line =2.5)

## Output GSD plots
## Figure 7


plot(grainout$size[1:11], grainout$g1[2:12], log = "x", type = 'l', col = '#fbc577',
     xlab = "Grain Size (mm)", ylab = "Cumulative Finer Than (%)", lwd = 2.5)
lines(grainout$size[1:11], grainout$r2[2:12], col = '#fbc577', lty = 2, lwd = 2.5)
lines(grainout$size[1:11], grainout$r4[2:12], col = '#fbc577', lty = 3, lwd = 2.5)
lines(grainout$size[1:11], grainout$r6[2:12], col = '#fbc577', lty = 4, lwd = 2.5)
lines(grainout$size[1:11], grainout$r5[2:12], col = '#fbc577', lty = 5, lwd = 2.5)
lines(grainout$size[1:11], grainout$g2[2:12], col = '#0000e7', lwd = 2.5)
lines(grainout$size[1:11], grainout$r7[2:12], col = '#0000e7', lty = 2, lwd = 2.5)
lines(grainout$size[1:11], grainout$r9[2:12], col = '#0000e7', lty = 3, lwd = 2.5)
lines(grainout$size[1:11], grainout$r10[2:12], col = '#0000e7', lty = 4, lwd = 2.5)
lines(grainout$size[1:11], grainout$r11[2:12], col = '#0000e7', lty = 5, lwd = 2.5)
legend('topleft', legend = c(expression(GSD[broad]), expression(GSD[narrow]), "Feed", "100L", "100H", "200L", "200H"), lwd = 2.5,
       ncol = 1, bty = 'n', lty = c(1,1,1,2,4,5,3,2,4,5,3), col = c('#fbc577','#0000e7', 1,1,1,1,1,1) )



## Boxplots of sediment transport efficiency
## Figure 8

par(mfrow = c(2,1), oma = c(4,4,1,1), mar = c(1.5,0,1.5,0))
plot(100, 50, ylim = c(0.0,0.1), xlim = c(50,250), pch= NA,
     ylab = expression(eta), xlab = '', xaxt = 'n')
box_plotR(100, g1q100l_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(100, g2q100l_transport, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200h_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200h_transport, colourfill = '#0000e7', width = 2.5)
axis(1, at = c(100,200), labels = c(0.1, 0.2))
legend('topleft', legend = '(a)', bty = 'n')
legend('topright', legend = c(expression(GSD[broad]), expression(GSD[narrow])),
       col = c('#fbc577','#0000e7'), lty = 1, bty = 'n', lwd = 2.5)
text(c(100,200), c(0.002), labels = c('Low Supply','High Supply'))
plot(100, 50, ylim = c(0.0,0.1), xlim = c(50,250), pch= NA,
     ylab = '', xlab = '', xaxt = 'n')
box_plotR(100, g1q100h_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(100, g2q100h_transport, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200l_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200l_transport, colourfill = '#0000e7', width = 2.5)
legend('topleft', legend = '(b)', bty = 'n')
text(c(100,200), c(0.002), labels = c('High Supply',  'Low Supply'))
axis(1, at = c(100,200), labels = c(0.1, 0.2))
par(mfrow =c(1,1),new = T)
plot(0,0,pch = NA, yaxt = 'n', xaxt='n', axes = F)
mtext(expression(Discharge~(l~s^-1)), side = 1, line =2.5)
mtext(expression(paste("Calculated Transport Efficiency (",eta,")")), side = 2, line =2.5)
