load('WB_BE_paper_code.RData')

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

par(mfrow = c(2,1), mar = c(0, 4.1, 0, 0), oma = c(5,0,0.2,0)) #2 by 1 framework
plot(gsizes[1:11], gs1d[1:11], ylim = c(0, 55), xlim =c(0.2, 12), col = '#fbc577',
     type = 'o', pch = 1, log = 'x', xlab = '', ylab = 'Proportion of Total Mass (%)',
     xaxt = 'n', lwd = 3) # individual classes 
points(gsizes[1:11], gs2d[1:11], col = '#0000e7', type = 'o', pch = 1, lwd = 3)
legend('topright', col = c('#fbc577', '#0000e7'), lty = 1, pch = 1, lwd = 3,
       legend = c(expression(GSD[1]), expression(GSD[2])), bty = 'n')
legend('topleft', legend = '(a)', bty = 'n')
plot(grainsizes[2:12], gs1[2:12], col = '#fbc577', type = 'o', pch = 1, lwd =3,
     log = 'x', xlab = 'Grain Size (mm)', ylab = 'Cumulative Finer Than (%)',
     xlim =c(0.2, 12)) # cumulative classes
points(grainsizes[2:12], gs2[1:11], col = '#0000e7', type = 'o', pch = 1, lwd = 3)
legend('topleft', legend = '(b)', bty = 'n')
par(mfrow = c(1,1), new = T)
plot(0,0, axes = F, xlab = '',ylab = '', pch = NA)
mtext(as.character('Grain Size (mm)'), side = 1, line = 4) # add x-axis label

## Example classified rasters
## Figure 3

breakpoints = c(-0.5, 0.5, 1.5, 2.5, 3.5)
colors = c('#7cba83', '#7d9dba', '#ffffff', '#dddddd')
legColors = c('#ffffff', '#7cba83', '#7d9dba', '#dddddd')
par(mfrow = c(2,1),mar = c(2.1,2.1,1.1,6.1))
plot(g1q100l_t327, box = F, legend = F, axes = F, col = colors)
plot(g2q100l_t150, box = F, legend = F, axes = F, col = colors)
par(mfrow=c(1, 1), new=FALSE)
plot(g1q100l_t327, legend.only=TRUE, legend.shrink=1, legend.width=2, zlim=c(0, 3),
     col = legColors, breaks = breakpoints,
     axis.args=list(at= c(0, 1, 2, 3),
                    labels= c('NA', 'Sediment', 'Water', 'Background')),
     smallplot = c(0.87,0.89, 0.1 , 0.95),
     legend.args=list(text='Pixel Class', side=4, line=5))


## Boxplots of slope values
## Figure 4


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
text(c(100, 200), c(0.024), labels = c('L', 'H'))
legend('topleft', legend = '(a)', bty = 'n', pch = NA)
legend('topright', legend = c(expression(GSD[1], GSD[2])), bty = 'n', pch = NA, lty = 1,col = c('#fbc577','#0000e7'), lwd = 2)
plot(100, 50, ylim = c(0.02,0.1), xlim = c(50,250), pch= NA,
     ylab = expression(eta), xlab ='', xaxt='n')
axis(1, at = c(100,200), labels = c(0.1, 0.2))
box_plotR(100, g2q100h_slope, colourfill = '#0000e7', width = 2.5)
box_plotR(100, g1q100h_slope, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200l_slope, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200l_slope, colourfill = '#fbc577', width = 2.5)
text(c(100, 200), c(0.024), labels = c('H',  'L'))
legend('topleft', legend = '(b)', bty = 'n', pch = NA)
par(mfrow =c(1,1),new = T)
plot(0,0,pch = NA, yaxt = 'n', xaxt='n', axes = F)
mtext(expression(Discharge~(l~s^-1)), side = 1, line =3)
mtext(expression(Slope~(m~m^-1)), side = 2, line =2)

## Boxplots of sediment transport efficiency
## Figure 5

par(mfrow = c(2,1), oma = c(4,4,1,1), mar = c(1.5,0,1.5,0))
plot(100, 50, ylim = c(0.0,0.1), xlim = c(50,250), pch= NA,
     ylab = expression(eta), xlab = '', xaxt = 'n')
box_plotR(100, g1q100l_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(100, g2q100l_transport, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200h_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200h_transport, colourfill = '#0000e7', width = 2.5)
axis(1, at = c(100,200), labels = c(0.1, 0.2))
legend('topright', legend = '(a)', bty = 'n')
legend('topleft', legend = c(expression(GSD[1]), expression(GSD[2])),
       col = c('#fbc577','#0000e7'), lty = 1, bty = 'n', lwd = 2)
text(c(100,200), c(0.002), labels = c('L', 'H'))
plot(100, 50, ylim = c(0.0,0.1), xlim = c(50,250), pch= NA,
     ylab = '', xlab = '', xaxt = 'n')
box_plotR(100, g1q100h_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(100, g2q100h_transport, colourfill = '#0000e7', width = 2.5)
box_plotR(200, g1q200l_transport, colourfill = '#fbc577', width = 2.5)
box_plotR(200, g2q200l_transport, colourfill = '#0000e7', width = 2.5)
legend('topright', legend = '(b)', bty = 'n')
text(c(100,200), c(0.002), labels = c('H', 'L'))
axis(1, at = c(100,200), labels = c(0.1, 0.2))
par(mfrow =c(1,1),new = T)
plot(0,0,pch = NA, yaxt = 'n', xaxt='n', axes = F)
mtext(expression(Discharge~(l~s^-1)), side = 1, line =3)
mtext(expression(eta), side = 2, line =3)
