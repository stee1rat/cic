library(XML)

# Information about the previous rounds

url="http://www.cic.gc.ca/english/express-entry/past-rounds.asp"
cic=xmlParse(url, isHTML = TRUE)
details=xpathSApply(cic,'//details')

time=c()
invitations=c()
points=c()

for (i in 1:length(details)) {
  x=xpathApply(cic,paste('//details[',i,']//time',sep=''),xmlGetAttr,'datetime')[[1]]
  time=append(time, as.Date(x)) 

  x=xpathApply(cic,paste('//details[',i,']//td',sep=''),xmlValue)[1]
  invitations=append(invitations, as.integer(gsub('[^0-9]','',x)))
  
  x=xpathApply(cic,paste('//details[',i,']//td',sep=''),xmlValue)[2]
  points=append(points,as.integer(gsub('[^0-9]','',x)))
}

# Information about the last round

url="http://www.cic.gc.ca/english/express-entry/rounds.asp"
cic=xmlParse(url, isHTML = TRUE)

x=xpathApply(cic,'//section//time', xmlGetAttr,'datetime')[[1]]
time=append(time, as.Date(x)) 

x=xpathApply(cic,'//td',xmlValue)[1]
invitations=append(invitations, as.integer(sub(',','', gsub('[^0-9]','',x))))

x=xpathApply(cic,'//td',xmlValue)[2]
points=append(points, as.integer(gsub('[^0-9]','',x)))

dat = data.frame(time, invitations, points)

# Plotting

par(oma=c(0,0,0,1))

plot(dat$time, dat$invitations, type='c', 
     col="blue", main='Rounds of invitations',
     ylim=c(min(dat$points),max(dat$invitations)),
     yaxt='n', xaxt='n', xlab = '', ylab = '')

#axis(2, pretty(dat$invitations), las=2)
axis.Date(1, dat$time, format="%b %Y")

text(dat$time, dat$invitations, dat$invitations, cex=0.6, col='blue', font=2)
 
par(new=TRUE)

plot(dat$time, dat$points, type='c', 
     col='red', axes=FALSE, ylab='', xlab='')

#axis(4, pretty(dat$points), las=2)

text(dat$time, dat$points, dat$points, cex=0.6, col='red', font=2)

legend('topright', c('Invitations', 'Points'), lty=1, col=c('red', 'blue'),
       horiz = TRUE, bty="n", xpd = TRUE, cex=.80)

rm(x, details, cic, i, url)
