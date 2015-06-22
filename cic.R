library(XML)
library(ggplot2)

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
  invitations=append(invitations, as.integer(sub(',','',sub('Footnote \\*','',x))))
  
  x=xpathApply(cic,paste('//details[',i,']//td',sep=''),xmlValue)[2]
  points=append(points,as.integer(sub(' points','',x)))
}

dat = data.frame(time, invitations, points)

plot(dat$time, 
     dat$invitations,
     type='l', 
     col="blue",
     #main='Rounds of invitations',
     ylim=c(min(dat$points),max(dat$invitations)),
     yaxt='n',
     xlab = 'Date',
     ylab = '')

yi = pretty(c(0,max(dat$invitations)))
axis(2, pretty(c(0, max(dat$invitations))), col='blue', las=2)

par(new=TRUE)

plot(dat$time, 
     dat$points, 
     type='l', 
     col='red', 
     axes=FALSE, 
     ylab='',
     xlab='')

yp = pretty(c(0,max(dat$points)))
axis(4, pretty(c(0, max(dat$points))), col='red',las=2)


#ggplot(dat, aes(time)) +                    
#  geom_line(aes(y=invitations), colour="blue") +  
#  geom_line(aes(y=points), colour="red")

rm(x, details, cic, i, url)
