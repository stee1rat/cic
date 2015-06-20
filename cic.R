library(XML)
library(ggplot2)
library(gridExtra)

url = "http://www.cic.gc.ca/english/express-entry/past-rounds.asp"
cic=xmlParse(url, isHTML = TRUE)
details=xpathSApply(cic,'//details')

time=c()
invitations=c()
points=c()

for (i in 1:length(details)) {
  x=xpathApply(cic,paste('//details[',i,']//time',sep=''),xmlGetAttr,'datetime')[[1]]
  time=append(time, as.Date(x)) 

  x=xpathApply(cic,paste('//details[',i,']//td',sep=''),xmlValue)[1]
  invitations=append(invitations, sub('Footnote \\*','',x))
  
  x=xpathApply(cic,paste('//details[',i,']//td',sep=''),xmlValue)[2]
  points=append(points,sub(' points','',x))
}

dat = data.frame(time, invitations, points)

p1 = ggplot(data=dat, aes(x=time, y=invitations, group=1)) + 
     geom_line(colour='red')
p2 = ggplot(data=dat, aes(x=time, y=points, group=1)) + 
     geom_line(colour='blue')

grid.arrange(p1,p2) 

rm(x, details, cic, i, url)