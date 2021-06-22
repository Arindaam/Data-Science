library(reshape2)
library(dplyr)
library(stringi)
library(ggplot2)
db=read.csv("steam2.csv",sep=",")
summary(db)
#Data Cleaning
new=db[is.na(db$positive_ratings),]
new1=db[is.na(db$negative_ratings),]

p=as.factor(tapply(db$positive_ratings,db$owners,mean,na.rm=TRUE))
p1=as.factor(tapply(db$negative_ratings,db$owners,mean,na.rm=TRUE))

new$positive_ratings=as.integer(as.character(p[new$owners]))
new1$negative_ratings=as.integer(as.character(p1[new1$owners]))
db[match(new$appid, db$appid), ] <- new
db[match(new1$appid, db$appid), ] <- new1

db$total_ratings=db$negative_ratings + db$positive_ratings

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

p=as.character(getmode(db$platforms))
db$platforms=replace(db$platforms,db$platforms=="", p)

db$positive_rat= db$positive_ratings / db$total_ratings

new=db[is.na(db$price),]
p=as.factor(tapply(db$price,db$publisher,mean,na.rm=TRUE))
new$price=as.integer(as.character(p[new$publisher]))
db[match(new$appid, db$appid), ] <- new

new=db[is.na(db$price),]
p=as.factor(tapply(db$price,db$developer,mean,na.rm=TRUE))
new$price=as.integer(as.character(p[new$developer]))
db[match(new$appid, db$appid), ] <- new

new=db[is.na(db$price),]
p=as.factor(tapply(db$price,db$owners,mean,na.rm=TRUE))
new$price=as.integer(as.character(p[new$owners]))
db[match(new$appid, db$appid), ] <- new

db=db[db$english==1,]

db$platforms=as.character(db$platforms)

db$windows=stri_detect_fixed(db$platforms,"windows")
db$mac=stri_detect_fixed(db$platforms,"mac")
db$linux=stri_detect_fixed(db$platforms,"linux")

db$Multiplayer=stri_detect_fixed(db$categories,"Multi-player")
db$Singleplayer=stri_detect_fixed(db$categories,"Single-player")

db$action=stri_detect_fixed(db$genres,"Action")
db$adventure=stri_detect_fixed(db$genres,"Adventure")
db$casual=stri_detect_fixed(db$genres,"Casual")
db$sports=stri_detect_fixed(db$genres,"Sports")
db$racing=stri_detect_fixed(db$genres,"Racing")
db$strategy=stri_detect_fixed(db$genres,"strategy")

db$steamspy_tags<-NULL
db$categories=NULL
db$genres=NULL
db$platforms=NULL

cut_points = c(-1, 0, 4, 10, 30, 50, 1000)
label_names = c('free', 'very cheap', 'cheap', 'moderate', 'expensive', 'very expensive')
db$price_range=cut(db$price,cut_points,labels=label_names)

db$release_date=as.Date(db$release_date)
db$year=as.numeric(format(db$release_date,format="%Y"))

#End of Preprocessing
pv=droplevels(db)
ggplot(pv, aes(x = owners, y = positive_rat)) +geom_boxplot()
ggplot(pv, aes(x = owners, y = price)) +geom_boxplot()
ggplot(pv, aes(x = price_range, y = positive_rat)) +geom_boxplot()
#----------------------------------------------------------------------------
#Removing Outliers
outliers=boxplot(pv$price)$out
pv[-which(pv$price %in% outliers)]
#------------------------------------------------------------------------------
pv=db[db$year>2006,]
cut_points = c(-1, 30,1000)
label_names = c('moderate', 'expensive')
pv$price_range=cut(pv$price,cut_points,labels=label_names)

pv %>%
  group_by(price_range) %>%
  summarize(mean.positive_rat = mean(positive_rat),
            sd.positive_rat = sd(positive_rat))
pv %>%
group_by(price_range) %>%
            summarize(num.obs = n(),
            mean.positive_rat = round(mean(positive_rat), 0),
            sd.positive_rat = round(sd(positive_rat), 0),
            se.positive_rat = round(sd(positive_rat) / sqrt(num.obs), 0))

pv.t.test <- t.test(pv$positive_ratings ~ pv$price_range, data = pv)
pv.t.test
#------------------------------------------------------------------------------
pv %>%
  group_by(action) %>%
  summarize(num.obs = n(),
            mean.average_playtime = round(mean(average_playtime), 0),
            sd.average_playtime= round(sd(average_playtime), 0),
            se.average_playtime= round(sd(average_playtime) / sqrt(num.obs), 0))

pv.t.test <- t.test(pv$average_playtime ~ pv$price_range, data = pv)
pv.t.test
#-----------------------------------------------------------------------------
#Before Normalising plot
p.pv <- ggplot(data = pv, aes(sample = pv$positive_rat))
p.pv + stat_qq() + stat_qq_line()

p.pv <- ggplot(data = pv, aes(sample = pv$average_playtime))
p.pv + stat_qq() + stat_qq_line()

pv$positive_rat=scale(pv$positive_rat)
pv$positive_ratings=scale(pv$positive_ratings)
pv$average_playtime=scale(pv$average_playtime)
#After Normalising Plot
p.pv <- ggplot(data = pv, aes(sample = pv$positive_rat))
p.pv + stat_qq() + stat_qq_line()

p.pv <- ggplot(data = pv, aes(sample = pv$average_playtime))
p.pv + stat_qq() + stat_qq_line()
#---------------------------------------------------------------------------
success=sum(db$action==1)
failed=sum(db$action==0)
psuccess=success/(success+failed)

x=seq(1,50,by=1)
y=dbinom(x,50,psuccess)
plot(x,y)

a=sum(db[db$action==1,]$positive_ratings)
b=sum(db[db$casual==1,]$positive_ratings)
c=sum(db[db$adventure==1,]$positive_ratings)
d=sum(db[db$sports==1,]$positive_ratings)
e=sum(db[db$racing==1,]$positive_ratings)
f=sum(db[db$strategy==1,]$positive_ratings)
m=cbind.data.frame(c('action','sports','casual','adventure','racing','strategy'),c(a,e,b,c,d,f))
colnames(m)=(c('Genre','positive_rating'))

p<-ggplot(data=m, aes(x=Genre, y=positive_rating)) +
  geom_bar(stat="identity")
p

a=sum(db$windows==1)
b=sum(db$linux==1)
c=sum(db$mac==1)
m=cbind.data.frame(c('Linux','Mac','Windows'),c(b,c,a))
colnames(m)=(c('Platform','Users'))
p<-ggplot(data=m, aes(x=Platform, y=Users)) +
  geom_bar(stat="identity")
p

mv=db[db$action==1,]
m <- mv %>%
  group_by(year) %>%
  summarise(counts = n())

ggplot(m, aes(x = year, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts))
ggplot(mv, aes(x=year)) + geom_histogram(binwidth=1)
-------------------------------------------------------------------------
pv=pv[pv$positive_ratings<1,]
pv=pv[pv$average_playtime<20,]
ggplot(db, aes(x=price, y=positive_rat)) + geom_point()

dm=db[sample(nrow(db),100),]
dm=dm[dm$price<18,]
ggplot(dm, aes(x=negative_ratings, y=positive_ratings)) + geom_point()
-------------------------------------------------------------------------
new=db[db$year>"2006",]
eventTime <- new %>%
  group_by(price_range, year) %>%
  summarise(n = n()) 
# Change the point size, and shape
library(circlize)
circos.par("track.height" =0.4)
circos.initialize(factors = eventTime$price_range, x = eventTime$year)

circos.trackPlotRegion(factors = eventTime$price_range, y=eventTime$n,  force.ylim = F,
                       panel.fun = function(x, y) {
                         circos.axis(h='bottom',direction='inside', labels.cex=0.3, lwd=0.3)
                         sector.index = get.cell.meta.data("sector.index")
                         xcenter = get.cell.meta.data("xcenter")
                         ycenter = get.cell.meta.data("ycenter")
                         circos.text(xcenter, ycenter + (ycenter/2), sector.index)})

circos.trackLines(eventTime$price_range, eventTime$year, eventTime$n,
                  col = eventTime$price_range, lwd = 1, type = "h")

text(0,0,"How Games were priced through the years?", cex = 0.7)

