#### author : Łukasz Wawrowski ####

library(Rfacebook)
library(rjson)
library(wordcloud)
library(XML)
library(Cairo)
library(maps)
library(mapdata)
library(wordcloud)
library(ggplot2)
library(geosphere)

token="CAACEdEose0cBdHRLNJuZAa2dEwYNoT9R4T7g9F8Qq6ol62ssOpIU7aiLuwdh4OKV3p443ge7BntfiIA0o3NZCcqtqw7uZCE0A4mZAOEx32x7lBxNHWcB4kVwOzVxDSBTxw1o630xMuBPbP5ld2ZC9kJnWwYoMeDYGW12cxHLp8k3ha6E7VLD9df9bLr9JMlQZD"

a_me=getUsers("me", token=token)

b_friends=getFriends(token=token)

c_likes=getLikes("me", 200, token)

d_newsFeed=getNewsfeed(token, 100)

e_page=getPage("sknEstymator", token)

f_posts=searchFacebook(string="pazur", token=token, n=100)

#mapy

location=b_friends[complete.cases(b_friends$location),]

geo_location=matrix(0,nrow(location),2)
colnames(geo_location)=c("location_lng", "location_lat")

for(i in 1:nrow(location)){
  Sys.sleep(0.5)
  miejsce=location$location[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_location[i, 1]=lng
    geo_location[i, 2]=lat
  }, error= function(err){
    geo_location[i, 1]=0
    geo_location[i, 2]=0
  })
}

location_geo=cbind(location, geo_location)

location_freq=as.data.frame(table(location$location))
colnames(location_freq)=c("place","freq")

geo_location_freq=matrix(0,nrow(location_freq),2)
colnames(geo_location_freq)=c("location_lng", "location_lat")

for(i in 1:nrow(location_freq)){
  Sys.sleep(0.5)
  miejsce=location_freq$place[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_location_freq[i, 1]=lng
    geo_location_freq[i, 2]=lat
  }, error= function(err){
    geo_location_freq[i, 1]=0
    geo_location_freq[i, 2]=0
  })
}

location_freq_geo=cbind(location_freq, geo_location_freq)

location_freq_sort=location_freq_geo[order(location_freq_geo$freq, decreasing=T),]

max.symbol.size=3
min.symbol.size=1
location_freq_sort$size=((location_freq_sort$freq-min(location_freq_sort$freq))/(max(location_freq_sort$freq)-min(location_freq_sort$freq))*(max.symbol.size-min.symbol.size)+min.symbol.size)

CairoPNG("mapa_fb_location.png", width=800, height=800)
map('worldHires', 'Poland')
points(location_freq_sort$location_lng, location_freq_sort$location_lat, pch=16, cex=location_freq_sort$size, col='#3B5998')
dev.off()

hometown=b_friends[complete.cases(b_friends$hometown),]

geo_hometown=matrix(0,nrow(hometown),2)
colnames(geo_hometown)=c("hometown_lng", "hometown_lat")

for(i in 1:nrow(hometown)){
  Sys.sleep(0.5)
  miejsce=hometown$hometown[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_hometown[i, 1]=lng
    geo_hometown[i, 2]=lat
  }, error= function(err){
    geo_hometown[i, 1]=0
    geo_hometown[i, 2]=0
  })
}

hometown_geo=cbind(hometown, geo_hometown)

hometown_freq=as.data.frame(table(hometown$hometown))
colnames(hometown_freq)=c("place","freq")

geo_hometown_freq=matrix(0,nrow(hometown_freq),2)
colnames(geo_hometown_freq)=c("hometown_lng", "hometown_lat")

for(i in 1:nrow(hometown_freq)){
  Sys.sleep(0.5)
  miejsce=hometown_freq$place[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_hometown_freq[i, 1]=lng
    geo_hometown_freq[i, 2]=lat
  }, error= function(err){
    geo_hometown_freq[i, 1]=0
    geo_hometown_freq[i, 2]=0
  })
}

hometown_freq_geo=cbind(hometown_freq, geo_hometown_freq)

hometown_freq_sort=hometown_freq_geo[order(hometown_freq_geo$freq, decreasing=T),]

max.symbol.size=3
min.symbol.size=1
hometown_freq_sort$size=((hometown_freq_sort$freq-min(hometown_freq_sort$freq))/(max(hometown_freq_sort$freq)-min(hometown_freq_sort$freq))*(max.symbol.size-min.symbol.size)+min.symbol.size)

CairoPNG("mapa_fb_hometown.png", width=800, height=800)
map('worldHires', 'Poland')
points(hometown_freq_sort$hometown_lng, hometown_freq_sort$hometown_lat, pch=16, cex=hometown_freq_sort$size, col='#3B5998')
dev.off()

#imiona

names=as.data.frame(table(b_friends$first_name))

CairoPNG("imiona.png", width=800, height=800)
wordcloud(names$Var1,names$Freq,c(8,.3),1,)
dev.off()

#wiek

data_ur=b_friends[complete.cases(b_friends$birthday),]
data_ur$rok=as.numeric(substr(data_ur$birthday,7,10))

data_ur_cc=data_ur[complete.cases(data_ur$rok),]

data_ur_cc$wiek=2014-data_ur_cc$rok

wiek=as.data.frame(table(data_ur_cc$wiek))

CairoPNG("wiek.png", width=1000, height=600)
ggplot(data_ur_cc, aes(x = wiek)) + stat_bin(binwidth=1)
dev.off()

#migracje

migration=b_friends[is.na(b_friends$hometown)==F & is.na(b_friends$location)==F,]

geo_hometown=matrix(0,nrow(migration),2)
colnames(geo_hometown)=c("hometown_lng", "hometown_lat")

for(i in 1:nrow(migration)){
  Sys.sleep(0.5)
  miejsce=migration$hometown[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_hometown[i, 1]=lng
    geo_hometown[i, 2]=lat
  }, error= function(err){
    geo_hometown[i, 1]=0
    geo_hometown[i, 2]=0
  })
}

geo_location=matrix(0,nrow(migration),2)
colnames(geo_location)=c("location_lng", "location_lat")

for(i in 1:nrow(migration)){
  Sys.sleep(0.5)
  miejsce=migration$location[i]
  url = paste('http://maps.googleapis.com/maps/api/geocode/xml?address=',miejsce,'&sensor=true', sep="")
  doc = xmlTreeParse(url, useInternal=TRUE)
  tryCatch({
    lat = as.numeric(xmlValue(getNodeSet(doc, '//location/lat')[[1]]))
    lng = as.numeric(xmlValue(getNodeSet(doc, '//location/lng')[[1]]))
    geo_location[i, 1]=lng
    geo_location[i, 2]=lat
  }, error= function(err){
    geo_location[i, 1]=0
    geo_location[i, 2]=0
  })
}

migration_geo=cbind(migration, geo_hometown, geo_location)

load("world_countries.rda")

CairoPNG("migracje.png", width=800, height=800)

plot(world_countries, xlim=c(5,25), ylim=c(45,58), col="#EAEAEA", lwd=0.5)
#map('worldHires', 'Poland')
for(i in 1:nrow(migration)){
  start=as.numeric(migration_geo[i,15:16])
  end=as.numeric(migration_geo[i,17:18])
  inter=gcIntermediate(start, end, n=100, addStartEnd=TRUE)
  lines(inter, col="cornflowerblue", lwd=1)
}

dev.off()