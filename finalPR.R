rdsfiles=dir(".","my.*[2,3].*rds")
mydf <- readRDS("xmy_flights.rds")

# grep proper colnames
colnames(mydf) <- namesx

#reduce df to significant observations
targetdf = mydf[,c(1,2,4,6,7,8)]

# remove factor
targetdf$callsign=unlist(lapply(targetdf$callsign, function(x) as.character(x)))
targetdf$icao24=unlist(lapply(targetdf$icao24, function(x) as.character(x)))
targetdf$latitude=unlist(lapply(targetdf$latitude, function(x) as.numeric(as.character(x))))
targetdf$longitude=unlist(lapply(targetdf$longitude, function(x) as.numeric(as.character(x))))
targetdf$baro_altitude=unlist(lapply(targetdf$baro_altitude, function(x) as.numeric(as.character(x))))
targetdf$time_position=unlist(lapply(targetdf$time_position, function(x) as.numeric(as.character(x))))

# countdf
targetcountcall = as.data.frame(table(targetdf$callsign))
targetcounticao = as.data.frame(table(targetdf$icao24))

# get a df with one row pr flight with a timestamp
targetmaxtime = aggregate(targetdf$time_position, by=list(targetdf$icao24), max)
# convert this timestamp to readable date. 
targetmaxtime$date = as.POSIXct(targetmaxtime$x,origin="1970-01-01")
# prepare this to be merged with the RSquared-results by common id - icao.
colnames(targetmaxtime) = c("icao","time_position","date")

# compute cirkfit=lm(retval$longitude ~ retval$latitude, data=retval) on all icaos
# do summary(cirkfit)$r.squared and add this to dficao

# split dataframen op i grupper ud fra deres icao
flightlist = split(targetdf,f=targetdf$icao24)

# beregn hvor godt en rute-sekvens lat og long fitter en lineær model
# jo mindre den fitter desto flere udsving undervejs

res=unlist(lapply(flightlist, function(x) summary(lm(x$longitude ~ x$latitude, data=x))$r.squared))
mres=as.data.frame(res)

# convert to dataframe and get icao as separate column - not index
resmres = cbind(icao= rownames(mres),mres)

# reset index
rownames(resmres) <- NULL

# THE FINAL RESULT - sorted by lowes rsquared first
total = merge(targetmaxtime,resmres,by="icao")
totalsorted = total[order(total$res),]

