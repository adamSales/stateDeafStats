library(openxlsx)
library(readr) ## read in the csvs faster
                                        #library(survey)
library(dplyr)
states <- read.csv('states.csv')


### reads in, formats, saves 5-year data
### ACS 2012-2016
makeDes <- function(hh=TRUE){

## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv')[,c('SERIALNO','DEAR','ST','AGEP','ENG','LANX','PWGTP',paste0('PWGTP',1:80))]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    for(ll in letters[2:4]){
        print(ll)
        ndat <- read_csv(paste0('../../data/acs5yr2016/ss16pus',ll,'.csv'))[,c('SERIALNO','DEAR','ST','AGEP','ENG','LANX','PWGTP',paste0('PWGTP',1:80))]
        for(nn in names(ndat)) if(is.character(ndat[[nn]])) ndat[[nn]] <- parse_integer(ndat[[nn]])
        sdat <- rbind(sdat,ndat)
        rm(ndat);gc()
    }

    hdat <- read_csv('../../data/acs5yr2016/ss16husa.csv')[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))]
    for(ll in letters[2:4])
        hdat <- rbind(hdat, read_csv(paste0('../../data/acs5yr2016/ss16hus',ll,'.csv'))[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))])
    for(nn in names(hdat)) if(is.character(hdat[[nn]])) hdat[[nn]] <- parse_integer(hdat[[nn]])

    if(hh) sdat <- sdat[,-grep('PWGTP',names(sdat))]

    sdat <- merge(sdat,hdat,by='SERIALNO')

    sdat$state <- states$abb[match(sdat$ST,states$x)]


    sdat
}


### reads in, formats, saves 5-year data
### ACS 2012-2016 Puerto Rico
makeDesPR <- function(hh=TRUE){

## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../../data/acs5yr2016/ss16ppr.csv')[,c('SERIALNO','DEAR','ST','AGEP','ENG','LANX','PWGTP',paste0('PWGTP',1:80))]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])

    hdat <- read_csv('../../data/acs5yr2016/ss16hpr.csv')[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))]
    for(nn in names(hdat)) if(is.character(hdat[[nn]])) hdat[[nn]] <- parse_integer(hdat[[nn]])

    if(hh) sdat <- sdat[,-grep('PWGTP',names(sdat))]

    sdat <- merge(sdat,hdat,by='SERIALNO'); rm(hdat);gc()

    sdat$state <- 'PR'

    sdat
}



pLevelKeep <- function(){

    sdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv')[,c('SERIALNO','DEAR','AGEP')]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    sdat <- sdat%>%filter(AGEP<65)%>%filter(AGEP>=25)
    sn <- unname(unlist(sdat%>%distinct(SERIALNO)))
    deaf <- unname(unlist(sdat%>%filter(DEAR==1)%>%distinct(SERIALNO)));rm(sdat);gc()


    for(ll in letters[2:4]){
        print(ll)
        sdat <- read_csv(paste0('../../data/acs5yr2016/ss16pus',ll,'.csv'))[,c('SERIALNO','DEAR','AGEP')]
        for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
        sdat <- sdat%>%filter(AGEP<65)%>%filter(AGEP>=25)
        sn <- c(sn, unname(unlist(sdat%>%distinct(SERIALNO))))
        deaf <- c(deaf,unname(unlist(sdat%>%filter(DEAR==1)%>%distinct(SERIALNO))))
        rm(sdat);gc()
    }
    sdat <- read_csv('../../data/acs5yr2016/ss16ppr.csv')[,c('SERIALNO','DEAR','AGEP')]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    sdat <- sdat%>%filter(AGEP<65)%>%filter(AGEP>=25)
    sn <- c(sn, unname(unlist(sdat%>%distinct(SERIALNO))))
    deaf <- c(deaf,unname(unlist(sdat%>%filter(DEAR==1)%>%distinct(SERIALNO))))
    rm(sdat);gc()


    list(deafid=deaf,allid=sn)
}

makeHHdat <- function(ids){
    hdat <- read_csv('../../data/acs5yr2016/ss16husa.csv')[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))]
    for(nn in names(hdat)) if(is.character(hdat[[nn]])) hdat[[nn]] <- parse_integer(hdat[[nn]])
    for(ll in letters[2:4]){
        nhdat <- read_csv(paste0('../../data/acs5yr2016/ss16hus',ll,'.csv'))[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))]
        for(nn in names(nhdat)) if(is.character(nhdat[[nn]])) nhdat[[nn]] <- parse_integer(nhdat[[nn]])
        hdat[(nrow(hdat)+1):(nrow(hdat)+nrow(nhdat)),] <- nhdat
    }

    nhdat <- read_csv('../../data/acs5yr2016/ss16hpr.csv')[,c('SERIALNO','TYPE','HHL','WGTP',paste0('WGTP',1:80))]
    for(nn in names(nhdat)) if(is.character(nhdat[[nn]])) nhdat[[nn]] <- parse_integer(nhdat[[nn]])
    hdat[(nrow(hdat)+1):(nrow(hdat)+nrow(nhdat)),] <- nhdat

    hearIDs <- setdiff(ids$allid,ids$deafid)

    hear <- filter(hdat,SERIALNO%in%hearIDs)
    deaf <- filter(hdat,SERIALNO%in%ids$deafid)

    list(deaf=deaf,hear=hear)
}


hhLev <- function(sdat){
    weights <- sdat[,c('SERIALNO','WGTP',paste0('WGTP',1:80))]
    sdat <- sdat%>%filter(TYPE==1)%>%filter(AGEP<65)%>%filter(AGEP>=25)%>%group_by(SERIALNO)%>%
        summarize(state=state[1],dear=min(DEAR,na.rm=TRUE),hhl=HHL[1])
    weights <- aggregate(weights,by=list(SERIALNO=weights$SERIALNO),function(x) x[1],na.rm=TRUE)
    sdat <- cbind(sdat,weights[match(sdat$SERIALNO,weights$SERIALNO),-1])
}

### more data formatting
### restricts data to ages 25-64 and excludes subjects in "institutional" housing
makeVars <- function(sdat){
    sdat <- sdat%>%filter(type!=2)%>%filter(AGEP<65)%>%filter(AGEP>=25)%>%
        mutate(onlyEng=LANX==2)
#    sdat <- select(sdat,-ST,-AGEP,-ADJINC,-SCHL,-ESR,-WKW,-WKHP)

    names(sdat) <- tolower(names(sdat))

    sdat
}

## weighted mean (faster than weighted.mean() for some reason. gives same result)
svmean <- function(x,w,na.rm=TRUE){
    w <- w/sum(w)
    sum(x*w,na.rm=na.rm)
}

est1 <- function(dat,wtnm='pwgtp',na.rm=TRUE){
    x <- dat$onlyeng
    w1 <- dat[[wtnm]]
    ws <- dat[,paste0(wtnm,1:80)]
    muhat <- svmean(x,w1,na.rm=na.rm)
    jk <- vapply(ws,function(w) svmean(x,w,na.rm=na.rm),1)
    c(muhat, sqrt(mean((jk-muhat)^2)*4))
}

est <- function(sdat,sdatPR){
    sdat <- rbind(sdat,sdatPR); rm(sdatPR); gc()
    sdat <- makeVars(sdat)%>%select(dear,onlyeng,contains('pwgtp'))
    hear <- sdat%>%filter(dear==2)
    deaf <- sdat%>%filter(dear==1)

    inSample <- c(overall=mean(sdat$onlyeng,na.rm=TRUE),
                  deaf=mean(deaf$onlyeng,na.rm=TRUE),
                  hear=mean(hear$onlyeng,na.rm=TRUE))

    inPop <- rbind(
        overall=est1(sdat),
        deaf=est1(deaf),
        hear=est1(hear))

    list(inSample, inPop)
}

estHH <- function(sdat,sdatPR){
    sdat <- rbind(sdat,sdatPR[,names(sdat)]); rm(sdatPR); gc()
    sdat <- hhLev(sdat); gc()


### if you want to re-run the analysis using saved datasets:
savedData <- function(){
    us <- load('../data/rankDataTot.RData')
    sdatUS <- makeVars(get(us))
    rm(list=us); gc()

    pr <- load('../data/rankDataPR.RData')
    sdatPR <- makeVars(get(pr))
    rm(list=pr); gc()


    sdat <- rbind(sdatUS,sdatPR); rm(sdatUS,sdatPR); gc()
    sdat
}

