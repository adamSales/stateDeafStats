library(openxlsx)
library(readr) ## read in the csvs faster
                                        #library(survey)
library(dplyr)
states <- read.csv('states.csv')


### reads in, formats, saves 5-year data
### ACS 2012-2016
makeDes <- function(){

## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv')[,c('SERIALNO','DEAR','ST','AGEP','ADJINC','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('PWGTP',1:80))]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    for(ll in letters[2:4]){
        print(ll)
        ndat <- read_csv(paste0('../../data/acs5yr2016/ss16pus',ll,'.csv'))[,c('SERIALNO','DEAR','ST','AGEP','ADJINC','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('PWGTP',1:80))]
        for(nn in names(ndat)) if(is.character(ndat[[nn]])) ndat[[nn]] <- parse_integer(ndat[[nn]])
        sdat <- rbind(sdat,ndat)
        rm(ndat);gc()
    }

    hdat <- read_csv('../../data/acs5yr2016/ss16husa.csv')[,c('SERIALNO','TYPE')]
    for(ll in letters[2:4])
        hdat <- rbind(hdat, read_csv(paste0('../../data/acs5yr2016/ss16hus',ll,'.csv'))[,c('SERIALNO','TYPE')])

    sdat$type <- hdat$TYPE[match(sdat$SERIALNO,hdat$SERIALNO)]; rm(hdat); gc()

    sdat$adj <- sdat$ADJINC/1e6
    sdat$PERNP <- sdat$PERNP*sdat$adj

    sdat$state <- states$abb[match(sdat$ST,states$x)]

    save(sdat,file='../data/rankDataTot.RData')
    sdat
}

### reads in, formats, saves 5-year data
### ACS 2012-2016 Puerto Rico
makeDesPR <- function(){

## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../../data/acs5yr2016/ss16ppr.csv')[,c('SERIALNO','DEAR','ST','AGEP','ADJINC','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('PWGTP',1:80))]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])

    hdat <- read_csv('../../data/acs5yr2016/ss16hpr.csv')[,c('SERIALNO','TYPE')]

    sdat$type <- hdat$TYPE[match(sdat$SERIALNO,hdat$SERIALNO)]; rm(hdat); gc()

    sdat$adj <- sdat$ADJINC/1e6
    sdat$PERNP <- sdat$PERNP*sdat$adj

    sdat$state <- 'PR'

    save(sdat,file='../data/rankDataPR.RData')
    sdat
}


### more data formatting
### restricts data to ages 25-64 and excludes subjects in "institutional" housing
makeVars <- function(sdat){
    sdat <- sdat%>%filter(type!=2)%>%filter(AGEP<65)%>%filter(AGEP>=25)%>%
        mutate(hs = SCHL>=16,
               ba = SCHL>=21,
               employed = ESR%in%c(1,2,4,5),
               unemployed = ESR==3,
               fulltime=(WKW==1 & WKHP>=35))
#    sdat <- select(sdat,-ST,-AGEP,-ADJINC,-SCHL,-ESR,-WKW,-WKHP)

    names(sdat) <- tolower(names(sdat))

    sdat
}

## weighted mean (faster than weighted.mean() for some reason. gives same result)
svmean <- function(x,w,na.rm=TRUE){
    w <- w/sum(w)
    sum(x*w,na.rm=na.rm)
}

### estimates mean of x using weights w from `data`
### for deaf,hearing, and the gap
est <- function(x,w,data,na.rm=TRUE){
    data <- data[,c(x,w,'dear')]
    names(data) <- c('x','w','dear')
    local <- with(data, list(
        deaf=svmean(x[dear==1],w[dear==1]),
        hear=svmean(x[dear==2],w[dear==2])
    ))
    local <- within(local,gap <- deaf-hear)
    return(unlist(local))
}

### estimates weighted means of variable x in state st for
### main weights and all 80 sets of replicate weights
### for deaf, hearing, and the gap
### returns a 3x81 array
tot1state <- function(x,st='',data){
    if(st==''|st=='US') stateDat <- data[,c(x,'dear',paste0('pwgtp',c('',1:80)))]
    else stateDat <- data[data$state==st,c(x,'dear',paste0('pwgtp',c('',1:80)))]
    vapply(c('',1:80),
           function(i) est(x=x,w=paste0('pwgtp',i),stateDat,st),
           numeric(3))
}


### estimates weighted means of variable x for mean weights and all replicate weights
### for deaf, hearing and the gap
### for all 50 stats +DC+total
### returns 3x81x52 array
tot1var <- function(x,data)
    vapply(c('US','DC',state.abb,'PR'),function(st) tot1state(x,st,data),matrix(1.1,3,81))


### computes SEs from t1v
### returns matrix 52x3
stateSEs <- function(t1v){
    est <- t1v[,1,]
    t1v <- t1v[,-1,]
    dn <- dimnames(t1v)
    out <- matrix(NA,nrow=dim(t1v)[3],ncol=dim(t1v)[1],dimnames=dn[c(3,1)])
    for(vv in dn[[1]])
        for(ss in dn[[3]])
            out[ss,vv] <- sqrt(mean((t1v[vv,,ss]-est[vv,ss])^2)*4)
    out
}

### combines estimates and SEs for variable x
### returns matrix 52x6 (deaf,hearing,gap)x(est,se)
est1var <- function(x,sdat){
    t1v <- tot1var(x,sdat)
    se <- stateSEs(t1v)
    out <- matrix(NA,nrow(se),ncol(se)*2)
    rownames(out) <- rownames(se)
    colnames(out) <- rep('',ncol(out))
    for(i in 1:ncol(se)){
        out[,2*i-1] <- t1v[i,1,]
        colnames(out)[2*i-1] <- colnames(se)[i]
        out[,2*i] <- se[,i]
        colnames(out)[2*i] <- paste0(colnames(se)[i],'.','SE')
    }
    out
}

sampleSize1 <- function(st,data){
    if(!st%in%c('US',''))
        data <- data[data$state==st,]
    setNames(as.vector(table(data$dear)),c('Deaf','Hearing'))
}

sampleSizeFunc <- function(data)
    vapply(c('US','DC',state.abb,'PR'),sampleSize1,data=data,1:2)

## if you've already downloaded, cleaned, etc the data, start here:
makeEstimates <- function(data){
    l <- list(
        HS=est1var('hs',data),
        BA=est1var('ba',data),
        Unemployment=est1var('unemployed',data),
        `Total Employment`=est1var('employed',data),
        `Sample Size`=t(sampleSizeFunc(data))
    )
    l <- lapply(l,function(x) round(x*100,1))
    write.xlsx(l,'HS.BA.Unemployment.Employment.byState.xlsx',row.names=TRUE)
    l
}

## otherwise this do
everything <- function(){
    sdat <- makeDes()
    sdatPR <- makeDesPR()
    sdat <- rbind(sdat,sdatPR)
    sdat <- makeVars(sdat)
    makeEstimates(sdat)
}
