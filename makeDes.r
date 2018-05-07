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

