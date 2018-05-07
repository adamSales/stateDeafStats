library(openxlsx)
library(readr) ## read in the csvs faster
                                        #library(survey)
library(sandwich)
library(lmtest)
library(dplyr)
states <- read.csv('states.csv')

source('estimate.r')

makeDat <- function(){
    dat <- NULL
    for(year in c('08','09','10','11','12','13','14','15','16')){
        print(year)
        for(part in c('usa','usb','pr')){
            print(part)
            #sink('temp.txt')
            p1 <-
                read_csv(
                    paste0('../../data/acs/historical/ss',year,'p',part,'.csv'),progress=FALSE,
                )[,c('SERIALNO','DEAR','ST','AGEP','SCHL',
                     'ESR','WKW','WKHP','PWGTP',paste0('pwgtp',1:80))]
            for(nn in names(p1)) if(is.character(p1[[nn]])) p1[[nn]] <- parse_integer(p1[[nn]])
            h1 <- read.csv(paste0('../../data/acs/historical/ss',year,'h',part,'.csv'))[,c('SERIALNO','TYPE')]
            p1 <- p1[p1$SERIALNO%in%h1$SERIALNO[h1$TYPE!=2],]
            rm(h1); gc()
            #sink()
            p1$SERIALNO <- NULL
            p1$year <- as.numeric(paste0('20',year))

            p1 <- makeVars(p1)

            p1$state <- NA

            p1$state <- if(part=='pr') 'PR' else states$abb[match(p1$st,states$x)]

            dat <- rbind(dat,p1)
        }
    }
    save(dat,file='trendDat.RData')
    dat
}

makeVars <- function(sdat){
    sdat <- sdat%>%filter(AGEP<65)%>%filter(AGEP>=25)%>%
        mutate(hs = SCHL>=16,
               ba = SCHL>=21,
               employed = ESR%in%c(1,2,4,5),
               unemployed = ESR==3,
               fulltime=(WKW==1 & WKHP>=35))
#    sdat <- select(sdat,-ST,-AGEP,-ADJINC,-SCHL,-ESR,-WKW,-WKHP)

    names(sdat) <- tolower(names(sdat))

    sdat
}


survlm <- function(dat,age,hs){
    form <- if(age){
                if(hs) hs~year+as.factor(AGEP) else ba~year+as.factor(AGEP)
            } else if(hs) hs~year else ba~year

    mod <- lm(form,dat,weights=PWGTP)
    b0 <- coef(mod)['year']
    var0 <- vcovHC(mod)['year','year']
    beta <- vapply(grep('pwgtp',names(dat)),
                   function(i){
                       dat$rw <- dat[,i]
                       coef(lm(form,dat,weights=rw,subset=rw>=0))['year']
                   },1)
    se <- sqrt(4/80*sum((beta-b0)^2)+var0)
    T <- b0/se
    p <- 2*pnorm(-abs(T))
    c(est=unname(b0),se=se,T=unname(T),p=unname(p))
}


stateYearEst <- function(ss,x){
    ests <- vapply(c('',1:80),
                   function(i) est(x=x,w=paste0('pwgtp',i),ss)['gap'],1)
    est <- ests[1]
    reps <- ests[-1]
    c(est=est,vvv=4*mean((reps-est)^2))
}

byYearState <- function(dat,x){
    sss <- split(dat,with(dat,list(state,year)))
    out <- vapply(sss,stateYearEst,numeric(2),x=x)
    out <- as.data.frame(t(out))
    stateYear <- strsplit(rownames(out),'.',fixed=TRUE)
    out$state <- vapply(stateYear,function(x) x[1],'a')
    out$year <- vapply(stateYear,function(x) x[2],'a')
    out
}

trendStateVar <- function(ss){
    ss$year <- as.numeric(ss$year)
    mod <- lm(est~year,weights=1/vvv,data=ss)
    coeftest(mod,vcovHC)['year',]
}

stateTrends <- function(x,dat){
    bys <- byYearState(dat,x)
    bys.splt <- split(bys,bys$state)
    out <- sapply(bys.splt,trendStateVar)
    out <- as.data.frame(t(out))
    out[['p.adj']] <- p.adjust(out[['Pr(>|t|)']],'holm')
    out[['p.fdr']] <- p.adjust(out[['Pr(>|t|)']],'fdr')
    out$Estimate <- out$Estimate*100
    out[['Std. Error']] <- out[['Std. Error']]*100
    out
}

allTrends <- function(dat){
    l <- list(
        hs=stateTrends('hs',dat),
        ba=stateTrends('ba',dat),
        employment=stateTrends('employed',dat))
    write.xlsx(l,'gapTrends.xlsx',row.names=TRUE)
}




