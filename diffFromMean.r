library(readr) ## read in the csvs faster
                                        #library(survey)
library(dplyr)
source('../generalCode/cumMean.r')
states <- read.csv('../data/acs5yr2016/states.csv')

makeDes <- function(){

## need: DEAR, attain, employment,PERNP, fulltime

    sdat <- read_csv('../data/acs5yr2016/ss16pusa.csv')[,c('SERIALNO','DEAR','ST','AGEP','ADJINC','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('PWGTP',1:80))]
    for(nn in names(sdat)) if(is.character(sdat[[nn]])) sdat[[nn]] <- parse_integer(sdat[[nn]])
    for(ll in letters[2:4]){
        print(ll)
        ndat <- read_csv(paste0('../data/acs5yr2016/ss16pus',ll,'.csv'))[,c('SERIALNO','DEAR','ST','AGEP','ADJINC','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('PWGTP',1:80))]
        for(nn in names(ndat)) if(is.character(ndat[[nn]])) ndat[[nn]] <- parse_integer(ndat[[nn]])
        sdat <- rbind(sdat,ndat)
        rm(ndat);gc()
    }

    hdat <- read_csv('../data/acs5yr2016/ss16husa.csv')[,c('SERIALNO','TYPE')]
    for(ll in letters[2:4])
        hdat <- rbind(hdat, read_csv(paste0('../data/acs5yr2016/ss16hus',ll,'.csv'))[,c('SERIALNO','TYPE')])

    sdat$type <- hdat$TYPE[match(sdat$SERIALNO,hdat$SERIALNO)]; rm(hdat); gc()

    sdat$adj <- sdat$ADJINC/1e6
    sdat$PERNP <- sdat$PERNP*sdat$adj

    sdat$state <- states$abb[match(sdat$ST,states$x)]

    save(sdat,file='data/rankDataTot.RData')
    sdat
}

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

svmean <- function(x,w,na.rm=TRUE){
    w <- w/sum(w)
    sum(x*w,na.rm=na.rm)
}

stateEst <- function(x,w,data,st,na.rm=TRUE){
    data <- data[data$state==st,c(x,w,'dear')]
    names(data) <- c('x','w','dear')
    ## xO <- x[state!=st]
    ## wO <- w[state!=st]
    ## dearO <- dear[state!=st]

    local <- with(data, list(
#        overall=svmean(x,w),
        deaf=svmean(x[dear==1],w[dear==1]),
        hear=svmean(x[dear==2],w[dear==2])
    ))
    local <- within(local,gap <- deaf-hear)
    return(unlist(local))
    ## oth <- list(
    ##     overall=svmean(xO,wO),
    ##     deaf=svmean(xO[dearO==1],wO[dearO==1]),
    ##     hear=svmean(xO[dearO==2],wO[dearO==2])
    ## )
    ## oth <- within(oth,gap <- deaf-hear)
    ## c(unlist(local),diff=unlist(local)-unlist(oth))
}

tot1state <- function(x,st,data)
    vapply(c('',1:80),
           function(i) stateEst(x=x,w=paste0('pwgtp',i),data,st),
           numeric(3))

tot1var <- function(x,data)
    vapply(c('DC',state.abb),function(st) tot1state(x,st,data),matrix(1.1,3,81))


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

makeWeights <- function(){
    deafWeights <- vapply(c('DC',state.abb),
                          function(st)
                              colSums(sdat[sdat$state==st & sdat$dear==1,paste0('pwgtp',c('',1:80))]),
                          numeric(81))
    deafWeights <- sweep(deafWeights,1,rowSums(deafWeights),'/')
    hearWeights <- vapply(c('DC',state.abb),
                          function(st)
                              colSums(sdat[sdat$state==st & sdat$dear==2,paste0('pwgtp',c('',1:80))]),
                          numeric(81))
    hearWeights <- sweep(hearWeights,1,rowSums(hearWeights),'/')
    list(deafWeights,hearWeights)
}

stateEstDiff1 <- function(t1v,deaf,ss){
    dn <- dimnames(t1v)
    vv <- ifelse(deaf,'deaf','hear')
    if(deaf) weights <- deafWeights[,-which(colnames(deafWeights)==ss)]
    else weights <- hearWeights[,-which(colnames(hearWeights)==ss)]
    stEsts <- t1v[vv,,ss]
    restEsts <- t1v[vv,,-which(dn[[3]]==ss)]

    restEsts <- vapply(1:nrow(restEsts),
                       function(ww) sum(restEsts[ww,]*weights[paste0('pwgtp',rownames(restEsts)[ww]),]),1)

    list(stEsts,restEsts)
}

pval <- function(mu,sig) 2*pnorm(-abs(mu/sig))

stateDiffP <- function(t1v,ss){
    df <- stateEstDiff1(t1v,TRUE,ss)
    hr <- stateEstDiff1(t1v,FALSE,ss)
    deafEst <- df[[1]][1]-df[[2]][1]
    deafSE <- sqrt(mean((df[[1]][-1]-df[[2]][-1]-deafEst)^2)*4)
    hearEst <- hr[[1]][1]-hr[[2]][1]
    hearSE <- sqrt(mean((hr[[1]][-1]-hr[[2]][-1]-hearEst)^2)*4)
    gapEst <- deafEst-hearEst
    gapSE <- sqrt(mean((df[[1]][-1]-hr[[1]][-1]-(df[[2]][-1]-hr[[2]][-1])-gapEst)^2)*4)
    c(deaf=pval(deafEst,deafSE),hear=pval(hearEst,hearSE),gap=pval(gapEst,gapSE))
}

stateDiff <- function(t1v){
    overall <- c(deaf=sum(deafWeights[1,]*t1v['deaf',1,]),
                 hear=sum(hearWeights[1,]*t1v['hear',1,]))
    overall['gap'] <- overall['deaf']-overall['hear']

    sweep(t1v[,1,],1,overall,'-')
}

est1var <- function(x,sdat){
    t1v <- tot1var(x,sdat)
    se <- stateSEs(t1v)
    out <- matrix(NA,nrow(se),ncol(se)*2)
    rownames(out) <- rownames(se)
    colnames(out) <- rep('',ncol(out))
    for(i in 1:ncol(se)){
        out[,2*i-1] <- t1v[1,1,]
        colnames(out)[2*i-1] <- colnames(se)[i]
        out[,2*i] <- se[,i]
        colnames(out)[2*i] <- paste0(colnames(se)[i],'.','SE')
    }
    out
}



### code to make first figure:
complete <- function(x){
    t1v <- tot1var(x,sdat)
    diff <- stateDiff(t1v)
    ps <- vapply(c('DC',state.abb),function(ss) stateDiffP(t1v,ss),numeric(3))
    write.csv(cbind(difference=diff['deaf',],`p-value`=round(ps['deaf',],5)),paste0(x,'deafDiffFromMean.csv'))
    write.csv(cbind(difference=diff['gap',],`p-value`=round(ps['gap',],5)),paste0(x,'gapDiffFromMean.csv'))
    pdf(paste0(x,'deafDiffFromMean.pdf'),width=9,height=6)
    barplot(diff['deaf',order(diff['deaf',])],las=2,main=paste('State Differences in',toupper(x),'from National Level: Deaf Rate'))
    deafStars <- ifelse(ps['deaf',]<0.05,'*','')#,ifelse(ps['deaf',]<0.01,'**',ifelse(ps['deaf',]<0.05,'*','')))
    text(seq(.7,by=1.2,length=51),ifelse(sort(diff['deaf',])< 0,0.002,-0.002),deafStars[order(diff['deaf',])])
    dev.off()

    pdf(paste0(x,'gapDiffFromMean.pdf'),,width=9,height=6)
    barplot(diff['gap',order(diff['gap',])],las=2,main=paste('State Differences in',toupper(x),'from National Level: Deaf-Hearing Gap'))
    gapStars <- ifelse(ps['gap',]<0.05,'*','')#**',ifelse(ps['gap',]<0.01,'**',ifelse(ps['gap',]<0.05,'*','')))
    text(seq(.7,by=1.2,length=51),ifelse(sort(diff['gap',])< 0,0.002,-0.002),gapStars[order(diff['gap',])])
    dev.off()
}

stateEstSE <- function(x,st,data){
    est <- stateEst(x=data[[x]],w=data$pwgtp,state=data$state,st=st,dear=data$dear)
    reps <- vapply(1:80,function(i)
        stateEst(x=data[[x]],w=data[[paste0('pwgtp',i)]],state=data$state,st=st,dear=data$dear),
        numeric(8))
    se <- vapply(1:8, function(p) sqrt(4*mean((reps[p,]-est[p])^2)),1)
    names(se) <- paste0(names(est),'SE')
    rm(reps); gc()
    Tstat <- est/se
    names(Tstat) <- paste0(names(est),'T')
    pval <- 2*pnorm(-abs(Tstat))
    names(pval) <- paste0(names(est),'P')
    c(est,se,Tstat,pval)
}

replicateDS <- function(st){

    dat <- read.csv(paste0('data/repDS/ss16p',st,'.csv'))[,c('SERIALNO','DEAR','AGEP','PERNP','SCHL','ESR','WKW','WKHP','PWGTP',paste0('pwgtp',1:80))]


    hdat <- read.csv(paste0('data/repDS/ss16h',st,'.csv'))[,c('SERIALNO','TYPE')]

    dat$type <- hdat$TYPE[match(dat$SERIALNO,hdat$SERIALNO)]#; rm(hdat); gc()


    dat <- dat%>%filter(type!=2)%>%filter(AGEP<65)%>%filter(AGEP>=21)%>%
        mutate(hs = SCHL>=16,
               ba = SCHL>=21,
               emp1 = ESR%in%c(1,2,4,5),
               emp2 = ESR%in%c(1,2),
               unemployed = ESR==3,
               fulltime=(WKW==1 & WKHP>=35))
    names(dat) <- tolower(names(dat))
    with(subset(dat,dear==1),weighted.mean(emp1,pwgtp))
    with(subset(dat,dear==1),weighted.mean(emp2,pwgtp))

    with(subset(dat,dear==2),weighted.mean(emp1,pwgtp))
    with(subset(dat,dear==2),weighted.mean(emp2,pwgtp))
}
