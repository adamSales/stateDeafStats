makeWeights <- function(sdat){
    deafWeights <- vapply(c('DC','PR',state.abb),
                          function(st)
                              colSums(sdat[sdat$state==st & sdat$dear==1,paste0('pwgtp',c('',1:80))]),
                          numeric(81))
    deafWeights <- sweep(deafWeights,1,rowSums(deafWeights),'/')
    hearWeights <- vapply(c('DC','PR',state.abb),
                          function(st)
                              colSums(sdat[sdat$state==st & sdat$dear==2,paste0('pwgtp',c('',1:80))]),
                          numeric(81))
    hearWeights <- sweep(hearWeights,1,rowSums(hearWeights),'/')
    list(deaf=deafWeights,hear=hearWeights)
}

stateEstDiff1 <- function(t1v,deaf,ss,weights){
    dn <- dimnames(t1v)
    vv <- ifelse(deaf,'deaf','hear')
    if(deaf) wghts <- weights$deaf[,-which(colnames(weights$deaf)==ss)]
    else wghts <- weights$hear[,-which(colnames(weights$hear)==ss)]
    wghts <- sweep(wghts,1,rowSums(wghts),'/')
    stEsts <- t1v[vv,,ss]
    restEsts <- t1v[vv,,-which(dn[[3]]==ss)]

    restEsts <- vapply(1:nrow(restEsts),
                       function(ww) sum(restEsts[ww,]*wghts[paste0('pwgtp',rownames(restEsts)[ww]),]),1)

    list(stEsts,restEsts)
}

pval <- function(mu,sig) 2*pnorm(-abs(mu/sig))

stateDiffP <- function(t1v,ss,weights){
    df <- stateEstDiff1(t1v,TRUE,ss,weights)
    hr <- stateEstDiff1(t1v,FALSE,ss,weights)
    deafEst <- df[[1]][1]-df[[2]][1]
    deafSE <- sqrt(mean((df[[1]][-1]-df[[2]][-1]-deafEst)^2)*4)
    hearEst <- hr[[1]][1]-hr[[2]][1]
    hearSE <- sqrt(mean((hr[[1]][-1]-hr[[2]][-1]-hearEst)^2)*4)
    gapEst <- deafEst-hearEst
    gapSE <- sqrt(mean((df[[1]][-1]-hr[[1]][-1]-(df[[2]][-1]-hr[[2]][-1])-gapEst)^2)*4)
    c(deaf=pval(deafEst,deafSE),hear=pval(hearEst,hearSE),gap=pval(gapEst,gapSE))
}

stateDiff <- function(t1v,weights){
    overall <- c(deaf=sum(weights$deaf[1,]*t1v['deaf',1,]),
                 hear=sum(weights$hear[1,]*t1v['hear',1,]))
    overall['gap'] <- overall['deaf']-overall['hear']

    sweep(t1v[,1,],1,overall,'-')
}

est1var <- function(x,sdat){
    t1v <- tot1var(x,sdat,us=FALSE)
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
complete <- function(sdat){
    weights <- makeWeights(sdat)
    vars <- c('hs','ba','employed')
    l <- lapply(vars,function(x){
        t1v <- tot1var(x,sdat,us=FALSE)
        diff <- stateDiff(t1v,weights)
        ps <- vapply(c('DC','PR',state.abb),function(ss) stateDiffP(t1v,ss,weights),numeric(3))
        ps.Adj <- p.adjust(ps['gap',],method='holm')
        cbind(difference=diff['gap',],`p-value`=round(ps['gap',],5),`p-value (adjusted)`=round(ps.Adj,5))
    })
    names(l) <- vars
    write.xlsx(l,'DiffFromMean.xlsx',row.names=TRUE)
    l
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
