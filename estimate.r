


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
tot1var <- function(x,data,us=TRUE){
    sts <- c('DC','PR',state.abb)
    if(us) sts <- c('US',sts)
    vapply(sts,function(st) tot1state(x,st,data),matrix(1.1,3,81))
}

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
    vapply(c('US','DC','PR',state.abb),sampleSize1,data=data,1:2)

## if you've already downloaded, cleaned, etc the data, start here:
makeEstimates <- function(data){
    l <- list(
        HS=est1var('hs',data),
        BA=est1var('ba',data),
        Unemployment=est1var('unemployed',data),
        `Total Employment`=est1var('employed',data))
    l <- lapply(l,function(x) round(x*100,1))
    l[["Sample Size"]] <- t(sampleSizeFunc(data))

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

