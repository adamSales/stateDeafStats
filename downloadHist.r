uz <- function(filename){
    files <- unzip(filename,list=TRUE)$Name
    files <- grep('.csv',files,fixed=TRUE,value=TRUE)
    unzip(filename, files=files)
}

downloadPR <- function(dest=''){
    for(year in c('08','09','10','11','12','13','14','15','16')){
        baseURL <- paste0(
            'https://www2.census.gov/programs-surveys/acs/data/pums/20',
            year,'/1-Year/')
        ##download.file(paste0(baseURL,'csv_pus.zip'),paste0(dest,'csv_pus',year,'.zip'))
        ## download.file(paste0(baseURL,'csv_hus.zip'),
        ##               paste0(dest,'csv_hus',year,'.zip'))
        download.file(paste0(baseURL,'csv_hpr.zip'),
                      paste0(dest,'csv_hpr.zip'))
        uz(paste0(dest,'csv_hpr.zip'))
        download.file(paste0(baseURL,'csv_ppr.zip'),
                      paste0(dest,'csv_ppr.zip'))
        uz(paste0(dest,'csv_ppr.zip'))
    }
}
