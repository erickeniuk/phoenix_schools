git_affidavit <- function(){
  affidavit_location <- '~/Dropbox/pkg.data/lincoln/clean/affidavit.rds'
  if(!file.exists(affidavit_location)){
      dt <- fread('~/Dropbox/Phoenix/Raw Data/Home Sales/Phoenix_Sales_2016.dat')
      dt2 <- fread('~/Dropbox/Phoenix/Raw Data/Home Sales/Phoenix_Sales_2016.dat', skip = 1256086, header = FALSE)
      dt <- rbindlist(list(dt, dt2), use.names = TRUE, fill=TRUE)
      dt2 <- NULL
      saveRDS(dt, affidavit_location)
  } else {
    dt <- readRDS(affidavit_location)
  }
  return(dt)
}