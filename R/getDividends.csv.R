`getDividends.csv` <-
function(Symbol,to=Sys.Date(),env=.GlobalEnv,src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,...) {
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))
  dividend.file<-paste("~/data/dividends/",Symbol.name,".dividend.csv",sep='')
  fr <- read.csv(dividend.file)
  fr <- xts(fr[,2],as.Date(fr[,1]))
  colnames(fr)<-paste(Symbol.name,'div',sep='.')
  if(is.xts(Symbol)) {
    if(auto.update) {
      xtsAttributes(Symbol) <- list(dividends=fr)
      assign(Symbol.name,Symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
  } else fr
}
