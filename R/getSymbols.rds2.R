# getSymbols.rds2 {{{
"getSymbols.rds2" <-
function(Symbols,env,
         dir="",
         return.class="xts",
         extension="rds",
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         from='2007-01-01',
         to=Sys.Date(),
         ...) {
  importDefaults("getSymbols.rds2")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  default.return.class <- return.class
  default.dir <- dir
  default.extension <- extension
  default.from <- from
  default.to <- to

  if(!hasArg(verbose)) verbose <- FALSE
  if(!hasArg(auto.assign)) auto.assign <- TRUE

  for(i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class),default.return.class,
                           return.class)
    dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
    dir <- ifelse(is.null(dir),default.dir,
                           dir)
    extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
    extension <- ifelse(is.null(extension),default.extension,
                           extension)
    if(verbose) cat("loading ",Symbols[[i]],".....\n")

    # generate sym.files 
    StartDate <- as.Date(from)
    EndDate <- as.Date(to)
    date.vec <- as.Date(StartDate:EndDate)
    ym<-unique(format(date.vec, "%Y%m"))
    sym.files <- paste(dir,"/",Symbols[[i]],"/",ym,'.',Symbols[[i]],'.rds', sep="")

    # do.call.rbind internal function
    do.call.rbind <- function(lst) {
        while(length(lst) > 1) {
          idxlst <- seq(from=1, to=length(lst), by=2)
          
          lst <- lapply(idxlst, function(i) {
            if(i==length(lst)) { return(lst[[i]]) }
            
            return(rbind(lst[[i]], lst[[i+1]]))
          })
        }
        lst[[1]]
    }
    dl <- lapply(
        sym.files,
        function(fp) {
            if (verbose) cat("Reading ", fp, "...\n")                            
            if(!file.exists(fp)) {
                cat(" failed. File not found in ", fp, " ... skipping\n")
            } else {
                dat <- readRDS(fp)
                dat
            }
        }
    )
    fr <- do.call.rbind(dl)
    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    }
    if(auto.assign)
      return(Symbols)
    return(fr)
}
#}}}
