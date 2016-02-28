.findVar<-function(idfName) {
    found<-FALSE
    value<-NA
    #print(sys.nframe())
    for (i in sys.nframe():1) {
        #print(i)
        if (exists(idfName,envir=sys.frame(i),inherits=FALSE)) {
            value<-get(idfName,envir=sys.frame(i),inherits=FALSE)
            found<-TRUE
            break
        }
    }
    return(list(value=value,found=found))
}

.gfc<-function
### GFC: "get from caller" function makes given variable that appears
### in the environment of some caller of the function calling the
### `.gfc' function usable directly by the caller of the `.gfc'
### function.
###
### Example:
###  f1() { a<-1; f2()}
###  f2() { a<-.gfc(a); <<now a copy of `a' appears in f2>> }
(nm,##<< identifier (not a character name) of a variable to get
required=TRUE##<< Require this variable and err if not found? If FALSE,
## NULL return value indicates either the variable exists and has the
## NULL value assigned, or the variable has not been found.
) {
    #if (.dbg.internal) cat(paste('gcf: looking for \'',deparse(substitute(nm)),'\'\n',sep=''))
    rv<-.findVar(deparse(substitute(nm)))
    if (rv$found) {
        return(rv$value)
    } else {
        if (required) {
            print(traceback())
            stop(paste('\'',deparse(substitute(nm)),'\' not found in caller stack',sep=''))
        }
        # obsoleted fallback:
        #return(attr(rv$value,'.sharedEnv')[deparse(substitute(nm))])
        return(NULL)
    }
}
