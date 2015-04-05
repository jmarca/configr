##' A function that returns a function that will read in one or more
##' config files, return each as an object, and on subsequent reads
##' skip the whole part about hitting the file system
##'
##' This is a closure generator.
##'
##' It will return a function.
##'
##' That function does the real work.
##'
##' The returned function takes either a file as an argument, or will
##' look up the environment variable R_CONFIGR_FILE.  And it presumes
##' the file is JSON.  If it isn't use another library. Or better, use
##' Perl and one of those anyconfig libraries that are so handy.
##'
##' If no file is passed in and the environment variable is not set,
##' it will return NULL.
##'
##' An example config file:
##'
##' {
##'     "couchdb": {
##'         "host": "192.168.0.1",
##'         "port":5984,
##'         "trackingdb":"a_test_state_db",
##'         "auth":{"username":"blabbity",
##'                 "password":"correct horse battery staple"
##'                }
##'     },
##'     "postgresql":{
##'         "host":"192.168.0.1",
##'         "port":5432,
##'         "auth":{"username":"sqlrulez"},
##'         "grid_merge_sqlquery_db":"spatialspaces"
##'     }
##' }
##'
##' Note that if you want to use any of the convenience functions then
##' you should follow the above patter for "auth", but really it
##' doesn't matter.  Just get the config file as an R object and do
##' what you want with in on your end.
##'
##' One approach is to follow namespacing.  So above CouchDB entry has
##' "trackingdb" that might be used by some program called tracking,
##' while the PostgreSQL entry has an entry called
##' grid_merge_sqlquery_db that would be used by a package or script
##' called grid_merge_sqlquery.  This way if you have multiple programs that all need access to a database or other, you can share the auth stuff.
##'
##' On the other hand, if program A needs access to postgresql on two
##' machines, say Q and R, then you'd have to use a different
##' approach, say namespace by machine.  So for example, you might
##' have:
##'
##'
##' {
##'     "postgresql":{
##'         "a":{
##'             "host":"192.168.0.1",
##'             "port":5432,
##'             "auth":{"username":"sqlrulez"},
##'             "grid_merge_sqlquery_db":"spatialspaces"
##'             }
##'         "b":{
##'             "host":"192.168.0.7",
##'             "port":5432,
##'             "auth":{"username":"spredsheetsdroolz"},
##'             "grid_merge_sqlquery_db":"spatialspaces"
##'             "open_street_map_db":"spatialspaces"
##'             }
##'     }
##' }
##'
##'
##' @title configrr
##' @return a function that in turn will return a fucntion that will
##' return: the configuration specified in the file, as an R object.
##' On the second and subsequent invocations, if you don't pass in a
##' file parameter, then it will return the *first* configuration file
##' read.
##'
##' You can actually run this multiple times, and store different
##' files each time.  In that case, you have to specify the file name
##' if you want something different than the first file stored.
##' @param storedconfig optional list to store config file results
##' @export
##' @author James E. Marca
configrr <- function(storedconfig=list()){
    get.config <- function(file){

        if(missing(file)){
            file <-  Sys.getenv(c("R_CONFIGR_FILE"))[1]
        }

        if(file == ''){
            if(length(storedconfig) == 0){
                return(NULL)
            }else{
                return(storedconfig[[1]])
            }
        }

        ## have a file of some sort, see if already parsed it

        ## see if we already have it
        key <- digest::digest(file,'sha1')

        if(!is.null(storedconfig[[key]])){
            return(storedconfig[[key]])
        }

        ## parse the file using rjson
        storedconfig[[key]] <<- rjson::fromJSON(file=file)

        ## return it
        storedconfig[[key]]
    }
    return (get.config)
}
