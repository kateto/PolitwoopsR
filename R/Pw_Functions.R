
## -----------------------------------------------------------------------
##
## PolitwoopsR Package
##
##  Download and examine deleted congress tweets  
##  Data from:  www.politwoops.com 
##              politwoops.sunlightfoundation.com
##  Author: Katherine Ognyanova (www.kateto.net)
##  Visualizations at: http://kateto.net/politwoops 
##
## -----------------------------------------------------------------------

 
#' Get deleted politician tweet data from the Politwoops project
#'
#' This function downloads and combines json files from any of the countries
#' participating in the Politwoops project (defaults to the US).
#' 
#' @param start.page  The first json page to get - defaults to page 1.
#' @param end.page    The last json page to get - defaults to the last available page.
#' @param domain  The domain for the politwoops project of interest (do not include "http://").
#' Defaults to the US domain (politwoops.sunlightfoundation.com). For a list of other countries,
#'  see \url{http://politwoops.com/countries}. 
#' @param json.ext The last part of the URL that points to the country JSON pages - defaults to 
#' "/index.json?page=" which seems to be correct for some countris (including the US and UK)
#' but not others. Future versions of this package will automate identifying this extension.
#'  
#' @return Data frame with deleted tweet information from Politwoops.
#'
#' @author Katya Ognyanova \email{rstats@@ognyanova.net}  
#' @export 
#' @examples \dontrun{
#' 
#' # Get all the US Politwoops tweets:
#' tweet.df <- get_pw_tweets()
#' 
#' # Get the first 5 pages from the UK Politwoops tweets:
#' tweet.df <- get_pw_tweets(1, 5, "politwoops.co.uk")
#' 
#' # Get the first 5 pages from Argentina's Politwoops tweets:
#' tweet.df <- get_pw_tweets(1, 5, "www.politwoops.com/g/Argentina", ".json?page=")
#' 
#' }
#' 
#' @seealso  \code{\link{get_pw_pol}} \code{\link{merge_pw}}
#' 
get_pw_tweets <- function(start.page=1, end.page="all", domain=NULL, json.ext=NULL) {
    
    if (getOption("stringsAsFactors")) on.exit(options("stringsAsFactors" = T))  
    options("stringsAsFactors" = F)
    
    if (start.page > end.page) stop("Error: start.page should be lower than end.page")
    
    cat("Getting deleted tweets... this may take a while.\n")
    cat(paste0("Processing page #", start.page,".\n"))
    
    if (is.null(domain)) domain <- "projects.propublica.org/politwoops"
    if (is.null(json.ext)) json.ext <- "/index.json?page="
    json.url <- paste0("http://", domain, json.ext)
    
    pw.raw <- GET(paste0(json.url, start.page))
    pw.json <- content(pw.raw, as="parsed")$tweets
    
    if ( length(pw.json)==0 ) { pw.json <- content(pw.raw, "parsed"); json.str <- 1} else {json.str <- 0}
    if ( length(pw.json)==0 )  stop("This start.page does not seem to exist, try a lower #.")
    
    pw.df.mn <- do.call("rbind", pw.json)
    for (j in 1:ncol(pw.df.mn)) {
           tmp <- do.call("rbind.fill", lapply(pw.df.mn[,j], function(x) as.data.frame(t(unlist(x))))) 
           if(j==1) { pw.df <- tmp } else { pw.df <- cbind(pw.df, tmp) }
           if (ncol(tmp)==1) colnames(pw.df)[ncol(pw.df)] <- colnames(pw.df.mn)[j]
         }
         
    i <- start.page+1
    
    if (i <= end.page)  repeat {       
        
        cat(paste0("Processing page #",i ,".\n"))
        
        pw.raw <- GET(paste0(json.url, i))
        pw.json <- tryCatch( { if (json.str) content(pw.raw, "parsed") else content(pw.raw, "parsed")$tweets }, 
                               error = function(e) {
                                      print(paste("Invalid json on page", i, " (", e, ")"))
                                      return(list(NULL)) } )
        if ( length(pw.json)==0 ) break else { i <- i+1 }
        if ( is.null(pw.json[[1]]) ) next 
       
        pw.df.mn <- suppressWarnings(do.call(rbind, pw.json))
        for (j in 1:ncol(pw.df.mn)) {
              tmp <- do.call("rbind.fill", lapply(pw.df.mn[,j], function(x) as.data.frame(t(unlist(x))))) 
              if(j==1) { pw.df.i <- tmp } else { pw.df.i <- cbind(pw.df.i, tmp) }
              if (ncol(tmp)==1) colnames(pw.df.i)[ncol(pw.df.i)] <- colnames(pw.df.mn)[j]    
          }        
        
        pw.df <- rbind.fill(pw.df, pw.df.i)
        
        if (i > end.page) break
     }
    
    pw.df <- data.frame(apply(pw.df, c(1,2), unlist), stringsAsFactors=F)
    return(pw.df)
 }



#' Get politician data from the US Politwoops project
#'
#' This function scrapes politician data from the US Politwoops project
#' maintained by the Sunlight Foundation. The data is online at 
#' \url{http://politwoops.sunlightfoundation.com/users}.
#' 
#' @param start.page  The first page to get - defaults to page 1 
#' (http://politwoops.sunlightfoundation.com/users?page=1)
#' @param end.page    The last page to get - defaults to the last available page.
#'
#' @return Data frame with politician info from Politwoops.
#'
#' @author Katya Ognyanova \email{rstats@@ognyanova.net} 
#' @export 
#' @examples \dontrun{
#' 
#' # Get all the politician data:
#' pol.df <- get_pw_pol()
#' 
#' # Get the first 5 pages of politicians:
#' pol.df <- get_pw_pol(1, 5)
#' 
#' }
#' 
#' @seealso  \code{\link{get_pw_tweets}}  \code{\link{merge_pw}}
#' 
get_pw_pol <- function(start.page=1, end.page="all") {
 
      if (getOption("stringsAsFactors")) on.exit(options("stringsAsFactors" = T))  
      options("stringsAsFactors" = F)
      
      if (start.page > end.page) stop("Note: start.page should be lower than end.page")
    
      cat("Getting politician data... this may take a while.\n")
      cat(paste0("Processing page #",start.page ,".\n"))
      
      pol.url <- "https://projects.propublica.org/politwoops/users?page="
      
      # parse the document for R representation:
      pol.doc <- htmlParse(paste0(pol.url, start.page))
      
      # get all the tables in the doc as data frames:
      pol.inf <- readHTMLTable(pol.doc) 
      if (is.null(pol.inf[[1]])) stop("This start.page does not seem to exist, try a lower #.")
      
      pol.inf <- data.frame(pol.inf[[1]], stringsAsFactors=F)[2:5]
      colnames(pol.inf) <- c("full.name", "state","type.tmp", "party.tmp")
        
      pol.links <- data.frame(url=xpathSApply(pol.doc, "//a/@href"), stringsAsFactors=F)
      pol.links <- pol.links[grepl("/user/", unlist(pol.links)),1]
      pol.inf$twitter <- gsub("/user/(.*)/", "\\1", pol.links)
       
      i <- start.page+1 
      
      if (i <= end.page) repeat {
        
         cat(paste0("Processing page #",i ,".\n"))
                
         pol.url.i <- paste0(pol.url, i)
         pol.doc.i <- htmlParse(pol.url.i)         
         pol.inf.i <- readHTMLTable(pol.doc.i) 
         
         if (is.null(pol.inf.i[[1]])) break else i <- i+1
         
         pol.inf.i <- data.frame(pol.inf.i[[1]], stringsAsFactors=F)[2:5]
         colnames(pol.inf.i) <- c("full.name", "state","type.tmp", "party.tmp")
        
         pol.par.i <- htmlParse(pol.url.i)
         pol.links.i <- data.frame(url=xpathSApply(pol.par.i, "//a/@href"), stringsAsFactors=F)
         free(pol.par.i)
             
         pol.links.i <- pol.links.i[grepl("/user/", unlist(pol.links.i)),1]
         pol.inf.i$twitter <- gsub("/user/(.*)/", "\\1", pol.links.i)
         
         pol.inf <- rbind(pol.inf, pol.inf.i)
         rm(pol.inf.i)
         
         if (i > end.page) break
      }
      
     # Clean the resulting data frame:  
      pol.inf$full.name <- gsub("\t", "",  pol.inf$full.name)
      pnames <- suppressWarnings(do.call(rbind, strsplit(pol.inf$full.name, "\n")))      
      pol.inf[,c("first.name", "middle.name", "last.name")] <- pnames[,1:3]
      pol.inf$full.name <- gsub("\n", " ", pol.inf$full.name)
      pol.inf$full.name <- gsub("  ", " ", pol.inf$full.name)
        
      pol.inf$party <- NA
      pol.inf$party[grepl("Rep", pol.inf$party.tmp, ignore.case=T)] <- "Rep"
      pol.inf$party[grepl("Dem", pol.inf$party.tmp, ignore.case=T)] <- "Dem"
      pol.inf$party[grepl("Ind", pol.inf$party.tmp, ignore.case=T)] <- "Ind"
      pol.inf$party[grepl("Oth", pol.inf$party.tmp, ignore.case=T)] <- "Oth"
    
      pol.inf$type <- NA
      pol.inf$type[grepl("Senat", pol.inf$type.tmp, ignore.case=T)] <- "Senate"
      pol.inf$type[grepl("House", pol.inf$type.tmp, ignore.case=T)] <- "House"
      pol.inf$type[grepl("Gubern", pol.inf$type.tmp, ignore.case=T)] <- "Governor"
      pol.inf$type[grepl("Govern", pol.inf$type.tmp, ignore.case=T)] <- "Governor"
      pol.inf$type[grepl("President", pol.inf$type.tmp, ignore.case=T)] <- "President"
      pol.inf$type[grepl("Presidential", pol.inf$type.tmp, ignore.case=T)] <- "Presidential Candidate"
      pol.inf$type[grepl("Vice", pol.inf$type.tmp, ignore.case=T)] <- "Vice President"
      
      pol.inf$inactive <- 0
      pol.inf$inactive[grepl("inactive", pol.inf$party.tmp, ignore.case=T)] <- 1
      
      pol.inf$challenger <- 0
      pol.inf$challenger[grepl("challeng", pol.inf$type.tmp, ignore.case=T)] <- 1 

      pol.inf <- pol.inf[, !grepl("tmp", colnames(pol.inf))]
      return(pol.inf)
}


#' Merge tweet and politician data
#'
#' Simple merge of tweets with politician data.
#' 
#' @param tweets Tweet data frame generated with  \code{\link{get_pw_tweets}}
#' @param politicians Politician data frame generated with  \code{\link{get_pw_pol}}
#'
#' @return Tweet data frame with added politician info for each tweet author.
#'
#' @author Katya Ognyanova \email{rstats@@ognyanova.net}  
#' @export 
#' @examples \dontrun{
#'
#' tweet.df <- get_pw_tweets(1, 5)
#' pol.df <-  get_pw_pol()
#' tweet.df <- merge_pw(tweet.df, pol.df)
#'  
#'  }
#'  
#' @seealso  \code{\link{get_pw_tweets}} \code{\link{get_pw_pol}}
#' 

merge_pw <- function(tweets, politicians) {
  
      tweets$twitter <- tolower(tweets$user_name)
      politicians$twitter <- tolower(politicians$twitter)
  
      out <- merge(tweets, politicians, by="twitter", all.x=T)
      
      return(out)
}
