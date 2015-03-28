
## -----------------------------------------------------------------------
##
## PolitwoopsR Package
##
##  Download and examine deleted congress tweets  
##  Data Source:  www.politwoops.com 
##                politwoops.sunlightfoundation.com
##  Author: Katherine Ognyanova (www.kateto.net)
##  Visualizations at: http://kateto.net/politwoops 
##
## -----------------------------------------------------------------------

 
#' Expand shortened URLs in tweets 
#'
#' Expand the shortened URLs in a deleted tweet data frame. This may take some time
#' since every tweet has to go through the longurl.org API. Note also that many
#' of deleted tweets have broken or wrong links - those of course cannot be expanded.
#' 
#' @param tweets A data frame of tweet info created with \code{\link{get_pw_tweets}} or
#' \code{\link{merge_pw}}  
#'        
#' @return A vector of expanded URLs.
#'
#' @author Katya Ognyanova \email{rstats@@ognyanova.net}  
#' @export 
#' @examples \dontrun{
#'
#' tweet.df <- get_pw_tweets(1, 5)
#' tweet.df$url.exp <- url_expand(tweet.df)
#'  
#'  }
#'  
#' @seealso  \code{\link{get_pw_tweets}} \code{\link{get_pw_pol}}
#'           \code{\link{merge_pw}} \code{\link{url_domain}}
#' 

url_expand <- function(tweets) {
  cat("Getting expanded URLs - this may take a while.\n")
  if (!requireNamespace("twitteR", quietly=T))
      stop("This function requires the twitteR package. Please install it: install.packages('twitteR')")
  
  nona.urls <- !is.na(tweets$entities.urls.url)
  tweets$url.decoded <- NA
  
  for(i in 1:length(tweets$entities.urls.url[nona.urls])) {
    tweets$url.decoded[nona.urls][i] <-   
            tryCatch({u <- twitteR::decode_short_url(tweets$entities.urls.url[nona.urls][i])
                  if (is.null(u)) {u <- twitteR::decode_short_url(tweets$entities.urls.display_url[nona.urls][i])}
                  if (is.null(u)) {u <- twitteR::decode_short_url(tweets$entities.urls.expanded_url[nona.urls][i])}
                  if (is.null(u)) {u <- tweets$entities.urls.expanded_url[nona.urls][i]  }
                  u }, 
             warning = function(warn){print(paste("WARNING:  ",warn, "at #", i))},
             error = function(err){print(paste("ERROR:  ",err, "at #", i)); return("Error") }) }
  
  
  return(tweets$url.decoded)
}
  
#' Get domains from URLs
#'
#' Extract the domain from a full url (e.g. "google.com" from 
#' "https://www.google.com/calendar/render")
#' 
#' @param urls Single URL or a vector of URLs.
#' @param extended If TRUE, returns the domain with a subomain 
#' (if present). For instance, "www.domain.com/sub/blah/meh.html" would 
#' return "domain.com/sub". Defaults to FALSE.
#'        
#' @author Katya Ognyanova \email{rstats@@ognyanova.net}  
#' @export 
#' @examples
#'
#' url_domain("https://www.google.com/search?num=100&q=politwoops") 
#' 
#' url_domain("www.domain.com/sub/blah/meh.html", extended=TRUE)
#'  
#'  
#' @seealso  \code{\link{get_pw_tweets}} \code{\link{get_pw_pol}}
#'           \code{\link{merge_pw}} \code{\link{url_expand}}
#' 

url_domain <- function (urls, extended=F) {
  protocol <- "^(?:(?:[[:alpha:]+.-]+)://)?(?:mailto:[^@]+@)?"
  www <- "(?:www[[:digit:]a-z]*\\.)?"
  host <- "([^/:]+)"
  post <- "(?:/([^/:]+))?"
  rest <- ".*$"
  url <- paste0(protocol, www, host, post, rest)
  
  if (extended == F) return(gsub(url, "\\1", urls, ignore.case=T))
  if (extended == T) return(gsub(url, "\\1/\\2", urls, ignore.case=T))
}


#' Clean tweet text
#'
#' Prepares tweets for text analysis by removing usernames (@@user), URLs (http://...), 
#' new lines, breaks and special symbols. Expands contractions (e.g. "won't" to "will not", 
#' "I've" to "I have", etc.).
#' 
#' @param content A vector of strings (for instance get_pw_tweets()$content)
#'         
#' @author Katya Ognyanova \email{rstats@@ognyanova.net}  
#' @export 
#' @examples  \dontrun{
#'
#' tweet.df <- get_pw_tweets(1, 5)
#' tweet.df$clean <- clean_text(tweet.df$content)
#' 
#' }
#'  
#' @seealso  \code{\link{get_pw_tweets}} \code{\link{get_pw_pol}}
#'           \code{\link{merge_pw}} 
#' 

clean_text <- function (content) {
  
  content <- gsub("@\\w+", "", content)
  content <- gsub("http[^ ]*", "", content)
  content <- gsub("[ \r\n\t]+", " ", content)
  content <- gsub("\\\\", "", content)
  content <- gsub("&amp;", "", content)
 
  # Single quotes to be replaced with ':
  qs  <- paste0( "['`\u0091\u0092\u2018\u2019\uFF07\u001F\u0027\u00B4]" )
  # Double quotes and other unicode chars to delete:
  qr  <- paste0("[\u2022\u00B7\u201C\u201D]")
  
  content <- gsub(qr, "", content)
  content <- gsub(qs, "'", content)
  content <- gsub("won't", "will not", content)
  content <- gsub("n't", " not", content)
  content <- gsub("'ll", " will", content)
  content <- gsub("'re", " are", content)
  content <- gsub("'ve", " have", content)
  content <- gsub("'m", " am", content) 
  content <- gsub("'s", "", content) 
  
  return(content)
}










