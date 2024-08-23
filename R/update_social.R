#' Update the social options for the dashboard, appearing in the bottom right
#' corner of the navbar
#'
#' The links for sharing the page on the social networks can be easily
#' created with the application https://www.sharelinkgenerator.com/.
#'
#' @param article_text The text to show for the button redirecting to an
#' external article.
#' @param article_link The url redirecting to an external article.
#' @param facebook_link The url redirecting to share the page on facebook.
#' @param twitter_link The url redirecting to share the page on twitter.
#' @param linkedin_link The url redirecting to share the page on linkedin.
#' @returns Save the new social.html in the user memory.
#' @import tools
#' @export
update_social <- function(article_text = NULL, article_link = NULL,
    facebook_link = NULL, twitter_link = NULL, linkedin_link = NULL) {

  # Get the user project data directory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  social_path <- file.path(user_data_path, "social.html")

  file <- file(social_path, open = 'w')
  write(c('<script id="flexdashboard-navbar" type="application/json">', '['), file, sep="\n")

  # If article_text and article_link are both set, add the article button
  if (!is.null(article_text) && !is.null(article_link)) {
    write(paste0('{"icon":"fa-newspaper", "href":"', article_link, '", "title": "', article_text, '", "target":"_blank", "align": "right"}'), file, sep="\n")
    # If at least one sharing link is set, add a ,
    if (!is.null(facebook_link) || !is.null(twitter_link) || !is.null(linkedin_link)) {
      write(",", file, sep="\n")
    }
  }

  # If at least one sharing link is set, add the sharing button
  if (!is.null(facebook_link) || !is.null(twitter_link) || !is.null(linkedin_link)) {
    write('{"icon":"fa-share-alt","items":[', file, sep="\n")
  }
  # If facebook link is set, show the facebook button
  if (!is.null(facebook_link)) {
    write(paste0('{"icon":"fa-facebook", "href":"', facebook_link, '","target":"_blank", "align": "right"}'), file, sep="\n")
    # If at least one more sharing link is set, add a ,
    if (!is.null(twitter_link) || !is.null(linkedin_link)) {
      write(",", file, sep="\n")
    }
  }
  # If twitter link is set, show the twitter button
  if (!is.null(twitter_link)) {
    write(paste0('{"icon":"fa-twitter", "href":"', twitter_link, '","target":"_blank", "align": "right"}'), file, sep="\n")
    # If at least one more sharing link is set, add a ,
    if (!is.null(linkedin_link)) {
      write(",", file, sep="\n")
    }
  }
  # If linkedin link is set, show the linkedin button
  if (!is.null(linkedin_link)) {
    write(paste0('{"icon":"fa-linkedin", "href":"', linkedin_link, '","target":"_blank", "align": "right"}'), file, sep="\n")
  }
  # If at least one sharing link is set, close the sharing button
  if (!is.null(facebook_link) || !is.null(twitter_link) || !is.null(linkedin_link)) {
    write(c(']', '}'), file, sep="\n")
  }

  write(c(']', '</script>'), file, sep="\n")
  close(file)

}
