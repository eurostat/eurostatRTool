#' Update the footer of the dashboard, appearing in the bottom of the dashboard
#'
#' @param last_update The last update text.
#' @param cookies_policy_link The url redirecting to the cookie policy.
#' @param privacy_policy_link The url redirecting to the privacy policy.
#' @param legal_notice_link The url redirecting to the legal notice.
#' @param feedback_email The email address to send the feedback to.
#' @param feedback_email_subject The text to automatically fill the subject
#' field for the feedback email.
#' @returns Save the new footer.html in the user memory.
#' @import tools
#' @export
update_footer <- function(last_update = NULL, cookies_policy_link = NULL,
                          privacy_policy_link = NULL, legal_notice_link = NULL,
                          feedback_email = NULL, feedback_email_subject = NULL) {

  # Get the user project data directory
  user_data_path <- tools::R_user_dir("eurostatRTool", which = "data")
  if (!dir.exists(user_data_path)) {
    dir.create(user_data_path, recursive=TRUE)
  }
  footer_path <- file.path(user_data_path, "footer.html")

  file <- file(footer_path, open = 'w')
  write('<div id="mainFooterContainer">', file, sep="\n")

  # If the last_update is passed, write it in the left corner
  if (!is.null(last_update)) {
    write(paste0('<div id="mainFooterLastUpdate" class="main-footer-item resize-tx" resize-offset="65">', last_update, '</div>'), file, sep="\n")
  }

  # If at least one link is passed, set the links container in the center
  if (!is.null(cookies_policy_link) || !is.null(privacy_policy_link) || !is.null(legal_notice_link)) {
    write('<div id="legalCookiesContainer" class="main-footer-item" style="justify-content:center; font-size: 16px;">', file, sep="\n")
  }
  # If the cookies_policy_link is passed, show it
  if (!is.null(cookies_policy_link)) {
    write(paste0('<a href="', cookies_policy_link, '" target="_blank">Cookies policy</a>'), file, sep="\n")
    # If at least one more link is passed, add a |
    if (!is.null(privacy_policy_link) || !is.null(legal_notice_link)) {
      write(" | ", file, sep="\n")
    }
  }
  # If the privacy_policy_link is passed, show it
  if (!is.null(privacy_policy_link)) {
    write(paste0('<a href="', privacy_policy_link, '" target="_blank">Privacy policy</a>'), file, sep="\n")
    # If at least one more link is passed, add a |
    if (!is.null(legal_notice_link)) {
      write(" | ", file, sep="\n")
    }
  }
  # If the legal_notice_link is passed, show it
  if (!is.null(legal_notice_link)) {
    write(paste0('<a href="', legal_notice_link, '" target="_blank">Legal notice</a>'), file, sep="\n")
  }
  # If at least one link is passed, close the links container
  if (!is.null(cookies_policy_link) || !is.null(privacy_policy_link) || !is.null(legal_notice_link)) {
    write('</div>', file, sep="\n")
  }

  # If the feedback_email is passed, show it
  if (!is.null(feedback_email)) {
    email <- paste0("mailto:", feedback_email)
    if (!is.null(feedback_email_subject)) {
      email <- paste0(email, "?subject=", feedback_email_subject)
    }
    write(c('<div id="mainFooterFeedback" class="main-footer-item" style="">',
            '<div id="feedbackContainer">',
            paste0('<a class="feedback-link" href="', email, '" target="_blank">'),
            '<i class="fa fa-envelope resize-tx" aria-hidden="true" resize-offset="65" style="font-size: 16px;"></i>&nbsp;</a>',
            paste0('<a class="feedback-link" href="', email, '" target="_blank" resize-offset="65">Feedback</a>'),
            '</div>',
            '</div>'), file, sep="\n")
  }

  write('</div>', file, sep="\n")
  close(file)

}
