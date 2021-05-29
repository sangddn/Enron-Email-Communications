# Problem 1
Email <- function(filename) {
  email <- readLines(filename)
  out <- email[which(str_detect(email, 'X-From: |X-To: '))] %>% str_remove('X-From: |X-To: ') 
  out[3] <- filename
  
  return(out) #Vector out: [1] sender, [2] recipient, [3] full path
}

from.Email <- function(em) em[1]
to.Email <- function(em) em[2]

# Problem 2
Employee <- function(directory_name) {
  allEmails <- list.files(
    paste('../raw_data/enron_dataset/', directory_name, sep = ''),
    full.names = T, recursive = T
  ) %>% 
  str_sort(numeric = TRUE) %>% 
    # Sort file names in correct order (1 then 2, not 1 then 10)
  purrr::map(function(.filename) Email(.filename))
    # Map a list of emails
  
  thisEmployee <- list(
    name = from.Email(allEmails[[1]]),
    emails = allEmails
  )
  
  return(thisEmployee)
}

get_number_of_emails.Employee <- function(e) length(e$emails)
get_email_filename.Employee <- function(e, i) e$emails[[i]][3]
get_name.Employee <- function(e) e$name
