utils.new.parse.prompt <- function (obj) 
{
  ##### REMOVE THIS WHEN UPDATE THE PACKAGE ####
  parsevector <- function(x){
    #the main purpose of this function is to map JSON null values to NA.
    ifelse(is.null(x) || x == "NOT_DISPLAYED" || x == "SKIPPED", return(NA), return(x))
  }
  varname <- names(obj)
  prompt_type <- obj[[1]]$context$prompt_type
  values <- lapply(obj[[1]]$values, parsevector)
  if (prompt_type == "multi_choice" || prompt_type == "multi_choice_custom" ) {
    values <- identity(values)
  }
  else if(prompt_type == "remote_activity"){
    # for variables from PAM activity, convert them to multi_choice_custom
    if(obj[[1]]$context$activity == "org.openmhealth.pam.PAMActivity"){
      prompt_type = "multi_choice_custom" 
      values <- lapply(values, function(response){
          lapply(response, function(entry){
            if(!is.na(entry)[1])
              return(entry$mood)
          })
      })
    }else{
      # for the other remote_activity variables, the structure of data could be varied
      # so we convert it to generic string of R object for further processing
      values <- unlist(lapply(values, function(l){
              toString(list(l))
      }))
    }
  }
  else {
    values <- unlist(values)
  }
  choice_glossary <- obj[[1]]$context$choice_glossary
  if (!is.null(choice_glossary) && !is.na(choice_glossary)) {
    if (!is.null(choice_glossary[[1]]$value)) {
      levels <- sort(names(choice_glossary))
      labels <- unname(unlist(choice_glossary)[paste(levels, 
                                                     "label", sep = ".")])
    }
    else {
      levels <- sort(names(choice_glossary))
      labels <- unname(unlist(choice_glossary)[paste(levels, 
                                                     "label", sep = ".")])
    }
    theorder <- order(as.numeric(levels))
    levels <- levels[theorder]
    labels <- labels[theorder]
  }
  newvar <- switch(prompt_type, single_choice = factor(values, 
                                                       levels, labels, ordered = TRUE), single_choice_custom = factor(values, 
                                                                                                                      levels = unique(c(labels, values)), ordered = TRUE), 
                   multi_choice = multifactor(values, levels, labels), multi_choice_custom = multifactor(values, 
                                                                                                         unique(c(labels, unlist(values)))), number = as.numeric(values), 
                   remote_activity = values, timestamp = as.POSIXct(strptime(values, 
                                                                             format = "%Y-%m-%dT%H:%M:%S")), hours_before_now = structure(as.numeric(values), 
                                                                                                                                          class = c("hours_before_now", "numeric")), photo = values, 
                   text = values, stop("Don't know how to parse item: ", 
                                       varname, "of prompt_type: ", prompt_type, "\n"))
  attr(newvar, "prompt_type") <- prompt_type
  return(newvar)
}

assignInNamespace("parse.prompt", utils.new.parse.prompt, "Ohmage", envir="Ohmage")