#https://joboutlook.gov.au/A-Z.aspx
install.packages("tidyverse")
install.packages("rvest")
library(rvest)
joboutlook<-read_html("https://joboutlook.gov.au/A-Z.aspx")


links_occupation<-html_nodes(joboutlook, "div#occupations a")

links_occupation_clean<-html_attr(links_occupation, "href")

#just occupation names from site
links_occupation_clean_text<-html_text(links_occupation, "href")

##scraping individual sites

all_links_complete<-paste0("https://joboutlook.gov.au/",links_occupation_clean)
job<-read_html(all_links_complete[1])

value<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-data"))
title<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-title"))

#gsub("\\r|\\n|\\t","", value)
# title<-strsplit(title, "\\r")
# title<-map_chr(title, 1)
# title<-gsub("\\r|\\n|\\t","", title)
# value<-gsub("\\r|\\n|\\t","", value)
# value<-trimws(value)
# title<-trimws(title)
# out<-tibble(occupation= links_occupation_clean_text[1],title=title, value=value)


#PURR

library(purrr)

get_occupation<-function(url,occupation, bow){
  bow<-nod(bow, url)
  job<-scrape(bow)
  value<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-data"))
  title<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-title"))
  #gsub("\\r|\\n|\\t","", value)
  title<-strsplit(title, "\\r")
  title<-map_chr(title, 1)
  title<-gsub("\\r|\\n|\\t","", title)
  value<-gsub("\\r|\\n|\\t","", value)
  value<-trimws(value)
  title<-trimws(title)
  out<-tibble(occupation= occupation,title=title, value=value)
  return(out)
}

library(polite)
bow_run<-bow("https://joboutlook.gov.au/", delay = 5)
purrr::map2_dfr(links_occupation_clean, links_occupation_clean_text, get_occupation, bow_run)


##get occupation mean
get_occupation_mean<-function(url,occupation){
  job<-read_html(url)
  value<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-data"))
  title<-html_text(html_nodes(job, "section.fast-facts li span.snapshot-title"))
  #gsub("\\r|\\n|\\t","", value)
  title<-strsplit(title, "\\r")
  title<-map_chr(title, 1)
  title<-gsub("\\r|\\n|\\t","", title)
  value<-gsub("\\r|\\n|\\t","", value)
  value<-trimws(value)
  title<-trimws(title)
  out<-tibble(occupation= occupation,title=title, value=value)
  return(out)
}

library(polite)
bow_run<-bow("https://joboutlook.gov.au/", delay = 5)
jerbs<-purrr::map2_dfr(all_links_complete, links_occupation_clean_text, get_occupation_mean)

