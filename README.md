
<!-- README.md is generated from README.Rmd. Please edit that file -->
jerbs
=====

The goal of jerbs is to scrape data politely from the department of employment's jobs website

Installation
------------

You can install the released version of jerbs from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("jerbs")
```

Code for creating the function
------------------------------

``` r
library(polite)
library(tidyverse)
library(rvest)
joboutlook<-read_html("https://joboutlook.gov.au/A-Z.aspx")


links_occupation<-html_nodes(joboutlook, "div#occupations a")

links_occupation_clean<-html_attr(links_occupation, "href")

#just occupation names from site
links_occupation_clean_text<-html_text(links_occupation, "href")

##scraping individual sites

all_links_complete<-paste0("https://joboutlook.gov.au/",links_occupation_clean)
job<-read_html(all_links_complete[1])
```

And then creating the function so we can scrape the website data

``` r
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


bow_run<-bow("https://joboutlook.gov.au/", delay = 5)
out_jobs<-purrr::map2_dfr(links_occupation_clean, links_occupation_clean_text, get_occupation, bow_run)
```

Cleaning the data (mostly tedious code)
---------------------------------------

``` r
out_test_spread<-out_test %>% spread(title,value )
out_test_spread<-out_test_spread %>% dplyr::rename_all(funs(make.names(.)))

out_test_spread$Average.age<-gsub(" years","",out_test_spread$Average.age)
out_test_spread$Employment.Size<-gsub("workers","",out_test_spread$Employment.Size)
out_test_spread$Full.Time.Share<-gsub("% Full-Time","",out_test_spread$Full.Time.Share)
out_test_spread$Gender.Share<-gsub("% female","",out_test_spread$Gender.Share)

out_test_spread<-out_test_spread %>%
  mutate(Weekly.Pay=recode(Weekly.Pay,"Unavailable"=NA_character_))

out_test_spread$Employment.Size<-trimws(out_test_spread$Employment.Size)

out_test_spread$Weekly.Pay<-gsub("$ ","",out_test_spread$Weekly.Pay)
```

``` r

out_test_spread_clean<-out_test_spread %>%
  mutate(Gender.Share=as.numeric(Gender.Share),
         Full.Time.Share=as.numeric(Full.Time.Share),
         Average.age=as.numeric(Average.age),
         Average.full.time=as.numeric(Average.full.time),
         Employment.Size=as.numeric(Employment.Size),
         Weekly.Pay=as.numeric(Weekly.Pay))
```

Bringin the AZSCO codes back in (because Duh)
---------------------------------------------

``` r
d_new<-regmatches(links_occupation_clean, regexpr("(\\d+)", links_occupation_clean))

with_occupation_code_new<-bind_cols(out_test_spread_clean, as.data.frame(d_new))
```
