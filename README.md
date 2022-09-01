# research-computing

A DT table generated using the following R code.

```
library('rlist')
library('dplyr')
library('rvest')
library('xml2')

## Search Research Computing organizations
rc.org <- "https://api.github.com/search/users?q=research+computing+in:name+type:org&type=Users&per_page=100&page=%d" %>%
  sprintf(1:2) %>% list.load("json") %>% list.ungroup(level = 3) %>%
  .[names(.) %in% 'login'] %>% as.character

# Scrape Function
#   The resulted on n=130 "Research Computing" organizations. 
#   To waive the GH API rate limit, github_scrape(...) was used
github_scrape <- function(org_names){

  URL <- paste0("https://github.com/", org_names)
  edata <- read_html(URL)
  body_nodes <- edata %>%
    html_node('body') %>%
    html_children()
  
  # LOCATION
  LOCATION <- body_nodes %>% 
    xml_find_all("//span[contains(@itemprop, 'location')]") %>% 
    html_text()
  
  # Organization website
  WEBSITE <- body_nodes %>% 
    xml_find_all("//a[contains(@itemprop, 'url')]") %>% 
    html_text() %>% tibble::enframe() %>% 
    filter(!startsWith(value,'@')) %>% 
    select(value) %>% as.character()
  
  # Organization Name
  NAME <- body_nodes %>% 
    xml_find_all("//h1[contains(@class, 'h2 lh-condensed')]") %>% 
    html_text() %>%
    gsub(pattern = "\\s+",replacement = " ") %>% 
    stringr::str_trim()
  
  # Twitter handle
  TWITTER <- body_nodes %>% 
    xml_find_all("//a[contains(@itemprop, 'url')]") %>% 
    html_text() %>% tibble::enframe() %>% 
    filter(startsWith(value,'@')) %>% 
    select(value) %>% as.character()
  
  # Metadata vector
  VEC <- c(github=org_names,name=NAME, website=WEBSITE,twitter=TWITTER)
  return(VEC)
}

# Do call
rc.metadata <- do.call(rbind,lapply(rc.org, FUN=github_scrape))

# Load scraped metadata
meta.df <- as.data.frame(rc.metadata)
meta.df <- meta.df[order(meta.df$github),]

# Add GitHub hyperlinks
ghURL <- paste0("https://github.com/", meta.df$github)
meta.df$github <-paste0("<a href='",ghURL,"'",
                        ' target=\"_blank\">',
                        meta.df$github,"</a>")
# Remove NA's
meta.df[meta.df == "character(0)"] <- NA

# Reorder columns
meta.df <- subset(meta.df, select=c('github','name','twitter','website'))

# Convert to DT
meta.dt <- DT::datatable(meta.df,escape = F,rownames = F,
                         options = list(pageLength = 100,
                                        autoWidth = TRUE,
                                        fixedColumns = list(leftColumns = 0)))
```
