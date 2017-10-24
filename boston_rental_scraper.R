cl_scraper <- function(){
  package_list <- c("tidyverse", "rvest")
  lapply(package_list, library, character.only = TRUE)
  
  data_loc <- "YOUR_MASTER_LOC_HERE"
  
  current_posts <- data_loc %>%
    read_csv() %>%
    mutate(pid = as.character(pid)) %>%
    select(pid) %>%
    unlist() %>%
    unname()
  
  rss_feed <- "https://boston.craigslist.org/search/sub?format=rss"
  
  read_rss <- rss_feed %>%
    read_html()
  
  link_df <- read_rss %>%
    html_nodes("items") %>%
    html_nodes("li") %>%
    html_attrs() %>%
    unlist() %>%
    unname() %>%
    as.data.frame() %>%
    filter(!is.na(.)) %>%
    mutate(id = gsub(".*(\\d{10}).html", "\\1", .)) %>%
    filter(!(id %in% current_posts))
  
  names(link_df) <- c("url", "id")
  
  if(link_df %>% nrow() < 1){
    print("No new posts")
  } else{
    for(i in link_df %>% nrow() %>% seq()){ #seq(1)){ #
      read_post_url <- link_df %>%
        slice(i) %>%
        select(url) %>%
        unlist() %>%
        unname() %>%
        as.character() %>%
        read_html()
      
      geo_xml <- read_post_url %>%
        html_nodes("#map")
      
      if(geo_xml %>% length() < 1){
        geo <- data.frame(id = "map")
        geo$`data-latitude` <- NA
        geo$`data-longitude` <- NA
      } else{
        geo <- geo_xml %>%
          html_attrs() %>%
          unlist() %>%
          as.matrix() %>%
          t() %>%
          as.data.frame()
      }
      text <- read_post_url %>%
        html_nodes("#postingbody") %>%
        html_text()
      
      price <- read_post_url %>%
        html_nodes("span.price") %>%
        html_text() %>%
        gsub("\\$", "", .)
      
      if(price %>% length() < 1){
        price <- read_post_url %>%
          html_nodes("#titletextonly") %>%
          html_text() %>%
          gsub(",", "", .) %>%
          gsub(".*\\$(\\d+).*", "\\1", .)    
      }
      
      date_post <- read_post_url %>%
        html_nodes("time") %>%
        html_attr("datetime") %>%
        .[1]
      
      post_id <- read_post_url %>%
        html_nodes("p.postinginfo") %>%
        .[2] %>%
        html_text() %>%
        gsub("post id: ", "", .)
      
      new_row <- data.frame(pid = post_id %>% as.character(),
                            date = date_post, 
                            cost = price,
                            t = text,
                            lat = geo$`data-latitude`,
                            lon = geo$`data-longitude`)
      
      write_csv(new_row, data_loc, append = TRUE)
      
      print(paste0("Post ", post_id, " scraped..."))
      Sys.sleep(.5)
    }
  }
}
