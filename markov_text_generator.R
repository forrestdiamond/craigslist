# source: https://gist.github.com/primaryobjects/4c7cca705eeba0d8bad6

package_list <- c("tidyverse", "markovchain")
lapply(package_list, library, character.only = TRUE)

data_loc <- "C:/Users/lenovo/OneDrive/Projects/CL Rentals/main.csv"

cl_texts <- data_loc %>%
  read_csv() %>%
  mutate(pid = as.character(pid),
         price_num = as.numeric(cost))%>% 
  filter((!is.na(lon)) & (!is.na(lat)) & (!is.na(price_num))) %>%
  mutate(t = gsub(" QR Code Link to This Post\n", "", t)) %>%
  mutate(t = gsub("\\s+", " ", t)) %>%
  mutate(t = gsub("\\.", " .", t)) %>%
  mutate(t = gsub("\\,", " ,", t)) %>%
  mutate(t = gsub("\\!", " !", t)) %>%
  mutate(t = gsub("\\?", " ?", t)) %>%
  mutate(t = gsub("\\(", " (", t)) %>%
  mutate(t = gsub("\\)", " )", t))

terms <- cl_texts %>%
  select(t) %>%
  unlist() %>%
  unname() %>%
  strsplit(., " ") %>%
  unlist()

fit <- markovchainFit(data = terms)
