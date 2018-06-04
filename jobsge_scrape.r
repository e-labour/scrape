library(rvest)
library(tibble)
library(magrittr)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(writexl)


#inputs: 
newFileName <- "newFileName"
# oldFileName <- "oldFileName" # uncomment if updating database
i <- 6 #number of post pages
#end inputs 

paste2 <- function(charvec, collapse = ", "){if (all(is.na(charvec))){NA} else {paste(charvec, collapse = collapse)}}
get_urls_jobsge <- function(url){
  page <- read_html(url)
  all_urls <- page %>% html_nodes("tr") %>%  html_nodes("td") %>% 
    html_node("a") %>% html_attr("href") %>% str_subset("^/\\d{6}/$") %>%
    paste("http://www.jobs.ge", ., sep = "") %>% unique()
}

# list of all ad urls to scrape 
urls <- paste("http://www.jobs.ge/?page=", as.character(1:i), "&keyword=&cat=&location=&view=jobs", 
              sep = "")
new_ad_urls <- map(urls, get_urls_jobsge) %>% unlist() %>% unique()

#--------uncomment if updating database
# filename <- paste0(oldFileName, ".csv")
# df_old <- read_csv(filename, col_names = TRUE) 
# df_old$X1 <- NULL
# old_ad_ulrs <-  df_old %>% use_series(ვაკანსიის_ლინკი)
# new_ad_urls <- setdiff(new_ad_urls, old_ad_urls) 
#--------

# function to scrape data from an ad page with x = ad url. returns a dataframe
scrape_jobsge <- function(x){
  # url to english ad -- position title, employer name and ad body are often translated
  x_eng <- x %>% str_replace(pattern = "http://www.jobs.ge", 
                             replacement = "http://www.jobs.ge/eng")
  ad_html <- read_html(x)
  ad_html_eng <- x_eng  %>% read_html()
  
  ad_content <- ad_html %>% html_nodes(".content") %>% html_nodes(xpath = "//div[@id = 'job']") 
  ad_content_eng <- ad_html_eng %>% html_nodes(".content") %>% html_nodes(xpath = "//div[@id = 'job']")
  
  # position title
  A <- ad_content %>% html_nodes(xpath = "//span[@style]") %>% html_text(trim = T) %>% 
    str_replace(pattern = " - .*$", replacement = "") %>% as.data.frame(stringsAsFactors = F)
  A_eng <- ad_content_eng %>% html_nodes(xpath = "//span[@style]") %>% html_text(trim = T) %>% 
    str_replace(pattern = " - .*$", replacement = "") %>% as.data.frame(stringsAsFactors = F)
  
  # position location 
  if (length(ad_content %>% html_nodes(xpath = "//span[@style]") %>% html_nodes("i")) == 0) {
    G <- NA %>% as.data.frame(stringsAsFactors = F)
  } else {
    G <- ad_content %>% html_nodes(xpath = "//span[@style]") %>% html_nodes("i") %>% html_text(trim = T) %>%
      as.data.frame(stringsAsFactors = F)
  }
  if (length(ad_content_eng %>% html_nodes(xpath = "//span[@style]") %>% html_nodes("i")) == 0) {
    G_eng <- NA %>% as.data.frame(stringsAsFactors = F)
  } else {
    G_eng <- ad_content_eng %>% html_nodes(xpath = "//span[@style]") %>% html_nodes("i") %>% html_text(trim = T) %>%
      as.data.frame(stringsAsFactors = F)
  }
  
  
  # ad url
  B <- x %>% as.data.frame(stringsAsFactors = F)
  B_eng <- x_eng %>% as.data.frame(stringsAsFactors = F)
  
  
  ad_table <- ad_content %>% html_nodes(".ad") %>% html_nodes("td")
  # employer info: 
  # C employer name
  # D url for employer page on jobs.ge
  # I description of employer from employer page on jobs.ge
  # K all urls from employer page on jobs.ge
  if (str_detect(string = ad_table[2] %>% html_text(trim = T), pattern = "^მომწოდებელი: ")) {
    C <- ad_table[2] %>% html_text(trim = T) %>% 
      str_replace(pattern = "^მომწოდებელი: ", replacement = "") %>%
      as.data.frame(stringsAsFactors = F)
    employer_url_given <- ad_table[2] %>% html_node("a")
    if (is.na(employer_url_given) | 
        "http://" %in% (employer_url_given %>% html_attr("href")) | 
        "https://" %in% (employer_url_given %>% html_attr("href"))) {
      D <- NA %>% as.data.frame(stringsAsFactors = F)
      I <- NA %>% as.data.frame(stringsAsFactors = F)
      K <- NA %>% as.data.frame(stringsAsFactors = F)
    } else {
      employer_url <- paste("http://www.jobs.ge", employer_url_given %>% html_attr("href"), sep = "") %>%
        str_replace_all(pattern = " ", replacement = "%20")
      D <- employer_url %>% as.data.frame(stringsAsFactors = F)
      
      employer_html = read_html(employer_url)
      employer_content <- employer_html %>% html_node(".content") %>% html_nodes("table") 
      if (length(employer_content) == 0) {
        I <- NA	%>% as.data.frame(stringsAsFactors = F)
        K <- NA	%>% as.data.frame(stringsAsFactors = F)
      } else {
        I <- employer_content %>% extract2(2) %>% html_text(trim = TRUE) %>% 
          as.data.frame(stringsAsFactors = F)
        K <- employer_content %>% extract2(2) %>% html_nodes("a") %>% html_attr("href") %>% 
          paste(collapse = ", ") %>% as.data.frame(stringsAsFactors = F)
      }
    }
  } else {
    C <- NA %>% as.data.frame(stringsAsFactors = F)
    D <- NA %>% as.data.frame(stringsAsFactors = F)
    I <- NA %>% as.data.frame(stringsAsFactors = F)
    K <- NA %>% as.data.frame(stringsAsFactors = F)
  }
  
  # text from ad body
  H <- ad_table[4] %>%  html_text(trim = T) %>% as.data.frame(stringsAsFactors = F)
  # all urls from ad body
  J <- ad_table[4] %>% html_nodes("a") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% as.data.frame(stringsAsFactors = F)

  ad_table_eng <- ad_content_eng %>% html_nodes(".ad") %>% html_nodes("td")
  if (str_detect(string = ad_table_eng[2] %>% html_text(trim = T), pattern = "^Provided by: ")) {
    C_eng <- ad_table_eng[2] %>% html_text(trim = T) %>% 
      str_replace(pattern = "^Provided by: ", replacement = "") %>%
      as.data.frame(stringsAsFactors = F)
    employer_url_given <- ad_table_eng[2] %>% html_node("a")
    if (is.na(employer_url_given) | 
        "http://" %in% (employer_url_given %>% html_attr("href")) | 
        "https://" %in% (employer_url_given %>% html_attr("href"))) {
      D_eng <- NA %>% as.data.frame(stringsAsFactors = F)
      I_eng <- NA %>% as.data.frame(stringsAsFactors = F)
      K_eng <- NA %>% as.data.frame(stringsAsFactors = F)
    } else {
      employer_url <- paste("http://www.jobs.ge", employer_url_given %>% html_attr("href"), sep = "") %>%
        str_replace_all(pattern = " ", replacement = "%20")
      D_eng <- employer_url %>% as.data.frame(stringsAsFactors = F)
      
      employer_html <- read_html(employer_url)
      employer_content <- employer_html %>% html_node(".content") %>% html_nodes("table") 
      if (length(employer_content) == 0) {
        I_eng <- NA	%>% as.data.frame(stringsAsFactors = F)
        K_eng <- NA	%>% as.data.frame(stringsAsFactors = F)
      } else {
        I_eng <- employer_content %>% extract2(2) %>% html_text(trim = TRUE) %>% 
          as.data.frame(stringsAsFactors = F)
        K_eng <- employer_content %>% extract2(2) %>% html_nodes("a") %>% html_attr("href") %>% 
          paste(collapse = ", ") %>% as.data.frame(stringsAsFactors = F)
      }
    }
  } else {
    C_eng <- NA %>% as.data.frame(stringsAsFactors = F)
    D_eng <- NA %>% as.data.frame(stringsAsFactors = F)
    I_eng <- NA	%>% as.data.frame(stringsAsFactors = F)
    K_eng <- NA	%>% as.data.frame(stringsAsFactors = F)
  }
  
  # posting date (E) and application deadline (F) only from english
  if (str_detect(string = ad_table_eng[3] %>% html_text(trim = T), pattern = "^Published: ")) {
    dates <- ad_table_eng[3] %>% html_nodes("b") %>% html_text(trim = T)
    if (nchar(dates[1]) == 6) {
      E_eng <- as.Date(paste(substr(dates[1], 1, 3), substr(dates[1], 5, 6), "2018", sep = ""), 
                       format = '%b%d%Y') %>% as.data.frame(stringsAsFactors = F)
    } else {
      E_eng <- as.Date(paste(substr(dates[1], 1, 3), "0", substr(dates[1], 5, 5), "2018", sep = ""), 
                       format = '%b%d%Y') %>% as.data.frame(stringsAsFactors = F)
    }
    
    if (nchar(dates[2]) == 6) {
      F_eng <- as.Date(paste(substr(dates[2], 1, 3), substr(dates[2], 5, 6), "2018", sep = ""), 
                       format = '%b%d%Y') %>% as.data.frame(stringsAsFactors = F)
    } else {
      F_eng <- as.Date(paste(substr(dates[2], 1, 3), "0", substr(dates[2], 5, 5), "2018", sep = ""), 
                       format = '%b%d%Y') %>% as.data.frame(stringsAsFactors = F)
    }
  } else {
    E_eng <- NA %>% as.data.frame(stringsAsFactors = F)
    F_eng <- NA %>% as.data.frame(stringsAsFactors = F)
  }
  
  
  H_eng <- ad_table_eng[4] %>%  html_text(trim = T) %>% as.data.frame(stringsAsFactors = F)
  J_eng <- ad_table_eng[4] %>% html_nodes("a") %>% html_attr("href") %>% 
    paste(collapse = ", ") %>% as.data.frame(stringsAsFactors = F)
  
  df_row <- cbind(A, A_eng, B, B_eng, C, C_eng, D, D_eng, E_eng, F_eng, G, G_eng, H, H_eng, 
                  I, I_eng, J, J_eng, K, K_eng)
  names(df_row) <- c("ვაკანსია", "Position", "ვაკანსიის_ლინკი", "Position_url", "დამსაქმებელი", "Employer",                   
                     "დამსაქმებლის_ლინკი", "Employer_url", "გამოქვეყნდა", "ბოლო_ვადა", "მდებარეობა", "Location",
                     "ვაკანსიის_დეტალები", "Position_info", "დამსაქმებლის_დეტალები", "Employer_info", 
                     "ლინკები_ვაკანსიის_დეტალებიდან", "urls_from_position_info", 
                     "ლინკები_დამსაქმებლის_დეტალებიდან", "urls_from_employer_info")
  
  df_row <- as.tibble(df_row)
  # print(x) # uncomment if you wish to follow progress in map_df call below
  return(df_row)  
}

df <- map_df(new_ad_urls, scrape_jobsge)
# df <- bind_rows(df, df_old) # uncomment if updating database

write.csv(df, file = paste(newFileName, '.csv', sep = ""))
write_xlsx(df, path = paste(newFileName, '.xlsx', sep = ""), col_names = TRUE)


