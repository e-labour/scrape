library(rvest)
library(tibble)
library(magrittr)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(writexl)


#inputs: 
newFileName <- "newFileName" # do not include extension 
# oldFileName <- "oldFileName" # do not include extension, uncomment if updating database
i <- 2 #number of active posts pages
j <- 2 #number of relative archive pages
#end inputs 

paste2 <- function(charvec, collapse = ", "){if (all(is.na(charvec))){NA} else {paste(charvec, collapse = collapse)}}
get_urls_govge <- function(url){
  page <- read_html(url)
  all_urls <- page %>% html_nodes("tr") %>%  html_nodes("td") %>% 
    html_node("a") %>% html_attr("href") %>% str_subset("/JobProvider/UserOrgVaks/Details/") %>%
    paste("https://www.hr.gov.ge", ., sep = "") %>% unique()
}

# list of all ad urls to scrape
current <- paste("https://www.hr.gov.ge/?pageNo=", as.character(1:i), sep = "")
archive <- paste("https://www.hr.gov.ge/?archive=true&pageNo=", as.character(1:j), sep = "")
urls <- c(current, archive)
new_ad_urls <- map(urls, get_urls_govge) %>% unlist()

#--------uncomment if updating database
# filename <- paste0(oldFileName, ".csv")
# df_old <- read_csv(filename, col_names = TRUE)
# df_old$X1 <- NULL
# old_ad_urls <-  df_old %>% use_series(ვაკანსიის_ლინკი) %>% substr(1,59)
# new_ad_urls <- new_ad_urls[!(substr(new_ad_urls, 1, 59) %in% old_ad_urls)] # cannot use setdiff function here due to different url structure for active and archive posts
#--------


# function to scrape data from an ad page with x = ad url and deviders for separating multiple sections of ad body text. 
# returns a dataframe
scrape_govge <- function(x, divider1 = "  XXXX  ", divider2 = " YYYY "){
  ad_html <- read_html(x) %>% html_nodes(xpath = '//*[@id="regForm"]') 
  ad_dls <- ad_html %>% html_nodes("dl")
  
  info_list = list("განცხადების ბოლო ვადა" = NA, "პოზიციის დასახელება" = NA, "თანამდებობის დასახელება"= NA, "ორგანიზაცია"= NA, 
                   "კატეგორია"= NA, "თანამდებობრივი სარგო:"= NA, "ადგილების რაოდენობა"= NA, "სამსახურის ადგილმდებარეობა"= NA, 
                   "სამუშაოს ტიპი"= NA, "გამოსაცდელი ვადა"= NA, "მინიმალური განათლება"= NA, "სამუშაო გამოცდილება"= NA, 
                   "ენები"= NA)
  
  info_matrix1 <- ad_dls[1] %>% html_text(trim = TRUE) %>% str_split(pattern = "\\s\\s\\s\\s\\s+") %>% extract2(1) 
  for (i in 1:length(info_matrix1)){
    info_list[[info_matrix1[i]]] <- info_matrix1[i+1]
  }
  
  A <- info_list[["პოზიციის დასახელება"]] %>% as.data.frame(stringsAsFactors = F)
  # ad url
	B <- x %>% as.data.frame(stringsAsFactors = F)
	
	C <- info_list[["ორგანიზაცია"]] %>% as.data.frame(stringsAsFactors = F)
	D <- as.Date(info_list[["განცხადების ბოლო ვადა"]], format = '%d.%m.%Y') %>% as.data.frame(stringsAsFactors = F)
		
	extra_info_vec <- ad_dls[-1] %>% html_text(trim = TRUE) %>% unlist() 
	extra_info_matrix <- extra_info_vec %>% str_split(pattern = "\\s\\s\\s\\s\\s+", n = 2, simplify = TRUE)			
					
	ind <- which(extra_info_matrix[,1] == "მოთხოვნები")	
	if (length(ind) != 0) {
		info_matrix2 <- extra_info_matrix[ind,2] %>% str_split(pattern = "\\s\\s\\s\\s\\s+") %>% extract2(1) %>% matrix(ncol = 2, byrow = TRUE)
			for (i in 1:dim(info_matrix2)[1]){
				info_list[[info_matrix2[i,1]]] <- info_matrix2[i,2]
			}
		}
		
	ad_tables <- ad_html %>% html_nodes("table") %>% html_text(trim = TRUE) %>% unlist()
	languages <- ad_tables %>% str_split(pattern = "\\s\\s\\s+") %>% unlist() %>% str_subset(pattern = "ური$|ული$") 
	if (length(languages) != 0){info_list[["ენები"]] <- languages %>% paste2()}
		
	E <- info_list[["თანამდებობის დასახელება"]] %>% as.data.frame(stringsAsFactors = F)
	G <- info_list[["კატეგორია"]] %>% as.data.frame(stringsAsFactors = F)
	H <- info_list[["თანამდებობრივი სარგო:"]] %>% as.data.frame(stringsAsFactors = F)
	I <- info_list[["ადგილების რაოდენობა"]] %>% as.data.frame(stringsAsFactors = F)
	J <- info_list[["სამსახურის ადგილმდებარეობა"]] %>% as.data.frame(stringsAsFactors = F)
	K <- info_list[["სამუშაოს ტიპი"]] %>% as.data.frame(stringsAsFactors = F)
	L <- info_list[["გამოსაცდელი ვადა"]] %>% as.data.frame(stringsAsFactors = F)
	M <- info_list[["მინიმალური განათლება"]] %>% as.data.frame(stringsAsFactors = F)
	N <- info_list[["სამუშაო გამოცდილება"]] %>% as.data.frame(stringsAsFactors = F)
	O <- info_list[["ენები"]] %>% as.data.frame(stringsAsFactors = F)
		
	if (length(extra_info_vec) != 0) {text1 <- paste2(extra_info_vec, collapse = divider1)} else {text1 <- NA}
	if (length(ad_tables) != 0) {text2 <- paste2(ad_tables, collapse = divider1)} else {text2 <- NA}
	text3 <- ad_dls[1] %>% html_text(trim = TRUE)
	texts <- paste(text1, text2, text3, collapse = divider2)
	# parts of text from ad body 	
	P <- texts %>% as.data.frame(stringsAsFactors = F)
	
	
	df_row <- cbind(A, C, G, D, E, I, J, K, L, H, M, N, O, P, B)
	names(df_row) <- c("ვაკანსია", "დამსაქმებელი", "კატეგორია_განცხადებიდან", "ბოლო_ვადა",
	                   "თანამდებობის_დასახელება", "ადგილების_რაოდენობა", "მდებარეობა", "სამუშაო_განრიგი",
	                   "გამოსაცდელი_ვადა", "ანაზღაურება", "განათლება", "გამოცდილება", "ენები", 
	                   "ვაკანსიის_დეტალები", "ვაკანსიის_ლინკი")
	
	df_row <- as.tibble(df_row)
	# print(x) # uncomment if you wish to follow progress in map_df call below
	return(df_row)  
}

df <- map_df(new_ad_urls, scrape_govge)
df_old$ადგილების_რაოდენობა <- as.character(df_old$ადგილების_რაოდენობა)
# df <- bind_rows(df, df_old) # uncomment if updating database

write.csv(df, file = paste(newFileName, '.csv', sep = ""))
write_xlsx(df, path = paste(newFileName, '.xlsx', sep = ""), col_names = TRUE)


