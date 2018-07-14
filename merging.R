library(tibble)
library(magrittr)
library(readr)
library(dplyr)
library(stringr)
library(writexl)


#----inputs----
filename_hr <- "hr.csv"
filename_jobs <- "jobs.csv"
filename_gov <- "gov.csv"
#--------------


# import and select columns -- hr
df_hr <- read_csv(filename_hr, col_names = TRUE) 
df_hr <- df_hr %>% select("ვაკანსია", "დამსაქმებელი", "ვაკანსიის_დეტალები",
                          "დამსაქმებლის_დეტალები", "გამოქვეყნდა", "ბოლო_ვადა",  
                          "მდებარეობა", "ვაკანსიის_ლინკი", "დამსაქმებლის_ლინკი",  
                          "კატეგორია_განცხადებიდან", "სამუშაო_განრიგი", "ანაზღაურება", 
                          "განათლება", "გამოცდილება", "ენები")
df_hr$source <- factor("hr", levels = c("hr", "jobs", "gov"))


# import and select columns -- jobs
df_jobs <- read_csv(filename_jobs, col_names = TRUE) 
divider2 = " YYYY "
df_jobs <- df_jobs %>%
  mutate(vakansiis_detalebi = paste(ვაკანსიის_დეტალები, Position_info, sep = divider2)) %>%
  mutate(damsaqmeblis_detalebi = paste(დამსაქმებლის_დეტალები, Employer_info, sep = divider2))
df_jobs <- df_jobs %>% select("ვაკანსია", "დამსაქმებელი", "Position", "Employer", "vakansiis_detalebi",
                              "damsaqmeblis_detalebi", "გამოქვეყნდა", "ბოლო_ვადა",  "მდებარეობა", 
                              "ვაკანსიის_ლინკი", "დამსაქმებლის_ლინკი")
names(df_jobs) <- c("ვაკანსია", "დამსაქმებელი", "Position", "Employer", "ვაკანსიის_დეტალები",
                    "დამსაქმებლის_დეტალები", "გამოქვეყნდა", "ბოლო_ვადა",  "მდებარეობა", 
                    "ვაკანსიის_ლინკი", "დამსაქმებლის_ლინკი")
df_jobs$source <- factor("jobs", levels = c("hr", "jobs", "gov"))


# import and select columns -- gov
df_gov <- read_csv(filename_gov, col_names = TRUE) 
df_gov <- df_gov %>% select("ვაკანსია", "დამსაქმებელი", "კატეგორია_განცხადებიდან", "ბოლო_ვადა", 
                            "თანამდებობის_დასახელება", "ადგილების_რაოდენობა", "მდებარეობა", 
                            "სამუშაო_განრიგი", "გამოსაცდელი_ვადა", "ანაზღაურება", "განათლება", 
                            "გამოცდილება", "ენები", "ვაკანსიის_ლინკი", "ვაკანსიის_დეტალები")
df_gov$source <- factor("gov", levels = c("hr", "jobs", "gov"))


#clean the ვაკანსია and დამსაქმებელი columns before attempting to merge.
pattern <- "\\+|\\)|\\(|\\||\\.|\\<|\\>"
df_jobs <- df_jobs %>% mutate(position_cleaned = str_replace_all(ვაკანსია, pattern, "")) %>%
  mutate(employer_cleaned = str_replace_all(დამსაქმებელი, pattern, "")) %>%
  mutate(position_eng_cleaned = str_replace_all(Position, pattern, "")) %>%
  mutate(employer_eng_cleaned = str_replace_all(Employer, pattern, ""))
df_hr <- df_hr %>% mutate(position_cleaned = str_replace_all(ვაკანსია, pattern, "")) %>%
  mutate(employer_cleaned = str_replace_all(დამსაქმებელი, pattern, "")) 
df_gov <- df_gov %>% mutate(position_cleaned = str_replace_all(ვაკანსია, pattern, "")) %>%
  mutate(employer_cleaned = str_replace_all(დამსაქმებელი, pattern, "")) 


# merging function to add a jobs row to hr, and to add a hr_jobs row to gov 
# jobs and hr_jobs have english and georgian translations 
add_row_to_df <- function(row, df, n = 4) {
  index1 <- (row$position_cleaned == df$position_cleaned) | (row$position_eng_cleaned == df$position_cleaned)
  index1[is.na(index1)] <- FALSE
  if (any(index1) == FALSE) {test <- FALSE} else {
    df1 <- df[index1,]
    index2 <- (row$employer_cleaned == df1$employer_cleaned) | (row$employer_eng_cleaned == df1$employer_cleaned)
    index2[is.na(index2)] <- FALSE
    if (any(index2) == FALSE) {test <- FALSE} else {
      df2 <- df1[index2,]
      x <- row$ბოლო_ვადა[1] 
      if (is.na(x)) {test <- TRUE} else {
        test <- any(abs(x - c(as.Date("01011900", format = '%d%m%Y'), 
                            df2$ბოლო_ვადა[!is.na(df2$ბოლო_ვადა)])) < n) # test is NA iff x is NA
      }
    }
  }
  return(!test)                          
}

#remove rows without deadline from hr. 
df_hr <- df_hr[!is.na(df_hr$ბოლო_ვადა),]

# add jobs to hr
jobs_test <- vapply(1:dim(df_jobs)[1], function(i) add_row_to_df(df_jobs[i,], df_hr), FUN.VALUE = TRUE)
df_hr_jobs <- bind_rows(df_hr, df_jobs[jobs_test,])




#remove rows without deadline from gov. 
df_gov <- df_gov[!is.na(df_gov$ბოლო_ვადა),]

# add hr_jobs to gov 
hr_jobs_test <- vapply(1:dim(df_hr_jobs)[1], function(i) add_row_to_df(df_hr_jobs[i,], df_gov), FUN.VALUE = TRUE)
df_gov_hr_jobs <- bind_rows(df_gov, df_hr_jobs[hr_jobs_test,])


# rearrange columns
df_gov_hr_jobs <- df_gov_hr_jobs %>% select(source, ვაკანსია, დამსაქმებელი, გამოქვეყნდა,
                                            ბოლო_ვადა, მდებარეობა, ანაზღაურება, განათლება, 
                                            გამოცდილება, ენები, ადგილების_რაოდენობა, გამოსაცდელი_ვადა, 
                                            სამუშაო_განრიგი, თანამდებობის_დასახელება, კატეგორია_განცხადებიდან, 
                                            ვაკანსიის_დეტალები, დამსაქმებლის_დეტალები, ვაკანსიის_ლინკი, 
                                            დამსაქმებლის_ლინკი, Position, Employer)


write.csv(df_gov_hr_jobs, file = "all.csv")
write_xlsx(df_gov_hr_jobs, "all.xlsx", col_names = TRUE)



