library(tidyr)
library(dplyr)
library(readr)
refine <- read_csv("Documents/Springboard/refine.csv")
View(refine)

refine %>% select(company) %>% unique()
refine$company <- gsub(pattern = ".*\\ps$", replacement = "philips", x = refine$company, ignore.case = TRUE)
refine$company <- gsub(pattern = ".*n$", replacement = "van houten", x = refine$company, ignore.case = TRUE)
refine$company <- gsub(pattern = "^u.*r$", replacement = "unilever", x = refine$company, ignore.case = TRUE)
refine$company <- gsub(pattern = ".*[o|0]$", replacement = "akzo", x = refine$company, ignore.case = TRUE)
refine %>% select(company) %>% unique()

refine <- separate(refine, "Product code / number", c("product_code", "product_number"), sep = "-")

refine$product_category <- gsub(pattern = "^p$", replacement = "Smartphone", sub("^x$", "Laptop", sub("^v$", "TV", sub("^q$", "Tablet", refine$product_code))))

refine <- unite(refine, "full_address", address, city, country, sep = ",")

refine <- refine %>%
  mutate(company_philips = ifelse(company == "philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = if_else(company=="van houten", 1, 0)) %>% 
  mutate(company_unilever = if_else(company=="unilever", 1, 0))
refine <- refine %>%
  mutate(product_smartphone = if_else(product_category == "Smartphone", 1, 0)) %>% 
  mutate(product_tv = if_else(product_category == "TV", 1, 0)) %>% 
  mutate(product_laptop = if_else(product_category == "Laptop", 1, 0)) %>% 
  mutate(product_tablet = if_else(product_category == "Tablet", 1, 0))

write.csv(refine, file="refine_clean.csv")
