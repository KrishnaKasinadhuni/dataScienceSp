library(readxl)
library(devtools)
library(dplyr)
library(tidyr)

modifyCompany <- function(x) {
 y <- gsub(pattern = "(^p.*|^f.*)", replacement = "philips", x) %>% 
  gsub(pattern = "(^a.*|^a.*$0)", replacement = "akzo", x) %>% 
  gsub(pattern = "(^v.*)", replacement = "van houten", x) %>% 
  gsub(pattern = "(^u.*)", replacement = "unilver", x)
  return(y)
}
refine_original <- read_excel("refine_original.csv")

refine_original <- tbl_df(refine_original)

refine_original$company <- tolower(refine_original$company)

refine_original$company <- modifyCompany(refine_original$company)

refine_original <- refine_original %>%
  separate(`Product code / number`, c("product_code", "product_number"), "\\-") 
  
refine_original <-  refine_original %>% mutate(product_category = "default") 

refine_original$product_category <- sapply(refine_original$product_code, function(code) {
  if (code == "p") {
    refine_original$product_category = 'Smartphone'
  }
  else if (code == "q") {
    refine_original$product_category  = 'Tablet'
  }
  else if (code == "v") {
    refine_original$product_category  = 'TV'
  }
  else if (code == "x") {
    refine_original$product_category  = 'Laptop'
  }
})

refine_original <- arrange(refine_original, product_category,company)

refine_clean <- refine_original %>% 
  unite(full_address, c(address, city, country), sep = ",") %>% 
  mutate(company_akzo = as.numeric(refine_original$company == 'akzo')) %>% 
  mutate(company_philips = as.numeric(refine_original$company == 'philips')) %>% 
  mutate(company_unilever = as.numeric(refine_original$company == 'unilever')) %>% 
  mutate(company_vanhouten = as.numeric(refine_original$company == 'van houten')) %>%
  mutate(product_laptop = as.numeric(refine_original$product_category == 'Laptop')) %>% 
  mutate(product_smartphone = as.numeric(refine_original$product_category == 'Smartphone')) %>% 
  mutate(product_tablet = as.numeric(refine_original$product_category == 'Tablet')) %>% 
  mutate(product_tv = as.numeric(refine_original$product_category == 'TV'))

glimpse(refine_clean)

write.csv(refine_clean, file = 'refine_clean.csv')
