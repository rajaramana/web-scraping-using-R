library("rvest")
library("dplyr")
library("plyr")
library("purrr")
library("RCurl")

# Set the directory
wd <- "C:\\Users\\rkova\\Desktop\\Github-Personal\\coin_info_scrape"
setwd(wd)

# Read the input file
cert_grade_info <- read.csv("./input_file/Sample - Certificates.csv")

# static url of the page
url <- "https://www.ngccoin.com/certlookup/"

# Function to scrape information
get_data <- function(cert,grade){
  full_url <- paste0(url,cert,'/',grade,'/')
  coin_info <- read_html(full_url)
  return(coin_info)
}


data_wrangle <- function(cert,grade){
  cert <- as.character(cert)
  grade <- as.character(grade)
  
  print(paste('scraping cert',cert,'grade',grade,sep=' '))
  
  #get coin data
  coin_data <- get_data(cert,grade)
  
  # get the header information
  coin_headers <- coin_data %>% html_nodes('.content-wrapper dt') %>% html_text()
  coin_headers <- c('Certificate','Grade',coin_headers)
  
  # get header information
  header_information <- coin_data %>% html_nodes('.content-wrapper dd') %>% html_text()
  header_information <- c(cert,grade,header_information)
  
  # download images
  coin_images <- coin_data %>% html_nodes("div .certlookup-images-item a") %>% html_attr("data-fancybox-href")
  
  if(length(coin_images)==0){
    download.file("http://via.placeholder.com/2160x2880",destfile= paste0('./img_files/','NGC',cert,'_OBV.JPG'),method='libcurl')
    
    # reverse image
    download.file("http://via.placeholder.com/2160x2880",destfile= paste0('./img_files/','NGC',cert,'_REV.JPG'),method='libcurl')
    
    # adding images to headers
    coin_headers <- c(coin_headers,'img_obv_url','img_rev_url')
    
    # Passing NA to header
    header_information <- c(header_information,NA,NA)
    
  }else{
    download.file(coin_images[[grep(".*_OBV.JPG.*",coin_images)]],destfile= paste0('./img_files/','NGC',cert,'_OBV.JPG'),method='libcurl')
    
    # reverse image
    download.file(coin_images[[grep(".*_REV.JPG.*",coin_images)]],destfile= paste0('./img_files/','NGC',cert,'_REV.JPG'),method='libcurl')
    
    # adding images to headers
    coin_headers <- c(coin_headers,'img_obv_url','img_rev_url')
    
    header_information <- c(header_information,coin_images[[grep(".*_OBV.JPG.*",coin_images)]],coin_images[[grep(".*_REV.JPG.*",coin_images)]])
  }
  
  # creating a list
  header_info_list <- map(header_information,function(x)x)
  
  # providing headers for the list
  names(header_info_list) <- coin_headers
  
  df <- data.frame(t(sapply(header_info_list,c)))
  return(df)
}


# main code
df <- data.frame()

for(i in 1:nrow(cert_grade_info)){
  df <- rbind.fill(df,data_wrangle(cert_grade_info[i,1],cert_grade_info[i,2]))
  Sys.sleep(runif(1,1,60))
}

write.csv(df,"./output_file/output.csv")