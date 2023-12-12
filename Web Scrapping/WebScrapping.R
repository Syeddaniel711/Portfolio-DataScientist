library(rvest)
library(dplyr)
library(tidyverse)
#Puma shoes
for (page_result in seq(from=1, to=3, by=1)){
  #link
  link=paste("https://www.amazon.com/s?k=puma+shoes&rh=p_72%3A2661620011&dc&crid=2UKOQWHZHCXYT&qid=1684000847&rnid=2661617011&sprefix=pum+shoes%2Caps%2C338&ref=sr_pg_1")
  page=read_html(link)
  #product name
  product=page %>%
    html_nodes(".a-color-base.a-text-normal") %>%
    html_text()
  #Rating review
  Ratings=page %>% html_nodes(".aok-align-bottom") %>%
    html_text()
  #Quantity sold
  Quantity=page%>% html_nodes(".s-link-style .s-underline-text")%>%
    html_text()
  #combine in one data frame
  Data<-data.frame(product,Ratings,Quantity)
  #To know what type shoes, use strsplit() to split name brand and product
  Type <- strsplit(Data$product, " ")
  
  # extract the the type name product
  type_name <- sapply(Type, `[`, 3)
  
  # new column
  new_data <- data.frame(Type = type_name)
  
  # combine the original data frame
  Final_Data_Puma<- cbind(Data, new_data)
  
}
#Nike Shoes
for (page_result in seq(from=1, to=3, by=1)){
  #link
  link=paste("https://www.amazon.com/s?k=adidas+shoes&crid=17NAWP6BT1OSR&sprefix=adidas+shoes%2Caps%2C436&ref=nb_sb_noss_1")
  page=read_html(link)
  
  #product name
  product=page %>%
    html_nodes(".a-color-base.a-text-normal") %>%
    html_text()
  
  #Rating review
  Ratings=page %>% html_nodes(".aok-align-bottom") %>%
    html_text()
  
  #Quantity sold
  Quantity=page%>% html_nodes(".s-link-style .s-underline-text")%>%
    html_text()
  #combine in one data frame
  Data<-data.frame(product,Ratings,Quantity)
  #To know what type shoes, use strsplit() to split name brand and product
  Type <- strsplit(Data$product, " ")
  
  # extract the the type name product
  type_name <- sapply(Type, `[`, 3)
  
  # new column
  new_data <- data.frame(Type = type_name)
  
  # combine the original data frame with the first name data frame
  Final_Data_Adidas <- cbind(Data, new_data)
  
}

Graph_Adidas<-ggplot(Final_Data_Adidas) +
  geom_bar(mapping = aes(x = Type, y =Quantity, fill=Ratings), stat = "identity",position = "stack")
Graph_Puma<-ggplot(Final_Data_Puma) +
  geom_bar(mapping = aes(x = Type, y =Quantity, fill=Ratings), stat = "identity",position = "stack")
Graph_Puma
