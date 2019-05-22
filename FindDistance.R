#install.packages("gmapsdistance")
library(gmapsdistance)  # for getting info from the API
library(dplyr)  # for data manipulation and pipes (%>%)
# apikey: AIzaSyBlHVJmbfieU6rZKDMxzDknlqwc7bxXyZc
#email: hanieh22181@gmail.com

set.seed(1337)  # for reproducibility of our sample
set.api.key("AIzaSyBlHVJmbfieU6rZKDMxzDknlqwc7bxXyZc")


library(rvest)
library(httr)

##   FOR SALE ##----------------------


#Specifying the url for desired website to be scraped
url="https://www.realestate.com.au/buy/property-house-with-1-bedroom-between-0-175000-in-new+south+wales%3b/list-1?includeSurrounding=false"

#rank_data_html <- html_nodes(webpage,".residential-card__details-link span, .property-price,.general-features__cars , .general-features__baths , .general-features__beds")

Page_html<-html_node(webpage,".pagination__ellipsis+ .rui-button-basic")
NumberofPages<-html_text(Page_html)

DF<-data.frame()
DF.pg<-data.frame()
for(pg in 1:NumberofPages){
  print(pg)
  url=paste0("https://www.realestate.com.au/buy/property-house-with-1-bedroom-between-0-175000-in-new+south+wales%3b/list-",pg,"?includeSurrounding=false")
  #Reading the HTML code from the website
  webpage <- read_html(url,timeout = 444)
  Address_html <- html_text(html_nodes(webpage,".residential-card__details-link span"))
  Price_html <- html_text(html_nodes(webpage,".property-price"))
  Baths_html <- html_text(html_nodes(webpage,".general-features__baths"))
  Beds_html <- html_text(html_nodes(webpage,".general-features__beds"))
  DF.pg<-data.frame("Suburb"=as.character(gsub(".*[,] ","",Address_html)),"Address"=Address_html,"Price"=Price_html,"Baths"=Baths_html,"Beds"=Beds_html,"DistanceTime(min)"=0,"Distance(km)"=0)
  #rank_data_html <- html_nodes(webpage,".residential-card__details-link span, .property-price,.general-)features__cars , .general-features__baths , .general-features__beds")
  
  DF.pg<-DF.pg[grepl("[0-9]{2,}",DF.pg$Price),]
  DF.pg<-DF.pg[!grepl("AUCTION|(@[0-9])",DF.pg$Price),]
  DF.pg<-DF.pg[!grepl("[0-9][/][0-9]",DF.pg$Address),]
  #ListOfSuburbs<-html_text(Suburb_html)
  if(nrow(DF.pg)==0)next()
  # Call the API
  for (i in 1:nrow(DF.pg) ){
    
    suburb<-as.character(DF.pg$Suburb[i])
    print(suburb)
    if(is.na(match(suburb,DF$Suburb))){
      sch_distances <- gmapsdistance::gmapsdistance(
        origin = paste0(gsub(" ","+",suburb),"+nsw"),  # start point of journey
        destination = "2117+nsw",  # end point of journey
        mode = "driving",  # driving time
        shape = "wide"  # format of output data (origin and destination as cols)
      )
      DF.pg$DistanceTime.min.[i]<-(sch_distances$Time/60)#min
      DF.pg$Distance.km.[i]<-(sch_distances$Distance/1000)#km
    }else{
      DF.pg$DistanceTime.min.[i]<-DF$DistanceTime.min.[match(suburb,DF$Suburb)]
      DF.pg$Distance.km.[i]<-DF$Distance.km.[match(suburb,DF$Suburb)]
    }
    
    
  }
  DF<-rbind(DF,DF.pg)
}

DF$Status<-"ForSale"
write.csv(DF,"ListofPropertiesForSale.csv",row.names = F)

##   SOLD ##----------------------


#Specifying the url for desired website to be scraped
url="https://www.realestate.com.au/sold/property-house-with-1-bedroom-between-0-175000-in-new+south+wales%3b/list-1?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate"

#rank_data_html <- html_nodes(webpage,".residential-card__details-link span, .property-price,.general-features__cars , .general-features__baths , .general-features__beds")

Page_html<-html_node(webpage,".pagination__ellipsis+ .rui-button-basic")
NumberofPages<-html_text(Page_html)

DF<-data.frame()
DF.pg<-data.frame()
for(pg in 1:NumberofPages){
  print(pg)
  url=paste0("https://www.realestate.com.au/sold/property-house-with-1-bedroom-between-0-175000-in-new+south+wales%3b/list-",pg,"?includeSurrounding=false&misc=ex-no-sale-price&activeSort=solddate")
  #Reading the HTML code from the website
  webpage <- read_html(url,timeout = 444)
  Address_html <- html_text(html_nodes(webpage,".residential-card__details-link span"))
  Price_html <- html_text(html_nodes(webpage,".property-price"))
  Baths_html <- html_text(html_nodes(webpage,".general-features__baths"))
  Beds_html <- html_text(html_nodes(webpage,".general-features__beds"))
  Date_html <- gsub("Sold on ","",html_text(html_nodes(webpage,".residential-card__property-type+ span")))
  DF.pg<-data.frame("Date"=Date_html,"Suburb"=as.character(gsub(".*[,] ","",Address_html)),"Address"=Address_html,"Price"=Price_html,"Baths"=Baths_html,"Beds"=Beds_html,"DistanceTime(min)"=0,"Distance(km)"=0)
  #rank_data_html <- html_nodes(webpage,".residential-card__details-link span, .property-price,.general-)features__cars , .general-features__baths , .general-features__beds")
  
  DF.pg<-DF.pg[grepl("[0-9]{2,}",DF.pg$Price),]
  DF.pg<-DF.pg[!grepl("AUCTION|(@[0-9])",DF.pg$Price),]
  DF.pg<-DF.pg[!grepl("[0-9][/][0-9]",DF.pg$Address),]
  #ListOfSuburbs<-html_text(Suburb_html)
  if(nrow(DF.pg)==0)next()
  #ListOfSuburbs<-html_text(Suburb_html)
  
  # Call the API
  for (i in 1:nrow(DF.pg) ){
    
    suburb<-as.character(DF.pg$Suburb[i])
    print(suburb)
    if(is.na(match(suburb,DF$Suburb))){
      sch_distances <- gmapsdistance::gmapsdistance(
        origin = paste0(gsub(" ","+",suburb),"+nsw"),  # start point of journey
        destination = "2117+nsw",  # end point of journey
        mode = "driving",  # driving time
        shape = "wide"  # format of output data (origin and destination as cols)
      )
      DF.pg$DistanceTime.min.[i]<-(sch_distances$Time/60)#min
      DF.pg$Distance.km.[i]<-(sch_distances$Distance/1000)#km
    }else{
      DF.pg$DistanceTime.min.[i]<-DF$DistanceTime.min.[match(suburb,DF$Suburb)]
      DF.pg$Distance.km.[i]<-DF$Distance.km.[match(suburb,DF$Suburb)]
    }
    
    
  }
  DF<-rbind(DF,DF.pg)
}


DF$Status<-"Sold"
write.csv(DF,"ListofPropertiesSold.csv",row.names = F)

##   Combine lists ##----------------------

ForSale<-read.csv("ListofPropertiesForSale.csv")
ForSale$Date<-NA
Sold<-read.csv("ListofPropertiesSold.csv")
ListofProperties<-rbind(ForSale,Sold)

# Cleanup #

ListofProperties<-ListofProperties[grepl("[0-9]{2,}",ListofProperties$Price),]
ListofProperties<-ListofProperties[!grepl("AUCTION|(@[0-9])",ListofProperties$Price),]
ListofProperties<-ListofProperties[!grepl("[0-9][/][0-9]",ListofProperties$Address),]


write.csv(ListofProperties,"ListofProperties_all.csv",row.names = F)

##   Median prive ##-------------------

#https://www.microburbs.com.au/NSW/Bathurst-Regional-Municipality/Kelso-(NSW)#investor
##investor-score-collapse .col-md-12