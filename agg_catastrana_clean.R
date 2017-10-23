rm(list = ls())

setwd("Z:\\Summer Semester\\Project seminar\\katastrana sales")

library(data.table)
library(dplyr)
library(plyr)
library(readr)
library(stringr)
library(rebus)

sales_2014 <- fread("sales_2014.csv")
sales_2015 <- fread("sales_2015.csv")
sales_2016 <- fread("sales_2016.csv")
sales_2017 <- fread("sales_2017.csv")


##Renaming variables for easier readability--all data sets have the same variable names----
##Make a list of all the sales data frames so we can use the lapply function over them later
sales_list <- list(sales_2014, sales_2015, sales_2016, sales_2017)

#Change variable names names
old_names <- c("Èíslo øízení",  "Datum podání", "Datum zplatnìní", "Listina", "Typ nemovitosti", "Nemovitost",     
               "k.ú.", "RIZENI_ID", "LISTINA_ID", "ID_NEMOVITOSTI")
new_names <- c("Order_Number", "Filing_Date", "Payment_Date", "Charter", "Property_Type",
               "Real_estate", "k.u.", "Management_ID", "LIST_ID", "IDENTITY_ID")

sales_list <- lapply(sales_list, setnames, old = old_names, new = new_names)


##Trying to figure out best way to group this geographically----
#k.u. is the catastrana name-- 48 are located in Brno----
kat_codes <- c("612006",
               "608505",
               "612227",
               "611778",
               "610771",
               "611263",
               "612111",
               "633895",
               "612243",
               "612065",
               "610844",
               "654132",
               "655856",
               "658201",
               "610542",
               "611905",
               "610313",
               "611026",
               "610585",
               "611484",
               "610887",
               "612405",
               "612499",
               "611743",
               "610003",
               "611701",
               "610283",
               "612553",
               "712680",
               "610208",
               "611379",
               "612146",
               "611646",
               "611565",
               "612286",
               "751910",
               "610089",
               "612014",
               "610330",
               "610186",
               "610950",
               "612171",
               "775550",
               "610372",
               "610704",
               "610470",
               "795674",
               "611115")

####Reducing larger sales sets to only those with the Brno Katastranas----


##Code parses through the variable "k.u." (i.e. the Katastrana of the transaction), and
##matches all of those that contain one of the 48 codes associated with the Brno Katastranas.
##Better to use the numeric aS there were problems inputing the names with characters
brno_all <- lapply(sales_list, function(x) x[grepl(paste(kat_codes, collapse="|"), x$k.u.),])

rm(sales_list) #remove these now that we don't need them to free up memory...

##Figure out total number of sales per Katastrana for all data frames
kat_sales <- lapply(brno_all, count, vars = "k.u.")

##Look up all the sales for Zabro over the years--apply over the list a function that looks
#up which observations from the data sets have it's code (610704) in the k.u. variables

###!!!!!!!!!!Need to find a way to filter the ones that have multiple Kadastras associate with them!!!!!!

zabro_sales <- lapply(brno_all, function(x) x[grepl("610704", x$k.u.)])


#################Subset all of the Zabro sales according to the property type to easily parse them----
#and find the "hot spots" from the various codes associated with the land areas
zabro_units <- lapply(zabro_sales, filter, Property_Type == "JED")
zabro_plots <- lapply(zabro_sales, filter, Property_Type == "PAR")
zabro_buildings <- lapply(zabro_sales, filter, Property_Type == "BUD")
zabro_right_construct <- lapply(zabro_sales, filter, Property_Type == "PS")



##Extract building/parcel number from the units sold----
#General starting pattern for text is "jednotka e. 3510009, (...)". Need to get the number in it,
#and then work fom there
#Use the mutate function from Dplyr to add another column to the data frames in the list
library(rebus)
    ###Extract the numbers for the building parcels from the data-set
    ##Create a function that can be easily used in lapply
    unit_code <- function(x){
          #Length of six, seven, or eight digits
          building_number <- dgt(6,7,8)
          
          #Create the new variable building number
            unit_number <- str_extract(x$Real_estate, pattern = building_number) %>% #Extract the building number from the string in Real_estate
            str_pad(width = 8, side = "left", pad = "0") %>%  #Pad all of the strings so that they're at a uniform length of 8 digits, zeros at the front
            strtrim(width = 4) #Reduce the strings down to only the 4 digits that we need
          return(unit_number)
        }

    ##Add the new variable plot_num to each of the data frames in the zabro_units list
    zabro_units <- lapply(zabro_units, function(x) x = mutate(x, plot_num = unit_code(x)))
    
    ##Combine all of the data frames from the zabro_units list into one

##Extract building/parcel number from the plots sold----
#General pattern is "e. 877/4 Zabrdovice (...)". Need to get the number that appears
#between the spaces
      ###Extract the numbers for the plot parcels from the data-set
      ##Create a function that can be easily used in lapply
      plot_code <- function(x){ 
        
        ##Pattern to take plot number, captures the numbers individually to adjust both before and after "/"
        plot_pat <- capture(one_or_more(DGT)) %R% capture(zero_or_more("/")) %R% capture(zero_or_more(DGT))
        #Save as data frame so it's easier to work with--otherwise it will save as matrix
        plot_divides <- as.data.frame(str_match(x$Real_estate, pattern = plot_pat), stringsAsFactors = F)
        unit_number  <- str_pad(plot_divides[,2], width = 4, side = "left", pad = "0") %>%  #Pad the first numbers so they're of uniform length 4
                        paste0(plot_divides[,3], plot_divides[,4]) #Combine the numbers with a "/" in the middle, use paste0 to concatenate them without a white space
        
        return(unit_number)
      } 
      ##Add the new variable plot_num to each of the data frames in the zabro_units list
      zabro_plots <- lapply(zabro_plots, function(x) x = mutate(x, plot_num = plot_code(x)))


      
###Extract building/parcel number from buildings sold----
#General pattern is "budova bez èp/èe, stavba technického vybavení, na parcele 556/4 Zábrdovice".
#Need to get the number between "na parcele (...) Zábrdovice"
      building_code <- function(x){ 
        ##Pattern to take plot number, captures the numbers individually to adjust both before and after "/" after
        #"na parcele" --is necessary because there are at times other digits that lead the string before the plot number
        plot_pat <- "na parcele" %R% SPC %R% capture(one_or_more(DGT)) %R% capture(zero_or_more("/")) %R% capture(zero_or_more(DGT))
        #Save as data frame so it's easier to work with--otherwise it will save as matrix
        plot_divides <- as.data.frame(str_match(x$Real_estate, pattern = plot_pat), stringsAsFactors = F)
        unit_number  <- str_pad(plot_divides[,2], width = 4, side = "left", pad = "0") %>%  #Pad the first numbers so they're of uniform length 4
          paste0(plot_divides[,3], plot_divides[,4]) #Combine the numbers with a "/" in the middle, use paste0 to concatenate them without a white space
        
        return(unit_number)
      } 
      
      zabro_buildings <- lapply(zabro_buildings, function(x) x = mutate(x, plot_num = building_code(x)))

###Extract building/parcel number from "right to construction"----
###Typical pattern: "úèel rodinný dùm, k parcele 156/79 Sadová"
#CAREFUL!!!! some have a different pattern (though none for the Brno area--only 26 total for the whole
#greater area)
      
      
      construction_code <- function(x){ 
        ##Pattern to take plot number, captures the numbers individually to adjust both before and after "/" after
        #"na parcele" --is necessary because there are at times other digits that lead the string before the plot number
        plot_pat <- "k parcele" %R% SPC %R% capture(one_or_more(DGT)) %R% capture(zero_or_more("/")) %R% capture(zero_or_more(DGT))
        #Save as data frame so it's easier to work with--otherwise it will save as matrix
        plot_divides <- as.data.frame(str_match(x$Real_estate, pattern = plot_pat), stringsAsFactors = F)
        unit_number  <- str_pad(plot_divides[,2], width = 4, side = "left", pad = "0") %>%  #Pad the first numbers so they're of uniform length 4
          paste0(plot_divides[,3], plot_divides[,4]) #Combine the numbers with a "/" in the middle, use paste0 to concatenate them without a white space
        
        return(unit_number)
      } 
      
      zabro_right_construct <- lapply(zabro_right_construct, function(x) x = mutate(x, plot_num = construction_code(x)))

####Next steps after Subsetting areas:----
      

View(zabro_units[1])      
2) Compare difference in location between 2, 3, and 4 digit parcels
3) Group by "hottest" parcels
-On larger and then smaller scale
-Try to see how they're grouped for Zabrdovice first




