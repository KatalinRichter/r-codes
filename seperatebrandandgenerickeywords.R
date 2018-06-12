file <- "keywords.xlsx"
options(java.parameters = "-Xmx12g")

install.packages("openxlsx")
library("openxlsx")
mydf <- read.xlsx(file, sheet = "DE",colNames = FALSE)
austria <-  read.xlsx(file,sheet="AT",colNames = FALSE)
french_belgium <-  read.xlsx(file,sheet="BEfr",colNames = FALSE)
dutch_belgium <- read.xlsx(file, sheet = "BEnl" ,colNames = FALSE)
swiss_de <- read.xlsx(file, sheet = "CHde" ,colNames = FALSE)
swiss_fr <- read.xlsx(file, sheet = "CHfr" ,colNames = FALSE)
germany <- read.xlsx(file, sheet = "DE",colNames = FALSE)
denmark <- read.xlsx(file, sheet = "DK",colNames = FALSE)
spain <- read.xlsx(file, sheet = "ES",colNames = FALSE)
finland <- read.xlsx(file, sheet = "FI",colNames = FALSE)
france <- read.xlsx(file, sheet = "FR",colNames = FALSE)
italy <- read.xlsx(file, sheet = "IT",colNames = FALSE)
netherlands <- read.xlsx(file, sheet = "NL",colNames = FALSE)
nroway <- read.xlsx(file, sheet = "NO",colNames = FALSE)
poland <- read.xlsx(file, sheet = "PL",colNames = FALSE)
sweden <- read.xlsx(file, sheet = "SE",colNames = FALSE)
uk <- read.xlsx(file, sheet = "UK",colNames = FALSE)

install.packages("xlsx")

###### Done importing files
cat("Finally! We imported the files to Rstudio!","\n")



# firstline <- data[1,]
# data <- data[2:length(data[,1]),]
brands <- read.xlsx("Brands.xlsx",1) # reads the data and the brands
brands <- brands[,1]
brands <- brands[!is.na(brands)] # There are some empty rows and with this we eliminate them.
brands <- tolower(as.character(brands)) # program will look for a match and we set all letters to small
kw_at <- as.character(austria[,1])
if (is.null(austria)){ # if empty, then we will just write in "No kw"
  kw_at <- "No keyword"
}
kw_fbe <- as.character(french_belgium[,1])
if (is.null(french_belgium)){
  kw_fbe <- "No keyword"
}
kw_dbe <- as.character(dutch_belgium[,1])
if (is.null(dutch_belgium)){
  kw_dbe <- "No keyword"
}
kw_ge <- as.character(germany[,1])
if (is.null(germany)){
  kw_ge <- "No keyword"
}
kw_swde <- as.character(swiss_de[,1])
if (is.null(swiss_de)){
  kw_swde <- "No keyword"
}
kw_swfr <- as.character(swiss_fr[,1])
if (is.null(swiss_fr)){
  kw_swfr <- "No keyword"
}
kw_sp <- as.character(spain[,1])
if (is.null(spain)){
  kw_sp <- "No keyword"
}
kw_fi <- as.character(finland[,1])
if (is.null(finland)){
  kw_fi <- "No keyword"
}
kw_fr <- as.character(france[,1])
if (is.null(france)){
  kw_fr <- "No keyword"
}
kw_it <- as.character(italy[,1])
if (is.null(italy)){
  kw_it <- "No keyword"
}
kw_nl <- as.character(netherlands[,1])
if (is.null(netherlands)){
  kw_nl <- "No keyword"
}
kw_no <- as.character(nroway[,1])
if (is.null(nroway)){
  kw_no <- "No keyword"
}
kw_po <- as.character(poland[,1])
if (is.null(poland)){
  kw_po <- "No keyword"
}
kw_sw <- as.character(sweden[,1])
if (is.null(sweden)){
  kw_sw <- "No keyword"
}
kw_uk <- as.character(uk[,1])
if (is.null(uk)){
  kw_uk <- "No keyword"
}
kw_dk <- as.character(denmark[,1])
if (is.null(uk)){
  kw_dk <- "No keyword"
}

sep_brand <- strsplit(brands," ")


starts <- function(brand,kw){
  return(substring(kw,1,nchar(brand))==brand) # looks whether it starts with that kw
}
ends <- function(brand,kw){
  return(substring(kw,(nchar(kw)-nchar(brand)+1),nchar(kw))==brand)
}



is.empty <- function(x, mode=NULL){
  if (is.null(mode)) mode <- class(x)
  identical(vector(mode,1),c(x,vector(class(x),1)))
}





space_after <- paste(brands,"") # stores a variable with space after every brand kw.
# This way it won't have to add a space on every loop.



space_before <- paste("",brands) # also before
twospaces <- paste("",brands,"") # and now in both ends


# check which keywords is a brand. Then it will return a vector of brand/generic split
# so what is left after this function is to simply call the command for every country.
getbrands <- function(keywords,brands,space_after,space_before,twospaces,country){
  keywords <- tolower(keywords)
  sep_brand <- strsplit(brands," ")
  sep_kw <- strsplit(keywords," ")
  brand_count <- (1:length(brands))*0
  brand_char <- brand_count # also nr of characters
  kw_count <- (1:length(keywords))*0
  kw_char <- kw_count
  i <- 1
  isbrand <- (1:length(keywords))*0
i <- 1
m <- length(brands)
n <- length(keywords)
#### Now we shall seperate different words and count how many words are there in different
### brand/keyword

i <- 1
while (i<=length(brands)){
  brand_count[i] <- length(unlist(sep_brand[i]))
  i <- i + 1
}
i <- 1
while (i<=length(keywords)) {
  kw_count[i] <- length(unlist(sep_kw[i]))
  i <- i + 1  
}


i <- 1
while(i<=n){
  cat("Keyword",i,"out of",n,"is being checked whether it's a brand or not.","Country:",
      country,"\n")
  j <- 1
  k <- 0
  while (j<=m){
    if (brand_count[j] < kw_count[i]  ){#&& brand_char[j]<kw_char[i]
      # So this is the optimization. It makes the code run faster. If number of words in brand
      # is smaller than nr of words in our keyword, then it is possible that our keyword
      # includes a brand. Otherwise we will skip all the checking and save up a lot of
      # computing time.
    

    check <- space_after[j] # adds empty space after the brand
    # looking only "brand" won't be sufficient
    # if (!is.empty(grep(check,keywords[i],useBytes = TRUE))){ # if check is in keyword
    if (!is.empty(grep(check,keywords[i]))){
      result <- starts(check,keywords[i]) # TRUE if keyword starts with check
      if (result){ # then we know it's brand ("adidas " will go for "adidas shoe")
        # cat(keywords[i], " contains: ",brands[j],".")
        k <- 1
      }
      if (!result){ # if it does not start with check
        check <- twospaces[j] # we add space before the kw. So now we have: " adidas "
        if (!is.empty(grep(check,keywords[i]))){#if kw still contains check, then we add it
          # cat(keywords[i], " contains: ",brands[j],".")#example: "red adidas shoes"
          k <- 1
        }
      }
      # cat("\n")
    }
    check <- space_before[j] # now we add space before the brand
    if (!is.empty(grep(check,keywords[i]))){ # if check is in keyword
      result <- ends(check,keywords[i]) # TRUE if keyword ends with check
      if (result){ # then we know it's brand (" adidas" will go for "winter collection adidas")
        # cat(keywords[i], "ENDS WITH:",brands[j],".")
        k <- 1

      }
      if (!result){ # if it does not end with check
        check <- twospaces[j] # we add space after the kw. So now we have: " adidas "
        if (!is.empty(grep(check,keywords[i]))){#if kw still contains check, then we add it
          # cat(keywords[i], " contains: ",brands[j],".")#example: "red adidas shoes"
          k <- 1
        }
      }
    }


    }
    if (brand_count[j]==kw_count[i]){ # && brand_char[j]==kw_char[i]
      if (keywords[i]==brands[j]){
        # cat("Exact match.",keywords[i],"equals",brands[j],"\n")
        k <- 1

      }# The goal here is to make as little comparissons with words as possible.
      # We prefer making number comparissons; Rstudio is better in that.
    }
    # If we find one match, we will end the loop
    if (k==1){
      j <- m + 1 # if k==1 that means that kw is a brand and we can end a loop early.
      # We save up a lot of computing time with this.
    }
    j <- j + 1
  }
  if (k==0){
    isbrand[i] <- "Generic"
  }
  if (k==1){
    isbrand[i] <- "Branded" 
  }
  i <- i + 1
}

return(isbrand)
}

##### and then we need an additional thing; eliminating the double names such as:
# "nike shoe" is the same as "shoe nike". Always choosing the one with more searches.


# Now we start the with checking for 
# every country. Here we check for every country whether it's a brand or not

brand_at <- getbrands(kw_at,brands,space_after,space_before,twospaces,"Austria")
brand_fbe <- getbrands(kw_fbe,brands,space_after,space_before,twospaces,"Belgium (French)")
brand_dbe <- getbrands(kw_dbe,brands,space_after,space_before,twospaces,"Belgium (Dutch)")
brand_ge <- getbrands(kw_ge,brands,space_after,space_before,twospaces,"Germany")
brand_swde <- getbrands(kw_swde,brands,space_after,space_before,twospaces,"Switzerland (German)")
brand_swfr <- getbrands(kw_swfr,brands,space_after,space_before,twospaces,"Switzerland (French)")
brand_sp <- getbrands(kw_sp,brands,space_after,space_before,twospaces,"Spain")
brand_fi <- getbrands(kw_fi,brands,space_after,space_before,twospaces, "Finland")
brand_fr <- getbrands(kw_fr,brands,space_after,space_before,twospaces,"France")
brand_nl <- getbrands(kw_nl,brands,space_after,space_before,twospaces,"the Netherlands")
brand_no <- getbrands(kw_no,brands,space_after,space_before,twospaces,"Norway")
brand_po <- getbrands(kw_po,brands,space_after,space_before,twospaces,"Poland")
brand_uk <- getbrands(kw_uk,brands,space_after,space_before,twospaces,"United Kingdom")
brand_dk <- getbrands(kw_dk,brands,space_after,space_before,twospaces,"Denmark")
brand_it <- getbrands(kw_it,brands,space_after,space_before,twospaces,"Italy")
brand_sw <- getbrands(kw_sw,brands,space_after,space_before,twospaces,"Sweden")


##########################################################################################
# Task seperating brands and generic kws completed! Let´s move on to the next task!
# Now checking for the same-meanining kws and looking at their search volume!

# First let's find sv (search volume) for all countries.


sv_at <- -1 # we look up for values of search volume. 
# If they are not given, we'll set them to -1
if (2 <= length(austria)){
  sv_at <- as.numeric(as.character(austria[,2]))
}
sv_fbe <- -1
if (2<=length(french_belgium)){
  sv_fbe <- as.numeric(as.character(french_belgium[,2]))
}
sv_dbe <- -1
if (2<=length(dutch_belgium)){
  sv_dbe <- as.numeric(as.character(dutch_belgium[,2]))
}
sv_ge <- -1
if (2<=length(germany)){
  sv_ge <- as.numeric(as.character(germany[,2]))
}
sv_swde <- -1
if (2<=length(swiss_de)){
  sv_swde <- as.numeric(as.character(swiss_de[,2]))
}
sv_swfr <- -1
if (2<=length(swiss_fr)){
  sv_swfr <- as.numeric(as.character(swiss_fr[,2]))
}
sv_sp <- -1
if (2<=length(spain)){
  sv_sp <- as.numeric(as.character(spain[,2]))
}
sv_fi <- -1
if (2<=length(finland)){
  sv_fi <- as.numeric(as.character(finland[,2]))
}
sv_fr <- -1
if (2<=length(france)){
  sv_fr <- as.numeric(as.character(france[,2]))
}
sv_it <- -1
if (2<=length(italy)){
  sv_it <- as.numeric(as.character(italy[,2]))
}
sv_nl <- -1
if (2<=length(netherlands)){
  sv_nl <- as.numeric(as.character(netherlands[,2]))
}
sv_no <- -1
if (2<=length(nroway)){
  sv_no <- as.numeric(as.character(nroway[,2]))
}
sv_po <- -1
if (2<=length(poland)){
  sv_po <- as.numeric(as.character(poland[,2]))
}
sv_sw <- -1
if (2<=length(sweden)){
  sv_sw <- as.numeric(as.character(sweden[,2]))
}
sv_uk <- -1
if (2<=length(uk)){
  sv_uk <- as.numeric(as.character(uk[,2]))
}
sv_dk <- -1
if (2<=length(denmark)){
  sv_dk <- as.numeric(as.character(denmark[,2]))
}
sv_it <- -1
if (2<=length(italy)){
  sv_it <- as.numeric(as.character(italy[,2]))
}


is_same <- function(kw1,kw2){
  sep_kw1 <- strsplit(kw1," ")
  sep_kw2 <- strsplit(kw2," ")
  i <- 1
  while (i <= length(unlist(sep_kw1))){
    k <- 0
    j <- 1
    while (j <= length(unlist(sep_kw2))){
      if (unlist(sep_kw1)[i]==unlist(sep_kw2)[j]){
        k <- 1
      }
      j <- j + 1
    }
    if (k==0){
      return(FALSE)
    }
    i <- i + 1
  }
  return(TRUE)
}

# will define a function which takes a kw and its search volume.
dup_remove <- function(keywords,sv,isbrand,country){
  # only kws with same number of words and characters should be compared.
  # but first... what if no numbers are given? Then just end the function and put
  # -1 for all kws
  if (length(sv)==1){
    mymatrix <- cbind(keywords,sv,isbrand)
    colnames(mymatrix) <- c("Keyword","Search volume","Branded/generic")
    return(mymatrix)
  }
  
  # Now we have some NA values when looking at sv. Sometimes sv is not given.
  # To fix that, we set all NA to 0.
  sv[is.na(sv)] <- 0
  sep_kw <- strsplit(keywords," ")
  kw_count <- (1:length(keywords))*0
  char_length <- (1:length(keywords))*0
  n <- length(keywords)
  getridoff <- c() # we will get rid of those indexes
  i <- 1 # checking number of words in every kw
  while (i<=n) {
    kw_count[i] <- length(unlist(sep_kw[i]))
    char_length[i] <- nchar(keywords[i])
    i <- i + 1  
  }
  i <- 1
  while (i <= n){
    cat("Removing duplicate keywords",i,"out of",n,". Currently checking for",country,"\n")
    j <- i + 1
    while (j<=n){
        if (char_length[i]==char_length[j]){
          if (kw_count[i]==kw_count[j]){
            # It'll only start checking IF keyword has the same nr of chars and words.
            # and if it is not the same index.

            if (is_same(keywords[i],keywords[j])){
              # Now checking if it's true that both are the same
              # cat("Found the dup:",keywords[i],"AND",keywords[j],"\n")
              if (sv[i]>sv[j]){
                getridoff <- c(getridoff,j)
              }
              if (sv[j]>=sv[i]){
                getridoff <- c(getridoff,i)
              }
              #We'll always get rid of one of them!
            }
            
            
          }
          
        }
        
      j <- j + 1
    }
    i <- i + 1
  }
  # Now I have a vector of indexes to get rid off. Time to only keep the relevant ones!
  getridoff <- unique(getridoff) # since it's filled with duplicate values
  useless <- length(getridoff) # nr of kws to get rid off
  getridoff <- sort(getridoff)
  if (useless==0){
    getridoff <- -1
  }
  mykws <- (1:(n-useless))*0
  mysv <- (1:(n-useless))*0
  mybrands <- mysv
  i <- 1
  j <- 1
  k <- 1 # clims over the indexes of getridoff
  while (i<=n){
    if (i!=getridoff[k]){
      mykws[j] <- keywords[i]
      mysv[j] <- sv[i]
      mybrands[j] <- isbrand[i]
      j <- j + 1
    }
    if (i==getridoff[k]){
      k <- k + 1
      if (k>length(getridoff)){
        k <- 1
      }
    }

    i <- i + 1
  }
  mymatrix <- cbind(mykws,mysv,mybrands)
  colnames(mymatrix) <- c("Keyword","Search volume","Branded/generic")
  return(mymatrix)
}


# ok, now let us remove duplicate kws for every country;
final_at <- dup_remove(kw_at,sv_at,brand_at,"Austria")
final_fbe <- dup_remove(kw_fbe,sv_fbe,brand_fbe,"Belgium (French)")
final_dbe <- dup_remove(kw_dbe,sv_dbe,brand_dbe,"Belgium (Dutch)")
final_ge <- dup_remove(kw_ge,sv_ge,brand_ge,"Germany")
final_swde <- dup_remove(kw_swde,sv_swde,brand_swde,"Switzerland (German)")
final_swfr <- dup_remove(kw_swfr,sv_swfr,brand_swfr,"Switzerland (French)")
final_sp <- dup_remove(kw_sp,sv_sp,brand_sp,"Spain")
final_fi <- dup_remove(kw_fi,sv_fi,brand_fi, "Finland")
final_fr <- dup_remove(kw_fr,sv_fr,brand_fr,"France")
final_nl <- dup_remove(kw_nl,sv_nl,brand_nl,"the Netherlands")
final_no <- dup_remove(kw_no,sv_no,brand_no,"Norway")
final_po <- dup_remove(kw_po,sv_po,brand_po,"Poland")
final_uk <- dup_remove(kw_uk,sv_uk,brand_uk,"United Kingdom")
final_dk <- dup_remove(kw_dk,sv_dk,brand_dk,"Denmark")
final_it <- dup_remove(kw_it,sv_it,brand_it,"Italy")
final_sw <- dup_remove(kw_sw,sv_sw,brand_sw,"Sweden")

library("xlsx")

# and the final step is to write down the results;
write.xlsx2(as.data.frame(final_at),file="types_of_keywords.xlsx",sheetName = "AT",row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_fbe),file="types_of_keywords.xlsx",sheetName = "BEfr",
            append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_dbe),file="types_of_keywords.xlsx",sheetName = "BEnl"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_swde),file="types_of_keywords.xlsx",sheetName = "CHde"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_swfr),file="types_of_keywords.xlsx",sheetName = "CHfr"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_ge),file="types_of_keywords.xlsx",sheetName = "DE"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_dk),file="types_of_keywords.xlsx",sheetName = "DK"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_sp),file="types_of_keywords.xlsx",sheetName = "ES"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_fi),file="types_of_keywords.xlsx",sheetName = "FI"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_fr),file="types_of_keywords.xlsx",sheetName = "FR"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_it),file="types_of_keywords.xlsx",sheetName = "IT"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_nl),file="types_of_keywords.xlsx",sheetName = "NL"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_no),file="types_of_keywords.xlsx",sheetName = "NO"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_po),file="types_of_keywords.xlsx",sheetName = "PL"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_sw),file="types_of_keywords.xlsx",sheetName = "SE"
            ,append = TRUE,row.names = FALSE)
Sys.sleep(0.1)

write.xlsx2(as.data.frame(final_uk),file="types_of_keywords.xlsx",sheetName = "UK"
            ,append = TRUE,row.names = FALSE)

write.csv(final_at,paste0(getwd(),"/data/","AT.csv"), row.names = FALSE)
write.csv(final_uk,paste0(getwd(),"/data/","UK.csv"), row.names = FALSE)
write.csv(final_sw,paste0(getwd(),"/data/","SE.csv"), row.names = FALSE)
write.csv(final_no,paste0(getwd(),"/data/","NO.csv"), row.names = FALSE)
write.csv(final_it,paste0(getwd(),"/data/","IT.csv"), row.names = FALSE)
write.csv(final_fr,paste0(getwd(),"/data/","FR.csv"), row.names = FALSE)
write.csv(final_fi,paste0(getwd(),"/data/","FI.csv"), row.names = FALSE)
write.csv(final_sp,paste0(getwd(),"/data/","ES.csv"), row.names = FALSE)
write.csv(final_dk,paste0(getwd(),"/data/","DK.csv"), row.names = FALSE)
write.csv(final_ge,paste0(getwd(),"/data/","DE.csv"), row.names = FALSE)
write.csv(final_fbe,paste0(getwd(),"/data/","BEfr.csv"), row.names = FALSE)
write.csv(final_dbe,paste0(getwd(),"/data/","BEnl.csv"), row.names = FALSE)
write.csv(final_nl,paste0(getwd(),"/data/","NL.csv"), row.names = FALSE)
write.csv(final_swde,paste0(getwd(),"/data/","CHde.csv"), row.names = FALSE)

write.csv(final_po,paste0(getwd(),"/data/","PL.csv"), row.names = FALSE)
write.csv(final_swfr,paste0(getwd(),"/data/","CHfr.csv"), row.names = FALSE)
