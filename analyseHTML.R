setwd("~/Google Drive/_CITY/JOM834_ScienceReporting/ContentAnalysis/Content Analysis II/part I - computational/contentanalysis")
library(XML)

DF = data.frame( outlet = character(), date = character(), title = character(), text = character())

for(i in 1:length(list.files("RAW/"))){
  file = list.files("RAW/")[i];
  print (file);

  tp <- htmlTreeParse(paste("RAW/",file,sep=""), useInternal=T)
  tpu <- unlist(xpathApply(tp, '//div', xmlValue))
  tpuNOs = gsub('\\n', ' ', tpu)                          # Replace all \n by spaces
  
  for(x in 1:length(tpuNOs)){
    if(regexpr("\\d+\\s\\w+\\s\\d+\\sDOCUMENTS", tpuNOs[x], perl=TRUE)[1] >= 0){
      outlet = tpuNOs[x+1];
      date = tpuNOs[x+2];
      title = tpuNOs[x+3];
    }
    else if(regexpr("LENGTH:\\s\\d+\\s\\w+", tpuNOs[x], perl=TRUE)[1] >= 0) {
      text = ifelse(outlet=="The Analyst", tpuNOs[x+2], tpuNOs[x+1]);
      df = data.frame(outlet=outlet, date=date, title=title, text=text);
      DF = rbind.data.frame(DF, df)  
    }
  }
  
  print("....done.");

}

save(DF, file="DF.RData")

## --------------------------------------------------------------------
## automated analysis
## --------------------------------------------------------------------
load("DF.RData")
DF_work <- DF

## OUTLET
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*THE DAILY TELEGRAPH.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Telegraph"
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*MAIL.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Daily Mail"
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*GUARDIAN.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Guardian"
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*ANALYST.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The Analyst"
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*INQUIRER.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The Inquirer"
DF_work[unlist(lapply(DF_work$outlet, function(x){(regexpr(".*THE NEWS.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The NEWS"
DF_work$PUBL <- as.factor(DF_work$PUBL)
summary(DF_work$PUBL)

## COUNTRY
DF_work$COUNTRY <- ifelse(DF_work$PUBL=="[LI] The Analyst"|DF_work$PUBL=="[LI] The Inquirer"|DF_work$PUBL=="[LI] The NEWS","LI","UK")
DF_work$COUNTRY <- as.factor(DF_work$COUNTRY)
summary(DF_work$COUNTRY)

## DATE
DF_work$PUBDATE <- as.Date(DF_work[,"date"], format="%B %d, %Y")
DF_work$PUBWEEK <- format(DF_work$PUBDATE, format="%U")

#######################################
######################### TEXT ANALYSIS
library(stringr)

searchterms <- (read.table("searchterms.txt",sep=";"))$V1
searchterms
head(DF_work[,-4])

for(i in 1:length(searchterms)){
  searchterm = searchterms[i];
  print (searchterm);
  DF_work[,paste("count",sub("\\s","",tolower(searchterm)),sep="_")] <- str_count(tolower(DF_work$text), tolower(searchterm));
}

## some sanity tests
as.data.frame(apply(DF_work[,-4], 2, max))
DF_work[DF_work$count_liberia==58,c(1:3)]

write.table(DF_work[,-4], file="articles_all.txt", sep="\t", quote=F, row.names=F, col.names=T)

## --------------------------------------------------------------------
save(DF_work, file="DF_work.RData")



