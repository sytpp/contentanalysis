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
load(DF)

## OUTLET
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*THE DAILY TELEGRAPH.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Telegraph"
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*MAIL.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Daily Mail"
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*GUARDIAN.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[UK] The Guardian"
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*ANALYST.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The Analyst"
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*INQUIRER.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The Inquirer"
DF[unlist(lapply(DF$outlet, function(x){(regexpr(".*THE NEWS.*", toupper(x), perl=TRUE)[1] >= 0)})),"PUBL"] <- "[LI] The NEWS"
DF$PUBL <- as.factor(DF$PUBL)
summary(DF$PUBL)

## COUNTRY
DF$COUNTRY <- ifelse(DF$PUBL=="[LI] The Analyst"|DF$PUBL=="[LI] The Inquirer"|DF$PUBL=="[LI] The NEWS","LI","UK")
DF$COUNTRY <- as.factor(DF$COUNTRY)
summary(DF$COUNTRY)

## DATE
DF$PUBDATE <- as.Date(DF[,"date"], format="%B %d, %Y")
DF$PUBWEEK <- format(DF$PUBDATE, format="%U")

#######################################
######################### TEXT ANALYSIS
library(stringr)

searchterms <- (read.table("searchterms.txt",sep=";"))$V1
searchterms
head(DF[,-4])

for(i in 1:length(searchterms)){
  searchterm = searchterms[i];
  print (searchterm);
  DF[,paste("count",sub("\\s","",tolower(searchterm)),sep="_")] <- str_count(tolower(DF$text), tolower(searchterm));
}

## some sanity tests
as.data.frame(apply(DF[,-4], 2, max))
DF[DF$count_liberia==58,c(1:3)]

write.table(DF[,-4], file="articles_all.txt", sep="\t", quote=F, row.names=F, col.names=T)

## --------------------------------------------------------------------
## manual analysis
## --------------------------------------------------------------------

## identify the articles which did not have any mention any of the case-specific terms
DF$all <- apply(DF[,c(15:85)],1,sum)
DF_sub <- DF[DF$all>0,]
dim(DF_sub)[1]

## now assign the subset of articles to people in the group randomly
a <- dim(DF_sub)[1]
names <- c("Matt", "Kevin","Caitlin","Fathima","Richard","Jocelyn",
           "Ines","Lindsay", "Sara", "Peter", "Fiona")

contentA_1 <- data.frame(articleNr = seq(1,a,by=1), reviewer_01 = c(sample( rep(names,(a)/length(names))), names[1:((a)%%length(names))]))
DF_manual_1 <- cbind.data.frame(contentA_1, DF_sub[,-4])
write.table(DF_manual_1, file="articles_subset_1.txt", sep="\t", quote=F, row.names=F, col.names=T)


contentA_rev <- data.frame(articleNr = seq(1,a,by=2), reviewer_02 = c(sample( rep(names,(a/2)/length(names))), names[1:((a/2)%%length(names))]))
DF_manual_rev <- cbind.data.frame(contentA_rev, DF_sub[seq(1,a,by=2),-4])
write.table(DF_manual_rev, file="articles_subset_rev.txt", sep="\t", quote=F, row.names=F, col.names=T)






colLI <- c("#8c2d04","#ec7014","#fec44f")
colUK <- c("#8c96c6","#88419d","#2171b5")
library(ggplot2)
ggplot(DF, aes(x=PUBWEEK, y=..count.., color=PUBL)) + 
  geom_point(stat="bin", size=4) + 
  geom_line(aes(x=PUBWEEK, y=..count..,group=PUBL, color=PUBL),stat="bin", position="identity") +
  scale_color_manual(values=c(colLI,colUK)) +
#  facet_wrap(~COUNTRY, nrow=2) +
  ylab("") + xlab("Publication calendar week")




ggplot(ddply(DF, .(PUBL,PUBWEEK), summarize, count_var=mean(count_sierraleone)), 
       aes(x=as.numeric(PUBWEEK), y=count_var, color=PUBL)) + 
  geom_point(stat="identity", size=4) +
  geom_line(stat="identity", aes(x=as.numeric(PUBWEEK), y=count_var, color=PUBL)) +
  scale_color_manual(values=c(colLI,colUK)) +
  ylab("") + xlab("Publication calendar week")




