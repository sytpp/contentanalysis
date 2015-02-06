load("DF_work.RData")

## --------------------------------------------------------------------
## set up stuff for manual analysis
## --------------------------------------------------------------------

## identify the articles which did not have any mention any of the case-specific terms
DF_work$all <- apply(DF_work[,c(15:85)],1,sum)
DF_work_sub <- DF_work[DF_work$all>0,]
dim(DF_work_sub)[1]

## now assign the subset of articles to people in the group randomly
a <- dim(DF_work_sub)[1]
names <- c("Matt", "Kevin","Caitlin","Fathima","Richard","Jocelyn",
           "Ines","Lindsay", "Sara", "Peter", "Fiona")

contentA_1 <- data.frame(articleNr = seq(1,a,by=1), reviewer_01 = c(sample( rep(names,(a)/length(names))), names[1:((a)%%length(names))]))
DF_work_manual_1 <- cbind.data.frame(contentA_1, DF_work_sub[,-4])
write.table(DF_work_manual_1, file="articles_subset_1.txt", sep="\t", quote=F, row.names=F, col.names=T)


contentA_rev <- data.frame(articleNr = seq(1,a,by=2), reviewer_02 = c(sample( rep(names,(a/2)/length(names))), names[1:((a/2)%%length(names))]))
DF_work_manual_rev <- cbind.data.frame(contentA_rev, DF_work_sub[seq(1,a,by=2),-4])

write.table(DF_work_manual_rev, file="articles_subset_rev.txt", sep="\t", quote=F, row.names=F, col.names=T)

