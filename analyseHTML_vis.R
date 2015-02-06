load("DF_work.RData")
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)

## identify the articles which did not have any mention any of the case-specific terms
DF_work1 <- DF_work
DF_work1[,c(9:85)] <- ifelse(DF_work[,c(9:85)]>0, 1, 0) ## if it mentions one of the cases at least once, set value=1
DF_work1$all <- apply(DF_work[,c(15:85)],1,sum)         ## all = sum = how many of the terms were mentioned
DF_work1$yn <- ifelse(DF_work1$all>0, 1, 0)             ## yn = if one of the terms was mentioned (all>0)
DF_work1$yn <- factor(DF_work1$yn, levels=c(0,1), labels=c("general Ebola reporting","mention a 'Western case'"))

## which terms were mentioned most?
df <- cbind.data.frame(names(DF_work1[,c(15:85)]),as.data.frame(apply(DF_work1[,c(15:85)], 2, sum)))
df[order(df[,2]),]

## visualise the results of the autmated text analysis
DF_vis <- DF_work1
DF_vis$weekday <- weekdays(as.Date(DF_vis$PUBDATE))

colLI <- c("#8c2d04","#ec7014","#fec44f")
colUK <- c("#8c96c6","#88419d","#2171b5")

#################################################################################################
## Number of Ebola articles per week
#################################################################################################
DF_vis$MONDATE <- as.Date(paste("1",DF_vis$PUBWEEK,"2014",sep="."),format("%w.%W.%Y"))

ggplot(DF_vis, aes(x=MONDATE, y=..count.., color=PUBL)) + 
  geom_point(stat="bin", size=4) + 
  geom_line(aes(x=MONDATE, y=..count..,group=PUBL, color=PUBL),stat="bin", position="identity") +
  scale_color_manual(values=c(colLI,colUK)) +
  #facet_wrap(~COUNTRY, nrow=2) +
  ylab("article count / week") + xlab("Publication calendar week") +
  theme(text=element_text(size=20), legend.position="top") +
  guides(colour = guide_legend(nrow = 2, byrow = T, title=NULL)) +
  theme_bw() + 
  theme(text=element_text(size=20), legend.position="top", 
        panel.border = element_blank(), legend.key = element_blank(),
        panel.grid.major = element_line(color="black")) 


#################################################################################################
## number of Western-Case articles per outlet
#################################################################################################

ggplot(DF_vis, aes(x=PUBL, y=..count.., fill=PUBL, color=as.factor(yn), alpha=as.factor(yn))) + geom_bar(stat="bin") +
  coord_flip() + 
  labs(y="Number of articles in timeframe", x="", alpha="", color="") +
  scale_fill_manual(values=c(colLI,colUK), guide = FALSE) +
  scale_color_manual(values=c(NA, "black")) +
  scale_alpha_discrete(range = c(0.65, 1)) +
  theme_bw() + 
  theme(text=element_text(size=20), legend.position="top", 
        panel.border = element_blank(), legend.key = element_blank(),
        panel.grid.major = element_line(color="black")) 


#################################################################################################
## ration of Westen-Case articles per outlet, over time   
#################################################################################################

z <- ddply(DF_vis, c("PUBWEEK", "PUBL"), summarise, 
      y=sum(yn=="mention a 'Western case'")/
        (sum(yn=="general Ebola reporting")+sum(yn=="mention a 'Western case'")) )
z$DATE <- as.Date(paste("1",z$PUBWEEK,"2014",sep="."),format("%w.%W.%Y"))

rect_left <- as.Date(paste("1",c(30, 39),"2014",sep="."),format("%w.%W.%Y"))
rect_right <- as.Date(paste("1",c(35, 42),"2014",sep="."),format("%w.%W.%Y"))
rectangles <- data.frame(
  xmin = rect_left,
  xmax = rect_right,
  ymin = 0,
  ymax = 1
)

ggplot(z) + 
  geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill='gray80', alpha=0.7) +
  geom_point(data=z, stat="identity", size=4, aes(DATE, y=y, color=PUBL)) +
  geom_line(data=z, stat="identity", aes(x=DATE, y=y, color=PUBL)) +
  scale_color_manual(values=c(colLI,colUK)) +
  scale_y_continuous(labels = percent) +
  ylab("Western case articles") + xlab("") +
  guides(colour = guide_legend(nrow = 2, byrow = T, title=NULL)) +
  theme_bw() + 
  theme(text=element_text(size=20), legend.position="top", 
        panel.border = element_blank(), legend.key = element_blank(),
        panel.grid.major = element_line(color="black")) 


#################################################################################################
##
#################################################################################################

DF_work2 <- DF_work1[DF_work1$yn=="mention a 'Western case'",c(5,6,8,15,22,26,30,33,34,38,43,44,45,48,55,56,59,80,87)]
 
agg_melt <- (melt(DF_work2[,c(3,4:18)]))
agg_melt$variable2 <- factor(agg_melt$variable, levels=levels(agg_melt$variable), labels=
                               c("Teresa Romero","Thomas Eric Duncan","Amber Vinson",       
                                 "Nina Pham","Kent Brantly","Craig Spencer",      
                                 "William Pooley","Nancy Writebol","Rick Sacra",         
                                 "Ashoka Mukpo","Martin Salia","Ian Crozier",       
                                 "Miguel Pajares","Manuel García Viejo","Timo Wolf"))  

ggplot(agg_melt[agg_melt$value==1,], aes(PUBWEEK, fill=variable2)) + 
  geom_bar(stat="bin", binwidth=10) +
  scale_fill_brewer(palette="Paired") +
  labs(x="Calendar week of Publication", y="Number of articles", fill="Mentioning...") +
  theme_bw() + 
  theme(text=element_text(size=20), legend.position="right", 
        panel.border = element_blank(), legend.key = element_blank(),
        panel.grid.major = element_line(color="black")) 





ggplot(ddply(DF_vis, .(PUBL,PUBWEEK), summarize, count_var=mean(count_ebolavictim)), 
       aes(x=as.numeric(PUBWEEK), y=count_var, color=PUBL)) + 
  geom_point(stat="identity", size=4) +
  geom_line(stat="identity", aes(x=as.numeric(PUBWEEK), y=count_var, color=PUBL)) +
  scale_color_manual(values=c(colLI,colUK)) +
  ylab("mention of 'ebola victim'") + xlab("Publication calendar week") +
  theme(text=element_text(size=20), legend.position="top") +
  guides(colour = guide_legend(nrow = 2, byrow = T, title=NULL))


ggplot(as.data.frame(table(DF_vis$PUBL)), aes(Var1, Freq, fill=Var1)) + geom_bar(stat="identity") +
  scale_fill_manual(values=c(colLI,colUK)) +
  coord_flip() +
  theme(text=element_text(size=20), legend.position="none") +
  xlab("") + ylab("number of articles on 'ebola'")
  
  
  

