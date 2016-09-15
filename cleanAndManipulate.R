cleanAndManipulate <- function(raw,logit=FALSE,clean=TRUE,aggrPlot=FALSE){
  raw$Cabin[raw$Cabin==""] <- NA
  
  
  if(aggrPlot==TRUE){ 
    library(VIM)
    aggr_plot <- aggr(raw, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(raw), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    print(aggr_plot)
  }
  
raw$familySize <- raw$Parch + raw$SibSp

#title
raw$title <- NA
for(string in c("master","mr","miss","mrs")) raw$title[grepl(string,raw$Name,ignore.case=T)] <- string
raw$title[is.na(raw$title)] <- ifelse(raw$Sex[is.na(raw$title)]=="female","mrs","mr")
raw$title <- as.factor(raw$title)

require(caret)
raw <- cbind(raw,predict(dummyVars(~title, data = raw), newdata = raw))

raw$child <- 0;raw$child[raw$Age<=10 | (raw$Age <= 15 & raw$Parch>0) | raw$SibSp > 1 | raw$title=="master"] <- 1

raw$ageMissing <- 0
raw$ageMissing[is.na(raw$Age)] <- 1

#Pclass
raw$Pclass <- as.factor(raw$Pclass);raw <- cbind(raw,predict(dummyVars(~Pclass, data = raw), newdata = raw))
#Embarked
raw <- raw[raw$Embarked!="",];raw$Embarked<-droplevels((raw$Embarked))
raw <- cbind(raw,predict(dummyVars(~Embarked, data = raw), newdata = raw))
#cabin
raw$cabin <- 0
raw$cabin[is.na(raw$Cabin)] <- 1
raw <- raw[,names(raw)!="Cabin"]
#Ticket
raw$Ticket_numeric <- as.numeric(gsub("[^0-9]","",raw$Ticket))
raw$Ticket_group[raw$Ticket_numeric < 300000] <- "lowTicketNumber"
raw$Ticket_group[raw$Ticket_numeric >= 300000] <- "highTicketNumber"
raw <- cbind(raw,predict(dummyVars(~Ticket_group, data = raw), newdata = raw))

library(dplyr)
raw<-tbl_df(raw)
raw<-raw %>%
  dplyr::select(-Ticket_numeric,-Sex,-title.master,-title.miss) %>%
  dplyr::select(Survived,child,contains("title"),contains("Pclass"),cabin,contains("Embarked"),contains("group"),Fare,Parch,familySize,ageMissing) %>%
  na.omit() %>%
  mutate_if(function(col) ifelse(is.factor(col),FALSE,max(col)==1 & min(col)==0),as.factor)
raw$Ticket_group <- as.factor(raw$Ticket_group)
if(clean==TRUE){ 
  if(logit==FALSE){
    clean <- list() 
    clean$predictors <- raw %>% dplyr::select(-Survived,-contains("Embarked."),-title,-Pclass) 
    clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
    return(clean)
  } else {
    raw <- raw %>%
      dplyr::select(-title.mrs,-Pclass.3,-Embarked.S,-Ticket_grouphighTicketNumber,-title,-Pclass,-Ticket_group)
    clean <- list() 
    clean$predictors <- raw %>% dplyr::select(-Survived,-Embarked) 
    clean$Survived <- raw %>% dplyr::select(Survived) %>% mutate_all(as.factor)
    return(clean)
  }
} else {
return(raw)}
}


f2n <- function(fac) as.numeric(as.character(fac))
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
grid <- 10^seq(10,-2, length=100)#possible values for lambda
