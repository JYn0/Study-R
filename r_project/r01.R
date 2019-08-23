library(dplyr)
library(sqldf)
library(ggplot2)
library(plotly)
library(rJava)
library(Rserve)
library(RJDBC);
library(DBI);

hive_lib <- '/home/centos/R/hivelib';
.jinit();
.jaddClassPath(dir(hive_lib,full.names = T));
.jclassPath();

drv=JDBC(driverClass='org.apache.hive.jdbc.HiveDriver',
         'hive-jdbc-1.0.1.jar');
conn=dbConnect(drv,"jdbc:hive2://70.12.114.211:10000",
               "root","111111");
animals=dbGetQuery(conn,"select * from animals");

dbDisconnect(conn);

#
f1 <- function(){
  ani.sex.result0 <- as.data.frame(table(animals$animals.sex));
  # View(ani.sex.result0)
  tmp <- sum(ani.sex.result0$Freq)
  ani.sex.result0$Freq <-  ani.sex.result0$Freq *100 /tmp

  
# png(paste(webpath,"/img/sex.png", sep=""),width=600,height=500);
#  a1 <- ggplot(data=ani.sex.result0,aes(x=Var1,y=Freq)) + geom_col();
#  dev.off();
  #interactive chart 
#  a2 <- ggplotly(a1);
  
  #interactive chart saved as html
#  htmlwidgets::saveWidget(as_widget(a2), paste(webpath,"/view/sex.html", sep="") )
  
  return(ani.sex.result0);
}


f3 <- function(){
  tmp3 <- animals[animals$animals.status %in% "END[안락사]",]
  tmp4 <- table(tmp3$animals.species)
  tmp4 <- as.data.frame(tmp4)
  colnames(tmp4) <- c("species","CNT")
  ani.status.result0 <- tmp4[tmp4$CNT != 0,]
  
  ani.status.result0 <- ani.status.result0[order(ani.status.result0$CNT,decreasing = T),]
  head(ani.status.result0,5)
  ggplot(data=head(ani.status.result0,5),aes(x=species,y=CNT)) +
    geom_col()
  
  c1<-ggplot(data=head(ani.status.result0,5),aes(x=species,y=CNT)) + geom_col()
  c2 <- ggplotly(c1);
  return(ani.status.result0);
}

f4 <-function(webpath){
  tmp5 <- (animals[substr(animals$animals.status,1,3) == "END",])
  tmp6 <- table(tmp5$animals.status)
  tmp7 <- sum(tmp6)
  tmp8 <- as.data.frame(tmp6)
  colnames(tmp8) <- c("status","CNT")
  tmp8$p <- tmp8$CNT * 100 / tmp7
    ani.endstatus.result0 <- tmp8[tmp8$status!="shelter",c(1,3)]

  lbls0 <- ani.endstatus.result0$status
  values0 <- ani.endstatus.result0$p
  
  #pie(as.numeric(values0), labels = lbls0, main=" Ended Abandoned Animal Status ")
  d1<-ggplot(data=head(ani.endstatus.result0,5),aes(x=status,y=p)) + geom_col()
  d2<- ggplotly(d1)
  htmlwidgets::saveWidget(as_widget(d2), paste(webpath,"view/endstatus.html", sep="") )

  return(ani.endstatus.result0)
}

f5 <- function(webpath){
  
  animals <- read.csv("/home/centos/R/rmini/animals_encoding.csv",
                 header=FALSE,
                 stringsAsFactors=FALSE,
                 na.strings="NA"
                 );
  colnames(animals) <- c("status","species","sex");
  ani.species.result <- sqldf('
                        select "species", count(*) as num
                        from animals
                        group by "species"
                        having count(*) > 150
                      ');
  
  ani.etc.result <- sqldf('
                        select count(*) as num
                        from animals
                        group by "species"
                        having count(*) <= 150
                      ');
  
  ani.etc.count <- sum(ani.etc.result$num)
  
  ani.species.result$species <- as.character(ani.species.result$species)
  
  ani.species.result <- rbind(ani.species.result, c("other", as.numeric(ani.etc.count)))
  
  e1<-ggplot(data=head(ani.species.result,5),aes(x=species,y=num)) + geom_col()
  e2<- ggplotly(e1)
  #htmlwidgets::saveWidget(as_widget(e2), paste(webpath,"/view/species.html", sep="") )
  
  lbls <- ani.species.result$species
  values <- ani.species.result$num
 
   png(paste(webpath,"img/Piespecies.png", sep=""),width=600,height=500);
  # png("Piespecies.png",width=600,height=500);
  pie(as.numeric(values), labels = lbls, main="Status of Abandont Animal per species")
  dev.off();
  return(ani.species.result)
}


