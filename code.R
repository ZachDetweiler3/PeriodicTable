library(httr)
dd <- read.csv("elements.txt", sep=",", header=TRUE)

decompose <- function(x) {
        m <- gregexpr("([A-Z][a-z]?)(\\d*)", x, perl=T)
        dx <- Map(function(x, y) {
                ElementSymbol <- gsub("\\d","", x)
                cnt <- as.numeric(gsub("\\D","", x))
                cnt[is.na(cnt)]<-1
                cbind(Sym=y, as.data.frame(xtabs(cnt~ElementSymbol)))
        }, regmatches(x,m), x)
        do.call(rbind, dx)
}

G<-read.csv("NBS Thermodynamic Values for Metal Chlorides at Various Temperatures 1.csv")

library(ggplot2)
library(data.table)

#Merging elements.txt with NBS.csv
BPTNA<-merge(G,dd,by="ElementSymbol",all=T)
BPTNA<-data.table(BPTNA)
#Removing NA and div/0 values from table
BPTT<-BPTNA[!is.na(BPTNA$TC),]
BPTT<-BPTT[!is.na(BPTT$dGmCl),]
BPTT<-BPTT[BPTT$dGmCl!="#DIV/0!",]
##TEMPERATURE SETTING
BPT<-BPTT[BPTT$TK=="1223",]
BPT$dGmClNo<-as.numeric(as.character(BPT$dGmCl))
##ELEMENTAL REFERENCE SPECIES (i.e. SUBSTRATE SPECIES)
BPT$ELdGmCl<-BPT$dGmClNo-min(BPT[ElementSymbol=="Fe",]$dGmClNo)


#For each BPT$ElementSymbol calculate minimum BPT$dGmCl
StBPT<-BPT[ , .SD[which.min(ELdGmCl)], by = ElementSymbol]

colors<-colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(68)
N <- nlevels(StBPT$ELdGmCl)

ggplot(StBPT, aes(Column, -Row)) + 
        geom_tile(aes(fill=cut(ELdGmCl, c(-Inf, -1, 1, Inf))),color="black") +
        scale_fill_manual(name = "dGmCl of EOI",
                          values = c("(-Inf,-1]" = "green","(-1,1]" = "yellow","(1, Inf]" = "purple"),
                          labels = c("Deposit on EOI", "EOI", "EOI will move to"))+
        geom_text(aes(label=ElementSymbol))+
        geom_text(aes(label=format(ELdGmCl/1000,digits=2)),vjust=3,hjust=.5,size=1.8)+
        geom_text(aes(label=Species),vjust=-2,hjust=.5,size=1.8)+
        labs(x="",y="")+
        theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank())
