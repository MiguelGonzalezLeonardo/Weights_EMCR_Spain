
setwd("C:\\Users\\Miguel González\\OneDrive - El Colegio de México A.C\\Drive\\Publicaciones___\\EMCR_EVR_EM\\EnglishV")

library(dplyr)
library(ggplot2)
library(viridis)


### DATA NUTS 2 ###

data_ccaa <- read.csv('Fig2_MigraDif_NUTS2.csv')

data_ccaa$NUTS <- "NUTS 2"

data_ccaa <- data_ccaa[,-1]


### DATA NUTS 3 ###

data_prov <- read.csv('Fig2_MigraDif_NUTS3.csv')

data_prov$NUTS <- "NUTS 3"

data_prov <- data_prov[,-1]


#### JOIN DATA ###

data <- rbind (data_ccaa, data_prov)


#### ORDER ###

data$move<- factor(data$move, levels=c("Out-migration",
                                       "In-migration"))

data$source <- factor(data$source, levels=c("EVR",
                                            "EM"))

data$type <- factor(data$type, levels=c("Internal migration",
                                        "International migration"))


###################
###### PLOTS ######
###################

bmp(file="Fig2_MigraDif.bmp", width=9, height=7, units='in', res=300)

ggplot(data, aes(x=move, y=value, fill=move)) + 
  
  facet_grid(NUTS ~ source + type)+
  
  geom_boxplot(width=0.5, outlier.color=NA) +

  geom_jitter(position=position_jitter(0.2), size=0.75, alpha=0.5, pch=21)+
  
  scale_fill_viridis(discrete=TRUE, alpha=0.5) +
  
  scale_y_continuous(limits=c(-50, 233), breaks=seq(-50,200,50))+
  
  geom_hline(yintercept=0, linewidth=0.5, colour="grey20", linetype="dashed")+
  
  labs(x=NULL,y="% difference") +
  
  # NUTS 2 labels #
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 2",
                        type=="Internal migration",
                        source=="EVR"),
            aes(x=move, y=median(value)+10,
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 2",
                        type=="Internal migration",
                        source=="EVR"),
            aes(x=move, y=median(value)-10,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 2",
                        type=="International migration",
                        source=="EVR"),
            aes(x=0.6, y=median(value)+2,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 2",
                        type=="International migration",
                        source=="EVR"),
            aes(x=2.4, y=median(value)+2,   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 2",
                        type=="Internal migration",
                        source=="EM"),
            aes(x=0.6, y=median(value)-1, 
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 2",
                        type=="Internal migration",
                        source=="EM"),
            aes(x=2.4, y=median(value),   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 2",
                        type=="International migration",
                        source=="EM"),
            aes(x=0.6, y=median(value)+1,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 2",
                        type=="International migration",
                        source=="EM"),
            aes(x=2.4, y=median(value)+1,   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  # NUTS 3 labels #
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 3",
                        type=="Internal migration",
                        source=="EVR"),
            aes(x=move, y=median(value)+10,
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 3",
                        type=="Internal migration",
                        source=="EVR"),
            aes(x=move, y=median(value)-10,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 3",
                        type=="International migration",
                        source=="EVR"),
            aes(x=0.6, y=median(value)+2,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 3",
                        type=="International migration",
                        source=="EVR"),
            aes(x=2.4, y=median(value)+2,   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 3",
                        type=="Internal migration",
                        source=="EM"),
            aes(x=0.6, y=median(value)-1, 
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 3",
                        type=="Internal migration",
                        source=="EM"),
            aes(x=2.4, y=median(value),   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="Out-migration",
                        NUTS=="NUTS 3",
                        type=="International migration",
                        source=="EM"),
            aes(x=0.6, y=median(value)+1,  
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  geom_text(data=filter(data, move=="In-migration",
                        NUTS=="NUTS 3",
                        type=="International migration",
                        source=="EM"),
            aes(x=2.4, y=median(value)+1,   
                label=round(median(value),1)),
            color="black",
            size=2.5,
            fontface="bold") +
  
  theme_bw()+
  
  theme(text=element_text(size=13),
        
        strip.background=element_rect(color="grey15", linewidth=0.35),
        panel.border=element_rect(color="grey15", linewidth=0.35),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        
        axis.title.y=element_text(vjust=0.5, size=11),
        axis.text.x=element_text(colour="grey10", size=11, angle=90, vjust=0.5, hjust=1),
        axis.text.y=element_text(colour="grey10", size=11, hjust=1, vjust=0.5),
        axis.ticks=element_line(color="grey15", linewidth=0.5),
        
        legend.title=element_blank (),
        legend.text=element_blank (),
        legend.position="none")

dev.off()
