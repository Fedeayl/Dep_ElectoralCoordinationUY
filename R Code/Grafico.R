##### Gráficos #####


# Load packages
library(ggplot2)
library(ggthemes)
library(patchwork) #Para ploteo conjunto

# Import Data

Data <- read.csv2(here::here("Data", "Elect_Coord_Dataset.csv"))


# Gráfico de dispersión con boxlpots - NEC general
G1 <- ggplot(Data, aes(x=as.factor(Año), y=Nro_Efectivo_Candidatos))+
        geom_boxplot(alpha = 0.1, outlier.alpha = 0, color="gray60")+
        geom_point(color="gray30", position = "jitter", size=0.8)+
        facet_wrap(~as.factor(Año), scales = "free_x",nrow = 1,strip.position = "bottom")+
        labs(title = "",
             subtitle = "",
             caption = "")+
        xlab("")+
        ylab("NEC")+
        geom_hline(size=.8, mapping= aes(yintercept=4), alpha=0.8)+
        theme_calc()+
        theme(text=element_text(size=12, family="Times New Roman"))+
        theme(axis.text.x=element_blank())
G1

# Gráficos de barras por partido
# Tablas de resumen
DFFA <- doBy::summary_by(Data, NECFA~ Año, FUN= c(mean,sd))
DFPN <- doBy::summary_by(Data, NECPN~ Año, FUN= c(mean,sd))
DFPC <- doBy::summary_by(Data, NECPC~ Año, FUN= c(mean,sd))

names(DFFA) <- c("Año", "Media", "DEst")
names(DFPN) <- c("Año", "Media", "DEst")
names(DFPC) <- c("Año", "Media", "DEst")


GFA <- ggplot(DFFA, aes(x=as.factor(Año), y=Media)) + 
        geom_bar(stat="identity", position=position_dodge(), fill="grey70") +
        scale_fill_manual(values=c("gray40"))+
        ylim(0,4) +
        geom_point(data = Data, mapping = aes(x=as.factor(Año), y=NECFA), 
                   color="gray40", position = "jitter", alpha=.8, size =.5)+
        geom_errorbar(aes(ymin = Media - DEst, ymax = Media + DEst), width = 0.15, position = position_dodge(0.9))+
        geom_hline(size=.7, mapping= aes(yintercept=2), alpha=0.6)+
        labs(x = "", y = "",
             title = "", 
             subtitle = "Frente Amplio")+
        theme_calc()+
        theme(text=element_text(size=10, family="Times New Roman"))

GPN <- ggplot(DFPN, aes(x=as.factor(Año), y=Media)) + 
        geom_bar(stat="identity", position=position_dodge(), fill="grey70") +
        scale_fill_manual(values=c("gray40"))+
        ylim(0,5) +
        geom_point(data = Data, mapping = aes(x=as.factor(Año), y=NECPN), 
                   color="gray40", position = "jitter", alpha=.8, size =.5)+
        geom_errorbar(aes(ymin = Media - DEst, ymax = Media + DEst), width = 0.15, position = position_dodge(0.9))+
        geom_hline(size=.7, mapping= aes(yintercept=2), alpha=0.6)+
        labs(x = "", y = "NEC",
             title = "", 
             subtitle = "Partido Nacional")+
        theme_calc()+
        theme(text=element_text(size=10, family="Times New Roman"))

GPC <- ggplot(DFPC, aes(x=as.factor(Año), y=Media)) + 
        geom_bar(stat="identity", position=position_dodge(), fill="grey70") +
        scale_fill_manual(values=c("gray40"))+
        ylim(0,6) +
        geom_point(data = Data, mapping = aes(x=as.factor(Año), y=NECPC), 
                   color="gray40", position = "jitter", alpha=.8, size =.5)+
        geom_errorbar(aes(ymin = Media - DEst, ymax = Media + DEst), width = 0.15, position = position_dodge(0.9))+
        geom_hline(size=.7, mapping= aes(yintercept=2), alpha=0.6)+
        labs(x = "Elección", y = "",
             title = "", 
             subtitle = "PartidoColorado")+
        theme_calc()+
        theme(text=element_text(size=10, family="Times New Roman"))


GFA / GPN / GPC



jpeg(filename = "Figures/NEC_sistema.jpg", 
     width = 2000, height = 2200, res = 300)

G1

dev.off()



jpeg(filename = "Figures/NEC_partidos.jpg", 
     width = 2000, height = 2200, res = 300)

GFA / GPN / GPC
dev.off()

citation("ggplot2")
