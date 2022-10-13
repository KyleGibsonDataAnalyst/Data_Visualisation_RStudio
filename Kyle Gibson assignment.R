#### Graphs R us assignment (Kyle Daniel Gibson 17965753)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggplotify)
library(gridGraphics)
library(gridBase)
library(reshape2)
library(dplyr)                                  # Loading installed Graphs R us packages 
library(raster)
library(ggmap)
library(ggthemes)
library(GGally)
library(viridis)
library(viridisLite)
library(colorRamps)
library(jpeg)
library(png)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(RStoolbox)
library(mapdata)
library(broom)
library(rmapshaper)
library(ggridges)
library(devtools)
library(gapminder)
library(ggpubr)
library(ggridges)
library(ggiraph)




setwd("C:\\AUT\\Graphs R us") # Set working directory
getwd()                                        # Checking working directory location is correct
cs <- read.csv("chilling_sensitivity.csv")     # Reading in datasets from my working directory
de <- read.csv("drought_elevation.csv")     

#### Data exploration of chilling sensitivity ####

str(cs)                                        # Checking overall dataframe type and variable type and amount 
head(cs)                                       # Viewing columns of first 6 rows
tail(cs)                                       # Viewing columns of last 6 rows
summary(cs)                                    # Viewing summary statistics 
View(cs)                                       # Complete view of dataset
plot(cs)                                       # Looking for obvious patterns between variables
dotchart(x = cs$photo, groups = cs$treat)      # Exploratory dot chart view
dotchart(x = cs$conc, groups = cs$photo)
dotchart(x = cs$photo, groups = cs$spec)
colo <- c("lightblue", "red2")
dotchart(x = cs$photo, groups = cs$treat,      # Exploring relationship between species, treatment and photosynthetic rate
         bg = colo[as.numeric(cs$spec)])
dotchart(x = cs$photo, groups = cs$conc,      # Exploring relationship between species, carbon dioxide concentration and photosynthetic rate
         bg = colo[as.numeric(cs$spec)])


op <- par(mar = rep(1.6, 4), oma = c(2, 2, 1, 1))           # Turning off margins and creating custom margins
layout(rbind(c(1, 2),                                     # Creating custom 4 plot layout
             c(3, 4)),
       heights = c(1, 1, 1, 1), widths = c(1, 1, 1, 1))
layout.show(4)                                            # Checking layout in plot window

par(op)


subspec <- subset(cs, spec == "Lolium perenne")       # subsetting dataframe to individual species including all replicas and all columns
subspec1 <- subset(cs, spec == "Holcus mollis")
subspec2 <- subset(cs, spec == "Dactylis glomerata")
subspec3 <- subset(cs, spec == "Cynosurus cristatus")

subspec <- droplevels(subspec)                           
subspec1 <- droplevels(subspec1)                             # Removing species rows that are not needed to clean sub-setted data 
subspec2 <- droplevels(subspec2)
subspec3 <- droplevels(subspec3)

plot(photo ~ conc, data = subspec)                      # Plotting each species to check scatter plot visually 
plot(photo ~ conc, data = subspec1)
plot(photo ~ conc, data = subspec2)
plot(photo ~ conc, data = subspec3)


#### Multi-panel scatter plot (Traditional graphics) ####

pdf("scatter.pdf", width = 7, height = 7)     # Exporting multi-panel plot
op <- par(mar = rep(1.6, 4), oma = c(2, 2, 1, 1))           # Turning off margins and creating custom margins
layout(rbind(c(1, 2),                                     # Creating custom 4 plot layout
             c(3, 4)),
       heights = c(1, 1, 1, 1), widths = c(1, 1, 1, 1))
                                            
par(op)

plot(subspec$conc, subspec$photo, pch = 21, col = "brown", bg = colo[as.numeric(subspec$treat)], ylab = NA, xlab = NA, ylim = c(0, 50)) # Plot concentration and photosynthetic rate for Lolium perenne with colour representing chilled and nonchilled
legend(x = 746, y = 8, legend = c("Nonchilled", "Chilled"), pch = c(22, 22),      # Adding legend to show control and treatment colours
       pt.bg = c("red2", "lightblue"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)
mtext(expression(paste("Photosynthetic rate (", mu, "mol m"^-2, " s"^-1, ")")), side = 2, line = 2.5, cex = 0.8)
mtext(expression(paste("CO"[2], " (ppm)")), side = 1, line = 2.75, cex = 0.8)     # Adding custom labels with correct symbols and font
mtext("Lolium perenne", font = 3, side = 3, line = 1.5, cex = 1)

plot(subspec1$conc, subspec1$photo, pch = 21, col = "brown", bg = colo[as.numeric(subspec1$treat)], ylab = NA, xlab = NA, ylim = c(0, 50)) # y limit set to allow better visual comparison between each plot
legend(x = 746, y = 8, legend = c("Nonchilled", "Chilled"), pch = c(22, 22),      
       pt.bg = c("red2", "lightblue"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)
mtext(expression(paste("Photosynthetic rate (", mu, "mol m"^-2, " s"^-1, ")")), side = 2, line = 2.5, cex = 0.8)            # Plots repeated for all species
mtext(expression(paste("CO"[2], " (ppm)")), side = 1, line = 2.75, cex = 0.8)     
mtext("Holcus mollis", font = 3, side = 3, line = 1.5, cex = 1)

plot(subspec2$conc, subspec2$photo, pch = 21, col = "brown", bg = colo[as.numeric(subspec2$treat)], ylab = NA, xlab = NA, ylim = c(0, 50)) # data point outline colour changed so each point is easily seen
legend(x = 746, y = 8, legend = c("Nonchilled", "Chilled"), pch = c(22, 22),      
       pt.bg = c("red2", "lightblue"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)
mtext(expression(paste("Photosynthetic rate (", mu, "mol m"^-2, " s"^-1, ")")), side = 2, line = 2.5, cex = 0.8)         
mtext(expression(paste("CO"[2], " (ppm)")), side = 1, line = 2.75, cex = 0.8)     
mtext("Dactylis glomerata", font = 3, side = 3, line = 1.5, cex = 1)

plot(subspec3$conc, subspec3$photo, pch = 21, col = "brown", bg = colo[as.numeric(subspec3$treat)], ylab = NA, xlab = NA, ylim = c(0, 50)) 
legend(x = 746, y = 8, legend = c("Nonchilled", "Chilled"), pch = c(22, 22),      
       pt.bg = c("red2", "lightblue"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)
mtext(expression(paste("Photosynthetic rate (", mu, "mol m"^-2, " s"^-1, ")")), side = 2, line = 2.5, cex = 0.8)
mtext(expression(paste("CO"[2], " (ppm)")), side = 1, line = 2.75, cex = 0.8)     
mtext("Cynosurus cristatus", font = 3, side = 3, line = 1.5, cex = 1)

dev.off() # Stop exporting


#### Multi-panel scatter plot (ggplot) ####


p1 <- ggplot(data = subspec, aes(x = conc, y = photo, color = treat)) + geom_point(size = 3, shape = 20)+
  scale_colour_manual(values=c("lightblue", "red2"))+ coord_cartesian(ylim = c(0, 50))+                       # Swapping point colours to 'chilled = blue' to make more sense to the reader, setting y axis limit to provide the same area for each plot
labs(x = expression(paste("CO"[2], " (ppm)")), y = expression(paste("Photosynthetic rate", " (", mu, "mol m"^-2, " s"^-1, ")")))+
theme_bw()+theme(panel.grid = element_blank(), legend.title = element_text(colour = "white"), plot.title = element_text(hjust = 0.5, face = "italic"), legend.background = element_rect(fill="transparent"))+      # Move plot title to center and change font to italic
 ggtitle("Lolium perenne")+ theme(legend.position = c(0.8, 0.2))

p2 <- ggplot(data = subspec1, aes(x = conc, y = photo, color = treat)) + geom_point(size = 3, shape = 20)+
  scale_colour_manual(values=c("lightblue", "red2"))+ coord_cartesian(ylim = c(0, 50))+                       
  labs(x = expression(paste("CO"[2], " (ppm)")), y = expression(paste("Photosynthetic rate", " (", mu, "mol m"^-2, " s"^-1, ")")))+
  theme_bw()+theme(panel.grid = element_blank(), legend.title = element_text(colour = "white"), plot.title = element_text(hjust = 0.5, face = "italic"), legend.background = element_rect(fill="transparent"))+      
  ggtitle("Holcus mollis")+ theme(legend.position = c(0.8, 0.2))
  
p3 <- ggplot(data = subspec2, aes(x = conc, y = photo, color = treat)) + geom_point(size = 3, shape = 20)+
  scale_colour_manual(values=c("lightblue", "red2"))+ coord_cartesian(ylim = c(0, 50))+                       
  labs(x = expression(paste("CO"[2], " (ppm)")), y = expression(paste("Photosynthetic rate", " (", mu, "mol m"^-2, " s"^-1, ")")))+
  theme_bw()+theme(panel.grid = element_blank(), legend.title = element_text(colour = "white"), plot.title = element_text(hjust = 0.5, face = "italic"), legend.background = element_rect(fill="transparent"))+      
  ggtitle("Dactylis glomerata")+ theme(legend.position = c(0.8, 0.2))

p4 <- ggplot(data = subspec3, aes(x = conc, y = photo, color = treat)) + geom_point(size = 3, shape = 20)+
  scale_colour_manual(values=c("lightblue", "red2"))+ coord_cartesian(ylim = c(0, 50))+                       
  labs(x = expression(paste("CO"[2], " (ppm)")), y = expression(paste("Photosynthetic rate", " (", mu, "mol m"^-2, " s"^-1, ")")))+
  theme_bw()+theme(panel.grid = element_blank(), legend.title = element_text(colour = "white"), plot.title = element_text(hjust = 0.5, face = "italic"), legend.background = element_rect(fill="transparent"))+      
  ggtitle("Cynosurus cristatus")+ theme(legend.position = c(0.8, 0.2))

pdf("scatterggplot.pdf", width = 7, height = 7)     # Exporting the below multi-panel plot
grid.arrange(p1, p2, p3, p4, ncol=2)   # Arranging my plots into grids to create a four panel plot (2 rows+2columns)
dev.off() # Stop exporting

#### Data exploration of drought elevation ####

str(de)                                        # Checking overall dataframe type and variable type and amount 
head(de)                                       # Viewing columns of first 6 rows
tail(de)                                       # Viewing columns of last 6 rows
summary(de)                                    # Viewing summary statistics 
View(de)                                       # Complete view of dataset
plot(de)                                       # View of all factors
se <- function(x, na.rm = FALSE)
{
  if(na.rm == TRUE)
  {
    sqrt(var(x, na.rm = T)/length(na.omit(x)))    # Creating standard error function
  }
  else
  {
    sqrt(var(x)/length(x))
  }
}



op <- par(mar = rep(0, 4), oma = c(4, 4, 2, 2))  # Changing the plot window margins 

## Create the layout
layout(rbind(c(1, 4, 2, 5, 3, 6),
             c(1, 1, 2, 2, 3, 3),      # Creating layout of 3 columns with one inset per column
             c(1, 1, 2, 2, 3, 3),
             c(1, 1, 2, 2, 3, 3)))

 layout.show(n = 6)
 par(op)  # Back to original parameters (plot margins default)
 
 de1 <- subset(de, elevation == "low")       # subsetting dataframe to individual elevations including all replicas and all columns
 de2 <- subset(de, elevation == "mid")
 de3 <- subset(de, elevation == "high")
 
 summary(de1)
 summary(de2)         # Checking rows and columns 
 summary(de3)
 
 de1 <- droplevels(de1)                           
 de2 <- droplevels(de2)                             # Removing elevation rows that are not needed to clean sub-setted data 
 de3 <- droplevels(de3)

 
de4 <- group_by(.data = de1, spec, treat) %>% summarise(ps_av = mean(height), pos = mean(height) + se(height), neg = mean(height) - se(height)) # Creating new dataset with average height and standard error for plotting
de4
summary(de4)  # Checking new dataset with averages
de5 <- group_by(.data = de2, spec, treat) %>% summarise(ps_av = mean(height), pos = mean(height) + se(height), neg = mean(height) - se(height)) 
de5
summary(de5)
de6 <- group_by(.data = de3, spec, treat) %>% summarise(ps_av = mean(height), pos = mean(height) + se(height), neg = mean(height) - se(height)) 
de6
summary(de6)

#### Multi-panel barplots with insets ####


pdf("barplot.pdf", width = 6, height = 8)        # Exporting multi-panel plot
op <- par(mar = rep(0, 4), oma = c(3, 6, 2, 4) + 0.1)  # Changing the plot window margins 
layout(rbind(c(1, 1, 1, 0, 4),
             c(1, 1, 1, 0, 1),
             c(0, 0, 0, 0, 0),   # Changed layout for better viewing
             c(2, 2, 2, 0, 5),
             c(2, 2, 2, 0, 2),
             c(0, 0, 0, 0, 0),
             c(3, 3, 3, 0, 6),
             c(3, 3, 3, 0, 3)))

#layout.show(n = 6)   # Layout shown



bp <- barplot(height = de4$ps_av, las = 1, axes = T, ylim = c(0, 45), xlim = c(0, 10), col = c("lightblue", "darkgrey"), main = " Low altitude", xpd = NA)
arrows(x0 = bp, y0 = de4$pos, x1 = bp, y1 = de4$neg, angle = 90, length = 0.04, code = 3)
mtext("Growth height (cm)", side = 2, line = 2.5, cex = 0.8)
text(x = 1.9, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 0.68, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)    
text(x = 3.1, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 4.3, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 5.5, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 6.7, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 1.2, y = -1, labels = "A. australis", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 3.6, y = -1, labels = "B. tawa", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 6, y = -1, labels = "P. totara", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
legend(x = 0, y = 40, legend = c("Control", "Rain exclusion"), pch = c(22, 22),      
       pt.bg = c("lightblue", "darkgrey"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)



de5 <- group_by(.data = de2, spec, treat) %>% summarise(ps_av = mean(height), pos = mean(height) + se(height), neg = mean(height) - se(height)) 
de5
summary(de5)

bp <- barplot(height = de5$ps_av, las = 1, axes = T, ylim = c(0, 45), xlim = c(0, 10), col = c("lightblue", "darkgrey"), main = "Medium altitude", xpd = NA)
arrows(x0 = bp, y0 = de5$pos, x1 = bp, y1 = de5$neg, angle = 90, length = 0.04, code = 3)
mtext("Growth height (cm)", side = 2, line = 2.5, cex = 0.8)
text(x = 1.9, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 0.68, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)    
text(x = 3.1, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 4.3, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 5.5, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 6.7, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 1.2, y = -1, labels = "A. australis", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 3.6, y = -1, labels = "B. tawa", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 6, y = -1, labels = "P. totara", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
legend(x = 0, y = 40, legend = c("Control", "Rain exclusion"), pch = c(22, 22),      
       pt.bg = c("lightblue", "darkgrey"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)


de6 <- group_by(.data = de3, spec, treat) %>% summarise(ps_av = mean(height), pos = mean(height) + se(height), neg = mean(height) - se(height)) 
de6
summary(de6)

bp <- barplot(height = de6$ps_av, las = 1, axes = T, ylim = c(0, 45), xlim = c(0, 10), col = c("lightblue", "darkgrey"), main = "High altitude", xpd = NA)
arrows(x0 = bp, y0 = de6$pos, x1 = bp, y1 = de6$neg, angle = 90, length = 0.04, code = 3)
mtext("Growth height (cm)", side = 2, line = 2.5, cex = 0.8)
text(x = 1.9, y = 3, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 0.68, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)    
text(x = 3.1, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 4.3, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 5.5, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 6.7, y = 6, labels = "n = 30", srt = 0, col = "red3", cex = 1.1)
text(x = 1.2, y = -1, labels = "A. australis", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 3.6, y = -1, labels = "B. tawa", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
text(x = 6, y = -1, labels = "P. totara", srt = 45, cex = 0.8, adj = c(1, 1), xpd = NA, font = 3)
legend(x = 0, y = 40, legend = c("Control", "Rain exclusion"), pch = c(22, 22),      
       pt.bg = c("lightblue", "darkgrey"), bty = "n", y.intersp = 0.9, pt.cex = 1, cex = 0.8)

#### Insets ####
                                                                          # Compute and add a probability density curve
hist(de1$height, col = "grey", main = NA, xlab = NA, las = 1, ylab = NA, freq = F, axes = F)
dens1 <- density(de1$height)
lines(dens1)

                                                                          # Compute and add a probability density curve
hist(de2$height, col = "grey", main = NA, xlab = NA, ylab = NA, axes = F, las = 1, freq = F)
dens2 <- density(de2$height)
lines(dens2)


                                                                          # Compute and add a probability density curve
hist(de3$height, col = "grey", main = NA, xlab = NA, ylab = NA, axes = F, las = 1, freq = F)
dens3 <- density(de3$height)
lines(dens3)

par(op)  # Back to original parameters (plot margins return to default)
dev.off() # Stop exporting

#### Multi-panel map ####

#### Exploration ####

aus <- getData(name = "GADM", country = "Australia", level = 1)
nrow(aus)
summary(aus)
unique(aus$NAME_1)
aus$NAME_1
aus1 <- aus[aus$NAME_1 %in% c( "New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia"), ] # Choosing the variables required for dataset and excluding the rest
aus1$NAME_1 # Dataset with states only


#### Map ####

cols <- colorRampPalette(colors = c("red", "green", "purple", "orange", "brown"))   # Creating colour function to use as a visual difference between each state

map2 <- (ggplot(data = aus, aes(x = long, y = lat, group = group, map_id = id)) +
  geom_polygon(aes(fill = id), size = 0.2) +                                              # Creating a map of Australia with state borders and state labels with the corresponding abbreviations
  coord_quickmap() + 
  guides(fill = F) + 
  scale_fill_manual(values = cols(n = 26)) +
annotate(geom = "text", x = 146.9211, y = -31.2532, label = "NSW", col = "yellow", size = 2) +
annotate(geom = "text", x = 132.5510, y = -19.4914, label = "NT", size = 2, col = "yellow") +     
annotate(geom = "text", x = 142.7028, y = -20.9176, label = "Qld", size = 2, col = "yellow") +
annotate(geom = "text", x = 136.2092, y = -30.0002, label = "SA", size = 2, col = "yellow") +
annotate(geom = "text", x = 147, y = -41.4545, label = "Tas", size = 2, col = "yellow") +
annotate(geom = "text", x = 144.7852, y = -36.3, label = "Vic", size = 2, col = "yellow") +
annotate(geom = "text", x = 121.6283, y = -27.6728, label = "WA", size =2, col = "yellow") +
theme_map())

map2  # Viewing map

#### Digital elevation model ####

alt.aus <- getData(name = "alt", country = "AUS") # Import Australian altitude data

## Compute the slope, aspect and other terrain characteristics from raster object

terr.aus1 <- terrain(x = alt.aus, opt = c("slope", "aspect"))


## Compute the hill shade from the slope and aspect layer 
hill.aus1 <- hillShade(slope = terr.aus1$slope, aspect = terr.aus1$aspect)


## Merge the two data sets
hill.aus2 <- merge(hill.aus1, terr.aus1, overlap = T) 
alt.aus

aoi <- extent(c(xmin = 112.8, xmax = 159.2, ymin = -54.9, ymax = -9.1)) 
# aoi = area of interest

## Use aoi as reference to crop the hill shade raster
hill.aus3 <- crop(x = hill.aus2, y = aoi)

## Same for the elevations
alt.aus3 <- crop(x = alt.aus, y = aoi)



## Colour palette for the DEM
library(ggplot2)
library(raster)
library(RColorBrewer)

schwarzwald <- colorRampPalette(c(
  # rgb(174, 239, 213, max = 255),        # Schwarzwald colour palette function created
  # rgb(175, 240, 211, max = 255),
  # rgb(176, 242, 208, max = 255),
  # rgb(176, 242, 202, max = 255),
  # rgb(177, 242, 196, max = 255),
  # rgb(176, 243, 190, max = 255),
  # rgb(176, 244, 186, max = 255),
  # rgb(178, 246, 181, max = 255),
  # rgb(181, 246, 178, max = 255),
  # rgb(186, 247, 178, max = 255),
  # rgb(192, 247, 178, max = 255),
  # rgb(198, 248, 178, max = 255),
  # rgb(204, 249, 178, max = 255),
  # rgb(210, 250, 177, max = 255),
  # rgb(217, 250, 178, max = 255),
  # rgb(224, 251, 178, max = 255),
  # rgb(231, 252, 178, max = 255),
  # rgb(238, 252, 179, max = 255),
  # rgb(245, 252, 179, max = 255),
  # rgb(250, 252, 178, max = 255),
  # rgb(248, 249, 172, max = 255),
  # rgb(238, 244, 162, max = 255),
  rgb(226, 240, 151, max = 255),
  rgb(213, 235, 140, max = 255),
  rgb(198, 228, 128, max = 255),
  rgb(184, 222, 118, max = 255),
  rgb(170, 216, 108, max = 255),
  rgb(154, 211, 98, max = 255),
  rgb(140, 205, 89, max = 255),
  rgb(125, 199, 82, max = 255),
  rgb(110, 194, 74, max = 255),
  rgb( 94, 188, 66, max = 255),
  rgb( 77, 182, 57, max = 255),
  rgb( 62, 176, 50, max = 255),
  rgb( 49, 171, 44, max = 255),
  rgb( 39, 165, 42, max = 255),
  rgb( 30, 160, 43, max = 255),
  rgb( 24, 154, 46, max = 255),
  rgb( 18, 148, 49, max = 255),
  rgb( 14, 142, 52, max = 255),
  rgb(  9, 137, 56, max = 255),
  rgb(  7, 132, 60, max = 255),
  rgb( 12, 130, 63, max = 255),
  rgb( 24, 130, 63, max = 255),
  rgb( 40, 132, 61, max = 255),
  rgb( 52, 136, 60, max = 255),
  rgb( 64, 140, 59, max = 255),
  rgb( 76, 142, 59, max = 255),
  rgb( 87, 146, 56, max = 255),
  rgb( 99, 148, 54, max = 255),
  rgb(110, 150, 52, max = 255),
  rgb(120, 154, 50, max = 255),
  rgb(128, 156, 48, max = 255),
  rgb(137, 160, 46, max = 255),
  rgb(147, 162, 43, max = 255),
  rgb(156, 164, 41, max = 255),
  rgb(166, 166, 39, max = 255),
  rgb(176, 170, 36, max = 255),
  rgb(187, 173, 34, max = 255),
  rgb(197, 176, 30, max = 255),
  rgb(207, 177, 28, max = 255),
  rgb(218, 179, 24, max = 255),
  rgb(228, 180, 20, max = 255),
  rgb(238, 182, 14, max = 255),
  rgb(246, 182, 8, max = 255),
  rgb(248, 176, 4, max = 255),
  rgb(244, 166, 2, max = 255),
  rgb(238, 155, 2, max = 255),
  rgb(232, 144, 2, max = 255),
  rgb(226, 132, 2, max = 255),
  rgb(220, 122, 2, max = 255),
  rgb(216, 111, 2, max = 255),
  rgb(211, 102, 2, max = 255),
  rgb(206, 92, 2, max = 255),
  rgb(200, 84, 2, max = 255),
  rgb(192, 74, 2, max = 255),
  rgb(186, 66, 2, max = 255),
  rgb(180, 58, 2, max = 255),
  rgb(174, 49, 2, max = 255),
  rgb(169, 42, 2, max = 255),
  rgb(163, 36, 2, max = 255),
  rgb(157, 30, 2, max = 255),
  rgb(151, 23, 2, max = 255),
  rgb(146, 18, 1, max = 255),
  rgb(141, 14, 1, max = 255),
  rgb(135, 8, 0, max = 255),
  rgb(130, 5, 0, max = 255),
  rgb(125, 4, 0, max = 255),
  rgb(122, 8, 2, max = 255),
  rgb(119, 13, 2, max = 255),
  rgb(118, 16, 2, max = 255),
  rgb(117, 18, 4, max = 255),
  rgb(117, 20, 4, max = 255),
  rgb(117, 21, 4, max = 255),
  rgb(116, 22, 4, max = 255),
  rgb(116, 24, 5, max = 255),
  rgb(114, 26, 6, max = 255),
  rgb(114, 29, 6, max = 255),
  rgb(112, 31, 7, max = 255),
  rgb(111, 33, 8, max = 255),
  rgb(110, 35, 8, max = 255),
  rgb(110, 36, 8, max = 255),
  rgb(109, 38, 9, max = 255),
  rgb(108, 40, 10, max = 255),
  rgb(108, 40, 10, max = 255),
  rgb(108, 42, 10, max = 255),
  rgb(107, 44, 11, max = 255),
  rgb(106, 44, 12, max = 255),
  rgb(106, 46, 12, max = 255),
  rgb(107, 48, 14, max = 255),
  rgb(110, 52, 18, max = 255),
  rgb(113, 57, 23, max = 255),
  rgb(116, 62, 28, max = 255),
  rgb(118, 66, 32, max = 255),
  rgb(121, 70, 37, max = 255),
  rgb(125, 74, 43, max = 255),
  rgb(128, 79, 50, max = 255),
  rgb(131, 85, 56, max = 255),
  rgb(135, 90, 63, max = 255),
  rgb(138, 96, 69, max = 255),
  rgb(140, 101, 76, max = 255),
  rgb(144, 106, 84, max = 255),
  rgb(147, 111, 90, max = 255),
  rgb(150, 116, 96, max = 255),
  rgb(152, 122, 104, max = 255),
  rgb(156, 129, 112, max = 255),
  rgb(158, 135, 120, max = 255),
  rgb(160, 141, 130, max = 255),
  rgb(163, 147, 139, max = 255),
  rgb(166, 154, 147, max = 255),
  rgb(167, 160, 156, max = 255),
  rgb(170, 167, 164, max = 255),
  rgb(172, 172, 171, max = 255),
  rgb(174, 174, 174, max = 255),
  rgb(178, 178, 178, max = 255),
  rgb(181, 181, 181, max = 255),
  rgb(184, 184, 184, max = 255),
  rgb(188, 188, 188, max = 255),
  rgb(192, 192, 192, max = 255),
  rgb(196, 196, 196, max = 255),
  rgb(200, 200, 200, max = 255),
  rgb(204, 204, 204, max = 255),
  rgb(208, 206, 208, max = 255),
  rgb(212, 210, 212, max = 255),
  rgb(216, 214, 216, max = 255),
  rgb(218, 216, 218, max = 255),
  rgb(221, 219, 221, max = 255),
  rgb(225, 223, 225, max = 255),
  rgb(229, 227, 229, max = 255),
  rgb(233, 231, 233, max = 255),
  rgb(235, 233, 235, max = 255)))

aus.dem <- ggR(hill.aus3) + ggR(alt.aus, geom_raster = T, ggLayer = T, alpha = 0.75,   # Creating digital elevation map of Australia with elevation colour coded to gradient
                              ggObj = T) +
  scale_fill_gradientn(colours = schwarzwald(n = 1500), name = "Elevation",        # Schwarzwald colour palette function used
                       na.value = "transparent", limits = c(0, 1200), 
                       expand = c(0, 0), 
                       guide = guide_colourbar(nbin = 2000, barwidth = 0.5, 
                                               barheight = 3, 
                                               title.theme = element_text(size = 6), 
                                               label.theme = element_text(size = 6.5), 
                                               draw.ulim = F)) + 
  coord_quickmap() + theme_map() +
  theme(legend.position = c(0.83, 0.4), legend.background = element_rect(fill = "transparent"))

aus.dem   # Viewing map



#### State border map and DEM ####
map1 <- aus.dem
grid.arrange(map2, map1, nrow = 1)  # Arranging maps side by side 
## Add North arrow
library(ggsn)
north2(ggp = grid.arrange(map2, map1, nrow = 1), x = 0.72, y = 0.8, symbol = 3)
pdf("maps.pdf", width = 5, height = 5)  # Exporting to pdf
north2(ggp = grid.arrange(map2, map1, nrow = 1), x = 0.72, y = 0.8, symbol = 3)
dev.off()      # Finish exporting
