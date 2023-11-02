#  Analysis of Ary James data, which will ve similar to mine. This is serving as practice...
# ...code to understand macroinvertebrate communities across seasons and varying salinity

# Read in the data from raw data 
setwd("~/Library/CloudStorage/GoogleDrive-ksinning@vt.edu/My Drive/Quantitative Analysis /Semester Project")

# Loading the vegan library in order to calculate distance matrices
library(vegan)
#library(readr)
# Reading in csv
DATAA <- data.frame(read.table(file='AJ_Community.csv', sep=',', header=TRUE))
DATAA <- read.csv("CodeReviewAssignment/AJ_Community.csv")
#  Rename the ID part of the matrix; take out the columns for streams and season
community <- DATAA[,-c(1,2)]

# Running the NMDS

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

X <- metaMDS(community, distance="bray",k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
# Create a factor specifying treatments for each site
# Example data frame
# Specify treatments by sites. ref is reference condition, mid is mid-salinity, high
# ... is high salinity
DATAA$salinity <- ifelse(DATAA$Stream == c("COP", "CRO", "EAS"),"ref",
                      ifelse(DATAA$Stream== "ROL", "mid","high"))

# Gives average stress
X$stress

# Gives weights that different species hold in the axis
X$species

# Basic plot of all of the points
plot(X, display=c('sites', 'species'), choices=c(1,2), type='p')

# Plotting species, this is messy so need to go back and specify certain taxa
TAXON <- envfit(X, DATAA, permutations = 999)
plot(TAXON)

# Making the plot
ordiplot(X, type="n", display = "sites", cex.lab=2, cex.axis=1.5)

# Drawing polygons for each sampling period and specific color by levels of salinity 
ordihull(X, groups=DATAA$salinity, draw="polygon", show="ref",col="darkgreen", label=F)
ordihull(X, groups=DATAA$salinity, draw="polygon", show="mid",col="chocolate1", label=F)
ordihull(X, groups=DATAA$salinity, draw="polygon", show="high",col="darkred", label=F)

# Adding points 
# Points aren't color coding how I want? Should be 6 green, 2 orange, 4 red?
points(X, display="sites", pch=16)

# Adding text for the points
# Why isn't it labeling sites by name?
text(X, display="sites", cex=0.8, col="black")

# Points divided into Spring vs. Fall without taxa overlayed
# How to use xlim and ylim to zoom in on data?
plot(X, display=c('sites','species'),choices=c(1,2), type='n')
points(X$points[DATAA$Season=='Spring',1], X$points[DATAA$Season=='Spring',2], pch=21,bg='black')
points(X$points[DATAA$Season=='Fall',1], X$points[DATAA$Season=='Fall',2], pch=23,bg='grey75')

# Points further divided by site and corresponding salinity level
# ...red = high Sc, orange = Mid Sc, green = reference
points(X$points[DATAA$Stream=='COP',1], X$points[DATAA$Stream=='COP',2], pch=21,bg='darkgreen')
points(X$points[DATAA$Stream=='CRO',1], X$points[DATAA$Stream=='CRO',2], pch=21,bg='darkgreen')
points(X$points[DATAA$Stream=='EAS',1], X$points[DATAA$Stream=='EAS',2], pch=21,bg='darkgreen')
points(X$points[DATAA$Stream=='KUT',1], X$points[DATAA$Stream=='KUT',2], pch=21,bg='darkred')
points(X$points[DATAA$Stream=='RIC',1], X$points[DATAA$Stream=='RIC',2], pch=21,bg='darkred')
points(X$points[DATAA$Stream=='ROL',1], X$points[DATAA$Stream=='ROL',2], pch=21,bg='chocolate1')

# Shows ellipse around CI of Fall and Spring  
ordiellipse(X, DATAA$Season, draw = c("polygon"))

# Adding text for site, but shows up as number instead of site code?
text(X, display="site", col="black")

# Shows ellipse around CI of Streams, looks funky but cool to see
ordiellipse(X, DATAA$Stream)


# Legend of seasons and sites by salinity
# ...How to get rid of small boxes behind legend icons? Resolution: in this case you don't need to add min(X$points[,1]), max(X$points[,2]),
#this was the reason of adding small boxes 
legend("bottomleft", 
       bty = "n", 
       legend=c('Spring', 'Fall', 'Low-Sc', 'Mid-Sc', 'High-Sc'),
      pch=c(21,23,21,21,21), pt.bg=c('black', 'grey75', 'darkgreen', 'chocolate1', 'darkred'),
      col=c('black', 'grey75', 'darkgreen', 'chocolate1', 'darkred'))


# Now, in ggplot... IN PROGRESS
# Load ggplot2 package for plotting desires
library(ggplot2)

X <- metaMDS(community, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

GGnmds <- data.frame(X$points,DATAA$salinity)
#The issue was calling NMDS and actually is MDS
nmds_plot <- ggplot(GGnmds, aes(x = MDS1, y = MDS2,color = DATAA.salinity))+
  geom_point(size = 3,shape = 19)
  
nmds_plot + scale_color_manual(values = c("ref"="darkgreen","mid"="chocolate1","high"="darkred"))+
  labs(
    x = "NMDS1",
    y = "NMDS2",
    title = "Macroinvertebrate composition over a salinity gradiente",
    color = "Levels of Sanility"  # Legend title
  )+theme_classic()

# 
print(nmds_plot)

