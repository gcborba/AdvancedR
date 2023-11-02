#  Analysis of Ary James data, which will ve similar to mine. This is serving as practice...
# ...code to understand macroinvertebrate communities across seasons and varying salinity

# Read in the data from raw data 
setwd("~/Library/CloudStorage/GoogleDrive-ksinning@vt.edu/My Drive/Quantitative Analysis /Semester Project")

# Loading the vegan library in order to calculate distance matrices
library(vegan)

# Reading in csv
DATAA <- data.frame(read.table(file='AJ_Community.csv', sep=',', header=TRUE))

#  Rename the ID part of the matrix; take out the columns for streams and season
community <- DATAA[,-c(1,2)]

# Running the NMDS

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package
X <- metaMDS(community, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Specify treatments by sites. ref is reference condition, mid is mid-salinity, high
# ... is high salinity
ref <- c("COP", "CRO", "EAS")
mid <- c("ROL")
high <- c("KUT","RIC")

# Gives average stress
X$stress

# Gives weights that different species hold in the axis
X$species

# Basic plot of all of the points
plot(X, display=c('sites', 'species'), choices=c(1,2), type='p')

# Plotting species, this is messy so need to go back and specify certain taxa
TAXON <- envfit(X, DATAA, permutations = 999)
plot(TAXON)

# Specify data and colors for polygons
type=c(rep("ref" ,3), rep("mid" ,1), rep("high" ,2))
pch.type=type
col.type=c("darkgreen", "chocolate1", "darkred")

# Adding symbols and colors for the points
pch.symbol= c(rep(16,3), rep(16,1), rep(16,2))
col.symbol= c(rep("darkgreen"), rep("chocolate1"), rep("darkred"))
par(mar=c(5.1, 5.1, 4.1, 2.1))

# Making the plot
ordiplot(X, type="n", display = "sites", cex.lab=2, cex.axis=1.5)

# Drawing polygons for each sampling period
# Keep getting error about finite coordinates?
ordihull(X, groups=type, draw="polygon", col=col.type, label=F)

# Adding points 
# Points aren't color coding how I want? Should be 6 green, 2 orange, 4 red?
points(X, display="sites", pch=pch.symbol, col=col.symbol)

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
# ...How to get rid of small boxes behind legend icons?
legend("topright", min(X$points[,1]), max(X$points[,2]), legend=c('Spring', 'Fall', 'Low-Sc', 'Mid-Sc', 'High-Sc'),
      pch=c(21,23,21,21,21), pt.bg=c('black', 'grey75', 'darkgreen', 'chocolate1', 'darkred'), box.lty = 0)


# Now, in ggplot... IN PROGRESS
# Load ggplot2 package for plotting desires
library(ggplot2)

X <- metaMDS(community, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

GGnmds <- data.frame(X$points)

nmds_plot <- ggplot(GGnmds, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 3, color = "red", shape = 19) +  # Customize points as needed
  geom_text() +  # Add labels
  labs(x = "NMDS1", y = "NMDS2")

# Getting repeated error 
print(nmds_plot)




