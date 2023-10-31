#  Analysis of Ary James data, which will ve similar to mine. This is serving as practice code.

# Read in the data from raw data 
setwd("~/Library/CloudStorage/GoogleDrive-ksinning@vt.edu/My Drive/Quantitative Analysis /Semester Project")

# loading the vegan library in order to calculate distance matrices
library(vegan)

DATAA <- data.frame(read.table(file='AJ_Community.csv', sep=',', header=TRUE))


#  Rename the non-ID part of the matrix;  it will save you some typing later
community <- DATAA[,-c(1,2)]

# Running the NMDS

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including our old friend 'vegdist' frm the vegan package
X <- metaMDS(community, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
#gives average stress
X$stress
#gives weights that different species hold in the axis
X$Taxon

#  Basic plot of all of the points
plot(X, display=c('Stream', 'Season'), choices=c(1,2), type='p')

#  Points divided into day vs. night
plot(X, display=c('Season','Stream'),choices=c(1,2), type='n')
points(X$points[DATA$time=='night',1], X$points[DATA$time=='night',2], pch=23,bg='black')
points(X$points[DATA$time=='day',1], X$points[DATA$time=='day',2], pch=21,bg='grey75')
# legend(min(X$points[,1]), max(X$points[,2]), legend=c('Night', 'Day'), pch=23, pt.bg=c('black', 'grey75')) 

#shows ellipse around CI of nights and day  
ordiellipse(X, DATA$time)  

#  Basic outline for making your own Scree plot
k <- c(1:6)
X <- metaMDS(dat.trans, distance='bray', k=6, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
X$stress
Stress <- c(0.34, 0.21, 0.16, 0.13, 0.10, 0.08)
plot(k, Stress, type='b', xlab='Dimensions', ylab='Stress', pch=21, bg='grey75', cex=1.3)  

