library(ggplot2) # read in ggplot2 to make plots

df <- read.csv("./just_NY_data.csv", header=TRUE) # read in data

df <- df[c(1:8,51:92)] # remove PCs from previous analysis

###################################################################
##### FIRST, CALCULATE RELATIVE AREAS AND VEIN-TO-BLADE RATIO #####
###################################################################

# calculate relative areas using shoelace algorithm
# from relative areas, calculate dimensionless ln(ratio of blade to vein areaa)

df$all_area = (0.5)*abs(

(df$x4*df$y3 + df$x3*df$y2 + df$x2*df$y1 + df$x1*df$y6 + df$x6*df$y14 + df$x14*df$y15 + df$x15*df$y16 + df$x16*df$y17 + df$x17*df$y18 + df$x18*df$y19 + df$x19*df$y20 + df$x20*df$y21 + df$x21*df$y13 + df$x13*df$y4) - 

(df$y4*df$x3 + df$y3*df$x2 + df$y2*df$x1 + df$y1*df$x6 + df$y6*df$x14 + df$y14*df$x15 + df$y15*df$x16 + df$y16*df$x17 + df$y17*df$x18 + df$y18*df$x19 + df$y19*df$x20 + df$y20*df$x21 + df$y21*df$x13 + df$y13*df$x4) 

)

# Next, calculate the area of the vasculature starting with the proximal vein & branch

df$prox = (0.5)*abs(

(df$x2*df$y1 + df$x1*df$y6 + df$x6*df$y14 + df$x14*df$y5 + df$x5*df$y15 + df$x15*df$y7 + df$x7*df$y2) -

(df$y2*df$x1 + df$y1*df$x6 + df$y6*df$x14 + df$y14*df$x5 + df$y5*df$x15 + df$y15*df$x7 + df$y7*df$x2)

)

# Then the distal vein and branch

df$dist = (0.5)*abs(

(df$x3*df$y2 + df$x2*df$y9 + df$x9*df$y17 + df$x17*df$y8 + df$x8*df$y18 + df$x18*df$y10 + df$x10*df$y3) -

(df$y3*df$x2 + df$y2*df$x9 + df$y9*df$x17 + df$y17*df$x8 + df$y8*df$x18 + df$y18*df$x10 + df$y10*df$x3)

)


# And the area of the midvein and its branch

df$mid = (0.5)*abs(

(df$x4*df$y3 + df$x3*df$y12 + df$x12*df$y20 + df$x20*df$y11 + df$x11*df$y21 + df$x21*df$y13 + df$x13*df$y4) -

(df$y4*df$x3 + df$y3*df$x12 + df$y12*df$x20 + df$y20*df$x11 + df$y11*df$x21 + df$y21*df$x13 + df$y13*df$x4)

)

# Then calculate the overall vein area as the sum of the proximal, distal, and midveins

df$veins = df$prox + df$dist + df$mid

# Calculate blade area as the overall area of the leaf minus vein area

df$blade = df$all_area - df$veins

# Calculate vein-to-blade ratio 
# We use natural log transformation which makes the distribution more normal

df$vein_to_blade = log(df$veins / df$blade)

##############################################
##### REPRODUCE REVIEWER #1 PCA ANALYSIS #####
##############################################

df<- subset(df, heteroblasty<0.6379505834434538) # subset data less than indicated shoot position value

summary(df) # check data
names(df) # double check column indexing

# perform PCA using ONLY Procrustes coordinate data
# Procrustes coordinate data is cols 9:50
# perform PCA using prcomp default settings, center=TRUE and scale.=FALSE

pca1 <- prcomp(df[9:50], center=TRUE, scale.=FALSE) 

summary(pca1) # get percent variance explained by each PC

# PC1 % var = 0.3508, # PC2 = 0.2163
# reproduces reviewer #1 percent variances

pca1_scores <- cbind(df,as.data.frame(pca1$x)) # merge data with PC values

p <- ggplot(pca1_scores, aes(PC1, PC2, color=vein_to_blade)) # plot PC2 vs PC1 and color by vein-to-blade ratio
p + geom_point()

# above plot reproduces reviewer #1's results

####################################################################
##### REPRODUCE PCA ANALYSIS IN ORGINALLY SUBMITTED MANUSCRIPT #####
####################################################################

summary(df) # check data
names(df) # double check column indexing

# perform PCA using the following:
# 1: bin data (col 8)
# 2: Procrustes coordinate data (cols 9:50)
# 3: relative areas and vein-to-blade ratio (cols 51:57)
# columns 8:57 will be included in the PCA
# perform PCA using prcomp default settings, center=TRUE and scale.=FALSE
# sklearn.decomposition.PCA default settings are also centered but not scaled

pca2 <- prcomp(df[9:57], center=TRUE, scale.=FALSE) 

summary(pca2) # get percent variance explained by each PC
# PC1 % var = 0.9019, # PC2 = 0.09640
# reproduces percent variances for each PC in originally submitted manuscript

pca2_scores <- cbind(df,as.data.frame(pca2$x)) # merge data with PC values

p <- ggplot(pca2_scores, aes(PC1, PC2, color=vein_to_blade)) # plot PC2 vs PC1 and color by vein-to-blade ratio
p + geom_point()

# above plot reproduces results in R's prcomp as originally analyzed in Python's sklearn.decomposition.PCA

##########################################################################
##### REVISED MANUSCRIPT ANALYSIS FROM PYTHON REPRODUCED IN R PRCOMP #####
##########################################################################

new_df <- read.table("./NY_and_CA_data.txt", header=TRUE) # read in NEW data

# perform PCA using ONLY Procrustes coordinate data
# however, now the analysis includes CA population data, whereas before it was only NY germplasm data
# Procrustes coordinate data is cols 14:55
# perform PCA using prcomp default settings, center=TRUE and scale.=FALSE
pca3 <- prcomp(new_df[c(14:55)], center=TRUE, scale.=FALSE) 

summary(pca3) # get percent variance explained by each PC
# PC1 % var = 39.7%, # PC2 = 17.6%
# reproduces percent variances for each PC in the revised manuscript

pca3_scores <- cbind(new_df,as.data.frame(pca3$x)) # merge data with PC values

p <- ggplot(pca3_scores, aes(PC1, PC2, color=dataset)) # plot PC2 vs PC1 and color by vein-to-blade ratio
p + geom_point()

# above plot reproduces results in R's prcomp as analyzed in Python's sklearn.decomposition.PCA in revised analysis


