###############################################################################
#####         This script reads, tidies, organizes and analyzes           #####
#####         the output of the individual difference measures.           #####
#####                      LETTER COMPARISON TASK                         #####
###############################################################################
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)

# count the number of files in folder = number of iterations of loop
NumberPP = sapply("./LETTER COMP",function(dir)
        {length(list.files(dir,pattern='csv'))})      

# list the files in the relevant folder
files = dir(path = "./LETTER COMP", pattern="*.csv")

# create empty result vectors for accuracy and speed
LetterCompAcc = c()
LetterCompSpeed = c()

# For testing only
#i = sample(1:20, 1)

for (i in 1:length(files)) {
        # read file
        data = read.csv(file=(paste("./LETTER COMP/", files[i], sep="")),skip=11, header=F,as.is=TRUE,dec=".")
        # select and rename relevant sections
        data = data %>% select(V40, V41) %>% dplyr::rename(acc = V40, rt = V41)
        # calculate accuracy
        accuracy = round((sum(data$acc == 1)/48)*100, digits = 2)
        # remove incorrect trials
        data_cor = data[data$acc != 0,]
        # funtion to remove outliers, repeated until 0 outliers left
        outlier.rm <- function(x) {
                repeat {
                        # do things
                        Q <- quantile(x$rt, probs=c(.25, .75), na.rm = FALSE)
                        iqr <- IQR(x$rt)
                        x <- subset(x, x$rt > (Q[1] - 1.5*iqr) & x$rt < (Q[2]+1.5*iqr)) # remove outlier
                        outliers = boxplot(x$rt, plot=FALSE)$out
                        # exit if no outliers are left
                        if (length(outliers) == 0 ) 
                                break
                }
                return(x)
        }
        # apply function to remove outliers
        data_outlier_rm = outlier.rm(data_cor)
        # mean reaction in correct trials time in milliseconds
        speed = round(mean(data_outlier_rm$rt), digits = 5)*1000
        # store result in vector
        LetterCompAcc <- c(LetterCompAcc, accuracy)
        LetterCompSpeed <- c(LetterCompSpeed, speed)
        
}

# create df of output
Subject = as.character(c(1:i))
LetterComp = data.frame(Subject, LetterCompAcc, LetterCompSpeed)

# write as text file into WD
write.table(LetterComp, "LetterComp.txt",row.names = F)