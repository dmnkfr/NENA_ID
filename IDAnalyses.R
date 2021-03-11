###############################################################################
#####          This script calls the scripts that analyze the             #####
#####                    individual difference tasks                      #####
###############################################################################
###############################################################################
###############################################################################
#####                       Load relevant packages                        #####
###############################################################################
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)
if(!require(stringi)){install.packages('stringi')}
library(stringi)
if(!require(Hmisc)){install.packages('Hmisc')}
library(Hmisc)
if(!require(readxl)){install.packages('readxl')}
library(readxl)
if(!require(eeptools)){install.packages('eeptools')}
library(eeptools)
if(!require(gridExtra)){install.packages('gridExtra')}
library(gridExtra)
if(!require(plyr)){install.packages('plyr')}
library(plyr)
###############################################################################
###############################################################################

#path = "H:/2019_NENA/1_DATA/2_ANALYSES/ID TASKS/NENA_ID"

# DON'T CHANGE ANYTHING FROM HERE
# set wd
#setwd(path)
# run the programs
source("LetterCompAnalysis.R")
source("ReadingSpanAnalysis.R")
source("DigitSpanAnalysis.R")
# unify the data
# read the files
DigitSpan = read.delim(file="DigitSpan.txt", header = T, sep = " ")
ReadingSpan = read.delim(file="ReadingSpan.txt", header = T, sep = " ")
LetterComp = read.delim(file="LetterComp.txt", header = T, sep = " ")
# merge them into one df and write to df
IndDiff <- ReadingSpan %>% 
        inner_join(DigitSpan, by = "Subject") %>% 
        inner_join(LetterComp, by = "Subject")
write.table(IndDiff, "IndDiff.txt",row.names = F)
# Check if nrow is correct
if(nrow(IndDiff) == i) {print("SUCCESS!")}
# run background info program
source("ParticipantInfoFusion.R")
