###############################################################################
#####         This script reads, tidies, organizes and analyzes           #####
#####         the output of the individual difference measures.           #####
#####                         DIGIT SPAN TASK                             #####
###############################################################################


# count the number of files in folder = number of iterations of loop
NumberPP = sapply("./DIGIT SPAN",function(dir)
{length(list.files(dir,pattern='csv'))})      

# list the files in the relevant folder
files = dir(path = "./DIGIT SPAN", pattern="*.csv")

# create empty result vectors for accuracy and speed
DigitSpanPercentCorTrials = c()
DigitSpanNumberCorTrials = c()
DigitSpanPercentCorDigits = c()
DigitSpanNumberCorDigits = c()

# For testing only
# i = sample(1:20, 1)
# i = 8
for (i in 1:length(files)) {
        # read file
        data = read.csv(file=(paste("./DIGIT SPAN/", files[i],sep="")), header=F,as.is=F,dec=".",sep = ",")
        # select relevant sections
        data = data[-17,]
        data = data %>% select(V1, V2, V5, V6, V7, V8, V9, V10, V105)
        # function to paste without NAs
        paste3 <- function(...,sep=", ") {
                L <- list(...)
                L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
                ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                           gsub(paste0(sep,sep),sep,
                                do.call(paste,c(L,list(sep=sep)))))
                is.na(ret) <- ret==""
                ret
        }
        data$numbers = paste3(data$V1, data$V2, data$V5, data$V6, data$V7, data$V8, data$V9, data$V10, sep = "")
        # select and rename relevant sections
        data = data %>% select(V105, numbers) %>% dplyr::rename(response = V105)
        # delete rows with NA
        data = data[complete.cases(data), ]
        # delete first row
        data = data[-1,]
        # reverse the numbers
        data$numbers = stri_reverse(data$numbers)
        # create new column for match
        data$match = ncol(data)
        # check if responses match and assign values
        data$match = ifelse(data$numbers == data$response, 1, 0)
        # calculate % of correct trials
        PercentTrials = round((sum(data$match == 1)/nrow(data))*100, digits = 2)
        # calculate number of correct trials
        NrTrials = (sum(data$match == 1))
        # create a list with the numbers and split the strings
        DigitList = str_split(data$numbers, pattern = "")
        # turn it into a matrix
        n.obs <- sapply(DigitList, length)
        seq.max <- seq_len(max(n.obs))
        DigitMat <- t(sapply(DigitList, "[", i = seq.max))
        
        # create a response list
        ResponseList = str_split(data$response, pattern = "")
        # turn it into a matrix
        n.obs <- sapply(ResponseList, length)
        seq.max <- seq_len(max(n.obs))
        ResponseMat <- t(sapply(ResponseList, "[", i = seq.max))
        # make it the same dim as the DigitMat (the ncol of the mat depends on the response!)
        if (ncol(ResponseMat) < 8) {
                ResponseMat = cbind(ResponseMat, c(rep(NA,17)))
        } else if (ncol(ResponseMat) > 8) {
                ResponseMat = ResponseMat[,1:8]
        } 
        # compare the two matrices; write result in new matrix
        MatComp = ResponseMat==DigitMat
        # calculate number of correctly remembered digits
        NumberDigits = sum(MatComp, na.rm = T)
        # Calculate percentage of correctly remembered digits 
        PercentDigits = round(((NumberDigits/76)*100), digits = 2)
        # store result in vector
        DigitSpanPercentCorTrials <- c(DigitSpanPercentCorTrials, PercentTrials)
        DigitSpanNumberCorTrials = c(DigitSpanNumberCorTrials, NrTrials)
        DigitSpanPercentCorDigits = c(DigitSpanPercentCorDigits, PercentDigits)
        DigitSpanNumberCorDigits = c(DigitSpanNumberCorDigits, NumberDigits)

}

# create df of output
Subject = as.character(c(1:i))
DigitSpan = data.frame(Subject,
                       DigitSpanPercentCorTrials, 
                       DigitSpanNumberCorTrials, 
                       DigitSpanPercentCorDigits,
                       DigitSpanNumberCorDigits)

# write as text file into WD
write.table(DigitSpan, "DigitSpan.txt",row.names = F)
