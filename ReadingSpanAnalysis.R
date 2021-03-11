###############################################################################
#####         This script reads, tidies, organizes and analyzes           #####
#####         the output of the individual difference measures.           #####
#####                        READING SPAN TASK                            #####
###############################################################################


# count the number of files in folder = number of iterations of loop
NumberPP = sapply("./READING SPAN",function(dir)
                {length(list.files(dir,pattern='csv'))})      

# list the files in the relevant folder
files = dir(path = "./READING SPAN", pattern="*.csv")

# create empty result vectors for accuracy and speed
ReadingSpanAcc = c()
ReadingSpanSpan = c()

# For testing only
#i = sample(1:20, 1)
#i = 4 # for has missing values and is critical!

for (i in 1:length(files)) {
        # read file
        data = read.table(file=(paste("./READING SPAN/", files[i], sep="")),skip=29, header=F,as.is=F,dec=".",sep = ",")
        # select and rename relevant sections
        data = data %>% 
                select(V5, V163, V261) %>% 
                dplyr::rename(letters = V5, response = V163, gjt = V261)
        # remove NAs
        data = na.omit(data)
        # get all the letters in a vector
        letters = as.vector(data$letters)
        #create a list in which each list entry is one trial with 2-5 letters
        LetterList = list(letters[1:2],
                          letters[3:4],
                          letters[5:6],
                          letters[7:9],
                          letters[10:12],
                          letters[13:15],
                          letters[16:19],
                          letters[20:23],
                          letters[24:27],
                          letters[28:32],
                          letters[33:37],
                          letters[38:42])
        # turn it into a matrix
        n.obs <- sapply(LetterList, length)
        seq.max <- seq_len(max(n.obs))
        LetterMat <- t(sapply(LetterList, "[", i = seq.max))
        # create a vector of the responses (there will be a lot of empty cells and missing vals)
        response = as.vector(data$response)
        # turn it into a list
        ResponseList = list(response[2],
                            response[4],
                            response[6],
                            response[9],
                            response[12],
                            response[15],
                            response[19],
                            response[23],
                            response[27],
                            response[32],
                            response[37],
                            response[42])
        
        # split the char responses into separate values
        for (j in 1:length(ResponseList)) {
                ResponseList[[j]] <- unlist(strsplit(ResponseList[[j]][1], split=NULL))
        }
        # turn it into a matrix
        n.obs <- sapply(ResponseList, length)
        seq.max <- seq_len(max(n.obs))
        ResponseMat <- t(sapply(ResponseList, "[", i = seq.max))
        # make it the same dim as the LetterMat in case there are only max 4 responses instead max 5
        if (ncol(ResponseMat) < 5) {
                ResponseMat = cbind(ResponseMat, c(rep(NA,12)))
        } else if (ncol(ResponseMat) > 5) {
               # j = ncol(ResponseMat) # PROBLEMATIC IF MORE THAN 6 RESPONSES!
                ResponseMat = ResponseMat[,-6]
        }
        # compare the two matrices and write the result in a new one
        MatComp = ResponseMat==LetterMat
        # calculate reading span
        span = round(((sum(MatComp, na.rm = T))/42)*100, digits = 2) # number of letters to remember
        # calculate gjt accuracy
        accuracy_gjt = round((sum(data$gjt == 1)/42)*100, digits = 2)
        # store result in vector
        ReadingSpanAcc <- c(ReadingSpanAcc, accuracy_gjt)
        ReadingSpanSpan <- c(ReadingSpanSpan, span)
        
}

# create df of output
Subject = as.character(c(1:i))
ReadingSpan = data.frame(Subject, ReadingSpanAcc, ReadingSpanSpan)

# write as text file into WD
write.table(ReadingSpan, "ReadingSpan.txt",row.names = F)
