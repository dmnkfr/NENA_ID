###############################################################################
#####                This script reads and tidies                         #####
#####                  the background info data                           #####
#####                 and writes the result to WD                         #####
###############################################################################

# read participant data
L1_Info = read_excel(path = "NENA_PP_Data.xlsx", sheet = 1)
L2_Info = read_excel(path = "NENA_PP_Data.xlsx", sheet = 2)

# Count rows (= participants)
L1_Count = sum(rowSums(!is.na(L1_Info[,1])))
L2_Count = sum(rowSums(!is.na(L2_Info[,1])))

# delete unused rows at the end of the DF
L1_Info = L1_Info[1:L1_Count,]
L2_Info = L2_Info[1:L2_Count,]

# bind the two dataframes
ParticipantInfo = rbind.fill(L1_Info, L2_Info)

# select relevant cols and rename them
ParticipantInfo = ParticipantInfo %>%
        relocate(CODE, 
               GROUP, 
               EEG_LIST, 
               DATE, 
               BIRTHDAY,
               AGE,
               TORONTO, 
               AMAZONAS, 
               RAVENS,
               `AGE_OF _ARRIVAL`, 
               EDUCATION, 
               EDUCATION_PARENTS, 
               INCOME_PARENTS, 
               AGE_OF_EXP, 
               LOR, 
               PERC_SWE) %>%
        dplyr::rename(Subject = CODE,
                      Group = GROUP,
                      List = EEG_LIST,
                      Date = DATE,
                      Birthday = BIRTHDAY,
                      Age = AGE,
                      SwedishProficiency = TORONTO,
                      SpanishProficiency = AMAZONAS,
                      RavensTest = RAVENS,
                      AgeOfArrival = `AGE_OF _ARRIVAL`,
                      Education = EDUCATION,
                      ParentsEducation = EDUCATION_PARENTS,
                      ParentsIncome = INCOME_PARENTS,
                      AgeOfExposure = AGE_OF_EXP,
                      LengthOfResidency = LOR,
                      DailySwedishUsage = PERC_SWE)

# calculate age based on dob and date of testing; delete columns afterwards
ParticipantInfo$Age = age_calc(as.Date(ParticipantInfo$Birthday), as.Date(ParticipantInfo$Date), units = "years")
ParticipantInfo$Birthday = NULL
ParticipantInfo$Date = NULL

# New column for L2 subgroups and assign values
ParticipantInfo$L2Group = ParticipantInfo$Group
ParticipantInfo$Group[ParticipantInfo$Group != "L1"] <- "L2"
ParticipantInfo$L2Group[ParticipantInfo$Group == "L1"] <- NA
ParticipantInfo$L2Group[ParticipantInfo$L2Group == "L2_SIM"] <- "Simultaneous"
ParticipantInfo$L2Group[ParticipantInfo$L2Group == "L2_SEQ"] <- "Sequential"
ParticipantInfo$L2Group[ParticipantInfo$L2Group == "L2_TYP"] <- "Typical"
ParticipantInfo$L2Group[ParticipantInfo$L2Group == "L2_ATYP"] <- "Atypical"
# rename values in list col
ParticipantInfo$List[ParticipantInfo$List == "LIST1"] <- "List1"
ParticipantInfo$List[ParticipantInfo$List == "LIST2"] <- "List2"
ParticipantInfo$List[ParticipantInfo$List == "LIST3"] <- "List3"
ParticipantInfo$List[ParticipantInfo$List == "LIST4"] <- "List4"

# Convert Subject to integer for matching later on
ParticipantInfo$Subject = as.integer(ParticipantInfo$Subject)

# Convert test results to percent correct rather than absolute points
ParticipantInfo$RavensTest = round(((ParticipantInfo$RavensTest/60)*100), digits = 2)
ParticipantInfo$SwedishProficiency = round(((ParticipantInfo$SwedishProficiency/42)*100), digits = 2)
ParticipantInfo$SpanishProficiency = round(((ParticipantInfo$SpanishProficiency/37)*100), digits = 2)

# read individual difference measure data and merge with participant info
IndDiff = read.csv("IndDiff.txt", sep = " ")
ParticipantInfo <- ParticipantInfo %>% 
        inner_join(IndDiff, by = "Subject")

# reorder columns
ParticipantInfo = ParticipantInfo %>%
        select(Subject, 
               Group,
               L2Group,
               List, 
               Age,
               Education,
               ParentsEducation,
               ParentsIncome,
               AgeOfArrival,
               AgeOfExposure,
               LengthOfResidency,
               SwedishProficiency,
               SpanishProficiency,
               DailySwedishUsage,
               RavensTest, 
               ReadingSpanAcc, 
               ReadingSpanSpan,
               LetterCompAcc, 
               LetterCompSpeed, 
               DigitSpanPercentCorTrials, 
               DigitSpanPercentCorDigits)

# show summary statistics for both groups (so, no L2-only vars)
ParticipantInfo = as.tibble(ParticipantInfo)
ParticipantSummary = ParticipantInfo %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(
                Mean_Age = round(mean(Age, na.rm = TRUE), digits = 2),
                Mean_Education = round(mean(as.numeric(Education), na.rm = TRUE),digits = 2),
                Mean_ParentsEducation = round(mean(as.numeric(ParentsEducation), na.rm = TRUE),digits = 2),
                Mean_ParentsIncome = round(mean(ParentsIncome, na.rm = TRUE),digits = 2),
                Mean_SwedishProficiency = round(mean(SwedishProficiency, na.rm = TRUE),digits = 2),
                Mean_ReadingSpanAcc = round(mean(ReadingSpanAcc, na.rm = TRUE),digits = 2),
                Mean_ReadingSpanSpan = round(mean(ReadingSpanSpan, na.rm = TRUE),digits = 2),
                Mean_LetterCompAcc = round(mean(LetterCompAcc, na.rm = TRUE),digits = 2),
                Mean_LetterCompSpeed = round(mean(LetterCompSpeed, na.rm = TRUE),digits = 2),
                Mean_DigitSpanPercentCorTrials = round(mean(DigitSpanPercentCorTrials, na.rm = TRUE),digits = 2),
                Mean_DigitSpanPercentCorDigits = round(mean(DigitSpanPercentCorDigits, na.rm = TRUE), digits = 2))
        )
print(ParticipantSummary)
# write into WD
write.table(ParticipantInfo, "ParticipantInfo.txt",row.names = F)

# write summary as pdf
pdf("ParticipantSummary.pdf")
grid.table(t(ParticipantSummary))
dev.off()

# Check if nrow is correct // DOESNT WORK YET! Files incomplete!
if(nrow(ParticipantInfo) == L1_Count + L2_Count) {print("SUCCESS!")}
