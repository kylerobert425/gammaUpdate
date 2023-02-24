library(tidyverse)
library(readxl)
library(writexl)
library(syuzhet)
library(RColorBrewer)
library(zoo)
library(ggthemes)
library(scales)
library(gridExtra)

#### FILE PATH INFO AND STUFF ####
extension <- "~/Desktop/Memfault Test/BlackBox/Update1/"
g_output_path <- "~/Desktop/Memfault Test/BlackBox/Update1 Graphs/"
r_output_file  <- "~/Desktop/Memfault Test/BlackBox/results2_2_NA.xlsx"
#file with tester's names, UUIDS etc, will use config value to determine grill size...
testerList <- "~/Desktop/Memfault Test/BlackBox/Gamma Testers with UUIDs copy.xlsx"
testerList <- read_excel(testerList)

#### Feedback forms  ####
filepath_t <- paste(extension,"Forms/troubleshooting.xlsx", sep = "")
filepath_g <- paste(extension, "Forms/general.xlsx", sep = "")

df_t <- read_excel(filepath_t)
df_g <- read_excel(filepath_g)

#rename columns? (if you do, refactor "Name: " or "Full Name" below...)

# count of number of names...
df_t %>% count(`Name:`)
names <- df_g %>% count(`Full Name`)
names <- names[order(-names$n),]

par(mar=c(11,4,4,4))

nBar <- barplot(names$n,
        names.arg = names$`Full Name`,
        las=2,
        col = "#69b3a2",
        ylab = 'Ressponses',
        main = 'M2 Gamma Test - GeneralFeedback Sumbissions by Tester')

#sentiment analysis of feedback?
comments <- df_g$`What is your feedback?`

s <- get_nrc_sentiment(comments)


# you should order these by how common they are so bar graph looks best...
sSums <- data.frame(colnames(s),colSums(s))
sSums <- sSums[order(-sSums$colSums.s.),]

#reset margins
par(mar=c(7,4,4,4))

gBar <- barplot(sSums$colSums.s.,
        names.arg = sSums$colnames.s.,
        las = 2,
        col = brewer.pal(10, name = "Pastel2"),
        ylab = 'Count',
        main = 'Gamma Test: General Feedback Sentiment Scores')

trouble <- df_t$`Please describe your issue in depth:`
t <- get_nrc_sentiment(trouble)

# order the columns again 
tSums <-  data.frame(colnames(t),colSums(t))
tSums <- tSums[order(-tSums$colSums.t.),]

tBar <- barplot(tSums$colSums.t.,
        names.arg = tSums$colnames.t.,
        las = 2,
        col = brewer.pal(10, name = "Set3"),
        ylab = 'Count',
        main = 'Gamma Test: Troubleshooting Sentiment Scores')

#### LOOP THRU ALL FILES ####

# Get all .xlsx files in the folder
files <- list.files(path = extension, pattern = ".xlsx", full.names=TRUE, recursive=FALSE)

#save output?
saveOutput <- TRUE

#result output table... How would this work with varibale number of cols?

result_names <- c("UUID", "CookID", "Size", "Date", "Time", "Ignition Time", "Set Temp 1", "Time To Temp 1", "Overshoot 1", "Set Temp 2", "Time To Temp 2", "Overshoot 2", "Set Temp 3", "Time To Temp 3", "Overshoot 3", "Set Temp 4", "Time To Temp 4", "Overshoot 4", "Set Temp 5", "Time To Temp 5", "Overshoot 5", "Errors")
results <- data.frame(matrix(nrow = 0, ncol = length(result_names)), stringsAsFactors = FALSE)
names(results) <- result_names

i <- 1

for(i in seq(from=1, to=length(files), by=1)) {
    x <- files[i]
    
    # #test file...
    # x <- "GammaUpdate1_group1_682719B2AACF_682719B2AACF1675368514_230202_1308.xlsx"
    # 
    # x <- paste(extension, x, sep ="")
    #
    
    #### SETUP ####
    #decompose file name into useful information...
    parsedList <- str_split(x, '/')
    fileDeets <- parsedList[[1]][9]
    
    #remove extension
    fileDeets <- tools::file_path_sans_ext(fileDeets)
    file_values <- str_split(fileDeets, '_') 
    
    #assign to variables for labeling...
    uuid <- file_values[[1]][3]
    cookid <- file_values[[1]][4]
    date <- file_values[[1]][5]
    time <- file_values[[1]][6]
    
    #get tester's specific information:
    testerInfo <-  testerList %>% filter(`Grill UUID` == uuid)
    tester <- testerInfo$`Gamma Tester`
    size <- if(testerInfo$Config == 2205.001) 'Large' else "Small"
  
    #### GET DATA ####
    cook_details <- read_excel(x, sheet = "Cook details")
    diag_data <- read_excel(x, sheet = "Diagnostic data")
    cook_stats <- read_excel(x, sheet = "Cook Statistics")
    
    #cook_stats has two useless columns in it...
    cook_stats <- select(cook_stats, -c(2,3))
    
    #merge df's?
    cook_details <- left_join(diag_data, cook_details, by = c("td" = "td"))
    cook_details$grill <- na.approx(cook_details$grill, na.rm=FALSE)
    cook_details$set_temp <- na.approx(cook_details$set_temp, na.rm=FALSE)
    
    #going to go up to 5 stats set temps, if they dont exist, then just need to have NA's in the rows
    # output <- c(uuid, cookid, size, date, time,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    output <- data.frame(matrix(nrow = 0, ncol = length(result_names)))
    output <- data.frame(UUID = c(uuid),
                         CookID = c(cookid),
                         Size = c(size),
                         Date = c(date),
                         Time = c(time),
                         `Ignition Time` = c(NA),
                         `Set Temp 1` = c(NA),
                         `Time To Temp 1` = c(NA),
                         `Overshoot 1` = c(NA),
                         `Set Temp 2` = c(NA),
                         `Time To Temp 2` = c(NA),
                         `Overshoot 2` = c(NA),
                         `Set Temp 3` = c(NA),
                         `Time To Temp 3` = c(NA),
                         `Overshoot 3` = c(NA),
                         `Set Temp 4` = c(NA),
                         `Time To Temp 4` = c(NA),
                         `Overshoot 4` = c(NA),
                         `Set Temp 5` = c(NA),
                         `Time To Temp 5` = c(NA),
                         `Overshoot 5` = c(NA),
                         Errors = c(NA), stringsAsFactors = FALSE)
    
    names(output) <- result_names
    
    temps <- ncol(cook_stats)
    stat_names <- colnames(cook_stats)
    
    #ignition time always the same...
    output[6] <- cook_stats[2,2]
    
    #errors always the same
    error_vals <- cook_details %>% count(errors)
    
    #check where there are any errors, and report that value.
  
    #TODO- test with multiple errors in a cook?  Only checking the first one here...
    error_index <- which(error_vals$errors != 0, arr.ind = T)
    if(length(error_index) != 0){
      output[22] <- error_vals$errors[error_index[1]]
    }
    
    
    if(temps == 2){
      #just one set temp
      output[7] <- as.numeric(stat_names[temps])
      #time to temp
      output[8] <- as.numeric(cook_stats[6,temps])
      #overshoot value
      output[9] <- as.numeric(cook_stats[1,temps])
    } else if (temps == 3){
      #two temps
      output[7] <- as.numeric(stat_names[temps-1])
      output[10] <- as.numeric(stat_names[temps])
      #t2t
      output[8] <- as.numeric(cook_stats[6,temps-1])
      output[11] <- as.numeric(cook_stats[6,temps])      
      #overshoot value
      output[9] <- as.numeric(cook_stats[1,temps-1])
      output[12] <- as.numeric(cook_stats[1,temps])
    } else if (temps == 4){
      #three temps
      output[7] <- as.numeric(stat_names[temps-2])
      output[10] <- as.numeric(stat_names[temps-1])
      output[15] <- as.numeric(stat_names[temps])
      #t2t
      output[8] <- as.numeric(cook_stats[6,temps-2])
      output[11] <- as.numeric(cook_stats[6,temps-1])
      output[14] <- as.numeric(cook_stats[6,temps])
      #overshoot value
      output[9] <- as.numeric(cook_stats[1,temps-2])
      output[12] <- as.numeric(cook_stats[1,temps-1])
      output[15] <- as.numeric(cook_stats[1,temps])
    } else if (temps == 5) {
      #four temps
      output[7] <- as.numeric(stat_names[temps-3])
      output[10] <- as.numeric(stat_names[temps-2])
      output[15] <- as.numeric(stat_names[temps-1])
      output[17] <- as.numeric(stat_names[temps])
      #t2t
      output[8] <- as.numeric(cook_stats[6,temps-3])
      output[11] <- as.numeric(cook_stats[6,temps-2])
      output[14] <- as.numeric(cook_stats[6,temps-1])
      output[17] <- as.numeric(cook_stats[6,temps])
      #overshoot value
      output[9] <- as.numeric(cook_stats[1,temps-3])
      output[12] <- as.numeric(cook_stats[1,temps-2])
      output[15] <- as.numeric(cook_stats[1,temps-1])
      output[17] <- as.numeric(cook_stats[1,temps])
    } else if (temps == 6){
      #five temps
      output[7] <- as.numeric(stat_names[temps-4])
      output[10] <- as.numeric(stat_names[temps-3])
      output[15] <- as.numeric(stat_names[temps-2])
      output[17] <- as.numeric(stat_names[temps-1])
      output[19] <- as.numeric(stat_names[temps])
      #t2t
      output[8] <- as.numeric(cook_stats[6,temps-4])
      output[11] <- as.numeric(cook_stats[6,temps-3])
      output[14] <- as.numeric(cook_stats[6,temps-2])
      output[17] <- as.numeric(cook_stats[6,temps-1])
      output[20] <- as.numeric(cook_stats[6,temps])
      #overshoot value
      output[9] <- as.numeric(cook_stats[1,temps-4])
      output[12] <- as.numeric(cook_stats[1,temps-3])
      output[15] <- as.numeric(cook_stats[1,temps-2])
      output[17] <- as.numeric(cook_stats[1,temps-1])
      output[21] <- as.numeric(cook_stats[1,temps])
    }
    
    #### OUTPUT STATS ####
    if(saveOutput){
      #view(output)
      results <- rbind(results, output)
      #view(results)
      #idk why but the names get randomly fucked up...
      #names(results) <- result_names
    }
    
    #### PLOT ####
    
    #ONLY IF Diag. data exists...
    if(nrow(diag_data)>0){
    
    # do you want to label these?
    
    #change axis to time
    
    cook_details$time <- as.POSIXct( cook_details$time, origin = "1970-01-01") 
    
    grillGraph <- cook_details %>%
      ggplot(aes(x=time)) +
      #add labels
      labs(title = paste(tester, "Gamma Test Cook", as.Date(cook_details$time[1], "MST")),
           #subtitle = "FW:01.00.09, CF:2204.a10",
           x = "Time",
           y = "Temp [˚F]") +
      #add lines
      geom_line(aes(y=grill), color = "tomato3", size = 2, lineend = "round") +
      geom_line(aes(y=set_temp, color = pidout.x), size = 1, lineend = "round") +
      scale_y_continuous(breaks = seq(50, 550, by = 50))
      
    grillGraph 
    # #plotly...
    # library(plotly)
    # ggplotly(grillGraph)
    
    #component graphs:
    compGraph <- cook_details %>% 
      ggplot(aes(x=time)) + 
      labs(title = "Grill Components During Cook",
           x = "Time",
           y = "Value",
           color = "Legend") +
      geom_line(aes(y = ac_ignitor_value, color = 'Ignitor' )) +
      geom_line(aes(y = fan.x, color = 'Fan')) +
      geom_line(aes(y = 4*auger.x, color = 'Auger (x4)')) +
      geom_line(aes(y = 50*lid_open.x, color = 'Lid open (x50)'))
    
    compGraph <- compGraph + scale_color_brewer(palette = "Set1")
    
    #flame sensor value
    flameGraph <- cook_details %>% 
      ggplot(aes(x = time)) +
      labs(title = "Flame Sensor Value",
           x = "Time",
           y = "Value",
           color = "Legend") +
      geom_line(aes(y = comb_flame_sensor_value, color = "Flame Sensor"))
    
    #flameGraph 
    
    comboGraph <- grid.arrange(grillGraph, compGraph, flameGraph, ncol = 1, nrow = 3, heights=c(1.5,1,1)) 
    #ggplotly(comboGraph)
    
    #save graph image
    
    g_filename <- paste(uuid,cookid,date, sep = "_")
    g_filename <- paste(g_output_path, g_filename, ".png", sep = "")
    
    g <- arrangeGrob(grillGraph, compGraph, flameGraph, ncol = 1, nrow = 3, heights=c(1.5,1,1))

    ggsave(
      file = g_filename,
      plot = g,
      height =  8.1,
      width = 7.2,
      dpi = 300
    )
    }
} #end main loop

#merge names and Results so we can get a count of those...

names_uuids <- testerList %>%
  select(`Gamma Tester`, `Grill UUID`)
colnames(names_uuids) = c('tester', 'UUID')


df_r <- left_join(results, names_uuids, by = "UUID")

#get number of cooks by tester...
# count of number of names...

names <- df_r %>% count(tester)
names <- names[order(-names$n),]

par(mar=c(11,4,4,4))

nBar <- barplot(names$n,
                names.arg = names$tester,
                las=2,
                col = "#69b3a2",
                ylab = 'Ressponses',
                main = 'Gamma Update 2 - Number of Cooks By Tester')

#output Results Table to excel...
write_xlsx(df_r, r_output_file)

#output cook count...

#FUCK you overrode the first one...
write_xlsx(names, "~/Desktop/Memfault Test/BlackBox/update1_cookCounts.xlsx")


#extra BS

#for graphing
#x-time scale
#scale_x_datetime(labels=date_format("%H:%M"), breaks = "1 hour")
#Other labels..
#20 min - would need to do some tricky shit here if you use the datetime...
#geom_vline(xintercept=20*60, linetype="dashed", color = "darkgreen")
# geom_vline(xintercept = time_to_temp, linetype = "dashed", color = "yellowgreen") +
# geom_text(data = labeldf, aes(x, y, label = text), nudge_y = 15)c
#


