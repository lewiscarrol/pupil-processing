setwd('/home/pultsinak/Рабочий стол/pupil')
library(ggplot2)
library(data.table)
source('readEL.R')
#library(dplyr)
#library(tidyr)




logfile <- read.table("/Volumes/My Passport for Mac/pupil/W997_run1.log",sep='@')

dat <- read.asc('/net/server/data/Archive/piansrann/pultsinak/pupil/asc/run_1/W995.asc')
raw<- dat$raw

#config
t1 <- -2000 #starting epoch latency
t2 <- 3000  #finish epoch latency
t3 <- -500
blink_threshold <- 400 #????? so-called-blinks that are too long for regular blinks: 350+ ms
folder <- '/net/server/data/Archive/piansrann/pultsinak/pupil/asc/r/'
#folder2 <- '/home/ixdon/pupil_logs/'
list_of_files <- list.files(path=folder,pattern='.asc')
time_scale <- t1:t2
#/config

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

blink_find <- function(dataset,diff_threshold=10,time_threshold=20){
  L <- length(dataset$ps) #number of rows: to simplify the code
  
  DT <- as.data.table(dataset[,1:5]) #removing extra columns
  DT$diff <- 0 #initializing "deriative" colums by zeros
  DT[1:L-1,]$diff <- abs(DT[-1,]$ps-DT[-L,]$ps) #calculating "deriative" values
  
  #DT2: taking only "bad sectors of data"
  
  L1 <- is.na(DT$ps) #vector of boolean: true=missing values in pupil size
  L2 <- DT$diff>diff_threshold #vector of boolean: true=deriative above threshold
  L1[L2] <- T #summarizing the two vectors
  DT2 <- DT[L1] #DT2 contains only "bad" data sectors
  
  #DT3: marking "start" and "end" points of "bad" segments
  
  DT2$time_diff <- 0 #this code block is for 
  L_DT2 <- length(DT2$time_diff)
  DT2[-L_DT2,]$time_diff <- DT2[-1,]$time-DT2[-L_DT2,]$time
  DT2$type <- 0
  DT2[c(TRUE,DT2[-L_DT2]$time_diff>time_threshold)]$type <- 1
  DT2[DT2$time_diff>time_threshold]$type <- 2
  DT2[L_DT2]$type <- 2
  DT3 <- DT2[DT2$type>0]
  
  #DT4: a table with 1 row for 1 "bad" segment
  
  DT3$single_point <- c((DT3[1,]$type==2),as.logical((DT3[-1]$type==2)*(DT3[-length(DT3$time)]$type==2)))
  DT3[DT3$type==1]$single_point <- NA
  DT3$start_time <- 0
  DT3[DT3$single_point]$start_time <- DT3[DT3$single_point==TRUE]$time-1
  logical_vector <- c((!DT3$single_point)[-1],FALSE)
  DT3[!DT3$single_point]$start_time <- DT3[logical_vector]$time-1
  DT4 <- DT3[DT3$type==2]
  
  DT4$NAs <- 0
  for(b_idx in 1:length(DT4$time)){
    if(!DT4[b_idx,]$single_point){
      n1 <- match(DT4[b_idx,]$start_time+1,DT2$time)
      n2 <- match(DT4[b_idx,]$time,DT2$time)
      DT_temp <- DT2[n1:n2,]
      DT4[b_idx,]$NAs <- sum(is.na(DT_temp$ps))
    }
    if(DT4[b_idx,]$single_point){
      DT4[b_idx,]$NAs <- is.na(DT4[b_idx,]$ps)
    }
  } 
  DT4$duration <- DT4$time-DT4$start_time
  DT4$bad <- as.logical((DT4$duration>20)+(DT4$NAs>0))
  
  if(DT4[1,]$time==DT[1,]$time){ #removing last string of output in case of NAs in end of file
    DT4 <- DT4[-1,]
  }
  
  if(DT4[length(DT4$bad),]$time==DT[L,]$time){ #removing last string of output in case of NAs in end of file
    DT4 <- DT4[-length(DT4$bad),]
  }
  DT5 <- DT4[DT4$bad]
  data.table(t1=DT5$start_time,t2=DT5$time,duration=DT5$duration)
  
}


print("3 steps pupil processor. Step 1: data import")
full_table <- data.table()
blink_table <- NULL

for(subject_idx in 1){
  #for(subject_idx in 1:length(list_of_files)){
  #for(subject_idx in 67:length(list_of_files)){
  
  print(paste("Reading ",list_of_files[subject_idx],sep=""))
  fn <- paste(folder,list_of_files[subject_idx],sep="")
  response_logs <- NULL
  
  D <- read.asc(fn) 
  D$raw[,is.na(colnames(D$raw))] <- NULL
  
  #this section is for 500-hz files
  tick_vector <- D$raw[-1,]$time-D$raw[-length(D$raw$time),]$time #fixing 500 Hz
  if(min(tick_vector!=1) || max(tick_vector!=1)){
    dt_mode <- getmode(tick_vector)
    if(dt_mode==2){
      print('warning: 500 Hz')
      D$raw$modulo <- D$raw$time%%2
      frequent_modulo <- getmode(D$raw$modulo)
      D$raw <- D$raw[D$raw$modulo==frequent_modulo,] #deleting extra rows
      N <- length(D$raw$time)
      D$raw <- D$raw[c(sort(rep(1:(N-1),2)),N),]
      D$raw$time <- D$raw[1,]$time:D$raw[2*N-1,]$time
      D$raw$modulo <- NULL
    }
  }

  #this section is for merging multiple asc files into one
  if(grepl('1.asc',list_of_files[subject_idx])){
    print('merging server file')
    time_check_vector <- rep(0,5)
    time_check_vector[1] <- D$raw[1,]$time
    for(next_file_idx in 2:6){
      S <- paste(as.character(next_file_idx),'.asc',sep='')
      next_file <- str_replace(list_of_files[subject_idx],'1.asc',S)
      fn_temp <- paste(folder,next_file,sep="")
      print(paste('part',as.character(next_file_idx)))
      D_temp <- read.asc(fn_temp)
      D_temp$raw[,is.na(colnames(D_temp$raw))] <- NULL
    
      time_check_vector[next_file_idx] <- D_temp$raw[1,]$time
    
      tick_vector <- D_temp$raw[-1,]$time-D_temp$raw[-length(D_temp$raw$time),]$time
      if(min(tick_vector!=1) || max(tick_vector!=1)){
        dt_mode <- getmode(tick_vector)
        if(dt_mode==2){
        print('!!! 500 Hz')
        D_temp$raw$modulo <- D_temp$raw$time%%2
        frequent_modulo <- getmode(D_temp$raw$modulo)
        D_temp$raw <- D_temp$raw[D_temp$raw$modulo==frequent_modulo,] #deleting extra rows
        N <- length(D_temp$raw$time)
        D_temp$raw <- D_temp$raw[c(sort(rep(1:(N-1),2)),N),]
        D_temp$raw$time <- D_temp$raw[1,]$time:D_temp$raw[2*N-1,]$time
        D_temp$raw$modulo <- NULL
      }
    }
    
    #raw data
    
    N_delay <- 400
    L <- length(D$raw$time)
    N <- D_temp$raw[1,]$time-D$raw[L,]$time-1
    time_finish <- D$raw[L,]$time
    
    raw_filler <- data.frame(time=time_finish+(1:N_delay),
                             xp=rep(NA,N_delay),
                             yp=rep(NA,N_delay),
                             ps=rep(NA,N_delay),
                             cr.info=rep('...',N_delay),
                             block=rep(-1,N_delay))
    
    D_temp$msg$time <- D_temp$msg$time-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    D_temp$raw$time <- D_temp$raw$time-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    
    D_temp$sacc$stime <- D_temp$sacc$stime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    D_temp$sacc$etime <- D_temp$sacc$etime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    
    D_temp$fix$stime <- D_temp$fix$stime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    D_temp$fix$etime <- D_temp$fix$etime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
    
    
    D$raw <- rbind(D$raw,raw_filler)
    D$raw <- rbind(D$raw,D_temp$raw)
    D$msg <- rbind(D$msg,D_temp$msg)
    D$sacc <- rbind(D$sacc,D_temp$sacc)
    D$fix <- rbind(D$fix,D_temp$fix)
    
  }
  print(paste("t-cons:",as.character(time_check_vector)))
}
  subject_name <- sub('.asc','',list_of_files[subject_idx])
  D$raw <- as.data.table(D$raw[,1:5])
  D$msg <- as.data.table(D$msg)
  tau_const <- D$raw[1,]$time
  responses <-D$msg$text
  responses <- as.data.frame(responses)
  responses$fname <- subject_name

  for(block_idx in unique(responses$block)){
    block_link <- responses$block==block_idx
    responses_temp <- responses[block_link]
    trained_bool <- F
    last_trial <- length(responses_temp$time)
    trial_idx <- 1
    while(!trained_bool & trial_idx<last_trial-5){
      V <- (trial_idx+4):last_trial
      cor_percentage <- 1-mean(responses_temp[V]$risk)
      if(prod(!responses_temp[trial_idx+(0:3)]$risk) & cor_percentage>0.65){
        trained_bool <- T
        responses_temp[1:(trial_idx-1)]$learning <- T
        responses_temp[trial_idx+(0:3)]$critical <- T
        responses_temp[V]$trained <- T
        responses_temp[length(responses_temp$time)]$last_trial <- T
      
        responses[block_link] <- responses_temp
    }
    trial_idx <- trial_idx+1
    }
  
  }
#this section is for eye movement interpolation
  BI <- blink_find(D$raw)
  print('Interpolating blinks')
  responses$blink <- F
  responses$blink_time <- -1
  responses$blink_duration <- 0
  for(blink_idx in 1:length(BI$t1)) {
    row_idx1 <- BI[blink_idx,]$t1-tau_const-1
    row_idx2 <- BI[blink_idx,]$t2-tau_const+1
    if(row_idx1>=2 && row_idx2<=length(D$raw$time)-1 && row_idx2-row_idx1-2<=blink_threshold){
      #print(c(blink_idx,row_idx1,row_idx2))
      ps1 <- D$raw[row_idx1-1,]$ps #????????? ????? ????????????
      ps2 <- D$raw[row_idx2+1,]$ps #???????? ????? ????????????
    
      D$raw[row_idx1:row_idx2,]$ps <- seq(from=ps1,to=ps2,length.out=row_idx2-row_idx1+1)
    #print(c(row_idx1,row_idx2))
  }
  
  #replacing bad intervals with NA
  if(row_idx1>=2 && row_idx2<=length(D$raw$time)-1 && row_idx2-row_idx1-2 > blink_threshold){
    #print(c(blink_idx,row_idx1,row_idx2))
    ps1 <- D$raw[row_idx1-1,]$ps #????????? ????? ????????????
    ps2 <- D$raw[row_idx2+1,]$ps #???????? ????? ????????????
    
    D$raw[row_idx1:row_idx2,]$ps <- NA
    #print(c(row_idx1,row_idx2))
  }
}

}

library(dplyr)
library(tidyr)
dt<- data.table()
raw<- D$raw
dt<-cbind(dt,raw[,4]$ps)
dt<-cbind(dt,raw[,1])
msg<- D$msg
stim<- msg[- grep("PUFFLOCH", msg$text),]
my_dt<- merge(dt,stim, by='time',all.x = TRUE)
my_dt<- my_dt %>% fill(text)
my_dt <- cbind(my_dt, subject_name)
setnames(my_dt,"V2","pupil,z")
unique(my_dt$text)



my_dt$txt <- gsub('_1$','_2',my_dt$text)
my_dt$text<- NULL
setnames(my_dt, "txt",'text')

setwd('/net/server/data/Archive/piansrann/pultsinak/pupil/asc/run_1_csv')
write.csv(my_dt, "W998_run1.csv")


