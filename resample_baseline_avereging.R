library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggpubr)




folder <- '/net/server/data/Archive/piansrann/pultsinak/pupil/asc/run_1_csv/'
#folder2 <- '/home/ixdon/pupil_logs/'
list_of_files <- list.files(path=folder,pattern='.csv')
#d<-data %>% filter(text == "STIM_aud_1_1" | lead(text == "STIM_aud_1_1", n=2000))


data<- data.table()
for(f in 1:length(list_of_files)){
  fn<-paste(folder,list_of_files[f],sep="")
  csv<- read_csv(fn)
  data<- rbind(data,csv)
}

events<- unique(data$text)
events<-events[ !events == "!MODE RECORD CR 1000 2 1 L"]
events<-events[ !events== "!MODE RECORD CR 1000 2 1 R"]
subj<- unique(data$subject_name)
subj= subj[-2]

d<-data.table()
for (s in 1:length(subj)){
  print(subj[s])
  sub_df<- subset(data, subject_name== subj[s])
  mean<- mean(sub_df$`pupil,z`, na.rm=TRUE)
  sd<- sd(sub_df$`pupil,z`, na.rm=TRUE)
  #wrong <- mutate(sub_df, prev = lag(time,2000))
  for (e in 1:length(events)){
    print(events[e])
    event_df<-subset(sub_df,text == events[e])
    is.na(event_df$`pupil,z`) <- event_df$`pupil,z` < mean - 3*sd| event_df$`pupil,z` >mean + 3*sd
    event_df$n<- c(1:length(event_df$time))
    na<- which(is.na(event_df$`pupil,z`), arr.ind=TRUE)
    
    if (length(event_df$n) < 9000){
      print("Short event")
    } else{
        t1<- event_df$time[1] # time of the stimul
        t2<- t1-1001 ### time before stim
        rn1<- which(sub_df$time == t1, arr.ind=TRUE)# time of the stimul
        rn2<- which(sub_df$time == t2, arr.ind=TRUE)### time before stim
        before_stim<- sub_df[rn2:rn1,]
        before_stim$text<- events[e]
        before_stim <- before_stim %>% filter(row_number() <= n()-1)
        new_event<-filter(event_df, n<9001)
        new_event$n<- NULL
        df_with_before_stim<- rbind(before_stim,new_event)
        df_with_before_stim$time_cor<- seq(-1000, length(new_event$time),1)
        na<- which(is.na(df_with_before_stim$`pupil,z`), arr.ind=TRUE)
        na_seq<- as.data.frame(seqToIntervals(na))
        print(na_seq)
        na_seq$diff<- na_seq[2] - na_seq[1]
        dif<- replace(na_seq, is.na(na_seq), 0)
        for (di in 1:length(dif$diff)){
          if (di > 2000 ){
            print(TRUE)
          }else{
            d<- rbind(d,df_with_before_stim)
        }
     }
    }
  }
}

############ RUN2 ##############
df_run1<- d
df_run1$round<- "run_1"
folder <- '/net/server/data/Archive/piansrann/pultsinak/pupil/asc/run_2_csv/'
#folder2 <- '/home/ixdon/pupil_logs/'
list_of_files <- list.files(path=folder,pattern='.csv')
#d<-data %>% filter(text == "STIM_aud_1_1" | lead(text == "STIM_aud_1_1", n=2000))


data<- data.table()
for(f in 1:length(list_of_files)){
  fn<-paste(folder,list_of_files[f],sep="")
  csv<- read_csv(fn)
  data<- rbind(data,csv)
}

events<- unique(data$text)
events<-events[ !events == "!MODE RECORD CR 1000 2 1 L"]
events<-events[ !events== "!MODE RECORD CR 1000 2 1 R"]
subj<- unique(data$subject_name)
subj= subj[-2]

d<-data.table()
for (s in 1:length(subj)){
  print(subj[s])
  sub_df<- subset(data, subject_name== subj[s])
  mean<- mean(sub_df$`pupil,z`, na.rm=TRUE)
  sd<- sd(sub_df$`pupil,z`, na.rm=TRUE)
  #wrong <- mutate(sub_df, prev = lag(time,2000))
  for (e in 1:length(events)){
    print(events[e])
    event_df<-subset(sub_df,text == events[e])
    is.na(event_df$`pupil,z`) <- event_df$`pupil,z` < mean - 3*sd| event_df$`pupil,z` >mean + 3*sd
    event_df$n<- c(1:length(event_df$time))
    na<- which(is.na(event_df$`pupil,z`), arr.ind=TRUE)
    
    if (length(event_df$n) < 9000){
      print("Short event")
    } else{
      t1<- event_df$time[1] # time of the stimul
      t2<- t1-1001 ### time before stim
      rn1<- which(sub_df$time == t1, arr.ind=TRUE)# time of the stimul
      rn2<- which(sub_df$time == t2, arr.ind=TRUE)### time before stim
      before_stim<- sub_df[rn2:rn1,]
      before_stim$text<- events[e]
      before_stim <- before_stim %>% filter(row_number() <= n()-1)
      new_event<-filter(event_df, n<9001)
      new_event$n<- NULL
      df_with_before_stim<- rbind(before_stim,new_event)
      df_with_before_stim$time_cor<- seq(-1000, length(new_event$time),1)
      na<- which(is.na(df_with_before_stim$`pupil,z`), arr.ind=TRUE)
      na_seq<- as.data.frame(seqToIntervals(na))
      print(na_seq)
      na_seq$diff<- na_seq[2] - na_seq[1]
      dif<- replace(na_seq, is.na(na_seq), 0)
      for (di in 1:length(dif$diff)){
        if (di > 2000 ){
          print(TRUE)
        }else{
          d<- rbind(d,df_with_before_stim)
        }
      }
    }
  }
}

df_run2<- d
df_run2$round<- "run_2"

df_with_norm_time<- rbind(df_run2,df_run1)


df_with_norm_time$stim<- (str_extract(df_with_norm_time$text, "[aA-zZ]+"))
#aud<- filter(data, stim=="STIM_aud_")
stimul_list<- unique(df_with_norm_time$stim)
trial_number = as.numeric(gsub(".*?([0-9]+).*", "\\1", df_with_norm_time$text))
data<- cbind(df_with_norm_time,trial_number)
data$block<- NULL




###################################### Resample, z-transform, avereging #################
runs<- c('run_1','run_2')
trials<- c(1:15)

slideFunct <- function(data, step){
  total <- length(data)
  spots <- seq(from=1, to=total, by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- median(data[spots[i]:(spots[i])], na.rm=TRUE)
  }
  return(result)
}

resample_df<- data.table()

for (s in 1:length(subj)){
  sub_df<- subset(data, subject_name== subj[s])
  mean<- mean(sub_df$`pupil,z`,na.rm=TRUE)
  sd<- sd(sub_df$`pupil,z`,na.rm=TRUE)
  for (r in 1:length(runs)){
    run_df<-subset(sub_df, round== runs[r])
    for (stims in 1:length(stimul_list)){
      st<- subset(run_df, stim== stimul_list[stims])
      if (length(st$time)==0){
        print("NO stimul")
      }else{
          for (t in 1:length(trials)){
            print(trials[t])
            t_df<-subset(st, trial_number== trials[t])
            if (length(t_df$time)==0){
              print("Trail not exists")
            } else{
                resample<-as.data.frame(slideFunct(t_df$`pupil,z`, 50))
                colnames(resample)[1]<- 'pupil_size'
                resample_time<- slideFunct(t_df$time_cor, 50)
                trial_number<- slideFunct(t_df$trial_number, 50)
                resample$stim<- stimul_list[stims]
                resample$run<- runs[r]
                resample$subject<- subj[s]
                resample$trial_number<- trials[t]
                resample<- cbind(resample,resample_time)
                resample_df<- rbind(resample_df,resample)
            }
          }
        }
      }
    }
  }

############## GLOBAL Z-transform (baseline correction) #######
colnames(resample_df)[1]<- 'pupil_size'
#colnames(run2)[5]<- 'time'
zscore_df<- data.table()

for (s in 1:length(subj)){
  sub_df<- subset(resample_df, subject== subj[s])
  mean<- mean(sub_df$pupil_size,na.rm=TRUE)
  sd<- sd(sub_df$pupil_size,na.rm=TRUE)
  for (r in 1:length(runs)){
    run_df<-subset(sub_df, run== runs[r])
    
    for (stims in 1:length(stimul_list)){
      st<- subset(run_df, stim== stimul_list[stims])
      if (length(st$time)==0){
        print("NO stimul")
      }else{
        for (t in 1:length(trials)){
          print(trials[t])
          t_df<-subset(st, trial_number== trials[t])
          if (length(t_df$resample_time)==0){
            print("Trail not exists")
          } else{
              #t_df<-subset(st, trial_number== trials[t])
              pupil_z<-as.data.frame((t_df$pupil_size - mean)/sd)
              #pupil_z_second_bl<-as.data.frame((pupil_z - before_stim_mean)/before_stim_sd)
              #resample_time<- seq.int(from = -2000, to = last_time, by = 5)
              time<- t_df$resample_time
              pupil_z$stim<- stimul_list[stims]
              pupil_z$run<- runs[r]
              pupil_z$subject<- subj[s]
              pupil_z$trial_number<- trials[t]
              pupil_z<- cbind(pupil_z,time)
              zscore_df<- rbind(zscore_df,pupil_z)
          } 
        }
      }
    }
  }
}

colnames(zscore_df)[1]<- 'pupil_size'


############## there is we apply baseline it's optional ############
second_bl_df<- data.table()

for (s in 1:length(subj)){
  sub_df<- subset(zscore_df, subject== subj[s])
  for (r in 1:length(runs)){
    run_df<-subset(sub_df, run== runs[r])
    before_stim<- run_df[1:20,]
    mean<- mean(before_stim$pupil_size,na.rm=TRUE)
    for (stims in 1:length(stimul_list)){
      st<- subset(run_df, stim== stimul_list[stims])
      if (length(st$resample_time)==0){
        print("NO stimul")
      }else{
        for (t in 1:length(trials)){
          pupil_z<-data.table()
          print(trials[t])
          t_df<-subset(st, trial_number== trials[t])
          if (length(t_df$resample_time)==0){
            print("Trail not exists")
          } else{
            t_df<-subset(st, trial_number== trials[t])
            pupil_z$pupil_size<- t_df$pupil_size - mean
            time<- t_df$resample_time
            pupil_z$stim<- stimul_list[stims]
            pupil_z$run<- runs[r]
            pupil_z$subject<- subj[s]
            pupil_z$trial_number<- trials[t]
            pupil_z$time<- time
            second_bl_df<- rbind(second_bl_df, pupil_z)
          }
        }
      }
    }
  }
}

colnames(second_bl_df)[1]<- 'pupil_size'



############# Average INTO subject ##############
avg_df_run1<- data.table()
for (s in 1:length(subj)){
  s_df<- data.table()
  print(subj[s])
  sub_df<- subset(zscore_df, subject== subj[s] & run=="run_1")
  for (stims in 1:length(stimul_list)){
    avg_in_stim<- data.table()
    print(stimul_list[stims])
    st<- subset(sub_df, stim== stimul_list[stims])
    if (length(st$stim)==0){
      print("STIM not exists")
    } else{
      for (t in 1:length(trials)){
        print(trials[t])
        t_df<- subset(st, trial_number== trials[t])
        if (length(t_df$stim)==0){
          print("Trail not exists")
        } else{
            pupil<-t_df$pupil_size
            avg_in_stim<- cbind(avg_in_stim,pupil)
            time<-t_df$time
        } 
      }
      mean<- as.data.frame(rowMeans(avg_in_stim,na.rm=TRUE))
      #mean <- mean %>% filter(row_number() <= n()-1)
      mean$subject<- subj[s]
      mean$run<- "run_1"
      mean$stim<- stimul_list[stims]
      mean<- cbind(mean,time)
      avg_df_run1<- rbind(avg_df_run1,mean)
    }
  }
}


#a<- filter(data, subject_name=="W995")
setnames(avg_df_run1, 'rowMeans(avg_in_stim, na.rm = TRUE)', 'pupil_size')
#setnames(avg_df_run1, 'rowMeans(time_in_trial, na.rm = TRUE)', 'time')

avg_df_run2<- data.table()
for (s in 1:length(subj)){
  s_df<- data.table()
  print(subj[s])
  sub_df<- subset(zscore_df, subject== subj[s] & run=="run_2")
  for (stims in 1:length(stimul_list)){
    avg_in_stim<- data.table()
    print(stimul_list[stims])
    st<- subset(sub_df, stim== stimul_list[stims])
    if (length(st$stim)==0){
      print("STIM not exists")
    } else{
      for (t in 1:length(trials)){
        print(trials[t])
        t_df<- subset(st, trial_number== trials[t])
        if (length(t_df$stim)==0){
          print("Trail not exists")
        } else{
            pupil<-t_df$pupil_size
            avg_in_stim<- cbind(avg_in_stim,pupil)
            time<-t_df$time
        } 
      }
      mean<- as.data.frame(rowMeans(avg_in_stim,na.rm=TRUE))
      #mean <- mean %>% filter(row_number() <= n()-1)
      mean$subject<- subj[s]
      mean$run<- "run_2"
      mean$stim<- stimul_list[stims]
      mean<- cbind(mean,time)
      avg_df_run2<- rbind(avg_df_run2,mean)
    }
  }
}


setnames(avg_df_run2, 'rowMeans(avg_in_stim, na.rm = TRUE)', 'pupil_size')
#setnames(avg_df_run2, 'rowMeans(time_in_trial, na.rm = TRUE)', 'time')


###### Average response to the stimuli BETWEEN subject ######

avg_between_subj_run1<- data.table()
for (stims in 1:length(stimul_list)){
  avg_stim<- data.table()
  st<- subset(avg_df_run1, stim== stimul_list[stims])
  for (s in 1:length(subj)){
    subj_df<- subset(st, subject== subj[s])
    if (length(subj_df$stim)==0){
      print("NO stim")
    }else {
      time<- subj_df$time
      pupil<- subj_df$pupil_size
      avg_stim<- cbind(avg_stim,pupil)
    }
    mean<- as.data.frame(rowMeans(avg_stim,na.rm=TRUE))
    mean$stim_type<- stimul_list[stims]
    mean<- cbind(mean,time)
  }
  avg_between_subj_run1<- rbind(avg_between_subj_run1, mean)
}
colnames(avg_between_subj_run1)[1]<- 'pupil_size'
colnames(avg_between_subj_run1)[3]<- 'time'

avg_between_subj_run2<- data.table()
for (stims in 1:length(stimul_list)){
  avg_stim<- data.table()
  st<- subset(avg_df_run2, stim== stimul_list[stims])
  for (s in 1:length(subj)){
    subj_df<- subset(st, subject== subj[s])
    if (length(subj_df$stim)==0){
      print("NO stim")
    }else {
      time<- subj_df$time
      pupil<- subj_df$pupil_size
      avg_stim<- cbind(avg_stim,pupil)
    }
    mean<- as.data.frame(rowMeans(avg_stim,na.rm=TRUE))
    mean$stim_type<- stimul_list[stims]
    mean<- cbind(mean,time)
  }
  avg_between_subj_run2<- rbind(avg_between_subj_run2, mean)
}
setnames(avg_between_subj_run2, 'rowMeans(avg_stim, na.rm = TRUE)', 'pupil_size')
colnames(avg_between_subj_run2)[3]<- 'time'


write.csv(avg_between_subj_run1,'/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run1.csv' )
write.csv(avg_between_subj_run2,'/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run2.csv' )

avg_between_subj_run1<- read.csv('/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run1.csv')
avg_between_subj_run2<- read.csv('/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run2.csv')

r1<- filter(resample_df, stim=="STIM_aud_vis_puf_" & run=="run_1")
r195<- filter(data, stim=="STIM_aud_vis_puf_" & round=="run_1" & subject_name=="W995")
p<-r195$`pupil,z`
mean(p,na.rm=TRUE)- 3*sd(p,na.rm=TRUE)


aud<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_")
#aud$run<-"run1"


aud1<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_puf_")
#aud1$run<-"run2"


aud<- filter(avg_between_subj, stim_type=="STIM_aud_vis_p")
time<- c(1:length(aud$pupil_size))
aud<- cbind(aud,time)

aud<- filter(avg_between_subj, stim_type=="STIM_vis_")
time<- c(1:length(aud$pupil_size))
aud<- cbind(aud,time)

aud<- filter(avg_between_subj, stim_type=="STIM_vis_puf_")
time<- c(1:length(aud$pupil_size))
aud<- cbind(aud,time)

aud<- filter(avg_between_subj, stim_type=="STIM_aud_vis_")
time<- c(1:length(aud$pupil_size))
aud<- cbind(aud,time)

aud<- filter(avg_between_subj, stim_type=="STIM_aud_vis_puf_")
time<- c(1:length(aud$pupil_size))
aud<- cbind(aud,time)

new<- rbind(aud,aud1)

q<-ggplot(data=new,
          aes(x=time, y=pupil_size, colour=run))+geom_line()+xlab("Time")+ylab("Pupil size ,Z")+
  geom_hline(yintercept = 0,lty="dashed")+geom_vline(xintercept= 3470, lty="dashed")+ylim(-1.5, 1.5)+
  geom_vline(xintercept= 0, lty="dashed", color="red")+xlim(-2000,10000)+
  scale_colour_manual(values = c('forestgreen', 'red'))

q<-ggpar(q,
         font.ytickslab = 30,
         font.xtickslab = 27,
         font.main = 25,
         font.submain = 25,
         font.x = 27,
         font.y = 27)

q


new<- rbind(aud,aud1)

q<-ggplot(data=new,
          aes(x=time, y=pupil_size, colour=stim_type))+geom_line()+xlab("Time")+ylab("Pupil size ,Z")+
  geom_hline(yintercept = 0,lty="dashed")+geom_vline(xintercept= 3470, lty="dashed")+ylim(-2.2, 2)+
  geom_vline(xintercept= 0, lty="dashed", color="red")+xlim(-2000,9000)

q<-ggpar(q,
         font.ytickslab = 30,
         font.xtickslab = 27,
         font.main = 25,
         font.submain = 25,
         font.x = 27,
         font.y = 27)

q



