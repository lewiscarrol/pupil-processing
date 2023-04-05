
library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggpubr)


avg_between_subj_run1<- read.csv('/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run1.csv')
avg_between_subj_run2<- read.csv('/net/server/data/Archive/piansrann/pultsinak/pupil/avg_between_subj_run2.csv')

########## Plot to compare stimulus CS+/CS- into one run !!! ###########
aud<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_")
#aud$run<-"run1"
aud1<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_puf_")
#aud1$run<-"run2"
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


######## Plot to compare stim in different runs#######

aud<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_")
aud$run<-"run1"
aud1<- filter(avg_between_subj_run1, stim_type=="STIM_aud_vis_puf_")
aud1$run<-"run2"

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
