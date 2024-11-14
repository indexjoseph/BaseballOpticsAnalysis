###IMPORT THE Track_Combo DATASET###

###INSTALL AND READ-IN NECESSARY R PACKAGES###
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

###FILTER OUT A SPECIFIC PITCHER TO EXAMINE FASTBALL SPEED IN A SPECIFIC GAME###
Filtered_Pitcher=Track_Combo%>%filter(PitcherId==1000103199,TaggedPitchType=="Fastball",Date=="11/6/2022")%>%
  mutate(Fast_Count=row_number())
###THE MUTATE FUNCTION CREATED A COLUMN COUNTING WHAT NUMBER FASTBALL WAS BEING THROWN IN THAT GAME###

###CREATE A LINE CHART SHOWING THE PITCHER'S RELEASE SPEED BY PITCH NUMBER###
Filtered_Pitcher%>%ggplot(aes(x=Fast_Count,y=RelSpeed))+geom_line()+
  geom_point()+scale_x_continuous()+scale_y_continuous()+
  labs(title="Fastball Release Speed Per Pitch",
       subtitle="Pitcher 1000103199 on 11/6/2022",
       x="Fastball Number",
       y="Release Speed")+
  theme_bw()+
  theme(
    axis.text=element_text(size=11),
    axis.title.x=element_text(size=11),
    axis.title.y=element_text(size=11))