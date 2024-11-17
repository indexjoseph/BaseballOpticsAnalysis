###IMPORT THE Track_Combo DATASET###


###INSTALL AND READ-IN THE NECESSARY R PACKAGES###
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

###CREATE DIMENSIONS FOR A STRIKE ZONE AND BUFFER ZONE (EDGE)###
Lower=1.5
Upper=3.3773
Side=.83083

Lower_Buffer=1.3775
Upper_Buffer=3.5
Side_Buffer=.9975

###LINK THESE DIMENSIONS TO OBJECTS THAT CAN BE PLOTTED###
###STRIKE ZONE###
x <- c(-Side,Side,Side,-Side,-Side)
z <- c(Lower,Lower,Upper,Upper,Lower)
sz <- data.frame(x,z)

###BUFFER ZONE###
x <- c(-Side_Buffer,Side_Buffer,Side_Buffer,-Side_Buffer,-Side_Buffer)
z <- c(Lower_Buffer,Lower_Buffer,Upper_Buffer,Upper_Buffer,Lower_Buffer)
sz_Buffer <- data.frame(x,z)

###LINE FOR HOME PLATE###
x <- c(-8.5/12,8.5/12)
z <- c(0)
Plate <- data.frame(x,z)

###FILTER DATA TO EXAMINE CALLED PITCHES THROWN BY T_A PITCHERS AGAINST RIGHT-HANDED BATTERS ON A SPECIFIC DATE###
Pitch_Calls=Track_Combo%>%filter(Date=="2/9/2023" & PitchCall=="BallCalled" | PitchCall=="StrikeCalled" &
                                   BatterSide=="Right" & PitcherTeam=="T_A")

###PLOT THE PITCH LOCATIONS FROM THE PITCHER'S PERSPECTIVE; COLOR AND SHAPE THE POINTS BASED ON UMPIRE CALL###
ggplot(Pitch_Calls)+geom_point(aes(x=PlateLocSide,y=PlateLocHeight,color=PitchCall,shape=PitchCall),alpha=0.5,size=4)+
  geom_path(data = sz,aes(x=x,y=z))+
  geom_path(data=sz_Buffer,aes(x=x,y=z),linetype=5)+
  geom_path(data=Plate,aes(x=x,y=z))+coord_equal()+
  scale_x_continuous(limits=c(-3,3))+
  scale_y_continuous(limits=c(0,5))+
  scale_shape_manual(values = c("StrikeCalled" = 4, "BallCalled" = 16))+
  labs(title="Pitch Locations from Pitcher's Perspective",subtitle="Balls and strikes called against right-handed batters",x=element_blank(),y=element_blank())+
  theme(legend.position="bottom")+theme_bw()