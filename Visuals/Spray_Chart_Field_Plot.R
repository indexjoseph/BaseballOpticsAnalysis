###IMPORT THE Track_Combo DATASET###

###INSTALL AND READ-IN NECESSARY R PACKAGES###
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

###CONVERT THE BEARING VALUES AND COMBINE WITH DISTANCE TO BE PLOTTED AS HIT COORDINATES ON A FIELD###   
Track_Adjusted=Track_Combo%>%mutate(hc_x=sin(Bearing*0.0174533)*Distance,
                                    hc_y=cos(Bearing*0.0174533)*Distance)%>%
    filter(!is.na(hc_x) & !is.na(hc_y))

###FILTER OUT SPECIFIC PITCHER AND PLAY RESULTS (HITS & OUTS)###
Track_Adjusted=Track_Adjusted%>%filter(PitcherId==1000036206,
                                       TaggedHitType!="Bunt",TaggedHitType!="Undefined",
                                       PlayResult!="Undefined",PlayResult!="Error",PlayResult!="FieldersChoice")

###PLOT A BASIC BASEBALL FIELD WITH FOUL LINES, THE FOUR PLATES, AND THE PITCHER'S MOUND REPRESENTED ###
ggplot(Track_Adjusted, aes(x=hc_x, y=hc_y))+
  geom_point(aes(color = TaggedPitchType, shape = PlayResult), alpha=.7,size = 3)+
  annotate("point", x = 0, y = 0, color = "black", shape = 23, size = 4)+ #HOME PLATE DIAMOND
  annotate("point", x = 62, y = 62, color = "black", shape = 15, size = 2.5)+ #1B SQUARE 
  annotate("point", x = 0, y = 127, color = "black", shape = 15, size = 2.5)+ #2B SQUARE
  annotate("point", x = -62, y = 62, color = "black", shape = 15, size = 2.5)+ #3B SQUARE
  annotate("point", x = 0, y = 60.5, color = "black", shape = 15, size = 2)+ #PITCHER'S MOUND SQUARE
  annotate("segment", x = 62, y = 62, xend = 0, yend = 127, color = "black",size=.9)+ #LINE FROM FIRST TO SECOND
  annotate("segment", x = 0, y = 127, xend = -62, yend = 62, color = "black",size=.9)+ #LINE FROM SECOND TO THIRD
  annotate("segment", x = 0, y = 0, xend = 300, yend = 300, color = "black",size=.9)+ #1B FOUL LINE
  annotate("segment", x = 0, y = 0, xend = -300, yend = 300, color = "black",size=.9)+ #3B FOUL LINE
  coord_equal() + ylim(-20, 400) + xlim(-300, 300)+
  labs(title = "Spray Chart (Hits & Outs)", x = "Feet", y = "Feet",
       subtitle = "Pitcher 1000036206")+
  coord_fixed()+
  theme_minimal()
###THE PLOTTED POINTS ARE SHAPED BY THE PLAY RESULT (HIT TYPES AND OUTS) AND COLORED BY THE PITCH TYPE###