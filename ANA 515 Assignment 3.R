    installed.packages()
    getwd()
    library(tidyverse)

#Reading and saving the csv as data frame "wrs" for Weather Related Storms
    

        wrs <- read_csv("C:/Users/rashm/Desktop/ANA 515 Fundamentals of Data Storage/StormEvents_details-ftp_v1.0_d1993_c20220425.csv/StormEvents_details-ftp_v1.0_d1993_c20220425.csv")

    view(wrs)
    head(wrs)
    colnames(wrs)

    
#Limiting wrs by creating a new data frame "wrs_new" to the following columns: 

# "BEGIN_DATE_TIME" 
# "END_DATE_TIME"
# "EPISODE_ID"
# "EVENT_ID"
# "STATE" 
# "STATE_FIPS"
# "CZ_NAME"
# "CZ_TYPE"
# "CZ_FIPS" 
# "EVENT_TYPE"
# "SOURCE"
# "BEGIN_LAT"         
# "BEGIN_LON"          
# "END_LAT"            
# "END_LON"


        wrs_new <- wrs[c(7,8,9,10,13,14,15,16,18,20,27,45,46,47,48)]

    view(wrs_new)
    head(wrs_new)

    
#Arranging the data by state name
    
    
    library(dplyr)
        SortedByState <- arrange(wrs_new, desc(STATE))
        

#Changing state and county names to title case 
        

    install.packages("stringr")
    library(stringr)
        StateTitleCase <- str_to_title(wrs_new$STATE)
        CountyTitleCase <- str_to_title(wrs_new$CZ_NAME)
        

#Limiting to the events listed by county FIPS (CZ_TYPE of "C") 
        

        CFilter <- filter(wrs_new, CZ_TYPE=="C")
        

#Removing CZ_TYPE column
        

        RemoveColumn <- select(wrs_new, -CZ_TYPE)
        

#Padding the state and county FIPS with a "0" at the beginning
        

        StatePadding <- str_pad(wrs_new$STATE_FIPS, width=3, side="left", pad="0")
        CountyPadding <- str_pad(wrs_new$CZ_FIPS, width = 3, side = "left", pad = "0")
        

#Uniting the two columns to make one FIPS column with the 5-digit county FIPS code.
        

    library(tidyr)
        NewCol <- unite(wrs_new, "FIPS", c("STATE_FIPS","CZ_FIPS"))
        


#Changing all the column names to lower case


        Lower <- rename_all(wrs_new, tolower)
        
        
#Creating a data frame with these three columns: state name, area, and region with base R state data
        
        
    data("state")
        State_Info <- data.frame(state = state.name, region = state.region, area = state.area)
    

#Creating a data frame with the number of events per state in the year of my birth
    
        
    table(wrs_new$STATE)
      StateFreq <- data.frame(table(wrs_new$STATE))
    head(StateFreq)

    
#Merging the state information data frame with state freq data frame
        
        
        StateFreqNew <- rename(StateFreq, c("state"="Var1"))
    head(StateFreqNew)
        StateInfoFreq <- merge(x=State_Info, y=StateFreqNew, by.x="state",.y="state")
    head(StateInfoFreq)

    
#Matching the letter cases of state info and state freq
    
    
        StateInfoNew <- (mutate_all(State_Info, toupper))
    head(StateInfoNew)
        StateInfoFreq <- merge(x=StateInfoNew, y=StateFreqNew, by.x="state",.y="state")
    head(StateInfoFreq)


#Converting the values for "area" from character to numeric

        AreaNew <- as.numeric(StateInfoFreq$area)
    
#Loading a custom font from my local machine       
    
    windowsFonts(CorpoS=windowsFont("CorpoS"))

#Plot generation  
    
    
            StormPlot <- ggplot(StateInfoFreq, aes(x = AreaNew, y = Freq)) + geom_point(aes(color=region)) + labs(x="Land Area (Square Miles)", y="Storm Events in 1993") + theme(text=element_text(family="CorpoS", size=14)) 


            StormPlot

