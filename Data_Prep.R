#Calculate: Cumulative Stats, League Averages,
library(zoo)

Master_Table_Team <-
    Extract %>%
  group_by(Season, Team, Opp, Loc, Date)

Master_Table_Team <- summarise(Master_Table_Team,
                  Shots = sum(S),
                   PIM = sum(PIM))

Master_Table_Team <- Master_Table_Team %>% arrange(Season, Team, Date)

Master_Table_Team <- Master_Table_Team  %>%
                      mutate(temp = (1)) %>%
                        group_by(Season, Team) %>%
                        mutate(Game_No = cumsum(temp))

# Create moving averages of different statistics
Master_Table_Team <- arrange(Master_Table_Team,Season,Team,Game_No) %>%
  mutate(

    Shots_ma03 = round(rollapply(Shots,3,mean,align='right',fill=NA),1),
    Shots_ma05 = round(rollapply(Shots,5,mean,align='right',fill=NA),1),
    Shots_ma10 = round(rollapply(Shots,10,mean,align='right',fill=NA),1),
    Shots_ma20 = round(rollapply(Shots,20,mean,align='right',fill=NA),1),

    PIM_ma03 = round(rollapply(PIM,3,mean,align='right',fill=NA),1),
    PIM_ma05 = round(rollapply(PIM,5,mean,align='right',fill=NA),1),
    PIM_ma10 = round(rollapply(PIM,10,mean,align='right',fill=NA),1),
    PIM_ma20 = round(rollapply(PIM,20,mean,align='right',fill=NA),1)
         )

# function to lag the moving average by one observation to remove current line from calculation
lag.mav <- function(Table_Input,Team_Name,Season_No,Stat_Input){

  colnumber = which(colnames(Table_Input)==Stat_Input)

  temp_vector <- Table_Input[[colnumber]][Table_Input$Team==Team_Name & Table_Input$Season==Season_No]
  Lag_Stat <- c(NA,temp_vector)[1:length(temp_vector)]

  Table_Input[[colnumber]][Table_Input$Team==Team_Name & Table_Input$Season==Season_No] <- Lag_Stat
  Table_Output <- Table_Input

  return(Table_Output)
}
lag.mav.opp <- function(Table_Input,Team_Name,Season_No,Stat_Input){

  colnumber = which(colnames(Table_Input)==Stat_Input)

  temp_vector <- Table_Input[[colnumber]][Table_Input$Opp==Team_Name & Table_Input$Season==Season_No]
  Lag_Stat <- c(NA,temp_vector)[1:length(temp_vector)]

  Table_Input[[colnumber]][Table_Input$Opp==Team_Name & Table_Input$Season==Season_No] <- Lag_Stat
  Table_Output <- Table_Input

  return(Table_Output)
}

season_list = c("Regular_1819") ## select desired seasons to lag the stats
stat_list = c("Shots_ma03", "Shots_ma05","Shots_ma10","Shots_ma20",
              "PIM_ma03", "PIM_ma05","PIM_ma10","PIM_ma20")

## make sure date properly ordered by season / team first
## looping for every team / season / desired statistic
for (i in all.teams ){
  for (j in season_list ){
    for (k in stat_list ){
      Master_Table_Team <- lag.mav( Master_Table_Team , i , j , k )
    }
  }
}

## Calculating stats on the opposition
Master_Table_Team <- arrange(Master_Table_Team,Season,Opp,Date) %>% ungroup() %>%
                    group_by(Season, Opp)

## opposition stats
Master_Table_Team <- Master_Table_Team %>%
  mutate(
    Shots_allowed_ma03 = round(rollapply(Shots,3,mean,align='right',fill=NA),1),
    Shots_allowed_ma05 = round(rollapply(Shots,5,mean,align='right',fill=NA),1),
    Shots_allowed_ma10 = round(rollapply(Shots,10,mean,align='right',fill=NA),1),
    Shots_allowed_ma20 = round(rollapply(Shots,20,mean,align='right',fill=NA),1)
  )

season_list = c("Regular_1819") ## select desired seasons to lag the stats
stat_list = c("Shots_allowed_ma03", "Shots_allowed_ma05","Shots_allowed_ma10","Shots_allowed_ma20")

for (i in all.teams ){
  for (j in season_list ){
    for (k in stat_list ){
      Master_Table_Team <- lag.mav.opp(Master_Table_Team , i , j , k )
    }
  }
}

Master_Table_Team <- arrange(Master_Table_Team,Season,Team,Date)

