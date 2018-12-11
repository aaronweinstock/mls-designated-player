# install.packages("ggplot2")
# install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

dps = readRDS("Data/R_Data/designated_players.rds")
attendance = readRDS("Data/R_Data/average_attendance.rds")
leaguetables = readRDS("Data/R_Data/league_tables.rds")
sal = readRDS("Data/R_Data/salaries.rds")

# Team names, abbreviations, year of entry into MLS (2007 if before 2007)
mlsteams = function(){
  eastern = c("ATL"="Atlanta United","CHI"="Chicago Fire",
              "CLB"="Columbus Crew","DCU"="DC United",
              "MTL"="Montreal Impact","NE"="New England Revolution",
              "NYC"="NYCFC","NYRB"="New York Red Bulls",
              "ORL"="Orlando City SC","PHI"="Philadelphia Union",
              "TOR"="Toronto FC")
  east_enter = c(2017, 2007, 2007, 2007, 2012, 2007, 2015, 2007, 2015, 2010, 2007)
  western = c("COL"="Colorado Rapids","DAL"="FC Dallas",
              "HOU"="Houston Dynamo","LAG"="LA Galaxy",
              "MIN"="Minnesota United","POR"="Portland Timbers",
              "RSL"="Real Salt Lake","SJ"="San Jose Earthquakes",
              "SEA"="Seattle Sounders","KC"="Sporting KC",
              "VAN"="Vancouver Whitecaps")
  west_enter = c(2007, 2007, 2007, 2007, 2017, 2011, 2007, 2008, 2009, 2007, 2011)
  df = data.frame(team = c(names(eastern), names(western)),
                  full = c(unname(eastern), unname(western)),
                  enter = c(east_enter, west_enter))
  return(df)
}
mls = mlsteams()

# Normalization function for plotting
minmax = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Visualize Results!
# Use an abbreviation from 'mls' as 'team', and either 'together' 
# or 'separate' for 'as'. If 'separate', 4 separate plots will
# be produced. If 'together', values will be minmax-normalized and
# put on the same plot
comparison_plots = function(team, as){
  # Basic info
  by_team = dps[dps$team == team,]
  by_year = colSums(by_team[,4:14])
  start_year = mls$enter[mls$team == team]
  years_active = start_year:2017
  full_name = mls$full[mls$team == team]
  # For plot of number of designated players
  df1 = data.frame(year = years_active, 
                  dps = by_year[(12-length(years_active)):11])
  # For plot of average attendance
  df2 = data.frame(year = years_active,
                  att = attendance[[team]][attendance$Season %in% years_active])
  # For plot of points
  df3 = data.frame(year = years_active,
                   pts = leaguetables[leaguetables$team == team,]$PTS)
  # For plot of salaries
  percent_dp = unlist(lapply(years_active, function(x){
    yt = sal[sal$team == team & sal$year == x,]
    return(sum(yt$guaranteed[yt$player %in% dps$player])/sum(yt$guaranteed))
  }))
  df4 = data.frame(year = years_active,
                   psal = percent_dp)
  # Separate
  if(as=="separate"){
    p1 = ggplot(data = df1) +
      geom_line(aes(x=year, y=dps)) +
      scale_x_continuous(name = "year",
                         breaks = years_active,
                         labels = as.character(years_active)) +
      scale_y_continuous(name = "designated players") +
      labs(title = "Number of designated players") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p2 = ggplot(data = df2) +
      geom_line(aes(x=year, y=att)) +
      scale_x_continuous(name = "year",
                         breaks = years_active,
                         labels = as.character(years_active)) +
      scale_y_continuous(name = "average attendance") +
      labs(title = "Average regular season attendance") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p3 = ggplot(data = df3) +
      geom_line(aes(x=year, y=pts)) +
      scale_x_continuous(name = "year",
                         breaks = years_active,
                         labels = as.character(years_active)) +
      scale_y_continuous(name = "points") +
      labs(title = "Final league points") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p4 = ggplot(data = df4) +
      geom_line(aes(x=year, y=psal)) +
      scale_x_continuous(name = "year",
                         breaks = years_active,
                         labels = as.character(years_active)) +
      scale_y_continuous(name = "percent of salary taken \n by designated players") +
      labs(title = "Percent salary for designated players") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    grid.arrange(p1, p2, p3, p4, ncol=2,
                 top = paste("Trends related to the designated player for", full_name))
  }
  if(as=="together"){
    df = data.frame(year = rep(years_active, times=4),
                    val = c(minmax(df1[,2]), minmax(df2[,2]), minmax(df3[,2]), minmax(df4[,2])),
                    type = rep(c("designated players","average attendance",
                                 "reg. season points","% salary for d.p."), 
                               each=length(years_active)))
    ggplot(data = df) +
      geom_line(aes(x = year, y = val, col = type)) +
      scale_x_continuous(name = "year",
                         breaks = years_active,
                         labels = as.character(years_active)) +
      scale_y_continuous(name = "minmax normalized statistics") +
      scale_color_manual(values=c("red","blue","green","orange")) +
      labs(title = paste("Trends related to the designated player for", full_name)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

# Example run: separate plots for the Portland Timbers
comparison_plots("POR","separate")
