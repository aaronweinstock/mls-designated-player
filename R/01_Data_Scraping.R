# install.packages("htmltab")
# install.packages("data.table)
library(htmltab)
library(data.table)

# Historical average attendances scraped from Wikipedia at
# https://en.wikipedia.org/wiki/Major_League_Soccer_attendance
avg_attendances = function(){
  x = htmltab("https://en.wikipedia.org/wiki/Major_League_Soccer_attendance",
              which = 6,
              rm_nodata_cols = FALSE)
  x = data.frame(apply(x, 2, function(y){
    n = which(!is.na(y))
    y[n] = as.numeric(gsub(",", "", y[n]))
    return(as.numeric(y))
  }))
  return(x)
}

# Historical designated player information scraped from Wikipedia at
# https://en.wikipedia.org/wiki/Designated_Player_Rule
designated_players = function(){
  # abbrevs = names(avg_attendances())
  abbrevs = names(avg_attendances())[-1]
  abbrevs[5:6] = c("DCU","DAL")
  tabnums = c(2:3, 6:11, 13:26)
  dps = lapply(tabnums, function(x){
    y = htmltab("https://en.wikipedia.org/wiki/Designated_Player_Rule",
                which=x,
                rm_nodata_cols = FALSE)
    y = data.frame(y)
    y$team = abbrevs[which(tabnums == x)]
    return(y)
  })
  dps = data.frame(rbindlist(dps))
  num_years = lapply(dps$Years.as.DP, function(x){
    spl = unlist(strsplit(x, ""), use.names=FALSE)
    if(length(spl) <= 5){
      y1 = as.numeric(paste(spl[1:4], collapse=""))
      return(data.frame(t(2007:2017 %in% y1)))
    } else if(length(spl) == 9){
      y1 = as.numeric(paste(spl[1:4], collapse=""))
      y2 = as.numeric(paste(spl[6:9], collapse=""))
      return(data.frame(t(2007:2017 %in% y1:y2)))
    } else if(length(spl) == 16){
      y1 = as.numeric(paste(spl[1:4], collapse=""))
      y2 = as.numeric(paste(spl[6:9], collapse=""))
      y3 = as.numeric(paste(spl[12:15], collapse=""))
      return(data.frame(t(2007:2017 %in% c(y1:y2, y3))))
    } else if(length(spl) == 20){
      y1 = as.numeric(paste(spl[1:4], collapse=""))
      y2 = as.numeric(paste(spl[6:9], collapse=""))
      y3 = as.numeric(paste(spl[12:15], collapse=""))
      y4 = as.numeric(paste(spl[17:20], collapse=""))
      return(data.frame(t(2007:2017 %in% c(y1:y2, y3:y4))))
    }
  })
  in_each = data.frame(rbindlist(num_years))
  dps = data.frame(cbind(dps, in_each))
  dps = dps[,-3]
  names(dps) = c("player","previous_club","team",
                 "in2007","in2008","in2009","in2010","in2011","in2012",
                 "in2013","in2014","in2015","in2016","in2017")
  dps$player = iconv(dps$player, to='ASCII//TRANSLIT')
  post = unlist(lapply(1:nrow(dps), function(x){
    sum(as.numeric(dps[x, 4:14]))
  }))
  dps = dps[which(post != 0),]
  return(dps)
}

# Historical salary information downloaded from kaggle at
# https://www.kaggle.com/crawford/us-major-league-soccer-salaries
# Original information in the data folder
salaries = function(){
  sal = lapply(2007:2017, function(x){
    y = read.csv(paste("Data/Salary_Files/mls-salaries-", x, ".csv", sep=""), 
                 stringsAsFactors = FALSE)
    y$player = paste(y$first_name, y$last_name)
    y$year = x
    y = y[,c(8,7,1,4,5,6)]
    names(y) = c("year","player","team","position","base","guaranteed")
    return(data.frame(y))
  })
  sal = data.frame(rbindlist(sal))
  sal$team[sal$team == "DC"] = "DCU"
  sal$team[sal$team == "LA"] = "LAG"
  sal$team[sal$team == "MNUFC"] = "MIN"
  sal$team[sal$team == "NY"] = "NYRB"
  sal$team[sal$team == "NYCFC"] = "NYC"
  sal$team[sal$team == "TFC"] = "TOR"
  sal = sal[-which(sal$team == "None" | sal$team == "" |
                     sal$team == "Pool" | sal$team == "POOL" |
                     sal$team == "CHV" | sal$team == "LAFC"),]
  sal$player = trimws(sal$player)
  sal$player[grep("Ljun",sal$player)] = "Freddie Ljungberg"
  sal$player[grep("Rios",sal$player)] = "Egidio Arevalo Rios"
  sal$player[grep("Gilberto Junior",sal$player)] = "Gilberto"
  sal$player[grep("Rosario",sal$player)] = "Dwayne De Rosario"
  sal$player[grep("Denilson",sal$player)] = "Denilson"
  sal$player[grep("Giovani Dos Santos",sal$player)] = "Giovani dos Santos"
  sal$player[grep("Kleb",sal$player)] = "Kleberson"
  sal$player[grep("Rubio",sal$player)] = "Diego Rubio"
  sal$player[grep("Mista",sal$player)] = "Mista"
  return(sal)
}

# Historical league tables scraped from MLS at
# https://www.mlssoccer.com/standings/mls/(year)
tables = function(){
  tab = lapply(2007:2017, function(x){
    print(paste("Working on", x))
    conferences = lapply(1:2, function(y){
      conf = htmltab(paste("https://www.mlssoccer.com/standings/mls/", x, sep=""),
                     which=y,
                     header=1,
                     rm_nodata_cols=FALSE)
      conf = conf[,-c(1,5,13,15)]
      names(conf) = conf[1,]
      conf = conf[-1,]
      conf[,2:10] = sapply(conf[,2:10], as.numeric)
      conf$Club[conf$Club == "TORToronto FC"] = "TOR"
      conf$Club[conf$Club == "NYCNew York City FC"] = "NYC"
      conf$Club[conf$Club == "CHIChicago Fire"] = "CHI"
      conf$Club[conf$Club == "ATLAtlanta United FC"] = "ATL"
      conf$Club[conf$Club == "CLBColumbus Crew SC"] = "CLB"
      conf$Club[conf$Club == "CLBColumbus Crew"] = "CLB"
      conf$Club[conf$Club == "RBNYNew York Red Bulls"] = "NYRB"
      conf$Club[conf$Club == "RBNYNY Red Bulls"] = "NYRB"
      conf$Club[conf$Club == "NENew England Revolution"] = "NE"
      conf$Club[conf$Club == "PHIPhiladelphia Union"] = "PHI"
      conf$Club[conf$Club == "MTLMontreal Impact"] = "MTL"
      conf$Club[conf$Club == "ORLOrlando City SC"] = "ORL"
      conf$Club[conf$Club == "DCD.C. United"] = "DCU"
      conf$Club[conf$Club == "PORPortland Timbers"] = "POR"
      conf$Club[conf$Club == "SEASeattle Sounders FC"] = "SEA"
      conf$Club[conf$Club == "VANVancouver Whitecaps FC"] = "VAN"
      conf$Club[conf$Club == "HOUHouston Dynamo"] = "HOU"
      conf$Club[conf$Club == "SKCSporting Kansas City"] = "KC"
      conf$Club[conf$Club == "SKCKansas City Wizards"] = "KC"
      conf$Club[conf$Club == "SJSan Jose Earthquakes"] = "SJ"
      conf$Club[conf$Club == "DALFC Dallas"] = "DAL"
      conf$Club[conf$Club == "RSLReal Salt Lake"] = "RSL"
      conf$Club[conf$Club == "MINMinnesota United FC"] = "MIN"
      conf$Club[conf$Club == "COLColorado Rapids"] = "COL"
      conf$Club[conf$Club == "LALos Angeles Galaxy"] = "LAG"
      conf$Club[conf$Club == "LALA Galaxy"] = "LAG"
      conf$Club[conf$Club == "CHVChivas USA"] = "CHV"
      names(conf)[c(1,11,12)] = c("team","HOME","AWAY")
      conf$year = x
      return(conf)
    })
    conferences = rbindlist(conferences)
    return(conferences)
  })
  tab = data.frame(rbindlist(tab))
}

# Finally, save the data we're going to use going forward as .rds files
att = avg_attendances()
dps = designated_players()
sal = salaries()
tab = tables()
saveRDS(att, "Data/R_Data/average_attendance.rds")
saveRDS(dps, "Data/R_Data/designated_players.rds")
saveRDS(sal, "Data/R_Data/salaries.rds")
saveRDS(tab, "Data/R_Data/league_tables.rds")
