## **Historical trends related to the MLS Designated Player**

### **Introduction**
___
The MLS Designated Player Rule was established in 2007 to empower teams in becoming more competitive for high quality international players. Specifically, it allowed teams to sign these players outside of the established league salary cap. In theory, getting these "international stars" would help increase a team's visibility and success, ultimately making that team more profitable. However, the effect of investing in Designated Players remains unstudied -- are they associated with increases in attendance, or improvements in league finish? Ultimately, are they worth the salary hit?

The data and code in this repository empower a user to begin answering these questions via visual inference. Ultimately, they produce a visualization of relevant trends related to the designated player for each team since the institution of the Rule in 2007. Specifically, a user can observe the number of designated players, average regular season attendance (a measure of team visibility), total regular season points (a measure of team success), and percent of salary dedicated to designated players (a measure of how designated players effect a team). 

The plots should be viewed with a specific hypothesis in mind. An increase in designated players should be associated with an increase in attendance and points. Of course, an increase in designated players means a larger proportion of team salary for designated players, but ideally a team should be able to find "value" by minimizing salary hit while maximizing attendance and points. Is this the case for every team? For any teams? Only during certain years? Explore the plots to find out!

[Note: a side benefit of this project is offering some well-organized, R-ready MLS data -- this is very, very hard to come by! Even if a user isn't ultimately interested in the provided visualization functionality, it is my hope that this repository can be a source of data for future analysts who seek to do Designated Player related work in R.]

### **Files**
___
In the `R` folder, two scripts can be found.

  * `01_Data_Scraping` contains the functions used to scrape and clean necessary data for the visualizations. Attendances, designated players, and league tables were scraped from the web and clean; salaries were downloaded from existing .csv files and cleaned.
  
  * `02_Plotting` contains the code for producing the visualizations.

In the `Data` folder, two subfolders can be found.

  * `R_Data` contain cleaned data frames for average annual attendance by team, a history of designated players in the MLS, final league tables, and salary information for all MLS players [all data covers the 2007 to 2017 seasons]. Because three of these four tables were formed by web scrapes, it is crucial that static files were saved as intermediates!
  
  * `Salary_Files` contain `.csv` files for MLS salaries between 2007 and 2017. These were used to organize the salary information for this project.



