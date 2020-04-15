/********************************************
***
***	Welcome to the Stata Basic Workshop
***
********************************************/

//The help command gives you information on 
//the syntax of a given command.
//Here we want help on the 'cd' command
help cd

//Change Directories
cd C:\Users\chaycer2\Downloads

//Talk about comments

/***************************************************
***
***	Data Management
***
***************************************************/



**************************1: Getting your data into Stata***********************

//Import CSV files 
//**The import excel command imports .xlsx files
//works on tab and comma delimited  files
import delimited entrance.txt, clear
import delimited flavor2.csv, clear

//Create Stata data file
save flavor2.dta, replace
save entrance.dta, replace

//Open Stata data file
use flavor2.dta, clear

**************************2: Cleaning up your data******************************

*******************Names/Labels
// Visually inspect your data
browse


//Rename variables
rename v1 Name
rename v2 Age
rename v3 Flavor
rename v4 Gender

//Label a variable
//Helps in understanding the variable content
label variable Flavor "Favorite Ice Cream Flavor"

//Label the values of a variable
//Create a name tag for variable values
tab Gender
label define sex 1 "male" 2 "female"
label values Gender sex
tab Gender
label define valFla 1"Chocolate" 2"Vanilla" 3"Mint" 4"Cookie Dough"
label values Flavor valFla



*******************Format

//Turn a number variable into a string variable
tostring Age, replace

//Turn a string variable into a number variable
destring Age, replace

//Browse
br

*******************Recoding and Changing Data

//Recode variables and generate new ones
recode Gender (2=1) (1=0), generate(sex)

// Reverse code 
recode Flavor (1=4)(2=3)(3=2)(4=1), generate Flavor_rv

//Browse 
br Gender GenderDummy

*Compute a teenager dummy variable
//must include , gen to avoid writing over data
recode Age (17/19=1) (20/max=0), gen(teenager)
//Recode variable3
recode Flavor (1=1) (2/max=0), gen(NewVariable)
br Flavor NewVariable

*****Generate*****
gen ImAStudent = 1

gen YoungFem = ((Gender==2) & (Age==18))

//Browse variables 
br YoungFem Gender Age


//Drop a variable (entire columns)
drop teenager

//Drop specific observations (entire rows)
drop if Flavor <= 2




****************************3: Creating variables*******************************

*****Encode*****
//Create a Labeled Numeric Variable from a string variable
webuse hbp2, clear
encode sex, generate(gender)
tab sex gender, nolabel
tab sex gender


*****Extensions to Generate*****
//Within-observation Across-variables Data Summaries
webuse egenxmpl4, clear

egen rmean = rowmean(a b c)  //row mean
egen rmed = rowmedian(a b c) //row median

egen colmean = mean(a)  //column mean
egen colmed = median(a) //column median

list
help egen




/***************************************************
***
***	Descriptive Statistics
***
***************************************************/
*Import Stata data file
use Entrance, clear

rename v1 id
rename v2 gender
rename v3 age
rename v4 gpa
rename v5 act
rename v6 sat
rename v7 actsat
rename v8 athlete
rename v9 race

label variable id "Personal ID"
label variable gender "What is your biological sex?"
label variable age "What is your age?"
label variable gpa "What is your GPA (out of 4.0)?"
label variable act "What was your ACT score?"
label variable sat "What was your SAT score?"
label variable actsat "What is your standardized ACT SAT score?"
label variable athlete "Are you an Athlete? (1=yes)"
label variable race "What is your race?"
label define valRace 0"White" 1"Black" 2"Hispanic" 3"Asian"
label values race valRace

*Summarize gpa
sum gpa
//Summarize gpa with details
sum gpa, d

*Obtain the mean of gpa
mean gpa id age

//Create a frequency table of gpa
tab gpa
*Create a two-way frequency table
tab gender race
//But this doesn't work with more than 2
tab gender race gpa

//We must install an optional package 'fre'
ssc install fre
//Create a frequency table of gender
fre gender gpa age

//Create a histogram of gpa
hist gpa
hist gpa, bin(10) // Create a higher precision histogram
help hist

//Create a scatter plot of gpa and act
scatter gpa act

//Create a correlation matrix with options
pwcorr gpa act, sig star(.05) // Print significance value and stars
pwcorr gpa act age sat actsat, star(0.05) // Print coeffs with stars
pwcorr gpa act age sat actsat, sig print(.05) // Only print significant


//Simple linear regression
reg gpa act age
//Simple linear regression with beta values
reg gpa act age, beta


/***************************************************
***
***	Mean Difference tests
***
***************************************************/
ttest gpa, by(athlete) // Is the mean gpa different for athletes and non-athletes?

oneway gpa race, b		// Is there a significant mean difference across more than two groups?




//Checkpoint
preserve
//Do something to your data
sort Gender
by Gender: pwcorr Flavor Age, sig star(.05)
//Go back
restore


















