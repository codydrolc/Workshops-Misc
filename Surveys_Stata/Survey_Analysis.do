********************************************************************************
*			          Analyzing Survey Data Using Stata                        *
************* Created: 23 January 2017                                         *
************* Edited: 14 February 2017                                         *
*                                                                              *
********************************************************************************

* Set working directory
cd "directory goes here"

/*
We will utilize data from the 2012 American National Election Study. This data
source is common in the study of American politics and offers a high-end source
to examine how survey data is coded and weighted.

These data are provided for you but I think it is important for you to know how 
to find data using ICPSR's data repository. I will also provide a brief 
discussion of Harvard's Dataverse.

ICPSR: icpsr.umich.edu

Harvard: dataverse.harvard.edu
*/

* Call data
use anes2012, clear

/*
Post-stratification weight						weight_full
Primary sampling unit (PSU)						psu_full
Stratification									strata_full
Party Identification							pid_x
2008 Presidential Vote							interest_whovote2008
2012 Presidential Vote							presvote2012_x
Male											male
Obama Feeling Therm								ft_dpc
Romney Feeling Therm							ft_rpc
Hillary Feeling Therm							ft_hclinton
Family Income									income
Minority status									minority
Southern sample region							south
Marital status									married
Handling of economy								economy

Note: some variable names were changed for ease of use
*/

********************************************************************************	
*** Using Stata's survey commands

* "svyset" tells Stata about survey elements in the data (e.g. weights)

svyset psu_full [pw = weight_full], strata(strata_full)

*** Descriptive statistics

svy: mean pid_x male // we also want the standard deviation
estat sd

* Compared to unweighted 
sum pid_x male

*** Cross-tabulations 

svy: tab male

* Actual counts
svy: tab male, count

*** 2008 and 2012 votes

* 2008
tab interest_whovote2008 // did Obama really kill it in '08?
* Weighted 
svy: tab interest_whovote2008

* 2012
tab presvote2012_x // and in 2012?
* Weighted
svy: tab presvote2012_x

*** Correlations
* findit corr_svy

corr_svy ft_dpc ft_rpc ft_hclinton [pw = weight_full], sig

*** Sub-population

* Proper
svy, subpop(male): mean ft_hclinton

* This will give you the same thing and follows familiar syntax
svy: mean ft_hclinton if male == 1

********************************************************************************
*** Basic graphics

* Histograms cannot be created using a probability weight
* Create a frequency weight out of a probability weight (not a perfect solution)

gen frequency = int(weight_full)

* Basic histogram without weight
hist ft_dpc, percent xtitle("Obabma Feeling Thermometer: Unweighted") ///
	ylabel(0(5)20) scheme(s2mono) name(hist1, replace)

* Same histogram with weight
hist ft_dpc [fw = frequency], percent xtitle("Obabma Feeling Thermometer: Weighted") ///
	ylabel(0(5)20) scheme(s2mono) name(hist2, replace)

* Put them side by side
graph combine hist1 hist2

*** Box plot with pw -- They're ugly, I know but bear with me

* Without weight
graph box ft_hclinton, by(male) ytitle("Hillary Clinton Feeling Thermometer") ///
	scheme(s2mono) note("Unweighted") name(box1, replace)

* With weight
graph box ft_hclinton [pw = weight_full], by(male) ytitle("Hillary Clinton Feeling Thermometer") /// 
	scheme(s2mono) note("Weighted") name(box2, replace)

* Side by side
graph combine box1 box2

********************************************************************************
*** Weighted data analysis
* Keep in mind that not all commands accept the svy prefix
* See the help file if you want to know more // help svy_estimation

*** Estimate Hillary Clinton feeling thermometer 

* Unweighted
reg ft_hclinton male pid_x income minority married south

* Weighted
svy: reg ft_hclinton male pid_x income minority married south
* Alternatively
reg ft_hclinton male pid_x income minority married south [pw = weight_full]

*** Predict 2012 presidential vote
* This model is sort of like predicting birth rates from preganancy statistics,
* but it will serve a demonstrative purpose

* Using svy prefix
svy: logit presvote2012_x pid_x economy income minority south ft_rpc ft_dpc

* What if I want options and model fit statistics? -- Drop the svy prefix
* findit spost -- you want the latest version

logit presvote2012_x pid_x economy income minority south ft_rpc ft_dpc [pw = weight_full], robust
fitstat, saving(full) // We're saving the the model statistics to compare later
* Receiver operator characteristic
predict pr, pr
roctab presvote2012_x pr
roctab presvote2012_x pr, graph // pretty, right?
drop pr

*** Did we improve over the bivariate model?

logit presvote2012_x pid_x [pw = weight_full], robust
fitstat, using(full) force // compares model fit from prior logit

*** Substantive effects using full vote choice model
* This code will let you see survey data analysis in action
* findit "clarify"

set seed 012345 // If you do not set the seed, your results will not be replicable!
qui estsimp logit presvote2012_x pid_x economy income minority south ft_rpc ft_dpc [pw = weight_full], robust sims(10000)

/*
Once the primary and ancillary parameters are simulated, you no longer need to 
worry about weights in your syntax. Why is this? Because the parameters are 
calculated using the weighted model and Clarify only cares about the parameters.
The underlying data is, to a degree, unimportant now at this point unless you 
use the observed values approach. However, you will need to use the "noinher"
option when setting the values of your variables because Clarify doesn't 
understand how probability weights work with the simulated parameters.
*/

* Baseline probability of voting for Obama
setx pid_x mean economy mean income mean minority 0 south 0 ft_rpc mean ft_dpc mean, noinher
simqi

*** Probability of Obama vote across values of party identification
cap postclose pv
postfile pv xaxis me se lo hi using "Predicted Probabilities.dta", replace

* This loop will calculate and save predicted probabilities for us to plot
qui foreach i of numlist 0(1)7 {
	setx pid_x `i' economy mean income mean minority 0 south 0 ft_rpc mean ft_dpc mean, noinher
	simqi, prval(1) genpr(pr)
	qui sum pr
	local me = r(mean)
	local se = r(sd)
	* Confidence intervals
	_pctile pr, p(2.5 97.5)
	local lo = r(r1)
	local hi = r(r2)
	drop pr
	post pv (`i') (`me') (`se') (`lo') (`hi')
}

postclose pv

* Run this chunk of code all at once so our ANES data remains in memory
preserve
	use "Predicted Probabilities.dta", clear
	twoway (rarea lo  hi xaxis, fi(inten10) lw(vvthin) lc(gs15)) (line me xaxis), ///
		xlabel(0(1)7) ylabel(0(.2)1) ///
		xtitle("Strong Dem.                                                                                                Strong Rep.") /// 
		ytitle("Pr(Obama Vote)") legend(off) scheme(s2mono)
restore

*** Probability of Obama vote across Romney feeling thermometer
cap postclose pv2
postfile pv2 xaxis me se lo hi using "Predicted Probabilities2.dta", replace

* This loop will calculate and save predicted probabilities for us to plot
qui foreach i of numlist 0(1)100 {
	setx pid_x mean economy mean income mean minority 0 south 0 ft_rpc `i' ft_dpc mean, noinher
	simqi, prval(1) genpr(pr)
	qui sum pr
	local me = r(mean)
	local se = r(sd)
	* Confidence intervals
	_pctile pr, p(2.5 97.5)
	local lo = r(r1)
	local hi = r(r2)
	drop pr
	post pv2 (`i') (`me') (`se') (`lo') (`hi')
}

postclose pv2

* Again, run this code all at once
preserve
	use "Predicted Probabilities2.dta", clear
	twoway (rarea lo  hi xaxis, fi(inten10) lw(vvthin) lc(gs15)) (line me xaxis), ///
		xlabel(0(10)100) ylabel(0(.2)1) ///
		xtitle("Romney Feeling Thermometer") /// 
		ytitle("Pr(Obama Vote)") legend(off) scheme(s2mono)
restore

