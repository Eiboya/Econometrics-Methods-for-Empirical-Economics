

* =================================== *
*        Empirical Exercise.          *
*      Zhihan Zhang & Jingwan LUO     *
* =================================== *

***************************** Exercise 1 ***************************************


* == (1) == *
use http://www.stata.com/data/jwooldridge/eacsap/bwght

/* in case the link does not work or change locations, use the following
and change your path:
cd "/Users/kongqingluo/Desktop/EEE/Empirical Exercise"
use bwght.dta */

* == (2) == *
gen smokes = 0
replace smokes = 1 if cigs != 0

* == (3) == *

* ===== (b) ===== descriptive stats ===== *
summarize
estpost summarize faminc cigtax cigprice bwght fatheduc motheduc parity ///
male white cigs lbwght bwghtlbs packs lfaminc
esttab . using "Table_1.tex", cells("mean sd min max count") noobs

* ===== (c) ===== # of smoking woman ===== *
count if smokes == 1
* # of smoking woman = 212 *

* ===== (d) ===== # of white smoking woman ===== *
count if smokes == 1 & white == 1
* # of white smoking woman = 165 *

/* ===== (e) ===== # of smoking women with 
family income above the sample average ===== */
egen meanfaminc=mean(faminc)
count if smokes == 1 & faminc > meanfaminc
/* # of smoking women with 
family income above the sample average = 53 */

/* ===== (f) ===== # of smoking women with 
family income above the sample median ===== */
egen medianfaminc=median(faminc)
count if smokes == 1 & faminc > medianfaminc
/* # of smoking women with 
family income above the sample average = 53 */

/* ===== (g) ===== # of observations with at least one 
missing value among smokes, motheduc, white, lfaminc ===== */
count if smokes==.| motheduc==.| white==.| lfaminc==.

* == (4) == LPM *
* ===== (a) ===== baseline LPM, robust se used *
reg smokes motheduc white lfaminc
eststo lpm1
reg smokes motheduc white lfaminc, robust
eststo lpm2
esttab lpm1 lpm2 using "Table_2.tex", se ar2 replace

* ===== (b) motheduc and lfaminc are sig at 5% level when r.se. used. *


* ===== (d) pp proportion outside the unit circle ===== *
predict psmokes_LPM
gen pp_out = .
replace pp_out = 1 if psmoke_LPM>1 | psmoke_LPM<0
sum psmoke_LPM pp_out
display 64/1387
* proportion = .04614275 *

* == (5) == Logit *

* ===== (a) baseline logit ===== *
* have to specify white as a bi-var.
logit smokes motheduc i.white lfaminc
eststo logitstandard
esttab logitstandard using "Table_3.tex", se ar2 replace

* ===== (b) AME & MEM ===== *

* AME, motheduc:
margins, dydx(motheduc)
* MEM, motheduc:
margins, dydx(motheduc) atmeans
* AME, white:
margins, dydx(white)
* MEM, white:
margins, dydx(white) atmeans
* AME, lfaminc:
margins, dydx(lfaminc)
* MEM, lfaminc:
margins, dydx(lfaminc) atmeans

* ===== (d) Proportions of correct predictions ===== *
* for LPM
qui reg smokes motheduc white lfaminc, robust
predict psmokes_LPM
generate smokes_LPM = (psmokes_LPM>=0.5)
count if smokes_LPM == smokes & smokes == 0
count if smokes == 0
count if smokes_LPM == smokes & smokes == 1
count if smokes == 1

* for Logit
qui logit smokes motheduc i.white lfaminc
predict psmokes_logit
generate smokes_logit = (psmokes_logit>=0.5)
count if smokes_logit==smokes & smokes == 0
count if smokes == 0
count if smokes_logit==smokes & smokes == 1
count if smokes == 1


* ===== (f) estimated difference of expectation ===== *
margins, dydx(lfaminc) atmeans at(white = 0 motheduc = 16)
margins, dydx(lfaminc) atmeans at(white = 0 motheduc = 12)
display (-.0142874-(-.0330342))
* difference in expectation=.05757651

* ===== (h) exogeneity test ===== *

reg lfaminc motheduc fatheduc bwght male white lbwght cigs cigtax cigprice
predict lfaminc_res
reg smokes lfaminc white motheduc lfaminc_res
test lfaminc_res

* == (6) == Probit *
probit smokes motheduc i.white lfaminc

* ===== AME & MEM ===== *
* AME, motheduc:
margins, dydx(motheduc)
* MEM, motheduc:
margins, dydx(motheduc) atmeans
* AME, white:
margins, dydx(white)
* MEM, white:
margins, dydx(white) atmeans
* AME, lfaminc:
margins, dydx(lfaminc)
* MEM, lfaminc:
margins, dydx(lfaminc) atmeans


***************************** Exercise 2 ***************************************

clear all

* == (1) == *
use http://www.stata.com/data/jwooldridge/eacsap/fringe

/* in case the link does not work or change locations, use the following
and change your path:
cd "/Users/kongqingluo/Desktop/EEE/Empirical Exercise"
use fringe.dta */

* == (2) == *
keep hrbens exper age educ tenure married male white nrtheast nrthcen south union

* == (3) == *
* ===== (b) ===== descriptive stats ===== *
summarize
estpost summarize hrbens exper age educ tenure married male white nrtheast ///
nrthcen south union
esttab . using "Table_6.tex", cells("mean sd min max count") noobs

* ===== (c) ===== # of woman ===== *
count if male == 0

* ===== (d) ===== # of married woman tenure larger than sample average ===== *
egen meantenure=mean(tenure)
count if married == 1 & tenure > meantenure & male ==0

* ===== (e) ===== mean of hrbens, man&woman *
sum hrbens if male == 1
sum hrbens if male == 0

* == (4) == Linear Model *
reg hrbens exper age educ tenure married male white nrtheast ///
nrthcen south union, robust
eststo hrbens_LPM

* If LPM is needed, we generate a binary variable
gen b_hrbens = 0
replace b_hrbens = 1 if hrbens > 0
reg b_hrbens exper age educ tenure married male white nrtheast ///
nrthcen south union, robust
eststo b_hrbens_LPM

* == (5) == Censored? Coner Solution? *
histogram hrbens
graph export histo.png

* == (6) == Tobit *
tobit hrbens exper age educ tenure married male white nrtheast ///
nrthcen south union, ll
eststo hrben_tobit1

* == (8) == Tobit, square added *
gen sexper = exper^2
gen stenure = tenure^2

qui tobit hrbens exper age educ tenure married male white nrtheast ///
nrthcen south union sexper stenure, ll

eststo hrbens_tobit2
esttab hrbens_LPM b_hrbens_LPM hrben_tobit1 hrbens_tobit2 using "Table_7.tex", ///
se ar2 replace

* whether squared terms should be included?
* first we harvest the min max of sexper
sum exper
* then we create a marginsplot to see the trend
margins, at(exper = (1(1)60))
marginsplot
graph export mp1_exper.png
sum tenure
margins, at(tenure = (1(0.2)25))
marginsplot
graph export mp2_tenure.png

* moreover, check relationship by fitting lines to scatter plot:
twoway scatter hrbens exper || qfit hrbens exper
graph export sc_exper.png
twoway scatter hrbens tenure || qfit hrbens tenure
graph export sc_tenure.png

/* From the graph, there's indeed non-linear relationship observed,
therefore we can add square terms.*/

* == (9) == estimated ME, subsample h>0, others at means *
sum tenure
display 7.751623^2
sum exper
display 18.61688^2
margins, dydx(tenure) predict(e(0,.)) atmeans ///
at(stenure==60.087659 sexper==346.58822)

* == (10) == exogenous test on educ *
reg educ married male white nrtheast nrthcen south union
predict res_TS, res
tobit hrbens exper age educ tenure married male white union res_TS, ll




