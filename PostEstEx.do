********************************************************************************
********************************************************************************
********************************************************************************
******Generalized Structural Equation Models and Post-Estimation Graphs*********
****************************Written in STATA 14*********************************
********************************************************************************
********************************************************************************
cd "YOUR CURRENT DIRECTORY GOES HERE"
use "gsemEx" //use gsemEx13 if you have STATA 13, might not work GSEM is fairly new to
*STATA and functionality is increasing, I'm not sure earlier versions support it

 
/*Theory and Background: Part of a Draft Chapter from my dissertation on 
Transnational Advocacy, Human Rights and Public Opinion. Very briefly, the
argument is that transnational advocacy influences public opinion, sometimes
negatively, which makes it harder for governments to change policy, which 
is the desired goal of these groups. These transnational social movements(TNSMs),
polarize opinion on their issues. This chapter shows how this polarization 
happens at the individual level. Short argument: Respondents prior predisposition
determines whether they will react positively or negatively to TNSM messages(via 
cognitive dissonance). This is measured by a latent variable made up from 
several more foundational beliefs. These models are Generalized(because
there are non-continuous dependent variables) Structural Equation Models(Because
 there is more than 1 dependant variable) using a latent(measured by proxy) variable
 interacted with the number of TNSMs operating*/

********************************************************************************
************Dependent Variable: Is Homosexuality Ever Justifiable***************
****************Measured on 10 point scale from Never to Always*****************
*****************************Modeled as Continuous******************************
********************************************************************************

/*Not reccomended to actually run this model, it takes about 24 hours to converge
*(On STATA SE so only 1 processor used, MP should be faster).
*If you insist, see instructions below command, and remove the slash before this comment

gsem (Predisposition -> cindep, family(binomial) link(logit)) /// These lines model the latent variable
 (Predisposition -> cobed, family(binomial) link(logit))  /// using the distribution family appropriate 
 (Predisposition -> relimp, family(ordinal) link(logit)) ///  to the dependent variable, latent variable "Causes them"
 (Predisposition -> lr, ) (Predisposition@1 -> F118, ) ///<-------Main DV of interest must be constrained to 1 to give the latent variable a scale
 (wvsyear -> F118, ) (i.region -> F118, ) /// Controls
 (ganyright -> F118, ) (polity2 -> F118, ) (socclass -> F118, ) /// Controls 
 (inc -> F118, ) (female -> F118, ) (educ -> F118, ) (age -> F118, ) /// Controls
 (gdppercapconp -> F118, ) (importgns -> F118, )  (presidential -> F118,) /// Controls
 (mixedsys -> F118,) (inf -> F118,) (mdmh -> F118,)(unemrate->F118,) /// Controls
 (Predisposition#c.ganyright -> F118), covstruct(_lexogenous, diagonal) /// Interactive term, main IV of interest, covariance is assumed
 latent(Predisposition ) nocapslatent vce(robust) iterate(70) //from(b) // to be exogenous, only way for the model to converge

************************************If You Insist******************************* 
/*First run the model as is(find something else to do), it will stop after 70 
iterations, looking at the coefficients, nothing looked especially problematic 
so none were altered to get better starting values for the process to work with.
Then, run this line*/
matrix define b=e(b)
/*Then, remove the iterate(70) option and remove the earlier comment on the last
line, which will tell the model to start from the previous values. Then find
several other things to do.*/

estimates save GayRights //Saves the model results
gen gay1samp=e(sample) //Saves the sample used


*/
*****************Alternatively, Load the saved result ************************** 

estimates use GayRights
gsem
estimates esample: if gay1samp==1 //Sets Stata's internal sample to sample used
predict gay1latent, latent //Generates Emprical Baye's estimates for unobserved latent variable
sum gay1latent

*Plot for Conditional Effect of TNSMs on Public Opinion on the Justifiability of Homosexuality

sum gay1latent // generates r(min) and r(max) for command
local min=round(r(min),0.01) 
local max=round(r(max),0.01) //Here and ^ Round to a level appropriate to range of variable
forvalues val=`min'(.01)`max' { //Step should be same size as rounding^
lincom _b[F118:c.ganyright#c.Predisposition]*`val'+_b[F118:ganyright] //Linear Combinations for the interaction
matrix define slice=( [r(estimate) + invnorm(0.025)*r(se)],[r(estimate)] ,[r(estimate) + invnorm(0.975)*r(se)],[`val'])
if `val'==`min' matrix plot=slice //^Storing each result in a matrix
else matrix plot=(plot\slice) //<-Appending each slice
}
svmat plot //Converting slices to a variable
rename plot? plot?g

*****Gay Plot
gen zerol=0 //Line for zero
  # delimit;
graph twoway hist gay1latent, yscale(alt range(-6.55 `=r(max)') off) fcolor(gs15) lwidth(vvvthin) //Histogram, shifted to be a 0 on y scale
		  || line plot2g plot4g, clpattern(solid) clwidth(medium) yaxis(2) yscale(alt axis(2)) // Effect line 
          || line plot3g plot4g, clpattern(solid) clwidth(thin) yaxis(2) yscale(alt axis(2)) // Confidence boundary
          || line plot1g plot4g, clpattern(solid) clwidth(thin) yaxis(2) yscale(alt axis(2)) // Confidence boundary
		  || line zerol plot4g,  lcolor(black) yaxis(2) 
		  ||,
             xscale(noline) //-----------------Options for format form here down
             yscale(noline)
             legend(off)
             title("Predisposition's Effect on TNSM Effect", size(5))
             subtitle("Dependent Variable: Homosexuality Justifiable", size(4))
             xtitle("Predisposition", size(3))
             ytitle("TNSM Effect Size", size (3) axis(2)) 
             ytitle("") 
             note("95% Confidence Intervals", size(2))
             xsca(titlegap(2))
             ysca(titlegap(4))
             scheme(s2mono)
             graphregion(fcolor(white))
             graphregion(margin(zero));
 # delimit cr            exit;



/**************************Stop Here to See First Graph*************************
********************************************************************************
Graph shows the effect of TNSMs dependent on predisposition, the histogram 
overlaid shows the proportion of the population with that predisposition, as can
be seen, there are both positive and negative statistically significant effects,
in other words, polarization of the population. Since the main DV is modeled as
continuous, no further graphs are really needed. A on unit increase in TNSMs, one
more group being in a state, leads to wherever on the Y units of change, on a 10 
point scale where the respondent is on the X axis So predisposition of .05 leads to
plus about .5, on of -.075, about a negative .25. */
 
******************************************************************************** 
**********Dependent Variable: Immigrant's Aceeptable as Neighbors***************
*********Dichotomous, reverse coded so 1 is favorable to immigrants*************
****************************Logistic Regression*********************************
******************************************************************************** 

/*Can skip and use saved results, takes about 45 minutes to converge with STATA 14 SE

gsem (Predisposition -> proudnat, family(ordinal) link(logit)) /// Ordinal scale of how proud of nationality, reverse coded
 (Predisposition -> geogroup, family(ordinal) link(logit))  ///Geographic identity, local, country, region, all humanity
 (Predisposition -> tolneigh, family(poisson) link(log)) /// Count of neighbors tolerated, hence poission dist.
(Predisposition -> lr,) ///<---------------------Latent determined variables above
 (Predisposition@1 -> immneigh, family(binomial) link(logit)) /// Key Depedent variable, must be constrained to 1 to set scale of latent variable
(wvsyear -> immneigh, family(binomial) link(logit)) /// Controls
 (i.region -> immneigh, family(binomial) link(logit)) /// Controls 
(im1right -> immneigh, family(binomial) link(logit)) /// Controls
 (polity2 -> immneigh, family(binomial) link(logit))  /// Controls
 (socclass -> immneigh, family(binomial) link(logit)) /// Controls
 (inc -> immneigh, family(binomial) link(logit)) /// Controls
 (female -> immneigh, family(binomial) link(logit)) /// Controls
 (educ -> immneigh, family(binomial) link(logit)) ///  Controls
 (age -> immneigh, family(binomial) link(logit)) ///  Controls
 (gdppercapconp -> immneigh, family(binomial) link(logit)) /// Controls
 (importgns -> immneigh, family(binomial) link(logit)) /// Controls
  (presidential -> immneigh, family(binomial) link(logit)) ///  Controls
 (mixedsys -> immneigh, family(binomial) link(logit)) /// Controls
 (inf -> immneigh, family(binomial) link(logit)) ///   Controls
 (unemrate -> immneigh, family(binomial) link(logit)) ///  Controls
 (mdmh -> immneigh, family(binomial) link(logit)) ///
 (Predisposition#c.im1right -> immneigh, family(binomial) link(logit)), /// Key Independent Variable, interaction 
 covstruct(_lexogenous, diagonal) latent(Predisposition ) nocapslatent vce(robust)
beep //Beeps so you know when it's done
estimates save ImmigrantNeighbors //Saves estimate
gen imm2samp=e(sample) //Saves sample

*/***************************Using Saved Results********************************


estimates use ImmigrantNeighbors //loads saved results
gsem //recalls the model
estimates esample: if imm2samp==1 //sets the sample
predict in2latent, latent //Empircal Bayes predictions for latent variable

*********************Interaction Statistical Significance***********************
**Identical to procedure for first example
sum in2latent
local min=round(r(min),0.01)
local max=round(r(max),0.01)
forvalues val=`min'(.01)`max' {
lincom _b[immneigh:c.im1right#c.Predisposition]*`val'+_b[immneigh:im1right]
matrix define slice=( [r(estimate) + invnorm(0.025)*r(se)],[r(estimate)] ,[r(estimate) + invnorm(0.975)*r(se)],[`val'])
if `val'==`min' matrix plot=slice
else matrix plot=(plot\slice)
}
svmat plot
rename plot? plot?i2

  # delimit;
graph twoway hist in2latent, yscale(alt range(-2.0 `=r(max)') off) fcolor(gs15) lwidth(vvvthin) 
		  || line plot2i2 plot4i2, clpattern(solid) clwidth(medium) yaxis(2) yscale(alt axis(2))  
          || line plot3i2 plot4i2, clpattern(solid) clwidth(thin) yaxis(2) yscale(alt axis(2)) 
          || line plot1i2 plot4i2, clpattern(solid) clwidth(thin) yaxis(2) yscale(alt axis(2)) 
		  || line zerol plot4i2,  lcolor(black) yaxis(2) 
		  ||,
             xscale(noline)
             yscale(noline)
             legend(off)
             title("Predisposition's Effect on TNSM Effect", size(5))
             subtitle("Dependent Variable: Immigrant Neighbors", size(4))
             xtitle("Predisposition", size(3))
             ytitle("TNSM Effect Size", size (3) axis(2)) 
             ytitle("") 
             note("95% Confidence Intervals", size(2))
             xsca(titlegap(2))
             ysca(titlegap(4))
             scheme(s2mono)
             graphregion(fcolor(white))
             graphregion(margin(zero));
 # delimit cr            exit;

/*************************Stop Here to See Second Graph*************************
********************************************************************************
Once again, predisposition conditions the response to TNSMs importantly
However, since the dependent variable is dichotomous, the coefficents
and linear combinations only really tell you about direction of effect and 
statistical significance. Predicted probabilities at various interesting points
are needed to see the real effects. The issue is that the margins command
doesn't work in STATA for GSEM models with latent variables, therefore the manual
calculation of predicted probabilities is used(exp^XB/(1+exp^XB))*/
 
********************************************************************************
********************************************************************************
******************************************************************************** 
*****Predicted probability curves for Immigrant neighbors.**********************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
tabulate region, generate(r)


*vvvvvvvvvvvvvvvvvNeed to run from here to next marker all at once vvvvvvvvvvvvv

foreach  v of varlist wvsyear im1right polity2 socclass inc female educ age gdppercapconp importgns presidential mixedsys inf unemrate mdmh {
sum `v'
local `v'm= r(mean) //getting mean values of all non-nomnial variables and storing in locals
di ``v'm' //Used to set other variables at mean value
}

foreach i of varlist r2 r4 r5 r6 r7 r8{
sum `i' //Sets the mean foreach regional indicator variable
local `i'=r(mean)
}
local r3=.1975334 //This local was set separately because sometimes a forvalues loop
*gave the wrong mean, I don't know why, but it is upsetting

local lowpim= "-r(sd)" //Conditions, low, medium and high predisposition
local highpim "+r(sd)"
local meanpim ""
foreach cond in lowpim highpim meanpim{ //repeat process for each condition
sum in2latent // generates r(mean) and r(sd)
local val=r(mean)``cond'' //``'' so it evaluates to the value of the local determined by the foreach command(local of the local)
sum im1right //sets generates r(min) and r(max)
local min=round(r(min),1) //Round to an value appropriate to the scale of the TNSMs present
local max=round(r(max),1)
forvalues morg=`min'(1)`max' { //Range of TNSM groups and their effect on the DV
local im2pred=exp(_b[immneigh:_cons] + _b[immneigh:im1right]*`morg'+ /// exp^XB with locals for all of the Xs
_b[immneigh:c.im1right#c.Predisposition]*`val'*`morg' + _b[immneigh:Predisposition]*`val' + /// exp^XB(cont.)
_b[immneigh:wvsyear]*`wvsyearm' +  _b[immneigh:polity2]*`polity2m' + ///exp^XB(cont.)
_b[immneigh:socclass]*`socclassm' + _b[immneigh:inc]*`incm' + /// exp^XB(cont.)
_b[immneigh:female]*`femalem' + _b[immneigh:educ]*`educm' + _b[immneigh:age]*`agem' + ///exp^XB(cont.)
_b[immneigh:gdppercapconp]*`gdppercapconpm' + _b[immneigh:importgns]*`importgnsm' + ///exp^XB(cont.)
_b[immneigh:presidential]*`presidentialm' + _b[immneigh:mixedsys]*`mixedsysm' + /// exp^XB(cont.)
_b[immneigh:inf]*`infm' + _b[immneigh:unemrate]*`unemratem' + ///exp^XB(cont.)
_b[immneigh:mdmh]*`mdmhm' + _b[immneigh:2.region]*`r2' + _b[immneigh:3.region]*`r3' + ///exp^XB(cont.)
_b[immneigh:4.region]*`r4'+ _b[immneigh:5.region]*`r5'+ _b[immneigh:6.region]*`r6' + ///exp^XB(cont.)
_b[immneigh:7.region]*`r7' + _b[immneigh:8.region]*`r8')/(1+exp(_b[immneigh:_cons] + /// /(1+exp^XB)
_b[immneigh:im1right]*`morg'+ /// /(1+exp^XB)(cont.)
_b[immneigh:c.im1right#c.Predisposition]*`val'*`morg' + _b[immneigh:Predisposition]*`val' +  ////(1+exp^XB)(cont.)
_b[immneigh:wvsyear]*`wvsyearm' +  _b[immneigh:polity2]*`polity2m' + ////(1+exp^XB)(cont.)
_b[immneigh:socclass]*`socclassm' + _b[immneigh:inc]*`incm' +  /// /(1+exp^XB)(cont.)
_b[immneigh:female]*`femalem' + _b[immneigh:educ]*`educm' + _b[immneigh:age]*`agem' + ////(1+exp^XB)(cont.)
_b[immneigh:gdppercapconp]*`gdppercapconpm' + _b[immneigh:importgns]*`importgnsm' + ////(1+exp^XB)(cont.)
_b[immneigh:presidential]*`presidentialm' + _b[immneigh:mixedsys]*`mixedsysm' + /// /(1+exp^XB)(cont.)
_b[immneigh:inf]*`infm' + _b[immneigh:unemrate]*`unemratem' + ////(1+exp^XB)(cont.)
_b[immneigh:mdmh]*`mdmhm'+ _b[immneigh:2.region]*`r2' + _b[immneigh:3.region]*`r3' + ////(1+exp^XB)(cont.)
_b[immneigh:4.region]*`r4'+ _b[immneigh:5.region]*`r5'+ _b[immneigh:6.region]*`r6' + ////(1+exp^XB)(cont.)
_b[immneigh:7.region]*`r7' + _b[immneigh:8.region]*`r8'))
matrix define slice=([`im2pred'],[`morg']) //saves predicted probability and # of TNSMs associated
if `morg'==`min' matrix plot=slice //If first slice matrix plot is slice
else matrix plot=(plot\slice) //Else append current slice to matrix plot
}
svmat plot //Turns plot into a variable from matrix
rename plot? plot?`cond' //Single `' so that it evaluates to the local names for conditions, not their values
}

*^^^^^^^^^^^^^^^^^^^^^Need to run from there to here all at once^^^^^^^^^^^^^^^^

  # delimit;
graph twoway hist im1right, yscale(alt off) fcolor(gs15) lwidth(vvvthin) discrete
		  || line plot1lowpim plot2lowpim, clpattern(solid) clwidth(vthin) yaxis(2) yscale(alt axis(2))  
          || line plot1meanpim plot2meanpim, clpattern(solid) clwidth(medium) yaxis(2) yscale(alt axis(2)) 
          || line plot1highpim plot2highpim, clpattern(solid) clwidth(medthick) yaxis(2) yscale(alt axis(2)) 
		  ||,
             xscale(noline)
             yscale(noline)
			 legend(order(2 3 4) label(2 "Low Predist.") label(3 "Mean Predist.") label(4 "High Predist.") rows(1))
             title("Predisposition's Effect on TNSM Effect", size(5))
             subtitle("Dependent Variable: Immigrant Neighbors", size(4))
             xtitle("TNSMs Present", size(3))
             ytitle("Predicted Probabilities", size (3) axis(2)) 
             ytitle("") 
             xsca(titlegap(2))
             ysca(titlegap(4))
             scheme(s2mono)
             graphregion(fcolor(white))
             graphregion(margin(r=5));
 # delimit cr            exit;


/*******************************************************************************
Just as the interaction coefficients showed, at mean or high level of predisposition,
the predicted probability incereases with TNSMs active, at low levels, there is
the opposite effect. */


