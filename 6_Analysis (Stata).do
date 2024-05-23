********************************************************************************
***                               Project:                                   ***
***                     Droughts & child under-nutrition                     ***
********************************************************************************

// Search NOTE for important notes


*	    ssc install egenmore, replace    
*		ssc install xbrcspline, replace
*    	ssc install outreg2	

		
	
global datain "D:\Droughts and child undernutrition\Data\data for analysis\"
global dataout "D:\Droughts and child undernutrition\Results\"

insheet using "$datain\data_spei_season_length_buffer10.csv", clear
     
	    drop v1
		
		foreach var of varlist * {
			cap replace `var' = "" if `var'=="NA"
		}
						
		encode sex, gen(nsex)
		encode birthsize, gen(nbirthsize)
		encode breastfed, gen(nbreastfed)
		encode edulevel, gen(nedulevel)
		encode partner, gen(npartner)
		encode occupationhead, gen(noccupationhead)
		encode residence, gen(nresidence)
		encode surveyid, gen(nsurveyid)
		encode countryname, gen(ncountryname)
		encode maincrop, gen(nmaincrop)
		encode regid, gen(nregid)
		encode wealth, gen(nwealth)
		
	    replace nedulevel =  4 if nedulevel == 1
		
		destring spei_utero_main spei_infancy_main spei_age1_main spei_age2_main spei_age3_main spei_age4_main, replace force
		destring spei_utero_weighted spei_infancy_weighted spei_age1_weighted spei_age2_weighted spei_age3_weighted spei_age4_weighted, replace force
		
		destring spei_infancy_barley spei_infancy_barleywinter spei_infancy_cassava spei_infancy_cotton spei_infancy_groundnut spei_infancy_maize spei_infancy_maize2 spei_infancy_millet spei_infancy_potato spei_infancy_pulsenes spei_infancy_rice spei_infancy_rice2 spei_infancy_sorghum spei_infancy_sorghum2 spei_infancy_soybean spei_infancy_sunflower spei_infancy_sweetpotato spei_infancy_wheat spei_infancy_wheatwinter spei_infancy_yam, replace force
		
		destring spei_infancy_oils spei_infancy_grains spei_infancy_roots spei_infancy_oils_pulsenes, replace force
		
		destring spei_age1_barleywinter spei_age2_barleywinter spei_age3_barleywinter  spei_age4_barleywinter spei_infancy_barleywinter spei_utero_barleywinter, replace force
		
		destring spei_age1_barley spei_age2_barley spei_age3_barley spei_age4_barley spei_utero_barley, replace force
				
		destring stunted, replace force
		
		destring cropland, replace force
		destring pasture, replace force
		
		destring seasonlengthmain, replace force
		replace seasonlengthmain=4 if seasonlengthmain==3
		replace seasonlengthmain=8 if seasonlengthmain>8
		
		egen psuid = group(surveyid psu)
		egen cnt_yr = group(countryname intyr)
		egen cnt_mo = group(countryname birthmo)
		
		gen pasture_2 = 2*pasture
		gen cropland_2 = 2*cropland
	
		* Generate DHS wave - every 5 years aprx.
		egen wave = cut(intyr), at(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)
		egen decade = cut(intyr), at(1980, 1990, 2000, 2010, 2020, 2030)
		
		gen agemonths_sqr = agemonths*agemonths
		
		gen bord_gr =.
		replace bord_gr = 1 if bord == 1
		replace bord_gr = 2 if bord == 2
		replace bord_gr = 3 if bord >= 3
		
		egen sex_bord = group(sex bord_gr), label
		
		recode cropland (0/0.2 = 1) (0.2/0.4 = 2) (0.4/0.6 = 3) (0.6/1 = 4), generate(cropland_cat)
		recode pasture (0/0.25 = 1) (0.25/0.5 = 2) (0.5/0.75 = 3) (0.75/1 = 4), generate(pasture_cat)
				
		drop wealth
		drop nwealth 
		destring wealthscore, replace force	
		gen wealthscore_help = wealthscore
		egen nwealth = cut(wealthscore_help) , group(3) 	
			
		keep if stunted!=. & nsex!=. & agemonths!=. & bord!=. & nbirthsize!=. & age_mother_at_birth!=. & nedulevel!=. & noccupationhead!=. & nwealth!=. & nresidence!=. & birthmo!=. & intmo!=. & nregid!=. 

			
		* Determine the main crop groups
		gen maincrop_gr = ""
		replace maincrop_gr = "grains" if ((grainstot>rootstot & grainstot>oilstot) & grainstot>0)
		replace maincrop_gr = "roots" if ((rootstot>grainstot & rootstot>oilstot) & rootstot>0)
		replace maincrop_gr = "oils" if ((oilstot>grainstot & oilstot>rootstot) & oilstot>0)
		encode maincrop_gr, gen(nmaincrop_gr)
		ta nmaincrop_gr
		
		gen agriland = cropland+pasture 

		gen livelihood = "."
		replace livelihood = "croppers" if cropland>=0.20 & cropland>=pasture_2
		replace livelihood = "pastoral" if pasture>=0.20 & pasture>=cropland_2 
		replace livelihood = "agro-pastoral" if livelihood!="croppers" & livelihood!="pastoral" & agriland>0.2
		replace livelihood = "other" if livelihood=="."
		encode livelihood, gen(nlivelihood)	
		
		
		destring spei_lag1_main spei_lag2_main spei_lag3_main, replace force
		
		destring spei_lag1_weighted spei_lag2_weighted spei_lag3_weighted, replace force
		
		
		recode spei_infancy_main (-10/-1 = 1) (-1/10 = 0), generate(drought_lag0_main)
		recode spei_lag1_main (-10/-1 = 1) (-1/10 = 0), generate(drought_lag1_main)
		recode spei_lag2_main (-10/-1 = 1) (-1/10 = 0), generate(drought_lag2_main)
		recode spei_lag3_main (-10/-1 = 1) (-1/10 = 0), generate(drought_lag3_main)
		
		gen drought_count_main = 0
		replace drought_count_main = 1 if drought_lag1_main==1 
		replace drought_count_main = 2 if drought_lag1_main==1 & drought_lag2_main==1 
		
		
		recode spei_lag1_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought_lag1_weighted)
		recode spei_lag2_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought_lag2_weighted)
		recode spei_lag3_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought_lag3_weighted)
		
		gen drought_count_weighted = 0
		replace drought_count_weighted = 1 if drought_lag1_weighted==1 
		replace drought_count_weighted = 2 if drought_lag1_weighted==1 & drought_lag2_weighted==1 
	
		
		gen birth_qrt = .
		replace birth_qrt = 1 if birthmo==1 | birthmo==2 | birthmo==3
		replace birth_qrt = 2 if birthmo==4 | birthmo==5 | birthmo==6
		replace birth_qrt = 3 if birthmo==7 | birthmo==8 | birthmo==9
		replace birth_qrt = 4 if birthmo==10 | birthmo==11 | birthmo==12
		
		gen int_qrt = .
		replace int_qrt = 1 if intmo==1 | intmo==2 | intmo==3
		replace int_qrt = 2 if intmo==4 | intmo==5 | intmo==6
		replace int_qrt = 3 if intmo==7 | intmo==8 | intmo==9
		replace int_qrt = 4 if intmo==10 | intmo==11 | intmo==12
		
		gen age = .
		replace age=0 if agemonths<12
		replace age=1 if agemonths>=12 & agemonths<24 
		replace age=2 if agemonths>=24 & agemonths<36
		replace age=3 if agemonths>=36 & agemonths<48
		replace age=4 if agemonths>48

	
		replace dryland_zone = "Arid" if dryland_zone == "Hyperarid"
		replace dryland_zone = "Not arid" if dryland_zone==""
		encode dryland_zone, gen(ndryland_zone)
		
		egen cropland_dec = cut(cropland) , group(10) 	
		egen pasture_dec = cut(pasture) , group(10) 	

		
	    codebook surveyid, compact //106 unique surveys
			
		gen irrigation_some = 0
		replace irrigation_some = 1 if pct_aei>0
		
		recode pct_aei (-10/0 = 1) (0/25 = 2) (25/100 = 3) , generate(irrigation_cat)
		
		encode aez, gen(naez)
						
********************************************************************************	
*** Exposure-response curves by crop-growing season
********************************************************************************

*** Grains

		gen spei = spei_infancy_grains  
		replace spei=. if age==0
		replace spei=. if cropland==0
		replace spei=round(spei, 0.01)
		tabstat spei, stats(n mean median min max)
		
		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)  title("Grains growing seasons", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI") 
			 
		export delimited sbp or lb ub using "$dataout\plot_seasonGrains.csv" , replace		
		drop spline* sbp or lb ub spei	
		
	
*** Roots

		gen spei = spei_infancy_roots 
		replace spei=. if age==0
		replace spei=. if cropland==0
		replace spei=round(spei, 0.01)
		tabstat spei, stats(n mean median min max)
		
		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)   title("Roots growing seasons", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI")
			
		export delimited sbp or lb ub using "$dataout\plot_seasonRoots.csv" , replace		
		drop spline* sbp or lb ub spei	
		
		
*** Oilseesd 

		gen spei = spei_infancy_oils 
		replace spei=. if age==0
		replace spei=. if cropland==0
		replace spei=round(spei, 0.01)
		tabstat spei, stats(n mean median min max)
		
		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)   title("Oilseeds growing seasons", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI")
			
		export delimited sbp or lb ub using "$dataout\plot_seasonOils.csv" , replace		
		drop spline* sbp or lb ub spei	
				
*** Pulses

		gen spei = spei_infancy_pulsenes 
		replace spei=. if age==0
		replace spei=. if cropland==0
		replace spei=round(spei, 0.01)
		tabstat spei, stats(n mean median min max)
		
		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)   title("Pulses growing seasons", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI")
		
					
		export delimited sbp or lb ub using "$dataout\plot_seasonPulses.csv" , replace		
		drop spline* sbp or lb ub spei	
		

*** Main crop-growing season
		
		gen spei = spei_infancy_main
		replace spei=. if age==0
		replace spei=round(spei, 0.01)
		replace spei=. if cropland==0 
		tabstat spei, stats(n mean median min max)
		
		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)   title("Main crop", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI")

		// Get the Akaike values
		estat ic
		mat s=r(S)
		local aic=s[1,5]
		gen aic=`aic'
		local N = s[1,1]
		gen N=`N'
		
		export delimited sbp or lb ub  aic N using "$dataout\plot_all_MainCrop.csv" , replace		
		drop spline* sbp or lb ub spei aic N 
					
*** Full sample: Weighted crop-growing seasons 
		
		gen spei = spei_infancy_weighted
		replace spei=. if age==0
		replace spei=round(spei, 0.01)
		replace spei=. if cropland==0 
		tabstat spei, stats(n mean median min max)
		tabstat spei, statistics(q)

		mkspline spline = spei, nknots(4) cubic displayknots
		mat knots = r(knots)
	
		logit stunted spline* i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid, vce(cluster nregid) or
		
		xbrcspline spline, values(-4(0.05)4) ref(0) eform matknots(knots) gen(sbp or lb ub)
		
		twoway (line lb ub or sbp, lp(- - l) lc(gray gray black)   title("Multiple crops", size(medsmall))  yline(1, lpattern(dot)) ylabel(, angle(horizontal)) aspectratio(1)) ,  scheme(s1mono) legend(off) ytitle("aOR") xtitle("SPEI")

		// Get the Akaike values
		estat ic
		mat s=r(S)
		local aic=s[1,5]
		gen aic=`aic'
		local N = s[1,1]
		gen N=`N'
		
		export delimited sbp or lb ub  aic N using "$dataout\plot_all_WtdCrop.csv" , replace		
		drop spline* sbp or lb ub spei aic N 
	
						
********************************************************************************	
*** Drought and extreme precipitagion events # livelihood group
********************************************************************************
	
	  *** Multiple seasons
	  recode spei_infancy_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought)
	  recode spei_infancy_weighted (-10/1 = 0) (1/10 = 1) , generate(ext_pre)
	  

	  logit stunted i.drought i.ext_pre i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid  if livelihood == "croppers" & age>=1, vce(cluster nregid) or	    
	  esttab using "$dataout\reg_tables\reg_DroughtExtPre_Croppers.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
	
		
	  logit stunted i.drought i.ext_pre  i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid  if livelihood == "agro-pastoral" & age>=1, vce(cluster nregid) or	
	  esttab using "$dataout\reg_tables\reg_DroughtExtPre_Agropast.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	
	  
	  
	  logit stunted i.drought i.ext_pre  i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid  if livelihood == "pastoral" & age>=1, vce(cluster nregid) or	
	  esttab using "$dataout\reg_tables\reg_DroughtExtPre_Pastoral.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	
	  
	 	 
      logit stunted i.drought i.ext_pre  i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid  if livelihood == "other" & age>=1, vce(cluster nregid) or	
	  esttab using "$dataout\reg_tables\reg_DroughtExtPre_NonAgri.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	
	  
	 
	  logit stunted i.drought i.ext_pre i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or	
	  esttab using "$dataout\reg_tables\reg_DroughtExtPre_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	
	    

********************************************************************************
*** Drought # age at exposure and age at measurement
********************************************************************************	      
   
		logit stunted drought_infancy drought_age1 drought_age2 drought_age3 i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age==4, vce(cluster nregid) or
	  
		esttab using "$dataout\reg_tables\reg_droughtAge0to3_Age4.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	 
  
		logit stunted drought_infancy drought_age1 drought_age2  i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age==3, vce(cluster nregid) or  
	      
		esttab using "$dataout\reg_tables\reg_droughtAge0to2_Age3.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	 	
		
		logit stunted drought_infancy drought_age1  i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age==2, vce(cluster nregid) or  
		
		esttab using "$dataout\reg_tables\reg_droughtAge0to1_Age2.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	 
 
		logit stunted drought_infancy i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if  age==1, vce(cluster nregid) or  
		
		esttab using "$dataout\reg_tables\reg_droughtAge0_Age1.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	 
 
********************************************************************************
*** Effect measure modification by individual and socio-economiccharacteristics
********************************************************************************	

		drop drought
		recode spei_infancy_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought)
					
*** EM by birth order
	
		logit stunted i.drought#i.bord_gr i.nsex i.agemonths i.bord_gr i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#1.bord_gr=1.drought#2.bord_gr
		test 1.drought#1.bord_gr=1.drought#3.bord_gr
		test 1.drought#2.bord_gr=1.drought#3.bord_gr
		
		esttab using "$dataout\reg_tables\reg_bord_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 
												
*** EM by residence
	
		logit stunted i.drought#i.nresidence i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#1.nresidence=1.drought#2.nresidence

		esttab using "$dataout\reg_tables\reg_residence_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 			
			
*** EM by occupation of the household head
			
		logit stunted i.drought#i.noccupation i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#1.noccupation=1.drought#2.noccupation

		esttab using "$dataout\reg_tables\reg_occupation_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 		
	
*** EM by mother's education			

		logit stunted i.drought#i.nedulevel i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#2.nedulevel=1.drought#3.nedulevel
		test 1.drought#2.nedulevel=1.drought#4.nedulevel
		test 1.drought#3.nedulevel=1.drought#4.nedulevel

		esttab using "$dataout\reg_tables\reg_education_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 
			
*** EM by sex of the child

		logit stunted i.drought#i.nsex i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#1.nsex=1.drought#2.nsex

		esttab using "$dataout\reg_tables\reg_sex_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 
					
*** EM by wealth					
			
		logit stunted i.drought#i.nwealth i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		test 1.drought#0.nwealth=1.drought#1.nwealth
		test 1.drought#0.nwealth=1.drought#2.nwealth
		test 1.drought#1.nwealth=1.drought#2.nwealth

		esttab using "$dataout\reg_tables\reg_wealth_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) 
		


********************************************************************************	
*** Supplementary Material: Drought # Share of land dedicated to crops 
********************************************************************************
	
		drop drought 
		recode spei_infancy_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought)
		replace drought=. if cropland==0
		replace cropland_cat=3 if cropland_cat==4
		replace pasture_cat=3 if pasture_cat==4
		
		logit stunted i.drought#i.cropland_cat i.cropland_cat i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1, vce(cluster nregid) or
		
		esttab using "$dataout\reg_tables\reg_cropGr_past0-100pc_wtdSPEI.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	
			
		logit stunted i.drought#i.cropland_cat i.cropland_cat i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1 & pasture_cat==1, vce(cluster nregid) or
		
		esttab using "$dataout\reg_tables\reg_cropGr_past0-20pc_wtdSPEI.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

		logit stunted i.drought#i.cropland_cat i.cropland_cat i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1 & pasture_cat==2, vce(cluster nregid) or
		
		esttab using "$dataout\reg_tables\reg_cropGr_past20-40pc_wtdSPEI.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

		logit stunted i.drought#i.cropland_cat i.cropland_cat i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez pct_aei i.nregid if age>=1 & pasture_cat==3, vce(cluster nregid) or

		esttab using "$dataout\reg_tables\reg_cropGr_past40-100pc_wtdSPEI.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
		
		
********************************************************************************	
*** Supplementary Material: Drought # Area equipped for irrigation
********************************************************************************
	
		drop drought 
		recode spei_infancy_weighted (-10/-1 = 1) (-1/10 = 0), generate(drought)

		logit stunted i.drought#i.irrigation_some i.irrigation_some i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid if livelihood == "croppers" & age>=1, vce(cluster nregid) or
		
	   esttab using "$dataout\reg_tables\reg_IrrigationArea_Croppers.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

		logit stunted i.drought#i.irrigation_some i.irrigation_some i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid if livelihood == "agro-pastoral" & age>=1, vce(cluster nregid) or
		
	    esttab using "$dataout\reg_tables\reg_IrrigationArea_Agropast.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

		logit stunted i.drought#i.irrigation_some i.irrigation_some i.nsex i.agemonths bord i.nbirthsize age_mother_at_birth i.nedulevel i.noccupationhead i.nwealth i.nresidence i.birth_qrt i.int_qrt i.seasonlengthmain i.nmaincrop i.naez i.nregid if age>=1, vce(cluster nregid) or
		
	    esttab using "$dataout\reg_tables\reg_IrrigationArea_All.csv" , replace label cells(b(star fmt(3)) se(par fmt(3))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
			
		
				 
			
