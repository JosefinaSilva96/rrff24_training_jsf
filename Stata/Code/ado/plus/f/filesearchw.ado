*! version 0.2  26oct2015
*! Minh Cong Nguyen
* version 0.1  2jun2015

** folder/file search webserver

capture program define _datalibweb, plugin using("DataLib`=cond(strpos(`"`=c(machine_type)'"',"64"),64,32)'.dll")

cap program drop filesearchw
program define filesearchw, rclass
	version 12, missing
	local verstata : di "version " string(_caller()) ", missing:"
	if c(more)=="on" set more off
	syntax, COUNtry(string) Year(numlist max=1) [rootname(string) save(string) col(string) surveyid(string) flnames(string) para1(string) para2(string) para3(string) para4(string) ext(string) latest NOMETA NET]
	
	// Error
	global errcode 0
	
	// pass to _datalibweb
	tempfile temp1
	local collname $rootname
	
	if "`para1'" == "" local para1 `surveyid'
	if ("`surveyid'" ~= "" & "`flnames'"~="") local flnames `flnames' 		
	if ("`surveyid'" ~= "" & "`flnames'"=="") {
		if strpos("`=upper("`col'")'","ORIGINAL") >0 local flnames 	
		else                                         local flnames `surveyid'
	}
	if ("`surveyid'" == "" & "`flnames'"~="") local flnames `flnames'
	
	local col2 `col'
	if strpos("`=upper("`col'")'","ORIGINAL") >0 local col2 
	
	plugin call _datalibweb, "`temp1'" "`collname'" "`country'" "`year'" "`col'" "`flnames'" "`para1'" "`para2'" "`para3'" "`para4'"
	if `dlibrc'==0 {
		if "`dlibType'"=="csv" { // results in list of files
			qui insheet using "`temp1'", clear	
			// Check errorcode in the list			
			if _N==1 {
				cap confirm numeric variable filename
				if _rc==0 {
					noi dis as text in white "{p 4 4 2}`=errordetail[1]'{p_end}"
					if errorcode[1] == 672 dis "{err}Please contact IT and activate UPI"
					if errorcode[1] == 673 dis "{err}Please register with the datalibweb system"
					if errorcode[1] == 674 dis "{err}Please subscribe the selected country/year/collection/module"
					if errorcode[1] == 675 dis "{err}Please check the syntax - countrycode/year/collection/etc."
					if errorcode[1] == 676 dis "{err}Please change countrycode/year/collection/etc."
					global errcode `=errorcode[1]'
					clear
					error 1
				}
			}
			
			cap drop filepath
			ren filename file
			cap split filesharepath, p("\")
			ren filesharepath3 survey
			ren filesharepath4 surveyid
			ren filesharepath path
			
			//Clean and Save the filesearch data
			order survey surveyid path file		
			qui compress
			cap drop __*  
			cap drop filesharepath*
			qui drop if path==""
			
			if _N>0 { // get version				
				qui if "`col2'"=="" { //raw data
					split surveyid, p(_)
					ren surveyid1 countrycode
					ren surveyid2 year
					ren surveyid3 surveyname
					ren surveyid4 verm
					ren surveyid5 verm_l
					gen collection = "$type"
					sort countrycode year surveyname verm verm_l
				}
				
				qui if "`col2'"~="" { //harmonized data			
					split file, p(_)
					ren file1 countrycode
					ren file2 year
					ren file3 surveyname
					ren file4 verm
					ren file5 verm_l
					ren file6 vera
					ren file7 vera_l
					ren file8 collection
					cap replace collection = trim(subinstr(collection,".dta","",.))					
					cap gen str mod = ""
					cap replace mod = substr(file9,1,length(file9)-4)					
					cap drop file9
					sort countrycode year surveyname verm verm_l vera vera_l
				}
				
				// check survey
				qui levelsof survey, local(surlist)
				if `=wordcount(`"`surlist'"')' >1 { // more than one type of survey
				}
				else { //one type of survey only					
					qui if "`latest'"~="" { // latest or not
						//get the latest only
						cap drop if upper(verm)=="WRK"
						cap drop if upper(vera)=="WRK"
						
						qui levelsof verm, local(mlist)
						listsort `"`mlist'"', lexicographic						
						keep if verm=="`=word(`"`s(list)'"',-1)'"
						cap confirm variable vera
						if _rc==0 {
							qui levelsof vera, local(alist)
							listsort `"`alist'"', lexicographic							
							keep if vera=="`=word(`"`s(list)'"',-1)'"
						}
					}
				}
										
				// list if there is more than one or get the file
				qui levelsof surveyid, local(surveylist)
				if `=wordcount(`"`surveylist'"')' >1 {
					noi dis in yellow _n "{p 4 4 2}There are `=wordcount(`"`surveylist'"')' survey/data/vintage with defined parameters. Click on the following to redefine the search.{p_end}"	
					local rn = 1
					bys surveyid (file): gen seq = _n
					bys surveyid (file): gen seqN = _N			
					foreach ids of local surveylist {							
						local text datalibweb, country(`=countrycode[`rn']') year(`=year[`rn']') type(`=collection[`rn']') surveyid(`ids') `nometa'						
						noi dis `"`rn'. [{stata `"`text'"':`text'}]"'								
						noi dis as text in white "{p 4 4 2}`=errordetail[`rn']'{p_end}"
						if "`net'" ~="" noi dis `"<a onclick="sendCommand('`text'')">`text'</a>"'
						local rn = `rn' + `=seqN[`rn']'						
						if "`nometa'"=="" _metadisplay, surveyid(`=trim("`ids'")')											
					}			
					qui clear
				} // end of many survey ID
				else { // one Survey ID
					qui levelsof file, local(filelist)
					local ids `=surveyid[1]'
					if `=wordcount(`"`filelist'"')' >1 {
						noi dis in yellow _n "{p 4 4 2}There are more than one data files with defined parameters. Click on the following to redefine the search.{p_end}"
						local rn = 1
						noi dis in yellow "{p 4 4 2}Files available for this Survey ID: `ids'{p_end}"
						noi dis as text in white "{p 4 4 2}`=errordetail[`rn']'{p_end}"
						if "`nometa'"=="" _metadisplay, surveyid(`=trim("`ids'")')
																		
						foreach fs of local filelist {												
							local text datalibweb, country(`=countrycode[`rn']') year(`=year[`rn']') type(`=collection[`rn']') surveyid(`ids') filename(`=file[`rn']') `nometa'
							noi dis `"`rn'. [{stata `"`text'"':`text'}]"'
							if "`net'" ~="" noi dis `"<a onclick="sendCommand('`text'')">`text'</a>"'
							local rn = `rn' + 1
						}
						qui clear
					}
					else { //only 1 file, load it
						cap gen mod = ""
						cap gen vera = ""

						dis as text "{hline}"
						noi dis as text "{p 4 4 2}{cmd:Country:} "             in y "`=upper("`country'")'" as text " {p_end}"
						noi dis as text "{p 4 4 2}{cmd:Year:} "                in y "`=year[1]'" as text " {p_end}"
						noi dis as text "{p 4 4 2}{cmd:Survey:} "              in y "`=surveyname[1]'" as text " {p_end}"
						cap confirm variable mod, ex
						if _rc==0 noi dis as text "{p 4 4 2}{cmd:Module:} "              in y "`=mod[1]'" as text " {p_end}"								
						noi dis as text "{p 4 4 2}{cmd:Type:} "                in y "`=collection[1]'" as text " {p_end}"
						noi dis as text "{p 4 4 2}{cmd:Master Version:} "      in y "`=verm[1]'" as text " {p_end}"
						cap confirm variable vera, ex
						if _rc==0 noi dis as text "{p 4 4 2}{cmd:Alternative Version:} " in y "`=vera[1]'" as text " {p_end}"
						noi dis as text "{p 4 4 2}{cmd:Data file name(s):} "   in y "`=file[1]'" as text " {p_end}"
						noi dis as text "{p 4 4 2}{cmd:Last modified date(s):} "   in y "`=filelastmodifeddate[1]'" as text " {p_end}"
						dis as text "{hline}" _newline
						
						if "`nometa'"=="" _metadisplay, surveyid(`=trim("`ids'")')
						
						return local type `=collection[1]'	
						cap confirm variable mod, ex
						if _rc==0 return local module `=mod[1]'						
						return local verm `=verm[1]'						
						cap confirm variable vera, ex
						if _rc==0 return local vera `=vera[1]'						
						return local surveyid `ids'
						return local filename `=file[1]'
						return local idno `r(id)'
						
						// call the single file
						tempfile temp2
						plugin call _datalibweb, "`temp2'" "`collname'" "`country'" "`year'" "`col'" "`=file[1]'" /* "`para1'" "`para2'" "`para3'" "`para4'" */
						if `dlibrc'==0 {
							if "`dlibType'"=="dta" {							
								use `temp2', clear		
							} 							
							else if "`dlibType'"=="csv" {
								qui insheet using "`temp2'", clear
								noi dis as text in white "{p 4 4 2}`=errordetail[1]'{p_end}"								
								if errorcode[1] == 672 dis "{err}Please contact IT and activate UPI"
								if errorcode[1] == 673 dis "{err}Please register with the datalibweb system"
								if errorcode[1] == 674 dis "{err}Please subscribe the selected country/year/collection/module"
								if errorcode[1] == 675 dis "{err}Please check the syntax - countrycode/year/collection/etc."
								if errorcode[1] == 676 dis "{err}Please change countrycode/year/collection/etc."
								global errcode `=errorcode[1]'
								clear
								error 1
							}
							else {
								noi dis as text in white "{p 4 4 2}File not supported yet{p_end}"
								global errcode = 999
								clear
								error 1
							}
						}						
						else {
							if `dlibrc'==601 dis in red "Error code 601 - Internet bad url format"
							if `dlibrc'==602 dis in red "Error code 602 - Internet authentication canceled"
							if `dlibrc'==603 dis in red "Error code 603 - Internet connectivity failure"
							if `dlibrc'==604 dis in red "Error code 604 - Internet datalib server unreachable"
							if `dlibrc'==605 dis in red "Error code 605 - Internet unknown local error"
							if `dlibrc'==610 dis in red "Error code 610 - Response error invalid content type header"
							if `dlibrc'==611 dis in red "Error code 611 - Response error invalid file name header"
							if `dlibrc'==612 dis in red "Error code 612 - Response error invalid content length header"
							if `dlibrc'==613 dis in red "Error code 613 - Response error invalid file extension"
							if `dlibrc'==614 dis in red "Error code 614 - Response error invalid status header"
							
							if `dlibrc'==701 dis in red "Error code 701 - Plugin usage error, parameter list"
							if `dlibrc'==702 dis in red "Error code 702 - File I/O error, local file system access"
							global errcode `dlibrc'
							clear
							error 1
						}
						
					} // end of onefile
				} // end of one survey ID
			} //end of r(N)>0
			else {
				noi dis as text in white "{p 4 4 2}There is no survey/data with defined parameters: `col2', `country', `year', `surveyid'{p_end}"
				error 1
			} //_N==0
		} // csv type
		else if "`dlibType'"=="dta" { // only one file matched/subscribed - load the file  
			//tokenize the survey id or filename dlibFileName
			if "`surveyid'"=="" local strtoken "`dlibFileName'"
			else local strtoken "`surveyid'"
			local surveyid3 = subinstr("`strtoken'","_"," ",.)
			local surveyid3 = subinstr("`surveyid3'",".dta","",.)
			if `=wordcount("`surveyid3'")-1' >= 4 {
				tokenize "`surveyid3'"
				local surveyname `3'
				local verm       `4'
				if `=wordcount("`surveyid3'")-1' >= 5 {
					local vera `6'
					local mod `9'
				}
			}
			
			dis as text "{hline}"
			noi dis as text "{p 4 4 2}{cmd:Country:} "             in y "`=upper("`country'")'" as text " {p_end}"
			noi dis as text "{p 4 4 2}{cmd:Year:} "                in y "`year'" as text " {p_end}"
			noi dis as text "{p 4 4 2}{cmd:Survey:} "              in y "`surveyname'" as text " {p_end}"
			if "`9'"~="" noi dis as text "{p 4 4 2}{cmd:Module:} "              in y "`mod'" as text " {p_end}"								
			noi dis as text "{p 4 4 2}{cmd:Type:} "                in y "$type" as text " {p_end}"
			**noi dis as text "{p 4 4 2}{cmd:Type:} "                in y "`col'" as text " {p_end}"
			noi dis as text "{p 4 4 2}{cmd:Master Version:} "      in y "`verm'" as text " {p_end}"
			if "`6'"~="" noi dis as text "{p 4 4 2}{cmd:Alternative Version:} " in y "`vera'" as text " {p_end}"
			noi dis as text "{p 4 4 2}{cmd:Data file name(s):} "   in y "`dlibFileName'" as text " {p_end}"
			//noi dis as text "{p 4 4 2}{cmd:Last modified date:} "   in y "`dlibFileName'" as text " {p_end}"
			dis as text "{hline}" _newline
			
			if "`surveyid'"=="" local surveyid = subinstr("`strtoken'",".dta","",.)
			if "`nometa'"==""   _metadisplay, surveyid(`=trim("`surveyid'")')
			
			//load the data
			use `temp1', clear	
			return local type `col'
			return local module `mod'
			return local verm `verm'
			return local vera `vera'
			return local surveyid `surveyid'
			return local filename `dlibFileName'
			return local idno `r(id)'
		} // dta type
		else {
			noi dis as text in white "{p 4 4 2}File not supported yet{p_end}"	
			global errcode = 999
			error 1
		}
	
	} //end of _rc plugin
	else {	
		if `dlibrc'==601 dis in red "Error code 601 - Internet bad url format"
		if `dlibrc'==602 dis in red "Error code 602 - Internet authentication canceled"
		if `dlibrc'==603 dis in red "Error code 603 - Internet connectivity failure"
		if `dlibrc'==604 dis in red "Error code 604 - Internet datalib server unreachable"
		if `dlibrc'==605 dis in red "Error code 605 - Internet unknown local error"
		if `dlibrc'==610 dis in red "Error code 610 - Response error invalid content type header"
		if `dlibrc'==611 dis in red "Error code 611 - Response error invalid file name header"
		if `dlibrc'==612 dis in red "Error code 612 - Response error invalid content length header"
		if `dlibrc'==613 dis in red "Error code 613 - Response error invalid file extension"
		if `dlibrc'==614 dis in red "Error code 614 - Response error invalid status header"
		
		if `dlibrc'==701 dis in red "Error code 701 - Plugin usage error, parameter list"
		if `dlibrc'==702 dis in red "Error code 702 - File I/O error, local file system access"
		global errcode `dlibrc'
		clear
		error 1
	} //end else of _rc plugin
	
end
