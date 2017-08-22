SELECT 

YEAR,
SITE_ID,


DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '1'")
	as [males],
	
DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '2'")	
	as [fems],
	
DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
  AND [SITE_ID] = '" & [SITE_ID] &"'
  AND [FK_SEX_CODE] = '2'
  AND [SHRIMP_EGG_DEVEL_CODE] In ('1','2') ")
  as [femsWEggs],

DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
  AND [SITE_ID] = '" & [SITE_ID] &"'
  AND [FK_SEX_CODE] = '2'
  AND [SHRIMP_EGG_DEVEL_CODE] In ('1','2','0') ")
  as [femsWValidEggCode],  

DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '1'") /
	
	(DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '2'") + 
	
	DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '1'"))
	as propMales,

DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '2'") /
	
	(DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '1'") + 
	
	DSUM("FREQUENCY", "tempAwlSel", "[YEAR] = '" & [YEAR] &"' 
	AND [SITE_ID] = '" & [SITE_ID] &"'
	AND [FK_SEX_CODE] = '2'"))
	as propFems,

propFems*	
[femsWEggs]/
[femsWValidEggCode]
	as propEgg
	
INTO
temp

FROM 
tempAwlSel
GROUP BY
 YEAR,SITE_ID
;