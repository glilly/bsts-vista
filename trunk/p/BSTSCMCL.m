BSTSCMCL	;GDIT/HS/BEE-Standard Terminology Cache Method Calls ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;**2**;Sep 10, 2014;Build 59
	;
	Q
	;
FSNSRCH(BSTSWS,RSLT)	;EP - DTS4 Fully Specified Name Search
	;
	;Perform Concept Search
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).FindConceptsWithNameMatching(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
CONSRCH(BSTSWS,RSLT)	;EP - DTS4 Concept Search
	;
	;Perform Concept Search
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).executeConceptTextSearch(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
DSCSRCH(BSTSWS,RSLT)	;EP - DTS4 Description Id Search
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).findDescWithIdMatch(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
TRMSRCH(BSTSWS,RSLT)	;EP - DTS4 Term Search
	;
	;Perform Concept Search
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).FullTextSearch(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
SUBLST(BSTSWS,RSLT)	;EP - DTS4 Retrieve Subset List
	;
	;Retrieve list of concepts in a specified subset
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).getSubsetList(.BSTSWS,.RSLT)" X EXEC
	;
	;Note - do not perform time check - this could be a longer running call
	;
	Q STS
	;
ICD2SMD(BSTSWS,RSLT)	;EP - DTS4 Retrieve ICD9 to SNOMED mappings
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).getICD9toSNOMED(.BSTSWS,.RSLT)" X EXEC
	;
	;Note - do not perform time check - this could be a longer running call
	;
	Q STS
	;
CNCSR(BSTSWS,RSLT)	;EP - DTS4 Lookup on Concept Id
	;
	;Perform Concept Id Search
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).findConceptsWithPropMatch(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
DETAIL(BSTSWS,RSLT)	;EP - DTS4 Retrieve Concept Detail
	;
	;Place call to retrieve detail for a concept
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).GetConceptDetail(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	;D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
FDESC(BSTSWS)	;EP - DTS4 Retrieve FSN Description Id
	;
	NEW RSLT,STS,EXEC
	;
	;Place call to retrieve the description id for a FSN
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).findTermsByName(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
GCDSDTS4(BSTSWS,RESULT)	;EP - DTS4 update codesets
	;
	;Place call to retrieve codesets (namespaces)
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).GetNamespaces(.BSTSWS,.RESULT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
GVRDTS4(BSTSWS)	;EP - DTS4 update versions
	;
	;Place call to retrieve versions
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).GetVersions(.BSTSWS)" X EXEC
	;
	;Note - do not perform server status check because the check
	;       uses this call
	Q STS
	;
PTYDTS4(BSTSWS,RSLT)	;EP - DTS4 Perform Property Search
	;
	;Search based on property value
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).propertyLookup(.BSTSWS,.RSLT)" X EXEC
	;
	;Check server status
	D SWLCL^BSTSWSV1(.BSTSWS,.STS)
	;
	Q STS
	;
SUBSET(BSTSWS,RESULT)	;EP - DTS4 get subset list
	;
	;Place call to retrieve list of subsets
	NEW STS,EXEC
	S STS="",EXEC="S STS=##class(BSTS.SOAP.WebFunctions).getListofSubsets(.BSTSWS,.RESULT)" X EXEC
	;
	;Note - do not perform time check - this could be a longer running call
	;
	Q STS
