BSTSVRSN	;GDIT/HS/BEE-Standard Terminology - Local File Handling ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;;Sep 10, 2014;Build 101
	;
	Q
	;
RESET(NMIEN)	;EP - Mark local entries as out of date
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSVRSN D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	;Get NMID - Only refresh 36
	S NMID=$$GET1^DIQ(9002318.1,NMIEN_",",.01,"I") I NMID'=36 Q
	;
	;Make sure process isn't running
	L +^BSTS(9002318.1,0):1 E  Q
	L -^BSTS(9002318.1,0)
	;
	;Make sure ICD9 to SNOMED background process isn't running
	L +^TMP("BSTSICD2SMD"):0 E  Q
	L -^TMP("BSTSICD2SMD")
	;
	;Queue the process off in the background
	D CDJOB^BSTSUTIL(NMIEN,"C")
	;
	Q
	;
RES	;EP - Mark Local Entries As Out of Date
	;
	NEW STS,BSTS,ERROR,VAR
	;
	;Passed in variables
	I $G(NMIEN)="" Q
	;
	;Perform lock so only one process is allowed
	L +^BSTS(9002318.1,0):1 E  Q
	;
	;Update LAST VERSION CHECK now so process won't keep getting called
	S BSTS(9002318.1,NMIEN_",",.05)=DT
	D FILE^DIE("","BSTS","ERROR")
	;
	NEW CRID,CIEN,NMID,VDTS,NVIEN,NVLCL
	;
	;Get NMID
	S NMID=$$GET1^DIQ(9002318.1,NMIEN_",",.01,"I") I NMID="" G XRES
	;
	;Loop through each concept in the codeset and make it Out of Date
	S VDTS="" F  S VDTS=$O(^BSTS(9002318.4,"D",NMID,VDTS)) Q:VDTS=""  S CIEN=0 F  S CIEN=$O(^BSTS(9002318.4,"D",NMID,VDTS,CIEN)) Q:'CIEN  D
	. ;
	. NEW BSTSUPD,ERR,TIEN
	. ;
	. ;Mark Entry as Out of Date
	. S BSTSUPD(9002318.4,CIEN_",",".11")="Y"
	. ;
	. ;Process the Associated Terms
	. S TIEN="" F  S TIEN=$O(^BSTS(9002318.3,"C",NMID,CIEN,TIEN)) Q:TIEN=""  D
	.. ;
	.. NEW BSTSTUPD,ERR
	.. ;
	.. ;Mark Entry as Out of Date
	.. S BSTSTUPD(9002318.3,TIEN_",",".11")="Y"
	.. D FILE^DIE("","BSTSTUPD","ERR")
	. ;
	. ;File entry
	. I $D(BSTSUPD) D FILE^DIE("","BSTSUPD","ERR")
	;
	;Update the current version
	;
	;Get the current version from the codeset multiple
	S NVIEN=$O(^BSTS(9002318.1,NMIEN,1,"A"),-1)
	S NVLCL="" I +NVIEN>0 D
	. NEW DA,IENS
	. S DA(1)=NMIEN,DA=+NVIEN,IENS=$$IENS^DILF(.DA)
	. S NVLCL=$$GET1^DIQ(9002318.11,IENS,".01","I")
	;
	;Now save it in the CURRENT VERSION field
	I NVLCL]"" D
	. NEW BSTS,ERROR
	. S BSTS(9002318.1,NMIEN_",",.04)=NVLCL
	. D FILE^DIE("","BSTS","ERROR")
	;
	;Unlock entry
XRES	L -^BSTS(9002318.1,0)
	;
	Q
	;
VCHK(NMID)	;EP - Daily check for new version
	;
	NEW NMIEN,LVCKDT,STS,BSTS,ERROR,VAR,CVLCL,NVIEN,NVLCL
	;
	S NMID=$G(NMID,"") S:NMID="" NMID=36 S:NMID=30 NMID=36
	;
	;Only process SNOMED
	I NMID'=36 G XCHECK
	;
	S NMIEN=$O(^BSTS(9002318.1,"B",NMID,"")) I NMIEN="" G XCHECK
	;
	;Pull last version check date
	S LVCKDT=$$GET1^DIQ(9002318.1,NMIEN_",",.05,"I")
	I LVCKDT'<DT G XCHECK
	;
	;Do not perform check if another process is already checking
	L +^BSTS("VERSION CHECK"):0 E  G XCHECK
	;
	;Do not perform check if background process is running
	L +^BSTS(9002318.1,0):0 E  G XCHECK
	L -^BSTS(9002318.1,0)
	;
	;Make sure ICD9 to SNOMED background process isn't running
	L +^TMP("BSTSICD2SMD"):0 E  G XCHECK
	L -^TMP("BSTSICD2SMD")
	;
	;Perform version check
	S STS=$$VERSIONS^BSTSAPI("VAR",NMID)
	;
	;Check for successful remote call - If failure, don't check again today
	I +STS'=2 D  G XCHECK
	. NEW BSTS,ERROR
	. S BSTS(9002318.1,NMIEN_",",.05)=DT
	. D FILE^DIE("","BSTS","ERROR")
	;
	;Pull the current version on file
	S CVLCL=$$GET1^DIQ(9002318.1,NMIEN_",",".04","I")
	;
	;Get the current version from the codeset multiple
	S NVIEN=$O(^BSTS(9002318.1,NMIEN,1,"A"),-1)
	S NVLCL="" I +NVIEN>0 D
	. NEW DA,IENS
	. S DA(1)=NMIEN,DA=+NVIEN,IENS=$$IENS^DILF(.DA)
	. S NVLCL=$$GET1^DIQ(9002318.11,IENS,".01","I")
	;
	;If the current version value isn't equal to the latest in the multiple need to process
	I NVLCL]"",CVLCL'=NVLCL D RESET^BSTSVRSN(NMIEN) G XCHECK
	;
	;Check to see if ICD9 to SMD process needs run - Only run once
	D PLOAD^BSTSUTIL(NMIEN)
	;
	;Mark the codeset as checked
	D
	. NEW BSTS,ERROR
	. S BSTS(9002318.1,NMIEN_",",.05)=DT
	. D FILE^DIE("","BSTS","ERROR")
	;
XCHECK	L -^BSTS("VERSION CHECK")
	Q
	;
SCHK(NMID)	;EP - Check for periodic subset updates
	;
	NEW LMDT,STS,BSTS,ERROR,ZTRTN,ZTSAVE,ZTDESC,ZTDTH,NMIEN
	NEW VAR,SITE,SDAYS,ZTIO,SUBLST,X1,X2,X,%H
	;
	S NMID=$G(NMID,"") S:NMID="" NMID=36 S:NMID=30 NMID=36
	;
	;Only run it for codeset 36
	I NMID'=36 Q
	;
	;Get Site Parameter IEN
	S SITE=$O(^BSTS(9002318,0)) Q:'SITE
	;
	;Get subset update days
	S SDAYS=$$GET1^DIQ(9002318,SITE_",",.02,"I") S:SDAYS="" SDAYS=60
	;
	;Make sure we have a codeset (namespace)
	S NMIEN=$O(^BSTS(9002318.1,"B",NMID,"")) Q:NMIEN=""
	;
	;Check if refresh needs run
	S LMDT=$$GET1^DIQ(9002318.1,NMIEN,".06","I")
	I LMDT>0 S X1=LMDT,X2=SDAYS D C^%DTC S LMDT=X
	I LMDT>DT Q
	;
	;Do not perform check if another process is already checking
	L +^BSTS("SUBSET CHECK"):0 E  Q
	;
	;Only one SNOMED background process can be running at a time
	L +^BSTS(9002318.1,0):1 E  G XSCHK
	L -^BSTS(9002318.1,0)
	;
	;Make sure ICD9 to SNOMED background process isn't running
	L +^TMP("BSTSICD2SMD"):0 E  G XSCHK
	L -^TMP("BSTSICD2SMD")
	;
	;Only run if server set up
	S STS=$$VERSIONS^BSTSAPI("VAR") ;Try a quick call to see if DTS is up
	I +STS'=2 D  G XSCHK
	. NEW X1,X2,X,BSTS,ERROR
	. ;
	. ;Find the date to use so that it won't run again until tomorrow
	. S X1=DT,X2=-SDAYS+1 D C^%DTC
	. ;
	. ;Update LAST SUBSET CHECK now so process won't keep getting called
	. S BSTS(9002318.1,NMIEN_",",.06)=X
	. D FILE^DIE("","BSTS","ERROR")
	;
	;Queue the process off in the background
	D CDJOB^BSTSUTIL(NMIEN,"S")
	;
XSCHK	L -^BSTS("SUBSET CHECK")
	Q
	;
SUB	;EP - Update IHS Standard Terminology Subsets
	;
	;Retrieve passed in variable
	S NMIEN=$G(NMIEN) I NMIEN="" Q
	;
	;Perform lock so only one process is allowed
	L +^BSTS(9002318.1,0):10 E  Q
	;
	NEW BSTS,ERROR,CIEN,BSTSSB,STS,CNC,SUBLST,SSCIEN,ICONC,SITE,SDAYS,NMID
	;
	;Get NMID
	S NMID=$$GET1^DIQ(9002318.1,NMIEN_",",.01,"I") I NMID="" Q
	;
	;Get Site Parameter IEN
	S SITE=$O(^BSTS(9002318,0)) Q:'SITE
	;
	;Get subset update days
	S SDAYS=$$GET1^DIQ(9002318,SITE_",",.02,"I") S:SDAYS="" SDAYS=60
	;
	;Try a quick call to make sure it works - if not set to run tomorrow
	S STS=$$SUBSET^BSTSAPI("SUBLST",NMID_"^2")
	I +STS'=2 D  G XSUB
	. NEW X1,X2,X,BSTS,ERROR,%H
	. ;
	. ;Find the date to use so that it won't run again until tomorrow
	. S X1=DT,X2=-SDAYS+1 D C^%DTC
	. ;
	. ;Update LAST SUBSET CHECK now so process won't keep getting called
	. S BSTS(9002318.1,NMIEN_",",.06)=X
	. D FILE^DIE("","BSTS","ERROR")
	;
	;Update LAST SUBSET CHECK now so process won't keep getting called
	S BSTS(9002318.1,NMIEN_",",.06)=DT
	D FILE^DIE("","BSTS","ERROR")
	;
	;Loop through concepts and clear out modified date for codes in codeset
	S ICONC="" F  S ICONC=$O(^BSTS(9002318.4,"C",NMID,ICONC)) Q:ICONC=""  D
	. S CIEN="" F  S CIEN=$O(^BSTS(9002318.4,"C",NMID,ICONC,CIEN)) Q:CIEN=""  D
	.. NEW CDSET,BSTS,ERR
	.. ;
	.. ;Skip partial entries
	.. I $$GET1^DIQ(9002318.4,CIEN_",",.03,"I")="P" Q
	.. ;
	.. ;Mark as out of date
	.. S BSTS(9002318.4,CIEN_",",".12")="@"
	.. D FILE^DIE("","BSTS","ERR")
	;
	;Process all subsets (SUBLST was created before call)
	S BSTSSB="" F  S BSTSSB=$O(SUBLST(BSTSSB)) Q:BSTSSB=""  D
	. ;
	. NEW IN,OUT,STS
	. ;
	. ;Skip SRCH* subsets since they are in IHS Problem List
	. S IN=SUBLST(BSTSSB)_"^^2"
	. I $E(IN,1,4)="SRCH" Q
	. ;
	. ;Pull the subset
	. S OUT=$NA(^TMP("BSTSSUPD",$J)) K @OUT
	. S STS=$$SUBLST^BSTSAPI(OUT,IN)
	. K @OUT
	;
	;Need to loop through list again to catch any deletes
	S ICONC="" F  S ICONC=$O(^BSTS(9002318.4,"C",NMID,ICONC)) Q:ICONC=""  S SSCIEN="" F  S SSCIEN=$O(^BSTS(9002318.4,"C",NMID,ICONC,SSCIEN)) Q:SSCIEN=""  D
	. ;
	. NEW LMOD,DTSID,STS,SBVAR,CDSET,X1,X2,X,%H
	. ;
	. ;Skip partial entries
	. I $$GET1^DIQ(9002318.4,SSCIEN_",",.03,"I")="P" Q
	. ;
	. ;Get last modified date for concept
	. S LMOD=$$GET1^DIQ(9002318.4,SSCIEN_",",".12","I")
	. I LMOD>0 S X1=LMOD,X2=SDAYS D C^%DTC S LMOD=X
	. ;
	. ;Skip if not out of date
	. I LMOD>DT Q
	. ;
	. ;Get DTSId
	. S DTSID=$$GET1^DIQ(9002318.4,SSCIEN_",",".08","I") Q:DTSID=""
	. ;
	. ;If Out of Date, retrieve detail from server
	. S STS=$$DTSLKP^BSTSAPI("SBVAR",DTSID)
	;
	;Unlock entry
XSUB	L -^BSTS(9002318.1,0)
	;
	Q
	;
SBRSET	;EP - BSTS REFRESH SUBSETS option
	;
	NEW II,NMID,NMIEN,BSTS,ERR,DIR,X,Y,DIC,CONC,CNT,DLAYGO
	;
	W !!,"This option refreshes the IHS BSTS Terminology Subsets"
	W !,"It will mark each concept's subsets as out of date and then"
	W !,"start a background process to retrieve the subsets assigned"
	W !,"to each concept."
	W !!
	S DIR("A")="Are you sure you want to do this"
	S DIR("B")="NO"
	S DIR(0)="Y"
	D ^DIR
	I '+Y Q
	;
	;Retrieve codeset
	S DIC=9002318.1,DIC(0)="AEM",DIC("A")="Select the codeset to refresh: "
	S DLAYGO=DIC D ^DIC
	I '+Y Q
	S NMID=$P(Y,U,2)
	S NMIEN=$P(Y,U)
	I '+Y Q
	;
	;Only one SNOMED background process can be running at a time
	L +^BSTS(9002318.1,0):1 E  W !!,"A Subset Refresh is Already Running. Please Try Later" H 3 Q
	L -^BSTS(9002318.1,0)
	;
	S DIR("A")="Start the process"
	S DIR("B")="NO"
	S DIR(0)="Y"
	D ^DIR
	I '+Y Q
	;
	;Remove the subset last checked date
CALL	;Clear LAST SUBSET CHECK
	S BSTS(9002318.1,NMIEN_",",.06)=""
	D FILE^DIE("","BSTS","ERR")
	;
	W !!,"Kicking off background process to set up subsets"
	D SCHK(NMID)
	H 2
	;
	Q
	;
ERR	;
	D ^%ZTER
	Q
