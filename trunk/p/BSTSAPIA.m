BSTSAPIA	;GDIT/HS/BEE-Standard Terminology API Program ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;**2**;Sep 10, 2014;Build 59
	;
	Q
	;
SEARCH(OUT,IN)	;EP - Perform Codeset Search
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - Search string
	;     - P2 - Search Type - (F-Fully specified name, S-Synonyms)
	;     - P3 (Optional) - Namespace ID - Default to SNOMED US EXT (#36)
	;                     ID      NAME
	;                     5180    FDA UNII
	;                     32773   GMRA Allergies with Maps
	;                     32772   GMRA Signs Symptoms
	;                     32771   IHS VANDF
	;                     32774   IHS Med Route
	;                     1552    RxNorm R
	;                     36      SNOMED CT US Extension
	; 
	;     - P4 (Optional) - Subset(s) to filter on (delimited by "~")
	;                       If blank default to "IHS Problem List". For SNOMED lookups
	;                       passing "ALL" searches on all available SNOMED terms.
	;     - P5 (Optional) - Date to check (default to T+2)
	;     - P6 (Optional) - Maximum number of concepts/terms to return (default 25)
	;     - P7 (Optional) - Return Info (P-Preferred,S-Synonym,B-Subset,I-IsA
	;                       X-ICD9/ICD10,C-Children,A-Associations,V-Inv Assoc)
	;                       (Default is all - "PSBIXCAV")
	;     - P8 (Optional) - Pass 1 to NOT return Add/Retire date info
	;     - P9 (Optional) - Batch Return - Start at record #
	;                       (used in conjunction with P7)
	;     - P10 (Optional) - Batch Return - # of concepts to return per batch
	;                       (used in conjunction with P6)
	;     - P11 (Optional) - LOCAL - Pass 1 to perform local listing, otherwise leave
	;                        blank for remote listing
	;     - P12 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]
	; [1] - 2:Remote information returned
	;       1:Local information returned
	;       0:No Information Returned
	; [2] - Primary Remote Error Message
	; [3] - Secondary Remote Error Message (if applicable)
	;
	; VAR(#) - List of Records
	;
	; The VAR(#) list of records returns the following sections
	; (based on the input piece 6 and 7 values):
	;
	;Concept ID/DTSID
	; VAR(#,"CON")=Concept Id
	; VAR(#,"DTS")=Internal DTS Id
	;
	;Fully Specified Name
	; VAR(#,"FSN","DSC")=Description Id of the FSN
	; VAR(#,"FSN","TRM")=Fully Specified Name
	; VAR(#,"FSN","XADT")=Date Added
	; VAR(#,"FSN","XRDT")=Date Retired
	;
	;ICD9 Information - Multiple Records Returned (CTR)
	; VAR(#,"ICD",CTR,"COD")=ICD9 Code
	; VAR(#,"ICD",CTR,"TYP")=Code Type(ICD)
	; VAR(#,"ICD",CTR,"XADT")=Date Added
	; VAR(#,"ICD",CTR,"XRDT")=Date Retired
	;
	;IsA Information - Multiple Records Returned (CTR)
	; VAR(#,"ISA",CTR,"CON")=Concept Id of IsA Term (may be blank prior to detail lookup)
	; VAR(#,"ISA",CTR,"DTS")=DTSId of the IsA Term
	; VAR(#,"ISA",CTR,"TRM")=IsA Term Name
	; VAR(#,"ISA",CTR,"XADT")=Date Added
	; VAR(#,"ISA",CTR,"XRDT")=Date Retired
	;
	;Association Information (SNOMED) - Multiple Records Returned (CTR)
	; VAR(#,"ASM",CTR,"CON")=SNOMED Concept CT Association
	; VAR(#,"ASM",CTR,"DTS")=DTSId of the SNOMED Concept
	;
	;Association Information (RxNorm) - Multiple Records Returned (CTR)
	; VAR(#,"ARX",CTR,"CON")=RxNorm Code Value Association
	; VAR(#,"ARX",CTR,"DTS")=DTSId of the RxNorm Concept
	;
	;Association Information (UNII) - Multiple Records Returned (CTR)
	; VAR(#,"ASN",CTR,"CON")=UNII Code Value Association
	; VAR(#,"ASN",CTR,"DTS")=DTSId of the UNII Concept
	;
	;Inverse Association Information (RxNorm) - Multiple Records Returned (CTR)
	; VAR(#,"IAR",CTR,"CON")=RxNorm Code Value of Inverse Association
	; VAR(#,"IAR",CTR,"DTS")=DTSId of the RxNorm Concept
	; VAR(#,"IAR",CTR,"TRM")=Inverse Association Term
	;
	;Child Information - Multiple Records Returned (CTR)
	; VAR(#,"CHD",CTR,"CON")=Concept Id of Child Term (may be blank prior to detail lookup)
	; VAR(#,"CHD",CTR,"DTS")=DTSId of the Child Term
	; VAR(#,"CHD",CTR,"TRM")=IsA Term Name
	; VAR(#,"CHD",CTR,"XADT")=Date Added
	; VAR(#,"CHD",CTR,"XRDT")=Date Retired
	;
	;Lookup Problem Column Value (Preferred Term Information for concept for Search Type
	;[F] or Synonym or Preferred Term Information for Search Type [S])
	;(Based on Search Type parameter - F/S)
	; VAR(#,"PRB","DSC")=Description Id of a Pref Term (Type F) or Synonym/Pref Term (S)
	; VAR(#,"PRB","TRM")=Preferred Name of a Concept (F) or a Synonym/Preferred Name (S)
	;
	;Preferred Term Information
	; VAR(#,"PRE","DSC")=Description ID of Preferred Term
	; VAR(#,"PRE","TRM")=Preferred Term
	; VAR(#,"PRE","XADT")=Date Added
	; VAR(#,"PRE","XRDT")=Date Retired
	;
	;Subset Information - Multiple Records Returned (CTR)
	; VAR(#,"SUB",CTR,"SUB")=Subset Name
	; VAR(#,"SUB",CTR,"XADT")=Date Added
	; VAR(#,"SUB",CTR,"XRDT")=Date Retired
	;
	;Synonym Information - Multiple Records Returned (CTR)
	; VAR(#,"SYN",CTR,"DSC")=Description ID of Synonym
	; VAR(#,"SYN",CTR,"TRM")=Synonym Term
	; VAR(#,"SYN",CTR,"XADT")=Date Added
	; VAR(#,"SYN",CTR,"XRDT")=Date Retired
	;
	;Date Concept Added/Retired
	; VAR(#,"XADT")=Date Added
	; VAR(#,"XRDT")=Date Retired
	;
	;RxNorm Only - TTY
	; VAR(#,"TTY",CTR,"TTY")=TTY Code
	; VAR(#,"TTY",CTR,"XADT")=Date Added
	; VAR(#,"TTY",CTR,"XRDT")=Date Retired
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIA D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	N SEARCH,STYPE,NMID,SUB,SNAPDT,MAX,BCTCHRC,BCTCHCT,LOCAL
	N RESULT,DEBUG,BSTSR,BSTSI,RET,DAT,BSTSWS,BSTSD,X,%,%H,%D
	K @OUT
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S SEARCH=$P(IN,U) Q:($TR(SEARCH," ")="") "0^Invalid Search String"
	S STYPE=$P(IN,U,2) I STYPE'="F",STYPE'="S" Q "0^Invalid Search Type"
	S NMID=$P(IN,U,3) S:NMID="" NMID=36 S:NMID=30 NMID=36
	S SUB=$P(IN,U,4)
	S SNAPDT=$P(IN,U,5) S:SNAPDT]"" SNAPDT=$$DATE^BSTSUTIL(SNAPDT)
	S:SNAPDT="" SNAPDT=$$DTCHG^BSTSUTIL(DT,2)_".0001"
	S SNAPDT=$$FMTE^BSTSUTIL(SNAPDT)
	S MAX=$P(IN,U,6) S:'MAX MAX=25
	S RET=$P(IN,U,7) S:RET="" RET="PSBIXCAV"
	S DAT=$P(IN,U,8)
	S BCTCHRC=$P(IN,U,9)
	S BCTCHCT=$P(IN,U,10) I BCTCHRC,'BCTCHCT S BCTCHCT=MAX
	S LOCAL=$P(IN,U,11),LOCAL=$S(LOCAL=1:"1",1:"")
	S DEBUG=$P(IN,U,12),DEBUG=$S(DEBUG=1:"1",1:"")
	S LOCAL="1" S DEBUG="1" ; GPL FOR TESTING
	;
	;Check for new version
	;D VCHK^BSTSVRSN(NMID) ; GPL FOR TESTING
	;
	;Periodic subset check
	;D SCHK^BSTSVRSN(NMID) ; GPL FOR TESTING
	;
	S BSTSWS("SEARCH")=SEARCH
	S BSTSWS("STYPE")=STYPE
	S BSTSWS("NAMESPACEID")=NMID
	S BSTSWS("SUBSET")=SUB
	S BSTSWS("SNAPDT")=SNAPDT
	S BSTSWS("MAXRECS")=MAX
	S BSTSWS("BCTCHRC")=BCTCHRC
	S BSTSWS("BCTCHCT")=BCTCHCT
	S BSTSWS("RET")=RET
	S BSTSWS("DAT")=DAT
	;
	S BSTSI=0
	;
	;Make DTS search call
	S BSTSR=1
	;
	;DTS Call
	I LOCAL'=1 S BSTSR=$$SEARCH^BSTSWSV("RESULT",.BSTSWS,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;If no results, try performing local search
	I $D(RESULT)<10 D
	. ;
	. ;Since in local, switch out of "ALL" search
	. S:BSTSWS("SUBSET")="ALL" BSTSWS("SUBSET")=""
	. ;
	. ;Make the local call
	. S BSTSD=$$SRC^BSTSSRCH("RESULT",.BSTSWS)
	;
	;Loop through search results and retrieve detail
	S BSTSD=$$DETAIL^BSTSSRCH(OUT,.BSTSWS,.RESULT)
	S $P(BSTSR,U)=$S(BSTSD=0:0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
CODESETS(OUT,IN)	;EP - Return list of available codesets
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 (Optional) - LOCAL - Pass 1 to perform local listing, otherwise leave
	;                       blank for remote listing
	;     - P2 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]
	; [1] - 2:Remote information returned
	;       1:Local information returned
	;       0:No Information Returned
	; [2] - Primary Remote Error Message
	; [3] - Secondary Remote Error Message (if applicable)
	;
	; VAR(#) - [1]^[2]^[3]
	; [1] - Codeset Id
	; [2] - Codeset Code
	; [3] - Codeset Name
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIA D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	N LOCAL,DEBUG,BSTSR,CDCD,CDIEN,BSTSI,X,%,%H,%D
	K @OUT
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S LOCAL=$P(IN,U),LOCAL=$S(LOCAL=1:"1",1:"")
	S DEBUG=$P(IN,U,2),DEBUG=$S(DEBUG=1:"1",1:"")
	S LOCAL="1" S DEBUG="1" ; GPL FOR TESTING
	;
	S BSTSI=0
	;
	;Make update call
	S BSTSR=1
	I LOCAL'=1 S BSTSR=$$GCDSET^BSTSWSV(DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;Loop through files and retrieve results
	S CDCD="" F  S CDCD=$O(^BSTS(9002318.1,"C",CDCD)) Q:CDCD=""  D
	. S CDIEN="" F  S CDIEN=$O(^BSTS(9002318.1,"C",CDCD,CDIEN)) Q:CDIEN=""  D
	.. NEW CDID,CDCODE,CDNAME
	.. S CDID=$$GET1^DIQ(9002318.1,CDIEN_",",.01,"E") Q:CDID=""
	.. S CDCODE=$$GET1^DIQ(9002318.1,CDIEN_",",.02,"E") Q:CDCODE=""
	.. S CDNAME=$$GET1^DIQ(9002318.1,CDIEN_",",.03,"E")
	.. S BSTSI=BSTSI+1,@OUT@(BSTSI)=CDID_U_CDCODE_U_CDNAME
	S $P(BSTSR,U)=$S(BSTSI=0:0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
VERSIONS(OUT,IN)	;EP - Return a list of available versions for a code set
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 (Optional) - Namespace ID - Default to SNOMED US EXT (#36)
	;     - P2 (Optional) - LOCAL - Pass 1 to perform local listing, otherwise leave
	;                       blank for remote listing
	;     - P3 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]
	; [1] - 2:Remote information returned
	;       1:Local information returned
	;       0:No Information Returned
	; [2] - Primary Remote Error Message
	; [3] - Secondary Remote Error Message (if applicable)
	;
	; VAR(#) - [1]^[2]^[3]^[4]
	; [1] - Version Id
	; [2] - Version Name
	; [3] - Version Release Date
	; [4] - Version Install Date (if available)
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIA D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	N LOCAL,DEBUG,BSTSR,NMID,NMIEN,BSTSI,VRID,X,%,%H,%D
	K @OUT
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S NMID=$P(IN,U) S:NMID="" NMID=36 S:NMID=30 NMID=36
	S LOCAL=$P(IN,U,2),LOCAL=$S(LOCAL=1:"1",1:"")
	S DEBUG=$P(IN,U,3),DEBUG=$S(DEBUG=1:"1",1:"")
	;
	S BSTSI=0
	;
	;Make update call
	S BSTSR=1
	I LOCAL'=1,NMID S BSTSR=$$GVRSET^BSTSWSV(NMID,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;Loop through files and retrieve results
	S NMIEN=$O(^BSTS(9002318.1,"B",NMID,""))
	I NMIEN]"" S VRID="" F  S VRID=$O(^BSTS(9002318.1,NMIEN,1,"B",VRID)) Q:VRID=""  D
	. N VRIEN
	. S VRIEN="" F  S VRIEN=$O(^BSTS(9002318.1,NMIEN,1,"B",VRID,VRIEN)) Q:VRIEN=""  D
	.. NEW VRNAME,VRRLDT,VRINDT,DA,IENS
	.. S DA(1)=NMIEN,DA=VRIEN,IENS=$$IENS^DILF(.DA)
	.. S VRNAME=$$GET1^DIQ(9002318.11,IENS,.02,"E") Q:VRNAME=""
	.. S VRRLDT=$$FMTE^XLFDT($$GET1^DIQ(9002318.11,IENS,.03,"I"),"5D")
	.. S VRINDT=$$FMTE^XLFDT($$GET1^DIQ(9002318.11,IENS,.04,"I"),"5D")
	.. S BSTSI=BSTSI+1,@OUT@(BSTSI)=VRID_U_VRNAME_U_VRRLDT_U_VRINDT
	S $P(BSTSR,U)=$S(BSTSI=0:0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
SUBSET(OUT,IN)	;EP - Return the list of subsets available for a Code Set
	;
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 (Optional) - Namespace ID - Default to SNOMED US EXT (#36)
	;     - P2 (Optional) - LOCAL - Pass 1 OR leave blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P3 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]
	; [1] - 2:Remote information returned
	;       1:Local information returned
	;       0:No Information Returned
	; [2] - Primary Remote Error Message
	; [3] - Secondary Remote Error Message (if applicable)
	;
	; VAR(#) - [1]
	; [1] - Subset
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIA D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	N SUB,NMID,CNT,X,%,%H,RESULT,NMIEN,BSTSR,LOCAL,DEBUG,%D
	K @OUT
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S NMID=$P(IN,U) S:NMID="" NMID=36 S:NMID=30 NMID=36
	S LOCAL=$P(IN,U,2),LOCAL=$S(LOCAL=2:"",1:"1")
	S DEBUG=$P(IN,U,3),DEBUG=$S(DEBUG=1:"1",1:"")
	;
	;Make sure we have a codeset (namespace)
	S NMIEN=$O(^BSTS(9002318.1,"B",NMID,"")) Q:NMIEN=""
	;
	;Make update call
	S BSTSR=1
	I LOCAL'=1,NMID S BSTSR=$$SUBSET^BSTSWSV("RESULT",NMID,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	I $D(RESULT)>9 M @OUT=RESULT
	;
	;If no results from call get from local
	I $D(RESULT)<10 S $P(BSTSR,U)=1,SUB="",CNT=0 F  S SUB=$O(^BSTS(9002318.4,"E",NMIEN,SUB)) Q:SUB=""  D
	. S CNT=CNT+1
	. S @OUT@(CNT)=SUB
	;
	;Mark if no results
	I $D(@OUT)<10 S $P(BSTSR,U)=0
	Q BSTSR
	;
DESC(IN)	;PEP - Returns detail information for a specified Description Id
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - The Description Id to look up
	;     - P2 (Optional) - The code set Id (default SNOMED US EXT '36')
	;     - P3 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P4 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]
	; [1] - Concept Id
	; [2] - Term Description
	;
	NEW VAR,RES,STS,ICD,IC,%D
	S STS=$$DSCLKP^BSTSAPIB("VAR",$G(IN))
	S RES=$G(VAR(1,"CON"))_U_$G(VAR(1,"PRB","TRM"))
	;
	;Tack on ICD9
	S ICD="",IC="" S IC="" F  S IC=$O(VAR(1,"ICD",IC)) Q:IC=""  D
	. NEW ICTYP,ICCOD
	. S ICCOD=$G(VAR(1,"ICD",IC,"COD")) Q:IC=""
	. S ICTYP=$G(VAR(1,"ICD",IC,"TYP")) Q:ICTYP'="IC9"
	. S ICD=ICD_$S(ICD]"":";",1:"")_ICCOD
	S RES=RES_U_ICD
	;
	Q RES
	;
CONC(IN)	;PEP - Returns basic information for a specified Concept Id
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - The Concept Id to look up
	;     - P2 (Optional) - The code set Id (default SNOMED '36')
	;     - P3 (Optional) - Snapshot Date to check (default DT)
	;     - P4 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P5 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]^[4]
	; [1] - Description Id of Fully Specified Name
	; [2] - Fully Specified Name
	; [3] - Description Id of Preferred Term
	; [4] - Preferred Term
	;
	NEW VAR,RES,STS,ICD,IC,%D
	S STS=$$CNCLKP^BSTSAPIB("VAR",$G(IN))
	S RES=$G(VAR(1,"FSN","DSC"))_U_$G(VAR(1,"FSN","TRM"))_U_$G(VAR(1,"PRE","DSC"))_U_$G(VAR(1,"PRE","TRM"))
	;
	;Tack on ICD9
	S ICD="",IC="" S IC="" F  S IC=$O(VAR(1,"ICD",IC)) Q:IC=""  D
	. NEW ICTYP,ICCOD
	. S ICCOD=$G(VAR(1,"ICD",IC,"COD")) Q:IC=""
	. S ICTYP=$G(VAR(1,"ICD",IC,"TYP")) Q:ICTYP'="IC9"
	. S ICD=ICD_$S(ICD]"":";",1:"")_ICCOD
	S RES=RES_U_ICD
	;
	Q RES
	;
ERR	;
	D ^%ZTER
	Q
