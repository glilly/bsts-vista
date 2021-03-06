BSTSAPID	;GDIT/HS/BEE-Standard Terminology API Program ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;**2**;Sep 10, 2014;Build 59
	;
	Q
	;
ICD2SMD(OUT,IN)	;EP - Return ICD9 to SNOMED mappings
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - ICD9 Code
	;     - P2 (Optional) - Return Info (P-Preferred,S-Synonym,B-Subset,I-IsA
	;                       X-ICD9/ICD10,C-Children) (Default is all - "CI")
	;     - P3 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P4 (Optional) - DEBUG - Pass 1 to display debug information
	;     - P5 (Optional) - Date to search on (FileMan format - Default to T+2)
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
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIC D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	;
	N SEARCH,STYPE,NMID,SUB,SNAPDT,MAX,BCTCHRC,BCTCHCT,LOCAL,%D
	N RESULT,DEBUG,BSTSR,BSTSI,RET,DAT,BSTSWS,BSTSD,X,%,%H
	K @OUT
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S SEARCH=$P(IN,U) Q:($TR(SEARCH," ")="") "0^Invalid Search String"
	S NMID=36
	S SNAPDT=$P(IN,U,5) S:SNAPDT="" SNAPDT=$$DTCHG^BSTSUTIL(DT,2)
	S SNAPDT=SNAPDT_".2400"
	S SNAPDT=$$FMDT2XML^BSTSUTIL(SNAPDT)
	S MAX=100000
	S RET=$P(IN,U,2) S:RET="" RET="BCI"
	S DAT=1
	S BCTCHRC=""
	S BCTCHCT=""
	S LOCAL=$P(IN,U,3),LOCAL=$S(LOCAL=2:"",1:"1")
	S DEBUG=$P(IN,U,4),DEBUG=$S(DEBUG=1:"1",1:"")
	;
	S BSTSWS("SEARCH")=SEARCH
	S BSTSWS("NAMESPACEID")=NMID
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
	I LOCAL'=1 S BSTSR=$$ICD2SMD^BSTSWSV("RESULT",.BSTSWS,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;If no results, try performing local search
	I $D(RESULT)<10 S BSTSR=$$ICD2SMD^BSTSAPIF("RESULT",BSTSWS("SEARCH"))
	;
	;If local search and no results try doing DTS lookup
	I $D(RESULT)<10,LOCAL S BSTSR=$$ICD2SMD^BSTSWSV("RESULT",.BSTSWS,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;Loop through search results and retrieve detail
	S BSTSWS("STYPE")="F"
	S BSTSD=$$DETAIL^BSTSSRCH(OUT,.BSTSWS,.RESULT)
	S $P(BSTSR,U)=$S(BSTSD=0:0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
CVRSN(OUT,IN)	;EP - Return the Current Version For the Code Set
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 (Optional) - The code set Id (default SNOMED US EXT '36')
	;  IN - P2 (Optional) - LOCAL - Pass 1 to perform local listing, otherwise leave
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
	; Single VAR record is returned
	; @VAR = [1]^[2]^[3]^[4]
	; [1] - Version Id
	; [2] - Version Name
	; [3] - Version Release Date
	; [4] - Version Install Date (if available)
	;
	N LOCAL,DEBUG,BSTSR,NMID,NMIEN,BSTSI,VRID,X,%,%H,%D
	K @OUT
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPID D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
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
	;Loop through files and retrieve result
	S NMIEN=$O(^BSTS(9002318.1,"B",NMID,""))
	I NMIEN]"" S VRID=$O(^BSTS(9002318.1,NMIEN,1,"B",""),-1) I VRID]"" D
	. N VRIEN
	. S VRIEN=$O(^BSTS(9002318.1,NMIEN,1,"B",VRID,""),-1) I VRIEN]"" D
	.. NEW VRNAME,VRRLDT,VRINDT,DA,IENS
	.. S DA(1)=NMIEN,DA=VRIEN,IENS=$$IENS^DILF(.DA)
	.. S VRNAME=$$GET1^DIQ(9002318.11,IENS,.02,"E") Q:VRNAME=""
	.. S VRRLDT=$$FMTE^XLFDT($$GET1^DIQ(9002318.11,IENS,.03,"I"),"5D")
	.. S VRINDT=$$FMTE^XLFDT($$GET1^DIQ(9002318.11,IENS,.04,"I"),"5D")
	.. S @OUT=VRID_U_VRNAME_U_VRRLDT_U_VRINDT
	S $P(BSTSR,U)=$S(@OUT="":0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
DTSLKP(OUT,IN)	;Returns detail information for a specified DTS Id
	;
	;This tag is only called by the ICD2SMD tag in the ICD2SMD tag in BSTSAPIB
	;Only required data is returned
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - The DTS Id to look up
	;     - P2 (Optional) - The code set Id (default SNOMED US EXT '36')
	;     - P3 (Optional) - Snapshot Date to check (default T+2) - Not implemented this build
	;     - P4 (Optional) - LOCAL - Pass 1 to perform local listing, otherwise leave
	;                       blank for remote listing
	;     - P5 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]
	; [1] - 2:Remote information returned
	;       1:Local information returned
	;       0:No Information Returned
	; [2] - Primary Remote Error Message
	; [3] - Secondary Remote Error Message (if applicable)
	;
	; VAR(#) record is returned for a match
	; Information returned is in the same (full detail) format
	; as the detail returned for each record in the
	; search API
	;
	NEW $ESTACK,$ETRAP S $ETRAP="D ERR^BSTSAPIB D UNWIND^%ZTER" ; SAC 2009 2.2.3.17
	; 
	K @OUT
	;
	N RESULT,SEARCH,NMID,SNAPDT,MAX,LOCAL,DEBUG,BSTSWS,BSTSR,BSTSD,X,%,%H,DIFILE,%D
	;
	I $G(DT)="" D DT^DICRW
	S IN=$G(IN,"")
	S SEARCH=$P(IN,U) I 'SEARCH Q "0^Invalid DTS Id"
	S NMID=$P(IN,U,2) S:NMID="" NMID=36 S:NMID=30 NMID=36
	S SNAPDT=$P(IN,U,3) S:SNAPDT]"" SNAPDT=$$DATE^BSTSUTIL(SNAPDT)
	S:SNAPDT="" SNAPDT=$$DTCHG^BSTSUTIL(DT,2)_".0001"
	S SNAPDT=$$FMTE^BSTSUTIL(SNAPDT)
	S MAX=100
	S LOCAL=$P(IN,U,4),LOCAL=$S(LOCAL=1:"1",1:"")
	S DEBUG=$P(IN,U,5),DEBUG=$S(DEBUG=1:"1",1:"")
	;
	S BSTSWS("SEARCH")=SEARCH
	S BSTSWS("STYPE")="F"
	S BSTSWS("NAMESPACEID")=NMID
	S BSTSWS("SUBSET")=""
	S BSTSWS("SNAPDT")=SNAPDT
	S BSTSWS("MAXRECS")=MAX
	S BSTSWS("BCTCHRC")=""
	S BSTSWS("BCTCHCT")=""
	S BSTSWS("RET")="PCBIX"
	S BSTSWS("DAT")=""
	;
	;Make DTS Lookup call
	S BSTSR=1
	;
	I LOCAL'=1 S BSTSR=$$DTSSR^BSTSWSV("RESULT",.BSTSWS,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;If no results, try performing local search
	I $D(RESULT)<10 S BSTSD=$$DTS^BSTSLKP("RESULT",.BSTSWS)
	;
	;If no results and local, make DTS call
	I $D(RESULT)<10,LOCAL S BSTSR=$$DTSSR^BSTSWSV("RESULT",.BSTSWS,DEBUG) S:+BSTSR $P(BSTSR,U)=2
	;
	;Get the detail for the record
	S BSTSD=0
	I $D(RESULT)>1 D
	. S BSTSWS("STYPE")="F"
	. S BSTSD=$$DETAIL^BSTSSRCH(OUT,.BSTSWS,.RESULT)
	S $P(BSTSR,U)=$S(BSTSD=0:0,(+BSTSR)>0:+BSTSR,1:1)
	Q BSTSR
	;
ERR	;
	D ^%ZTER
	Q
