BSTSCDET	;GDIT/HS/BEE-Get Concept Detail ; 15 Nov 2012  4:26 PM
	;;1.0;IHS STANDARD TERMINOLOGY;**2**;Sep 10, 2014;Build 59
	Q
	;
DETAIL(OUT,BSTSWS,RESULT)	;EP - Return Details for each Concept/Term
	;
	;Input
	; BSTSWS Array
	; RESULT - [1]^[2]^[3]
	;          [1] - Concept ID
	;          [2] - DTS ID
	;          [3] - Description Id
	;
	;Output
	; Function returns - # Records Returned
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
	; VAR(#,"CHD",CTR,"TRM")=Child Term Name
	; VAR(#,"CHD",CTR,"XADT")=Date Added
	; VAR(#,"CHD",CTR,"XRDT")=Date Retired
	;
	;Lookup Problem Column Value (Fully Specified Name or a Synonym/Preferred Term)
	;(Based on Search Type parameter - F/S)
	; VAR(#,"PRB","DSC")=Description Id of a Pref Term (Type F) or Synonym/Pref Term (S)
	; VAR(#,"PRB","TRM")=Preferred term of a Concept (F) or a Synonym (S)
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
	N CNT,INMID,XNMID,NCNT,RET,DAT,STYPE
	;
	;Get the Namespace ID
	S XNMID=$G(BSTSWS("NAMESPACEID"))
	;
	;Pull return request
	S RET=$G(BSTSWS("RET"))
	S DAT=$G(BSTSWS("DAT"))
	S STYPE=$G(BSTSWS("STYPE"))
	S INMID=$O(^BSTS(9002318.1,"B",XNMID,""))
	;
	S NCNT=0,CNT="" F  S CNT=$O(RESULT(CNT)) Q:CNT=""  D
	. ;
	. N CONC,DESC,CIEN,ADT,RDT,PRB,PRBIEN,ICNT,ISIEN
	. N BCNT,SBIEN,ICIEN,SCNT,TIEN,DTSID,ACNT,PDESC,CHIEN
	. N ASCNT,ASIEN,ARCNT,ARIEN,AUCNT,AUIEN,IARCNT,IASIEN,TTCNT,TTIEN
	. ;
	. S CONC=$P(RESULT(CNT),U)
	. S DTSID=$P(RESULT(CNT),U,2)
	. S DESC=$P(RESULT(CNT),U,3)
	. ;
	. ;Get Concept IEN
	. S CIEN=$$CIEN^BSTSLKP(CONC,XNMID) Q:CIEN=""
	. S ADT=$$GET1^DIQ(9002318.4,CIEN,".05","I")
	. S RDT=$$GET1^DIQ(9002318.4,CIEN,".06","I")
	. ;
	. ;If FSN Search retrieve Description ID for Preferred Term
	. S PDESC=$P($$PDESC^BSTSSRCH(CIEN),U) Q:PDESC=""
	. I STYPE="F" S DESC=PDESC
	. ;
	. ;Determine PRB value - For now use FSN or SYN/PRE
	. S PRB=DESC
	. S PRBIEN=$O(^BSTS(9002318.3,"D",INMID,PRB,"")) Q:PRBIEN=""
	. S NCNT=NCNT+1
	. S @OUT@(NCNT,"PRB","TRM")=$$GET1^DIQ(9002318.3,PRBIEN_",",1)
	. S @OUT@(NCNT,"PRB","DSC")=DESC
	. S @OUT@(NCNT,"CON")=CONC
	. S @OUT@(NCNT,"DTS")=DTSID
	. I 'DAT S @OUT@(NCNT,"XADT")=ADT
	. I 'DAT S @OUT@(NCNT,"XRDT")=RDT
	. ;
	. ;Pull IsA Relationships
	. I RET["I" S (ACNT,ISIEN)=0 F  S ISIEN=$O(^BSTS(9002318.4,CIEN,5,ISIEN)) Q:'ISIEN  D
	.. ;
	.. N ISA,DA,IENS,ICONC,ADT,RDT,DTS,FSN
	.. S DA(1)=CIEN,DA=ISIEN,IENS=$$IENS^DILF(.DA)
	.. S ISA=$$GET1^DIQ(9002318.45,IENS,".01","I") Q:ISA=""
	.. S ADT=$$GET1^DIQ(9002318.45,IENS,".02","I")
	.. S RDT=$$GET1^DIQ(9002318.45,IENS,".03","I")
	.. S ICONC=$$GET1^DIQ(9002318.4,ISA,".02","I")
	.. S DTS=$$GET1^DIQ(9002318.4,ISA,".08","I")
	.. S FSN=$$GET1^DIQ(9002318.4,ISA,1,"I")
	.. S ACNT=ACNT+1
	.. S @OUT@(NCNT,"ISA",ACNT,"TRM")=FSN
	.. S @OUT@(NCNT,"ISA",ACNT,"CON")=ICONC
	.. S @OUT@(NCNT,"ISA",ACNT,"DTS")=DTS
	.. Q:DAT  ;Exclude ADT/RDT
	.. S @OUT@(NCNT,"ISA",ACNT,"XADT")=ADT
	.. S @OUT@(NCNT,"ISA",ACNT,"XRDT")=RDT
	. ;
	. ;RxNorm - Pull TTY
	. I XNMID=1552 S (TTCNT,TTIEN)=0 F  S TTIEN=$O(^BSTS(9002318.4,CIEN,12,TTIEN)) Q:'TTIEN  D
	.. ;
	.. N TTY,DA,IENS,ICONC,ADT,RDT,DTS,FSN
	.. S DA(1)=CIEN,DA=TTIEN,IENS=$$IENS^DILF(.DA)
	.. S TTY=$$GET1^DIQ(9002318.412,IENS,".01","I") Q:TTY=""
	.. S ADT=$$GET1^DIQ(9002318.412,IENS,".02","I")
	.. S RDT=$$GET1^DIQ(9002318.412,IENS,".03","I")
	.. S TTCNT=TTCNT+1
	.. S @OUT@(NCNT,"TTY",TTCNT,"TTY")=TTY
	.. Q:DAT  ;Exclude ADT/RDT
	.. S @OUT@(NCNT,"TTY",TTCNT,"XADT")=ADT
	.. S @OUT@(NCNT,"TTY",TTCNT,"XRDT")=RDT
	. ;
	. ;Pull Child Relationships
	. I RET["C" S (ACNT,CHIEN)=0 F  S CHIEN=$O(^BSTS(9002318.4,CIEN,6,CHIEN)) Q:'CHIEN  D
	.. ;
	.. N CHD,DA,IENS,ICONC,ADT,RDT,DTS,FSN
	.. S DA(1)=CIEN,DA=CHIEN,IENS=$$IENS^DILF(.DA)
	.. S CHD=$$GET1^DIQ(9002318.46,IENS,".01","I") Q:CHD=""
	.. S ADT=$$GET1^DIQ(9002318.46,IENS,".02","I")
	.. S RDT=$$GET1^DIQ(9002318.46,IENS,".03","I")
	.. S ICONC=$$GET1^DIQ(9002318.4,CHD,".02","I")
	.. S DTS=$$GET1^DIQ(9002318.4,CHD,".08","I")
	.. S FSN=$$GET1^DIQ(9002318.4,CHD,1,"I")
	.. S ACNT=ACNT+1
	.. S @OUT@(NCNT,"CHD",ACNT,"TRM")=FSN
	.. S @OUT@(NCNT,"CHD",ACNT,"CON")=ICONC
	.. S @OUT@(NCNT,"CHD",ACNT,"DTS")=DTS
	.. Q:DAT  ;Exclude ADT/RDT
	.. S @OUT@(NCNT,"CHD",ACNT,"XADT")=ADT
	.. S @OUT@(NCNT,"CHD",ACNT,"XRDT")=RDT
	. ;
	. ;Pull Associations
	. I RET["A" S (ASCNT,ARCNT,AUCNT,ASIEN)=0 F  S ASIEN=$O(^BSTS(9002318.4,CIEN,9,ASIEN)) Q:'ASIEN  D
	.. ;
	.. N COD,DA,IENS,NAM,DTS
	.. S DA(1)=CIEN,DA=ASIEN,IENS=$$IENS^DILF(.DA)
	.. S COD=$$GET1^DIQ(9002318.49,IENS,".01","I") Q:COD=""
	.. S NAM=$$GET1^DIQ(9002318.49,IENS,".02","I")
	.. S DTS=$$GET1^DIQ(9002318.49,IENS,".03","I")
	.. ;
	.. ;Define SNOMED, RxNorm, and UNII
	.. I (NAM=36)!(NAM=1552)!(NAM=5180) D
	... NEW CNT,NOD
	... S (CNT,NOD)=""
	... I NAM=36 S ASCNT=ASCNT+1,CNT=ASCNT,NOD="ASM"
	... I NAM=1552 S ARCNT=ARCNT+1,CNT=ARCNT,NOD="ARX"
	... I NAM=5180 S AUCNT=AUCNT+1,CNT=AUCNT,NOD="AUN"
	... Q:CNT=""
	... S @OUT@(NCNT,NOD,CNT,"CON")=COD
	... S @OUT@(NCNT,NOD,CNT,"DTS")=DTS
	. ;
	. ;Pull Inverse Associations
	. I RET["V" S (IARCNT,IASIEN)=0 F  S IASIEN=$O(^BSTS(9002318.4,CIEN,11,IASIEN)) Q:'IASIEN  D
	.. ;
	.. N COD,DA,IENS,NAM,DTS,TRM
	.. S DA(1)=CIEN,DA=IASIEN,IENS=$$IENS^DILF(.DA)
	.. S COD=$$GET1^DIQ(9002318.411,IENS,".01","I") Q:COD=""
	.. S NAM=$$GET1^DIQ(9002318.411,IENS,".02","I")
	.. S DTS=$$GET1^DIQ(9002318.411,IENS,".03","I")
	.. S TRM=$$GET1^DIQ(9002318.411,IENS,".04","I")
	.. ;
	.. ;Define RxNorm
	.. I (NAM=1552) D
	... NEW CNT,NOD
	... S (CNT,NOD)=""
	... I NAM=1552 S IARCNT=IARCNT+1,CNT=IARCNT,NOD="IAR"
	... Q:CNT=""
	... S @OUT@(NCNT,NOD,CNT,"CON")=COD
	... S @OUT@(NCNT,NOD,CNT,"DTS")=DTS
	... S @OUT@(NCNT,NOD,CNT,"TRM")=TRM
	. ;
	. ;Pull Subsets
	. I RET["B" S (BCNT,SBIEN)=0 F  S SBIEN=$O(^BSTS(9002318.4,CIEN,4,SBIEN)) Q:'SBIEN  D
	.. ;
	.. N SUB,DA,IENS,ADT,RDT
	.. S DA(1)=CIEN,DA=SBIEN,IENS=$$IENS^DILF(.DA)
	.. S SUB=$$GET1^DIQ(9002318.44,IENS,".01","I") Q:SUB=""
	.. S ADT=$$GET1^DIQ(9002318.44,IENS,".02","I")
	.. S RDT=$$GET1^DIQ(9002318.44,IENS,".03","I")
	.. S BCNT=BCNT+1
	.. S @OUT@(NCNT,"SUB",BCNT,"SUB")=SUB
	.. Q:DAT  ;Exclude ADT/RDT
	.. S @OUT@(NCNT,"SUB",BCNT,"XADT")=ADT
	.. S @OUT@(NCNT,"SUB",BCNT,"XRDT")=RDT
	. ;
	. ;Pull ICD9
	. I XNMID=36,RET["X" D
	.. S ICIEN=0 F  S ICIEN=$O(^BSTS(9002318.4,CIEN,3,ICIEN)) Q:'ICIEN  D
	... ;
	... N ICD,DA,IENS,ADT,RDT,ICDT
	... S DA(1)=CIEN,DA=ICIEN,IENS=$$IENS^DILF(.DA)
	... S ICD=$$GET1^DIQ(9002318.43,IENS,".02","I") Q:ICD=""
	... S:ICD'["." ICD=ICD_"."
	... S ICDT=$$GET1^DIQ(9002318.43,IENS,".03","I")
	... S ADT=$$GET1^DIQ(9002318.43,IENS,".04","I")
	... S RDT=$$GET1^DIQ(9002318.43,IENS,".05","I")
	... ;
	... ;Save Current Mapped value
	... I ICDT="IC9" D
	.... S ICNT("ICD")=$G(ICNT("ICD"))+1
	.... S @OUT@(NCNT,"ICD",ICNT("ICD"),"COD")=ICD
	.... S @OUT@(NCNT,"ICD",ICNT("ICD"),"TYP")=ICDT
	.... Q:DAT  ;Exclude ADT/RDT
	.... S @OUT@(NCNT,"ICD",ICNT("ICD"),"XADT")=ADT
	.... S @OUT@(NCNT,"ICD",ICNT("ICD"),"XRDT")=RDT
	.. ;
	.. ;Add in defaults if needed
	.. I $D(@OUT@(NCNT,"ICD"))<10 D
	... S @OUT@(NCNT,"ICD",1,"COD")=".9999"
	... S @OUT@(NCNT,"ICD",1,"TYP")="IC9"
	. ;
	. ;Set up FSN, Synonyms, Preferred
	. S SCNT=0,TIEN="" F  S TIEN=$O(^BSTS(9002318.3,"C",XNMID,CIEN,TIEN),-1) Q:TIEN=""  D
	.. N TRM,TYP,ADT,RDT,DSC
	.. ;
	.. S TYP=$$GET1^DIQ(9002318.3,TIEN_",",.09,"I") Q:TYP=""
	.. S TRM=$$GET1^DIQ(9002318.3,TIEN_",",1) Q:TRM=""
	.. S DSC=$$GET1^DIQ(9002318.3,TIEN_",",.02,"I") Q:DSC=""
	.. S ADT=$$GET1^DIQ(9002318.3,TIEN_",",.06,"I")
	.. S RDT=$$GET1^DIQ(9002318.3,TIEN_",",.07,"I")
	.. ;
	.. ;Handle multiple preferred terms - switch to synonym
	.. I $D(@OUT@(NCNT,"PRE")),TYP="P" S TYP="S"
	.. ;
	.. ;Synonyms
	.. I RET["S",TYP="S" D
	... S SCNT=SCNT+1,@OUT@(NCNT,"SYN",SCNT,"TRM")=TRM
	... S @OUT@(NCNT,"SYN",SCNT,"DSC")=DSC
	... Q:DAT  ;Exclude ADT/RDT
	... S @OUT@(NCNT,"SYN",SCNT,"XADT")=ADT
	... S @OUT@(NCNT,"SYN",SCNT,"XRDT")=RDT
	.. ;
	.. ;Fully specified name
	.. I TYP="F"!((XNMID=1552)&(TYP="P")) D
	... S @OUT@(NCNT,"FSN","TRM")=TRM
	... S @OUT@(NCNT,"FSN","DSC")=DSC
	... Q:DAT  ;Exclude ADT/RDt
	... S @OUT@(NCNT,"FSN","XADT")=ADT
	... S @OUT@(NCNT,"FSN","XRDT")=RDT
	.. ;
	.. ;Preferred term
	.. I RET["P",TYP="P" D
	... S @OUT@(NCNT,"PRE","TRM")=TRM
	... S @OUT@(NCNT,"PRE","DSC")=DSC
	... Q:DAT  ;Exclude ADT/RDT
	... S @OUT@(NCNT,"PRE","XADT")=ADT
	... S @OUT@(NCNT,"PRE","XRDT")=RDT
	... ;
	... ;If STYPE="F" switch problem value to preferred values
	.. I TYP="P",STYPE="F" D
	... S @OUT@(NCNT,"PRB","TRM")=TRM
	... S @OUT@(NCNT,"PRB","DSC")=DSC
	;
	Q NCNT
