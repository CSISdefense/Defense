USE [DIIG]
GO

/****** Object:  View [Project].[ProjectDetail]    Script Date: 11/7/2016 2:03:02 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








ALTER VIEW [Project].[ProjectDetail]
AS
SELECT      
CTID.CSIScontractID
,idv.CSISidvpiidID
,idv.idvpiid
,c.idvagencyid
,c.idvmodificationnumber
,cid.piid
,c.ContractNumber
,ctid.transactionnumber
,ctid.modnumber
,unique_transaction_id
,transaction_status
 ,signeddate
,effectivedate
,currentcompletiondate
,ultimatecompletiondate
,lastdatetoorder
,C.fiscal_year
--,max(C.Fiscal_Year) as MaxOfFiscalYear
,ISNULL(Agency.Customer,Agency.AgencyIDtext) AS Customer
,Agency.SubCustomer
,Agency.AgencyIDtext
,c.contractingofficeid
,cmcid.ContractingOfficeName
,cmcid.MajorCommandName as  ContractingMajorCommandName
,ISNULL(Funder.Customer,Funder.AgencyIDtext) AS Funder
,Funder.SubCustomer SubFunder
,Funder.AgencyIDtext as FundingAgencyIDtext
,fmcid.MajorCommandName as FundingMajorCommandName
,c.FundingRequestingOfficeid
,fmcid.ContractingOfficeName as FundingOfficeName
    ,maj_agency_cat
,mod_agency
,maj_fund_agency_cat
,tac.TreasuryAgencyCode
,tacname.SubCustomer as TACsubfunder
,mac.MainAccountCode
,isnull(nullif(c.account_title,''),mac.AccountTitle) as MainAccountTitle
,sac.SubAccountCode
,isnull(nullif(c.account_title,''),sac.AccountTitle) as SubAccountTitle
,GWAname.GWAagencyName
--OMB
,isnull(ombbureau.OMBagencyCode,ombagency.OMBagencyCode) as OMBagencycode
,ombbureau.bureaucode as OMBbureaucode
,ProductOrServiceArea
,isnull(cpc.PlatformPortfolio,PSC.PlatformPortfolio) as PlatformPortfolio
,cpc.ClaimantProgramCodeText
,Proj.ProjectID
,proj.ProjectName
,proj.ProjectAbbreviation
,psc.ProductOrServiceCode
,psc.ProductOrServiceCodeText as PSCText
,PSC.RnD_BudgetActivity
 ,descriptionofcontractrequirement as ContractRequirement

,vendorname
,vendoralternatename
,vendorlegalorganizationname
,vendordoingasbusinessname
,divisionname
,divisionnumberorofficecode
,vendorenabled
,vendorlocationdisableflag
,ccrexception
,streetaddress
,streetaddress2
,streetaddress3
,city
,state
,zipcode
,vendorcountrycode
,vendor_state_code
,vendor_cd
,congressionaldistrict
,vendorsitecode
,vendoralternatesitecode
,dunsnumber
,parentdunsnumber
,phoneno
,faxno
,registrationdate
,renewaldate
,mod_parent
,locationcode
,statecode
,pop_state_code
,placeofperformancecountrycode
,placeofperformancezipcode
,C.obligatedamount
,baseandexercisedoptionsvalue
,baseandalloptionsvalue
,C.numberofactions
,contractingofficeagencyid
,fundingrequestingagencyid
,fundedbyforeignentity
,c.contractactiontype
,reasonformodification
,c.typeofcontractpricing
,priceevaluationpercentdifference
,subcontractplan
,lettercontract
,multiyearcontract
,performancebasedservicecontract
,majorprogramcode
,contingencyhumanitarianpeacekeepingoperation
,contractfinancing
,costorpricingdata
,costaccountingstandardsclause
,descriptionofcontractrequirement
,purchasecardaspaymentmethod
,nationalinterestactioncode
,c.progsourceagency
,c.progsourceaccount
,c.progsourcesubacct
,account_title
,rec_flag
,c.typeofidc
,c.multipleorsingleawardidc
,programacronym
,pop_cd
,placeofperformancecongressionaldistrict
,psc_cat
,c.systemequipmentcode
,c.claimantprogramcode
,principalnaicscode
,informationtechnologycommercialitemcategory
,gfe_gfp
,useofepadesignatedproducts
,recoveredmaterialclauses
,seatransportation
,contractbundling
,consolidatedcontract
,countryoforigin
,placeofmanufacture
,manufacturingorganizationtype
,c.agencyid
,solicitationid
,c.extentcompeted
,reasonnotcompeted
,numberofoffersreceived
,commercialitemacquisitionprocedures
,commercialitemtestprogram
,smallbusinesscompetitivenessdemonstrationprogram
,a76action
,competitiveprocedures
,solicitationprocedures
,typeofsetaside
,localareasetaside
,evaluatedpreference
,fedbizopps
,research
,c.statutoryexceptiontofairopportunity
,organizationaltype
,firm8aflag
,hubzoneflag
,sdbflag
,issbacertifiedsmalldisadvantagedbusiness
,shelteredworkshopflag
,hbcuflag
,educationalinstitutionflag
,womenownedflag
,veteranownedflag
,srdvobflag
,localgovernmentflag
,minorityinstitutionflag
,aiobflag
,stategovernmentflag
,federalgovernmentflag
,minorityownedbusinessflag
,apaobflag
,tribalgovernmentflag
,baobflag
,naobflag
,saaobflag
,nonprofitorganizationflag
,isothernotforprofitorganization
,isforprofitorganization
,isfoundation
,haobflag
,ishispanicservicinginstitution
,emergingsmallbusinessflag
,hospitalflag
,contractingofficerbusinesssizedetermination
,is1862landgrantcollege
,is1890landgrantcollege
,is1994landgrantcollege
,isveterinarycollege
,isveterinaryhospital
,isprivateuniversityorcollege
,isschoolofforestry
,isstatecontrolledinstitutionofhigherlearning
,isserviceprovider
,receivescontracts
,receivesgrants
,receivescontractsandgrants
,isairportauthority
,iscouncilofgovernments
,ishousingauthoritiespublicortribal
,isinterstateentity
,isplanningcommission
,isportauthority
,istransitauthority
,issubchapterscorporation
,islimitedliabilitycorporation
,isforeignownedandlocated
,isarchitectureandengineering
,isdotcertifieddisadvantagedbusinessenterprise
,iscitylocalgovernment
,iscommunitydevelopedcorporationownedfirm
,iscommunitydevelopmentcorporation
,isconstructionfirm
,ismanufacturerofgoods
,iscorporateentitynottaxexempt
,iscountylocalgovernment
,isdomesticshelter
,isfederalgovernmentagency
,isfederallyfundedresearchanddevelopmentcorp
,isforeigngovernment
,isindiantribe
,isintermunicipallocalgovernment
,isinternationalorganization
,islaborsurplusareafirm
,islocalgovernmentowned
,ismunicipalitylocalgovernment
,isnativehawaiianownedorganizationorfirm
,isotherbusinessororganization
,isotherminorityowned
,ispartnershiporlimitedliabilitypartnership
,isschooldistrictlocalgovernment
,issmallagriculturalcooperative
,issoleproprietorship
,istownshiplocalgovernment
,istriballyownedfirm
,istribalcollege
,isalaskannativeownedcorporationorfirm
,iscorporateentitytaxexempt
,iswomenownedsmallbusiness
,isecondisadvwomenownedsmallbusiness
,isjointventurewomenownedsmallbusiness
,isjointventureecondisadvwomenownedsmallbusiness
,walshhealyact
,servicecontractact
,davisbaconact
,clingercohenact
,otherstatutoryauthority
,interagencycontractingauthority
,c.CSISCreatedDate
,c.CSISModifiedDate
,numberofemployees
,annualrevenue
,c.CSIStransactionID
,CSISUniqueIndexID
,CSISUniqueIndexIdentity
,PlaceofPerformanceCity
,prime_awardee_executive1
,prime_awardee_executive2
,prime_awardee_executive3
,prime_awardee_executive4
,prime_awardee_executive5
,prime_awardee_executive5_compensation
,prime_awardee_executive2_compensation
,prime_awardee_executive3_compensation
,prime_awardee_executive4_compensation
,prime_awardee_executive1_compensation
,last_modified_date
,TypeOfBusiness
,ContractActionTypeDD350
,TypeIndefiniteDeliveryContract
,headquartercode
,CAGE



						 --CountryCode.Country3LetterCodeText,
						 -- CountryCode.Region, 
						 -- pop_state_code as StateCode, 
						
						 --PSC.ProductOrServiceCodeText , 
						 --contractingofficeagencyid as ContractAgency, 
						 --C.contractingofficeid as ContractOffice, 
						 --CountryCode.ISOcountryCode as ISOCountryCode, 
						 --CountryCode.IsInternational as IsInternational,
						 --CountryCode.ISOGIS,						 
						  
						  -- CD.MinOfFiscal_Year


FROM Contract.FPDS AS C 
LEFT OUTER JOIN FPDSTypeTable.ProductOrServiceCode AS PSC 
ON C.productorservicecode = PSC.ProductOrServiceCode 

--Agency Link ups
	LEFT OUTER JOIN FPDSTypeTable.AgencyID AS Agency 
	ON C.contractingofficeagencyid = Agency.AgencyID 
		LEFT OUTER JOIN FPDSTypeTable.AgencyID AS Funder
	ON C.fundingrequestingagencyid = Funder.AgencyID 
				 

--Contracting Office LInk ups
left outer join office.ContractingAgencyIDofficeIDtoMajorCommandIDhistory cmcid
		on c.contractingofficeagencyid=cmcid.contractingagencyid and
		c.contractingofficeid=cmcid.contractingofficeid and
		c.fiscal_year=cmcid.fiscal_year

left outer join office.ContractingAgencyIDofficeIDtoMajorCommandIDhistory fmcid
		on c.fundingrequestingagencyid=fmcid.contractingagencyid and
		c.fundingrequestingofficeid=fmcid.contractingofficeid and
		c.fiscal_year=fmcid.fiscal_year

		--LEFT OUTER JOIN FPDSTypeTable.Country3LetterCode AS CountryCode ON 
		--C.placeofperformancecountrycode = CountryCode.Country3LetterCode
						 
left outer join FPDSTypeTable.ClaimantProgramCode  as cpc 
on cpc.ClaimantProgramCode=c.claimantprogramcode

--Block of CSISIDjoins
              left join contract.csistransactionid as CTID
                     on ctid.CSIStransactionID=c.CSIStransactionID
              left join contract.CSISidvmodificationID as idvmod
                     on idvmod.CSISidvmodificationID=ctid.CSISidvmodificationID
              left join contract.CSISidvpiidID as idv
                     on idv.CSISidvpiidID=idvmod.CSISidvpiidID
              left join contract.CSIScontractID as cid
                     on cid.CSIScontractID=ctid.CSIScontractID					


              left join Contract.ContractLabelID label
                     on coalesce(ctid.ContractLabelID,cid.COntractlabelid,idv.ContractLabelID) = label.ContractLabelID
              LEFT JOIN Project.SystemEquipmentCodetoProjectIDhistory as SYS
                     ON SYS.systemequipmentcode=C.systemequipmentcode
                     and SYS.StartFiscalYear <= c.fiscal_year
                     and isnull(SYS.EndFiscalYear,9999) >= c.fiscal_year
              left join project.projectID Proj
                     on proj.projectid=isnull(sys.projectid,label.PrimaryProjectID)

--Translate from prog source to standardized budget codes
left outer join budget.progsource progsource
	on c.progsourceagency=progsource.progsourceagency
	and c.progsourceaccount=progsource.progsourceaccount
	and c.progsourcesubacct=progsource.progsourcesubacct

--Account Code link ups
left outer join agency.TreasuryAgencyCode as tac
	on progsource.treasuryagencycode=tac.TreasuryAgencyCode
left outer join budget.MainAccountCode mac
	on progsource.mainaccountcode=mac.MainAccountCode
		and progsource.treasuryagencycode=mac.TreasuryAgencyCode
left outer join budget.subAccountCode sac
	on progsource.subaccountcode=sac.subaccountcode
		and progsource.mainaccountcode=sac.MainAccountCode
		and progsource.treasuryagencycode=sac.TreasuryAgencyCode

--GWA Agency Name
left outer join agency.GWAagencyName as GWAname
	on coalesce(sac.GWAagencyName,mac.GWAagencyName,tac.GWAagencyName)=GWAname.GWAagencyName


--Input protection for agencyIDs from FAADS
LEFT OUTER JOIN
	FPDSTypeTable.AgencyID AS CAgency_check
		ON C.contractingofficeagencyid = CAgency_check.AgencyID
LEFT OUTER JOIN
	FPDSTypeTable.AgencyID AS FAgency_check
		ON C.fundingrequestingagencyid = FAgency_check.AgencyID 

--Link OMBagencycode and OMBbureaucode
	left outer join agency.OMBagencyCode ombagency
		on ombagency.OMBagencyCode=coalesce(sac.agencycode,mac.agencycode,tac.agencycode) 
	left outer join agency.BureauCode ombbureau
		on ombbureau.OMBagencyCode=coalesce(sac.agencycode,mac.agencycode,tac.agencycode) 
		and ombbureau.bureaucode=coalesce(sac.bureaucode,mac.bureaucode,tac.bureaucode)

		LEFT OUTER JOIN FPDSTypeTable.AgencyID AS TACname
	ON tac.FundingAgencyID = TACname.AgencyID 



































GO


