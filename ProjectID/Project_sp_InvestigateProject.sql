USE [DIIG]
GO

/****** Object:  StoredProcedure [Project].[sp_InvestigateProject]    Script Date: 11/7/2016 1:12:21 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO









-- =============================================
-- Author:		Greg Sanders
-- Create date: 2013-03-13
-- Description:	Assign a parent ID to a dunsnumber for a range of years
-- =============================================
ALTER  PROCEDURE [Project].[sp_InvestigateProject]
	-- Add the parameters for the stored procedure here
	@ProjectName nvarchar(255)

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;


	if @ProjectName is null
		begin 
		--raiserror('The value for @ProjectName should not be null.',15,1)
		SELECT [ProjectID]
      ,[ProjectName]
      ,[ProjectAbbreviation]
      ,[IsJointDevelopmentCaseStudy]
  FROM [DIIG].[Project].[ProjectID]
  end

  else
  begin


  declare @ProjectID int
	set @ProjectID=(SELECT [ProjectID]
	FROM [DIIG].[Project].[ProjectID]
	where projectname =@ProjectName)



if @ProjectID is null
begin
  	SELECT [ProjectID]
      ,[ProjectName]
      ,[ProjectAbbreviation]
      ,[IsJointDevelopmentCaseStudy]
  FROM [DIIG].[Project].[ProjectID]
  where projectname like '%'+@ProjectName+'%'

  raiserror('No match on @ProjectName.',15,1)
  end
else
begin
  Select sec.systemequipmentcode
  ,sec.systemequipmentcodetext
  ,sec.startfiscalyear
  ,sec.endfiscalyear
  ,sec.projectid
  ,sec.csismodifieddate
  ,sec.CSISmodifiedBy
  from  Project.SystemEquipmentCodetoProjectIDhistory sec
inner join (select [ProjectID]
  FROM [DIIG].[Project].[ProjectID]
  where projectname like '%'+@ProjectName+'%') p
  on sec.projectid=p.projectid


select [ContractLabelText]
      ,[ContractLabelID]
      ,[TFBSOoriginated]
      ,[TFBSOmentioned]
      ,[IsPerformanceBasedLogistics]
      ,[IsOrganicPBL]
      ,[IsOfficialPBL]
      ,[PrimaryProjectID]
from  Contract.ContractLabelID clid
where primaryprojectid=@ProjectID


SELECT 'Linked to ProjectID' as TableName
      ,r1.[ProgramElement]
      ,r1.[ProgramElementTitle]
      ,r1.[BudgetActivity]
      ,[BudgetActivityTitle]
      ,r1.AccountDSI
      ,r1.[Classified]
      ,[FiscalYear]
	  ,OriginType
      ,r1.PBtotal
      ,r1.PBtype
	  ,r1.EnactedTotal   
	  ,r1.EnactedType
	  ,r1.SpecialType
	  ,r1.ActualTotal      
  FROM [DIIG].[budget].[DefenseR1consolidated] r1
  --inner join project.ProgramElement pe
  --on r1.ProgramElement=pe.ProgramElement
  inner join Project.RDTEidToProjectIDmanyToMany r
  on r1.RDTEid=r.RDTEid
  where r.ProjectID=@ProjectID
	order by ProgramElement,FiscalYear

	SELECT 'Not linked to ProjectID' as TableName
      ,r1.[ProgramElement]
      ,r1.[ProgramElementTitle]
      ,r1.[BudgetActivity]
      ,[BudgetActivityTitle]
      ,r1.AccountDSI
      ,r1.[Classified]
      ,[FiscalYear]
	  ,OriginType
      ,r1.PBtotal
      ,r1.PBtype
	  ,r1.EnactedTotal   
	  ,r1.EnactedType
	  ,r1.SpecialType
	  ,r1.ActualTotal     
  FROM [DIIG].[budget].[DefenseR1consolidated] r1
  inner join project.ProgramElement pe
  on r1.ProgramElement=pe.ProgramElement
  left outer join Project.RDTEidToProjectIDmanyToMany r
  on r1.RDTEid=r.RDTEid
  where r1.ProgramElementTitle like '%'+@ProjectName+'%' and 
	(projectID is null or projectID<>@ProjectID)
  order by ProgramElement,FiscalYear
  
  SELECT 'Linked to ProjectID' as TableName
      ,p1.[AccountDSI]
      ,p1.[BudgetActivity]
      ,[BudgetActivityTitle]
      ,p1.[BSA]
      ,[BSAtitle]
      ,p1.[LineItem]
      ,[LineItemTitle]
      ,[CostType]
      ,[CostTypeTitle]
      ,[ProcurementCategory]
      ,[Classified]
      ,[FiscalYear]
	  ,OriginType
      ,PBtotal
      ,[PBtotal]
      ,[PBtype]
      ,[EnactedTotal]
      ,[EnactedType]
      ,[SpecialType]
      ,[ActualTotal]
      ,[QuantPBtotal]
      ,[QuantPBtype]
      ,[QuantEnactedTotal]
      ,[QuantEnactedType]
      ,[QuantSpecialType]
      ,[QuantActualTotal]
  FROM [DIIG].[budget].[DefenseP1consolidated] p1
  inner join project.ProcIDtoProjectIDmanyToMany p
  on p.ProcID=p1.ProcID
where p.projectid =@ProjectID 
	
order by p1.TreasuryAgencyCode
	,p1.AccountDSI
	,p1.BudgetActivity
	,p1.BSA
	,p1.lineitem,FiscalYear

	  
 SELECT 'Linked to ProjectID' as TableName
      ,p1.[AccountDSI]
      ,p1.[BudgetActivity]
      ,[BudgetActivityTitle]
      ,p1.[BSA]
      ,[BSAtitle]
      ,p1.[LineItem]
      ,[LineItemTitle]
      ,[CostType]
      ,[CostTypeTitle]
      ,[ProcurementCategory]
      ,[Classified]
      ,[FiscalYear]
	  ,OriginType
      ,PBtotal
      ,[PBtotal]
      ,[PBtype]
      ,[EnactedTotal]
      ,[EnactedType]
      ,[SpecialType]
      ,[ActualTotal]
      ,[QuantPBtotal]
      ,[QuantPBtype]
      ,[QuantEnactedTotal]
      ,[QuantEnactedType]
      ,[QuantSpecialType]
      ,[QuantActualTotal]
  FROM [DIIG].[budget].[DefenseP1consolidated] p1
  inner join project.ProcIDtoProjectIDmanyToMany p
  on p.ProcID=p1.ProcID
  where p1.[LineItemTitle] like '%'+@ProjectName+'%'  and 
	(projectID is null or projectID<>@ProjectID)
order by p1.AccountDSI
	,p1.BudgetActivity
	,p1.BSA
	,p1.lineitem,FiscalYear



	SELECT  [CSIScontractID]
      ,[CSISidvpiidID]
      ,[idvpiid]
      ,[idvagencyid]
      ,[idvmodificationnumber]
      ,[piid]
      ,[ContractNumber]
      ,[transactionnumber]
      ,[modnumber]
      ,[unique_transaction_id]
      ,[transaction_status]
      ,[signeddate]
      ,[effectivedate]
      ,[currentcompletiondate]
      ,[ultimatecompletiondate]
      ,[lastdatetoorder]
      ,[fiscal_year]
      ,[Customer]
      ,[SubCustomer]
      ,[AgencyIDtext]
      ,[contractingofficeid]
      ,[ContractingOfficeName]
      ,[ContractingMajorCommandName]
      ,[Funder]
      ,[SubFunder]
      ,[FundingAgencyIDtext]
      ,[FundingMajorCommandName]
      ,[FundingRequestingOfficeid]
      ,[FundingOfficeName]
      ,[maj_agency_cat]
      ,[mod_agency]
      ,[maj_fund_agency_cat]
      ,[TreasuryAgencyCode]
      ,[TACsubfunder]
      ,[MainAccountCode]
      ,[MainAccountTitle]
      ,[SubAccountCode]
      ,[SubAccountTitle]
      ,[GWAagencyName]
      ,[OMBagencycode]
      ,[OMBbureaucode]
      ,[ProductOrServiceArea]
      ,[PlatformPortfolio]
      ,[ClaimantProgramCodeText]
      ,[ProjectID]
      ,[ProjectName]
      ,[ProjectAbbreviation]
      ,[ProductOrServiceCode]
      ,[PSCText]
      ,[RnD_BudgetActivity]
      ,[ContractRequirement]
      ,[vendorname]
      ,[vendoralternatename]
      ,[vendorlegalorganizationname]
      ,[vendordoingasbusinessname]
      ,[divisionname]
      ,[divisionnumberorofficecode]
      ,[vendorenabled]
      ,[vendorlocationdisableflag]
      ,[ccrexception]
      ,[streetaddress]
      ,[streetaddress2]
      ,[streetaddress3]
      ,[city]
      ,[state]
      ,[zipcode]
      ,[vendorcountrycode]
      ,[vendor_state_code]
      ,[vendor_cd]
      ,[congressionaldistrict]
      ,[vendorsitecode]
      ,[vendoralternatesitecode]
      ,[dunsnumber]
      ,[parentdunsnumber]
      ,[phoneno]
      ,[faxno]
      ,[registrationdate]
      ,[renewaldate]
      ,[mod_parent]
      ,[locationcode]
      ,[statecode]
      ,[pop_state_code]
      ,[placeofperformancecountrycode]
      ,[placeofperformancezipcode]
      ,[obligatedamount]
      ,[baseandexercisedoptionsvalue]
      ,[baseandalloptionsvalue]
      ,[numberofactions]
      ,[contractingofficeagencyid]
      ,[fundingrequestingagencyid]
      ,[fundedbyforeignentity]
      ,[contractactiontype]
      ,[reasonformodification]
      ,[typeofcontractpricing]
      ,[priceevaluationpercentdifference]
      ,[subcontractplan]
      ,[lettercontract]
      ,[multiyearcontract]
      ,[performancebasedservicecontract]
      ,[majorprogramcode]
      ,[contingencyhumanitarianpeacekeepingoperation]
      ,[contractfinancing]
      ,[costorpricingdata]
      ,[costaccountingstandardsclause]
      ,[descriptionofcontractrequirement]
      ,[purchasecardaspaymentmethod]
      ,[nationalinterestactioncode]
      ,[progsourceagency]
      ,[progsourceaccount]
      ,[progsourcesubacct]
      ,[account_title]
      ,[rec_flag]
      ,[typeofidc]
      ,[multipleorsingleawardidc]
      ,[programacronym]
      ,[pop_cd]
      ,[placeofperformancecongressionaldistrict]
      ,[psc_cat]
      ,[systemequipmentcode]
      ,[claimantprogramcode]
      ,[principalnaicscode]
      ,[informationtechnologycommercialitemcategory]
      ,[gfe_gfp]
      ,[useofepadesignatedproducts]
      ,[recoveredmaterialclauses]
      ,[seatransportation]
      ,[contractbundling]
      ,[consolidatedcontract]
      ,[countryoforigin]
      ,[placeofmanufacture]
      ,[manufacturingorganizationtype]
      ,[agencyid]
      ,[solicitationid]
      ,[extentcompeted]
      ,[reasonnotcompeted]
      ,[numberofoffersreceived]
      ,[commercialitemacquisitionprocedures]
      ,[commercialitemtestprogram]
      ,[smallbusinesscompetitivenessdemonstrationprogram]
      ,[a76action]
      ,[competitiveprocedures]
      ,[solicitationprocedures]
      ,[typeofsetaside]
      ,[localareasetaside]
      ,[evaluatedpreference]
      ,[fedbizopps]
      ,[research]
      ,[statutoryexceptiontofairopportunity]
      ,[organizationaltype]
      ,[firm8aflag]
      ,[hubzoneflag]
      ,[sdbflag]
      ,[issbacertifiedsmalldisadvantagedbusiness]
      ,[shelteredworkshopflag]
      ,[hbcuflag]
      ,[educationalinstitutionflag]
      ,[womenownedflag]
      ,[veteranownedflag]
      ,[srdvobflag]
      ,[localgovernmentflag]
      ,[minorityinstitutionflag]
      ,[aiobflag]
      ,[stategovernmentflag]
      ,[federalgovernmentflag]
      ,[minorityownedbusinessflag]
      ,[apaobflag]
      ,[tribalgovernmentflag]
      ,[baobflag]
      ,[naobflag]
      ,[saaobflag]
      ,[nonprofitorganizationflag]
      ,[isothernotforprofitorganization]
      ,[isforprofitorganization]
      ,[isfoundation]
      ,[haobflag]
      ,[ishispanicservicinginstitution]
      ,[emergingsmallbusinessflag]
      ,[hospitalflag]
      ,[contractingofficerbusinesssizedetermination]
      ,[is1862landgrantcollege]
      ,[is1890landgrantcollege]
      ,[is1994landgrantcollege]
      ,[isveterinarycollege]
      ,[isveterinaryhospital]
      ,[isprivateuniversityorcollege]
      ,[isschoolofforestry]
      ,[isstatecontrolledinstitutionofhigherlearning]
      ,[isserviceprovider]
      ,[receivescontracts]
      ,[receivesgrants]
      ,[receivescontractsandgrants]
      ,[isairportauthority]
      ,[iscouncilofgovernments]
      ,[ishousingauthoritiespublicortribal]
      ,[isinterstateentity]
      ,[isplanningcommission]
      ,[isportauthority]
      ,[istransitauthority]
      ,[issubchapterscorporation]
      ,[islimitedliabilitycorporation]
      ,[isforeignownedandlocated]
      ,[isarchitectureandengineering]
      ,[isdotcertifieddisadvantagedbusinessenterprise]
      ,[iscitylocalgovernment]
      ,[iscommunitydevelopedcorporationownedfirm]
      ,[iscommunitydevelopmentcorporation]
      ,[isconstructionfirm]
      ,[ismanufacturerofgoods]
      ,[iscorporateentitynottaxexempt]
      ,[iscountylocalgovernment]
      ,[isdomesticshelter]
      ,[isfederalgovernmentagency]
      ,[isfederallyfundedresearchanddevelopmentcorp]
      ,[isforeigngovernment]
      ,[isindiantribe]
      ,[isintermunicipallocalgovernment]
      ,[isinternationalorganization]
      ,[islaborsurplusareafirm]
      ,[islocalgovernmentowned]
      ,[ismunicipalitylocalgovernment]
      ,[isnativehawaiianownedorganizationorfirm]
      ,[isotherbusinessororganization]
      ,[isotherminorityowned]
      ,[ispartnershiporlimitedliabilitypartnership]
      ,[isschooldistrictlocalgovernment]
      ,[issmallagriculturalcooperative]
      ,[issoleproprietorship]
      ,[istownshiplocalgovernment]
      ,[istriballyownedfirm]
      ,[istribalcollege]
      ,[isalaskannativeownedcorporationorfirm]
      ,[iscorporateentitytaxexempt]
      ,[iswomenownedsmallbusiness]
      ,[isecondisadvwomenownedsmallbusiness]
      ,[isjointventurewomenownedsmallbusiness]
      ,[isjointventureecondisadvwomenownedsmallbusiness]
      ,[walshhealyact]
      ,[servicecontractact]
      ,[davisbaconact]
      ,[clingercohenact]
      ,[otherstatutoryauthority]
      ,[interagencycontractingauthority]
      ,[CSISCreatedDate]
      ,[CSISModifiedDate]
      ,[numberofemployees]
      ,[annualrevenue]
      ,[CSIStransactionID]
      ,[CSISUniqueIndexID]
      ,[CSISUniqueIndexIdentity]
      ,[PlaceofPerformanceCity]
      ,[prime_awardee_executive1]
      ,[prime_awardee_executive2]
      ,[prime_awardee_executive3]
      ,[prime_awardee_executive4]
      ,[prime_awardee_executive5]
      ,[prime_awardee_executive5_compensation]
      ,[prime_awardee_executive2_compensation]
      ,[prime_awardee_executive3_compensation]
      ,[prime_awardee_executive4_compensation]
      ,[prime_awardee_executive1_compensation]
      ,[last_modified_date]
      ,[TypeOfBusiness]
      ,[ContractActionTypeDD350]
      ,[TypeIndefiniteDeliveryContract]
      ,[headquartercode]
      ,[CAGE]
  FROM [DIIG].[Project].[ProjectDetail] p

where projectID=@projectID
	 
		Order by [Customer],[SubCustomer],[AgencyIDtext], signeddate
		
	end
  	--Verify that the parameter is already in the relevant type table.
	--if(select p.systemequipmentcodeText
	--	from Project.systemequipmentcode as p
	--	where p.systemequipmentcodeText=@ProjectName) is null 
	--begin
	--	--If there's a similar value to the missing one, suggest that instead.
	--	if (select top 1 p.systemequipmentcodeText
	--			from Project.systemequipmentcode as p
	--			where p.systemequipmentcodeText like '%'+@ProjectName+'%') is not null 
	--	begin
	--		select  p.systemequipmentcodeText
	--			from Project.systemequipmentcode as p
	--			where p.systemequipmentcodeText like '%'+@ProjectName+'%'
	--		select 'The value for @ProjectName is not found in FPDSTypeTable.systemequipmentcode. Did you mean one of the above?' as ErrorDescription
	--		return -1
	--	end
	--	--If there's no similar value, return an error, they'll have to check the table themselves.
	--	else
	--	begin
	--			select s.systemequipmentcodeText
	--			, s.systemequipmentcode
	--from Project.systemequipmentcode as s
	--order by s.systemequipmentcodeText
	----where [systemequipmentcodeText] =@ProjectName

	--		select 'The value for @ProjectName is not found in FPDSTypeTable.systemequipmentcode. Above is a complete listing' as ErrorDescription
			
	--		--raiserror('The value for @ProjectName is not found in FPDSTypeTable.systemequipmentcode.',15,1)
	--	end
	--end
	----End input protection

	--else
	--begin


	--select *
	--from Project.systemequipmentcode as s
	--where [systemequipmentcodeText] =@ProjectName

	----SELECT [systemequipmentcodeText]
 ----     ,[Customer]
 ----     ,[ServicesCategory]
 ----     ,[SumOfobligatedAmount]
 ----     ,[SumOfnumberOfActions]
 ----     ,[SumOfbaseandexercisedoptionsvalue]
 ----     ,[SumOfbaseandalloptionsvalue]
 ---- FROM [DIIG].[SystemEquipment].[SystemEquipmentHistoryBucketCustomer]
	----where [systemequipmentcodeText] =@ProjectName

 
 --end


END


end



















GO


