library(haven)
library(dplyr)

dat = read_sav("./Data/private_landlord_survey/spss/epls_2018_forarchive.sav")

dat = dat %>% filter(RoleTyp==1) # Filter for LLs

dat = dat %>% # Select cols
  select(serial_2, # Unique Identifier
         LLeth2cat, # LL ethnicity
         Age4cat, # LL Age
         ValperPrpA, # Avg mkt val per prop
         MktValgrpA, # Mkt val all props
         BTL, # Buy to let loan/other
         LnperPrpA, # Avg loan per prop
         LnValgrpA, # Total val all loans
         LTVgrpA, # LTV market val
         TmLLgrpB, # How long been LL
         LLExp, # Length experience as LL
         NumPrpgrp, # How many props
         RtIncgrpa, # Gross rental inc
         LLIncgrpA, # Total LL inc no rent
         TotIncgrp, # Total LL inc
         RtPrIncgrpA, # Rent as prop. inc
         Port_size, # Portfolio size
         LLEmpl01, # Emp. Status: FT
         LLEmpl02, # Emp. Status: PT
         LLEmpl03, # Emp. Status: Self-employed as LL
         LLEmpl04, # Emp. Status: Self-employed
         LLEmpl05, # Emp. Status: Retired
         LLEmpl06, # Emp. Status: Company director
         LLEmpl07, # Emp. Status: Student
         LLEmpl08, # Emp. Status: FT carer
         LLEmpl09, # Emp. Status: Unemployed
         LLOrg1, # LL Org: National LLs Assc.
         LLOrg2, # LL Org: Residential LL Assc.
         LLOrg3, # LL Org: Other professional
         LLOrg4, # LL Org: None
         AgOrg1, # Rental Org: ARLA
         AgOrg2, # Rental Org: NAEA
         AgOrg3, # Rental Org: NALS
         AgOrg4, # Rental Org: UKALA
         AgOrg5, # Rental Org: Other
         AgOrg6, # Rental Org: None
         Regprop01, # Prop Region: East England
         Regprop02, # Prop Region: East Midlands
         Regprop03, # Prop Region: Inner London
         Regprop04, # Prop Region: Outer London
         Regprop05, # Prop Region: North East
         Regprop06, # Prop Region: North West
         Regprop07, # Prop Region: South East
         Regprop08, # Prop Region: South West
         Regprop09, # Prop Region: West Midlands
         Regprop10, # Prop Region: Yorkshire/Humber
         LLInfo01, # Where info: Letting Agent
         LLInfo02, # Where info: LL membership ass. or org
         LLInfo03, # Where info: Online landlord forums/websites
         LLInfo04, # Where info: Gov't websites
         LLInfo05, # Where info: Family/friends
         LLInfo06, # Where info: Online media
         LLInfo07, # Where info: TV
         LLInfo08, # Where info: Radio
         LLInfo09, # Where info: Newspapers
         AgtuseA1, # Agent use: For letting svcs
         AgtuseA2, # Agent use: For mgmt svcs
         AgtuseA3, # Agent use: None
         FutVac, # Next Vacancy will relet
         FutProp, # Next two years, plan
         FutRLv1, # LL leave/reduce: Financial
         FutRLv2, # LL leave/reduce: Personal
         FutRLv3, # LL leave/reduce: Legislative
         FutRLv4, # LL leave/reduce: Other
         FutRLv5, # LL leave/reduce: None
         FutRLv6, # LL leave/reduce: Don't Know
         FutInc1, # LL Increase/maintain: Financial
         FutInc2, # LL Increase/maintain: Personal
         FutInc3, # LL Increase/maintain: Legislative
         FutInc4, # LL Increase/maintain: Other
         FutInc5, # LL Increase/maintain: None
         FutInc6, # LL Increase/maintain: Don't know
         LLPmb1, # LL Serious problems: Financial
         LLPmb2, # LL Serious problems: Legislative
         LLPmb3, # LL Serious problems: Tenant behaviour
         LLPmb4, # LL Serious problems: Other
         LLPmb5, # LL Serious problems: None
         Lntyp1, # Current loan: B2L mort
         Lntyp2, # Current loan: Commercial loan
         Lntyp3, # Current loan: Family/friends
         Lntyp4, # Current loan: No debt
         Lntyp5, # Current loan: Other
         TaxB201, # Awareness: Stamp duty increase
         TaxB202, # Awareness: Reduced tax relief
         TaxB203, # Awareness: Reduced CG tax non-prop
         TaxB204, # Awareness: Change wear/tear allow
         TaxB205, # Awareness: Change minimum rent B2L mort
         TaxB206, # Awareness: Letting agent fee ban
         TaxB207, # Awareness: Tax treatment foreign prop own
         TaxB208, # Awareness: None
         TaxB301, # As above but understanding
         TaxB302,
         TaxB303,
         TaxB304,
         TaxB305,
         TaxB306,
         TaxB307,
         EPCEFG, # Any props. w/ EPC rating E, F or G
         EPCE, # Awareness: EPC requirements
         LetReq03 # Requirement: EPC Certificate
         )


low_ehs = dat %>% filter((EPCEFG==1) | (EPCEFG==3))

dat %>%
  summarise_all()
