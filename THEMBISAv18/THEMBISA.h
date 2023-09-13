// THEMBISA.h
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <string>
// #using <mscorlib.dll>

// using namespace std;


//=============================================================================
// Parameters in the 'Projection' sheet & other settings
//=============================================================================

int StartYear = 1985;
int ProjectionTerm = 116; // Note that entering term of 10 will give you results in 1994
int CurrYear;
int CurrMonth; // 0 to 11

int FixedUncertainty = 1;
const int VaryFutureInterventions = 0; // 0 = fix the future rollout at Rollout.txt values
const int FixedARTinitiation = 0; // 1 = fix the rates of ART initiation at the values generated in
								  // the uncertainty analysis (only valid if FixedUncertainty = 1)
const int InputARTinitiationRates = 0; // 1 = specify rates of ART initiation in Rollout.txt
									   // 0 = specify numbers starting ART in Rollout.txt
const int PropnalImmART = 1; // 1 if immediate ART start is proportional to later rate of ART start
const int ExcludeInterrupters = 1; // 1 = exclude temporary ART interrupters when calculating
								   // numbers currently on ART
const int UseNumbersTests = 1; // 1 = calculate rates of testing from numbers of HIV tests
const int ProvModel = 0; // 1 if modelling a province, 0 for national model
std::string ProvID = "NC"; // Choose from EC, FS, GT, KZ, LM, MP, NC, NW, WC
int PrEPorVM = 0; // 1 if allowing for PrEP or vaginal microbicides. Keep set to 0 as default; it
				  // will automatically get recalculated if there is PrEP/VM rollout.
double RRtestingDiagnosed = 1.0; // Relative rate of consent to testing if individual is diagnosed
								 // positive (relevant in calibration to HSRC data)

const int CalibPaedPrev = 0;
const int CalibAdultPrev = 1; // 1 = calibrate to adult HIV prevalence data from HSRC & DHS surveys
const int CalibANCprev = 1; // 1 = calibrate to HIV prevalence data from antenatal surveys
const int InclANCpre1997 = 1; // 1 = include ANC prevalence data prior to 1997
const int InclAS_ANCprov = 1; // 1 = include age-specific ANC prevalence data in prov calibration
const int CalibFSWprev = 0; // 1 = calibrate to HIV prevalence data from sex worker surveys
const int CalibMSMprev = 0; // 1 = calibrate to HIV prevalence data from MSM surveys
const int CalibCD4 = 0; // 1 = calibrate to CD4 distributions in household/workforce surveys
const int CalibCD4ANC = 0; // 1 = calibrate to CD4 distributions in antenatal surveys
const int CalibHCT_HH = 0; // 1 = calibrate to propn ever tested in HSRC surveys
const int CalibHCT_ANC = 0; // 1 = calibrate to propn ever tested in antenatal surveys
const int CalibHCTprev = 0; // 1 = calibrate to HIV prevalence in adults receiving HCT
const int CalibHCTprevP = 0; // 1 = calibrate to HIV prevalence in children receiving HCT
const int CalibHCTtotP = 0; // 1 = calibrate to recorded number of kids receiving HCT
const int CalibDeathsA = 1; // 1 = calibrate to recorded numbers of adult deaths
const int AgeLimitMortCalib = 60; // Death data below this age are used in mort calibration
								  // Should be multiple of 5 (set to 95 if all ages included)
const int CalibDeathsP = 0; // 1 = calibrate to recorded numbers of paediatric deaths
const int CalibAIDStrend = 0; // 1 = calibrate to reported new AIDS cases in adults (1990-4)
const int CalibAIDSage = 0; // 1 = calibrate to reported new AIDS cases by age and sex (1993-4)
const int CalibARTtotals = 1; // 1 = calibrate to reported total numbers of ART patients
const int CalibARTtotalsP = 0; // 1 = calibrate to reported total numbers of children on ART
const int CalibCD4atARTstart = 0; // 1 = calibrate to recorded # starting ART by CD4 (WC only)
const int CalibARTbyAge = 1; // 1 = calibrate to age distribution of adults on ART
const int CalibARTbyAgeP = 0; // 1 = calibrate to age distribution of kids starting ART
const int CalibARTbyAgeP2 = 1; // 1 = calibrate to age distribution of kids on ART
const int CalibChildPIP = 0; // 1 = calibrate to child deaths in facilities
const int CalibARTcoverage = 1; // 1 = calibrate to ARV metabolite data
const int CalibMarriageData = 0; // 1 = calibrate to marriage data
const int UTTretention = 0; // 1 = toggle HIV Investment Case 95% ART coverage by 2025 scenario
double UTTretval = 0.002; // adjusment to ART retention in HIV Investment Case 95% ART coverage by 2025 scenario

//=============================================================================
// Parameters in the 'Adult assumptions' sheet
//=============================================================================

// Sexual behaviour assumptions

// (a) Partnership formation, sexual debut, mixing
double HighRiskPropn[2]; // % of adults in high risk group, by sex
double PartnerRate20F; // rate of partnership formation in 20-year old unmarried women in high risk
double RRpartnerLow[2]; // relative rate of partnership formation in low risk group
double RRpartnerMarried[2]; // relative rate of partnership formation in married high risk group
double GammaMeanF; // partnership formation age adjustment factor: gamma mean
double GammaSDF; // partnership formation age adjustment factor: gamma standard deviation
double DebutMedian[2]; // Median age at start of sexual activity in high risk group
double DebutLow[2]; // relative rate of starting sexual activity in low risk group
double DebutShape[2]; // Log-logistic shape parameter for rate of starting sexual activity
double MeanAgeDif; // Mean age difference in short-term relationships (years)
double SDageDif; // Standard deviation of age difference in short-term relationships
double MeanAgeDifLT; // Mean age difference in long-term relationships (years)
double SDageDifLT; // Standard deviation of age difference in long-term relationships
double Assortativeness; // Assortativeness of sexual mixing (0 = completely assortative, 1 = random)
double MarriageConstant[2]; // Scale parameter for log-logistic distribution (M, F)
double MarriageTrend[2]; // Effect of birth cohort on log-logistic distribution (M, F)
double MarriageShape[2]; // Shape parameter for log-logistic distribution (M, F)
double MarriageMin[2]; // Minimum age at which marriage can occur (M, F)
double DivorceAdj; // Multiplier applied to empirically-derived rates of union dissolution
double DivorceTrend; // Annual change in rates of union dissolution (multiplier)
double ORremarriage[2]; // Odds of remarriage in recently divorced/widowed, relative to never-married

// (b) Commercial sex
double FSWageMean; // Mean age of FSWs (female sex workers)
double FSWageSD; // Standard deviation of FSW ages
double FSWageAlpha; // Alpha parameter for gamma distribution of FSW ages
double FSWageLambda; // Lambda parameter for gamma distribution of FSW ages
double ClientsPA; // Number of sex acts with clients per annum
double DurFSW; // Average duration of sex work (years)
double FSWcontactAge21; // Annual # contacts with FSW for unmarried high risk male aged 21
double FSWcontactMarried; // Relative rate of visiting FSWs in married men
double FSWcontactMean; // FSW contact age adjustment factor: gamma mean
double FSWcontactSD; // FSW contact age adjustment factor: gamma standard deviation
double FSWcontactAlpha; // Alpha parameter for gamma distribution of FSW contact rates
double FSWcontactLambda; // Lambda parameter for gamma distribution of FSW contact rates

// (c) Coital frequencies and condom use
double SexActsST; // Average # sex acts per short-term relationship
double SexActsLT; // Average monthly frequency of sex for married women aged 20
double RelFreqSexAge; // Relative freq of sex in marital relationships, per 20-yr increase in age
double AgeEffectCondom; // Redn in log odds of condom use per yr of increase in age
double RRcondomMarital; // OR of condom use in LT relationships relative to ST relationships
double ORcondomFSW; // OR of condom use in FSW-client contacts relative to ST relationships
double InitCondom; // Initial condom use in 1985 (women aged 20 in ST relationships)
double CondomBias; // Condom reporting bias parameter (0 = no bias, 1 = maximum bias)
double CondomAdjProv; // Constant multiplier for specific province (1 for national)
double RRcondomContracep; // RR of reporting of condom use for contraceptive purposes
double IncrCondomBCC; // Increase in condom use due to behaviour change communication (BCC) programmes
double RiskCompensation; // Change in condom use due to risk compensation in recent years
double MedianCondomBCC; // Median time to increase in condom use following BCC
double ShapeCondomBCC; // Shape parameter for time to increase in condom use following BCC

// (d) Effect of HIV and knowledge of HIV status
double VCTcondom; // Reduction in propn of sex acts that are unprotected after HIV diagnosis
double VCT_FSWentry; // Reduction in entry into sex work after HIV diagnosis
double ARTcondom; // Reduction in propn of sex acts that are unprotected after ART initiation
double HIVeffectSex[4]; // Reduction in freq of sex by HIV disease stage
double HIVeffectFSWentry[4]; // Reduction in rate of entry into commercial sex, by HIV disease stage
double HIVeffectFSWexit[4]; // Increase in rate of exit from commercial sex, by HIV disease stage

// HIV transmission assumptions

double InitFSWprev; // Initial HIV prevalence in FSWs and high risk group, ages 15-49
double TransmST[2]; // Transm prob per act of unprotected sex with infected ST partner, by sex
					// of susceptible partner (0 ==> F-to-M transmission prob)
double TransmLT[2]; // Transm prob per act of unprotected sex with infected LT partner
double TransmFSW[2]; // Transm prob per act of unprotected sex between FSW and client
double RRclientToFSW1985; // RR client-to-FSW transmission in 1985 compared to 1995
double EctopyEffect[2]; // Increase in transm prob, per yr of age below 25 (M and F)
double RelInfecRiskST[2][2][2]; // Relative infectiousness by risk of M (1st index), risk of F
								// (2nd index) and sex of susceptible partner (3rd index), in ST rels
double RelInfecRiskLT[2][2][2]; // Relative infectiousness in LT relationships
double RelInfecStage[5]; // Relative infectiousness by HIV stage
double MedianVLuntreated200; // Median VL (log scale) prior to ART initiation at CD4 <200
double ShapeVL; // Weibull shape parameter for distribution of viral loads (log scale) /
				// modifier for effect of VL of HIV infectiousness
double VLeffectInfectivity; // Increase in HIV infectiousness per unit increase in log of VL (at 4 log)
double VLdifPer100CD4; // Decrease in VL per 100-unit increase in baseline CD4 count
double ORsuppressionIeDEA; // Ratio of odds of viral suppression, to that in IeDEA-SA cohorts
double ORsuppressionCD4[4]; // Ratio of odds of VLS in baseline CD4 category s, to that in <200 category
double ARTinfectivity[4]; // Infectiousness after ART initiation, relative to pre-ART, by CD4
double CondomEfficacy; // Condom efficacy in preventing transmission, per act of sex
double DiscordantPropn; // % of individuals married to HIV+ partners, who are HIV-negative
double InfToVirulenceRatio; // Ratio of increase in infectivity to increase in virulence, per unit
							// increase in log VL, on ln scale

// HIV survival assumptions

double CD4decline[3]; // Rate of transition to next CD4 stage
double CD4duration[4]; // Average time to next CD4 stage
double RRmort200to350; // Ratio of untreated mort at CD4 200-349 to that at CD4 <200
double CD4mort[2]; // HIV mortality rate in last 2 CD4 stages
double OIincidence[6]; // Annual OI incidence rate by CD4 stage
double WHO4incidence[5]; // Annual incidence of WHO stage IV-defining conditions
double PTBincidence[5]; // Annual incidence of pulmonary TB by CD4 stage
double RR_ARTinitiation[5]; // Rate of ART initiation (relative to CD4 <200) if eligible
double Haemodilution[4]; // Propn of pregnant women with CD4 reduced to next stage due to haemodilution
double RRper10yr; // Increase in rate of CD4 decline and HIV mortality per 10 yr increase in age
double RRperCalYr; // Increase in rate of CD4 decline per calendar year
double RRuntreatedMortF; // Rate of CD4 decline & mort in females relative to that in males
double MinMort[3]; // Minimum mort (as ART access -> infinity) as fraction of mort when ART access -> 0
				   // 1st elt is for untreated mort, 2nd for 1st yr of ART, 3rd for >1 yr on ART
double RednLogMort[3]; // Redn in log of mort per unit increase in rate of ART initiation
double IeDEAbias[2]; // Bias in IeDEA mortality rates (1 = no bias)
double ARTinterruptionRate; // Annual rate of ART interruption
double RRinterruptionM; // Relative rate of ART interruption in males
double ARTresumptionRate[2]; // Annual rate of resuming ART after interruption (M, F)
double ARTresumptionMin[2]; // Minimum rate of resuming ART after interruption (M, F)
double RR_ARTstartM; // Relative rate of ART initiation in men (compared to women)
double RR_ARTstart100CD4; // Relative rate of ART initiation per 100 cell increase in CD4 count
double RR_ARTstart1stMo; // RR of ART initiation in 1st month after diagnosis (vs longer durations)
double COVIDimpactARTstart; // % reduction in ART initiation due to COVID, April-June 2020
double UltARTdelay[2]; // Ultimate delay to starting ART in adults if CD4 <200 (M then F)
double MinARTdelay[2]; // Minimum average delay to starting ART in adults if CD4 <200 (M then F)
int ARTdataYr; // Year to which ART initiation numbers are specified
int UltARTyr[2]; // Year in which ultimate ART initiation rates first apply
double RRper10yrART[2]; // Increase in rate of HIV mortality on ART, per 10 yr increase in age
double AnnHIVmortART[5][4][2]; // Annual HIV mortality rate, by ART duration (1st index), CD4 stage at
							   // ART initiation (2nd index) and sex (3rd index)
double SwitchAdult[4]; // Annual rate of switching by baseline CD4

// HIV testing assumptions

double VCTmale2002; // Rate of testing in asymptomatic males in 2002, relative to non-pregnant females
double VCTmale2010; // Rate of testing in asymptomatic males in 2010, relative to non-pregnant females
double VCTage[2]; // Age at which VCT uptake peaks, in men and women
double QuadParam[2]; // Coefficient for age^2 term (absolute)
double RetestAdj; // Factor by which testing rate increases in previously tested individuals
double RetestAdjInit;
double RetestAdjMax;
double RetestPos; // Factor by which testing rate reduces in HIV-diagnosed individuals
double RetestPosInit;
double RetestART; // Factor by which testing rate reduces in ART patients
double RetestARTinit;
double HIVeffectVCT; // Factor by which testing rates reduces in HIV-positive individuals
double OItoTBtestingRatio; // Ratio of HIV testing in all OI patients to that in TB patients
double ORdiagOItreat; // OR for HIV diagnosis in OI patients at OI treatment service compared to
					  // all HIV diagnosis as a result of the OI episode
double RapidDiagSp; // Specificity of rapid testing algorithm
double FreqRegHCT[2]; // Frequency of HIV testing (per annum) if receiving regular HCT
double RegHCTdur[2]; // Average duration of retention in regular testing programme (years)
double SelfTestConfirm; // Fraction of positive self-testers who seek confirmatory testing at clinic
double SelfTestDataYr[6]; // Year to which annual # self-tests are available, by distribution strategy
double SelfTestUptakeUlt[6]; // Annual rate of self-test uptake in future, by distribution strategy
double SelfTestWastage[6]; // Fraction of self-test kits that aren't used, by distribution strategy
double STageTaxi[2]; // Alpha and lambda parameters for age effects on self-test uptake through taxi ranks
double STageWork[2][2]; // Alpha and lambda parameters for age effects on ST uptake through work, by sex
double STtaxiMtoFratio; // M-to-F ratio of self-test uptake through taxi ranks
double STworkMtoFratio[2]; // M-to-F ratio of ST uptake through workplace campaigns (primary & 2ndary)
double EmployedPropn[10][2]; // Proportion employed, by age (15-19, 20-24, ..., 60-64) and sex
double RetestPosST[2]; // RR retesting in previously diagnosed ART-naive and ART-experienced
double ORpartnerPosMetareg[2]; // Constant and log(prevalence) coefficients in meta-regression of
// log OR for HIV if partner is positive compared to negative
double DiscloseAndTest; // Prob that newly diagnosed individual discloses to partner and partner tests

// Male circumcision assumptions

double CircPrevBirth; // Propn of males circumcised soon after birth
double CircPrevUlt; // Propn of men who ever get circumcised (pre-MMC promotion)
double MedianCirc; // Median age at circumcisions (including infants)
double ShapeCirc; // Weibull shape parameter for times to MC post-infancy
double MCefficacy; // Redn in susceptibility in men who are circumcised
int MMCdataYear; // Year to which numbers of MMCs are specified
double UltMMCprob; // Ultimate annual prob of MMC in men who are highly sexually active
int UltMMCyear; // Year in which ultimate MMC prob first applies

// PrEP and vaginal microbicide (VM) assumptions

double PrEPefficacy[2]; // PrEP efficacy in heterosexual M and F
double PrEPefficacyMSM; // PrEP efficacy in MSM
double CondomRednPrEP[2]; // Reduction in condom use if using PrEP
double FreqHCTinPrEP[2]; // Frequency of HIV testing (per annum) if receiving PrEP
double PrEPdur[2]; // Average duration of retention in PrEP programme (years)
double PrEPdataYr; // Last year for which we have data on number initiating PrEP
double UltPrEPrateFSW; // Monthly rate at which FSWs start PrEP after PrEPdataYr
double CurrPrEPrateFSW; // Current monthly rate at which FSWs start PrEP
double StoredPrEPrateFSW; // monthly rate at which FSWs start PrEP in PrEPdataYr
//double RR_PrEPlow[2]; // Relative rate of PrEP initiation in low risk heterosexuals
double RR_PrEPlowPreg; // Relative rate of PrEP initiation in low risk pregnant women
double MicrobicideEff; // Microbicide efficacy
double CondomRednVM; // Reduction in condom use if using microbicide
double FreqHCTinVM; // Frequency of HIV testing (per annum) if receiving microbicide
double VMdur; // Average duration of retention in microbicide programme (years)

// Demographic assumptions

double RecencyBiasANC; // Effect of recency on bias in ANC HIV prevalence data
double RRfertHIV; // RR of fertility in newly-infected HIV+ women (undiagnosed)
double RRfertCD4[5]; // RR of fertility in HIV+ women by current CD4
double RRfertDiag; // RR of fertility in diagnosed untreated women (cf undiagnosed)
double RRfertART; // RR of fertility in treated women (cf diagnosed ART-naive)
double SexRatio; // Propn of births that are male

// MSM assumptions

double MSMpropn; // Fraction of men who start sexual activity as MSM
double MSMpartnersM20; // Fraction of MSM partners who are male at age 20
double RednMSMpartnersM; // Factor by which % of MSM partners who are male
						 // reduces per year of age
double MeanAgeDifMSM25; // Average partner age dif in MSM at age 25
double ChangeAgeDifMSM; // Change in partner age dif in MSM per year of age
double SDageDifMSM; // Standard deviation of partner age differences in MSM
double MtoM_ST; // Ave transmission prob per sex act (male-to-male)
double InitMSMprevRatio; // Ratio of init HIV prev in MSM to that in hetero men

//=============================================================================
// Parameters in the 'Paed assumptions' sheet
//=============================================================================

double TransmUntreated; // prob of transm at/before birth if mother is untreated
double CD4transm[4]; // prob of transm at/before birth by CD4 category
double TransmAcute; // transm prob at/before birth if untreated and acutely infected
double TransmBFfirst3; // transm prob per month of breastfeeding for 1st 3 months
double TransmBFafter3; // transm prob per month of breastfeeding after 1st 3 months
double RRforEBF; // relative risk of transm if practising EBF instead of mixed feeding
double TransmBFacute; // transm prob per month of breastfeeding if acutely infected
double FirstANCwk; // mean gestation (in weeks) at first ANC screening visit
double DeliveryWk; // mean gestation (in weeks) at delivery

double RRprogressionPostnatal; // RR of progression to ART eligibility if infected postnatally
double ProgToNeedLT; // annual rate of progression to need in older children infected at birth
double ExcessProgToNeed; // excess rate of progression to need in newborn
double ExcessProgRedn; // factor by which excess rate of progression reduces per year of age
double ProgAdjNoPMTCT; // factor by which the excess progression is adjusted if no PMTCT received
double AIDSmortLT; // annual AIDS mortality rate in older children who are ART-eligible & untreated
double ExcessAIDSmort; //excess rate of AIDS mortality in untreated ART-eligible newborn
double ExcessMortRedn; // factor by which excess rate of AIDS mortality reduces per year of age
double ProvAdjPaedMort; // RR mortality in province relative to national

// Paed ART
double RRmortART1; // relative rate of mortality on ART during the initial high risk phase
double RRmortART2; // relative rate of mortality on ART during the subsequent low risk phase
double EarlyARTmort; // AIDS mortality at age 0 in children starting ART in early disease
double RRearlyARTmort; // RR AIDS mortality per year of age, for children starting ART in early disease
double ARTinterruptionPaed; // rate of ART discontinuation in children
double ARTresumptionPaed; // rate of ART resumption in children, after an interruption
double Discontinuation1; // rate of ART discontinuation during the initial high risk phase (REDUNDANT)
double Discontinuation2; // rate of ART discontinuation during the subsequent low risk phase (REDUNDANT)
double RRearlyPaedART; // relative rate of ART initiation in diagnosed early HIV cf diagnosed late HIV
double UltARTdelayC; // Ultimate delay to starting ART in kids if 2006 WHO eligibility criteria are met
double MinARTdelayC; // Minimum average delay to starting ART in kids if 2006 WHO criteria are met
int InterpolationStart; // Year in which interpolation of ART initiation rate begins
int UltimateYr; // Year in which ultimate ART coverage is reached
double MinMortP[3]; // Minimum mort (as ART access -> infinity) as fraction of mort when ART access -> 0
					// 1st elt is for untreated mort, 2nd for 1st 3 mo of ART, 3rd for >3 mo on ART
double RednLogMortP[3]; // Redn in log of mort per unit increase in rate of ART initiation
double SwitchPaed[2]; // Annual rate of switch to 2nd line in kids starting ART in early/late disease

double IMRshape; // shape parameter for non-AIDS mortality in 1st year of life

double Sensitivity; // sensitivity of HIV screening algorithm
double Window; // window period (weeks) for rapid test
double NVPefficacy; // redn in transm prob at/before birth if mother receives sd NVP
double DualEfficacy; // redn in transm prob at/before birth if mother receives sd NVP + AZT from 28 wks
double AZTefficacy; // redn in transm prob at/before birth if mother receives AZT from 28 wks
double TransmART200; // transm prob at/before birth if mother starts ART with CD4 <200
double TransmARTpre200; // transm prob at/before birth if mother starts ART with CD4 >=200
double InitTransmART200; // baseline value (pre-2010 guidelines)
double InitTransmARTpre200; // baseline value (pre-2010 guidelines)
double TransmARTprePreg; // transm prob at/before birth if mother started ART prior to pregnancy
double RRtransmPerWeekART; // RR of transm at/before birth per week of ART prior to delivery
double MeanARTdurPreg; // Mean duration of ART prior to delivery if LT ART is started during pregnancy
double SD_ARTdurPreg; // SD of duration of ART prior to delivery if LT ART is started during pregnancy
double NVPuptake; // prob that a woman testing positive receives sd NVP
double AZTpropnAdj; // prob that a woman testing positive but not getting sd NVP receives AZT
double RescreenWk; // mean gestation (weeks) at time of retesting for HIV
double RescreenUptake; // % of women testing negative on initial screen who agree to rescreening
double UntestedRescreen; // % of women who missed initial screen who agree to screening
double RednExtNVP; // % redn in rate of MTCT after birth if child receives extended NVP
double RednHAARTduringPreg; // % redn in rate of postnatal MTCT if mother started ART during pregnancy
double RednHAARTbeforePreg; // % redn in rate of postnatal MTCT if mother started ART before pregnancy
double RednHAART; // Average redn in rate of MTCT after birth if mother receives HAART

// Paed HIV testing
double RRtestVirginTrend[2]; // RR virgin testing up to 2005 and 2010+
double RRtestVirgin; // Relative rate of testing in virgins to that in sexually experienced girls aged 15
double RRtestPaedAdvanced; // Relative rate of testing in advanced HIV disease to that in early disease
double RRtestAgePaed; // Relative rate of testing at ages 19-59 mo (compared to ages 5-14 years)
double RetestPosP; // Relative rate of testing in children previously diagnosed positive
double BirthSe[2]; // sensitivity of PCR at birth in detecting HIV in non-PMTCT and PMTCT-exposed
double SixWeekSe[3]; // sensitivity of PCR at 6 weeks in detecting HIV in non-PMTCT and PMTCT-exposed
					 // early disease and in late disease
double BaseARTuptakeEID; // % of kids starting ART soon after early infant diagnosis, before correction
double PaedARTuptakeEID; // % of kids starting ART soon after early infant diagnosis
double ImmARTcorrectionP; // Adjustment to % of kids starting ART immediately after diagnosis (excl EID)
double VirginTestAdjProv; // RR testing in virgins in province, relative to national

// Breastfeeding durations
double EverFeed[3]; // propn of mothers who ever adopt the feeding strategy (0 = mixed feeding
					// by HIV-neg mothers, 1 = mixed feeding by mothers who know they're HIV+,
					// 2 = EBF by mothers who know they're EBF, under original PMTCT programme
double EverFeedCurr[3]; // updated to reflect change in policy on free formula provision in 2011
double MedianFeed[3]; // median duration of feeding (in months)
double ShapeFeed[3]; // shape parameter for duration of feeding
double SwitchingToFF; // propn of mothers who switch to formula if they discover they're HIV+
double AbruptWeaningFirst3; // propn of women who practise abrupt weaning if stopping EBF in
							// 1st 3 months of feeding
double AbruptWeaningAfter3; // propn of women who practise abrupt weaning if stopping EBF
							// after 1st 3 months of feeding
double InitAbruptWeaning1; // baseline parameter for first 3 months
double InitAbruptWeaning2; // baseline parameter after first 3 months

double NoBFmortAdj[24]; // Factors by which mort rates increase in non-breastfed kids in 1st 12m
double BFbiasAdj; // exponential adjustment to NoBFmortAdj to allow for bias
double BFadjProv; // Increase in mean duration of BF in undiagnosed HIV+ women in relevant province

//============================================================================
// Parameters and arrays in the 'Rollout' sheet
//============================================================================

// VCT assumptions
double HCT1stTimeF25[116]; // rate of 1st-time HIV testing in women aged 25
double HCT1stTimeF25init[116];
double NumbersTested[116]; // Numbers of individuals 15+ tested for HIV
double NumbersTested5to14[116]; // Numbers of individuals 5-14 tested for HIV
double OIsDiagnosed[116]; // propn of HIV-positive OI patients who are diagnosed
double OIsTested[116]; // propn of OIs that lead to HIV diagnosis
double RegHCT_FSW[116]; // rate of initiation into regular HCT in FSWs
double RegHCT_15[116]; // rate of initiation into regular HCT in 15-19 yr olds
double RegHCT_20[116]; // rate of initiation into regular HCT in 20-24 yr olds
double RegHCT_25[116]; // rate of initiation into regular HCT in 25-49 yr olds
double RegHCT_50[116]; // rate of initiation into regular HCT in 50+ yr olds
double RegHCTpregnant[116]; // rate of initiation into regular HCT in pregnant women
double PCR6week[116]; // propn of HIV+ mothers who get infant PCR tested at 6 weeks
					 // AND receive the test results
double PCRuptake; // propn of perinatally-infected infants diagnosed at 6 weeks, in current year
double PCRbirth[116]; // propn of HIV+ mothers who get infant PCR tested at birth
double PCRuptakeB; // propn of HIV+ mothers who get infant PCR tested at birth, in current year
double TestingAt18mo[116]; // propn of kids who get tested for HIV at 18 months
double CurrTesting18mo[2]; // % screened at 18 months, in HIV-negative and HIV-positive children
double HBCTuptake[116]; // rate of HCT through home-based testing
double SelfTestTotals[116][6]; // Numbers of self-test kits distributed, by distribution strategy
double SelfTestUptake[6]; // Proportion using self-test kits, by distribution strategy

// PMTCT assumptions
double PregnantWomenTested[116]; // propn of pregnant women who receive HIV testing, by year
double VCTuptake; // propn of pregnant women who receive HIV testing in current year
double AZTrollout[116]; // propn of women receiving sd NVP who also get AZT, by year
double AZTpropn; // propn of women receiving sd NVP who also get AZT in current year
double RescreenPropnLate[116]; // propn of clinics that offer rescreening in late pregnancy, by year
double RescreenLate; // propn of clinics that offer rescreening in late pregnancy in current year
double RescreenPropnImm[116]; // propn of women who get rescreened at 6-wk immunization, by yr
double RescreenImm; // propn of women who get rescreened at immunization in current year
double ExtNVProllout[116]; // propn of women knowing they are HIV+ whose infants receive extended NVP
double ExtNVPpropn; // propn of women whose infants receive extended NVP, in current yr
double EligibleOptionB[116]; // propn of pregnant women eligible for lifelong ART under WHO option B
double OptionB; // propn of pregnant women eligible for lifelong ART under option B, in current yr
double NoBFpropn[116]; // propn of HIV-diagnosed mothers choosing not to formula feed
double FFpropn; // propn of HIV-diagnosed mothers choosing not to breastfeed in current year
double IncreaseARTdurPreg[116]; // % increase in mean duration of ART prior to delivery if ART is
							   // is started during pregnancy (relative to pre-2010 period)

// ART assumptions
double NumStartingART_M[116]; // number of males (15+) starting ART, by year
double StartingART_M; // number of males (15+) starting ART in current year
double NumStartingART_F[116]; // number of females (15+) starting ART, by year
double StartingART_F; // number of females (15+) starting ART in current year
double NumStartingART_P[116]; // number of children (<15) starting ART, by year
double StartingART_P; // number of children starting ART in current year
double RateARTstartF[116]; // monthly rates of ART start in diagnosed ART-eligible women with CD4 <200
double RateARTstartC[116]; // monthly rates of ART start in diagnosed ART-eligible kids with late HIV
double RR_ARTinterruption[116]; // Relative rates of ART interruption, by year
double EligiblePTB350[116]; // propn of pulmonary TB patients, CD4 200-349, who are ART-eligible
double EligiblePTBpre350[116]; // propn of pulmonary TB patients, CD4 350+, who are ART-eligible
double EligibleWHO3[116]; // propn of WHO stage 3 patients (excl. TB), CD4 350+, who are ART-eligible
double EligiblePreg350[116]; // propn of pregnant women, CD4 200-349, who are ART-eligible
double EligiblePregPre350[116]; // propn of pregnant women, CD4 350+, who are ART-eligible
double EligibleAsym350[116]; // propn of asymptomatic non-pregnant people, CD4 200-349, who are eligible
double EligibleAsym500[116]; // propn of asymptomatic non-pregnant people, CD4 350-499, who are eligible
double EligibleAsymPre500[116]; // propn of asymptomatic non-pregnant people, CD4 500+, who are eligible
double EligibleInfants[116]; // propn of infected infants eligible to start ART in early disease
double EligInfants; // propn of infected infants eligible to start ART in early disease in current year
double EarlyART1to4[116]; // % eligible in early disease in kids aged 1-4
double EarlyART5to14[116]; // % eligible in early disease in kids aged 5-14
double MatARTuptake[116]; // propn of newly-diagnosed ART-eligible women starting ART prior to delivery
double MatARTpropn; // propn of ART-eligible women starting ART prior to delivery in current yr
double OI_ARTuptake[116]; // propn of newly-diagnosed ART-eligible OI patients who start ART
double OIstart;
double HCT_ARTuptake[116]; // propn of other newly-diagnosed ART-eligible adults who start ART
double AsymStart[2];
double PaedARTuptake[116]; // propn of newly-diagnosed ART-eligible children who start ART
double PaedARTuptakeC; // propn of newly-diagnosed ART-eligible children who start ART in current yr
double POC_CD4[116]; // propn of HIV-positive patients in whom point-of-care CD4 tests are used
double VLsuppression200[116]; // % of adults starting ART with CD4 <200 who are virally suppressed (VL<400)
double CurrSuppression200; // VLsuppression200 in current year, after adjusting for ORsuppressionIeDEA
double VLsuppressionPaed[116]; // % of children on ART who are virally suppressed (VL<400)
//int ImmARTstage = 2; // Stage after which normal ART initiation rates apply (0 ==> no early ART for CSW)
double ImmART_CSW;

// PrEP assumptions
/*double PrEP_FSW[116]; // rate of PrEP initiation in FSWs
double PrEP_MSM[116]; // rate of PrEP initiation in MSM
double PrEP_15[116][2]; // rate of PrEP initiation in 15-19 yr olds (M, F)
double PrEP_20[116][2]; // rate of PrEP initiation in 20-24 yr olds (M, F)
double PrEP_25[116][2]; // rate of PrEP initiation in 25-49 yr olds (M, F)
double PrEP_50[116][2]; // rate of PrEP initiation in 50+ yr olds (M, F)*/
double TotStartingPrEP[116]; // Annual PrEP initiations
double RR_PrEPstartMSM[116]; // RR of PrEP initiation in HR MSM age 20 (relative to FSW)
double RR_PrEPstartF20[116]; // RR of PrEP initiation in HR fem age 20 (relative to FSW)
double PrEPeligMSM[116]; // Proportion of MSM eligible to initiate PrEP
double PrEPeligAGYW[116]; // Proportion of AGYW (fem 15-24) eligible to initiate PrEP
double PrEPeligOther[116]; // Proportion of other groups eligible to initiate PrEP
double PrEPpregnant[116]; // prob of PrEP initiation in pregnant women

// Vaginal microbicide (VM) assumptions
double VM_FSW[116]; // rate of VM initiation in FSWs
double VM_15[116]; // rate of VM initiation in 15-19 yr olds
double VM_20[116]; // rate of VM initiation in 20-24 yr olds
double VM_25[116]; // rate of VM initiation in 25-49 yr olds
double VM_50[116]; // rate of VM initiation in 50+ yr olds
double VMpregnant[116]; // rate of VM initiation in pregnant women

// MMC assumptions
double MMCoperations[116]; // number of medical circumcisions performed
double RR_MMCpromo10[116]; // RR of MMC promotion at ages 10-14
double RR_MMCpromo15[116]; // RR of MMC promotion at ages 15-19
double RR_MMCpromo20[116]; // RR of MMC promotion at ages 20-24
double RR_MMCpromo25[116]; // RR of MMC promotion at ages 25-49
double RR_MMCpromo50[116]; // RR of MMC promotion at ages 50+
double NeonatalMMC[116]; // % of children circumcised at birth
double CurrCircPrev10; // prevalence of circumcision in 10-year olds at end of yr

// Condom usage reduction assumptions
double CondomFSWreduction[116]; // Proportion reduction of FSW condom usage probability
double FSWreduction;
double CondomSTreduction[116]; // Proportion reduction of ST condom usage probability
double STreduction;
double CondomLTreduction[116]; // Proportion reduction of LT condom usage probability
double LTreduction;
//============================================================================
// Parameters and arrays in the 'Results' sheet
//============================================================================

double AdultMortBy5yr[116][16][2]; // Starting from age 15-19, up to 90+
double NonAIDSmortBy5yr[116][16][2]; // Starting from age 15-19, up to 90+
double AdultHHprev[116][10][2]; // Prevalence in households by year, age and sex
double ChildMortBy5yr[116][4][2]; // For ages 0, 1-4, 5-9, 10-14
double ChildAIDSmortDiag[116][2]; // For ages 1-4, 5-9
double ChildAIDSmortART[116][2]; // For ages 1-4, 5-9
double PrevPregnant[7][116]; // Rows 100-6 (last element is prevalence for all ages combined)
double RateARTstartU200[116][2]; // Rate of starting ART at CD4 <200 (rows 122-3)
double RateARTstartLate[116]; // Rate of starting ART in children (<10) in late HIV disease
int ARTerrorInd; // 1 if ART initiation rate ever exceeds maximum in adults/kids, 0 otherwise
double MaxARTerror;

// Results not required for calibration purposes
//double AdolHIVprofile[56][4];
//double FSW_HIVprofile[56][15]; // 3 neg stages, 4 undiag pos, 4 diag pre-ART, 4 ART
double NewHIVinClients; // Used in calculating % of transmission due to commercial sex
double HIVprev15to49[116]; // Row 89
double FSWprev[116]; // Row 107
double CondomUseFSW[116]; // % of sex workers using condoms at last sex
double TotalSex;
double TotalSexProt;
double TotalSexProt18;
double SummaryOutputs[2000][116]; // Means and 95% CIs
int SummOutRow = 0; // Row ID for SummaryOutputs

//============================================================================
// Parameters and arrays in the 'Population' sheet
//============================================================================

double StartingPop[91][2]; // Size of population in 1985, by age and by sex
double TotalPop[91][2]; // Size of the current population, by age and sex (cols E-F)
double TotalPositive[91][2]; // Size of HIV-positive population (cols G-H)
double TotalDiagnosed[91][2]; // Number of people diagnosed HIV-positive
double TotalART[91][2]; // Number of ART patients (cols I-J)
double TotalART2ndL[91][2]; // Number of ART patients on second-line
double TotalNaiveElig[91][2]; // Number who are ART-naive but eligible for ART
double TotalInterrupt[91][2]; // Number who have received ART but are currently off ART
double TotalSexuallyExp[91][2]; // Number of individuals who are sexually experienced
double TotalMarried[91][2]; // Number of individuals who are married (cols O-P)
double SumGroupsM[81][44]; // Sum of adult male risk groups (cols R-BI)
double SumGroupsF[81][44]; // Sum of adult female risk groups (cols R-BI)
double SumGroupsVM[20][44]; // Sum of virgin male risk groups (cols BK-DB)
double SumGroupsVF[20][44]; // Sum of virgin female risk groups (cols BK-DB)
double InitPrevAdj[35][2]; // Adjustments to initial prevalence in women aged 15-49

//============================================================================
// Parameters and arrays in the 'Sex activity' sheet
//============================================================================

double MarriageRate[76][2]; // Average rates of marriage (1st row is for age 15)
double DivorceRate[76][2]; // Rates of divorce in M (1st index) and F (2nd index)
double InitBehavDbn[81][6][2]; // Initial sex activity distribution by age (1st index),
							   // risk/behav (2nd index) and sex (3rd index)
double CurrBehavDbn[81][4][2]; // Current sex activity distribution (omitting virgins)
double CurrBehavDbnMSM[81][2]; // Current MSM sex activity distribution by risk group
double PartnerAcqF[81]; // Age adjustment factor for female partnership formation rate
double MinPartnerAge[81]; // Minimum partner age, by female age
double GammaParametersST[81][2]; // Alpha and beta parameters for ST partner age prefs
double AgePrefST[81][81][2]; // Proportion of ST partners in different age groups (3rd
							 // index is sex of selecting indiv, whose age is 1st index)
double GammaParametersLT[81][2]; // Alpha and beta parameters for LT partner age prefs
double AgePrefLT[81][81][2]; // Proportion of LT partners in different age groups
double MSMpartnersM[81]; // Fraction of MSM partners who are male
double InitWidowhoodRate[76][2]; // Initial rates at which married individuals become
								 // widowed, indexed by sex of the partner
double CurrWidowhoodRate[76][2]; // Current rates at which married individuals become
								 // widowed, due to non-AIDS mort
double FSWageDbn[81]; // Propn of sex workers at each age
double PartnerAcqM[81]; // Rate at which high risk unmarried men form ST partnerships
double FSWcontactRate[81]; // Rate at which high risk unmarried men visit sex workers
double TotalFSW; // Total demand for sex workers in current period
double DebutProb[21][2][2]; // Prob of debut by age, risk group and sex (last index)
double AnnAIDSmortM[76][2][2]; // By risk of male (2nd index) & risk of female (3rd index)
double AnnAIDSmortF[76][2][2]; // By risk of female (2nd index) & risk of male (3rd index)
double ProbDivorceOrWidowM[76][2][2]; // Prob male of age x (1st index) in risk group y
									  // (2nd index) loses spouse of risk z (3rd index)
double ProbDivorceOrWidowF[76][2][2]; // Prob woman of age x (1st index) in risk group y
									  // (2nd index) loses spouse of risk z (3rd index)
double GammaParametersMSM[81][2]; // Alpha and beta parameters for MSM partner age prefs
double AgePrefMSM[81][81]; // Proportion of MSM partners in different age groups

//============================================================================
// Parameters and arrays in the 'Mixing' sheet
//============================================================================

double ProbMarriageSE[76][2][2]; // Prob of marriage in sexually experienced adults, by
								 // age, risk group and sex (last index)
double InitMarriedHigh[2][2]; // Initial proportion of married individuals whose partners
							  // are high risk (1st index = sex, 2nd index = risk group)
double CurrSThigh[2][2]; // Current proportion of individuals in ST rels whose partners
						 // are high risk (1st index = sex, 2nd index = risk group)
double CurrLThigh[2][2]; // Current propn of individuals getting married whose partners
						 // are high risk (1st index = sex, 2nd index = risk group)
double MaleMarriageAdj; // Balancing factor to make # male marriages = # female marriages
double CurrSThighMSM[2]; // Current propn of MSM whose partners are high risk

//============================================================================
// Parameters and arrays in the 'Condoms' and 'PrEP + VM' sheets
//============================================================================

double SexFreqMarital[76][2]; // Monthly frequency of sex in marital relationships
double MedianBehavChange[3]; // Median time to behaviour change
double ProbCondomST[81][2]; // Prob of condom use per sex act in ST relationships
double ProbCondomLT[81][2]; // Prob of condom use per sex act in LT relationships
double ProbCondomFSW; // Prob of condom use per sex act in FSW-client contacts
double RelativeInf[39][2]; // Relative levels of infectiousness by HIV stage, sex
double RelativeCoit[39][2]; // Relative frequency of sex by HIV stage, sex
double RelativeUnprot[39]; // Relative proportion of sex acts that are unprotected
double RelativeTransm[39][3][2]; // RR of transm (HIV stage, , sex)
double RelativeCSWentry[39]; // Relative rates of CSW entry
double RelativeCSWexit[39]; // Relative rates of exit from sex work by HIV stage

double RR_PrEP_MSM[81][2]; // By age & risk group, relative to high-risk MSM aged 20
double RR_PrEP_Het[81][2][2]; // By age, risk group & sex, relative to high-risk F20
double JoinPrEP[81][7]; // Columns O-T in "PrEP + VM" sheet
double JoinVM[81][3]; // Columns U-W in "PrEP + VM" sheet
double JoinRegHCT[81][3]; // Columns X-Z in "PrEP + VM" sheet

//============================================================================
// Parameters and arrays in the 'Circumcision' sheet
//============================================================================

double InitCircumcised[91]; // Initial propn of men circumcised
double WeibullProb[81]; // Prob of getting traditionally circumcised if uncircumcised
						// but still intending to get traditionally circumcised
double CurrNegCircumcised[91]; // % of HIV-negative men who are circumcised
double CircProbPreMMC[91]; // Prob of circumcision in next yr, before MMC campaigns
double CircProbStored; // MMC prob in last year for which MMC data were
					   // specified, for men who are high sexually active
double CurrCircProb[81][4]; // Prob of circumcision in current year, by age,
							// risk group and marital status

//============================================================================
// Parameters and arrays in the 'Transmission' sheet
//============================================================================

double TransmMtoF_ST[81][12]; // Cols B-G, L-Q
double TransmMtoM_ST[81][8]; // Cols H-K, R-U
double TransmFtoM_ST[81][8]; // Cols V-AC (relates only to high risk females)
double TransmMtoF_LT[81][12]; // Cols AE-AP
double TransmMtoFSW[3];
double TransmFSWtoM[4]; // cells AV87:AY87 in the FH_SW sheet
double EctopyFactor[81][2];

//============================================================================
// Parameters and arrays in the 'Progression' sheet
//============================================================================

double MnthlyCD4trans[81][4][2]; // Monthly rates of CD4 transition by age,
								 // CD4 stage and sex
double AveARTstartU200[2]; // Ave rate of starting ART at CD4<200, over last 3 yrs
double MortAdjBelow200[2]; // Adjustment for past ART rollout
double MnthlyAIDSmort[81][2][2]; // Monthly rates of AIDS mortality by age,
								 // CD4 stage and sex
double ProbExitAtEntry[81][5][2]; // Prob of exit in same month as entry

//============================================================================
// Parameters and arrays in the 'Testing' and 'ART start' sheets
//============================================================================

double TestingRateSE[81][12][2]; // Monthly rates of HIV testing by age (1st index),
								 // HIV stage (2nd index) and sex (3rd index)
double TestingRateNPF[81]; // Testing rates in sexually-experienced non-pregnant women
double TestingRateM[81]; // Testing rates in sexually-experienced HIV-neg men
double TestingRateV[20][5][2]; // Testing rates in vertically-infected virgins
double TestingRateNegV[20][2]; // Testing rates in HIV-neg virgins by age and sex
double TestingRateEligSE[81][15][2]; // TestingRateSE * ART-eligible proportion
double TestingRateEligV[20][5][2]; // TestingRateV * ART-eligible proportion
double HCTtoART_SE[81][15][2]; // Monthly rates of HIV diagnosis leading to
							  // immediate ART initiation in sexually experienced
double HCTtoART_V[20][5][2]; // Monthly rates of HIV diagnosis leading to immediate
							 // ART initiation in vertically-infected virgins
double AdultRoot[2]; // AQ96, AS96
double StoredRoot[2]; // AQ98, AS98
double CurrARTinitiation[2]; // AQ99, AS99
double ARTinitByStage[5][3]; // AX14:AZ18

// Self-testing and index testing

double AvePosTesting[81][2]; // Average testing rate in HIV+ indivs, by age & sex
double AvePartnerCoverage[81][2]; // Average ART coverage in HIV+ indivs, by age & sex
double ORpartnerPos[81]; // OR for HIV if partner is positive compared to negative
double PosPartnerProb[81][2][2]; // Prob that partner is pos, by indiv age, HIV & sex
double SelfTestingRate[81][12][2]; // Same indexing as for TestingRateSE
double SelfTestingRateM[81][12][2][6]; // As before, last index referring to modality
double TotTestingRateSE[81][12][2]; // Self-testing + other testing combined

//============================================================================
// Parameters and arrays in the 'ART' sheet
//============================================================================

double OnARTbyIntDur[6][2]; // Fraction of survivors still receiving ART, by time
						 // since ART initiation (in integer years) and sex
double OnARThalfIntDur[6][2]; // As above, but at durations 0.25, 0.5, 1.5, 2.5, 3.5, 4.5
double OnARThalfIntDurP[5]; // % kids still on ART at 0.5, 1.5, 2.5, 3.5, 4.5 yrs
double OnARTpaed[10]; // Fraction of ART-experienced kids currently on ART, by age
double AveCD4byARTdur[4][6]; // Average CD4 count by baseline CD4 (1st index) and
							 // time since ART initiation (2nd index)
double InitAveCD4byARTdur[6]; // Initial assumps for CD4 <200 at ART start
double AveCD4nonIntDur[4][6]; // Average CD4 count by baseline CD4 (1st index) and
							 // non-integer ART duration (2nd index)
double CoV_CD4byARTdur[4][6]; // Coefficient of variation in CD4 counts by baseline
							  // CD4 (1st index) and time since ART initiation
double CoV_CD4nonIntDur[4][6];
double CD4dbnByARTdur[4][4][6]; // Propn of ART patients in each CD4 band (1st index)
								// by baseline CD4 (2nd index) & ART dur (3rd index)
double CD4dbnNonIntDur[4][4][6];
double MortByARTdurM[81][4][5]; // Monthly male mort by age, baseline CD4 and ART dur
double MortByARTdurF[81][4][5]; // Monthly female mort by age, baseline CD4 and ART dur
double MortAdj200M[2]; // Adjustments to mort in men starting ART at CD4 <200 in 1st
					   // year on ART and after >1 year on ART
double MortAdj200F[2]; // Adjustments to mort in women starting ART at CD4 <200
double MortStartART[81][4][2]; // Prob of AIDS death in month of starting ART, by age,
							   // baseline CD4 category and sex (3rd index)
double RatioMaxToMinCD4[6];
double CumPaedARTearly[11][5]; // Cum ART initiations by age and ART dur, in kids (early)
double CumPaedARTlate[11][5]; // Cum ART initiations by age and ART dur, in kids (late)

//============================================================================
// Parameters and arrays in the 'Non-HIV mort' sheet
//============================================================================

double CurrNonAIDSmortEA[91][2]; // Current non-AIDS mortality prob by EXACT age
double CurrNonAIDSmortALB[91][2]; // Current non-AIDS mortality prob by age last b'day
double NonAIDSmortM[91][37]; // non-AIDS mortality rates in males, by age and by year
double NonAIDSmortF[91][37]; // non-AIDS mortality rates in females, by age and by year
double UltNonAIDSmort[91][2]; // Ultimate non-AIDS mortality rates, by age and sex
double RednNonAIDSmort[91][2]; // Non-AIDS mortality reduction factor, post-2007
double WestLifeExpectP[11][2]; // West level 26 life expectancy for children, undiscounted
double WestLifeExpectA[81][2]; // West level 26 life expectancy for adults, undiscounted

//============================================================================
// Parameters and arrays in the 'Fertility' sheet
//============================================================================

double ObservedFert[35][36]; // Observed fertility rates by age and year up to 2020
double FertByAgeLB[36]; // col Y
double HIVnegSEfert[36]; // Fertility in sexually experienced HIV-neg women (col Z)
double StoredFert[36]; // Stored values of FertByAgeLB (col AB)
double FertLastYr[36][2]; // Fertility last year, in HIV-neg/undiagnosed and diagnosed
double UltFert[35]; // Ultimate fertility rate
double AdjustedFert[35]; // col AD
double RednFert[35]; // Fertility reduction factor post-2007
double CurrentFert[36]; // col AF
double RelativeFert[44]; // Relative rates of fertility by HIV stage
double LactatingAdj[36]; // RR of HIV-diagnosed fertility by age due to difs in lactation
double RelativeFertART[20]; // Relative rates of fertility x % on ART, in ART states
double BirthsByHIVstage[36][9]; // columns AR-AZ
double BirthsByHIVstageAdj[36][5]; // As before, adjusted to reflect CURRENT ART use
double TotBirthsByStage[9]; // Range AR42:AZ42
double CD4propn[4]; // Range AT50:AW50
double MatIncidence; // annual HIV incidence rate in pregnant women in current year

//============================================================================
// Parameters and arrays in the 'Migration' sheet
//============================================================================

double MigrationAdj[91][2]; // Migration adjustment factors at end of current yr
double NetMigrants[91][38][2]; // Number of net in-mgrants, by age, year and sex
double MigrationHIVadj[91][3]; // RR HIV after migration (cf before): 1995, 2000, 2010

//=========================================================================
// Parameters in the 'Births' sheet
//=========================================================================

double PTP; // positive at 1st visit, tested positive
double PTN; // positive at 1st visit, tested negative
double PUT; // positive at 1st visit, untested
double PTNRP; // positive at 1st visit, tested negative, retested positive
double PTNRN; // positive at 1st visit, tested negative, retested negative
			  // or not retested
double PUTLP; // positive at 1st visit, untested, later tested positive
double PUTLN; // positive at 1st visit, untested, later tested negative or
			  // remained untested
double ARTmothers; // mothers already on HAART prior to 1st ANC visit

double NTNRP; // negative at 1st visit, tested negative, retested positive
double NTNRN; // tested negative, became infected, retested negative or not retested
double NUTLP; // untested at 1st visit, became infected, later tested positive
double NUTLN; // untested at 1st visit, became infected, later tested negative or
			  // remained untested
double NRN; // negative at 1st visit, remained negative

double TestedPosCD4[4]; // tested positive prior to delivery, not on HAART, by CD4
double TestedPosOnART[2]; // tested positive prior to delivery, initiated HAART
double UndiagnosedPos; // positive at 1st antenatal visit but never diagnosed
double RecentPosDet; // recent positive infection, detected
double RecentPosDetART; // recent positive infection, detected & started ART
double RecentPosUndet; // recent positive infection, undetected

double ChildPosNoPMTCT; // cell O4
double ChildPosPMTCT; // cell O5
double ChildNegMotherPosKnown; // cell O6
double ChildNegMotherPosUnknown; // cell O7
double ChildNegMotherNeg; // cell O8

//=========================================================================
// Parameters and arrays in the 'Child rates' sheet
//=========================================================================

double AveARTstartLate; // average rate of ART start in last 3 years, for kids with late HIV
double MortAdjLate[3]; // effect of ART on change in CD4 distribution at CD4 <15%, current year
double PropnBF[37][3]; // propn of mothers practising breastfeeding strategy, by age of child
double RateOfBFchange[36][3]; // rate at which mothers change breastfeeding strategy
double StartingARTnoBirthPCR[132][2]; // Used to calculate ART initiation rate
double PaedARTinitiation; // Monthly rate of ART initiation in curr year (range U152)
double ARTinitiation[132]; // Monthly cts rate at which ART is initiated
double ARTinitiationStore; // Stored rate of ART initiation from last year for which data available
double ProgToNeedNoPMTCT[132]; // prob of progression to ART need if infected at birth, no PMTCT
double ProgToNeedPMTCT[132]; // prob of progression to ART need if infected at birth, failing PMTCT
double ProgToNeedPostnatal[132]; // prob of progression to ART need if infected postnatally
double AIDSmortNoART[132]; // prob of AIDS mortality if untreated and ART-eligible
double AIDSmortEarlyART[132]; // prob of AIDS mortality if initiated ART early
double NegMatExit[36][3]; // rates at which HIV-negative mothers leave the breastfeeding state
double AcuteMatExit[36][4]; // rates at which acutely-infected mothers leave the BF state
double ChronicMatExit[36][3]; // rates at which chronicly-infected mothers leave the BF state
double KnownMatMFexit[36][3]; // rates at which women of known HIV+ status leave the BF state
double KnownMatEBFexit[36][4]; // rates at which women of known HIV+ status leave the EBF state
double ARTmatMFexit[36][3]; // rates at which women on ART leave the BF state
double ARTmatEBFexit[36][4]; // rates at which women on ART leave the BF state
double ARTexitHR[132][4]; // rates at which treated children leave the high risk ART state
double MnthlyTestingPaed[132][2]; // Mnthly prob of testing in kids by age (early/late disease)
double MnthlyPaedTestToART[132][4]; // Mnthly prob of testing AND starting ART
double MnthlyPaedDiagNoART[132][4]; // Mnthly prob of being diagnosed but NOT starting ART

//=========================================================================
// Arrays in the 'Monthly' sheet
//=========================================================================

// Flow variables that are updated monthly

double AIDSdeathsOnART;
double AIDSdeathsDiagP[3];
double AIDSdeathsART_P[3];
double AIDSdeathsYOB[2]; // AIDS deaths in kids born in the current projection year
double NewPerinatal;
double NewPostnatal;
double NewPostnatal18;
double NewHIVlactating;
double BirthsNegMothers;
double BirthsPosMothers;
double BirthsARTmothers;
double BirthsDiagMothers;
double AdultsNewARTbyCD4[4][2]; // Note stage runs from highest CD4 to lowest
double ChildNewARTlate;
double NewElig350[2]; // Number of M and F (15+) becoming eligible at 350 threshold
double NewElig500[2]; // Number of M and F (15+) becoming eligible at 500 threshold
double NewlyTestedNeg[3]; // Indivs who tested negative through HCT in current year (M, F, child)
double NewlyTestedPos[3];
double NewlyTestedFalseNeg[3]; // Indivs who tested in the acute phase of infection
double NewlyTested1stPos[2];
double NewlyTestedPaed[3][2]; // Newly tested by age (18, 19-59, 60-179 mo) and HIV status
double NewlyTestedAdult[3][2]; // Newly tested by age (15-24, 25-49, 50+) and sex
double NewlyTestedNegST[6]; // # HIV-neg self-test results, by testing modality
double NewlyTestedPosST[6]; // # HIV-pos self-test results, by testing modality
double NewSTtoART[6]; // # HIV-pos self-testers who subsequently start ART, by modality

// Age-specific flow variables that are updated monthly

double PaedNewARTbyAge[15]; // Age groups
double PaedNewARTlate[11];
double NewHIVbyAgeSex[81][2];
double NonAIDSdeathsP[11][2]; // Non-AIDS deaths in children
double AIDSdeathsByAgeP[11][2]; // AIDS deaths at ages 0-10
double AIDSdeathsByAge[81][2]; // AIDS deaths at ages 10+
double TotBirthsByMatAge[36]; // Ages 14-49
double AIDSdeathsMarriedM[76][2][2]; // AIDS deaths in married men
double AIDSdeathsMarriedF[76][2][2]; // AIDS deaths in married women
double AdultsNewARTbyAge[4][2]; // Number of adults starting ART by age & sex
double NewOIdiagnoses[10][2]; // OIs leading to HIV diagnosis, by age and sex

// Age-specific flow variables that are calculated from monthly outputs

double NonAIDSdeathsA[81][2]; // Non-AIDS deaths in adults
double TotDeathsByAgeP[11][2]; // Total deaths in children, by age and sex
double TotDeathsByAge[81][2]; // Total deaths in adults, by age and sex
double MortProbExact[91][2]; // Mortality prob by exact age
double LifeTable[91][2];

// Stock variables that are updated annually

double TotalPop_S[91][2]; // Size of the population, by age and sex, at start of yr
double TotalPositive_S[91][2]; // Size of HIV-positive population at start of yr
double TotalDiagnosed_S[91][2]; // Number of people diagnosed positive at start of yr
double TotalART_S[91][2]; // Number of ART patients at start of yr
double TotalNaiveElig_S[91][2]; // Number who are ART-naive but eligible for ART
double TotalInterrupt_S[91][2]; // Number who are ART-naive but eligible for ART
double TotalSexuallyExp_S[91][2]; // Number of individuals who are sexually experienced
double TotalMarried_S[91][2]; // Number of individuals who are married at start of yr

//=========================================================================
// Arrays and variables used in calibration and uncertainty analysis
//=========================================================================

double ObservedPrev05[12][2]; // HIV prevalence in 2005 HSRC survey by age & sex
double ObservedPrev08[12][2];
double ObservedPrev12[12][2];
double ObservedPrev16[9][2];
double ObservedPrev17[12][2];
double ObservedPrevU208; // HIV prevalence in kids aged <2 in 2008
double ObservedProvHH_P[4]; // HIV prevalence in kids aged 2-14 in selected province, by year
double ObservedProvHH[5][2]; // HIV prevalence in selected province by year and age (15-24, 25+)
double ObservedPrevANC[5][27]; // ANC prevalence by age and year (1991-2015, 2017)
double ObservedProvANC[26]; // ANC prevalence in selected province (1990-2015, 2017)
double ObservedPrevHCT[13]; // HIV prevalence in adults tested for HIV 2004-8, 2010, 2015-7
double ObservedPrevHCT_P[6]; // HIV prevalence in kids tested for HIV, 2015-9
double ObsCoverage[2][2]; // % of HIV-positive M & F (1st index) on ART in 2012 & 2017 (2nd index)
double SEprev05[12][2]; // std error of HIV prevalence estimates in 2005 HSRC survey
double SEprev08[12][2];
double SEprev12[12][2];
double SEprev16[9][2];
double SEprev17[12][2];
double SEprevU208;
double SEprovHH_P[4];
double SEprovHH[5][2];
double SEprevANC[5][27];
double SEprovANC[26];
double SEprevHCT[13];
double SEprevHCT_P[6];
double SEcoverage[2][2];
double RecordedHCT_P[5]; // Recorded numbers of HIV tests in children, 2015-17
double RecordedHCT_P_CoV[5]; // Coefficients of variation for numbers of HIV tests in kids (on log scale)
double CSWstudyDetails[29][3];
double MSMstudyDetails[17][4];
double AgeDbnAdultsOnART[7][9][2]; // % of treated adults in each age group (2nd index) by yr (1st index), sex
double AgeDbnKidsStartingART[10][2]; // Propn of kids starting ART in <1 and 1-4 age groups (2nd index)
									// by year (1st index, starting 2004)
double AgeDbnKidsOnART[8][2]; // Propn of kids on ART in 0-4 & 5-9 age groups (2nd index), since 2011
// by year (1st index, starting 2004)
double ModelPrevPaed[3][2][2]; // Prevalence by age (2-4, 5-9, 10-14), sex and year (2005, 2008)
double ModelPrevPaed2[2][2][2]; // Prevalence by age (0-4, 5-14), sex and year (2012, 2017)
double ModelPrevU208; // Prevalence under age 2 in 2008
double ModelProvHH[5][2];
double ModelProvHH_P[4];
double ModelPrevHCT[13];
double ModelPrevHCT_P[6];
double ModelCoverage[2][2];
double ModelAgeDbnAdultART[7][9][2];
double ProvANCbias = 0.0;
double ANCageWeights[6]; // Propn of births in 15-19, 20-24, 25-29, 30-34, 35-39, 40-49 age groups
double ANCageW_init[6]; // Initial ANC age weights, up to 2012
double ANCageW_change[6]; // Annual change in ANC age weights after 2012
double SpecificityANC; // Specificity of ELISA used in ANC surveys, 1997-2015
double RRprevPrivateANC; // RR HIV in women attending private antenatal clinics
double PropnPregPrivate[7]; // Proportion of pregnant women seeking private antenatal care, by age
double Miscarriage[2]; // Prob of miscarriage/stillbirth in HIV-neg and HIV-positive pregnant women
double FractionRecentF[6][81]; // % of F infections acquired in last year, by age (15-19, ..., 40-49)

double RecordedDeathsP[4][20][2]; // Stats SA recorded paed deaths by age, year (1997-2016) & sex
double ModelDeathsP[4][20][2]; // Model estimates of paed deaths by age, year (1997-2016) & sex
double RecordedDeathsA[16][20][2]; // Stats SA recorded adult deaths by age, year (1997-2016) & sex
double ModelDeathsA[16][20][2]; // Model estimates of adult deaths by age, year (1997-2016) & sex
double AdultCompleteness[16][20][2]; // % of deaths at ages 20+ that get recorded, by age, year & sex
double PaedCompleteness[4][20][2]; // % of paed deaths that get recorded, by age, year and sex
double AdjustedCompleteness[20][2]; // completeness adjustment in provincial model, by year and sex
double SElogNonHIVmort; // std error in non-HIV mortality estimates, on log scale

double EverTested05[5][2][2]; // Proportion ever tested in 2005 HSRC survey by age, sex & HIV status
double EverTested08[5][2][2]; // Proportion ever tested in 2008 HSRC survey by age, sex & HIV status
double EverTested12[5][2][2]; // Proportion ever tested in 2012 HSRC survey by age, sex & HIV status
double EverTested16[4][2][2]; // Proportion ever tested in 2016 DHS by age, sex & HIV status
double EverTested17[5][2][2]; // Proportion ever tested in 2017 HSRC survey by age, sex & HIV status
double SEtested05[5][2][2]; // std error of HIV prevalence estimates in 2005 HSRC survey
double SEtested08[5][2][2];
double SEtested12[5][2][2];
double SEtested16[4][2][2];
double SEtested17[5][2][2];
double ModelTested05[5][2][2]; // Model estimate of % ever tested in 2005, by age, sex & HIV status
double ModelTested08[5][2][2]; // Model estimate of % ever tested in 2008, by age, sex & HIV status
double ModelTested12[5][2][2]; // Model estimate of % ever tested in 2012, by age, sex & HIV status
double ModelTested16[4][2][2]; // Model estimate of % ever tested in 2016, by age, sex & HIV status
double ModelTested17[5][2][2]; // Model estimate of % ever tested in 2017, by age, sex & HIV status
double SeTestingHistory[2]; // Sensitivity of self-reported HIV testing history (HIV-neg & -pos)
double SpTestingHistory; // Specificity of self-reported HIV testing history

double ChildPIPdeaths[13][2]; // Total audited deaths 2005-2017, ages 1-4 and 5-9
double ChildPIPdiag[13][2]; // % of deaths diagnosed HIV-positive, 2005-2017, ages 1-4 and 5-9
double ChildPIP_ART[13][2]; // % of HIV-diagnosed deaths on ART 2005-2017, ages 1-4 and 5-9
double ModelPIPdiag[13][2];
double ModelPIP_ART[13][2];
double RRdiagDeathsPIP[2]; // RR of death being recorded in facility if HIV-neg/undiagnosed compared to
						   // (a) diagnosed ART-naive, and (b) ART-experienced

double AIDScasesByYr[5]; // Reported new adult AIDS cases, 1990-94
double AIDScasesProfile[10][2]; // Reported new adult AIDS cases (1993-94) by age (1st index) & sex
double OIsDiagnosedByYr[5]; // Modelled # OIs leading to HIV diagnosis, 1990-94
double OIsDiagnosedProfile[10][2]; // Modelled # OIs leading to HIV diagnosis (1993-4), by age & sex

const int ARTdataPoints = 155; // Number of reported ART totals that combine adults and children
const int ARTdataPointsP = 98; // Number of reported ART totals in children
const int ARTdataPointsM = 8; // Number of estimates of fraction of adult ART patients who are men
double ARTtotals[ARTdataPoints][4]; // Total numbers on ART in public and private sectors
double ARTtotalsP[ARTdataPointsP][4]; // Total children on ART in public and private sectors
double ARTmale[ARTdataPointsM][3]; // Proportions of ART patients who are male (year, mean, SE)
double RecordedARTstart[12][4][2]; // Number starting ART by year, CD4 and sex
double ARTmodelled[ARTdataPoints][2]; // Modelled total number on ART (current and cumulative)
double ARTmodelledP[ARTdataPointsP][2]; // Modelled total children on ART (current and cumulative)
double ARTmodelledM[ARTdataPointsM]; // Modelled % of adult ART patients who are male
double LastDateCum = 2009.08; // Last date when reported public totals were definitely cumulative
double AnnSwitchCumToCurr=0.2626; // Annual rate of switch from reporting cumulative totals to current
						   // totals, after LastDateCum
double BsplineCoef[10]; // B spline coefficients for total numbers starting ART
double BsplineCoefP[10]; // B spline coefficients for total children starting ART
double SDchangeBspline; // Std deviation of change in B spline coefficient (sampled from hyperprior)
double MalePropnART[116]; // Fraction of adult ART initiators who are male (by year)
double TotBeginART[116]; // Total # patients starting ART (by year)
double MarriageData[15][2][4]; // % of adults married by age and sex, in 1996, 2001, 2007, 2016
double ModelMarried[15][2][4]; // % of adults married by age and sex, in 1996, 2001, 2007, 2016

double LogLikelihood;
const int MCMCdim = 43; // Number of parameters in uncertainty analysis
const int MaxPriors = 132; // Corresponds to the number of rows of data in Priors.txt
int InclPriors[MaxPriors][2]; // Indicator of which priors are included (1st index) and if
							  // included their index in MCMCdim (2nd index)
double RandPrior[MCMCdim]; // Random numbers used to sample from prior in current simulation
double Cholesky1[MCMCdim][MCMCdim];
double Covariance[MCMCdim][MCMCdim]; // The covariance matrix for the MCMC parameters
const int InitSample = 2;
const int ResampleSize = 2;
int SampleID[ResampleSize];
int CurrSim;
double temp[ResampleSize][41]; // Previously local to the SampleInput function in OutputArray class

// Parameters for IMIS

const int IMISind=1; // 0 = no IMIS; 1 = IMIS
const int IMISsteps=200; // max # steps in IMIS algorithm
const int StepSample=1000; // # additional simulations at each IMIS step after the first
const int TotalSimulations=209000; // Must be = InitSample + StepSample * (IMISsteps - 1)
const int ImportanceSamplingStep1=0; // 1 if importance sampling is used in step 1, 0 otherwise
int CurrIMISstep; // indicates current position in IMIS algorithm
double LogLxWeight[TotalSimulations]; // Log of (likelihood * importance ratio)
double weights[TotalSimulations]; // Sample weights calculated from LogLxWeight
double RandomParameterIMIS[MCMCdim+1][TotalSimulations]; // The '+1' is to record logL
int PriorTypes[MCMCdim]; // Indicators for prior types (0 = beta, 1 = gamma, 2 = normal)
double aParameters[MCMCdim]; // Alpha parameters for priors
double bParameters[MCMCdim]; // Beta parameters for priors
double aParameters2[MCMCdim]; // Alpha parameters for importance distributions
double bParameters2[MCMCdim]; // Beta parameters for importance distributions
double MixtureMean[IMISsteps][MCMCdim]; // Means of the different mixture components
double PosteriorMean[IMISsteps][MCMCdim]; // Means of the posterior dbn at different IMIS steps
double NextMode[MCMCdim+1]; // Parameter values & LogLxWeight for best parameter combination
double cholIMIS[MCMCdim][MCMCdim][IMISsteps];
double invIMIS[MCMCdim][MCMCdim][IMISsteps];
double sorted[TotalSimulations]; // Used in the GetPercentile function
double LogIntegratedL[IMISsteps]; // Log of integrated likelihood estimated at each IMIS step
double FractionUnique[IMISsteps]; // Expected fraction of points in resample that are unique
double EffectiveSS[IMISsteps]; // Effective sample size
double Efficiency[IMISsteps]; // Effective sample size/ # function evaluations
double Distance[TotalSimulations]; // Mahalanobis distance
double CumLxWeight[TotalSimulations+1];
double ARTerrorHistory[TotalSimulations];

//===========================================================================
// Classes
//===========================================================================

class Child
{
	public:

	int Sex; // 0 = male, 1 = female
	double PropnBirths;
	double NonAIDSmort[132];
	double NewNonVertCurrY;

	// Arrays to represent population profile at START of month
	double NegMatMF[133];
	double NegMatEBF[133];
	double AcuteMatMF[133];
	double AcuteMatEBF[133];
	double UnawareMatMF[133];
	double UnawareMatEBF[133];
	double AwareMatMF[133];
	double AwareMatEBF[133];
	double ARTmatMF[133];
	double ARTmatEBF[133];
	double NegChildFF[133];
	double PosChildAtBirthNoPMTCT[133];
	double PosChildAtBirthPMTCT[133];
	double PosChildAfterBirth[133];
	double ARTeligible[133];
	double DiagChildAtBirthNoPMTCT[133];
	double DiagChildAtBirthPMTCT[133];
	double DiagChildAfterBirth[133];
	double DiagARTeligible[133];
	double OnARTearly[133];
	double OnARTlate1st3m[133];
	double OnARTlateAfter3m[133];
	double StoppedART[133];
	double Total[133];

	// Arrays to represent population profile at END of month
	double NegMatMF_E[133];
	double NegMatEBF_E[133];
	double AcuteMatMF_E[133];
	double AcuteMatEBF_E[133];
	double UnawareMatMF_E[133];
	double UnawareMatEBF_E[133];
	double AwareMatMF_E[133];
	double AwareMatEBF_E[133];
	double ARTmatMF_E[133];
	double ARTmatEBF_E[133];
	double NegChildFF_E[133];
	double PosChildAtBirthNoPMTCT_E[133];
	double PosChildAtBirthPMTCT_E[133];
	double PosChildAfterBirth_E[133];
	double ARTeligible_E[133];
	double DiagChildAtBirthNoPMTCT_E[133];
	double DiagChildAtBirthPMTCT_E[133];
	double DiagChildAfterBirth_E[133];
	double DiagARTeligible_E[133];
	double OnARTearly_E[133];
	double OnARTlate1st3m_E[133];
	double OnARTlateAfter3m_E[133];
	double StoppedART_E[133];
	double Total_E[133];

	// Arrays to represent movements in the course of the month
	double AIDSdeaths[133];
	double AIDSdeathsUntreated[133];
	double AIDSdeathsUndiagnosed[133];
	double NonAIDSdeaths[133];
	double NewHIVpostnatal[133];
	double StartingART[133];
	double StartingARTlate[133];
	double NewMatHIV[133];
	double NewMatUnaware[133];
	double NewMatAwareMF[133];
	double NewMatARTMF[133];
	double NewPostnatalDiag[18];

	Child(int Gender);
	void GetStartYrProfile();
	void GetNonAIDSmort(); // Converts annual mortality to monthly mortality
	void GetEndProfile();
	void UpdateStartProfile();
	void UpdateMigP();
};

class Adult
{
	public:

	int Sex; // 0 = male, 1 = female
	int Risk; // 1 = high risk, 2 = low risk
	int MarriedInd; // 0 = unmarried, 1 = married
	int VirginInd; // 0 = sexually experienced, 1 = virgin
	int FSWind; // 1 => Female sex worker
	int SpouseRisk; // 1 = high risk, 2 = low risk (only relevant if married)
	int CircInd; // 0 = uncircumcised, 1 = circumcised (only relevant to men)
	int MSMind; // 0 = heterosexual, 1 = bisexual/MSM (only relevant to men)

	// Arrays to represent population profile at START of month
	double NegNoHCT[81]; // HIV-negative, never tested for HIV
	double NegPastHCT[81]; // HIV-negative, previously tested
	double RegHCT[81]; // HIV-negative, receiving regular HCT
	double RegPrEP[81]; // HIV-negative, receiving regular PrEP
	double RegVM[81]; // HIV-negative, receiving regular vaginal microbicide
	double PosNoHCT[81][5]; // HIV-positive, never tested for HIV (2nd index is CD4 stage)
	double PosHCTpreHIV[81][5]; // HIV-positive, last tested before HIV acquisition
	double PosDiagnosedPreART[81][5]; // Diagnosed HIV-positive, not yet on ART
	double OnARTpre500[81][5]; // On ART, having started at CD4 >=500 (2nd index is ART duration)
	double OnART500[81][5]; // On ART, having started at CD4 350-499
	double OnART350[81][5]; // On ART, having started at CD4 200-349
	double OnART200[81][5]; // On ART, having started at CD4 <200
	double StoppedART[81][4]; // Stopped ART (2nd index is CD4 stage)
	double Total[81];

	// Arrays to represent population profile at END of month
	double NegNoHCT_E[81];
	double NegPastHCT_E[81];
	double RegHCT_E[81];
	double RegPrEP_E[81];
	double RegVM_E[81];
	double PosNoHCT_E[81][5];
	double PosHCTpreHIV_E[81][5];
	double PosDiagnosedPreART_E[81][5];
	double OnARTpre500_E[81][5];
	double OnART500_E[81][5];
	double OnART350_E[81][5];
	double OnART200_E[81][5];
	double StoppedART_E[81][4];
	double Total_E[81];

	// Arrays to represent HIV transmission probabilities
	double ProbTransm[81][12]; // Note that the stage indices are defined
							   // differently for M and F.
	double ProbHIVacq[81][3]; // Prob of HIV acquisition
	double FSWcontactsByAge[81]; // Annual total, only relevant to high risk men

	// Arrays to represent movements in the course of the month/year
	double AIDSdeathsUntreated[81];
	double AIDSdeathsTreated[81];
	double NewHIV[81];
	double StartingART[81][4]; // 2nd index is baseline CD4 stage
	double NewARTbyCD4[4];
	double NewARTbyAge[4]; // Age groups 15-24, 25-34, 35-44, 45+

	Adult(int Gender, int iRisk, int Married, int Virgin, int SRisk, int FSW, int Circ, int MSM);
	void GetStartYrProfile();
	void UpdateProbTransmM();
	void UpdateProbTransmF();
	void UpdateProbTransmSW(); // Only applies to sex workers
	void UpdateProbAcqM(int group);
	void UpdateProbAcqF(int group);
	void GetEndProfile();
	//void GetEndProfileFSW();
	void SumNewART();
	void UpdateStartProfile();
	void UpdateStartTotal();
	void UpdateDemog(); // Changes in age, ART duration, non-AIDS mort, migration
	void GetFSWcontacts(); // Only relevant to high risk men
};

class OutputArray
{
	public:
	OutputArray(int n);

	int columns;
	double out[InitSample][50]; // None of the arrays require > 40 columns.

	void Record(const char* filout, int n);
	void RecordSample(const char* filout, int n);
	void SampleInput();
};

class PostOutputArray
{
	// Same as OutputArray class except that we only use this to record outputs from
	// the posterior distribution (and hence require smaller output array).

	public:
	PostOutputArray(int n);

	int columns;
	double out[ResampleSize][116]; // None of the arrays require > 86 columns.
	double Means[116][3]; // Mean, 95% LL and 95% UL

	void RecordSample(const char* filout);
	void GetMeans(); // Calculate Means and 95% CIs
};

class OutputByAge
{
	// Same as PostOutputArray class except that we only use this to record outputs from
	// the posterior distribution (and hence require smaller output array).

	public:
	OutputByAge(int n1, int n2);

	int columns;
	int rows;
	double out[92][116]; // None of the arrays require > 86 columns.

	void GetMeans(); // Calculate Means
};

//============================================================================
// General functions
//============================================================================

// Input and initialization functions
void ReadPaedAssumps();
void ReadAdultAssumps();
void ReadRollout();
void ReadStartingPop();
void ReadMarriage();
void ReadCD4dbn();
void ReadNonHIVmort();
void ReadWestTable();
void ReadFertility();
void ReadMigration();
void ReadCalibData();
void ReadHIVprevData();
void ReadANCprevData();
void ReadCSWprevData();
void ReadMSMprevData();
void ReadProvHIV();
void ReadHCTprevData();
void ReadHCTpaedData();
void ReadMortData();
void ReadCompleteness();
void ReadHCTdata();
void ReadAIDScases();
void ReadARTtotals();
void ReadChildPIP();
void ReadInitPrev();
void ReadAdultRoot();
void ReadAllFiles();
void SetInitialParameters();
void SetInitSexActivity();
double GetMarriedPropnAtStart(int Sex, int Age);
void SetCD4byARTdur(); // Calculations in cols G-S of "ART" sheet
void CalcInterruptions(); // Calculation of values in row 5 of "ART" sheet
void SetActivityByStage(); // Calculations in cols R-S and AA-AB of "Condoms" sheet
void SetFertByStage(); // Calculations in col AK of "Fertility" sheet
void SetProgression(int setting); // setting = 0 implies start year, 1 otherwise
void SetFutureRollout(); // Only gets called if VaryFutureInterventions = 1

// Functions called on a monthly basis
void GetCurrBehavDbnM(); // Calculations in cols N-Q of 'Sex activity' sheet
void GetCurrBehavDbnF(); // Calculations in cols N-Q of 'Sex activity' sheet
void UpdatePop(); // Calculations in cols E-DB of "Population" sheet
void UpdateFSWdemand(); // Calculation of TotalFSW (V175 in 'Sex activity' sheet)
void UpdateAgePrefsF(); // Calculation of ST and LT age prefs for females
void UpdatePartnerAcqM(); // Male partnership formation rates, age prefs, coital freq
void UpdateMixingST(); // Calculation of propn of ST partners who are high risk
void UpdateCondomUse(); // Female and male rates of condom use
void UpdateFert(); // Calculations in cols Y-AF of 'Fertility' sheet
void UpdateBirths(); // Calculations in cols AN-AV of 'Fertility' sheet
void GetBirthsByHIV(); // Calculations in Births sheet
void GetPrevPregnant(); // Calculations of PrevPregnant in 'Monthly' sheet
void SetMnthlyPaedParameters(); // Calculations in "Child rates" depending on maternal
								// HIV incidence & paediatric ART initiation rates
void UpdatePrEPandVM();
void UpdateTestingRates(); // Calcs in 'Testing' sheet
void UpdateTestingToART(); // Calcs in B-AE of 'ART start' sheet
void UpdateTransmProbs();
void CalcHIVtransitions(); // Calculate movements between HIV states
void CopyEndToStart();
void SetARTinitiation(); // Calls the Adult and Paed ART initiation functions below
void SetAdultARTinitiation(); // Called when inputs are numbers starting ART
void SetAdultARTinitiation2(); // Called when inputs are numbers starting ART
void CalcCOVIDimpactART();
void SetPaedARTinitiation(); // Called when inputs are numbers starting ART
void SetPaedARTinitiation2(); // Called when inputs are numbers starting ART
void UpdateFSW();
void UpdateMonthlyCum();
void UpdateMonthlySTesting();
void OneMonth(int im);

// Functions called on an annual basis
void ResetMonthlyCum(); // Sets to zero the running totals in the Monthly sheet
void ResultsAtStartOfYr();
void Get2ndLineOutput();
void GetMarriageForCalib();
void CalcMultPartners();
void GetPrEPrateFSW();
void SetCurrYearParameters();
void SetAnnPaedParameters();
void CalcOIsTested();
void CalcPosPartnerProb();
void CalcSelfTestingRates();
void GetTotalTesting();
void CalcHCT1stTime(); // Only relevant if user specifies # HIV tests as model input
void CalcRRtestVirgin();
void UpdateNonAIDSmort();
void UpdateAIDSmort();
void UpdateARTmort(); // Calculations in cols V-BS of ART sheet
void UpdateMigration();
void UpdateCircProb();
void UpdateMC(Adult* Uncirc, Adult* Circ);
void CalcCurrMarriageRates();
void GetMarriageAndDivorceRates();
double SpouseAIDSmort(int ia, int ir1, int ir2, int ig);
void UpdateMarital(Adult* Single, Adult* MarriedH, Adult* MarriedL);
void UpdateAllDemog();
void MoveIntoAdultGroups();
void UpdateDebut(Adult* Virgin, Adult* SexExp, Adult* SexExp2, int ind); // ind = 0 at month end, 1 at year end
void UpdateStartTot();
void ResultsAtEndOfYr();
void ResultsAtEndOfYr2(); // Mostly for HIV incidence outputs
void OneYear();

// Output functions
//void SaveHSRCcalib(const char* filout);
//void SaveAdolProj(const char* filout);
//void SaveFSWprofile(const char* filout);
//void SaveHCTbyAge(const char* filout);
void GetSummaryOutputs(const char* filout);
void GetAddedOutputs(const char* filout);
void GetOutputsByAge(const char* filout);

//============================================================================
// Functions used in uncertainty analysis
//============================================================================

// SIR

void CalcLikelihood();
double CalcPaedPrevLogL();
double CalcAdultPrevLogL();
double CalcHHprovLogL();
double CalcHHprovPlogL();
double CalcANCprevLogL();
double CalcANCprevLogL2();
double CalcANCprovLogL();
double CalcCSWprevLogL();
double CalcMSMprevLogL();
double CalcMortLikelihoodA();
double CalcMortLikelihoodP();
double CalcHCTlogL();
double CalcHCTprevLogL();
double CalcHCTprevPlogL();
double CalcRecHCT_PlogL();
double CalcAIDStrendLogL();
double CalcAIDSageLogL();
double CalcARTtotalLogL();
double CalcMaleARTlogL();
double CalcARTbyCD4logL();
double CalcAgeARTlogL();
double CalcAgeART_PlogL();
double CalcAgeART_P2logL();
double CalcChildPIPlogL();
double CalcARTcoverageLogL();
double CalcMarriageLogL();
void GetCholesky1(double mat[MCMCdim][MCMCdim]);
void GetInverse1(double mat[MCMCdim][MCMCdim], double matinv[MCMCdim][MCMCdim]);
void SimulateParameters();
void SimulateParameters_FSW();
double SamplePrior(int PriorIndex);
void GetAnnNewART();
void GetCurrART();
void GenerateSample(); // Not yet updated in THEMBISA
void RunSample();

// IMIS

void ReadPriors();
void ReadImportanceDbns();
void ReadPrevIMIS();
void ReadPrevIMIS2(double completed);
void SaveTempIMIS();
void SaveTempIMIS2(double completed);
void GetMultNorm(double RandUnif[MCMCdim], double MultNorm[MCMCdim]);
double GetMultNormPDF(double MultNorm[MCMCdim], int Component);
void GetMahalanobis(double distance[TotalSimulations]);
double GetPercentile(double values[TotalSimulations], double percentile);
void runIMIS(double CumSteps);
void OneIMISstep(double CumSteps);
void SimulateParameters_IMIS();
double GetParameter(int PriorIndex);

// Nelder-Mead algorithm

double ReturnNegLogL(double ParameterSet[10]);
void ReadInitSimplex(const char* input, double ParameterCombinations[21][20], int Dimension);
void SaveFinalSimplex(const char* filout, double ParameterCombinations[21][20], int Dimension);
void SaveNegLogL(const char* filout, double NegLogL[21]);
void MaximizeLikelihood(double FTol, const char* input, const char* filout);

//============================================================================
// Objects created from defined classes
//============================================================================

Child MaleChild(0);
Child FemChild(1);
// Adult indices are Gender, Risk, Married, Virgin, SRisk, FSW and Circ
Adult MHU_virgin(0, 1, 0, 1, 0, 0, 0, 0);
Adult MHC_virgin(0, 1, 0, 1, 0, 0, 1, 0);
Adult MHU_ST(0, 1, 0, 0, 0, 0, 0, 0);
Adult MHC_ST(0, 1, 0, 0, 0, 0, 1, 0);
Adult MHU_STM(0, 1, 0, 0, 0, 0, 0, 1);
Adult MHC_STM(0, 1, 0, 0, 0, 0, 1, 1);
Adult MHU_LTH(0, 1, 1, 0, 1, 0, 0, 0);
Adult MHC_LTH(0, 1, 1, 0, 1, 0, 1, 0);
Adult MHU_LTL(0, 1, 1, 0, 2, 0, 0, 0);
Adult MHC_LTL(0, 1, 1, 0, 2, 0, 1, 0);
Adult MLU_virgin(0, 2, 0, 1, 0, 0, 0, 0);
Adult MLC_virgin(0, 2, 0, 1, 0, 0, 1, 0);
Adult MLU_ST(0, 2, 0, 0, 0, 0, 0, 0);
Adult MLC_ST(0, 2, 0, 0, 0, 0, 1, 0);
Adult MLU_STM(0, 2, 0, 0, 0, 0, 0, 1);
Adult MLC_STM(0, 2, 0, 0, 0, 0, 1, 1);
Adult MLU_LTH(0, 2, 1, 0, 1, 0, 0, 0);
Adult MLC_LTH(0, 2, 1, 0, 1, 0, 1, 0);
Adult MLU_LTL(0, 2, 1, 0, 2, 0, 0, 0);
Adult MLC_LTL(0, 2, 1, 0, 2, 0, 1, 0);
Adult FH_virgin(1, 1, 0, 1, 0, 0, 0, 0);
Adult FH_ST(1, 1, 0, 0, 0, 0, 0, 0);
Adult FH_SW(1, 1, 0, 0, 0, 1, 0, 0);
Adult FH_LTH(1, 1, 1, 0, 1, 0, 0, 0);
Adult FH_LTL(1, 1, 1, 0, 2, 0, 0, 0);
Adult FL_virgin(1, 2, 0, 1, 0, 0, 0, 0);
Adult FL_ST(1, 2, 0, 0, 0, 0, 0, 0);
Adult FL_LTH(1, 2, 1, 0, 1, 0, 0, 0);
Adult FL_LTL(1, 2, 1, 0, 2, 0, 0, 0);

OutputArray RandomUniform(MCMCdim);
OutputArray ModelParameters(MCMCdim);
PostOutputArray FutureInterventions(34);
OutputArray LogL(1);

// Prevalence outputs
PostOutputArray PrevPreg15to49(116);
PostOutputArray PrevPreg15to19(116);
PostOutputArray PrevPreg20to24(116);
PostOutputArray PrevPreg25to29(116);
PostOutputArray PrevPreg30to34(116);
PostOutputArray PrevPreg35to39(116);
PostOutputArray PrevPreg40to49(116);
PostOutputArray AdjPreg15to49(116);
PostOutputArray AdjPreg15to19(116);
PostOutputArray AdjPreg20to24(116);
PostOutputArray AdjPreg25to29(116);
PostOutputArray AdjPreg30to34(116);
PostOutputArray AdjPreg35to39(116);
PostOutputArray ANCbias(2);
PostOutputArray ErrorVariance(3);
PostOutputArray PrevFSW(116);
PostOutputArray PrevFSW15to24(116);
PostOutputArray PrevFSW25plus(116);
PostOutputArray PrevClients(41);
PostOutputArray TotalHIV(116);
PostOutputArray TotPaedHIV(116);
PostOutputArray TotHIV15M(116);
PostOutputArray TotHIV15F(116);
PostOutputArray TotHIV15(116);
PostOutputArray TotHIV15to24(116);
PostOutputArray TotHIV15to24M(116);
PostOutputArray TotHIV15to24F(116);
PostOutputArray TotHIV15to49(116);
PostOutputArray TotHIV15to49M(116);
PostOutputArray TotHIV15to49F(116);
PostOutputArray TotHIV25to49(116);
PostOutputArray TotHIV25to49M(116);
PostOutputArray TotHIV25to49F(116);
PostOutputArray TotHIV50plus(116);
PostOutputArray TotHIV50plusM(116);
PostOutputArray TotHIV50plusF(116);
PostOutputArray Prev0to14(116);
PostOutputArray Prev2to14(116);
PostOutputArray Prev15to24(116);
PostOutputArray Prev15to24M(116);
PostOutputArray Prev15to24F(116);
PostOutputArray Prev15to49(116);
PostOutputArray Prev15to49M(116);
PostOutputArray Prev15to49F(116);
PostOutputArray Prev25to49(116);
PostOutputArray Prev25to49M(116);
PostOutputArray Prev25to49F(116);
PostOutputArray Prev25plus(116);
PostOutputArray Prev50plus(116);
PostOutputArray Prev50plusM(116);
PostOutputArray Prev50plusF(116);
PostOutputArray Prev15plus(116);
PostOutputArray Prev15plusM(116);
PostOutputArray Prev15plusF(116);
PostOutputArray HSRCcalib2002(16);
PostOutputArray HSRCcalib2005(18);
PostOutputArray HSRCcalib2008(18);
PostOutputArray HSRCcalib2012(18);
PostOutputArray HSRCcalib2017(18);
PostOutputArray DHScalib2016(18);
PostOutputArray MSMprev18plus(116);
PostOutputArray MSMprev18to24(116);
PostOutputArray MSMprev25plus(116);
PostOutputArray MSMprev15to49(116);
PostOutputArray MalePrev18plus(116);
PostOutputArray HIVprevalence(116);
/*PostOutputArray Prev0to1(31);
PostOutputArray Prev2to4M(31);
PostOutputArray Prev2to4F(31);
PostOutputArray Prev5to9M(31);
PostOutputArray Prev5to9F(31);
PostOutputArray Prev10to14M(31);
PostOutputArray Prev10to14F(31);*/

// HIV incidence outputs
PostOutputArray NewHIVinFSW(116);
PostOutputArray NewHIVclients(116);
PostOutputArray HIVinc0to14(116);
PostOutputArray HIVinc15to49(116);
PostOutputArray HIVinc15to49adj(116);
PostOutputArray HIVinc15to49M(116);
PostOutputArray HIVinc15to49F(116);
PostOutputArray HIVinc15to24(116);
PostOutputArray HIVinc15to24M(116);
PostOutputArray HIVinc15to24F(116);
PostOutputArray HIVinc25to49(116);
PostOutputArray HIVinc25to49M(116);
PostOutputArray HIVinc25to49F(116);
PostOutputArray HIVinc50(116);
PostOutputArray HIVinc50M(116);
PostOutputArray HIVinc50F(116);
PostOutputArray HIVinc15plus(116);
PostOutputArray HIVinc15plusM(116);
PostOutputArray HIVinc15plusF(116);
PostOutputArray HIVinc2000(18); // HIV incidence by age, sex in 2000
PostOutputArray HIVinc2010(18); // HIV incidence by age, sex in 2010
PostOutputArray PAFforCSW(41);
PostOutputArray HIVincFSW(116);
PostOutputArray HIVincMSM(116);
PostOutputArray ANCincidence(116);
PostOutputArray ANCincidenceAdj(116);
PostOutputArray NewMTCT(116);
PostOutputArray NewHIVatBirth(116);
PostOutputArray NewHIVafterBirth(116);
PostOutputArray NewHIVto18mo(116);
PostOutputArray NewHIVmothersBF(116);
PostOutputArray NewDiagnosesPregnancy(116);
PostOutputArray RediagnosesPregnancy(116);
PostOutputArray TotANCtests(116);
PostOutputArray VertTransmKnownPos(116);
PostOutputArray MTCTrateAtBirth(116);
PostOutputArray MTCTrateBirthDiag(116);
PostOutputArray MTCTrate18moDiag(116);
PostOutputArray TotMTCTrate(116);
PostOutputArray TotMTCTallBirths(116);
PostOutputArray TotalNewHIV(116);
PostOutputArray NewAdultHIV(116);
PostOutputArray NewHIV_M(116); // ages 10+
PostOutputArray NewHIV_F(116); // ages 10+
PostOutputArray TotIncidence(116);
PostOutputArray IncPrevRatio(116);
PostOutputArray IncPrevRatioFtoM(116);
PostOutputArray IncPrevRatioMtoF(116);

// Mortality outputs
PostOutputArray Deaths0M(41);
PostOutputArray Deaths1M(41);
PostOutputArray Deaths5M(41);
PostOutputArray Deaths10M(41);
PostOutputArray Deaths20M(41);
PostOutputArray Deaths25M(41);
PostOutputArray Deaths30M(41);
PostOutputArray Deaths35M(41);
PostOutputArray Deaths40M(41);
PostOutputArray Deaths45M(41);
PostOutputArray Deaths50M(41);
PostOutputArray Deaths55M(41);
PostOutputArray Deaths0F(41);
PostOutputArray Deaths1F(41);
PostOutputArray Deaths5F(41);
PostOutputArray Deaths10F(41);
PostOutputArray Deaths20F(41);
PostOutputArray Deaths25F(41);
PostOutputArray Deaths30F(41);
PostOutputArray Deaths35F(41);
PostOutputArray Deaths40F(41);
PostOutputArray Deaths45F(41);
PostOutputArray Deaths50F(41);
PostOutputArray Deaths55F(41);
PostOutputArray AIDSdeathsTot(116);
PostOutputArray AIDSdeathsPaed(116);
PostOutputArray AIDSdeathsAdultM(116);
PostOutputArray AIDSdeathsAdultF(116);
PostOutputArray AIDSdeaths0(116);
PostOutputArray AIDSdeaths1to4(116);
PostOutputArray AIDSdeaths5to9(116);
PostOutputArray AIDSdeaths10to14(116);
PostOutputArray AIDSdeaths15to24(116);
PostOutputArray AIDSdeaths15to24M(116);
PostOutputArray AIDSdeaths15to24F(116);
PostOutputArray AIDSdeaths15to49(116);
PostOutputArray AIDSdeaths15to49M(116);
PostOutputArray AIDSdeaths15to49F(116);
PostOutputArray AIDSdeaths25to49(116);
PostOutputArray AIDSdeaths25to49M(116);
PostOutputArray AIDSdeaths25to49F(116);
PostOutputArray AIDSdeaths50plus(116);
PostOutputArray AIDSdeaths50plusM(116);
PostOutputArray AIDSdeaths50plusF(116);
PostOutputArray AIDSdeaths20to59M(31);
PostOutputArray AIDSdeaths20to59F(31);
PostOutputArray NonAIDSdeaths2005(16); // Males by age, then females by age
PostOutputArray NonAIDSdeaths(116);
PostOutputArray NonAIDSdeathsHIVpos(116);
PostOutputArray NonAIDSdeaths20M(41);
PostOutputArray NonAIDSdeaths25M(41);
PostOutputArray NonAIDSdeaths30M(41);
PostOutputArray NonAIDSdeaths35M(41);
PostOutputArray NonAIDSdeaths40M(41);
PostOutputArray NonAIDSdeaths45M(41);
PostOutputArray NonAIDSdeaths50M(41);
PostOutputArray NonAIDSdeaths55M(41);
PostOutputArray NonAIDSdeaths20F(41);
PostOutputArray NonAIDSdeaths25F(41);
PostOutputArray NonAIDSdeaths30F(41);
PostOutputArray NonAIDSdeaths35F(41);
PostOutputArray NonAIDSdeaths40F(41);
PostOutputArray NonAIDSdeaths45F(41);
PostOutputArray NonAIDSdeaths50F(41);
PostOutputArray NonAIDSdeaths55F(41);
PostOutputArray IMR(116);
PostOutputArray U5MR(116);
PostOutputArray Tot45q15(116);
PostOutputArray M45q15(116);
PostOutputArray F45q15(116);
PostOutputArray LifeExpectTot(116);
PostOutputArray LifeExpectM(116);
PostOutputArray LifeExpectF(116);
PostOutputArray AIDSdeathsUndiag(41);
PostOutputArray AIDSdeathsDiagPreART(41);
PostOutputArray AIDSdeaths1st6moART(41);
PostOutputArray AIDSdeathsAfter6moART(41);
PostOutputArray AIDSdeathsART(116);
PostOutputArray DiagDeaths1to4(116);
PostOutputArray DiagDeaths5to9(116);
PostOutputArray DiagDeaths10to14(116);
PostOutputArray ARTdeaths1to4(116);
PostOutputArray ARTdeaths5to9(116);
PostOutputArray ARTdeaths10to14(116);
PostOutputArray LYlostAIDS(116);
PostOutputArray CrudeAIDSmort(116);
PostOutputArray CompletenessPaed(16);
PostOutputArray CompletenessAdj(2);

// Other demographic outputs
PostOutputArray TotPop(116);
PostOutputArray TotBirths(116);
PostOutputArray TotFertRate(116);
PostOutputArray BirthRate(116);
PostOutputArray TotInfants(116);
PostOutputArray Children1to2(116);
PostOutputArray Children3to5(116);
PostOutputArray Children6to13(116);
PostOutputArray TotalUnder15(116);
PostOutputArray Adolesc15to19(116);
PostOutputArray Children6to18(116);
PostOutputArray MalesOver15(116);
PostOutputArray FemalesOver15(116);
PostOutputArray Males15to64(116);
PostOutputArray Females15to64(116);
PostOutputArray Total15to24(116);
PostOutputArray Total15to24M(116);
PostOutputArray Total15to24F(116);
PostOutputArray Total15to49(116);
PostOutputArray Total15to49M(116);
PostOutputArray Total15to49F(116);
PostOutputArray Total25to49(116);
PostOutputArray Total25to49M(116);
PostOutputArray Total25to49F(116);
PostOutputArray Total50plus(116);
PostOutputArray Total50plusM(116);
PostOutputArray Total50plusF(116);
PostOutputArray DependencyRatio(116);
PostOutputArray AgingIndex(116);
PostOutputArray MarriedPropn1996(30);
PostOutputArray MarriedPropn2001(30);
PostOutputArray MarriedPropn2007(30);
PostOutputArray MarriedPropn2016(30);

// ART/disease stage outputs
PostOutputArray AdultsUnder200(116);
PostOutputArray Adults200to349(116);
PostOutputArray Adults350to499(116);
PostOutputArray AdultsOver500(116);
PostOutputArray OnARTcurrUnder200(116);
PostOutputArray OnARTcurr200to349(116);
PostOutputArray OnARTcurr350to499(116);
PostOutputArray OnARTcurrOver500(116);
PostOutputArray StartingART0(116);
PostOutputArray StartingART1(116);
PostOutputArray StartingART2to4(116);
PostOutputArray StartingART5to14(116);
PostOutputArray StartingART6to9(116);
PostOutputArray StartingART10to14(116);
PostOutputArray StartingART15to24M(116);
PostOutputArray StartingART25to34M(116);
PostOutputArray StartingART35to44M(116);
PostOutputArray StartingART45M(116);
PostOutputArray StartingART15to24F(116);
PostOutputArray StartingART25to34F(116);
PostOutputArray StartingART35to44F(116);
PostOutputArray StartingART45F(116);
PostOutputArray StartingARTtot(116);
PostOutputArray StartingART0to14(116);
PostOutputArray StartingART_M15(116);
PostOutputArray StartingART_F15(116);
PostOutputArray NewARTunder200(116);
PostOutputArray NewART200to349(116);
PostOutputArray NewART350to499(116);
PostOutputArray NewARTover500(116);
PostOutputArray TotalOnART(116);
PostOutputArray TotalART15F(116);
PostOutputArray TotalART15M(116);
PostOutputArray TotalARTunder15(116);
PostOutputArray TotalART1to2(116);
PostOutputArray TotalART3to5(116);
PostOutputArray TotalART6to9(116);
PostOutputArray TotalART10to14(116);
PostOutputArray PaedARTpropn0to4(116);
PostOutputArray PaedARTpropn5to9(116);
PostOutputArray TotUnmet15F(31); // Based on guidelines in SA at start of 2014
PostOutputArray TotUnmet15M(31);
PostOutputArray TotUnmetUnder15(31);
PostOutputArray TotNewNeed15F(31);
PostOutputArray TotNewNeed15M(31);
PostOutputArray VLsuppressed(116);
PostOutputArray VLsuppressed15(116);
PostOutputArray VLsuppressed15total(116);
PostOutputArray VLunsuppressed15total(116);
PostOutputArray VLunsuppressed15(116);
PostOutputArray VLsuppressedM(116);
PostOutputArray VLsuppressedF(116);
PostOutputArray VLsuppressedU15(116);
PostOutputArray VLsuppressed1000(116);
PostOutputArray VLsuppressed1000M(116);
PostOutputArray VLsuppressed1000F(116);
PostOutputArray VLsuppressed1000P(116);
PostOutputArray ARTerror(1);
PostOutputArray CumARTtot(116);
PostOutputArray CumART15F(116);
PostOutputArray CumART15M(116);
PostOutputArray CumARTunder15(116);
PostOutputArray PreARTunder200M(116);
PostOutputArray PreART200to349M(116);
PostOutputArray PreART350to499M(116);
PostOutputArray PreARTover500M(116);
PostOutputArray PreARTunder200F(116);
PostOutputArray PreART200to349F(116);
PostOutputArray PreART350to499F(116);
PostOutputArray PreARTover500F(116);
PostOutputArray DiscontinuedART_M(116);
PostOutputArray DiscontinuedART_F(116);
PostOutputArray TotNewNeed500M(116);
PostOutputArray TotNewNeed500F(116);
PostOutputArray ARTcoverage(116);
PostOutputArray ARTcoverage15M(116);
PostOutputArray ARTcoverage15F(116);
PostOutputArray ARTcoverageAdult(116);
PostOutputArray ARTcoverageU15(116);
PostOutputArray ARTcoverageFSW(116);
PostOutputArray ARTcoverageMSM(116);
PostOutputArray AdultRootM(116);
PostOutputArray AdultRootF(116);
PostOutputArray ChildRoot(116);
PostOutputArray EnrolmentRatio(116);
PostOutputArray EnrolmentRatio15M(116);
PostOutputArray EnrolmentRatio15F(116);
PostOutputArray EnrolmentRatioU15(116);
PostOutputArray ARTcoverageDiag(116);
PostOutputArray ARTcoverageDiag15(116);
PostOutputArray ARTcoverageDiagM(116);
PostOutputArray ARTcoverageDiagF(116);
PostOutputArray ARTcoverageDiagU15(116);
PostOutputArray VLsuppressedAllHIV(116);
PostOutputArray VLsuppressedAllM(116);
PostOutputArray VLsuppressedAllF(116);
PostOutputArray VLsuppressedAllU15(116);
PostOutputArray VLsuppressedAll1000(116);
PostOutputArray VLsuppressedAllM1000(116);
PostOutputArray VLsuppressedAllF1000(116);
PostOutputArray VLsuppressedAllP1000(116);
PostOutputArray AdultARTinterrupters(116);
PostOutputArray AdultInterruptPropn(116);
PostOutputArray ChildARTinterrupters(116);
PostOutputArray ChildInterruptPropn(116);
PostOutputArray ARTresumptionRateM(116);
PostOutputArray ARTresumptionRateF(116);
PostOutputArray TotalART15F2L(116);
PostOutputArray TotalART15M2L(116);
PostOutputArray TotalARTunder15_2L(116);
PostOutputArray AgeDbnOnART_M(70);
PostOutputArray AgeDbnOnART_F(70);

// Other prevention/cascade indicators
PostOutputArray TotBirthsHIV(116);
PostOutputArray TotBirthsART(116);
PostOutputArray TotBirthsARTconcep(116);
PostOutputArray FSWcondomUse(116);
PostOutputArray CondomUse15to24F(116);
PostOutputArray CondomUse25to49F(116);
PostOutputArray PrevTested05(20);
PostOutputArray PrevTested08(20);
PostOutputArray PrevTested12(20);
PostOutputArray PrevTested17(20);
PostOutputArray PrevTested16(16);
PostOutputArray PrevTested09(16);
PostOutputArray MMC10to14(116);
PostOutputArray MMC15to19(116);
PostOutputArray MMC20to24(116);
PostOutputArray MMC25to49(116);
PostOutputArray MMCover50(116);
PostOutputArray Circumcised15to49(116);
PostOutputArray Circumcised15to24(116);
PostOutputArray Circumcised15plus(116);
PostOutputArray NeonatalMMCops(116);
PostOutputArray MMCprob10to14(116);
PostOutputArray AdultsEverTestedM(116);
PostOutputArray AdultsEverTestedF(116);
PostOutputArray AdultsEverTested(116);
//PostOutputArray TestingBias(2);
PostOutputArray TotalHIVtests(116); // Adults
PostOutputArray TotalHIVtestsU15(116);
PostOutputArray TotalHIVtests15to24M(116);
PostOutputArray TotalHIVtests15to24F(116);
PostOutputArray TotalHIVtests25to49M(116);
PostOutputArray TotalHIVtests25to49F(116);
PostOutputArray TotalHIVtests50plusM(116);
PostOutputArray TotalHIVtests50plusF(116);
PostOutputArray HIVtestsPos(116);
PostOutputArray HIVtestsPosU15(116);
PostOutputArray HIVtestsPos18mo(116);
PostOutputArray HIVtestsPos19to59mo(116);
PostOutputArray HIVtestsPos5to14(116);
PostOutputArray FalseNegPropn(116);
PostOutputArray AdultHIVtestsPos(116);
PostOutputArray AdultHIVtestsNeg(116);
PostOutputArray PaedsHIVtestsPos(116);
PostOutputArray PaedsHIVtestsNeg(116);
PostOutputArray FirstHIVtestsPos(116);
PostOutputArray Number1stHIVtestsPos(116);
PostOutputArray Prop1stHIVtestsPos(116);
PostOutputArray TotSTestANC(116);
PostOutputArray TotSTestIndex(116);
PostOutputArray TotSTestTaxi(116);
PostOutputArray TotSTestFixedPoint(116);
PostOutputArray TotSTestWork1(116);
PostOutputArray TotSTestWork2(116);
PostOutputArray PosSTestANC(116);
PostOutputArray PosSTestIndex(116);
PostOutputArray PosSTestTaxi(116);
PostOutputArray PosSTestFixedPoint(116);
PostOutputArray PosSTestWork1(116);
PostOutputArray PosSTestWork2(116);
PostOutputArray STtoART_ANC(116);
PostOutputArray STtoARTindex(116);
PostOutputArray STtoARTtaxi(116);
PostOutputArray STtoARTfixedPoint(116);
PostOutputArray STtoARTwork1(116);
PostOutputArray STtoARTwork2(116);
PostOutputArray STuptakeByYr(18);
PostOutputArray HIVtestUptakeF25(116);
PostOutputArray OItestingRate(116);
PostOutputArray RelativeTestingVirgins(116);
PostOutputArray ProbTestedNextYr(41);
PostOutputArray DiagnosedHIVtot(116);
PostOutputArray DiagnosedHIV_M(116);
PostOutputArray DiagnosedHIV_F(116);
PostOutputArray DiagnosedHIV_U15(116);
PostOutputArray DiagnosedHIV_FSW(116);
PostOutputArray DiagnosedHIV_MSM(116);
PostOutputArray UndiagnosedHIV_M(116);
PostOutputArray UndiagnosedHIV_F(116);
PostOutputArray UndiagnosedHIV_U15(116);
PostOutputArray DiagnosedPropnM(116);
PostOutputArray DiagnosedPropnF(116);
PostOutputArray DiagnosedPropnAdult(116);
PostOutputArray DiagnosedPropnU15(116);
PostOutputArray DiagnosedPropn(116);
PostOutputArray Undiagnosed2012(20); // By sex and age
PostOutputArray DiagnosedUntreated2012(20);
PostOutputArray Treated2012(20);
PostOutputArray UntreatedByCD4_2012(8); // By diagnosis and CD4 stage
PostOutputArray PaedCascade2018(45); // By diagnosis and CD4 stage
PostOutputArray OutRRdiagDeathsPIP(2);
PostOutputArray MenOnPrEP(116);
PostOutputArray WomenOnPrEP(116);
PostOutputArray FSWonPrEP(116);
PostOutputArray MSMonPrEP(116);
PostOutputArray AGYWonPrEP(116);
PostOutputArray NewPrEP_M(116);
PostOutputArray NewPrEP_F(116);
PostOutputArray NewPrEPrateFSW(116);
PostOutputArray PrEPcoverageFSW(116);
PostOutputArray PrEPcoverageMSM(116);
PostOutputArray PrEPcoverageAGYW(116);
PostOutputArray PrEPcoverageAllM(116);
PostOutputArray PrEPcoverageAllF(116);
PostOutputArray PrEPcoverageAll(116);
PostOutputArray WomenOnVM(116);
PostOutputArray AdolescOnPrEP(116);
PostOutputArray NewAIDSdiagTrend(5);
PostOutputArray NewAIDSdiagAge(20);
PostOutputArray MSMpropn18to24(116);
PostOutputArray MultPartners15to24M(116);
PostOutputArray MultPartners15to24F(116);
PostOutputArray MultPartners25to49M(116);
PostOutputArray MultPartners25to49F(116);

// HIV-negative populations
PostOutputArray TotNegPop(116);
PostOutputArray NegChildrenU15(116);
PostOutputArray Neg15to49(116);
PostOutputArray Neg15to49M(116);
PostOutputArray Neg15to49F(116);
PostOutputArray Neg15to24(116);
PostOutputArray Neg15to24F(116);
PostOutputArray Neg15to24M(116);
PostOutputArray Neg25to49F(116);
PostOutputArray Neg25to49M(116);
PostOutputArray Neg50(116);
PostOutputArray Neg50M(116);
PostOutputArray Neg50F(116);
PostOutputArray NegFSW(116);
PostOutputArray NegMSM(116);
PostOutputArray NegClients(41);

// New infection total
PostOutputArray NewHIVU15(116);
PostOutputArray NewHIV15M(116);
PostOutputArray NewHIV15F(116);
PostOutputArray NewHIV15to49(116);
PostOutputArray NewHIV15to49M(116);
PostOutputArray NewHIV15to49F(116);
PostOutputArray NewHIV15to24(116);
PostOutputArray NewHIV15to24F(116);
PostOutputArray NewHIV15to24M(116);
PostOutputArray NewHIV25to49(116);
PostOutputArray NewHIV25to49F(116);
PostOutputArray NewHIV25to49M(116);
PostOutputArray NewHIV50(116);
PostOutputArray NewHIV50M(116);
PostOutputArray NewHIV50F(116);

// Other outputs
PostOutputArray MarriedM17to49(116);
PostOutputArray MarriedF17to49(116);
PostOutputArray MarriedM50(116);
PostOutputArray MarriedF50(116);
PostOutputArray NewARTunder200F(116);
PostOutputArray NewART200to349F(116);
PostOutputArray NewART350to499F(116);
PostOutputArray NewARTover500F(116);
PostOutputArray StartingART1to2(116);
PostOutputArray StartingART3to5(116);
PostOutputArray StartingART6to13(116);
PostOutputArray TotLateUnder15(116);
PostOutputArray TotEarlyInfants(116);
PostOutputArray TotEarly1to4(116);
PostOutputArray TotSexActs(116);
PostOutputArray TotProtSexActs(116);
PostOutputArray TotProtSexActs18(116);
PostOutputArray BirthsDiagHIV(116);
PostOutputArray BirthsOver500(116);
PostOutputArray Births350to499(116);
PostOutputArray Births200to349(116);
PostOutputArray BirthsUnder200(116);
PostOutputArray TotSexWorkers(116);
PostOutputArray SWsexActs(116);
PostOutputArray SWsexActsProt(116);
PostOutputArray FSWonART(116);
PostOutputArray DiscordantARTelig(116);
PostOutputArray DiscordantPrEPelig(116);
PostOutputArray OnARTover500(116);
PostOutputArray OnART350to499(116);
PostOutputArray OnART200to349(116);
PostOutputArray OnARTunder200(116);
PostOutputArray DiscARTover500(116);
PostOutputArray DiscART350to499(116);
PostOutputArray DiscART200to349(116);
PostOutputArray DiscARTunder200(116);
PostOutputArray AdolRegTests(116);
PostOutputArray ChildrenOnExtNVP(116);
PostOutputArray TotBirthDiagnosed(116);

// Age-specific output tables
OutputByAge MalePopAS(91, 116);
OutputByAge FemPopAS(91, 116);
OutputByAge MaleIncAS(81, 116);
OutputByAge FemIncAS(81, 116);
OutputByAge MalePrevAS(91, 116);
OutputByAge FemPrevAS(91, 116);
OutputByAge MaleMortAS(91, 116);
OutputByAge FemMortAS(91, 116);
OutputByAge MaleDiagAS(91, 116);
OutputByAge FemDiagAS(91, 116);
OutputByAge MaleART_AS(91, 116);
OutputByAge FemART_AS(91, 116);
OutputByAge MaleAIDSdeathsAS(92, 116);
OutputByAge FemAIDSdeathsAS(92, 116);
