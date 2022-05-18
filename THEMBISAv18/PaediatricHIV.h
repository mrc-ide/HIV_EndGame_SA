// PaediatricHIV.h
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
int EarlyARTyr = 2010; // Year in which new WHO paediatric ART guidelines are introduced
int InterpolationStart = 2008; // Year in which interpolation of ART initiation rate begins
int UltimateYr = 2009; // Year in which ultimate ART coverage is reached
int ProjectionTerm = 31; // Note that entering term of 10 will give you results in 1994
int CurrYear;
int CurrMonth; // 0 to 11

const int FixedUncertainty = 1;
const int InclMortData = 0; // 1 = include mortality data in calculation of likelihood
const int NonVertTransm = 0; // 1 = allow for non-vertical transmission
const int FixedARTinitiation = 0; // 1 = fix the rates of ART initiation at the values generated in
								  // the uncertainty analysis (only valid if FixedUncertainty = 1)
int EarlyART = 1; // 1 = early initiation of ART, 0 = none
int MatARTchange; // year in which maternal ART guidelines change to CD4 <350
int MatARTall; // year in which maternal ART guidelines change to include all pregnant women

//=============================================================================
// Parameters in the 'Assumptions' sheet
//=============================================================================

double TransmUntreated; // transm prob at/before birth if untreated and chronically infected
double CD4transm[4]; // prob of transm at/before birth by CD4 category
double CD4propn[4]; // propn of pregnant HIV+ women in different CD4 categories
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

double RRmortART1; // relative rate of mortality on ART during the initial high risk phase
double RRmortART2; // relative rate of mortality on ART during the subsequent low risk phase
double RRexcessEarlyART; // redn factor applied to excess mortality in newborn if ART initiated early
double Discontinuation1; // rate of ART discontinuation during the initial high risk phase
double Discontinuation2; // rate of ART discontinuation during the subsequent low risk phase
double PCRuptake; // % of infants receiving PCR testing at 6 weeks
double ARTcoverage; // ultimate propn of ART-eligible children who access ART

double IMRshape; // shape parameter for non-AIDS mortality in 1st year of life
double SexRatio; // Propn of births that are male

double Sensitivity; // sensitivity of HIV screening algorithm
double NVPefficacy; // redn in transm prob at/before birth if mother receives sd NVP
double DualEfficacy; // redn in transm prob at/before birth if mother receives sd NVP + AZT from 28 wks
double AZTefficacy; // redn in transm prob at/before birth if mother receives AZT from 28 wks
double TransmHAART; // transm prob at/before birth if mother receives HAART
double NVPuptake; // prob that a woman testing positive receives sd NVP
double AZTpropnAdj; // prob that a woman testing positive but not getting sd NVP receives AZT
double RescreenWk; // mean gestation (weeks) at time of retesting for HIV
double RescreenUptake; // % of women testing negative on initial screen who agree to rescreening
double UntestedRescreen; // % of women who missed initial screen who agree to screening
double RednExtNVP; // % redn in rate of MTCT after birth if child receives extended NVP
double RednHAART; // % redn in rate of MTCT after birth if mother receives HAART

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

double NoBFmortAdj[24]; // Factors by which mort rates increase in non-breastfed kids in 1st 12m
double BFbiasAdj; // exponential adjustment to NoBFmortAdj to allow for bias

double NonVertIncidence2004; // HIV incidence in children aged 2-14 in 2004, not due to MTCT
double NonVertAgeAdj[15]; // Multiple applied to average incidence rate to represent age effect
double EligibleHealthSeeking; // Multiple by which rate of health seeking is increased in children
							  // who are untreated but ART eligible

//============================================================================
// Parameters and arrays in the 'ASSA input' sheet
//============================================================================

double BirthsHIVnegMothers[41]; // births to HIV-negative mothers, by year
double BirthsHIVposMothers[41]; // births to HIV-positive mothers, by year
double HIVincidence[41]; // annual HIV incidence rate in pregnant women, by year
double MatIncidence; // annual HIV incidence rate in pregnant women in current year
double NonAIDSmortM[15][41]; // non-AIDS mortality rates in males, by age and by year
double NonAIDSmortF[15][41]; // non-AIDS mortality rates in females, by age and by year
double PregnantWomenTested[41]; // propn of pregnant women who receive HIV testing, by year
double VCTuptake; // propn of pregnant women who receive HIV testing in current year
double AZTrollout[41]; // propn of women receiving sd NVP who also get AZT, by year
double AZTpropn; // propn of women receiving sd NVP who also get AZT in current year
double MatARTrollout[41]; // propn of newly eligible women starting ART
double MatARTuptake[41]; // propn of newly ART-eligible women initiating ART prior to delivery
double MatARTpropn; // propn of ART-eligible women starting ART prior to delivery in current yr
double RescreenPropnLate[41]; // propn of clinics that offer rescreening in late pregnancy, by year
double RescreenLate; // propn of clinics that offer rescreening in late pregnancy in current year
double RescreenPropnImm[41]; // propn of women who get rescreened at 6-wk immunization, by yr
double RescreenImm; // propn of women who get rescreened at immunization in current year
double ExtNVProllout[41]; // propn of women knowing they are HIV+ whose infants receive extended NVP
double ExtNVPpropn; // propn of women whose infants receive extended NVP, in current yr
double NumStartingART[41]; // number of children starting ART, by year
double StartingART; // number of children starting ART in current year
double NonVertYearAdj[41]; // adjustment factor applied to non-vertical HIV incidence rate
double NonVertCurrYear; // adjustment factor applied to non-vertical HIV incidence rate
double FFphaseOut[41]; // propn redn in practice of replacement feeding
double FFredn;

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

double NTNRP; // negative at 1st visit, tested negative, retested positive
double NTNRN; // tested negative, became infected, retested negative or not retested
double NUTLP; // untested at 1st visit, became infected, later tested positive
double NUTLN; // untested at 1st visit, became infected, later tested negative or
			  // remained untested
double NRN; // negative at 1st visit, remained negative

double TestedPosCD4[4]; // tested positive prior to delivery, not on HAART, by CD4
double TestedPosOnART; // tested positive prior to delivery, initiated HAART
double UndiagnosedPos; // positive at 1st antenatal visit but never diagnosed
double RecentPosDet; // recent positive infection, detected
double RecentPosUndet; // recent positive infection, undetected

double ChildPosNoPMTCT; // cell O4
double ChildPosPMTCT; // cell O5
double ChildNegMotherPosKnown; // cell O6
double ChildNegMotherPosUnknown; // cell O7
double ChildNegMotherNeg; // cell O8

//=========================================================================
// Parameters and arrays in the 'Age-specific calcs' sheet
//=========================================================================

double StartingPop[15][2]; // size of population in 1985, by age and by sex
double PropnBF[37][3]; // propn of mothers practising breastfeeding strategy, by age of child
double RateOfBFchange[36][3]; // rate at which mothers change breastfeeding strategy
double QuadraticA[180]; // 1st term in quadratic for ART initiation rate
double QuadraticB[180]; // 2nd term in quadratic for ART initiation rate
double ARTinitiation[180]; // Monthly cts rate at which ART is initiated
double ARTinitiationStore; // Stored rate of ART initiation from last year for which data available
double ProgToNeedNoPMTCT[180]; // prob of progression to ART need if infected at birth, no PMTCT
double ProgToNeedPMTCT[180]; // prob of progression to ART need if infected at birth, failing PMTCT
double AIDSmortNoART[180]; // prob of AIDS mortality if untreated and ART-eligible
double AIDSmortEarlyART[180]; // prob of AIDS mortality if initiated ART early
double NegMatExit[36][3]; // rates at which HIV-negative mothers leave the breastfeeding state
double AcuteMatExit[36][4]; // rates at which acutely-infected mothers leave the BF state
double ChronicMatExit[36][3]; // rates at which chronicly-infected mothers leave the BF state
double KnownMatMFexit[36][3]; // rates at which women of known HIV+ status leave the BF state
double KnownMatEBFexit[36][4]; // rates at which women of known HIV+ status leave the EBF state
double ARTmatMFexit[36][3]; // rates at which women on ART leave the BF state
double ARTmatEBFexit[36][4]; // rates at which women on ART leave the BF state
double ARTeligibleExit[180][3]; // rates at which untreated children leave the ART-eligible state
double EarlyARTexit[180][3]; // rates at which treated children leave the early ART state
double ARTexitHR[180][4]; // rates at which treated children leave the high risk ART state
double ARTexitLR[180][3]; // rates at which treated children leave the low risk ART state

//=========================================================================
// Arrays in the 'Intermediate' sheet
//=========================================================================

double AliveMonthly[15][13]; // # children alive by age and by month
double EligibleMonthly[15][13]; // # children eligible for ART & ART-naive, by age and month
double NonAIDSdeathsMonthly[15][12];
double AIDSdeathsMonthly[15][12];
double AIDSdeathsUntreatedMonthly[15][12];
double DeathsMonthly[15][12];
double StartingARTmonthly[15][12];
double NewHIVatBirthMonthly[12];
double NewHIVafterBirthMonthly[12];

double AliveTotal[15]; // # children alive by age, summed across 12 months
double EligibleTotal[15]; // # children ART eligible, summed across 12 months
double NonAIDSdeathsTotal[15];
double AIDSdeathsTotal[15];
double AIDSdeathsUntreatedTotal[15];
double DeathsTotal[15];
double StartingARTtotal[15];
double NewHIVatBirthTotal, NewHIVafterBirthTotal;
double CentralMort[15];
double NotNeedingTotal[15];
double ARTeligibleTotal[15];
double OnARTtotal[15];
double OffARTtotal[15];

double ChildrenAlive[15][2]; // numbers at start of year by age and sex
double ChildrenPositive[15][2];

double NewTestsPerformed;

//=========================================================================
// Arrays and variables used in calibration and uncertainty analysis
//=========================================================================

double ObservedPrev05[3][2]; // HIV prevalence in 2005 HSRC survey by age & sex
double ObservedPrev08[3][2];
double ObservedPrevU208; // HIV prevalence in kids aged <2 in 2008
double SEprev05[3][2]; // std error of HIV prevalence estimates in 2005 HSRC survey
double SEprev08[3][2];
double SEprevU208;
double ModelPrev05[3][2];
double ModelPrev08[3][2];
double ModelPrevU208;

double RecordedDeaths[5][10]; // Stats SA recorded deaths by age group and by year (1997-2006)
double ModelDeaths[5][10]; // Model estimates of total deaths
double RelCompleteness[5]; // Relative completeness of reported deaths by age
double ConstantLogitq; // constant parameter in relational logit system for qx
double SlopeLogitq; // slope parameter in relational logit system for qx
double AveAnnMortRedn; // average annual reduction in non-HIV mortality
double IncrMortRednWRTage; // increase in average annual mortality reduction per year of age

double LogLikelihood;
const int MCMCdim = 10; // Number of parameters in uncertainty analysis
double Cholesky1[MCMCdim][MCMCdim];
double Covariance[MCMCdim][MCMCdim]; // The covariance matrix for the MCMC parameters
const int InitSample = 10000;
const int ResampleSize = 3000;
int SampleID[ResampleSize];
int CurrSim;
double temp[ResampleSize][41]; // Previously local to the SampleInput function in OutputArray class

// Parameters for IMIS

const int IMISind=1; // 0 = no IMIS; 1 = IMIS
const int IMISsteps=150; // max # steps in IMIS algorithm
const int StepSample=1000; // # additional simulations at each IMIS step after the first
const int TotalSimulations=159000; // Must be = InitSample + StepSample * (IMISsteps - 1)
int CurrIMISstep; // indicates current position in IMIS algorithm
double LogLxWeight[TotalSimulations]; // Log of (likelihood * importance ratio)
double weights[TotalSimulations]; // Sample weights calculated from LogLxWeight
double RandomParameterIMIS[MCMCdim+1][TotalSimulations]; // The '+1' is to record logL
int PriorTypes[MCMCdim]; // Indicators for prior types (0 = beta, 1 =gamma)
double aParameters[MCMCdim]; // Alpha parameters for priors
double bParameters[MCMCdim]; // Beta parameters for priors
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

//===========================================================================
// Classes
//===========================================================================

class GenderGroup
{
	public:

	int Sex; // 0 = male, 1 = female
	double PropnBirths;
	double NonAIDSmort[180];
	double NewNonVertCurrY;

	// Arrays to represent population profile at START of month
	double NegMatMF[180];
	double NegMatEBF[180];
	double AcuteMatMF[180];
	double AcuteMatEBF[180];
	double UnawareMatMF[180];
	double UnawareMatEBF[180];
	double AwareMatMF[180];
	double AwareMatEBF[180];
	double ARTmatMF[180];
	double ARTmatEBF[180];
	double NegChildFF[180];
	double PosChildAtBirthNoPMTCT[180];
	double PosChildAtBirthPMTCT[180];
	double PosChildAfterBirth[180];
	double ARTeligible[180];
	double OnARTearly[180];
	double OnARTlate1st3m[180];
	double OnARTlateAfter3m[180];
	double StoppedART[180];
	double Total[180];

	// Arrays to represent population profile at END of month
	double NegMatMF_E[180];
	double NegMatEBF_E[180];
	double AcuteMatMF_E[180];
	double AcuteMatEBF_E[180];
	double UnawareMatMF_E[180];
	double UnawareMatEBF_E[180];
	double AwareMatMF_E[180];
	double AwareMatEBF_E[180];
	double ARTmatMF_E[180];
	double ARTmatEBF_E[180];
	double NegChildFF_E[180];
	double PosChildAtBirthNoPMTCT_E[180];
	double PosChildAtBirthPMTCT_E[180];
	double PosChildAfterBirth_E[180];
	double ARTeligible_E[180];
	double OnARTearly_E[180];
	double OnARTlate1st3m_E[180];
	double OnARTlateAfter3m_E[180];
	double StoppedART_E[180];
	double Total_E[180];

	// Arrays to represent movements in the course of the month
	double AIDSdeaths[180];
	double AIDSdeathsUntreated[180];
	double NonAIDSdeaths[180];
	double NewHIVpostnatal[180];
	double StartingART[180];
	double NewMatHIV[180];
	double NewMatUnaware[180];
	double NewMatAwareMF[180];
	double NewMatARTMF[180];
	double NewEligible[180];
	double StartingARTno6wkScreen[180];
	double StabilizingOnART[180];
	double StoppingART[180];

	GenderGroup(int Gender);
	void GetStartYrProfile();
	void GetNonAIDSmort();
	void GetEndProfile1();
	void GetEndProfile2();
	void UpdateStartProfile();
};

class OutputArray
{
	public:
	OutputArray(int n);

	int columns;
	double out[InitSample][25]; // None of the arrays require > 20 columns. 

	void Record(char* filout, int n);
	void RecordSample(char* filout, int n);
	void SampleInput();
};

class PostOutputArray
{
	// Same as OutputArray class except that we only use this to record outputs from
	// the posterior distribution (and hence require smaller output array).
	
	public:
	PostOutputArray(int n);

	int columns;
	double out[ResampleSize][41]; // None of the arrays require > 41 columns. 

	void RecordSample(char* filout);
};

//============================================================================
// General functions
//============================================================================

void ReadAssumptions();
void ReadASSAinputs();
void ReadPhaseIn();
void ReadHIVprevData();
void ReadMortData();
void ReadARTinitiation();

void SetInitialParameters();
void SetCurrYearParameters();
void SetARTinitiationRates();
void GetCalibOutput();
void CalcOutput();
void GetMonthlyResults();
void GetAnnualResults();
void OneYear();

//============================================================================
// Functions used in uncertainty analysis
//============================================================================

// SIR

void CalcLikelihood();
void CalcMortLikelihood();
void CalcMortLikelihood2();
void GetCholesky1(double mat[MCMCdim][MCMCdim]);
void GetInverse1(double mat[MCMCdim][MCMCdim], double matinv[MCMCdim][MCMCdim]);
void SimulateParameters();
void GenerateSample();
void RunSample();

// IMIS

void ReadPriors();
void GetMultNorm(double RandUnif[MCMCdim], double MultNorm[MCMCdim]); 
double GetMultNormPDF(double MultNorm[MCMCdim], int Component);
void GetMahalanobis(double distance[TotalSimulations]);
double GetPercentile(double values[TotalSimulations], double percentile);
void runIMIS();
void OneIMISstep();
void SimulateParameters_IMIS();

//============================================================================
// Objects created from defined classes
//============================================================================

GenderGroup Male(0);
GenderGroup Female(1);

OutputArray RandomUniform(10);
OutputArray PaedParameters(10);
OutputArray LogL(1);
OutputArray PrevLogL(1);

PostOutputArray Prev0to14(31);
PostOutputArray Prev0to1(31);
PostOutputArray Prev2to4M(31);
PostOutputArray Prev2to4F(31);
PostOutputArray Prev5to9M(31);
PostOutputArray Prev5to9F(31);
PostOutputArray Prev10to14M(31);
PostOutputArray Prev10to14F(31);
PostOutputArray Prev2004(8);
PostOutputArray HIVageProfile2000(15);
PostOutputArray HIVageProfile2010(15);
PostOutputArray UntreatedAgeProfile(15);
PostOutputArray OnARTageProfile(15);
PostOutputArray NotNeedingAgeProfile(15);
PostOutputArray TreatedMortAgeProfile(15);
PostOutputArray UntreatedMortAgeProfile(15);
PostOutputArray TotalHIV(31);
PostOutputArray TestsPerformed(31);
PostOutputArray NewOnART(36);
PostOutputArray TotalARTinitiation(31);
PostOutputArray StartingARTunder12(36);
PostOutputArray StartingART12to23(36);
PostOutputArray StartingART24to59(36);
PostOutputArray NewHIVatBirth(37);
PostOutputArray NewHIVafterBirth(37);
PostOutputArray NewNonVertInc(31);
PostOutputArray AIDSdeaths(41);
PostOutputArray AIDSdeathsM1(31);
PostOutputArray AIDSdeathsYr1(31);
PostOutputArray AIDSdeaths1to4(31);
PostOutputArray AIDSdeaths5to9(31);
PostOutputArray AIDSdeaths10to14(31);
PostOutputArray NotNeeding(31);
PostOutputArray UntreatedNeed(31);
PostOutputArray TreatedNeed(31);
PostOutputArray MeanAgeNeed(41);
PostOutputArray DiscontinuedART(31);
PostOutputArray VertTransm(31);
PostOutputArray VertTransmKnownPos(31);
PostOutputArray VertTransmGotART(31);
PostOutputArray IMR(31);
PostOutputArray U5MR(31);
PostOutputArray Deaths0to4(31);
PostOutputArray DeathsM1(31);
PostOutputArray DeathsYr1(31);
PostOutputArray Deaths1to4(31);
PostOutputArray Deaths5to9(31);
PostOutputArray Deaths10to14(31);
PostOutputArray Deaths0to14(37);
PostOutputArray AgeDbnIMR(36);
PostOutputArray ARTneed1(31);
PostOutputArray ARTneed1to4(31);
PostOutputArray TotHIV1(31);
PostOutputArray TotHIV1to4(31);
PostOutputArray TotPopU5(31);
