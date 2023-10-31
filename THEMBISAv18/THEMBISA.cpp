// This is the main project file for VC++ application project
// generated using an Application Wizard.

// #include "stdafx.h"
// #using <mscorlib.dll>
// using namespace System;

#include "THEMBISA.h"
#include "StatFunctions.h"
#include "randomc.h"
#include <time.h>
#include <cstdlib>
//#include <omp.h>

int main()
{
	int iy;
	clock_t start, finish;
	double elapsed_time;

	start = clock();
	//GenerateSample(); // Not yet updated in THEMBISA
	RunSample();	// Remember to set FixedUncertainty = 1 before running this function
	//runIMIS(0);
	//MaximizeLikelihood(0.0000001, "InitialSimplex.txt", "FinalSimplex.txt");
	/*ReadAllFiles();
	CurrSim = 1;
	CurrYear = StartYear;
	UpdateNonAIDSmort();
	SetInitialParameters();
	SetCD4byARTdur();
	CalcInterruptions();
	SetActivityByStage();
	SetInitSexActivity();
	UpdateMixingST();
	SetCurrYearParameters();
	SetFertByStage();
	SetProgression(0);
	UpdateARTmort();
	CurrYear = 1984;
	for(iy=0; iy<ProjectionTerm; iy++){
		OneYear();
	}
	CalcLikelihood();
	for(iy=0; iy<ProjectionTerm; iy++){
		Prev15to49.out[0][iy] = HIVprev15to49[iy];
		PrevPreg15to49.out[0][iy] = PrevPregnant[6][iy];
		PrevPreg15to19.out[0][iy] = PrevPregnant[0][iy];
		PrevPreg20to24.out[0][iy] = PrevPregnant[1][iy];
		PrevPreg25to29.out[0][iy] = PrevPregnant[2][iy];
		PrevPreg30to34.out[0][iy] = PrevPregnant[3][iy];
		PrevPreg35to39.out[0][iy] = PrevPregnant[4][iy];
		//PrevFSW.out[0][iy] = FSWprev[iy];
		//FSWcondomUse.out[0][iy] = CondomUseFSW[iy];
		//FSWincidence.out[0][iy] = NewHIVinFSW[iy]/(0.5 *
		//	(NegFSW[iy] + NegFSW[iy+1]));
		//FSWincidence.out[0][iy] = NewHIVinFSW[iy];
	}
	Prev15to49.RecordSample("Prev15to49.txt");
	PrevPreg15to49.RecordSample("PrevPreg15to49.txt");
	PrevPreg15to19.RecordSample("PrevPreg15to19.txt");
	PrevPreg20to24.RecordSample("PrevPreg20to24.txt");
	PrevPreg25to29.RecordSample("PrevPreg25to29.txt");
	PrevPreg30to34.RecordSample("PrevPreg30to34.txt");
	PrevPreg35to39.RecordSample("PrevPreg35to39.txt");
	PrevFSW.RecordSample("PrevFSW.txt");
	FSWcondomUse.RecordSample("FSWcondomUse.txt");
	Prev0to14.RecordSample("Prev0to14.txt");
	FSWincidence.RecordSample("FSWincidence.txt");
	HIVinc15to49M.RecordSample("HIVinc15to49M.txt");
	PAFforCSW.RecordSample("PAFforCSW.txt");
	SaveHSRCcalib("HSRCcalib.txt");
	SaveAdolProj("AdolHIVprofile.txt");
	SaveFSWprofile("FSW_HIVprofile.txt");
	SaveHCTbyAge("HCTbyAge.txt");*/
	//std::cout<<setprecision(15)<<"MHU_ST.NegNoHCT[10]: "<<MHU_ST.NegNoHCT[10]<<std::endl;
	//CalcLikelihood();
	//std::cout<<"Log Likelihood: "<<LogLikelihood<<std::endl;
	//std::cout<<"AdultMortBy5yr[25][5][1]: "<<AdultMortBy5yr[25][5][1]<<std::endl;

	finish = clock();
	elapsed_time = (finish - start);
	std::cout<<"Time taken: "<<elapsed_time<<std::endl;
	system("PAUSE");
	return 0;
}

Child::Child(int Gender)
{
	Sex = Gender;
}

void Child::GetStartYrProfile()
{
	int im;
	double PopByAge[133];
	double x1, intpart1, DoubleIntDif1, MatPrev;
	int x2;

	// Initialize PopByAge
	for(im=0; im<133; im++){
		PopByAge[im] = 0.0;}

	MatPrev = HighRiskPropn[1] * InitFSWprev;
	for(im=0; im<133; im++){
		x1 = im/12.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0 || im==0){
			x2 = x1;
			PopByAge[im] = StartingPop[x2][Sex]/12.0;
		}
		else{
			PopByAge[im] = PopByAge[im - 1];}
		if(im<36){
			NegMatMF[im] = PopByAge[im] * PropnBF[im][0] * (1.0 - MatPrev);
			UnawareMatMF[im] = PopByAge[im] * PropnBF[im][0] * MatPrev;
			NegChildFF[im] = PopByAge[im] * (1.0 - PropnBF[im][0]);
		}
		else{
			NegChildFF[im] = PopByAge[im];}
		NegMatEBF[im] = 0.0;
		AcuteMatMF[im] = 0.0;
		AcuteMatEBF[im] = 0.0;
		UnawareMatEBF[im] = 0.0;
		AwareMatMF[im] = 0.0;
		AwareMatEBF[im] = 0.0;
		ARTmatMF[im] = 0.0;
		ARTmatEBF[im] = 0.0;
		PosChildAtBirthNoPMTCT[im] = 0.0;
		PosChildAtBirthPMTCT[im] = 0.0;
		PosChildAfterBirth[im] = 0.0;
		ARTeligible[im] = 0.0;
		DiagChildAtBirthNoPMTCT[im] = 0.0;
		DiagChildAtBirthPMTCT[im] = 0.0;
		DiagChildAfterBirth[im] = 0.0;
		DiagARTeligible[im] = 0.0;
		OnARTearly[im] = 0.0;
		OnARTlate1st3m[im] = 0.0;
		OnARTlateAfter3m[im] = 0.0;
		StoppedART[im] = 0.0;
		Total[im] = PopByAge[im];
	}
}

void Child::GetNonAIDSmort()
{
	int iy, im;
	double MortTable[15];
	double x1, intpart1, DoubleIntDif1;
	int x2;

	for(iy=0; iy<15; iy++){
		if(Sex==0){MortTable[iy] = CurrNonAIDSmortEA[iy][0];}
		else{MortTable[iy] = CurrNonAIDSmortEA[iy][1];}
	}

	for(im=0; im<12; im++){
		NonAIDSmort[im] = 1.0 - pow(1.0 - MortTable[0], pow((im + 1.0)/12.0, IMRshape) -
			pow(im/12.0, IMRshape));
	}
	for(im=12; im<132; im++){
		x1 = im/12.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			x2 = x1;
			NonAIDSmort[im] = 1.0 - pow(1 - MortTable[x2], 1.0/12.0);
		}
		else{
			NonAIDSmort[im] = NonAIDSmort[im - 1];}
	}
	for(im=0; im<24; im++){
		NonAIDSmort[im] = NonAIDSmort[im]/(PropnBF[im][0] + (1.0 - PropnBF[im][0]) *
			pow(NoBFmortAdj[im], BFbiasAdj));
	}
}

void Child::GetEndProfile()
{
	int im, iy;
	double NVI, NewNonVertCurrM; // non-vertical incidence of HIV

	iy = CurrYear - StartYear;

	// Set PropnBirths
	if(Sex==0){
		PropnBirths = SexRatio;}
	else{
		PropnBirths = 1.0 - SexRatio;}

	for(im=0; im<133; im++){

		// Calculate NegMatMF_E
		if(im==0){
			NegMatMF_E[0] = ChildNegMotherNeg * PropnBirths * PropnBF[0][0]/12.0;}
		else if(im<36){
			NegMatMF_E[im] = NegMatMF[im-1] * (1.0 - NegMatExit[im-1][0]) * (1.0 - NonAIDSmort[im-1]);}

		// Calculate NegMatEBF_E
		NegMatEBF_E[im] = 0.0;

		// Calculate NewMatHIV
		if(im<36){
			NewMatHIV[im] = NegMatMF[im] * NegMatExit[im][0] * NegMatExit[im][2];}

		// Calculate AcuteMatMF_E
		if(im>0 && im<36){
			AcuteMatMF_E[im] = (AcuteMatMF[im-1] * (1.0 - AcuteMatExit[im-1][0]) + NewMatHIV[im-1] *
				(-AcuteMatExit[im-1][0]/log(1.0 - AcuteMatExit[im-1][0]))) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate AcuteMatEBF_E
		AcuteMatEBF_E[im] = 0.0;

		// Calculate NewMatUnaware
		if(im<36){
			NewMatUnaware[im] = AcuteMatMF[im] * AcuteMatExit[im][0] * AcuteMatExit[im][1] +
				NewMatHIV[im] * (1.0 + AcuteMatExit[im][0]/log(1.0 - AcuteMatExit[im][0])) *
				AcuteMatExit[im][1];
		}

		// Calculate UnawareMatMF_E
		if(im==0){
			UnawareMatMF_E[0] = ChildNegMotherPosUnknown * PropnBirths * PropnBF[0][0]/12.0;}
		else if(im<36){
			UnawareMatMF_E[im] = (UnawareMatMF[im-1] * (1.0 - ChronicMatExit[im-1][0]) +
				NewMatUnaware[im-1] * (-ChronicMatExit[im-1][0]/log(1.0 - ChronicMatExit[im-1][0]))) *
				(1.0 - NonAIDSmort[im-1]);
		}
		if(im==2){
			UnawareMatMF_E[im] *= (1.0 - RescreenImm);}

		// Calculate UnawareMatEBF_E
		UnawareMatEBF_E[0] = 0.0;

		// Calculate AwareMatEBF_E
		if(im==0){
			AwareMatEBF_E[0] = (ChildNegMotherPosKnown - TestedPosOnART[0] * (1.0 - TransmARTpre200) -
				TestedPosOnART[1] * (1.0 - TransmART200) - ARTmothers * (1.0 - TransmARTprePreg)) *
				EverFeedCurr[2] * PropnBirths/12.0;}
		else if(im<7){
			AwareMatEBF_E[im] = AwareMatEBF[im-1] * (1.0 - KnownMatEBFexit[im-1][0]) *
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate NewMatAwareMF
		if(im<6){
			NewMatAwareMF[im] = AwareMatEBF[im] * KnownMatEBFexit[im][0] * KnownMatEBFexit[im][2];}

		// Calculate AwareMatMF_E
		if(im==0){
			AwareMatMF_E[0] = (ChildNegMotherPosKnown - TestedPosOnART[0] * (1.0 - TransmARTpre200) -
				TestedPosOnART[1] * (1.0 - TransmART200) - ARTmothers * (1.0 - TransmARTprePreg)) *
				EverFeedCurr[1] * PropnBirths/12.0;}
		else if (im<36){
			AwareMatMF_E[im] = (AwareMatMF[im-1] * (1.0 - KnownMatMFexit[im-1][0]) +
				NewMatAwareMF[im-1] * (-KnownMatMFexit[im-1][0]/log(1.0 - KnownMatMFexit[im-1][0]))) *
				(1.0 - NonAIDSmort[im-1]);
		}
		if(im==2){
			AwareMatMF_E[im] += RescreenImm * (1.0 - SwitchingToFF) * (UnawareMatMF[im-1] *
				(1.0 - ChronicMatExit[im-1][0]) + NewMatUnaware[im-1] * (-ChronicMatExit[im-1][0]/
				log(1.0 - ChronicMatExit[im-1][0]))) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate ARTmatEBF_E
		if(im==0){
			ARTmatEBF_E[0] = (TestedPosOnART[0] * (1.0 - TransmARTpre200) + TestedPosOnART[1] *
				(1.0 - TransmART200) + ARTmothers * (1.0 - TransmARTprePreg)) * EverFeedCurr[2] *
				PropnBirths/12.0;}
		else if(im<7){
			ARTmatEBF_E[im] = ARTmatEBF[im-1] * (1.0 - ARTmatEBFexit[im-1][0]) * (1.0 - NonAIDSmort[im-1]);}

		// Calculate NewMatARTMF
		if(im<6){
			NewMatARTMF[im] = ARTmatEBF[im] * ARTmatEBFexit[im][0] * ARTmatEBFexit[im][2];}

		// Calculate ARTmatMF_E
		if(im==0){
			ARTmatMF_E[0] = (TestedPosOnART[0] * (1.0 - TransmARTpre200) + TestedPosOnART[1] *
				(1.0 - TransmART200) + ARTmothers * (1.0 - TransmARTprePreg)) * EverFeedCurr[1] *
				PropnBirths/12.0;}
		else if (im<36){
			ARTmatMF_E[im] = (ARTmatMF[im-1] * (1.0 - ARTmatMFexit[im-1][0]) +
				NewMatARTMF[im-1] * (-ARTmatMFexit[im-1][0]/log(1.0 - ARTmatMFexit[im-1][0]))) *
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate NegChildFF_E
		if(im==0){
			NegChildFF_E[0] = ChildNegMotherPosKnown * (1.0 - EverFeedCurr[1] - EverFeedCurr[2]) *
				PropnBirths/12.0 + (ChildNegMotherPosUnknown + ChildNegMotherNeg) *
				(1.0 - EverFeedCurr[0]) * PropnBirths/12.0;
		}
		else if(im<=36){
			NegChildFF_E[im] = (1.0 - NonAIDSmort[im-1]) * (NegChildFF[im-1] +
				NegMatMF[im-1] * NegMatExit[im-1][0] * NegMatExit[im-1][1] +
				AcuteMatMF[im-1] * AcuteMatExit[im-1][0] * AcuteMatExit[im-1][2] +
				UnawareMatMF[im-1] * ChronicMatExit[im-1][0] * ChronicMatExit[im-1][1] +
				AwareMatMF[im-1] * KnownMatMFexit[im-1][0] * KnownMatMFexit[im-1][1] +
				AwareMatEBF[im-1] * KnownMatEBFexit[im-1][0] * KnownMatEBFexit[im-1][1] +
				ARTmatMF[im-1] * ARTmatMFexit[im-1][0] * ARTmatMFexit[im-1][1] +
				ARTmatEBF[im-1] * ARTmatEBFexit[im-1][0] * ARTmatEBFexit[im-1][1] +
				NewMatHIV[im-1] * (1.0 + AcuteMatExit[im-1][0]/log(1.0 - AcuteMatExit[im-1][0])) * AcuteMatExit[im-1][2] +
				NewMatUnaware[im-1] * (1.0 + ChronicMatExit[im-1][0]/log(1.0 - ChronicMatExit[im-1][0])) * ChronicMatExit[im-1][1] +
				NewMatAwareMF[im-1] * (1.0 + KnownMatMFexit[im-1][0]/log(1.0 - KnownMatMFexit[im-1][0])) * KnownMatMFexit[im-1][1] +
				NewMatARTMF[im-1] * (1.0 + ARTmatMFexit[im-1][0]/log(1.0 - ARTmatMFexit[im-1][0])) * ARTmatMFexit[im-1][1]);
		}
		else{
			NegChildFF_E[im] = NegChildFF[im-1] * (1.0 - NonAIDSmort[im-1]);}
		if(im>0 && im<=24){
			NegChildFF_E[im] = NegChildFF_E[im] - NegChildFF[im-1] * NonAIDSmort[im-1] *
				(pow(NoBFmortAdj[im-1], BFbiasAdj) - 1.0);
		}
		if(im==2){
			NegChildFF_E[im] += (UnawareMatMF[im-1] * (1.0 - ChronicMatExit[im-1][0]) +
				NewMatUnaware[im-1] * (-ChronicMatExit[im-1][0]/log(1.0 - ChronicMatExit[im-1][0]))) *
				RescreenImm * SwitchingToFF * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate NewHIVpostnatal
		if(im<36){
			NewHIVpostnatal[im] = ((AcuteMatMF[im] * AcuteMatExit[im][0] + NewMatHIV[im] *
				(1.0 + AcuteMatExit[im][0]/log(1.0 - AcuteMatExit[im][0]))) * AcuteMatExit[im][3] +
				(UnawareMatMF[im] * ChronicMatExit[im][0] + NewMatUnaware[im] *
				(1.0 + ChronicMatExit[im][0]/log(1.0 - ChronicMatExit[im][0]))) * ChronicMatExit[im][2] +
				(AwareMatMF[im] * KnownMatMFexit[im][0] + NewMatAwareMF[im] *
				(1.0 + KnownMatMFexit[im][0]/log(1.0 - KnownMatMFexit[im][0]))) * KnownMatMFexit[im][2] +
				(ARTmatMF[im] * ARTmatMFexit[im][0] + NewMatARTMF[im] *
				(1.0 + ARTmatMFexit[im][0]/log(1.0 - ARTmatMFexit[im][0]))) * ARTmatMFexit[im][2] +
				AwareMatEBF[im] * KnownMatEBFexit[im][0] * KnownMatEBFexit[im][3] +
				ARTmatEBF[im] * ARTmatEBFexit[im][0]* ARTmatEBFexit[im][3]) *
				pow(1.0 - NonAIDSmort[im], 0.5);
		}
		else{
			NewHIVpostnatal[im] = 0.0;}
		if (im < 18 && FixedUncertainty == 1){
			NewPostnatalDiag[im] = ((AwareMatMF[im] * KnownMatMFexit[im][0] + NewMatAwareMF[im] *
				(1.0 + KnownMatMFexit[im][0] / log(1.0 - KnownMatMFexit[im][0]))) * KnownMatMFexit[im][2] +
				(ARTmatMF[im] * ARTmatMFexit[im][0] + NewMatARTMF[im] *
				(1.0 + ARTmatMFexit[im][0] / log(1.0 - ARTmatMFexit[im][0]))) * ARTmatMFexit[im][2] +
				AwareMatEBF[im] * KnownMatEBFexit[im][0] * KnownMatEBFexit[im][3] +
				ARTmatEBF[im] * ARTmatEBFexit[im][0] * ARTmatEBFexit[im][3]) *
				pow(1.0 - NonAIDSmort[im], 0.5);
		}

		// Calculate PosChildAtBirthNoPMTCT_E
		if(im==0){
			PosChildAtBirthNoPMTCT_E[0] = ChildPosNoPMTCT * (PropnBirths/12.0) * (1.0 -
				PCRuptakeB * BirthSe[0]);}
		else{
			PosChildAtBirthNoPMTCT_E[im] = PosChildAtBirthNoPMTCT[im - 1] * (1.0 - ProgToNeedNoPMTCT[im - 1]) *
				(1.0 - MnthlyPaedTestToART[im-1][0] - MnthlyPaedDiagNoART[im-1][0]) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate PosChildAtBirthPMTCT_E
		if(im==0){
			PosChildAtBirthPMTCT_E[0] = ChildPosPMTCT * (PropnBirths / 12.0) * (1.0 -
				PCRuptakeB * BirthSe[1]);}
		else{
			PosChildAtBirthPMTCT_E[im] = PosChildAtBirthPMTCT[im - 1] * (1.0 - ProgToNeedPMTCT[im - 1]) *
				(1.0 - MnthlyPaedTestToART[im-1][1] - MnthlyPaedDiagNoART[im-1][1]) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate PosChildAfterBirth_E
		if(im==0){
			PosChildAfterBirth_E[0] = 0.0;}
		else{
			PosChildAfterBirth_E[im] = PosChildAfterBirth[im - 1] * (1.0 - ProgToNeedPostnatal[im - 1]) *
				(1.0 - MnthlyPaedTestToART[im-1][2] - MnthlyPaedDiagNoART[im-1][2]) * (1.0 - NonAIDSmort[im-1]);
			if (im < 36){
				PosChildAfterBirth_E[im] += NewHIVpostnatal[im - 1] * pow(1.0 - NonAIDSmort[im - 1], 0.5); }
		}

		// Calculate ARTeligible_E
		if (im == 0){
			ARTeligible_E[0] = 0.0;
		}
		else{
			ARTeligible_E[im] = (ARTeligible[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0]) *
				(1.0 - MnthlyTestingPaed[im - 1][1]) +
				PosChildAtBirthNoPMTCT[im - 1] * ProgToNeedNoPMTCT[im - 1] * (1.0 -
				MnthlyPaedTestToART[im - 1][0] - MnthlyPaedDiagNoART[im - 1][0]) +
				PosChildAtBirthPMTCT[im - 1] * ProgToNeedPMTCT[im - 1] * (1.0 -
				MnthlyPaedTestToART[im - 1][1] - MnthlyPaedDiagNoART[im - 1][1]) +
				PosChildAfterBirth[im - 1] * ProgToNeedPostnatal[im - 1] *
				(1.0 - MnthlyPaedTestToART[im - 1][2] -
				MnthlyPaedDiagNoART[im - 1][2])) * (1.0 - NonAIDSmort[im - 1]);
		}

		// Calculate DiagChildAtBirthNoPMTCT_E
		if (im == 0){
			DiagChildAtBirthNoPMTCT_E[0] = ChildPosNoPMTCT * (PropnBirths / 12.0) *
				PCRuptakeB * BirthSe[0] * (1.0 - EligInfants * PaedARTuptakeEID);
		}
		else{
			if (im < 13){
				DiagChildAtBirthNoPMTCT_E[im] = DiagChildAtBirthNoPMTCT[im - 1] * (1.0 -
					ProgToNeedNoPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EligInfants) * (1.0 - NonAIDSmort[im - 1]);
			}
			else if (im < 61){
				DiagChildAtBirthNoPMTCT_E[im] = DiagChildAtBirthNoPMTCT[im - 1] * (1.0 -
					ProgToNeedNoPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART1to4[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
			else{
				DiagChildAtBirthNoPMTCT_E[im] = DiagChildAtBirthNoPMTCT[im - 1] * (1.0 -
					ProgToNeedNoPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART5to14[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
			DiagChildAtBirthNoPMTCT_E[im] += PosChildAtBirthNoPMTCT[im - 1] * (1.0 - ProgToNeedNoPMTCT[im - 1]) *
				MnthlyPaedDiagNoART[im - 1][0] * (1.0 - NonAIDSmort[im - 1]);
		}

		// Calculate DiagChildAtBirthPMTCT_E
		if (im == 0){
			DiagChildAtBirthPMTCT_E[0] = ChildPosPMTCT * (PropnBirths / 12.0) *
				PCRuptakeB * BirthSe[1] * (1.0 - EligInfants * PaedARTuptakeEID);
		}
		else{
			if (im < 13){
				DiagChildAtBirthPMTCT_E[im] = DiagChildAtBirthPMTCT[im - 1] * (1.0 -
					ProgToNeedPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EligInfants) * (1.0 - NonAIDSmort[im - 1]);
			}
			else if (im < 61){
				DiagChildAtBirthPMTCT_E[im] = DiagChildAtBirthPMTCT[im - 1] * (1.0 -
					ProgToNeedPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART1to4[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
			else{
				DiagChildAtBirthPMTCT_E[im] = DiagChildAtBirthPMTCT[im - 1] * (1.0 -
					ProgToNeedPMTCT[im - 1]) * exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART5to14[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
			DiagChildAtBirthPMTCT_E[im] += PosChildAtBirthPMTCT[im - 1] * (1.0 - ProgToNeedPMTCT[im - 1]) *
				MnthlyPaedDiagNoART[im - 1][1] * (1.0 - NonAIDSmort[im - 1]);
		}

		// Calculate DiagChildAfterBirth_E
		if (im == 0){ DiagChildAfterBirth_E[0] = 0.0;}
		else{
			if (im < 13){
				DiagChildAfterBirth_E[im] = DiagChildAfterBirth[im - 1] * exp(-ARTinitiation[im - 1] *
					RRearlyPaedART * EligInfants) * (1.0 - ProgToNeedPostnatal[im - 1]) *
					(1.0 - NonAIDSmort[im - 1]);
			}
			else if (im < 61){
				DiagChildAfterBirth_E[im] = DiagChildAfterBirth[im - 1] * exp(-ARTinitiation[im - 1] *
					RRearlyPaedART * EarlyART1to4[iy]) * (1.0 - ProgToNeedPostnatal[im - 1]) *
					(1.0 - NonAIDSmort[im - 1]);
			}
			else{
				DiagChildAfterBirth_E[im] = DiagChildAfterBirth[im - 1] * exp(-ARTinitiation[im - 1] *
					RRearlyPaedART * EarlyART5to14[iy]) * (1.0 - ProgToNeedPostnatal[im - 1]) *
					(1.0 - NonAIDSmort[im - 1]);
			}
			DiagChildAfterBirth_E[im] += PosChildAfterBirth[im - 1] * MnthlyPaedDiagNoART[im - 1][2] *
				(1.0 - ProgToNeedPostnatal[im - 1]) * (1.0 - NonAIDSmort[im - 1]);
		}

		// Calculate DiagARTeligible_E
		if (im == 0){ DiagARTeligible_E[0] = 0.0; }
		else{
			DiagARTeligible_E[im] = (DiagARTeligible[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0]) * exp(-ARTinitiation[im - 1]) +
				PosChildAtBirthNoPMTCT[im - 1] * ProgToNeedNoPMTCT[im - 1] * MnthlyPaedDiagNoART[im - 1][0] +
				PosChildAtBirthPMTCT[im - 1] * ProgToNeedPMTCT[im - 1] * MnthlyPaedDiagNoART[im - 1][1] +
				PosChildAfterBirth[im - 1] * MnthlyPaedDiagNoART[im - 1][2] * ProgToNeedPostnatal[im - 1] +
				ARTeligible[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0]) *
				MnthlyPaedDiagNoART[im - 1][3]) * (1.0 - NonAIDSmort[im - 1]);
			if (im < 13){
				DiagARTeligible_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] * ProgToNeedNoPMTCT[im - 1] +
					DiagChildAtBirthPMTCT[im - 1] * ProgToNeedPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1] * ProgToNeedPostnatal[im - 1]) *
					exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EligInfants) * (1.0 - NonAIDSmort[im - 1]);
			}
			else if (im < 61){
				DiagARTeligible_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] * ProgToNeedNoPMTCT[im - 1] +
					DiagChildAtBirthPMTCT[im - 1] * ProgToNeedPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1] * ProgToNeedPostnatal[im - 1]) *
					exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART1to4[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
			else{
				DiagARTeligible_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] * ProgToNeedNoPMTCT[im - 1] +
					DiagChildAtBirthPMTCT[im - 1] * ProgToNeedPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1] * ProgToNeedPostnatal[im - 1]) *
					exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART5to14[iy]) * (1.0 - NonAIDSmort[im - 1]);
			}
		}

		// Calculate OnARTearly_E
		if (im == 0){
			OnARTearly_E[im] = (ChildPosNoPMTCT * BirthSe[0] + ChildPosPMTCT * BirthSe[1]) *
				(PropnBirths / 12.0) * EligInfants * PCRuptakeB * PaedARTuptakeEID;
		}
		else{
			OnARTearly_E[im] = (OnARTearly[im - 1] * (1.0 - AIDSmortEarlyART[im-1]) * exp(-Discontinuation1 / 12.0) +
				PosChildAtBirthNoPMTCT[im - 1] * MnthlyPaedTestToART[im - 1][0] +
				PosChildAtBirthPMTCT[im - 1] * MnthlyPaedTestToART[im - 1][1] +
				PosChildAfterBirth[im - 1] * MnthlyPaedTestToART[im - 1][2]) * (1.0 - NonAIDSmort[im - 1]);
			if (im < 13){
				OnARTearly_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] + DiagChildAtBirthPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1]) * (1.0 - exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EligInfants)) * (1.0 - NonAIDSmort[im - 1]);
			}
			else if (im < 61){
				OnARTearly_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] + DiagChildAtBirthPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1]) * (1.0 - exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART1to4[iy])) * (1.0 - NonAIDSmort[im - 1]);
			}
			else{
				OnARTearly_E[im] += (DiagChildAtBirthNoPMTCT[im - 1] + DiagChildAtBirthPMTCT[im - 1] +
					DiagChildAfterBirth[im - 1]) * (1.0 - exp(-ARTinitiation[im - 1] * RRearlyPaedART *
					EarlyART5to14[iy])) * (1.0 - NonAIDSmort[im - 1]);
			}
		}

		// Calculate OnARTlate1st3m_E
		if (im == 0){ OnARTlate1st3m_E[im] = 0.0; }
		else{
			OnARTlate1st3m_E[im] = (OnARTlate1st3m[im-1] * (1.0 - ARTexitHR[im-1][0]) +
				ARTeligible[im - 1] * MnthlyPaedTestToART[im - 1][3] * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0]) +
				DiagARTeligible[im - 1] * (1.0 - exp(-ARTinitiation[im - 1])) * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0])) *
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate OnARTlateAfter3m_E
		if (im == 0){ OnARTlateAfter3m_E[im] = 0.0; }
		else{
			OnARTlateAfter3m_E[im] = (OnARTlateAfter3m[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], RRmortART2 * MortAdjLate[2]) *
				exp(-Discontinuation2 / 12.0) + OnARTlate1st3m[im - 1] * ARTexitHR[im - 1][0] * ARTexitHR[im - 1][3]) *
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate StoppedART_E
		if (im == 0){ StoppedART_E[im] = 0.0; }
		else{
			StoppedART_E[im] = (StoppedART[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], MortAdjLate[0]) +
				OnARTearly[im - 1] * (1.0 - AIDSmortEarlyART[im - 1]) * (1.0 - exp(-Discontinuation1/12.0)) +
				OnARTlate1st3m[im - 1] * ARTexitHR[im - 1][0] * ARTexitHR[im - 1][2] +
				OnARTlateAfter3m[im - 1] * pow(1.0 - AIDSmortNoART[im - 1], RRmortART2 * MortAdjLate[2]) *
				(1.0 - exp(-Discontinuation2 / 12.0))) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate Total (at the START of the month)
		if(CurrMonth==0){
			Total[im] = NegMatMF[im] + NegMatEBF[im] + AcuteMatMF[im] + AcuteMatEBF[im] +
				UnawareMatMF[im] + UnawareMatEBF[im] + AwareMatMF[im] + AwareMatEBF[im] +
				ARTmatMF[im] + ARTmatEBF[im] + NegChildFF[im] + PosChildAtBirthNoPMTCT[im] +
				PosChildAtBirthPMTCT[im] + PosChildAfterBirth[im] + ARTeligible[im] + DiagChildAtBirthNoPMTCT[im] +
				DiagChildAtBirthPMTCT[im] + DiagChildAfterBirth[im] + DiagARTeligible[im] +
				OnARTearly[im] + OnARTlate1st3m[im] + OnARTlateAfter3m[im] + StoppedART[im];
		}

		// Calculate Total (at the END of the month)
		Total_E[im] = NegMatMF_E[im] + NegMatEBF_E[im] + AcuteMatMF_E[im] + AcuteMatEBF_E[im] +
			UnawareMatMF_E[im] + UnawareMatEBF_E[im] + AwareMatMF_E[im] + AwareMatEBF_E[im] +
			ARTmatMF_E[im] + ARTmatEBF_E[im] + NegChildFF_E[im] + PosChildAtBirthNoPMTCT_E[im] +
			PosChildAtBirthPMTCT_E[im] + PosChildAfterBirth_E[im] + ARTeligible_E[im] + DiagChildAtBirthNoPMTCT_E[im] +
			DiagChildAtBirthPMTCT_E[im] + DiagChildAfterBirth_E[im] + DiagARTeligible_E[im] +
			OnARTearly_E[im] + OnARTlate1st3m_E[im] + OnARTlateAfter3m_E[im] + StoppedART_E[im];

		// Flow variable calculations don't get done for im=132
		if (im < 132){
			// Calculate AIDSdeathsUntreated
			AIDSdeathsUntreated[im] = (ARTeligible[im] + DiagARTeligible[im] + StoppedART[im]) *
				(1.0 - pow(1.0 - AIDSmortNoART[im], MortAdjLate[0])) * (1.0 - NonAIDSmort[im]);

			// Calculate AIDSdeathsUndiagnosed
			if (FixedUncertainty == 1 || CalibChildPIP == 1){
				AIDSdeathsUndiagnosed[im] = ARTeligible[im] * (1.0 - pow(1.0 - AIDSmortNoART[im],
					MortAdjLate[0])) * (1.0 - NonAIDSmort[im]);
			}

			// Calculate AIDSdeaths
			AIDSdeaths[im] = AIDSdeathsUntreated[im] + (OnARTearly[im] * AIDSmortEarlyART[im] +
				OnARTlate1st3m[im] * ARTexitHR[im][0] * ARTexitHR[im][1] +
				OnARTlateAfter3m[im] * (1.0 - pow(1.0 - AIDSmortNoART[im], RRmortART2 * MortAdjLate[2]))) *
				(1.0 - NonAIDSmort[im]);

			// Calculate NonAIDSdeaths
			NonAIDSdeaths[im] = Total[im] * NonAIDSmort[im];
			if (im < 24){
				NonAIDSdeaths[im] += NegChildFF[im] * NonAIDSmort[im] * (pow(NoBFmortAdj[im],
					BFbiasAdj) - 1.0);
			}

			// Get the age dbn of infant deaths and neonatal deaths
			/*if(im<12 && CurrMonth==0){
				if(CurrYear==1997){
				AgeDbnIMR.out[CurrSim-1][im] = AIDSdeaths[im] + NonAIDSdeaths[im];}
				if(CurrYear==2002){
				AgeDbnIMR.out[CurrSim-1][im+12] = AIDSdeaths[im] + NonAIDSdeaths[im];}
				if(CurrYear==2007){
				AgeDbnIMR.out[CurrSim-1][im+24] = AIDSdeaths[im] + NonAIDSdeaths[im];}
				}
				if(im==0){
				DeathsM1.out[CurrSim-1][CurrYear-1985] += AIDSdeaths[0] + NonAIDSdeaths[0];
				AIDSdeathsM1.out[CurrSim-1][CurrYear-1985] += AIDSdeaths[0];
				}*/

			// Calculating StartingART
			StartingARTlate[im] = (ARTeligible[im] * MnthlyPaedTestToART[im][3] + DiagARTeligible[im] *
				(1.0 - exp(-ARTinitiation[im]))) * pow(1.0 - AIDSmortNoART[im], MortAdjLate[0]) * (1.0 - NonAIDSmort[im]);
			StartingART[im] = ((ARTeligible[im] * MnthlyPaedTestToART[im][3] + DiagARTeligible[im] *
				(1.0 - exp(-ARTinitiation[im]))) * pow(1.0 - AIDSmortNoART[im], MortAdjLate[0]) +
				PosChildAtBirthNoPMTCT[im] * MnthlyPaedTestToART[im][0] + PosChildAtBirthPMTCT[im] *
				MnthlyPaedTestToART[im][1] + PosChildAfterBirth[im] * MnthlyPaedTestToART[im][2]) * (1.0 - NonAIDSmort[im]);
			if (im < 12){
				StartingART[im] += (DiagChildAtBirthNoPMTCT[im] + DiagChildAtBirthPMTCT[im] + DiagChildAfterBirth[im]) *
					(1.0 - exp(-ARTinitiation[im] * RRearlyPaedART * EligInfants)) * (1.0 - NonAIDSmort[im]);
			}
			else if (im < 60){
				StartingART[im] += (DiagChildAtBirthNoPMTCT[im] + DiagChildAtBirthPMTCT[im] + DiagChildAfterBirth[im]) *
					(1.0 - exp(-ARTinitiation[im] * RRearlyPaedART * EarlyART1to4[iy])) * (1.0 - NonAIDSmort[im]);
			}
			else{
				StartingART[im] += (DiagChildAtBirthNoPMTCT[im] + DiagChildAtBirthPMTCT[im] + DiagChildAfterBirth[im]) *
					(1.0 - exp(-ARTinitiation[im] * RRearlyPaedART * EarlyART5to14[iy])) * (1.0 - NonAIDSmort[im]);
			}
			if (im == 0){
				StartingART[im] += (ChildPosNoPMTCT * BirthSe[0] + ChildPosPMTCT * BirthSe[1]) *
					(PropnBirths / 12.0) * EligInfants * PCRuptakeB * PaedARTuptakeC * RRearlyPaedART;
			}
		}
	}

	if (FixedUncertainty == 1){
		for (im = 0; im <= CurrMonth; im++){
			AIDSdeathsYOB[Sex] += AIDSdeaths[im];
		}
	}
}

void Child::UpdateStartProfile()
{
	int im;

	for(im=0; im<133; im++){
		if(im<36){
			NegMatMF[im] = NegMatMF_E[im];
			NegMatEBF[im] = NegMatEBF_E[im];
			AcuteMatMF[im] = AcuteMatMF_E[im];
			AcuteMatEBF[im] = AcuteMatEBF_E[im];
			UnawareMatMF[im] = UnawareMatMF_E[im];
			UnawareMatEBF[im] = UnawareMatEBF_E[im];
			AwareMatMF[im] = AwareMatMF_E[im];
			AwareMatEBF[im] = AwareMatEBF_E[im];
			ARTmatMF[im] = ARTmatMF_E[im];
			ARTmatEBF[im] = ARTmatEBF_E[im];
		}
		NegChildFF[im] = NegChildFF_E[im];
		PosChildAtBirthNoPMTCT[im] = PosChildAtBirthNoPMTCT_E[im];
		PosChildAtBirthPMTCT[im] = PosChildAtBirthPMTCT_E[im];
		PosChildAfterBirth[im] = PosChildAfterBirth_E[im];
		ARTeligible[im] = ARTeligible_E[im];
		DiagChildAtBirthNoPMTCT[im] = DiagChildAtBirthNoPMTCT_E[im];
		DiagChildAtBirthPMTCT[im] = DiagChildAtBirthPMTCT_E[im];
		DiagChildAfterBirth[im] = DiagChildAfterBirth_E[im];
		DiagARTeligible[im] = DiagARTeligible_E[im];
		OnARTearly[im] = OnARTearly_E[im];
		OnARTlate1st3m[im] = OnARTlate1st3m_E[im];
		OnARTlateAfter3m[im] = OnARTlateAfter3m_E[im];
		StoppedART[im] = StoppedART_E[im];
		Total[im] = Total_E[im];
	}
}

void Child::UpdateMigP()
{
	int ia, im;
	double MigrAdj[132][2], Prev, PrevAdj;

	for (ia = 0; ia <= 10; ia++){
		Prev = TotalPositive[ia][Sex]/TotalPop[ia][Sex];
		if (CurrYear <= 1995){ PrevAdj = MigrationHIVadj[ia][0]; }
		else if (CurrYear <= 2000){
			PrevAdj = MigrationHIVadj[ia][0] + (MigrationHIVadj[ia][1]
				- MigrationHIVadj[ia][0]) * (CurrYear - 1995) / 5.0;
		}
		else if (CurrYear <= 2010){
			PrevAdj = MigrationHIVadj[ia][1] + (MigrationHIVadj[ia][2]
				- MigrationHIVadj[ia][1]) * (CurrYear - 2000) / 10.0;
		}
		else{ PrevAdj = MigrationHIVadj[ia][2]; }
		for (im = 0; im<12; im++){
			MigrAdj[ia * 12 + im][0] = MigrationAdj[ia][Sex] * (1.0 - Prev * PrevAdj)/(1.0 - Prev);
			MigrAdj[ia * 12 + im][1] = MigrationAdj[ia][Sex] * PrevAdj;
		}
	}

	for(im=0; im<132; im++){
		if(im<36){
			NegMatMF[im] *= MigrAdj[im][0];
			NegMatEBF[im] *= MigrAdj[im][0];
			AcuteMatMF[im] *= MigrAdj[im][0];
			AcuteMatEBF[im] *= MigrAdj[im][0];
			UnawareMatMF[im] *= MigrAdj[im][0];
			UnawareMatEBF[im] *= MigrAdj[im][0];
			AwareMatMF[im] *= MigrAdj[im][0];
			AwareMatEBF[im] *= MigrAdj[im][0];
			ARTmatMF[im] *= MigrAdj[im][0];
			ARTmatEBF[im] *= MigrAdj[im][0];
		}
		NegChildFF[im] *= MigrAdj[im][0];
		PosChildAtBirthNoPMTCT[im] *= MigrAdj[im][1];
		PosChildAtBirthPMTCT[im] *= MigrAdj[im][1];
		PosChildAfterBirth[im] *= MigrAdj[im][1];
		ARTeligible[im] *= MigrAdj[im][1];
		DiagChildAtBirthNoPMTCT[im] *= MigrAdj[im][1];
		DiagChildAtBirthPMTCT[im] *= MigrAdj[im][1];
		DiagChildAfterBirth[im] *= MigrAdj[im][1];
		DiagARTeligible[im] *= MigrAdj[im][1];
		OnARTearly[im] *= MigrAdj[im][1];
		OnARTlate1st3m[im] *= MigrAdj[im][1];
		OnARTlateAfter3m[im] *= MigrAdj[im][1];
		StoppedART[im] *= MigrAdj[im][1];
		Total[im] = 0.0;
		if (im < 36){
			Total[im] = NegMatMF[im] + NegMatEBF[im] + AcuteMatMF[im] + AcuteMatEBF[im] + UnawareMatMF[im] +
				UnawareMatEBF[im] + AwareMatMF[im] + AwareMatEBF[im] + ARTmatMF[im] + ARTmatEBF[im];
		}
		Total[im] += NegChildFF[im] + PosChildAtBirthNoPMTCT[im] + PosChildAtBirthPMTCT[im] + PosChildAfterBirth[im] +
			ARTeligible[im] + DiagChildAtBirthNoPMTCT[im] + DiagChildAtBirthPMTCT[im] + DiagChildAfterBirth[im] +
			DiagARTeligible[im] + OnARTearly[im] + OnARTlate1st3m[im] + OnARTlateAfter3m[im] + StoppedART[im];
	}
}

Adult::Adult(int Gender, int iRisk, int Married, int Virgin, int SRisk, int FSW, int Circ, int MSM)
{
	Sex = Gender;
	Risk = iRisk;
	MarriedInd = Married;
	VirginInd = Virgin;
	SpouseRisk = SRisk;
	FSWind = FSW;
	CircInd = Circ;
	MSMind = MSM;
}

void Adult::GetStartYrProfile()
{
	int ia, ic;
	int BehavGroup;
	double PartnerRiskPropn, Base, MSMadj;

	BehavGroup = 0;
	if(Risk==2){
		BehavGroup = 3;}
	if(VirginInd==0){
		BehavGroup += 1;}
	if(MarriedInd==1){
		BehavGroup += 1;
		if(SpouseRisk==1){
			PartnerRiskPropn = InitMarriedHigh[Sex][Risk-1];}
		else{
			PartnerRiskPropn = 1.0 - InitMarriedHigh[Sex][Risk-1];}
	}
	if (MSMind == 1){ MSMadj = InitMSMprevRatio; }
	else{ MSMadj = 1.0; }

	for(ia=0; ia<81; ia++){
		// Set all arrays after the first to zero
		NegPastHCT[ia] = 0.0;
		NegPastHCT_E[ia] = 0.0;
		RegHCT[ia] = 0.0;
		RegHCT_E[ia] = 0.0;
		RegPrEP[ia] = 0.0;
		RegPrEP_E[ia] = 0.0;
		RegVM[ia] = 0.0;
		RegVM_E[ia] = 0.0;
		for(ic=0; ic<5; ic++){
			PosNoHCT[ia][ic] = 0.0;
			PosNoHCT_E[ia][ic] = 0.0;
			PosHCTpreHIV[ia][ic] = 0.0;
			PosHCTpreHIV_E[ia][ic] = 0.0;
			PosDiagnosedPreART[ia][ic] = 0.0;
			PosDiagnosedPreART_E[ia][ic] = 0.0;
			OnARTpre500[ia][ic] = 0.0;
			OnARTpre500_E[ia][ic] = 0.0;
			OnART500[ia][ic] = 0.0;
			OnART500_E[ia][ic] = 0.0;
			OnART350[ia][ic] = 0.0;
			OnART350_E[ia][ic] = 0.0;
			OnART200[ia][ic] = 0.0;
			OnART200_E[ia][ic] = 0.0;
		}
		for(ic=0; ic<4; ic++){
			StoppedART[ia][ic] = 0.0;
			StoppedART_E[ia][ic] = 0.0;
		}
		// Set values for the first array + initial infections
		Base = InitBehavDbn[ia][BehavGroup][Sex];
		if(MarriedInd==1){
			Base *= PartnerRiskPropn;}
		if(Sex==0){
			if(CircInd==1){
				Base *= InitCircumcised[ia+10];}
			else{
				Base *= (1.0 - InitCircumcised[ia+10]);}
			if (BehavGroup == 1 || BehavGroup == 4){
				if (MSMind == 1){ Base *= MSMpropn; }
				else{ Base *= (1.0 - MSMpropn); }
			}
		}
		if(FSWind==1){
			Base = TotalFSW * FSWageDbn[ia];
			if(Base>InitBehavDbn[ia][1][1]){
				Base = InitBehavDbn[ia][1][1];}
		}
		if(Sex==1 && VirginInd==0 && MarriedInd==0 && Risk==1 && FSWind==0){
			Base = Base - TotalFSW * FSWageDbn[ia];
			if(Base<0.0){Base = 0.0;}
		}
		Total[ia] = Base;
		if(Risk==2 || ia<5 || ia>39 || VirginInd==1){
			NegNoHCT[ia] = Base;}
		else{
			NegNoHCT[ia] = Base * (1.0 - InitFSWprev * InitPrevAdj[ia-5][Sex] *
				MSMadj * HighRiskPropn[1]/HighRiskPropn[Sex]);
			PosNoHCT[ia][1] = Base * InitFSWprev * InitPrevAdj[ia-5][Sex] *
				MSMadj * HighRiskPropn[1] / HighRiskPropn[Sex];
		}
	}
}

void Adult::UpdateProbTransmM()
{
	int ia, ib, is;
	double BaseProb[12], BaseProbMSM[4], Temp1, Temp2;

	// Initialize BaseProb
	for(ib=0; ib<12; ib++){
		BaseProb[ib] = 0.0;}
	for (ib = 0; ib<4; ib++){
		BaseProbMSM[ib] = 0.0;}

	BaseProb[0] = TransmST[1] * RelInfecRiskST[Risk-1][0][1];
	BaseProb[3] = TransmST[1] * RelInfecRiskST[Risk-1][1][1];
	if (MSMind == 0 && Risk==1){
		//if (CurrYear >= 1995){ BaseProb[6] = TransmFSW[1] * RelInfecRiskST[0][0][1]; }
		//else{ BaseProb[6] = TransmFSW[1] * RelInfecRiskST[0][0][1] * (1.0 + (RRclientToFSW1985 - 1.0) *
		//	(1995 - CurrYear)/10.0); }
		BaseProb[6] = TransmFSW[1] * RelInfecRiskST[0][0][1] * (1.0 + (RRclientToFSW1985 - 1.0) *
			pow(0.75, CurrYear - 1985));
	}
	if(MarriedInd==1){
		BaseProb[9] = TransmLT[1] * RelInfecRiskLT[Risk-1][SpouseRisk-1][1];}
	if(PrEPorVM==1){
		for(ib=0; ib<4; ib++){
			BaseProb[ib*3+1] = BaseProb[ib*3] * (1.0 - PrEPefficacy[1]);
			BaseProb[ib*3+2] = BaseProb[ib*3] * (1.0 - MicrobicideEff);
		}
	}
	if (MSMind == 1){
		BaseProbMSM[0] = MtoM_ST * RelInfecRiskST[Risk - 1][0][0];
		BaseProbMSM[2] = MtoM_ST * RelInfecRiskST[Risk - 1][1][0];
		if (PrEPorVM == 1){
			BaseProbMSM[1] = BaseProbMSM[0] * (1.0 - PrEPefficacyMSM);
			BaseProbMSM[3] = BaseProbMSM[2] * (1.0 - PrEPefficacyMSM);
		}
	}

	// Adjust BaseProb if virulence is changing
	if (RRperCalYr != 1.0){
		for (ib = 0; ib < 12; ib++){
			BaseProb[ib] *= pow(RRperCalYr, 2.5 * InfToVirulenceRatio * (CurrYear - 1999));
			if (BaseProb[ib]>1.0){ BaseProb[ib] = 1.0; }
		}
		for (ib = 0; ib < 4; ib++){
			BaseProbMSM[ib] *= pow(RRperCalYr, 2.5 * InfToVirulenceRatio * (CurrYear - 1999));
			if (BaseProbMSM[ib]>1.0){ BaseProbMSM[ib] = 1.0; }
		}
	}

	// Non-commercial sex
	for(ia=0; ia<81; ia++){
		if(Total[ia]>0.0){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(is=0; is<5; is++){
				Temp1 += PosNoHCT[ia][is] * RelativeTransm[is][0][0];
				Temp2 += PosNoHCT[ia][is] * RelativeTransm[is][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += PosHCTpreHIV[ia][is] * RelativeTransm[is + 5][0][0];
				Temp2 += PosHCTpreHIV[ia][is] * RelativeTransm[is + 5][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += PosDiagnosedPreART[ia][is] * RelativeTransm[is + 10][0][0];
				Temp2 += PosDiagnosedPreART[ia][is] * RelativeTransm[is + 10][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += OnARTpre500[ia][is] * RelativeTransm[is + 15][0][0];
				Temp2 += OnARTpre500[ia][is] * RelativeTransm[is + 15][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART500[ia][is] * RelativeTransm[is + 20][0][0];
				Temp2 += OnART500[ia][is] * RelativeTransm[is + 20][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART350[ia][is] * RelativeTransm[is + 25][0][0];
				Temp2 += OnART350[ia][is] * RelativeTransm[is + 25][1][0];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART200[ia][is] * RelativeTransm[is + 30][0][0];
				Temp2 += OnART200[ia][is] * RelativeTransm[is + 30][1][0];
			}
			for(is=0; is<4; is++){
				Temp1 += StoppedART[ia][is] * RelativeTransm[is + 35][0][0];
				Temp2 += StoppedART[ia][is] * RelativeTransm[is + 35][1][0];
			}
			if(MarriedInd==0 || Risk==1){
				// Transm to ST partner in high risk group
				ProbTransm[ia][0] = 1.0 - exp(-BaseProb[0] * SexActsST * ((1.0 -
					ProbCondomST[ia][0]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				if(PrEPorVM==1){
					ProbTransm[ia][1] = 1.0 - exp(-BaseProb[1] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednPrEP[1])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
					ProbTransm[ia][2] = 1.0 - exp(-BaseProb[2] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednVM)) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
				}
				// Transm to ST partner in low risk group
				ProbTransm[ia][3] = 1.0 - exp(-BaseProb[3] * SexActsST * ((1.0 -
					ProbCondomST[ia][0]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				if(PrEPorVM==1){
					ProbTransm[ia][4] = 1.0 - exp(-BaseProb[4] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednPrEP[1])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
					ProbTransm[ia][5] = 1.0 - exp(-BaseProb[5] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednVM)) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
				}
			}
			// Transm to LT partner
			if(MarriedInd==1 && ia>=5){
				ProbTransm[ia][9] = BaseProb[9] * SexFreqMarital[ia-5][0] * ((1.0 - ProbCondomLT[ia][0]) *
					CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				if(PrEPorVM==1){
					ProbTransm[ia][10] = BaseProb[10] * SexFreqMarital[ia-5][0] * ((1.0 -
						ProbCondomLT[ia][0] * (1.0 - CondomRednPrEP[1])) * CondomEfficacy * Temp2 +
						(1.0 - CondomEfficacy) * Temp1)/Total[ia];
					ProbTransm[ia][11] = BaseProb[11] * SexFreqMarital[ia-5][0] * ((1.0 -
						ProbCondomLT[ia][0] * (1.0 - CondomRednVM)) * CondomEfficacy * Temp2 +
						(1.0 - CondomEfficacy) * Temp1)/Total[ia];
				}
			}
			// Transm to CSW
			if(Risk==1 && MSMind==0){
				ProbTransm[ia][6] = BaseProb[6] * ((1.0 - ProbCondomFSW) * CondomEfficacy *
					Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				if(PrEPorVM==1){
					ProbTransm[ia][7] = BaseProb[7] * ((1.0 - ProbCondomFSW *
						(1.0 - CondomRednPrEP[1])) * CondomEfficacy * Temp2 + (1.0 -
						CondomEfficacy) * Temp1)/Total[ia];
					ProbTransm[ia][8] = BaseProb[8] * ((1.0 - ProbCondomFSW *
						(1.0 - CondomRednVM)) * CondomEfficacy * Temp2 + (1.0 -
						CondomEfficacy) * Temp1)/Total[ia];
				}
			}
			// Transm to male ST partner (MSM)
			if (MSMind == 1){
				ProbTransm[ia][6] = 1.0 - exp(-BaseProbMSM[0] * SexActsST * ((1.0 -
					ProbCondomST[ia][0]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1) / Total[ia]);
				ProbTransm[ia][8] = 1.0 - exp(-BaseProbMSM[2] * SexActsST * ((1.0 -
					ProbCondomST[ia][0]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1) / Total[ia]);
				if (PrEPorVM == 1){
					ProbTransm[ia][7] = 1.0 - exp(-BaseProbMSM[1] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1) / Total[ia]);
					ProbTransm[ia][9] = 1.0 - exp(-BaseProbMSM[3] * SexActsST * ((1.0 -
						ProbCondomST[ia][0] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1) / Total[ia]);
				}
			}
		}
	}

	// Commercial sex
	/*if(Risk==1){
		for(ia=0; ia<81; ia++){
			if(Total[ia]>0.0){
				Temp1 = 0.0;
				Temp2 = 0.0;
				for(is=0; is<5; is++){
					Temp1 += PosNoHCT[ia][is] * RelativeInf[is];
					Temp2 += PosNoHCT[ia][is] * RelativeTransm[is][2];
				}
				for(is=0; is<5; is++){
					Temp1 += PosHCTpreHIV[ia][is] * RelativeInf[is+5];
					Temp2 += PosHCTpreHIV[ia][is] * RelativeTransm[is+5][2];
				}
				for(is=0; is<5; is++){
					Temp1 += PosDiagnosedPreART[ia][is] * RelativeInf[is+10];
					Temp2 += PosDiagnosedPreART[ia][is] * RelativeTransm[is+10][2];
				}
				for(is=0; is<5; is++){
					Temp1 += OnARTpre500[ia][is] * RelativeInf[is+15];
					Temp2 += OnARTpre500[ia][is] * RelativeTransm[is+15][2];
				}
				for(is=0; is<5; is++){
					Temp1 += OnART500[ia][is] * RelativeInf[is+20];
					Temp2 += OnART500[ia][is] * RelativeTransm[is+20][2];
				}
				for(is=0; is<5; is++){
					Temp1 += OnART350[ia][is] * RelativeInf[is+25];
					Temp2 += OnART350[ia][is] * RelativeTransm[is+25][2];
				}
				for(is=0; is<5; is++){
					Temp1 += OnART200[ia][is] * RelativeInf[is+30];
					Temp2 += OnART200[ia][is] * RelativeTransm[is+30][2];
				}
				for(is=0; is<4; is++){
					Temp1 += StoppedART[ia][is] * RelativeInf[is+35];
					Temp2 += StoppedART[ia][is] * RelativeTransm[is+35][2];
				}
				ProbTransm[ia][6] = BaseProb[6] * ((1.0 - ProbCondomFSW) * CondomEfficacy *
					Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				if(PrEPorVM==1){
					ProbTransm[ia][7] = BaseProb[7] * ((1.0 - ProbCondomFSW *
						(1.0 - CondomRednPrEP[1])) * CondomEfficacy * Temp2 + (1.0 -
						CondomEfficacy) * Temp1)/Total[ia];
					ProbTransm[ia][8] = BaseProb[8] * ((1.0 - ProbCondomFSW *
						(1.0 - CondomRednVM)) * CondomEfficacy * Temp2 + (1.0 -
						CondomEfficacy) * Temp1)/Total[ia];
				}
			}
		}
	}*/
}

void Adult::UpdateProbTransmF()
{
	int ia, ib, is;
	double BaseProb[12], Temp1, Temp2;

	// Initialize BaseProb
	for(ib=0; ib<12; ib++){
		BaseProb[ib] = 0.0;}

	BaseProb[0] = TransmST[0] * RelInfecRiskST[0][Risk-1][0];
	BaseProb[4] = TransmST[0] * RelInfecRiskST[1][Risk-1][0];
	if(MarriedInd==1){
		BaseProb[8] = TransmLT[0] * RelInfecRiskLT[SpouseRisk-1][Risk-1][0];}
	for(ib=0; ib<3; ib++){
		BaseProb[ib*4+2] = BaseProb[ib*4] * (1.0 - MCefficacy);}
	if(PrEPorVM==1){
		for(ib=0; ib<6; ib++){
			BaseProb[ib*2+1] = BaseProb[ib*2] * (1.0 - PrEPefficacy[0]);}
	}

	// Adjust BaseProb if virulence is changing
	if (RRperCalYr != 1.0){
		for (ib = 0; ib < 12; ib++){
			BaseProb[ib] *= pow(RRperCalYr, 2.5 * InfToVirulenceRatio * (CurrYear - 1999));
			if (BaseProb[ib]>1.0){ BaseProb[ib] = 1.0; }
		}
	}

	for(ia=0; ia<81; ia++){
		if(Total[ia]>0.0){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(is=0; is<5; is++){
				Temp1 += PosNoHCT[ia][is] * RelativeTransm[is][0][1];
				Temp2 += PosNoHCT[ia][is] * RelativeTransm[is][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += PosHCTpreHIV[ia][is] * RelativeTransm[is + 5][0][1];
				Temp2 += PosHCTpreHIV[ia][is] * RelativeTransm[is + 5][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += PosDiagnosedPreART[ia][is] * RelativeTransm[is + 10][0][1];
				Temp2 += PosDiagnosedPreART[ia][is] * RelativeTransm[is + 10][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnARTpre500[ia][is] * RelativeTransm[is + 15][0][1];
				Temp2 += OnARTpre500[ia][is] * RelativeTransm[is + 15][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART500[ia][is] * RelativeTransm[is + 20][0][1];
				Temp2 += OnART500[ia][is] * RelativeTransm[is + 20][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART350[ia][is] * RelativeTransm[is + 25][0][1];
				Temp2 += OnART350[ia][is] * RelativeTransm[is + 25][1][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART200[ia][is] * RelativeTransm[is + 30][0][1];
				Temp2 += OnART200[ia][is] * RelativeTransm[is + 30][1][1];
			}
			for(is=0; is<4; is++){
				Temp1 += StoppedART[ia][is] * RelativeTransm[is + 35][0][1];
				Temp2 += StoppedART[ia][is] * RelativeTransm[is + 35][1][1];
			}
			if(MarriedInd==0 || Risk==1){
				// Transm to ST partner in high risk group
				ProbTransm[ia][0] = 1.0 - exp(-BaseProb[0] * SexActsST * ((1.0 -
					ProbCondomST[ia][1]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				ProbTransm[ia][2] = 1.0 - exp(-BaseProb[2] * SexActsST * ((1.0 -
					ProbCondomST[ia][1]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				if(PrEPorVM==1){
					ProbTransm[ia][1] = 1.0 - exp(-BaseProb[1] * SexActsST * ((1.0 -
						ProbCondomST[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
					ProbTransm[ia][3] = 1.0 - exp(-BaseProb[3] * SexActsST * ((1.0 -
						ProbCondomST[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
				}
				// Transm to ST partner in low risk group
				ProbTransm[ia][4] = 1.0 - exp(-BaseProb[4] * SexActsST * ((1.0 -
					ProbCondomST[ia][1]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				ProbTransm[ia][6] = 1.0 - exp(-BaseProb[6] * SexActsST * ((1.0 -
					ProbCondomST[ia][1]) * CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) *
					Temp1)/Total[ia]);
				if(PrEPorVM==1){
					ProbTransm[ia][5] = 1.0 - exp(-BaseProb[5] * SexActsST * ((1.0 -
						ProbCondomST[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
					ProbTransm[ia][7] = 1.0 - exp(-BaseProb[7] * SexActsST * ((1.0 -
						ProbCondomST[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy *
						Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia]);
				}
			}
			// Transm to LT partner
			if(MarriedInd==1 && ia>=5){
				ProbTransm[ia][8] = BaseProb[8] * SexFreqMarital[ia-5][1] * ((1.0 - ProbCondomLT[ia][1]) *
					CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				ProbTransm[ia][10] = BaseProb[10] * SexFreqMarital[ia-5][1] * ((1.0 - ProbCondomLT[ia][1]) *
					CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				if(PrEPorVM==1){
					ProbTransm[ia][9] = BaseProb[9] * SexFreqMarital[ia-5][1] * ((1.0 -
						ProbCondomLT[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy * Temp2 +
						(1.0 - CondomEfficacy) * Temp1)/Total[ia];
					ProbTransm[ia][11] = BaseProb[11] * SexFreqMarital[ia-5][1] * ((1.0 -
						ProbCondomLT[ia][1] * (1.0 - CondomRednPrEP[0])) * CondomEfficacy * Temp2 +
						(1.0 - CondomEfficacy) * Temp1)/Total[ia];
				}
			}
		}
	}
}

void Adult::UpdateProbTransmSW()
{
	int ia, ib, is;
	double BaseProb[4], Temp1, Temp2, Temp3[4];

	BaseProb[0] = TransmFSW[0] * RelInfecRiskST[0][0][0];
	BaseProb[2] = BaseProb[0] * (1.0 - MCefficacy);
	if(PrEPorVM==1){
		BaseProb[1] = BaseProb[0] * (1.0 - PrEPefficacy[0]);
		BaseProb[3] = BaseProb[2] * (1.0 - PrEPefficacy[0]);
	}

	// Adjust BaseProb if virulence is changing
	if (RRperCalYr != 1.0){
		for (ib = 0; ib < 4; ib++){
			BaseProb[ib] *= pow(RRperCalYr, 2.5 * InfToVirulenceRatio * (CurrYear - 1999));
			if (BaseProb[ib]>1.0){ BaseProb[ib] = 1.0; }
		}
	}

	for(ia=0; ia<81; ia++){
		if(Total[ia]>0.0){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(is=0; is<5; is++){
				Temp1 += PosNoHCT[ia][is] * RelativeInf[is][1];
				Temp2 += PosNoHCT[ia][is] * RelativeTransm[is][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += PosHCTpreHIV[ia][is] * RelativeInf[is + 5][1];
				Temp2 += PosHCTpreHIV[ia][is] * RelativeTransm[is + 5][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += PosDiagnosedPreART[ia][is] * RelativeInf[is + 10][1];
				Temp2 += PosDiagnosedPreART[ia][is] * RelativeTransm[is + 10][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnARTpre500[ia][is] * RelativeInf[is + 15][1];
				Temp2 += OnARTpre500[ia][is] * RelativeTransm[is + 15][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART500[ia][is] * RelativeInf[is + 20][1];
				Temp2 += OnART500[ia][is] * RelativeTransm[is + 20][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART350[ia][is] * RelativeInf[is + 25][1];
				Temp2 += OnART350[ia][is] * RelativeTransm[is + 25][2][1];
			}
			for(is=0; is<5; is++){
				Temp1 += OnART200[ia][is] * RelativeInf[is + 30][1];
				Temp2 += OnART200[ia][is] * RelativeTransm[is + 30][2][1];
			}
			for(is=0; is<4; is++){
				Temp1 += StoppedART[ia][is] * RelativeInf[is + 35][1];
				Temp2 += StoppedART[ia][is] * RelativeTransm[is + 35][2][1];
			}
			ProbTransm[ia][0] = BaseProb[0] * ((1.0 - ProbCondomFSW) *
				CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
			ProbTransm[ia][2] = BaseProb[2] * ((1.0 - ProbCondomFSW) *
				CondomEfficacy * Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
			if(PrEPorVM==1){
				ProbTransm[ia][1] = BaseProb[1] * ((1.0 - ProbCondomFSW *
					(1.0 - CondomRednPrEP[0])) * CondomEfficacy *
					Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
				ProbTransm[ia][3] = BaseProb[3] * ((1.0 - ProbCondomFSW *
					(1.0 - CondomRednPrEP[0])) * CondomEfficacy *
					Temp2 + (1.0 - CondomEfficacy) * Temp1)/Total[ia];
			}
		}
	}

	// Calculate TransmFSWtoM
	Temp1 = 0.0;
	for(ib=0; ib<4; ib++){
		Temp3[ib] = 0.0;}
	for(ia=0; ia<81; ia++){
		Temp1 += Total[ia];
		for(ib=0; ib<4; ib++){
			Temp3[ib] += Total[ia] * ProbTransm[ia][ib];}
	}
	for(ib=0; ib<4; ib++){
		TransmFSWtoM[ib] = Temp3[ib]/Temp1;}
}

void Adult::UpdateProbAcqM(int group)
{
	int ia, ib, offset, offsetLT, offsetMSM;
	double FromST, FromLT, FromFSW, FromMSM, Temp1, Temp2;

	offset = (Risk - 1) * 4 + CircInd * 2 + group;
	offsetLT = 8 + CircInd * 2 + group;
	offsetMSM = (Risk - 1) * 2 + group;
	FromST = 0.0;
	FromLT = 0.0;
	FromFSW = 0.0;
	FromMSM = 0.0;

	for(ia=0; ia<81; ia++){
		// Transmission from ST partners
		if(Risk==1 || MarriedInd==0){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(ib=0; ib<81; ib++){
				Temp1 += AgePrefST[ia][ib][0] * TransmFtoM_ST[ib][offset];
				Temp2 += AgePrefST[ia][ib][0] * FL_ST.ProbTransm[ib][offset];
			}
			if (MSMind == 0){
				FromST = (PartnerAcqM[ia] / 12.0) * (CurrSThigh[0][Risk - 1] * Temp1 +
					(1.0 - CurrSThigh[0][Risk - 1]) * Temp2);
			}
			else{
				FromST = (PartnerAcqM[ia] / 12.0) * (1.0 - MSMpartnersM[ia]) *
					(CurrSThigh[0][Risk - 1] * Temp1 + (1.0 - CurrSThigh[0][Risk - 1]) * Temp2);
			}
			if(Risk==2){
				FromST *= RRpartnerLow[0];}
			if(Risk==1 && MarriedInd==1){
				FromST *= RRpartnerMarried[0];}
		}
		// Transmission from LT partner
		if(MarriedInd==1){
			FromLT = 0.0;
			if(Risk==1 && SpouseRisk==1){
				for(ib=0; ib<81; ib++){
					FromLT += AgePrefLT[ia][ib][0] * FH_LTH.ProbTransm[ib][offsetLT];}
			}
			else if(Risk==1 && SpouseRisk==2){
				for(ib=0; ib<81; ib++){
					FromLT += AgePrefLT[ia][ib][0] * FL_LTH.ProbTransm[ib][offsetLT];}
			}
			else if(Risk==2 && SpouseRisk==1){
				for(ib=0; ib<81; ib++){
					FromLT += AgePrefLT[ia][ib][0] * FH_LTL.ProbTransm[ib][offsetLT];}
			}
			else{
				for(ib=0; ib<81; ib++){
					FromLT += AgePrefLT[ia][ib][0] * FL_LTL.ProbTransm[ib][offsetLT];}
			}
		}
		// Transmission from sex workers
		if(Risk==1 && MSMind==0){
			FromFSW = (FSWcontactRate[ia]/12.0) * TransmFSWtoM[CircInd*2+group];
			if(MarriedInd==1){
				FromFSW *= FSWcontactMarried;}
		}
		// Transmission from MSM
		if (MSMind == 1 && MarriedInd == 0){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for (ib = 0; ib<81; ib++){
				Temp1 += AgePrefMSM[ia][ib] * TransmMtoM_ST[ib][offsetMSM];
				Temp2 += AgePrefMSM[ia][ib] * TransmMtoM_ST[ib][offsetMSM+4];
			}
			FromMSM = (PartnerAcqM[ia] / 12.0) * MSMpartnersM[ia] *
				(CurrSThighMSM[Risk - 1] * Temp1 + (1.0 - CurrSThighMSM[Risk - 1]) * Temp2);
			if (Risk == 2){
				FromMSM *= RRpartnerLow[0];
			}
		}
		// Combining transmission from all sources
		ProbHIVacq[ia][group] = 1.0 - exp(-(FromST + FromLT + FromFSW + FromMSM) * EctopyFactor[ia][0]);
	}
}

void Adult::UpdateProbAcqF(int group)
{
	int ia, ib, offset1, offset2, offsetLT;
	double FromST, FromLT, Temp1, Temp2;

	offset1 = (Risk - 1) * 3 + group; // High risk ST partner
	offset2 = 6 + (Risk - 1) * 3 + group; // Low risk ST partner
	offsetLT = (SpouseRisk - 1) * 6 + (Risk - 1) * 3 + group; // LT partner
	FromST = 0.0;
	FromLT = 0.0;

	for(ia=0; ia<81; ia++){
		// Transmission from ST partners
		if(FSWind==0 && (Risk==1 || MarriedInd==0)){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(ib=0; ib<81; ib++){
				Temp1 += AgePrefST[ia][ib][1] * TransmMtoF_ST[ib][offset1];
				Temp2 += AgePrefST[ia][ib][1] * TransmMtoF_ST[ib][offset2];
			}
			FromST = PartnerRate20F * (PartnerAcqF[ia]/12.0) * (CurrSThigh[1][Risk-1] *
				Temp1 + (1.0 - CurrSThigh[1][Risk-1]) * Temp2);
			if(Risk==2){
				FromST *= RRpartnerLow[1];}
			if(Risk==1 && MarriedInd==1){
				FromST *= RRpartnerMarried[1];}
		}
		// Transmission from LT partner
		if(MarriedInd==1){
			FromLT = 0.0;
			for(ib=0; ib<81; ib++){
				FromLT += AgePrefLT[ia][ib][1] * TransmMtoF_LT[ib][offsetLT];}
		}
		// Combining transmission from ST and LT sources
		ProbHIVacq[ia][group] = 1.0 - exp(-(FromST + FromLT) * EctopyFactor[ia][1]);
	}

	if(FSWind==1){
		for(ia=0; ia<81; ia++){
			ProbHIVacq[ia][group] = 1.0 - exp(-(ClientsPA/12.0) * TransmMtoFSW[group] *
				EctopyFactor[ia][1]);
		}
	}

}

void Adult::GetEndProfile()
{
	int ia, is, id, UpperAge;
	double StopRegHCT, StopPrEP, StopVM, NewHIVgroup[3], PosTesting[10], TestingToART[10];
	double Fraction200, Temp1, Temp2, TempART, NewDiag[4][2], NewDiagAcute, NewARTacute, AcuteLink;
	double ProbAcqT, ProbAcqU, RateAcq, UntestedPropn; // Variables new to THEMBISA v4

	if(VirginInd==0){
		UpperAge = 81;}
	else{
		UpperAge = 20;}
	if(PrEPorVM==1){
		StopRegHCT = 1.0 - exp(-1.0/(12.0 * RegHCTdur[Sex]));
		StopPrEP = 1.0 - exp(-1.0/(12.0 * PrEPdur[Sex]));
		StopVM = 1.0 - exp(-1.0/(12.0 * VMdur));
	}
	else{
		StopRegHCT = 0.0;
		StopPrEP = 0.0;
		StopVM = 0.0;
	}
	if (PropnalImmART == 0){ AcuteLink = AsymStart[Sex] * EligibleAsym500[CurrYear - StartYear]; }
	else{
		if (Sex == 1){
			AcuteLink = (1.0 - exp(AsymStart[Sex] * RR_ARTinitiation[1])) *
			EligibleAsym500[CurrYear - StartYear]; }
		else{
			AcuteLink = (1.0 - exp(AsymStart[Sex] * RR_ARTinitiation[1] * RR_ARTstartM)) *
			EligibleAsym500[CurrYear - StartYear]; }
	}
	NewHIVgroup[0] = 0.0;
	NewHIVgroup[1] = 0.0;
	NewHIVgroup[2] = 0.0;

	for(ia=0; ia<UpperAge; ia++){
		// Calculate positive testing rates and Fraction200
		if(VirginInd==1){
			for(is=0; is<5; is++){
				PosTesting[is] = TestingRateV[ia][is][Sex];
				PosTesting[is+5] = TestingRateV[ia][is][Sex];
				TestingToART[is] = HCTtoART_V[ia][is][Sex];
				TestingToART[is+5] = HCTtoART_V[ia][is][Sex];
			}
		}
		else{
			for(is=0; is<10; is++){
				PosTesting[is] = TotTestingRateSE[ia][is+2][Sex];
				TestingToART[is] = HCTtoART_SE[ia][is][Sex];
			}
		}
		Fraction200 = MnthlyCD4trans[ia][3][Sex]/(MnthlyCD4trans[ia][3][Sex] +
			MnthlyAIDSmort[ia][0][Sex]);
		// Calculate transitions for HIV-negative states and new infections
		UntestedPropn = NegNoHCT[ia]/(NegNoHCT[ia] + NegPastHCT[ia]);
		if(UntestedPropn<1.0 && UntestedPropn>0.0 && ProbHIVacq[ia][0]<1.0){
			/*RateAcq = -log(1.0 - ProbHIVacq[ia][0]);
			ProbAcqT = RateAcq * (1.0 - pow(UntestedPropn, HIVeffectVCT))/
				(1.0 - UntestedPropn);
			ProbAcqU = RateAcq + (RateAcq - ProbAcqT) * NegPastHCT[ia]/NegNoHCT[ia];
			ProbAcqT = 1.0 - exp(-ProbAcqT);
			ProbAcqU = 1.0 - exp(-ProbAcqU);*/
			ProbAcqT = ProbHIVacq[ia][0];
			ProbAcqU = ProbHIVacq[ia][0];
		}
		else{
			ProbAcqT = ProbHIVacq[ia][0];
			ProbAcqU = ProbHIVacq[ia][0];
		}
		NegNoHCT_E[ia] = NegNoHCT[ia];
		if (VirginInd == 1){
			NegNoHCT_E[ia] *= exp(-TestingRateNegV[ia][Sex]);
			NegPastHCT_E[ia] = NegPastHCT[ia] + NegNoHCT[ia] * (1.0 - exp(-TestingRateNegV[ia][Sex]));
		}
		if(VirginInd==0){
			NegNoHCT_E[ia] *= (1.0 - ProbAcqU) * exp(-TotTestingRateSE[ia][0][Sex]);
			NewHIVgroup[0] = NegNoHCT[ia] * ProbAcqU * exp(-TotTestingRateSE[ia][0][Sex]);
			NegPastHCT_E[ia] = (NegPastHCT[ia] + NegNoHCT[ia] * (1.0 - exp(-
				TotTestingRateSE[ia][0][Sex]))) * (1.0 - ProbAcqT);
			NewHIVgroup[1] = (NegPastHCT[ia] + NegNoHCT[ia] * (1.0 - exp(-
				TotTestingRateSE[ia][0][Sex]))) * ProbAcqT;
			NewHIV[ia] = NewHIVgroup[0] + NewHIVgroup[1];
			if(PrEPorVM==1 && Sex==0 && MSMind==0){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][Risk+1]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][Risk + 1]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][0] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][Risk + 1] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if (PrEPorVM == 1 && MSMind == 1){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][Risk-1]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][Risk - 1]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][0] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][Risk - 1] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1 && Sex==1 && FSWind==0){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][2] - JoinPrEP[ia][Risk+4] - JoinVM[ia][2]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][2] - JoinPrEP[ia][Risk+4] - JoinVM[ia][2]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][2] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][Risk+4] * (1.0 -
					ProbHIVacq[ia][0]);
				RegVM_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinVM[ia][2] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1 && FSWind==1){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][1] - JoinPrEP[ia][4] - JoinVM[ia][1]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][1] - JoinPrEP[ia][4] - JoinVM[ia][1]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][1] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][4] * (1.0 -
					ProbHIVacq[ia][0]);
				RegVM_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinVM[ia][1] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1){
				NegPastHCT_E[ia] += RegHCT[ia] * StopRegHCT * (1.0 - ProbHIVacq[ia][0]) +
					RegPrEP[ia] * StopPrEP * (1.0 - ProbHIVacq[ia][1]);
				RegHCT_E[ia] += RegHCT[ia] * (1.0 - StopRegHCT) * (1.0 - ProbHIVacq[ia][0]);
				RegPrEP_E[ia] += RegPrEP[ia] * (1.0 - StopPrEP) * (1.0 - ProbHIVacq[ia][1]);
				NewHIVgroup[2] = RegHCT[ia] * ProbHIVacq[ia][0] + RegPrEP[ia] * ProbHIVacq[ia][1];
				if(Sex==1){
					NegPastHCT_E[ia] += RegVM[ia] * StopVM * (1.0 - ProbHIVacq[ia][2]);
					RegVM_E[ia] += RegVM[ia] * (1.0 - StopVM) * (1.0 - ProbHIVacq[ia][2]);
					NewHIVgroup[2] += RegVM[ia] * ProbHIVacq[ia][2];
				}
				NewHIV[ia] += NewHIVgroup[2];
			}
			if(Sex==0 && Risk==1){
				Temp1 = (FSWcontactRate[ia]/12.0) * TransmFSWtoM[CircInd*2] * EctopyFactor[ia][0];
				if(MarriedInd==1){
					Temp1 *= FSWcontactMarried;}
				if(ProbHIVacq[ia][0]>0.0){
					NewHIVinClients = NewHIVinClients - NewHIV[ia] * Temp1/log(1.0 - ProbHIVacq[ia][0]);}
			}
		}
		// Calculate transitions for HIV-positive untreated states
		// (a) Acute infections
		if(VirginInd==0){
			PosNoHCT_E[ia][0] = PosNoHCT[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) *
				exp(-PosTesting[0]) + NewHIVgroup[0] * (1.0 - ProbExitAtEntry[ia][0][Sex]);
			PosHCTpreHIV_E[ia][0] = PosHCTpreHIV[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) +
				PosNoHCT[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) * (1.0 - exp(-
				PosTesting[0])) + NewHIVgroup[1] * (1.0 - ProbExitAtEntry[ia][0][Sex]);
			if(PrEPorVM==1){
				PosDiagnosedPreART_E[ia][0] = PosDiagnosedPreART[ia][0] * exp(-
					MnthlyCD4trans[ia][0][Sex]) + NewHIVgroup[2] * (1.0 -
					ProbExitAtEntry[ia][0][Sex]);
			}
		}
		// (b) Post-acute, previously untested
		for(is=1; is<4; is++){
			PosNoHCT_E[ia][is] = PosNoHCT[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]) *
				exp(-PosTesting[is]);}
		PosNoHCT_E[ia][4] = PosNoHCT[ia][4] * exp(-PosTesting[4]);
		for(is=3; is<5; is++){
			PosNoHCT_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		for(is=1; is<4; is++){
			PosNoHCT_E[ia][is] += PosNoHCT[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]) * exp(-PosTesting[is-1]);}
		PosNoHCT_E[ia][4] += PosNoHCT[ia][3] * exp(-PosTesting[3] - MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * (1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosNoHCT_E[ia][1] += NewHIVgroup[0] * ProbExitAtEntry[ia][0][Sex];}
		for(is=2; is<4; is++){
			PosNoHCT_E[ia][is] += PosNoHCT[ia][is-2] * (1.0 - exp(-MnthlyCD4trans[ia][is-2][Sex])) *
				ProbExitAtEntry[ia][is-1][Sex] * exp(-PosTesting[is-2]);}
		PosNoHCT_E[ia][4] += PosNoHCT[ia][2] * (1.0 - exp(-MnthlyCD4trans[ia][2][Sex])) *
			ProbExitAtEntry[ia][3][Sex] * Fraction200 * exp(-PosTesting[2]);
		// (c) Post-acute, last tested HIV-negative
		for(is=1; is<4; is++){
			PosHCTpreHIV_E[ia][is] = PosHCTpreHIV[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]) *
				exp(-PosTesting[is+5]);}
		PosHCTpreHIV_E[ia][4] = PosHCTpreHIV[ia][4] * exp(-PosTesting[9]);
		for(is=3; is<5; is++){
			PosHCTpreHIV_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		PosHCTpreHIV_E[ia][1] += PosHCTpreHIV[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) *
			(1.0 - ProbExitAtEntry[ia][1][Sex]);
		for(is=2; is<4; is++){
			PosHCTpreHIV_E[ia][is] += PosHCTpreHIV[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]) * exp(-PosTesting[is+4]);}
		PosHCTpreHIV_E[ia][4] += PosHCTpreHIV[ia][3] * exp(-PosTesting[8] - MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * (1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosHCTpreHIV_E[ia][1] += NewHIVgroup[1] * ProbExitAtEntry[ia][0][Sex];}
		PosHCTpreHIV_E[ia][2] += PosHCTpreHIV[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) *
			ProbExitAtEntry[ia][1][Sex];
		PosHCTpreHIV_E[ia][3] += PosHCTpreHIV[ia][1] * (1.0 - exp(-MnthlyCD4trans[ia][1][Sex])) *
			ProbExitAtEntry[ia][2][Sex] * exp(-PosTesting[6]);
		PosHCTpreHIV_E[ia][4] += PosHCTpreHIV[ia][2] * (1.0 - exp(-MnthlyCD4trans[ia][2][Sex])) *
			ProbExitAtEntry[ia][3][Sex] * Fraction200 * exp(-PosTesting[7]);
		Temp1 = PosNoHCT[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) * (1.0 - exp(-PosTesting[0]));
		PosHCTpreHIV_E[ia][1] += Temp1 * (1.0 - ProbExitAtEntry[ia][1][Sex]);
		PosHCTpreHIV_E[ia][2] += Temp1 * ProbExitAtEntry[ia][1][Sex];
		// (d) Get numbers recently diagnosed
		if(VirginInd==0){
			for(is=0; is<2; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TotTestingRateSE[ia][is+3][Sex])) *
					(1.0 - HCTtoART_SE[ia][is+1][Sex]/TotTestingRateSE[ia][is+3][Sex]);
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TotTestingRateSE[ia][is+8][Sex])) *
					(1.0 - HCTtoART_SE[ia][is+6][Sex]/TotTestingRateSE[ia][is+8][Sex]);
			}
			for(is=2; is<4; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TotTestingRateSE[ia][is+3][Sex] +
					HCTtoART_SE[ia][is+1][Sex]));
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TotTestingRateSE[ia][is+8][Sex] +
					HCTtoART_SE[ia][is+6][Sex]));
			}
		}
		else{
			for(is=0; is<2; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex])) *
					(1.0 - HCTtoART_V[ia][is+1][Sex]/TestingRateV[ia][is+1][Sex]);
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex])) *
					(1.0 - HCTtoART_V[ia][is+1][Sex]/TestingRateV[ia][is+1][Sex]);
			}
			for(is=2; is<4; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex] +
					HCTtoART_V[ia][is+1][Sex]));
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex] +
					HCTtoART_V[ia][is+1][Sex]));
			}
		}
		NewDiagAcute = NewHIVgroup[2] * ProbExitAtEntry[ia][0][Sex];
		// (e) Post-acute, previously diagnosed HIV-positive
		// (e1) Firstly the people who were previously diagnosed in the relevant CD4 state
		for(is=1; is<4; is++){
			PosDiagnosedPreART_E[ia][is] = PosDiagnosedPreART[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]);
			if (ia<5){ PosDiagnosedPreART_E[ia][is] *= exp(-ARTinitByStage[is][2]); }
			else{
				if (VirginInd == 0){
					PosDiagnosedPreART_E[ia][is] *= exp(-ARTinitByStage[is][Sex] - HCTtoART_SE[ia][10 + is][Sex]);
				}
				else{
					PosDiagnosedPreART_E[ia][is] *= exp(-ARTinitByStage[is][Sex] - HCTtoART_V[ia][is][Sex] * RetestPos);
				}
			}
		}
		PosDiagnosedPreART_E[ia][4] = PosDiagnosedPreART[ia][4];
		if(ia<5){
			PosDiagnosedPreART_E[ia][4] *= exp(-ARTinitByStage[4][2]);}
		else{
			if (VirginInd == 0){
				PosDiagnosedPreART_E[ia][4] *= exp(-ARTinitByStage[4][Sex] - HCTtoART_SE[ia][14][Sex]);
			}
			else{
				PosDiagnosedPreART_E[ia][4] *= exp(-ARTinitByStage[4][Sex] - HCTtoART_V[ia][4][Sex] * RetestPos);
			}
		}
		for(is=3; is<5; is++){
			PosDiagnosedPreART_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		// (e2) Add number previously diagnosed from previous CD4 state
		for(is=1; is<5; is++){
			Temp1 = PosDiagnosedPreART[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]);
			if(is==1){
				Temp2 = Temp1 * (1.0 - AcuteLink);
				NewARTacute = Temp1 * AcuteLink;
				Temp1 = Temp2;
			}
			if (is>1){
				if (ia < 5){ Temp1 *= exp(-ARTinitByStage[is - 1][2]); }
				else{
					if (VirginInd == 0){
						Temp1 *= exp(-ARTinitByStage[is - 1][Sex] - HCTtoART_SE[ia][9 + is][Sex]);
					}
					else{
						Temp1 *= exp(-ARTinitByStage[is - 1][Sex] - HCTtoART_V[ia][is-1][Sex] * RetestPos);
					}
				}
			}
			if(is==4){
				Temp1 *= exp(-MnthlyAIDSmort[ia][0][Sex]);}
			PosDiagnosedPreART_E[ia][is] += Temp1;
		}
		// (e3) Add number previously diagnosed from 2 states prior
		for(is=2; is<5; is++){
			Temp1 = PosDiagnosedPreART[ia][is-2] * (1.0 - exp(-MnthlyCD4trans[ia][is-2][Sex])) *
				ProbExitAtEntry[ia][is-1][Sex];
			if(is==2){
				Temp2 = Temp1 * (1.0 - AcuteLink);
				NewARTacute += Temp1 * AcuteLink;
				Temp1 = Temp2;
			}
			if(is>2){
				if (ia<5){ Temp1 *= exp(-ARTinitByStage[is - 2][2]); }
				else{
					if (VirginInd == 0){
						Temp1 *= exp(-ARTinitByStage[is - 2][Sex] - HCTtoART_SE[ia][8 + is][Sex]);
					}
					else{
						Temp1 *= exp(-ARTinitByStage[is - 2][Sex] - HCTtoART_V[ia][is - 2][Sex] * RetestPos);
					}
				}
			}
			if(is==4){
				Temp1 *= Fraction200;}
			PosDiagnosedPreART_E[ia][is] += Temp1;
		}
		// (e4) Add new HIV diagnoses in the relevant CD4 state
		for(is=1; is<3; is++){
			PosDiagnosedPreART_E[ia][is] += (NewDiag[is-1][0] + NewDiag[is-1][1]) *
				exp(-MnthlyCD4trans[ia][is][Sex]);
		}
		Temp1 = exp(-MnthlyCD4trans[ia][3][Sex] - MnthlyAIDSmort[ia][0][Sex]);
		if(VirginInd==0){
			PosDiagnosedPreART_E[ia][3] += (NewDiag[2][0] * exp(-HCTtoART_SE[ia][3][Sex]) +
				NewDiag[2][1] * exp(-HCTtoART_SE[ia][8][Sex])) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][3] += (NewDiag[2][0] + NewDiag[2][1]) *
				 exp(-HCTtoART_V[ia][3][Sex]) * Temp1;}
		Temp1 = exp(-MnthlyAIDSmort[ia][1][Sex]);
		if(VirginInd==0){
			PosDiagnosedPreART_E[ia][4] += (NewDiag[3][0] * exp(-HCTtoART_SE[ia][4][Sex]) +
				NewDiag[3][1] * exp(-HCTtoART_SE[ia][9][Sex])) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][4] += (NewDiag[3][0] + NewDiag[3][1]) *
				exp(-HCTtoART_V[ia][4][Sex]) * Temp1;}
		// (e5) Add new HIV diagnoses from the previous CD4 state
		PosDiagnosedPreART_E[ia][1] += NewDiagAcute * (1.0 - AcuteLink);
		NewARTacute += NewDiagAcute * AcuteLink;
		for(is=2; is<4; is++){
			PosDiagnosedPreART_E[ia][is] += (NewDiag[is-2][0] + NewDiag[is-2][1]) * (1.0 -
				exp(-MnthlyCD4trans[ia][is-1][Sex])) * (1.0 - ProbExitAtEntry[ia][is][Sex]);
		}
		Temp1 = (1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * exp(-MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosDiagnosedPreART_E[ia][4] += (NewDiag[2][0] * exp(-HCTtoART_SE[ia][3][Sex]) +
				NewDiag[2][1] * exp(-HCTtoART_SE[ia][8][Sex])) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][4] += (NewDiag[2][0] + NewDiag[2][1]) *
				 exp(-HCTtoART_V[ia][3][Sex]) * Temp1;}
		// (e6) Add new HIV diagnoses from 2 states prior
		PosDiagnosedPreART_E[ia][3] += (NewDiag[0][0] + NewDiag[0][1]) * (1.0 -
			exp(-MnthlyCD4trans[ia][1][Sex])) * ProbExitAtEntry[ia][2][Sex];
		PosDiagnosedPreART_E[ia][4] += (NewDiag[1][0] + NewDiag[1][1]) * (1.0 -
			exp(-MnthlyCD4trans[ia][2][Sex])) * ProbExitAtEntry[ia][3][Sex] *
			Fraction200;
		// (f) Number starting ART
		for(is=0; is<2; is++){
			StartingART[ia][is] = PosNoHCT[ia][is+1] * (1.0 - exp(-PosTesting[is+1])) *
				(TestingToART[is+1]/PosTesting[is+1]) + PosHCTpreHIV[ia][is+1] *
				(1.0 - exp(-PosTesting[is+6])) * (TestingToART[is+6]/PosTesting[is+6]);
			if (VirginInd == 0){
				if (ia < 5){
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][2]));
				}
				else{
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][Sex] - HCTtoART_SE[ia][11 + is][Sex]));
				}
			}
			else{
				if (ia < 5){
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][2]));
				}
				else{
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][Sex] - HCTtoART_V[ia][1 + is][Sex] * RetestPos));
				}
			}
		}
		StartingART[ia][0] += NewARTacute;
		for(is=2; is<4; is++){
			StartingART[ia][is] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingToART[is+1] -
				MnthlyAIDSmort[ia][is-2][Sex])) * TestingToART[is+1]/(TestingToART[is+1] +
				MnthlyAIDSmort[ia][is-2][Sex]) + PosHCTpreHIV[ia][is+1] * (1.0 - exp(-
				TestingToART[is+6] - MnthlyAIDSmort[ia][is-2][Sex])) * TestingToART[is+6]/
				(TestingToART[is+6] + MnthlyAIDSmort[ia][is-2][Sex]);
			if (ia<5){
				StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
					ARTinitByStage[is + 1][2] - MnthlyAIDSmort[ia][is - 2][Sex])) *
					ARTinitByStage[is + 1][2] / (ARTinitByStage[is + 1][2] + MnthlyAIDSmort[ia][is - 2][Sex]);
			}
			else{
				if (VirginInd == 0){
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][Sex] - HCTtoART_SE[ia][11 + is][Sex] - MnthlyAIDSmort[ia][is - 2][Sex])) *
						(ARTinitByStage[is + 1][Sex] + HCTtoART_SE[ia][11 + is][Sex]) / (ARTinitByStage[is + 1][Sex] +
						HCTtoART_SE[ia][11 + is][Sex] + MnthlyAIDSmort[ia][is - 2][Sex]);
				}
				else{
					StartingART[ia][is] += PosDiagnosedPreART[ia][is + 1] * (1.0 - exp(-
						ARTinitByStage[is + 1][Sex] - HCTtoART_V[ia][1 + is][Sex] * RetestPos - MnthlyAIDSmort[ia][is - 2][Sex])) *
						(ARTinitByStage[is + 1][Sex] + HCTtoART_V[ia][1 + is][Sex] * RetestPos) / (ARTinitByStage[is + 1][Sex] +
						HCTtoART_V[ia][1 + is][Sex] * RetestPos + MnthlyAIDSmort[ia][is - 2][Sex]);
				}
			}
		}
		// (g) Number who started ART prior to current month
		if(Sex==0){
			for(id=0; id<5; id++){
				OnARTpre500_E[ia][id] = OnARTpre500[ia][id] * exp(-MortByARTdurM[ia][0][id]);
				OnART500_E[ia][id] = OnART500[ia][id] * exp(-MortByARTdurM[ia][1][id]);
				OnART350_E[ia][id] = OnART350[ia][id] * exp(-MortByARTdurM[ia][2][id]);
				OnART200_E[ia][id] = OnART200[ia][id] * exp(-MortByARTdurM[ia][3][id]);
			}
		}
		else{
			for(id=0; id<5; id++){
				OnARTpre500_E[ia][id] = OnARTpre500[ia][id] * exp(-MortByARTdurF[ia][0][id]);
				OnART500_E[ia][id] = OnART500[ia][id] * exp(-MortByARTdurF[ia][1][id]);
				OnART350_E[ia][id] = OnART350[ia][id] * exp(-MortByARTdurF[ia][2][id]);
				OnART200_E[ia][id] = OnART200[ia][id] * exp(-MortByARTdurF[ia][3][id]);
			}
		}
		// (h) Add number who started ART in current month
		OnARTpre500_E[ia][0] += StartingART[ia][0] * (1.0 - MortStartART[ia][0][Sex]);
		OnART500_E[ia][0] += StartingART[ia][1] * (1.0 - MortStartART[ia][1][Sex]);
		OnART350_E[ia][0] += StartingART[ia][2] * (1.0 - MortStartART[ia][2][Sex]);
		OnART200_E[ia][0] += StartingART[ia][3] * (1.0 - MortStartART[ia][3][Sex]);
		// (i) Patients who have stopped ART but are still alive
		// Code still to be added
		// (j) Calculate AIDS deaths and total survivors
		Total_E[ia] = NegNoHCT_E[ia] + NegPastHCT_E[ia] + RegHCT_E[ia] + RegPrEP_E[ia] +
			RegVM_E[ia];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(is=0; is<5; is++){
			Temp1 += PosNoHCT[ia][is] + PosHCTpreHIV[ia][is] + PosDiagnosedPreART[ia][is];
			Temp2 += PosNoHCT_E[ia][is] + PosHCTpreHIV_E[ia][is] + PosDiagnosedPreART_E[ia][is];
		}
		Total_E[ia] += Temp2;
		TempART = 0.0;
		for(is=0; is<4; is++){
			TempART += StartingART[ia][is];}
		AIDSdeathsUntreated[ia] = Temp1 + NewHIV[ia] - Temp2 - TempART;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(id=0; id<5; id++){
			Temp1 += OnARTpre500[ia][id] + OnART500[ia][id] + OnART350[ia][id] + OnART200[ia][id];
			Temp2 += OnARTpre500_E[ia][id] + OnART500_E[ia][id] + OnART350_E[ia][id] + OnART200_E[ia][id];
		}
		for(is=0; is<4; is++){
			Temp1 += StoppedART[ia][is];
			Temp2 += StoppedART_E[ia][is];
		}
		Total_E[ia] += Temp2;
		AIDSdeathsTreated[ia] = Temp1 + TempART - Temp2;
	}

	// Calculate totals
	SumNewART();
	if(FixedUncertainty==1){
		if(FSWind==1){
			Temp1 = 0.0;
			for(ia=0; ia<81; ia++){
				Temp1 += NewHIV[ia];}
			NewHIVinFSW.out[CurrSim-1][CurrYear-1985] += Temp1;
		}
		if (MSMind == 1){
			Temp1 = 0.0;
			for (ia = 5; ia<40; ia++){
				Temp1 += NewHIV[ia];}
			HIVincMSM.out[CurrSim - 1][CurrYear - 1985] += Temp1;
		}
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 81; ia++){
			for (is = 0; is < 2; is++){
				Temp1 += (PosNoHCT[ia][is + 3] + PosHCTpreHIV[ia][is + 3]) * (1.0 - exp(-MnthlyAIDSmort[ia][is][Sex]));
				Temp2 += PosDiagnosedPreART[ia][is + 3] * (1.0 - exp(-MnthlyAIDSmort[ia][is][Sex]));
			}
		}
		AIDSdeathsUndiag.out[CurrSim - 1][CurrYear - StartYear] += Temp1;
		AIDSdeathsDiagPreART.out[CurrSim - 1][CurrYear - StartYear] += Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 81; ia++){
			if (Sex == 0){
				Temp1 += OnARTpre500[ia][0] * (1.0 - exp(-MortByARTdurM[ia][0][0])) +
					OnART500[ia][0] * (1.0 - exp(-MortByARTdurM[ia][1][0])) +
					OnART350[ia][0] * (1.0 - exp(-MortByARTdurM[ia][2][0])) +
					OnART200[ia][0] * (1.0 - exp(-MortByARTdurM[ia][3][0]));
				for (id = 1; id < 5; id++){
					Temp2 += OnARTpre500[ia][id] * (1.0 - exp(-MortByARTdurM[ia][0][id])) +
						OnART500[ia][id] * (1.0 - exp(-MortByARTdurM[ia][1][id])) +
						OnART350[ia][id] * (1.0 - exp(-MortByARTdurM[ia][2][id])) +
						OnART200[ia][id] * (1.0 - exp(-MortByARTdurM[ia][3][id]));
				}
			}
			else{
				Temp1 += OnARTpre500[ia][0] * (1.0 - exp(-MortByARTdurF[ia][0][0])) +
					OnART500[ia][0] * (1.0 - exp(-MortByARTdurF[ia][1][0])) +
					OnART350[ia][0] * (1.0 - exp(-MortByARTdurF[ia][2][0])) +
					OnART200[ia][0] * (1.0 - exp(-MortByARTdurF[ia][3][0]));
				for (id = 1; id < 5; id++){
					Temp2 += OnARTpre500[ia][id] * (1.0 - exp(-MortByARTdurF[ia][0][id])) +
						OnART500[ia][id] * (1.0 - exp(-MortByARTdurF[ia][1][id])) +
						OnART350[ia][id] * (1.0 - exp(-MortByARTdurF[ia][2][id])) +
						OnART200[ia][id] * (1.0 - exp(-MortByARTdurF[ia][3][id]));
				}
			}
		}
		AIDSdeaths1st6moART.out[CurrSim - 1][CurrYear - StartYear] += Temp1;
		AIDSdeathsAfter6moART.out[CurrSim - 1][CurrYear - StartYear] += Temp2;
	}
}

/*void Adult::GetEndProfileFSW()
{
	int ia, is, id, UpperAge;
	double StopRegHCT, StopPrEP, StopVM, NewHIVgroup[3], PosTesting[10], TestingToART[10];
	double Fraction200, Temp1, Temp2, TempART, NewDiag[4][2], NewDiagAcute, NewARTacute;
	double ProbAcqT, ProbAcqU, RateAcq, UntestedPropn; // Variables new to THEMBISA v4

	if(VirginInd==0){
		UpperAge = 81;}
	else{
		UpperAge = 20;}
	if(PrEPorVM==1){
		StopRegHCT = 1.0 - exp(-1.0/(12.0 * RegHCTdur[Sex]));
		StopPrEP = 1.0 - exp(-1.0/(12.0 * PrEPdur[Sex]));
		StopVM = 1.0 - exp(-1.0/(12.0 * VMdur));
	}
	else{
		StopRegHCT = 0.0;
		StopPrEP = 0.0;
		StopVM = 0.0;
	}
	NewHIVgroup[0] = 0.0;
	NewHIVgroup[1] = 0.0;
	NewHIVgroup[2] = 0.0;

	for(ia=0; ia<UpperAge; ia++){
		// Calculate positive testing rates and Fraction200
		if(VirginInd==1){
			for(is=0; is<5; is++){
				PosTesting[is] = TestingRateV[ia][is][Sex];
				PosTesting[is+5] = TestingRateV[ia][is][Sex];
				TestingToART[is] = HCTtoART_V[ia][is][Sex];
				TestingToART[is+5] = HCTtoART_V[ia][is][Sex];
			}
		}
		else{
			for(is=0; is<10; is++){
				PosTesting[is] = 2.0/12.0;
				TestingToART[is] = (2.0/12.0)*ImmART_CSW;
			}
		}
		Fraction200 = MnthlyCD4trans[ia][3][Sex]/(MnthlyCD4trans[ia][3][Sex] +
			MnthlyAIDSmort[ia][0][Sex]);
		// Calculate transitions for HIV-negative states and new infections
		UntestedPropn = NegNoHCT[ia]/(NegNoHCT[ia] + NegPastHCT[ia]);
		if(UntestedPropn<1.0 && UntestedPropn>0.0 && ProbHIVacq[ia][0]<1.0){
			RateAcq = -log(1.0 - ProbHIVacq[ia][0]);
			ProbAcqT = RateAcq * (1.0 - pow(UntestedPropn, HIVeffectVCT))/
				(1.0 - UntestedPropn);
			ProbAcqU = RateAcq + (RateAcq - ProbAcqT) * NegPastHCT[ia]/NegNoHCT[ia];
			ProbAcqT = 1.0 - exp(-ProbAcqT);
			ProbAcqU = 1.0 - exp(-ProbAcqU);
		}
		else{
			ProbAcqT = ProbHIVacq[ia][0];
			ProbAcqU = ProbHIVacq[ia][0];
		}
		NegNoHCT_E[ia] = NegNoHCT[ia];
		if(VirginInd==0){
			NegNoHCT_E[ia] *= (1.0 - ProbAcqU) * exp(-2.0/12.0);
			NewHIVgroup[0] = NegNoHCT[ia] * ProbAcqU * exp(-2.0/12.0);
			NegPastHCT_E[ia] = (NegPastHCT[ia] + NegNoHCT[ia] * (1.0 - exp(-
				2.0/12.0))) * (1.0 - ProbAcqT);
			NewHIVgroup[1] = (NegPastHCT[ia] + NegNoHCT[ia] * (1.0 - exp(-
				2.0/12.0))) * ProbAcqT;
			NewHIV[ia] = NewHIVgroup[0] + NewHIVgroup[1];
			if(PrEPorVM==1 && Sex==0){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][0]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][0] - JoinPrEP[ia][0]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][0] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][0] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1 && Sex==1 && FSWind==0){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][2] - JoinPrEP[ia][2] - JoinVM[ia][2]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][2] - JoinPrEP[ia][2] - JoinVM[ia][2]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][2] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][2] * (1.0 -
					ProbHIVacq[ia][0]);
				RegVM_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinVM[ia][2] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1 && FSWind==1){
				NegNoHCT_E[ia] *= (1.0 - JoinRegHCT[ia][1] - JoinPrEP[ia][1] - JoinVM[ia][1]);
				NegPastHCT_E[ia] *= (1.0 - JoinRegHCT[ia][1] - JoinPrEP[ia][1] - JoinVM[ia][1]);
				RegHCT_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinRegHCT[ia][1] * (1.0 -
					ProbHIVacq[ia][0]);
				RegPrEP_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinPrEP[ia][1] * (1.0 -
					ProbHIVacq[ia][0]);
				RegVM_E[ia] = (NegNoHCT[ia] + NegPastHCT[ia]) * JoinVM[ia][1] * (1.0 -
					ProbHIVacq[ia][0]);
			}
			if(PrEPorVM==1){
				NegPastHCT_E[ia] += RegHCT[ia] * StopRegHCT * (1.0 - ProbHIVacq[ia][0]) +
					RegPrEP[ia] * StopPrEP * (1.0 - ProbHIVacq[ia][1]);
				RegHCT_E[ia] += RegHCT[ia] * (1.0 - StopRegHCT) * (1.0 - ProbHIVacq[ia][0]);
				RegPrEP_E[ia] += RegPrEP[ia] * (1.0 - StopPrEP) * (1.0 - ProbHIVacq[ia][1]);
				NewHIVgroup[2] = RegHCT[ia] * ProbHIVacq[ia][0] + RegPrEP[ia] * ProbHIVacq[ia][1];
				if(Sex==1){
					NegPastHCT_E[ia] += RegVM[ia] * StopVM * (1.0 - ProbHIVacq[ia][2]);
					RegVM_E[ia] += RegVM[ia] * (1.0 - StopVM) * (1.0 - ProbHIVacq[ia][2]);
					NewHIVgroup[2] += RegVM[ia] * ProbHIVacq[ia][2];
				}
				NewHIV[ia] += NewHIVgroup[2];
			}
			if(Sex==0 && Risk==1){
				Temp1 = (FSWcontactRate[ia]/12.0) * TransmFSWtoM[CircInd*2] * EctopyFactor[ia][0];
				if(MarriedInd==1){
					Temp1 *= FSWcontactMarried;}
				if(ProbHIVacq[ia][0]>0.0){
					NewHIVinClients = NewHIVinClients - NewHIV[ia] * Temp1/log(1.0 - ProbHIVacq[ia][0]);}
			}
		}
		// Calculate transitions for HIV-positive untreated states
		// (a) Acute infections
		if(VirginInd==0){
			PosNoHCT_E[ia][0] = PosNoHCT[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) *
				exp(-PosTesting[0]) + NewHIVgroup[0] * (1.0 - ProbExitAtEntry[ia][0][Sex]);
			PosHCTpreHIV_E[ia][0] = PosHCTpreHIV[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) +
				PosNoHCT[ia][0] * exp(-MnthlyCD4trans[ia][0][Sex]) * (1.0 - exp(-
				PosTesting[0])) + NewHIVgroup[1] * (1.0 - ProbExitAtEntry[ia][0][Sex]);
			if(PrEPorVM==1){
				PosDiagnosedPreART_E[ia][0] = PosDiagnosedPreART[ia][0] * exp(-
					MnthlyCD4trans[ia][0][Sex]) + NewHIVgroup[2] * (1.0 -
					ProbExitAtEntry[ia][0][Sex]);
			}
		}
		// (b) Post-acute, previously untested
		for(is=1; is<4; is++){
			PosNoHCT_E[ia][is] = PosNoHCT[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]) *
				exp(-PosTesting[is]);}
		PosNoHCT_E[ia][4] = PosNoHCT[ia][4] * exp(-PosTesting[4]);
		if(ia<5){
			PosNoHCT_E[ia][4] *= exp(-PaedARTinitiation);}
		for(is=3; is<5; is++){
			PosNoHCT_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		for(is=1; is<4; is++){
			PosNoHCT_E[ia][is] += PosNoHCT[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]) * exp(-PosTesting[is-1]);}
		PosNoHCT_E[ia][4] += PosNoHCT[ia][3] * exp(-PosTesting[3] - MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * (1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosNoHCT_E[ia][1] += NewHIVgroup[0] * ProbExitAtEntry[ia][0][Sex];}
		for(is=2; is<4; is++){
			PosNoHCT_E[ia][is] += PosNoHCT[ia][is-2] * (1.0 - exp(-MnthlyCD4trans[ia][is-2][Sex])) *
				ProbExitAtEntry[ia][is-1][Sex] * exp(-PosTesting[is-2]);}
		PosNoHCT_E[ia][4] += PosNoHCT[ia][2] * (1.0 - exp(-MnthlyCD4trans[ia][2][Sex])) *
			ProbExitAtEntry[ia][3][Sex] * Fraction200 * exp(-PosTesting[2]);
		// (c) Post-acute, last tested HIV-negative
		for(is=1; is<4; is++){
			PosHCTpreHIV_E[ia][is] = PosHCTpreHIV[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]) *
				exp(-PosTesting[is+5]);}
		PosHCTpreHIV_E[ia][4] = PosHCTpreHIV[ia][4] * exp(-PosTesting[9]);
		if(ia<5){
			PosHCTpreHIV_E[ia][4] *= exp(-PaedARTinitiation);}
		for(is=3; is<5; is++){
			PosHCTpreHIV_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		PosHCTpreHIV_E[ia][1] += PosHCTpreHIV[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) *
			(1.0 - ProbExitAtEntry[ia][1][Sex]);
		for(is=2; is<4; is++){
			PosHCTpreHIV_E[ia][is] += PosHCTpreHIV[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]) * exp(-PosTesting[is+4]);}
		PosHCTpreHIV_E[ia][4] += PosHCTpreHIV[ia][3] * exp(-PosTesting[8] - MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * (1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosHCTpreHIV_E[ia][1] += NewHIVgroup[1] * ProbExitAtEntry[ia][0][Sex];}
		PosHCTpreHIV_E[ia][2] += PosHCTpreHIV[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) *
			ProbExitAtEntry[ia][1][Sex];
		PosHCTpreHIV_E[ia][3] += PosHCTpreHIV[ia][1] * (1.0 - exp(-MnthlyCD4trans[ia][1][Sex])) *
			ProbExitAtEntry[ia][2][Sex] * exp(-PosTesting[6]);
		PosHCTpreHIV_E[ia][4] += PosHCTpreHIV[ia][2] * (1.0 - exp(-MnthlyCD4trans[ia][2][Sex])) *
			ProbExitAtEntry[ia][3][Sex] * Fraction200 * exp(-PosTesting[7]);
		Temp1 = PosNoHCT[ia][0] * (1.0 - exp(-MnthlyCD4trans[ia][0][Sex])) * (1.0 - exp(-PosTesting[0]));
		PosHCTpreHIV_E[ia][1] += Temp1 * (1.0 - ProbExitAtEntry[ia][1][Sex]);
		PosHCTpreHIV_E[ia][2] += Temp1 * ProbExitAtEntry[ia][1][Sex];
		// (d) Get numbers recently diagnosed
		if(VirginInd==0){
			for(is=0; is<2; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-2.0/12.0)) *
					(1.0 - ImmART_CSW);
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-2.0/12.0)) *
					(1.0 - ImmART_CSW);
			}
			for(is=2; is<4; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-2.0/12.0 +
					(2.0/12.0)*ImmART_CSW));
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-2.0/12.0 +
					(2.0/12.0)*ImmART_CSW));
			}
		}
		else{
			for(is=0; is<2; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex])) *
					(1.0 - HCTtoART_V[ia][is+1][Sex]/TestingRateV[ia][is+1][Sex]);
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+3][Sex])) *
					(1.0 - HCTtoART_V[ia][is+1][Sex]/TestingRateV[ia][is+3][Sex]);
			}
			for(is=2; is<4; is++){
				NewDiag[is][0] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex] +
					HCTtoART_V[ia][is+1][Sex]));
				NewDiag[is][1] = PosHCTpreHIV[ia][is+1] * (1.0 - exp(-TestingRateV[ia][is+1][Sex] +
					HCTtoART_V[ia][is+1][Sex]));
			}
		}
		NewDiagAcute = NewHIVgroup[2] * ProbExitAtEntry[ia][0][Sex];
		// (e) Post-acute, previously diagnosed HIV-positive
		// (e1) Firstly the people who were previously diagnosed in the relevant CD4 state
		for(is=1; is<4; is++){
			PosDiagnosedPreART_E[ia][is] = PosDiagnosedPreART[ia][is] * exp(-MnthlyCD4trans[ia][is][Sex]);
			if(ia>=5){
				if(CurrYear==2015 && CurrMonth==0 && is<=ImmARTstage){
					PosDiagnosedPreART_E[ia][is] *= (1.0 - ImmART_CSW) * exp(-HCTtoART_SE[ia][10+is][Sex]);}
				else{
					PosDiagnosedPreART_E[ia][is] *= exp(-ARTinitByStage[is][Sex] - HCTtoART_SE[ia][10+is][Sex]);}
			}
		}
		PosDiagnosedPreART_E[ia][4] = PosDiagnosedPreART[ia][4];
		if(ia<5){
			PosDiagnosedPreART_E[ia][4] *= exp(-PaedARTinitiation);}
		else{
			PosDiagnosedPreART_E[ia][4] *= exp(-ARTinitByStage[4][Sex] - HCTtoART_SE[ia][10+is][Sex]);}
		for(is=3; is<5; is++){
			PosDiagnosedPreART_E[ia][is] *= exp(-MnthlyAIDSmort[ia][is-3][Sex]);}
		// (e2) Add number previously diagnosed from previous CD4 state
		for(is=1; is<5; is++){
			Temp1 = PosDiagnosedPreART[ia][is-1] * (1.0 - exp(-MnthlyCD4trans[ia][is-1][Sex])) *
				(1.0 - ProbExitAtEntry[ia][is][Sex]);
			if(is==1){
				Temp2 = Temp1 * (1.0 - ImmART_CSW);
				NewARTacute = Temp1 * ImmART_CSW;
				Temp1 = Temp2;
			}
			if(ia>=5 && is>1){
				if(CurrYear==2015 && CurrMonth==0 && is<=ImmARTstage){
					Temp1 *= (1.0 - ImmART_CSW) * exp(-HCTtoART_SE[ia][9+is][Sex]);}
				else{
					Temp1 *= exp(-ARTinitByStage[is-1][Sex] - HCTtoART_SE[ia][9+is][Sex]);}
			}
			if(is==4){
				Temp1 *= exp(-MnthlyAIDSmort[ia][0][Sex]);}
			PosDiagnosedPreART_E[ia][is] += Temp1;
		}
		// (e3) Add number previously diagnosed from 2 states prior
		for(is=2; is<5; is++){
			Temp1 = PosDiagnosedPreART[ia][is-2] * (1.0 - exp(-MnthlyCD4trans[ia][is-2][Sex])) *
				ProbExitAtEntry[ia][is-1][Sex];
			if(is==2){
				Temp2 = Temp1 * (1.0 - ImmART_CSW);
				NewARTacute += Temp1 * ImmART_CSW;
				Temp1 = Temp2;
			}
			if(ia>=5 && is>2){
				if(CurrYear==2015 && CurrMonth==0){
					Temp1 *= (1.0 - ImmART_CSW) * exp(-HCTtoART_SE[ia][8+is][Sex]);}
				else{
					Temp1 *= exp(-ARTinitByStage[is-2][Sex] - HCTtoART_SE[ia][8+is][Sex]);}
			}
			if(is==4){
				Temp1 *= Fraction200;}
			PosDiagnosedPreART_E[ia][is] += Temp1;
		}
		// (e4) Add new HIV diagnoses in the relevant CD4 state
		for(is=1; is<3; is++){
			PosDiagnosedPreART_E[ia][is] += (NewDiag[is-1][0] + NewDiag[is-1][1]) *
				exp(-MnthlyCD4trans[ia][is][Sex]);
		}
		Temp1 = exp(-MnthlyCD4trans[ia][3][Sex] - MnthlyAIDSmort[ia][0][Sex]);
		if(VirginInd==0){
			PosDiagnosedPreART_E[ia][3] += (NewDiag[2][0] * exp(-(2.0/12.0)*ImmART_CSW) +
				NewDiag[2][1] * exp(-(2.0/12.0)*ImmART_CSW)) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][3] += (NewDiag[2][0] + NewDiag[2][1]) *
				 exp(-HCTtoART_V[ia][3][Sex]) * Temp1;}
		Temp1 = exp(-MnthlyAIDSmort[ia][1][Sex]);
		if(VirginInd==0 && ia>=5){
			PosDiagnosedPreART_E[ia][4] += (NewDiag[3][0] * exp(-(2.0/12.0)*ImmART_CSW) +
				NewDiag[3][1] * exp(-(2.0/12.0)*ImmART_CSW)) * Temp1;}
		else if(VirginInd==1 && ia>=5){
			PosDiagnosedPreART_E[ia][4] += (NewDiag[3][0] + NewDiag[3][1]) *
				exp(-HCTtoART_V[ia][4][Sex]) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][4] += (NewDiag[3][0] + NewDiag[3][1]) *
				exp(-PaedARTinitiation) * Temp1;}
		// (e5) Add new HIV diagnoses from the previous CD4 state
		PosDiagnosedPreART_E[ia][1] += NewDiagAcute * (1.0 - ImmART_CSW);
		NewARTacute += NewDiagAcute * ImmART_CSW;
		for(is=2; is<4; is++){
			PosDiagnosedPreART_E[ia][is] += (NewDiag[is-2][0] + NewDiag[is-2][1]) * (1.0 -
				exp(-MnthlyCD4trans[ia][is-1][Sex])) * (1.0 - ProbExitAtEntry[ia][is][Sex]);
		}
		Temp1 = (1.0 - exp(-MnthlyCD4trans[ia][3][Sex])) * exp(-MnthlyAIDSmort[ia][0][Sex]) *
			(1.0 - ProbExitAtEntry[ia][4][Sex]);
		if(VirginInd==0){
			PosDiagnosedPreART_E[ia][4] += (NewDiag[2][0] * exp(-(2.0/12.0)*ImmART_CSW) +
				NewDiag[2][1] * exp(-(2.0/12.0)*ImmART_CSW)) * Temp1;}
		else{
			PosDiagnosedPreART_E[ia][4] += (NewDiag[2][0] + NewDiag[2][1]) *
				 exp(-HCTtoART_V[ia][3][Sex]) * Temp1;}
		// (e6) Add new HIV diagnoses from 2 states prior
		PosDiagnosedPreART_E[ia][3] += (NewDiag[0][0] + NewDiag[0][1]) * (1.0 -
			exp(-MnthlyCD4trans[ia][1][Sex])) * ProbExitAtEntry[ia][2][Sex];
		PosDiagnosedPreART_E[ia][4] += (NewDiag[1][0] + NewDiag[1][1]) * (1.0 -
			exp(-MnthlyCD4trans[ia][2][Sex])) * ProbExitAtEntry[ia][3][Sex] *
			Fraction200;
		// (f) Number starting ART
		for(is=0; is<2; is++){
			StartingART[ia][is] = PosNoHCT[ia][is+1] * (1.0 - exp(-PosTesting[is+1])) *
				(TestingToART[is+1]/PosTesting[is+1]) + PosHCTpreHIV[ia][is+1] *
				(1.0 - exp(-PosTesting[is+6])) * (TestingToART[is+6]/PosTesting[is+6]);
			if(ia>=5){
				if(CurrYear==2015 && CurrMonth==0 && is<ImmARTstage){
					StartingART[ia][is] += PosDiagnosedPreART[ia][is+1] * (1.0 - (1.0 -
						ImmART_CSW) * exp(-HCTtoART_SE[ia][11+is][Sex]));}
				else{
					StartingART[ia][is] += PosDiagnosedPreART[ia][is+1] * (1.0 - exp(-
						ARTinitByStage[is+1][Sex] - HCTtoART_SE[ia][11+is][Sex]));}
			}
		}
		StartingART[ia][0] += NewARTacute;
		for(is=2; is<4; is++){
			StartingART[ia][is] = PosNoHCT[ia][is+1] * (1.0 - exp(-TestingToART[is+1] -
				MnthlyAIDSmort[ia][is-2][Sex])) * TestingToART[is+1]/(TestingToART[is+1] +
				MnthlyAIDSmort[ia][is-2][Sex]) + PosHCTpreHIV[ia][is+1] * (1.0 - exp(-
				TestingToART[is+6] - MnthlyAIDSmort[ia][is-2][Sex])) * TestingToART[is+6]/
				(TestingToART[is+6] + MnthlyAIDSmort[ia][is-2][Sex]);
			if(ia>=5){
				if(CurrYear==2015 && CurrMonth==0 && is<ImmARTstage){
				StartingART[ia][is] += PosDiagnosedPreART[ia][is+1] * (1.0 - (1.0 - ImmART_CSW) *
					exp(-HCTtoART_SE[ia][11+is][Sex])) * exp(-MnthlyAIDSmort[ia][is-2][Sex]);}
				else{
				StartingART[ia][is] += PosDiagnosedPreART[ia][is+1] * (1.0 - exp(-
					ARTinitByStage[is+1][Sex] - HCTtoART_SE[ia][11+is][Sex] - MnthlyAIDSmort[ia][is-2][Sex])) *
					(ARTinitByStage[is+1][Sex] + HCTtoART_SE[ia][11+is][Sex])/(ARTinitByStage[is+1][Sex] +
					HCTtoART_SE[ia][11+is][Sex] + MnthlyAIDSmort[ia][is-2][Sex]);}
			}
			if(ia<5 && is==3){
				StartingART[ia][is] = (PosNoHCT[ia][is+1] + PosHCTpreHIV[ia][is+1] +
					PosDiagnosedPreART[ia][is+1]) * (1.0 - exp(-PaedARTinitiation -
					MnthlyAIDSmort[ia][is-2][Sex])) * PaedARTinitiation/(PaedARTinitiation +
					MnthlyAIDSmort[ia][is-2][Sex]);
			}
		}
		// (g) Number who started ART prior to current month
		if(Sex==0){
			for(id=0; id<5; id++){
				OnARTpre500_E[ia][id] = OnARTpre500[ia][id] * exp(-MortByARTdurM[ia][0][id]);
				OnART500_E[ia][id] = OnART500[ia][id] * exp(-MortByARTdurM[ia][1][id]);
				OnART350_E[ia][id] = OnART350[ia][id] * exp(-MortByARTdurM[ia][2][id]);
				OnART200_E[ia][id] = OnART200[ia][id] * exp(-MortByARTdurM[ia][3][id]);
			}
		}
		else{
			for(id=0; id<5; id++){
				OnARTpre500_E[ia][id] = OnARTpre500[ia][id] * exp(-MortByARTdurF[ia][0][id]);
				OnART500_E[ia][id] = OnART500[ia][id] * exp(-MortByARTdurF[ia][1][id]);
				OnART350_E[ia][id] = OnART350[ia][id] * exp(-MortByARTdurF[ia][2][id]);
				OnART200_E[ia][id] = OnART200[ia][id] * exp(-MortByARTdurF[ia][3][id]);
			}
		}
		// (h) Add number who started ART in current month
		OnARTpre500_E[ia][0] += StartingART[ia][0] * (1.0 - MortStartART[ia][0][Sex]);
		OnART500_E[ia][0] += StartingART[ia][1] * (1.0 - MortStartART[ia][1][Sex]);
		OnART350_E[ia][0] += StartingART[ia][2] * (1.0 - MortStartART[ia][2][Sex]);
		OnART200_E[ia][0] += StartingART[ia][3] * (1.0 - MortStartART[ia][3][Sex]);
		// (i) Patients who have stopped ART but are still alive
		// Code still to be added
		// (j) Calculate AIDS deaths and total survivors
		Total_E[ia] = NegNoHCT_E[ia] + NegPastHCT_E[ia] + RegHCT_E[ia] + RegPrEP_E[ia] +
			RegVM_E[ia];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(is=0; is<5; is++){
			Temp1 += PosNoHCT[ia][is] + PosHCTpreHIV[ia][is] + PosDiagnosedPreART[ia][is];
			Temp2 += PosNoHCT_E[ia][is] + PosHCTpreHIV_E[ia][is] + PosDiagnosedPreART_E[ia][is];
		}
		Total_E[ia] += Temp2;
		TempART = 0.0;
		for(is=0; is<4; is++){
			TempART += StartingART[ia][is];}
		AIDSdeathsUntreated[ia] = Temp1 + NewHIV[ia] - Temp2 - TempART;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(id=0; id<5; id++){
			Temp1 += OnARTpre500[ia][id] + OnART500[ia][id] + OnART350[ia][id] + OnART200[ia][id];
			Temp2 += OnARTpre500_E[ia][id] + OnART500_E[ia][id] + OnART350_E[ia][id] + OnART200_E[ia][id];
		}
		for(is=0; is<4; is++){
			Temp1 += StoppedART[ia][is];
			Temp2 += StoppedART_E[ia][is];
		}
		Total_E[ia] += Temp2;
		AIDSdeathsTreated[ia] = Temp1 + TempART - Temp2;
	}

	// Calculate totals
	SumNewART();
	if(FixedUncertainty==1){
		if(FSWind==1){
			Temp1 = 0.0;
			for(ia=0; ia<81; ia++){
				Temp1 += NewHIV[ia];}
			NewHIVinFSW.out[CurrSim-1][CurrYear-1985] += Temp1;
		}
	}
}*/

void Adult::SumNewART()
{
	int ia, is;

	for(ia=0; ia<4; ia++){
		NewARTbyAge[ia] = 0.0;}
	for(is=0; is<4; is++){
		NewARTbyCD4[is] = 0.0;}

	for(ia=0; ia<5; ia++){
		for(is=0; is<4; is++){
			PaedNewARTbyAge[ia+10] += StartingART[ia][is];}
	}
	for(ia=5; ia<15; ia++){
		for(is=0; is<4; is++){
			NewARTbyAge[0] += StartingART[ia][is];
			NewARTbyCD4[is] += StartingART[ia][is];
		}
	}
	for(ia=15; ia<25; ia++){
		for(is=0; is<4; is++){
			NewARTbyAge[1] += StartingART[ia][is];
			NewARTbyCD4[is] += StartingART[ia][is];
		}
	}
	for(ia=25; ia<35; ia++){
		for(is=0; is<4; is++){
			NewARTbyAge[2] += StartingART[ia][is];
			NewARTbyCD4[is] += StartingART[ia][is];
		}
	}
	for(ia=35; ia<81; ia++){
		for(is=0; is<4; is++){
			NewARTbyAge[3] += StartingART[ia][is];
			NewARTbyCD4[is] += StartingART[ia][is];
		}
	}
}

void Adult::UpdateStartProfile()
{
	int ia, is, UpperLimit;

	if(VirginInd==1){
		UpperLimit = 20;}
	else{
		UpperLimit = 81;}

	for(ia=0; ia<UpperLimit; ia++){
		NegNoHCT[ia] = NegNoHCT_E[ia];
		NegPastHCT[ia] = NegPastHCT_E[ia];
		if(PrEPorVM==1){
			RegHCT[ia] = RegHCT_E[ia];
			RegPrEP[ia] = RegPrEP_E[ia];
			RegVM[ia] = RegVM_E[ia];
		}
		for(is=0; is<5; is++){
			PosNoHCT[ia][is] = PosNoHCT_E[ia][is];
			PosHCTpreHIV[ia][is] = PosHCTpreHIV_E[ia][is];
			PosDiagnosedPreART[ia][is] = PosDiagnosedPreART_E[ia][is];
			OnARTpre500[ia][is] = OnARTpre500_E[ia][is];
			OnART500[ia][is] = OnART500_E[ia][is];
			OnART350[ia][is] = OnART350_E[ia][is];
			OnART200[ia][is] = OnART200_E[ia][is];
		}
		for(is=0; is<4; is++){
			StoppedART[ia][is] = StoppedART_E[ia][is];}
		Total[ia] = Total_E[ia];
	}
}

void Adult::UpdateStartTotal()
{
	int ia, is, UpperLimit;
	double Temp;

	if(VirginInd==1){
		UpperLimit = 20;}
	else{
		UpperLimit= 81;}

	for(ia=0; ia<UpperLimit; ia++){
		Temp = NegNoHCT[ia] + NegPastHCT[ia] + RegHCT[ia] + RegPrEP[ia] + RegVM[ia];
		for(is=0; is<5; is++){
			Temp += PosNoHCT[ia][is] + PosHCTpreHIV[ia][is] + PosDiagnosedPreART[ia][is] +
				OnARTpre500[ia][is] + OnART500[ia][is] + OnART350[ia][is] +
				OnART200[ia][is];
		}
		for(is=0; is<4; is++){
			Temp += StoppedART[ia][is];}
		Total[ia] = Temp;
	}
}

void Adult::UpdateDemog()
{
	int ia, ib, is, UpperLimit;
	double mort, migration[2], DemogAdj[2], DemogAdj2[2], Prev, PrevAdj;

	if(VirginInd==1){
		UpperLimit = 21;}
	else{
		UpperLimit = 81;}

	for(ia=1; ia<UpperLimit; ia++){
		ib = UpperLimit - ia - 1; // Age at START of year
		mort = CurrNonAIDSmortALB[ib+10][Sex];
		Prev = TotalPositive[ib + 10][Sex] / TotalPop[ib + 10][Sex];
		if (CurrYear <= 1995){ PrevAdj = MigrationHIVadj[ib + 11][0]; }
		else if (CurrYear <= 2000){
			PrevAdj = MigrationHIVadj[ib + 11][0] + (MigrationHIVadj[ib + 11][1] -
				MigrationHIVadj[ib + 11][0]) * (CurrYear - 1995)/5.0;
		}
		else if (CurrYear <= 2010){
			PrevAdj = MigrationHIVadj[ib + 11][1] + (MigrationHIVadj[ib + 11][2] -
				MigrationHIVadj[ib + 11][1]) * (CurrYear - 2000) / 10.0;
		}
		else{ PrevAdj = MigrationHIVadj[ib + 11][2]; }
		if (Prev < 0.9999){
			migration[0] = MigrationAdj[ib + 11][Sex] * (1.0 - Prev * PrevAdj) / (1.0 - Prev);
			migration[1] = MigrationAdj[ib + 11][Sex] * PrevAdj;
		}
		else{
			migration[0] = MigrationAdj[ib + 11][Sex];
			migration[1] = MigrationAdj[ib + 11][Sex];
		}
		DemogAdj[0] = migration[0] - mort;
		DemogAdj[1] = migration[1] - mort;
		if (DemogAdj[0] <= 0.0){ DemogAdj[0] = 0.00001; } // To prevent divide-by-zero/NAN errors
		if (DemogAdj[1] <= 0.0){ DemogAdj[1] = 0.00001; }
		if(ib<79){
			NegNoHCT[ib + 1] = NegNoHCT[ib] * DemogAdj[0];
			NegPastHCT[ib + 1] = NegPastHCT[ib] * DemogAdj[0];
			if(PrEPorVM==1){
				RegHCT[ib + 1] = RegHCT[ib] * DemogAdj[0];
				RegPrEP[ib + 1] = RegPrEP[ib] * DemogAdj[0];
				RegVM[ib + 1] = RegVM[ib] * DemogAdj[0];
			}
			for(is=0; is<5; is++){
				PosNoHCT[ib + 1][is] = PosNoHCT[ib][is] * DemogAdj[1];
				PosHCTpreHIV[ib + 1][is] = PosHCTpreHIV[ib][is] * DemogAdj[1];
				PosDiagnosedPreART[ib + 1][is] = PosDiagnosedPreART[ib][is] * DemogAdj[1];
			}
			OnARTpre500[ib + 1][4] = (OnARTpre500[ib][4] + OnARTpre500[ib][3]) * DemogAdj[1];
			OnART500[ib + 1][4] = (OnART500[ib][4] + OnART500[ib][3]) * DemogAdj[1];
			OnART350[ib + 1][4] = (OnART350[ib][4] + OnART350[ib][3]) * DemogAdj[1];
			OnART200[ib + 1][4] = (OnART200[ib][4] + OnART200[ib][3]) * DemogAdj[1];
			for(is=0; is<3; is++){
				// is is now the number of durations prior to final duration
				OnARTpre500[ib + 1][3 - is] = OnARTpre500[ib][2 - is] * DemogAdj[1];
				OnART500[ib + 1][3 - is] = OnART500[ib][2 - is] * DemogAdj[1];
				OnART350[ib + 1][3 - is] = OnART350[ib][2 - is] * DemogAdj[1];
				OnART200[ib + 1][3 - is] = OnART200[ib][2 - is] * DemogAdj[1];
			}
			OnARTpre500[ib+1][0] = 0.0;
			OnART500[ib+1][0] = 0.0;
			OnART350[ib+1][0] = 0.0;
			OnART200[ib+1][0] = 0.0;
			for(is=0; is<4; is++){
				StoppedART[ib + 1][is] = StoppedART[ib][is] * DemogAdj[1];}
		}
		if(ib==79){
			DemogAdj2[0] = MigrationAdj[90][Sex] * (1.0 - Prev * PrevAdj) / (1.0 - Prev) - CurrNonAIDSmortALB[90][Sex];
			DemogAdj2[1] = MigrationAdj[90][Sex] * PrevAdj - CurrNonAIDSmortALB[90][Sex];
			if (DemogAdj2[0] <= 0.0){ DemogAdj2[0] = 0.00001; }
			if (DemogAdj2[1] <= 0.0){ DemogAdj2[1] = 0.00001; }
			NegNoHCT[80] = NegNoHCT[79] * DemogAdj[0] + NegNoHCT[80] * DemogAdj2[0];
			NegPastHCT[80] = NegPastHCT[79] * DemogAdj[0] + NegPastHCT[80] * DemogAdj2[0];
			if(PrEPorVM==1){
				RegHCT[80] = RegHCT[79] * DemogAdj[0] + RegHCT[80] * DemogAdj2[0];
				RegPrEP[80] = RegPrEP[79] * DemogAdj[0] + RegPrEP[80] * DemogAdj2[0];
				RegVM[80] = RegVM[79] * DemogAdj[0] + RegVM[80] * DemogAdj2[0];
			}
			for(is=0; is<5; is++){
				PosNoHCT[80][is] = PosNoHCT[79][is] * DemogAdj[1] + PosNoHCT[80][is] * DemogAdj2[1];
				PosHCTpreHIV[80][is] = PosHCTpreHIV[79][is] * DemogAdj[1] + PosHCTpreHIV[80][is] * DemogAdj2[1];
				PosDiagnosedPreART[80][is] = PosDiagnosedPreART[79][is] * DemogAdj[1] +
					PosDiagnosedPreART[80][is] * DemogAdj2[1];
			}
			OnARTpre500[80][4] = (OnARTpre500[79][4] + OnARTpre500[79][3]) * DemogAdj[1] +
				(OnARTpre500[80][4] + OnARTpre500[80][3]) * DemogAdj2[1];
			OnART500[80][4] = (OnART500[79][4] + OnART500[79][3]) * DemogAdj[1] +
				(OnART500[80][4] + OnART500[80][3]) * DemogAdj2[1];
			OnART350[80][4] = (OnART350[79][4] + OnART350[79][3]) * DemogAdj[1] +
				(OnART350[80][4] + OnART350[80][3]) * DemogAdj2[1];
			OnART200[80][4] = (OnART200[79][4] + OnART200[79][3]) * DemogAdj[1] +
				(OnART200[80][4] + OnART200[80][3]) * DemogAdj2[1];
			for(is=0; is<3; is++){
				// is is now the number of durations prior to final duration
				OnARTpre500[80][3 - is] = OnARTpre500[79][2 - is] * DemogAdj[1] + OnARTpre500[80][2 - is] * DemogAdj2[1];
				OnART500[80][3 - is] = OnART500[79][2 - is] * DemogAdj[1] + OnART500[80][2 - is] * DemogAdj2[1];
				OnART350[80][3 - is] = OnART350[79][2 - is] * DemogAdj[1] + OnART350[80][2 - is] * DemogAdj2[1];
				OnART200[80][3 - is] = OnART200[79][2 - is] * DemogAdj[1] + OnART200[80][2 - is] * DemogAdj2[1];
			}
			OnARTpre500[80][0] = 0.0;
			OnART500[80][0] = 0.0;
			OnART350[80][0] = 0.0;
			OnART200[80][0] = 0.0;
			for(is=0; is<4; is++){
				StoppedART[80][is] = StoppedART[79][is] * DemogAdj[1] + StoppedART[80][is] * DemogAdj2[1];}
		}
	}

	// Update 10-year olds
	NegNoHCT[0] = 0.0;
	NegPastHCT[0] = 0.0;
	if(PrEPorVM==1){
		RegHCT[0] = 0.0;
		RegPrEP[0] = 0.0;
		RegVM[0] = 0.0;
	}
	for(is=0; is<5; is++){
		PosNoHCT[0][is] = 0.0;
		PosHCTpreHIV[0][is] = 0.0;
		PosDiagnosedPreART[0][is] = 0.0;
	}
	for(is=0; is<5; is++){
		OnARTpre500[0][is] = 0.0;
		OnART500[0][is] = 0.0;
		OnART350[0][is] = 0.0;
		OnART200[0][is] = 0.0;
	}
	for(is=0; is<4; is++){
		StoppedART[0][is] = 0.0;}
}

void Adult::GetFSWcontacts()
{
	int ia, is;
	double Temp;

	for(ia=0; ia<81; ia++){
		Temp = NegNoHCT[ia] + NegPastHCT[ia] + RegHCT[ia] + RegPrEP[ia];
		for(is=0; is<5; is++){
			Temp += PosNoHCT[ia][is] * RelativeCoit[is][0] +
				PosHCTpreHIV[ia][is] * RelativeCoit[is + 5][0] +
				PosDiagnosedPreART[ia][is] * RelativeCoit[is + 10][0] +
				OnARTpre500[ia][is] * RelativeCoit[is + 15][0] +
				OnART500[ia][is] * RelativeCoit[is + 20][0] +
				OnART350[ia][is] * RelativeCoit[is + 25][0] +
				OnART200[ia][is] * RelativeCoit[is + 30][0];
		}
		for(is=0; is<4; is++){
			Temp += StoppedART[ia][is] * RelativeCoit[is + 35][0];
		}
		FSWcontactsByAge[ia] = Temp * FSWcontactRate[ia];
		if(MarriedInd==1){
			FSWcontactsByAge[ia] *= FSWcontactMarried;}
	}
}

OutputArray::OutputArray(int n)
{
	columns = n;
}

void OutputArray::Record(const char* filout, int n)
{
	int i, c;
	std::ofstream file(filout);

	for(i=0; i<InitSample; i++){
		file<<std::setw(6)<<std::right<<i;
		for(c=0; c<columns; c++){
			file<<"	"<<std::setw(10)<<std::right<<out[i][c];}
		file<<std::endl;
	}
	file.close();
}

void OutputArray::RecordSample(const char* filout, int n)
{
	int i, c;
	std::ofstream file(filout);

	for(i=0; i<ResampleSize; i++){
		file<<std::setw(6)<<std::right<<i<<"	"<<std::setw(6)<<std::right<<SampleID[i];
		for(c=0; c<columns; c++){
			file<<"	"<<std::setw(10)<<std::right<<out[i][c];}
		file<<std::endl;
	}
	file.close();
}

void OutputArray::SampleInput()
{
	int i, c;

	for(i=0; i<ResampleSize; i++){
		for(c=0; c<columns; c++){
			temp[i][c] = out[SampleID[i]][c];}
	}
	for(i=0; i<ResampleSize; i++){
		for(c=0; c<columns; c++){
			out[i][c] = temp[i][c];}
	}
}

PostOutputArray::PostOutputArray(int n)
{
	columns = n;
}

void PostOutputArray::RecordSample(const char* filout)
{
	int i, c;
	std::ofstream file(filout);

	for(i=0; i<ResampleSize; i++){
		file<<std::setw(6)<<std::right<<i<<"	"<<std::setw(6)<<std::right<<SampleID[i];
		for(c=0; c<columns; c++){
			file << "	" << std::setw(10) << std::right << out[i][c];
		}
		file<<std::endl;
	}
	file.close();
}

void PostOutputArray::GetMeans()
{
	int iy, ic;
	int i, j, k, offset1, offset2;
	double Temp;

	for (iy = 0; iy <= CurrYear - StartYear; iy++){
		Temp = 0.0;
		for (ic = 0; ic < ResampleSize; ic++){ Temp += out[ic][iy]; }
		Means[iy][0] = Temp / ResampleSize;
	}

	// Get percentiles
	// Copied from GetPercentile function
	offset1 = ResampleSize * 0.025;
	offset2 = ResampleSize * 0.975;
	for (iy = 0; iy <= CurrYear - StartYear; iy++){
		if (out[0][iy] <= 0.0 || out[0][iy] > 0.0){
			sorted[0] = out[0][iy];
			for (i = 1; i < ResampleSize; i++){
				if (out[i][iy] < sorted[0]){
					for (j = i; j > 0; j--){ sorted[j] = sorted[j - 1]; }
					sorted[0] = out[i][iy];
				}
				else if (out[i][iy] >= sorted[i - 1]){
					sorted[i] = out[i][iy];
				}
				else for (j = 0; j < i - 1; j++){ // Previously j < ResampleSize
					if (out[i][iy] >= sorted[j] && out[i][iy] < sorted[j + 1]){
						for (k = i; k > j + 1; k--){
							sorted[k] = sorted[k - 1];
						}
						sorted[j + 1] = out[i][iy];
						break;
					}
				}
			}
			Means[iy][1] = sorted[offset1];
			Means[iy][2] = sorted[offset2];
		}
		else{ // Undefined values, so outputs cannot be ordered
			Means[iy][0] = 0.0;
			Means[iy][1] = 0.0;
			Means[iy][2] = 0.0;
		}
	}

	// Store results
	for (ic = 0; ic < 3; ic++){
		for (iy = 0; iy < 116; iy++){
			SummaryOutputs[ic + SummOutRow][iy] = Means[iy][ic];
		}
	}
	SummOutRow += 3;
}

OutputByAge::OutputByAge(int n1, int n2)
{
	int ic, ir;

	columns = n2;
	rows = n1;

	for (ir = 0; ir < rows; ir++){
		for (ic = 0; ic < columns; ic++){
			out[ir][ic] = 0.0;
		}
	}
}

void OutputByAge::GetMeans()
{
	int iy, ic;

	// Store results
	for (ic = 0; ic < rows; ic++){
		for (iy = 0; iy <= CurrYear - StartYear; iy++){
			SummaryOutputs[SummOutRow][iy] = out[ic][iy] / ResampleSize;;
		}
		SummOutRow += 1;
	}
}

void ReadPaedAssumps()
{
	std::ifstream file;
	int ia, ic;
	std::string InputFile = "PaedAssumps.txt";

	if (ProvModel == 1){ InputFile = "PaedAssumps" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
		std::cerr << "Could not open PaedAssumps.txt\n";
		exit(1);
	}

	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>CD4transm[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmAcute;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmBFfirst3;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmBFafter3;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRforEBF;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmBFacute;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FirstANCwk;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DeliveryWk;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRprogressionPostnatal;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ProgToNeedLT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ExcessProgToNeed;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ExcessProgRedn;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ProgAdjNoPMTCT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AIDSmortLT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ExcessAIDSmort;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ExcessMortRedn;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ProvAdjPaedMort;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRmortART1;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRmortART2;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>EarlyARTmort;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRearlyARTmort;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ARTinterruptionPaed;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ARTresumptionPaed;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Discontinuation1;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Discontinuation2;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRearlyPaedART;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ImmARTcorrectionP;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UltARTdelayC;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MinARTdelayC;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>InterpolationStart;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UltimateYr;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> MinMortP[0] >> MinMortP[1] >> MinMortP[2];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RednLogMortP[0] >> RednLogMortP[1] >> RednLogMortP[2];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SwitchPaed[0] >> SwitchPaed[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>IMRshape;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Sensitivity;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Window;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>NVPefficacy;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DualEfficacy;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AZTefficacy;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmART200;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmARTpre200;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmARTprePreg;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRtransmPerWeekART;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MeanARTdurPreg;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SD_ARTdurPreg;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>NVPuptake;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AZTpropnAdj;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RescreenWk;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RescreenUptake;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UntestedRescreen;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RednExtNVP;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RednHAARTduringPreg;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>RednHAARTbeforePreg;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRtestVirginTrend[0] >> RRtestVirginTrend[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRtestVirgin;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> VirginTestAdjProv;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRtestPaedAdvanced;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRtestAgePaed;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RetestPosP;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> BirthSe[0]>>BirthSe[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SixWeekSe[0]>>SixWeekSe[1]>>SixWeekSe[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> BaseARTuptakeEID;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>EverFeed[0]>>EverFeed[1]>>EverFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MedianFeed[0]>>MedianFeed[1]>>MedianFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ShapeFeed[0]>>ShapeFeed[1]>>ShapeFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>BFadjProv;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> AbruptWeaningFirst3;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>AbruptWeaningAfter3;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<24; ia++){
		file>>NoBFmortAdj[ia];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>BFbiasAdj;
	file.close();

	InitTransmART200 = TransmART200;
	InitTransmARTpre200 = TransmARTpre200;
	InitAbruptWeaning1 = AbruptWeaningFirst3;
	InitAbruptWeaning2 = AbruptWeaningAfter3;
	PaedARTuptakeEID = BaseARTuptakeEID * ImmARTcorrectionP;

	/*CD4transm[0] = 0.0;
	CD4transm[1] = 0.0;
	CD4transm[2] = 0.0;
	CD4transm[3] = 0.0;
	TransmAcute = 0.0;
	TransmBFacute = 0.0;
	TransmBFfirst3 = 0.0;
	TransmBFafter3 = 0.0;
	TransmART200 = 0.0;
	TransmARTpre200 = 0.0;
	TransmARTprePreg = 0.0;
	InitTransmART200 = 0.0;
	InitTransmARTpre200 = 0.0;*/

}

void ReadAdultAssumps()
{
	std::ifstream file;
	int ic, id, ig, ii, ij;
	std::string InputFile = "AdultAssumps2.txt";

	if (ProvModel == 1){ InputFile = "AdultAssumps" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open AdultAssumps.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	file>>HighRiskPropn[0]>>HighRiskPropn[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>PartnerRate20F;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRpartnerLow[0]>>RRpartnerLow[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRpartnerMarried[0]>>RRpartnerMarried[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>GammaMeanF>>GammaSDF;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DebutMedian[0]>>DebutMedian[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DebutLow[0]>>DebutLow[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DebutShape[0]>>DebutShape[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MeanAgeDif>>SDageDif;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MeanAgeDifLT>>SDageDifLT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Assortativeness;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FSWageMean>>FSWageSD;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ClientsPA;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>DurFSW;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FSWcontactAge21;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FSWcontactMarried;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FSWcontactMean>>FSWcontactSD;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SexActsST;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SexActsLT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RelFreqSexAge;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AgeEffectCondom;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRcondomMarital;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ORcondomFSW;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>InitCondom;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CondomBias;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> CondomAdjProv;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> RRcondomContracep;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> IncrCondomBCC;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> RiskCompensation;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MedianCondomBCC;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ShapeCondomBCC;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>VCTcondom;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> VCT_FSWentry;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>ARTcondom;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>HIVeffectSex[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>HIVeffectFSWentry[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>InitFSWprev;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmST[0]>>TransmST[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmLT[0]>>TransmLT[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>TransmFSW[0]>>TransmFSW[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> RRclientToFSW1985;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>EctopyEffect[0]>>EctopyEffect[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ii=0; ii<2; ii++){
		for(ij=0; ij<2; ij++){
			for(ig=0; ig<2; ig++){
				file>>RelInfecRiskST[ii][ij][ig];}
		}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ii=0; ii<2; ii++){
		for(ij=0; ij<2; ij++){
			for(ig=0; ig<2; ig++){
				file>>RelInfecRiskLT[ii][ij][ig];}
		}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<5; ic++){
		file>>RelInfecStage[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MedianVLuntreated200;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>ShapeVL;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>VLeffectInfectivity;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>VLdifPer100CD4;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ORsuppressionIeDEA;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic<4; ic++){
		file >> ORsuppressionCD4[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CondomEfficacy;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> InfToVirulenceRatio;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CD4decline[0]>>CD4decline[1]>>CD4decline[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CD4duration[0];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRmort200to350;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CD4mort[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<6; ic++){
		file>>OIincidence[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<5; ic++){
		file>>WHO4incidence[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<5; ic++){
		file>>PTBincidence[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<5; ic++){
		file>>RR_ARTinitiation[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>Haemodilution[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRper10yr;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRperCalYr;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRuntreatedMortF;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MinMort[0]>>MinMort[1]>>MinMort[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RednLogMort[0]>>RednLogMort[1]>>RednLogMort[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> IeDEAbias[0] >> IeDEAbias[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> ARTinterruptionRate;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRinterruptionM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ARTresumptionMin[0] >> ARTresumptionMin[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RR_ARTstartM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RR_ARTstart1stMo;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> COVIDimpactARTstart;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>UltARTdelay[0]>>UltARTdelay[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MinARTdelay[0]>>MinARTdelay[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ARTdataYr;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UltARTyr[0]>>UltARTyr[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRper10yrART[0]>>RRper10yrART[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ig=0; ig<2; ig++){
		for(id=0; id<5; id++){
			for(ic=0; ic<4; ic++){
				file>>AnnHIVmortART[id][ic][ig];}
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic<4; ic++){
		file >> SwitchAdult[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>VCTmale2002>>VCTmale2010;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>VCTage[0]>>VCTage[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>QuadParam[0]>>QuadParam[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RetestAdj>>RetestAdjMax;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> RetestPos;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RetestART;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>HIVeffectVCT;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>OItoTBtestingRatio;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ORdiagOItreat;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RapidDiagSp;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FreqRegHCT[0]>>FreqRegHCT[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RegHCTdur[0]>>RegHCTdur[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SelfTestConfirm;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic<6; ic++){
		file >> SelfTestDataYr[ic];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic<6; ic++){
		file >> SelfTestUptakeUlt[ic];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic<6; ic++){
		file >> SelfTestWastage[ic];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> STageTaxi[0] >> STageTaxi[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig<2; ig++){
		for (ic = 0; ic<2; ic++){
			file >> STageWork[ic][ig];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> STtaxiMtoFratio;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> STworkMtoFratio[0] >> STworkMtoFratio[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig<2; ig++){
		for (ic = 0; ic<10; ic++){
			file >> EmployedPropn[ic][ig];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RetestPosST[0] >> RetestPosST[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ORpartnerPosMetareg[0] >> ORpartnerPosMetareg[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> DiscloseAndTest;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CircPrevBirth;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CircPrevUlt;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MedianCirc;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ShapeCirc;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MCefficacy;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MMCdataYear;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UltMMCprob;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>UltMMCyear;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>PrEPefficacy[0]>>PrEPefficacy[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file >> PrEPefficacyMSM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file>>CondomRednPrEP[0]>>CondomRednPrEP[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FreqHCTinPrEP[0]>>FreqHCTinPrEP[1];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>PrEPdur[0]>>PrEPdur[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> PrEPdataYr;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> UltPrEPrateFSW;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RR_PrEPlowPreg;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MicrobicideEff;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>CondomRednVM;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>FreqHCTinVM;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>VMdur;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RecencyBiasANC;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRfertHIV;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic < 5; ic++){
		file >> RRfertCD4[ic];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRfertDiag;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRfertART;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SexRatio;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MSMpropn;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MSMpartnersM20;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RednMSMpartnersM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MeanAgeDifMSM25;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ChangeAgeDifMSM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SDageDifMSM;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MtoM_ST;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> InitMSMprevRatio;
	file.close();

	// Perform basic calculations in "Adult assumptions" sheet
	FSWageLambda = (FSWageMean - 10.0)/(FSWageSD * FSWageSD);
	FSWageAlpha = (FSWageMean - 10.0) * FSWageLambda;
	FSWcontactLambda = (FSWcontactMean - 10.0)/(FSWcontactSD * FSWcontactSD);
	FSWcontactAlpha = (FSWcontactMean - 10.0) * FSWcontactLambda;
	for(ic=0; ic<4; ic++){
		HIVeffectFSWexit[ic] = 1.0/(1.0 - HIVeffectFSWentry[ic]);}
	CD4duration[1] = (1.0/CD4decline[0]) - CD4duration[0];
	CD4duration[2] = 1.0/CD4decline[1];
	CD4duration[3] = 1.0/CD4decline[2];
	CD4mort[0] = CD4mort[1] * RRmort200to350;
	RetestAdjInit = RetestAdj;
	RetestPosInit = RetestPos;
	RetestARTinit = RetestART;
	ARTresumptionRate[0] = ARTresumptionMin[0];
	ARTresumptionRate[1] = ARTresumptionMin[1];
}

void ReadRollout()
{
	int iy, ig;
	std::ifstream file;
	std::string InputFile = "Rollout.txt";

	if (ProvModel == 1){ InputFile = "Rollout" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open Rollout.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>HCT1stTimeF25[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> NumbersTested[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> NumbersTested5to14[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>OIsDiagnosed[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCT_FSW[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCT_15[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCT_20[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCT_25[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCT_50[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RegHCTpregnant[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>PCR6week[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> PCRbirth[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> TestingAt18mo[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> HBCTuptake[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 6; ig++){
		for (iy = 0; iy < 116; iy++){ file >> SelfTestTotals[iy][ig]; }
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>PregnantWomenTested[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>AZTrollout[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RescreenPropnLate[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>RescreenPropnImm[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>ExtNVProllout[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligibleOptionB[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>NoBFpropn[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> IncreaseARTdurPreg[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>NumStartingART_M[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>NumStartingART_F[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>NumStartingART_P[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for (iy = 0; iy<116; iy++){
		file >> RateARTstartF[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RateARTstartC[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_ARTinterruption[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for(iy=0; iy<116; iy++){
		file>>EligiblePTB350[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligiblePTBpre350[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligibleWHO3[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligiblePreg350[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligiblePregPre350[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligibleAsym350[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligibleAsym500[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>EligibleAsymPre500[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> EligibleInfants[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> EarlyART1to4[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> EarlyART5to14[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>MatARTuptake[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>OI_ARTuptake[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>HCT_ARTuptake[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>PaedARTuptake[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file>>VLsuppression200[iy];}
	for (iy = 0; iy<116; iy++){
		file >> VLsuppressionPaed[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>TotStartingPrEP[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_PrEPstartMSM[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_PrEPstartF20[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for (iy = 0; iy<116; iy++){
		file >> PrEPeligMSM[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for (iy = 0; iy<116; iy++){
		file >> PrEPeligAGYW[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for (iy = 0; iy<116; iy++){
		file >> PrEPeligOther[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>PrEPpregnant[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VM_FSW[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VM_15[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VM_20[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VM_25[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VM_50[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>VMpregnant[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<116; iy++){
		file>>MMCoperations[iy];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_MMCpromo10[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_MMCpromo15[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_MMCpromo20[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_MMCpromo25[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> RR_MMCpromo50[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> NeonatalMMC[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> CondomFSWreduction[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> CondomSTreduction[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<116; iy++){
		file >> CondomLTreduction[iy];
	}
	file.close();

	for (iy = 0; iy < 116; iy++){
		HCT1stTimeF25init[iy] = HCT1stTimeF25[iy];
		OIsTested[iy] = OIsDiagnosed[iy];
		if (CalibARTtotals == 1 || CalibARTtotalsP == 1){
			if (NumStartingART_M[iy] == 0){ MalePropnART[iy] = 0.0; }
			else{ MalePropnART[iy] = NumStartingART_M[iy] / (NumStartingART_M[iy] +
				NumStartingART_F[iy]); }
			TotBeginART[iy] = NumStartingART_P[iy] + NumStartingART_M[iy] +
				NumStartingART_F[iy];
		}
	}
	if (InputARTinitiationRates == 1 && PropnalImmART == 1){
		for (iy = 0; iy < 116; iy++){
			HCT_ARTuptake[iy] = RateARTstartF[iy] * RR_ARTstart1stMo;
			PaedARTuptake[iy] = RateARTstartC[iy] * RR_ARTstart1stMo;
		}
	}
}

void ReadStartingPop()
{
	int ia;
	std::ifstream file;
	std::string InputFile = "StartingPop.txt";

	if (ProvModel == 1){ InputFile = "StartingPop" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open StartingPop.txt\n";
      exit(1);
    }

	for(ia=0; ia<91; ia++){
		file>>StartingPop[ia][0]>>StartingPop[ia][1];}
	file.close();
}

void ReadNonHIVmort()
{
	int ia, iy;
	std::ifstream file;
	std::string InputFile = "Non-HIVmort.txt";

	if (ProvModel == 1){ InputFile = "Non-HIVmort" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open Non-HIVmort.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(ia=0; ia<91; ia++){
		for(iy=0; iy<37; iy++){
			file>>NonAIDSmortM[ia][iy];}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<91; ia++){
		for(iy=0; iy<37; iy++){
			file>>NonAIDSmortF[ia][iy];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<91; ia++){
		file >> UltNonAIDSmort[ia][0] >> UltNonAIDSmort[ia][1];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<91; ia++){
		file >> RednNonAIDSmort[ia][0] >> RednNonAIDSmort[ia][1];
	}
	file.close();
}

void ReadWestTable()
{
	int ia, iy, idum;
	std::ifstream file;

	file.open("WestLevel26.txt");
	if (file.fail()) {
		std::cerr << "Could not open WestLevel26.txt\n";
		exit(1);
	}

	file.ignore(255, '\n');
	for (ia = 0; ia<11; ia++){
		file >> idum >> WestLifeExpectP[ia][0] >> WestLifeExpectP[ia][1];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<81; ia++){
		file >> idum >> WestLifeExpectA[ia][0] >> WestLifeExpectA[ia][1];
	}
	file.close();
}

void ReadMarriage()
{
	int ia, ig;
	std::ifstream file;
	std::string InputFile = "Marriage.txt";

	if (ProvModel == 1){ InputFile = "Marriage" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open Marriage.txt\n";
      exit(1);
    }

	for(ia=0; ia<76; ia++){
		file>>MarriageRate[ia][0]>>MarriageRate[ia][1]>>
			DivorceRate[ia][0]>>DivorceRate[ia][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia<15; ia++){ file >> MarriageData[ia][ig][0]; }
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia<15; ia++){ file >> MarriageData[ia][ig][1]; }
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia<15; ia++){ file >> MarriageData[ia][ig][2]; }
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia<15; ia++){ file >> MarriageData[ia][ig][3]; }
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MarriageConstant[0] >> MarriageConstant[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MarriageTrend[0] >> MarriageTrend[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MarriageShape[0] >> MarriageShape[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> MarriageMin[0] >> MarriageMin[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> DivorceAdj;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> DivorceTrend;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> ORremarriage[0] >> ORremarriage[1];
	file.close();
}

void ReadCD4dbn()
{
	int ic, id;
	std::ifstream file;

	file.open("CD4dbn.txt");
	if (file.fail()) {
      std::cerr << "Could not open CD4dbn.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		for(id=0; id<6; id++){
			file>>AveCD4byARTdur[ic][id];}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		for(id=0; id<6; id++){
			file>>CoV_CD4byARTdur[ic][id];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (id = 0; id<6; id++){
		file >> RatioMaxToMinCD4[id];}
	file.close();

	for (id = 0; id<6; id++){
		InitAveCD4byARTdur[id] = AveCD4byARTdur[0][id]; }
}

void ReadFertility()
{
	int ia, iy;
	std::ifstream file;
	std::string InputFile = "Fertility.txt";

	if (ProvModel == 1){ InputFile = "Fertility" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open Fertility.txt\n";
      exit(1);
    }

	for(ia=0; ia<35; ia++){
		for(iy=0; iy<36; iy++){
			file>>ObservedFert[ia][iy];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<35; ia++){
		file >> UltFert[ia];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<35; ia++){
		file >> RednFert[ia];
	}
	file.close();
}

void ReadMigration()
{
	int ia, iy;
	std::ifstream file;
	std::string InputFile = "Migration.txt";

	if (ProvModel == 1){ InputFile = "Migration" + ProvID + ".txt"; }
	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open Migration.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(ia=0; ia<91; ia++){
		for(iy=0; iy<38; iy++){
			file>>NetMigrants[ia][iy][0];}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<91; ia++){
		for(iy=0; iy<38; iy++){
			file>>NetMigrants[ia][iy][1];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<91; ia++){
		for (iy = 0; iy<3; iy++){
			file >> MigrationHIVadj[ia][iy];
		}
	}
	file.close();
}

void ReadCalibData()
{
	if((CalibPaedPrev==1 || CalibAdultPrev==1) && ProvModel == 0){
		ReadHIVprevData();}
	if(CalibANCprev==1){
		ReadANCprevData();}
	if (CalibFSWprev == 1){
		ReadCSWprevData();}
	if (CalibMSMprev == 1){
		ReadMSMprevData();}
	if(CalibHCTprev==1){
		ReadHCTprevData();}
	if (CalibHCTprevP == 1 || CalibHCTtotP == 1){
		ReadHCTpaedData();}
	if(CalibDeathsA==1 || CalibDeathsP==1){
		ReadMortData();}
	if (CalibDeathsA == 1 || CalibDeathsP == 1){
		ReadCompleteness();}
	if(CalibHCT_HH==1){
		ReadHCTdata();}
	if(CalibAIDStrend==1 || CalibAIDSage==1){
		ReadAIDScases();}
	if (ProvModel == 1 && (CalibANCprev == 1 || CalibAdultPrev == 1 || CalibPaedPrev == 1)){
		ReadProvHIV();}
	if (CalibARTtotals == 1 || CalibARTtotalsP == 1 || CalibARTbyAgeP == 1 || CalibARTbyAgeP2 == 1 ||
		CalibARTbyAge == 1 || CalibARTcoverage == 1 || CalibCD4atARTstart == 1){
		ReadARTtotals();}
	if (CalibChildPIP == 1){
		ReadChildPIP();}
	// More code to follow for other types of calibration data
}

void ReadHIVprevData()
{
	int ia, idum;
	std::ifstream file;

	file.open("HIVprevData.txt");
	if (file.fail()) {
      std::cerr << "Could not open HIVprevData.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev05[ia][0]>>SEprev05[ia][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev05[ia][1]>>SEprev05[ia][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev08[ia][0]>>SEprev08[ia][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev08[ia][1]>>SEprev08[ia][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>idum>>ObservedPrevU208>>SEprevU208;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev12[ia][0]>>SEprev12[ia][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<12; ia++){
		file>>idum>>ObservedPrev12[ia][1]>>SEprev12[ia][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<9; ia++){
		file >> idum >> ObservedPrev16[ia][0] >> SEprev16[ia][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<9; ia++){
		file >> idum >> ObservedPrev16[ia][1] >> SEprev16[ia][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<12; ia++){
		file >> idum >> ObservedPrev17[ia][0] >> SEprev17[ia][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<12; ia++){
		file >> idum >> ObservedPrev17[ia][1] >> SEprev17[ia][1];
	}
	file.close();
}

void ReadANCprevData()
{
	int ia, iy, idum;
	std::ifstream file;

	file.open("ANCprevData.txt");
	if (file.fail()) {
      std::cerr << "Could not open ANCprevData.txt\n";
      exit(1);
    }

	for(iy=0; iy<27; iy++){
		for(ia=0; ia<5; ia++){
			file>>idum>>idum>>ObservedPrevANC[ia][iy]>>SEprevANC[ia][iy];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<6; ia++){
		file >> ANCageW_init[ia];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<6; ia++){
		file >> ANCageW_change[ia];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SpecificityANC;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRprevPrivateANC;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<7; ia++){
		file >> PropnPregPrivate[ia];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> Miscarriage[0] >> Miscarriage[1];
	file.close();
}

void ReadCSWprevData()
{
	int ir, ic;
	std::ifstream file;

	file.open("CSWstudies.txt");
	if (file.fail()) {
		std::cerr << "Could not open CSWstudies.txt\n";
		exit(1);
	}

	for (ir = 0; ir<29; ir++){
		for (ic = 0; ic<3; ic++){
			file >> CSWstudyDetails[ir][ic];
		}
	}
	file.close();
}

void ReadMSMprevData()
{
	int ir, ic;
	std::ifstream file;

	file.open("MSMstudies.txt");
	if (file.fail()) {
		std::cerr << "Could not open MSMstudies.txt\n";
		exit(1);
	}

	for (ir = 0; ir<17; ir++){
		for (ic = 0; ic<4; ic++){
			file >> MSMstudyDetails[ir][ic];
		}
	}
	file.close();
}

void ReadProvHIV()
{
	int ia, iy, idum;
	std::ifstream file;
	std::string InputFile;

	InputFile = "HIVprevData" + ProvID + ".txt";

	file.open(InputFile);
	if (file.fail()) {
		std::cerr << "Could not open provincial HIVprevData.txt\n";
		exit(1);
	}

	file.ignore(255, '\n');
	for (iy = 0; iy<26; iy++){
		file >> idum >> ObservedProvANC[iy] >> SEprovANC[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<4; iy++){
		file >> idum >> ObservedProvHH_P[iy] >> SEprovHH_P[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<5; iy++){
		file >> idum >> ObservedProvHH[iy][0] >> SEprovHH[iy][0];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<5; iy++){
		file >> idum >> ObservedProvHH[iy][1] >> SEprovHH[iy][1];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 3; iy < 27; iy++){ //3 corresponds to 1994, 24 to 2015, 25 to 2017, 26 to 2019
		for (ia = 0; ia < 5; ia++){
			file >> idum >> idum >> ObservedPrevANC[ia][iy] >> SEprevANC[ia][iy];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<6; ia++){
		file >> ANCageW_init[ia];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<6; ia++){
		file >> ANCageW_change[ia];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SpecificityANC;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> RRprevPrivateANC;
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia < 7; ia++){
		file >> PropnPregPrivate[ia];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> Miscarriage[0] >> Miscarriage[1];
	file.close();
}

void ReadHCTprevData()
{
	int iy, idum;
	std::ifstream file;

	file.open("HCTprevData.txt");
	if (file.fail()) {
		std::cerr << "Could not open HCTprevData.txt\n";
		exit(1);
	}

	for (iy = 0; iy<13; iy++){
		file >> idum >> ObservedPrevHCT[iy] >> SEprevHCT[iy];
	}
	file.close();
}

void ReadHCTpaedData()
{
	int iy, idum;
	std::ifstream file;
	std::string InputFile = "HCTpaedData.txt";

	if (ProvModel == 1){ InputFile = "HCTpaedData" + ProvID + ".txt"; }

	file.open(InputFile);
	if (file.fail()) {
		std::cerr << "Could not open HCTpaedData.txt\n";
		exit(1);
	}

	file.ignore(255, '\n');
	for (iy = 0; iy<6; iy++){
		file >> idum >> ObservedPrevHCT_P[iy] >> SEprevHCT_P[iy];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<5; iy++){
		file >> idum >> RecordedHCT_P[iy] >> RecordedHCT_P_CoV[iy];
	}
	file.close();
}

void ReadMortData()
{
	int ia, iy, ig;
	std::ifstream file;
	std::string InputFile = "MortData.txt";

	if (ProvModel == 1){ InputFile = "MortData" + ProvID + ".txt"; }

	file.open(InputFile);
	if (file.fail()) {
      std::cerr << "Could not open MortData.txt\n";
      exit(1);
    }

	// Male deaths
	file.ignore(255,'\n');
	for(ia=0; ia<4; ia++){
		for(iy=0; iy<20; iy++){
			file>>RecordedDeathsP[ia][iy][0];}
	}
	for(ia=0; ia<16; ia++){
		for(iy=0; iy<20; iy++){
			file>>RecordedDeathsA[ia][iy][0];}
	}

	// Female deaths
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<4; ia++){
		for(iy=0; iy<20; iy++){
			file>>RecordedDeathsP[ia][iy][1];}
	}
	for(ia=0; ia<16; ia++){
		for(iy=0; iy<20; iy++){
			file>>RecordedDeathsA[ia][iy][1];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SElogNonHIVmort;
	if (ProvModel == 1){
		file.ignore(255, '\n');
		file.ignore(255, '\n');
		for (ig = 0; ig<2; ig++){
			for (iy = 0; iy<20; iy++){
				file >> AdjustedCompleteness[iy][ig];
			}
		}
	}

	file.close();
}

void ReadCompleteness()
{
	int ia, iy;
	std::ifstream file;

	file.open("Completeness.txt");
	if (file.fail()) {
		std::cerr << "Could not open Completeness.txt\n";
		exit(1);
	}

	// Male completeness
	file.ignore(255, '\n');
	for (ia = 0; ia<16; ia++){
		for (iy = 0; iy<20; iy++){
			file >> AdultCompleteness[ia][iy][0];
		}
	}

	// Female completeness
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<16; ia++){
		for (iy = 0; iy<20; iy++){
			file >> AdultCompleteness[ia][iy][1];
		}
	}

	// Completeness in boys <15
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<4; ia++){
		for (iy = 0; iy<20; iy++){
			file >> PaedCompleteness[ia][iy][0];
		}
	}

	// Completeness in girls <15
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<4; ia++){
		for (iy = 0; iy<20; iy++){
			file >> PaedCompleteness[ia][iy][1];
		}
	}

	file.close();
}

void ReadHCTdata()
{
	int ia, idum;
	std::ifstream file;

	file.open("HCTdata.txt");
	if (file.fail()) {
      std::cerr << "Could not open HCTdata.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested05[ia][0][0]>>SEtested05[ia][0][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested05[ia][0][1]>>SEtested05[ia][0][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested05[ia][1][0]>>SEtested05[ia][1][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested05[ia][1][1]>>SEtested05[ia][1][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested08[ia][0][0]>>SEtested08[ia][0][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested08[ia][0][1]>>SEtested08[ia][0][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested08[ia][1][0]>>SEtested08[ia][1][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested08[ia][1][1]>>SEtested08[ia][1][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested12[ia][0][0]>>SEtested12[ia][0][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested12[ia][0][1]>>SEtested12[ia][0][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested12[ia][1][0]>>SEtested12[ia][1][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<5; ia++){
		file>>idum>>EverTested12[ia][1][1]>>SEtested12[ia][1][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<4; ia++){
		file >> idum >> EverTested16[ia][0][0] >> SEtested16[ia][0][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<4; ia++){
		file >> idum >> EverTested16[ia][0][1] >> SEtested16[ia][0][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<4; ia++){
		file >> idum >> EverTested16[ia][1][0] >> SEtested16[ia][1][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia < 4; ia++){
		file >> idum >> EverTested16[ia][1][1] >> SEtested16[ia][1][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<5; ia++){
		file >> idum >> EverTested17[ia][0][0] >> SEtested17[ia][0][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<5; ia++){
		file >> idum >> EverTested17[ia][0][1] >> SEtested17[ia][0][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<5; ia++){
		file >> idum >> EverTested17[ia][1][0] >> SEtested17[ia][1][0];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<5; ia++){
		file >> idum >> EverTested17[ia][1][1] >> SEtested17[ia][1][1];}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SeTestingHistory[0] >> SeTestingHistory[1];
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	file >> SpTestingHistory;
	file.close();
}

void ReadAIDScases()
{
	int ia, iy;
	std::ifstream file;

	file.open("AIDScases.txt");
	if (file.fail()) {
      std::cerr << "Could not open AIDScases.txt\n";
      exit(1);
    }

	file.ignore(255,'\n');
	for(iy=0; iy<5; iy++){
		file>>AIDScasesByYr[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<10; ia++){
		file>>AIDScasesProfile[ia][0]>>AIDScasesProfile[ia][1];}
	file.close();
}

void ReadARTtotals()
{
	int iy, ic, ig;
	std::ifstream file;
	std::string InputFile;

	if (ProvModel == 1){ InputFile = "ARTtotals" + ProvID + ".txt"; }
	else{ InputFile = "ARTtotals.txt"; }

	file.open(InputFile);
	if (file.fail()) {
		std::cerr << "Could not open provincial ARTtotals.txt\n";
		exit(1);
	}

	file.ignore(255, '\n');
	for (iy = 0; iy<ARTdataPoints; iy++){
		for (ic = 0; ic < 4; ic++){
			file >> ARTtotals[iy][ic];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy<ARTdataPointsP; iy++){
		for (ic = 0; ic < 4; ic++){
			file >> ARTtotalsP[iy][ic];}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic < 2; ic++){
		for (iy = 0; iy<10; iy++){
			file >> AgeDbnKidsStartingART[iy][ic];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic < 2; ic++){
		for (iy = 0; iy<8; iy++){
			file >> AgeDbnKidsOnART[iy][ic];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic < 9; ic++){
		for (iy = 0; iy<7; iy++){
			file >> AgeDbnAdultsOnART[iy][ic][0];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ic = 0; ic < 9; ic++){
		for (iy = 0; iy<7; iy++){
			file >> AgeDbnAdultsOnART[iy][ic][1];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		file >> ObsCoverage[ig][0] >> SEcoverage[ig][0];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ig = 0; ig < 2; ig++){
		file >> ObsCoverage[ig][1] >> SEcoverage[ig][1];
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (iy = 0; iy < ARTdataPointsM; iy++){
		file >> ARTmale[iy][0] >> ARTmale[iy][1] >> ARTmale[iy][2];
	}
	if (ProvModel == 1){
		if (CalibCD4atARTstart == 1){
			file.ignore(255, '\n');
			file.ignore(255, '\n');
			for (iy = 0; iy < 12; iy++){
				file >> RecordedARTstart[iy][3][0] >> RecordedARTstart[iy][2][0] >>
					RecordedARTstart[iy][1][0] >> RecordedARTstart[iy][0][0];
			}
			file.ignore(255, '\n');
			file.ignore(255, '\n');
			for (iy = 0; iy < 12; iy++){
				file >> RecordedARTstart[iy][3][1] >> RecordedARTstart[iy][2][1] >>
					RecordedARTstart[iy][1][1] >> RecordedARTstart[iy][0][1];
			}
		}
	}
	file.close();
}

void ReadChildPIP()
{
	int iy, ia;
	std::ifstream file;
	std::string InputFile;

	InputFile = "ChildPIP.txt";

	file.open(InputFile);
	if (file.fail()) {
		std::cerr << "Could not open ChildPIP.txt\n";
		exit(1);
	}

	file.ignore(255, '\n');
	for (ia = 0; ia<2; ia++){
		for (iy = 0; iy < 13; iy++){
			file >> ChildPIPdeaths[iy][ia];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<2; ia++){
		for (iy = 0; iy < 13; iy++){
			file >> ChildPIPdiag[iy][ia];
		}
	}
	file.ignore(255, '\n');
	file.ignore(255, '\n');
	for (ia = 0; ia<2; ia++){
		for (iy = 0; iy < 13; iy++){
			file >> ChildPIP_ART[iy][ia];
		}
	}
	file.close();
}

void ReadInitPrev()
{
	int ia;
	std::ifstream file;

	file.open("InitPrev.txt");
	for(ia=0; ia<35; ia++){
		file>>InitPrevAdj[ia][0]>>InitPrevAdj[ia][1];}
	file.close();
}

void ReadAdultRoot()
{
	int ir, ic;
	std::ifstream file;
	std::ifstream file2;

	file.open("AdultRootM.txt");
	for (ir = 0; ir<ResampleSize; ir++){
		for (ic = 0; ic < 41; ic++){
			file >> AdultRootM.out[ir][ic];
		}
	}
	file.close();

	file2.open("AdultRootF.txt");
	for (ir = 0; ir<ResampleSize; ir++){
		for (ic = 0; ic < 41; ic++){
			file2 >> AdultRootF.out[ir][ic];
		}
	}
	file2.close();
}

void ReadAllFiles()
{
	ReadPaedAssumps();
	ReadAdultAssumps();
	ReadRollout();
	ReadStartingPop();
	ReadMarriage();
	ReadCD4dbn();
	ReadNonHIVmort();
	if (FixedUncertainty == 1){ ReadWestTable(); }
	ReadFertility();
	ReadMigration();
	ReadCalibData();
	ReadInitPrev();
	if (FixedARTinitiation == 1 && FixedUncertainty == 1){ ReadAdultRoot(); }
}

void SetInitialParameters()
{
	int ia, im, ib;
	double MedianAge, Temp;

	// Set initial proportion of married partners who are high risk (Mixing sheet)
	InitMarriedHigh[1][0] = (1.0 - Assortativeness) + Assortativeness * HighRiskPropn[0];
	InitMarriedHigh[1][1] = Assortativeness * HighRiskPropn[0];
	InitMarriedHigh[0][0] = HighRiskPropn[1] * InitMarriedHigh[1][0]/(HighRiskPropn[1] *
		InitMarriedHigh[1][0] + (1.0 - HighRiskPropn[1]) * InitMarriedHigh[1][1]);
	InitMarriedHigh[0][1] = HighRiskPropn[1] * (1.0 - InitMarriedHigh[1][0])/(HighRiskPropn[1] *
		(1.0 - InitMarriedHigh[1][0]) + (1.0 - HighRiskPropn[1]) * (1.0 - InitMarriedHigh[1][1]));

	// Set fraction of MSM partners who are men (Sex activity sheet)
	for (ia = 0; ia < 81; ia++){
		MSMpartnersM[ia] = MSMpartnersM20 * pow(RednMSMpartnersM, ia - 10); }

	// Set proportion of men who are circumcised and rates of circumcision
	MedianAge = MedianCirc/pow(log(CircPrevUlt/(2.0*(CircPrevUlt - CircPrevBirth)))/log(0.5),
		(1.0/ShapeCirc));
	for(ia=0; ia<91; ia++){
		Temp = pow((0.5 + ia)/MedianAge, ShapeCirc);
		InitCircumcised[ia] = CircPrevBirth + (CircPrevUlt - CircPrevBirth) * (1.0 - pow(0.5,
			Temp));
	}
	for(ia=0; ia<90; ia++){
		CircProbPreMMC[ia] = 1.0 - (1.0 - InitCircumcised[ia+1])/(1.0 - InitCircumcised[ia]);}
	CircProbPreMMC[90] = 0.0;
	for(ia=0; ia<81; ia++){
		for(ib=0; ib<4; ib++){
			CurrCircProb[ia][ib] = CircProbPreMMC[ia+10];}
	}
	for (ia = 0; ia < 80; ia++){
		if (CircPrevUlt - InitCircumcised[ia + 10] > 0.00001){
			WeibullProb[ia] = (InitCircumcised[ia + 11] - InitCircumcised[ia + 10]) / (CircPrevUlt -
				InitCircumcised[ia + 10]);
		}
		else{ WeibullProb[ia] = 1.0; }
	}
	WeibullProb[80] = 1.0;

	// Set initial frequency of sex for women in marital relationships (col C in Condom sheet)
	for(ia=0; ia<76; ia++){
		SexFreqMarital[ia][1] = SexActsLT * pow(RelFreqSexAge, (ia - 5.0)/20.0);}

	// Set Ectopy adjustment factors (col AN in Transmission sheet)
	for(ia=15; ia<81; ia++){
		EctopyFactor[ia][0] = 1.0;
		EctopyFactor[ia][1] = 1.0;
	}
	for(ia=0; ia<15; ia++){
		EctopyFactor[14-ia][0] = EctopyFactor[15-ia][0] * (1.0 + EctopyEffect[0]);
		EctopyFactor[14-ia][1] = EctopyFactor[15-ia][1] * (1.0 + EctopyEffect[1]);
	}

	// Set breastfeeding rates
	for(im=0; im<36; im++){
		PropnBF[im][0] = EverFeed[0] * pow(0.5, pow(im / (MedianFeed[0] * BFadjProv), ShapeFeed[0]));
		PropnBF[im][1] = EverFeed[1] * pow(0.5, pow(im / MedianFeed[1], ShapeFeed[1]));
	}
	for(im=0; im<6; im++){
		PropnBF[im][2] = EverFeed[2] * pow(0.5, pow(im/MedianFeed[2], ShapeFeed[2]));}
	for (im = 6; im<37; im++){
		PropnBF[im][2] = 0.0;}
	for(im=0; im<36; im++){
		for(ib=0; ib<2; ib++){
			RateOfBFchange[im][ib] = 1.0 - PropnBF[im + 1][ib]/PropnBF[im][ib];
		}
	}
	for(im=0; im<6; im++){
			RateOfBFchange[im][2] = 1.0 - PropnBF[im + 1][2]/PropnBF[im][2];
	}

	// Set rates of transition out of the HIV states
	for(im=0; im<132; im++){
		ProgToNeedNoPMTCT[im] = 1.0 - exp(-ProgAdjNoPMTCT * ProvAdjPaedMort * (ProgToNeedLT/12.0 + ExcessProgToNeed *
			(pow(ExcessProgRedn, (im + 1.0)/12.0) - pow(ExcessProgRedn, im/12.0))/
			log(ExcessProgRedn)));
		ProgToNeedPMTCT[im] = 1.0 - exp(-ProvAdjPaedMort * (ProgToNeedLT/12.0 + ExcessProgToNeed * (pow(ExcessProgRedn,
			(im + 1.0)/12.0) - pow(ExcessProgRedn, im/12.0))/log(ExcessProgRedn)));
		ProgToNeedPostnatal[im] = 1.0 - exp(-ProgAdjNoPMTCT * ProvAdjPaedMort * (ProgToNeedLT * RRprogressionPostnatal / 12.0 +
			ExcessProgToNeed * (pow(ExcessProgRedn * RRprogressionPostnatal, (im + 1.0) / 12.0) - pow(ExcessProgRedn *
			RRprogressionPostnatal, im / 12.0)) / log(ExcessProgRedn * RRprogressionPostnatal)));
		AIDSmortNoART[im] = 1.0 - exp(-ProvAdjPaedMort * (AIDSmortLT/12.0 + ExcessAIDSmort * (pow(ExcessMortRedn,
			(im + 1.0)/12.0) - pow(ExcessMortRedn, im/12.0))/log(ExcessMortRedn)));
		AIDSmortEarlyART[im] = 1.0 - exp(-EarlyARTmort * pow(RRearlyARTmort, im/12.0) / 12.0);
	}
	AIDSmortEarlyART[0] = 0.0;

	// Set initial population profile at ages 0-10, by sex, feeding and age in months
	MaleChild.GetStartYrProfile();
	FemChild.GetStartYrProfile();

	// Set initial ART initiation rates to 0
	for(im=0; im<132; im++){
		ARTinitiation[im] = 0.0;}

	// Set average rates of ART initiation in last 3 years to 0
	AveARTstartU200[0] = 0.0;
	AveARTstartU200[1] = 0.0;

	// Set cumulative ART enrolment in kids to 0
	for (ia = 0; ia < 11; ia++){
		for (ib = 0; ib < 5; ib++){
			CumPaedARTearly[ia][ib] = 0.0;
			CumPaedARTlate[ia][ib] = 0.0;
		}
	}
}

void SetInitSexActivity()
{
	// This function performs the calculations in columns G-L of the "Sex activity" sheet and also
	// sets the age preference matrices for females and MSM.

	int ia, ib, ig, ind;
	double Temp, Temp2, MarriedPreviousAge, MeanAgeDifMSM;
	double X, A, B, P, term1, term2;

	// Get numbers of virgins (ia = 0 corresponds to age 10)
	for(ia=0; ia<20; ia++){
		for(ig=0; ig<2; ig++){
			Temp = 1.0/(1.0 + pow((ia + 0.5)/(DebutMedian[ig] - 10.0), DebutShape[ig]));
			InitBehavDbn[ia][0][ig] = StartingPop[ia+10][ig] * HighRiskPropn[ig] * Temp;
			InitBehavDbn[ia][3][ig] = StartingPop[ia+10][ig] * (1.0 - HighRiskPropn[ig]) *
				pow(Temp, DebutLow[ig]);
			if (ia <= 5){
				InitBehavDbn[ia][1][ig] = StartingPop[ia + 10][ig] * HighRiskPropn[ig] -
					InitBehavDbn[ia][0][ig];
				InitBehavDbn[ia][4][ig] = StartingPop[ia + 10][ig] * (1.0 - HighRiskPropn[ig]) -
					InitBehavDbn[ia][3][ig];
			}
		}
	}

	// Calculate time-constant gamma distribution parameters (ia = 0 corresponds to age 10)
	for(ia=0; ia<81; ia++){
		if(ia<=7){
			MinPartnerAge[ia] = ia;}
		else{
			MinPartnerAge[ia] = 7.0 + (ia - 7.0)/2.0;}
		GammaParametersST[ia][1] = (SDageDif * SDageDif)/(MeanAgeDif + ia - MinPartnerAge[ia]);
		GammaParametersST[ia][0] = (MeanAgeDif + ia - MinPartnerAge[ia])/GammaParametersST[ia][1];
		GammaParametersLT[ia][1] = (SDageDifLT * SDageDifLT)/(MeanAgeDifLT + ia - MinPartnerAge[ia]);
		GammaParametersLT[ia][0] = (MeanAgeDifLT + ia - MinPartnerAge[ia])/GammaParametersLT[ia][1];
		MeanAgeDifMSM = MeanAgeDifMSM25 - ChangeAgeDifMSM * (ia - 15.0);
		if (MeanAgeDifMSM < - 10){ MeanAgeDifMSM = - 10.0; }
		GammaParametersMSM[ia][1] = (SDageDifMSM * SDageDifMSM) / (MeanAgeDifMSM + ia);
		GammaParametersMSM[ia][0] = (MeanAgeDifMSM + ia) / GammaParametersMSM[ia][1];
	}
	UpdateAgePrefsF();

	// Calculate age prefs in MSM
	for (ia = 0; ia < 81; ia++){
		ind = 1;
		A = GammaParametersMSM[ia][0];
		B = 1.0 / GammaParametersMSM[ia][1];
		term1 = 0.0;
		for (ib = 0; ib<81; ib++){
			term2 = term1;
			X = ib + 0.5;
			P = 0.0;
			cdfgam(&ind, &P, 0, &X, &A, &B, 0, 0);
			term1 = P;
			if (ib<80){
				AgePrefMSM[ia][ib] = term1 - term2; }
			else{
				AgePrefMSM[ia][ib] = 1.0 - term2; }
		}
	}

	// Get rates of widowhood for females
	for(ia=0; ia<76; ia++){ // 0 corresponds to age 15
		Temp = 0.0;
		for(ib=0; ib<76; ib++){
			Temp += AgePrefLT[ia+5][ib+5][1] * CurrNonAIDSmortALB[ib+15][0];}
		InitWidowhoodRate[ia][0] = Temp;
	}

	// Get numbers of sexually experienced married and unmarried females
	for(ia=0; ia<81; ia++){
		if(ia<=5){
			InitBehavDbn[ia][2][1] = 0.0;
			InitBehavDbn[ia][5][1] = 0.0;
			InitBehavDbn[ia][1][1] = StartingPop[ia+10][1] * HighRiskPropn[1] -
				InitBehavDbn[ia][0][1];
			InitBehavDbn[ia][4][1] = StartingPop[ia+10][1] * (1.0 - HighRiskPropn[1]) -
				InitBehavDbn[ia][3][1];
		}
		else{
			Temp = GetMarriedPropnAtStart(1, ia+10);
			InitBehavDbn[ia][2][1] = StartingPop[ia+10][1] * HighRiskPropn[1] * Temp;
			InitBehavDbn[ia][5][1] = StartingPop[ia+10][1] * (1.0 - HighRiskPropn[1]) * Temp;
			InitBehavDbn[ia][1][1] = StartingPop[ia+10][1] * HighRiskPropn[1] -
				InitBehavDbn[ia][0][1] - InitBehavDbn[ia][2][1];
			InitBehavDbn[ia][4][1] = StartingPop[ia+10][1] * (1.0 - HighRiskPropn[1]) -
				InitBehavDbn[ia][3][1] - InitBehavDbn[ia][5][1];
		}
	}

	// Get rates of widowhood for males
	for(ia=0; ia<76; ia++){ // 0 corresponds to age 15
		Temp = 0.0;
		Temp2 = 0.0;
		for(ib=0; ib<76; ib++){
			Temp += (InitBehavDbn[ib+5][2][1] + InitBehavDbn[ib+5][5][1]) *
				AgePrefLT[ib+5][ia+5][1] * CurrNonAIDSmortALB[ib+15][1];
			Temp2 += (InitBehavDbn[ib+5][2][1] + InitBehavDbn[ib+5][5][1]) *
				AgePrefLT[ib+5][ia+5][1];
		}
		if(Temp2==0){
			InitWidowhoodRate[ia][1] = 0.0;}
		else{
			InitWidowhoodRate[ia][1] = Temp/Temp2;}
	}

	// Get numbers of sexually experienced married and unmarried males
	for(ia=0; ia<81; ia++){
		if(ia<=5){
			InitBehavDbn[ia][2][0] = 0.0;
			InitBehavDbn[ia][5][0] = 0.0;
			InitBehavDbn[ia][1][0] = StartingPop[ia+10][0] * HighRiskPropn[0] -
				InitBehavDbn[ia][0][0];
			InitBehavDbn[ia][4][0] = StartingPop[ia+10][0] * (1.0 - HighRiskPropn[0]) -
				InitBehavDbn[ia][3][0];
		}
		else{
			Temp = GetMarriedPropnAtStart(0, ia+10);
			InitBehavDbn[ia][2][0] = StartingPop[ia+10][0] * HighRiskPropn[0] * Temp;
			InitBehavDbn[ia][5][0] = StartingPop[ia+10][0] * (1.0 - HighRiskPropn[0]) * Temp;
			InitBehavDbn[ia][1][0] = StartingPop[ia+10][0] * HighRiskPropn[0] -
				InitBehavDbn[ia][0][0] - InitBehavDbn[ia][2][0];
			InitBehavDbn[ia][4][0] = StartingPop[ia+10][0] * (1.0 - HighRiskPropn[0]) -
				InitBehavDbn[ia][3][0] - InitBehavDbn[ia][5][0];
		}
	}

	// Get age adjustment factors for female rate of ST partnership formation
	Temp = pow((GammaMeanF - 10.0)/GammaSDF, 2.0);
	Temp2 = (GammaMeanF - 10.0)/(GammaSDF * GammaSDF);
	for(ia=0; ia<81; ia++){
		PartnerAcqF[ia] = pow((ia + 0.5)/10.0, Temp - 1) * exp(-(ia - 9.5) * Temp2);}

	// Get male rates of visiting sex workers
	for(ia=0; ia<81; ia++){
		FSWcontactRate[ia] = FSWcontactAge21 * pow((ia + 0.5)/11.5,
			FSWcontactAlpha - 1.0) * exp(-FSWcontactLambda * (ia - 11.0));
	}

	// Get female sex worker age dbn
	ind = 1;
	X = 0.0;
	A = FSWageAlpha;
	B = FSWageLambda;
	P = 0.0;
	cdfgam(&ind,&P,0,&X,&A,&B,0,0);
	Temp = P;
	FSWageDbn[0] = Temp;
	for(ia=1; ia<80; ia++){
		Temp2 = Temp;
		X = ia;
		P = 0.0;
		cdfgam(&ind,&P,0,&X,&A,&B,0,0);
		Temp = P;
		FSWageDbn[ia] = Temp - Temp2;
	}
	FSWageDbn[80] = 1.0 - Temp;

	// Set initial male risk groups
	MHU_virgin.GetStartYrProfile();
	MHC_virgin.GetStartYrProfile();
	MHU_ST.GetStartYrProfile();
	MHC_ST.GetStartYrProfile();
	MHU_STM.GetStartYrProfile();
	MHC_STM.GetStartYrProfile();
	MHU_LTH.GetStartYrProfile();
	MHC_LTH.GetStartYrProfile();
	MHU_LTL.GetStartYrProfile();
	MHC_LTL.GetStartYrProfile();
	MLU_virgin.GetStartYrProfile();
	MLC_virgin.GetStartYrProfile();
	MLU_ST.GetStartYrProfile();
	MLC_ST.GetStartYrProfile();
	MLU_STM.GetStartYrProfile();
	MLC_STM.GetStartYrProfile();
	MLU_LTH.GetStartYrProfile();
	MLC_LTH.GetStartYrProfile();
	MLU_LTL.GetStartYrProfile();
	MLC_LTL.GetStartYrProfile();

	// Set male demand for sex workers and get initial number of sex workers
	GetCurrBehavDbnM();
	UpdateFSWdemand();

	// Set initial female risk groups
	FH_virgin.GetStartYrProfile();
	FH_ST.GetStartYrProfile();
	FH_SW.GetStartYrProfile();
	FH_LTH.GetStartYrProfile();
	FH_LTL.GetStartYrProfile();
	FL_virgin.GetStartYrProfile();
	FL_ST.GetStartYrProfile();
	FL_LTH.GetStartYrProfile();
	FL_LTL.GetStartYrProfile();

	// Set CurrBehavDbn for F
	GetCurrBehavDbnF();

	// Calculate male rate of ST partnership formation and age prefs
	UpdatePartnerAcqM();

	// Get probabilities of sexual debut by age
	for(ig=0; ig<2; ig++){
		Temp = 1.0/(1.0 + pow(0.5/(DebutMedian[ig] - 10.0), DebutShape[ig]));
		DebutProb[0][0][ig] = 1.0 - Temp;
		DebutProb[0][1][ig] = 1.0 - pow(Temp, DebutLow[ig]);
		for(ia=1; ia<20; ia++){
			Temp2 = Temp;
			Temp = 1.0/(1.0 + pow((0.5 + ia)/(DebutMedian[ig] - 10.0), DebutShape[ig]));
			DebutProb[ia][0][ig] = 1.0 - pow(Temp/Temp2, 1.0/12.0);
			DebutProb[ia][1][ig] = 1.0 - pow(Temp/Temp2, DebutLow[ig]/12.0);
		}
		DebutProb[20][0][ig] = 1.0;
		DebutProb[20][1][ig] = 1.0;
	}
}

double GetMarriedPropnAtStart(int Sex, int Age)
{
	int ia, iy;
	double PropnNeverMarried[76], CurrentMarried[76], Dissolution, RemainMarried, FirstMarriageRate;

	PropnNeverMarried[0] = 1.0; // Corresponds to age 15 last birthday
	for (ia = 1; ia <= Age - 15; ia++){ // Max value of Age is 90, minimum is 16
		if (ia < MarriageMin[Sex] - 15){ PropnNeverMarried[ia] = 1.0; }
		else{
			PropnNeverMarried[ia] = 1.0 / (1.0 + pow((15.0 + ia - MarriageMin[Sex]) *
				exp(-MarriageConstant[Sex] + MarriageTrend[Sex] * Age), 1.0 / MarriageShape[Sex]));
		}
	}

	CurrentMarried[0] = 0.0;
	for (ia = 1; ia <= Age - 15; ia++){
		if (ia < MarriageMin[Sex] - 15){ CurrentMarried[ia] = 0.0; }
		else{
			FirstMarriageRate = 1.0 - PropnNeverMarried[ia] / PropnNeverMarried[ia - 1];
			CurrentMarried[ia] = (1.0 - CurrentMarried[ia - 1]) * FirstMarriageRate;
			iy = StartYear - 2004 + ia - Age; // Year of dissolution, relative to 2004
			Dissolution = DivorceRate[ia-1][Sex] * DivorceAdj * pow(DivorceTrend, iy);
			RemainMarried = exp(-Dissolution) * (1.0 - InitWidowhoodRate[ia-1][Sex]);
			// Adjust for rapid remarriage in those who are divorced/widowed
			RemainMarried = RemainMarried + (1.0 - RemainMarried) * ORremarriage[Sex] *
				FirstMarriageRate / (ORremarriage[Sex] * FirstMarriageRate + 1.0 - FirstMarriageRate);
			CurrentMarried[ia] += CurrentMarried[ia - 1] * RemainMarried;
		}
	}

	return CurrentMarried[Age - 15];
}

void SetCD4byARTdur()
{
	int ia, ic, id, ind;
	double alpha, beta;
	double X, A, B, P;

	for (id = 0; id < 6; id++){
		AveCD4byARTdur[0][id] = InitAveCD4byARTdur[id] * (RatioMaxToMinCD4[id] + (1.0 -
			RatioMaxToMinCD4[id]) * exp(-RednLogMort[0] * (AveARTstartU200[0] + AveARTstartU200[1]) / 2.0));
	}

	// Calculate AveCD4nonIntDur and CoV_CD4nonIntDur
	for(ic=0; ic<4; ic++){
		AveCD4nonIntDur[ic][0] = 0.75 * AveCD4byARTdur[ic][0] + 0.25 * AveCD4byARTdur[ic][1];
		CoV_CD4nonIntDur[ic][0] = 0.75 * CoV_CD4byARTdur[ic][0] + 0.25 * CoV_CD4byARTdur[ic][1];
		for(id=1; id<6; id++){
			AveCD4nonIntDur[ic][id] = 0.5 * (AveCD4byARTdur[ic][id-1] + AveCD4byARTdur[ic][id]);
			CoV_CD4nonIntDur[ic][id] = 0.5 * (CoV_CD4byARTdur[ic][id-1] + CoV_CD4byARTdur[ic][id]);
		}
	}

	// Calculate CD4dbnByARTdur[4][4][6]
	ind = 1;
	for(ic=0; ic<4; ic++){
		for(id=1; id<6; id++){
			// Note that we don't actually use the values for id=0, which is why we start from 1.
			// Calculate alpha and beta parameters
			beta = pow(AveCD4byARTdur[ic][id] * CoV_CD4byARTdur[ic][id], 2.0)/
				AveCD4byARTdur[ic][id];
			alpha = AveCD4byARTdur[ic][id]/beta;
			A = alpha;
			B = 1.0/beta;
			X = 200.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnByARTdur[3][ic][id] = P;
			X = 350.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnByARTdur[2][ic][id] = P - CD4dbnByARTdur[3][ic][id];
			X = 500.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnByARTdur[1][ic][id] = P - CD4dbnByARTdur[2][ic][id] -
				CD4dbnByARTdur[3][ic][id];
			CD4dbnByARTdur[0][ic][id] = 1.0 - CD4dbnByARTdur[1][ic][id] -
				CD4dbnByARTdur[2][ic][id] - CD4dbnByARTdur[3][ic][id];
		}
	}

	// Calculate CD4dbnNonIntDur[4][4][6]
	for(ic=0; ic<4; ic++){
		for(id=0; id<6; id++){
			// Calculate alpha and beta parameters
			beta = pow(AveCD4nonIntDur[ic][id] * CoV_CD4nonIntDur[ic][id], 2.0)/
				AveCD4nonIntDur[ic][id];
			alpha = AveCD4nonIntDur[ic][id]/beta;
			A = alpha;
			B = 1.0/beta;
			X = 200.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnNonIntDur[3][ic][id] = P;
			X = 350.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnNonIntDur[2][ic][id] = P - CD4dbnNonIntDur[3][ic][id];
			X = 500.0;
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			CD4dbnNonIntDur[1][ic][id] = P - CD4dbnNonIntDur[2][ic][id] -
				CD4dbnNonIntDur[3][ic][id];
			CD4dbnNonIntDur[0][ic][id] = 1.0 - CD4dbnNonIntDur[1][ic][id] -
				CD4dbnNonIntDur[2][ic][id] - CD4dbnNonIntDur[3][ic][id];
		}
	}
}

void CalcInterruptions()
{
	int id, ig, iy;
	double CurrInterruption[2], CurrInterruptionPaed;

	iy = CurrYear - StartYear;

	// ART interruptions in adults
	CurrInterruption[1] = ARTinterruptionRate * RR_ARTinterruption[iy];
	CurrInterruption[0] = CurrInterruption[1] * RRinterruptionM;
	for (ig = 0; ig < 2; ig++){
		ARTresumptionRate[ig] += CurrARTinitiation[ig] * 12.0;
		for (id = 0; id < 6; id++){
			OnARTbyIntDur[id][ig] = (CurrInterruption[ig] * exp(-(CurrInterruption[ig] +
				ARTresumptionRate[ig]) * id) + ARTresumptionRate[ig]) /
				(CurrInterruption[ig] + ARTresumptionRate[ig]);
		}
		OnARThalfIntDur[0][ig] = (CurrInterruption[ig] * exp(-(CurrInterruption[ig] +
			ARTresumptionRate[ig]) * 0.25) + ARTresumptionRate[ig]) /
			(CurrInterruption[ig] + ARTresumptionRate[ig]);
		for (id = 1; id < 6; id++){
			OnARThalfIntDur[id][ig] = (CurrInterruption[ig] * exp(-(CurrInterruption[ig] +
				ARTresumptionRate[ig]) * (id - 0.5)) + ARTresumptionRate[ig]) /
				(CurrInterruption[ig] + ARTresumptionRate[ig]);
		}
	}

	// ART interruptions in children
	CurrInterruptionPaed = ARTinterruptionPaed * RR_ARTinterruption[CurrYear - StartYear];
	for (id = 0; id < 5; id++){
		OnARThalfIntDurP[id] = (CurrInterruptionPaed * exp(-(CurrInterruptionPaed + ARTresumptionPaed) * (0.5 + id)) +
			ARTresumptionPaed) / (CurrInterruptionPaed + ARTresumptionPaed);
	}

	// Store results
	if (FixedUncertainty == 1 && CurrMonth==0){
		ARTresumptionRateM.out[CurrSim - 1][iy] = ARTresumptionRate[0];
		ARTresumptionRateF.out[CurrSim - 1][iy] = ARTresumptionRate[1];
	}
}

void SetActivityByStage()
{
	int is, id, ic, iy, ig;
	double UnadjRelSex[5], Temp1, Temp2, TempCD4, VCTcondomTemp, omega[4][2], theta;

	iy = CurrYear - StartYear;

	// Calculate ARTinfectivity
	theta = log(VLeffectInfectivity) / (ShapeVL * pow(2.0, ShapeVL - 1.0));
	for(ic = 0; ic < 4; ic++){
		if (ic == 0){ TempCD4 = 4.75; }
		if (ic == 1){ TempCD4 = 3.25; }
		if (ic == 2){ TempCD4 = 1.75; }
		if (ic == 3){ TempCD4 = 0.00; }
		Temp1 = MedianVLuntreated200 + VLdifPer100CD4 * TempCD4;
		omega[ic][0] = -log(0.5) / pow(6.0 - Temp1, ShapeVL);
		Temp2 = 1.0 / (1.0 + (1.0 - CurrSuppression200) / (CurrSuppression200 * ORsuppressionCD4[ic]));
		omega[ic][1] = -log(Temp2) / pow(6.0 - log10(400), ShapeVL);
		ARTinfectivity[ic] = (omega[ic][1] / (omega[ic][1] + theta)) /
			(omega[ic][0] / (omega[ic][0] + theta));
	}

	UnadjRelSex[0] = CD4duration[0];
	UnadjRelSex[1] = CD4duration[1] * (1.0 - HIVeffectSex[0]);
	UnadjRelSex[2] = CD4duration[2] * (1.0 - HIVeffectSex[1]);
	UnadjRelSex[3] = (1.0 - HIVeffectSex[2])/(CD4decline[2] + CD4mort[0]);
	UnadjRelSex[4] = (1.0 - HIVeffectSex[3]) * (CD4decline[2]/(CD4decline[2] +
		CD4mort[0]))/CD4mort[1];

	Temp1 = 0.0;
	Temp2 = 0.0;
	for(is=0; is<5; is++){
		Temp1 += UnadjRelSex[is] * RelInfecStage[is];
		Temp2 += UnadjRelSex[is];
	}
	Temp1 = Temp2/Temp1;

	// Calculate RelativeInf
	for (ig = 0; ig < 2; ig++){
		for (is = 0; is < 5; is++){
			RelativeInf[is][ig] = Temp1 * RelInfecStage[is];
			RelativeInf[is + 5][ig] = RelativeInf[is][ig];
			RelativeInf[is + 10][ig] = RelativeInf[is][ig];
		}
		for (is = 1; is < 5; is++){
			RelativeInf[10 + is * 5][ig] = RelativeInf[is][ig] * (OnARThalfIntDur[0][ig] *
				ARTinfectivity[is - 1] + (1.0 - OnARThalfIntDur[0][ig]));
			for (id = 1; id < 5; id++){
				RelativeInf[10 + is * 5 + id][ig] = RelativeInf[is][ig] * (OnARTbyIntDur[id][ig] *
					ARTinfectivity[is - 1] + (1.0 - OnARTbyIntDur[id][ig]));
			}
		}
		for (is = 0; is < 4; is++){
			RelativeInf[is + 35][ig] = RelativeInf[is + 1][ig];
		}
	}

	// Calculate RelativeCoit
	for (ig = 0; ig < 2; ig++){
		RelativeCoit[0][ig] = 1.0;
		for (is = 0; is < 5; is++){
			if (is > 0){
				RelativeCoit[is][ig] = 1.0 - HIVeffectSex[is - 1];
			}
			RelativeCoit[is + 5][ig] = RelativeCoit[is][ig];
			RelativeCoit[is + 10][ig] = RelativeCoit[is][ig];
		}
		for (is = 0; is < 4; is++){ // baseline CD4
			for (id = 0; id < 5; id++){ // ART duration
				Temp1 = 0.0;
				for (ic = 0; ic < 4; ic++){ // current CD4
					if (id == 0){
						Temp1 += HIVeffectSex[ic] * CD4dbnNonIntDur[ic][3 - is][0];
					}
					else{
						Temp1 += HIVeffectSex[ic] * CD4dbnByARTdur[ic][3 - is][id];
					}
				}
				RelativeCoit[15 + 5 * is + id][ig] = 1.0 - Temp1;
				// Adjustment to take into account ART interruptions
				if (id == 0){
					RelativeCoit[15 + 5 * is + id][ig] = RelativeCoit[15 + 5 * is + id][ig] *
						OnARThalfIntDur[0][ig] + (1.0 - OnARThalfIntDur[0][ig]) * RelativeCoit[is + 1][ig];
				}
				else{
					RelativeCoit[15 + 5 * is + id][ig] = RelativeCoit[15 + 5 * is + id][ig] *
						OnARTbyIntDur[id][ig] + (1.0 - OnARTbyIntDur[id][ig]) * RelativeCoit[is + 1][ig];
				}
			}
		}
		for (is = 0; is < 4; is++){
			RelativeCoit[is + 35][ig] = RelativeCoit[is + 1][ig];
		}
	}

	// Calculate RelativeUnprot
	VCTcondomTemp = VCTcondom;
	if (VaryFutureInterventions == 1 && CurrYear >= 2015 + FutureInterventions.out[CurrSim - 1][25]){
		VCTcondomTemp = 1.0 - (1.0 - VCTcondom)*(1.0 - FutureInterventions.out[CurrSim - 1][4] *
			FutureInterventions.out[CurrSim - 1][26]);
	}
	for(is=0; is<11; is++){
		RelativeUnprot[is] = 1.0;}
	for(is=11; is<15; is++){
		RelativeUnprot[is] = 1.0 - VCTcondomTemp;}
	for(is=15; is<35; is++){
		RelativeUnprot[is] = (1.0 - VCTcondomTemp) * (1.0 - ARTcondom);}
	for(is=35; is<39; is++){
		RelativeUnprot[is] = 1.0 - VCTcondomTemp;}

	// Calculate RelativeTransm
	for (ig = 0; ig < 2; ig++){
		for (is = 0; is < 39; is++){
			RelativeTransm[is][0][ig] = RelativeInf[is][ig] * RelativeCoit[is][ig];
			RelativeTransm[is][1][ig] = RelativeInf[is][ig] * RelativeCoit[is][ig] * RelativeUnprot[is];
			RelativeTransm[is][2][ig] = RelativeInf[is][ig] * RelativeUnprot[is];
		}
	}

	// Calculate RelativeCSWentry
	RelativeCSWentry[0] = 1.0;
	for(is=0; is<5; is++){
		if(is>0){
			RelativeCSWentry[is] = 1.0 - HIVeffectFSWentry[is-1];}
		RelativeCSWentry[is+5] = RelativeCSWentry[is];
		RelativeCSWentry[is+10] = RelativeCSWentry[is];
	}
	for(is=0; is<4; is++){ // baseline CD4
		for(id=0; id<5; id++){ // ART duration
			Temp1 = 0.0;
			for(ic=0; ic<4; ic++){ // current CD4
				if(id==0){
					Temp1 += HIVeffectFSWentry[ic] * CD4dbnNonIntDur[ic][3-is][0];}
				else{
					Temp1 += HIVeffectFSWentry[ic] * CD4dbnByARTdur[ic][3-is][id];}
			}
			RelativeCSWentry[15+5*is+id] = 1.0 - Temp1;
		}
	}
	for(is=0; is<4; is++){
		RelativeCSWentry[is+35] = RelativeCSWentry[is+1];}
	// Accounting for effect of knowing one's HIV status
	for (is = 10; is < 39; is++){
		RelativeCSWentry[is] *= (1.0 - VCT_FSWentry);}

	// Calculate RelativeCSWexit
	RelativeCSWexit[0] = 1.0;
	for(is=0; is<5; is++){
		if(is>0){
			RelativeCSWexit[is] = HIVeffectFSWexit[is-1];}
		RelativeCSWexit[is+5] = RelativeCSWexit[is];
		RelativeCSWexit[is+10] = RelativeCSWexit[is];
	}
	for(is=0; is<4; is++){ // baseline CD4
		for(id=0; id<5; id++){ // ART duration
			Temp1 = 0.0;
			for(ic=0; ic<4; ic++){ // current CD4
				if(id==0){
					Temp1 += HIVeffectFSWexit[ic] * CD4dbnNonIntDur[ic][3-is][0];}
				else{
					Temp1 += HIVeffectFSWexit[ic] * CD4dbnByARTdur[ic][3-is][id];}
			}
			RelativeCSWexit[15+5*is+id] = Temp1;
		}
	}
	for(is=0; is<4; is++){
		RelativeCSWexit[is+35] = RelativeCSWexit[is+1];}
}

void SetFertByStage()
{
	int is, id, ic;
	double RRtemp;

	for(is=0; is<5; is++){
		RelativeFert[is] = 1.0;}
	for (is = 0; is < 5; is++){
		RelativeFert[is + 5] = RRfertHIV * RRfertCD4[is];
		RelativeFert[is + 10] = RelativeFert[is + 5];
		RelativeFert[is + 15] = RelativeFert[is + 5] * RRfertDiag;
	}
	for (is = 1; is < 5; is++){
		// For ART-experienced patients, is = baseline CD4 category
		RRtemp = 0.0;
		for (ic = 0; ic < 4; ic++){ // ic = current CD4 category
			RRtemp += RelativeFert[ic + 16] * CD4dbnNonIntDur[ic][is - 1][0];
		}
		RelativeFert[is * 5 + 15] = (OnARThalfIntDur[0][1] * RRtemp + (1.0 - OnARThalfIntDur[0][1]) *
			RelativeFert[is + 15]) * RRfertART;
		RelativeFertART[(is - 1) * 5] = OnARThalfIntDur[0][1] * RRtemp * RRfertART;
		for (id = 1; id < 5; id++){
			RRtemp = 0.0;
			for (ic = 0; ic < 4; ic++){
				RRtemp += RelativeFert[ic + 16] * CD4dbnByARTdur[ic][is - 1][id];
			}
			RelativeFert[is * 5 + id + 15] = (OnARTbyIntDur[id][1] * RRtemp + (1.0 - OnARTbyIntDur[id][1]) *
				RelativeFert[is + 15]) * RRfertART;
			RelativeFertART[(is - 1) * 5 + id] = OnARTbyIntDur[id][1] * RRtemp * RRfertART;
		}
	}
}

void SetProgression(int setting)
{
	int ia, is, ig;
	double Temp;

	for(ia=0; ia<81; ia++){
		Temp = pow(RRper10yr, (ia - 20.0)/10.0);
		if (setting == 0){ Temp *= pow(RRperCalYr, StartYear - 1999); }
		else{ Temp *= pow(RRperCalYr, CurrYear - 1999); }
		for(is=0; is<4; is++){
			MnthlyCD4trans[ia][is][0] = Temp/(12.0 * CD4duration[is]);
			MnthlyCD4trans[ia][is][1] = MnthlyCD4trans[ia][is][0] * RRuntreatedMortF;
		}
		MnthlyAIDSmort[ia][0][0] = pow(RRper10yr, (ia - 20.0) / 10.0) * CD4mort[0] / 12.0;
		MnthlyAIDSmort[ia][0][1] = MnthlyAIDSmort[ia][0][0] * RRuntreatedMortF;
		for(is=0; is<3; is++){
			ProbExitAtEntry[ia][is][0] = 1.0 - (1.0 - exp(-MnthlyCD4trans[ia][is][0]))/
				MnthlyCD4trans[ia][is][0];
			ProbExitAtEntry[ia][is][1] = 1.0 - (1.0 - exp(-MnthlyCD4trans[ia][is][1]))/
				MnthlyCD4trans[ia][is][1];
		}
		ProbExitAtEntry[ia][3][0] = 1.0 - (1.0 - exp(-MnthlyCD4trans[ia][3][0] -
			MnthlyAIDSmort[ia][0][0]))/(MnthlyCD4trans[ia][3][0] +
			MnthlyAIDSmort[ia][0][0]);
		ProbExitAtEntry[ia][3][1] = 1.0 - (1.0 - exp(-MnthlyCD4trans[ia][3][1] -
			MnthlyAIDSmort[ia][0][1]))/(MnthlyCD4trans[ia][3][1] +
			MnthlyAIDSmort[ia][0][1]);
	}
	MortAdjBelow200[0] = 1.0;
	MortAdjBelow200[1] = 1.0;
	if (setting == 0){ UpdateAIDSmort(); }
}

void SetFutureRollout()
{
	int iy, ig, StartRollout, FirstRollout;

	// HCT assumptions
	for (iy = 31; iy < 116; iy++){
		HCT1stTimeF25[iy] = FutureInterventions.out[CurrSim - 1][0];}
	StartRollout = FutureInterventions.out[CurrSim - 1][2] + 30;
	for (iy = 0; iy < 116; iy++){ HBCTuptake[iy] = 0.0; }
	if (StartRollout < 116){
		for (iy = StartRollout; iy < 116; iy++){
			HBCTuptake[iy] = FutureInterventions.out[CurrSim - 1][3];}
	}

	// ART assumptions
	StartRollout = FutureInterventions.out[CurrSim - 1][7] + 30;
	for (iy = 30; iy < 116; iy++){ EligibleAsymPre500[iy] = 0.0; }
	if (StartRollout < 116){
		for (iy = StartRollout; iy < 116; iy++){
			EligibleAsymPre500[iy] = 1.0;}
	}
	FirstRollout = StartRollout;
	StartRollout = FutureInterventions.out[CurrSim - 1][8] + 30;
	for (iy = 0; iy < 116; iy++){POC_CD4[iy] = 0.0;}
	if (StartRollout < 116){
		for (iy = StartRollout; iy < 116; iy++){
			POC_CD4[iy] = 1.0;}
	}
	if (StartRollout < FirstRollout){ FirstRollout = StartRollout; }
	for (iy = 30; iy < 116; iy++){ HCT_ARTuptake[iy] = HCT_ARTuptake[29]; }
	if (FirstRollout < 116){
		for (iy = FirstRollout; iy < 116; iy++){
			HCT_ARTuptake[iy] = FutureInterventions.out[CurrSim - 1][9];
		}
	}
	UltARTdelay[0] = FutureInterventions.out[CurrSim - 1][10] * 2.0;
	UltARTdelay[1] = FutureInterventions.out[CurrSim - 1][10];

	// Behaviour change assumptions

	// PMTCT assumptions
	for (iy = 27; iy < 31; iy++){
		RescreenPropnLate[iy] = RescreenPropnLate[26] + (0.2 * (iy - 26)) *
			(FutureInterventions.out[CurrSim - 1][17] - RescreenPropnLate[26]);}
	for (iy = 31; iy < 116; iy++){
		RescreenPropnLate[iy] = FutureInterventions.out[CurrSim - 1][17];}
	for (iy = 27; iy < 31; iy++){
		MatARTuptake[iy] = MatARTuptake[26] + (0.2 * (iy - 26)) *
			(FutureInterventions.out[CurrSim - 1][18] - MatARTuptake[26]);}
	for (iy = 31; iy < 116; iy++){
		MatARTuptake[iy] = FutureInterventions.out[CurrSim - 1][18];}
	for (iy = 27; iy < 31; iy++){
		IncreaseARTdurPreg[iy] = IncreaseARTdurPreg[26] + (0.2 * (iy - 26)) *
			(FutureInterventions.out[CurrSim - 1][19] - IncreaseARTdurPreg[26]);}
	for (iy = 31; iy < 116; iy++){
		IncreaseARTdurPreg[iy] = FutureInterventions.out[CurrSim - 1][19];}

	// EID and paediatric ART assumptions
	for (iy = 27; iy < 31; iy++){
		PCR6week[iy] = PCR6week[26] + (0.2 * (iy - 26)) *
			(FutureInterventions.out[CurrSim - 1][24] - PCR6week[26]);}
	for (iy = 31; iy < 116; iy++){
		PCR6week[iy] = FutureInterventions.out[CurrSim - 1][24];}
	//StartRollout = FutureInterventions.out[CurrSim - 1][25] + 30;
	for (iy = 30; iy < 116; iy++){ PCRbirth[iy] = 0.0; }
	/*if (StartRollout < 116){
		for (iy = StartRollout; iy < 116; iy++){
			PCRbirth[iy] = 1.0;}
	}*/
	UltARTdelayC = FutureInterventions.out[CurrSim - 1][26];

	// MMC assumptions
	UltMMCprob = FutureInterventions.out[CurrSim - 1][27];

	// PrEP assumptions
	PrEPefficacy[0] = FutureInterventions.out[CurrSim - 1][28];
	PrEPefficacy[1] = FutureInterventions.out[CurrSim - 1][28];
	CondomRednPrEP[0] = FutureInterventions.out[CurrSim - 1][29];
	CondomRednPrEP[1] = FutureInterventions.out[CurrSim - 1][29];
	StartRollout = FutureInterventions.out[CurrSim - 1][30] + 30;
	/*for (iy = 30; iy < 116; iy++){PrEP_FSW[iy] = 0.0;}
	if (StartRollout < 116){
		for (iy = StartRollout; iy < 116; iy++){
			PrEP_FSW[iy] = FutureInterventions.out[CurrSim - 1][32];}
	}
	StartRollout = FutureInterventions.out[CurrSim - 1][31] + 30;
	for (ig = 0; ig < 2; ig++){
		for (iy = 30; iy < 116; iy++){
			PrEP_15[iy][ig] = 0.0;
			PrEP_20[iy][ig] = 0.0;
		}
		if (StartRollout < 116){
			for (iy = StartRollout; iy < 116; iy++){
				PrEP_15[iy][ig] = FutureInterventions.out[CurrSim - 1][33];
				PrEP_20[iy][ig] = FutureInterventions.out[CurrSim - 1][33];
			}
		}
	}*/
}

void GetCurrBehavDbnM()
{
	int ia;

	for(ia=0; ia<81; ia++){
		CurrBehavDbn[ia][0][0] = MHU_ST.Total[ia] + MHC_ST.Total[ia] +
			(MHU_STM.Total[ia] + MHC_STM.Total[ia]) * (1.0 - MSMpartnersM[ia]);
		CurrBehavDbn[ia][1][0] = MHU_LTH.Total[ia] + MHC_LTH.Total[ia] +
			MHU_LTL.Total[ia] + MHC_LTL.Total[ia];
		CurrBehavDbn[ia][2][0] = MLU_ST.Total[ia] + MLC_ST.Total[ia] +
			(MLU_STM.Total[ia] + MLC_STM.Total[ia]) * (1.0 - MSMpartnersM[ia]);
		CurrBehavDbn[ia][3][0] = MLU_LTH.Total[ia] + MLC_LTH.Total[ia] +
			MLU_LTL.Total[ia] + MLC_LTL.Total[ia];
		CurrBehavDbnMSM[ia][0] = (MHU_STM.Total[ia] + MHC_STM.Total[ia]) * MSMpartnersM[ia];
		CurrBehavDbnMSM[ia][1] = (MLU_STM.Total[ia] + MLC_STM.Total[ia]) * MSMpartnersM[ia];
	}
}

void GetCurrBehavDbnF()
{
	int ia;

	for(ia=0; ia<81; ia++){
		// Note that we don't include FSW in CurrBehavDbn[ia][0][1], since
		// FSWs aren't eligible to form ST partnerships or LT partnerships.
		CurrBehavDbn[ia][0][1] = FH_ST.Total[ia];
		CurrBehavDbn[ia][1][1] = FH_LTH.Total[ia] + FH_LTL.Total[ia];
		CurrBehavDbn[ia][2][1] = FL_ST.Total[ia];
		CurrBehavDbn[ia][3][1] = FL_LTH.Total[ia] + FL_LTL.Total[ia];
	}
}

void UpdatePop()
{
	int ia, im, is, id;
	double Temp1, Temp2;

	for(ia=0; ia<10; ia++){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(im=0; im<12; im++){
			Temp1 += MaleChild.Total[ia*12+im];
			Temp2 += FemChild.Total[ia*12+im];
		}
		TotalPop[ia][0] = Temp1;
		TotalPop[ia][1] = Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(im=0; im<12; im++){
			Temp1 += MaleChild.ARTeligible[ia * 12 + im] + MaleChild.DiagARTeligible[ia * 12 + im];
			Temp2 += FemChild.ARTeligible[ia * 12 + im] + FemChild.DiagARTeligible[ia * 12 + im];
		}
		TotalNaiveElig[ia][0] = Temp1;
		TotalNaiveElig[ia][1] = Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(im=0; im<12; im++){
			Temp1 += MaleChild.OnARTearly[ia*12+im] + MaleChild.OnARTlate1st3m[ia*12+im] +
				MaleChild.OnARTlateAfter3m[ia*12+im];
			Temp2 += FemChild.OnARTearly[ia*12+im] + FemChild.OnARTlate1st3m[ia*12+im] +
				FemChild.OnARTlateAfter3m[ia*12+im];
		}
		if (ExcludeInterrupters == 0){
			TotalART[ia][0] = Temp1;
			TotalART[ia][1] = Temp2;
			TotalDiagnosed[ia][0] = 0.0;
			TotalDiagnosed[ia][1] = 0.0;
			TotalInterrupt[ia][0] = 0.0;
			TotalInterrupt[ia][1] = 0.0;
		}
		else{
			TotalART[ia][0] = Temp1 * OnARTpaed[ia];
			TotalART[ia][1] = Temp2 * OnARTpaed[ia];
			TotalDiagnosed[ia][0] = Temp1 * (1.0 - OnARTpaed[ia]);
			TotalDiagnosed[ia][1] = Temp2 * (1.0 - OnARTpaed[ia]);
			TotalInterrupt[ia][0] = Temp1 * (1.0 - OnARTpaed[ia]);
			TotalInterrupt[ia][1] = Temp2 * (1.0 - OnARTpaed[ia]);
		}
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (im = 0; im<12; im++){
			Temp1 += MaleChild.DiagChildAtBirthNoPMTCT[ia * 12 + im] + MaleChild.DiagChildAtBirthPMTCT[ia * 12 + im] +
				MaleChild.DiagChildAfterBirth[ia * 12 + im] + MaleChild.DiagARTeligible[ia * 12 + im] +
				MaleChild.StoppedART[ia * 12 + im];
			Temp2 += FemChild.DiagChildAtBirthNoPMTCT[ia * 12 + im] + FemChild.DiagChildAtBirthPMTCT[ia * 12 + im] +
				FemChild.DiagChildAfterBirth[ia * 12 + im] + FemChild.DiagARTeligible[ia * 12 + im] +
				FemChild.StoppedART[ia * 12 + im];
		}
		TotalDiagnosed[ia][0] += Temp1 + TotalART[ia][0];
		TotalDiagnosed[ia][1] += Temp2 + TotalART[ia][1];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(im=0; im<12; im++){
			Temp1 += MaleChild.PosChildAtBirthNoPMTCT[ia*12+im] + MaleChild.PosChildAtBirthPMTCT[ia*12+im] +
				MaleChild.PosChildAfterBirth[ia*12+im] + MaleChild.ARTeligible[ia*12+im];
			Temp2 += FemChild.PosChildAtBirthNoPMTCT[ia*12+im] + FemChild.PosChildAtBirthPMTCT[ia*12+im] +
				FemChild.PosChildAfterBirth[ia*12+im] + FemChild.ARTeligible[ia*12+im];
		}
		TotalPositive[ia][0] = Temp1 + TotalDiagnosed[ia][0];
		TotalPositive[ia][1] = Temp2 + TotalDiagnosed[ia][1];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (im = 0; im<12; im++){
			Temp1 += MaleChild.StoppedART[ia * 12 + im];
			Temp2 += FemChild.StoppedART[ia * 12 + im];
		}
		TotalInterrupt[ia][0] += Temp1;
		TotalInterrupt[ia][1] += Temp2;
	}
	for(ia=0; ia<81; ia++){
		TotalPop[ia+10][0] = MHU_virgin.Total[ia] + MHC_virgin.Total[ia] +
			MHU_ST.Total[ia] + MHC_ST.Total[ia] + MHU_STM.Total[ia] +
			MHC_STM.Total[ia] + MHU_LTH.Total[ia] + MHC_LTH.Total[ia] +
			MHU_LTL.Total[ia] + MHC_LTL.Total[ia] + MLU_virgin.Total[ia] +
			MLC_virgin.Total[ia] + MLU_ST.Total[ia] + MLC_ST.Total[ia] +
			MLU_STM.Total[ia] + MLC_STM.Total[ia] + MLU_LTH.Total[ia] +
			MLC_LTH.Total[ia] +MLU_LTL.Total[ia] + MLC_LTL.Total[ia];
		TotalPop[ia+10][1] = FH_virgin.Total[ia] + FH_ST.Total[ia] + FH_SW.Total[ia] +
			FH_LTH.Total[ia] + FH_LTL.Total[ia] + FL_virgin.Total[ia] + FL_ST.Total[ia] +
			FL_LTH.Total[ia] + FL_LTL.Total[ia];
		SumGroupsM[ia][0] = MHU_virgin.NegNoHCT[ia] + MHC_virgin.NegNoHCT[ia] +
			MHU_ST.NegNoHCT[ia] + MHC_ST.NegNoHCT[ia] + MHU_STM.NegNoHCT[ia] +
			MHC_STM.NegNoHCT[ia] + MHU_LTH.NegNoHCT[ia] + MHC_LTH.NegNoHCT[ia] +
			MHU_LTL.NegNoHCT[ia] + MHC_LTL.NegNoHCT[ia] + MLU_virgin.NegNoHCT[ia] +
			MLC_virgin.NegNoHCT[ia] + MLU_ST.NegNoHCT[ia] + MLC_ST.NegNoHCT[ia] +
			MLU_STM.NegNoHCT[ia] + MLC_STM.NegNoHCT[ia] + MLU_LTH.NegNoHCT[ia] +
			MLC_LTH.NegNoHCT[ia] +MLU_LTL.NegNoHCT[ia] + MLC_LTL.NegNoHCT[ia];
		SumGroupsF[ia][0] = FH_virgin.NegNoHCT[ia] + FH_ST.NegNoHCT[ia] + FH_SW.NegNoHCT[ia] +
			FH_LTH.NegNoHCT[ia] + FH_LTL.NegNoHCT[ia] + FL_virgin.NegNoHCT[ia] + FL_ST.NegNoHCT[ia] +
			FL_LTH.NegNoHCT[ia] + FL_LTL.NegNoHCT[ia];
		SumGroupsM[ia][1] = MHU_virgin.NegPastHCT[ia] + MHC_virgin.NegPastHCT[ia] +
			MHU_ST.NegPastHCT[ia] + MHC_ST.NegPastHCT[ia] + MHU_STM.NegPastHCT[ia] +
			MHC_STM.NegPastHCT[ia] + MHU_LTH.NegPastHCT[ia] + MHC_LTH.NegPastHCT[ia] +
			MHU_LTL.NegPastHCT[ia] + MHC_LTL.NegPastHCT[ia] + MLU_virgin.NegPastHCT[ia] +
			MLC_virgin.NegPastHCT[ia] + MLU_ST.NegPastHCT[ia] + MLC_ST.NegPastHCT[ia] +
			MLU_STM.NegPastHCT[ia] + MLC_STM.NegPastHCT[ia] + MLU_LTH.NegPastHCT[ia] +
			MLC_LTH.NegPastHCT[ia] +MLU_LTL.NegPastHCT[ia] + MLC_LTL.NegPastHCT[ia];
		SumGroupsF[ia][1] = FH_virgin.NegPastHCT[ia] + FH_ST.NegPastHCT[ia] + FH_SW.NegPastHCT[ia] +
			FH_LTH.NegPastHCT[ia] + FH_LTL.NegPastHCT[ia] + FL_virgin.NegPastHCT[ia] + FL_ST.NegPastHCT[ia] +
			FL_LTH.NegPastHCT[ia] + FL_LTL.NegPastHCT[ia];
		SumGroupsM[ia][2] = MHU_virgin.RegHCT[ia] + MHC_virgin.RegHCT[ia] +
			MHU_ST.RegHCT[ia] + MHC_ST.RegHCT[ia] + MHU_STM.RegHCT[ia] +
			MHC_STM.RegHCT[ia] + MHU_LTH.RegHCT[ia] + MHC_LTH.RegHCT[ia] +
			MHU_LTL.RegHCT[ia] + MHC_LTL.RegHCT[ia] + MLU_virgin.RegHCT[ia] +
			MLC_virgin.RegHCT[ia] + MLU_ST.RegHCT[ia] + MLC_ST.RegHCT[ia] +
			MLU_STM.RegHCT[ia] + MLC_STM.RegHCT[ia] + MLU_LTH.RegHCT[ia] +
			MLC_LTH.RegHCT[ia] +MLU_LTL.RegHCT[ia] + MLC_LTL.RegHCT[ia];
		SumGroupsF[ia][2] = FH_virgin.RegHCT[ia] + FH_ST.RegHCT[ia] + FH_SW.RegHCT[ia] +
			FH_LTH.RegHCT[ia] + FH_LTL.RegHCT[ia] + FL_virgin.RegHCT[ia] + FL_ST.RegHCT[ia] +
			FL_LTH.RegHCT[ia] + FL_LTL.RegHCT[ia];
		SumGroupsM[ia][3] = MHU_virgin.RegPrEP[ia] + MHC_virgin.RegPrEP[ia] +
			MHU_ST.RegPrEP[ia] + MHC_ST.RegPrEP[ia] + MHU_STM.RegPrEP[ia] +
			MHC_STM.RegPrEP[ia] + MHU_LTH.RegPrEP[ia] + MHC_LTH.RegPrEP[ia] +
			MHU_LTL.RegPrEP[ia] + MHC_LTL.RegPrEP[ia] + MLU_virgin.RegPrEP[ia] +
			MLC_virgin.RegPrEP[ia] + MLU_ST.RegPrEP[ia] + MLC_ST.RegPrEP[ia] +
			MLU_STM.RegPrEP[ia] + MLC_STM.RegPrEP[ia] + MLU_LTH.RegPrEP[ia] +
			MLC_LTH.RegPrEP[ia] +MLU_LTL.RegPrEP[ia] + MLC_LTL.RegPrEP[ia];
		SumGroupsF[ia][3] = FH_virgin.RegPrEP[ia] + FH_ST.RegPrEP[ia] + FH_SW.RegPrEP[ia] +
			FH_LTH.RegPrEP[ia] + FH_LTL.RegPrEP[ia] + FL_virgin.RegPrEP[ia] + FL_ST.RegPrEP[ia] +
			FL_LTH.RegPrEP[ia] + FL_LTL.RegPrEP[ia];
		SumGroupsF[ia][4] = FH_virgin.RegVM[ia] + FH_ST.RegVM[ia] + FH_SW.RegVM[ia] +
			FH_LTH.RegVM[ia] + FH_LTL.RegVM[ia] + FL_virgin.RegVM[ia] + FL_ST.RegVM[ia] +
			FL_LTH.RegVM[ia] + FL_LTL.RegVM[ia];
		for(is=0; is<5; is++){
			SumGroupsM[ia][5+is] = MHU_virgin.PosNoHCT[ia][is] + MHC_virgin.PosNoHCT[ia][is] +
				MHU_ST.PosNoHCT[ia][is] + MHC_ST.PosNoHCT[ia][is] + MHU_STM.PosNoHCT[ia][is] +
				MHC_STM.PosNoHCT[ia][is] + MHU_LTH.PosNoHCT[ia][is] + MHC_LTH.PosNoHCT[ia][is] +
				MHU_LTL.PosNoHCT[ia][is] + MHC_LTL.PosNoHCT[ia][is] + MLU_virgin.PosNoHCT[ia][is] +
				MLC_virgin.PosNoHCT[ia][is] + MLU_ST.PosNoHCT[ia][is] + MLC_ST.PosNoHCT[ia][is] +
				MLU_STM.PosNoHCT[ia][is] + MLC_STM.PosNoHCT[ia][is] + MLU_LTH.PosNoHCT[ia][is] +
				MLC_LTH.PosNoHCT[ia][is] +MLU_LTL.PosNoHCT[ia][is] + MLC_LTL.PosNoHCT[ia][is];
			SumGroupsF[ia][5+is] = FH_virgin.PosNoHCT[ia][is] + FH_ST.PosNoHCT[ia][is] + FH_SW.PosNoHCT[ia][is] +
				FH_LTH.PosNoHCT[ia][is] + FH_LTL.PosNoHCT[ia][is] + FL_virgin.PosNoHCT[ia][is] + FL_ST.PosNoHCT[ia][is] +
				FL_LTH.PosNoHCT[ia][is] + FL_LTL.PosNoHCT[ia][is];
			SumGroupsM[ia][10+is] = MHU_virgin.PosHCTpreHIV[ia][is] + MHC_virgin.PosHCTpreHIV[ia][is] +
				MHU_ST.PosHCTpreHIV[ia][is] + MHC_ST.PosHCTpreHIV[ia][is] + MHU_STM.PosHCTpreHIV[ia][is] +
				MHC_STM.PosHCTpreHIV[ia][is] + MHU_LTH.PosHCTpreHIV[ia][is] + MHC_LTH.PosHCTpreHIV[ia][is] +
				MHU_LTL.PosHCTpreHIV[ia][is] + MHC_LTL.PosHCTpreHIV[ia][is] + MLU_virgin.PosHCTpreHIV[ia][is] +
				MLC_virgin.PosHCTpreHIV[ia][is] + MLU_ST.PosHCTpreHIV[ia][is] + MLC_ST.PosHCTpreHIV[ia][is] +
				MLU_STM.PosHCTpreHIV[ia][is] + MLC_STM.PosHCTpreHIV[ia][is] + MLU_LTH.PosHCTpreHIV[ia][is] +
				MLC_LTH.PosHCTpreHIV[ia][is] +MLU_LTL.PosHCTpreHIV[ia][is] + MLC_LTL.PosHCTpreHIV[ia][is];
			SumGroupsF[ia][10+is] = FH_virgin.PosHCTpreHIV[ia][is] + FH_ST.PosHCTpreHIV[ia][is] + FH_SW.PosHCTpreHIV[ia][is] +
				FH_LTH.PosHCTpreHIV[ia][is] + FH_LTL.PosHCTpreHIV[ia][is] + FL_virgin.PosHCTpreHIV[ia][is] + FL_ST.PosHCTpreHIV[ia][is] +
				FL_LTH.PosHCTpreHIV[ia][is] + FL_LTL.PosHCTpreHIV[ia][is];
			SumGroupsM[ia][15+is] = MHU_virgin.PosDiagnosedPreART[ia][is] + MHC_virgin.PosDiagnosedPreART[ia][is] +
				MHU_ST.PosDiagnosedPreART[ia][is] + MHC_ST.PosDiagnosedPreART[ia][is] + MHU_STM.PosDiagnosedPreART[ia][is] +
				MHC_STM.PosDiagnosedPreART[ia][is] + MHU_LTH.PosDiagnosedPreART[ia][is] + MHC_LTH.PosDiagnosedPreART[ia][is] +
				MHU_LTL.PosDiagnosedPreART[ia][is] + MHC_LTL.PosDiagnosedPreART[ia][is] + MLU_virgin.PosDiagnosedPreART[ia][is] +
				MLC_virgin.PosDiagnosedPreART[ia][is] + MLU_ST.PosDiagnosedPreART[ia][is] + MLC_ST.PosDiagnosedPreART[ia][is] +
				MLU_STM.PosDiagnosedPreART[ia][is] + MLC_STM.PosDiagnosedPreART[ia][is] + MLU_LTH.PosDiagnosedPreART[ia][is] +
				MLC_LTH.PosDiagnosedPreART[ia][is] +MLU_LTL.PosDiagnosedPreART[ia][is] + MLC_LTL.PosDiagnosedPreART[ia][is];
			SumGroupsF[ia][15+is] = FH_virgin.PosDiagnosedPreART[ia][is] + FH_ST.PosDiagnosedPreART[ia][is] + FH_SW.PosDiagnosedPreART[ia][is] +
				FH_LTH.PosDiagnosedPreART[ia][is] + FH_LTL.PosDiagnosedPreART[ia][is] + FL_virgin.PosDiagnosedPreART[ia][is] + FL_ST.PosDiagnosedPreART[ia][is] +
				FL_LTH.PosDiagnosedPreART[ia][is] + FL_LTL.PosDiagnosedPreART[ia][is];
			SumGroupsM[ia][20+is] = MHU_virgin.OnARTpre500[ia][is] + MHC_virgin.OnARTpre500[ia][is] +
				MHU_ST.OnARTpre500[ia][is] + MHC_ST.OnARTpre500[ia][is] + MHU_STM.OnARTpre500[ia][is] +
				MHC_STM.OnARTpre500[ia][is] + MHU_LTH.OnARTpre500[ia][is] + MHC_LTH.OnARTpre500[ia][is] +
				MHU_LTL.OnARTpre500[ia][is] + MHC_LTL.OnARTpre500[ia][is] + MLU_virgin.OnARTpre500[ia][is] +
				MLC_virgin.OnARTpre500[ia][is] + MLU_ST.OnARTpre500[ia][is] + MLC_ST.OnARTpre500[ia][is] +
				MLU_STM.OnARTpre500[ia][is] + MLC_STM.OnARTpre500[ia][is] + MLU_LTH.OnARTpre500[ia][is] +
				MLC_LTH.OnARTpre500[ia][is] +MLU_LTL.OnARTpre500[ia][is] + MLC_LTL.OnARTpre500[ia][is];
			SumGroupsF[ia][20+is] = FH_virgin.OnARTpre500[ia][is] + FH_ST.OnARTpre500[ia][is] + FH_SW.OnARTpre500[ia][is] +
				FH_LTH.OnARTpre500[ia][is] + FH_LTL.OnARTpre500[ia][is] + FL_virgin.OnARTpre500[ia][is] + FL_ST.OnARTpre500[ia][is] +
				FL_LTH.OnARTpre500[ia][is] + FL_LTL.OnARTpre500[ia][is];
			SumGroupsM[ia][25+is] = MHU_virgin.OnART500[ia][is] + MHC_virgin.OnART500[ia][is] +
				MHU_ST.OnART500[ia][is] + MHC_ST.OnART500[ia][is] + MHU_STM.OnART500[ia][is] +
				MHC_STM.OnART500[ia][is] + MHU_LTH.OnART500[ia][is] + MHC_LTH.OnART500[ia][is] +
				MHU_LTL.OnART500[ia][is] + MHC_LTL.OnART500[ia][is] + MLU_virgin.OnART500[ia][is] +
				MLC_virgin.OnART500[ia][is] + MLU_ST.OnART500[ia][is] + MLC_ST.OnART500[ia][is] +
				MLU_STM.OnART500[ia][is] + MLC_STM.OnART500[ia][is] + MLU_LTH.OnART500[ia][is] +
				MLC_LTH.OnART500[ia][is] +MLU_LTL.OnART500[ia][is] + MLC_LTL.OnART500[ia][is];
			SumGroupsF[ia][25+is] = FH_virgin.OnART500[ia][is] + FH_ST.OnART500[ia][is] + FH_SW.OnART500[ia][is] +
				FH_LTH.OnART500[ia][is] + FH_LTL.OnART500[ia][is] + FL_virgin.OnART500[ia][is] + FL_ST.OnART500[ia][is] +
				FL_LTH.OnART500[ia][is] + FL_LTL.OnART500[ia][is];
			SumGroupsM[ia][30+is] = MHU_virgin.OnART350[ia][is] + MHC_virgin.OnART350[ia][is] +
				MHU_ST.OnART350[ia][is] + MHC_ST.OnART350[ia][is] + MHU_STM.OnART350[ia][is] +
				MHC_STM.OnART350[ia][is] + MHU_LTH.OnART350[ia][is] + MHC_LTH.OnART350[ia][is] +
				MHU_LTL.OnART350[ia][is] + MHC_LTL.OnART350[ia][is] + MLU_virgin.OnART350[ia][is] +
				MLC_virgin.OnART350[ia][is] + MLU_ST.OnART350[ia][is] + MLC_ST.OnART350[ia][is] +
				MLU_STM.OnART350[ia][is] + MLC_STM.OnART350[ia][is] + MLU_LTH.OnART350[ia][is] +
				MLC_LTH.OnART350[ia][is] +MLU_LTL.OnART350[ia][is] + MLC_LTL.OnART350[ia][is];
			SumGroupsF[ia][30+is] = FH_virgin.OnART350[ia][is] + FH_ST.OnART350[ia][is] + FH_SW.OnART350[ia][is] +
				FH_LTH.OnART350[ia][is] + FH_LTL.OnART350[ia][is] + FL_virgin.OnART350[ia][is] + FL_ST.OnART350[ia][is] +
				FL_LTH.OnART350[ia][is] + FL_LTL.OnART350[ia][is];
			SumGroupsM[ia][35+is] = MHU_virgin.OnART200[ia][is] + MHC_virgin.OnART200[ia][is] +
				MHU_ST.OnART200[ia][is] + MHC_ST.OnART200[ia][is] + MHU_STM.OnART200[ia][is] +
				MHC_STM.OnART200[ia][is] + MHU_LTH.OnART200[ia][is] + MHC_LTH.OnART200[ia][is] +
				MHU_LTL.OnART200[ia][is] + MHC_LTL.OnART200[ia][is] + MLU_virgin.OnART200[ia][is] +
				MLC_virgin.OnART200[ia][is] + MLU_ST.OnART200[ia][is] + MLC_ST.OnART200[ia][is] +
				MLU_STM.OnART200[ia][is] + MLC_STM.OnART200[ia][is] + MLU_LTH.OnART200[ia][is] +
				MLC_LTH.OnART200[ia][is] + MLU_LTL.OnART200[ia][is] + MLC_LTL.OnART200[ia][is];
			SumGroupsF[ia][35+is] = FH_virgin.OnART200[ia][is] + FH_ST.OnART200[ia][is] + FH_SW.OnART200[ia][is] +
				FH_LTH.OnART200[ia][is] + FH_LTL.OnART200[ia][is] + FL_virgin.OnART200[ia][is] + FL_ST.OnART200[ia][is] +
				FL_LTH.OnART200[ia][is] + FL_LTL.OnART200[ia][is];
		}
		for(is=0; is<4; is++){
			SumGroupsM[ia][40+is] = MHU_virgin.StoppedART[ia][is] + MHC_virgin.StoppedART[ia][is] +
				MHU_ST.StoppedART[ia][is] + MHC_ST.StoppedART[ia][is] + MHU_STM.StoppedART[ia][is] +
				MHC_STM.StoppedART[ia][is] + MHU_LTH.StoppedART[ia][is] + MHC_LTH.StoppedART[ia][is] +
				MHU_LTL.StoppedART[ia][is] + MHC_LTL.StoppedART[ia][is] + MLU_virgin.StoppedART[ia][is] +
				MLC_virgin.StoppedART[ia][is] + MLU_ST.StoppedART[ia][is] + MLC_ST.StoppedART[ia][is] +
				MLU_STM.StoppedART[ia][is] + MLC_STM.StoppedART[ia][is] + MLU_LTH.StoppedART[ia][is] +
				MLC_LTH.StoppedART[ia][is] + MLU_LTL.StoppedART[ia][is] + MLC_LTL.StoppedART[ia][is];
			SumGroupsF[ia][40+is] = FH_virgin.StoppedART[ia][is] + FH_ST.StoppedART[ia][is] + FH_SW.StoppedART[ia][is] +
				FH_LTH.StoppedART[ia][is] + FH_LTL.StoppedART[ia][is] + FL_virgin.StoppedART[ia][is] + FL_ST.StoppedART[ia][is] +
				FL_LTH.StoppedART[ia][is] + FL_LTL.StoppedART[ia][is];
		}
		TotalPositive[ia+10][0] = TotalPop[ia+10][0] - SumGroupsM[ia][0] - SumGroupsM[ia][1] -
			SumGroupsM[ia][2] - SumGroupsM[ia][3];
		TotalPositive[ia+10][1] = TotalPop[ia+10][1] - SumGroupsF[ia][0] - SumGroupsF[ia][1] -
			SumGroupsF[ia][2] - SumGroupsF[ia][3] - SumGroupsF[ia][4];
		Temp1 = 0.0;
		Temp2 = 0.0;
		if(ExcludeInterrupters==0){
			for(is=20; is<40; is++){
				Temp1 += SumGroupsM[ia][is];
				Temp2 += SumGroupsF[ia][is];
			}
		}
		else{
			for(is=0; is<4; is++){
				for(id=0; id<5; id++){
					Temp1 += SumGroupsM[ia][20+is*5+id] * OnARThalfIntDur[id][0];
					Temp2 += SumGroupsF[ia][20+is*5+id] * OnARThalfIntDur[id][1];
				}
			}
		}
		TotalART[ia+10][0] = Temp1;
		TotalART[ia+10][1] = Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (is = 20; is<44; is++){
			Temp1 += SumGroupsM[ia][is];
			Temp2 += SumGroupsF[ia][is];
		}
		TotalInterrupt[ia + 10][0] = Temp1 - TotalART[ia + 10][0];
		TotalInterrupt[ia + 10][1] = Temp2 - TotalART[ia + 10][1];
		TotalDiagnosed[ia + 10][0] = 0.0;
		TotalDiagnosed[ia + 10][1] = 0.0;
		for (is = 16; is < 44; is++){
			TotalDiagnosed[ia + 10][0] += SumGroupsM[ia][is];
			TotalDiagnosed[ia + 10][1] += SumGroupsF[ia][is];
		}
		TotalNaiveElig[ia+10][0] = SumGroupsM[ia][9] + SumGroupsM[ia][14] + SumGroupsM[ia][19];
		TotalNaiveElig[ia+10][1] = SumGroupsF[ia][9] + SumGroupsF[ia][14] + SumGroupsF[ia][19];
		TotalSexuallyExp[ia+10][0] = TotalPop[ia+10][0] - MHU_virgin.Total[ia] -
			MHC_virgin.Total[ia] - MLU_virgin.Total[ia] - MLC_virgin.Total[ia];
		TotalSexuallyExp[ia+10][1] = TotalPop[ia+10][1] - FH_virgin.Total[ia] -
			FL_virgin.Total[ia];
		//TotalMarried[ia+10][0] = TotalSexuallyExp[ia+10][0] - MHU_ST.Total[ia] -
		//	MHC_ST.Total[ia] - MHU_STM.Total[ia] - MHC_STM.Total[ia] -
		//	MLU_ST.Total[ia] - MLC_ST.Total[ia] - MLU_STM.Total[ia] - MLC_STM.Total[ia];
		TotalMarried[ia + 10][0] = MHU_LTH.Total[ia] + MHC_LTH.Total[ia] + MHU_LTL.Total[ia] +
			MHC_LTL.Total[ia] + MLU_LTH.Total[ia] + MLC_LTH.Total[ia] + MLU_LTL.Total[ia] +
			MLC_LTL.Total[ia];
		//TotalMarried[ia+10][1] = TotalSexuallyExp[ia+10][1] - FH_ST.Total[ia] -
		//	FH_SW.Total[ia] - FL_ST.Total[ia];
		TotalMarried[ia + 10][1] = FH_LTH.Total[ia] + FH_LTL.Total[ia] +
			FL_LTH.Total[ia] + FL_LTL.Total[ia];
	}
	if (CalibARTtotals == 1 || CalibARTtotalsP == 1 || CalibARTcoverage){ GetCurrART(); }

	// Get virgin totals
	for(ia=0; ia<20; ia++){
		SumGroupsVM[ia][0] = MHU_virgin.NegNoHCT[ia] + MHC_virgin.NegNoHCT[ia] +
			MLU_virgin.NegNoHCT[ia] + MLC_virgin.NegNoHCT[ia];
		SumGroupsVF[ia][0] = FH_virgin.NegNoHCT[ia] + FL_virgin.NegNoHCT[ia];
		SumGroupsVM[ia][1] = MHU_virgin.NegPastHCT[ia] + MHC_virgin.NegPastHCT[ia] +
			MLU_virgin.NegPastHCT[ia] + MLC_virgin.NegPastHCT[ia];
		SumGroupsVF[ia][1] = FH_virgin.NegPastHCT[ia] + FL_virgin.NegPastHCT[ia];
		SumGroupsVM[ia][2] = MHU_virgin.RegHCT[ia] + MHC_virgin.RegHCT[ia] +
			MLU_virgin.RegHCT[ia] + MLC_virgin.RegHCT[ia];
		SumGroupsVF[ia][2] = FH_virgin.RegHCT[ia] + FL_virgin.RegHCT[ia];
		SumGroupsVM[ia][3] = MHU_virgin.RegPrEP[ia] + MHC_virgin.RegPrEP[ia] +
			MLU_virgin.RegPrEP[ia] + MLC_virgin.RegPrEP[ia];
		SumGroupsVF[ia][3] = FH_virgin.RegPrEP[ia] + FL_virgin.RegPrEP[ia];
		SumGroupsVF[ia][4] = FH_virgin.RegVM[ia] + FL_virgin.RegVM[ia];
		for(is=0; is<5; is++){
			SumGroupsVM[ia][5+is] = MHU_virgin.PosNoHCT[ia][is] + MHC_virgin.PosNoHCT[ia][is] +
				MLU_virgin.PosNoHCT[ia][is] + MLC_virgin.PosNoHCT[ia][is];
			SumGroupsVF[ia][5+is] = FH_virgin.PosNoHCT[ia][is] + FL_virgin.PosNoHCT[ia][is];
			SumGroupsVM[ia][10+is] = MHU_virgin.PosHCTpreHIV[ia][is] + MHC_virgin.PosHCTpreHIV[ia][is] +
				MLU_virgin.PosHCTpreHIV[ia][is] + MLC_virgin.PosHCTpreHIV[ia][is];
			SumGroupsVF[ia][10+is] = FH_virgin.PosHCTpreHIV[ia][is] + FL_virgin.PosHCTpreHIV[ia][is];
			SumGroupsVM[ia][15+is] = MHU_virgin.PosDiagnosedPreART[ia][is] + MHC_virgin.PosDiagnosedPreART[ia][is] +
				MLU_virgin.PosDiagnosedPreART[ia][is] + MLC_virgin.PosDiagnosedPreART[ia][is];
			SumGroupsVF[ia][15+is] = FH_virgin.PosDiagnosedPreART[ia][is] + FL_virgin.PosDiagnosedPreART[ia][is];
			SumGroupsVM[ia][20+is] = MHU_virgin.OnARTpre500[ia][is] + MHC_virgin.OnARTpre500[ia][is] +
				MLU_virgin.OnARTpre500[ia][is] + MLC_virgin.OnARTpre500[ia][is];
			SumGroupsVF[ia][20+is] = FH_virgin.OnARTpre500[ia][is] + FL_virgin.OnARTpre500[ia][is];
			SumGroupsVM[ia][25+is] = MHU_virgin.OnART500[ia][is] + MHC_virgin.OnART500[ia][is] +
				MLU_virgin.OnART500[ia][is] + MLC_virgin.OnART500[ia][is];
			SumGroupsVF[ia][25+is] = FH_virgin.OnART500[ia][is] + FL_virgin.OnART500[ia][is];
			SumGroupsVM[ia][30+is] = MHU_virgin.OnART350[ia][is] + MHC_virgin.OnART350[ia][is] +
				MLU_virgin.OnART350[ia][is] + MLC_virgin.OnART350[ia][is];
			SumGroupsVF[ia][30+is] = FH_virgin.OnART350[ia][is] + FL_virgin.OnART350[ia][is];
			SumGroupsVM[ia][35+is] = MHU_virgin.OnART200[ia][is] + MHC_virgin.OnART200[ia][is] +
				MLU_virgin.OnART200[ia][is] + MLC_virgin.OnART200[ia][is];
			SumGroupsVF[ia][35+is] = FH_virgin.OnART200[ia][is] + FL_virgin.OnART200[ia][is];
		}
		for(is=0; is<4; is++){
			SumGroupsVM[ia][40+is] = MHU_virgin.StoppedART[ia][is] + MHC_virgin.StoppedART[ia][is] +
				MLU_virgin.StoppedART[ia][is] + MLC_virgin.StoppedART[ia][is];
			SumGroupsVF[ia][40+is] = FH_virgin.StoppedART[ia][is] + FL_virgin.StoppedART[ia][is];
		}
	}
}

void UpdateFSWdemand()
{
	int ia;
	double NegContacts;

	MHU_ST.GetFSWcontacts();
	MHC_ST.GetFSWcontacts();
	MHU_LTH.GetFSWcontacts();
	MHC_LTH.GetFSWcontacts();
	MHU_LTL.GetFSWcontacts();
	MHC_LTL.GetFSWcontacts();

	TotalFSW = 0.0;
	for(ia=0; ia<81; ia++){
		TotalFSW += MHU_ST.FSWcontactsByAge[ia] + MHC_ST.FSWcontactsByAge[ia] +
			MHU_LTH.FSWcontactsByAge[ia] + MHC_LTH.FSWcontactsByAge[ia] +
			MHU_LTL.FSWcontactsByAge[ia] + MHC_LTL.FSWcontactsByAge[ia];
		//TotalFSW += (CurrBehavDbn[ia][0][0] + CurrBehavDbn[ia][1][0] *
		//	FSWcontactMarried) * FSWcontactRate[ia];
	}
	TotalFSW = TotalFSW/ClientsPA;

	// Calculate HIV prevalence in clients
	if(CurrMonth==0 && FixedUncertainty==1){
		NegContacts = 0.0;
		for(ia=0; ia<81; ia++){
			NegContacts += (MHU_ST.NegNoHCT[ia] + MHU_ST.NegPastHCT[ia] +
				MHC_ST.NegNoHCT[ia] + MHC_ST.NegPastHCT[ia] + FSWcontactMarried *
				(MHU_LTH.NegNoHCT[ia] + MHU_LTH.NegPastHCT[ia] +
				MHC_LTH.NegNoHCT[ia] + MHC_LTH.NegPastHCT[ia] +
				MHU_LTL.NegNoHCT[ia] + MHU_LTL.NegPastHCT[ia] +
				MHC_LTL.NegNoHCT[ia] + MHC_LTL.NegPastHCT[ia])) * FSWcontactRate[ia];
		}
		PrevClients.out[CurrSim-1][CurrYear-1985] = 1.0 - NegContacts/(TotalFSW * ClientsPA);
		NegContacts = 0.0;
		for(ia=0; ia<81; ia++){
			NegContacts += (MHU_ST.NegNoHCT[ia] + MHU_ST.NegPastHCT[ia] +
				MHC_ST.NegNoHCT[ia] + MHC_ST.NegPastHCT[ia]) * (1.0 - exp(-FSWcontactRate[ia])) +
				(MHU_LTH.NegNoHCT[ia] + MHU_LTH.NegPastHCT[ia] +
				MHC_LTH.NegNoHCT[ia] + MHC_LTH.NegPastHCT[ia] +
				MHU_LTL.NegNoHCT[ia] + MHU_LTL.NegPastHCT[ia] +
				MHC_LTL.NegNoHCT[ia] + MHC_LTL.NegPastHCT[ia]) *
				(1.0 - exp(-FSWcontactMarried * FSWcontactRate[ia]));
		}
		NegClients.out[CurrSim-1][CurrYear-1985] = NegContacts;
	}
}

void UpdateAgePrefsF()
{
	int ia, ib, ig, ind;
	double AdjBeta, ChangeMaleCohort, ChangeMaleMarried;
	double term1, term2, NewAgeDif, NewSD;
	double X, A, B, P;

	if (VaryFutureInterventions == 1 && CurrYear>=2012){
		if (CurrYear >= 2016){ NewAgeDif = FutureInterventions.out[CurrSim - 1][11]; }
		else{
			NewAgeDif = MeanAgeDif + 0.2 * (CurrYear - 2011) *
				(FutureInterventions.out[CurrSim - 1][11] - MeanAgeDif);
		}
		NewSD = SDageDif * NewAgeDif / MeanAgeDif;
		for (ia = 0; ia < 81; ia++){
			GammaParametersST[ia][1] = (NewSD * NewSD) / (NewAgeDif + ia - MinPartnerAge[ia]);
			GammaParametersST[ia][0] = (NewAgeDif + ia - MinPartnerAge[ia]) / GammaParametersST[ia][1];
		}
	}

	// Calculate female age preference matrices (ia = 0 corresponds to age 10)
	for(ia=0; ia<81; ia++){
		if(ia<75){
			if(CurrYear==StartYear){
				ChangeMaleCohort = -log(StartingPop[ia+15][0]/StartingPop[ia+10][0])/5.0;}
			else{
				if(TotalPop[ia+15][0] > 0.0 && TotalPop[ia+10][0] > 0.0){
					ChangeMaleCohort = -log(TotalPop[ia+15][0]/TotalPop[ia+10][0])/5.0;}
				else{
					ChangeMaleCohort = 0.00001;} // Arbitrarilty low value
			}
		}
		if(ia<70){
			if(CurrYear==StartYear){
				ChangeMaleMarried = -log(StartingPop[ia+20][0]/StartingPop[ia+10][0])/10.0;}
			else{
				if(TotalPop[ia+20][0] > 0.0 && TotalPop[ia+10][0] > 0.0){
					ChangeMaleMarried = -log(TotalPop[ia+20][0]/TotalPop[ia+10][0])/10.0;}
				else{
					ChangeMaleMarried = 0.00001;} // Arbitrarilty low value
			}
		}
		// ST age preferences
		AdjBeta = 1.0/(ChangeMaleCohort + 1.0/GammaParametersST[ia][1]);
		ind = 1;
		A = GammaParametersST[ia][0];
		B = 1.0/AdjBeta;
		term1 = 0.0;
		for(ib=0; ib<81; ib++){
			term2 = term1;
			X = ib + 0.5 - MinPartnerAge[ia];
			if(X<0.0){X = 0.0;}
			P = 0.0;
			cdfgam(&ind,&P,0,&X,&A,&B,0,0);
			term1 = P;
			if(ib<80){
				AgePrefST[ia][ib][1] = term1 - term2;}
			else{
				AgePrefST[ia][ib][1] = 1.0 - term2;}
		}
		// LT age preferences
		if(ia>=5){
			AdjBeta = 1.0/(ChangeMaleMarried + 1.0/GammaParametersLT[ia][1]);
			ind = 1;
			A = GammaParametersLT[ia][0];
			B = 1.0/AdjBeta;
			term1 = 0.0;
			for(ib=0; ib<81; ib++){
				term2 = term1;
				X = ib + 0.5 - MinPartnerAge[ia];
				if(X<0.0){X = 0.0;}
				P = 0.0;
				cdfgam(&ind,&P,0,&X,&A,&B,0,0);
				term1 = P;
				if(ib<80){
					AgePrefLT[ia][ib][1] = term1 - term2;}
				else{
					AgePrefLT[ia][ib][1] = 1.0 - term2;}
			}
		}
	}
}

void UpdatePartnerAcqM()
{
	int ia, ib;
	double STrels[81][2], LTrels[81][2], Temp;

	// Calculate total ST & LT partnerships by age of female partner
	for(ia=0; ia<81; ia++){
		STrels[ia][1] = PartnerRate20F * PartnerAcqF[ia] *
			(CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][1][1] * RRpartnerMarried[1] +
			CurrBehavDbn[ia][2][1] * RRpartnerLow[1]);
		LTrels[ia][1] = CurrBehavDbn[ia][1][1] + CurrBehavDbn[ia][3][1];
	}

	// Calculate total ST partnerships by age of male partner and get PartnerAcqM
	for(ia=0; ia<81; ia++){
		Temp = 0.0;
		for(ib=0; ib<81; ib++){
			Temp += STrels[ib][1] * AgePrefST[ib][ia][1];}
		STrels[ia][0] = Temp;
		PartnerAcqM[ia] = STrels[ia][0]/(CurrBehavDbn[ia][0][0] + CurrBehavDbn[ia][1][0] *
			RRpartnerMarried[0] + CurrBehavDbn[ia][2][0] * RRpartnerLow[0]);
	}

	// Calculate ST age prefs
	for(ia=0; ia<81; ia++){
		for(ib=0; ib<81; ib++){
			if(STrels[ia][0]==0.0){
				AgePrefST[ia][ib][0] = 0.0;}
			else{
				AgePrefST[ia][ib][0] = STrels[ib][1] * AgePrefST[ib][ia][1] / STrels[ia][0];}
		}
	}

	// Calculate total LT partnerships by age of male partner
	for(ia=0; ia<81; ia++){
		Temp = 0.0;
		for(ib=0; ib<81; ib++){
			Temp += LTrels[ib][1] * AgePrefLT[ib][ia][1];}
		LTrels[ia][0] = Temp;
	}

	// Calculate LT age prefs
	for(ia=0; ia<81; ia++){
		for(ib=0; ib<81; ib++){
			if(LTrels[ia][0]==0.0){
				AgePrefLT[ia][ib][0] = 0.0;}
			else{
				AgePrefLT[ia][ib][0] = LTrels[ib][1] * AgePrefLT[ib][ia][1] / LTrels[ia][0];}
		}
	}

	// Calculate freq of sex in LT rels (col B in Condoms sheet)
	for(ia=0; ia<76; ia++){
		Temp = 0.0;
		for(ib=0; ib<76; ib++){
			Temp += AgePrefLT[ia+5][ib+5][0] * SexFreqMarital[ib][1];}
		SexFreqMarital[ia][0] = Temp;
	}
}

void UpdateMixingST()
{
	int ia;
	double NewSTpartners[2][2]; // Demand for new partners by risk (1st index) & sex (2nd index)
	double NewMSMpartners[2]; // By risk group
	double Temp;

	// New heterosexual partnerships

	Temp = 0.0;
	for(ia=0; ia<81; ia++){
		Temp += PartnerAcqM[ia] * (CurrBehavDbn[ia][0][0] + CurrBehavDbn[ia][1][0] *
			RRpartnerMarried[0]);}
	NewSTpartners[0][0] = Temp;

	Temp = 0.0;
	for(ia=0; ia<81; ia++){
		Temp += PartnerAcqM[ia] * CurrBehavDbn[ia][2][0] * RRpartnerLow[0];}
	NewSTpartners[1][0] = Temp;

	Temp = 0.0;
	for(ia=0; ia<81; ia++){
		Temp += PartnerAcqF[ia] * (CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][1][1] *
			RRpartnerMarried[1]) * PartnerRate20F;}
	NewSTpartners[0][1] = Temp;

	Temp = 0.0;
	for(ia=0; ia<81; ia++){
		Temp += PartnerAcqF[ia] * CurrBehavDbn[ia][2][1] * RRpartnerLow[1] * PartnerRate20F;}
	NewSTpartners[1][1] = Temp;

	CurrSThigh[1][0] = (1.0 - Assortativeness) + Assortativeness * NewSTpartners[0][0]/
		(NewSTpartners[0][0] + NewSTpartners[1][0]);
	CurrSThigh[1][1] = Assortativeness * NewSTpartners[0][0]/(NewSTpartners[0][0] +
		NewSTpartners[1][0]);
	CurrSThigh[0][0] = NewSTpartners[0][1] * CurrSThigh[1][0]/(NewSTpartners[0][1] *
		CurrSThigh[1][0] + NewSTpartners[1][1] * CurrSThigh[1][1]);
	CurrSThigh[0][1] = NewSTpartners[0][1] * (1.0 - CurrSThigh[1][0])/(NewSTpartners[0][1] *
		(1.0 - CurrSThigh[1][0]) + NewSTpartners[1][1] * (1.0 - CurrSThigh[1][1]));

	// New MSM partnerships

	Temp = 0.0;
	for (ia = 0; ia<81; ia++){
		Temp += PartnerAcqM[ia] * CurrBehavDbnMSM[ia][0]; }
	NewMSMpartners[0] = Temp;

	Temp = 0.0;
	for (ia = 0; ia<81; ia++){
		Temp += PartnerAcqM[ia] * CurrBehavDbnMSM[ia][1] * RRpartnerLow[0]; }
	NewMSMpartners[1] = Temp;

	CurrSThighMSM[0] = (1.0 - Assortativeness) + Assortativeness * NewMSMpartners[0] /
		(NewMSMpartners[0] + NewMSMpartners[1]);
	CurrSThighMSM[1] = Assortativeness * NewMSMpartners[0] /
		(NewMSMpartners[0] + NewMSMpartners[1]);
}

void UpdateCondomUse()
{
	int ia, ib;
	double CondomUse20, Temp1, Temp2, Temp3, Temp4, OddsCondomST[81][2], OddsCondomLT[81][2], OddsCondomFSW;

	Temp1 = pow((CurrYear - 1985) / MedianCondomBCC, ShapeCondomBCC);
	Temp2 = 1.0 - pow(0.5, Temp1);
	Temp3 = pow((CurrYear - 1985) / (MedianCondomBCC * 2.0), 2.0 * ShapeCondomBCC);
	Temp4 = 1.0 - pow(0.5, Temp3);
	CondomUse20 = (InitCondom + IncrCondomBCC * Temp2) * CondomAdjProv + RiskCompensation * Temp4;
	if (CondomUse20 < 0.0001){ CondomUse20 = 0.0001; }
	if (CondomUse20 > 0.9999){ CondomUse20 = 0.9999; }

	// Adjust for reporting bias
	Temp1 = 1.0 - CondomBias*(1.0 - RRcondomContracep);
	CondomUse20 *= Temp1;

	// Calculate female rates of condom use
	for(ia=0; ia<81; ia++){
		ProbCondomST[ia][1] = CondomUse20 * pow(AgeEffectCondom, ia - 10.0);
		if (ProbCondomST[ia][1] > 0.9999){ ProbCondomST[ia][1] = 0.9999; }
		if(ia>=5){
			ProbCondomLT[ia][1] = ProbCondomST[ia][1] * RRcondomMarital;
			if (ProbCondomLT[ia][1] > 0.9999){ ProbCondomLT[ia][1] = 0.9999; }
		}
		OddsCondomST[ia][1] = ProbCondomST[ia][1] / (1 - ProbCondomST[ia][1]);
		OddsCondomST[ia][1] *= STreduction;
		ProbCondomST[ia][1] = OddsCondomST[ia][1] / (1 + OddsCondomST[ia][1]);
		
		OddsCondomLT[ia][1] = ProbCondomLT[ia][1] / (1 - ProbCondomLT[ia][1]);
		OddsCondomLT[ia][1] *= LTreduction;
		ProbCondomLT[ia][1] = OddsCondomLT[ia][1] / (1 + OddsCondomLT[ia][1]);
	}

	ProbCondomFSW = 1.0 / (1.0 + (1.0 - CondomUse20) / (CondomUse20 * ORcondomFSW));
	OddsCondomFSW = ProbCondomFSW / (1 - ProbCondomFSW);
	OddsCondomFSW *= FSWreduction;
	ProbCondomFSW = OddsCondomFSW / (1 + OddsCondomFSW);

	// Calculate male rates of condom use
	for(ia=0; ia<81; ia++){
		Temp1 = 0.0;
		for(ib=0; ib<81; ib++){
			Temp1 += AgePrefST[ia][ib][0] * ProbCondomST[ib][1];}
		ProbCondomST[ia][0] = Temp1;
	}
	for(ia=5; ia<81; ia++){
		Temp1 = 0.0;
		for(ib=5; ib<81; ib++){
			Temp1 += AgePrefLT[ia][ib][0] * ProbCondomLT[ib][1];}
		ProbCondomLT[ia][0] = Temp1;
	}
}

void UpdateFert()
{
	int ia, is, iy, offset;
	double Temp;

	offset = CurrYear - StartYear;
	for(ia=0; ia<36; ia++){
		if(CurrYear<=2020){
			if(ia==0){
				FertByAgeLB[0] = 0.5 * ObservedFert[0][offset];}
			else if(ia<35){
				FertByAgeLB[ia] = 0.5 * (ObservedFert[ia-1][offset] +
					ObservedFert[ia][offset]);}
			else{
				FertByAgeLB[ia] = 0.5 * ObservedFert[ia-1][offset];}
			Temp = 0.0;
			for(is=0; is<44; is++){
				if(ia<16){
					Temp += RelativeFert[is] * (SumGroupsF[ia+4][is] -
						SumGroupsVF[ia+4][is]);}
				else{
					Temp += RelativeFert[is] * SumGroupsF[ia+4][is];}
			}
			HIVnegSEfert[ia] = FertByAgeLB[ia] * TotalPop[ia+14][1]/Temp;
		}
		else{
			iy = CurrYear - 2020;
			if(ia<35){
				AdjustedFert[ia] = UltFert[ia] * StartingPop[15+ia][1]/
					(StartingPop[15+ia][1] - InitBehavDbn[ia+5][0][1] -
					InitBehavDbn[ia+5][3][1]);
			}
			if(ia==0){
				CurrentFert[ia] = StoredFert[ia] * pow(RednFert[ia], iy) +
					0.5 * AdjustedFert[ia] * (1.0 - pow(RednFert[ia], iy));
			}
			else if(ia<35){
				CurrentFert[ia] = StoredFert[ia] * 0.5 * (pow(RednFert[ia], iy) +
					pow(RednFert[ia-1], iy)) + 0.5 * (AdjustedFert[ia-1] *
					(1.0 - pow(RednFert[ia-1], iy)) + AdjustedFert[ia] * (1.0 -
					pow(RednFert[ia], iy)));
			}
			else if(ia==35){
				CurrentFert[ia] = StoredFert[ia] * pow(RednFert[ia-1], iy) +
					0.5 * AdjustedFert[ia-1] * (1.0 - pow(RednFert[ia-1], iy));
			}
		}
	}
	if(CurrYear>2020){
		for(ia=0; ia<36; ia++){
			FertByAgeLB[ia] = CurrentFert[ia];
			HIVnegSEfert[ia] = FertByAgeLB[ia];
		}
	}
	if(CurrYear==2020 && CurrMonth==6){
		for(ia=0; ia<36; ia++){
			StoredFert[ia] = HIVnegSEfert[ia];}
	}

	// Calculate FertLastYr
	if (CurrMonth == 5){
		for (ia = 0; ia<36; ia++){
			FertLastYr[ia][0] = HIVnegSEfert[ia];
			FertLastYr[ia][1] = 0.0;
			Temp = 0.0;
			for (is = 15; is < 44; is++){
				if (ia < 16){
					FertLastYr[ia][1] += (SumGroupsF[ia + 4][is] - SumGroupsVF[ia + 4][is]) *
						HIVnegSEfert[ia] * RelativeFert[is];
					Temp += SumGroupsF[ia + 4][is] - SumGroupsVF[ia + 4][is];
				}
				else{
					FertLastYr[ia][1] += SumGroupsF[ia + 4][is] * HIVnegSEfert[ia] * RelativeFert[is];
					Temp += SumGroupsF[ia + 4][is];
				}
			}
			if (Temp > 0.0){ FertLastYr[ia][1] = FertLastYr[ia][1] / Temp; }
			else{ FertLastYr[ia][1] = FertLastYr[ia][0]; }
		}
	}
}

void UpdateBirths()
{
	int ia, ic, is, iy;
	double Temp1, Temp2[4], TempShape, TempMF, TempEBF, AveBFdur[2], EverFeedPrev[3];

	// Calculate LactatingAdj
	if (CurrMonth == 0 && CurrYear>StartYear){
		iy = CurrYear - StartYear - 1; // Note the -1 offset to give us previous year rates.
		EverFeedPrev[0] = EverFeed[0];
		EverFeedPrev[1] = (EverFeed[1] / (EverFeed[1] + EverFeed[2])) * (1.0 - NoBFpropn[iy]);
		EverFeedPrev[2] = (EverFeed[2] / (EverFeed[1] + EverFeed[2])) * (1.0 - NoBFpropn[iy]);
		TempShape = 1.0 + 1.0 / ShapeFeed[0];
		AveBFdur[0] = gamma_log(&TempShape);
		AveBFdur[0] *= EverFeedPrev[0] * (MedianFeed[0] / 12.0) / pow(log(2.0), 1.0 / ShapeFeed[0]);
		TempShape = 1.0 + 1.0 / ShapeFeed[1];
		TempMF = gamma_log(&TempShape);
		TempMF *= (MedianFeed[1] / 12.0) / pow(log(2.0), 1.0 / ShapeFeed[1]);
		TempShape = 1.0 + 1.0 / ShapeFeed[2];
		TempEBF = gamma_log(&TempShape);
		TempEBF *= (MedianFeed[2] / 12.0) / pow(log(2.0), 1.0 / ShapeFeed[2]);
		AveBFdur[1] = TempMF * (EverFeedPrev[1] + EverFeedPrev[2] * (1.0 - AbruptWeaningFirst3)) +
			TempEBF * EverFeedPrev[2];
		LactatingAdj[0] = 1.0;
		for (ia = 1; ia < 36; ia++){
			LactatingAdj[ia] = (1.0 - FertLastYr[ia - 1][1] * AveBFdur[1]) /
				(1.0 - FertLastYr[ia - 1][0] * AveBFdur[0]);
		}
	}
	if (CurrMonth == 0 && CurrYear == StartYear){
		for (ia = 0; ia < 36; ia++){ LactatingAdj[ia] = 1.0; }
	}

	for(ia=0; ia<16; ia++){
		// Births to acutely-infected mothers
		Temp1 = 0.0;
		for(is=0; is<3; is++){
			Temp1 += RelativeFert[5 + is * 5] * (SumGroupsF[ia + 4][5 + is * 5] -
				SumGroupsVF[ia+4][5+is*5]);}
		BirthsByHIVstage[ia][8] = HIVnegSEfert[ia] * Temp1;
		// Births to ART-naive HIV-positive mothers
		for(ic=0; ic<4; ic++){
			Temp1 = 0.0;
			for(is=0; is<2; is++){
				Temp1 += RelativeFert[6+ic+is*5] * (SumGroupsF[ia+4][6+ic+is*5] -
					SumGroupsVF[ia+4][6+ic+is*5]);
			}
			Temp1 += RelativeFert[16 + ic] * LactatingAdj[ia] * (SumGroupsF[ia + 4][16 + ic] -
				SumGroupsVF[ia + 4][16 + ic]);
			Temp1 += RelativeFert[40 + ic] * LactatingAdj[ia] * (SumGroupsF[ia + 4][40 + ic] -
				SumGroupsVF[ia+4][40 + ic]);
			BirthsByHIVstage[ia][2+ic] = HIVnegSEfert[ia] * Temp1;
		}
		BirthsByHIVstage[ia][2] += BirthsByHIVstage[ia][8];
		// Births to ART-experienced mothers
		Temp1 = 0.0;
		for(is=0; is<20; is++){
			Temp1 += RelativeFert[20 + is] * LactatingAdj[ia] * (SumGroupsF[ia + 4][20 + is] -
				SumGroupsVF[ia+4][20+is]);
		}
		BirthsByHIVstage[ia][6] = HIVnegSEfert[ia] * Temp1;
		// Births to HIV-diagnosed mothers
		Temp1 = 0.0;
		for(is=0; is<4; is++){
			Temp1 += RelativeFert[16 + is] * LactatingAdj[ia] * (SumGroupsF[ia + 4][16 + is] -
				SumGroupsVF[ia + 4][16 + is]) + RelativeFert[40 + is] * LactatingAdj[ia] *
				(SumGroupsF[ia+4][40+is] - SumGroupsVF[ia+4][40+is]);
		}
		BirthsByHIVstage[ia][7] = HIVnegSEfert[ia] * Temp1 + BirthsByHIVstage[ia][6];
		// Births to HIV-negative mothers
		Temp1 = 0.0;
		for(is=0; is<4; is++){
			Temp1 += RelativeFert[is] * (SumGroupsF[ia+4][is] -
				SumGroupsVF[ia+4][is]);
		}
		BirthsByHIVstage[ia][1] = HIVnegSEfert[ia] * Temp1;
		// Total births
		Temp1 = 0.0;
		for(is=1; is<7; is++){
			Temp1 += BirthsByHIVstage[ia][is];}
		BirthsByHIVstage[ia][0] = Temp1;
		if (ExcludeInterrupters == 1){
			// Births to mothers currently on ART
			Temp1 = 0.0;
			for (is = 0; is<20; is++){
				Temp1 += RelativeFertART[is] * LactatingAdj[ia] * (SumGroupsF[ia + 4][20 + is] -
					SumGroupsVF[ia + 4][20 + is]);
			}
			BirthsByHIVstageAdj[ia][4] = HIVnegSEfert[ia] * Temp1;
			// Births to HIV-positive mothers not currently on ART
			for (ic = 0; ic < 4; ic++){
				Temp1 = 0.0;
				for (is = 0; is < 2; is++){ // looping across testing histories
					Temp1 += RelativeFert[6 + ic + is * 5] * (SumGroupsF[ia + 4][6 + ic + is * 5] -
						SumGroupsVF[ia + 4][6 + ic + is * 5]);
				}
				Temp1 += RelativeFert[16 + ic] * LactatingAdj[ia] *
					(SumGroupsF[ia + 4][16 + ic] - SumGroupsVF[ia + 4][16 + ic]);
				for (is = 0; is<5; is++){ // looping across ART durations
					Temp1 += (RelativeFert[is + ic * 5 + 20] - RelativeFertART[is + ic * 5]) * LactatingAdj[ia] *
						(SumGroupsF[ia + 4][20 + ic * 5 + is] - SumGroupsVF[ia + 4][20 + ic * 5 + is]);
				}
				Temp1 += RelativeFert[40 + ic] * LactatingAdj[ia] * (SumGroupsF[ia + 4][40 + ic] -
					SumGroupsVF[ia + 4][40 + ic]);
				BirthsByHIVstageAdj[ia][ic] = HIVnegSEfert[ia] * Temp1;
			}
			BirthsByHIVstageAdj[ia][0] += BirthsByHIVstage[ia][8];
		}
	}
	for(ia=16; ia<36; ia++){
		// Births to acutely-infected mothers
		Temp1 = 0.0;
		for(is=0; is<3; is++){
			Temp1 += RelativeFert[5+is*5] * SumGroupsF[ia+4][5+is*5];}
		BirthsByHIVstage[ia][8] = HIVnegSEfert[ia] * Temp1;
		// Births to ART-naive HIV-positive mothers
		for(ic=0; ic<4; ic++){
			Temp1 = 0.0;
			for(is=0; is<2; is++){
				Temp1 += RelativeFert[6+ic+is*5] * SumGroupsF[ia+4][6+ic+is*5];}
			Temp1 += RelativeFert[16 + ic] * LactatingAdj[ia] * SumGroupsF[ia + 4][16 + ic];
			Temp1 += RelativeFert[40 + ic] * LactatingAdj[ia] * SumGroupsF[ia + 4][40 + ic];
			BirthsByHIVstage[ia][2+ic] = HIVnegSEfert[ia] * Temp1;
		}
		BirthsByHIVstage[ia][2] += BirthsByHIVstage[ia][8];
		// Births to ART-experienced mothers
		Temp1 = 0.0;
		for(is=0; is<20; is++){
			Temp1 += RelativeFert[20 + is] * LactatingAdj[ia] * SumGroupsF[ia + 4][20 + is];}
		BirthsByHIVstage[ia][6] = HIVnegSEfert[ia] * Temp1;
		// Births to HIV-diagnosed mothers
		Temp1 = 0.0;
		for(is=0; is<4; is++){
			Temp1 += (RelativeFert[16+is] * SumGroupsF[ia+4][16+is] +
				RelativeFert[40 + is] * SumGroupsF[ia + 4][40 + is]) * LactatingAdj[ia]; }
		BirthsByHIVstage[ia][7] = HIVnegSEfert[ia] * Temp1 + BirthsByHIVstage[ia][6];
		// Births to HIV-negative mothers
		Temp1 = 0.0;
		for(is=0; is<4; is++){
			Temp1 += RelativeFert[is] * SumGroupsF[ia+4][is];}
		BirthsByHIVstage[ia][1] = HIVnegSEfert[ia] * Temp1;
		// Total births
		Temp1 = 0.0;
		for(is=1; is<7; is++){
			Temp1 += BirthsByHIVstage[ia][is];}
		BirthsByHIVstage[ia][0] = Temp1;
		if (ExcludeInterrupters == 1){
			// Births to mothers currently on ART
			Temp1 = 0.0;
			for (is = 0; is<20; is++){
				Temp1 += RelativeFertART[is] * LactatingAdj[ia] * SumGroupsF[ia + 4][20 + is];}
			BirthsByHIVstageAdj[ia][4] = HIVnegSEfert[ia] * Temp1;
			// Births to HIV-positive mothers not currently on ART
			for (ic = 0; ic < 4; ic++){
				Temp1 = 0.0;
				for (is = 0; is < 2; is++){ // looping across testing histories
					Temp1 += RelativeFert[6 + ic + is * 5] * SumGroupsF[ia + 4][6 + ic + is * 5]; }
				Temp1 += RelativeFert[16 + ic] * LactatingAdj[ia] * SumGroupsF[ia + 4][16 + ic];
				for (is = 0; is<5; is++){ // looping across ART durations
					Temp1 += (RelativeFert[is + ic * 5 + 20] - RelativeFertART[is + ic * 5]) *
						LactatingAdj[ia] * SumGroupsF[ia + 4][20 + ic * 5 + is];
				}
				Temp1 += RelativeFert[40 + ic] * LactatingAdj[ia] * SumGroupsF[ia + 4][40 + ic];
				BirthsByHIVstageAdj[ia][ic] = HIVnegSEfert[ia] * Temp1;
			}
			BirthsByHIVstageAdj[ia][0] += BirthsByHIVstage[ia][8];
		}
	}

	// Get totals
	for(is=0; is<9; is++){
		Temp1 = 0.0;
		if (ExcludeInterrupters == 0 || is < 2 || is>6){
			for (ia = 0; ia < 36; ia++){
				Temp1 += BirthsByHIVstage[ia][is];
			}
		}
		else{
			for (ia = 0; ia < 36; ia++){
				Temp1 += BirthsByHIVstageAdj[ia][is-2];
			}
		}
		TotBirthsByStage[is] = Temp1;
	}

	// Get CD4propn
	Temp1 = 0.0;
	Temp2[0] = TotBirthsByStage[2] * (1.0 - Haemodilution[1]);
	Temp2[1] = TotBirthsByStage[3] * (1.0 - Haemodilution[2]) + (TotBirthsByStage[2] -
		TotBirthsByStage[8] * (Window + DeliveryWk - FirstANCwk) / (CD4duration[0] * 52.0)) *
		Haemodilution[1];
	Temp2[2] = TotBirthsByStage[4] * (1.0 - Haemodilution[3]) + TotBirthsByStage[3] *
		Haemodilution[2];
	Temp2[3] = TotBirthsByStage[5] + TotBirthsByStage[4] * Haemodilution[3];
	for(is=0; is<4; is++){
		Temp1 += Temp2[is];}
	for(is=0; is<4; is++){
		CD4propn[is] = Temp2[is]/Temp1;}
}

void GetBirthsByHIV()
{
	int ic, ia;
	double PosMothers, NegMothers, Temp, FutureAdj;
	double Temp1, Temp2, PropnIncidentHigh;

	// Adjust MatIncidence for PrEP/microbicides
	if (PrEPpregnant[CurrYear - StartYear] > 0.0){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 40; ia++){
			Temp1 += FH_ST.NewHIV[ia] + FH_SW.NewHIV[ia] + FH_LTH.NewHIV[ia] + FH_LTL.NewHIV[ia];
			Temp2 += FL_ST.NewHIV[ia] + FL_LTH.NewHIV[ia] + FL_LTL.NewHIV[ia];
		}
		PropnIncidentHigh = Temp1 / (Temp1 + Temp2);
	}
	else{ PropnIncidentHigh = 0.0; }
	MatIncidence *= (1.0 - PrEPpregnant[CurrYear - StartYear] * PrEPefficacy[1] *
		(PropnIncidentHigh + (1.0 - PropnIncidentHigh) * RR_PrEPlowPreg) -
		VMpregnant[CurrYear - StartYear] * MicrobicideEff);
	if (CurrYear == StartYear && CurrMonth == 0){ MatIncidence = 0.0; }
	if (MatIncidence>0.9999){ MatIncidence = 0.9999; }

	// Calculate NegMothers and PosMothers. We're interested in HIV serostatus at time of
	// first ANC visit, ignoring mortality between first ANC visit and delivery.
	NegMothers = TotBirthsByStage[1] / (1.0 - MatIncidence * (Window + DeliveryWk -
		FirstANCwk) / 52.0);
	Temp = NegMothers * MatIncidence * (Window + DeliveryWk - FirstANCwk) / 52.0;
	PosMothers = (TotBirthsByStage[2] + TotBirthsByStage[3] + TotBirthsByStage[4] +
		TotBirthsByStage[5]) - Temp;
	ARTmothers = TotBirthsByStage[6];

	TransmUntreated = 0.0;
	for(ic=0; ic<4; ic++){
		TransmUntreated += CD4propn[ic] * CD4transm[ic];}

	PTP = PosMothers * VCTuptake * Sensitivity;
	PTN = PosMothers * VCTuptake * (1.0 - Sensitivity);
	PUT = PosMothers * (1.0 - VCTuptake);
	PTNRP = PTN * RescreenLate * RescreenUptake * Sensitivity;
	PTNRN = PTN - PTNRP;
	PUTLP = PUT * RescreenLate * UntestedRescreen * Sensitivity;
	PUTLN = PUT - PUTLP;

	NTNRP = NegMothers * VCTuptake * MatIncidence * ((RescreenWk - FirstANCwk)/52.0) *
		RescreenLate * RescreenUptake * Sensitivity;
	NTNRN = NegMothers * VCTuptake * MatIncidence * ((RescreenWk - FirstANCwk)/52.0) *
		(1.0 - RescreenLate * RescreenUptake * Sensitivity);
	NUTLP = NegMothers * (1.0 - VCTuptake) * MatIncidence * ((RescreenWk - FirstANCwk)/52.0) *
		RescreenLate * UntestedRescreen * Sensitivity;
	NUTLN = NegMothers * (1.0 - VCTuptake) * MatIncidence * ((RescreenWk - FirstANCwk)/52.0) *
		(1.0 - RescreenLate * UntestedRescreen * Sensitivity);
	NRN = NegMothers * (1.0 - MatIncidence * ((RescreenWk - FirstANCwk)/52.0));

	for(ic=0; ic<4; ic++){
		TestedPosCD4[ic] = (PTP + PTNRP + PUTLP) * CD4propn[ic];}
	Temp = EligiblePregPre350[CurrYear-StartYear];
	if(Temp<OptionB){Temp = OptionB;}
	Temp *= MatARTpropn;
	TestedPosOnART[0] = (TestedPosCD4[0] + TestedPosCD4[1]) * Temp;
	TestedPosCD4[0] *= (1.0 - Temp);
	TestedPosCD4[1] *= (1.0 - Temp);
	TestedPosOnART[0] += TestedPosCD4[2] * MatARTpropn * EligiblePreg350[CurrYear-StartYear];
	TestedPosCD4[2] *= (1.0 - MatARTpropn * EligiblePreg350[CurrYear-StartYear]);
	TestedPosOnART[1] = TestedPosCD4[3] * MatARTpropn;
	TestedPosCD4[3] *= (1.0 - MatARTpropn);
	UndiagnosedPos = PUTLN + PTNRN;

	RecentPosDet = NTNRP + NUTLP;
	RecentPosDetART = RecentPosDet * Temp;
	RecentPosDet *= (1.0 - Temp);
	RecentPosUndet = NTNRN + NUTLN + NRN * MatIncidence * (DeliveryWk - RescreenWk + Window)/52.0;

	if (ARTmothers > 0.0){
		RednHAART = ((TestedPosOnART[0] + TestedPosOnART[1] + RecentPosDetART) * RednHAARTduringPreg +
			ARTmothers * RednHAARTbeforePreg) / (TestedPosOnART[0] + TestedPosOnART[1] + RecentPosDetART +
			ARTmothers);
	}
	else{ RednHAART = RednHAARTduringPreg; }

	FutureAdj = 1.0;
	if (VaryFutureInterventions == 1 && CurrYear > 2011){
		FutureAdj = FutureInterventions.out[CurrSim - 1][20];
		if (CurrYear == 2012){ FutureAdj = 0.5 * (1.0 + FutureInterventions.out[CurrSim - 1][20]); }
	}

	ChildPosNoPMTCT = RecentPosDet * TransmAcute;
	for(ic=0; ic<4; ic++){
		ChildPosNoPMTCT += TestedPosCD4[ic] * CD4transm[ic];}
	ChildPosPMTCT = ChildPosNoPMTCT;
	ChildNegMotherPosKnown = -ChildPosNoPMTCT;
	ChildPosNoPMTCT *= (1.0 - NVPuptake * FutureAdj) * (1.0 - AZTpropn * AZTpropnAdj * FutureAdj);
	Temp = ChildPosNoPMTCT;
	ChildPosNoPMTCT += UndiagnosedPos * TransmUntreated + RecentPosUndet * TransmAcute;
	ChildPosPMTCT *= (NVPuptake * FutureAdj * (1.0 - NVPefficacy * (1.0 - AZTpropn) - DualEfficacy * AZTpropn) +
		(1.0 - NVPuptake * FutureAdj) * AZTpropn * AZTpropnAdj * FutureAdj * (1.0 - AZTefficacy));
	ChildPosPMTCT += TestedPosOnART[0] * TransmARTpre200 + TestedPosOnART[1] * TransmART200 +
		ARTmothers * TransmARTprePreg + RecentPosDetART * TransmARTpre200;
	ChildNegMotherPosKnown *= (1.0 - NVPuptake * FutureAdj * (NVPefficacy * (1.0 - AZTpropn) + DualEfficacy *
		AZTpropn) - (1.0 - NVPuptake * FutureAdj) * AZTpropn * AZTpropnAdj * FutureAdj * AZTefficacy);
	ChildNegMotherPosKnown += TestedPosCD4[0] + TestedPosCD4[1] + TestedPosCD4[2] + TestedPosCD4[3] +
		RecentPosDet + TestedPosOnART[0] * (1.0 - TransmARTpre200) + TestedPosOnART[1] * (1.0 -
		TransmART200) + ARTmothers * (1.0 - TransmARTprePreg) + RecentPosDetART * (1.0 - TransmARTpre200);
	ChildNegMotherPosUnknown = UndiagnosedPos * (1.0 - TransmUntreated) + RecentPosUndet * (1.0 -
		TransmAcute);
	ChildNegMotherNeg = NRN * (1.0 - MatIncidence * (DeliveryWk - RescreenWk + Window)/52.0);

	if (FixedUncertainty == 1){
		VertTransmKnownPos.out[CurrSim - 1][CurrYear - StartYear] += (Temp + ChildPosPMTCT)/12.0;
		BirthsDiagMothers += (ARTmothers + PTP + PTNRP + PUTLP + RecentPosDet)/12.0;
	}
    // Calculate New Diagnoses amongst pregnant women
    if (FixedUncertainty == 1){
           NewDiagnosesPregnancy.out[CurrSim - 1][CurrYear - StartYear] += ((PTP + PTNRP + PUTLP) *
                  (1.0 - (TotBirthsByStage[7] - ARTmothers) / PosMothers) + NTNRP + NUTLP) / 12.0;
    }
    // Calculate Rediagnoses amongst pregnant women
    if (FixedUncertainty == 1){
        RediagnosesPregnancy.out[CurrSim - 1][CurrYear - StartYear] += ((PTP + PTNRP + PUTLP) * (TotBirthsByStage[7] - ARTmothers) / PosMothers + NTNRP + NUTLP) / 12.0;
    }
    // Total tests on pregnant women at ANC
    if (FixedUncertainty == 1){
        TotANCtests.out[CurrSim - 1][CurrYear - StartYear] += (NegMothers * (VCTuptake * (1.0 + RescreenLate) + (1.0 - VCTuptake) * RescreenLate * UntestedRescreen) + PosMothers * (VCTuptake * Sensitivity + (1.0 - VCTuptake) * RescreenLate * UntestedRescreen + VCTuptake * (1 - Sensitivity) * RescreenLate)) / 12.0;
    }
}

void GetPrevPregnant()
{
	int ia, ii, iy;
	double Temp1, Temp2, TimeToBirth;
	double MatIncidenceAS[6]; // age-specific HIV incidence in pregnant women
	double PrevAtBirth[6], BirthsByAge[6];

	iy = CurrYear - StartYear;
	TimeToBirth = (DeliveryWk - FirstANCwk) / 52.0;

	// Age-specific prevalence
	for(ia=0; ia<5; ia++){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ii=0; ii<5; ii++){
			Temp1 += BirthsByHIVstage[ia*5+ii+1][1];
			Temp2 += BirthsByHIVstage[ia*5+ii+1][0];
		}
		PrevAtBirth[ia] = 1.0 - (Temp1 / Temp2);
		BirthsByAge[ia] = Temp2;
	}

	// Prevalence at ages 40-49
	Temp1 = 0.0;
	Temp2 = 0.0;
	for(ii=26; ii<36; ii++){
		Temp1 += BirthsByHIVstage[ii][1];
		Temp2 += BirthsByHIVstage[ii][0];
	}
	PrevAtBirth[5] = 1.0 - (Temp1 / Temp2);
	BirthsByAge[5] = Temp2;

	// Age-specific incidence
	for (ia = 0; ia<5; ia++){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ii = 0; ii<5; ii++){
			Temp1 += NewHIVbyAgeSex[ii+ia*5+5][1] * 12.0 / (CurrMonth + 1.0);
			Temp2 += TotalSexuallyExp_S[ii + ia * 5 + 15][1] * (1.0 -
				TotalPositive_S[ii + ia * 5 + 15][1] / TotalPop_S[ii + ia * 5 + 15][1]);
		}
		MatIncidenceAS[ia] = Temp1 / Temp2;
	}
	Temp1 = 0.0;
	Temp2 = 0.0;
	for (ii = 40; ii<50; ii++){
		Temp1 += NewHIVbyAgeSex[ii-10][1] * 12.0 / (CurrMonth + 1.0);
		Temp2 += TotalSexuallyExp_S[ii][1] * (1.0 -
			TotalPositive_S[ii][1] / TotalPop_S[ii][1]);
	}
	MatIncidenceAS[5] = Temp1 / Temp2;

	// Back-calculate HIV prevalence in pregnant women
	for (ia = 0; ia < 6; ia++){
		PrevPregnant[ia][iy] = (1.0 - Miscarriage[0]) * (PrevAtBirth[ia] - MatIncidenceAS[ia] * TimeToBirth) /
			(1.0 - Miscarriage[1] - PrevAtBirth[ia] * (Miscarriage[0] - Miscarriage[1]) - (1.0 - Miscarriage[0]) *
			MatIncidenceAS[ia] * TimeToBirth);
	}

	// Prevalence in all pregnant women
	if (ProvModel == 0 || (ProvModel==1 && InclAS_ANCprov==1)){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 0; ia < 6; ia++){
			Temp1 += BirthsByAge[ia] * PrevPregnant[ia][iy];
			Temp2 += BirthsByAge[ia];
		}
		PrevPregnant[6][iy] = Temp1/Temp2;
	}
	else{
		PrevPregnant[6][iy] = 0.0;
		for (ia = 0; ia < 6; ia++){
			if (CurrYear < 2013){ ANCageWeights[ia] = ANCageW_init[ia]; }
			else{ ANCageWeights[ia] = ANCageW_init[ia] + ANCageW_change[ia] * (CurrYear - 2012); }
			PrevPregnant[6][iy] += ANCageWeights[ia] * PrevPregnant[ia][iy];
		}
	}
}

void SetMnthlyPaedParameters()
{
	// Note that this function should only get called after SetPaedARTinitiation and after
	// GetBirthsByHIV (where MatIncidence is calculated).

	int ia;

	for(ia=0; ia<36; ia++){
		NegMatExit[ia][0] = 1.0 - pow(1.0 - MatIncidence, 1.0/12.0) * (1.0 - RateOfBFchange[ia][0]);
		NegMatExit[ia][1] = log(1.0 - RateOfBFchange[ia][0])/
			log(pow(1.0 - MatIncidence, 1.0/12.0) * (1.0 - RateOfBFchange[ia][0]));
		NegMatExit[ia][2] = log(1.0 - MatIncidence) * (1.0/12.0)/
			log(pow(1.0 - MatIncidence, 1.0/12.0) * (1.0 - RateOfBFchange[ia][0]));
		if(ia==35){
			NegMatExit[ia][0] = 1.0;
			NegMatExit[ia][1] = 1.0;
			NegMatExit[ia][2] = 0.0;
		}
	}
}

void UpdatePrEPandVM()
{
	int ia, ig, ii, iy;
	double PrEPrates[81][7], VMrates[81][2], RegHCTrates[81][3];
	double Temp, Temp2, married, MarriedAdj, PartnerHIV;

	// Calculate RR of PrEP initiation, required for estimating PrEP initiation rates
	iy = CurrYear - StartYear;
	if (TotStartingPrEP[iy] > 0 && CurrMonth == 0){
		for (ia = 0; ia < 81; ia++){
			for (ii = 0; ii < 2; ii++){
				// MSM calculations
				RR_PrEP_MSM[ia][ii] = PartnerAcqM[ia];
				if (ii == 1){ RR_PrEP_MSM[ia][ii] *= RRpartnerLow[0]; }
				PartnerHIV = 1.0 - (MHU_STM.NegNoHCT[ia] + MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] +
					MHU_STM.RegPrEP[ia] + MHC_STM.NegNoHCT[ia] + MHC_STM.NegPastHCT[ia] + MHC_STM.RegHCT[ia] +
					MHC_STM.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] + MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] +
					MLU_STM.RegPrEP[ia] + MLC_STM.NegNoHCT[ia] + MLC_STM.NegPastHCT[ia] + MLC_STM.RegHCT[ia] +
					MLC_STM.RegPrEP[ia]) / (MHU_STM.Total[ia] + MHC_STM.Total[ia] + MLU_STM.Total[ia] +
					MLC_STM.Total[ia]);
				RR_PrEP_MSM[ia][ii] *= PartnerHIV * PrEPeligMSM[iy];
				// Heterosexual calculations
				RR_PrEP_Het[ia][ii][0] = PartnerAcqM[ia];
				RR_PrEP_Het[ia][ii][1] = PartnerRate20F * PartnerAcqF[ia];
				for (ig = 0; ig < 2; ig++){
					married = TotalMarried[ia + 10][ig] / TotalSexuallyExp[ia + 10][ig];
					if (ii == 0){ MarriedAdj = RRpartnerMarried[ig]; }
					else{ MarriedAdj = 0.0; }
					if (ii == 1){ RR_PrEP_Het[ia][ii][ig] *= RRpartnerLow[ig]; }
					RR_PrEP_Het[ia][ii][ig] *= (married * MarriedAdj + 1.0 - married); // ST partners
					RR_PrEP_Het[ia][ii][ig] += married; // LT partners
					if (ig == 0 && ia > 3){ PartnerHIV = TotalPositive[ia + 7][1] / TotalPop[ia + 7][1]; }
					else if (ig == 1 && ia<78){ PartnerHIV = TotalPositive[ia + 13][0] / TotalPop[ia + 13][0]; }
					else{ PartnerHIV = 0.0; }
					RR_PrEP_Het[ia][ii][ig] *= PartnerHIV;
					if (ig == 1 && ia < 15){ RR_PrEP_Het[ia][ii][ig] *= PrEPeligAGYW[iy]; }
					else{ RR_PrEP_Het[ia][ii][ig] *= PrEPeligOther[iy]; }
				}
			}
		}
		// Standardize to age 20, high risk
		Temp = RR_PrEP_MSM[10][0] / PrEPeligMSM[iy];
		Temp2 = RR_PrEP_Het[10][0][1] / PrEPeligAGYW[iy];
		for (ia = 0; ia < 81; ia++){
			for (ii = 0; ii < 2; ii++){
				if (Temp > 0.0){ RR_PrEP_MSM[ia][ii] *= 1.0 / Temp; }
				for (ig = 0; ig < 2; ig++){
					if (Temp2 > 0.0){ RR_PrEP_Het[ia][ii][ig] *= 1.0 / Temp2; }
				}
			}
		}
	}
	else if (CurrYear > PrEPdataYr + 5){ CurrPrEPrateFSW = UltPrEPrateFSW;}
	else if (CurrYear > PrEPdataYr){ CurrPrEPrateFSW = StoredPrEPrateFSW + (UltPrEPrateFSW - StoredPrEPrateFSW) *
		(CurrYear - PrEPdataYr) / 5.0; }
	else if(CurrMonth == 0){ CurrPrEPrateFSW = 0.0; }
	if (DiscontinuePrEP == 1 && (CurrYear > PrEPdataYr + 5)){
		CurrPrEPrateFSW = 0.0;}
	if ((TotStartingPrEP[iy] > 0 || (FixedUncertainty == 1 && CurrYear > PrEPdataYr)) && CurrMonth == 0){
		GetPrEPrateFSW();
	}

	// PrEP initiation rates
	/*for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia < 5; ia++){
			PrEPrates[ia][1+ig*3] = 0.0;
		}
		Temp = PrEP_15[CurrYear - StartYear][ig] / 12.0;
		for (ia = 5; ia < 10; ia++){
			PrEPrates[ia][1+ig*3] = Temp;
		}
		Temp = PrEP_20[CurrYear - StartYear][ig] / 12.0;
		for (ia = 10; ia < 15; ia++){
			PrEPrates[ia][1 + ig * 3] = Temp;
		}
		Temp = PrEP_25[CurrYear - StartYear][ig] / 12.0;
		for (ia = 15; ia < 40; ia++){
			PrEPrates[ia][1 + ig * 3] = Temp;
		}
		Temp = PrEP_50[CurrYear - StartYear][ig] / 12.0;
		for (ia = 40; ia < 81; ia++){
			PrEPrates[ia][1 + ig * 3] = Temp;
		}
		for (ia = 0; ia < 81; ia++){
			PrEPrates[ia][2 + ig * 3] = PrEPrates[ia][1 + ig * 3] * RR_PrEPlow[ig];
		}
	}
	Temp = PrEPpregnant[CurrYear - StartYear] / 12.0;
	for (ia = 5; ia < 40; ia++){
		PrEPrates[ia][4] = PrEPrates[ia][4] - log(1.0 - HIVnegSEfert[ia - 4]) * Temp;
		PrEPrates[ia][5] = PrEPrates[ia][5] - log(1.0 - HIVnegSEfert[ia - 4]) * Temp * RR_PrEPlowPreg;
	}
	Temp = PrEP_FSW[CurrYear - StartYear] / 12.0;
	for (ia = 0; ia<81; ia++){
		PrEPrates[ia][3] = Temp;
		if (PrEPrates[ia][3] < PrEPrates[ia][4]){
			PrEPrates[ia][3] = PrEPrates[ia][4];
		}
	}
	Temp = PrEP_MSM[CurrYear - StartYear] / 12.0;
	for (ia = 0; ia<81; ia++){
		PrEPrates[ia][0] = Temp;
		if (PrEPrates[ia][0] < PrEPrates[ia][1]){
			PrEPrates[ia][0] = PrEPrates[ia][1];
		}
	}*/
	for (ia = 0; ia < 81; ia++){
		for (ii = 0; ii < 2; ii++){
			PrEPrates[ia][ii] = CurrPrEPrateFSW * RR_PrEPstartMSM[iy] * RR_PrEP_MSM[ia][ii];
			PrEPrates[ia][ii + 2] = CurrPrEPrateFSW * RR_PrEPstartF20[iy] * RR_PrEP_Het[ia][ii][0];
			PrEPrates[ia][4] = CurrPrEPrateFSW;
			PrEPrates[ia][ii + 5] = CurrPrEPrateFSW * RR_PrEPstartF20[iy] * RR_PrEP_Het[ia][ii][1];
		}
	}
	// Pregnancy adjustment in women
	Temp = PrEPpregnant[iy] / 12.0;
	for (ia = 5; ia < 40; ia++){
		PrEPrates[ia][5] = PrEPrates[ia][5] - log(1.0 - HIVnegSEfert[ia - 4]) * Temp;
		PrEPrates[ia][6] = PrEPrates[ia][6] - log(1.0 - HIVnegSEfert[ia - 4]) * Temp * RR_PrEPlowPreg;
	}

	// VM initiation rates
	for (ia = 0; ia<5; ia++){
		VMrates[ia][1] = 0.0;
	}
	Temp = VMpregnant[CurrYear - StartYear] / 12.0;
	for (ia = 5; ia<40; ia++){
		VMrates[ia][1] = - Temp * log(1.0 - HIVnegSEfert[ia - 4]);
	}
	Temp = VM_15[CurrYear - StartYear] / 12.0;
	for (ia = 5; ia<10; ia++){
		VMrates[ia][1] += Temp;
	}
	Temp = VM_20[CurrYear - StartYear] / 12.0;
	for (ia = 10; ia<15; ia++){
		VMrates[ia][1] += Temp;
	}
	Temp = VM_25[CurrYear - StartYear] / 12.0;
	for (ia = 15; ia<40; ia++){
		VMrates[ia][1] += Temp;
	}
	Temp = VM_50[CurrYear - StartYear] / 12.0;
	for (ia = 40; ia<81; ia++){
		VMrates[ia][1] = Temp;
	}
	Temp = VM_FSW[CurrYear - StartYear] / 12.0;
	for (ia = 0; ia<81; ia++){
		VMrates[ia][0] = Temp;
		if (VMrates[ia][0] < VMrates[ia][1]){
			VMrates[ia][0] = VMrates[ia][1];
		}
	}

	// Rates of joining regular HCT programmes
	for (ia = 0; ia<5; ia++){
		RegHCTrates[ia][0] = 0.0;
	}
	Temp = RegHCT_15[CurrYear - StartYear] / 12.0;
	for (ia = 5; ia<10; ia++){
		RegHCTrates[ia][0] = Temp;
	}
	Temp = RegHCT_20[CurrYear - StartYear] / 12.0;
	for (ia = 10; ia<15; ia++){
		RegHCTrates[ia][0] = Temp;
	}
	Temp = RegHCT_25[CurrYear - StartYear] / 12.0;
	for (ia = 15; ia<40; ia++){
		RegHCTrates[ia][0] = Temp;
	}
	Temp = RegHCT_50[CurrYear - StartYear] / 12.0;
	for (ia = 40; ia<81; ia++){
		RegHCTrates[ia][0] = Temp;
	}
	Temp = RegHCTpregnant[CurrYear - StartYear] / 12.0;
	for (ia = 0; ia<81; ia++){
		RegHCTrates[ia][2] = RegHCTrates[ia][0];
	}
	for (ia = 5; ia<40; ia++){
		RegHCTrates[ia][2] = RegHCTrates[ia][2] - Temp * log(1.0 - HIVnegSEfert[ia - 4]);
	}
	Temp = RegHCT_FSW[CurrYear - StartYear] / 12.0;
	for (ia = 0; ia<81; ia++){
		RegHCTrates[ia][1] = Temp;
		if (RegHCTrates[ia][1] < RegHCTrates[ia][2]){
			RegHCTrates[ia][1] = RegHCTrates[ia][2];
		}
	}

	// Calculate dependent probabilities
	for (ia = 0; ia<81; ia++){
		// MSM and male probabilities
		Temp = RegHCTrates[ia][0];
		if (Temp == 0.0){
			for (ii = 0; ii < 4; ii++){
				JoinPrEP[ia][ii] = 1.0 - exp(-PrEPrates[ia][ii]);
			}
			JoinRegHCT[ia][0] = 0.0;
		}
		else{
			for (ii = 0; ii < 4; ii++){
				JoinPrEP[ia][ii] = (1.0 - exp(-PrEPrates[ia][ii])) * exp(-Temp);
			}
			JoinRegHCT[ia][0] = (1.0 - exp(-Temp));
		}
		// Female probabilities (excluding sex workers)
		Temp = VMrates[ia][1] + RegHCTrates[ia][2];
		if (Temp == 0.0){
			JoinPrEP[ia][5] = 1.0 - exp(-PrEPrates[ia][5]);
			JoinPrEP[ia][6] = 1.0 - exp(-PrEPrates[ia][6]);
			JoinVM[ia][2] = 0.0;
			JoinRegHCT[ia][2] = 0.0;
		}
		else{
			JoinPrEP[ia][5] = (1.0 - exp(-PrEPrates[ia][5])) * exp(-Temp);
			JoinPrEP[ia][6] = (1.0 - exp(-PrEPrates[ia][6])) * exp(-Temp);
			JoinVM[ia][2] = (1.0 - exp(-Temp)) * VMrates[ia][1] / Temp;
			JoinRegHCT[ia][2] = (1.0 - exp(-Temp)) * RegHCTrates[ia][2] / Temp;
		}
		// Sex worker probabilities
		Temp = VMrates[ia][0] + RegHCTrates[ia][1];
		if (Temp == 0.0){
			JoinPrEP[ia][4] = 1.0 - exp(-PrEPrates[ia][4]);
			JoinVM[ia][1] = 0.0;
			JoinRegHCT[ia][1] = 0.0;
		}
		else{
			JoinPrEP[ia][4] = (1.0 - exp(-PrEPrates[ia][4])) * exp(-Temp);
			JoinVM[ia][1] = (1.0 - exp(-Temp)) * VMrates[ia][0] / Temp;
			JoinRegHCT[ia][1] = (1.0 - exp(-Temp)) * RegHCTrates[ia][1] / Temp;
		}
	}
}

void UpdateTestingRates()
{
	// In version 4.4 of Thembisa, I've removed the HIVeffectVCT (which was only used in the
	// first version of Thembisa) and added in self-testing and index testing. I've also allowed
	// for immediate linkage to ART in people who are re-diagnosed (not previously included).

	int ia, is, ig, ib, offset;
	double BaseHCT, Temp, TempF, TempF2, Temp1, bParamM, bParamF, aParamM, aParamF, VCTmale;
	double SymptomTesting[5]; // Formulas in row 89
	double CurrEligPreg[4], CurrEligOI[4], CurrEligOther[4][2], HBCTrate, PaedElig;
	double IndexTesting[2][2]; // Rate of index testing by HIV status and sex

	offset = CurrYear-StartYear;
	BaseHCT = HCT1stTimeF25[offset];
	HBCTrate = HBCTuptake[offset] / 12.0;
	bParamM = pow(QuadParam[0], 2.0) / VCTage[0];
	bParamF = pow(QuadParam[1], 2.0) / VCTage[1];
	aParamM = VCTage[0] / bParamM;
	aParamF = VCTage[1] / bParamF;
	if(CurrYear<=2002){VCTmale = VCTmale2002;}
	else{
		if(CurrYear>=2010){VCTmale = VCTmale2010;}
		else{VCTmale = VCTmale2002 + (VCTmale2010 - VCTmale2002) * (CurrYear - 2002.0)/8.0;}
	}
	if (VaryFutureInterventions == 1 && FixedUncertainty==1 && CurrYear>=2012){
		if (CurrYear >= 2016){ VCTmale = FutureInterventions.out[CurrSim - 1][1]; }
		else{
			VCTmale = VCTmale2010 + (0.2 * (CurrYear - 2011)) *
				(FutureInterventions.out[CurrSim - 1][1] - VCTmale2010);
		}
	}

	// Calculate proportions eligible to start ART immediately after HIV diagnosis
	for(is=0; is<3; is++){
		if(is<2){
			Temp1 = EligiblePTBpre350[offset];}
		else{
			Temp1 = EligiblePTB350[offset];}
		CurrEligOI[is] = (PTBincidence[is + 1] * Temp1 +
			(OIincidence[is + 1] - PTBincidence[is + 1] - WHO4incidence[is + 1]) *
			EligibleWHO3[offset] + WHO4incidence[is + 1]) / OIincidence[is + 1];
	}
	CurrEligOI[3] = 1.0;
	CurrEligPreg[0] = EligiblePregPre350[offset];
	CurrEligPreg[1] = EligiblePregPre350[offset];
	CurrEligPreg[2] = EligiblePreg350[offset];
	CurrEligPreg[3] = 1.0;
	for (ig = 0; ig < 2; ig++){
		CurrEligOther[0][ig] = EligibleAsymPre500[offset];
		CurrEligOther[1][ig] = EligibleAsym500[offset];
		CurrEligOther[2][ig] = EligibleAsym350[offset];
		CurrEligOther[3][ig] = 1.0;
	}

	// Calculate testing rates
	for(is=0; is<5; is++){
		SymptomTesting[is] = OIincidence[is] * OIsTested[offset] * OItoTBtestingRatio/12.0;}
	for(ia=0; ia<81; ia++){
		Temp = BaseHCT * pow((ia + 10.0)/25.0, aParamM - 1.0) * exp((15.0 - ia)/bParamM) / 12.0;
		TempF = BaseHCT * pow((ia + 10.0) / 25.0, aParamF - 1.0) * exp((15.0 - ia) / bParamF) / 12.0;
		TestingRateM[ia] = Temp * VCTmale;
		TestingRateNPF[ia] = TempF;
		if(ia>=5 && ia<40){
			TempF2 = HIVnegSEfert[ia-4] * VCTuptake/12.0;}
		else{
			TempF2 = 0.0;}
		TestingRateSE[ia][0][0] = Temp * VCTmale;
		TestingRateSE[ia][1][0] = Temp * VCTmale * RetestAdj;
		TestingRateSE[ia][0][1] = TempF + TempF2;
		TestingRateSE[ia][1][1] = TempF * RetestAdj + TempF2;
		// Calculate index testing (new in version 4.5)
		ib = ia - 3;
		if (ib < 0){ ib = 0; }
		IndexTesting[0][0] = PosPartnerProb[ia][0][0] * AvePosTesting[ib][1] * DiscloseAndTest;
		IndexTesting[1][0] = PosPartnerProb[ia][1][0] * AvePosTesting[ib][1] * DiscloseAndTest;
		ib = ia + 3;
		if (ib > 80){ ib = 80; }
		IndexTesting[0][1] = PosPartnerProb[ia][0][1] * AvePosTesting[ib][0] * DiscloseAndTest;
		IndexTesting[1][1] = PosPartnerProb[ia][1][1] * AvePosTesting[ib][0] * DiscloseAndTest;
        if (CurrYear == StartYear){
               for (ig = 0; ig < 2; ig++){
                      IndexTesting[0][ig] = 0.0;
                      IndexTesting[1][ig] = 0.0;
               }
        }
		for(is=0; is<5; is++){
			// Rates of testing in positives (including index testing)
			TestingRateSE[ia][is + 2][0] = TestingRateSE[ia][0][0] + IndexTesting[1][0] +
				SymptomTesting[is];
			TestingRateSE[ia][is + 7][0] = TestingRateSE[ia][1][0] + IndexTesting[1][0] +
				SymptomTesting[is];
			TestingRateSE[ia][is + 2][1] = TempF + TempF2 * RelativeFert[5 + is] + IndexTesting[1][1] +
				SymptomTesting[is];
			TestingRateSE[ia][is + 7][1] = TempF * RetestAdj + TempF2 * RelativeFert[10 + is] +
				IndexTesting[1][1] + SymptomTesting[is];
			if (is > 0){ // Add confirmatory testing after self-testing positive
				TestingRateSE[ia][is + 2][0] += SelfTestingRate[ia][is + 2][0] * SelfTestConfirm;
				TestingRateSE[ia][is + 7][0] += SelfTestingRate[ia][is + 7][0] * SelfTestConfirm;
				TestingRateSE[ia][is + 2][1] += SelfTestingRate[ia][is + 2][1] * SelfTestConfirm;
				TestingRateSE[ia][is + 7][1] += SelfTestingRate[ia][is + 7][1] * SelfTestConfirm;
			}

			// Rates of testing, multiplied by ART-eligible fraction
			if (is>0 && ia<5){
				if (is == 4){ PaedElig = 1.0; }
				else{ PaedElig = EarlyART5to14[CurrYear - StartYear]; }
				TestingRateEligSE[ia][is][0] = (TestingRateSE[ia][0][0] + IndexTesting[1][0] + SelfTestingRate[ia][is + 2][0] *
					SelfTestConfirm) * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 5][0] = (TestingRateSE[ia][1][0] + IndexTesting[1][0] + SelfTestingRate[ia][is + 7][0] *
					SelfTestConfirm) * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 10][0] = (TestingRateSE[ia][1][0] + SelfTestingRate[ia][is + 7][0] *
					SelfTestConfirm) * RetestPos * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is][1] = (TempF + IndexTesting[1][1] + SelfTestingRate[ia][is + 2][1] *
					SelfTestConfirm) * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 5][1] = (TempF  * RetestAdj + IndexTesting[1][1] + SelfTestingRate[ia][is + 7][1] *
					SelfTestConfirm) * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 10][1] = (TempF + SelfTestingRate[ia][is + 7][1] *
					SelfTestConfirm) * RetestPos * PaedElig + SymptomTesting[is] * CurrEligOI[is - 1];
			}
			if(is>0 && ia>=5){
				TestingRateEligSE[ia][is][0] = (TestingRateSE[ia][0][0] + IndexTesting[1][0] + SelfTestingRate[ia][is + 2][0] *
					SelfTestConfirm) * CurrEligOther[is - 1][0] + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 5][0] = (TestingRateSE[ia][1][0] + IndexTesting[1][0] + SelfTestingRate[ia][is + 7][0] *
					SelfTestConfirm) * CurrEligOther[is - 1][0] + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 10][0] = (TestingRateSE[ia][1][0] + SelfTestingRate[ia][is + 7][0] *
					SelfTestConfirm) * RetestPos * CurrEligOther[is - 1][0] + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is][1] = (TempF + IndexTesting[1][1] + SelfTestingRate[ia][is + 2][1] *
					SelfTestConfirm) * CurrEligOther[is - 1][1] + TempF2 * RelativeFert[5 + is] *
					CurrEligPreg[is - 1] + SymptomTesting[is] * CurrEligOI[is - 1];
				TestingRateEligSE[ia][is + 5][1] = (TempF * RetestAdj + IndexTesting[1][1] + SelfTestingRate[ia][is + 7][1] *
					SelfTestConfirm) * CurrEligOther[is - 1][1] + TempF2 * RelativeFert[10 + is] *
					CurrEligPreg[is - 1] + SymptomTesting[is] * CurrEligOI[is - 1];
				//TestingRateEligSE[ia][is + 10][1] = TempF2 * RelativeFert[15 + is] * CurrEligPreg[is - 1];
				TestingRateEligSE[ia][is + 10][1] = (TempF + SelfTestingRate[ia][is + 7][1] * SelfTestConfirm) *
					RetestPos * CurrEligOther[is - 1][1] + TempF2 * RelativeFert[15 + is] *
					CurrEligPreg[is - 1] + SymptomTesting[is] * CurrEligOI[is - 1];
			}
		}
		// Now account for index testing in HIV-negative individuals
		TestingRateSE[ia][0][0] += IndexTesting[0][0];
		TestingRateSE[ia][1][0] += IndexTesting[0][0];
		TestingRateSE[ia][0][1] += IndexTesting[0][1];
		TestingRateSE[ia][1][1] += IndexTesting[0][1];
	}
	for(ia=0; ia<20; ia++){
		TestingRateNegV[ia][0] = TestingRateNPF[5] * RRtestVirgin;
		TestingRateNegV[ia][1] = TestingRateNPF[5] * RRtestVirgin;
		for(is=0; is<5; is++){
			TestingRateV[ia][is][0] = SymptomTesting[is] + TestingRateNegV[ia][0];
			TestingRateV[ia][is][1] = SymptomTesting[is] + TestingRateNegV[ia][1];
			if (is>0 && ia < 5){
				if (is == 4){ PaedElig = 1.0; }
				else{ PaedElig = EarlyART5to14[CurrYear - StartYear]; }
				TestingRateEligV[ia][is][0] = SymptomTesting[is] * CurrEligOI[is - 1] +
					TestingRateNegV[ia][0] * PaedElig;
				TestingRateEligV[ia][is][1] = SymptomTesting[is] * CurrEligOI[is - 1] +
					TestingRateNegV[ia][1] * PaedElig;
			}
			if(is>0 && ia>=5){
				TestingRateEligV[ia][is][0] = SymptomTesting[is] * CurrEligOI[is - 1] +
					TestingRateNegV[ia][0] * CurrEligOther[is - 1][0];
				TestingRateEligV[ia][is][1] = SymptomTesting[is] * CurrEligOI[is - 1] +
					TestingRateNegV[ia][1] * CurrEligOther[is - 1][1];
			}
		}
	}

	// Modify formulas to reflect HBCT
	if (HBCTrate > 0.0){
		if (CurrYear < 2015){ std::cout << "Problem in year " << CurrYear << std::endl; }
		for (ia = 0; ia < 81; ia++){
			for (is = 0; is < 12; is++){
				TestingRateSE[ia][is][0] += HBCTrate;
				TestingRateSE[ia][is][1] += HBCTrate;
			}
			for (is = 1; is < 5; is++){
				// Note that we assume no ART initiation through HBCT if
				// previously diagnosed (i.e. for is<10).
				if (ia>=5){
					TestingRateEligSE[ia][is][0] += HBCTrate * CurrEligOther[is - 1][0];
					TestingRateEligSE[ia][is][1] += HBCTrate * CurrEligOther[is - 1][1];
					TestingRateEligSE[ia][is + 5][0] += HBCTrate * CurrEligOther[is - 1][0];
					TestingRateEligSE[ia][is + 5][1] += HBCTrate * CurrEligOther[is - 1][1];
				}
			}
		}
		for (ia = 0; ia < 20; ia++){
			for (is = 0; is < 5; is++){
				TestingRateV[ia][is][0] += HBCTrate;
				TestingRateV[ia][is][1] += HBCTrate;
				if (is>0 && ia >= 5){
					TestingRateEligV[ia][is][0] += HBCTrate * CurrEligOther[is - 1][0];
					TestingRateEligV[ia][is][1] += HBCTrate * CurrEligOther[is - 1][1];
				}
			}
		}
	}

	// Calculate testing rates in children
	for (ia = 0; ia < 132; ia++){
		// MnthlyTestingPaed
		if (ia <18){ MnthlyTestingPaed[ia][0] = 0.0; }
		else if (ia == 18){ MnthlyTestingPaed[ia][0] = CurrTesting18mo[1]; }
		else if (ia<60){ MnthlyTestingPaed[ia][0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestAgePaed); }
		else{ MnthlyTestingPaed[ia][0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin); }
		if (ia < 60){ MnthlyTestingPaed[ia][1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced * RRtestAgePaed);}
		else{ MnthlyTestingPaed[ia][1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced); }
		if (ia == 18){ MnthlyTestingPaed[ia][1] = CurrTesting18mo[1]; }
		if (ia == 1){
			MnthlyTestingPaed[ia][0] = PCRuptake;
			MnthlyTestingPaed[ia][1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced * RRtestAgePaed) * (1.0 - PCRuptake);
		}
		// MnthlyPaedTestToART
		for (is = 0; is < 3; is++){ // early disease
			if (ia < 18){ MnthlyPaedTestToART[ia][is] = 0.0; }
			else if (ia<60){ MnthlyPaedTestToART[ia][is] = MnthlyTestingPaed[ia][0] * PaedARTuptakeC * RRearlyPaedART * EarlyART1to4[offset]; }
			else{ MnthlyPaedTestToART[ia][is] = MnthlyTestingPaed[ia][0] * PaedARTuptakeC * RRearlyPaedART * EarlyART5to14[offset]; }
		}
		MnthlyPaedTestToART[ia][3] = MnthlyTestingPaed[ia][1] * PaedARTuptakeC;
		if (ia < 2){ MnthlyPaedTestToART[ia][3] *= SixWeekSe[2]; }
		if (ia == 1){
			MnthlyPaedTestToART[ia][0] = PCRuptake * SixWeekSe[0] * EligInfants * PaedARTuptakeEID;
			MnthlyPaedTestToART[ia][1] = PCRuptake * SixWeekSe[1] * EligInfants * PaedARTuptakeEID;
			MnthlyPaedTestToART[ia][3] = 1.0 - (1.0 - MnthlyPaedTestToART[ia][3]) *
				(1.0 - PCRuptake * SixWeekSe[2] * PaedARTuptakeEID);
		}
		// MonthlyPaedDiagNoART
		for (is = 0; is < 3; is++){
			MnthlyPaedDiagNoART[ia][is] = MnthlyTestingPaed[ia][0] - MnthlyPaedTestToART[ia][is];
		}
		MnthlyPaedDiagNoART[ia][3] = MnthlyTestingPaed[ia][1] - MnthlyPaedTestToART[ia][3];
		// Note that we need a different approach for infants because sensitivity is <100%, so testing is not diagnosis.
		if (ia == 0){
			MnthlyPaedDiagNoART[ia][3] = MnthlyTestingPaed[ia][1] * SixWeekSe[2] * (1.0 - PaedARTuptakeC);
		}
		if (ia == 1){
			MnthlyPaedDiagNoART[ia][0] = PCRuptake * SixWeekSe[0] * (1.0 - EligInfants * PaedARTuptakeEID);
			MnthlyPaedDiagNoART[ia][1] = PCRuptake * SixWeekSe[1] * (1.0 - EligInfants * PaedARTuptakeEID);
			MnthlyPaedDiagNoART[ia][3] = MnthlyTestingPaed[ia][1] * SixWeekSe[2] - MnthlyPaedTestToART[ia][3];
		}
	}
}

void UpdateTestingToART()
{
	// This function updates rates of immediate ART iniation in newly diagnosed
	// (cols B-AE in "ART start" sheet).

	int ia, is, ig, ib, offset;
	double AdultLinkage[4][2], PaedLinkage;

	offset = CurrYear - StartYear;

	// Calculate proportions starting ART immediately if eligible
	for (is = 0; is < 4; is++){
		if (PropnalImmART == 0){
			AdultLinkage[is][0] = AsymStart[0];
			AdultLinkage[is][1] = AsymStart[1];
		}
		else{
			AdultLinkage[is][0] = 1.0 - exp(-AsymStart[0] * RR_ARTinitiation[is + 1]);
			AdultLinkage[is][1] = 1.0 - exp(-AsymStart[1] * RR_ARTinitiation[is + 1]);
		}
	}

	// Calculate rate of testing leading to immediate ART initiation
	for (ia = 0; ia<81; ia++){
		for (is = 0; is<5; is++){
			for (ig = 0; ig < 2; ig++){
				for (ib = 0; ib < 3; ib++){ // ib is testing history indicator
					if (is>0 && ia < 5){
						PaedLinkage = PaedARTuptakeC;
						if (is < 4){ PaedLinkage *= RRearlyPaedART; }
						HCTtoART_SE[ia][is+ib*5][ig] = TestingRateEligSE[ia][is+ib*5][ig] * PaedLinkage;
					}
					if (is > 0 && ia >= 5){
						HCTtoART_SE[ia][is+ib*5][ig] = TestingRateEligSE[ia][is+ib*5][ig] * AdultLinkage[is-1][ig];
					}
				}
			}
		}
	}
	for (ia = 0; ia<20; ia++){
		for (is = 0; is<5; is++){
			for (ig = 0; ig < 2; ig++){
				if (is>0 && ia < 5){
					PaedLinkage = PaedARTuptakeC;
					if (is < 4){ PaedLinkage *= RRearlyPaedART; }
					HCTtoART_V[ia][is][ig] = TestingRateEligV[ia][is][ig] * PaedLinkage;
				}
				if (is > 0 && ia >= 5){
					HCTtoART_V[ia][is][ig] = TestingRateEligV[ia][is][ig] * AdultLinkage[is-1][ig];
				}
			}
		}
	}

	// Calculate rate of ART re-initiation
	for (ig = 0; ig < 2; ig++){
		ARTresumptionRate[ig] = ARTresumptionMin[ig] + TestingRateEligSE[30][14][ig] *
			(RetestART / RetestPos) * AdultLinkage[3][ig] * 12.0;
	}
}

void UpdateTransmProbs()
{
	int ia, ib;
	double Temp1, Temp2[8], Temp3;

	// Update male transmission probs
	MHU_ST.UpdateProbTransmM();
	MHC_ST.UpdateProbTransmM();
	MHU_STM.UpdateProbTransmM();
	MHC_STM.UpdateProbTransmM();
	MHU_LTH.UpdateProbTransmM();
	MHC_LTH.UpdateProbTransmM();
	MHU_LTL.UpdateProbTransmM();
	MHC_LTL.UpdateProbTransmM();
	MLU_ST.UpdateProbTransmM();
	MLC_ST.UpdateProbTransmM();
	MLU_STM.UpdateProbTransmM();
	MLC_STM.UpdateProbTransmM();
	MLU_LTH.UpdateProbTransmM();
	MLC_LTH.UpdateProbTransmM();
	MLU_LTL.UpdateProbTransmM();
	MLC_LTL.UpdateProbTransmM();

	// Update female transmission probs
	FH_ST.UpdateProbTransmF();
	FH_LTH.UpdateProbTransmF();
	FH_LTL.UpdateProbTransmF();
	FL_ST.UpdateProbTransmF();
	FL_LTH.UpdateProbTransmF();
	FL_LTL.UpdateProbTransmF();
	FH_SW.UpdateProbTransmSW();

	//Calculate TransmMtoF_ST
	for(ia=0; ia<81; ia++){
		Temp1 = MHU_ST.Total[ia] + MHC_ST.Total[ia] + (MHU_STM.Total[ia] + MHC_STM.Total[ia]) *
			(1.0 - MSMpartnersM[ia]) + (MHU_LTH.Total[ia] + MHC_LTH.Total[ia] +
			MHU_LTL.Total[ia] + MHC_LTL.Total[ia]) * RRpartnerMarried[0];
		for(ib=0; ib<6; ib++){
			if(Temp1>0.0 && (PrEPorVM==1 || ib==0 || ib==3)){
				Temp2[ib] = MHU_ST.Total[ia] * MHU_ST.ProbTransm[ia][ib] + MHC_ST.Total[ia] *
					MHC_ST.ProbTransm[ia][ib] + (MHU_STM.Total[ia] * MHU_STM.ProbTransm[ia][ib] +
					MHC_STM.Total[ia] * MHC_STM.ProbTransm[ia][ib]) * (1.0 - MSMpartnersM[ia]) +
					(MHU_LTH.Total[ia] * MHU_LTH.ProbTransm[ia][ib] + MHC_LTH.Total[ia] *
					MHC_LTH.ProbTransm[ia][ib] + MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][ib] +
					MHC_LTL.Total[ia] * MHC_LTL.ProbTransm[ia][ib]) * RRpartnerMarried[0];
				TransmMtoF_ST[ia][ib] = Temp2[ib]/Temp1;
			}
			else{
				TransmMtoF_ST[ia][ib] = 0.0;}
		}
		Temp1 = MLU_ST.Total[ia] + MLC_ST.Total[ia] + (MLU_STM.Total[ia] + MLC_STM.Total[ia]) *
			(1.0 - MSMpartnersM[ia]);
		for(ib=0; ib<6; ib++){
			if(Temp1>0.0 && (PrEPorVM==1 || ib==0 || ib==3)){
				Temp2[ib] = MLU_ST.Total[ia] * MLU_ST.ProbTransm[ia][ib] + MLC_ST.Total[ia] *
					MLC_ST.ProbTransm[ia][ib] + (MLU_STM.Total[ia] * MLU_STM.ProbTransm[ia][ib] +
					MLC_STM.Total[ia] * MLC_STM.ProbTransm[ia][ib]) * (1.0 - MSMpartnersM[ia]);
				TransmMtoF_ST[ia][ib+6] = Temp2[ib]/Temp1;
			}
			else{
				TransmMtoF_ST[ia][ib+6] = 0.0;}
		}
	}

	// Calculate TransmMtoM_ST
	for (ia = 0; ia < 81; ia++){
		Temp1 = MHU_STM.Total[ia] + MHC_STM.Total[ia];
		for (ib = 0; ib<4; ib++){
			if (Temp1>0.0 && (PrEPorVM == 1 || ib == 0 || ib == 2)){
				Temp2[ib] = MHU_STM.Total[ia] * MHU_STM.ProbTransm[ia][6+ib] +
					MHC_STM.Total[ia] * MHC_STM.ProbTransm[ia][6+ib];
				TransmMtoM_ST[ia][ib] = Temp2[ib] / Temp1;
			}
			else{
				TransmMtoM_ST[ia][ib] = 0.0;
			}
		}
		Temp1 = MLU_STM.Total[ia] + MLC_STM.Total[ia];
		for (ib = 0; ib<4; ib++){
			if (Temp1>0.0 && (PrEPorVM == 1 || ib == 0 || ib == 2)){
				Temp2[ib] = MLU_STM.Total[ia] * MLU_STM.ProbTransm[ia][6+ib] +
					MLC_STM.Total[ia] * MLC_STM.ProbTransm[ia][6+ib];
				TransmMtoM_ST[ia][ib + 4] = Temp2[ib] / Temp1;
			}
			else{
				TransmMtoM_ST[ia][ib + 4] = 0.0;
			}
		}
	}

	// Calculate TransmFtoM_ST
	for(ia=0; ia<81; ia++){
		Temp1 = FH_ST.Total[ia] + (FH_LTH.Total[ia] + FH_LTL.Total[ia]) * RRpartnerMarried[1];
		for(ib=0; ib<8; ib++){
			if(Temp1>0.0 && (PrEPorVM==1 || ib==0 || ib==2 || ib==4 || ib==6)){
				Temp2[ib] = FH_ST.Total[ia] * FH_ST.ProbTransm[ia][ib] + (FH_LTH.Total[ia] *
					FH_LTH.ProbTransm[ia][ib]+ FH_LTL.Total[ia] * FH_LTL.ProbTransm[ia][ib]) *
					RRpartnerMarried[1];
				TransmFtoM_ST[ia][ib] = Temp2[ib]/Temp1;
			}
			else{
				TransmFtoM_ST[ia][ib] = 0.0;}
		}
	}

	// Calculate TransmMtoF_LT
	for(ia=5; ia<81; ia++){
		Temp1 = MHU_LTH.Total[ia] + MHC_LTH.Total[ia];
		if(Temp1>0.0){
			Temp3 = MHU_LTH.Total[ia] * MHU_LTH.ProbTransm[ia][9] + MHC_LTH.Total[ia] *
				MHC_LTH.ProbTransm[ia][9];
			TransmMtoF_LT[ia][0] = Temp3/Temp1;
			if(PrEPorVM==1){
				Temp3 = MHU_LTH.Total[ia] * MHU_LTH.ProbTransm[ia][10] + MHC_LTH.Total[ia] *
					MHC_LTH.ProbTransm[ia][10];
				TransmMtoF_LT[ia][1] = Temp3/Temp1;
				Temp3 = MHU_LTH.Total[ia] * MHU_LTH.ProbTransm[ia][11] + MHC_LTH.Total[ia] *
					MHC_LTH.ProbTransm[ia][11];
				TransmMtoF_LT[ia][2] = Temp3/Temp1;
			}
		}
		Temp1 = MHU_LTL.Total[ia] + MHC_LTL.Total[ia];
		if(Temp1>0.0){
			Temp3 = MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][9] + MHC_LTL.Total[ia] *
				MHC_LTL.ProbTransm[ia][9];
			TransmMtoF_LT[ia][3] = Temp3/Temp1;
			if(PrEPorVM==1){
				Temp3 = MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][10] + MHC_LTL.Total[ia] *
					MHC_LTL.ProbTransm[ia][10];
				TransmMtoF_LT[ia][4] = Temp3/Temp1;
				Temp3 = MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][11] + MHC_LTL.Total[ia] *
					MHC_LTL.ProbTransm[ia][11];
				TransmMtoF_LT[ia][5] = Temp3/Temp1;
			}
		}
		Temp1 = MLU_LTH.Total[ia] + MLC_LTH.Total[ia];
		if(Temp1>0.0){
			Temp3 = MLU_LTH.Total[ia] * MLU_LTH.ProbTransm[ia][9] + MLC_LTH.Total[ia] *
				MLC_LTH.ProbTransm[ia][9];
			TransmMtoF_LT[ia][6] = Temp3/Temp1;
			if(PrEPorVM==1){
				Temp3 = MLU_LTH.Total[ia] * MLU_LTH.ProbTransm[ia][10] + MLC_LTH.Total[ia] *
					MLC_LTH.ProbTransm[ia][10];
				TransmMtoF_LT[ia][7] = Temp3/Temp1;
				Temp3 = MLU_LTH.Total[ia] * MLU_LTH.ProbTransm[ia][11] + MLC_LTH.Total[ia] *
					MLC_LTH.ProbTransm[ia][11];
				TransmMtoF_LT[ia][8] = Temp3/Temp1;
			}
		}
		Temp1 = MLU_LTL.Total[ia] + MLC_LTL.Total[ia];
		if(Temp1>0.0){
			Temp3 = MLU_LTL.Total[ia] * MLU_LTL.ProbTransm[ia][9] + MLC_LTL.Total[ia] *
				MLC_LTL.ProbTransm[ia][9];
			TransmMtoF_LT[ia][9] = Temp3/Temp1;
			if(PrEPorVM==1){
				Temp3 = MLU_LTL.Total[ia] * MLU_LTL.ProbTransm[ia][10] + MLC_LTL.Total[ia] *
					MLC_LTL.ProbTransm[ia][10];
				TransmMtoF_LT[ia][10] = Temp3/Temp1;
				Temp3 = MLU_LTL.Total[ia] * MLU_LTL.ProbTransm[ia][11] + MLC_LTL.Total[ia] *
					MLC_LTL.ProbTransm[ia][11];
				TransmMtoF_LT[ia][11] = Temp3/Temp1;
			}
		}
	}

	// Calculate TransmMtoFSW
	Temp1 = 0.0;
	Temp2[0] = 0.0;
	Temp2[1] = 0.0;
	for(ia=0; ia<81; ia++){
		/*Temp1 += FSWcontactRate[ia] * (MHU_ST.Total[ia] * MHU_ST.ProbTransm[ia][6] +
			MHC_ST.Total[ia] * MHC_ST.ProbTransm[ia][6] + (MHU_LTH.Total[ia] *
			MHU_LTH.ProbTransm[ia][6] + MHC_LTH.Total[ia] * MHC_LTH.ProbTransm[ia][6] +
			MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][6] + MHC_LTL.Total[ia] *
			MHC_LTL.ProbTransm[ia][6]) * FSWcontactMarried);*/
		Temp1 += MHU_ST.FSWcontactsByAge[ia] * MHU_ST.ProbTransm[ia][6] +
			MHC_ST.FSWcontactsByAge[ia] * MHC_ST.ProbTransm[ia][6] + MHU_LTH.FSWcontactsByAge[ia] *
			MHU_LTH.ProbTransm[ia][6] + MHC_LTH.FSWcontactsByAge[ia] * MHC_LTH.ProbTransm[ia][6] +
			MHU_LTL.FSWcontactsByAge[ia] * MHU_LTL.ProbTransm[ia][6] + MHC_LTL.FSWcontactsByAge[ia] *
			MHC_LTL.ProbTransm[ia][6];
		if(PrEPorVM==1){
			/*Temp2[0] += FSWcontactRate[ia] * (MHU_ST.Total[ia] * MHU_ST.ProbTransm[ia][7] +
				MHC_ST.Total[ia] * MHC_ST.ProbTransm[ia][7] + (MHU_LTH.Total[ia] *
				MHU_LTH.ProbTransm[ia][7] + MHC_LTH.Total[ia] * MHC_LTH.ProbTransm[ia][7] +
				MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][7] + MHC_LTL.Total[ia] *
				MHC_LTL.ProbTransm[ia][7]) * FSWcontactMarried);
			Temp2[1] += FSWcontactRate[ia] * (MHU_ST.Total[ia] * MHU_ST.ProbTransm[ia][8] +
				MHC_ST.Total[ia] * MHC_ST.ProbTransm[ia][8] + (MHU_LTH.Total[ia] *
				MHU_LTH.ProbTransm[ia][8] + MHC_LTH.Total[ia] * MHC_LTH.ProbTransm[ia][8] +
				MHU_LTL.Total[ia] * MHU_LTL.ProbTransm[ia][8] + MHC_LTL.Total[ia] *
				MHC_LTL.ProbTransm[ia][8]) * FSWcontactMarried);*/
			Temp2[0] += MHU_ST.FSWcontactsByAge[ia] * MHU_ST.ProbTransm[ia][7] +
				MHC_ST.FSWcontactsByAge[ia] * MHC_ST.ProbTransm[ia][7] + MHU_LTH.FSWcontactsByAge[ia] *
				MHU_LTH.ProbTransm[ia][7] + MHC_LTH.FSWcontactsByAge[ia] * MHC_LTH.ProbTransm[ia][7] +
				MHU_LTL.FSWcontactsByAge[ia] * MHU_LTL.ProbTransm[ia][7] + MHC_LTL.FSWcontactsByAge[ia] *
				MHC_LTL.ProbTransm[ia][7];
			Temp2[1] += MHU_ST.FSWcontactsByAge[ia] * MHU_ST.ProbTransm[ia][8] +
				MHC_ST.FSWcontactsByAge[ia] * MHC_ST.ProbTransm[ia][8] + MHU_LTH.FSWcontactsByAge[ia] *
				MHU_LTH.ProbTransm[ia][8] + MHC_LTH.FSWcontactsByAge[ia] * MHC_LTH.ProbTransm[ia][8] +
				MHU_LTL.FSWcontactsByAge[ia] * MHU_LTL.ProbTransm[ia][8] + MHC_LTL.FSWcontactsByAge[ia] *
				MHC_LTL.ProbTransm[ia][8];
		}
	}
	TransmMtoFSW[0] = Temp1/(TotalFSW * ClientsPA);
	TransmMtoFSW[1] = Temp2[0]/(TotalFSW * ClientsPA);
	TransmMtoFSW[2] = Temp2[1]/(TotalFSW * ClientsPA);

	// Calculate male probabilities of HIV acquisition
	MHU_ST.UpdateProbAcqM(0);
	MHC_ST.UpdateProbAcqM(0);
	MHU_STM.UpdateProbAcqM(0);
	MHC_STM.UpdateProbAcqM(0);
	MHU_LTH.UpdateProbAcqM(0);
	MHC_LTH.UpdateProbAcqM(0);
	MHU_LTL.UpdateProbAcqM(0);
	MHC_LTL.UpdateProbAcqM(0);
	MLU_ST.UpdateProbAcqM(0);
	MLC_ST.UpdateProbAcqM(0);
	MLU_STM.UpdateProbAcqM(0);
	MLC_STM.UpdateProbAcqM(0);
	MLU_LTH.UpdateProbAcqM(0);
	MLC_LTH.UpdateProbAcqM(0);
	MLU_LTL.UpdateProbAcqM(0);
	MLC_LTL.UpdateProbAcqM(0);

	if(PrEPorVM==1){
		MHU_ST.UpdateProbAcqM(1);
		MHC_ST.UpdateProbAcqM(1);
		MHU_STM.UpdateProbAcqM(1);
		MHC_STM.UpdateProbAcqM(1);
		MHU_LTH.UpdateProbAcqM(1);
		MHC_LTH.UpdateProbAcqM(1);
		MHU_LTL.UpdateProbAcqM(1);
		MHC_LTL.UpdateProbAcqM(1);
		MLU_ST.UpdateProbAcqM(1);
		MLC_ST.UpdateProbAcqM(1);
		MLU_STM.UpdateProbAcqM(1);
		MLC_STM.UpdateProbAcqM(1);
		MLU_LTH.UpdateProbAcqM(1);
		MLC_LTH.UpdateProbAcqM(1);
		MLU_LTL.UpdateProbAcqM(1);
		MLC_LTL.UpdateProbAcqM(1);
	}

	// Calculate female probabilities of HIV acquisition
	FH_ST.UpdateProbAcqF(0);
	FH_SW.UpdateProbAcqF(0);
	FH_LTH.UpdateProbAcqF(0);
	FH_LTL.UpdateProbAcqF(0);
	FL_ST.UpdateProbAcqF(0);
	FL_LTH.UpdateProbAcqF(0);
	FL_LTL.UpdateProbAcqF(0);

	if(PrEPorVM==1){
		FH_ST.UpdateProbAcqF(1);
		FH_SW.UpdateProbAcqF(1);
		FH_LTH.UpdateProbAcqF(1);
		FH_LTL.UpdateProbAcqF(1);
		FL_ST.UpdateProbAcqF(1);
		FL_LTH.UpdateProbAcqF(1);
		FL_LTL.UpdateProbAcqF(1);
		FH_ST.UpdateProbAcqF(2);
		FH_SW.UpdateProbAcqF(2);
		FH_LTH.UpdateProbAcqF(2);
		FH_LTL.UpdateProbAcqF(2);
		FL_ST.UpdateProbAcqF(2);
		FL_LTH.UpdateProbAcqF(2);
		FL_LTL.UpdateProbAcqF(2);
	}
}

void CalcHIVtransitions()
{
	// Male transitions
	MHU_virgin.GetEndProfile();
	MHC_virgin.GetEndProfile();
	MHU_ST.GetEndProfile();
	MHC_ST.GetEndProfile();
	MHU_STM.GetEndProfile();
	MHC_STM.GetEndProfile();
	MHU_LTH.GetEndProfile();
	MHC_LTH.GetEndProfile();
	MHU_LTL.GetEndProfile();
	MHC_LTL.GetEndProfile();
	MLU_virgin.GetEndProfile();
	MLC_virgin.GetEndProfile();
	MLU_ST.GetEndProfile();
	MLC_ST.GetEndProfile();
	MLU_STM.GetEndProfile();
	MLC_STM.GetEndProfile();
	MLU_LTH.GetEndProfile();
	MLC_LTH.GetEndProfile();
	MLU_LTL.GetEndProfile();
	MLC_LTL.GetEndProfile();

	// Female transitions
	FH_virgin.GetEndProfile();
	FH_ST.GetEndProfile();
	FH_SW.GetEndProfile();
	//if(CurrYear>=2015){FH_SW.GetEndProfileFSW();}
	//else{FH_SW.GetEndProfile();}
	FH_LTH.GetEndProfile();
	FH_LTL.GetEndProfile();
	FL_virgin.GetEndProfile();
	FL_ST.GetEndProfile();
	FL_LTH.GetEndProfile();
	FL_LTL.GetEndProfile();
}

void CopyEndToStart()
{
	// Male transitions
	MHU_virgin.UpdateStartProfile();
	MHC_virgin.UpdateStartProfile();
	MHU_ST.UpdateStartProfile();
	MHC_ST.UpdateStartProfile();
	MHU_STM.UpdateStartProfile();
	MHC_STM.UpdateStartProfile();
	MHU_LTH.UpdateStartProfile();
	MHC_LTH.UpdateStartProfile();
	MHU_LTL.UpdateStartProfile();
	MHC_LTL.UpdateStartProfile();
	MLU_virgin.UpdateStartProfile();
	MLC_virgin.UpdateStartProfile();
	MLU_ST.UpdateStartProfile();
	MLC_ST.UpdateStartProfile();
	MLU_STM.UpdateStartProfile();
	MLC_STM.UpdateStartProfile();
	MLU_LTH.UpdateStartProfile();
	MLC_LTH.UpdateStartProfile();
	MLU_LTL.UpdateStartProfile();
	MLC_LTL.UpdateStartProfile();

	// Female transitions
	FH_virgin.UpdateStartProfile();
	FH_ST.UpdateStartProfile();
	FH_SW.UpdateStartProfile();
	FH_LTH.UpdateStartProfile();
	FH_LTL.UpdateStartProfile();
	FL_virgin.UpdateStartProfile();
	FL_ST.UpdateStartProfile();
	FL_LTH.UpdateStartProfile();
	FL_LTL.UpdateStartProfile();
}

void SetARTinitiation()
{
	int ig, is, im;
	double Temp;

	// Set adult ART initiation
	if (InputARTinitiationRates == 0){
		Temp = StartingART_M + StartingART_F;
		if (Temp == 0.0 && CurrYear <= ARTdataYr){
			CurrARTinitiation[0] = 0.0;
			CurrARTinitiation[1] = 0.0;
		}
		else if (Temp == 0.0 && CurrYear > ARTdataYr){
			for (ig = 0; ig<2; ig++){
				if (CurrYear>UltARTyr[ig]){
					CurrARTinitiation[ig] = 1.0 / UltARTdelay[ig];
				}
				else{
					CurrARTinitiation[ig] = StoredRoot[ig] + ((1.0 / UltARTdelay[ig]) -
						StoredRoot[ig]) * (CurrYear - ARTdataYr) / (UltARTyr[ig] - ARTdataYr);
				}
			}
		}
		else{
			SetAdultARTinitiation2();
		}
	}
	else{
		CurrARTinitiation[0] = RateARTstartF[CurrYear - StartYear] * RR_ARTstartM;
		CurrARTinitiation[1] = RateARTstartF[CurrYear - StartYear];
		if ((CurrYear == 2019 && CurrMonth >= 9) || CurrYear == 2020){
			CurrARTinitiation[0] *= (1.0 - COVIDimpactARTstart);
			CurrARTinitiation[1] *= (1.0 - COVIDimpactARTstart);
		}
	}
	AsymStart[0] = CurrARTinitiation[0] * RR_ARTstart1stMo;
	AsymStart[1] = CurrARTinitiation[1] * RR_ARTstart1stMo;
	for (ig = 0; ig < 2; ig++){
		ARTinitByStage[0][ig] = 0.0;
		for (is = 1; is < 5; is++){
			ARTinitByStage[is][ig] = CurrARTinitiation[ig] * RR_ARTinitiation[is] /
				RR_ARTinitiation[4];
		}
		ARTinitByStage[1][ig] *= EligibleAsymPre500[CurrYear - StartYear];
		ARTinitByStage[2][ig] *= EligibleAsym500[CurrYear - StartYear];
		ARTinitByStage[3][ig] *= EligibleAsym350[CurrYear - StartYear];
	}

	// Set paediatric ART initiation
	if (InputARTinitiationRates == 0){
		if (CurrYear <= InterpolationStart){
			if (StartingART_P == 0.0){
				PaedARTinitiation = 0.0;
			}
			else{
				SetPaedARTinitiation2();
			}
		}
		else{
			if (CurrYear >= UltimateYr){
				PaedARTinitiation = 1.0 / UltARTdelayC;
			}
			else{
				PaedARTinitiation = ARTinitiationStore + ((1.0 / UltARTdelayC) - ARTinitiationStore) *
					(CurrYear - InterpolationStart) / (UltimateYr - InterpolationStart);
			}
		}
		PaedARTuptakeC = 1.0 - exp(-PaedARTinitiation * RR_ARTstart1stMo);
		// Note that we're not applying ImmARTcorrectionP (which is now only used for linkage after EID).
		// Also note PaedARTuptakeC is always a probability, unlike AsymStart, which is a rate.
	}
	else{
		PaedARTinitiation = RateARTstartC[CurrYear - StartYear];
	}
	ARTinitByStage[0][2] = 0.0;
	ARTinitByStage[4][2] = PaedARTinitiation;
	for (is = 1; is < 4; is++){
		ARTinitByStage[is][2] = ARTinitByStage[4][2] * RRearlyPaedART*EarlyART5to14[CurrYear-StartYear];}

	//if(FixedARTinitiation==1 && FixedUncertainty==1 && CurrYear<InterpolationStart){
	//	PaedARTinitiation = TotalARTinitiation.out[CurrSim-1][CurrYear-1985]/12.0;}
	ARTinitiation[0] = 0.0;
	ARTinitiation[1] = 0.0;
	for(im=2; im<132; im++){
		ARTinitiation[im] = PaedARTinitiation;}
}

void SetAdultARTinitiation()
{
	// This function is redundant.

	int ia, ig, is, is1;
	double SumQuad[2][2]; // Last index is sex (cell AG94:AK94 of 'ART start')
	double RelARTinitiation[5], TempA, TempB, Temp, DiagnosedToART[2];
	double Cparameter[2]; // Starting ART amongst previously diagnosed (AJ8:AK8)
	double Discriminant[2]; // b^2 - 4ac (cells AG97, AI97)

	// Calculate numbers starting ART immediately after diagnosis
	for(ig=0; ig<2; ig++){
		DiagnosedToART[ig] = 0.0;
		for(ia=0; ia<76; ia++){
			for(is=0; is<15; is++){
				if(ig==0){
					TempA = SumGroupsM[ia+5][5+is];
					if(ia<15){
						TempB = SumGroupsVM[ia+5][5+is];}
					else{
						TempB = 0.0;}
				}
				else{
					TempA = SumGroupsF[ia+5][5+is];
					if(ia<15){
						TempB = SumGroupsVF[ia+5][5+is];}
					else{
						TempB = 0.0;}
				}
				if(is==3 || is==4){
					Temp = MnthlyAIDSmort[ia+5][is-3][ig];}
				else if(is==8 || is==9){
					Temp = MnthlyAIDSmort[ia+5][is-8][ig];}
				else if(is==13 || is==14){
					Temp = MnthlyAIDSmort[ia+5][is-13][ig];}
				else{
					Temp = 0.0;}
				DiagnosedToART[ig] += (TempA - TempB) * HCTtoART_SE[ia+5][is][ig] *
					(1.0 - 0.5 * (HCTtoART_SE[ia+5][is][ig] + Temp));
				if(ia<15){
					if(is<5){is1 = is;}
					else if(is<10){is1 = is - 5;}
					else{is1 = is - 10;}
					DiagnosedToART[ig] += TempB * HCTtoART_V[ia+5][is1][ig] *
						(1.0 - 0.5 * (HCTtoART_V[ia+5][is1][ig] + Temp));
				}
			}
		}
	}
	Cparameter[0] = (StartingART_M/12.0) - DiagnosedToART[0];
	Cparameter[1] = (StartingART_F/12.0) - DiagnosedToART[1];

	// Calculate quadratic terms for rate of ART initiation in previously-diagnosed.
	RelARTinitiation[0] = 0.0;
	RelARTinitiation[1] = RR_ARTinitiation[1] * EligibleAsymPre500[CurrYear-StartYear];
	RelARTinitiation[2] = RR_ARTinitiation[2] * EligibleAsym500[CurrYear-StartYear];
	RelARTinitiation[3] = RR_ARTinitiation[3] * EligibleAsym350[CurrYear-StartYear];
	RelARTinitiation[4] = RR_ARTinitiation[4];

	SumQuad[0][0] = 0.0;
	SumQuad[0][1] = 0.0;
	SumQuad[1][0] = 0.0;
	SumQuad[1][1] = 0.0;
	for(ia=0; ia<76; ia++){
		// Males
		TempA = 0.0;
		TempB = 0.0;
		for(is=0; is<5; is++){
			TempA += 0.5 * SumGroupsM[ia+5][15+is] * pow(RelARTinitiation[is], 2.0);
			TempB += SumGroupsM[ia+5][15+is] * RelARTinitiation[is];
		}
		TempB = -TempB;
		for(is=0; is<2; is++){
			TempB += 0.5 * SumGroupsM[ia+5][18+is] * RelARTinitiation[is+3] *
				MnthlyAIDSmort[ia+5][is][0];}
		SumQuad[0][0] += TempA;
		SumQuad[1][0] += TempB;
		// Females
		TempA = 0.0;
		TempB = 0.0;
		for(is=0; is<5; is++){
			TempA += 0.5 * SumGroupsF[ia+5][15+is] * pow(RelARTinitiation[is], 2.0);
			TempB += SumGroupsF[ia+5][15+is] * RelARTinitiation[is];
		}
		TempB = -TempB;
		for(is=0; is<2; is++){
			TempB += 0.5 * SumGroupsF[ia+5][18+is] * RelARTinitiation[is+3] *
				MnthlyAIDSmort[ia+5][is][1];}
		SumQuad[0][1] += TempA;
		SumQuad[1][1] += TempB;
	}

	// Calculate rates of ART initiation in non-pregant, asymptomatic previously-diagnosed adults.
	for(ig=0; ig<2; ig++){
		Discriminant[ig] = pow(SumQuad[1][ig], 2.0) - 4.0 * SumQuad[0][ig] * Cparameter[ig];
		if(Cparameter[ig]<0.0){
			AdultRoot[ig] = 0.0;}
		else if(Discriminant[ig]>0.0){
			AdultRoot[ig] = (-SumQuad[1][ig] - pow(Discriminant[ig], 0.5))/(2.0 *
				SumQuad[0][ig]);}
		else{
			AdultRoot[ig] = 1.0/MinARTdelay[ig];}
		if (AdultRoot[ig] >= 1.0 / MinARTdelay[ig]){
			ARTerrorInd = 1;
			if (pow(AdultRoot[ig] - 1.0 / MinARTdelay[ig], 2.0) > MaxARTerror){
				MaxARTerror = pow(AdultRoot[ig] - 1.0 / MinARTdelay[ig], 2.0);
			}
		}
		CurrARTinitiation[ig] = AdultRoot[ig];
		if(CurrYear==ARTdataYr){
			StoredRoot[ig] = AdultRoot[ig];}
	}
	if (FixedUncertainty == 1 && FixedARTinitiation == 1){
		AdultRoot[0] = AdultRootM.out[CurrSim - 1][CurrYear - StartYear];
		AdultRoot[1] = AdultRootF.out[CurrSim - 1][CurrYear - StartYear];
	}
	if (FixedUncertainty == 1 && CurrMonth == 6 && FixedARTinitiation == 0){
		AdultRootM.out[CurrSim - 1][CurrYear - StartYear] = AdultRoot[0];
		AdultRootF.out[CurrSim - 1][CurrYear - StartYear] = AdultRoot[1];
	}
}

void SetAdultARTinitiation2()
{
	int ia, ig, is, is1;
	double SumQuad[2][2]; // Last index is sex (cell AG94:AK94 of 'ART start')
	double RelARTinitiation[5], TempA, TempB, Temp, NewDiagnosedElig[4][2];
	double Cparameter[2]; // Starting ART
	double Discriminant[2]; // b^2 - 4ac (cells AG97, AI97)

	// Calculate numbers newly diagnosed/rediagnosed who are ART-eligible
	for (ig = 0; ig<2; ig++){
		for (is1 = 0; is1 < 4; is1++){ // is1 = CD4 stage
			NewDiagnosedElig[is1][ig] = 0.0;
			for (ia = 0; ia < 76; ia++){
				for (is = 0; is < 3; is++){
					// is is the testing history indicator (0 = never tested, 1 = last tested negative,
					// 2 = previously diagnosed)
					if (ig == 0){
						TempA = SumGroupsM[ia + 5][6 + is * 5 + is1];
						if (ia < 15){
							TempB = SumGroupsVM[ia + 5][6 + is * 5 + is1];
						}
						else{
							TempB = 0.0;
						}
					}
					else{
						TempA = SumGroupsF[ia + 5][6 + is * 5 + is1];
						if (ia < 15){
							TempB = SumGroupsVF[ia + 5][6 + is * 5 + is1];
						}
						else{
							TempB = 0.0;
						}
					}
					NewDiagnosedElig[is1][ig] += (TempA - TempB) *
						TestingRateEligSE[ia + 5][1 + is * 5 + is1][ig];
					if (ia < 15){
						NewDiagnosedElig[is1][ig] += TempB * TestingRateEligV[ia + 5][1 + is1][ig];
					}
				}
				/*if (is1 == 0){
					// We only need to execute this loop once, since ART interrupters aren't
					// stratified by CD4 (i.e. no need to loop over is1).
					NewDiagnosedElig[is1][ig] += TotalInterrupt[ia + 15][ig] *
						TestingRateEligSE[ia + 5][14][ig] * RetestART / RetestPos;
				}*/
			}
		}
	}
	Cparameter[0] = (StartingART_M / 12.0);
	Cparameter[1] = (StartingART_F / 12.0);
	if (CurrYear == 2019){
		Temp = 9.0 + 3.0 * (1.0 - COVIDimpactARTstart);
		for (ig = 0; ig < 2; ig++){
			if (CurrMonth < 9){ Cparameter[ig] *= 12.0 / Temp; }
			else{ Cparameter[ig] *= 12.0 * (1.0 - COVIDimpactARTstart) / Temp; }
		}
	}

	// Calculate quadratic terms for rate of ART initiation in previously-diagnosed.
	RelARTinitiation[0] = 0.0;
	RelARTinitiation[1] = RR_ARTinitiation[1] * EligibleAsymPre500[CurrYear - StartYear];
	RelARTinitiation[2] = RR_ARTinitiation[2] * EligibleAsym500[CurrYear - StartYear];
	RelARTinitiation[3] = RR_ARTinitiation[3] * EligibleAsym350[CurrYear - StartYear];
	RelARTinitiation[4] = RR_ARTinitiation[4];

	SumQuad[0][0] = 0.0;
	SumQuad[0][1] = 0.0;
	SumQuad[1][0] = 0.0;
	SumQuad[1][1] = 0.0;
	for (ia = 0; ia<76; ia++){
		// Males
		TempA = 0.0;
		TempB = 0.0;
		for (is = 0; is<5; is++){ // Now is = CD4 stage
			TempA += 0.5 * SumGroupsM[ia + 5][15 + is] * pow(RelARTinitiation[is], 2.0);
			TempB += SumGroupsM[ia + 5][15 + is] * RelARTinitiation[is];
		}
		TempB = -TempB;
		/*for (is = 0; is<2; is++){
			// Consider removing this mortality correction in next version of Thembisa?
			// But doing so leads to poorer consistency between ART inputs and outputs.
			TempB += 0.5 * SumGroupsM[ia + 5][18 + is] * RelARTinitiation[is + 3] *
				MnthlyAIDSmort[ia + 5][is][0];
		}*/
		SumQuad[0][0] += TempA;
		SumQuad[1][0] += TempB;
		// Females
		TempA = 0.0;
		TempB = 0.0;
		for (is = 0; is<5; is++){
			TempA += 0.5 * SumGroupsF[ia + 5][15 + is] * pow(RelARTinitiation[is], 2.0);
			TempB += SumGroupsF[ia + 5][15 + is] * RelARTinitiation[is];
		}
		TempB = -TempB;
		/*for (is = 0; is<2; is++){
			// Consider removing this mortality correction in next version of Thembisa?
			TempB += 0.5 * SumGroupsF[ia + 5][18 + is] * RelARTinitiation[is + 3] *
				MnthlyAIDSmort[ia + 5][is][1];
		}*/
		SumQuad[0][1] += TempA;
		SumQuad[1][1] += TempB;
		/*for (ig = 0; ig < 2; ig++){
			SumQuad[0][ig] += 0.5 * TotalInterrupt[ia + 15][ig] * pow(RelARTinitiation[4], 2.0);
			SumQuad[1][ig] = SumQuad[1][ig] - TotalInterrupt[ia + 15][ig] * RelARTinitiation[4];
		}*/
	}
	for (is = 1; is < 5; is++){
		SumQuad[0][0] += 0.5 * NewDiagnosedElig[is - 1][0] * pow(RR_ARTinitiation[is] * RR_ARTstart1stMo, 2.0);
		SumQuad[0][1] += 0.5 * NewDiagnosedElig[is - 1][1] * pow(RR_ARTinitiation[is] * RR_ARTstart1stMo, 2.0);
		SumQuad[1][0] = SumQuad[1][0] - NewDiagnosedElig[is - 1][0] * RR_ARTinitiation[is] * RR_ARTstart1stMo;
		SumQuad[1][1] = SumQuad[1][1] - NewDiagnosedElig[is - 1][1] * RR_ARTinitiation[is] * RR_ARTstart1stMo;
	}

	// Calculate rates of ART initiation in non-pregant, asymptomatic previously-diagnosed adults.
	for (ig = 0; ig<2; ig++){
		Discriminant[ig] = pow(SumQuad[1][ig], 2.0) - 4.0 * SumQuad[0][ig] * Cparameter[ig];
		if (Cparameter[ig]<0.0){
			AdultRoot[ig] = 0.0;
		}
		else if (Discriminant[ig]>0.0){
			AdultRoot[ig] = (-SumQuad[1][ig] - pow(Discriminant[ig], 0.5)) / (2.0 *
				SumQuad[0][ig]);
		}
		else{
			AdultRoot[ig] = 1.0 / MinARTdelay[ig];
		}
		if (AdultRoot[ig] >= 1.0 / MinARTdelay[ig]){
			ARTerrorInd = 1;
			if (pow(AdultRoot[ig] - 1.0 / MinARTdelay[ig], 2.0) > MaxARTerror){
				MaxARTerror = pow(AdultRoot[ig] - 1.0 / MinARTdelay[ig], 2.0);
			}
		}
		CurrARTinitiation[ig] = AdultRoot[ig];
		if (CurrYear == ARTdataYr && CurrMonth == 5){
			StoredRoot[ig] = AdultRoot[ig];
		}
	}
	if (FixedUncertainty == 1 && FixedARTinitiation == 1){
		AdultRoot[0] = AdultRootM.out[CurrSim - 1][CurrYear - StartYear];
		AdultRoot[1] = AdultRootF.out[CurrSim - 1][CurrYear - StartYear];
	}
	if (FixedUncertainty == 1 && CurrMonth == 6 && FixedARTinitiation == 0){
		AdultRootM.out[CurrSim - 1][CurrYear - StartYear] = AdultRoot[0];
		AdultRootF.out[CurrSim - 1][CurrYear - StartYear] = AdultRoot[1];
	}
}

void CalcCOVIDimpactART()
{
	int ia, is, ig;

	for (ia = 0; ia < 81; ia++){
		for (is = 0; is < 15; is++){
			for (ig = 0; ig < 2; ig++){
				HCTtoART_SE[ia][is][ig] *= (1.0 - COVIDimpactARTstart);
			}
		}
	}
	for (ia = 0; ia < 20; ia++){
		for (is = 0; is < 5; is++){
			for (ig = 0; ig < 2; ig++){
				HCTtoART_V[ia][is][ig] *= (1.0 - COVIDimpactARTstart);
			}
		}
	}
}

void SetPaedARTinitiation()
{
	int ia, im, iy, is;
	double sumA, sumB, TargetStartingART, root;
	double MnthlyTestProb[2]; // for early and late disease

	iy = CurrYear - StartYear;
	sumA = 0.0;
	sumB = 0.0;
	for(im=0; im<132; im++){
		MnthlyTestProb[0] = 0.0;
		if (im == 18){ MnthlyTestProb[0] = CurrTesting18mo[1]; }
		if (im > 18 && im<60){ MnthlyTestProb[0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestAgePaed); }
		if (im >= 60){ MnthlyTestProb[0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin); }
		if (im < 60){ MnthlyTestProb[1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced * RRtestAgePaed); }
		else{ MnthlyTestProb[1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced); }
		StartingARTnoBirthPCR[im][0] = MnthlyTestProb[1] * (MaleChild.ARTeligible[im] + FemChild.ARTeligible[im]) * PaedARTuptakeC *
			pow(1.0 - AIDSmortNoART[im], MortAdjLate[0]); // New
		if (im >= 18 && im<60){
			StartingARTnoBirthPCR[im][0] += MnthlyTestProb[0] * (MaleChild.PosChildAtBirthPMTCT[im] +
				MaleChild.PosChildAtBirthNoPMTCT[im] + MaleChild.PosChildAfterBirth[im] + FemChild.PosChildAtBirthPMTCT[im] +
				FemChild.PosChildAtBirthNoPMTCT[im] + FemChild.PosChildAfterBirth[im]) * EarlyART1to4[iy] * RRearlyPaedART *
				PaedARTuptakeC;
		}
		if (im >= 60){
			StartingARTnoBirthPCR[im][0] += MnthlyTestProb[0] * (MaleChild.PosChildAtBirthPMTCT[im] +
				MaleChild.PosChildAtBirthNoPMTCT[im] + MaleChild.PosChildAfterBirth[im] + FemChild.PosChildAtBirthPMTCT[im] +
				FemChild.PosChildAtBirthNoPMTCT[im] + FemChild.PosChildAfterBirth[im]) * EarlyART5to14[iy] * RRearlyPaedART *
				PaedARTuptakeC;
		}
		StartingARTnoBirthPCR[im][1] = (MaleChild.DiagChildAtBirthPMTCT[im] + MaleChild.DiagChildAtBirthNoPMTCT[im] +
			MaleChild.DiagChildAfterBirth[im] + FemChild.DiagChildAtBirthPMTCT[im] +
			FemChild.DiagChildAtBirthNoPMTCT[im] + FemChild.DiagChildAfterBirth[im]) * RRearlyPaedART;
		if (im < 12){ StartingARTnoBirthPCR[im][1] *= EligInfants; }
		else if (im < 60){ StartingARTnoBirthPCR[im][1] *= EarlyART1to4[iy]; }
		else{ StartingARTnoBirthPCR[im][1] *= EarlyART5to14[iy]; }
		StartingARTnoBirthPCR[im][1] += (MaleChild.DiagARTeligible[im] + FemChild.DiagARTeligible[im]) *
			pow(1.0 - AIDSmortNoART[im], MortAdjLate[0]); // New
		sumA += StartingARTnoBirthPCR[im][0];
		sumB += StartingARTnoBirthPCR[im][1];
	}
	for(ia=0; ia<5; ia++){
		for (is = 1; is < 5; is++){
			sumA += ((SumGroupsM[ia][is + 5] + SumGroupsM[ia][is + 10]) * TestingRateV[ia][is][0] +
				(SumGroupsF[ia][is + 5] + SumGroupsF[ia][is + 10]) * TestingRateV[ia][is][1]) *
				EarlyART5to14[iy] * PaedARTuptakeC;
		}
		sumA += (1.0 - EarlyART5to14[iy]) * PaedARTuptakeC * ((SumGroupsM[ia][9] + SumGroupsM[ia][14]) *
			TestingRateV[ia][4][0] * (1.0 - MnthlyAIDSmort[ia][1][0]) + (SumGroupsF[ia][9] + SumGroupsF[ia][14]) * TestingRateV[ia][4][1] *
			(1.0 - MnthlyAIDSmort[ia][1][1])); // New
		for (is = 0; is < 4; is++){
			sumB += (SumGroupsM[ia][is + 15] + SumGroupsF[ia][is + 15]) * EarlyART5to14[iy] * RRearlyPaedART;}
		sumB += SumGroupsM[ia][19] * (1.0 - MnthlyAIDSmort[ia][1][0]) + SumGroupsF[ia][19] * (1.0 - MnthlyAIDSmort[ia][1][1]); // New
	}

	if(CurrYear<=InterpolationStart){
		TargetStartingART = (StartingART_P / 12.0) - ((MaleChild.PosChildAtBirthNoPMTCT[1] * SixWeekSe[0] +
			MaleChild.PosChildAtBirthPMTCT[1] * SixWeekSe[1]) * EligInfants + MaleChild.ARTeligible[1] * SixWeekSe[2] +
			(FemChild.PosChildAtBirthNoPMTCT[1] * SixWeekSe[0] + FemChild.PosChildAtBirthPMTCT[1] * SixWeekSe[1]) *
			EligInfants + FemChild.ARTeligible[1] * SixWeekSe[2]) * PCRuptake * PaedARTuptakeEID;
		TargetStartingART = TargetStartingART - (ChildPosNoPMTCT * BirthSe[0] + ChildPosPMTCT * BirthSe[1]) *
			EligInfants * PCRuptakeB * PaedARTuptakeEID/12.0;
		root = (TargetStartingART - sumA) / sumB;
		if (root < 0.0){ root = 0.0; }
		else if (root < 1.0){ root = -log(1.0 - root); }
		else if (root >= 1.0){
			ARTerrorInd = 1;
			if (pow(root - 1.0, 2.0) > MaxARTerror){
				MaxARTerror = pow(root - 1.0, 2.0);
			}
		}
	}
	else{
		if(CurrYear>=UltimateYr){
			root = 1.0/UltARTdelayC;}
		else{
			root = ARTinitiationStore + ((1.0/UltARTdelayC) - ARTinitiationStore) *
				(CurrYear - InterpolationStart) * (UltimateYr - InterpolationStart);}
	}

	PaedARTinitiation = root;
	if (FixedUncertainty == 1 && CurrMonth == 6 && FixedARTinitiation == 0){
		ChildRoot.out[CurrSim - 1][CurrYear - StartYear] = root;
	}
	if(CurrYear==InterpolationStart){
		ARTinitiationStore = root;}
}

void SetPaedARTinitiation2()
{
	int ia, im, iy, is;
	double sumA, sumB, TargetStartingART, root;
	double MnthlyTestProb[2]; // for early and late disease

	// Note that where we refer to StartingARTnoBirthPCR, we are either referring to numbers of new diagnoses (2nd index = 0)
	// or numbers of ART-naive, previously diagnosed children (2nd index = 1).

	iy = CurrYear - StartYear;
	sumA = 0.0;
	sumB = 0.0;
	for (im = 0; im<132; im++){
		MnthlyTestProb[0] = 0.0;
		if (im == 18){ MnthlyTestProb[0] = CurrTesting18mo[1]; }
		if (im > 18 && im<60){ MnthlyTestProb[0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestAgePaed); }
		if (im >= 60){ MnthlyTestProb[0] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin); }
		if (im < 60){ MnthlyTestProb[1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced * RRtestAgePaed); }
		else{ MnthlyTestProb[1] = 1.0 - exp(-TestingRateNPF[5] * RRtestVirgin * RRtestPaedAdvanced); }
		StartingARTnoBirthPCR[im][0] = MnthlyTestProb[1] * (MaleChild.ARTeligible[im] + FemChild.ARTeligible[im]) *
			RR_ARTstart1stMo;
		if (im >= 18 && im<60){
			StartingARTnoBirthPCR[im][0] += MnthlyTestProb[0] * (MaleChild.PosChildAtBirthPMTCT[im] +
				MaleChild.PosChildAtBirthNoPMTCT[im] + MaleChild.PosChildAfterBirth[im] + FemChild.PosChildAtBirthPMTCT[im] +
				FemChild.PosChildAtBirthNoPMTCT[im] + FemChild.PosChildAfterBirth[im]) * EarlyART1to4[iy] * RRearlyPaedART *
				RR_ARTstart1stMo;
		}
		if (im >= 60){
			StartingARTnoBirthPCR[im][0] += MnthlyTestProb[0] * (MaleChild.PosChildAtBirthPMTCT[im] +
				MaleChild.PosChildAtBirthNoPMTCT[im] + MaleChild.PosChildAfterBirth[im] + FemChild.PosChildAtBirthPMTCT[im] +
				FemChild.PosChildAtBirthNoPMTCT[im] + FemChild.PosChildAfterBirth[im]) * EarlyART5to14[iy] * RRearlyPaedART *
				RR_ARTstart1stMo;
		}
		StartingARTnoBirthPCR[im][1] = (MaleChild.DiagChildAtBirthPMTCT[im] + MaleChild.DiagChildAtBirthNoPMTCT[im] +
			MaleChild.DiagChildAfterBirth[im] + FemChild.DiagChildAtBirthPMTCT[im] +
			FemChild.DiagChildAtBirthNoPMTCT[im] + FemChild.DiagChildAfterBirth[im]) * RRearlyPaedART;
		if (im < 12){ StartingARTnoBirthPCR[im][1] *= EligInfants; }
		else if (im < 60){ StartingARTnoBirthPCR[im][1] *= EarlyART1to4[iy]; }
		else{ StartingARTnoBirthPCR[im][1] *= EarlyART5to14[iy]; }
		StartingARTnoBirthPCR[im][1] += (MaleChild.DiagARTeligible[im] + FemChild.DiagARTeligible[im]);
		sumA += StartingARTnoBirthPCR[im][0];
		sumB += StartingARTnoBirthPCR[im][1];
	}
	for (ia = 0; ia<5; ia++){
		for (is = 1; is < 4; is++){
			sumA += ((SumGroupsM[ia][is + 5] + SumGroupsM[ia][is + 10]) * TestingRateV[ia][is][0] +
				(SumGroupsF[ia][is + 5] + SumGroupsF[ia][is + 10]) * TestingRateV[ia][is][1]) *
				EarlyART5to14[iy] * RR_ARTstart1stMo;
		}
		sumA += ((SumGroupsM[ia][9] + SumGroupsM[ia][14]) * TestingRateV[ia][4][0] +
			(SumGroupsF[ia][9] + SumGroupsF[ia][14]) * TestingRateV[ia][4][1]) * RR_ARTstart1stMo;
		for (is = 0; is < 4; is++){
			sumB += (SumGroupsM[ia][is + 15] + SumGroupsF[ia][is + 15]) * EarlyART5to14[iy] * RRearlyPaedART;
		}
		sumB += SumGroupsM[ia][19] + SumGroupsF[ia][19];
	}

	if (CurrYear <= InterpolationStart){
		TargetStartingART = (StartingART_P / 12.0) - ((MaleChild.PosChildAtBirthNoPMTCT[1] * SixWeekSe[0] +
			MaleChild.PosChildAtBirthPMTCT[1] * SixWeekSe[1]) * EligInfants + MaleChild.ARTeligible[1] * SixWeekSe[2] +
			(FemChild.PosChildAtBirthNoPMTCT[1] * SixWeekSe[0] + FemChild.PosChildAtBirthPMTCT[1] * SixWeekSe[1]) *
			EligInfants + FemChild.ARTeligible[1] * SixWeekSe[2]) * PCRuptake * PaedARTuptakeEID;
		TargetStartingART = TargetStartingART - (ChildPosNoPMTCT * BirthSe[0] + ChildPosPMTCT * BirthSe[1]) *
			EligInfants * PCRuptakeB * PaedARTuptakeEID / 12.0;
		root = TargetStartingART / (sumA + sumB);
		if (root < 0.0){ root = 0.0; }
		else if (root < 1.0){ root = -log(1.0 - root); }
		else if (root >= 1.0){
			ARTerrorInd = 1;
			if (pow(root - 1.0, 2.0) > MaxARTerror){
				MaxARTerror = pow(root - 1.0, 2.0);
			}
		}
	}
	else{
		if (CurrYear >= UltimateYr){
			root = 1.0 / UltARTdelayC;
		}
		else{
			root = ARTinitiationStore + ((1.0 / UltARTdelayC) - ARTinitiationStore) *
				(CurrYear - InterpolationStart) * (UltimateYr - InterpolationStart);
		}
	}

	PaedARTinitiation = root;
	if (FixedUncertainty == 1 && CurrMonth == 6 && FixedARTinitiation == 0){
		ChildRoot.out[CurrSim - 1][CurrYear - StartYear] = root;
	}
	if (CurrYear == InterpolationStart){
		ARTinitiationStore = root;
	}
}

void UpdateFSW()
{
	int ia, is;
	double NewFSW, LeaveFSW, Temp, Temp2, MnthlyExit;
	double Retiring[81], BaseFSWentry[81], TempART[4];

	MnthlyExit = 1.0 - exp(-1/(12.0*DurFSW));
	for(ia=0; ia<81; ia++){
		// First calculate number of women retiring from sex work
		Temp = FH_SW.NegNoHCT_E[ia] + FH_SW.NegPastHCT_E[ia] + FH_SW.RegHCT_E[ia] +
			FH_SW.RegPrEP_E[ia] + FH_SW.RegVM_E[ia];
		for(is=0; is<5; is++){
			Temp += FH_SW.PosNoHCT_E[ia][is] * RelativeCSWexit[is];
			Temp += FH_SW.PosHCTpreHIV_E[ia][is] * RelativeCSWexit[is+5];
			Temp += FH_SW.PosDiagnosedPreART_E[ia][is] * RelativeCSWexit[is+10];
			Temp += FH_SW.OnARTpre500_E[ia][is] * RelativeCSWexit[is+15];
			Temp += FH_SW.OnART500_E[ia][is] * RelativeCSWexit[is+20];
			Temp += FH_SW.OnART350_E[ia][is] * RelativeCSWexit[is+25];
			Temp += FH_SW.OnART200_E[ia][is] * RelativeCSWexit[is+30];
		}
		for(is=0; is<4; is++){
			Temp += FH_SW.StoppedART_E[ia][is] * RelativeCSWexit[is+35];}
		Retiring[ia] = MnthlyExit * Temp;
		// Second calculate number eligible to enter sex work
		Temp2 = FH_ST.NegNoHCT_E[ia] + FH_ST.NegPastHCT_E[ia] + FH_ST.RegHCT_E[ia] +
			FH_ST.RegPrEP_E[ia] + FH_ST.RegVM_E[ia];
		for(is=0; is<5; is++){
			Temp2 += FH_ST.PosNoHCT_E[ia][is] * RelativeCSWentry[is];
			Temp2 += FH_ST.PosHCTpreHIV_E[ia][is] * RelativeCSWentry[is+5];
			Temp2 += FH_ST.PosDiagnosedPreART_E[ia][is] * RelativeCSWentry[is+10];
			Temp2 += FH_ST.OnARTpre500_E[ia][is] * RelativeCSWentry[is+15];
			Temp2 += FH_ST.OnART500_E[ia][is] * RelativeCSWentry[is+20];
			Temp2 += FH_ST.OnART350_E[ia][is] * RelativeCSWentry[is+25];
			Temp2 += FH_ST.OnART200_E[ia][is] * RelativeCSWentry[is+30];
		}
		for(is=0; is<4; is++){
			Temp2 += FH_ST.StoppedART_E[ia][is] * RelativeCSWentry[is+35];}
		// Third calculate rate of entry into sex work for HIV-negative women
		BaseFSWentry[ia] = TotalFSW * FSWageDbn[ia] + Retiring[ia] - FH_SW.Total_E[ia];
		if(BaseFSWentry[ia] < 0.0 || Temp2 <= 0.0){
			BaseFSWentry[ia] = 0.0;}
		else{
			BaseFSWentry[ia] = BaseFSWentry[ia]/Temp2;}
		if(BaseFSWentry[ia]>1.0){BaseFSWentry[ia] = 1.0;}
		// Fourth calculate entrants and exits
		// HIV-neg, never tested
		NewFSW = FH_ST.NegNoHCT[ia] * BaseFSWentry[ia];
		LeaveFSW = FH_SW.NegNoHCT[ia] * MnthlyExit;
		FH_ST.NegNoHCT[ia] += LeaveFSW - NewFSW;
		FH_SW.NegNoHCT[ia] += NewFSW - LeaveFSW;
		// HIV-neg, previously tested
		NewFSW = FH_ST.NegPastHCT[ia] * BaseFSWentry[ia];
		LeaveFSW = FH_SW.NegPastHCT[ia] * MnthlyExit;
		FH_ST.NegPastHCT[ia] += LeaveFSW - NewFSW;
		FH_SW.NegPastHCT[ia] += NewFSW - LeaveFSW;
		if(PrEPorVM==1){
			// Regular HCT
			NewFSW = FH_ST.RegHCT[ia] * BaseFSWentry[ia];
			LeaveFSW = FH_SW.RegHCT[ia] * MnthlyExit;
			FH_ST.RegHCT[ia] += LeaveFSW - NewFSW;
			FH_SW.RegHCT[ia] += NewFSW - LeaveFSW;
			// Regular PrEP
			NewFSW = FH_ST.RegPrEP[ia] * BaseFSWentry[ia];
			LeaveFSW = FH_SW.RegPrEP[ia] * MnthlyExit;
			FH_ST.RegPrEP[ia] += LeaveFSW - NewFSW;
			FH_SW.RegPrEP[ia] += NewFSW - LeaveFSW;
			// Regular VM
			NewFSW = FH_ST.RegVM[ia] * BaseFSWentry[ia];
			LeaveFSW = FH_SW.RegVM[ia] * MnthlyExit;
			FH_ST.RegVM[ia] += LeaveFSW - NewFSW;
			FH_SW.RegVM[ia] += NewFSW - LeaveFSW;
		}
		for(is=0; is<5; is++){
			// Positive, never tested
			NewFSW = FH_ST.PosNoHCT[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is];
			LeaveFSW = FH_SW.PosNoHCT[ia][is] * MnthlyExit * RelativeCSWexit[is];
			FH_ST.PosNoHCT[ia][is] += LeaveFSW - NewFSW;
			FH_SW.PosNoHCT[ia][is] += NewFSW - LeaveFSW;
			// Positive, last tested when negative
			NewFSW = FH_ST.PosHCTpreHIV[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+5];
			LeaveFSW = FH_SW.PosHCTpreHIV[ia][is] * MnthlyExit  * RelativeCSWexit[is+5];
			FH_ST.PosHCTpreHIV[ia][is] += LeaveFSW - NewFSW;
			FH_SW.PosHCTpreHIV[ia][is] += NewFSW - LeaveFSW;
			// Diagnosed positive but not yet on ART
			NewFSW = FH_ST.PosDiagnosedPreART[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+10];
			LeaveFSW = FH_SW.PosDiagnosedPreART[ia][is] * MnthlyExit  * RelativeCSWexit[is+10];
			FH_ST.PosDiagnosedPreART[ia][is] += LeaveFSW - NewFSW;
			FH_SW.PosDiagnosedPreART[ia][is] += NewFSW - LeaveFSW;
			/*if(CurrYear>=2015 && is>0 && is<=ImmARTstage){
				TempART[is-1] = NewFSW * ImmART_CSW;
				FH_SW.PosDiagnosedPreART[ia][is] = FH_SW.PosDiagnosedPreART[ia][is] - TempART[is-1];
			}*/
			// Started ART at CD4 500+
			NewFSW = FH_ST.OnARTpre500[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+15];
			LeaveFSW = FH_SW.OnARTpre500[ia][is] * MnthlyExit  * RelativeCSWexit[is+15];
			FH_ST.OnARTpre500[ia][is] += LeaveFSW - NewFSW;
			FH_SW.OnARTpre500[ia][is] += NewFSW - LeaveFSW;
			// Started ART at CD4 350-499
			NewFSW = FH_ST.OnART500[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+20];
			LeaveFSW = FH_SW.OnART500[ia][is] * MnthlyExit  * RelativeCSWexit[is+20];
			FH_ST.OnART500[ia][is] += LeaveFSW - NewFSW;
			FH_SW.OnART500[ia][is] += NewFSW - LeaveFSW;
			// Started ART at CD4 200-349
			NewFSW = FH_ST.OnART350[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+25];
			LeaveFSW = FH_SW.OnART350[ia][is] * MnthlyExit  * RelativeCSWexit[is+25];
			FH_ST.OnART350[ia][is] += LeaveFSW - NewFSW;
			FH_SW.OnART350[ia][is] += NewFSW - LeaveFSW;
			// Started ART at CD4 <200
			NewFSW = FH_ST.OnART200[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+30];
			LeaveFSW = FH_SW.OnART200[ia][is] * MnthlyExit  * RelativeCSWexit[is+30];
			FH_ST.OnART200[ia][is] += LeaveFSW - NewFSW;
			FH_SW.OnART200[ia][is] += NewFSW - LeaveFSW;
		}
		/*if(CurrYear>=2015){
			FH_SW.OnARTpre500[ia][0] += TempART[0];
			FH_SW.OnART500[ia][0] += TempART[1];
			FH_SW.OnART350[ia][0] += TempART[2];
			FH_SW.OnART200[ia][0] += TempART[3];
		}*/
		// Stopped ART
		for(is=0; is<4; is++){
			NewFSW = FH_ST.StoppedART[ia][is] * BaseFSWentry[ia] * RelativeCSWentry[is+35];
			LeaveFSW = FH_SW.StoppedART[ia][is] * MnthlyExit  * RelativeCSWexit[is+35];
			FH_ST.StoppedART[ia][is] += LeaveFSW - NewFSW;
			FH_SW.StoppedART[ia][is] += NewFSW - LeaveFSW;
		}
	}
}

void UpdateMonthlyCum()
{
	int ia, ii, ij, is, ig, im;
	double DiagnosisRate[5], TempPaed, TempPaed2, TempPaed3, TempNeg[2], TempPos[2], Temp1stPos[2], FalseNeg[2];
	double Temp1, Temp2, Temp3;

	// ChildPIP calibration outputs
	if (FixedUncertainty == 1 || CalibChildPIP == 1){
		for (ia = 12; ia < 60; ia++){
			AIDSdeathsDiagP[0] += MaleChild.AIDSdeaths[ia] - MaleChild.AIDSdeathsUndiagnosed[ia] +
				FemChild.AIDSdeaths[ia] - FemChild.AIDSdeathsUndiagnosed[ia];
			AIDSdeathsART_P[0] += MaleChild.AIDSdeaths[ia] - MaleChild.AIDSdeathsUntreated[ia] +
				FemChild.AIDSdeaths[ia] - FemChild.AIDSdeathsUntreated[ia];
		}
		for (ia = 60; ia < 120; ia++){
			AIDSdeathsDiagP[1] += MaleChild.AIDSdeaths[ia] - MaleChild.AIDSdeathsUndiagnosed[ia] +
				FemChild.AIDSdeaths[ia] - FemChild.AIDSdeathsUndiagnosed[ia];
			AIDSdeathsART_P[1] += MaleChild.AIDSdeaths[ia] - MaleChild.AIDSdeathsUntreated[ia] +
				FemChild.AIDSdeaths[ia] - FemChild.AIDSdeathsUntreated[ia];
		}
	}

	// Flow variables that are not age-specific
	if (FixedUncertainty == 1){
		for (ia = 0; ia < 132; ia++){
			AIDSdeathsOnART += MaleChild.AIDSdeaths[ia] - MaleChild.AIDSdeathsUntreated[ia] +
				FemChild.AIDSdeaths[ia] - FemChild.AIDSdeathsUntreated[ia];
		}
		for (ia = 0; ia < 81; ia++){
			AIDSdeathsOnART += MHU_virgin.AIDSdeathsTreated[ia] + MHC_virgin.AIDSdeathsTreated[ia] +
				MHU_ST.AIDSdeathsTreated[ia] + MHC_ST.AIDSdeathsTreated[ia] + MHU_STM.AIDSdeathsTreated[ia] +
				MHC_STM.AIDSdeathsTreated[ia] + MHU_LTH.AIDSdeathsTreated[ia] +
				MHC_LTH.AIDSdeathsTreated[ia] + MHU_LTL.AIDSdeathsTreated[ia] + MHC_LTL.AIDSdeathsTreated[ia] +
				MLU_virgin.AIDSdeathsTreated[ia] + MLC_virgin.AIDSdeathsTreated[ia] + MLU_ST.AIDSdeathsTreated[ia] +
				MLC_ST.AIDSdeathsTreated[ia] + MLU_STM.AIDSdeathsTreated[ia] +
				MLC_STM.AIDSdeathsTreated[ia] + MLU_LTH.AIDSdeathsTreated[ia] + MLC_LTH.AIDSdeathsTreated[ia] +
				MLU_LTL.AIDSdeathsTreated[ia] + MLC_LTL.AIDSdeathsTreated[ia];
			AIDSdeathsOnART += FH_virgin.AIDSdeathsTreated[ia] + FH_ST.AIDSdeathsTreated[ia] +
				FH_SW.AIDSdeathsTreated[ia] + FH_LTH.AIDSdeathsTreated[ia] + FH_LTL.AIDSdeathsTreated[ia] +
				FL_virgin.AIDSdeathsTreated[ia] + FL_ST.AIDSdeathsTreated[ia] + FL_LTH.AIDSdeathsTreated[ia] +
				FL_LTL.AIDSdeathsTreated[ia];
		}
		NewPerinatal += MaleChild.PosChildAtBirthNoPMTCT_E[0] +
			MaleChild.PosChildAtBirthPMTCT_E[0] + MaleChild.DiagChildAtBirthNoPMTCT_E[0] +
			MaleChild.DiagChildAtBirthPMTCT_E[0] + FemChild.PosChildAtBirthNoPMTCT_E[0] +
			FemChild.PosChildAtBirthPMTCT_E[0] + FemChild.DiagChildAtBirthNoPMTCT_E[0] +
			FemChild.DiagChildAtBirthPMTCT_E[0] + MaleChild.OnARTearly_E[0] +
			FemChild.OnARTearly_E[0];
		for (ia = 0; ia < 36; ia++){
			NewPostnatal += MaleChild.NewHIVpostnatal[ia] + FemChild.NewHIVpostnatal[ia];
			NewHIVlactating += MaleChild.NewMatHIV[ia] + FemChild.NewMatHIV[ia];
		}
		for (ia = 0; ia < 18; ia++){
			NewPostnatal18 += MaleChild.NewPostnatalDiag[ia] + FemChild.NewPostnatalDiag[ia]; }
		BirthsNegMothers += ChildNegMotherNeg / 12.0;
		BirthsPosMothers += (ChildPosNoPMTCT + ChildPosPMTCT + ChildNegMotherPosKnown +
			ChildNegMotherPosUnknown) / 12.0;
		BirthsARTmothers += (ARTmothers + TestedPosOnART[0] + TestedPosOnART[1] + RecentPosDetART) / 12.0;
		TotBirthsARTconcep.out[CurrSim - 1][CurrYear - StartYear] += ARTmothers / 12.0;
		//for (ia = 0; ia < 36; ia++){ BirthsDiagMothers += BirthsByHIVstage[ia][7] / 12.0; }
	}
	for (is = 0; is < 4; is++){
		AdultsNewARTbyCD4[is][0] += MHU_virgin.NewARTbyCD4[is] + MHC_virgin.NewARTbyCD4[is] +
			MHU_ST.NewARTbyCD4[is] + MHC_ST.NewARTbyCD4[is] + MHU_STM.NewARTbyCD4[is] +
			MHC_STM.NewARTbyCD4[is] + MHU_LTH.NewARTbyCD4[is] +
			MHC_LTH.NewARTbyCD4[is] + MHU_LTL.NewARTbyCD4[is] + MHC_LTL.NewARTbyCD4[is] +
			MLU_virgin.NewARTbyCD4[is] + MLC_virgin.NewARTbyCD4[is] +
			MLU_ST.NewARTbyCD4[is] + MLC_ST.NewARTbyCD4[is] + MLU_STM.NewARTbyCD4[is] +
			MLC_STM.NewARTbyCD4[is] + MLU_LTH.NewARTbyCD4[is] +
			MLC_LTH.NewARTbyCD4[is] + MLU_LTL.NewARTbyCD4[is] + MLC_LTL.NewARTbyCD4[is];
		AdultsNewARTbyCD4[is][1] += FH_virgin.NewARTbyCD4[is] + FH_ST.NewARTbyCD4[is] +
			FH_SW.NewARTbyCD4[is] + FH_LTH.NewARTbyCD4[is] + FH_LTL.NewARTbyCD4[is] +
			FL_virgin.NewARTbyCD4[is] + FL_ST.NewARTbyCD4[is] + FL_LTH.NewARTbyCD4[is] +
			FL_LTL.NewARTbyCD4[is];
	}
	for (im = 0; im < 132; im++){
		ChildNewARTlate += MaleChild.StartingARTlate[im] + FemChild.StartingARTlate[im];
	}
	if (FixedUncertainty == 1 || CalibHCTprev == 1 || CalibHCTprevP == 1 || CalibHCTtotP == 1){
		for (ig = 0; ig < 2; ig++){
			TempNeg[ig] = 0.0;
			TempPos[ig] = 0.0;
			FalseNeg[ig] = 0.0;
			Temp1stPos[ig] = 0.0;
		}
		// Calculate NewlyTested at ages <10
		for (ia = 18; ia < 132; ia++){
			TempPaed = MaleChild.DiagChildAtBirthNoPMTCT[ia] + MaleChild.DiagChildAtBirthPMTCT[ia] +
				MaleChild.DiagChildAfterBirth[ia] + MaleChild.DiagARTeligible[ia] +
				FemChild.DiagChildAtBirthNoPMTCT[ia] + FemChild.DiagChildAtBirthPMTCT[ia] +
				FemChild.DiagChildAfterBirth[ia] + FemChild.DiagARTeligible[ia];
			TempPaed2 = MaleChild.PosChildAtBirthNoPMTCT[ia] + MaleChild.PosChildAtBirthPMTCT[ia] +
				MaleChild.PosChildAfterBirth[ia] + MaleChild.ARTeligible[ia] +
				FemChild.PosChildAtBirthNoPMTCT[ia] + FemChild.PosChildAtBirthPMTCT[ia] +
				FemChild.PosChildAfterBirth[ia] + FemChild.ARTeligible[ia];
			TempPaed3 = MaleChild.OnARTearly[ia] + MaleChild.OnARTlate1st3m[ia] +
				MaleChild.OnARTlateAfter3m[ia] + MaleChild.StoppedART[ia] +
				FemChild.OnARTearly[ia] + FemChild.OnARTlate1st3m[ia] +
				FemChild.OnARTlateAfter3m[ia] + FemChild.StoppedART[ia];
			NewlyTestedPos[2] += TempPaed2 * MnthlyTestingPaed[ia][0];
			if (ia > 18){
				NewlyTestedPos[2] += TempPaed * MnthlyTestingPaed[ia][0] * RetestPosP;
				NewlyTestedPos[2] += TempPaed3 * MnthlyTestingPaed[ia][0] * RetestPosP *
					RetestART / RetestPos;
				NewlyTestedPos[2] += (MaleChild.ARTeligible[ia] + FemChild.ARTeligible[ia])*
					(MnthlyTestingPaed[ia][1] - MnthlyTestingPaed[ia][0]);
			}
			if (ia == 18){
				NewlyTestedNeg[2] += (MaleChild.Total[ia] + FemChild.Total[ia] - TempPaed - TempPaed2 -
					TempPaed3) * CurrTesting18mo[0];
			}
			else{
				NewlyTestedNeg[2] += (MaleChild.Total[ia] + FemChild.Total[ia] - TempPaed - TempPaed2 -
					TempPaed3) * MnthlyTestingPaed[ia][0];
			}
			if (ia == 18){
				NewlyTestedPaed[0][0] += (MaleChild.Total[ia] + FemChild.Total[ia] - TempPaed - TempPaed2 -
					TempPaed3) * CurrTesting18mo[0];
				NewlyTestedPaed[0][1] += TempPaed2 * MnthlyTestingPaed[ia][0];
			}
			else if (ia < 60){
				NewlyTestedPaed[1][0] += (MaleChild.Total[ia] + FemChild.Total[ia] - TempPaed - TempPaed2 -
					TempPaed3) * MnthlyTestingPaed[ia][0];
				NewlyTestedPaed[1][1] += TempPaed2 * MnthlyTestingPaed[ia][0];
				NewlyTestedPaed[1][1] += TempPaed * MnthlyTestingPaed[ia][0] * RetestPosP;
				NewlyTestedPaed[1][1] += TempPaed3 * MnthlyTestingPaed[ia][0] * RetestART * (RetestPosP/RetestPos);
				NewlyTestedPaed[1][1] += (MaleChild.ARTeligible[ia] + FemChild.ARTeligible[ia])*
					(MnthlyTestingPaed[ia][1] - MnthlyTestingPaed[ia][0]);
			}
			else{
				NewlyTestedPaed[2][0] += (MaleChild.Total[ia] + FemChild.Total[ia] - TempPaed - TempPaed2 -
					TempPaed3) * MnthlyTestingPaed[ia][0];
				NewlyTestedPaed[2][1] += TempPaed2 * MnthlyTestingPaed[ia][0];
				NewlyTestedPaed[2][1] += TempPaed * MnthlyTestingPaed[ia][0] * RetestPosP;
				NewlyTestedPaed[2][1] += TempPaed3 * MnthlyTestingPaed[ia][0] * RetestART * (RetestPosP/RetestPos);
				NewlyTestedPaed[2][1] += (MaleChild.ARTeligible[ia] + FemChild.ARTeligible[ia])*
					(MnthlyTestingPaed[ia][1] - MnthlyTestingPaed[ia][0]);
			}
		}
		// Calculate NewlyTested at ages 10+ (excluding self-testing unless confirmed positive)
		for (ia = 0; ia < 81; ia++){
			TempNeg[0] += SumGroupsM[ia][0] * TestingRateSE[ia][0][0] + SumGroupsM[ia][1] *
				TestingRateSE[ia][1][0];
			TempNeg[1] += SumGroupsF[ia][0] * TestingRateSE[ia][0][1] + SumGroupsF[ia][1] *
				TestingRateSE[ia][1][1];
			if (PrEPorVM == 1){
				TempNeg[0] += SumGroupsM[ia][2] * FreqRegHCT[0] / 12.0 + SumGroupsM[ia][3] *
					FreqHCTinPrEP[0] / 12.0;
				TempNeg[1] += SumGroupsF[ia][2] * FreqRegHCT[1] / 12.0 + SumGroupsF[ia][3] *
					FreqHCTinPrEP[1] / 12.0 + SumGroupsF[ia][4] * FreqHCTinVM / 12.0;
			}
			for (is = 2; is < 12; is++){
				TempPos[0] += SumGroupsM[ia][is + 3] * TestingRateSE[ia][is][0];
				TempPos[1] += SumGroupsF[ia][is + 3] * TestingRateSE[ia][is][1];
			}
			for (is = 2; is < 12; is++){
				Temp1stPos[0] += SumGroupsM[ia][is + 3] * TestingRateSE[ia][is][0];
				Temp1stPos[1] += SumGroupsF[ia][is + 3] * TestingRateSE[ia][is][1];
			}
			for (is = 15; is < 20; is++){
				TempPos[0] += SumGroupsM[ia][is] * (TestingRateM[ia] * RetestPos +
					SelfTestingRate[ia][is - 8][0] * RetestPosST[0] * SelfTestConfirm);
				TempPos[1] += SumGroupsF[ia][is] * (TestingRateNPF[ia] * RetestPos +
					SelfTestingRate[ia][is - 8][1] * RetestPosST[0] * SelfTestConfirm);
			}
			for (is = 20; is < 44; is++){
				TempPos[0] += SumGroupsM[ia][is] * (TestingRateM[ia] * RetestART +
					SelfTestingRate[ia][8][0] * RetestPosST[1] * SelfTestConfirm);
				TempPos[1] += SumGroupsF[ia][is] * (TestingRateNPF[ia] * RetestART +
					SelfTestingRate[ia][8][1] * RetestPosST[1] * SelfTestConfirm);
			}
			FalseNeg[0] += SumGroupsM[ia][5] * TestingRateSE[ia][2][0] +
				SumGroupsM[ia][10] * TestingRateSE[ia][7][0];
			FalseNeg[1] += SumGroupsF[ia][5] * TestingRateSE[ia][2][1] +
				SumGroupsF[ia][10] * TestingRateSE[ia][7][1];
			if (ia < 20){
				// Adjustment for HIV-negative virgins
				TempNeg[0] = TempNeg[0] - SumGroupsVM[ia][0] * (TestingRateSE[ia][0][0] - TestingRateNegV[ia][0]) -
					SumGroupsVM[ia][1] * (TestingRateSE[ia][1][0] - TestingRateNegV[ia][0]);
				TempNeg[1] = TempNeg[1] - SumGroupsVF[ia][0] * (TestingRateSE[ia][0][1] - TestingRateNegV[ia][1]) -
					SumGroupsVF[ia][1] * (TestingRateSE[ia][1][1] - TestingRateNegV[ia][1]);
				// Subtract tests in HIV-pos virgins
				for (is = 2; is < 12; is++){
					TempPos[0] = TempPos[0] - SumGroupsVM[ia][is + 3] * TestingRateSE[ia][is][0];
					TempPos[1] = TempPos[1] - SumGroupsVF[ia][is + 3] * TestingRateSE[ia][is][1];
				}
				for (is = 2; is < 12; is++){
					Temp1stPos[0] = Temp1stPos[0] - SumGroupsVM[ia][is + 3] * TestingRateSE[ia][is][0];
					Temp1stPos[1] = Temp1stPos[1] - SumGroupsVF[ia][is + 3] * TestingRateSE[ia][is][1];
				}
				for (is = 15; is < 20; is++){
					TempPos[0] = TempPos[0] - SumGroupsVM[ia][is] * (TestingRateM[ia] * RetestPos +
						SelfTestingRate[ia][is - 8][0] * RetestPosST[0] * SelfTestConfirm);
					TempPos[1] = TempPos[1] - SumGroupsVF[ia][is] * (TestingRateNPF[ia] * RetestPos +
						SelfTestingRate[ia][is - 8][1] * RetestPosST[0] * SelfTestConfirm);
				}
				for (is = 20; is < 44; is++){
					TempPos[0] = TempPos[0] - SumGroupsVM[ia][is] * (TestingRateM[ia] * RetestART +
						SelfTestingRate[ia][8][0] * RetestPosST[1] * SelfTestConfirm);
					TempPos[1] = TempPos[1] - SumGroupsVF[ia][is] * (TestingRateNPF[ia] * RetestART +
						SelfTestingRate[ia][8][1] * RetestPosST[1] * SelfTestConfirm);
				}
				// Add back tests in HIV-pos virgins
				for (is = 0; is < 5; is++){
					TempPos[0] += (SumGroupsVM[ia][is + 5] + SumGroupsVM[ia][is + 10]) *
						TestingRateV[ia][is][0];
					TempPos[1] += (SumGroupsVF[ia][is + 5] + SumGroupsVF[ia][is + 10]) *
						TestingRateV[ia][is][1];
				}
				for (is = 0; is < 5; is++){
					Temp1stPos[0] += (SumGroupsVM[ia][is + 5] + SumGroupsVM[ia][is + 10]) *
						TestingRateV[ia][is][0];
					Temp1stPos[1] += (SumGroupsVF[ia][is + 5] + SumGroupsVF[ia][is + 10]) *
						TestingRateV[ia][is][1];
				}
				for (is = 15; is < 20; is++){
					if (ia < 5){
						TempPos[0] += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestPosP;
						TempPos[1] += SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestPosP;
					}
					else{
						TempPos[0] += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestPos;
						TempPos[1] += SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestPos;
					}
				}
				for (is = 20; is < 44; is++){
					TempPos[0] += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestPosP *
						RetestART / RetestPos;
					TempPos[1] += SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestPosP *
						RetestART / RetestPos;
				}
			}
			if (ia == 4){
				NewlyTestedPos[2] += TempPos[0] + TempPos[1] - FalseNeg[0] - FalseNeg[1];
				NewlyTestedNeg[2] += TempNeg[0] + TempNeg[1] + FalseNeg[0] + FalseNeg[1];
				NewlyTestedPaed[2][1] += TempPos[0] + TempPos[1] - FalseNeg[0] - FalseNeg[1];
				NewlyTestedPaed[2][0] += TempNeg[0] + TempNeg[1] + FalseNeg[0] + FalseNeg[1];
				NewlyTestedFalseNeg[2] += FalseNeg[0] + FalseNeg[1];
				for (ig = 0; ig < 2; ig++){
					TempPos[ig] = 0.0;
					TempNeg[ig] = 0.0;
					FalseNeg[ig] = 0.0;
					Temp1stPos[ig] = 0.0;
				}
			}
			if (ia == 14){
				Temp1 = TempNeg[0] + TempPos[0];
				Temp2 = TempNeg[1] + TempPos[1];
				NewlyTestedAdult[0][0] += Temp1;
				NewlyTestedAdult[0][1] += Temp2;
			}
			if (ia == 39){
				NewlyTestedAdult[1][0] += TempNeg[0] + TempPos[0] - Temp1;
				NewlyTestedAdult[1][1] += TempNeg[1] + TempPos[1] - Temp2;
				Temp1 = TempNeg[0] + TempPos[0];
				Temp2 = TempNeg[1] + TempPos[1];
			}
			if (ia == 80){
				for (ig = 0; ig < 2; ig++){
					NewlyTestedPos[ig] += TempPos[ig] - FalseNeg[ig];
					NewlyTestedNeg[ig] += TempNeg[ig] + FalseNeg[ig];
					NewlyTestedFalseNeg[ig] += FalseNeg[ig];
					NewlyTested1stPos[ig] += Temp1stPos[ig] - FalseNeg[ig];
				}
				NewlyTestedAdult[2][0] += TempNeg[0] + TempPos[0] - Temp1;
				NewlyTestedAdult[2][1] += TempNeg[1] + TempPos[1] - Temp2;
			}
		}
		// Calculate self-testing outputs
		if (FixedUncertainty == 1){ UpdateMonthlySTesting(); }
		// Calculate NewElig350
		for (ia = 5; ia < 81; ia++){
			NewElig350[0] += (SumGroupsM[ia][7] + SumGroupsM[ia][12] + SumGroupsM[ia][17]) *
				MnthlyCD4trans[ia][2][0];
			NewElig350[1] += (SumGroupsF[ia][7] + SumGroupsF[ia][12] + SumGroupsF[ia][17]) *
				MnthlyCD4trans[ia][2][1];
		}
		// Calculate NewElig500
		for (ia = 5; ia < 81; ia++){
			NewElig500[0] += (SumGroupsM[ia][6] + SumGroupsM[ia][11] + SumGroupsM[ia][16]) *
				MnthlyCD4trans[ia][1][0];
			NewElig500[1] += (SumGroupsF[ia][6] + SumGroupsF[ia][11] + SumGroupsF[ia][16]) *
				MnthlyCD4trans[ia][1][1];
		}
	}

	// Age-specific flows
	for (ia = 0; ia < 81; ia++){
		NewHIVbyAgeSex[ia][0] += MHU_ST.NewHIV[ia] + MHC_ST.NewHIV[ia] + MHU_STM.NewHIV[ia] +
			MHC_STM.NewHIV[ia] + MHU_LTH.NewHIV[ia] + MHC_LTH.NewHIV[ia] +
			MHU_LTL.NewHIV[ia] + MHC_LTL.NewHIV[ia] + MLU_ST.NewHIV[ia] +
			MLC_ST.NewHIV[ia] + MLU_STM.NewHIV[ia] + MLC_STM.NewHIV[ia] + MLU_LTH.NewHIV[ia] +
			MLC_LTH.NewHIV[ia] + MLU_LTL.NewHIV[ia] + MLC_LTL.NewHIV[ia];
		NewHIVbyAgeSex[ia][1] += FH_ST.NewHIV[ia] + FH_SW.NewHIV[ia] + FH_LTH.NewHIV[ia] +
			FH_LTL.NewHIV[ia] + FL_ST.NewHIV[ia] + FL_LTH.NewHIV[ia] + FL_LTL.NewHIV[ia];
	}
	Temp1 = 0.0;
	Temp2 = 0.0;
	for (ia = 0; ia<35; ia++){
		Temp1 += NewHIVbyAgeSex[ia + 5][1] * HIVnegSEfert[ia + 1] * 12.0 / (CurrMonth + 1.0);
		Temp2 += TotalSexuallyExp_S[ia + 15][1] * HIVnegSEfert[ia + 1] *
			(1.0 - TotalPositive_S[ia + 15][1] / TotalPop_S[ia + 15][1]);
	}
	MatIncidence = Temp1 / Temp2;
	if (FixedUncertainty == 1 && CurrMonth == 5){
		ANCincidence.out[CurrSim - 1][CurrYear - StartYear] = MatIncidence;
		Temp3 = 0.0;
		for (ia = 0; ia<35; ia++){
			Temp3 += NewHIVbyAgeSex[ia + 5][1] * HIVnegSEfert[ia + 1] * ((1.0 - exp(-HCTtoART_SE[ia + 5][6][1] * 12.0)) /
				(HCTtoART_SE[ia + 5][6][1] * 12.0) - exp(-HCTtoART_SE[ia + 5][6][1] * 12.0)) * 12.0 / (CurrMonth + 1.0);
		}
		ANCincidenceAdj.out[CurrSim - 1][CurrYear - StartYear] = (Temp1 - Temp3) / Temp2;
	}
	for (ia = 0; ia < 76; ia++){
		AIDSdeathsMarriedM[ia][0][0] += MHU_LTH.AIDSdeathsTreated[ia + 5] + MHC_LTH.AIDSdeathsTreated[ia + 5] +
			MHU_LTH.AIDSdeathsUntreated[ia + 5] + MHC_LTH.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedM[ia][0][1] += MHU_LTL.AIDSdeathsTreated[ia + 5] + MHC_LTL.AIDSdeathsTreated[ia + 5] +
			MHU_LTL.AIDSdeathsUntreated[ia + 5] + MHC_LTL.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedM[ia][1][0] += MLU_LTH.AIDSdeathsTreated[ia + 5] + MLC_LTH.AIDSdeathsTreated[ia + 5] +
			MLU_LTH.AIDSdeathsUntreated[ia + 5] + MLC_LTH.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedM[ia][1][1] += MLU_LTL.AIDSdeathsTreated[ia + 5] + MLC_LTL.AIDSdeathsTreated[ia + 5] +
			MLU_LTL.AIDSdeathsUntreated[ia + 5] + MLC_LTL.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedF[ia][0][0] += FH_LTH.AIDSdeathsTreated[ia + 5] + FH_LTH.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedF[ia][0][1] += FH_LTL.AIDSdeathsTreated[ia + 5] + FH_LTL.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedF[ia][1][0] += FL_LTH.AIDSdeathsTreated[ia + 5] + FL_LTH.AIDSdeathsUntreated[ia + 5];
		AIDSdeathsMarriedF[ia][1][1] += FL_LTL.AIDSdeathsTreated[ia + 5] + FL_LTL.AIDSdeathsUntreated[ia + 5];
	}
	for (ia = 0; ia < 81; ia++){
		// Note that for now we are only considering the unmarried deaths. In the final step
		// we will add the AIDS deaths in married individuals.
		AIDSdeathsByAge[ia][0] += MHU_virgin.AIDSdeathsTreated[ia] + MHU_virgin.AIDSdeathsUntreated[ia] +
			MHC_virgin.AIDSdeathsTreated[ia] + MHC_virgin.AIDSdeathsUntreated[ia] +
			MHU_ST.AIDSdeathsTreated[ia] + MHU_ST.AIDSdeathsUntreated[ia] + MHC_ST.AIDSdeathsTreated[ia] +
			MHC_ST.AIDSdeathsUntreated[ia] + MHU_STM.AIDSdeathsTreated[ia] + MHU_STM.AIDSdeathsUntreated[ia] +
			MHC_STM.AIDSdeathsTreated[ia] + MHC_STM.AIDSdeathsUntreated[ia] + MLU_virgin.AIDSdeathsTreated[ia] +
			MLU_virgin.AIDSdeathsUntreated[ia] + MLC_virgin.AIDSdeathsTreated[ia] + MLC_virgin.AIDSdeathsUntreated[ia] +
			MLU_ST.AIDSdeathsTreated[ia] + MLU_ST.AIDSdeathsUntreated[ia] + MLC_ST.AIDSdeathsTreated[ia] +
			MLC_ST.AIDSdeathsUntreated[ia] + MLU_STM.AIDSdeathsTreated[ia] + MLU_STM.AIDSdeathsUntreated[ia] +
			MLC_STM.AIDSdeathsTreated[ia] + MLC_STM.AIDSdeathsUntreated[ia];
		AIDSdeathsByAge[ia][1] += FH_virgin.AIDSdeathsTreated[ia] + FH_virgin.AIDSdeathsUntreated[ia] +
			FH_ST.AIDSdeathsTreated[ia] + FH_ST.AIDSdeathsUntreated[ia] + FH_SW.AIDSdeathsTreated[ia] +
			FH_SW.AIDSdeathsUntreated[ia] + FL_virgin.AIDSdeathsTreated[ia] + FL_virgin.AIDSdeathsUntreated[ia] +
			FL_ST.AIDSdeathsTreated[ia] + FL_ST.AIDSdeathsUntreated[ia];
	}
	if (FixedUncertainty == 1){
		for (ia = 0; ia < 36; ia++){
			TotBirthsByMatAge[ia] += BirthsByHIVstage[ia][0] / 12.0;}
	}
	if (CurrMonth == 11){
		for (ia = 0; ia < 76; ia++){
			AIDSdeathsByAge[ia + 5][0] += AIDSdeathsMarriedM[ia][0][0] + AIDSdeathsMarriedM[ia][0][1] +
				AIDSdeathsMarriedM[ia][1][0] + AIDSdeathsMarriedM[ia][1][1];
			AIDSdeathsByAge[ia + 5][1] += AIDSdeathsMarriedF[ia][0][0] + AIDSdeathsMarriedF[ia][0][1] +
				AIDSdeathsMarriedF[ia][1][0] + AIDSdeathsMarriedF[ia][1][1];
		}
	}
	if (CalibAIDStrend == 1 || CalibAIDSage == 1){
		for (is = 0; is < 5; is++){
			DiagnosisRate[is] = OIincidence[is] * OIsTested[CurrYear - 1985] *
				OItoTBtestingRatio / 12.0;
		}
		for (ia = 0; ia < 9; ia++){
			for (ii = 0; ii < 5; ii++){
				for (is = 1; is < 5; is++){
					NewOIdiagnoses[ia][0] += (SumGroupsM[ia * 5 + ii][5 + is] +
						SumGroupsM[ia * 5 + ii][10 + is]) * DiagnosisRate[is];
					NewOIdiagnoses[ia][1] += (SumGroupsF[ia * 5 + ii][5 + is] +
						SumGroupsF[ia * 5 + ii][10 + is]) * DiagnosisRate[is];
				}
			}
		}
		for (ii = 0; ii < 31; ii++){
			for (is = 1; is < 5; is++){
				NewOIdiagnoses[9][0] += (SumGroupsM[50 + ii][5 + is] +
					SumGroupsM[50 + ii][10 + is]) * DiagnosisRate[is];
				NewOIdiagnoses[9][1] += (SumGroupsF[50 + ii][5 + is] +
					SumGroupsF[50 + ii][10 + is]) * DiagnosisRate[is];
			}
		}
	}
	// AIDS deaths and non-AIDS deaths in children aged <=10
	if (FixedUncertainty == 1 || CalibDeathsP == 1 || CalibChildPIP == 1){
		for (ia = 0; ia <= 10; ia++){
			for (ii = 0; ii < 12; ii++){
				AIDSdeathsByAgeP[ia][0] += MaleChild.AIDSdeaths[ia * 12 + ii];
				AIDSdeathsByAgeP[ia][1] += FemChild.AIDSdeaths[ia * 12 + ii];
				NonAIDSdeathsP[ia][0] += MaleChild.NonAIDSdeaths[ia * 12 + ii];
				NonAIDSdeathsP[ia][1] += FemChild.NonAIDSdeaths[ia * 12 + ii];
			}
		}
	}

	if (FixedUncertainty == 1 || CalibARTtotals == 1 || CalibARTtotalsP == 1 || CalibARTbyAgeP == 1){
		for (ia = 0; ia < 4; ia++){
			AdultsNewARTbyAge[ia][0] += MHU_virgin.NewARTbyAge[ia] + MHC_virgin.NewARTbyAge[ia] +
				MHU_ST.NewARTbyAge[ia] + MHC_ST.NewARTbyAge[ia] + MHU_STM.NewARTbyAge[ia] +
				MHC_STM.NewARTbyAge[ia] + MHU_LTH.NewARTbyAge[ia] + MHC_LTH.NewARTbyAge[ia] +
				MHU_LTL.NewARTbyAge[ia] + MHC_LTL.NewARTbyAge[ia] + MLU_virgin.NewARTbyAge[ia] +
				MLC_virgin.NewARTbyAge[ia] + MLU_ST.NewARTbyAge[ia] + MLC_ST.NewARTbyAge[ia] +
				MLU_STM.NewARTbyAge[ia] + MLC_STM.NewARTbyAge[ia] + MLU_LTH.NewARTbyAge[ia] +
				MLC_LTH.NewARTbyAge[ia] + MLU_LTL.NewARTbyAge[ia] + MLC_LTL.NewARTbyAge[ia];
			AdultsNewARTbyAge[ia][1] += FH_virgin.NewARTbyAge[ia] + FH_ST.NewARTbyAge[ia] +
				FH_SW.NewARTbyAge[ia] + FH_LTH.NewARTbyAge[ia] + FH_LTL.NewARTbyAge[ia] +
				FL_virgin.NewARTbyAge[ia] + FL_ST.NewARTbyAge[ia] + FL_LTH.NewARTbyAge[ia] +
				FL_LTL.NewARTbyAge[ia];
		}
		for (ia = 0; ia < 11; ia++){
			for (ii = 0; ii < 12; ii++){
				PaedNewARTbyAge[ia] += MaleChild.StartingART[ia * 12 + ii] + FemChild.StartingART[ia * 12 + ii];
				PaedNewARTlate[ia] += MaleChild.StartingARTlate[ia * 12 + ii] + FemChild.StartingARTlate[ia * 12 + ii];
			}
		}
	}
	if (CalibARTtotals == 1 || CalibARTtotalsP == 1){
		for (ii = 0; ii<ARTdataPoints; ii++){
			if (ARTtotals[ii][0] == CurrYear && ARTtotals[ii][1] == CurrMonth){
				ARTmodelled[ii][1] = CumART15M.out[CurrSim - 1][CurrYear - StartYear - 1] +
					CumART15F.out[CurrSim - 1][CurrYear - StartYear - 1] + AdultsNewARTbyAge[0][0] +
					AdultsNewARTbyAge[1][0] + AdultsNewARTbyAge[2][0] + AdultsNewARTbyAge[3][0] +
					AdultsNewARTbyAge[0][1] + AdultsNewARTbyAge[1][1] + AdultsNewARTbyAge[2][1] +
					AdultsNewARTbyAge[3][1];
				ARTmodelled[ii][1] += CumARTunder15.out[CurrSim - 1][CurrYear - StartYear - 1];
				for (ia = 0; ia < 15; ia++){ ARTmodelled[ii][1] += PaedNewARTbyAge[ia]; }
			}
			if (ARTtotals[ii][0] > CurrYear){ break; }
		}
		for (ii = 0; ii<ARTdataPointsP; ii++){
			if (ARTtotalsP[ii][0] == CurrYear && ARTtotalsP[ii][1] == CurrMonth){
				ARTmodelledP[ii][1] = CumARTunder15.out[CurrSim - 1][CurrYear - StartYear - 1];
				for (ia = 0; ia < 15; ia++){ ARTmodelledP[ii][1] += PaedNewARTbyAge[ia]; }
			}
			if (ARTtotalsP[ii][0] > CurrYear){ break; }
		}
	}
	if ((FixedUncertainty == 1 || CalibARTtotals == 1 || CalibARTtotalsP == 1) && CurrMonth == 11 &&
		CurrYear > StartYear){
		CumART15M.out[CurrSim - 1][CurrYear - StartYear] =
			CumART15M.out[CurrSim - 1][CurrYear - StartYear - 1] + AdultsNewARTbyAge[0][0] +
			AdultsNewARTbyAge[1][0] + AdultsNewARTbyAge[2][0] + AdultsNewARTbyAge[3][0];
		CumART15F.out[CurrSim - 1][CurrYear - StartYear] =
			CumART15F.out[CurrSim - 1][CurrYear - StartYear - 1] + AdultsNewARTbyAge[0][1] +
			AdultsNewARTbyAge[1][1] + AdultsNewARTbyAge[2][1] + AdultsNewARTbyAge[3][1];
		TempPaed = 0.0;
		for (ia = 0; ia < 15; ia++){ TempPaed += PaedNewARTbyAge[ia]; }
		CumARTunder15.out[CurrSim - 1][CurrYear - StartYear] =
			CumARTunder15.out[CurrSim - 1][CurrYear - StartYear - 1] + TempPaed;
		CumARTtot.out[CurrSim - 1][CurrYear - StartYear] = CumART15M.out[CurrSim - 1][CurrYear - StartYear] +
			CumART15F.out[CurrSim - 1][CurrYear - StartYear] + CumARTunder15.out[CurrSim - 1][CurrYear - StartYear];
	}
}

void UpdateMonthlySTesting()
{
	int ia, ii, is, ig, im;
	double TempNeg[6], TempPos[6], TempART[6], Temp1, Temp2, Linkage[2];

	Linkage[0] = SelfTestConfirm * AsymStart[0];
	Linkage[1] = SelfTestConfirm * AsymStart[1];
	for (im = 0; im < 6; im++){
		TempNeg[im] = 0.0;
		TempPos[im] = 0.0;
		TempART[im] = 0.0;
	}
	for (ia = 0; ia<81; ia++){
		for (ii = 0; ii<44; ii++){
			Temp1 = SumGroupsM[ia][ii];
			Temp2 = SumGroupsF[ia][ii];
			if (ia < 20){
				Temp1 = Temp1 - SumGroupsVM[ia][ii];
				Temp2 = Temp2 - SumGroupsVF[ia][ii];
			}
			for (im = 0; im<6; im++){
				if (ii < 2){
					TempNeg[im] += Temp1 * SelfTestingRateM[ia][ii][0][im] + Temp2 * SelfTestingRateM[ia][ii][1][im];
				}
				if (ii == 5 || ii == 10){
					TempNeg[im] += Temp1 * SelfTestingRateM[ia][ii - 3][0][im] + Temp2 * SelfTestingRateM[ia][ii - 3][1][im];
				}
				if (ii > 5 && ii <15 && ii != 10){
					TempPos[im] += Temp1 * SelfTestingRateM[ia][ii - 3][0][im] + Temp2 * SelfTestingRateM[ia][ii - 3][1][im];
					TempART[im] += Temp1 * SelfTestingRateM[ia][ii - 3][0][im] * Linkage[0] +
						Temp2 * SelfTestingRateM[ia][ii - 3][1][im] * Linkage[1];
				}
				if (ii > 15 && ii < 20){
					TempPos[im] += (Temp1 * SelfTestingRateM[ia][ii - 13][0][im] + Temp2 * SelfTestingRateM[ia][ii - 13][1][im]) *
						RetestPosST[0];
				}
				if (ii >= 20 && ii < 44){
					TempPos[im] += (Temp1 * SelfTestingRateM[ia][3][0][im] + Temp2 * SelfTestingRateM[ia][3][1][im]) *
						RetestPosST[1];
				}
			}
		}
	}

	for (im = 0; im < 6; im++){
		NewlyTestedNegST[im] += TempNeg[im];
		NewlyTestedPosST[im] += TempPos[im];
		NewSTtoART[im] += TempART[im];
	}
}

void OneMonth(int im)
{
	CurrMonth = im;

	// Although the Calculate command in the VBA code comes at the end of the month, there is also
	// a Calculate command before the first OneMonth function is called. This means that we have
	// to update all relevant parameters at the START of the month.
	GetCurrBehavDbnM();
	GetCurrBehavDbnF();
	if (FixedUncertainty == 1 && CurrMonth == 0){ CalcMultPartners(); }
	UpdatePop();
	UpdateFSWdemand();
	/*UpdateAgePrefsF();
	UpdatePartnerAcqM();
	UpdateMixingST();
	UpdateARTmort();
	UpdateCondomUse();*/
	UpdateFert();
	UpdateBirths();
	GetBirthsByHIV();
	if(PrEPorVM==1){
		UpdatePrEPandVM();}
	//UpdateTestingRates();
	if (CurrYear == 2019 && CurrMonth == 9 && InputARTinitiationRates == 1){ CalcCOVIDimpactART(); }
	SetARTinitiation();
	if (im==0){
		UpdateTestingToART();
		CalcInterruptions();
		SetFertByStage();
		SetActivityByStage();
	}

	// Handle operations in child sheets
	SetMnthlyPaedParameters();
	MaleChild.GetEndProfile();
	FemChild.GetEndProfile();
	MaleChild.UpdateStartProfile();
	FemChild.UpdateStartProfile();

	// Handle operations in adult risk group sheets
	UpdateTransmProbs();
	CalcHIVtransitions();
	UpdateMonthlyCum();
	CopyEndToStart();

	// Update entry into and exit from sex work
	UpdateFSW();
	FH_SW.UpdateStartTotal();
	FH_ST.UpdateStartTotal();

	// Update sexual debut
	if (im < 11){
		// Calcs are also in the OneYear function, so im<11 condition prevents repetition
		UpdateDebut(&MHU_virgin, &MHU_ST, &MHU_STM, 0);
		UpdateDebut(&MHC_virgin, &MHC_ST, &MHC_STM, 0);
		UpdateDebut(&MLU_virgin, &MLU_ST, &MLU_STM, 0);
		UpdateDebut(&MLC_virgin, &MLC_ST, &MLC_STM, 0);
		UpdateDebut(&FH_virgin, &FH_ST, &FH_ST, 0);
		UpdateDebut(&FL_virgin, &FL_ST, &FL_ST, 0);
		UpdateStartTot();
	}

	// Calculate command is called at the END of the month, so at this point you should
	// call the functions for doing the monthly parameter updates.
	// Note that the same functions have to be called at the START of the year, so there is
	// some redundancy, and some of this redundancy we will remove in the revised model.
}

void ResetMonthlyCum()
{
	int ia, ig, ii, ij, is;

	AIDSdeathsOnART = 0.0;
	AIDSdeathsDiagP[0] = 0.0;
	AIDSdeathsDiagP[1] = 0.0;
	AIDSdeathsART_P[0] = 0.0;
	AIDSdeathsART_P[1] = 0.0;
	AIDSdeathsYOB[0] = 0.0;
	AIDSdeathsYOB[1] = 0.0;
	NewPerinatal = 0.0;
	NewPostnatal = 0.0;
	NewPostnatal18 = 0.0;
	NewHIVlactating = 0.0;
	BirthsNegMothers = 0.0;
	BirthsPosMothers = 0.0;
	BirthsARTmothers = 0.0;
	BirthsDiagMothers = 0.0;
	TotBirthsARTconcep.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
	VertTransmKnownPos.out[CurrSim - 1][CurrYear - StartYear] = 0.0;

	for(is=0; is<4; is++){
		AdultsNewARTbyCD4[is][0] = 0.0;
		AdultsNewARTbyCD4[is][1] = 0.0;
	}
	ChildNewARTlate = 0.0;
	if(FixedUncertainty==1 || CalibHCTprev==1 || CalibHCTprevP == 1 || CalibHCTtotP == 1){
		NewHIVinClients = 0.0;
		NewHIVinFSW.out[CurrSim-1][CurrYear-1985] = 0.0;
		HIVincMSM.out[CurrSim - 1][CurrYear - 1985] = 0.0;
		NewElig350[0] = 0.0;
		NewElig350[1] = 0.0;
		NewElig500[0] = 0.0;
		NewElig500[1] = 0.0;
		NewlyTestedNeg[0] = 0.0;
		NewlyTestedNeg[1] = 0.0;
		NewlyTestedNeg[2] = 0.0;
		NewlyTestedPos[0] = 0.0;
		NewlyTestedPos[1] = 0.0;
		NewlyTestedPos[2] = 0.0;
		NewlyTestedFalseNeg[0] = 0.0;
		NewlyTestedFalseNeg[1] = 0.0;
		NewlyTestedFalseNeg[2] = 0.0;
		NewlyTested1stPos[0] = 0.0;
		NewlyTested1stPos[1] = 0.0;
		for (ia = 0; ia < 3; ia++){
			for (ii = 0; ii < 2; ii++){
				NewlyTestedPaed[ia][ii] = 0.0;
				NewlyTestedAdult[ia][ii] = 0.0;
			}
		}
		for (ii = 0; ii < 6; ii++){
			NewlyTestedNegST[ii] = 0.0;
			NewlyTestedPosST[ii] = 0.0;
			NewSTtoART[ii] = 0.0;
		}
	}

	// Age-specific flows
	for(ia=0; ia<81; ia++){
		NewHIVbyAgeSex[ia][0] = 0.0;
		NewHIVbyAgeSex[ia][1] = 0.0;
	}
	if(FixedUncertainty==1 || CalibDeathsP==1 || CalibChildPIP==1){
		for(ia=0; ia<=10; ia++){
			NonAIDSdeathsP[ia][0] = 0.0;
			NonAIDSdeathsP[ia][1] = 0.0;
			AIDSdeathsByAgeP[ia][0] = 0.0;
			AIDSdeathsByAgeP[ia][1] = 0.0;
		}
	}
	for(ia=0; ia<81; ia++){
		for(ig=0; ig<2; ig++){
			AIDSdeathsByAge[ia][ig] = 0.0;}
	}
	if (FixedUncertainty == 1){
		for (ia = 0; ia < 36; ia++){
			TotBirthsByMatAge[ia] = 0.0;}
	}
	for(ia=0; ia<76; ia++){
		for(ii=0; ii<2; ii++){
			for(ij=0; ij<2; ij++){
				AIDSdeathsMarriedM[ia][ii][ij] = 0.0;
				AIDSdeathsMarriedF[ia][ii][ij] = 0.0;
			}
		}
	}
	if(CalibAIDStrend==1 || CalibAIDSage==1){
		for(ia=0; ia<10; ia++){
			for(ig=0; ig<2; ig++){
				NewOIdiagnoses[ia][ig] = 0.0;}
		}
	}

	for(ia=0; ia<4; ia++){
		AdultsNewARTbyAge[ia][0] = 0.0;
		AdultsNewARTbyAge[ia][1] = 0.0;
	}
	for(ia=0; ia<15; ia++){
		PaedNewARTbyAge[ia] = 0.0;}
	for (ia = 0; ia<11; ia++){
		PaedNewARTlate[ia] = 0.0;}

	// Cumulative ART totals
	if (FixedUncertainty == 1 || CalibARTtotals == 1 || CalibARTtotalsP == 1){
		if (CurrYear == StartYear){
			CumART15M.out[CurrSim - 1][0] = 0.0;
			CumART15F.out[CurrSim - 1][0] = 0.0;
			CumARTunder15.out[CurrSim - 1][0] = 0.0;
		}
		else{
			CumART15M.out[CurrSim - 1][CurrYear - StartYear] =
				CumART15M.out[CurrSim - 1][CurrYear - StartYear - 1];
			CumART15F.out[CurrSim - 1][CurrYear - StartYear] =
				CumART15F.out[CurrSim - 1][CurrYear - StartYear - 1];
			CumARTunder15.out[CurrSim - 1][CurrYear - StartYear] =
				CumARTunder15.out[CurrSim - 1][CurrYear - StartYear - 1];
		}
	}

    // New Diagnoses amongst pregnant women
    if (FixedUncertainty == 1){
     NewDiagnosesPregnancy.out[CurrSim - 1][CurrYear - StartYear] = 0.0; }
    // Readiagnoses amongst pregnant women
    if (FixedUncertainty == 1){
        RediagnosesPregnancy.out[CurrSim -1][CurrYear - StartYear] = 0.0;
    }
    // Total ANC tests
    if (FixedUncertainty == 1){
        TotANCtests.out[CurrSim -1][CurrYear - StartYear] = 0.0;
    }
	// AIDS deaths by stage
	AIDSdeathsUndiag.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
	AIDSdeathsDiagPreART.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
	AIDSdeaths1st6moART.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
	AIDSdeathsAfter6moART.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
}

void ResultsAtStartOfYr()
{
	int ia, ig, ii, is, iy, id, StartAge, EndAge, AgeLength;
	double Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, TempCD4[4], TempVL[4], TempVL2[4];

	iy = CurrYear - StartYear;

	for(ia=0; ia<91; ia++){
		for(ig=0; ig<2; ig++){
			TotalPop_S[ia][ig] = TotalPop[ia][ig];
			TotalPositive_S[ia][ig] = TotalPositive[ia][ig];
			TotalDiagnosed_S[ia][ig] = TotalDiagnosed[ia][ig];
			TotalART_S[ia][ig] = TotalART[ia][ig];
			TotalNaiveElig_S[ia][ig] = TotalNaiveElig[ia][ig];
			TotalInterrupt_S[ia][ig] = TotalInterrupt[ia][ig];
			TotalSexuallyExp_S[ia][ig] = TotalSexuallyExp[ia][ig];
			TotalMarried_S[ia][ig] = TotalMarried[ia][ig];
		}
	}

	if (FixedUncertainty == 1){
		// Store age-specific outputs
		for (ia = 0; ia < 91; ia++){
			MalePopAS.out[ia][iy] += TotalPop[ia][0];
			FemPopAS.out[ia][iy] += TotalPop[ia][1];
			MalePrevAS.out[ia][iy] += TotalPositive[ia][0] / TotalPop[ia][0];
			FemPrevAS.out[ia][iy] += TotalPositive[ia][1] / TotalPop[ia][1];
			MaleDiagAS.out[ia][iy] += TotalDiagnosed[ia][0];
			FemDiagAS.out[ia][iy] += TotalDiagnosed[ia][1];
			MaleART_AS.out[ia][iy] += TotalART[ia][0];
			FemART_AS.out[ia][iy] += TotalART[ia][1];
		}
		// Age-specific paediatric ART cascade outputs
		if (CurrYear == 2018){
			for (ia = 0; ia < 15; ia++){
				PaedCascade2018.out[CurrSim - 1][ia] = TotalPositive[ia][0] + TotalPositive[ia][1];
				PaedCascade2018.out[CurrSim - 1][ia + 15] = TotalDiagnosed[ia][0] + TotalDiagnosed[ia][1];
				PaedCascade2018.out[CurrSim - 1][ia + 30] = TotalART[ia][0] + TotalART[ia][1];
			}
		}
		// Non-AIDS mortality in HIV-positive adults
		// Note that this is an approximation (only considering non-HIV deaths among people who are
		// HIV-positive at the START of the year, ignoring competing AIDS mortality risk).
		Temp1 = 0.0;
		for (ia = 0; ia < 91; ia++){
			for (ig = 0; ig < 2; ig++){
				Temp1 += TotalPositive[ia][ig] * CurrNonAIDSmortALB[ia][ig];}
		}
		NonAIDSdeathsHIVpos.out[CurrSim - 1][iy] = Temp1;
	}

	if (FixedUncertainty == 1 || (FixedUncertainty == 0 && ProvModel == 1)){
		// HIV prevalence 0-14
		Temp3 = 0.0;
		Temp4 = 0.0;
		for (ia = 0; ia < 15; ia++){
			Temp3 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
			Temp4 += TotalPop_S[ia][0] + TotalPop_S[ia][1];
			if (ia == 4){
				NegChildrenU15.out[CurrSim - 1][iy] = Temp4 - Temp3;
			}
		}
		Prev0to14.out[CurrSim - 1][iy] = 1.0 * Temp3 / Temp4;
		TotPaedHIV.out[CurrSim - 1][iy] = Temp3;
		Prev2to14.out[CurrSim - 1][iy] = (Temp3 - TotalPositive_S[0][0] -
			TotalPositive_S[1][0] - TotalPositive_S[0][1] - TotalPositive_S[1][1] ) /
			(Temp4 - TotalPop_S[0][0] - TotalPop_S[1][0] - TotalPop_S[0][1] -
			TotalPop_S[1][1]);

		// HIV prevalence 15-24
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 15; ia < 25; ia++){
			Temp1 += TotalPop_S[ia][0] - TotalPositive_S[ia][0];
			Temp2 += TotalPop_S[ia][1] - TotalPositive_S[ia][1];
		}
		Neg15to24.out[CurrSim - 1][iy] = Temp1 + Temp2;
		Neg15to24M.out[CurrSim - 1][iy] = Temp1;
		Neg15to24F.out[CurrSim - 1][iy] = Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 15; ia < 25; ia++){
			Temp1 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
			Temp2 += TotalPop_S[ia][0] + TotalPop_S[ia][1];
		}
		Prev15to24.out[CurrSim - 1][iy] = Temp1 / Temp2;
		Total15to24.out[CurrSim - 1][iy] = Temp2;
		TotHIV15to24.out[CurrSim - 1][iy] = Temp1;
		Temp5 = Temp1;
		Temp6 = Temp2;

		// HIV prevalence 15-49
		for (ia = 25; ia < 50; ia++){
			Temp1 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
			Temp2 += TotalPop_S[ia][0] + TotalPop_S[ia][1];
		}
		HIVprev15to49[iy] = Temp1 / Temp2;
		TotHIV15to49.out[CurrSim - 1][iy] = Temp1;
		Total15to49.out[CurrSim - 1][iy] = Temp2;
		Prev15to49.out[CurrSim - 1][iy] = HIVprev15to49[iy];
		Neg15to49.out[CurrSim - 1][iy] = Temp2 - Temp1;

		// Total HIV and total population, prevalence in 25+
		for (ia = 50; ia < 91; ia++){
			Temp1 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
			Temp2 += TotalPop_S[ia][0] + TotalPop_S[ia][1];
		}
		TotalHIV.out[CurrSim - 1][iy] = Temp1 + Temp3;
		TotHIV50plus.out[CurrSim - 1][iy] = Temp1 - TotHIV15to49.out[CurrSim - 1][iy];
		TotPop.out[CurrSim - 1][iy] = Temp2 + Temp4;
		Prev25plus.out[CurrSim - 1][iy] = (Temp1 - Temp5) / (Temp2 - Temp6);
		HIVprevalence.out[CurrSim - 1][iy] = (Temp1 + Temp3) / (Temp2 + Temp4);
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		Temp4 = 0.0;
		for (ia = 50; ia < 91; ia++){
			Temp1 += TotalPositive_S[ia][0];
			Temp2 += TotalPop_S[ia][0];
			Temp3 += TotalPositive_S[ia][1];
			Temp4 += TotalPop_S[ia][1];
		}
		Prev50plus.out[CurrSim - 1][iy] = (Temp1 + Temp3) / (Temp2 + Temp4);
		Prev50plusM.out[CurrSim - 1][iy] = Temp1 / Temp2;
		Prev50plusF.out[CurrSim - 1][iy] = Temp3 / Temp4;
		Total50plus.out[CurrSim - 1][iy] = Temp2 + Temp4;
		Total50plusM.out[CurrSim - 1][iy] = Temp2;
		Total50plusF.out[CurrSim - 1][iy] = Temp4;
		TotHIV50plusM.out[CurrSim - 1][iy] = Temp1;
		TotHIV50plusF.out[CurrSim - 1][iy] = Temp3;

		// Negative at ages 25-49 and 50+
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 25; ia < 50; ia++){
			Temp1 += TotalPop_S[ia][0] - TotalPositive_S[ia][0];
			Temp2 += TotalPop_S[ia][1] - TotalPositive_S[ia][1];
		}
		Neg25to49M.out[CurrSim - 1][iy] = Temp1;
		Neg25to49F.out[CurrSim - 1][iy] = Temp2;
		Neg15to49M.out[CurrSim - 1][iy] = Neg15to24M.out[CurrSim - 1][iy] + Temp1;
		Neg15to49F.out[CurrSim - 1][iy] = Neg15to24F.out[CurrSim - 1][iy] + Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 50; ia < 91; ia++){
			Temp1 += TotalPop_S[ia][0] - TotalPositive_S[ia][0];
			Temp2 += TotalPop_S[ia][1] - TotalPositive_S[ia][1];
		}
		Neg50.out[CurrSim - 1][iy] = Temp1 + Temp2;
		Neg50M.out[CurrSim - 1][iy] = Temp1;
		Neg50F.out[CurrSim - 1][iy] = Temp2;
	}

	// Fraction of HIV-positive women infected in last year
	if (CalibANCprev == 1){
		for (ia = 0; ia < 6; ia++){
			Temp1 = 0.0;
			for (ii = 0; ii < 5; ii++){
				Temp1 += TotalPositive_S[15 + ia * 5 + ii][1];}
			if (CurrYear > StartYear){
				FractionRecentF[ia][iy] *= 1.0 / Temp1;}
			else{ FractionRecentF[ia][iy] = 1.0; }
		}
	}

	// HIV prevalence in children
	if (CalibPaedPrev == 1){
		if (CurrYear == 2005 || CurrYear == 2008 || CurrYear == 2012 || CurrYear == 2017){
			for (ia = 0; ia < 3; ia++){
				for (ig = 0; ig < 2; ig++){
					Temp1 = 0.0;
					Temp2 = 0.0;
					if (ia == 0 && CurrYear < 2012){ StartAge = 2; }
					else{ StartAge = ia * 5; }
					if (ia == 1 && CurrYear >= 2012){ EndAge = 15; }
					else{ EndAge = (ia + 1) * 5; }
					for (ii = StartAge; ii < EndAge; ii++){
						Temp1 += TotalPop_S[ii][ig];
						Temp2 += TotalPositive_S[ii][ig];
					}
					if (CurrYear == 2005){ ModelPrevPaed[ia][ig][0] = Temp2 / Temp1; }
					if (CurrYear == 2008){ ModelPrevPaed[ia][ig][1] = Temp2 / Temp1; }
					if (CurrYear == 2012 && ia<2){ ModelPrevPaed2[ia][ig][0] = Temp2 / Temp1; }
					if (CurrYear == 2017 && ia<2){ ModelPrevPaed2[ia][ig][1] = Temp2 / Temp1; }
				}
			}
		}
		if (CurrYear == 2008){
			ModelPrevU208 = (TotalPositive_S[0][0] + TotalPositive_S[0][1] +
				TotalPositive_S[1][0] + TotalPositive_S[1][1]) / (TotalPop_S[0][0] +
				TotalPop_S[0][1] + TotalPop_S[1][0] + TotalPop_S[1][1]);
		}
	}

	// HIV prevalence in adults by sex and 5-year age group
	for(ia=0; ia<10; ia++){
		StartAge = 15 + (ia * 5);
		for(ig=0; ig<2; ig++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			Temp3 = 0.0;
			for(ii=StartAge; ii<StartAge+5; ii++){
				Temp1 += TotalPop_S[ii][ig];
				Temp2 += TotalPositive_S[ii][ig];
				Temp3 += TotalDiagnosed_S[ii][ig];
			}
			AdultHHprev[iy][ia][ig] = (Temp2 - Temp3 * (1.0 -
				RRtestingDiagnosed))/(Temp1 - Temp3 * (1.0 - RRtestingDiagnosed));
		}
	}
	if(FixedUncertainty==1){
		if(CurrYear==2002){
			for(ia=0; ia<8; ia++){
				HSRCcalib2002.out[CurrSim-1][ia] = AdultHHprev[iy][ia][0];
				HSRCcalib2002.out[CurrSim-1][ia+8] = AdultHHprev[iy][ia][1];
			}
		}
		if(CurrYear==2005){
			for(ia=0; ia<9; ia++){
				HSRCcalib2005.out[CurrSim-1][ia] = AdultHHprev[iy][ia][0];
				HSRCcalib2005.out[CurrSim-1][ia+9] = AdultHHprev[iy][ia][1];
			}
		}
		if(CurrYear==2008){
			for(ia=0; ia<9; ia++){
				HSRCcalib2008.out[CurrSim-1][ia] = AdultHHprev[iy][ia][0];
				HSRCcalib2008.out[CurrSim-1][ia+9] = AdultHHprev[iy][ia][1];
			}
		}
		if(CurrYear==2012){
			for(ia=0; ia<9; ia++){
				HSRCcalib2012.out[CurrSim-1][ia] = AdultHHprev[iy][ia][0];
				HSRCcalib2012.out[CurrSim-1][ia+9] = AdultHHprev[iy][ia][1];
			}
		}
		if (CurrYear == 2016){
			for (ia = 0; ia<9; ia++){
				DHScalib2016.out[CurrSim - 1][ia] = AdultHHprev[iy][ia][0];
				DHScalib2016.out[CurrSim - 1][ia + 9] = AdultHHprev[iy][ia][1];
			}
		}
		if (CurrYear == 2017){
			for (ia = 0; ia<9; ia++){
				HSRCcalib2017.out[CurrSim - 1][ia] = AdultHHprev[iy][ia][0];
				HSRCcalib2017.out[CurrSim - 1][ia + 9] = AdultHHprev[iy][ia][1];
			}
		}
	}

	// HIV prevalence statistics for provincial model calibration
	if (ProvModel == 1 && (iy == 20 || iy == 23 || iy == 27 || iy == 31 || iy == 32)){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ii = 2; ii<15; ii++){
			Temp1 += TotalPop_S[ii][0] + TotalPop_S[ii][1];
			Temp2 += TotalPositive_S[ii][0] + TotalPositive_S[ii][1];
		}
		Temp2 = Temp2 / Temp1;
		if (iy == 20){ ModelProvHH_P[0] = Temp2; }
		if (iy == 23){ ModelProvHH_P[1] = Temp2; }
		if (iy == 27){ ModelProvHH_P[2] = Temp2; }
		if (iy == 32){ ModelProvHH_P[3] = Temp2; }
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ii = 15; ii<25; ii++){
			Temp1 += TotalPop_S[ii][0] + TotalPop_S[ii][1];
			Temp2 += TotalPositive_S[ii][0] + TotalPositive_S[ii][1];
			Temp3 += TotalDiagnosed_S[ii][0] + TotalDiagnosed_S[ii][1];
		}
		Temp2 = (Temp2 - Temp3 * (1.0 - RRtestingDiagnosed)) /
			(Temp1 - Temp3 * (1.0 - RRtestingDiagnosed));
		if (iy == 20){ ModelProvHH[0][0] = Temp2; }
		if (iy == 23){ ModelProvHH[1][0] = Temp2; }
		if (iy == 27){ ModelProvHH[2][0] = Temp2; }
		if (iy == 31){ ModelProvHH[3][0] = Temp2; }
		if (iy == 32){ ModelProvHH[4][0] = Temp2; }
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ii = 25; ii<91; ii++){
			Temp1 += TotalPop_S[ii][0] + TotalPop_S[ii][1];
			Temp2 += TotalPositive_S[ii][0] + TotalPositive_S[ii][1];
			Temp3 += TotalDiagnosed_S[ii][0] + TotalDiagnosed_S[ii][1];
		}
		Temp2 = (Temp2 - Temp3 * (1.0 - RRtestingDiagnosed)) /
			(Temp1 - Temp3 * (1.0 - RRtestingDiagnosed));
		if (iy == 20){ ModelProvHH[0][1] = Temp2; }
		if (iy == 23){ ModelProvHH[1][1] = Temp2; }
		if (iy == 27){ ModelProvHH[2][1] = Temp2; }
		if (iy == 31){ ModelProvHH[3][1] = Temp2; }
		if (iy == 32){ ModelProvHH[4][1] = Temp2; }
	}

	// HIV prevalence in sex workers
	if(FixedUncertainty==1 || CalibFSWprev==1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ia=0; ia<81; ia++){
			Temp1 += FH_SW.Total[ia];
			Temp2 += FH_SW.NegNoHCT[ia] + FH_SW.NegPastHCT[ia] + FH_SW.RegHCT[ia] +
				FH_SW.RegPrEP[ia] + FH_SW.RegVM[ia];
			if (ia == 15){
				Temp3 = Temp1;
				Temp4 = Temp2;
			}
		}
		FSWprev[iy] = 1.0 - (Temp2/Temp1);
		PrevFSW.out[CurrSim-1][iy] = FSWprev[iy];
		NegFSW.out[CurrSim-1][iy] = Temp2;
		PrevFSW15to24.out[CurrSim - 1][iy] = 1.0 - (Temp4 / Temp3);
		PrevFSW25plus.out[CurrSim - 1][iy] = 1.0 - ((Temp2 - Temp4) / (Temp1 - Temp3));
	}

	// HIV prevalence in MSM
	if (FixedUncertainty == 1 || CalibMSMprev==1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ia = 8; ia < 81; ia++){
			Temp1 += MHU_STM.NegNoHCT[ia] + MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] +
				MHU_STM.RegPrEP[ia] + MHC_STM.NegNoHCT[ia] + MHC_STM.NegPastHCT[ia] + MHC_STM.RegHCT[ia] +
				MHC_STM.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] + MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] +
				MLU_STM.RegPrEP[ia] + MLC_STM.NegNoHCT[ia] + MLC_STM.NegPastHCT[ia] + MLC_STM.RegHCT[ia] +
				MLC_STM.RegPrEP[ia];
			Temp2 += MHU_STM.Total[ia] + MHC_STM.Total[ia] + MLU_STM.Total[ia] + MLC_STM.Total[ia];
			if (ia == 14){
				MSMprev18to24.out[CurrSim - 1][iy] = 1.0 -  Temp1 / Temp2;
				Temp3 = Temp2;
				Temp1 = 0.0;
				Temp2 = 0.0;
			}
		}
		TotalMSM.out[CurrSim - 1][iy] = Temp2;
		MSMprev25plus.out[CurrSim - 1][iy] = 1.0 - (Temp1 / Temp2);
		MSMpropn18to24.out[CurrSim - 1][iy] = Temp3 / (Temp3 + Temp2);
		MSMprev18plus.out[CurrSim - 1][iy] = MSMpropn18to24.out[CurrSim - 1][iy] * MSMprev18to24.out[CurrSim - 1][iy] +
			(1.0 - MSMpropn18to24.out[CurrSim - 1][iy]) * MSMprev25plus.out[CurrSim - 1][iy];
		Temp1 = 0.0;
		for (ia = 5; ia < 40; ia++){
			Temp1 += MHU_STM.NegNoHCT[ia] + MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] +
				MHU_STM.RegPrEP[ia] + MHC_STM.NegNoHCT[ia] + MHC_STM.NegPastHCT[ia] + MHC_STM.RegHCT[ia] +
				MHC_STM.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] + MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] +
				MLU_STM.RegPrEP[ia] + MLC_STM.NegNoHCT[ia] + MLC_STM.NegPastHCT[ia] + MLC_STM.RegHCT[ia] +
				MLC_STM.RegPrEP[ia];
		}
		NegMSM.out[CurrSim - 1][iy] = Temp1;
		/*Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 18; ia < 91; ia++){
			Temp1 += TotalPop_S[ia][0];
			Temp2 += TotalPositive_S[ia][0];
		}
		MalePrev18plus.out[CurrSim - 1][iy] = Temp2 / Temp1;*/
	}
	if (FixedUncertainty == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 40; ia++){
			Temp1 += MHU_STM.NegNoHCT[ia] + MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] +
				MHU_STM.RegPrEP[ia] + MHC_STM.NegNoHCT[ia] + MHC_STM.NegPastHCT[ia] + MHC_STM.RegHCT[ia] +
				MHC_STM.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] + MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] +
				MLU_STM.RegPrEP[ia] + MLC_STM.NegNoHCT[ia] + MLC_STM.NegPastHCT[ia] + MLC_STM.RegHCT[ia] +
				MLC_STM.RegPrEP[ia];
			Temp2 += MHU_STM.Total[ia] + MHC_STM.Total[ia] + MLU_STM.Total[ia] + MLC_STM.Total[ia];
		}
		MSMprev15to49.out[CurrSim - 1][iy] = 1.0 - (Temp1 / Temp2);
	}

	// Proportions of adults in different CD4 stages
	if(FixedUncertainty==1 || CalibCD4==1){
		for(is=0; is<4; is++){
			TempCD4[is] = 0.0;}
		for(ia=5; ia<81; ia++){
			TempCD4[0] += SumGroupsM[ia][5] + SumGroupsF[ia][5] +
				SumGroupsM[ia][10] + SumGroupsF[ia][10] +
				SumGroupsM[ia][15] + SumGroupsF[ia][15];
			for(is=0; is<4; is++){ // Current CD4
				TempCD4[is] += SumGroupsM[ia][6+is] + SumGroupsF[ia][6+is] +
					SumGroupsM[ia][11+is] + SumGroupsF[ia][11+is] +
					SumGroupsM[ia][16+is] + SumGroupsF[ia][16+is] +
					SumGroupsM[ia][40+is] + SumGroupsF[ia][40+is];
				for(ii=1; ii<5; ii++){ // baseline CD4
					for (id = 1; id < 5; id++){ // ART duration
						TempCD4[is] += (SumGroupsM[ia][15 + ii * 5 + id] * OnARThalfIntDur[id][0] +
							SumGroupsF[ia][15 + ii * 5 + id] * OnARThalfIntDur[id][1]) * CD4dbnNonIntDur[is][4-ii][id];
						if (is == ii){
							TempCD4[is] += SumGroupsM[ia][15 + ii * 5 + id] * (1.0 - OnARThalfIntDur[id][0]) +
								SumGroupsF[ia][15 + ii * 5 + id] * (1.0 - OnARThalfIntDur[id][1]);
						}
					}
				}
			}
		}
		Temp1 = TempCD4[0] + TempCD4[1] + TempCD4[2] + TempCD4[3];
		AdultsUnder200.out[CurrSim-1][iy] = TempCD4[3]/Temp1;
		Adults200to349.out[CurrSim-1][iy] = TempCD4[2]/Temp1;
		Adults350to499.out[CurrSim-1][iy] = TempCD4[1]/Temp1;
		AdultsOver500.out[CurrSim-1][iy] = TempCD4[0]/Temp1;
	}

	// ART totals
	Temp1 = 0.0;
	for(ia=0; ia<15; ia++){
		Temp1 += TotalART[ia][0] + TotalART[ia][1];}
	TotalARTunder15.out[CurrSim-1][iy] = Temp1;
	
	Temp1 = 0.0;
	for(ia=1; ia<3; ia++){
		Temp1 += TotalART[ia][0] + TotalART[ia][1];}
	TotalART1to2.out[CurrSim-1][iy] = Temp1;
	
	Temp1 = 0.0;
	for(ia=3; ia<6; ia++){
		Temp1 += TotalART[ia][0] + TotalART[ia][1];}
	TotalART3to5.out[CurrSim-1][iy] = Temp1;
	
	Temp1 = 0.0;
	for(ia=6; ia<10; ia++){
		Temp1 += TotalART[ia][0] + TotalART[ia][1];}
	TotalART6to9.out[CurrSim-1][iy] = Temp1;
	
	Temp1 = 0.0;
	for(ia=10; ia<15; ia++){
		Temp1 += TotalART[ia][0] + TotalART[ia][1];}
	TotalART10to14.out[CurrSim-1][iy] = Temp1;

	if (FixedUncertainty == 1 || CalibARTcoverage == 1 || CalibARTtotals == 1){
		ARTcoverageU15.out[CurrSim - 1][iy] = Temp1 / TotPaedHIV.out[CurrSim - 1][iy];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ia=15; ia<91; ia++){
			Temp1 += TotalART[ia][0];
			Temp2 += TotalART[ia][1];
		}
		TotalART15M.out[CurrSim-1][iy] = Temp1;
		TotalART15F.out[CurrSim-1][iy] = Temp2;
		TotalOnART.out[CurrSim - 1][iy] = Temp1 + Temp2 +
			TotalARTunder15.out[CurrSim - 1][iy];
		ARTcoverage.out[CurrSim - 1][iy] = TotalOnART.out[CurrSim - 1][iy] /
			TotalHIV.out[CurrSim - 1][iy];
		// Calculate unmet need based on SA protocols at start of 2014
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ia=0; ia<5; ia++){
			Temp1 += TotalPositive[ia][0] - TotalART[ia][0];
			Temp2 += TotalPositive[ia][0] - TotalART[ia][0];
		}
		for(ia=5; ia<10; ia++){
			Temp1 += TotalNaiveElig[ia][0];
			Temp2 += TotalNaiveElig[ia][0];
		}
		for(ia=10; ia<91; ia++){
			Temp1 += TotalNaiveElig[ia][0] + SumGroupsM[ia-10][8] +
				SumGroupsM[ia-10][13] + SumGroupsM[ia-10][18];
			Temp2 += TotalNaiveElig[ia][1] + SumGroupsF[ia-10][8] +
				SumGroupsF[ia-10][13] + SumGroupsF[ia-10][18];
			if(ExcludeInterrupters==1){
				for(is=0; is<4; is++){
					for(id=1; id<5; id++){
						Temp1 += SumGroupsM[ia-10][20+is*5+id] * (1.0 - OnARThalfIntDur[id][0]);
						Temp2 += SumGroupsF[ia-10][20+is*5+id] * (1.0 - OnARThalfIntDur[id][1]);
					}
				}
			}
			if(ia==14){
				TotUnmetUnder15.out[CurrSim-1][iy] = Temp1 + Temp2;
				Temp1 = 0.0;
				Temp2 = 0.0;
			}
		}
		TotUnmet15M.out[CurrSim-1][iy] = Temp1;
		TotUnmet15F.out[CurrSim-1][iy] = Temp2;
		if (ExcludeInterrupters == 1){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for (ia = 0; ia < 10; ia++){
				Temp2 += (TotalART[ia][0] + TotalART[ia][1]) * (1.0 - OnARTpaed[ia]) / OnARTpaed[ia]; }
			for (ia = 10; ia < 91; ia++){
				for (is = 0; is < 4; is++){
					for (id = 1; id < 5; id++){
						Temp1 += SumGroupsM[ia - 10][20 + is * 5 + id] * (1.0 - OnARThalfIntDur[id][0]) +
							SumGroupsF[ia - 10][20 + is * 5 + id] * (1.0 - OnARThalfIntDur[id][1]);
					}
				}
				if (ia == 14){
					Temp2 += Temp1;
					Temp1 = 0.0;
				}
			}
			AdultARTinterrupters.out[CurrSim - 1][iy] = Temp1;
			AdultInterruptPropn.out[CurrSim - 1][iy] = Temp1 / (TotalHIV.out[CurrSim - 1][iy] - TotPaedHIV.out[CurrSim - 1][iy]);
			ChildARTinterrupters.out[CurrSim - 1][iy] = Temp2;
			ChildInterruptPropn.out[CurrSim - 1][iy] = Temp2 / TotPaedHIV.out[CurrSim - 1][iy];
		}
	}
	if (FixedUncertainty == 1 || CalibARTbyAgeP2 == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ia = 0; ia < 5; ia++){
			Temp1 += TotalART[ia][0] + TotalART[ia][1];
			Temp2 += TotalART[ia+5][0] + TotalART[ia+5][1];
			Temp3 += TotalART[ia+10][0] + TotalART[ia+10][1];
		}
		PaedARTpropn0to4.out[CurrSim - 1][iy] = Temp1 / (Temp1 + Temp2 + Temp3);
		PaedARTpropn5to9.out[CurrSim - 1][iy] = Temp2 / (Temp1 + Temp2 + Temp3);
	}
	if (CalibARTbyAge == 1){
		if (CurrYear >= 2015 && CurrYear <= 2021){
			for (ig = 0; ig < 2; ig++){
				Temp1 = TotalART[15][ig] + TotalART[16][ig] + TotalART[17][ig] + TotalART[18][ig] + TotalART[19][ig];
				for (ia = 0; ia < 9; ia++){
					Temp2 = 0.0;
					if (ia < 8){ AgeLength = 5; }
					else{ AgeLength = 31; }
					for (ii = ia * 5 + 20; ii < ia * 5 + 20 + AgeLength; ii++){
						Temp2 += TotalART[ii][ig];
					}
					ModelAgeDbnAdultART[CurrYear - 2015][ia][ig] = Temp2;
					Temp1 += Temp2;
				}
				for (ia = 0; ia < 9; ia++){
					ModelAgeDbnAdultART[CurrYear - 2015][ia][ig] = ModelAgeDbnAdultART[CurrYear - 2015][ia][ig] / Temp1;
				}
			}
		}
	}

	// Viral suppression
	if (FixedUncertainty == 1){
		Temp1 = pow((6.0 - log10(1000)) / (6.0 - log10(400)), ShapeVL); // Slide 7 of 'Viral suppression thresholds.ppt'
		for (ig = 0; ig < 2; ig++){
			for (is = 0; is < 4; is++){
				TempCD4[is] = 0.0;
				TempVL[is] = 1.0 / (1.0 + (1.0 - CurrSuppression200) / (CurrSuppression200 *
					ORsuppressionCD4[is]));
				TempVL2[is] = pow(TempVL[is], Temp1);
			}
			for (ia = 5; ia < 81; ia++){
				for (is = 0; is < 4; is++){
					for (id = 1; id < 5; id++){
						if (ig == 0){ TempCD4[is] += SumGroupsM[ia][20 + is * 5 + id] * OnARThalfIntDur[id][0]; }
						else{ TempCD4[is] += SumGroupsF[ia][20 + is * 5 + id] * OnARThalfIntDur[id][1]; }
					}
				}
			}
			Temp2 = 0.0;
			Temp3 = 0.0;
			Temp4 = 0.0;
			for (is = 0; is < 4; is++){
				Temp2 += TempCD4[is] * TempVL[is];
				Temp4 += TempCD4[is] * TempVL2[is];
				Temp3 += TempCD4[is];
			}
			if (ig == 0){
				VLsuppressedM.out[CurrSim - 1][iy] = Temp2 / Temp3;
				VLsuppressed1000M.out[CurrSim - 1][iy] = Temp4 / Temp3;
			}
			else{
				VLsuppressedF.out[CurrSim - 1][iy] = Temp2 / Temp3;
				VLsuppressed1000F.out[CurrSim - 1][iy] = Temp4 / Temp3;
			}
		}
		if (TotalARTunder15.out[CurrSim - 1][iy] > 0.0){
			VLsuppressedU15.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 - VLsuppressionPaed[iy]) / (VLsuppressionPaed[iy] * ORsuppressionIeDEA));
			VLsuppressed1000P.out[CurrSim - 1][iy] = pow(VLsuppressedU15.out[CurrSim - 1][iy], Temp1);
		}
		else{
			VLsuppressedU15.out[CurrSim - 1][iy] = 0.0;
			VLsuppressed1000P.out[CurrSim - 1][iy] = 0.0;
		}
		VLsuppressed.out[CurrSim - 1][iy] = (VLsuppressionPaed[iy] * TotalARTunder15.out[CurrSim - 1][iy] +
			VLsuppressedM.out[CurrSim - 1][iy] * TotalART15M.out[CurrSim - 1][iy] +
			VLsuppressedF.out[CurrSim - 1][iy] * TotalART15F.out[CurrSim - 1][iy]) / TotalOnART.out[CurrSim - 1][iy];
		VLsuppressed1000.out[CurrSim - 1][iy] = (VLsuppressed1000P.out[CurrSim - 1][iy] * TotalARTunder15.out[CurrSim - 1][iy] +
			VLsuppressed1000M.out[CurrSim - 1][iy] * TotalART15M.out[CurrSim - 1][iy] +
			VLsuppressed1000F.out[CurrSim - 1][iy] * TotalART15F.out[CurrSim - 1][iy]) / TotalOnART.out[CurrSim - 1][iy];
		VLsuppressed15.out[CurrSim - 1][iy] = (VLsuppressedM.out[CurrSim - 1][iy] * TotalART15M.out[CurrSim - 1][iy] +
		  VLsuppressedF.out[CurrSim - 1][iy] * TotalART15F.out[CurrSim - 1][iy]) / (TotalART15M.out[CurrSim - 1][iy] + 
		  TotalART15F.out[CurrSim - 1][iy]);
		VLunsuppressed15.out[CurrSim - 1][iy] = 1 - VLsuppressed15.out[CurrSim - 1][iy];
	}

	// Condom use
	if(FixedUncertainty==1){
		// By sex workers
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ia=0; ia<81; ia++){
			Temp1 += FH_SW.Total[ia];
			Temp2 += FH_SW.NegNoHCT[ia] + FH_SW.NegPastHCT[ia] + FH_SW.RegHCT[ia] +
				FH_SW.RegPrEP[ia] + FH_SW.RegVM[ia];
			for(is=0; is<5; is++){
				Temp2 += FH_SW.PosNoHCT[ia][is] * RelativeUnprot[is] +
					FH_SW.PosHCTpreHIV[ia][is] * RelativeUnprot[is+5] +
					FH_SW.PosDiagnosedPreART[ia][is] * RelativeUnprot[is+10] +
					FH_SW.OnARTpre500[ia][is] * RelativeUnprot[is+15] +
					FH_SW.OnART500[ia][is] * RelativeUnprot[is+20] +
					FH_SW.OnART350[ia][is] * RelativeUnprot[is+25] +
					FH_SW.OnART200[ia][is] * RelativeUnprot[is+30];
			}
			for(is=0; is<4; is++){
				Temp2 += FH_SW.StoppedART[ia][is] * RelativeUnprot[is+35];}
		}
		CondomUseFSW[iy] = 1.0 - (1.0 - ProbCondomFSW) * (Temp2/Temp1);
		FSWcondomUse.out[CurrSim-1][iy] = CondomUseFSW[iy];
		// By young women 15-24
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia<15; ia++){
			Temp3 = 0.0; // Condom use at indiv age
			for (is = 0; is<5; is++){
				Temp3 += SumGroupsF[ia][is] - SumGroupsVF[ia][is];}
			for (is = 0; is<39; is++){
				Temp3 += (SumGroupsF[ia][is+5] - SumGroupsVF[ia][is+5]) * RelativeUnprot[is];}
			Temp3 = 1.0 - (1.0 - (ProbCondomST[ia][1] * (TotalSexuallyExp[ia + 10][1] - TotalMarried[10 + ia][1]) +
				ProbCondomLT[ia][1] * TotalMarried[10 + ia][1]) / TotalSexuallyExp[ia + 10][1]) * Temp3 /
				TotalSexuallyExp[ia + 10][1];
			Temp1 += TotalSexuallyExp[ia + 10][1];
			Temp2 += TotalSexuallyExp[ia + 10][1] * Temp3;
		}
		CondomUse15to24F.out[CurrSim - 1][iy] = Temp2 / Temp1;
		// By women 25-49 (slight simplification: ignoring virgins in 25-29 age group)
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 15; ia<40; ia++){
			Temp3 = 0.0; // Condom use at indiv age
			for (is = 0; is<5; is++){
				Temp3 += SumGroupsF[ia][is];}
			for (is = 0; is<39; is++){
				Temp3 += SumGroupsF[ia][is + 5] * RelativeUnprot[is];}
			Temp3 = 1.0 - (1.0 - (ProbCondomST[ia][1] * (TotalSexuallyExp[ia + 10][1] - TotalMarried[10 + ia][1]) +
				ProbCondomLT[ia][1] * TotalMarried[10 + ia][1]) / TotalSexuallyExp[ia + 10][1]) * Temp3 /
				TotalPop[ia + 10][1];
			Temp1 += TotalSexuallyExp[ia + 10][1];
			Temp2 += TotalSexuallyExp[ia + 10][1] * Temp3;
		}
		CondomUse25to49F.out[CurrSim - 1][iy] = Temp2 / Temp1;
	}

	// Adolescent numbers
	/*Temp1 = 0.0;
	Temp2 = 0.0;
	Temp3 = 0.0;
	Temp4 = 0.0;
	for(ia=10; ia<20; ia++){
		Temp4 += TotalPop_S[ia][0] + TotalPop_S[ia][1];
		Temp3 += TotalART_S[ia][0] + TotalART_S[ia][1];
		Temp2 += TotalNaiveElig_S[ia][0] + TotalNaiveElig_S[ia][1];
		Temp1 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
	}
	AdolHIVprofile[iy][0] = Temp1 - Temp2 - Temp3;
	AdolHIVprofile[iy][1] = Temp2;
	AdolHIVprofile[iy][2] = Temp3;
	AdolHIVprofile[iy][3] = Temp4;

	// FSW numbers
	for(ia=0; ia<81; ia++){
		FSW_HIVprofile[iy][0] += FH_SW.NegNoHCT[ia] +
			FH_SW.NegPastHCT[ia] + FH_SW.RegHCT[ia];
		FSW_HIVprofile[iy][1] += FH_SW.RegPrEP[ia];
		FSW_HIVprofile[iy][2] += FH_SW.RegVM[ia];
		FSW_HIVprofile[iy][3] += FH_SW.PosNoHCT[ia][0] +
			FH_SW.PosNoHCT[ia][1] + FH_SW.PosHCTpreHIV[ia][0] + FH_SW.PosHCTpreHIV[ia][1];
		for(is=2; is<5; is++){
			FSW_HIVprofile[iy][2+is] += FH_SW.PosNoHCT[ia][is] +
				FH_SW.PosHCTpreHIV[ia][is];}
		FSW_HIVprofile[iy][7] += FH_SW.PosDiagnosedPreART[ia][0] +
			FH_SW.PosDiagnosedPreART[ia][1];
		for(is=2; is<5; is++){
			FSW_HIVprofile[iy][6+is] += FH_SW.PosDiagnosedPreART[ia][is];}
		for(is=0; is<5; is++){
			FSW_HIVprofile[iy][11] += FH_SW.OnARTpre500[ia][is];
			FSW_HIVprofile[iy][12] += FH_SW.OnART500[ia][is];
			FSW_HIVprofile[iy][13] += FH_SW.OnART350[ia][is];
			FSW_HIVprofile[iy][14] += FH_SW.OnART200[ia][is];
		}
	}*/

	// Previously tested
	if (CalibHCT_HH == 1 && (CurrYear == 2005 || CurrYear == 2008 || CurrYear == 2012 || CurrYear == 2016 || CurrYear == 2017)){
		// HIV-negative men
		for(ia=0; ia<5; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			if(ia<4){StartAge = 5 + ia*10;}
			else{StartAge = 50;}
			if(ia<3){AgeLength = 10;}
			else if(ia==3){AgeLength = 15;}
			else{AgeLength = 31;}
			for(ii=0; ii<AgeLength; ii++){
				ig = StartAge + ii;
				Temp1 += SumGroupsM[ig][0];
				Temp2 += TotalPop[ig+10][0] - TotalPositive[ig+10][0];
			}
			if(CurrYear==2005){ModelTested05[ia][0][0] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2008){ModelTested08[ia][0][0] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2012){ModelTested12[ia][0][0] = 1.0 - Temp1/Temp2;}
			if (CurrYear == 2016 && ia<4){ ModelTested16[ia][0][0] = 1.0 - Temp1 / Temp2; }
			if (CurrYear == 2017){ ModelTested17[ia][0][0] = 1.0 - Temp1 / Temp2; }
		}
		// HIV-positive men
		for(ia=0; ia<5; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			if(ia<4){StartAge = 5 + ia*10;}
			else{StartAge = 50;}
			if(ia<3){AgeLength = 10;}
			else if(ia==3){AgeLength = 15;}
			else{AgeLength = 31;}
			for(ii=0; ii<AgeLength; ii++){
				ig = StartAge + ii;
				Temp1 += SumGroupsM[ig][5] + SumGroupsM[ig][6] + SumGroupsM[ig][7] +
					SumGroupsM[ig][8] + SumGroupsM[ig][9];
				Temp2 += TotalPositive[ig+10][0];
			}
			if(CurrYear==2005){ModelTested05[ia][0][1] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2008){ModelTested08[ia][0][1] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2012){ModelTested12[ia][0][1] = 1.0 - Temp1/Temp2;}
			if (CurrYear == 2016 && ia<4){ ModelTested16[ia][0][1] = 1.0 - Temp1 / Temp2; }
			if (CurrYear == 2017){ ModelTested17[ia][0][1] = 1.0 - Temp1 / Temp2; }
		}
		// HIV-negative women
		for(ia=0; ia<5; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			if(ia<4){StartAge = 5 + ia*10;}
			else{StartAge = 50;}
			if(ia<3){AgeLength = 10;}
			else if(ia==3){AgeLength = 15;}
			else{AgeLength = 31;}
			for(ii=0; ii<AgeLength; ii++){
				ig = StartAge + ii;
				Temp1 += SumGroupsF[ig][0];
				Temp2 += TotalPop[ig+10][1] - TotalPositive[ig+10][1];
			}
			if(CurrYear==2005){ModelTested05[ia][1][0] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2008){ModelTested08[ia][1][0] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2012){ModelTested12[ia][1][0] = 1.0 - Temp1/Temp2;}
			if (CurrYear == 2016 && ia<4){ ModelTested16[ia][1][0] = 1.0 - Temp1 / Temp2; }
			if (CurrYear == 2017){ ModelTested17[ia][1][0] = 1.0 - Temp1 / Temp2; }
		}
		// HIV-positive women
		for(ia=0; ia<5; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			if(ia<4){StartAge = 5 + ia*10;}
			else{StartAge = 50;}
			if(ia<3){AgeLength = 10;}
			else if(ia==3){AgeLength = 15;}
			else{AgeLength = 31;}
			for(ii=0; ii<AgeLength; ii++){
				ig = StartAge + ii;
				Temp1 += SumGroupsF[ig][5] + SumGroupsF[ig][6] + SumGroupsF[ig][7] +
					SumGroupsF[ig][8] + SumGroupsF[ig][9];
				Temp2 += TotalPositive[ig+10][1];
			}
			if(CurrYear==2005){ModelTested05[ia][1][1] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2008){ModelTested08[ia][1][1] = 1.0 - Temp1/Temp2;}
			if(CurrYear==2012){ModelTested12[ia][1][1] = 1.0 - Temp1/Temp2;}
			if (CurrYear == 2016 && ia<4){ ModelTested16[ia][1][1] = 1.0 - Temp1 / Temp2; }
			if (CurrYear == 2017){ ModelTested17[ia][1][1] = 1.0 - Temp1 / Temp2; }
		}
	}
	if(CurrYear==2009 && FixedUncertainty==1){
		for(ia=0; ia<8; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(ii=0; ii<5; ii++){
				ig = (ia+1)*5 + ii;
				Temp1 += SumGroupsM[ig][0] + SumGroupsM[ig][5] +
					SumGroupsM[ig][6] + SumGroupsM[ig][7] +
					SumGroupsM[ig][8] + SumGroupsM[ig][9];
				if(ia<3){
					Temp1 = Temp1 - SumGroupsVM[ig][0] - SumGroupsVM[ig][5] -
						SumGroupsVM[ig][6] - SumGroupsVM[ig][7] -
						SumGroupsVM[ig][8] - SumGroupsVM[ig][9];
				}
				if(ia<3){
					Temp2 += TotalSexuallyExp[ig+10][0];}
				else{
					Temp2 += TotalPop[ig+10][0];}
			}
			PrevTested09.out[CurrSim-1][ia] = 1.0 - Temp1/Temp2;
			Temp1 = 0.0;
			Temp2 = 0.0;
			for(ii=0; ii<5; ii++){
				ig = (ia+1)*5 + ii;
				Temp1 += SumGroupsF[ig][0] + SumGroupsF[ig][5] +
					SumGroupsF[ig][6] + SumGroupsF[ig][7] +
					SumGroupsF[ig][8] + SumGroupsF[ig][9];
				if(ia<3){
					Temp1 = Temp1 - SumGroupsVF[ig][0] - SumGroupsVF[ig][5] -
						SumGroupsVF[ig][6] - SumGroupsVF[ig][7] -
						SumGroupsVF[ig][8] - SumGroupsVF[ig][9];
				}
				if(ia<3){
					Temp2 += TotalSexuallyExp[ig+10][1];}
				else{
					Temp2 += TotalPop[ig+10][1];}
			}
			PrevTested09.out[CurrSim-1][ia+8] = 1.0 - Temp1/Temp2;
		}
	}
	if (FixedUncertainty == 1){
		// All adults
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		Temp4 = 0.0;
		for (ia = 5; ia < 81; ia++){
			Temp1 += SumGroupsM[ia][0];
			Temp2 += SumGroupsF[ia][0];
			for (is = 0; is < 5; is++){
				Temp1 += SumGroupsM[ia][is + 5];
				Temp2 += SumGroupsF[ia][is + 5];
			}
			Temp3 += TotalPop[ia + 10][0];
			Temp4 += TotalPop[ia + 10][1];
		}
		AdultsEverTested.out[CurrSim - 1][iy] = 1.0 - (Temp1 + Temp2) / (Temp3 + Temp4);
		AdultsEverTestedM.out[CurrSim - 1][iy] = 1.0 - Temp1 / Temp3;
		AdultsEverTestedF.out[CurrSim - 1][iy] = 1.0 - Temp2 / Temp4;
	}

	// Prob of testing in next 12 months (code for Temp2 modified from CalcHCT1stTime function)
	if (FixedUncertainty == 1 && UseNumbersTests == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 81; ia++){
			Temp1 += TotalPop[ia + 10][0] + TotalPop[ia + 10][1];
			Temp2 += SumGroupsM[ia][0] * (1.0 - exp(-TestingRateSE[ia][0][0]*12.0)) +
				SumGroupsM[ia][1] * (1.0 - exp(-TestingRateSE[ia][1][0]*12.0));
			Temp2 += SumGroupsF[ia][0] * (1.0 - exp(-TestingRateSE[ia][0][1]*12.0)) +
				SumGroupsF[ia][1] * (1.0 - exp(-TestingRateSE[ia][1][1]*12.0));
			if (PrEPorVM == 1){
				Temp2 += SumGroupsM[ia][2] + SumGroupsM[ia][3];
				Temp2 += SumGroupsF[ia][2] + SumGroupsF[ia][3] + SumGroupsF[ia][4];
			}
			for (is = 2; is<12; is++){
				Temp2 += SumGroupsM[ia][is + 3] * (1.0 - exp(-TestingRateSE[ia][is][0]*12.0));
				Temp2 += SumGroupsF[ia][is + 3] * (1.0 - exp(-TestingRateSE[ia][is][1]*12.0));
			}
			for (is = 15; is < 20; is++){
				Temp2 += SumGroupsM[ia][is] * (1.0 - exp(-TestingRateSE[ia][0][0] * RetestPos*12.0));
				Temp2 += SumGroupsF[ia][is] * (1.0 - exp(-TestingRateNPF[ia] * RetestPos*12.0));
			}
			for (is = 20; is < 44; is++){
				Temp2 += SumGroupsM[ia][is] * (1.0 - exp(-TestingRateSE[ia][0][0] * RetestART*12.0));
				Temp2 += SumGroupsF[ia][is] * (1.0 - exp(-TestingRateNPF[ia] * RetestART*12.0));
			}
			if (ia<20){
				// Adjustment for HIV-neg virgins
				Temp2 = Temp2 + SumGroupsVM[ia][0] * (exp(-TestingRateSE[ia][0][0] * 12.0) - exp(-TestingRateNegV[ia][0] *
					12.0)) + SumGroupsVM[ia][1] * (exp(-TestingRateSE[ia][1][0] * 12.0) - exp(-TestingRateNegV[ia][0] * 12.0));
				Temp2 = Temp2 + SumGroupsVF[ia][0] * (exp(-TestingRateSE[ia][0][1] * 12.0) - exp(-TestingRateNegV[ia][1] *
					12.0)) + SumGroupsVF[ia][1] * (exp(-TestingRateSE[ia][1][1] * 12.0) - exp(-TestingRateNegV[ia][1] * 12.0));
				// Subtract tests in HIV-pos virgins
				for (is = 2; is<12; is++){
					Temp2 = Temp2 - SumGroupsVM[ia][is + 3] * (1.0 - exp(-TestingRateSE[ia][is][0]*12.0));
					Temp2 = Temp2 - SumGroupsVF[ia][is + 3] * (1.0 - exp(-TestingRateSE[ia][is][1]*12.0));
				}
				for (is = 15; is < 20; is++){
					Temp2 = Temp2 - SumGroupsVM[ia][is] * (1.0 - exp(-TestingRateSE[ia][0][0] *
						RetestPos*12.0));
					Temp2 = Temp2 - SumGroupsVF[ia][is] * (1.0 - exp(-TestingRateNPF[ia] *
						RetestPos*12.0));
				}
				for (is = 20; is < 44; is++){
					Temp2 = Temp2 - SumGroupsVM[ia][is] * (1.0 - exp(-TestingRateSE[ia][0][0] *
						RetestART*12.0));
					Temp2 = Temp2 - SumGroupsVF[ia][is] * (1.0 - exp(-TestingRateNPF[ia] *
						RetestART*12.0));
				}
				// Add back tests in HIV-pos virgins
				for (is = 0; is<5; is++){
					Temp2 += (SumGroupsVM[ia][is + 5] + SumGroupsVM[ia][is + 10]) *
						(1.0 - exp(-TestingRateV[ia][is][0]*12.0));
					Temp2 += (SumGroupsVF[ia][is + 5] + SumGroupsVF[ia][is + 10]) *
						(1.0 - exp(-TestingRateV[ia][is][1]*12.0));
				}
				for (is = 15; is < 20; is++){
					Temp2 += SumGroupsVM[ia][is] * (1.0 - exp(-TestingRateNegV[ia][0] * RetestPos * 12.0)) +
						SumGroupsVF[ia][is] * (1.0 - exp(-TestingRateNegV[ia][1] * RetestPos * 12.0));
				}
				for (is = 20; is < 44; is++){
					Temp2 += SumGroupsVM[ia][is] * (1.0 - exp(-TestingRateNegV[ia][0] * RetestART * 12.0)) +
						SumGroupsVF[ia][is] * (1.0 - exp(-TestingRateNegV[ia][1] * RetestART * 12.0));
				}
			}
		}
		ProbTestedNextYr.out[CurrSim - 1][iy] = Temp2 / Temp1;
	}

	// Diagnosed and undiagnosed and other cascade indicators
	if (FixedUncertainty == 1 || CalibARTcoverage == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 0; ia < 15; ia++){
			Temp1 += TotalDiagnosed_S[ia][0] + TotalDiagnosed_S[ia][1];
			Temp2 += TotalPositive_S[ia][0] + TotalPositive_S[ia][1];
		}
		UndiagnosedHIV_U15.out[CurrSim - 1][iy] = Temp2 - Temp1;
		DiagnosedHIV_U15.out[CurrSim - 1][iy] = Temp1;
		DiagnosedPropnU15.out[CurrSim - 1][iy] = Temp1 / Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		Temp4 = 0.0;
		for (ia = 15; ia < 91; ia++){
			Temp1 += TotalPositive_S[ia][0] - TotalDiagnosed_S[ia][0];
			Temp2 += TotalPositive_S[ia][1] - TotalDiagnosed_S[ia][1];
			Temp3 += TotalDiagnosed_S[ia][0];
			Temp4 += TotalDiagnosed_S[ia][1];
		}
		UndiagnosedHIV_M.out[CurrSim - 1][iy] = Temp1;
		UndiagnosedHIV_F.out[CurrSim - 1][iy] = Temp2;
		DiagnosedHIV_M.out[CurrSim - 1][iy] = Temp3;
		DiagnosedHIV_F.out[CurrSim - 1][iy] = Temp4;
		DiagnosedHIVtot.out[CurrSim - 1][iy] = Temp3 + Temp4 + DiagnosedHIV_U15.out[CurrSim - 1][iy];
		DiagnosedPropn.out[CurrSim - 1][iy] = DiagnosedHIVtot.out[CurrSim - 1][iy] /
			(Temp1 + Temp2 + DiagnosedHIV_U15.out[CurrSim - 1][iy] + Temp3 + Temp4 +
			UndiagnosedHIV_U15.out[CurrSim - 1][iy]);
		DiagnosedPropnM.out[CurrSim - 1][iy] = Temp3 / (Temp1 + Temp3);
		DiagnosedPropnF.out[CurrSim - 1][iy] = Temp4 / (Temp2 + Temp4);
		DiagnosedPropnAdult.out[CurrSim - 1][iy] = (Temp3 + Temp4) / (Temp1 + Temp2 + Temp3 + Temp4);
		TotHIV15M.out[CurrSim - 1][iy] = Temp1 + Temp3;
		TotHIV15F.out[CurrSim - 1][iy] = Temp2 + Temp4;
		TotHIV15.out[CurrSim - 1][iy] = Temp1 + Temp2 + Temp3 + Temp4;
		ARTcoverage15M.out[CurrSim - 1][iy] = TotalART15M.out[CurrSim - 1][iy] / (Temp1 + Temp3);
		ARTcoverage15F.out[CurrSim - 1][iy] = TotalART15F.out[CurrSim - 1][iy] / (Temp2 + Temp4);
		ARTcoverageAdult.out[CurrSim - 1][iy] = (TotalART15F.out[CurrSim - 1][iy] +
			TotalART15M.out[CurrSim - 1][iy])/ (Temp1 + Temp2 + Temp3 + Temp4);
		ARTcoverageDiag.out[CurrSim - 1][iy] = TotalOnART.out[CurrSim - 1][iy] / (Temp3 + Temp4 +
			DiagnosedHIV_U15.out[CurrSim - 1][iy]);
		ARTcoverageDiag15.out[CurrSim - 1][iy] = (TotalART15F.out[CurrSim - 1][iy] + TotalART15M.out[CurrSim - 1][iy]) / (Temp3 + Temp4);
		ARTcoverageDiagM.out[CurrSim - 1][iy] = TotalART15M.out[CurrSim - 1][iy] / Temp3;
		ARTcoverageDiagF.out[CurrSim - 1][iy] = TotalART15F.out[CurrSim - 1][iy] / Temp4;
		ARTcoverageDiagU15.out[CurrSim - 1][iy] = TotalARTunder15.out[CurrSim - 1][iy] /
			DiagnosedHIV_U15.out[CurrSim - 1][iy];
		VLsuppressedAllHIV.out[CurrSim - 1][iy] = ARTcoverage.out[CurrSim - 1][iy] * VLsuppressed.out[CurrSim - 1][iy];
		VLsuppressedAllM.out[CurrSim - 1][iy] = ARTcoverage15M.out[CurrSim - 1][iy] * VLsuppressedM.out[CurrSim - 1][iy];
		VLsuppressedAllF.out[CurrSim - 1][iy] = ARTcoverage15F.out[CurrSim - 1][iy] * VLsuppressedF.out[CurrSim - 1][iy];
		VLsuppressedAllU15.out[CurrSim - 1][iy] = ARTcoverageU15.out[CurrSim - 1][iy] * VLsuppressedU15.out[CurrSim - 1][iy];
		VLsuppressedAll1000.out[CurrSim - 1][iy] = ARTcoverage.out[CurrSim - 1][iy] * VLsuppressed1000.out[CurrSim - 1][iy];
		VLsuppressedAllM1000.out[CurrSim - 1][iy] = ARTcoverage15M.out[CurrSim - 1][iy] * VLsuppressed1000M.out[CurrSim - 1][iy];
		VLsuppressedAllF1000.out[CurrSim - 1][iy] = ARTcoverage15F.out[CurrSim - 1][iy] * VLsuppressed1000F.out[CurrSim - 1][iy];
		VLsuppressedAllP1000.out[CurrSim - 1][iy] = ARTcoverageU15.out[CurrSim - 1][iy] * VLsuppressed1000P.out[CurrSim - 1][iy];
		VLsuppressed15total.out[CurrSim - 1][iy] = (VLsuppressedM.out[CurrSim - 1][iy] * TotalART15M.out[CurrSim - 1][iy] +
		  VLsuppressedF.out[CurrSim - 1][iy] * TotalART15F.out[CurrSim - 1][iy]) / TotHIV15.out[CurrSim - 1][iy];
		VLunsuppressed15total.out[CurrSim - 1][iy] = 1 - VLsuppressed15total.out[CurrSim - 1][iy];

		if (CurrYear == 2012){
			// Calculate profile by age and sex in 2012
			for (ig = 0; ig < 2; ig++){
				for (ia = 0; ia < 10; ia++){
					AgeLength = 5;
					if (ia == 9){ AgeLength = 31; }
					Temp1 = 0.0;
					Temp2 = 0.0;
					Temp3 = 0.0;
					for (ii = 0; ii < AgeLength; ii++){
						Temp1 += TotalPositive_S[ii + ia * 5 + 15][ig] -
							TotalDiagnosed_S[ii + ia * 5 + 15][ig];
						Temp2 += TotalDiagnosed_S[ii + ia * 5 + 15][ig] -
							TotalART_S[ii + ia * 5 + 15][ig];
						Temp3 += TotalART_S[ii + ia * 5 + 15][ig];
					}
					Undiagnosed2012.out[CurrSim - 1][10 * ig + ia] = Temp1;
					DiagnosedUntreated2012.out[CurrSim - 1][10 * ig + ia] = Temp2;
					Treated2012.out[CurrSim - 1][10 * ig + ia] = Temp3;
				}
			}
			// Calculate profile by CD4 stage in 2012
			for (is = 0; is < 4; is++){
				Temp1 = 0.0;
				Temp2 = 0.0;
				for (ia = 5; ia < 81; ia++){
					Temp1 += SumGroupsM[ia][is + 6] + SumGroupsF[ia][is + 6] +
						SumGroupsM[ia][is + 11] + SumGroupsF[ia][is + 11];
					if (is == 0){
						Temp1 += SumGroupsM[ia][5] + SumGroupsF[ia][5] +
							SumGroupsM[ia][10] + SumGroupsF[ia][10];
					}
					Temp2 += SumGroupsM[ia][is + 16] + SumGroupsF[ia][is + 16];
					if (is == 0){
						Temp2 += SumGroupsM[ia][15] + SumGroupsF[ia][15];}
				}
				UntreatedByCD4_2012.out[CurrSim - 1][is] = Temp1;
				UntreatedByCD4_2012.out[CurrSim - 1][is+4] = Temp2;
			}
		}
	}
	if (FixedUncertainty == 1){
		// Cascade indicators for sex workers
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ia = 0; ia < 81; ia++){
			for (is = 0; is < 5; is++){
				Temp1 += FH_SW.PosNoHCT[ia][is] + FH_SW.PosHCTpreHIV[ia][is];
				Temp2 += FH_SW.PosDiagnosedPreART[ia][is] + FH_SW.StoppedART[ia][is];
			}
			for (id = 1; id < 5; id++){
				Temp2 += FH_SW.OnARTpre500[ia][id] + FH_SW.OnART500[ia][id] +
					FH_SW.OnART350[ia][id] + FH_SW.OnART200[ia][id];
				Temp3 += (FH_SW.OnARTpre500[ia][id] + FH_SW.OnART500[ia][id] +
					FH_SW.OnART350[ia][id] + FH_SW.OnART200[ia][id]) * OnARThalfIntDur[id][1];
			}
		}
		DiagnosedHIV_FSW.out[CurrSim - 1][iy] = Temp2 / (Temp1 + Temp2);
		FSWonART.out[CurrSim - 1][iy] = Temp3;
		ARTcoverageFSW.out[CurrSim-1][iy] = Temp3 / (Temp1 + Temp2);
		// Cascade indicators for MSM
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ia = 0; ia < 81; ia++){
			for (is = 0; is < 5; is++){
				Temp1 += MHU_STM.PosNoHCT[ia][is] + MHU_STM.PosHCTpreHIV[ia][is] +
					MHC_STM.PosNoHCT[ia][is] + MHC_STM.PosHCTpreHIV[ia][is] +
					MLU_STM.PosNoHCT[ia][is] + MLU_STM.PosHCTpreHIV[ia][is] +
					MLC_STM.PosNoHCT[ia][is] + MLC_STM.PosHCTpreHIV[ia][is];
				Temp2 += MHU_STM.PosDiagnosedPreART[ia][is] + MHU_STM.StoppedART[ia][is] +
					MHC_STM.PosDiagnosedPreART[ia][is] + MHC_STM.StoppedART[ia][is] +
					MLU_STM.PosDiagnosedPreART[ia][is] + MLU_STM.StoppedART[ia][is] +
					MLC_STM.PosDiagnosedPreART[ia][is] + MLC_STM.StoppedART[ia][is];
			}
			for (id = 1; id < 5; id++){
				Temp2 += MHU_STM.OnARTpre500[ia][id] + MHU_STM.OnART500[ia][id] + MHU_STM.OnART350[ia][id] + MHU_STM.OnART200[ia][id] +
					MHC_STM.OnARTpre500[ia][id] + MHC_STM.OnART500[ia][id] + MHC_STM.OnART350[ia][id] + MHC_STM.OnART200[ia][id] +
					MLU_STM.OnARTpre500[ia][id] + MLU_STM.OnART500[ia][id] + MLU_STM.OnART350[ia][id] + MLU_STM.OnART200[ia][id] +
					MLC_STM.OnARTpre500[ia][id] + MLC_STM.OnART500[ia][id] + MLC_STM.OnART350[ia][id] + MLC_STM.OnART200[ia][id];
				Temp3 += (MHU_STM.OnARTpre500[ia][id] + MHU_STM.OnART500[ia][id] + MHU_STM.OnART350[ia][id] + MHU_STM.OnART200[ia][id] +
					MHC_STM.OnARTpre500[ia][id] + MHC_STM.OnART500[ia][id] + MHC_STM.OnART350[ia][id] + MHC_STM.OnART200[ia][id] +
					MLU_STM.OnARTpre500[ia][id] + MLU_STM.OnART500[ia][id] + MLU_STM.OnART350[ia][id] + MLU_STM.OnART200[ia][id] +
					MLC_STM.OnARTpre500[ia][id] + MLC_STM.OnART500[ia][id] + MLC_STM.OnART350[ia][id] + MLC_STM.OnART200[ia][id]) *
					OnARThalfIntDur[id][0];
			}
		}
		DiagnosedHIV_MSM.out[CurrSim - 1][iy] = Temp2 / (Temp1 + Temp2);
		MSMonART.out[CurrSim - 1][iy] = Temp3;
		ARTcoverageMSM.out[CurrSim - 1][iy] = Temp3 / (Temp1 + Temp2);
	}

	// PrEP outputs
	if (FixedUncertainty == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		for (ia = 0; ia < 81; ia++){
			Temp1 += SumGroupsM[ia][3];
			Temp2 += SumGroupsF[ia][3];
			Temp3 += SumGroupsF[ia][4];
			if (ia == 9){ AdolescOnPrEP.out[CurrSim - 1][iy] = Temp1 + Temp2; }
			if (ia == 14){ AGYWonPrEP.out[CurrSim - 1][iy] = Temp2; }
		}
		MenOnPrEP.out[CurrSim - 1][iy] = Temp1;
		WomenOnPrEP.out[CurrSim - 1][iy] = Temp2;
		if (iy>0){
			NewPrEP_M.out[CurrSim - 1][iy - 1] = (MenOnPrEP.out[CurrSim - 1][iy] -
				MenOnPrEP.out[CurrSim - 1][iy - 1] * exp(-1.0 / PrEPdur[0])) /
				(PrEPdur[0] * (1.0 - exp(-1.0 / PrEPdur[0])));
			NewPrEP_F.out[CurrSim - 1][iy - 1] = (WomenOnPrEP.out[CurrSim - 1][iy] -
				WomenOnPrEP.out[CurrSim - 1][iy - 1] * exp(-1.0 / PrEPdur[1])) /
				(PrEPdur[1] * (1.0 - exp(-1.0 / PrEPdur[1])));
		}
		WomenOnVM.out[CurrSim - 1][iy] = Temp3;
		Temp1 = 0.0;
		for (ia = 0; ia < 81; ia++){ Temp1 += FH_SW.RegPrEP[ia]; }
		FSWonPrEP.out[CurrSim - 1][iy] = Temp1;
		Temp1 = 0.0;
		for (ia = 0; ia < 81; ia++){
			Temp1 += MHU_STM.RegPrEP[ia] + MHC_STM.RegPrEP[ia] +
				MLU_STM.RegPrEP[ia] + MLC_STM.RegPrEP[ia];
		}
		MSMonPrEP.out[CurrSim - 1][iy] = Temp1;
		GenAdultOnPrEP.out[CurrSim - 1][iy] = (MenOnPrEP.out[CurrSim - 1][iy] + 
		WomenOnPrEP.out[CurrSim - 1][iy]) - (FSWonPrEP.out[CurrSim - 1][iy] + 
		AGYWonPrEP.out[CurrSim - 1][iy] + MSMonPrEP.out[CurrSim - 1][iy]);
	}

	// Additional outputs for Investment Case
	if (FixedUncertainty == 1){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 15; ia < 65; ia++){
			Temp1 += TotalPop_S[ia][0];
			Temp2 += TotalPop_S[ia][1];
		}
		Males15to64.out[CurrSim - 1][iy] = Temp1;
		Females15to64.out[CurrSim - 1][iy] = Temp2;
		DependencyRatio.out[CurrSim - 1][iy] = (TotPop.out[CurrSim - 1][iy] -
			Males15to64.out[CurrSim - 1][iy] - Females15to64.out[CurrSim - 1][iy]) /
			(Males15to64.out[CurrSim - 1][iy] + Females15to64.out[CurrSim - 1][iy]);
		for (ia = 65; ia < 91; ia++){
			Temp1 += TotalPop_S[ia][0];
			Temp2 += TotalPop_S[ia][1];
		}
		MalesOver15.out[CurrSim - 1][iy] = Temp1;
		FemalesOver15.out[CurrSim - 1][iy] = Temp2;
		Prev15plus.out[CurrSim - 1][iy] = TotHIV15.out[CurrSim - 1][iy] / (Temp1 + Temp2);
		Prev15plusM.out[CurrSim - 1][iy] = TotHIV15M.out[CurrSim - 1][iy] / Temp1;
		Prev15plusF.out[CurrSim - 1][iy] = TotHIV15F.out[CurrSim - 1][iy] / Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 17; ia < 50; ia++){
			Temp1 += TotalMarried_S[ia][0];
			Temp2 += TotalMarried_S[ia][1];
		}
		MarriedM17to49.out[CurrSim - 1][iy] = Temp1;
		MarriedF17to49.out[CurrSim - 1][iy] = Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 50; ia < 91; ia++){
			Temp1 += TotalMarried_S[ia][0];
			Temp2 += TotalMarried_S[ia][1];
		}
		MarriedM50.out[CurrSim - 1][iy] = Temp1;
		MarriedF50.out[CurrSim - 1][iy] = Temp2;
		TotInfants.out[CurrSim - 1][iy] = TotalPop_S[0][0] + TotalPop_S[0][1];
		Children1to2.out[CurrSim - 1][iy] = TotalPop_S[1][0] + TotalPop_S[1][1] +
			TotalPop_S[2][0] + TotalPop_S[2][1];
		Children3to5.out[CurrSim - 1][iy] = TotalPop_S[3][0] + TotalPop_S[3][1] +
			TotalPop_S[4][0] + TotalPop_S[4][1] + TotalPop_S[5][0] + TotalPop_S[5][1];
		Temp1 = 0.0;
		for (ia = 6; ia < 14; ia++){Temp1 += TotalPop_S[ia][0] + TotalPop_S[ia][1];}
		Children6to13.out[CurrSim - 1][iy] = Temp1;
		for (ia = 14; ia < 19; ia++){ Temp1 += TotalPop_S[ia][0] + TotalPop_S[ia][1]; }
		Children6to18.out[CurrSim - 1][iy] = Temp1;
		TotalUnder15.out[CurrSim - 1][iy] = TotInfants.out[CurrSim - 1][iy] +
			Children1to2.out[CurrSim - 1][iy] + Children3to5.out[CurrSim - 1][iy] +
			Children6to13.out[CurrSim - 1][iy] + TotalPop_S[14][1] + TotalPop_S[14][1];
		Temp1 = 0.0;
		for (ia = 15; ia < 20; ia++){ Temp1 += TotalPop_S[ia][0] + TotalPop_S[ia][1]; }
		Adolesc15to19.out[CurrSim - 1][iy] = Temp1;
		for (ia = 20; ia < 25; ia++){ Temp1 += TotalPop_S[ia][0] + TotalPop_S[ia][1]; }
		Total15to24.out[CurrSim - 1][iy] = Temp1;
		Temp1 = 0.0;
		AgingIndex.out[CurrSim - 1][iy] = (MalesOver15.out[CurrSim - 1][iy] + FemalesOver15.out[CurrSim - 1][iy] -
			Males15to64.out[CurrSim - 1][iy] - Females15to64.out[CurrSim - 1][iy]) / TotalUnder15.out[CurrSim - 1][iy];
		Temp1 = 0.0;
		Temp2 = 0.0;
		Temp3 = 0.0;
		Temp4 = 0.0;
		for (ia = 15; ia < 25; ia++){
			Temp1 += TotalPositive_S[ia][0];
			Temp2 += TotalPositive_S[ia][1];
			Temp3 += TotalPop_S[ia][0];
			Temp4 += TotalPop_S[ia][1];
		}
		Prev15to24M.out[CurrSim - 1][iy] = Temp1 / Temp3;
		Prev15to24F.out[CurrSim - 1][iy] = Temp2 / Temp4;
		Total15to24M.out[CurrSim - 1][iy] = Temp3;
		Total15to24F.out[CurrSim - 1][iy] = Temp4;
		TotHIV15to24M.out[CurrSim - 1][iy] = Temp1;
		TotHIV15to24F.out[CurrSim - 1][iy] = Temp2;
		for (ia = 25; ia < 50; ia++){
			Temp1 += TotalPositive_S[ia][0];
			Temp2 += TotalPositive_S[ia][1];
			Temp3 += TotalPop_S[ia][0];
			Temp4 += TotalPop_S[ia][1];
		}
		Prev15to49M.out[CurrSim - 1][iy] = Temp1 / Temp3;
		Prev15to49F.out[CurrSim - 1][iy] = Temp2 / Temp4;
		Total15to49M.out[CurrSim - 1][iy] = Temp3;
		Total15to49F.out[CurrSim - 1][iy] = Temp4;
		TotHIV15to49M.out[CurrSim - 1][iy] = Temp1;
		TotHIV15to49F.out[CurrSim - 1][iy] = Temp2;
		Total25to49M.out[CurrSim - 1][iy] = Temp3 - Total15to24M.out[CurrSim - 1][iy];
		Total25to49F.out[CurrSim - 1][iy] = Temp4 - Total15to24F.out[CurrSim - 1][iy];
		Total25to49.out[CurrSim - 1][iy] = Total25to49M.out[CurrSim - 1][iy] + Total25to49F.out[CurrSim - 1][iy];
		TotHIV25to49M.out[CurrSim - 1][iy] = Temp1 - TotHIV15to24M.out[CurrSim - 1][iy];
		TotHIV25to49F.out[CurrSim - 1][iy] = Temp2 - TotHIV15to24F.out[CurrSim - 1][iy];
		TotHIV25to49.out[CurrSim - 1][iy] = TotHIV25to49M.out[CurrSim - 1][iy] + TotHIV25to49F.out[CurrSim - 1][iy];
		Prev25to49M.out[CurrSim - 1][iy] = TotHIV25to49M.out[CurrSim - 1][iy] / Total25to49M.out[CurrSim - 1][iy];
		Prev25to49F.out[CurrSim - 1][iy] = TotHIV25to49F.out[CurrSim - 1][iy] / Total25to49F.out[CurrSim - 1][iy];
		Prev25to49.out[CurrSim - 1][iy] = TotHIV25to49.out[CurrSim - 1][iy] / Total25to49.out[CurrSim - 1][iy];
		for (is = 0; is<4; is++){
			TempCD4[is] = 0.0;}
		for (ia = 5; ia < 81; ia++){
			TempCD4[0] = SumGroupsM[ia][5] + SumGroupsM[ia][10] + SumGroupsM[ia][15];
			for (is = 0; is < 4; is++){
				TempCD4[is] += SumGroupsM[ia][is+6] + SumGroupsM[ia][is+11] +
					SumGroupsM[ia][is+16];
			}
		}
		PreARTover500M.out[CurrSim - 1][iy] = TempCD4[0];
		PreART350to499M.out[CurrSim - 1][iy] = TempCD4[1];
		PreART200to349M.out[CurrSim - 1][iy] = TempCD4[2];
		PreARTunder200M.out[CurrSim - 1][iy] = TempCD4[3];
		for (is = 0; is<4; is++){
			TempCD4[is] = 0.0;}
		for (ia = 5; ia < 81; ia++){
			TempCD4[0] = SumGroupsF[ia][5] + SumGroupsF[ia][10] + SumGroupsF[ia][15];
			for (is = 0; is < 4; is++){
				TempCD4[is] += SumGroupsF[ia][is + 6] + SumGroupsF[ia][is + 11] +
					SumGroupsF[ia][is + 16];
			}
		}
		PreARTover500F.out[CurrSim - 1][iy] = TempCD4[0];
		PreART350to499F.out[CurrSim - 1][iy] = TempCD4[1];
		PreART200to349F.out[CurrSim - 1][iy] = TempCD4[2];
		PreARTunder200F.out[CurrSim - 1][iy] = TempCD4[3];
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 15; ia < 91; ia++){
			Temp1 += TotalInterrupt_S[ia][0];
			Temp2 += TotalInterrupt_S[ia][1];
		}
		DiscontinuedART_M.out[CurrSim - 1][iy] = Temp1;
		DiscontinuedART_F.out[CurrSim - 1][iy] = Temp2;
		TotEarlyInfants.out[CurrSim - 1][iy] = TotalPositive[0][1] + TotalPositive[0][1] -
			TotalNaiveElig_S[0][0] - TotalNaiveElig_S[0][1] - TotalART_S[0][0] -
			TotalART_S[0][1] - TotalInterrupt_S[0][0] - TotalInterrupt_S[0][1];
		Temp1 = 0.0;
		for (ia = 1; ia < 5; ia++){
			 Temp1 += TotalPositive[ia][1] + TotalPositive[ia][1] -
				TotalNaiveElig_S[ia][0] - TotalNaiveElig_S[ia][1] - TotalART_S[ia][0] -
				TotalART_S[ia][1] - TotalInterrupt_S[ia][0] - TotalInterrupt_S[ia][1];
		}
		TotEarly1to4.out[CurrSim - 1][iy] = Temp1;
		Temp1 = 0.0;
		for (ia = 0; ia < 15; ia++){
			Temp1 += TotalNaiveElig_S[ia][0] + TotalNaiveElig_S[ia][1] +
				TotalInterrupt_S[ia][0] + TotalInterrupt_S[ia][1];
		}
		TotLateUnder15.out[CurrSim - 1][iy] = Temp1;
		TotalSex = 0.0;
		for (ia = 0; ia < 81; ia++){
			TotalSex += (CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][2][1] * RRpartnerLow[1] +
				CurrBehavDbn[ia][1][1] * RRpartnerMarried[1]) * PartnerRate20F * PartnerAcqF[ia] *
				SexActsST;
			if (ia>5){
				TotalSex += (CurrBehavDbn[ia][1][1] + CurrBehavDbn[ia][3][1]) *
					SexFreqMarital[ia-5][1] * 12.0;
			}
		}
		TotalSex += TotalFSW * ClientsPA;
		TotSexActs.out[CurrSim - 1][iy] = TotalSex;
		SWsexActs.out[CurrSim - 1][iy] = TotalFSW * ClientsPA;
		// TotalSexProt is a crude approximation, ignoring effect of HCT on condom use
		TotalSexProt = 0.0;
		for (ia = 0; ia < 81; ia++){
			TotalSexProt += (CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][2][1] * RRpartnerLow[1] +
				CurrBehavDbn[ia][1][1] * RRpartnerMarried[1]) * PartnerRate20F * PartnerAcqF[ia] *
				SexActsST * ProbCondomST[ia][1];
			if (ia>5){
				TotalSexProt += (CurrBehavDbn[ia][1][1] + CurrBehavDbn[ia][3][1]) *
					SexFreqMarital[ia - 5][1] * 12.0 * ProbCondomLT[ia][1];
			}
			if (ia == 8){
				TotalSexProt18 = TotalSexProt;
			}
		}
		TotalSexProt += TotalFSW * ClientsPA * ProbCondomFSW;
		TotProtSexActs.out[CurrSim - 1][iy] = TotalSexProt;
		TotProtSexActs18.out[CurrSim - 1][iy] = TotalSexProt18;
		SWsexActsProt.out[CurrSim - 1][iy] = TotalFSW * ClientsPA * ProbCondomFSW;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 81; ia++){
			Temp1 += (MHC_virgin.Total[ia] + MHC_ST.Total[ia] + MHC_STM.Total[ia] +
				MHC_LTH.Total[ia] + MHC_LTL.Total[ia] + MLC_virgin.Total[ia] + MLC_ST.Total[ia] +
				MLC_STM.Total[ia] + MLC_LTH.Total[ia] + MLC_LTL.Total[ia]);
			Temp2 += TotalPop_S[ia + 10][0];
			if (ia == 14){
				Circumcised15to24.out[CurrSim - 1][iy] = Temp1 / Temp2;
			}
			if (ia == 39){
				Circumcised15to49.out[CurrSim - 1][iy] = Temp1 / Temp2; }
		}
		Circumcised15plus.out[CurrSim - 1][iy] = Temp1 / Temp2;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (is = 0; is < 4; is++){ TempCD4[is] = 0.0; }
		for (ia = 0; ia < 36; ia++){
			Temp1 += BirthsByHIVstage[ia][7];
			Temp2 += BirthsByHIVstage[ia][6];
			for (is = 0; is < 4; is++){ TempCD4[is] += BirthsByHIVstage[ia][2+is]; }
		}
		BirthsDiagHIV.out[CurrSim - 1][iy] = Temp1 / (Temp2 + TempCD4[0] + TempCD4[1] +
			TempCD4[2] + TempCD4[3]);
		BirthsOver500.out[CurrSim - 1][iy] = TempCD4[0];
		Births350to499.out[CurrSim - 1][iy] = TempCD4[1];
		Births200to349.out[CurrSim - 1][iy] = TempCD4[2];
		BirthsUnder200.out[CurrSim - 1][iy] = TempCD4[3];
		TotSexWorkers.out[CurrSim - 1][iy] = TotalFSW;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (ia = 5; ia < 81; ia++){
			Temp1 += MHC_LTH.PosDiagnosedPreART[ia][1] + MHU_LTH.PosDiagnosedPreART[ia][1] +
				MHC_LTL.PosDiagnosedPreART[ia][1] + MHU_LTL.PosDiagnosedPreART[ia][1] +
				MLC_LTH.PosDiagnosedPreART[ia][1] + MLU_LTH.PosDiagnosedPreART[ia][1] +
				MLC_LTL.PosDiagnosedPreART[ia][1] + MLU_LTL.PosDiagnosedPreART[ia][1] +
				FH_LTH.PosDiagnosedPreART[ia][1] + FL_LTH.PosDiagnosedPreART[ia][1] +
				FH_LTL.PosDiagnosedPreART[ia][1] + FL_LTL.PosDiagnosedPreART[ia][1];
			for (is = 1; is < 5; is++){
				Temp2 += MHC_LTH.PosDiagnosedPreART[ia][is] + MHU_LTH.PosDiagnosedPreART[ia][is] +
					MHC_LTL.PosDiagnosedPreART[ia][is] + MHU_LTL.PosDiagnosedPreART[ia][is] +
					MLC_LTH.PosDiagnosedPreART[ia][is] + MLU_LTH.PosDiagnosedPreART[ia][is] +
					MLC_LTL.PosDiagnosedPreART[ia][is] + MLU_LTL.PosDiagnosedPreART[ia][is] +
					FH_LTH.PosDiagnosedPreART[ia][is] + FL_LTH.PosDiagnosedPreART[ia][is] +
					FH_LTL.PosDiagnosedPreART[ia][is] + FL_LTL.PosDiagnosedPreART[ia][is];
			}
		}
		//DiscordantARTelig.out[CurrSim-1][iy] = Temp1 * DiscordantPropn;
		//DiscordantPrEPelig.out[CurrSim - 1][iy] = Temp2 * DiscordantPropn;
		for (is = 0; is < 4; is++){
			TempCD4[is] = 0.0;
		}
		for (ia = 5; ia < 81; ia++){
			for (is = 0; is<4; is++){
				for (id = 1; id<5; id++){
					for (ii = 0; ii < 4; ii++){
						TempCD4[ii] += (SumGroupsM[ia][20 + is * 5 + id] * OnARThalfIntDur[id][0] +
							SumGroupsF[ia][20 + is * 5 + id] * OnARThalfIntDur[id][1]) * CD4dbnNonIntDur[ii][3 - is][id];
					}
				}
			}
		}
		OnARTover500.out[CurrSim - 1][iy] = TempCD4[0];
		OnART350to499.out[CurrSim - 1][iy] = TempCD4[1];
		OnART200to349.out[CurrSim - 1][iy] = TempCD4[2];
		OnARTunder200.out[CurrSim - 1][iy] = TempCD4[3];
		for (is = 0; is < 4; is++){
			TempCD4[is] = 0.0;
		}
		for (ia = 5; ia < 81; ia++){
			for (is = 0; is<4; is++){
				for (id = 1; id<5; id++){
					for (ii = 0; ii < 4; ii++){
						TempCD4[ii] += (SumGroupsM[ia][20 + is * 5 + id] * (1.0 - OnARThalfIntDur[id][0]) +
							SumGroupsF[ia][20 + is * 5 + id] * (1.0 - OnARThalfIntDur[id][1])) *
							CD4dbnNonIntDur[ii][3 - is][id];
					}
				}
			}
		}
		DiscARTover500.out[CurrSim - 1][iy] = TempCD4[0];
		DiscART350to499.out[CurrSim - 1][iy] = TempCD4[1];
		DiscART200to349.out[CurrSim - 1][iy] = TempCD4[2];
		DiscARTunder200.out[CurrSim - 1][iy] = TempCD4[3];
		Temp1 = 0.0;
		for (ia = 5; ia < 10; ia++){
			Temp1 += SumGroupsM[ia][2] * FreqRegHCT[0] + SumGroupsF[ia][2] * FreqRegHCT[1];
		}
		AdolRegTests.out[CurrSim - 1][iy] = Temp1;
		Temp1 = 0.0;
		for (ia = 2; ia < 36; ia++){
			Temp1 += (MaleChild.AwareMatMF[ia] + FemChild.AwareMatMF[ia] + MaleChild.AwareMatEBF[ia] +
				FemChild.AwareMatEBF[ia]) * ExtNVProllout[iy];
		}
		ChildrenOnExtNVP.out[CurrSim - 1][iy] = Temp1;
	}

	if (FixedUncertainty == 1){ Get2ndLineOutput(); }
	if (CalibMarriageData == 1 || FixedUncertainty == 1){ GetMarriageForCalib(); }
}

void Get2ndLineOutput()
{
	double Temp1, Temp2, SecondLine[4][16], NewART[4][16];
	int ia, is, id;

	// Adult calculations
	if (CurrYear >= 2002){
		for (id = 3; id < 16; id++){
			NewART[3][id] = NewARTunder200.out[CurrSim - 1][CurrYear - StartYear - id - 1];
			NewART[2][id] = NewART200to349.out[CurrSim - 1][CurrYear - StartYear - id - 1];
			NewART[1][id] = NewART350to499.out[CurrSim - 1][CurrYear - StartYear - id - 1];
			NewART[0][id] = NewARTover500.out[CurrSim - 1][CurrYear - StartYear - id - 1];
		}
	}
	else{
		for (id = 3; id < 16; id++){
			for (is = 0; is < 4; is++){
				NewART[3][id] = 0.0;}
		}
	}
	for (is = 0; is < 4; is++){
		SecondLine[is][0] = 0.0;
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (id = 1; id < 16; id++){
			SecondLine[is][id] = 1.0 - exp(-SwitchAdult[is] * id);}
		if (CurrYear >= 2002){
			for (id = 3; id < 16; id++){
				Temp1 += NewART[is][id];
				Temp2 += NewART[is][id] * SecondLine[is][id];
			}
			if (Temp1 > 0.0){ SecondLine[is][3] = Temp2 / Temp1; }
			else{ SecondLine[is][3] = 0.0; }
		}
		else{ SecondLine[is][3] = 0.0; }
	}

	for (ia = 10; ia < 91; ia++){
		for (is = 0; is<4; is++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for (id = 1; id<5; id++){
				if (ExcludeInterrupters == 1){
					Temp1 += SumGroupsM[ia - 10][20 + is * 5 + id] * OnARThalfIntDur[id][0] * SecondLine[is][id-1];
					Temp2 += SumGroupsF[ia - 10][20 + is * 5 + id] * OnARThalfIntDur[id][1] * SecondLine[is][id-1];
				}
				else{
					Temp1 += SumGroupsM[ia - 10][20 + is * 5 + id] * SecondLine[is][id];
					Temp2 += SumGroupsF[ia - 10][20 + is * 5 + id] * SecondLine[is][id];
				}
			}
		}
		TotalART2ndL[ia][0] = Temp1;
		TotalART2ndL[ia][1] = Temp2;
	}

	Temp1 = 0.0;
	Temp2 = 0.0;
	for (ia = 15; ia < 91; ia++){
		Temp1 += TotalART2ndL[ia][0];
		Temp2 += TotalART2ndL[ia][1];
	}
	TotalART15M2L.out[CurrSim - 1][CurrYear - StartYear] = Temp1;
	TotalART15F2L.out[CurrSim - 1][CurrYear - StartYear] = Temp2;

	// Paediatric calculations
	for (is = 0; is < 2; is++){
		SecondLine[is][0] = 0.0;
		for (id = 1; id < 5; id++){
			SecondLine[0][id] = 1.0 - exp(-SwitchPaed[is] * id);}
	}

	for (ia = 0; ia < 10; ia++){
		Temp1 = 0.0;
		Temp2 = 0.0;
		for (id = 0; id<5; id++){
			if (ExcludeInterrupters == 1){
				Temp1 += OnARThalfIntDurP[id] * (CumPaedARTearly[ia][id] * SecondLine[0][id] + CumPaedARTlate[ia][id] * SecondLine[1][id]);
				Temp2 += OnARThalfIntDurP[id] * (CumPaedARTearly[ia][id] + CumPaedARTlate[ia][id]);
			}
			else{
				Temp1 += CumPaedARTearly[ia][id] * SecondLine[0][id] + CumPaedARTlate[ia][id] * SecondLine[1][id];
				Temp2 += CumPaedARTearly[ia][id] + CumPaedARTlate[ia][id];
			}
		}
		TotalART2ndL[ia][0] = TotalART[ia][0] * Temp1 / Temp2;
		TotalART2ndL[ia][1] = TotalART[ia][1] * Temp1 / Temp2;
	}

	Temp1 = 0.0;
	for (ia = 0; ia < 15; ia++){
		Temp1 += TotalART2ndL[ia][0] + TotalART2ndL[ia][1];
	}
	TotalARTunder15_2L.out[CurrSim - 1][CurrYear - StartYear] = Temp1;
}

void GetMarriageForCalib()
{
	int ia, ii, ig, iy;
	double Temp1, Temp2;

	if (CurrYear == 1996 || CurrYear == 2001 || CurrYear == 2007 || CurrYear == 2016){
		if (CurrYear == 1996){ iy = 0; }
		if (CurrYear == 2001){ iy = 1; }
		if (CurrYear == 2007){ iy = 2; }
		if (CurrYear == 2016){ iy = 3; }

		for (ig = 0; ig < 2; ig++){
			for (ia = 0; ia < 15; ia++){
				Temp1 = 0.0;
				Temp2 = 0.0;
				for (ii = 15 + ia * 5; ii < 20 + ia * 5; ii++){
					Temp1 += TotalMarried[ii][ig];
					Temp2 += TotalPop[ii][ig];
				}
				if (ia == 14){
					Temp1 += TotalMarried[90][ig];
					Temp2 += TotalPop[90][ig];
				}
				ModelMarried[ia][ig][iy] = Temp1 / Temp2;
			}
		}

		if (FixedUncertainty == 1){
			for (ig = 0; ig < 2; ig++){
				for (ia = 0; ia < 15; ia++){
					if (iy == 0){ MarriedPropn1996.out[CurrSim - 1][15 * ig + ia] =
						ModelMarried[ia][ig][iy]; }
					if (iy == 1){ MarriedPropn2001.out[CurrSim - 1][15 * ig + ia] =
						ModelMarried[ia][ig][iy]; }
					if (iy == 2){ MarriedPropn2007.out[CurrSim - 1][15 * ig + ia] =
						ModelMarried[ia][ig][iy]; }
					if (iy == 3){ MarriedPropn2016.out[CurrSim - 1][15 * ig + ia] =
						ModelMarried[ia][ig][iy]; }
				}
			}
		}
	}
}

void CalcMultPartners()
{
	int ia, ig, iy;
	double lambda, numerator, denominator;

	iy = CurrYear - StartYear;

	numerator = 0.0;
	denominator = 0.0;
	for (ia = 5; ia < 15; ia++){
		lambda = PartnerAcqM[ia];
		denominator += CurrBehavDbn[ia][0][0] + CurrBehavDbn[ia][1][0] +
			CurrBehavDbn[ia][2][0] + CurrBehavDbn[ia][3][0];
		numerator += CurrBehavDbn[ia][0][0] * (1.0 - exp(-lambda) * (1.0 + lambda));
		numerator += CurrBehavDbn[ia][1][0] * (1.0 - exp(-lambda * RRpartnerMarried[0]) *
			(1.0 + lambda * RRpartnerMarried[0]));
		numerator += CurrBehavDbn[ia][2][0] * (1.0 - exp(-lambda * RRpartnerLow[0]) *
			(1.0 + lambda * RRpartnerLow[0]));
	}
	MultPartners15to24M.out[CurrSim - 1][iy] = numerator / denominator;

	numerator = 0.0;
	denominator = 0.0;
	for (ia = 5; ia < 15; ia++){
		lambda = PartnerAcqF[ia];
		denominator += CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][1][1] +
			CurrBehavDbn[ia][2][1] + CurrBehavDbn[ia][3][1];
		numerator += CurrBehavDbn[ia][0][1] * (1.0 - exp(-lambda) * (1.0 + lambda));
		numerator += CurrBehavDbn[ia][1][1] * (1.0 - exp(-lambda * RRpartnerMarried[1]) *
			(1.0 + lambda * RRpartnerMarried[1]));
		numerator += CurrBehavDbn[ia][2][1] * (1.0 - exp(-lambda * RRpartnerLow[1]) *
			(1.0 + lambda * RRpartnerLow[1]));
	}
	MultPartners15to24F.out[CurrSim - 1][iy] = numerator / denominator;

	numerator = 0.0;
	denominator = 0.0;
	for (ia = 15; ia < 40; ia++){
		lambda = PartnerAcqM[ia];
		denominator += CurrBehavDbn[ia][0][0] + CurrBehavDbn[ia][1][0] +
			CurrBehavDbn[ia][2][0] + CurrBehavDbn[ia][3][0];
		numerator += CurrBehavDbn[ia][0][0] * (1.0 - exp(-lambda) * (1.0 + lambda));
		numerator += CurrBehavDbn[ia][1][0] * (1.0 - exp(-lambda * RRpartnerMarried[0]) *
			(1.0 + lambda * RRpartnerMarried[0]));
		numerator += CurrBehavDbn[ia][2][0] * (1.0 - exp(-lambda * RRpartnerLow[0]) *
			(1.0 + lambda * RRpartnerLow[0]));
	}
	MultPartners25to49M.out[CurrSim - 1][iy] = numerator / denominator;

	numerator = 0.0;
	denominator = 0.0;
	for (ia = 15; ia < 40; ia++){
		lambda = PartnerAcqF[ia];
		denominator += CurrBehavDbn[ia][0][1] + CurrBehavDbn[ia][1][1] +
			CurrBehavDbn[ia][2][1] + CurrBehavDbn[ia][3][1];
		numerator += CurrBehavDbn[ia][0][1] * (1.0 - exp(-lambda) * (1.0 + lambda));
		numerator += CurrBehavDbn[ia][1][1] * (1.0 - exp(-lambda * RRpartnerMarried[1]) *
			(1.0 + lambda * RRpartnerMarried[1]));
		numerator += CurrBehavDbn[ia][2][1] * (1.0 - exp(-lambda * RRpartnerLow[1]) *
			(1.0 + lambda * RRpartnerLow[1]));
	}
	MultPartners25to49F.out[CurrSim - 1][iy] = numerator / denominator;
}

void GetPrEPrateFSW()
{
	int ia, ig, ii, iy;
	double PrEPeligFSW, PrEPeligMSM[81][2], PrEPeligOther[81][2][2], temp, temp2, tempMSM;

	PrEPeligFSW = 0.0;
	for (ia = 0; ia < 81; ia++){
		PrEPeligFSW += FH_SW.NegNoHCT[ia] + FH_SW.NegPastHCT[ia];
		PrEPeligMSM[ia][0] = MHU_STM.NegNoHCT[ia] + MHU_STM.NegPastHCT[ia] +
			MHC_STM.NegNoHCT[ia] + MHC_STM.NegPastHCT[ia];
		PrEPeligMSM[ia][1] = MLU_STM.NegNoHCT[ia] + MLU_STM.NegPastHCT[ia] +
			MLC_STM.NegNoHCT[ia] + MLC_STM.NegPastHCT[ia];
		PrEPeligOther[ia][0][0] = MHU_ST.NegNoHCT[ia] + MHU_ST.NegPastHCT[ia] +
			MHC_ST.NegNoHCT[ia] + MHC_ST.NegPastHCT[ia] + MHU_LTH.NegNoHCT[ia] +
			MHU_LTH.NegPastHCT[ia] + MHC_LTH.NegNoHCT[ia] + MHC_LTH.NegPastHCT[ia] +
			MHU_LTL.NegNoHCT[ia] + MHU_LTL.NegPastHCT[ia] + MHC_LTL.NegNoHCT[ia] +
			MHC_LTL.NegPastHCT[ia];
		PrEPeligOther[ia][1][0] = MLU_ST.NegNoHCT[ia] + MLU_ST.NegPastHCT[ia] +
			MLC_ST.NegNoHCT[ia] + MLC_ST.NegPastHCT[ia] + MLU_LTH.NegNoHCT[ia] +
			MLU_LTH.NegPastHCT[ia] + MLC_LTH.NegNoHCT[ia] + MLC_LTH.NegPastHCT[ia] +
			MLU_LTL.NegNoHCT[ia] + MLU_LTL.NegPastHCT[ia] + MLC_LTL.NegNoHCT[ia] +
			MLC_LTL.NegPastHCT[ia];
		PrEPeligOther[ia][0][1] = FH_ST.NegNoHCT[ia] + FH_ST.NegPastHCT[ia] +
			FH_LTH.NegNoHCT[ia] + FH_LTH.NegPastHCT[ia] + FH_LTL.NegNoHCT[ia] +
			FH_LTL.NegPastHCT[ia];
		PrEPeligOther[ia][1][1] = FL_ST.NegNoHCT[ia] + FL_ST.NegPastHCT[ia] +
			FL_LTH.NegNoHCT[ia] + FL_LTH.NegPastHCT[ia] + FL_LTL.NegNoHCT[ia] +
			FL_LTL.NegPastHCT[ia];
	}

	iy = CurrYear - StartYear;
	if (TotStartingPrEP[iy] > 0.0){
		temp = PrEPeligFSW;
		for (ia = 0; ia < 81; ia++){
			for (ii = 0; ii < 2; ii++){
				temp += PrEPeligMSM[ia][ii] * RR_PrEPstartMSM[iy] * RR_PrEP_MSM[ia][ii];
				for (ig = 0; ig < 2; ig++){
					temp += PrEPeligOther[ia][ii][ig] * RR_PrEPstartF20[iy] *
						RR_PrEP_Het[ia][ii][ig];
				}
			}
		}
		CurrPrEPrateFSW = (TotStartingPrEP[iy] / 12.0) / temp;
		if (CurrYear == PrEPdataYr){ StoredPrEPrateFSW = CurrPrEPrateFSW; }
	}

	// Calculate PrEP initiation and coverage outputs
	if (FixedUncertainty == 1){
		NewPrEPrateFSW.out[CurrSim - 1][iy] = CurrPrEPrateFSW;
		if (PrEPeligFSW > 0.0){ PrEPcoverageFSW.out[CurrSim - 1][iy] = FSWonPrEP.out[CurrSim - 1][iy] / PrEPeligFSW; }
		tempMSM = 0.0;
		for (ia = 0; ia < 81; ia++){ tempMSM += PrEPeligMSM[ia][0] + PrEPeligMSM[ia][1]; }
		if (tempMSM > 0.0){ PrEPcoverageMSM.out[CurrSim - 1][iy] = MSMonPrEP.out[CurrSim - 1][iy] / tempMSM; 
		PrEPeligibleMSM.out[CurrSim - 1][iy] = tempMSM;}
		temp = 0.0;
		for (ia = 0; ia < 15; ia++){ temp += PrEPeligOther[ia][0][1] + PrEPeligOther[ia][1][1]; }
		if (temp > 0.0){ PrEPcoverageAGYW.out[CurrSim - 1][iy] = AGYWonPrEP.out[CurrSim - 1][iy] / temp; }
		for (ia = 15; ia < 81; ia++){ temp += PrEPeligOther[ia][0][1] + PrEPeligOther[ia][1][1]; }
		if (PrEPeligFSW > 0.0){ PrEPcoverageAllF.out[CurrSim - 1][iy] = WomenOnPrEP.out[CurrSim - 1][iy] / (temp + PrEPeligFSW); }
		temp2 = 0.0;
		for (ia = 0; ia < 81; ia++){ temp2 += PrEPeligOther[ia][0][0] + PrEPeligOther[ia][1][0]; }
		if (tempMSM > 0.0){ PrEPcoverageAllM.out[CurrSim - 1][iy] = MenOnPrEP.out[CurrSim - 1][iy] / (temp2 + tempMSM); }
		if (PrEPeligFSW > 0.0){
			PrEPcoverageAll.out[CurrSim - 1][iy] = (WomenOnPrEP.out[CurrSim - 1][iy] + MenOnPrEP.out[CurrSim - 1][iy]) /
				(temp + PrEPeligFSW + temp2 + tempMSM);
		}
	}
}

void SetCurrYearParameters()
{
	double temp, temp2, MaxMinDif1, MaxMinDif2;
	int ic, ia, im, iy;

	iy = CurrYear - StartYear;

	// Calculate the parameters in the 'Rollout' sheet
	if (UseNumbersTests == 1 && (VaryFutureInterventions==0 || CurrYear<2015)){
		HCT1stTimeF25[iy] = HCT1stTimeF25init[iy];
	}
	//if (CurrYear > 2011){
	//	NumbersTested[iy] = 10000000;}
	PCRuptake = PCR6week[iy];
	PCRuptakeB = PCRbirth[iy];
	VCTuptake = PregnantWomenTested[iy];
	//VCTuptake = 0.0;
	AZTpropn = AZTrollout[iy];
	//if (CurrYear>2011){ AZTpropn = AZTrollout[26]; }
	RescreenLate = RescreenPropnLate[iy];
	RescreenImm = RescreenPropnImm[iy];
	ExtNVPpropn = ExtNVProllout[iy];
	//if (CurrYear>2011){ ExtNVPpropn = ExtNVProllout[26]; }
	//RescreenLate = 0.0;
	//RescreenImm = 0.0;
	//ExtNVPpropn = 0.0;
	OptionB = EligibleOptionB[iy];
	//OptionB = 0.0;
	FFpropn = NoBFpropn[iy];
	SwitchingToFF = NoBFpropn[iy];
	/*if (CurrYear>2010){
		FFpropn = NoBFpropn[25];
		SwitchingToFF = NoBFpropn[25];
	}*/
	EverFeedCurr[0] = EverFeed[0];
	EverFeedCurr[1] = (EverFeed[1]/(EverFeed[1] + EverFeed[2])) * (1.0 - FFpropn);
	EverFeedCurr[2] = (EverFeed[2]/(EverFeed[1] + EverFeed[2])) * (1.0 - FFpropn);
	//EverFeedCurr[1] = EverFeedCurr[0];
	//EverFeedCurr[2] = 0.0;

	StartingART_M = NumStartingART_M[iy];
	StartingART_F = NumStartingART_F[iy];
	StartingART_P = NumStartingART_P[iy];
	//StartingART_M = 0.0;
	//StartingART_F = 0.0;
	//StartingART_P = 0.0;
	EligInfants = EligibleInfants[iy];
	MatARTpropn = MatARTuptake[iy];
	PaedARTuptakeC = PaedARTuptake[iy] * ImmARTcorrectionP;
	OIstart = OI_ARTuptake[iy];
	AsymStart[0] = HCT_ARTuptake[iy];
	FSWreduction = CondomFSWreduction[iy];
	STreduction = CondomSTreduction[iy];
	LTreduction = CondomLTreduction[iy];
	//EligInfants = 0.0;
	//MatARTpropn = 0.0;
	//PaedARTuptakeC = 0.0;
	//OIstart = 0.0;
	//AsymStart[0] = 0.0;
	AsymStart[1] = AsymStart[0] * RR_ARTstartM;
	if (CurrYear > StartYear + 10){
		CurrCircPrev10 = 1.0 - (1.0 - NeonatalMMC[iy - 10]) * (1.0 -
			InitCircumcised[10]) /(1.0 - CircPrevBirth);
	}
	else{ CurrCircPrev10 = InitCircumcised[10]; }
	//MMCoperations[CurrYear-StartYear] = 0.0;

	//HCT1stTimeF25[CurrYear-StartYear] = 0.0;
	//PCRuptake = 0.0;
	//PCRuptakeB = 0.0;
	//VCTuptake = 0.0;
	//RescreenLate = 0.0;
	//OIsDiagnosed[CurrYear-StartYear] = 0.000001;
	//OIsTested[CurrYear-StartYear] = 0.000001;
	//if (UseNumbersTests == 1){ NumbersTested[CurrYear - StartYear] = 0.0; }

	// 18-mo testing coverage in HIV+ undiagnosed is higher than crude coverage:
	CurrTesting18mo[0] = TestingAt18mo[iy];
	CurrTesting18mo[1] = TestingAt18mo[iy] * (1.0 + 4.23 * exp(-8.41 * TestingAt18mo[iy]));

	// Viral suppression
	CurrSuppression200 = 1.0 / (1.0 + (1.0 - VLsuppression200[iy]) / (VLsuppression200[iy] * ORsuppressionIeDEA));
	if (VaryFutureInterventions == 1 && CurrYear>2012){
		if (CurrYear >= 2017){ CurrSuppression200 = FutureInterventions.out[CurrSim - 1][5]; }
		else{
			CurrSuppression200 = VLsuppression200[2012 - StartYear] + 0.2 * (CurrYear - 2012) *
				(FutureInterventions.out[CurrSim - 1][5] - VLsuppression200[2012 - StartYear]);
		}
	}

	// Calculate TransmART200 and TransmARTpre200
	if (IncreaseARTdurPreg[iy] == 0.0){
		TransmART200 = InitTransmART200;
		TransmARTpre200 = InitTransmARTpre200;
	}
	else{
		temp = MeanARTdurPreg / (SD_ARTdurPreg * SD_ARTdurPreg); // lambda
		temp2 = MeanARTdurPreg * temp; // alpha
		MaxMinDif1 = (InitTransmART200 - TransmARTprePreg) / pow(temp / (temp -
			log(RRtransmPerWeekART)), temp2);
		MaxMinDif2 = (InitTransmARTpre200 - TransmARTprePreg) / pow(temp / (temp -
			log(RRtransmPerWeekART)), temp2);
		TransmART200 = TransmARTprePreg + MaxMinDif1 * pow(1.0 - log(RRtransmPerWeekART) *
			(1.0 + IncreaseARTdurPreg[CurrYear - StartYear]) / temp, -temp2);
		TransmARTpre200 = TransmARTprePreg + MaxMinDif2 * pow(1.0 - log(RRtransmPerWeekART) *
			(1.0 + IncreaseARTdurPreg[CurrYear - StartYear]) / temp, -temp2);
	}
	if (VaryFutureInterventions == 1){
		AbruptWeaningFirst3 = InitAbruptWeaning1;
		AbruptWeaningAfter3 = InitAbruptWeaning2;
		if (CurrYear > 2010){
			temp = MedianFeed[2];
			if (CurrYear >= 2013){ temp = FutureInterventions.out[CurrSim - 1][22]; }
			else{
				temp = MedianFeed[2] + ((CurrYear - 2010) / 3.0) *
					(FutureInterventions.out[CurrSim - 1][22] - MedianFeed[2]);
			}
			for (im = 0; im < 6; im++){
				PropnBF[im][2] = EverFeed[2] * pow(0.5, pow(im / temp, ShapeFeed[2]));}
			for (im = 0; im<6; im++){
				RateOfBFchange[im][2] = 1.0 - PropnBF[im + 1][2] / PropnBF[im][2];}
			if (CurrYear >= 2013){
				AbruptWeaningFirst3 = FutureInterventions.out[CurrSim - 1][23];
				AbruptWeaningAfter3 = FutureInterventions.out[CurrSim - 1][23];
			}
			else{
				AbruptWeaningFirst3 = InitAbruptWeaning1 + ((CurrYear - 2010)/3.0) *
					(FutureInterventions.out[CurrSim - 1][23] - InitAbruptWeaning1);
				AbruptWeaningAfter3 = InitAbruptWeaning2 + ((CurrYear - 2010) / 3.0) *
					(FutureInterventions.out[CurrSim - 1][23] - InitAbruptWeaning2);
			}
		}
	}

	// Set PrEPorVM
	if ((TotStartingPrEP[iy] > 0.0 || CurrYear > PrEPdataYr || PrEPpregnant[iy] > 0.0 || VM_FSW[iy] > 0.0 ||
		VM_15[iy] > 0.0 || VM_20[iy] > 0.0 || VM_25[iy] > 0.0 || VM_50[iy] > 0.0 || VMpregnant[iy] > 0.0 ||
		RegHCT_FSW[iy] > 0.0 || RegHCT_15[iy] > 0.0 || RegHCT_20[iy] > 0.0 || RegHCT_25[iy] > 0.0 ||
		RegHCT_50[iy] > 0.0 || RegHCTpregnant[iy] > 0.0) || (PrEPorVM == 1 && CurrYear>StartYear)){
		PrEPorVM = 1;
	}
	else{
		PrEPorVM = 0;
	}

	// Calculate OnARTpaed
	for (ia = 0; ia < 10; ia++){
		if (iy > 0){
			temp = 0.0;
			temp2 = 0.0;
			for (ic = 0; ic < 5; ic++){
				temp += CumPaedARTearly[ia][ic] + CumPaedARTlate[ia][ic];
				temp2 += (CumPaedARTearly[ia][ic] + CumPaedARTlate[ia][ic]) * OnARThalfIntDurP[ic];
			}
			if (temp > 0.0){
				OnARTpaed[ia] = temp2 / temp;}
			else{ OnARTpaed[ia] = 1.0; }
		}
		else{ OnARTpaed[ia] = 1.0; }
	}

	// Paediatric testing
	if (CurrYear <= 2005){ RRtestVirgin = RRtestVirginTrend[0]; }
	else if (CurrYear < 2010){
		RRtestVirgin = RRtestVirginTrend[0] + (RRtestVirginTrend[1] - RRtestVirginTrend[0]) *
			1.0 * (CurrYear - 2005) / 5.0;
	}
	else{ RRtestVirgin = RRtestVirginTrend[1]; }
	RRtestVirgin *= VirginTestAdjProv;

	// Retesting adjustments
	if (iy > 0){
		RetestAdj = RetestAdjInit + (RetestAdjMax - RetestAdjInit) * HCT1stTimeF25[iy - 1] / 0.5;
		if (RetestAdj < RetestAdjInit && RetestAdj < RetestAdjMax){
			RetestAdj = RetestAdjInit;
			if (RetestAdjInit > RetestAdjMax){ RetestAdj = RetestAdjMax; }
		}
		RetestPos = RetestPosInit * RetestAdj / RetestAdjInit;
		RetestART = RetestARTinit * RetestAdj / RetestAdjInit;
	}
}

void SetAnnPaedParameters()
{
	int ia, is;

	AveARTstartLate = 0.0;
	if (CurrYear>1985){ AveARTstartLate = RateARTstartLate[CurrYear - 1986]; }
	if (CurrYear>1986){ AveARTstartLate += RateARTstartLate[CurrYear - 1987]; }
	if (CurrYear > 1987){ AveARTstartLate += RateARTstartLate[CurrYear - 1988]; }
	AveARTstartLate = AveARTstartLate / 3.0;
	for (is = 0; is < 3; is++){
		MortAdjLate[is] = MinMortP[is] + (1.0 - MinMortP[is]) * exp(-RednLogMortP[is] *
			AveARTstartLate);
	}

	MaleChild.GetNonAIDSmort();
	FemChild.GetNonAIDSmort();
	for(ia=0; ia<36; ia++){
		AcuteMatExit[ia][0] = 1.0 - exp(-1.0/(12.0 * CD4duration[0])) * (1.0 -
			RateOfBFchange[ia][0]) * (1 - TransmBFacute);
		AcuteMatExit[ia][1] = (-1.0/(12.0 * CD4duration[0]))/log(exp(-1.0/(12.0 *
			CD4duration[0])) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
		AcuteMatExit[ia][2] = log(1.0 - RateOfBFchange[ia][0])/log(exp(-1.0/(12.0 *
			CD4duration[0])) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
		AcuteMatExit[ia][3] = log(1 - TransmBFacute)/log(exp(-1.0/(12.0 *
			CD4duration[0])) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
		if(ia==35){
			AcuteMatExit[ia][0] = 1.0;
			AcuteMatExit[ia][1] = 0.0;
			AcuteMatExit[ia][2] = 1.0;
			AcuteMatExit[ia][3] = 0.0;
		}
		if(ia<3){
			ChronicMatExit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFfirst3);
			ChronicMatExit[ia][1] = log(1.0 - RateOfBFchange[ia][0])/
				log((1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFfirst3));
			ChronicMatExit[ia][2] = log(1 - TransmBFfirst3)/
				log((1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFfirst3));
		}
		else{
			ChronicMatExit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFafter3);
			ChronicMatExit[ia][1] = log(1.0 - RateOfBFchange[ia][0])/
				log((1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFafter3));
			ChronicMatExit[ia][2] = log(1 - TransmBFfirst3)/
				log((1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFafter3));
		}
		if(ia==35){
			ChronicMatExit[ia][0] = 1.0;
			ChronicMatExit[ia][1] = 1.0;
			ChronicMatExit[ia][2] = 0.0;
		}
		if(ia<3){
			KnownMatMFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 *
				(1.0 - ExtNVPpropn * RednExtNVP));
			KnownMatMFexit[ia][1] = log(1.0 - RateOfBFchange[ia][1])/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatMFexit[ia][2] = log(1.0 - TransmBFfirst3 * (1.0 - ExtNVPpropn * RednExtNVP))/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 * (1.0 - ExtNVPpropn * RednExtNVP)));
		}
		else{
			KnownMatMFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 *
				(1.0 - ExtNVPpropn * RednExtNVP));
			KnownMatMFexit[ia][1] = log(1.0 - RateOfBFchange[ia][1])/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatMFexit[ia][2] = log(1.0 - TransmBFafter3 * (1.0 - ExtNVPpropn * RednExtNVP))/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 * (1.0 - ExtNVPpropn * RednExtNVP)));
		}
		if(ia==35){
			KnownMatMFexit[ia][0] = 1.0;
			KnownMatMFexit[ia][1] = 1.0;
			KnownMatMFexit[ia][2] = 0.0;
		}
		if(ia<3){
			KnownMatEBFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 *
				RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP));
			KnownMatEBFexit[ia][1] = log(1.0 - RateOfBFchange[ia][2]) * AbruptWeaningFirst3/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatEBFexit[ia][2] = log(1.0 - RateOfBFchange[ia][2]) * (1.0 - AbruptWeaningFirst3)/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatEBFexit[ia][3] = log(1.0 - TransmBFfirst3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP))/
				log((1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - ExtNVPpropn *
				RednExtNVP)));
		}
		else if(ia<5){
			KnownMatEBFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 *
				RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP));
			KnownMatEBFexit[ia][1] = log(1.0 - RateOfBFchange[ia][2]) * AbruptWeaningAfter3/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatEBFexit[ia][2] = log(1.0 - RateOfBFchange[ia][2]) * (1.0 - AbruptWeaningAfter3)/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP)));
			KnownMatEBFexit[ia][3] = log(1.0 - TransmBFafter3 * RRforEBF * (1.0 - ExtNVPpropn * RednExtNVP))/
				log((1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - ExtNVPpropn *
				RednExtNVP)));
		}
		if(ia==5){
			KnownMatEBFexit[ia][0] = 1.0;
			KnownMatEBFexit[ia][1] = AbruptWeaningAfter3;
			KnownMatEBFexit[ia][2] = 1.0 - AbruptWeaningAfter3;
			KnownMatEBFexit[ia][3] = 0.0;
		}
		if(ia<3){
			ARTmatMFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 *
				(1.0 - RednHAART));
			ARTmatMFexit[ia][1] = log(1.0 - RateOfBFchange[ia][1])/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 * (1.0 - RednHAART)));
			ARTmatMFexit[ia][2] = log(1.0 - TransmBFfirst3 * (1.0 - RednHAART))/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFfirst3 * (1.0 - RednHAART)));
		}
		else{
			ARTmatMFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 *
				(1.0 - RednHAART));
			ARTmatMFexit[ia][1] = log(1.0 - RateOfBFchange[ia][1])/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 * (1.0 - RednHAART)));
			ARTmatMFexit[ia][2] = log(1.0 - TransmBFafter3 * (1.0 - RednHAART))/log((1.0 -
				RateOfBFchange[ia][1]) * (1.0 - TransmBFafter3 * (1.0 - RednHAART)));
		}
		if(ia==35){
			ARTmatMFexit[ia][0] = 1.0;
			ARTmatMFexit[ia][1] = 1.0;
			ARTmatMFexit[ia][2] = 0.0;
		}
		if(ia<3){
			ARTmatEBFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 *
				RRforEBF * (1.0 - RednHAART));
			ARTmatEBFexit[ia][1] = log(1.0 - RateOfBFchange[ia][2]) * AbruptWeaningFirst3/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - RednHAART)));
			ARTmatEBFexit[ia][2] = log(1.0 - RateOfBFchange[ia][2]) * (1.0 - AbruptWeaningFirst3)/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - RednHAART)));
			ARTmatEBFexit[ia][3] = log(1.0 - TransmBFfirst3 * RRforEBF * (1.0 - RednHAART))/
				log((1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFfirst3 * RRforEBF * (1.0 - RednHAART)));
		}
		else if(ia<5){
			ARTmatEBFexit[ia][0] = 1.0 - (1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 *
				RRforEBF * (1.0 - RednHAART));
			ARTmatEBFexit[ia][1] = log(1.0 - RateOfBFchange[ia][2]) * AbruptWeaningAfter3/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - RednHAART)));
			ARTmatEBFexit[ia][2] = log(1.0 - RateOfBFchange[ia][2]) * (1.0 - AbruptWeaningAfter3)/log((1.0 -
				RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - RednHAART)));
			ARTmatEBFexit[ia][3] = log(1.0 - TransmBFafter3 * RRforEBF * (1.0 - RednHAART))/
				log((1.0 - RateOfBFchange[ia][2]) * (1.0 - TransmBFafter3 * RRforEBF * (1.0 - RednHAART)));
		}
		if(ia==5){
			ARTmatEBFexit[ia][0] = 1.0;
			ARTmatEBFexit[ia][1] = AbruptWeaningAfter3;
			ARTmatEBFexit[ia][2] = 1.0 - AbruptWeaningAfter3;
			ARTmatEBFexit[ia][3] = 0.0;
		}
	}
	for(ia=0; ia<132; ia++){
		ARTexitHR[ia][0] = 1.0 - exp(-1.0/3.0) * pow(1.0 - AIDSmortNoART[ia], RRmortART1 * MortAdjLate[1]) *
			exp(-Discontinuation1/12.0);
		ARTexitHR[ia][1] = log(pow(1.0 - AIDSmortNoART[ia], RRmortART1 * MortAdjLate[1])) / log(exp(-1.0 / 3.0) *
			pow(1.0 - AIDSmortNoART[ia], RRmortART1 * MortAdjLate[1]) * exp(-Discontinuation1 / 12.0));
		ARTexitHR[ia][2] = (-Discontinuation1/12.0)/log(exp(-1.0/3.0) *
			pow(1.0 - AIDSmortNoART[ia], RRmortART1 * MortAdjLate[1]) * exp(-Discontinuation1 / 12.0));
		ARTexitHR[ia][3] = (-1.0/3.0)/log(exp(-1.0/3.0) * pow(1.0 - AIDSmortNoART[ia],
			RRmortART1 * MortAdjLate[1]) * exp(-Discontinuation1 / 12.0));
	}
}

void CalcOIsTested()
{
	int iy, ia, is;
	double UndiagOIs, DiagnosedOIs, OIincidenceNeg;

	// Firstly calculate OIsTested from OIsDiagnosed. This is to take into account that
	// since 2005, the indicator reported is the % of TB patients who know their HIV
	// status (i.e. including those previously diagnosed), but we want to estimate the
	// % who received HIV testing at the OI treatment facility.

	iy = CurrYear - StartYear;
	UndiagOIs = 0.0; // OIs in people who are HIV-neg or undiagnosed
	DiagnosedOIs = 0.0; // OIs in people who were previously diagnosed
	OIincidenceNeg = 0.019; // Same value as in MicroCOSM
	if (CurrYear >= 2009 && OIsDiagnosed[iy]>0.0){
		for (ia = 0; ia < 81; ia++){
			UndiagOIs += (SumGroupsM[ia][0] + SumGroupsF[ia][0] +
				SumGroupsM[ia][1] + SumGroupsF[ia][1]) * OIincidenceNeg;
			for (is = 0; is < 5; is++){
				UndiagOIs += (SumGroupsM[ia][is + 5] + SumGroupsF[ia][is + 5] +
					SumGroupsM[ia][is + 10] + SumGroupsF[ia][is + 10]) *
					OIincidence[is];
				DiagnosedOIs += (SumGroupsM[ia][is + 15] + SumGroupsF[ia][is + 15]) *
					OIincidence[is];
			}
			for (is = 20; is < 44; is++){
				DiagnosedOIs += (SumGroupsM[ia][is] + SumGroupsF[ia][is]) *
					OIincidence[5];
			}
		}
		OIsTested[iy] = (OIsDiagnosed[iy] * (UndiagOIs + DiagnosedOIs) - DiagnosedOIs) /
			UndiagOIs;
		if (OIsTested[iy] < 0.0){ OIsTested[iy] = 0.0; }
	}
	else{
		OIsTested[iy] = OIsDiagnosed[iy];
	}

	// Secondly adust OIsTested to take into account that some OI patients
	// will get HIV diagnosed as a result of their OI despite not receiving HIV
	// testing at the OI treatment facility.
	if (OIsTested[iy] > 0.0){
		OIsTested[iy] = 1.0 / (1.0 + (1.0 / OIsTested[iy] - 1.0) * ORdiagOItreat);
	}
	if (FixedUncertainty == 1){
		OItestingRate.out[CurrSim - 1][iy] = OIsTested[iy];
	}
}

void CalcPosPartnerProb()
{
	int ia, ib, ig, ii;
	double TotNeg, HIVprev[2], ORconcordance, Temp, Determinant, ConcordantPos, ConcordantNeg;

	// Calculate prob that partner is HIV-positive
	for (ia = 0; ia < 81; ia++){
		// Treat ia as if it is the female partner's age. So ib is the male partner's age.
		ib = ia + 3;
		if (ib > 80){ ib = 80; }
		if (ib >= 20){ HIVprev[0] = TotalPositive[ib + 10][0] / TotalPop[ib + 10][0]; }
		if (ia >= 20){ HIVprev[1] = TotalPositive[ia + 10][1] / TotalPop[ia + 10][1]; }
		if (ib < 20){
			TotNeg = 0.0;
			for (ii = 0; ii < 5; ii++){ TotNeg += SumGroupsM[ib][ii] - SumGroupsVM[ib][ii]; }
			if (TotalSexuallyExp[ib + 10][0] > 0.0 && TotNeg < TotalSexuallyExp[ib + 10][0]){
				HIVprev[0] = 1.0 - TotNeg / TotalSexuallyExp[ib + 10][0];
			}
			else{ HIVprev[0] = 0.0; }
		}
		if (ia < 20){
			TotNeg = 0.0;
			for (ii = 0; ii < 5; ii++){ TotNeg += SumGroupsF[ia][ii] - SumGroupsVF[ia][ii]; }
			if (TotalSexuallyExp[ia + 10][1] > 0.0 && TotNeg < TotalSexuallyExp[ia + 10][1]){
				HIVprev[1] = 1.0 - TotNeg / TotalSexuallyExp[ia + 10][1];
			}
			else{ HIVprev[1] = 0.0; }
		}
		ORconcordance = exp(ORpartnerPosMetareg[0]) * pow(HIVprev[1], ORpartnerPosMetareg[1]);
		Temp = 1.0 + (ORconcordance - 1.0) * (HIVprev[0] + HIVprev[1]); // Equivalent to -b
		Determinant = pow(Temp, 2.0) - 4.0 * ORconcordance * (ORconcordance - 1.0) *
			HIVprev[0] * HIVprev[1];
		if (HIVprev[0] > 0.0 && HIVprev[1] > 0.0){
			ConcordantPos = (Temp - pow(Determinant, 0.5)) / (2.0 * (ORconcordance - 1.0));
			ConcordantNeg = 1.0 - HIVprev[0] - HIVprev[1] + ConcordantPos;
			// PosPartnerProb is the prob that an individual of age x (1st argument), HIV status s
			// (2nd argument) and sex g (3rd argument) has an HIV-positive partner.
			PosPartnerProb[ib][1][0] = ConcordantPos / HIVprev[0]; // HIV+ male
			PosPartnerProb[ia][1][1] = ConcordantPos / HIVprev[1]; // HIV+ female
			PosPartnerProb[ib][0][0] = (1.0 - HIVprev[0] - ConcordantNeg) / (1.0 - HIVprev[0]); // HIV- M
			PosPartnerProb[ia][0][1] = (1.0 - HIVprev[1] - ConcordantNeg) / (1.0 - HIVprev[1]); // HIV- F
			for (ii = 0; ii < 2; ii++){
				if (HIVprev[0] == 1.0){ PosPartnerProb[ib][0][0] = 1.0; }
				if (HIVprev[1] == 1.0){ PosPartnerProb[ia][0][1] = 1.0; }
				if (PosPartnerProb[ib][ii][0] > 1.0){ PosPartnerProb[ib][ii][0] = 1.0; }
				if (PosPartnerProb[ib][ii][0] < 0.0){ PosPartnerProb[ib][ii][0] = 0.0; }
				if (PosPartnerProb[ia][ii][1] > 1.0){ PosPartnerProb[ia][ii][1] = 1.0; }
				if (PosPartnerProb[ia][ii][1] < 0.0){ PosPartnerProb[ia][ii][1] = 0.0; }
			}
		}
		else{
			PosPartnerProb[ib][1][0] = 0.0; // HIV+ male
			PosPartnerProb[ia][1][1] = 0.0; // HIV+ female
			PosPartnerProb[ib][0][0] = 0.0; // HIV- M
			PosPartnerProb[ib][0][1] = 0.0; // HIV- F
		}
	}

	// Calculate ART coverage in HIV-positive individuals
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			AvePartnerCoverage[ia][ig] = TotalART[ia + 10][ig] / TotalPositive[ia + 10][ig];
		}
	}
}

void CalcSelfTestingRates()
{
	int ia, ii, ig, ib, im;
	double Temp, Temp2, Temp3, Temp4;
	double aParamM, aParamF, bParamM, bParamF, AgeSexAdjFixed[81][2], AgeSexAdjTaxi[81][2];
	double AgeSexAdjWork[81][2], AgeSexAdjWork2[81][2];

	// 1. Calculate SelfTestUptake
	for (ii = 0; ii < 6; ii++){
		if (CurrYear>SelfTestDataYr[ii]){ SelfTestUptake[ii] = SelfTestUptakeUlt[ii]; }
		else{ SelfTestUptake[ii] = SelfTestTotals[CurrYear - StartYear][ii]; }
        if(CurrYear>=2020){ SelfTestUptake[ii] = 0.0;}
	}

	// (a) Fixed point distribution
	if (SelfTestUptake[0] > 0.0){
		bParamM = pow(QuadParam[0], 2.0) / VCTage[0];
		bParamF = pow(QuadParam[1], 2.0) / VCTage[1];
		aParamM = VCTage[0] / bParamM;
		aParamF = VCTage[1] / bParamF;
		Temp = 0.0;
		for (ia = 0; ia < 81; ia++){
			AgeSexAdjFixed[ia][0] = VCTmale2010 * pow((ia + 10.0) / 25.0, aParamM - 1.0) * exp((15.0 - ia) /
				bParamM);
			AgeSexAdjFixed[ia][1] = pow((ia + 10.0) / 25.0, aParamF - 1.0) * exp((15.0 - ia) / bParamF);
			if (CurrYear <= SelfTestDataYr[0]){
				for (ig = 0; ig < 2; ig++){
					if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][ig]; }
					else{ Temp2 = TotalART[ia + 10][ig] + TotalInterrupt[ia + 10][ig]; }
					Temp3 = TotalSexuallyExp[ia + 10][ig] / TotalPop[ia + 10][ig];
					// Note that this is a quick approximation, assuming proportions diagnosed and on ART are
					// the same in virgins and sexually experienced youth of the same age.
					Temp += (TotalPop[ia + 10][ig] - TotalDiagnosed[ia + 10][ig]) * Temp3 * AgeSexAdjFixed[ia][ig];
					Temp += (TotalDiagnosed[ia + 10][ig] - Temp2) * Temp3 * AgeSexAdjFixed[ia][ig] * RetestPosST[0];
					Temp += Temp2 * Temp3 * AgeSexAdjFixed[ia][ig] * RetestPosST[1];
				}
			}
		}
		if (CurrYear <= SelfTestDataYr[0]){ SelfTestUptake[0] *= (1.0 - SelfTestWastage[0]) / Temp; }
	}

	// (b) Taxi rank distribution
	if (SelfTestUptake[1] > 0.0){
		Temp = 0.0;
		for (ia = 0; ia < 81; ia++){
			AgeSexAdjTaxi[ia][1] = pow((ia + 10.0) / 25.0, STageTaxi[0] - 1.0) * exp((15.0 - ia) * STageTaxi[1]);
			AgeSexAdjTaxi[ia][0] = AgeSexAdjTaxi[ia][1] * STtaxiMtoFratio;
			if (CurrYear <= SelfTestDataYr[1]){
				for (ig = 0; ig < 2; ig++){
					if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][ig]; }
					else{ Temp2 = TotalART[ia + 10][ig] + TotalInterrupt[ia + 10][ig]; }
					Temp3 = TotalSexuallyExp[ia + 10][ig] / TotalPop[ia + 10][ig];
					Temp += (TotalPop[ia + 10][ig] - TotalDiagnosed[ia + 10][ig]) * Temp3 * AgeSexAdjTaxi[ia][ig];
					Temp += (TotalDiagnosed[ia + 10][ig] - Temp2) * Temp3 * AgeSexAdjTaxi[ia][ig] * RetestPosST[0];
					Temp += Temp2 * Temp3 * AgeSexAdjTaxi[ia][ig] * RetestPosST[1];
				}
			}
		}
		if (CurrYear <= SelfTestDataYr[1]){ SelfTestUptake[1] *= (1.0 - SelfTestWastage[1]) / Temp; }
	}

	// (c) Distribution to partners of HIV+ pregnant women
	if (SelfTestUptake[2] > 0.0 && CurrYear <= SelfTestDataYr[2]){
		Temp = 0.0;
		for (ia = 7; ia < 43; ia++){
			// Male ages 17-52 correspond to female ages 14-49, the range over which HIVnegSEfert is defined.
			// (Assuming for simplicity that every sexually experienced male has a female partner 3 years younger)
			if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][0]; }
			else{ Temp2 = TotalART[ia + 10][0] + TotalInterrupt[ia + 10][0]; }
			// In the following 2 lines we previously multiplied by PosPartnerProb, but this is now removed as the 2ndary
			// test distribution seems to be independent of the woman's HIV status.
			Temp3 = HIVnegSEfert[ia - 7] * TotalSexuallyExp[ia + 10][0] / TotalPop[ia + 10][0];
			Temp4 = HIVnegSEfert[ia - 7] * TotalSexuallyExp[ia + 10][0] / TotalPop[ia + 10][0];
			Temp += (TotalPop[ia + 10][0] - TotalPositive[ia + 10][0]) * Temp3;
			Temp += (TotalPositive[ia + 10][0] - TotalDiagnosed[ia + 10][0]) * Temp4;
			Temp += (TotalDiagnosed[ia + 10][0] - Temp2) * Temp4;
			Temp += Temp2 * Temp4;
			// Note that we don't multiply by RetestPos in the last 2 lines because coverage is gross of wastage.
		}
		SelfTestUptake[2] *= 1.0 / Temp;
	}

	// (d) Distribution to partners of ART patients
	if (SelfTestUptake[3] > 0.0 && CurrYear <= SelfTestDataYr[3]){
		Temp = 0.0;
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				if (ig == 0){ ib = ia - 3; }
				else{ ib = ia + 3; }
				if (ib < 0 || ib > 80){ ib = ia; }
				if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][ig]; }
				else{ Temp2 = TotalART[ia + 10][ig] + TotalInterrupt[ia + 10][ig]; }
				Temp3 = PosPartnerProb[ia][0][ig] * AvePartnerCoverage[ib][1 - ig];
				Temp3 *= TotalSexuallyExp[ia + 10][ig] / TotalPop[ia + 10][ig];
				Temp += (TotalPop[ia + 10][ig] - TotalPositive[ia + 10][ig]) * Temp3;
				if (PosPartnerProb[ia][0][ig] > 0.0){
					Temp3 *= PosPartnerProb[ia][1][ig] / PosPartnerProb[ia][0][ig];
				}
				else{ Temp3 = 0.0; }
				Temp += (TotalPositive[ia + 10][ig] - TotalDiagnosed[ia + 10][ig]) * Temp3;
				Temp += (TotalDiagnosed[ia + 10][ig] - Temp2) * Temp3;
				Temp += Temp2 * Temp3;
				// Note that we don't multiply by RetestPos in the last 2 lines because coverage is gross of wastage.
			}
		}
		SelfTestUptake[3] *= 1.0 / Temp;
	}

	// (e) Primary distribution through workplaces
	if (SelfTestUptake[4] > 0.0){
		Temp = 0.0;
		for (ia = 0; ia < 81; ia++){
			AgeSexAdjWork[ia][1] = 0.0;
			AgeSexAdjWork[ia][0] = 0.0;
		}
		for (ia = 5; ia < 55; ia++){
			AgeSexAdjWork[ia][1] = pow((ia + 10.0) / 25.0, STageWork[0][1] - 1.0) * exp((15.0 - ia) * STageWork[1][1]) *
				EmployedPropn[ia / 5 - 1][1];
			AgeSexAdjWork[ia][0] = pow((ia + 10.0) / 25.0, STageWork[0][0] - 1.0) * exp((15.0 - ia) * STageWork[1][0]) *
				STworkMtoFratio[0] * EmployedPropn[ia / 5 - 1][0];
			if (CurrYear <= SelfTestDataYr[4]){
				for (ig = 0; ig < 2; ig++){
					if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][ig]; }
					else{ Temp2 = TotalART[ia + 10][ig] + TotalInterrupt[ia + 10][ig]; }
					Temp3 = TotalSexuallyExp[ia + 10][ig] / TotalPop[ia + 10][ig];
					Temp += (TotalPop[ia + 10][ig] - TotalDiagnosed[ia + 10][ig]) * Temp3 * AgeSexAdjWork[ia][ig];
					Temp += (TotalDiagnosed[ia + 10][ig] - Temp2) * Temp3 * AgeSexAdjWork[ia][ig] * RetestPosST[0];
					Temp += Temp2 * Temp3 * AgeSexAdjWork[ia][ig] * RetestPosST[1];
				}
			}
		}
		if (CurrYear <= SelfTestDataYr[4]){ SelfTestUptake[4] *= (1.0 - SelfTestWastage[4]) / Temp; }
	}

	// (f) Secondary distribution through workplaces
	if (SelfTestUptake[5] > 0.0){
		Temp = 0.0;
		for (ia = 0; ia < 81; ia++){
			AgeSexAdjWork2[ia][1] = 0.0;
			AgeSexAdjWork2[ia][0] = 0.0;
		}
		for (ia = 2; ia < 52; ia++){
			ib = (ia + 3) / 5 - 1;
			AgeSexAdjWork2[ia][1] = pow((ia + 10.0) / 25.0, aParamF - 1.0) * exp((15.0 - ia) / bParamF) *
				EmployedPropn[ib][0];
		}
		for (ia = 8; ia < 58; ia++){
			ib = (ia - 3) / 5 - 1;
			AgeSexAdjWork2[ia][0] = pow((ia + 10.0) / 25.0, aParamM - 1.0) * exp((15.0 - ia) / bParamM) *
				VCTmale2010 * STworkMtoFratio[1] * EmployedPropn[ib][1];
		}
		for (ia = 2; ia < 58; ia++){
			if (CurrYear <= SelfTestDataYr[5]){
				for (ig = 0; ig < 2; ig++){
					if (ExcludeInterrupters == 0){ Temp2 = TotalART[ia + 10][ig]; }
					else{ Temp2 = TotalART[ia + 10][ig] + TotalInterrupt[ia + 10][ig]; }
					Temp3 = TotalSexuallyExp[ia + 10][ig] / TotalPop[ia + 10][ig];
					Temp += (TotalPop[ia + 10][ig] - TotalDiagnosed[ia + 10][ig]) * Temp3 * AgeSexAdjWork2[ia][ig];
					Temp += (TotalDiagnosed[ia + 10][ig] - Temp2) * Temp3 * AgeSexAdjWork2[ia][ig];
					Temp += Temp2 * Temp3 * AgeSexAdjWork2[ia][ig];
					// Note that we don't multiply by RetestPos in the last 2 lines because coverage is gross of wastage.
				}
			}
		}
		if (CurrYear <= SelfTestDataYr[5]){ SelfTestUptake[5] = SelfTestUptake[5] / Temp; }
	}

	if (FixedUncertainty == 1 && CurrYear >= 2017 && CurrYear <= 2019){
		for (im = 0; im < 6; im++){
			STuptakeByYr.out[CurrSim - 1][im * 3 + CurrYear - 2017] = SelfTestUptake[im];
		}
	}

	// 2. Calculate SelfTestingRateM
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			for (im = 0; im<6; im++){
				SelfTestingRateM[ia][0][ig][im] = 0.0;
				SelfTestingRateM[ia][2][ig][im] = 0.0;
			}
		}
	}
	if (SelfTestUptake[0] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				SelfTestingRateM[ia][0][ig][0] = SelfTestUptake[0] * AgeSexAdjFixed[ia][ig];
			}
		}
	}
	if (SelfTestUptake[1] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				SelfTestingRateM[ia][0][ig][1] = SelfTestUptake[1] * AgeSexAdjTaxi[ia][ig];
			}
		}
	}
	if (SelfTestUptake[4] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				SelfTestingRateM[ia][0][ig][4] = SelfTestUptake[4] * AgeSexAdjWork[ia][ig];
			}
		}
	}
	if (SelfTestUptake[5] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				SelfTestingRateM[ia][0][ig][5] = SelfTestUptake[5] * AgeSexAdjWork2[ia][ig] *
					(1.0 - SelfTestWastage[5]);
			}
		}
	}
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			SelfTestingRateM[ia][2][ig][0] = SelfTestingRateM[ia][0][ig][0];
			SelfTestingRateM[ia][2][ig][1] = SelfTestingRateM[ia][0][ig][1];
			SelfTestingRateM[ia][2][ig][4] = SelfTestingRateM[ia][0][ig][4];
			SelfTestingRateM[ia][2][ig][5] = SelfTestingRateM[ia][0][ig][5];
		}
	}
	// All calculations prior to this point are independent of HIV status. But not so for ANC and index testing:
	if (SelfTestUptake[2] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				if (ig == 0 && ia >= 7 && ia < 43){
					// In the following 2 lines we previously multiplied by PosPartnerProb, but this is now removed as the
					// 2ndary test distribution seems to be independent of the woman's HIV status.
					SelfTestingRateM[ia][0][0][2] =
						SelfTestUptake[2] * HIVnegSEfert[ia - 7] * (1.0 - SelfTestWastage[2]);
					SelfTestingRateM[ia][2][0][2] =
						SelfTestUptake[2] * HIVnegSEfert[ia - 7] * (1.0 - SelfTestWastage[2]);
				}
				else{
					SelfTestingRateM[ia][0][ig][2] = 0.0;
					SelfTestingRateM[ia][2][ig][2] = 0.0;
				}
			}
		}
	}
	if (SelfTestUptake[3] > 0.0){
		for (ia = 0; ia < 81; ia++){
			for (ig = 0; ig < 2; ig++){
				if (ig == 0){ ib = ia - 3; }
				else{ ib = ia + 3; }
				if (ib < 0 || ib > 80){ ib = ia; }
				SelfTestingRateM[ia][0][ig][3] = SelfTestUptake[3] * PosPartnerProb[ia][0][ig] * AvePartnerCoverage[ib][1 - ig] *
					(1.0 - SelfTestWastage[3]);
				SelfTestingRateM[ia][2][ig][3] = SelfTestUptake[3] * PosPartnerProb[ia][1][ig] * AvePartnerCoverage[ib][1 - ig] *
					(1.0 - SelfTestWastage[3]);
			}
		}
	}
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			for (im = 0; im < 6; im++){
				SelfTestingRateM[ia][1][ig][im] = SelfTestingRateM[ia][0][ig][im];
				for (ii = 3; ii < 12; ii++){
					SelfTestingRateM[ia][ii][ig][im] = SelfTestingRateM[ia][2][ig][im];
				}
			}
		}
	}

	// Convert all annual rates to monthly rates
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			for (im = 0; im < 6; im++){
				for (ii = 0; ii < 12; ii++){
					SelfTestingRateM[ia][ii][ig][im] = SelfTestingRateM[ia][ii][ig][im] / 12.0;
				}
			}
		}
	}

	// 3. Calculate SelfTestingRate
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			for (ii = 0; ii < 12; ii++){
				SelfTestingRate[ia][ii][ig] = 0.0;
				for (im = 0; im < 6; im++){
					SelfTestingRate[ia][ii][ig] += SelfTestingRateM[ia][ii][ig][im];
				}
			}
		}
	}
}

void GetTotalTesting()
{
	int ia, ii, ig;
	double Temp1, Temp2, Temp3, Temp4;

	// Calculate TotTestingRateSE
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			for (ii = 0; ii < 12; ii++){
				if (ii < 3 || ii == 7){
					TotTestingRateSE[ia][ii][ig] = TestingRateSE[ia][ii][ig] +
						SelfTestingRate[ia][ii][ig];
				}
				else{
					TotTestingRateSE[ia][ii][ig] = TestingRateSE[ia][ii][ig] +
						SelfTestingRate[ia][ii][ig] * (1.0 - SelfTestConfirm);
				}
			}
		}
	}

	// Calculate AvePosTesting
	// Note that in MicroCOSM we assume that index testing only occurs in the event
	// of first-time diagnosis, not in the case of re-diagnosis. So to be consistent
	// we only include the first-time diagnoses in the numerator here.
	for (ia = 0; ia < 81; ia++){
		for (ig = 0; ig < 2; ig++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			for (ii = 5; ii < 44; ii++){
				if (ig == 0 && ia < 20){ Temp3 = (SumGroupsM[ia][ii] - SumGroupsVM[ia][ii]); }
				if (ig == 1 && ia < 20){ Temp3 = (SumGroupsF[ia][ii] - SumGroupsVF[ia][ii]); }
				if (ig == 0 && ia >= 20){ Temp3 = SumGroupsM[ia][ii]; }
				if (ig == 1 && ia >= 20){ Temp3 = SumGroupsF[ia][ii]; }
				Temp4 = 0.0;
				if ((ii > 5 && ii < 10) || (ii > 10 && ii < 15)){
					Temp4 = TotTestingRateSE[ia][ii - 3][ig];
				}
				Temp1 += Temp3 * Temp4;
				Temp2 += Temp3;
			}
			if (Temp2 > 0.0){ AvePosTesting[ia][ig] = Temp1 / Temp2; }
			else{ AvePosTesting[ia][ig] = 0.0; }
		}
	}
}

void CalcHCT1stTime()
{
	int ia, is, iy, ii;
	double NewlyTestedTot, NewlyTestedANP; // ANP = asymptomatic and non-pregnant

	NewlyTestedTot = 0.0;
	NewlyTestedANP = 0.0; // the # tests you'd expect in non-pregnant adults without OIs

	// Calculate NewlyTestedTot
	for (ia = 5; ia<81; ia++){
		NewlyTestedTot += SumGroupsM[ia][0] * TestingRateSE[ia][0][0] + SumGroupsM[ia][1] *
			TestingRateSE[ia][1][0];
		NewlyTestedTot += SumGroupsF[ia][0] * TestingRateSE[ia][0][1] + SumGroupsF[ia][1] *
			TestingRateSE[ia][1][1];
		if (PrEPorVM == 1){
			NewlyTestedTot += SumGroupsM[ia][2] * FreqRegHCT[0] / 12.0 + SumGroupsM[ia][3] *
				FreqHCTinPrEP[0] / 12.0;
			NewlyTestedTot += SumGroupsF[ia][2] * FreqRegHCT[1] / 12.0 + SumGroupsF[ia][3] *
				FreqHCTinPrEP[1] / 12.0 + SumGroupsF[ia][4] * FreqHCTinVM / 12.0;
		}
		for (is = 2; is<12; is++){
			NewlyTestedTot += SumGroupsM[ia][is + 3] * TestingRateSE[ia][is][0];
			NewlyTestedTot += SumGroupsF[ia][is + 3] * TestingRateSE[ia][is][1];
		}
		for (is = 15; is < 20; is++){
			NewlyTestedTot += SumGroupsM[ia][is] * TestingRateSE[ia][0][0] * RetestPos;
			NewlyTestedTot += SumGroupsF[ia][is] * TestingRateNPF[ia] * RetestPos;
		}
		for (is = 20; is < 44; is++){
			NewlyTestedTot += SumGroupsM[ia][is] * TestingRateSE[ia][0][0] * RetestART;
			NewlyTestedTot += SumGroupsF[ia][is] * TestingRateNPF[ia] * RetestART;
		}
		if (ia<20){
			// Adjustments to tests in HIV-neg virgins
			NewlyTestedTot = NewlyTestedTot - SumGroupsVM[ia][0] * (TestingRateSE[ia][0][0] - TestingRateNegV[ia][0]) -
				SumGroupsVM[ia][1] * (TestingRateSE[ia][1][0] - TestingRateNegV[ia][0]);
			NewlyTestedTot = NewlyTestedTot - SumGroupsVF[ia][0] * (TestingRateSE[ia][0][1] - TestingRateNegV[ia][1]) -
				SumGroupsVF[ia][1] * (TestingRateSE[ia][1][1] - TestingRateNegV[ia][1]);
			// Subtract tests in HIV-pos virgins
			for (is = 2; is<12; is++){
				NewlyTestedTot = NewlyTestedTot - SumGroupsVM[ia][is + 3] * TestingRateSE[ia][is][0];
				NewlyTestedTot = NewlyTestedTot - SumGroupsVF[ia][is + 3] * TestingRateSE[ia][is][1];
			}
			for (is = 15; is < 20; is++){
				NewlyTestedTot = NewlyTestedTot - SumGroupsVM[ia][is] * TestingRateSE[ia][0][0] * RetestPos;
				NewlyTestedTot = NewlyTestedTot - SumGroupsVF[ia][is] * TestingRateNPF[ia] * RetestPos;
			}
			for (is = 20; is < 44; is++){
				NewlyTestedTot = NewlyTestedTot - SumGroupsVM[ia][is] * TestingRateSE[ia][0][0] * RetestART;
				NewlyTestedTot = NewlyTestedTot - SumGroupsVF[ia][is] * TestingRateNPF[ia] * RetestART;
			}
			// Add back the tests in HIV-pos virgins
			for (is = 0; is<5; is++){
				NewlyTestedTot += (SumGroupsVM[ia][is + 5] + SumGroupsVM[ia][is + 10]) * TestingRateV[ia][is][0];
				NewlyTestedTot += (SumGroupsVF[ia][is + 5] + SumGroupsVF[ia][is + 10]) * TestingRateV[ia][is][1];
			}
			for (is = 15; is < 20; is++){
				NewlyTestedTot += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestPos +
					SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestPos;
			}
			for (is = 20; is < 44; is++){
				NewlyTestedTot += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestART +
					SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestART;
			}
		}
	}

	// Calculate NewlyTestedANP (asymptomatic, non-pregnant, not receiving regular HCT)
	for (ia = 5; ia<81; ia++){
		NewlyTestedANP += SumGroupsM[ia][0] * TestingRateSE[ia][0][0] + SumGroupsM[ia][1] *
			TestingRateSE[ia][1][0];
		NewlyTestedANP += SumGroupsF[ia][0] * TestingRateNPF[ia] + SumGroupsF[ia][1] *
			TestingRateNPF[ia] * RetestAdj;
		// No addition of individuals receiving testing through PrEP/VM programmes
		for (is = 2; is<7; is++){
			NewlyTestedANP += SumGroupsM[ia][is + 3] * TestingRateSE[ia][0][0] * HIVeffectVCT;
			NewlyTestedANP += SumGroupsF[ia][is + 3] * TestingRateNPF[ia] * HIVeffectVCT;
		}
		for (is = 10; is<15; is++){
			NewlyTestedANP += SumGroupsM[ia][is] * TestingRateSE[ia][1][0] * HIVeffectVCT;
			NewlyTestedANP += SumGroupsF[ia][is] * TestingRateNPF[ia] * RetestAdj * HIVeffectVCT;
		}
		for (is = 15; is < 20; is++){
			NewlyTestedANP += SumGroupsM[ia][is] * TestingRateSE[ia][0][0] * RetestPos * HIVeffectVCT;
			NewlyTestedANP += SumGroupsF[ia][is] * TestingRateNPF[ia] * RetestPos * HIVeffectVCT;
		}
		for (is = 20; is < 44; is++){
			NewlyTestedANP += SumGroupsM[ia][is] * TestingRateSE[ia][0][0] * RetestART * HIVeffectVCT;
			NewlyTestedANP += SumGroupsF[ia][is] * TestingRateNPF[ia] * RetestART * HIVeffectVCT;
		}
		if (ia<20){
			// Adjustments to tests in HIV-neg virgins
			NewlyTestedANP = NewlyTestedANP - SumGroupsVM[ia][0] * (TestingRateSE[ia][0][0] - TestingRateNegV[ia][0]) -
				SumGroupsVM[ia][1] * (TestingRateSE[ia][1][0] - TestingRateNegV[ia][0]);
			NewlyTestedANP = NewlyTestedANP - SumGroupsVF[ia][0] * (TestingRateNPF[ia] - TestingRateNegV[ia][1]) -
				SumGroupsVF[ia][1] * (TestingRateNPF[ia] * RetestAdj - TestingRateNegV[ia][1]);
			// Subtract tests in HIV-pos virgins
			for (is = 2; is<7; is++){
				NewlyTestedANP = NewlyTestedANP - SumGroupsVM[ia][is + 3] * TestingRateSE[ia][0][0];
				NewlyTestedANP = NewlyTestedANP - SumGroupsVF[ia][is + 3] * TestingRateNPF[ia];
			}
			for (is = 7; is<12; is++){
				NewlyTestedANP = NewlyTestedANP - SumGroupsVM[ia][is + 3] * TestingRateSE[ia][1][0];
				NewlyTestedANP = NewlyTestedANP - SumGroupsVF[ia][is + 3] * TestingRateNPF[ia] * RetestAdj;
			}
			for (is = 15; is < 20; is++){
				NewlyTestedANP = NewlyTestedANP - SumGroupsVM[ia][is] * TestingRateSE[ia][0][0] * RetestPos;
				NewlyTestedANP = NewlyTestedANP - SumGroupsVF[ia][is] * TestingRateNPF[ia] * RetestPos;
			}
			for (is = 20; is < 44; is++){
				NewlyTestedANP = NewlyTestedANP - SumGroupsVM[ia][is] * TestingRateSE[ia][0][0] * RetestART;
				NewlyTestedANP = NewlyTestedANP - SumGroupsVF[ia][is] * TestingRateNPF[ia] * RetestART;
			}
			// Add back the tests in HIV-pos virgins
			for (is = 0; is < 5; is++){
				NewlyTestedANP += (SumGroupsVM[ia][is + 5] + SumGroupsVM[ia][is + 10]) * TestingRateNegV[ia][0];
				NewlyTestedANP += (SumGroupsVF[ia][is + 5] + SumGroupsVF[ia][is + 10]) * TestingRateNegV[ia][1];
			}
			for (is = 15; is < 20; is++){
				NewlyTestedANP += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestPos +
					SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestPos;
			}
			for (is = 20; is < 44; is++){
				NewlyTestedANP += SumGroupsVM[ia][is] * TestingRateNegV[ia][0] * RetestART +
					SumGroupsVF[ia][is] * TestingRateNegV[ia][1] * RetestART;
			}
		}
	}

	// Finally adjust HCT1stTimeF25
	iy = CurrYear - 1985;
	HCT1stTimeF25[iy] *= (1.0 + ((NumbersTested[iy]/12.0) - NewlyTestedTot) / NewlyTestedANP);
	if (FixedUncertainty == 1){
		HIVtestUptakeF25.out[CurrSim - 1][iy] = HCT1stTimeF25[iy];}
	if (FixedUncertainty == 1 && VaryFutureInterventions == 1 && CurrYear == 2014){
		for (iy = 30; iy < 34; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[29] + (0.2 * (iy - 29)) *
				(FutureInterventions.out[CurrSim - 1][0] - HCT1stTimeF25[29]);
		}
	}
}

void CalcRRtestVirgin()
{
	// Similar to CalcHCT1stTime

	int ia, iy, is;
	double ExpectedTests, ActualTests, OItests, temp, TempF, BaseHCT, aParamF, bParamF;

	iy = CurrYear - StartYear;
	ActualTests = NumbersTested5to14[iy];
	ExpectedTests = 0.0;
	//TestsSE = 0.0;
	OItests = 0.0;

	// TempF is testing rate in 15-year old females who are sexually experienced
	// Calculation is the same as in the UpdateTestingRates function but is ANNUAL.
	bParamF = pow(QuadParam[1], 2.0) / VCTage[1];
	aParamF = VCTage[1] / bParamF;
	TempF = HCT1stTimeF25[iy] * pow(15.0 / 25.0, aParamF - 1.0) * exp(10.0 / bParamF) ;

	// Calculate expected tests in 5-9 year olds
	for (ia = 60; ia < 132; ia++){
		temp = MaleChild.NegChildFF[ia] + MaleChild.PosChildAtBirthNoPMTCT[ia] +
			MaleChild.PosChildAtBirthPMTCT[ia] + MaleChild.PosChildAfterBirth[ia] +
			FemChild.NegChildFF[ia] + FemChild.PosChildAtBirthNoPMTCT[ia] +
			FemChild.PosChildAtBirthPMTCT[ia] + FemChild.PosChildAfterBirth[ia];
		temp += (MaleChild.ARTeligible[ia] + FemChild.ARTeligible[ia]) *
			RRtestPaedAdvanced;
		temp += (MaleChild.DiagChildAtBirthNoPMTCT[ia] + MaleChild.DiagChildAtBirthPMTCT[ia] +
			MaleChild.DiagChildAfterBirth[ia] + FemChild.DiagChildAtBirthNoPMTCT[ia] +
			FemChild.DiagChildAtBirthPMTCT[ia] + FemChild.DiagChildAfterBirth[ia]) * RetestPosP;
		temp += (MaleChild.DiagARTeligible[ia] + FemChild.DiagARTeligible[ia]) *
			RRtestPaedAdvanced * RetestPosP;
		temp += (MaleChild.OnARTearly[ia] + MaleChild.OnARTlate1st3m[ia] +
			MaleChild.OnARTlateAfter3m[ia] + MaleChild.StoppedART[ia] +
			FemChild.OnARTearly[ia] + FemChild.OnARTlate1st3m[ia] +
			FemChild.OnARTlateAfter3m[ia] + FemChild.StoppedART[ia]) * RetestPosP *
			RetestART / RetestPos;
		ExpectedTests += temp * TempF;
	}

	// Calculate expected tests in 10-14 year olds
	// We're using a crude approximation here: ignoring the higher rate of testing in sexually
	// experienced 10-14 year olds.
	for (ia = 0; ia < 5; ia++){
		temp = TotalPop[ia + 10][0] + TotalPop[ia + 10][1] - TotalDiagnosed[ia + 10][0] -
			TotalDiagnosed[ia + 10][1];
		temp += (TotalDiagnosed[ia + 10][0] + TotalDiagnosed[ia + 10][1] - TotalART[ia + 10][0] -
			TotalART[ia + 10][1]) * RetestPosP;
		temp += (TotalART[ia + 10][0] + TotalART[ia + 10][1]) * RetestPosP * RetestART / RetestPos;
		ExpectedTests += temp * TempF;
		//TestsSE += TotalSexuallyExp[10 + ia][0] * TestingRateSE[ia][0][0] +
		//	TotalSexuallyExp[10 + ia][1] * TestingRateSE[ia][0][1];
		for (is = 0; is < 5; is++){
			OItests += (SumGroupsM[ia][5 + is] + SumGroupsM[ia][10 + is] + SumGroupsM[ia][15 + is] *
				RetestPosP + SumGroupsF[ia][5 + is] + SumGroupsF[ia][10 + is] + SumGroupsF[ia][15 + is] *
				RetestPosP) * OIsTested[iy] * OIincidence[is];
		}
	}

	RRtestVirgin = (ActualTests - OItests) / (ExpectedTests - OItests);
	if (FixedUncertainty == 1){ RelativeTestingVirgins.out[CurrSim - 1][iy] = RRtestVirgin; }
}

void UpdateNonAIDSmort()
{
	int ia, offset;
	double Weight2018;

	// Get current non-AIDS mortality rates by exact age.
	if(CurrYear<=2021){
		offset = CurrYear - StartYear;
		for(ia=0; ia<91; ia++){
			CurrNonAIDSmortEA[ia][0] = NonAIDSmortM[ia][offset];
			CurrNonAIDSmortEA[ia][1] = NonAIDSmortF[ia][offset];
		}
	}
	else if (CurrYear < 2025){
		Weight2018 = 0.25 * (CurrYear - 2021);
		for (ia = 0; ia<91; ia++){
			CurrNonAIDSmortEA[ia][0] = Weight2018 * NonAIDSmortM[ia][33] + (1.0 - Weight2018) * NonAIDSmortM[ia][36];
			CurrNonAIDSmortEA[ia][1] = Weight2018 * NonAIDSmortF[ia][33] + (1.0 - Weight2018) * NonAIDSmortF[ia][36];
		}
	}
	else{
		for(ia=0; ia<91; ia++){
			offset = CurrYear - 2025;
			CurrNonAIDSmortEA[ia][0] = UltNonAIDSmort[ia][0] + (NonAIDSmortM[ia][33] -
				UltNonAIDSmort[ia][0]) * pow(RednNonAIDSmort[ia][0], offset);
			CurrNonAIDSmortEA[ia][1] = UltNonAIDSmort[ia][1] + (NonAIDSmortF[ia][33] -
				UltNonAIDSmort[ia][1]) * pow(RednNonAIDSmort[ia][1], offset);
		}
	}

	// Get current non-AIDS mortality rates by age last birthday
	for(ia=10; ia<90; ia++){
		CurrNonAIDSmortALB[ia][0] = 1.0 - (1.0 - CurrNonAIDSmortEA[ia][0]) * (2.0 -
			CurrNonAIDSmortEA[ia+1][0])/(2.0 - CurrNonAIDSmortEA[ia][0]);
		CurrNonAIDSmortALB[ia][1] = 1.0 - (1.0 - CurrNonAIDSmortEA[ia][1]) * (2.0 -
			CurrNonAIDSmortEA[ia+1][1])/(2.0 - CurrNonAIDSmortEA[ia][1]);
	}
	CurrNonAIDSmortALB[90][0] = 1.039 * CurrNonAIDSmortEA[90][0] + 0.055;
	CurrNonAIDSmortALB[90][1] = 1.039 * CurrNonAIDSmortEA[90][1] + 0.055;
}

void UpdateAIDSmort()
{
	int ia, ig;

	for(ig=0; ig<2; ig++){
		AveARTstartU200[ig] = 0.0;
		if(CurrYear>1985){
			AveARTstartU200[ig] = RateARTstartU200[CurrYear-1986][ig];}
		if(CurrYear>1986){
			AveARTstartU200[ig] += RateARTstartU200[CurrYear-1987][ig];}
		if(CurrYear>1987){
			AveARTstartU200[ig] += RateARTstartU200[CurrYear-1988][ig];}
		AveARTstartU200[ig] = AveARTstartU200[ig]/3.0;
		MortAdjBelow200[ig] = MinMort[0] + (1.0 - MinMort[0]) * exp(-RednLogMort[0] *
			AveARTstartU200[ig]);
	}

	for(ia=0; ia<81; ia++){
		//MnthlyAIDSmort[ia][1][0] = (CD4mort[1]/12.0) * MortAdjBelow200[0] *
		//	pow(RRper10yr, (ia - 20.0)/10.0) * pow(RRperCalYr, CurrYear - 1999);
		MnthlyAIDSmort[ia][1][0] = (CD4mort[1] / 12.0) * MortAdjBelow200[0] *
			pow(RRper10yr, (ia - 20.0)/10.0);
		MnthlyAIDSmort[ia][1][1] = MnthlyAIDSmort[ia][1][0] * (MortAdjBelow200[1] /
			MortAdjBelow200[0]) * RRuntreatedMortF;
		ProbExitAtEntry[ia][4][0] = 1.0 - (1.0 - exp(-MnthlyAIDSmort[ia][1][0]))/
			MnthlyAIDSmort[ia][1][0];
		ProbExitAtEntry[ia][4][1] = 1.0 - (1.0 - exp(-MnthlyAIDSmort[ia][1][1]))/
			MnthlyAIDSmort[ia][1][1];
	}
}

void UpdateARTmort()
{
	int ia, is, id;
	double Temp1, Temp2, FutureAdj;

	for(id=0; id<2; id++){
		MortAdj200M[id] = MinMort[id+1] + (1.0 - MinMort[id+1]) * exp(-AveARTstartU200[0] *
			RednLogMort[id+1]);
		MortAdj200F[id] = MinMort[id+1] + (1.0 - MinMort[id+1]) * exp(-AveARTstartU200[1] *
			RednLogMort[id+1]);
	}
	FutureAdj = 1.0;
	if (CurrYear >= 2011 && VaryFutureInterventions == 1){
		if (CurrYear >= 2016){ FutureAdj = FutureInterventions.out[CurrSim - 1][6]; }
		else{
			FutureAdj = 1.0 + 0.2 * (CurrYear - 2011) *
				(FutureInterventions.out[CurrSim - 1][6] - 1.0);
		}
	}

	// Male mortality
	for(ia=0; ia<81; ia++){
		Temp1 = pow(RRper10yrART[0], (ia - 20.0)/10.0);
		//Temp1 *= pow(RRperCalYr, CurrYear - 2007) * FutureAdj / 12.0;
		Temp1 *= FutureAdj / 12.0;
		for(is=0; is<3; is++){
			for(id=0; id<5; id++){
				MortByARTdurM[ia][is][id] = Temp1 * AnnHIVmortART[id][is][0] *
					IeDEAbias[0] * pow(IeDEAbias[1] / IeDEAbias[0], 1.0 * id / 4.0);
			}
		}
		for(id=0; id<5; id++){
			if(id==0){Temp2 = MortAdj200M[0];}
			else if(id==1){Temp2 = (MortAdj200M[0] + MortAdj200M[1])/2.0;}
			else{Temp2 = MortAdj200M[1];}
			MortByARTdurM[ia][3][id] = Temp1 * AnnHIVmortART[id][3][0] * Temp2 *
				IeDEAbias[0] * pow(IeDEAbias[1] / IeDEAbias[0], 1.0 * id / 4.0);
		}
		for(is=0; is<4; is++){
			MortStartART[ia][is][0] = 1.0 - (1.0 - exp(-MortByARTdurM[ia][is][0]))/
				MortByARTdurM[ia][is][0];
		}
	}

	// Female mortality
	for(ia=0; ia<81; ia++){
		Temp1 = pow(RRper10yrART[1], (ia - 20.0)/10.0);
		//Temp1 *= pow(RRperCalYr, CurrYear - 2007) * FutureAdj / 12.0;
		Temp1 *= FutureAdj / 12.0;
		for(is=0; is<3; is++){
			for(id=0; id<5; id++){
				MortByARTdurF[ia][is][id] = Temp1 * AnnHIVmortART[id][is][1] *
					IeDEAbias[0] * pow(IeDEAbias[1] / IeDEAbias[0], 1.0 * id / 4.0);
			}
		}
		for(id=0; id<5; id++){
			if(id==0){Temp2 = MortAdj200F[0];}
			else if(id==1){Temp2 = (MortAdj200F[0] + MortAdj200F[1])/2.0;}
			else{Temp2 = MortAdj200F[1];}
			MortByARTdurF[ia][3][id] = Temp1 * AnnHIVmortART[id][3][1] * Temp2 *
				IeDEAbias[0] * pow(IeDEAbias[1] / IeDEAbias[0], 1.0 * id / 4.0);
		}
		for(is=0; is<4; is++){
			MortStartART[ia][is][1] = 1.0 - (1.0 - exp(-MortByARTdurF[ia][is][0]))/
				MortByARTdurF[ia][is][0];
		}
	}
}

void UpdateMigration()
{
	int ia, ig, im, offset;
	double Temp[2], TempExp;

	if(CurrYear<=2022){
		offset = CurrYear - 1985;
		for(ia=0; ia<10; ia++){
			for(ig=0; ig<2; ig++){
				MigrationAdj[ia][ig] = 1.0 + NetMigrants[ia][offset][ig]/TotalPop[ia][ig];}
		}
		Temp[0] = 0.0;
		Temp[1] = 0.0;
		for(im=120; im<132; im++){
			Temp[0] += MaleChild.Total[im];
			Temp[1] += FemChild.Total[im];
		}
		for(ig=0; ig<2; ig++){
			MigrationAdj[10][ig] = 1.0 + NetMigrants[10][offset][ig]/Temp[ig];}
		for(ia=11; ia<90; ia++){
			for(ig=0; ig<2; ig++){
				MigrationAdj[ia][ig] = 1.0 + NetMigrants[ia][offset][ig]/TotalPop[ia-1][ig];}
		}
		for(ig=0; ig<2; ig++){
			MigrationAdj[90][ig] = 1.0 + NetMigrants[90][offset][ig]/(TotalPop[89][ig] +
				TotalPop[90][ig]);}
	}
	else{
		TempExp = pow(0.955, CurrYear - 2022);
		for(ia=0; ia<91; ia++){
			for(ig=0; ig<2; ig++){
				MigrationAdj[ia][ig] = NetMigrants[ia][37][ig] * TempExp;
				if(ia<10){
					Temp[ig] = TotalPop[ia][ig];}
				else if(ia==10){
					Temp[ig] = 0.0;
					if(ig==0){
						for(im=120; im<132; im++){
							Temp[0] += MaleChild.Total[im];}
					}
					else{
						for(im=120; im<132; im++){
							Temp[1] += FemChild.Total[im];}
					}
				}
				else if(ia<90){
					Temp[ig] = TotalPop[ia-1][ig];}
				else{
					Temp[ig] = TotalPop[ia-1][ig] + TotalPop[ia][ig];}
				MigrationAdj[ia][ig] = 1.0 + MigrationAdj[ia][ig]/Temp[ig];
			}
		}
	}
	for(ia=0; ia<10; ia++){
		for(ig=0; ig<2; ig++){
			if(MigrationAdj[ia][ig] < 0.0){
				MigrationAdj[ia][ig] = 0.0;}
		}
	}
}

void UpdateCircProb()
{
	int ia, ii;
	double NewMMC, ProbMMCbase, Temp;
	double HIVnegMales[4], MMCweights[81];

	NewMMC = MMCoperations[CurrYear - StartYear];
	if (NewMMC>0.0 || CurrYear >= MMCdataYear){
		// Calculate MMCweights
		for (ia = 0; ia<81; ia++){
			if (ia<5){ MMCweights[ia] = RR_MMCpromo10[CurrYear - StartYear]; }
			if (ia<10 && ia >= 5){ MMCweights[ia] = RR_MMCpromo15[CurrYear - StartYear]; }
			if (ia<15 && ia >= 10){ MMCweights[ia] = RR_MMCpromo20[CurrYear - StartYear]; }
			if (ia<40 && ia >= 15){ MMCweights[ia] = RR_MMCpromo25[CurrYear - StartYear]; }
			if (ia >= 40){ MMCweights[ia] = RR_MMCpromo50[CurrYear - StartYear]; }
		}
		// Calculate ProbMMChigh
		Temp = 0.0;
		for (ia = 0; ia<81; ia++){
			HIVnegMales[0] = MHU_ST.NegNoHCT[ia] + MHU_ST.NegPastHCT[ia] +
				MHU_ST.RegHCT[ia] + MHU_ST.RegPrEP[ia] + MHU_STM.NegNoHCT[ia] +
				MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] + MHU_STM.RegPrEP[ia] +
				MHU_virgin.NegNoHCT[ia] + MHU_virgin.NegPastHCT[ia] +
				MHU_virgin.RegHCT[ia] + MHU_virgin.RegPrEP[ia];
			HIVnegMales[1] = MHU_LTH.NegNoHCT[ia] + MHU_LTH.NegPastHCT[ia] +
				MHU_LTH.RegHCT[ia] + MHU_LTH.RegPrEP[ia] + MHU_LTL.NegNoHCT[ia] +
				MHU_LTL.NegPastHCT[ia] + MHU_LTL.RegHCT[ia] + MHU_LTL.RegPrEP[ia];
			HIVnegMales[2] = MLU_ST.NegNoHCT[ia] + MLU_ST.NegPastHCT[ia] +
				MLU_ST.RegHCT[ia] + MLU_ST.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] +
				MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] + MLU_STM.RegPrEP[ia] +
				MLU_virgin.NegNoHCT[ia] + MLU_virgin.NegPastHCT[ia] +
				MLU_virgin.RegHCT[ia] + MLU_virgin.RegPrEP[ia];
			HIVnegMales[3] = MLU_LTH.NegNoHCT[ia] + MLU_LTH.NegPastHCT[ia] +
				MLU_LTH.RegHCT[ia] + MLU_LTH.RegPrEP[ia] + MLU_LTL.NegNoHCT[ia] +
				MLU_LTL.NegPastHCT[ia] + MLU_LTL.RegHCT[ia] + MLU_LTL.RegPrEP[ia];
			CurrNegCircumcised[ia + 10] = 1.0 - (HIVnegMales[0] + HIVnegMales[1] + HIVnegMales[2] +
				HIVnegMales[3]) / (SumGroupsM[ia][0] + SumGroupsM[ia][1] + SumGroupsM[ia][2] +
				SumGroupsM[ia][3]);
			if (CircPrevUlt - InitCircumcised[ia + 10] < 1.0 - CurrNegCircumcised[ia + 10]){
				MMCweights[ia] *= (1.0 - (CircPrevUlt - InitCircumcised[ia + 10]) / (1.0 -
					CurrNegCircumcised[ia + 10]));
			}
			else{
				MMCweights[ia] = 0.0;
			}
			if (MMCweights[ia] < 0.0){ MMCweights[ia] = 0.0; }
			Temp += (HIVnegMales[0] + HIVnegMales[1] + HIVnegMales[2] +
				HIVnegMales[3]) * MMCweights[ia];
		}
		if (CurrYear >= UltMMCyear){
			ProbMMCbase = UltMMCprob;
		}
		else if (CurrYear>MMCdataYear){
			ProbMMCbase = CircProbStored + (UltMMCprob - CircProbStored) *
				(CurrYear - MMCdataYear) / (UltMMCyear - MMCdataYear);
		}
		else{
			ProbMMCbase = NewMMC / Temp;
			if (ProbMMCbase>0.9999){ ProbMMCbase = 0.9999; }
		}
		if (CurrYear == MMCdataYear){
			CircProbStored = ProbMMCbase;
		}
		// Calculate CurrCircProb (cols N-Q)
		for (ia = 0; ia<81; ia++){
			for (ii = 0; ii<4; ii++){
				if (CircPrevUlt - InitCircumcised[ia + 10] < 1.0 - CurrNegCircumcised[ia + 10]){
					CurrCircProb[ia][ii] = MMCweights[ia] * ProbMMCbase + WeibullProb[ia] *
						(CircPrevUlt - InitCircumcised[ia + 10]) / (1.0 - CurrNegCircumcised[ia + 10]);
				}
				else{
					CurrCircProb[ia][ii] = WeibullProb[ia];
				}
				if (CurrCircProb[ia][ii] > 0.9999){ CurrCircProb[ia][ii] = 0.9999; }
			}
		}
	}

	// Additional outputs for Investment Case
	if (FixedUncertainty == 1){
		if (NewMMC>0.0 || CurrYear >= MMCdataYear){
			Temp = 0.0;
			for (ia = 0; ia < 81; ia++){
				HIVnegMales[0] = MHU_ST.NegNoHCT[ia] + MHU_ST.NegPastHCT[ia] +
					MHU_ST.RegHCT[ia] + MHU_ST.RegPrEP[ia] + MHU_STM.NegNoHCT[ia] +
					MHU_STM.NegPastHCT[ia] + MHU_STM.RegHCT[ia] + MHU_STM.RegPrEP[ia] +
					MHU_virgin.NegNoHCT[ia] + MHU_virgin.NegPastHCT[ia] +
					MHU_virgin.RegHCT[ia] + MHU_virgin.RegPrEP[ia];
				HIVnegMales[1] = MHU_LTH.NegNoHCT[ia] + MHU_LTH.NegPastHCT[ia] +
					MHU_LTH.RegHCT[ia] + MHU_LTH.RegPrEP[ia] + MHU_LTL.NegNoHCT[ia] +
					MHU_LTL.NegPastHCT[ia] + MHU_LTL.RegHCT[ia] + MHU_LTL.RegPrEP[ia];
				HIVnegMales[2] = MLU_ST.NegNoHCT[ia] + MLU_ST.NegPastHCT[ia] +
					MLU_ST.RegHCT[ia] + MLU_ST.RegPrEP[ia] + MLU_STM.NegNoHCT[ia] +
					MLU_STM.NegPastHCT[ia] + MLU_STM.RegHCT[ia] + MLU_STM.RegPrEP[ia] +
					MLU_virgin.NegNoHCT[ia] + MLU_virgin.NegPastHCT[ia] +
					MLU_virgin.RegHCT[ia] + MLU_virgin.RegPrEP[ia];
				HIVnegMales[3] = MLU_LTH.NegNoHCT[ia] + MLU_LTH.NegPastHCT[ia] +
					MLU_LTH.RegHCT[ia] + MLU_LTH.RegPrEP[ia] + MLU_LTL.NegNoHCT[ia] +
					MLU_LTL.NegPastHCT[ia] + MLU_LTL.RegHCT[ia] + MLU_LTL.RegPrEP[ia];
				Temp += (HIVnegMales[0] + HIVnegMales[1] + HIVnegMales[2] +
					HIVnegMales[3]) * MMCweights[ia];
				if (ia == 4){
					MMC10to14.out[CurrSim - 1][CurrYear - StartYear] = Temp * ProbMMCbase;
					Temp = 0.0;
				}
				if (ia == 9){
					MMC15to19.out[CurrSim - 1][CurrYear - StartYear] = Temp * ProbMMCbase;
					Temp = 0.0;
				}
				if (ia == 14){
					MMC20to24.out[CurrSim - 1][CurrYear - StartYear] = Temp * ProbMMCbase;
					Temp = 0.0;
				}
				if (ia == 39){
					MMC25to49.out[CurrSim - 1][CurrYear - StartYear] = Temp * ProbMMCbase;
					Temp = 0.0;
				}
				if (ia == 80){
					MMCover50.out[CurrSim - 1][CurrYear - StartYear] = Temp * ProbMMCbase;
				}
			}
			MMCprob10to14.out[CurrSim - 1][CurrYear - StartYear] = ProbMMCbase;
		}
		else{
			MMC10to14.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
			MMC15to19.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
			MMC20to24.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
			MMC25to49.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
			MMCover50.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
			MMCprob10to14.out[CurrSim - 1][CurrYear - StartYear] = 0.0;
		}
	}
}

void UpdateMC(Adult* Uncirc, Adult* Circ)
{
	int ia, is, UpperLimit, MMCoffset;
	double NegCirc, PosCirc;

	if(Uncirc->VirginInd==1){
		UpperLimit = 20;}
	else{
		UpperLimit = 81;}
	if(Uncirc->Risk==1 && Uncirc->MarriedInd==0){
		MMCoffset = 0;}
	else if(Uncirc->Risk==1 && Uncirc->MarriedInd==1){
		MMCoffset = 1;}
	else if(Uncirc->Risk==2 && Uncirc->MarriedInd==0){
		MMCoffset = 2;}
	else{
		MMCoffset = 3;}

	for(ia=0; ia<UpperLimit; ia++){
		NegCirc = CurrCircProb[ia][MMCoffset];
		PosCirc = CircProbPreMMC[ia+10];
		Circ->NegNoHCT[ia] += Uncirc->NegNoHCT[ia] * NegCirc;
		Uncirc->NegNoHCT[ia] *= (1.0 - NegCirc);
		Circ->NegPastHCT[ia] += Uncirc->NegPastHCT[ia] * NegCirc;
		Uncirc->NegPastHCT[ia] *= (1.0 - NegCirc);
		if(PrEPorVM==1){
			Circ->RegHCT[ia] += Uncirc->RegHCT[ia] * NegCirc;
			Uncirc->RegHCT[ia] *= (1.0 - NegCirc);
			Circ->RegPrEP[ia] += Uncirc->RegPrEP[ia] * NegCirc;
			Uncirc->RegPrEP[ia] *= (1.0 - NegCirc);
		}
		for(is=0; is<5; is++){
			Circ->PosNoHCT[ia][is] += Uncirc->PosNoHCT[ia][is] * PosCirc;
			Uncirc->PosNoHCT[ia][is] *= (1.0 - PosCirc);
			Circ->PosHCTpreHIV[ia][is] += Uncirc->PosHCTpreHIV[ia][is] * PosCirc;
			Uncirc->PosHCTpreHIV[ia][is] *= (1.0 - PosCirc);
			Circ->PosDiagnosedPreART[ia][is] += Uncirc->PosDiagnosedPreART[ia][is] * PosCirc;
			Uncirc->PosDiagnosedPreART[ia][is] *= (1.0 - PosCirc);
			Circ->OnARTpre500[ia][is] += Uncirc->OnARTpre500[ia][is] * PosCirc;
			Uncirc->OnARTpre500[ia][is] *= (1.0 - PosCirc);
			Circ->OnART500[ia][is] += Uncirc->OnART500[ia][is] * PosCirc;
			Uncirc->OnART500[ia][is] *= (1.0 - PosCirc);
			Circ->OnART350[ia][is] += Uncirc->OnART350[ia][is] * PosCirc;
			Uncirc->OnART350[ia][is] *= (1.0 - PosCirc);
			Circ->OnART200[ia][is] += Uncirc->OnART200[ia][is] * PosCirc;
			Uncirc->OnART200[ia][is] *= (1.0 - PosCirc);
		}
		for(is=0; is<4; is++){
			Circ->StoppedART[ia][is] += Uncirc->StoppedART[ia][is] * PosCirc;
			Uncirc->StoppedART[ia][is] *= (1.0 - PosCirc);
		}
	}
}

void CalcCurrMarriageRates()
{
	int ia, ig, iy;
	double PropnNeverMarried[77];

	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia <= 75; ia++){ // 0 corresponds to age 15 last birthday
			iy = CurrYear - (ia + 15) - StartYear; // Year of birth, in years after 1985
			if (ia < MarriageMin[ig] - 15){ MarriageRate[ia][ig] = 0.0; }
			else{
				PropnNeverMarried[ia] = 1.0 / (1.0 + pow((15.0 + ia - MarriageMin[ig]) *
					exp(-MarriageConstant[ig] - MarriageTrend[ig] * iy), 1.0 / MarriageShape[ig]));
				PropnNeverMarried[ia + 1] = 1.0 / (1.0 + pow((15.0 + (ia + 1.0) - MarriageMin[ig]) *
					exp(-MarriageConstant[ig] - MarriageTrend[ig] * iy), 1.0 / MarriageShape[ig]));
				MarriageRate[ia][ig] = 1.0 - PropnNeverMarried[ia + 1] / PropnNeverMarried[ia];
			}
		}
	}
}

void GetMarriageAndDivorceRates()
{
	int ia, ib, ig, ii, ij;
	double Temp1, Temp2, NewMarriages[2][2], CurrDivorceAdj;

	// Calculate AnnAIDSmort
	for(ia=0; ia<76; ia++){
		if(AIDSdeathsMarriedF[ia][0][0]>0.0){
			AnnAIDSmortF[ia][0][0] = AIDSdeathsMarriedF[ia][0][0]/(AIDSdeathsMarriedF[ia][0][0] +
				FH_LTH.Total_E[ia+5]);}
		else{
			AnnAIDSmortF[ia][0][0] = 0.0;}
		if(AIDSdeathsMarriedF[ia][0][1]>0.0){
			AnnAIDSmortF[ia][0][1] = AIDSdeathsMarriedF[ia][0][1]/(AIDSdeathsMarriedF[ia][0][1] +
				FH_LTL.Total_E[ia+5]);}
		else{
			AnnAIDSmortF[ia][0][1] = 0.0;}
		if(AIDSdeathsMarriedF[ia][1][0]>0.0){
			AnnAIDSmortF[ia][1][0] = AIDSdeathsMarriedF[ia][1][0]/(AIDSdeathsMarriedF[ia][1][0] +
				FL_LTH.Total_E[ia+5]);}
		else{
			AnnAIDSmortF[ia][1][0] = 0.0;}
		if(AIDSdeathsMarriedF[ia][1][1]>0.0){
			AnnAIDSmortF[ia][1][1] = AIDSdeathsMarriedF[ia][1][1]/(AIDSdeathsMarriedF[ia][1][1] +
				FL_LTL.Total_E[ia+5]);}
		else{
			AnnAIDSmortF[ia][1][1] = 0.0;}
		if(AIDSdeathsMarriedM[ia][0][0]>0.0){
			AnnAIDSmortM[ia][0][0] = AIDSdeathsMarriedM[ia][0][0]/(AIDSdeathsMarriedM[ia][0][0] +
				MHU_LTH.Total_E[ia+5] + MHC_LTH.Total_E[ia+5]);}
		else{
			AnnAIDSmortM[ia][0][0] = 0.0;}
		if(AIDSdeathsMarriedM[ia][0][1]>0.0){
			AnnAIDSmortM[ia][0][1] = AIDSdeathsMarriedM[ia][0][1]/(AIDSdeathsMarriedM[ia][0][1] +
				MHU_LTL.Total_E[ia+5] + MHC_LTL.Total_E[ia+5]);}
		else{
			AnnAIDSmortM[ia][0][1] = 0.0;}
		if(AIDSdeathsMarriedM[ia][1][0]>0.0){
			AnnAIDSmortM[ia][1][0] = AIDSdeathsMarriedM[ia][1][0]/(AIDSdeathsMarriedM[ia][1][0] +
				MLU_LTH.Total_E[ia+5] + MLC_LTH.Total_E[ia+5]);}
		else{
			AnnAIDSmortM[ia][1][0] = 0.0;}
		if(AIDSdeathsMarriedM[ia][1][1]>0.0){
			AnnAIDSmortM[ia][1][1] = AIDSdeathsMarriedM[ia][1][1]/(AIDSdeathsMarriedM[ia][1][1] +
				MLU_LTL.Total_E[ia+5] + MLC_LTL.Total_E[ia+5]);}
		else{
			AnnAIDSmortM[ia][1][1] = 0.0;}
	}

	CalcCurrMarriageRates();
	CurrDivorceAdj = DivorceAdj * pow(DivorceTrend, CurrYear - 2004);

	for(ia=0; ia<76; ia++){
		// Marriage rates
		for(ii=0; ii<2; ii++){
			for(ig=0; ig<2; ig++){
				ProbMarriageSE[ia][ii][ig] = MarriageRate[ia][ig];}
		}
		if(ia<15){
			ProbMarriageSE[ia][0][0] *= (CurrBehavDbn[ia + 5][0][0] + CurrBehavDbnMSM[ia + 5][0] +
				MHU_virgin.Total[ia + 5] + MHC_virgin.Total[ia + 5]) / (CurrBehavDbn[ia + 5][0][0] +
				CurrBehavDbnMSM[ia + 5][0]);
			ProbMarriageSE[ia][1][0] *= (CurrBehavDbn[ia + 5][2][0] + CurrBehavDbnMSM[ia + 5][1] +
				MLU_virgin.Total[ia + 5] + MLC_virgin.Total[ia + 5]) / (CurrBehavDbn[ia + 5][2][0] +
				CurrBehavDbnMSM[ia + 5][1]);
			ProbMarriageSE[ia][0][1] *= (CurrBehavDbn[ia+5][0][1] + FH_virgin.Total[ia+5])/
				CurrBehavDbn[ia+5][0][1];
			ProbMarriageSE[ia][1][1] *= (CurrBehavDbn[ia+5][2][1] + FL_virgin.Total[ia+5])/
				CurrBehavDbn[ia+5][2][1];
		}
		// Rates of widowhood in current year
		Temp1 = 0.0;
		Temp2 = 0.0;
		for(ib=0; ib<76; ib++){
			Temp1 += AgePrefLT[ia+5][ib+5][0] * CurrNonAIDSmortALB[ib+15][1];
			Temp2 += AgePrefLT[ia+5][ib+5][1] * CurrNonAIDSmortALB[ib+15][0];
		}
		CurrWidowhoodRate[ia][0] = Temp2;
		CurrWidowhoodRate[ia][1] = Temp1;
		for(ii=0; ii<2; ii++){
			for(ij=0; ij<2; ij++){
				ProbDivorceOrWidowM[ia][ii][ij] = (1.0 - CurrWidowhoodRate[ia][1]) *
					exp(-DivorceRate[ia][0] * CurrDivorceAdj) * (1.0 - SpouseAIDSmort(ia, ii, ij, 0));
				ProbDivorceOrWidowM[ia][ii][ij] += (1.0 - ProbDivorceOrWidowM[ia][ii][ij]) * ORremarriage[0] *
					MarriageRate[ia][0] / (ORremarriage[0] * MarriageRate[ia][0] + 1.0 - MarriageRate[ia][0]);
				ProbDivorceOrWidowM[ia][ii][ij] = 1.0 - ProbDivorceOrWidowM[ia][ii][ij];
				ProbDivorceOrWidowF[ia][ii][ij] = (1.0 - CurrWidowhoodRate[ia][0]) *
					exp(-DivorceRate[ia][1] * CurrDivorceAdj) * (1.0 - SpouseAIDSmort(ia, ii, ij, 1));
				ProbDivorceOrWidowF[ia][ii][ij] += (1.0 - ProbDivorceOrWidowF[ia][ii][ij]) * ORremarriage[1] *
					MarriageRate[ia][1] / (ORremarriage[1] * MarriageRate[ia][1] + 1.0 - MarriageRate[ia][1]);
				ProbDivorceOrWidowF[ia][ii][ij] = 1.0 - ProbDivorceOrWidowF[ia][ii][ij];
			}
		}
	}

	// Calculate remaining parameters in Mixing sheet (balancing factor and risk group prefs)
	for(ig=0; ig<2; ig++){
		for(ii=0; ii<2; ii++){
			NewMarriages[ii][ig] = 0.0;
			for(ia=0; ia<76; ia++){
				NewMarriages[ii][ig] += ProbMarriageSE[ia][ii][ig] * CurrBehavDbn[ia+5][ii*2][ig];
				if (ig == 0){
					NewMarriages[ii][ig] += ProbMarriageSE[ia][ii][0] * CurrBehavDbnMSM[ia + 5][ii];}
			}
		}
	}
	CurrLThigh[1][0] = (1.0 - Assortativeness) + Assortativeness * NewMarriages[0][0]/
		(NewMarriages[0][0] + NewMarriages[1][0]);
	CurrLThigh[1][1] = Assortativeness * NewMarriages[0][0]/(NewMarriages[0][0] +
		NewMarriages[1][0]);
	CurrLThigh[0][0] = NewMarriages[0][1] * CurrLThigh[1][0]/(NewMarriages[0][1] *
		CurrLThigh[1][0] + NewMarriages[1][1] * CurrLThigh[1][1]);
	CurrLThigh[0][1] = NewMarriages[0][1] * (1.0 - CurrLThigh[1][0])/(NewMarriages[0][1] *
		(1.0 - CurrLThigh[1][0]) + NewMarriages[1][1] * (1.0 - CurrLThigh[1][1]));
}

double SpouseAIDSmort(int ia, int ir1, int ir2, int ig)
{
	int ib;
	double Temp;

	// ir1 is risk group of the individual, ir2 is the risk group of their partner
	// (whose mortality risk is being calculated), ig is the sex of the individual

	Temp = 0.0;
	for(ib=0; ib<76; ib++){
		if(ig==0){
			Temp += AgePrefLT[ia+5][ib+5][0] * AnnAIDSmortF[ib][ir2][ir1];}
		else{
			Temp += AgePrefLT[ia+5][ib+5][1] * AnnAIDSmortM[ib][ir2][ir1];}
	}

	return Temp;
}

void UpdateMarital(Adult* Single, Adult* MarriedH, Adult* MarriedL)
{
	int ia, is, ig, ii;
	double EnterLT1, EnterLT2, ExitLT1, ExitLT2;
	double NewMarried1, NewMarried2, NewSingle1, NewSingle2;

	ig = Single->Sex;
	ii = Single->Risk-1;

	for(ia=5; ia<81; ia++){
		EnterLT1 = ProbMarriageSE[ia-5][ii][ig] * CurrLThigh[ig][ii];
		EnterLT2 = ProbMarriageSE[ia-5][ii][ig] * (1.0 - CurrLThigh[ig][ii]);
		if(ig==0 && Single->MSMind==0){
			ExitLT1 = ProbDivorceOrWidowM[ia-5][ii][0];
			ExitLT2 = ProbDivorceOrWidowM[ia-5][ii][1];
		}
		else if(ig==1){
			ExitLT1 = ProbDivorceOrWidowF[ia-5][ii][0];
			ExitLT2 = ProbDivorceOrWidowF[ia-5][ii][1];
		}
		else { // MSM: set to zero to avoid double-counting divorce & widowhood
			ExitLT1 = 0.0;
			ExitLT2 = 0.0;
		}
		// Negative, never tested
		NewMarried1 = Single->NegNoHCT[ia] * EnterLT1;
		NewMarried2 = Single->NegNoHCT[ia] * EnterLT2;
		NewSingle1 = MarriedH->NegNoHCT[ia] * ExitLT1;
		NewSingle2 = MarriedL->NegNoHCT[ia] * ExitLT2;
		Single->NegNoHCT[ia] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
		MarriedH->NegNoHCT[ia] += NewMarried1 - NewSingle1;
		MarriedL->NegNoHCT[ia] += NewMarried2 - NewSingle2;
		// Negative, previously tested
		NewMarried1 = Single->NegPastHCT[ia] * EnterLT1;
		NewMarried2 = Single->NegPastHCT[ia] * EnterLT2;
		NewSingle1 = MarriedH->NegPastHCT[ia] * ExitLT1;
		NewSingle2 = MarriedL->NegPastHCT[ia] * ExitLT2;
		Single->NegPastHCT[ia] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
		MarriedH->NegPastHCT[ia] += NewMarried1 - NewSingle1;
		MarriedL->NegPastHCT[ia] += NewMarried2 - NewSingle2;
		if(PrEPorVM==1){
			// RegHCT
			NewMarried1 = Single->RegHCT[ia] * EnterLT1;
			NewMarried2 = Single->RegHCT[ia] * EnterLT2;
			NewSingle1 = MarriedH->RegHCT[ia] * ExitLT1;
			NewSingle2 = MarriedL->RegHCT[ia] * ExitLT2;
			Single->RegHCT[ia] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->RegHCT[ia] += NewMarried1 - NewSingle1;
			MarriedL->RegHCT[ia] += NewMarried2 - NewSingle2;
			// RegPrEP
			NewMarried1 = Single->RegPrEP[ia] * EnterLT1;
			NewMarried2 = Single->RegPrEP[ia] * EnterLT2;
			NewSingle1 = MarriedH->RegPrEP[ia] * ExitLT1;
			NewSingle2 = MarriedL->RegPrEP[ia] * ExitLT2;
			Single->RegPrEP[ia] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->RegPrEP[ia] += NewMarried1 - NewSingle1;
			MarriedL->RegPrEP[ia] += NewMarried2 - NewSingle2;
			// RegVM
			NewMarried1 = Single->RegVM[ia] * EnterLT1;
			NewMarried2 = Single->RegVM[ia] * EnterLT2;
			NewSingle1 = MarriedH->RegVM[ia] * ExitLT1;
			NewSingle2 = MarriedL->RegVM[ia] * ExitLT2;
			Single->RegVM[ia] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->RegVM[ia] += NewMarried1 - NewSingle1;
			MarriedL->RegVM[ia] += NewMarried2 - NewSingle2;
		}
		for(is=0; is<5; is++){
			// Positive, never tested
			NewMarried1 = Single->PosNoHCT[ia][is] * EnterLT1;
			NewMarried2 = Single->PosNoHCT[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->PosNoHCT[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->PosNoHCT[ia][is] * ExitLT2;
			Single->PosNoHCT[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->PosNoHCT[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->PosNoHCT[ia][is] += NewMarried2 - NewSingle2;
			// Positive, tested before HIV
			NewMarried1 = Single->PosHCTpreHIV[ia][is] * EnterLT1;
			NewMarried2 = Single->PosHCTpreHIV[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->PosHCTpreHIV[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->PosHCTpreHIV[ia][is] * ExitLT2;
			Single->PosHCTpreHIV[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->PosHCTpreHIV[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->PosHCTpreHIV[ia][is] += NewMarried2 - NewSingle2;
			// Diagnosed positive, not yet on ART
			NewMarried1 = Single->PosDiagnosedPreART[ia][is] * EnterLT1;
			NewMarried2 = Single->PosDiagnosedPreART[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->PosDiagnosedPreART[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->PosDiagnosedPreART[ia][is] * ExitLT2;
			Single->PosDiagnosedPreART[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->PosDiagnosedPreART[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->PosDiagnosedPreART[ia][is] += NewMarried2 - NewSingle2;
			// On ART, baseline CD4 500+
			NewMarried1 = Single->OnARTpre500[ia][is] * EnterLT1;
			NewMarried2 = Single->OnARTpre500[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->OnARTpre500[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->OnARTpre500[ia][is] * ExitLT2;
			Single->OnARTpre500[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->OnARTpre500[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->OnARTpre500[ia][is] += NewMarried2 - NewSingle2;
			// On ART, baseline CD4 350-499
			NewMarried1 = Single->OnART500[ia][is] * EnterLT1;
			NewMarried2 = Single->OnART500[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->OnART500[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->OnART500[ia][is] * ExitLT2;
			Single->OnART500[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->OnART500[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->OnART500[ia][is] += NewMarried2 - NewSingle2;
			// On ART, baseline CD4 200-349
			NewMarried1 = Single->OnART350[ia][is] * EnterLT1;
			NewMarried2 = Single->OnART350[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->OnART350[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->OnART350[ia][is] * ExitLT2;
			Single->OnART350[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->OnART350[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->OnART350[ia][is] += NewMarried2 - NewSingle2;
			// On ART, baseline CD4 <200
			NewMarried1 = Single->OnART200[ia][is] * EnterLT1;
			NewMarried2 = Single->OnART200[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->OnART200[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->OnART200[ia][is] * ExitLT2;
			Single->OnART200[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->OnART200[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->OnART200[ia][is] += NewMarried2 - NewSingle2;
		}
		// Stopped ART
		for(is=0; is<4; is++){
			NewMarried1 = Single->StoppedART[ia][is] * EnterLT1;
			NewMarried2 = Single->StoppedART[ia][is] * EnterLT2;
			NewSingle1 = MarriedH->StoppedART[ia][is] * ExitLT1;
			NewSingle2 = MarriedL->StoppedART[ia][is] * ExitLT2;
			Single->StoppedART[ia][is] += NewSingle1 + NewSingle2 - NewMarried1 - NewMarried2;
			MarriedH->StoppedART[ia][is] += NewMarried1 - NewSingle1;
			MarriedL->StoppedART[ia][is] += NewMarried2 - NewSingle2;
		}
	}
}

void UpdateAllDemog()
{
	// Adult males
	MHU_virgin.UpdateDemog();
	MHC_virgin.UpdateDemog();
	MHU_ST.UpdateDemog();
	MHC_ST.UpdateDemog();
	MHU_STM.UpdateDemog();
	MHC_STM.UpdateDemog();
	MHU_LTH.UpdateDemog();
	MHC_LTH.UpdateDemog();
	MHU_LTL.UpdateDemog();
	MHC_LTL.UpdateDemog();
	MLU_virgin.UpdateDemog();
	MLC_virgin.UpdateDemog();
	MLU_ST.UpdateDemog();
	MLC_ST.UpdateDemog();
	MLU_STM.UpdateDemog();
	MLC_STM.UpdateDemog();
	MLU_LTH.UpdateDemog();
	MLC_LTH.UpdateDemog();
	MLU_LTL.UpdateDemog();
	MLC_LTL.UpdateDemog();

	// Adult females
	FH_virgin.UpdateDemog();
	FH_ST.UpdateDemog();
	FH_SW.UpdateDemog();
	FH_LTH.UpdateDemog();
	FH_LTL.UpdateDemog();
	FL_virgin.UpdateDemog();
	FL_ST.UpdateDemog();
	FL_LTH.UpdateDemog();
	FL_LTL.UpdateDemog();

	// Migration in children
	MaleChild.UpdateMigP();
	FemChild.UpdateMigP();

	// Code for aging of kids into age 10 group
	MoveIntoAdultGroups();
}

void MoveIntoAdultGroups()
{
	double Temp, Temp2, TempByS[4];
	int im, is;

	// Uninfected males
	Temp = 0.0;
	for(im=120; im<132; im++){
		Temp += MaleChild.NegChildFF[im];}
	MHU_virgin.NegNoHCT[0] = Temp * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
	MHC_virgin.NegNoHCT[0] = Temp * HighRiskPropn[0] * CurrCircPrev10;
	MLU_virgin.NegNoHCT[0] = Temp * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
	MLC_virgin.NegNoHCT[0] = Temp * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;

	// Infected males, undiagnosed
	Temp = 0.0;
	TempByS[3] = 0.0;
	for(im=120; im<132; im++){
		Temp += MaleChild.PosChildAtBirthNoPMTCT[im] + MaleChild.PosChildAtBirthPMTCT[im] +
			MaleChild.PosChildAfterBirth[im];
		TempByS[3] += MaleChild.ARTeligible[im];
	}
	for(is=0; is<3; is++){
		TempByS[is] = Temp/3.0;}
	for(is=0; is<4; is++){
		MHU_virgin.PosNoHCT[0][is+1] = TempByS[is] * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
		MHC_virgin.PosNoHCT[0][is+1] = TempByS[is] * HighRiskPropn[0] * CurrCircPrev10;
		MLU_virgin.PosNoHCT[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
		MLC_virgin.PosNoHCT[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;
	}

	// Infected males, diagnosed but ART-naive
	Temp = 0.0;
	TempByS[3] = 0.0;
	for (im = 120; im<132; im++){
		Temp += MaleChild.DiagChildAtBirthNoPMTCT[im] + MaleChild.DiagChildAtBirthPMTCT[im] +
			MaleChild.DiagChildAfterBirth[im];
		TempByS[3] += MaleChild.DiagARTeligible[im];
	}
	for (is = 0; is<3; is++){
		TempByS[is] = Temp / 3.0;}
	for (is = 0; is<4; is++){
		MHU_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
		MHC_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * HighRiskPropn[0] * CurrCircPrev10;
		MLU_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
		MLC_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;
	}

	// Infected males, on ART
	Temp = 0.0;
	Temp2 = 0.0;
	for(im=120; im<132; im++){
		Temp += MaleChild.OnARTlate1st3m[im] + MaleChild.OnARTlateAfter3m[im];}
	for (is = 0; is<5; is++){ Temp2 += CumPaedARTlate[10][is]; }
	for (is = 0; is<4; is++){
		if (Temp2 > 0.0){ TempByS[is] = Temp * CumPaedARTlate[10][is] / Temp2; }
		else{ TempByS[is] = 0.0; }
	}
	if (Temp2 > 0.0){ TempByS[3] += Temp * CumPaedARTlate[10][4] / Temp2; }
	for(is=0; is<4; is++){
		MHU_virgin.OnART200[0][is+1] = TempByS[is] * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
		MHC_virgin.OnART200[0][is+1] = TempByS[is] * HighRiskPropn[0] * CurrCircPrev10;
		MLU_virgin.OnART200[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
		MLC_virgin.OnART200[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;
	}
	Temp = 0.0;
	Temp2 = 0.0;
	for (im = 120; im<132; im++){ Temp += MaleChild.OnARTearly[im]; }
	for (is = 0; is<5; is++){ Temp2 += CumPaedARTearly[10][is]; }
	for (is = 0; is<4; is++){
		if (Temp2 > 0.0){ TempByS[is] = Temp * CumPaedARTearly[10][is] / Temp2; }
		else{ TempByS[is] = 0.0; }
	}
	if (Temp2 > 0.0){ TempByS[3] += Temp * CumPaedARTearly[10][4] / Temp2; }
	for (is = 0; is<4; is++){
		MHU_virgin.OnART500[0][is + 1] = TempByS[is] * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
		MHC_virgin.OnART500[0][is + 1] = TempByS[is] * HighRiskPropn[0] * CurrCircPrev10;
		MLU_virgin.OnART500[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
		MLC_virgin.OnART500[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;
	}

	// Infected males who have stopped ART
	Temp = 0.0;
	for(im=120; im<132; im++){
		Temp += MaleChild.StoppedART[im];}
	MHU_virgin.StoppedART[0][3] = Temp * HighRiskPropn[0] * (1.0 - CurrCircPrev10);
	MHC_virgin.StoppedART[0][3] = Temp * HighRiskPropn[0] * CurrCircPrev10;
	MLU_virgin.StoppedART[0][3] = Temp * (1.0 - HighRiskPropn[0]) * (1.0 - CurrCircPrev10);
	MLC_virgin.StoppedART[0][3] = Temp * (1.0 - HighRiskPropn[0]) * CurrCircPrev10;

	// Set number of male 10-year olds in Child groups to zero
	for(im=120; im<=132; im++){
		MaleChild.NegChildFF[im] = 0.0;
		MaleChild.PosChildAtBirthNoPMTCT[im] = 0.0;
		MaleChild.PosChildAtBirthPMTCT[im] = 0.0;
		MaleChild.PosChildAfterBirth[im] = 0.0;
		MaleChild.ARTeligible[im] = 0.0;
		MaleChild.DiagChildAtBirthNoPMTCT[im] = 0.0;
		MaleChild.DiagChildAtBirthPMTCT[im] = 0.0;
		MaleChild.DiagChildAfterBirth[im] = 0.0;
		MaleChild.DiagARTeligible[im] = 0.0;
		MaleChild.OnARTlate1st3m[im] = 0.0;
		MaleChild.OnARTlateAfter3m[im] = 0.0;
		MaleChild.OnARTearly[im] = 0.0;
		MaleChild.StoppedART[im] = 0.0;
	}

	// Uninfected females
	Temp = 0.0;
	for(im=120; im<132; im++){
		Temp += FemChild.NegChildFF[im];}
	FH_virgin.NegNoHCT[0] = Temp * HighRiskPropn[1];
	FL_virgin.NegNoHCT[0] = Temp * (1.0 - HighRiskPropn[1]);

	// Infected females, undiagnosed
	Temp = 0.0;
	TempByS[3] = 0.0;
	for(im=120; im<132; im++){
		Temp += FemChild.PosChildAtBirthNoPMTCT[im] + FemChild.PosChildAtBirthPMTCT[im] +
			FemChild.PosChildAfterBirth[im];
		TempByS[3] += FemChild.ARTeligible[im];
	}
	for(is=0; is<3; is++){
		TempByS[is] = Temp/3.0;}
	for(is=0; is<4; is++){
		FH_virgin.PosNoHCT[0][is+1] = TempByS[is] * HighRiskPropn[1];
		FL_virgin.PosNoHCT[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[1]);
	}

	// Infected females, diagnosed but ART-naive
	Temp = 0.0;
	TempByS[3] = 0.0;
	for (im = 120; im<132; im++){
		Temp += FemChild.DiagChildAtBirthNoPMTCT[im] + FemChild.DiagChildAtBirthPMTCT[im] +
			FemChild.DiagChildAfterBirth[im];
		TempByS[3] += FemChild.DiagARTeligible[im];
	}
	for (is = 0; is<3; is++){
		TempByS[is] = Temp / 3.0;}
	for (is = 0; is<4; is++){
		FH_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * HighRiskPropn[1];
		FL_virgin.PosDiagnosedPreART[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[1]);
	}

	// Infected females, on ART
	Temp = 0.0;
	Temp2 = 0.0;
	for(im=120; im<132; im++){
		Temp += FemChild.OnARTlate1st3m[im] + FemChild.OnARTlateAfter3m[im];}
	for (is = 0; is<5; is++){ Temp2 += CumPaedARTlate[10][is]; }
	for (is = 0; is<4; is++){
		if (Temp2 > 0.0){ TempByS[is] = Temp * CumPaedARTlate[10][is] / Temp2; }
		else{ TempByS[is] = 0.0; }
	}
	if (Temp2 > 0.0){ TempByS[3] += Temp * CumPaedARTlate[10][4] / Temp2; }
	for(is=0; is<4; is++){
		FH_virgin.OnART200[0][is+1] = TempByS[is] * HighRiskPropn[1];
		FL_virgin.OnART200[0][is+1] = TempByS[is] * (1.0 - HighRiskPropn[1]);
	}
	Temp = 0.0;
	Temp2 = 0.0;
	for (im = 120; im<132; im++){ Temp += FemChild.OnARTearly[im]; }
	for (is = 0; is<5; is++){ Temp2 += CumPaedARTearly[10][is]; }
	for (is = 0; is<4; is++){
		if (Temp2 > 0.0){ TempByS[is] = Temp * CumPaedARTearly[10][is] / Temp2; }
		else{ TempByS[is] = 0.0; }
	}
	if (Temp2 > 0.0){ TempByS[3] += Temp * CumPaedARTearly[10][4] / Temp2; }
	for (is = 0; is<4; is++){
		FH_virgin.OnART500[0][is + 1] = TempByS[is] * HighRiskPropn[1];
		FL_virgin.OnART500[0][is + 1] = TempByS[is] * (1.0 - HighRiskPropn[1]);
	}

	// Infected females who have stopped ART
	Temp = 0.0;
	for(im=120; im<132; im++){
		Temp += FemChild.StoppedART[im];}
	FH_virgin.StoppedART[0][3] = Temp * HighRiskPropn[1];
	FL_virgin.StoppedART[0][3] = Temp * (1.0 - HighRiskPropn[1]);

	// Set number of female 10-year olds in Child groups to zero
	for(im=120; im<=132; im++){
		FemChild.NegChildFF[im] = 0.0;
		FemChild.PosChildAtBirthNoPMTCT[im] = 0.0;
		FemChild.PosChildAtBirthPMTCT[im] = 0.0;
		FemChild.PosChildAfterBirth[im] = 0.0;
		FemChild.ARTeligible[im] = 0.0;
		FemChild.DiagChildAtBirthNoPMTCT[im] = 0.0;
		FemChild.DiagChildAtBirthPMTCT[im] = 0.0;
		FemChild.DiagChildAfterBirth[im] = 0.0;
		FemChild.DiagARTeligible[im] = 0.0;
		FemChild.OnARTlate1st3m[im] = 0.0;
		FemChild.OnARTlateAfter3m[im] = 0.0;
		FemChild.OnARTearly[im] = 0.0;
		FemChild.StoppedART[im] = 0.0;
	}
}

void UpdateDebut(Adult* Virgin, Adult* SexExp, Adult* SexExp2, int ind)
{
	int ia, is, ig, group;
	double rate, FutureAdj, MSMfraction;

	ig = Virgin->Sex;
	group = Virgin->Risk - 1;
	if (ig == 0){ MSMfraction = MSMpropn; }
	else{ MSMfraction = 0.0; }

	FutureAdj = 1.0;
	if (VaryFutureInterventions == 1 && CurrYear > 2011){
		if (CurrYear >= 2016){ FutureAdj = FutureInterventions.out[CurrSim - 1][13]; }
		else{
			FutureAdj = 1.0 + 0.2 * (CurrYear - 2011) *
				(FutureInterventions.out[CurrSim - 1][13] - 1.0);
		}
	}

	for(ia=0; ia<20+ind; ia++){
		rate = 1.0 - pow(1.0 - DebutProb[ia+1-ind][group][ig], FutureAdj);
		SexExp->NegNoHCT[ia] += Virgin->NegNoHCT[ia] * rate * (1.0 - MSMfraction);
		if (ig == 0){ SexExp2->NegNoHCT[ia] += Virgin->NegNoHCT[ia] * rate * MSMfraction; }
		Virgin->NegNoHCT[ia] *= (1.0 - rate);
		SexExp->NegPastHCT[ia] += Virgin->NegPastHCT[ia] * rate * (1.0 - MSMfraction);
		if (ig == 0){ SexExp2->NegPastHCT[ia] += Virgin->NegPastHCT[ia] * rate * MSMfraction; }
		Virgin->NegPastHCT[ia] *= (1.0 - rate);
		// Currently the model doesn't consider regular HCT, PrEP or VM in virgins, so
		// no updates are required for the corresponding states.
		for(is=1; is<5; is++){
			SexExp->PosNoHCT[ia][is] += Virgin->PosNoHCT[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->PosNoHCT[ia][is] += Virgin->PosNoHCT[ia][is] * rate * MSMfraction; }
			Virgin->PosNoHCT[ia][is] *= (1.0 - rate);
			SexExp->PosHCTpreHIV[ia][is] += Virgin->PosHCTpreHIV[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->PosHCTpreHIV[ia][is] += Virgin->PosHCTpreHIV[ia][is] * rate * MSMfraction; }
			Virgin->PosHCTpreHIV[ia][is] *= (1.0 - rate);
			SexExp->PosDiagnosedPreART[ia][is] += Virgin->PosDiagnosedPreART[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->PosDiagnosedPreART[ia][is] += Virgin->PosDiagnosedPreART[ia][is] * rate * MSMfraction; }
			Virgin->PosDiagnosedPreART[ia][is] *= (1.0 - rate);
		}
		for(is=0; is<5; is++){
			SexExp->OnARTpre500[ia][is] += Virgin->OnARTpre500[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->OnARTpre500[ia][is] += Virgin->OnARTpre500[ia][is] * rate * MSMfraction; }
			Virgin->OnARTpre500[ia][is] *= (1.0 - rate);
			SexExp->OnART500[ia][is] += Virgin->OnART500[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->OnART500[ia][is] += Virgin->OnART500[ia][is] * rate * MSMfraction; }
			Virgin->OnART500[ia][is] *= (1.0 - rate);
			SexExp->OnART350[ia][is] += Virgin->OnART350[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->OnART350[ia][is] += Virgin->OnART350[ia][is] * rate * MSMfraction; }
			Virgin->OnART350[ia][is] *= (1.0 - rate);
			SexExp->OnART200[ia][is] += Virgin->OnART200[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->OnART200[ia][is] += Virgin->OnART200[ia][is] * rate * MSMfraction; }
			Virgin->OnART200[ia][is] *= (1.0 - rate);
		}
		for(is=0; is<4; is++){
			SexExp->StoppedART[ia][is] += Virgin->StoppedART[ia][is] * rate * (1.0 - MSMfraction);
			if (ig == 0){ SexExp2->StoppedART[ia][is] += Virgin->StoppedART[ia][is] * rate * MSMfraction; }
			Virgin->StoppedART[ia][is] *= (1.0 - rate);
		}
	}
}

void UpdateStartTot()
{
	// Adult males
	MHU_virgin.UpdateStartTotal();
	MHC_virgin.UpdateStartTotal();
	MHU_ST.UpdateStartTotal();
	MHC_ST.UpdateStartTotal();
	MHU_STM.UpdateStartTotal();
	MHC_STM.UpdateStartTotal();
	MHU_LTH.UpdateStartTotal();
	MHC_LTH.UpdateStartTotal();
	MHU_LTL.UpdateStartTotal();
	MHC_LTL.UpdateStartTotal();
	MLU_virgin.UpdateStartTotal();
	MLC_virgin.UpdateStartTotal();
	MLU_ST.UpdateStartTotal();
	MLC_ST.UpdateStartTotal();
	MLU_STM.UpdateStartTotal();
	MLC_STM.UpdateStartTotal();
	MLU_LTH.UpdateStartTotal();
	MLC_LTH.UpdateStartTotal();
	MLU_LTL.UpdateStartTotal();
	MLC_LTL.UpdateStartTotal();

	// Adult females
	FH_virgin.UpdateStartTotal();
	FH_ST.UpdateStartTotal();
	FH_SW.UpdateStartTotal();
	FH_LTH.UpdateStartTotal();
	FH_LTL.UpdateStartTotal();
	FL_virgin.UpdateStartTotal();
	FL_ST.UpdateStartTotal();
	FL_LTH.UpdateStartTotal();
	FL_LTL.UpdateStartTotal();

	// No additional calcs required for children
}

void ResultsAtEndOfYr()
{
	int ia, ig, ii, iy;
	double Temp[2];

	iy = CurrYear - StartYear;
	Temp[0] = 0.0;
	Temp[1] = 0.0;

	// Results in the Monthly sheet

	for(ia=0; ia<81; ia++){
		for(ig=0; ig<2; ig++){
			NonAIDSdeathsA[ia][ig] = TotalPop[ia+10][ig] * CurrNonAIDSmortALB[ia+10][ig];
			TotDeathsByAge[ia][ig] = NonAIDSdeathsA[ia][ig] + AIDSdeathsByAge[ia][ig];
		}
	}

	// Results in the main Results sheet

	for(ia=0; ia<15; ia++){
		for(ig=0; ig<2; ig++){
			AdultMortBy5yr[iy][ia][ig] = 0.5 * (TotDeathsByAge[ia * 5 + 4][ig] + TotDeathsByAge[ia * 5 + 9][ig]);
			NonAIDSmortBy5yr[iy][ia][ig] = 0.5 * (NonAIDSdeathsA[ia * 5 + 4][ig] + NonAIDSdeathsA[ia * 5 + 9][ig]);
			for(ii=0; ii<4; ii++){
				AdultMortBy5yr[iy][ia][ig] += TotDeathsByAge[ia*5+5+ii][ig];
				NonAIDSmortBy5yr[iy][ia][ig] += NonAIDSdeathsA[ia * 5 + 5 + ii][ig];
			}
		}
	}
	AdultMortBy5yr[iy][15][0] = 0.5 * TotDeathsByAge[79][0] + TotDeathsByAge[80][0];
	AdultMortBy5yr[iy][15][1] = 0.5 * TotDeathsByAge[79][1] + TotDeathsByAge[80][1];
	NonAIDSmortBy5yr[iy][15][0] = 0.5 * NonAIDSdeathsA[79][0] + NonAIDSdeathsA[80][0];
	NonAIDSmortBy5yr[iy][15][1] = 0.5 * NonAIDSdeathsA[79][1] + NonAIDSdeathsA[80][1];

	for(ia=15; ia<91; ia++){
		Temp[0] += 0.5 * (TotalNaiveElig_S[ia][0] + TotalNaiveElig[ia][0]);
		Temp[1] += 0.5 * (TotalNaiveElig_S[ia][1] + TotalNaiveElig[ia][1]);
	}
	RateARTstartU200[iy][0] = AdultsNewARTbyCD4[3][0]/Temp[0];
	RateARTstartU200[iy][1] = AdultsNewARTbyCD4[3][1]/Temp[1];
	Temp[0] = 0.0;
	for (ia = 0; ia<10; ia++){
		Temp[0] += 0.5 * (TotalNaiveElig_S[ia][0] + TotalNaiveElig[ia][0] +
			TotalNaiveElig_S[ia][1] + TotalNaiveElig[ia][1]);
	}
	RateARTstartLate[iy] = ChildNewARTlate / Temp[0];

	if(CalibAIDStrend==1 && iy<10 && iy>=5){
		OIsDiagnosedByYr[iy-5] = 0.0;
		for(ia=0; ia<10; ia++){
			for(ig=0; ig<2; ig++){
				OIsDiagnosedByYr[iy-5] += NewOIdiagnoses[ia][ig];}
		}
	}
	if(CalibAIDSage==1 && iy<10 && iy>=8){
		if(iy==8){
			for(ia=0; ia<10; ia++){
				for(ig=0; ig<2; ig++){
					OIsDiagnosedProfile[ia][ig] = 0.0;}
			}
		}
		for(ia=0; ia<10; ia++){
			for(ig=0; ig<2; ig++){
				OIsDiagnosedProfile[ia][ig] += NewOIdiagnoses[ia][ig];}
		}
	}
	if (CalibHCTprev == 1 && CurrYear >=2004){
		Temp[0] = (NewlyTestedPos[0] + NewlyTestedPos[1] + (NewlyTestedNeg[0] + NewlyTestedNeg[1]) *
			(1.0 - RapidDiagSp)) / (NewlyTestedPos[0] + NewlyTestedPos[1] +
			NewlyTestedNeg[0] + NewlyTestedNeg[1]);
		if (CurrYear <= 2008){ ModelPrevHCT[CurrYear - 2004] = Temp[0];}
		if (CurrYear == 2010){ ModelPrevHCT[5] = Temp[0]; }
		if (CurrYear == 2012){ ModelPrevHCT[6] = Temp[0]; }
		if (CurrYear >= 2015 && CurrYear<2021){ ModelPrevHCT[iy-23] = Temp[0]; }
	}
	if (CalibHCTprevP == 1 && CurrYear >= 2015 && CurrYear <= 2020){
		ModelPrevHCT_P[CurrYear - 2015] = (NewlyTestedPos[2] + NewlyTestedNeg[2] *
			(1.0 - RapidDiagSp)) / (NewlyTestedPos[2] + NewlyTestedNeg[2]);
	}

	// Paediatric mortality outputs needed in calibration
	if (FixedUncertainty == 1 || CalibDeathsP == 1 || CalibChildPIP == 1){
		// Total deaths in children
		for (ia = 0; ia < 11; ia++){
			for (ig = 0; ig < 2; ig++){
				TotDeathsByAgeP[ia][ig] = AIDSdeathsByAgeP[ia][ig] +
					NonAIDSdeathsP[ia][ig];
			}
		}
		ChildMortBy5yr[iy][0][0] = TotDeathsByAgeP[0][0];
		ChildMortBy5yr[iy][0][1] = TotDeathsByAgeP[0][1];
		ChildMortBy5yr[iy][1][0] = TotDeathsByAgeP[1][0] + TotDeathsByAgeP[2][0] +
			TotDeathsByAgeP[3][0] + TotDeathsByAgeP[4][0];
		ChildMortBy5yr[iy][1][1] = TotDeathsByAgeP[1][1] + TotDeathsByAgeP[2][1] +
			TotDeathsByAgeP[3][1] + TotDeathsByAgeP[4][1];
		ChildMortBy5yr[iy][2][0] = TotDeathsByAgeP[5][0] + TotDeathsByAgeP[6][0] +
			TotDeathsByAgeP[7][0] + TotDeathsByAgeP[8][0] + TotDeathsByAgeP[9][0];
		ChildMortBy5yr[iy][2][1] = TotDeathsByAgeP[5][1] + TotDeathsByAgeP[6][1] +
			TotDeathsByAgeP[7][1] + TotDeathsByAgeP[8][1] + TotDeathsByAgeP[9][1];
		ChildMortBy5yr[iy][3][0] = 0.0;
		ChildMortBy5yr[iy][3][1] = 0.0;
		for (ia = 0; ia < 5; ia++){
			ChildMortBy5yr[iy][3][0] += AIDSdeathsByAge[ia][0] + NonAIDSdeathsA[ia][0];
			ChildMortBy5yr[iy][3][1] += AIDSdeathsByAge[ia][1] + NonAIDSdeathsA[ia][1];
		}
		Deaths0M.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][0][0];
		Deaths0F.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][0][1];
		Deaths1M.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][1][0];
		Deaths1F.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][1][1];
		Deaths5M.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][2][0];
		Deaths5F.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][2][1];
		Deaths10M.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][3][0];
		Deaths10F.out[CurrSim - 1][iy] = ChildMortBy5yr[iy][3][1];
	}
	if (FixedUncertainty == 1 || CalibChildPIP == 1){
		ChildAIDSmortDiag[iy][0] = AIDSdeathsDiagP[0];
		ChildAIDSmortDiag[iy][1] = AIDSdeathsDiagP[1];
		ChildAIDSmortART[iy][0] = AIDSdeathsART_P[0];
		ChildAIDSmortART[iy][1] = AIDSdeathsART_P[1];
		DiagDeaths1to4.out[CurrSim - 1][iy] = AIDSdeathsDiagP[0];
		DiagDeaths5to9.out[CurrSim - 1][iy] = AIDSdeathsDiagP[1];
		ARTdeaths1to4.out[CurrSim - 1][iy] = AIDSdeathsART_P[0];
		ARTdeaths5to9.out[CurrSim - 1][iy] = AIDSdeathsART_P[1];
	}

	StartingART0to14.out[CurrSim - 1][iy] = 0.0;
	for (ia = 0; ia < 15; ia++){
		StartingART0to14.out[CurrSim - 1][iy] += PaedNewARTbyAge[ia];
	}

	// Fraction of infections acquired recently
	if (CalibANCprev == 1){
		for (ia = 0; ia < 6; ia++){
			FractionRecentF[ia][CurrYear - StartYear + 1] = 0.0;
			for (ii = 0; ii < 5; ii++){
				FractionRecentF[ia][CurrYear - StartYear + 1] += NewHIVbyAgeSex[4 + ia * 5 + ii][1];
				// Note we use 4+, not 5+, because we haven't yet updated ages.
			}
		}
	}

	// New ART by baseline CD4 count
	if (FixedUncertainty == 1 || CalibCD4atARTstart == 1){
		NewARTunder200.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[3][0] + AdultsNewARTbyCD4[3][1];
		NewART200to349.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[2][0] + AdultsNewARTbyCD4[2][1];
		NewART350to499.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[1][0] + AdultsNewARTbyCD4[1][1];
		NewARTover500.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[0][0] + AdultsNewARTbyCD4[0][1];
		NewARTunder200F.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[3][1];
		NewART200to349F.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[2][1];
		NewART350to499F.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[1][1];
		NewARTover500F.out[CurrSim - 1][iy] = AdultsNewARTbyCD4[0][1];
	}

	// Generate outputs not required for calculating likelihood
	if (FixedUncertainty == 1){
		// Mortality outputs
		Deaths20M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][1][0];
		Deaths25M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][2][0];
		Deaths30M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][3][0];
		Deaths35M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][4][0];
		Deaths40M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][5][0];
		Deaths45M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][6][0];
		Deaths50M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][7][0];
		Deaths55M.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][8][0];
		Deaths20F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][1][1];
		Deaths25F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][2][1];
		Deaths30F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][3][1];
		Deaths35F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][4][1];
		Deaths40F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][5][1];
		Deaths45F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][6][1];
		Deaths50F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][7][1];
		Deaths55F.out[CurrSim - 1][iy] = AdultMortBy5yr[iy][8][1];
		NonAIDSdeaths20M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][1][0];
		NonAIDSdeaths25M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][2][0];
		NonAIDSdeaths30M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][3][0];
		NonAIDSdeaths35M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][4][0];
		NonAIDSdeaths40M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][5][0];
		NonAIDSdeaths45M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][6][0];
		NonAIDSdeaths50M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][7][0];
		NonAIDSdeaths55M.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][8][0];
		NonAIDSdeaths20F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][1][1];
		NonAIDSdeaths25F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][2][1];
		NonAIDSdeaths30F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][3][1];
		NonAIDSdeaths35F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][4][1];
		NonAIDSdeaths40F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][5][1];
		NonAIDSdeaths45F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][6][1];
		NonAIDSdeaths50F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][7][1];
		NonAIDSdeaths55F.out[CurrSim - 1][iy] = NonAIDSmortBy5yr[iy][8][1];
		AIDSdeathsPaed.out[CurrSim - 1][iy] = 0.0;
		for (ii = 0; ii <= 10; ii++){
			AIDSdeathsPaed.out[CurrSim - 1][iy] += AIDSdeathsByAgeP[ii][0] +
				AIDSdeathsByAgeP[ii][1];
			if (ii == 0){ AIDSdeaths0.out[CurrSim - 1][iy] = AIDSdeathsPaed.out[CurrSim - 1][iy]; }
			if (ii == 4){ AIDSdeaths1to4.out[CurrSim - 1][iy] = AIDSdeathsPaed.out[CurrSim - 1][iy] -
				AIDSdeaths0.out[CurrSim - 1][iy]; }
			if (ii == 9){
				AIDSdeaths5to9.out[CurrSim - 1][iy] = AIDSdeathsPaed.out[CurrSim - 1][iy] -
					AIDSdeaths0.out[CurrSim - 1][iy] - AIDSdeaths1to4.out[CurrSim - 1][iy];
			}
		}
		for (ii = 10; ii < 15; ii++){
			AIDSdeathsPaed.out[CurrSim - 1][iy] += AIDSdeathsByAge[ii - 10][0] +
				AIDSdeathsByAge[ii - 10][1];
			if (ii == 14){
				AIDSdeaths10to14.out[CurrSim - 1][iy] = AIDSdeathsPaed.out[CurrSim - 1][iy] -
					AIDSdeaths0.out[CurrSim - 1][iy] - AIDSdeaths1to4.out[CurrSim - 1][iy] -
					AIDSdeaths5to9.out[CurrSim - 1][iy];
			}
		}
		AIDSdeaths20to59M.out[CurrSim - 1][iy] = AIDSdeathsByAge[10][0];
		for (ii = 1; ii < 40; ii++){
			AIDSdeaths20to59M.out[CurrSim - 1][iy] += AIDSdeathsByAge[10 + ii][0];
		}
		AIDSdeaths20to59F.out[CurrSim - 1][iy] = AIDSdeathsByAge[10][1];
		for (ii = 1; ii < 40; ii++){
			AIDSdeaths20to59F.out[CurrSim - 1][iy] += AIDSdeathsByAge[10 + ii][1];
		}
		AIDSdeathsAdultM.out[CurrSim - 1][iy] = AIDSdeaths20to59M.out[CurrSim - 1][iy];
		AIDSdeathsAdultF.out[CurrSim - 1][iy] = AIDSdeaths20to59F.out[CurrSim - 1][iy];
		for (ii = 0; ii < 5; ii++){
			AIDSdeathsAdultM.out[CurrSim - 1][iy] += AIDSdeathsByAge[5 + ii][0];
			AIDSdeathsAdultF.out[CurrSim - 1][iy] += AIDSdeathsByAge[5 + ii][1];
		}
		for (ii = 45; ii < 76; ii++){
			AIDSdeathsAdultM.out[CurrSim - 1][iy] += AIDSdeathsByAge[5 + ii][0];
			AIDSdeathsAdultF.out[CurrSim - 1][iy] += AIDSdeathsByAge[5 + ii][1];
		}
		AIDSdeathsTot.out[CurrSim - 1][iy] = AIDSdeathsPaed.out[CurrSim - 1][iy] +
			AIDSdeathsAdultM.out[CurrSim - 1][iy] + AIDSdeathsAdultF.out[CurrSim - 1][iy];
		Temp[0] = 0.0;
		Temp[1] = 0.0;
		for (ii = 5; ii < 15; ii++){
			Temp[0] += AIDSdeathsByAge[ii][0];
			Temp[1] += AIDSdeathsByAge[ii][1];
		}
		AIDSdeaths15to24M.out[CurrSim - 1][iy] = Temp[0];
		AIDSdeaths15to24F.out[CurrSim - 1][iy] = Temp[1];
		AIDSdeaths15to24.out[CurrSim - 1][iy] = Temp[0] + Temp[1];
		Temp[0] = 0.0;
		Temp[1] = 0.0;
		for (ii = 15; ii < 40; ii++){
			Temp[0] += AIDSdeathsByAge[ii][0];
			Temp[1] += AIDSdeathsByAge[ii][1];
		}
		AIDSdeaths25to49M.out[CurrSim - 1][iy] = Temp[0];
		AIDSdeaths25to49F.out[CurrSim - 1][iy] = Temp[1];
		AIDSdeaths25to49.out[CurrSim - 1][iy] = Temp[0] + Temp[1];
		AIDSdeaths15to49M.out[CurrSim - 1][iy] = Temp[0] + AIDSdeaths15to24M.out[CurrSim - 1][iy];
		AIDSdeaths15to49F.out[CurrSim - 1][iy] = Temp[1] + AIDSdeaths15to24F.out[CurrSim - 1][iy];
		AIDSdeaths15to49.out[CurrSim - 1][iy] = Temp[0] + Temp[1] + AIDSdeaths15to24.out[CurrSim - 1][iy];
		Temp[0] = 0.0;
		Temp[1] = 0.0;
		for (ii = 40; ii < 81; ii++){
			Temp[0] += AIDSdeathsByAge[ii][0];
			Temp[1] += AIDSdeathsByAge[ii][1];
		}
		AIDSdeaths50plusM.out[CurrSim - 1][iy] = Temp[0];
		AIDSdeaths50plusF.out[CurrSim - 1][iy] = Temp[1];
		AIDSdeaths50plus.out[CurrSim - 1][iy] = Temp[0] + Temp[1];
		CrudeAIDSmort.out[CurrSim - 1][iy] = 100000 * AIDSdeathsTot.out[CurrSim - 1][iy] /
			TotPop.out[CurrSim - 1][iy];
		if (CurrYear == 2005){
			for (ia = 2; ia < 10; ia++){
				for (ig = 0; ig < 2; ig++){
					Temp[ig] = 0.0;
					for (ii = 0; ii < 5; ii++){
						Temp[ig] += NonAIDSdeathsA[ia * 5 + ii][ig];
					}
				}
				NonAIDSdeaths2005.out[CurrSim - 1][ia - 2] = Temp[0];
				NonAIDSdeaths2005.out[CurrSim - 1][ia + 6] = Temp[1];
			}
		}
		// Additional mortality outputs for Investment Case
		NonAIDSdeaths.out[CurrSim - 1][iy] = 0.0;
		for (ii = 0; ii <= 10; ii++){
			NonAIDSdeaths.out[CurrSim - 1][iy] += NonAIDSdeathsP[ii][0] +
				NonAIDSdeathsP[ii][1];
		}
		for (ii = 0; ii < 81; ii++){
			NonAIDSdeaths.out[CurrSim - 1][iy] += NonAIDSdeathsA[ii][0] +
				NonAIDSdeathsA[ii][1];
		}
		LYlostAIDS.out[CurrSim - 1][iy] = 0.0;
		for (ii = 0; ii <= 10; ii++){
			LYlostAIDS.out[CurrSim - 1][iy] += AIDSdeathsByAgeP[ii][0] * WestLifeExpectP[ii][0] +
				AIDSdeathsByAgeP[ii][1] * WestLifeExpectP[ii][1];
		}
		for (ii = 0; ii < 81; ii++){
			LYlostAIDS.out[CurrSim - 1][iy] += AIDSdeathsByAge[ii][0] * WestLifeExpectA[ii][0] +
				AIDSdeathsByAge[ii][1] * WestLifeExpectA[ii][1];
		}

		// Mortality probabilities by exact age
		for (ia = 0; ia < 10; ia++){
			for (ig = 0; ig < 2; ig++){
				MortProbExact[ia][ig] = 1.0 - exp(-TotDeathsByAgeP[ia][ig] / (0.5 *
					(TotalPop_S[ia][ig] + TotalPop[ia][ig])));
			}
		}
		for (ig = 0; ig < 2; ig++){
			MortProbExact[10][ig] = 1.0 - exp(-(TotDeathsByAgeP[10][ig] + 0.5 *
				TotDeathsByAge[0][ig]) / (0.5 * (TotalPop_S[10][ig] + TotalPop[10][ig])));
		}
		for (ia = 11; ia < 90; ia++){
			for (ig = 0; ig < 2; ig++){
				MortProbExact[ia][ig] = 0.5 * ((TotDeathsByAge[ia - 11][ig] /
					TotalPop_S[ia - 1][ig]) + (TotDeathsByAge[ia - 10][ig] / TotalPop_S[ia][ig]));
			}
		}
		for (ig = 0; ig < 2; ig++){
			MortProbExact[90][ig] = 1.0 - exp(-(TotDeathsByAge[80][ig] + 0.5 *
				TotDeathsByAge[79][ig]) / (0.5 * (TotalPop_S[90][ig] + TotalPop[89][ig] +
				TotalPop[90][ig])));
		}
		for (ia = 0; ia < 91; ia++){
			MaleMortAS.out[ia][iy] += MortProbExact[ia][0];
			FemMortAS.out[ia][iy] += MortProbExact[ia][1];
		}

		// Life tables
		for (ig = 0; ig < 2; ig++){
			LifeTable[0][ig] = 1.0;
			for (ia = 1; ia < 91; ia++){
				LifeTable[ia][ig] = LifeTable[ia - 1][ig] * (1.0 - MortProbExact[ia - 1][ig]);
			}
		}

		// Mortality indicators
		IMR.out[CurrSim - 1][iy] = 1000.0 * (SexRatio * MortProbExact[0][0] +
			(1.0 - SexRatio) * MortProbExact[0][1]);
		U5MR.out[CurrSim - 1][iy] = 1000.0 * (1.0 - SexRatio * LifeTable[5][0] -
			(1.0 - SexRatio) * LifeTable[5][1]);
		M45q15.out[CurrSim - 1][iy] = 1.0 - (LifeTable[60][0] / LifeTable[15][0]);
		F45q15.out[CurrSim - 1][iy] = 1.0 - (LifeTable[60][1] / LifeTable[15][1]);
		Tot45q15.out[CurrSim - 1][iy] = 0.5 * (M45q15.out[CurrSim - 1][iy] + F45q15.out[CurrSim - 1][iy]);
		for (ig = 0; ig < 2; ig++){
			Temp[ig] = 0.3 + 1.2 * LifeTable[1][ig];
			for (ia = 2; ia < 90; ia++){
				Temp[ig] += LifeTable[ia][ig];
			}
			Temp[ig] += 0.5 * LifeTable[90][ig];
			Temp[ig] += LifeTable[90][ig] / (-log(1.0 - MortProbExact[90][ig]));
		}
		LifeExpectM.out[CurrSim - 1][iy] = Temp[0];
		LifeExpectF.out[CurrSim - 1][iy] = Temp[1];
		LifeExpectTot.out[CurrSim - 1][iy] = Temp[0] * SexRatio + Temp[1] * (1.0 - SexRatio);

		// ART/disease stage outputs
		StartingART15to24M.out[CurrSim - 1][iy] = AdultsNewARTbyAge[0][0];
		StartingART25to34M.out[CurrSim - 1][iy] = AdultsNewARTbyAge[1][0];
		StartingART35to44M.out[CurrSim - 1][iy] = AdultsNewARTbyAge[2][0];
		StartingART45M.out[CurrSim - 1][iy] = AdultsNewARTbyAge[3][0];
		StartingART15to24F.out[CurrSim - 1][iy] = AdultsNewARTbyAge[0][1];
		StartingART25to34F.out[CurrSim - 1][iy] = AdultsNewARTbyAge[1][1];
		StartingART35to44F.out[CurrSim - 1][iy] = AdultsNewARTbyAge[2][1];
		StartingART45F.out[CurrSim - 1][iy] = AdultsNewARTbyAge[3][1];
		StartingARTtot.out[CurrSim - 1][iy] = StartingART0to14.out[CurrSim - 1][iy];
		StartingART_M15.out[CurrSim - 1][iy] = 0.0;
		StartingART_F15.out[CurrSim - 1][iy] = 0.0;
		for (ia = 0; ia < 4; ia++){
			StartingART_M15.out[CurrSim - 1][iy] += AdultsNewARTbyAge[ia][0];
			StartingART_F15.out[CurrSim - 1][iy] += AdultsNewARTbyAge[ia][1];
			for (ig = 0; ig < 2; ig++){
				StartingARTtot.out[CurrSim - 1][iy] += AdultsNewARTbyAge[ia][ig];
			}
		}
		TotNewNeed15M.out[CurrSim - 1][iy] = NewElig350[0];
		TotNewNeed15F.out[CurrSim - 1][iy] = NewElig350[1];

		// Additional ART outputs for Investment Case
		StartingART0.out[CurrSim - 1][iy] = PaedNewARTbyAge[0];
		StartingART1.out[CurrSim - 1][iy] = PaedNewARTbyAge[1];
		StartingART2to4.out[CurrSim - 1][iy] = PaedNewARTbyAge[2] + PaedNewARTbyAge[3] + PaedNewARTbyAge[4];
		StartingART1to2.out[CurrSim - 1][iy] = PaedNewARTbyAge[1] + PaedNewARTbyAge[2];
		StartingART3to5.out[CurrSim - 1][iy] = PaedNewARTbyAge[3] + PaedNewARTbyAge[4] + PaedNewARTbyAge[5];
		StartingART6to13.out[CurrSim - 1][iy] = 0.0;
		for (ia = 6; ia < 14; ia++){
			StartingART6to13.out[CurrSim - 1][iy] += PaedNewARTbyAge[ia];
		}
		StartingART6to9.out[CurrSim - 1][iy] = 0.0;
		for (ia = 6; ia < 10; ia++){
			StartingART6to9.out[CurrSim - 1][iy] += PaedNewARTbyAge[ia];
		}
		StartingART10to14.out[CurrSim - 1][iy] = 0.0;
		for (ia = 10; ia < 15; ia++){
			StartingART10to14.out[CurrSim - 1][iy] += PaedNewARTbyAge[ia];
		}
		TotNewNeed500M.out[CurrSim - 1][iy] = NewElig500[0];
		TotNewNeed500F.out[CurrSim - 1][iy] = NewElig500[1];
	}

	if (FixedUncertainty == 1 || CalibHCTprevP == 1 || CalibHCTtotP == 1){
		// HIV testing outputs
		TotalHIVtests.out[CurrSim-1][iy] = NewlyTestedNeg[0] + NewlyTestedNeg[1] +
			NewlyTestedPos[0] + NewlyTestedPos[1];
		if (CurrYear > 1990){
			HIVtestsPos.out[CurrSim - 1][iy] = (NewlyTestedPos[0] + NewlyTestedPos[1] + (NewlyTestedNeg[0] +
				NewlyTestedNeg[1]) * (1.0 - RapidDiagSp)) / TotalHIVtests.out[CurrSim - 1][iy];
		}
		FirstHIVtestsPos.out[CurrSim - 1][iy] = (NewlyTested1stPos[0] + NewlyTested1stPos[1]) /
			(NewlyTestedPos[0] + NewlyTestedPos[1]);
		AdultHIVtestsPos.out[CurrSim - 1][iy] = (NewlyTestedPos[0] + NewlyTestedPos[1]);
		AdultHIVtestsNeg.out[CurrSim - 1][iy] = (NewlyTestedNeg[0] + NewlyTestedNeg[1]);
        Number1stHIVtestsPos.out[CurrSim - 1][iy] = (NewlyTested1stPos[0] + NewlyTested1stPos[1]);
        Prop1stHIVtestsPos.out[CurrSim - 1][iy] = (NewlyTested1stPos[0] + NewlyTested1stPos[1]) / (NewlyTestedNeg[0] + NewlyTestedNeg[1] +
        NewlyTestedPos[0] + NewlyTestedPos[1]);
		TotalHIVtestsU15.out[CurrSim - 1][iy] = NewlyTestedNeg[2] + NewlyTestedPos[2];
		TotalHIVtests15to24M.out[CurrSim - 1][iy] = NewlyTestedAdult[0][0];
		TotalHIVtests15to24F.out[CurrSim - 1][iy] = NewlyTestedAdult[0][1];
		TotalHIVtests25to49M.out[CurrSim - 1][iy] = NewlyTestedAdult[1][0];
		TotalHIVtests25to49F.out[CurrSim - 1][iy] = NewlyTestedAdult[1][1];
		TotalHIVtests50plusM.out[CurrSim - 1][iy] = NewlyTestedAdult[2][0];
		TotalHIVtests50plusF.out[CurrSim - 1][iy] = NewlyTestedAdult[2][1];
		if (CurrYear > 1990){
			HIVtestsPosU15.out[CurrSim - 1][iy] = (NewlyTestedPos[2] + NewlyTestedNeg[2] * (1.0 - RapidDiagSp)) /
				TotalHIVtestsU15.out[CurrSim - 1][iy];
		}
		HIVtestsPos18mo.out[CurrSim - 1][iy] = (NewlyTestedPaed[0][1] + NewlyTestedPaed[0][0] * (1.0 - RapidDiagSp)) /
			(NewlyTestedPaed[0][1] + NewlyTestedPaed[0][0]);
		HIVtestsPos19to59mo.out[CurrSim - 1][iy] = (NewlyTestedPaed[1][1] + NewlyTestedPaed[1][0] * (1.0 - RapidDiagSp)) /
			(NewlyTestedPaed[1][1] + NewlyTestedPaed[1][0]);
		HIVtestsPos5to14.out[CurrSim - 1][iy] = (NewlyTestedPaed[2][1] + NewlyTestedPaed[2][0] * (1.0 - RapidDiagSp)) /
			(NewlyTestedPaed[2][1] + NewlyTestedPaed[2][0]);
		FalseNegPropn.out[CurrSim - 1][iy] = (NewlyTestedFalseNeg[0] + NewlyTestedFalseNeg[1]) /
			(NewlyTestedPos[0] + NewlyTestedPos[1] + NewlyTestedFalseNeg[0] + NewlyTestedFalseNeg[1]);
	}

	// Self-testing outputs
	if (FixedUncertainty == 1){
		TotSTestFixedPoint.out[CurrSim - 1][iy] = NewlyTestedNegST[0] + NewlyTestedPosST[0];
		TotSTestTaxi.out[CurrSim - 1][iy] = NewlyTestedNegST[1] + NewlyTestedPosST[1];
		TotSTestANC.out[CurrSim - 1][iy] = NewlyTestedNegST[2] + NewlyTestedPosST[2];
		TotSTestIndex.out[CurrSim - 1][iy] = NewlyTestedNegST[3] + NewlyTestedPosST[3];
		TotSTestWork1.out[CurrSim - 1][iy] = NewlyTestedNegST[4] + NewlyTestedPosST[4];
		TotSTestWork2.out[CurrSim - 1][iy] = NewlyTestedNegST[5] + NewlyTestedPosST[5];
		PosSTestFixedPoint.out[CurrSim - 1][iy] = NewlyTestedPosST[0];
		PosSTestTaxi.out[CurrSim - 1][iy] = NewlyTestedPosST[1];
		PosSTestANC.out[CurrSim - 1][iy] = NewlyTestedPosST[2];
		PosSTestIndex.out[CurrSim - 1][iy] = NewlyTestedPosST[3];
		PosSTestWork1.out[CurrSim - 1][iy] = NewlyTestedPosST[4];
		PosSTestWork2.out[CurrSim - 1][iy] = NewlyTestedPosST[5];
		STtoARTfixedPoint.out[CurrSim - 1][iy] = NewSTtoART[0];
		STtoARTtaxi.out[CurrSim - 1][iy] = NewSTtoART[1];
		STtoART_ANC.out[CurrSim - 1][iy] = NewSTtoART[2];
		STtoARTindex.out[CurrSim - 1][iy] = NewSTtoART[3];
		STtoARTwork1.out[CurrSim - 1][iy] = NewSTtoART[4];
		STtoARTwork2.out[CurrSim - 1][iy] = NewSTtoART[5];
	}

	if (FixedUncertainty == 1 || CalibARTbyAgeP == 1){
		// Paediatric ART initiation
		StartingART0.out[CurrSim - 1][iy] = PaedNewARTbyAge[0];
		StartingART1.out[CurrSim - 1][iy] = PaedNewARTbyAge[1];
		StartingART2to4.out[CurrSim - 1][iy] = PaedNewARTbyAge[2] + PaedNewARTbyAge[3] + PaedNewARTbyAge[4];
		StartingART5to14.out[CurrSim - 1][iy] = 0.0;
		for (ia = 5; ia < 15; ia++){
			StartingART5to14.out[CurrSim - 1][iy] += PaedNewARTbyAge[ia];
		}
	}

	// Cumulative paed ART initiation
	for (ia = 10; ia > 0; ia--){
		CumPaedARTearly[ia][4] = CumPaedARTearly[ia - 1][4] + CumPaedARTearly[ia - 1][3];
		CumPaedARTlate[ia][4] = CumPaedARTlate[ia - 1][4] + CumPaedARTlate[ia - 1][3];
		for (ii = 3; ii > 0; ii--){
			CumPaedARTearly[ia][ii] = CumPaedARTearly[ia - 1][ii - 1];
			CumPaedARTlate[ia][ii] = CumPaedARTlate[ia - 1][ii - 1];
		}
		// In the next few lines, average across ages ia and ia-1 because the PaedNewART arrays
		// are defined by age at ART start, and age at end of year will be on average 6 mo older
		CumPaedARTearly[ia][0] = 0.5 *(PaedNewARTbyAge[ia - 1] - PaedNewARTlate[ia - 1] +
			PaedNewARTbyAge[ia] - PaedNewARTlate[ia]);
		CumPaedARTlate[ia][0] = 0.5 * (PaedNewARTlate[ia - 1] + PaedNewARTlate[ia]);
	}
	CumPaedARTearly[0][0] = 0.5 *(PaedNewARTbyAge[0] - PaedNewARTlate[0]);
	CumPaedARTlate[0][0] = 0.5 * PaedNewARTlate[0];
}

void ResultsAtEndOfYr2()
{
	int ia, iy, ii;
	double Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, IncAdj[2];

	iy = CurrYear - StartYear;

	// AIDS deaths on ART
	AIDSdeathsART.out[CurrSim - 1][iy] = AIDSdeathsOnART;

	// TFR and total births
	Temp1 = 0.0;
	for (ia = 0; ia < 36; ia++){
		Temp1 += TotBirthsByMatAge[ia] / (0.5 *(TotalPop[ia + 15][1] + TotalPop_S[ia + 14][1]));}
	TotFertRate.out[CurrSim - 1][iy] = Temp1;
	TotBirths.out[CurrSim - 1][iy] = BirthsPosMothers + BirthsNegMothers;
	BirthRate.out[CurrSim - 1][iy] = 1000.0 * (BirthsPosMothers + BirthsNegMothers)/(TotPop.out[CurrSim - 1][iy] +
		0.5 * (BirthsPosMothers + BirthsNegMothers - AIDSdeathsTot.out[CurrSim - 1][iy] -
		NonAIDSdeaths.out[CurrSim - 1][iy]));
	NeonatalMMCops.out[CurrSim - 1][iy] = TotBirths.out[CurrSim - 1][iy] * SexRatio * NeonatalMMC[iy];

	// HIV incidence in children
	NewMTCT.out[CurrSim - 1][iy] = NewPerinatal + NewPostnatal;
	NewHIVatBirth.out[CurrSim-1][iy] = NewPerinatal;
	NewHIVafterBirth.out[CurrSim-1][iy] = NewPostnatal;
	NewHIVto18mo.out[CurrSim - 1][iy] = NewPostnatal18;
	NewHIVmothersBF.out[CurrSim - 1][iy] = NewHIVlactating;
	TotBirthsHIV.out[CurrSim-1][iy] = BirthsPosMothers;
	TotBirthsART.out[CurrSim-1][iy] = BirthsARTmothers;
	TotBirthDiagnosed.out[CurrSim-1][iy] = BirthsDiagMothers;
	MTCTrateAtBirth.out[CurrSim-1][iy] = NewPerinatal/BirthsPosMothers;
	MTCTrateBirthDiag.out[CurrSim-1][iy] = VertTransmKnownPos.out[CurrSim-1][iy] / BirthsDiagMothers;
	MTCTrate18moDiag.out[CurrSim-1][iy] = NewPostnatal18 / BirthsDiagMothers;
	TotMTCTrate.out[CurrSim-1][iy] = (NewPerinatal + NewPostnatal) / BirthsPosMothers;
	TotMTCTallBirths.out[CurrSim-1][iy] = (NewPerinatal + NewPostnatal) / TotBirths.out[CurrSim-1][iy];
	Temp1 = 0.0;
	Temp2 = 0.0;
	for (ia = 0; ia < 5; ia++){ Temp1 += NewHIVbyAgeSex[ia][0] + NewHIVbyAgeSex[ia][1]; }
	for (ia = 0; ia < 15; ia++){
		Temp2 += TotalPop_S[ia][0] + TotalPop_S[ia][1] - TotalPositive_S[ia][0] - TotalPositive_S[ia][1];}
	HIVinc0to14.out[CurrSim-1][iy] = (NewPerinatal + NewPostnatal + Temp1) / Temp2;
	NewHIVU15.out[CurrSim-1][iy] = NewPerinatal + NewPostnatal + Temp1;

	// Age-specific incidence output table
	for (ia = 0; ia < 81; ia++){
		MaleIncAS.out[ia][iy] += NewHIVbyAgeSex[ia][0] / (TotalPop_S[ia + 10][0] - TotalPositive_S[ia + 10][0]);
		FemIncAS.out[ia][iy] += NewHIVbyAgeSex[ia][1] / (TotalPop_S[ia + 10][1] - TotalPositive_S[ia + 10][1]);
	}

	// HIV incidence in 15 to 49 age group
	Temp1 = 0.0;
	Temp2 = 0.0;
	IncAdj[0] = 0.0;
	for(ia=5; ia<15; ia++){
		Temp1 += NewHIVbyAgeSex[ia][0];
		IncAdj[0] += NewHIVbyAgeSex[ia][0] * ((1.0 - exp(-HCTtoART_SE[ia][6][0] * 12.0)) /
			(HCTtoART_SE[ia][6][0] * 12.0) - exp(-HCTtoART_SE[ia][6][0] * 12.0));
		Temp2 += TotalPop_S[ia+10][0] - TotalPositive_S[ia+10][0];
	}
	HIVinc15to24M.out[CurrSim-1][iy] = Temp1/Temp2;
	NewHIV15to24M.out[CurrSim-1][iy] = Temp1;
	Temp5 = Temp1;
	Temp6 = Temp2;
	for(ia=15; ia<40; ia++){
		Temp1 += NewHIVbyAgeSex[ia][0];
		IncAdj[0] += NewHIVbyAgeSex[ia][0] * ((1.0 - exp(-HCTtoART_SE[ia][6][0] * 12.0)) /
			(HCTtoART_SE[ia][6][0] * 12.0) - exp(-HCTtoART_SE[ia][6][0] * 12.0));
		Temp2 += TotalPop_S[ia+10][0] - TotalPositive_S[ia+10][0];
	}
	HIVinc15to49M.out[CurrSim-1][iy] = Temp1/Temp2;
	NewHIV15to49M.out[CurrSim-1][iy] = Temp1;
	HIVinc25to49M.out[CurrSim-1][iy] = (Temp1 - Temp5)/(Temp2 - Temp6);
	NewHIV25to49M.out[CurrSim-1][iy] = Temp1 - Temp5;
	Temp3 = 0.0;
	Temp4 = 0.0;
	IncAdj[1] = 0.0;
	for(ia=5; ia<15; ia++){
		Temp3 += NewHIVbyAgeSex[ia][1];
		IncAdj[1] += NewHIVbyAgeSex[ia][1] * ((1.0 - exp(-HCTtoART_SE[ia][6][1] * 12.0)) /
			(HCTtoART_SE[ia][6][1] * 12.0) - exp(-HCTtoART_SE[ia][6][1] * 12.0));
		Temp4 += TotalPop_S[ia+10][1] - TotalPositive_S[ia+10][1];
	}
	HIVinc15to24F.out[CurrSim-1][iy] = Temp3/Temp4;
	NewHIV15to24F.out[CurrSim-1][iy] = Temp3;
	HIVinc15to24.out[CurrSim-1][iy] = (Temp3 + Temp5) / (Temp4 + Temp6);
	NewHIV15to24.out[CurrSim-1][iy] = Temp3 + Temp5;
	Temp5 = Temp3;
	Temp6 = Temp4;
	for(ia=15; ia<40; ia++){
		Temp3 += NewHIVbyAgeSex[ia][1];
		IncAdj[1] += NewHIVbyAgeSex[ia][1] * ((1.0 - exp(-HCTtoART_SE[ia][6][1] * 12.0)) /
			(HCTtoART_SE[ia][6][1] * 12.0) - exp(-HCTtoART_SE[ia][6][1] * 12.0));
		Temp4 += TotalPop_S[ia+10][1] - TotalPositive_S[ia+10][1];
	}
	HIVinc15to49F.out[CurrSim-1][iy] = Temp3/Temp4;
	NewHIV15to49F.out[CurrSim-1][iy] = Temp3;
	HIVinc25to49F.out[CurrSim-1][iy] = (Temp3 - Temp5)/(Temp4 - Temp6);
	NewHIV25to49F.out[CurrSim-1][iy] = Temp3 - Temp5;
	NewHIV25to49.out[CurrSim-1][iy] = Temp3 - Temp5 + NewHIV25to49M.out[CurrSim-1][iy];
	HIVinc25to49.out[CurrSim-1][iy] = NewHIV25to49.out[CurrSim-1][iy] /
		(Neg25to49M.out[CurrSim - 1][iy] + Neg25to49F.out[CurrSim-1][iy]);
	HIVinc15to49.out[CurrSim-1][iy] = (Temp1 + Temp3)/(Temp2 + Temp4);
	HIVinc15to49adj.out[CurrSim-1][iy] = (Temp1 + Temp3 - IncAdj[0] - IncAdj[1]) /
		(Temp2 + Temp4);
	NewHIV15to49.out[CurrSim - 1][iy] = Temp1 + Temp3;

	// Total new HIV and % of heterosexual transmission attributable to commercial sex
	for(ia=0; ia<5; ia++){
		Temp1 += NewHIVbyAgeSex[ia][0];
		Temp3 += NewHIVbyAgeSex[ia][1];
	}
	for(ia=40; ia<81; ia++){
		Temp1 += NewHIVbyAgeSex[ia][0];
		Temp3 += NewHIVbyAgeSex[ia][1];
	}
	TotalNewHIV.out[CurrSim-1][iy] = Temp1 + Temp3 + NewPerinatal + NewPostnatal;
	NewAdultHIV.out[CurrSim-1][iy] = Temp1 + Temp3;
	NewHIV_M.out[CurrSim-1][iy] = Temp1;
	NewHIV_F.out[CurrSim-1][iy] = Temp3;
	TotNegPop.out[CurrSim-1][iy] = TotPop.out[CurrSim-1][iy] - TotalHIV.out[CurrSim-1][iy];
	TotIncidence.out[CurrSim-1][iy] = TotalNewHIV.out[CurrSim-1][iy] / TotNegPop.out[CurrSim-1][iy];
	NewHIVclients.out[CurrSim-1][iy] = NewHIVinClients;
	PAFforCSW.out[CurrSim-1][iy] = (NewHIVinFSW.out[CurrSim-1][iy] + NewHIVinClients)/
		(Temp1 + Temp3);

	// HIV incidence in 50+ age group (and 15+)
	Temp1 = 0.0;
	Temp2 = 0.0;
	for (ia = 40; ia<81; ia++){
		Temp1 += NewHIVbyAgeSex[ia][0];
		Temp2 += TotalPop_S[ia + 10][0] - TotalPositive_S[ia + 10][0];
	}
	HIVinc50M.out[CurrSim-1][iy] = Temp1 / Temp2;
	NewHIV50M.out[CurrSim-1][iy] = Temp1;
	NewHIV15M.out[CurrSim-1][iy] = Temp1 + NewHIV15to49M.out[CurrSim-1][iy];
	HIVinc15plusM.out[CurrSim-1][iy] = NewHIV15M.out[CurrSim-1][iy] /
		(MalesOver15.out[CurrSim-1][iy] * (1.0 - Prev15plusM.out[CurrSim - 1][iy]));
	Temp3 = 0.0;
	Temp4 = 0.0;
	for (ia = 40; ia<81; ia++){
		Temp3 += NewHIVbyAgeSex[ia][1];
		Temp4 += TotalPop_S[ia + 10][1] - TotalPositive_S[ia + 10][1];
	}
	HIVinc50F.out[CurrSim - 1][iy] = Temp3 / Temp4;
	NewHIV50F.out[CurrSim - 1][iy] = Temp3;
	NewHIV15F.out[CurrSim - 1][iy] = Temp3 + NewHIV15to49F.out[CurrSim - 1][iy];
	HIVinc15plusF.out[CurrSim - 1][iy] = NewHIV15F.out[CurrSim - 1][iy] /
		(FemalesOver15.out[CurrSim - 1][iy] * (1.0 - Prev15plusF.out[CurrSim - 1][iy]));
	HIVinc15plus.out[CurrSim - 1][iy] = (NewHIV15M.out[CurrSim - 1][iy] + NewHIV15F.out[CurrSim - 1][iy]) /
		(Neg15to49.out[CurrSim - 1][iy] + Neg50.out[CurrSim - 1][iy]);
	HIVinc50.out[CurrSim - 1][iy] = (Temp1 + Temp3) / (Temp2 + Temp4);
	NewHIV50.out[CurrSim - 1][iy] = Temp1 + Temp3;

	// HIV incidence in key populations
	HIVincFSW.out[CurrSim - 1][iy] = NewHIVinFSW.out[CurrSim - 1][iy] / NegFSW.out[CurrSim - 1][iy];
	NewHIVinMSM.out[CurrSim - 1][iy] = HIVincMSM.out[CurrSim - 1][iy];
	HIVincMSM.out[CurrSim - 1][iy] = HIVincMSM.out[CurrSim - 1][iy] / NegMSM.out[CurrSim - 1][iy];
	HIVincClients.out[CurrSim - 1][iy] = NewHIVclients.out[CurrSim - 1][iy] / NegClients.out[CurrSim - 1][iy];

	// Incidence:prevalence ratio
	IncPrevRatio.out[CurrSim - 1][iy] = TotalNewHIV.out[CurrSim - 1][iy] / TotalHIV.out[CurrSim - 1][iy];
	IncPrevRatioFtoM.out[CurrSim - 1][iy] = NewHIV15F.out[CurrSim - 1][iy] / TotHIV15M.out[CurrSim - 1][iy];
	IncPrevRatioMtoF.out[CurrSim - 1][iy] = NewHIV15M.out[CurrSim - 1][iy] / TotHIV15F.out[CurrSim - 1][iy];

	// HIV incidence by age in 2000 and 2010
	if(CurrYear==2000 || CurrYear==2010){
		for(ia=1; ia<10; ia++){
			Temp1 = 0.0;
			Temp2 = 0.0;
			Temp3 = 0.0;
			Temp4 = 0.0;
			for(ii=0; ii<5; ii++){
				Temp1 += NewHIVbyAgeSex[ia*5+ii][0];
				Temp3 += NewHIVbyAgeSex[ia*5+ii][1];
				Temp2 += TotalPop_S[ia*5+ii+10][0] - TotalPositive_S[ia*5+ii+10][0];
				Temp4 += TotalPop_S[ia*5+ii+10][1] - TotalPositive_S[ia*5+ii+10][1];
			}
			if(CurrYear==2000){
				HIVinc2000.out[CurrSim-1][ia-1] = Temp1/Temp2;
				HIVinc2000.out[CurrSim-1][9+ia-1] = Temp3/Temp4;
			}
			if(CurrYear==2010){
				HIVinc2010.out[CurrSim-1][ia-1] = Temp1/Temp2;
				HIVinc2010.out[CurrSim-1][9+ia-1] = Temp3/Temp4;
			}
		}
	}

	// ART enrolment ratios
	// I've simplified by including the new infections in 10-14 years olds in the adult
	// denominator, which is not strictly correct.
	EnrolmentRatio.out[CurrSim - 1][iy] = StartingARTtot.out[CurrSim - 1][iy] /
		TotalNewHIV.out[CurrSim - 1][iy];
	EnrolmentRatio15M.out[CurrSim - 1][iy] = StartingART_M15.out[CurrSim - 1][iy] /
		NewHIV_M.out[CurrSim - 1][iy];
	EnrolmentRatio15F.out[CurrSim - 1][iy] = StartingART_F15.out[CurrSim - 1][iy] /
		NewHIV_F.out[CurrSim - 1][iy];
	EnrolmentRatioU15.out[CurrSim - 1][iy] = StartingART0to14.out[CurrSim - 1][iy] /
		(NewPerinatal + NewPostnatal);

	// AIDS deaths by age
	MaleAIDSdeathsAS.out[0][iy] += AIDSdeathsByAgeP[0][0] + 0.5 * AIDSdeathsByAgeP[1][0] -
		AIDSdeathsYOB[0];
	FemAIDSdeathsAS.out[0][iy] += AIDSdeathsByAgeP[0][1] + 0.5 * AIDSdeathsByAgeP[1][1] -
		AIDSdeathsYOB[1];
	for (ia = 1; ia < 9; ia++){
		MaleAIDSdeathsAS.out[ia][iy] += 0.5 * (AIDSdeathsByAgeP[ia][0] + AIDSdeathsByAgeP[ia+1][0]);
		FemAIDSdeathsAS.out[ia][iy] += 0.5 * (AIDSdeathsByAgeP[ia][1] + AIDSdeathsByAgeP[ia + 1][1]);
	}
	MaleAIDSdeathsAS.out[9][iy] += 0.5 * AIDSdeathsByAgeP[9][0] + AIDSdeathsByAgeP[10][0];
	FemAIDSdeathsAS.out[9][iy] += 0.5 * AIDSdeathsByAgeP[9][1] + AIDSdeathsByAgeP[10][1];
	for (ia = 10; ia <= 90; ia++){
		MaleAIDSdeathsAS.out[ia][iy] += AIDSdeathsByAge[ia - 10][0];
		FemAIDSdeathsAS.out[ia][iy] += AIDSdeathsByAge[ia - 10][1];
	}
	MaleAIDSdeathsAS.out[91][iy] += AIDSdeathsYOB[0];
	FemAIDSdeathsAS.out[91][iy] += AIDSdeathsYOB[1];
}

void OneYear()
{
	int im;
	CurrYear +=1;
	if (UTTretention == 1 && CurrYear >= 2020) {
						for (int id = 0; id < 6; id++) {
									 for (int is = 0; is < 2; is++) {
													OnARTbyIntDur[id][is] = 1.0 - UTTretval * (1.0 - OnARTbyIntDur[id][is]);
													OnARThalfIntDur[id][is] = 1.0 - UTTretval * (1.0 - OnARThalfIntDur[id][is]);
									 }
						}
			}

	if(CurrYear==1985){
		GetCurrBehavDbnF();
		GetCurrBehavDbnM();
		UpdatePop();
	}
	ResetMonthlyCum();
	//ResultsAtStartOfYr();
	if (RRperCalYr != 1.00){ SetProgression(1); }
	UpdateNonAIDSmort();
	UpdateAIDSmort();
	SetCurrYearParameters();
	SetAnnPaedParameters();
	MaleChild.NewNonVertCurrY = 0.0;
	FemChild.NewNonVertCurrY = 0.0;
	// Temporarily changing placement of functions called monthly
	UpdateAgePrefsF();
	UpdatePartnerAcqM();
	UpdateMixingST();
	UpdateARTmort();
	SetCD4byARTdur();
	//if(CurrYear==StartYear){
	//	UpdateCondomUse();}
	UpdateCondomUse();
	//CalcOIsTested();
	CalcPosPartnerProb();
	CalcSelfTestingRates();
	UpdateTestingRates();
	if (UseNumbersTests==1 && (NumbersTested[CurrYear - 1985] > 0.0)){
		CalcHCT1stTime();
		if (NumbersTested5to14[CurrYear - 1985] > 0.0){ CalcRRtestVirgin(); }
		UpdateTestingRates();
	}
	GetTotalTesting();
	ResultsAtStartOfYr(); // Should come after updating rates of condom use
	for(im=0; im<12; im++){
		OneMonth(im);
		if(im==7){GetPrevPregnant();}
	}

	// Update calculation of annual RATES
	// (Note that we first have to update the calcs of totals, since these are our denominators.)
	GetCurrBehavDbnF();
	GetCurrBehavDbnM();
	UpdatePop();
	ResultsAtEndOfYr();
	UpdateAgePrefsF();
	UpdatePartnerAcqM();
	UpdateCircProb();
	UpdateMigration();
	GetMarriageAndDivorceRates();

	// Update male circumcision
	UpdateMC(&MHU_virgin, &MHC_virgin);
	UpdateMC(&MHU_ST, &MHC_ST);
	UpdateMC(&MHU_STM, &MHC_STM);
	UpdateMC(&MHU_LTH, &MHC_LTH);
	UpdateMC(&MHU_LTL, &MHC_LTL);
	UpdateMC(&MLU_virgin, &MLC_virgin);
	UpdateMC(&MLU_ST, &MLC_ST);
	UpdateMC(&MLU_STM, &MLC_STM);
	UpdateMC(&MLU_LTH, &MLC_LTH);
	UpdateMC(&MLU_LTL, &MLC_LTL);

	// Update marital status
	UpdateMarital(&MHU_ST, &MHU_LTH, &MHU_LTL);
	UpdateMarital(&MHU_STM, &MHU_LTH, &MHU_LTL);
	UpdateMarital(&MHC_ST, &MHC_LTH, &MHC_LTL);
	UpdateMarital(&MHC_STM, &MHC_LTH, &MHC_LTL);
	UpdateMarital(&MLU_ST, &MLU_LTH, &MLU_LTL);
	UpdateMarital(&MLU_STM, &MLU_LTH, &MLU_LTL);
	UpdateMarital(&MLC_ST, &MLC_LTH, &MLC_LTL);
	UpdateMarital(&MLC_STM, &MLC_LTH, &MLC_LTL);
	UpdateMarital(&FH_ST, &FH_LTH, &FH_LTL);
	UpdateMarital(&FL_ST, &FL_LTH, &FL_LTL);

	UpdateAllDemog();

	// Update sexual debut
	UpdateDebut(&MHU_virgin, &MHU_ST, &MHU_STM, 1);
	UpdateDebut(&MHC_virgin, &MHC_ST, &MHC_STM, 1);
	UpdateDebut(&MLU_virgin, &MLU_ST, &MLU_STM, 1);
	UpdateDebut(&MLC_virgin, &MLC_ST, &MLC_STM, 1);
	UpdateDebut(&FH_virgin, &FH_ST, &FH_ST, 1);
	UpdateDebut(&FL_virgin, &FL_ST, &FL_ST, 1);

	// Update totals
	UpdateStartTot();
	GetCurrBehavDbnF();
	GetCurrBehavDbnM();
	UpdatePop();

	if(FixedUncertainty==1){
		ResultsAtEndOfYr2();} // None of these outputs are required for calibration
}

/*void SaveHSRCcalib(const char* filout)
{
	int ia;
	std::ofstream file(filout);

	file<<std::right<<"2005 male prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[20][ia][0]<<std::endl;}
	file<<std::right<<"2005 female prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[20][ia][1]<<std::endl;}
	file<<std::right<<"2008 male prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[23][ia][0]<<std::endl;}
	file<<std::right<<"2008 female prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[23][ia][1]<<std::endl;}
	file<<std::right<<"2012 male prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[27][ia][0]<<std::endl;}
	file<<std::right<<"2012 female prevalence"<<std::endl;
	for(ia=0; ia<9; ia++){
		file<<std::right<<AdultHHprev[27][ia][1]<<std::endl;}
	file.close();
}*/

/*void SaveAdolProj(const char* filout)
{
	int iy, ii;
	std::ofstream file(filout);

	for(ii=0; ii<4; ii++){
		for(iy=0; iy<56; iy++){
			file<<std::right<<AdolHIVprofile[iy][ii]<<"	";}
		file<<std::endl;
	}
	file.close();
}

void SaveFSWprofile(const char* filout)
{
	int iy, ii;
	std::ofstream file(filout);

	for(ii=0; ii<15; ii++){
		for(iy=0; iy<56; iy++){
			file<<std::right<<FSW_HIVprofile[iy][ii]<<"	";}
		file<<std::endl;
	}
	file.close();
}

void SaveHCTbyAge(const char* filout)
{
	int ia, ii;
	std::ofstream file(filout);

	for(ii=0; ii<2; ii++){
		for(ia=0; ia<8; ia++){
			file<<std::right<<PrevTested[ia][ii]<<"	";}
		file<<std::endl;
	}
	file.close();
}*/

void GetSummaryOutputs(const char* filout)
{
	int i, c;
	std::ofstream file(filout);

	// Mortality outputs (28)
	IMR.GetMeans();
	U5MR.GetMeans();
	AIDSdeathsTot.GetMeans();
	NonAIDSdeaths.GetMeans();
	NonAIDSdeathsHIVpos.GetMeans();
	AIDSdeathsART.GetMeans();
	AIDSdeathsAdultM.GetMeans();
	AIDSdeathsAdultF.GetMeans();
	AIDSdeathsPaed.GetMeans();
	AIDSdeaths15to24.GetMeans();
	AIDSdeaths15to24M.GetMeans();
	AIDSdeaths15to24F.GetMeans();
	AIDSdeaths15to49.GetMeans();
	AIDSdeaths15to49M.GetMeans();
	AIDSdeaths15to49F.GetMeans();
	AIDSdeaths25to49.GetMeans();
	AIDSdeaths25to49M.GetMeans();
	AIDSdeaths25to49F.GetMeans();
	AIDSdeaths50plus.GetMeans();
	AIDSdeaths50plusM.GetMeans();
	AIDSdeaths50plusF.GetMeans();
	LifeExpectTot.GetMeans();
	LifeExpectM.GetMeans();
	LifeExpectF.GetMeans();
	Tot45q15.GetMeans();
	M45q15.GetMeans();
	F45q15.GetMeans();
	CrudeAIDSmort.GetMeans();

	// Other demographic indicators (21)
	SummOutRow += 3;
	TotPop.GetMeans();
	MalesOver15.GetMeans();
	FemalesOver15.GetMeans();
	TotalUnder15.GetMeans();
	Total15to24.GetMeans();
	Total15to24M.GetMeans();
	Total15to24F.GetMeans();
	Total15to49.GetMeans();
	Total15to49M.GetMeans();
	Total15to49F.GetMeans();
	Total25to49.GetMeans();
	Total25to49M.GetMeans();
	Total25to49F.GetMeans();
	Total50plus.GetMeans();
	Total50plusM.GetMeans();
	Total50plusF.GetMeans();
	TotBirths.GetMeans();
	TotFertRate.GetMeans();
	BirthRate.GetMeans();
	DependencyRatio.GetMeans();
	AgingIndex.GetMeans();

	// HIV incidence statistics (48)
	SummOutRow += 3;
	TotalNewHIV.GetMeans();
	NewAdultHIV.GetMeans();
	NewHIV_M.GetMeans();
	NewHIV_F.GetMeans();
	NewHIV15M.GetMeans();
	NewHIV15F.GetMeans();
	NewHIVU15.GetMeans();
	NewHIV15to24.GetMeans();
	NewHIV15to24M.GetMeans();
	NewHIV15to24F.GetMeans();
	NewHIV15to49.GetMeans();
	NewHIV15to49M.GetMeans();
	NewHIV15to49F.GetMeans();
	NewHIV25to49.GetMeans();
	NewHIV25to49M.GetMeans();
	NewHIV25to49F.GetMeans();
	NewHIV50.GetMeans();
	NewHIV50M.GetMeans();
	NewHIV50F.GetMeans();
	TotIncidence.GetMeans();
	HIVinc15plus.GetMeans();
	HIVinc15plusM.GetMeans();
	HIVinc15plusF.GetMeans();
	HIVinc0to14.GetMeans();
	HIVinc15to24.GetMeans();
	HIVinc15to24M.GetMeans();
	HIVinc15to24F.GetMeans();
	HIVinc15to49.GetMeans();
	HIVinc15to49M.GetMeans();
	HIVinc15to49F.GetMeans();
	HIVinc25to49.GetMeans();
	HIVinc25to49M.GetMeans();
	HIVinc25to49F.GetMeans();
	HIVinc50.GetMeans();
	HIVinc50M.GetMeans();
	HIVinc50F.GetMeans();
	HIVincFSW.GetMeans();
	HIVincMSM.GetMeans();
	IncPrevRatio.GetMeans(); // New in version 4.3
	IncPrevRatioFtoM.GetMeans(); // New in version 4.3
	IncPrevRatioMtoF.GetMeans(); // New in version 4.3
	NewMTCT.GetMeans();
	NewHIVatBirth.GetMeans();
	NewHIVafterBirth.GetMeans();
	TotBirthsHIV.GetMeans();
	TotBirthsART.GetMeans();
	TotMTCTrate.GetMeans();
	MTCTrateAtBirth.GetMeans();
	MTCTrateBirthDiag.GetMeans();
	MTCTrate18moDiag.GetMeans();

	// HIV prevalence statistics (42)
	SummOutRow += 3;
	TotalHIV.GetMeans();
	TotHIV15M.GetMeans();
	TotHIV15F.GetMeans();
	TotPaedHIV.GetMeans();
	TotHIV15to24.GetMeans();
	TotHIV15to24M.GetMeans();
	TotHIV15to24F.GetMeans();
	TotHIV15to49.GetMeans();
	TotHIV15to49M.GetMeans();
	TotHIV15to49F.GetMeans();
	TotHIV25to49.GetMeans();
	TotHIV25to49M.GetMeans();
	TotHIV25to49F.GetMeans();
	TotHIV50plus.GetMeans();
	TotHIV50plusM.GetMeans();
	TotHIV50plusF.GetMeans();
	HIVprevalence.GetMeans();
	Prev15plusM.GetMeans();
	Prev15plusF.GetMeans();
	Prev0to14.GetMeans();
	Prev15to24.GetMeans();
	Prev15to24M.GetMeans();
	Prev15to24F.GetMeans();
	Prev15to49.GetMeans();
	Prev15to49M.GetMeans();
	Prev15to49F.GetMeans();
	Prev25to49.GetMeans();
	Prev25to49M.GetMeans();
	Prev25to49F.GetMeans();
	Prev25plus.GetMeans();
	Prev50plus.GetMeans();
	Prev50plusM.GetMeans();
	Prev50plusF.GetMeans();
	PrevPreg15to49.GetMeans();
	PrevPreg15to19.GetMeans();
	PrevPreg20to24.GetMeans();
	PrevPreg25to29.GetMeans();
	PrevPreg30to34.GetMeans();
	PrevPreg35to39.GetMeans();
	PrevPreg40to49.GetMeans();
	PrevFSW.GetMeans();
	MSMprev18plus.GetMeans();

	// ART outputs (27)
	SummOutRow += 3;
	StartingARTtot.GetMeans();
	StartingART_M15.GetMeans();
	StartingART_F15.GetMeans();
	StartingART0to14.GetMeans();
	NewARTunder200.GetMeans();
	NewART200to349.GetMeans();
	NewART350to499.GetMeans();
	NewARTover500.GetMeans();
	TotalOnART.GetMeans();
	TotalART15M.GetMeans();
	TotalART15F.GetMeans();
	TotalARTunder15.GetMeans();
	ARTcoverage.GetMeans();
	ARTcoverage15M.GetMeans();
	ARTcoverage15F.GetMeans();
	ARTcoverageU15.GetMeans();
	ARTcoverageFSW.GetMeans(); // New to Thembisa 4.3
	ARTcoverageMSM.GetMeans(); // New to Thembisa 4.3
	EnrolmentRatio.GetMeans();
	EnrolmentRatio15M.GetMeans();
	EnrolmentRatio15F.GetMeans();
	EnrolmentRatioU15.GetMeans();
	VLsuppressed.GetMeans();
	VLsuppressedM.GetMeans();
	VLsuppressedF.GetMeans();
	VLsuppressedU15.GetMeans();
	VLsuppressed1000.GetMeans();
	VLsuppressed1000M.GetMeans();
	VLsuppressed1000F.GetMeans();
	VLsuppressed1000P.GetMeans();
	TotalART15M2L.GetMeans();
	TotalART15F2L.GetMeans();
	TotalARTunder15_2L.GetMeans();

	// HIV testing and other 90-90-90 indicators (23)
	SummOutRow += 3;
	TotalHIVtests.GetMeans();
	TotalHIVtestsU15.GetMeans();
	HIVtestsPos.GetMeans();
	HIVtestsPosU15.GetMeans();
	AdultsEverTested.GetMeans();
	AdultsEverTestedM.GetMeans();
	AdultsEverTestedF.GetMeans();
	DiagnosedPropn.GetMeans();
	DiagnosedPropnM.GetMeans();
	DiagnosedPropnF.GetMeans();
	DiagnosedPropnU15.GetMeans();
	DiagnosedHIV_FSW.GetMeans(); // New to Thembisa 4.3
	DiagnosedHIV_MSM.GetMeans(); // New to Thembisa 4.3
	ARTcoverageDiag.GetMeans();
	ARTcoverageDiagM.GetMeans();
	ARTcoverageDiagF.GetMeans();
	ARTcoverageDiagU15.GetMeans();
	VLsuppressedAllHIV.GetMeans();
	VLsuppressedAllM.GetMeans();
	VLsuppressedAllF.GetMeans();
	VLsuppressedAllU15.GetMeans();
	VLsuppressedAll1000.GetMeans();
	VLsuppressedAllM1000.GetMeans();
	VLsuppressedAllF1000.GetMeans();
	VLsuppressedAllP1000.GetMeans();

	// Other prevention indicators (13)
	SummOutRow += 3;
	CondomUse15to24F.GetMeans();
	CondomUse25to49F.GetMeans();
	Circumcised15to49.GetMeans();
	NeonatalMMCops.GetMeans();
	MMC10to14.GetMeans();
	MMC15to19.GetMeans();
	MMC20to24.GetMeans();
	MMC25to49.GetMeans();
	MMCover50.GetMeans();
	MenOnPrEP.GetMeans();
	WomenOnPrEP.GetMeans();
	FSWonPrEP.GetMeans();
	MSMonPrEP.GetMeans();
	AGYWonPrEP.GetMeans();
	NewPrEP_M.GetMeans();
	NewPrEP_F.GetMeans();
	PrEPcoverageFSW.GetMeans();
	PrEPcoverageMSM.GetMeans();
	PrEPcoverageAGYW.GetMeans();
	PrEPcoverageAllM.GetMeans();
	PrEPcoverageAllF.GetMeans();
	PrEPcoverageAll.GetMeans();

	for (i = 0; i<=SummOutRow; i++){
		for (c = 0; c<86; c++){
			if (SummaryOutputs[i][c]<0.0 || SummaryOutputs[i][c]>0.0){
				file << std::setw(10) << std::right << SummaryOutputs[i][c] << "	";
			}
			else{
				file << std::setw(10) << std::right << "	";
			}
		}
		file << std::endl;
	}
	file.close();
}

void GetAddedOutputs(const char* filout)
{
	int i, c;
	std::ofstream file(filout);

	// Clear SummaryOutputs
	for (i = 0; i <= SummOutRow; i++){
		for (c = 0; c < 86; c++){
			SummaryOutputs[i][c] = 0.0;
		}
	}

	SummOutRow = 0;

	// Additional calibration stats
	AdjPreg15to19.GetMeans();
	AdjPreg20to24.GetMeans();
	AdjPreg25to29.GetMeans();
	AdjPreg30to34.GetMeans();
	AdjPreg35to39.GetMeans();
	AdjPreg15to49.GetMeans();
	Prev2to14.GetMeans();
	StartingART0.GetMeans();
	StartingART1.GetMeans();
	StartingART2to4.GetMeans();
	StartingART5to14.GetMeans();
	Deaths0F.GetMeans();
	Deaths1F.GetMeans();
	Deaths5F.GetMeans();
	Deaths10F.GetMeans();
	Deaths20F.GetMeans();
	Deaths25F.GetMeans();
	Deaths30F.GetMeans();
	Deaths35F.GetMeans();
	Deaths40F.GetMeans();
	Deaths45F.GetMeans();
	Deaths50F.GetMeans();
	Deaths55F.GetMeans();
	Deaths0M.GetMeans();
	Deaths1M.GetMeans();
	Deaths5M.GetMeans();
	Deaths10M.GetMeans();
	Deaths20M.GetMeans();
	Deaths25M.GetMeans();
	Deaths30M.GetMeans();
	Deaths35M.GetMeans();
	Deaths40M.GetMeans();
	Deaths45M.GetMeans();
	Deaths50M.GetMeans();
	Deaths55M.GetMeans();
	NonAIDSdeaths20F.GetMeans();
	NonAIDSdeaths25F.GetMeans();
	NonAIDSdeaths30F.GetMeans();
	NonAIDSdeaths35F.GetMeans();
	NonAIDSdeaths40F.GetMeans();
	NonAIDSdeaths45F.GetMeans();
	NonAIDSdeaths50F.GetMeans();
	NonAIDSdeaths55F.GetMeans();
	NonAIDSdeaths20M.GetMeans();
	NonAIDSdeaths25M.GetMeans();
	NonAIDSdeaths30M.GetMeans();
	NonAIDSdeaths35M.GetMeans();
	NonAIDSdeaths40M.GetMeans();
	NonAIDSdeaths45M.GetMeans();
	NonAIDSdeaths50M.GetMeans();
	NonAIDSdeaths55M.GetMeans();

	// Numerators and denominators for UNAIDS indicators
	SummOutRow += 3;
	TotNegPop.GetMeans();
	NegChildrenU15.GetMeans();
	Neg15to24.GetMeans();
	Neg15to24M.GetMeans();
	Neg15to24F.GetMeans();
	Neg15to49.GetMeans();
	Neg15to49M.GetMeans();
	Neg15to49F.GetMeans();
	Neg25to49M.GetMeans();
	Neg25to49F.GetMeans();
	Neg50.GetMeans();
	Neg50M.GetMeans();
	Neg50F.GetMeans();
	DiagnosedHIVtot.GetMeans();
	DiagnosedHIV_U15.GetMeans();
	DiagnosedHIV_M.GetMeans();
	DiagnosedHIV_F.GetMeans();

	// Other outputs
	SummOutRow += 3;
	OItestingRate.GetMeans();
	HIVtestUptakeF25.GetMeans();
	TotalHIVtests15to24M.GetMeans(); // New to 4.4
	TotalHIVtests15to24F.GetMeans(); // New to 4.4
	TotalHIVtests25to49M.GetMeans(); // New to 4.4
	TotalHIVtests25to49F.GetMeans(); // New to 4.4
	TotalHIVtests50plusM.GetMeans(); // New to 4.4
	TotalHIVtests50plusF.GetMeans(); // New to 4.4
	AdultRootM.GetMeans();
	AdultRootF.GetMeans();
	ChildRoot.GetMeans();
	AdultARTinterrupters.GetMeans();
	ChildARTinterrupters.GetMeans(); // New to 4.3
	DiagnosedPropnAdult.GetMeans();
	ARTcoverageAdult.GetMeans();
	TotMTCTallBirths.GetMeans();
	AdultInterruptPropn.GetMeans();
	ChildInterruptPropn.GetMeans(); // New to 4.3
	TotBirthsARTconcep.GetMeans();
	MSMprev15to49.GetMeans();
	//MalePrev18plus.GetMeans();
	Circumcised15to24.GetMeans();
	Circumcised15plus.GetMeans();
	MMCprob10to14.GetMeans();
	HIVtestsPos18mo.GetMeans();
	HIVtestsPos19to59mo.GetMeans();
	HIVtestsPos5to14.GetMeans();
	PaedARTpropn0to4.GetMeans(); // New to 4.3
	PaedARTpropn5to9.GetMeans(); // New to 4.3
	AdultsOver500.GetMeans(); // New to 4.3
	Adults350to499.GetMeans(); // New to 4.3
	Adults200to349.GetMeans(); // New to 4.3
	AdultsUnder200.GetMeans(); // New to 4.3
	NewARTunder200F.GetMeans(); // New to 4.3
	NewART200to349F.GetMeans(); // New to 4.3
	NewART350to499F.GetMeans(); // New to 4.3
	NewARTover500F.GetMeans(); // New to 4.3
	HIVinc15to49adj.GetMeans();
	ANCincidence.GetMeans();
	ANCincidenceAdj.GetMeans();
	MultPartners15to24M.GetMeans();
	MultPartners15to24F.GetMeans();
	MultPartners25to49M.GetMeans();
	MultPartners25to49F.GetMeans();
	ARTresumptionRateM.GetMeans();
	ARTresumptionRateF.GetMeans();
	RelativeTestingVirgins.GetMeans();
	NewPrEPrateFSW.GetMeans();

	for (i = 0; i <= SummOutRow; i++){
		for (c = 0; c<86; c++){
			if (SummaryOutputs[i][c]<0.0 || SummaryOutputs[i][c]>0.0){
				file << std::setw(10) << std::right << SummaryOutputs[i][c] << "	";
			}
			else{
				file << std::setw(10) << std::right << "	";
			}
		}
		file << std::endl;
	}
	file.close();
}

void GetOutputsByAge(const char* filout)
{
	int i, c;
	std::ofstream file(filout);

	// Clear SummaryOutputs
	for (i = 0; i <= SummOutRow; i++){
		for (c = 0; c < 86; c++){
			SummaryOutputs[i][c] = 0.0;
		}
	}

	SummOutRow = 0;

	// Mortality outputs (27)
	MalePopAS.GetMeans();
	SummOutRow += 2;
	FemPopAS.GetMeans();
	SummOutRow += 2;
	MaleIncAS.GetMeans();
	SummOutRow += 2;
	FemIncAS.GetMeans();
	SummOutRow += 2;
	MalePrevAS.GetMeans();
	SummOutRow += 2;
	FemPrevAS.GetMeans();
	SummOutRow += 2;
	MaleMortAS.GetMeans();
	SummOutRow += 2;
	FemMortAS.GetMeans();
	SummOutRow += 2;
	MaleDiagAS.GetMeans();
	SummOutRow += 2;
	FemDiagAS.GetMeans();
	SummOutRow += 2;
	MaleART_AS.GetMeans();
	SummOutRow += 2;
	FemART_AS.GetMeans();
	SummOutRow += 2;
	MaleAIDSdeathsAS.GetMeans();
	SummOutRow += 2;
	FemAIDSdeathsAS.GetMeans();

	for (i = 0; i < SummOutRow; i++){
		for (c = 0; c<=CurrYear - StartYear; c++){
			if (SummaryOutputs[i][c]<0.0 || SummaryOutputs[i][c]>0.0){
				file << std::setw(10) << std::right << SummaryOutputs[i][c] << "	";
			}
			else{
				file << std::setw(10) << std::right << "	";
			}
		}
		file << std::endl;
	}
	file.close();
}

void CalcLikelihood()
{
	LogLikelihood = 0.0;
	if (CalibARTtotals == 1 || CalibARTtotalsP == 1){
		LogLikelihood += CalcARTtotalLogL();}
	if (CalibARTcoverage == 1){
		LogLikelihood += CalcARTcoverageLogL();}
	if (CalibARTbyAge == 1){
		LogLikelihood += CalcAgeARTlogL();}
	if (CalibARTbyAgeP == 1){
		LogLikelihood += CalcAgeART_PlogL();}
	if (CalibHCTprevP == 1){
		LogLikelihood += CalcHCTprevPlogL();}
	if (CalibHCTtotP == 1){
		LogLikelihood += CalcRecHCT_PlogL();}
	if (CalibARTbyAgeP2 == 1){
		LogLikelihood += CalcAgeART_P2logL();}
	if (ProvModel == 0){
		if (CalibPaedPrev == 1){
			LogLikelihood += CalcPaedPrevLogL();}
		if (CalibAdultPrev == 1){
			LogLikelihood += CalcAdultPrevLogL();}
		if (CalibANCprev == 1){
			LogLikelihood += CalcANCprevLogL2();}
		if (CalibDeathsA == 1){
			LogLikelihood += CalcMortLikelihoodA();}
		if (CalibDeathsP == 1){
			LogLikelihood += CalcMortLikelihoodP();}
		if (CalibHCT_HH == 1){
			LogLikelihood += CalcHCTlogL();}
		if (CalibHCTprev == 1){
			LogLikelihood += CalcHCTprevLogL();}
		if (CalibAIDStrend == 1){
			LogLikelihood += CalcAIDStrendLogL();}
		if (CalibAIDSage == 1){
			LogLikelihood += CalcAIDSageLogL();}
		if (CalibFSWprev == 1){
			LogLikelihood += CalcCSWprevLogL();}
		if (CalibMSMprev == 1){
			LogLikelihood += CalcMSMprevLogL();}
		if (CalibChildPIP == 1){
			LogLikelihood += CalcChildPIPlogL();}
		if (CalibMarriageData == 1){
			LogLikelihood += CalcMarriageLogL();}
	}
	else{
		if (CalibAdultPrev == 1){
			LogLikelihood += CalcHHprovLogL();}
		if (CalibPaedPrev == 1){
			LogLikelihood += CalcHHprovPlogL();}
		if (CalibANCprev == 1){
			LogLikelihood += CalcANCprevLogL2();}
		if (CalibDeathsA == 1){
			LogLikelihood += CalcMortLikelihoodA();}
		if (CalibARTcoverage == 1){
			LogLikelihood += CalcMaleARTlogL();}
		if (CalibCD4atARTstart == 1){ // Currently only works for WC
			LogLikelihood += CalcARTbyCD4logL();}
		//if (ARTerrorInd == 1){ LogLikelihood = -100000.0; }
	}

	//PrevLogL.out[CurrSim-1][0] = LogLikelihood;
}

double CalcPaedPrevLogL()
{
	int ia, ig;
	double SElogitPrev05, SElogitPrev08, SElogitPrev12, SElogitPrev17, PaedLogL;

	PaedLogL = 0.0;
	for(ia=0; ia<3; ia++){
		for(ig=0; ig<2; ig++){
			SElogitPrev05 = SEprev05[ia][ig]/(ObservedPrev05[ia][ig] * (1.0 - ObservedPrev05[ia][ig]));
			PaedLogL -= 0.5 * pow((log(ModelPrevPaed[ia][ig][0] / (1.0 - ModelPrevPaed[ia][ig][0])) -
				log(ObservedPrev05[ia][ig]/(1.0 - ObservedPrev05[ia][ig])))/SElogitPrev05, 2.0);
			SElogitPrev08 = SEprev08[ia][ig]/(ObservedPrev08[ia][ig] * (1.0 - ObservedPrev08[ia][ig]));
			PaedLogL -= 0.5 * pow((log(ModelPrevPaed[ia][ig][1] / (1.0 - ModelPrevPaed[ia][ig][1])) -
				log(ObservedPrev08[ia][ig]/(1.0 - ObservedPrev08[ia][ig])))/SElogitPrev08, 2.0);
			if (ia < 2){
				SElogitPrev12 = SEprev12[ia][ig] / (ObservedPrev12[ia][ig] * (1.0 - ObservedPrev12[ia][ig]));
				PaedLogL -= 0.5 * pow((log(ModelPrevPaed2[ia][ig][0] / (1.0 - ModelPrevPaed2[ia][ig][0])) -
					log(ObservedPrev12[ia][ig] / (1.0 - ObservedPrev12[ia][ig]))) / SElogitPrev12, 2.0);
				SElogitPrev17 = SEprev17[ia][ig] / (ObservedPrev17[ia][ig] * (1.0 - ObservedPrev17[ia][ig]));
				PaedLogL -= 0.5 * pow((log(ModelPrevPaed2[ia][ig][1] / (1.0 - ModelPrevPaed2[ia][ig][1])) -
					log(ObservedPrev17[ia][ig] / (1.0 - ObservedPrev17[ia][ig]))) / SElogitPrev17, 2.0);
			}
		}
	}
	SElogitPrev08 = SEprevU208/(ObservedPrevU208 * (1.0 - ObservedPrevU208));
	PaedLogL -= 0.5 * pow((log(ModelPrevU208/(1.0 - ModelPrevU208)) -
		log(ObservedPrevU208/(1.0 - ObservedPrevU208)))/SElogitPrev08, 2.0);

	return PaedLogL;
}

double CalcAdultPrevLogL()
{
	int ia, ig;
	double SElogitPrev05, SElogitPrev08, SElogitPrev12, SElogitPrev16, SElogitPrev17, AdultLogL;

	AdultLogL = 0.0;

	// HSRC data
	for(ia=0; ia<9; ia++){
		for(ig=0; ig<2; ig++){
			if (AdultHHprev[20][ia][ig] < 0.0001){ AdultHHprev[20][ia][ig] = 0.0001; }
			if (AdultHHprev[23][ia][ig] < 0.0001){ AdultHHprev[23][ia][ig] = 0.0001; }
			if (AdultHHprev[27][ia][ig] < 0.0001){ AdultHHprev[27][ia][ig] = 0.0001; }
			if (AdultHHprev[32][ia][ig] < 0.0001){ AdultHHprev[32][ia][ig] = 0.0001; }
			SElogitPrev05 = SEprev05[ia+3][ig]/(ObservedPrev05[ia+3][ig] * (1.0 - ObservedPrev05[ia+3][ig]));
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev05, 2.0)) +
				pow((log(AdultHHprev[20][ia][ig]/(1.0 - AdultHHprev[20][ia][ig])) -
				log(ObservedPrev05[ia+3][ig]/(1.0 - ObservedPrev05[ia+3][ig])))/SElogitPrev05, 2.0));
			SElogitPrev08 = SEprev08[ia+3][ig]/(ObservedPrev08[ia+3][ig] * (1.0 - ObservedPrev08[ia+3][ig]));
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev08, 2.0)) +
				pow((log(AdultHHprev[23][ia][ig]/(1.0 - AdultHHprev[23][ia][ig])) -
				log(ObservedPrev08[ia+3][ig]/(1.0 - ObservedPrev08[ia+3][ig])))/SElogitPrev08, 2.0));
			// Note that for 2012 survey a different age offset applies
			SElogitPrev12 = SEprev12[ia+3][ig]/(ObservedPrev12[ia+3][ig] * (1.0 - ObservedPrev12[ia+3][ig]));
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev12, 2.0)) +
				pow((log(AdultHHprev[27][ia][ig]/(1.0 - AdultHHprev[27][ia][ig])) -
				log(ObservedPrev12[ia+3][ig]/(1.0 - ObservedPrev12[ia+3][ig])))/SElogitPrev12, 2.0));
			SElogitPrev17 = SEprev17[ia + 3][ig] / (ObservedPrev17[ia + 3][ig] * (1.0 - ObservedPrev17[ia + 3][ig]));
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev17, 2.0)) +
				pow((log(AdultHHprev[32][ia][ig] / (1.0 - AdultHHprev[32][ia][ig])) -
				log(ObservedPrev17[ia + 3][ig] / (1.0 - ObservedPrev17[ia + 3][ig]))) / SElogitPrev17, 2.0));
		}
	}

	// DHS data
	for (ig = 0; ig < 2; ig++){
		for (ia = 0; ia < 9; ia++){
			SElogitPrev16 = SEprev16[ia][ig] / (ObservedPrev16[ia][ig] * (1.0 - ObservedPrev16[ia][ig]));
			if (AdultHHprev[31][ia][ig] < 0.0001){ AdultHHprev[31][ia][ig] = 0.0001; }
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev16, 2.0)) +
				pow((log(AdultHHprev[31][ia][ig] / (1.0 - AdultHHprev[31][ia][ig])) - log(ObservedPrev16[ia][ig] /
				(1.0 - ObservedPrev16[ia][ig]))) / SElogitPrev16, 2.0));
		}
	}

	return AdultLogL;
}

double CalcHHprovLogL()
{
	int ia, iy;
	double SElogitPrev, AdultLogL;

	AdultLogL = 0.0;

	// HSRC and DHS data
	for (ia = 0; ia<2; ia++){
		for (iy = 0; iy<5; iy++){
			if (ModelProvHH[iy][ia] < 0.0001){ ModelProvHH[iy][ia] = 0.0001; }
			SElogitPrev = SEprovHH[iy][ia] / (ObservedProvHH[iy][ia] * (1.0 - ObservedProvHH[iy][ia]));
			AdultLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev, 2.0)) +
				pow((log(ModelProvHH[iy][ia] / (1.0 - ModelProvHH[iy][ia])) -
				log(ObservedProvHH[iy][ia] / (1.0 - ObservedProvHH[iy][ia]))) / SElogitPrev, 2.0));
		}
	}

	return AdultLogL;
}

double CalcHHprovPlogL()
{
	int ia, iy;
	double SElogitPrev, PaedLogL;

	PaedLogL = 0.0;

	for (iy = 0; iy<4; iy++){
		if (ModelProvHH_P[iy] < 0.0001){ ModelProvHH_P[iy] = 0.0001; }
		SElogitPrev = SEprovHH_P[iy] / (ObservedProvHH_P[iy] * (1.0 - ObservedProvHH_P[iy]));
		PaedLogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogitPrev, 2.0)) +
			pow((log(ModelProvHH_P[iy] / (1.0 - ModelProvHH_P[iy])) -
			log(ObservedProvHH_P[iy] / (1.0 - ObservedProvHH_P[iy]))) / SElogitPrev, 2.0));
	}

	return PaedLogL;
}

double CalcANCprevLogL()
{
	int ia, iy, iystart;
	double ANCbias1; // Bias over the 1997-2017, on logit scale
	double ANCbiasInit;
	double ANClogL, VarLogitPrev[5][26], ModelVarEst, RecencyAdj;
	double AdjPrevPregnant[6][26]; // PrevPregnant adjusted to reflect antenatal bias
	double AdjObsPrevANC[5][26]; // Survey estimates adjusted for false positives

	// Corrections for specificity
	for (ia = 0; ia < 5; ia++){
		for (iy = 0; iy < 26; iy++){
			if (iy < 6 || iy >= 25){
				AdjObsPrevANC[ia][iy] = ObservedPrevANC[ia][iy];}
			else{
				AdjObsPrevANC[ia][iy] = 1.0 - SpecificityANC * (1.0 - ObservedPrevANC[ia][iy]);}
		}
	}

	// In Thembisa 4.3 I simplified the code so that there is only 1 ANCbias in the national calibration.

	// Antenatal bias adjustments
	ANCbias1 = 0.0;
	for(ia=0; ia<5; ia++){
		for(iy=6; iy<25; iy++){ // 1997-2015
			RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
			ANCbias1 += log(AdjObsPrevANC[ia][iy] / (1.0 - AdjObsPrevANC[ia][iy])) -
				log(PrevPregnant[ia][iy + 6] * RecencyAdj / (1.0 - PrevPregnant[ia][iy + 6] *
				RecencyAdj));
		}
		// 2017+
		RecencyAdj = pow(FractionRecentF[ia][32], RecencyBiasANC);
		ANCbias1 += log(AdjObsPrevANC[ia][25] / (1.0 - AdjObsPrevANC[ia][25])) -
			log(PrevPregnant[ia][32] * RecencyAdj / (1.0 - PrevPregnant[ia][32] * RecencyAdj));
	}
	ANCbias1 = ANCbias1/(5.0 * 20);
	if(InclANCpre1997==1){
		for(ia=0; ia<5; ia++){
			for(iy=0; iy<6; iy++){
				RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
				ANCbiasInit += log(AdjObsPrevANC[ia][iy] / (1.0 - AdjObsPrevANC[ia][iy])) -
					log(PrevPregnant[ia][iy+6] * RecencyAdj/(1.0 - PrevPregnant[ia][iy+6] *
					RecencyAdj));
			}
		}
		ANCbias1 = (ANCbias1 * (5.0 * 20) + ANCbiasInit) / ((5.0 * 20) + 30.0);
	}
	for(ia=0; ia<6; ia++){
		for(iy=0; iy<25; iy++){
			RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
			AdjPrevPregnant[ia][iy] = 1.0/(1.0 + (1.0/(PrevPregnant[ia][iy+6] * RecencyAdj) - 1.0) * exp(-ANCbias1));
		}
		RecencyAdj = pow(FractionRecentF[ia][32], RecencyBiasANC);
		AdjPrevPregnant[ia][25] = 1.0 / (1.0 + (1.0 / (PrevPregnant[ia][32] * RecencyAdj) - 1.0) * exp(-ANCbias1));
	}

	// Variance calculations
	ModelVarEst = 0.0;
	if(InclANCpre1997==1){iystart = 0;}
	else{iystart = 6;}
	for(ia=0; ia<5; ia++){
		for(iy=iystart; iy<26; iy++){
			VarLogitPrev[ia][iy] = pow(SEprevANC[ia][iy] / (AdjObsPrevANC[ia][iy] *
				(1.0 - AdjObsPrevANC[ia][iy])), 2.0);
			ModelVarEst += pow(log(AdjObsPrevANC[ia][iy] / (1.0 - AdjObsPrevANC[ia][iy])) -
				log(AdjPrevPregnant[ia][iy]/(1.0 - AdjPrevPregnant[ia][iy])), 2.0) -
				VarLogitPrev[ia][iy];
		}
	}
	if(ModelVarEst < 0.0){
		ModelVarEst = 0.0;}
	else{
		ModelVarEst = ModelVarEst/(5.0 * (26.0 - iystart));}

	// Calculate likelihood
	ANClogL = 0.0;
	for(ia=0; ia<5; ia++){
		for(iy=iystart; iy<26; iy++){
			ANClogL += -0.5 * (log(2.0 * 3.141592654 * (VarLogitPrev[ia][iy] + ModelVarEst)) +
				pow(log(AdjObsPrevANC[ia][iy] / (1.0 - AdjObsPrevANC[ia][iy])) -
				log(AdjPrevPregnant[ia][iy]/(1.0 - AdjPrevPregnant[ia][iy])), 2.0)/
				(VarLogitPrev[ia][iy] + ModelVarEst));
		}
	}

	// Store outputs
	if(FixedUncertainty==1){
		ANCbias.out[CurrSim-1][0] = ANCbias1;
		ANCbias.out[CurrSim-1][1] = ANCbias1;
		ErrorVariance.out[CurrSim-1][0] = ModelVarEst;
		for(iy=0; iy<=CurrYear-StartYear; iy++){
			PrevPreg15to19.out[CurrSim-1][iy] = PrevPregnant[0][iy];
			PrevPreg20to24.out[CurrSim-1][iy] = PrevPregnant[1][iy];
			PrevPreg25to29.out[CurrSim-1][iy] = PrevPregnant[2][iy];
			PrevPreg30to34.out[CurrSim-1][iy] = PrevPregnant[3][iy];
			PrevPreg35to39.out[CurrSim-1][iy] = PrevPregnant[4][iy];
			PrevPreg40to49.out[CurrSim-1][iy] = PrevPregnant[5][iy];
			PrevPreg15to49.out[CurrSim - 1][iy] = PrevPregnant[6][iy];
			if (iy >= 6 && iy<=30){
				AdjPreg15to19.out[CurrSim - 1][iy] = AdjPrevPregnant[0][iy - 6];
				AdjPreg20to24.out[CurrSim - 1][iy] = AdjPrevPregnant[1][iy - 6];
				AdjPreg25to29.out[CurrSim - 1][iy] = AdjPrevPregnant[2][iy - 6];
				AdjPreg30to34.out[CurrSim - 1][iy] = AdjPrevPregnant[3][iy - 6];
				AdjPreg35to39.out[CurrSim - 1][iy] = AdjPrevPregnant[4][iy - 6];
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] * AdjPrevPregnant[ia][iy - 6]; }
			}
			if (iy > 30){
				AdjPreg15to19.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[0][iy] *
					pow(FractionRecentF[0][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				AdjPreg20to24.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[1][iy] *
					pow(FractionRecentF[1][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				AdjPreg25to29.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[2][iy] *
					pow(FractionRecentF[2][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				AdjPreg30to34.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[3][iy] *
					pow(FractionRecentF[3][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				AdjPreg35to39.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[4][iy] *
					pow(FractionRecentF[4][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] / (1.0 + (1.0 / (PrevPregnant[ia][iy] *
						pow(FractionRecentF[ia][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbias1));
				}
			}
		}
	}

	return ANClogL;
}

double CalcANCprevLogL2()
{
	// This differs from the original function in the way we calculate antenatal bias.
	// Also the variance calculation is more accurate - I've added a Newton-Raphson step.

	int ia, iy, iystart, ii;
	double TempSp, FirstDeriv, SecondDeriv;
	double ANClogL, VarLogitPrev[5][27], SquaredDif[5][27], ModelVarEst;
	double AdjPrevPregnant[7][27]; // PrevPregnant adjusted to reflect antenatal bias

	// Adjust for bias due to test specificity and private sector
	for (ia = 0; ia < 7; ia++){
		for (iy = 0; iy < 27; iy++){
			if (iy < 6 || iy >= 25){ TempSp = 1.0; }
			else{ TempSp = SpecificityANC; }
			if (iy < 25){ AdjPrevPregnant[ia][iy] = PrevPregnant[ia][iy + 6]; }
			if (iy == 25){ AdjPrevPregnant[ia][iy] = PrevPregnant[ia][32]; }
			if (iy == 26){ AdjPrevPregnant[ia][iy] = PrevPregnant[ia][34]; }
			AdjPrevPregnant[ia][iy] = 1.0 - TempSp + TempSp * AdjPrevPregnant[ia][iy] /
				(PropnPregPrivate[ia] * (RRprevPrivateANC - 1.0) + 1.0);
			if (AdjPrevPregnant[ia][iy] > 1.0){ AdjPrevPregnant[ia][iy] = 0.9999; }
		}
	}

	// Variance calculations
	ModelVarEst = 0.0;
	//if (InclANCpre1997 == 1){ iystart = 0; }
	//else{ iystart = 6; }
	iystart = 3; // Corresponds to 1994
	for (ia = 0; ia<5; ia++){
		for (iy = iystart; iy<27; iy++){
			VarLogitPrev[ia][iy] = pow(SEprevANC[ia][iy] / (ObservedPrevANC[ia][iy] *
				(1.0 - ObservedPrevANC[ia][iy])), 2.0);
			SquaredDif[ia][iy] = pow(log(ObservedPrevANC[ia][iy] / (1.0 - ObservedPrevANC[ia][iy])) -
				log(AdjPrevPregnant[ia][iy] / (1.0 - AdjPrevPregnant[ia][iy])), 2.0);
			ModelVarEst += SquaredDif[ia][iy] - VarLogitPrev[ia][iy];
		}
	}
	if (ModelVarEst < 0.0){
		ModelVarEst = 0.0;}
	else{
		ModelVarEst = ModelVarEst / (5.0 * (27.0 - iystart));}

	// Update variance using Newton-Raphson method
	/*for (ii = 0; ii < 3; ii++){
		FirstDeriv = 0.0;
		SecondDeriv = 0.0;
		for (ia = 0; ia < 5; ia++){
			for (iy = iystart; iy < 26; iy++){
				FirstDeriv += 0.5 * SquaredDif[ia][iy] / pow(ModelVarEst + VarLogitPrev[ia][iy], 2.0) -
					0.5 / (ModelVarEst + VarLogitPrev[ia][iy]);
				SecondDeriv += 0.5 / pow(ModelVarEst + VarLogitPrev[ia][iy], 2.0) - SquaredDif[ia][iy] /
					pow(ModelVarEst + VarLogitPrev[ia][iy], 3.0);
			}
		}
		ModelVarEst = ModelVarEst - FirstDeriv / SecondDeriv;
	}
	if (ModelVarEst < 0.0){ ModelVarEst = 0.0; }*/
	if (ProvModel == 0){ ModelVarEst = 0.16; }
	else{ ModelVarEst = 0.09; }

	// Calculate likelihood
	ANClogL = 0.0;
	for (ia = 0; ia<5; ia++){
		for (iy = iystart; iy<27; iy++){
			ANClogL += -0.5 * (log(2.0 * 3.141592654 * (VarLogitPrev[ia][iy] + ModelVarEst)) +
				SquaredDif[ia][iy] / (VarLogitPrev[ia][iy] + ModelVarEst));
		}
	}

	// Store outputs
	if (FixedUncertainty == 1){
		ErrorVariance.out[CurrSim - 1][0] = ModelVarEst;
		for (iy = 0; iy <= CurrYear - StartYear; iy++){
			PrevPreg15to19.out[CurrSim - 1][iy] = PrevPregnant[0][iy];
			PrevPreg20to24.out[CurrSim - 1][iy] = PrevPregnant[1][iy];
			PrevPreg25to29.out[CurrSim - 1][iy] = PrevPregnant[2][iy];
			PrevPreg30to34.out[CurrSim - 1][iy] = PrevPregnant[3][iy];
			PrevPreg35to39.out[CurrSim - 1][iy] = PrevPregnant[4][iy];
			PrevPreg40to49.out[CurrSim - 1][iy] = PrevPregnant[5][iy];
			PrevPreg15to49.out[CurrSim - 1][iy] = PrevPregnant[6][iy];
			if (iy >= 6 && iy <= 30){
				AdjPreg15to19.out[CurrSim - 1][iy] = AdjPrevPregnant[0][iy - 6];
				AdjPreg20to24.out[CurrSim - 1][iy] = AdjPrevPregnant[1][iy - 6];
				AdjPreg25to29.out[CurrSim - 1][iy] = AdjPrevPregnant[2][iy - 6];
				AdjPreg30to34.out[CurrSim - 1][iy] = AdjPrevPregnant[3][iy - 6];
				AdjPreg35to39.out[CurrSim - 1][iy] = AdjPrevPregnant[4][iy - 6];
				//AdjPreg15to49.out[CurrSim - 1][iy] = AdjPrevPregnant[6][iy - 6];
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					if (iy < 28){ ANCageWeights[ia] = ANCageW_init[ia]; }
					else{ ANCageWeights[ia] = ANCageW_init[ia] + ANCageW_change[ia] * (iy - 27); }
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] * AdjPrevPregnant[ia][iy - 6];
				}
			}
			if (iy > 30){
				AdjPreg15to19.out[CurrSim - 1][iy] = PrevPregnant[0][iy] / (PropnPregPrivate[0] *
					(RRprevPrivateANC - 1.0) + 1.0);
				AdjPreg20to24.out[CurrSim - 1][iy] = PrevPregnant[1][iy] / (PropnPregPrivate[1] *
					(RRprevPrivateANC - 1.0) + 1.0);
				AdjPreg25to29.out[CurrSim - 1][iy] = PrevPregnant[2][iy] / (PropnPregPrivate[2] *
					(RRprevPrivateANC - 1.0) + 1.0);
				AdjPreg30to34.out[CurrSim - 1][iy] = PrevPregnant[3][iy] / (PropnPregPrivate[3] *
					(RRprevPrivateANC - 1.0) + 1.0);
				AdjPreg35to39.out[CurrSim - 1][iy] = PrevPregnant[4][iy] / (PropnPregPrivate[4] *
					(RRprevPrivateANC - 1.0) + 1.0);
				//AdjPreg15to49.out[CurrSim - 1][iy] = PrevPregnant[6][iy] / (PropnPregPrivate[6] *
				//	(RRprevPrivateANC - 1.0) + 1.0);
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] * (1.0 - TempSp + TempSp *
						PrevPregnant[ia][iy] / (PropnPregPrivate[ia] * (RRprevPrivateANC - 1.0) + 1.0));
				}
			}
		}
	}

	return ANClogL;
}

double CalcANCprovLogL()
{
	// This function no longer gets used since we are now (in version 17) using the same function for
	// calculating the likelihood in the provincial and national models. But I'm keeping it in case we
	// need to revert to using aggregated (not age-specific) prevalence data.

	int iy, iystart, ia;
	double ANCbias1, ANCbias2; // Bias over the 1997-2005 and 2006-2015 periods, on logit scale
	double ANCbiasInit, ANCbiasTemp; // ANC bias in 1991
	double ANClogL, VarLogitPrev[26], ModelVarEst, RecencyAdj;
	double AdjPrevPregnant[26]; // PrevPregnant adjusted to reflect antenatal bias
	double ASprevPregnant[6][26]; // Age-specific PrevPregnant adjusted to reflect antenatal bias
	double ASlogitVar[5][26]; // Age-specific variance on logit scale

	if (InclAS_ANCprov == 0){
		// Calibrate to AGGREGATED prevalence data
		// Note that this hasn't been updated to include post-2015 data, since we've revised the
		// calibration approach to work with age-specific data.
		// Antenatal bias adjustments
		ANCbias1 = 0.0;
		ANCbias2 = 0.0;
		ANCbiasInit = 0.0;
		if (ProvANCbias == 0){
			for (iy = 7; iy < 16; iy++){
				ANCbias1 += log(ObservedProvANC[iy] / (1.0 - ObservedProvANC[iy])) -
					log(PrevPregnant[6][iy + 5] / (1.0 - PrevPregnant[6][iy + 5]));
			}
			for (iy = 16; iy < 26; iy++){
				ANCbias2 += log(ObservedProvANC[iy] / (1.0 - ObservedProvANC[iy])) -
					log(PrevPregnant[6][iy + 5] / (1.0 - PrevPregnant[6][iy + 5]));
			}
			//ANCbias1 = ANCbias1/9.0;
			//ANCbias2 = ANCbias2/10.0;
			ANCbias1 = (ANCbias1 * 9.0 + ANCbias2 * 10.0) / 19.0;
		}
		else{
			ANCbias1 = ProvANCbias;
		}
		ANCbias2 = ANCbias1;
		if (InclANCpre1997 == 1){
			ANCbiasInit = ANCbias1;
			/*for (iy = 1; iy<7; iy++){
				ANCbiasInit += log(ObservedProvANC[iy] / (1.0 - ObservedProvANC[iy])) -
				log(PrevPregnant[6][iy + 5] / (1.0 - PrevPregnant[6][iy + 5]));
				}
				ANCbiasInit = 2.0 * (ANCbiasInit - 12.5 * ANCbias1) / 35.0;
				if (ANCbiasInit<ANCbias1){
				ANCbiasInit = ANCbias1;
				}*/
		}
		if (InclANCpre1997 == 1){
			for (iy = 0; iy < 7; iy++){
				ANCbiasTemp = (ANCbiasInit * (7.0 - iy) + ANCbias1 * (iy - 1.0)) / 6.0;
				AdjPrevPregnant[iy] = 1.0 / (1.0 + (1.0 / PrevPregnant[6][iy + 5] - 1.0) * exp(-ANCbiasTemp));
			}
		}
		for (iy = 7; iy < 16; iy++){
			AdjPrevPregnant[iy] = 1.0 / (1.0 + (1.0 / PrevPregnant[6][iy + 5] - 1.0) * exp(-ANCbias1));
		}
		for (iy = 16; iy < 26; iy++){
			AdjPrevPregnant[iy] = 1.0 / (1.0 + (1.0 / PrevPregnant[6][iy + 5] - 1.0) * exp(-ANCbias2));
		}

		// Variance calculations
		ModelVarEst = 0.0;
		if (InclANCpre1997 == 1){ iystart = 0; }
		else{ iystart = 7; }
		for (iy = iystart; iy < 26; iy++){
			VarLogitPrev[iy] = pow(SEprovANC[iy] / (ObservedProvANC[iy] *
				(1.0 - ObservedProvANC[iy])), 2.0);
			ModelVarEst += pow(log(ObservedProvANC[iy] / (1.0 - ObservedProvANC[iy])) -
				log(AdjPrevPregnant[iy] / (1.0 - AdjPrevPregnant[iy])), 2.0) -
				VarLogitPrev[iy];
		}
		if (ModelVarEst < 0.0){
			ModelVarEst = 0.0;
		}
		else{
			ModelVarEst = ModelVarEst / (26.0 - iystart);
		}

		// Calculate likelihood
		ANClogL = 0.0;
		for (iy = iystart; iy < 26; iy++){
			ANClogL += -0.5 * (log(2.0 * 3.141592654 * (VarLogitPrev[iy] + ModelVarEst)) +
				pow(log(ObservedProvANC[iy] / (1.0 - ObservedProvANC[iy])) -
				log(AdjPrevPregnant[iy] / (1.0 - AdjPrevPregnant[iy])), 2.0) /
				(VarLogitPrev[iy] + ModelVarEst));
		}
	}
	else{
		// Calibrate to age-specific HIV prevalence data.
		// Code copied mostly from CalcANCprevLogL function.

		// Antenatal bias adjustments
		// Calculations assume no change in bias over time, on logit scale.
		ANCbias1 = 0.0;
		ANCbias2 = 0.0;
		ANCbiasInit = 0.0;
		if (ProvANCbias == 0.0){
			for (ia = 0; ia < 5; ia++){
				for (iy = 3; iy < 6; iy++){ // 1994-1996
					RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
					ANCbias1 += log(ObservedPrevANC[ia][iy] / (1.0 - ObservedPrevANC[ia][iy])) -
						log(PrevPregnant[ia][iy + 6] * RecencyAdj / (1.0 - PrevPregnant[ia][iy + 6] *
						RecencyAdj));
				}
				for (iy = 6; iy < 25; iy++){ // 1997-2015
					RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
					ANCbias2 += log(ObservedPrevANC[ia][iy] / (1.0 - ObservedPrevANC[ia][iy])) -
						log(PrevPregnant[ia][iy + 6] * RecencyAdj / (1.0 - PrevPregnant[ia][iy + 6] *
						RecencyAdj));
				}
				// 2017+
				RecencyAdj = pow(FractionRecentF[ia][32], RecencyBiasANC);
				ANCbias2 += log(ObservedPrevANC[ia][25] / (1.0 - ObservedPrevANC[ia][25])) -
					log(PrevPregnant[ia][32] * RecencyAdj / (1.0 - PrevPregnant[ia][32] * RecencyAdj));
			}
			if (InclANCpre1997 == 0){
				ANCbiasTemp = ANCbias2 / (5.0 * 20);
			}
			else{
				ANCbiasTemp = (ANCbias1 + ANCbias2) / (5.0 * 23);
			}
		}
		else{
			ANCbias1 = ProvANCbias;
			ANCbias2 = ProvANCbias;
			ANCbiasTemp = ProvANCbias;
			ANCbiasInit = ProvANCbias;
		}
		for (ia = 0; ia<6; ia++){
			for (iy = 0; iy<25; iy++){
				RecencyAdj = pow(FractionRecentF[ia][iy + 6], RecencyBiasANC);
				ASprevPregnant[ia][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[ia][iy + 6] * RecencyAdj) - 1.0) *
					exp(-ANCbiasTemp));
			}
			RecencyAdj = pow(FractionRecentF[ia][32], RecencyBiasANC);
			ASprevPregnant[ia][25] = 1.0 / (1.0 + (1.0 / (PrevPregnant[ia][32] * RecencyAdj) - 1.0) *
				exp(-ANCbiasTemp));
		}

		// Variance calculations
		ModelVarEst = 0.0;
		if (InclANCpre1997 == 1){ iystart = 3; }
		else{ iystart = 6; }
		for (ia = 0; ia<5; ia++){
			for (iy = iystart; iy<26; iy++){
				ASlogitVar[ia][iy] = pow(SEprevANC[ia][iy] / (ObservedPrevANC[ia][iy] *
					(1.0 - ObservedPrevANC[ia][iy])), 2.0);
				ModelVarEst += pow(log(ObservedPrevANC[ia][iy] / (1.0 - ObservedPrevANC[ia][iy])) -
					log(ASprevPregnant[ia][iy] / (1.0 - ASprevPregnant[ia][iy])), 2.0) -
					ASlogitVar[ia][iy];
			}
		}
		if (ModelVarEst < 0.0){
			ModelVarEst = 0.0;
		}
		else{
			ModelVarEst = ModelVarEst / (5.0 * (26.0 - iystart));
		}

		// Calculate likelihood
		ANClogL = 0.0;
		for (ia = 0; ia<5; ia++){
			for (iy = iystart; iy<26; iy++){
				ANClogL += -0.5 * (log(2.0 * 3.141592654 * (ASlogitVar[ia][iy] + ModelVarEst)) +
					pow(log(ObservedPrevANC[ia][iy] / (1.0 - ObservedPrevANC[ia][iy])) -
					log(ASprevPregnant[ia][iy] / (1.0 - ASprevPregnant[ia][iy])), 2.0) /
					(ASlogitVar[ia][iy] + ModelVarEst));
			}
		}
	}

	// Store outputs
	if (FixedUncertainty == 1){
		ANCbias.out[CurrSim - 1][0] = ANCbiasTemp;
		ANCbias.out[CurrSim - 1][1] = ANCbiasTemp;
		ErrorVariance.out[CurrSim - 1][0] = ModelVarEst;
		for (iy = 0; iy <= CurrYear - StartYear; iy++){
			PrevPreg15to19.out[CurrSim - 1][iy] = PrevPregnant[0][iy];
			PrevPreg20to24.out[CurrSim - 1][iy] = PrevPregnant[1][iy];
			PrevPreg25to29.out[CurrSim - 1][iy] = PrevPregnant[2][iy];
			PrevPreg30to34.out[CurrSim - 1][iy] = PrevPregnant[3][iy];
			PrevPreg35to39.out[CurrSim - 1][iy] = PrevPregnant[4][iy];
			PrevPreg40to49.out[CurrSim - 1][iy] = PrevPregnant[5][iy];
			PrevPreg15to49.out[CurrSim - 1][iy] = PrevPregnant[6][iy];
			if (iy >= 6 && iy <= 30 && InclAS_ANCprov == 1){
				AdjPreg15to19.out[CurrSim - 1][iy] = ASprevPregnant[0][iy - 6];
				AdjPreg20to24.out[CurrSim - 1][iy] = ASprevPregnant[1][iy - 6];
				AdjPreg25to29.out[CurrSim - 1][iy] = ASprevPregnant[2][iy - 6];
				AdjPreg30to34.out[CurrSim - 1][iy] = ASprevPregnant[3][iy - 6];
				AdjPreg35to39.out[CurrSim - 1][iy] = ASprevPregnant[4][iy - 6];
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] * ASprevPregnant[ia][iy - 6];
				}
			}
			if (iy > 30 && InclAS_ANCprov == 1){
				AdjPreg15to19.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[0][iy] *
					pow(FractionRecentF[0][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				AdjPreg20to24.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[1][iy] *
					pow(FractionRecentF[1][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				AdjPreg25to29.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[2][iy] *
					pow(FractionRecentF[2][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				AdjPreg30to34.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[3][iy] *
					pow(FractionRecentF[3][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				AdjPreg35to39.out[CurrSim - 1][iy] = 1.0 / (1.0 + (1.0 / (PrevPregnant[4][iy] *
					pow(FractionRecentF[4][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				AdjPreg15to49.out[CurrSim - 1][iy] = 0.0;
				for (ia = 0; ia < 6; ia++){
					AdjPreg15to49.out[CurrSim - 1][iy] += ANCageWeights[ia] / (1.0 + (1.0 / (PrevPregnant[ia][iy] *
						pow(FractionRecentF[ia][iy], RecencyBiasANC)) - 1.0) * exp(-ANCbiasTemp));
				}
			}
		}
		ARTerror.out[CurrSim - 1][0] = ARTerrorInd;
	}

	return ANClogL;
}

double CalcCSWprevLogL()
{
	double ModelPrev[29], RandomError[29], VarRandEffects, CSWlogL;
	int ic, year;

	// Calculate ModelPrev and RandomError
	for (ic = 0; ic < 29; ic++){
		year = CSWstudyDetails[ic][0];
		ModelPrev[ic] = FSWprev[year-StartYear];
		RandomError[ic] = 1.0 / (CSWstudyDetails[ic][1] * CSWstudyDetails[ic][2] *
			(1.0 - CSWstudyDetails[ic][2]));
	}

	// Calculate variance of random effects
	/*VarRandEffects = 0.0;
	for (ic = 0; ic < 29; ic++){
		VarRandEffects += pow(log(CSWstudyDetails[ic][2] / (1.0 - CSWstudyDetails[ic][2])) -
			log(ModelPrev[ic] / (1.0 - ModelPrev[ic])), 2.0) - RandomError[ic];
	}
	VarRandEffects = VarRandEffects / 29.0;*/
	VarRandEffects = 0.63;
	if (VarRandEffects < 0.0){ VarRandEffects = 0.0; }
	if (FixedUncertainty == 1){ ErrorVariance.out[CurrSim - 1][0] = VarRandEffects; }

	// Calculate likelihood
	CSWlogL = 0.0;
	for (ic = 0; ic < 29; ic++){
		CSWlogL += -0.5 * (log(2.0 * 3.141592654 * (RandomError[ic] + VarRandEffects)) +
			pow(log(CSWstudyDetails[ic][2] / (1.0 - CSWstudyDetails[ic][2])) -
			log(ModelPrev[ic] / (1.0 - ModelPrev[ic])), 2.0) /
			(RandomError[ic] + VarRandEffects));
	}

	return CSWlogL;
}

double CalcMSMprevLogL()
{
	double ModelPrev[17], RandomError[17], VarRandEffects, MSMlogL;
	int ic, year;

	// Calculate ModelPrev and RandomError
	for (ic = 0; ic < 17; ic++){
		year = MSMstudyDetails[ic][0] - StartYear;
		ModelPrev[ic] = MSMstudyDetails[ic][3] * MSMprev18to24.out[CurrSim - 1][year] +
			(1.0 - MSMstudyDetails[ic][3]) * MSMprev25plus.out[CurrSim - 1][year];
		RandomError[ic] = 1.0 / (MSMstudyDetails[ic][1] * MSMstudyDetails[ic][2] *
			(1.0 - MSMstudyDetails[ic][2]));
	}

	// Calculate variance of random effects
	/*VarRandEffects = 0.0;
	for (ic = 0; ic < 17; ic++){
		VarRandEffects += pow(log(MSMstudyDetails[ic][2] / (1.0 - MSMstudyDetails[ic][2])) -
			log(ModelPrev[ic] / (1.0 - ModelPrev[ic])), 2.0) - RandomError[ic];
	}
	VarRandEffects = VarRandEffects / 17.0;*/
	VarRandEffects = 0.59;
	if (VarRandEffects < 0.0){ VarRandEffects = 0.0; }
	if (FixedUncertainty == 1){ ErrorVariance.out[CurrSim - 1][1] = VarRandEffects; }

	// Calculate likelihood
	MSMlogL = 0.0;
	for (ic = 0; ic < 17; ic++){
		MSMlogL += -0.5 * (log(2.0 * 3.141592654 * (RandomError[ic] + VarRandEffects)) +
			pow(log(MSMstudyDetails[ic][2] / (1.0 - MSMstudyDetails[ic][2])) -
			log(ModelPrev[ic] / (1.0 - ModelPrev[ic])), 2.0) /
			(RandomError[ic] + VarRandEffects));
	}

	return MSMlogL;
}

double CalcMortLikelihoodA()
{
	int ia, ig, ii, iy, UpperAge;
	double SquaredDif[20][8][2], AIDSmortPropn[20][8][2];
	double FirstDeriv, SecondDeriv, ErrorVar, MortLogL, Temp1;

	// First calculate ModelDeathsA, after adjusting for completeness & interpolation
	UpperAge = (AgeLimitMortCalib/5) - 3;
	if(UpperAge>16){UpperAge = 16;}
	for(iy=0; iy<20; iy++){
		for(ia=1; ia<UpperAge; ia++){
			for(ig=0; ig<2; ig++){
				ModelDeathsA[ia][iy][ig] = 0.5 * (AdultMortBy5yr[iy+11][ia][ig] +
					AdultMortBy5yr[iy+12][ia][ig]) * AdultCompleteness[ia][iy][ig];
				if (ProvModel == 1){ ModelDeathsA[ia][iy][ig] *= AdjustedCompleteness[iy][ig]; }
			}
		}
	}

	// Calculate variance of model error term and % of deaths due to AIDS
	ErrorVar = 0.0;
	for(iy=0; iy<20; iy++){
		for(ia=1; ia<UpperAge; ia++){
			for(ig=0; ig<2; ig++){
				AIDSmortPropn[iy][ia-1][ig] = 1.0 - (NonAIDSmortBy5yr[iy + 11][ia][ig] +
					NonAIDSmortBy5yr[iy + 12][ia][ig]) / (AdultMortBy5yr[iy + 11][ia][ig] +
					AdultMortBy5yr[iy + 12][ia][ig]);
				if (AIDSmortPropn[iy][ia-1][ig] < 0.0001){ AIDSmortPropn[iy][ia-1][ig] = 0.0001; }
				if (AIDSmortPropn[iy][ia-1][ig] > 0.9999){ AIDSmortPropn[iy][ia-1][ig] = 0.9999; }
				SquaredDif[iy][ia-1][ig] = pow(log(ModelDeathsA[ia][iy][ig]) -
					log(RecordedDeathsA[ia][iy][ig]), 2.0);
				ErrorVar += SquaredDif[iy][ia-1][ig];
			}
		}
	}
	ErrorVar = ErrorVar/(20.0 * 2.0 * (UpperAge - 1.0));

	// Newton-Raphson step to improve variance estimate
	/*for (ii = 0; ii < 3; ii++){
		FirstDeriv = 0.0;
		SecondDeriv = 0.0;
		for (iy = 0; iy < 20; iy++){
			for (ia = 1; ia < UpperAge; ia++){
				for (ig = 0; ig < 2; ig++){
					Temp1 = pow(SElogNonHIVmort * (1.0 - AIDSmortPropn[iy][ia-1][ig]), 2.0) + ErrorVar *
						pow(AIDSmortPropn[iy][ia-1][ig], 2.0);
					FirstDeriv += 0.5 * pow(AIDSmortPropn[iy][ia-1][ig], 2.0) * (SquaredDif[iy][ia-1][ig] /
						pow(Temp1, 2.0) - 1.0 / Temp1);
					SecondDeriv += pow(AIDSmortPropn[iy][ia-1][ig], 2.0) * (-SquaredDif[iy][ia-1][ig] /
						pow(Temp1, 3.0) + 0.5 / pow(Temp1, 2.0));
				}
			}
		}
		ErrorVar = ErrorVar - FirstDeriv / SecondDeriv;
	}
	if (ErrorVar < 0.0){ ErrorVar = 0.0; }*/
	ErrorVar = 0.09;

	// Calculate likelihood
	MortLogL = 0.0;
	for(iy=0; iy<20; iy++){
		for(ia=1; ia<UpperAge; ia++){
			for(ig=0; ig<2; ig++){
				Temp1 = pow(SElogNonHIVmort * (1.0 - AIDSmortPropn[iy][ia-1][ig]), 2.0) + ErrorVar *
					pow(AIDSmortPropn[iy][ia-1][ig], 2.0);
				MortLogL += -0.5 * (log(2.0 * 3.141592654 * Temp1) + SquaredDif[iy][ia-1][ig] / Temp1);
			}
		}
	}

	// Store outputs
	if(FixedUncertainty==1){
		ErrorVariance.out[CurrSim-1][1] = ErrorVar;}

	return MortLogL;
}

double CalcMortLikelihoodP()
{
	int ia, ig, ii, iy;
	double complete0, complete1, temp1, ErrorVar, MortLogL;

	// First calculate ModelDeathsP, after interpolation
	for (iy = 0; iy<20; iy++){
		for (ia = 0; ia < 4; ia++){
			for (ig = 0; ig < 2; ig++){
				ModelDeathsP[ia][iy][ig] = 0.5 * (ChildMortBy5yr[iy + 11][ia][ig] +
					ChildMortBy5yr[iy + 12][ia][ig]);
			}
		}
	}

	// Next estimate completeness
	/*temp1 = 0;
	for (iy = 0; iy < 7; iy++){
		temp1 += pow(1.0 - pow(1.0 * iy / 7.0, 0.75), 2.0); }
	for (ia = 0; ia < 4; ia++){
		for (ig = 0; ig < 2; ig++){
			// Calculate completeness over 2004-2014 period
			// ML approach
			//complete1 = 1.0;
			//for (iy = 7; iy < 20; iy++){
			//	complete1 *= RecordedDeathsP[ia][iy][ig] / ModelDeathsP[ia][iy][ig];}
			//complete1 = pow(complete1, 1.0 / 13.0);
			//if (complete1 > 1.0){ complete1 = 1.0; }
			// Approach based on completion estimates in Johnson et al (2015)
			if (ia == 0){ complete1 = 0.882; }
			if (ia == 1){ complete1 = 0.743; }
			if (ia == 2){ complete1 = 0.812; }
			if (ia == 3){
				if (ig == 0){ complete1 = 0.849; }
				if (ig == 1){ complete1 = 0.877; }
			}
			for (iy = 7; iy < 20; iy++){ PaedCompleteness[ia][iy][ig] = complete1; }
			// Approach based on completion estimates in Dorrington et al (2018)
			//if (ia == 0){ complete1 = 0.82; }
			//if (ia == 1){ complete1 = 0.61; }
			//if (ia == 2){ complete1 = 0.73; }
			//if (ia == 3){ complete1 = 0.85; }
			//for (iy = 7; iy < 20; iy++){ PaedCompleteness[ia][iy][ig] = complete1; }
			// Calculate completeness in 1997
			complete0 = 0.0;
			for (iy = 0; iy < 7; iy++){
				complete0 += (log(complete1) * pow(1.0 * iy / 7.0, 0.75) - log(RecordedDeathsP[ia][iy][ig] /
					ModelDeathsP[ia][iy][ig])) * (1.0 - pow(1.0 * iy / 7.0, 0.75));
			}
			complete0 = exp(-complete0 / temp1);
			if (complete0 > complete1){ complete0 = complete1; }
			for (iy = 0; iy < 7; iy++){
				PaedCompleteness[ia][iy][ig] = complete0 * pow(complete0/complete1,
					-pow(1.0 * iy / 7.0, 0.75));
			}
			// Store completeness values
			if (FixedUncertainty == 1){
				CompletenessPaed.out[CurrSim - 1][ig * 8 + ia * 2] = complete0;
				CompletenessPaed.out[CurrSim - 1][ig * 8 + ia * 2 + 1] = complete1;
			}
		}
	}*/

	// Calculate variance of model error term
	ErrorVar = 0.0;
	for (iy = 0; iy<20; iy++){
		for (ia = 0; ia<4; ia++){
			for (ig = 0; ig<2; ig++){
				temp1 = log(ModelDeathsP[ia][iy][ig] * PaedCompleteness[ia][iy][ig]) - log(RecordedDeathsP[ia][iy][ig]);
				ErrorVar += pow(temp1, 2.0);
			}
		}
	}

	ErrorVar = ErrorVar / (20.0 * 2.0 * 4.0);

	// Calculate likelihood
	MortLogL = 0.0;
	for (iy = 0; iy<20; iy++){
		for (ia = 0; ia<4; ia++){
			for (ig = 0; ig<2; ig++){
				temp1 = log(ModelDeathsP[ia][iy][ig] * PaedCompleteness[ia][iy][ig]) - log(RecordedDeathsP[ia][iy][ig]);
				MortLogL += -0.5 * (log(2.0 * 3.141592654 * ErrorVar) +
					pow(temp1, 2.0) / ErrorVar);
			}
		}
	}

	// Store outputs
	if (FixedUncertainty == 1){
		ErrorVariance.out[CurrSim - 1][2] = ErrorVar;
	}

	return MortLogL;
}

double CalcHCTlogL()
{
	int ia, ig, ih;
	double HCTlogL, SElogit, HCTbias[2], ModelError;

	// Calculate HCT bias
	/*HCTbias[0] = 0.0;
	HCTbias[1] = 0.0;
	HCTlogL = 0.0;
	for(ia=0; ia<5; ia++){
		for(ig=0; ig<2; ig++){
			for(ih=0; ih<2; ih++){
				HCTbias[ih] += log(EverTested05[ia][ig][ih]/(1.0 - EverTested05[ia][ig][ih])) -
					log(ModelTested05[ia][ig][ih]/(1.0 - ModelTested05[ia][ig][ih]));
				HCTbias[ih] += log(EverTested08[ia][ig][ih] / (1.0 - EverTested08[ia][ig][ih])) -
					log(ModelTested08[ia][ig][ih]/(1.0 - ModelTested08[ia][ig][ih]));
				HCTbias[ih] += log(EverTested12[ia][ig][ih] / (1.0 - EverTested12[ia][ig][ih])) -
					log(ModelTested12[ia][ig][ih]/(1.0 - ModelTested12[ia][ig][ih]));
				HCTbias[ih] += log(EverTested17[ia][ig][ih] / (1.0 - EverTested17[ia][ig][ih])) -
					log(ModelTested17[ia][ig][ih] / (1.0 - ModelTested17[ia][ig][ih]));
				if (ia < 4){
					HCTbias[ih] += log(EverTested16[ia][ig][ih] / (1.0 - EverTested16[ia][ig][ih])) -
						log(ModelTested16[ia][ig][ih] / (1.0 - ModelTested16[ia][ig][ih]));
				}
			}
		}
	}
	HCTbias[0] = HCTbias[0] / 48.0;
	HCTbias[1] = HCTbias[1] / 48.0;*/

	// Recalculate ModelTested, after adjusting for bias
	for(ia=0; ia<5; ia++){
		for(ig=0; ig<2; ig++){
			for(ih=0; ih<2; ih++){
				/*ModelTested05[ia][ig][ih] = 1.0/(1.0 - (1.0 - (1.0/ModelTested05[ia][ig][ih])) *
					exp(-HCTbias[ih]));
				ModelTested08[ia][ig][ih] = 1.0/(1.0 - (1.0 - (1.0/ModelTested08[ia][ig][ih])) *
					exp(-HCTbias[ih]));
				ModelTested12[ia][ig][ih] = 1.0/(1.0 - (1.0 - (1.0/ModelTested12[ia][ig][ih])) *
					exp(-HCTbias[ih]));
				ModelTested17[ia][ig][ih] = 1.0 / (1.0 - (1.0 - (1.0 / ModelTested17[ia][ig][ih])) *
					exp(-HCTbias[ih]));
				if (ia < 4){
					ModelTested16[ia][ig][ih] = 1.0 / (1.0 - (1.0 - (1.0 / ModelTested16[ia][ig][ih])) *
						exp(-HCTbias[ih]));
				}*/
				ModelTested05[ia][ig][ih] = ModelTested05[ia][ig][ih] * SeTestingHistory[ih] +
					(1.0 - ModelTested05[ia][ig][ih]) * (1.0 - SpTestingHistory);
				ModelTested08[ia][ig][ih] = ModelTested08[ia][ig][ih] * SeTestingHistory[ih] +
					(1.0 - ModelTested08[ia][ig][ih]) * (1.0 - SpTestingHistory);
				ModelTested12[ia][ig][ih] = ModelTested12[ia][ig][ih] * SeTestingHistory[ih] +
					(1.0 - ModelTested12[ia][ig][ih]) * (1.0 - SpTestingHistory);
				ModelTested17[ia][ig][ih] = ModelTested17[ia][ig][ih] * SeTestingHistory[ih] +
					(1.0 - ModelTested17[ia][ig][ih]) * (1.0 - SpTestingHistory);
				if (ia < 4){
					ModelTested16[ia][ig][ih] = ModelTested16[ia][ig][ih] * SeTestingHistory[ih] +
						(1.0 - ModelTested16[ia][ig][ih]) * (1.0 - SpTestingHistory);
				}
			}
		}
	}
	// Note: no adjustment to the PrevTested09 model outputs at this stage.

	//Calculate ModelError
	ModelError = 0.0;
	for(ia=0; ia<5; ia++){
		for(ig=0; ig<2; ig++){
			for(ih=0; ih<2; ih++){
				SElogit = SEtested05[ia][ig][ih]/(EverTested05[ia][ig][ih] * (1.0 - EverTested05[ia][ig][ih]));
				ModelError += pow(log(ModelTested05[ia][ig][ih]/(1.0 - ModelTested05[ia][ig][ih])) -
					log(EverTested05[ia][ig][ih]/(1.0 - EverTested05[ia][ig][ih])), 2.0) - pow(SElogit, 2.0);
				SElogit = SEtested08[ia][ig][ih]/(EverTested08[ia][ig][ih] * (1.0 - EverTested08[ia][ig][ih]));
				ModelError += pow(log(ModelTested08[ia][ig][ih]/(1.0 - ModelTested08[ia][ig][ih])) -
					log(EverTested08[ia][ig][ih]/(1.0 - EverTested08[ia][ig][ih])), 2.0) - pow(SElogit, 2.0);
				SElogit = SEtested12[ia][ig][ih]/(EverTested12[ia][ig][ih] * (1.0 - EverTested12[ia][ig][ih]));
				ModelError += pow(log(ModelTested12[ia][ig][ih]/(1.0 - ModelTested12[ia][ig][ih])) -
					log(EverTested12[ia][ig][ih]/(1.0 - EverTested12[ia][ig][ih])), 2.0) - pow(SElogit, 2.0);
				SElogit = SEtested17[ia][ig][ih] / (EverTested17[ia][ig][ih] * (1.0 - EverTested17[ia][ig][ih]));
				ModelError += pow(log(ModelTested17[ia][ig][ih] / (1.0 - ModelTested17[ia][ig][ih])) -
					log(EverTested17[ia][ig][ih] / (1.0 - EverTested17[ia][ig][ih])), 2.0) - pow(SElogit, 2.0);
				if (ia < 4){
					SElogit = SEtested16[ia][ig][ih] / (EverTested16[ia][ig][ih] * (1.0 - EverTested16[ia][ig][ih]));
					ModelError += pow(log(ModelTested16[ia][ig][ih] / (1.0 - ModelTested16[ia][ig][ih])) -
						log(EverTested16[ia][ig][ih] / (1.0 - EverTested16[ia][ig][ih])), 2.0) - pow(SElogit, 2.0);
				}
			}
		}
	}
	ModelError = ModelError/96.0;
	if(ModelError<0.0){
		ModelError = 0.0;}

	// Calculate likelihood
	HCTlogL = 0.0;
	for(ia=0; ia<5; ia++){
		for(ig=0; ig<2; ig++){
			for(ih=0; ih<2; ih++){
				SElogit = pow(ModelError + pow(SEtested05[ia][ig][ih]/(EverTested05[ia][ig][ih] *
					(1.0 - EverTested05[ia][ig][ih])), 2.0), 0.5);
				HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogit, 2.0)) +
					pow((log(ModelTested05[ia][ig][ih]/(1.0 - ModelTested05[ia][ig][ih])) -
					log(EverTested05[ia][ig][ih]/(1.0 - EverTested05[ia][ig][ih])))/SElogit, 2.0));
				SElogit = pow(ModelError + pow(SEtested08[ia][ig][ih]/(EverTested08[ia][ig][ih] *
					(1.0 - EverTested08[ia][ig][ih])), 2.0), 0.5);
				HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogit, 2.0)) +
					pow((log(ModelTested08[ia][ig][ih]/(1.0 - ModelTested08[ia][ig][ih])) -
					log(EverTested08[ia][ig][ih]/(1.0 - EverTested08[ia][ig][ih])))/SElogit, 2.0));
				SElogit = pow(ModelError + pow(SEtested12[ia][ig][ih]/(EverTested12[ia][ig][ih] *
					(1.0 - EverTested12[ia][ig][ih])), 2.0), 0.5);
				HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogit, 2.0)) +
					pow((log(ModelTested12[ia][ig][ih]/(1.0 - ModelTested12[ia][ig][ih])) -
					log(EverTested12[ia][ig][ih]/(1.0 - EverTested12[ia][ig][ih])))/SElogit, 2.0));
				SElogit = pow(ModelError + pow(SEtested17[ia][ig][ih] / (EverTested17[ia][ig][ih] *
					(1.0 - EverTested17[ia][ig][ih])), 2.0), 0.5);
				HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogit, 2.0)) +
					pow((log(ModelTested17[ia][ig][ih] / (1.0 - ModelTested17[ia][ig][ih])) -
					log(EverTested17[ia][ig][ih] / (1.0 - EverTested17[ia][ig][ih]))) / SElogit, 2.0));
				if (ia < 4){
					SElogit = pow(ModelError + pow(SEtested16[ia][ig][ih] / (EverTested16[ia][ig][ih] *
						(1.0 - EverTested16[ia][ig][ih])), 2.0), 0.5);
					HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(SElogit, 2.0)) +
						pow((log(ModelTested16[ia][ig][ih] / (1.0 - ModelTested16[ia][ig][ih])) -
						log(EverTested16[ia][ig][ih] / (1.0 - EverTested16[ia][ig][ih]))) / SElogit, 2.0));
				}
			}
		}
	}

	// Store outputs
	if(FixedUncertainty==1){
		//TestingBias.out[CurrSim - 1][0] = HCTbias[0];
		//TestingBias.out[CurrSim - 1][1] = HCTbias[1];
		for(ia=0; ia<5; ia++){
			for(ig=0; ig<2; ig++){
				for(ih=0; ih<2; ih++){
					PrevTested05.out[CurrSim-1][ia+10*ig+5*ih] = ModelTested05[ia][ig][ih];
					PrevTested08.out[CurrSim-1][ia+10*ig+5*ih] = ModelTested08[ia][ig][ih];
					PrevTested12.out[CurrSim-1][ia+10*ig+5*ih] = ModelTested12[ia][ig][ih];
					PrevTested17.out[CurrSim - 1][ia + 10 * ig + 5 * ih] = ModelTested17[ia][ig][ih];
					if (ia < 4){
						PrevTested16.out[CurrSim - 1][ia + 8 * ig + 4 * ih] = ModelTested16[ia][ig][ih];
					}
				}
			}
		}
	}

	return HCTlogL;
}

double CalcHCTprevLogL()
{
	int iy;
	double HCTlogL, ModelError, VarLogit[13];

	// Calculate ModelError and VarLogit
	ModelError = 0.0;
	for (iy = 0; iy < 13; iy++){
		VarLogit[iy] = pow(SEprevHCT[iy] / (ObservedPrevHCT[iy] * (1.0 - ObservedPrevHCT[iy])), 2.0);
		//ModelError += pow(log(ModelPrevHCT[iy] / (1.0 - ModelPrevHCT[iy])) -
		//	log(ObservedPrevHCT[iy] / (1.0 - ObservedPrevHCT[iy])), 2.0) - VarLogit[iy];
	}
	if (ModelError < 0.0){ ModelError = 0.0; }

	// Calculate likelihood
	HCTlogL = 0.0;
	for (iy = 0; iy < 13; iy++){
		HCTlogL += -0.5 * (log(2.0 * 3.141592654 * (VarLogit[iy] + ModelError)) +
			pow(log(ModelPrevHCT[iy] / (1.0 - ModelPrevHCT[iy])) - log(ObservedPrevHCT[iy]/
			(1.0 - ObservedPrevHCT[iy])), 2.0) / (VarLogit[iy] + ModelError));
	}

	return HCTlogL;
}

double CalcHCTprevPlogL()
{
	int iy;
	double HCTlogL, VarLogit[6];

	// Calculate ModelError and VarLogit
	for (iy = 0; iy < 6; iy++){
		VarLogit[iy] = pow(SEprevHCT_P[iy] / (ObservedPrevHCT_P[iy] * (1.0 - ObservedPrevHCT_P[iy])), 2.0);
	}

	// Calculate likelihood
	HCTlogL = 0.0;
	for (iy = 0; iy < 6; iy++){
		HCTlogL += -0.5 * (log(2.0 * 3.141592654 * VarLogit[iy]) +
			pow(log(ModelPrevHCT_P[iy] / (1.0 - ModelPrevHCT_P[iy])) - log(ObservedPrevHCT_P[iy] /
			(1.0 - ObservedPrevHCT_P[iy])), 2.0) / VarLogit[iy]);
	}

	return HCTlogL;
}

double CalcRecHCT_PlogL()
{
	int iy;
	double HCTlogL, ModelTot[5], ModelError;

	// Calculate ModelError and VarLogit
	for (iy = 0; iy < 5; iy++){
		ModelTot[iy] = TotalHIVtestsU15.out[CurrSim - 1][30+iy];
	}
	//ModelError = 0.0;
	//for (iy = 0; iy < 3; iy++){
	//	ModelError += pow(log(ModelTot[iy]) - RecordedHCT_P[iy], 2.0);}
	//ModelError = ModelError / 3.0;

	// Calculate likelihood
	HCTlogL = 0.0;
	for (iy = 0; iy < 5; iy++){
		HCTlogL += -0.5 * (log(2.0 * 3.141592654 * pow(RecordedHCT_P_CoV[iy], 2.0)) +
			pow(log(ModelTot[iy]) - log(RecordedHCT_P[iy]), 2.0) / pow(RecordedHCT_P_CoV[iy], 2.0));
		//HCTlogL += -0.5 * (log(2.0 * 3.141592654 * ModelError) +
		//	pow(log(ModelTot[iy]) - log(RecordedHCT_P[iy]), 2.0) / ModelError);
	}

	return HCTlogL;
}

double CalcAIDStrendLogL()
{
	int iy;
	double ReportingBias, ErrorVar, AIDSlogL, Temp1, Temp2;

	// Calculate ReportingBias
	ReportingBias = 0.0;
	for(iy=0; iy<5; iy++){
		ReportingBias += log(AIDScasesByYr[iy]/OIsDiagnosedByYr[iy]);}
	ReportingBias = ReportingBias/5.0;
	if(ReportingBias>0.0){
		ReportingBias = 0.0;}

	// Calculate ErrorVar
	ErrorVar = 0.0;
	for(iy=0; iy<5; iy++){
		ErrorVar += pow(log(AIDScasesByYr[iy]) - log(OIsDiagnosedByYr[iy]) -
			ReportingBias, 2.0) - 1.0/AIDScasesByYr[iy];
	}
	ErrorVar = ErrorVar/5.0;
	if(ErrorVar < 0.0){
		ErrorVar = 0.0;}

	// Calculate log likelihood
	AIDSlogL = 0.0;
	for(iy=0; iy<5; iy++){
		Temp1 = log(AIDScasesByYr[iy]) - log(OIsDiagnosedByYr[iy]) - ReportingBias;
		Temp2 = ErrorVar + 1.0/AIDScasesByYr[iy];
		AIDSlogL += -0.5 * (log(2.0 * 3.141592654 * Temp2) + pow(Temp1, 2.0)/Temp2);
	}

	// Store outputs
	if(FixedUncertainty==1){
		for(iy=0; iy<5; iy++){
			NewAIDSdiagTrend.out[CurrSim-1][iy] = OIsDiagnosedByYr[iy] *
				exp(ReportingBias);
		}
	}

	return AIDSlogL;
}

double CalcAIDSageLogL()
{
	// The structure of this function is very similar to that of CalcAIDStrendLogL().

	int ia, ig;
	double ReportingBias, ErrorVar, AIDSlogL, Temp1, Temp2;

	// Calculate ReportingBias
	ReportingBias = 0.0;
	for(ia=0; ia<10; ia++){
		for(ig=0; ig<2; ig++){
			ReportingBias += log(AIDScasesProfile[ia][ig]/OIsDiagnosedProfile[ia][ig]);
		}
	}
	ReportingBias = ReportingBias/(10.0 * 2.0);
	if(ReportingBias>0.0){
		ReportingBias = 0.0;}

	// Calculate ErrorVar
	ErrorVar = 0.0;
	for(ia=0; ia<10; ia++){
		for(ig=0; ig<2; ig++){
			ErrorVar += pow(log(AIDScasesProfile[ia][ig]/OIsDiagnosedProfile[ia][ig]) -
				ReportingBias, 2.0) - 1.0/AIDScasesProfile[ia][ig];
		}
	}
	ErrorVar = ErrorVar/(10.0 * 2.0);
	if(ErrorVar < 0.0){
		ErrorVar = 0.0;}

	// Calculate log likelihood
	AIDSlogL = 0.0;
	for(ia=0; ia<10; ia++){
		for(ig=0; ig<2; ig++){
			Temp1 = log(AIDScasesProfile[ia][ig]/OIsDiagnosedProfile[ia][ig]) -
				ReportingBias;
			Temp2 = ErrorVar + 1.0/AIDScasesProfile[ia][ig];
			AIDSlogL += -0.5 * (log(2.0 * 3.141592654 * Temp2) + pow(Temp1, 2.0)/Temp2);
		}
	}

	// Store outputs
	if(FixedUncertainty==1){
		for(ia=0; ia<10; ia++){
			NewAIDSdiagAge.out[CurrSim-1][ia] = OIsDiagnosedProfile[ia][0] *
				exp(ReportingBias);
			NewAIDSdiagAge.out[CurrSim-1][ia+10] = OIsDiagnosedProfile[ia][1] *
				exp(ReportingBias);
		}
	}

	return AIDSlogL;
}

double CalcARTtotalLogL()
{
	int ii, TotDataPoints;
	double ARTlogL, VarEst, modelest[ARTdataPoints], modelestP[ARTdataPointsP];
	double CumFraction, projdate, datapoint, public_frac, public_abs, Temp1;

	// Estimate variance
	VarEst = 0.0;
	TotDataPoints = 0;
	if (CalibARTtotals == 1){
		TotDataPoints += ARTdataPoints;
		for (ii = 0; ii < ARTdataPoints; ii++){
			projdate = 1.0 * ARTtotals[ii][0] + 1.0 * ARTtotals[ii][1] / 12.0;
			if (projdate <= LastDateCum){ CumFraction = 1.0; }
			else{
				CumFraction = pow(AnnSwitchCumToCurr, projdate - LastDateCum); }
			datapoint = ARTtotals[ii][2] + ARTtotals[ii][3];
			if (ARTtotals[ii][2] == 0.0){ modelest[ii] = ARTmodelled[ii][0]; }
			else{
				if (ARTtotals[ii][3] < ARTmodelled[ii][0]){
					public_frac = 1.0 - ARTtotals[ii][3] / ARTmodelled[ii][0];
					public_abs = ARTmodelled[ii][0] - ARTtotals[ii][3];
					modelest[ii] = CumFraction* ARTmodelled[ii][1] * public_frac +
						(1.0 - CumFraction) * public_abs + ARTtotals[ii][3];
				}
				else{ modelest[ii] = ARTmodelled[ii][0]; }
			}
			VarEst += pow(log(modelest[ii]) - log(datapoint), 2.0);
		}
	}
	if (CalibARTtotalsP == 1){
		TotDataPoints += ARTdataPointsP;
		for (ii = 0; ii < ARTdataPointsP; ii++){
			projdate = 1.0 * ARTtotalsP[ii][0] + 1.0 * ARTtotalsP[ii][1] / 12.0;
			if (projdate <= LastDateCum){ CumFraction = 1.0; }
			else{
				CumFraction = pow(AnnSwitchCumToCurr, projdate - LastDateCum);}
			datapoint = ARTtotalsP[ii][2] + ARTtotalsP[ii][3];
			if (ARTtotalsP[ii][2] == 0.0){ modelestP[ii] = ARTmodelledP[ii][0]; }
			else{
				if (ARTtotalsP[ii][3] < ARTmodelledP[ii][0]){
					public_frac = 1.0 - ARTtotalsP[ii][3] / ARTmodelledP[ii][0];
					public_abs = ARTmodelledP[ii][0] - ARTtotalsP[ii][3];
					modelestP[ii] = CumFraction* ARTmodelledP[ii][1] * public_frac +
						(1.0 - CumFraction) * public_abs + ARTtotalsP[ii][3];
				}
				else{ modelestP[ii] = ARTmodelledP[ii][0]; }
			}
			VarEst += pow(log(modelestP[ii]) - log(datapoint), 2.0);
		}
	}
	//VarEst = VarEst / TotDataPoints;
	VarEst = pow(0.100, 2.0);

	// Next calculate likelihood
	ARTlogL = 0.0;
	if (CalibARTtotals == 1){
		for (ii = 0; ii < ARTdataPoints; ii++){
			Temp1 = log(modelest[ii]) - log(ARTtotals[ii][2] + ARTtotals[ii][3]);
			if (ii < ARTdataPoints - 24){ ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst); }
			else{ ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / (VarEst/4.0)); }
			//ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst);
		}
	}
	if (CalibARTtotalsP == 1){
		for (ii = 0; ii < ARTdataPointsP; ii++){
			Temp1 = log(modelestP[ii]) - log(ARTtotalsP[ii][2] + ARTtotalsP[ii][3]);
			if (ii < ARTdataPointsP - 24){ ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst); }
			else{ ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / (VarEst/4.0)); }
			//ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst);
		}
	}
	//if (ARTerrorInd == 1){ ARTlogL = ARTlogL - 100000.0; }

	// Store outputs
	ARTerror.out[CurrSim - 1][0] = ARTerrorInd;

	return ARTlogL;
}

double CalcMaleARTlogL()
{
	int ii, iy;
	double ARTlogL, Temp1, VarEst;

	ARTlogL = 0.0;
	for (ii = 0; ii < ARTdataPointsM; ii++){
		iy = ARTmale[ii][0] - StartYear;
		Temp1 = ARTmale[ii][1] - ARTmodelledM[ii];
		VarEst = pow(ARTmale[ii][2], 2.0);
		ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst);
	}

	return ARTlogL;
}

double CalcARTbyCD4logL()
{
	int is, iy, ig, EligStartYr[4][2];
	double ARTlogL, Temp1, Temp2, VarEst;
	double ModelCD4dbn[12][4][2]; // Comparable to RecordedARTstart

	// EligStartYr is the time (in years after 2007) in which people become ART-eligible, by CD4 and sex
	EligStartYr[3][0] = 0; // Eligibility at CD4 <200 since the start
	EligStartYr[3][1] = 0;
	EligStartYr[2][0] = 4; // Eligibility at CD4 200-349 since 2011
	EligStartYr[2][1] = 4;
	EligStartYr[1][1] = 6; // Eligibility at CD4 350+ starts in pregnant women in 2013
	EligStartYr[0][1] = 6;
	EligStartYr[1][0] = 7; // Eligibility at CD4 350-499 in men since 2014
	EligStartYr[0][0] = 9; // Eligibility at CD4 500+ in men since 2016

	for (iy = 0; iy < 12; iy++){
		ModelCD4dbn[iy][0][1] = NewARTover500F.out[CurrSim - 1][iy + 22];
		ModelCD4dbn[iy][1][1] = NewART350to499F.out[CurrSim - 1][iy + 22];
		ModelCD4dbn[iy][2][1] = NewART200to349F.out[CurrSim - 1][iy + 22];
		ModelCD4dbn[iy][3][1] = NewARTunder200F.out[CurrSim - 1][iy + 22];
		ModelCD4dbn[iy][0][0] = NewARTover500.out[CurrSim - 1][iy + 22] - ModelCD4dbn[iy][0][1];
		ModelCD4dbn[iy][1][0] = NewART350to499.out[CurrSim - 1][iy + 22] - ModelCD4dbn[iy][1][1];
		ModelCD4dbn[iy][2][0] = NewART200to349.out[CurrSim - 1][iy + 22] - ModelCD4dbn[iy][2][1];
		ModelCD4dbn[iy][3][0] = NewARTunder200.out[CurrSim - 1][iy + 22] - ModelCD4dbn[iy][3][1];
	}

	Temp1 = 0.0;
	Temp2 = 0.0;
	for (iy = 0; iy < 12; iy++){
		for (is = 0; is < 4; is++){
			for (ig = 0; ig < 2; ig++){
				if (EligStartYr[is][ig] <= iy){
					Temp1 += 1.00;
					Temp2 += pow(log(ModelCD4dbn[iy][is][ig]) - log(RecordedARTstart[iy][is][ig]), 2.0);
				}
			}
		}
	}
	VarEst = Temp2 / Temp1;

	ARTlogL = 0.0;
	for (iy = 0; iy < 12; iy++){
		for (is = 0; is < 4; is++){
			for (ig = 0; ig < 2; ig++){
				if (EligStartYr[is][ig] <= iy){
					Temp1 = log(ModelCD4dbn[iy][is][ig]) - log(RecordedARTstart[iy][is][ig]);
					ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(Temp1, 2.0) / VarEst);
				}
			}
		}
	}

	return ARTlogL;
}

double CalcAgeARTlogL()
{
	int iy, ia, ig;
	double ARTlogL, VarEst, temp;

	VarEst = pow(0.45, 2.0);

	// Calculate likelihood
	ARTlogL = 0.0;
	for (iy = 0; iy < 7; iy++){
		for (ia = 0; ia < 9; ia++){
			for (ig = 0; ig < 2; ig++){
				if (AgeDbnAdultsOnART[iy][ia][ig] > 0.0){
					temp = log(ModelAgeDbnAdultART[iy][ia][ig] / (1.0 - ModelAgeDbnAdultART[iy][ia][ig])) -
						log(AgeDbnAdultsOnART[iy][ia][ig] / (1.0 - AgeDbnAdultsOnART[iy][ia][ig]));
					ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(temp, 2.0) / VarEst);
				}
			}
		}
	}

	if (FixedUncertainty == 1){
		for (iy = 0; iy < 7; iy++){
			temp = 0.0;
			for (ia = 0; ia < 9; ia++){
				temp += ModelAgeDbnAdultART[iy][ia][0];
				AgeDbnOnART_M.out[CurrSim - 1][iy * 10 + ia + 1] = ModelAgeDbnAdultART[iy][ia][0];
			}
			AgeDbnOnART_M.out[CurrSim - 1][iy * 10] = 1.0 - temp; // for ages 15-19
			temp = 0.0;
			for (ia = 0; ia < 9; ia++){
				temp += ModelAgeDbnAdultART[iy][ia][1];
				AgeDbnOnART_F.out[CurrSim - 1][iy * 10 + ia + 1] = ModelAgeDbnAdultART[iy][ia][1];
			}
			AgeDbnOnART_F.out[CurrSim - 1][iy * 10] = 1.0 - temp; // for ages 15-19
		}
	}

	return ARTlogL;
}

double CalcAgeART_PlogL()
{
	int iy, ii;
	double ARTlogL, VarEst, modelest[10][2], temp;

	// Get modelest
	for (iy = 0; iy < 10; iy++){
		temp = StartingART0.out[CurrSim - 1][iy + 19] + StartingART1.out[CurrSim - 1][iy + 19] +
			StartingART2to4.out[CurrSim - 1][iy + 19] + StartingART5to14.out[CurrSim - 1][iy + 19];
		modelest[iy][0] = StartingART0.out[CurrSim - 1][iy + 19] / temp;
		modelest[iy][1] = (StartingART1.out[CurrSim - 1][iy + 19] +
			StartingART2to4.out[CurrSim - 1][iy + 19]) / temp;
	}

	// Estimate variance
	VarEst = 0.0;
	for (iy = 0; iy < 10; iy++){
		for (ii = 0; ii < 2; ii++){
			VarEst += pow(log(modelest[iy][ii] / (1.0 - modelest[iy][ii])) -
				log(AgeDbnKidsStartingART[iy][ii] / (1.0 - AgeDbnKidsStartingART[iy][ii])), 2.0);
		}
	}
	VarEst = VarEst / 20.0;

	// Next calculate likelihood
	ARTlogL = 0.0;
	for (iy = 0; iy < 10; iy++){
		for (ii = 0; ii < 2; ii++){
			temp = log(modelest[iy][ii] / (1.0 - modelest[iy][ii])) -
				log(AgeDbnKidsStartingART[iy][ii] / (1.0 - AgeDbnKidsStartingART[iy][ii]));
			ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(temp, 2.0) / VarEst);
		}
	}

	return ARTlogL;
}

double CalcAgeART_P2logL()
{
	int iy, ii;
	double ARTlogL, modelest[8][2], VarEst, temp;

	// Get modelest
	for (iy = 0; iy < 8; iy++){
		modelest[iy][0] = PaedARTpropn0to4.out[CurrSim - 1][iy + 26];
		modelest[iy][1] = PaedARTpropn5to9.out[CurrSim - 1][iy + 26];
	}

	// Next calculate likelihood
	ARTlogL = 0.0;
	for (iy = 0; iy < 8; iy++){
		for (ii = 0; ii < 2; ii++){
			VarEst = pow(modelest[iy][ii] * 0.05, 2.0);
			temp = modelest[iy][ii] - AgeDbnKidsOnART[iy][ii];
			ARTlogL += -0.5 * (log(2.0 * 3.141592654 * VarEst) + pow(temp, 2.0) / VarEst);
		}
	}

	return ARTlogL;
}

double CalcChildPIPlogL()
{
	int ia, iy;
	double TempLogL, StdErrorDiag[13][2], StdErrorART[13][2], temp, ErrorVar;
	double AdjDeaths[3][2]; // Deaths in HIV-neg/undiagnosed, diagnosed untreated and treated, by age

	// Calculate the bias in the ART deaths
	if (InclPriors[107][0] == 0){
		RRdiagDeathsPIP[1] = 1.0;
		/*for (iy = 0; iy < 13; iy++){
			RRdiagDeathsPIP[1] *= (ARTdeaths1to4.out[CurrSim - 1][iy + 20] / (DiagDeaths1to4.out[CurrSim - 1][iy + 20] -
				ARTdeaths1to4.out[CurrSim - 1][iy + 20])) / (ChildPIPdiag[iy][0] / (1.0 - ChildPIPdiag[iy][0]));
			RRdiagDeathsPIP[1] *= (ARTdeaths5to9.out[CurrSim - 1][iy + 20] / (DiagDeaths5to9.out[CurrSim - 1][iy + 20] -
				ARTdeaths5to9.out[CurrSim - 1][iy + 20])) / (ChildPIPdiag[iy][1] / (1.0 - ChildPIPdiag[iy][1]));
		}
		RRdiagDeathsPIP[1] = pow(RRdiagDeathsPIP[1], 1.0 / (13.0 * 2.0));
		if (RRdiagDeathsPIP[1] > 1.0){ RRdiagDeathsPIP[1] = 1.0; }*/
	}
	if (FixedUncertainty == 1){
		OutRRdiagDeathsPIP.out[CurrSim - 1][0] = RRdiagDeathsPIP[0];
		OutRRdiagDeathsPIP.out[CurrSim - 1][1] = RRdiagDeathsPIP[1];
	}

	// Calculate modelled fractions of deaths diagnosed and on ART
	for (iy = 0; iy < 13; iy++){
		for (ia = 0; ia < 2; ia++){
			AdjDeaths[0][ia] = ChildMortBy5yr[iy + 20][ia+1][0] + ChildMortBy5yr[iy + 20][ia+1][1] -
				ChildAIDSmortDiag[iy + 20][ia];
			AdjDeaths[1][ia] = (ChildAIDSmortDiag[iy + 20][ia] - ChildAIDSmortART[iy + 20][ia]) /
				RRdiagDeathsPIP[0];
			AdjDeaths[2][ia] = ChildAIDSmortART[iy + 20][ia] / (RRdiagDeathsPIP[0] * RRdiagDeathsPIP[1]);
			ModelPIPdiag[iy][ia] = (AdjDeaths[1][ia] + AdjDeaths[2][ia]) / (AdjDeaths[0][ia] +
				AdjDeaths[1][ia] + AdjDeaths[2][ia]);
			ModelPIP_ART[iy][ia] = AdjDeaths[2][ia] / (AdjDeaths[1][ia] + AdjDeaths[2][ia]);
		}
	}

	// Calculate model error variance and standard deviation
	ErrorVar = 0.0;
	for (iy = 5; iy < 13; iy++){
		// Note we are starting from 2010 NOT 2005.
		for (ia = 0; ia < 2; ia++){
			StdErrorDiag[iy][ia] = pow(ChildPIPdiag[iy][ia] * (1.0 - ChildPIPdiag[iy][ia]) / ChildPIPdeaths[iy][ia], 0.5) /
				(ChildPIPdiag[iy][ia] * (1.0 - ChildPIPdiag[iy][ia]));
			StdErrorART[iy][ia] = pow(ChildPIP_ART[iy][ia] * (1.0 - ChildPIP_ART[iy][ia]) / (ChildPIPdeaths[iy][ia] *
				ChildPIPdiag[iy][ia]), 0.5) / (ChildPIP_ART[iy][ia] * (1.0 - ChildPIP_ART[iy][ia]));
			ErrorVar += pow(log(ChildPIPdiag[iy][ia] / (1.0 - ChildPIPdiag[iy][ia])) - log(ModelPIPdiag[iy][ia] /
				(1.0 - ModelPIPdiag[iy][ia])), 2.0) - pow(StdErrorDiag[iy][ia], 2.0);
			//ErrorVar += pow(log(ChildPIP_ART[iy][ia] / (1.0 - ChildPIP_ART[iy][ia])) - log(ModelPIP_ART[iy][ia] /
			//	(1.0 - ModelPIP_ART[iy][ia])), 2.0) - pow(StdErrorART[iy][ia], 2.0);
		}
	}
	ErrorVar = ErrorVar / (2.0 * 8.0);
	//ErrorVar = ErrorVar / (13.0 * 2.0);
	if (ErrorVar < 0.0){ ErrorVar = 0.0; }

	// Calculate likelihood for % diagnosed
	TempLogL = 0.0;
	for (iy = 5; iy < 13; iy++){
		for (ia = 0; ia < 2; ia++){
			temp = log(ChildPIPdiag[iy][ia] / (1.0 - ChildPIPdiag[iy][ia])) - log(ModelPIPdiag[iy][ia] /
				(1.0 - ModelPIPdiag[iy][ia]));
			TempLogL += -0.5 * (log(2.0 * 3.141592654 * (ErrorVar + pow(StdErrorDiag[iy][ia], 2.0))) + pow(temp, 2.0) /
				(ErrorVar + pow(StdErrorDiag[iy][ia], 2.0)));
		}
	}

	// Calculate likelihood for % on ART
	/*for (iy = 5; iy < 13; iy++){
		for (ia = 0; ia < 2; ia++){
			temp = log(ChildPIP_ART[iy][ia] / (1.0 - ChildPIP_ART[iy][ia])) - log(ModelPIP_ART[iy][ia] /
				(1.0 - ModelPIP_ART[iy][ia]));
			TempLogL += -0.5 * (log(2.0 * 3.141592654 * (ErrorVar + pow(StdErrorART[iy][ia], 2.0))) + pow(temp, 2.0) /
				(ErrorVar + pow(StdErrorART[iy][ia], 2.0)));
		}
	}*/

	return TempLogL;
}

double CalcARTcoverageLogL()
{
	int ig;
	double TempLogL, LogitDif, LogitSE;

	TempLogL = 0.0;

	// 2012 survey
	LogitDif = log(ModelCoverage[0][0] / (1.0 - ModelCoverage[0][0])) -
		log(ObsCoverage[0][0] / (1.0 - ObsCoverage[0][0]));
	LogitSE = SEcoverage[0][0] / (ObsCoverage[0][0] * (1.0 - ObsCoverage[0][0]));
	TempLogL += -0.5 * (log(2.0 * 3.141592654 * pow(LogitSE, 2.0)) + pow(LogitDif, 2.0) /
		pow(LogitSE, 2.0));
	LogitDif = log(ModelCoverage[1][0] / (1.0 - ModelCoverage[1][0])) -
		log(ObsCoverage[1][0] / (1.0 - ObsCoverage[1][0]));
	LogitSE = SEcoverage[1][0] / (ObsCoverage[1][0] * (1.0 - ObsCoverage[1][0]));
	TempLogL += -0.5 * (log(2.0 * 3.141592654 * pow(LogitSE, 2.0)) + pow(LogitDif, 2.0) /
		pow(LogitSE, 2.0));

	// 2017 survey
	LogitDif = log(ModelCoverage[0][1] / (1.0 - ModelCoverage[0][1])) -
		log(ObsCoverage[0][1] / (1.0 - ObsCoverage[0][1]));
	LogitSE = SEcoverage[0][1] / (ObsCoverage[0][1] * (1.0 - ObsCoverage[0][1]));
	TempLogL += -0.5 * (log(2.0 * 3.141592654 * pow(LogitSE, 2.0)) + pow(LogitDif, 2.0) /
		pow(LogitSE, 2.0));
	LogitDif = log(ModelCoverage[1][1] / (1.0 - ModelCoverage[1][1])) -
		log(ObsCoverage[1][1] / (1.0 - ObsCoverage[1][1]));
	LogitSE = SEcoverage[1][1] / (ObsCoverage[1][1] * (1.0 - ObsCoverage[1][1]));
	TempLogL += -0.5 * (log(2.0 * 3.141592654 * pow(LogitSE, 2.0)) + pow(LogitDif, 2.0) /
		pow(LogitSE, 2.0));

	return TempLogL;
}

double CalcMarriageLogL()
{
	int ig, ia, iy;
	double TempLogL, ErrorVar;

	TempLogL = 0.0;
	ErrorVar = 0.0015;
	for (ia = 0; ia < 15; ia++){
		for (ig = 0; ig < 2; ig++){
			for (iy = 0; iy < 4; iy++){
				TempLogL += -0.5 * (log(2.0 * 3.141592654 * ErrorVar) + pow(MarriageData[ia][ig][iy] -
					ModelMarried[ia][ig][iy], 2.0) / ErrorVar);
			}
		}
	}

	return TempLogL;
}

void GetCholesky1(double mat[MCMCdim][MCMCdim])
{
	// This method for calculating the Cholesky decomposition is from Healy M.J.R. (1986)
	// "Matrices for Statistics" Clarendon Press, Oxford. (See pp. 54-5)

	int ir, ic, ii;
	double sumprod;
	int PositiveDefinite = 1; // 0 if matrix is NOT positive definite (default
							  // assumption is that it is positive definite)

	for(ir=0; ir<MCMCdim; ir++){
		if(PositiveDefinite==1){
			for(ic=0; ic<MCMCdim; ic++){
				if(ir>ic){
					Cholesky1[ir][ic] = 0.0;}
				if(ir==ic){
					sumprod = 0.0;
					if(ir>0){
						for(ii=0; ii<ir; ii++){
							sumprod += pow(Cholesky1[ii][ic], 2.0);}
					}
					if(mat[ir][ic]>sumprod){
						Cholesky1[ir][ic] = sqrt(mat[ir][ic] - sumprod);}
					else{
						PositiveDefinite = 0;
						break;
					}
				}
				if(ir<ic){
					sumprod = 0.0;
					if(ir>0){
						for(ii=0; ii<ir; ii++){
							sumprod += Cholesky1[ii][ir] * Cholesky1[ii][ic];}
					}
					Cholesky1[ir][ic] = (mat[ir][ic] - sumprod)/Cholesky1[ir][ir];
				}
			}
		}
	}
	if(PositiveDefinite==0){
		// Cholesky decomposition fails
		for(ir=0; ir<MCMCdim; ir++){
			for(ic=0; ic<MCMCdim; ic++){
				if(ir==ic){
					Cholesky1[ir][ic] = sqrt(mat[ir][ic]);}
				else{
					Cholesky1[ir][ic] = 0.0;}
			}
		}
	}
}

void GetInverse1(double mat[MCMCdim][MCMCdim], double matinv[MCMCdim][MCMCdim])
{
	// The arguments are matrices A and B, where B is the inverse of A. It is assumed
	// that the elements of A are already known. We find the upper and lower triangular
	// matrices, U and L respectively, such that A = LU. Hence we wish to find matrix B
	// such that LUB = I. We set Y = UB and then solve for LY = I. Having obtained Y, we
	// then solve for UB = Y.

	// NB: The function I've written only works for symmetric matrices, since the
	// code for the Cholesky decomposition that I copied from Healy only works for
	// symmetric matrices. As long as we're only using the function to invert covariance
	// matrices, that shouldn't be a problem.

	int ir, ic, ij;
	double CholeskyT[MCMCdim][MCMCdim]; // Transpose of Cholesky, i.e. a lower triangular matrix
	double Ymatrix[MCMCdim][MCMCdim]; // Intermediate matrix, Y = UB
	double sumLY, sumUB;

	// Convert the Covariance matrix A into LU form
	GetCholesky1(mat);
	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			CholeskyT[ir][ic] = Cholesky1[ic][ir];}
	}

	// Calculate the elements of Ymatrix by forward-substitution
	for(ic=0; ic<MCMCdim; ic++){
		for(ir=0; ir<MCMCdim; ir++){
			if(ic>ir){Ymatrix[ir][ic] = 0.0;}
			if(ic==ir){Ymatrix[ir][ic] = 1.0/CholeskyT[ir][ic];}
			if(ic<ir){
				sumLY = 0.0;
				for(ij=0; ij<ir; ij++){
					sumLY += CholeskyT[ir][ij] * Ymatrix[ij][ic];}
				Ymatrix[ir][ic] = -sumLY/CholeskyT[ir][ir];
			}
		}
	}

	// Calculate the elements of matinv by back-substitution
	for(ir=MCMCdim-1; ir>=0; ir--){
		for(ic=MCMCdim-1; ic>=0; ic--){
			if(ir==MCMCdim-1){
				matinv[ir][ic] = Ymatrix[ir][ic]/Cholesky1[ir][ir];}
			else{
				sumUB = 0.0;
				for(ij=MCMCdim-1; ij>ir; ij--){
					sumUB += Cholesky1[ir][ij] * matinv[ij][ic];}
				matinv[ir][ic] = (Ymatrix[ir][ic] - sumUB)/Cholesky1[ir][ir];
			}
		}
	}
}

void SimulateParameters()
{
	int i, is, iy, MCMCindex;
	double Temp, Temp2, Adj1996;

	if(FixedUncertainty==0){
		int seed = 8222 + CurrSim * 53;
		CRandomMersenne rg(seed);
		for(i=0; i<MCMCdim; i++){
			RandPrior[i] = rg.Random();
			RandomUniform.out[CurrSim-1][i] = RandPrior[i];
		}
	}
	else{
		for(i=0; i<RandomUniform.columns; i++){
			RandPrior[i] = RandomUniform.out[CurrSim-1][i];}
	}

	if(InclPriors[0][0]==1){
		DualEfficacy = SamplePrior(0);}
	if(InclPriors[1][0]==1){
		TransmBFacute = SamplePrior(1);}
	if(InclPriors[2][0]==1){
		RRforEBF = SamplePrior(2);}
	if(InclPriors[3][0]==1){
		TransmAcute = SamplePrior(3);}
	if(InclPriors[4][0]==1){
		ProgToNeedLT = SamplePrior(4);}
	if(InclPriors[5][0]==1){
		ExcessProgToNeed = SamplePrior(5);}
	if(InclPriors[6][0]==1){
		RRprogressionPostnatal = SamplePrior(6);}
	if(InclPriors[7][0]==1){
		AIDSmortLT = SamplePrior(7);}
	if(InclPriors[8][0]==1){
		ExcessAIDSmort = SamplePrior(8);}
	if(InclPriors[9][0]==1){
		Temp = SamplePrior(9);
		TransmBFfirst3 = 1.0 - pow(1.0 - Temp, 1.0/12.0);
		TransmBFafter3 = TransmBFfirst3;
	}
	if(InclPriors[10][0]==1){
		PartnerRate20F = SamplePrior(10);}
	if(InclPriors[11][0]==1){
		RRpartnerLow[0] = SamplePrior(11);}
	if(InclPriors[12][0]==1){
		RRpartnerLow[1] = SamplePrior(12);}
	if(InclPriors[13][0]==1){
		RRpartnerMarried[0] = SamplePrior(13);}
	if(InclPriors[14][0]==1){
		RRpartnerMarried[1] = SamplePrior(14);}
	if(InclPriors[15][0]==1){
		Assortativeness = SamplePrior(15);}
	if(InclPriors[16][0]==1){
		CondomBias = SamplePrior(16);}
	if(InclPriors[17][0]==1){
		VCTcondom = SamplePrior(17);}
	if(InclPriors[18][0]==1){
		ARTcondom = 1.0 - pow(0.46, SamplePrior(18));}
	if(InclPriors[19][0]==1){
		InitFSWprev = SamplePrior(19) * 0.002;}
	if(InclPriors[20][0]==1){
		TransmST[1] = SamplePrior(20);}
	if(InclPriors[21][0]==1){
		TransmLT[1] = SamplePrior(21);}
	if(InclPriors[22][0]==1){
		TransmST[0] = SamplePrior(22);
		TransmFSW[0] = TransmST[0];
	}
	if(InclPriors[23][0]==1){
		TransmLT[0] = SamplePrior(23) * TransmST[0];
		TransmLT[1] = TransmST[1] * TransmLT[0] / TransmST[0];
	}
	if(InclPriors[24][0]==1){
		EctopyEffect[1] = SamplePrior(24);}
	if(InclPriors[25][0]==1){
		VLeffectInfectivity = SamplePrior(25);}
	if(InclPriors[26][0]==1){
		Temp = SamplePrior(26);
		Temp2 = (CD4duration[0] + CD4duration[1] + CD4duration[2] +
			1.0/(CD4decline[2] + CD4mort[0]) + CD4decline[2]/((CD4decline[2] +
			CD4mort[0]) * CD4mort[1]))/Temp;
		for(is=0; is<3; is++){
			CD4decline[is] *= Temp2;}
		CD4mort[0] *= Temp2;
		CD4mort[1] *= Temp2;
	}
	if(InclPriors[27][0]==1){
		CD4decline[0] = SamplePrior(27);}
	if(InclPriors[28][0]==1){
		CD4decline[1] = SamplePrior(28);}
	if(InclPriors[29][0]==1){
		CD4decline[2] = SamplePrior(29);}
	if(InclPriors[30][0]==1){
		CD4mort[1] = SamplePrior(30);}
	if(InclPriors[31][0]==1){
		RRmort200to350 = SamplePrior(31);}
	if(InclPriors[32][0]==1){
		RRuntreatedMortF = SamplePrior(32);}
	if(InclPriors[33][0]==1){
		RRprevPrivateANC = SamplePrior(33);}
	if(InclPriors[34][0]==1){
		IeDEAbias[0] = SamplePrior(34);}
	if(InclPriors[35][0]==1){
		RRper10yr = 1.0 + SamplePrior(35);}
	if(InclPriors[36][0]==1){ // Code to be added - trend in Brass alpha
		Temp = SamplePrior(36);}
	if(InclPriors[37][0]==1){
		RRfertHIV = SamplePrior(37);}
	if(InclPriors[38][0]==1){
		RRfertDiag = SamplePrior(38);
		//RRfertHIV = 1.0 / RRfertDiag;
	}
	if(InclPriors[39][0]==1){
		ARTinterruptionRate = SamplePrior(39);}
	if(InclPriors[40][0]==1){
		RR_ARTinterruption[34] = SamplePrior(40);
		RR_ARTinterruption[31] = 1.0 - 0.25 * (1.0 - RR_ARTinterruption[34]);
		RR_ARTinterruption[32] = 1.0 - 0.50 * (1.0 - RR_ARTinterruption[34]);
		RR_ARTinterruption[33] = 1.0 - 0.75 * (1.0 - RR_ARTinterruption[34]);
	}
	if(InclPriors[41][0]==1){
		GammaMeanF = SamplePrior(41);}
	if(InclPriors[42][0]==1){
		GammaSDF = SamplePrior(42);}
	if(InclPriors[43][0]==1){
		VCTage[0] = SamplePrior(43);}
	if(InclPriors[44][0]==1){
		VCTage[1] = SamplePrior(44);}
	if(InclPriors[45][0]==1){
		VCTmale2002 = SamplePrior(45);}
	if(InclPriors[46][0]==1){
		Temp = SamplePrior(46);} // Redundant code
	if(InclPriors[47][0]==1){
		HCT1stTimeF25[21] = SamplePrior(47);}
	if(InclPriors[48][0]==1){
		HCT1stTimeF25[24] = SamplePrior(48);}
	if(InclPriors[49][0]==1){
		HIVeffectVCT = SamplePrior(49);}
	if(InclPriors[50][0]==1){ // Previously OItoTBtestingRatio
		ORdiagOItreat = SamplePrior(50);}
	if(InclPriors[51][0]==1){
		EctopyEffect[0] = SamplePrior(51);}
	if(InclPriors[52][0]==1){
		RelInfecStage[0] = SamplePrior(52);}
	if(InclPriors[53][0]==1){
		RelInfecStage[4] = SamplePrior(53);
		RelInfecStage[3] = pow(RelInfecStage[4], 0.5);
	}
	if(InclPriors[54][0]==1){
		RednLogMort[0] = SamplePrior(54);
		RednLogMort[1] = RednLogMort[0];
		RednLogMort[2] = RednLogMort[0];
	}
	if(InclPriors[55][0]==1){
		FSWcontactAge21 = SamplePrior(55);}
	if(InclPriors[56][0]==1){
		HCT1stTimeF25[25] = SamplePrior(56);}
	if(InclPriors[57][0]==1){
		QuadParam[0] = SamplePrior(57);}
	if(InclPriors[58][0]==1){
		QuadParam[1] = SamplePrior(58);}
	if(InclPriors[59][0]==1){
		VCTmale2010 = SamplePrior(59);}
	if (InclPriors[60][0] == 1){
		RetestAdj = SamplePrior(60);
		RetestAdjInit = RetestAdj;
	}
	if (InclPriors[61][0] == 1){
		RetestPos = SamplePrior(61);
		RetestPosInit = RetestPos;
	}
	if (InclPriors[62][0] == 1){ // Previously Adj1996
		RetestART = SamplePrior(62);
		RetestARTinit = RetestART;
	}
	if (InclPriors[63][0] == 1){
		HighRiskPropn[0] = 0.35 * SamplePrior(63);
		HighRiskPropn[1] = 0.25 * SamplePrior(63);
	}
	if (InclPriors[64][0] == 1){
		CondomAdjProv = SamplePrior(64);}
	if (InclPriors[65][0] == 1){
		HighRiskPropn[1] = 0.25 * SamplePrior(65);
	}
	if (InclPriors[66][0] == 1){
		ProvANCbias = SamplePrior(66);}
	if (InclPriors[67][0] == 1){
		RRperCalYr = SamplePrior(67);}
	/*if (InclPriors[68][0] == 1){
		BsplineCoef[0] = SamplePrior(68);}
	else{
		BsplineCoef[0] = log(NumStartingART_M[15] + NumStartingART_F[15] + NumStartingART_P[15]); }
	if (InclPriors[69][0] == 1){
		BsplineCoef[1] = SamplePrior(69);
		BsplineCoef[1] += BsplineCoef[0];
	}
	else{
		BsplineCoef[1] = log(NumStartingART_M[17] + NumStartingART_F[17] + NumStartingART_P[17]);}
	if (InclPriors[88][0] == 1){
		SDchangeBspline = SamplePrior(88);}
	for (i = 70; i < 78; i++){
		if (InclPriors[i][0] == 1){
			//MCMCindex = InclPriors[i][1];
			//bParameters[MCMCindex] = SDchangeBspline;
			BsplineCoef[i-68] = SamplePrior(i);
			BsplineCoef[i-68] += BsplineCoef[i-69] * 2.0 - BsplineCoef[i-70];
		}
		else if(2000+(i-68)*2<=ARTdataYr){
			BsplineCoef[i - 68] = log(NumStartingART_M[(i - 68) * 2 + 15] +
				NumStartingART_F[(i - 68) * 2 + 15] + NumStartingART_P[(i - 68) * 2 + 15]);
		}
	}*/
	if (InclPriors[68][0] == 1){
		RateARTstartF[19] = SamplePrior(68);}
	if (InclPriors[69][0] == 1){
		RateARTstartF[25] = SamplePrior(69);}
	if (InclPriors[70][0] == 1){
		RateARTstartF[26] = SamplePrior(70);}
	if (InclPriors[71][0] == 1){
		RateARTstartF[29] = SamplePrior(71);}
	if (InclPriors[72][0] == 1){
		RateARTstartF[31] = SamplePrior(72);}
	if (InclPriors[73][0] == 1){
		RateARTstartF[15] = SamplePrior(73);}
	if (InclPriors[76][0] == 1){
		RR_ARTstart1stMo = SamplePrior(76); }
	if (InclPriors[77][0] == 1){
		RR_ARTstartM = SamplePrior(77); }
	if (InclPriors[68][0] == 1 || InclPriors[69][0] == 1 || InclPriors[70][0] == 1 || InclPriors[71][0] == 1 || InclPriors[72][0] == 1){
		for (iy = 16; iy < 19; iy++){ RateARTstartF[iy] = RateARTstartF[15]; }
		for (iy = 20; iy < 25; iy++){
			RateARTstartF[iy] = RateARTstartF[19] * (25 - iy) / 6.0 + RateARTstartF[25] * (iy - 19) / 6.0; }
		for (iy = 27; iy <= 30; iy++){
			RateARTstartF[iy] = RateARTstartF[26]; }
		//RateARTstartF[30] = RateARTstartF[29];
		for (iy = 32; iy < 116; iy++){
			RateARTstartF[iy] = RateARTstartF[31]; }
		if (PropnalImmART == 1){
			for (iy = 15; iy < 116; iy++){
				HCT_ARTuptake[iy] = RateARTstartF[iy] * RR_ARTstart1stMo;
				if (HCT_ARTuptake[iy] > 0.9999){ HCT_ARTuptake[iy] = 0.9999; }
			}
		}
	}
	/*if (InclPriors[78][0] == 1){
		BsplineCoefP[0] = SamplePrior(78);}
	else{
		BsplineCoefP[0] = log(NumStartingART_P[15]);}
	if (InclPriors[79][0] == 1){
		BsplineCoefP[1] = SamplePrior(79);
		BsplineCoefP[1] += BsplineCoefP[0];
	}
	else{
		BsplineCoefP[1] = log(NumStartingART_P[17]);}
	for (i = 80; i < 88; i++){
		if (InclPriors[i][0] == 1){
			//MCMCindex = InclPriors[i][1];
			//bParameters[MCMCindex] = SDchangeBspline;
			BsplineCoefP[i-78] = SamplePrior(i);
			BsplineCoefP[i-78] += BsplineCoefP[i-79] * 2.0 - BsplineCoefP[i-80];
		}
		else if (2000 + (i - 78) * 2<=ARTdataYr){
			BsplineCoefP[i - 78] = log(NumStartingART_P[(i - 78) * 2 + 15]);
		}
	}*/
	if (InclPriors[78][0] == 1){
		RateARTstartC[19] = SamplePrior(78);}
	if (InclPriors[79][0] == 1){
		RateARTstartC[24] = SamplePrior(79);}
	if (InclPriors[80][0] == 1){
		RateARTstartC[25] = SamplePrior(80);}
	if (InclPriors[81][0] == 1){
		RateARTstartC[27] = SamplePrior(81);}
	if (InclPriors[82][0] == 1){
		RateARTstartC[31] = SamplePrior(82);}
	if (InclPriors[83][0] == 1){
		RateARTstartC[15] = SamplePrior(83);}
	if (InclPriors[78][0] == 1 || InclPriors[79][0] == 1 || InclPriors[80][0] == 1 || InclPriors[81][0] == 1 || InclPriors[82][0] == 1){
		for (iy = 16; iy < 19; iy++){ RateARTstartC[iy] = RateARTstartC[15]; }
		for (iy = 20; iy < 24; iy++){
			RateARTstartC[iy] = RateARTstartC[19] * (24 - iy) / 5.0 + RateARTstartC[24] * (iy - 19) / 5.0;}
		for (iy = 26; iy < 31; iy++){ RateARTstartC[iy] = RateARTstartC[25]; }
		for (iy = 32; iy < 116; iy++){ RateARTstartC[iy] = RateARTstartC[31]; }
		if (PropnalImmART == 1){
			for (iy = 15; iy < 116; iy++){
				PaedARTuptake[iy] = RateARTstartC[iy] * RR_ARTstart1stMo;
				if (PaedARTuptake[iy] > 0.9999){ PaedARTuptake[iy] = 0.9999; }
			}
		}
	}
	if (InclPriors[89][0] == 1){
		AnnSwitchCumToCurr = SamplePrior(89);}
	if (InclPriors[90][0] == 1){
		IeDEAbias[1] = SamplePrior(90) * IeDEAbias[0];
		// Previous version did not adjust for IeDEAbias[1].
	}
	if (InclPriors[91][0] == 1){
		InfToVirulenceRatio = SamplePrior(91);}
	if (InclPriors[92][0] == 1){
		MtoM_ST = SamplePrior(92);}
	if (InclPriors[93][0] == 1){
		RRtestVirginTrend[0] = SamplePrior(93);}
	if (InclPriors[94][0] == 1){
		RRtestPaedAdvanced = 1.0 / SamplePrior(94);}
	if (InclPriors[95][0] == 1){
		ImmARTcorrectionP = SamplePrior(95);}
	if (InclPriors[96][0] == 1){
		RiskCompensation = SamplePrior(96);}
	if (InclPriors[97][0] == 1){
		RRearlyPaedART = SamplePrior(97);}
	if (InclPriors[98][0] == 1){
		VCT_FSWentry = SamplePrior(98);}
	if (InclPriors[99][0] == 1){
		TransmFSW[1] = SamplePrior(99) / RelInfecRiskST[0][0][1];
		if (InclPriors[109][0] == 0){
			RRclientToFSW1985 = 27.0 * exp(-770.8 * TransmFSW[1] * RelInfecRiskST[0][0][1]);
		}
	}
	if (InclPriors[100][0] == 1){
		RRmortART2 = SamplePrior(100);
		RRmortART1 = 0.5 * (RRmortART2 + 1.0);
	}
	if (InclPriors[101][0] == 1){
		RednLogMortP[0] = SamplePrior(101);
		RednLogMortP[1] = RednLogMortP[0];
		RednLogMortP[2] = RednLogMortP[0];
	}
	if (InclPriors[102][0] == 1){
		ExcessProgRedn = SamplePrior(102);}
	if (InclPriors[103][0] == 1){
		ExcessMortRedn = SamplePrior(103);}
	if (InclPriors[104][0] == 1){
		RetestAdjMax = SamplePrior(104);}
	if (InclPriors[105][0] == 1){
		RRtestVirginTrend[1] = SamplePrior(105);}
	if (InclPriors[106][0] == 1){
		RRdiagDeathsPIP[0] = SamplePrior(106);}
	if (InclPriors[107][0] == 1){
		RRfertART = SamplePrior(107);}
	if (InclPriors[108][0] == 1){
		RRfertCD4[0] = SamplePrior(108) + 1.0;}
	if (InclPriors[109][0] == 1){
		RRclientToFSW1985 = SamplePrior(109);}
	if (InclPriors[110][0] == 1){
		ORsuppressionIeDEA = SamplePrior(110);}
	if (InclPriors[111][0] == 1){
		RRinterruptionM = SamplePrior(111);}
	if (InclPriors[112][0] == 1){
		RR_ARTstart100CD4 = SamplePrior(112);}
	if (InclPriors[113][0] == 1){
		VirginTestAdjProv = SamplePrior(113);}
	if (InclPriors[114][0] == 1){
		BFadjProv = SamplePrior(114);}
	if (InclPriors[115][0] == 1){
		SpecificityANC = SamplePrior(115);}
	if (InclPriors[116][0] == 1){
		MarriageConstant[0] = SamplePrior(116);}
	if (InclPriors[117][0] == 1){
		MarriageConstant[1] = SamplePrior(117);}
	if (InclPriors[118][0] == 1){
		MarriageTrend[0] = SamplePrior(118);}
	if (InclPriors[119][0] == 1){
		MarriageTrend[1] = SamplePrior(119);}
	if (InclPriors[120][0] == 1){
		MarriageShape[0] = SamplePrior(120);}
	if (InclPriors[121][0] == 1){
		MarriageShape[1] = SamplePrior(121);}
	if (InclPriors[122][0] == 1){
		ORremarriage[0] = 1.0 / SamplePrior(122);}
	if (InclPriors[123][0] == 1){
		ORremarriage[1] = 1.0 / SamplePrior(123);}
	if (InclPriors[124][0] == 1){
		DivorceAdj = SamplePrior(124);}
	if (InclPriors[125][0] == 1){
		DivorceTrend = SamplePrior(125);}
	if (InclPriors[126][0] == 1){
		SeTestingHistory[0] = SamplePrior(126); }
	if (InclPriors[127][0] == 1){
		SeTestingHistory[1] = SamplePrior(127); }
	if (InclPriors[128][0] == 1){
		SpTestingHistory = SamplePrior(128); }
	if (InclPriors[129][0] == 1){
		RetestPosP = SamplePrior(129);}
	if (InclPriors[130][0] == 1){
		COVIDimpactARTstart = SamplePrior(130);}
	if (InclPriors[131][0] == 1){
		ProvAdjPaedMort = SamplePrior(131);}

	// Update natural history calculations
	CD4duration[1] = (1.0/CD4decline[0]) - CD4duration[0];
	CD4duration[2] = 1.0/CD4decline[1];
	CD4duration[3] = 1.0/CD4decline[2];
	CD4mort[0] = CD4mort[1] * RRmort200to350;

	// Update HCT rollout rates
	if(InclPriors[46][0]==1 || InclPriors[47][0]==1 || InclPriors[48][0]==1){
		for(iy=5; iy<17; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + HCT1stTimeF25[17]/12.0;}
		for(iy=18; iy<21; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + (HCT1stTimeF25[21] -
				HCT1stTimeF25[17])/4.0;}
		for(iy=22; iy<24; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + (HCT1stTimeF25[24] -
				HCT1stTimeF25[21])/3.0;}
		HCT1stTimeF25[26] = HCT1stTimeF25[25] * 0.8;
	}
	/*if (InclPriors[62][0] == 1){
		NumbersTested[11] = Adj1996 * NumbersTested[17];
		for (iy = 5; iy < 11; iy++){
			NumbersTested[iy] = NumbersTested[11] * (iy - 5.0) / 6.0;}
		for (iy = 12; iy < 17; iy++){
			NumbersTested[iy] = NumbersTested[iy-1] + (NumbersTested[17] -
				NumbersTested[11]) / 6.0;
		}
	}*/

	// Update numbers starting ART in each year
	//if ((CalibARTtotals == 1 || CalibARTtotalsP == 1) && InputARTinitiationRates == 0){ GetAnnNewART(); }

	// Handle IeDEA bias
	if (InclPriors[34][0] == 1 && InclPriors[90][0] == 0){
		IeDEAbias[1] = IeDEAbias[0];}

	if (InclPriors[95][0] == 1){
		PaedARTuptakeEID = BaseARTuptakeEID * ImmARTcorrectionP;
	}

	// Calculate RR_ARTinitiation
	if (InclPriors[112][0] == 1){
		// Same as in the SetActivityByStage function: Temp represents dif in average CD4 between
		// each CD4 category and the base CD4 category (<200)
		for (is = 0; is < 4; is++){
			if (is == 0){ Temp = 4.75; }
			if (is == 1){ Temp = 3.25; }
			if (is == 2){ Temp = 1.75; }
			if (is == 3){ Temp = 0.00; }
			RR_ARTinitiation[is + 1] = pow(RR_ARTstart100CD4, Temp);
		}
	}

	if(FixedUncertainty==0 && IMISind==1){
		for(i=0; i<MCMCdim; i++){
			if(PriorTypes[i]==0){ // Beta prior
				RandomParameterIMIS[i][CurrSim-1] = log(ModelParameters.out[CurrSim-1][i]/
					(1.0 - ModelParameters.out[CurrSim-1][i]));}
			else if(PriorTypes[i]==1){ // Gamma prior
				RandomParameterIMIS[i][CurrSim-1] = log(ModelParameters.out[CurrSim-1][i]);}
			else{ // Normal prior
				RandomParameterIMIS[i][CurrSim-1] = ModelParameters.out[CurrSim-1][i];}
		}
	}
}

void SimulateParameters_FSW()
{
	int i, ind, s;
	double x, y, a, b, p, q, bound;

	// Male rate of visiting sex workers
	a = 5.44;
	b = 1.56;
	p = RandomUniform.out[CurrSim-1][0];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	FSWcontactAge21 = x;

	// Rate of retirement from sex work
	a = 10.9;
	b = 33.0;
	p = RandomUniform.out[CurrSim-1][1];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	DurFSW = 1.0/x;

	// PrEP uptake in sex workers
	a = 9.0;
	b = 30.0;
	p = RandomUniform.out[CurrSim-1][2];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	//for(i=30; i<116; i++){
	//	PrEP_FSW[i] = x;}

	// PrEP efficacy
	a = 1.27;
	b = 1.90;
	p = RandomUniform.out[CurrSim-1][3];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	PrEPefficacy[0] = x;
	PrEPefficacy[1] = x;

	// Reduction in condom use if using PrEP
	a = 0.80;
	b = 7.20;
	p = RandomUniform.out[CurrSim-1][4];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	CondomRednPrEP[0] = x;
	CondomRednPrEP[1] = x;

	// Microbicide uptake in sex workers
	a = 9.0;
	b = 30.0;
	p = RandomUniform.out[CurrSim-1][5];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	//for(i=30; i<116; i++){
	//	VM_FSW[i] = x;}

	// Microbicide efficacy
	a = 2.75;
	b = 8.25;
	p = RandomUniform.out[CurrSim-1][6];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	MicrobicideEff = x;

	// Reduction in condom use if using microbicides
	a = 0.80;
	b = 7.20;
	p = RandomUniform.out[CurrSim-1][7];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	CondomRednVM = x;

	// ART uptake in FSW with CD4 >350
	a = 5.03;
	b = 3.35;
	p = RandomUniform.out[CurrSim-1][8];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	//ImmART_CSW = x;

	// Reduction in infectiousness after ART initiation
	/*a = 8.09;
	b = 2.02;
	p = RandomUniform.out[CurrSim-1][9];
	q = 1.0 - p;
	bound = 0.0;
	ind = 2;
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	ARTinfectivity = 1.0 - x;*/
}

double SamplePrior(int PriorIndex)
{
	int MCMCindex, ind, s;
	double x, y, a, b, p, q, bound, PriorOut;

	MCMCindex = InclPriors[PriorIndex][1];
	a = aParameters[MCMCindex];
	b = bParameters[MCMCindex];
	if (ImportanceSamplingStep1 == 1){
		a = aParameters2[MCMCindex];
		b = bParameters2[MCMCindex];
	}
	p = RandPrior[MCMCindex];
	q = 1.0 - RandPrior[MCMCindex];
	bound = 0.0;
	ind = 2;

	if(PriorTypes[MCMCindex]==0){ // Beta/uniform prior
		cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
		PriorOut = x;
	}
	else if(PriorTypes[MCMCindex]==1){ // Gamma prior
		cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
		PriorOut = x;
	}
	else{ // Normal prior
		s = 0;
		cdfnor(&ind,&p,&q,&x,&a,&b,&s,&bound);
		PriorOut = x;
	}
	ModelParameters.out[CurrSim-1][MCMCindex] = x;

	return PriorOut;
}

void GetAnnNewART()
{
	int ib, iy;
	double AdultsStarting;

	// Simulate paediatric ART numbers
	if (CalibARTtotalsP == 1){
		for (ib = 0; ib < 10; ib++){
			if (InclPriors[ib + 78][0] == 1 && (2000 + ib * 2) <= InterpolationStart){
				NumStartingART_P[15 + ib * 2] = exp(BsplineCoefP[ib]);
			}
		}
		for (ib = 1; ib < 10; ib++){
			if (InclPriors[ib + 78][0] == 1 && (1999 + ib * 2) <= InterpolationStart){
				NumStartingART_P[14 + ib * 2] = exp(0.5*(BsplineCoefP[ib - 1] +
					BsplineCoefP[ib]));
			}
		}
	}

	// Simulate adult ART numbers
	if (CalibARTtotals == 1){
		for (ib = 0; ib < 10; ib++){
			if (InclPriors[ib + 68][0] == 1 && (2000 + ib * 2) <= ARTdataYr){
				AdultsStarting = exp(BsplineCoef[ib]) - NumStartingART_P[15 + ib * 2];
				if (AdultsStarting < 0.0){ AdultsStarting = 0.0; }
				NumStartingART_M[15 + ib * 2] = AdultsStarting * MalePropnART[15 + ib * 2];
				NumStartingART_F[15 + ib * 2] = AdultsStarting * (1.0 - MalePropnART[15 + ib * 2]);
			}
		}
		for (ib = 1; ib < 10; ib++){
			if (InclPriors[ib + 68][0] == 1 && (1999 + ib * 2) <= ARTdataYr){
				AdultsStarting = exp(0.5*(BsplineCoef[ib - 1] + BsplineCoef[ib])) -
					NumStartingART_P[14 + ib * 2];
				if (AdultsStarting < 0.0){ AdultsStarting = 0.0; }
				NumStartingART_M[14 + ib * 2] = AdultsStarting * MalePropnART[14 + ib * 2];
				NumStartingART_F[14 + ib * 2] = AdultsStarting * (1.0 - MalePropnART[14 + ib * 2]);
			}
		}
	}
	if (CalibARTtotals == 0 && CalibARTtotalsP == 1){
		for (iy = 15; iy < 34; iy++){
			AdultsStarting = TotBeginART[iy] - NumStartingART_P[iy];
			if (AdultsStarting < 0.0){ AdultsStarting = 0.0; }
			NumStartingART_M[iy] = AdultsStarting * MalePropnART[iy];
			NumStartingART_F[iy] = AdultsStarting * (1.0 - MalePropnART[iy]);
		}
	}
}

void GetCurrART()
{
	int ii, ia;
	double temp1, temp2;

	for (ii = 0; ii<ARTdataPoints; ii++){
		if ((ARTtotals[ii][0] == CurrYear && ARTtotals[ii][1] == (CurrMonth-1)) ||
			(ARTtotals[ii][0] == (CurrYear-1) && ARTtotals[ii][1] == 11 && CurrMonth==0)){
			ARTmodelled[ii][0] = 0.0;
			for (ia = 0; ia < 91; ia++){
				ARTmodelled[ii][0] += TotalART[ia][0] + TotalART[ia][1]; }
		}
		if (ARTtotals[ii][0] > CurrYear){ break; }
	}
	for (ii = 0; ii<ARTdataPointsP; ii++){
		if ((ARTtotalsP[ii][0] == CurrYear && ARTtotalsP[ii][1] == (CurrMonth - 1)) ||
			(ARTtotalsP[ii][0] == (CurrYear - 1) && ARTtotalsP[ii][1] == 11 && CurrMonth == 0)){
			ARTmodelledP[ii][0] = 0.0;
			for (ia = 0; ia < 15; ia++){
				ARTmodelledP[ii][0] += TotalART[ia][0] + TotalART[ia][1]; }
		}
		if (ARTtotalsP[ii][0] > CurrYear){ break; }
	}
	if (ProvModel == 1){
		for (ii = 0; ii < ARTdataPointsM; ii++){
			if (ARTmale[ii][0] == CurrYear && CurrMonth == 0){
				temp1 = 0.0;
				temp2 = 0.0;
				for (ia = 15; ia < 91; ia++){
					temp1 += TotalART[ia][0];
					temp2 += TotalART[ia][1];
				}
				ARTmodelledM[ii] = temp1 / (temp1 + temp2);
			}
			if (ARTmale[ii][0] > CurrYear){ break; }
		}
	}
	if (CalibARTcoverage == 1 && (CurrYear == 2012 || CurrYear == 2017) && CurrMonth == 0){
		temp1 = 0.0;
		temp2 = 0.0;
		for (ia = 15; ia < 91; ia++){
			temp1 += TotalART[ia][0];
			temp2 += TotalPositive[ia][0];
		}
		if (CurrYear == 2012){ ModelCoverage[0][0] = temp1 / temp2; }
		else{ ModelCoverage[0][1] = temp1 / temp2; }
		temp1 = 0.0;
		temp2 = 0.0;
		for (ia = 15; ia < 91; ia++){
			temp1 += TotalART[ia][1];
			temp2 += TotalPositive[ia][1];
		}
		if (CurrYear == 2012){ ModelCoverage[1][0] = temp1 / temp2; }
		else{ ModelCoverage[1][1] = temp1 / temp2; }
	}
}

void GenerateSample()
{
	/*int i, j, iy;
	double CumTotL[InitSample+1], r[ResampleSize];
	double lookup;
	double x1, DoubleIntDif1, intpart1;
	double MaxLogL;

	clock_t start2, finish2;
	double elapsed_time2;
	start2 = clock();

	CurrSim = 0;
	for(i=1; i<=InitSample; i++){
		CurrSim += 1;
		x1 = CurrSim/1000.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			finish2 = clock();
			elapsed_time2 = finish2 - start2;
			std::cout<<"Time to "<<i<<"th iteration: "<<elapsed_time2<<std::endl;
		}

		if(i==1){
			ReadPaedAssumps();
			ReadRollout();
			ReadHIVprevData();
		}
		SimulateParameters();
		SetInitialParameters();

		CurrYear = StartYear-1;
		for(iy=0; iy<ProjectionTerm; iy++){
			OneYear();
		}
		CalcLikelihood();
		LogL.out[i-1][0] = LogLikelihood;
		if(LogLikelihood<0 || LogLikelihood>=0){
			continue;}
		else{
			break;}
	}
	std::cout<<"Initial set of parameter combinations evaluated."<<std::endl;

	// Calculate cumulative likelihood for each simulation
	CumTotL[0] = 0;
	MaxLogL = LogL.out[0][0];
	for(i=0; i<InitSample; i++){
		if(LogL.out[i][0] > MaxLogL){
			MaxLogL = LogL.out[i][0];}
	}
	for(i=0; i<InitSample; i++){
		CumTotL[i+1] = CumTotL[i] + exp(LogL.out[i][0] - MaxLogL);}

	// Generate random variables from the uniform (0, 1) distribution
	int seed = time(0);
	CRandomMersenne rg(seed);
	for(i=0; i<ResampleSize; i++){
		r[i] = rg.Random();}

	// Sample from the simulations and record ID numbers of the sampled simulations
	// in the SampleID array
	for(i=0; i<ResampleSize; i++){
		lookup = r[i]*CumTotL[InitSample];
		for(j=0; j<InitSample; j++){
			if(lookup>=CumTotL[j] && lookup<CumTotL[j+1]){
				SampleID[i] = j;
				break;
			}
		}
	}
	std::cout<<"Generated sample from initial set of parameter combinations."<<std::endl;

	LogL.Record("LogL.txt", 8);
	PrevLogL.Record("PrevLogL.txt", 8);
	PaedParameters.Record("PaedParameters.txt", 8);
	RandomUniform.SampleInput();
	RandomUniform.RecordSample("RandomUniform.txt", 8);*/
}

void RunSample()
{
	// Remember to set FixedUncertainty to 1 before calling this function.

	std::ifstream file1, file2;
	char filout[18];
	int i, c, ia, iy, idum;
	double x1, intpart1, DoubleIntDif1;

	// Read in random numbers for each parameter combination in sample
	ReadPriors();
	if(IMISind==0){
		file1.open("RandomUniform.txt");
		for(i=0; i<ResampleSize; i++){
			file1>>idum>>idum;
			for(c=0; c<RandomUniform.columns; c++){
				file1>>RandomUniform.out[i][c];}
		}
		file1.close();
	}
	else{
		file1.open("ModelParameters.txt");
		for(i=0; i<ResampleSize; i++){
			file1>>idum>>idum;
			for(c=0; c<ModelParameters.columns; c++){
				file1>>ModelParameters.out[i][c];}
		}
		file1.close();
		if (VaryFutureInterventions == 1){
			file2.open("FutureInterventions.txt");
			for (i = 0; i<ResampleSize; i++){
				file2 >> idum;
				for (c = 0; c<34; c++){
					file2 >> FutureInterventions.out[i][c];
				}
			}
			file2.close();
		}
	}
	/*file1.open("RandomUniform.txt");
	for(i=0; i<ResampleSize; i++){
		file1>>idum>>idum;
		for(c=0; c<10; c++){
			file1>>RandomUniform.out[i][c];}
	}
	file1.close();*/
	std::cout<<"Read input files"<<std::endl;

	// Run the model for each of the sampled parameter combinations
	CurrSim = 0;
	//#pragma omp parallel for
	for(i=1; i<=ResampleSize; i++){ // Same code as in the OneIMISstep function
		CurrSim += 1;
		CurrYear = StartYear;
		if(i==1){
			ReadAllFiles();}
		if(IMISind==0){
			SimulateParameters();}
		else{
			SimulateParameters_IMIS();}
		//SimulateParameters_FSW();
		if (VaryFutureInterventions == 1){
			SetFutureRollout();}
		UpdateNonAIDSmort();
		SetInitialParameters();
		SetCD4byARTdur();
		CalcInterruptions();
		SetActivityByStage();
		SetInitSexActivity();
		UpdateMixingST();
		SetCurrYearParameters();
		SetFertByStage();
		SetProgression(0);
		UpdateARTmort();
		ARTerrorInd = 0;
		CurrYear = StartYear-1;
		for(iy=0; iy<ProjectionTerm; iy++){
			OneYear();
		}
		CalcLikelihood(); // For getting bias
		//LogL.out[i-1][0] = LogLikelihood;
		x1 = CurrSim/100.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			std::cout<<"Completed simulation "<<CurrSim<<std::endl;}
	}

	// Write prevalence outputs to text files
	// PrevPreg15to49.RecordSample("PrevPreg15to49.txt");
    // PrevPreg15to19.RecordSample("PrevPreg15to19.txt");
    // PrevPreg20to24.RecordSample("PrevPreg20to24.txt");
    // PrevPreg25to29.RecordSample("PrevPreg25to29.txt");
    // PrevPreg30to34.RecordSample("PrevPreg30to34.txt");
    // PrevPreg35to39.RecordSample("PrevPreg35to39.txt");
    // PrevPreg40to49.RecordSample("PrevPreg40to49.txt");
    // ANCbias.RecordSample("ANCbias.txt");
    // ErrorVariance.RecordSample("ErrorVar.txt");
    PrevFSW.RecordSample("PrevFSW.txt");
	//PrevFSW15to24.RecordSample("PrevFSW15to24.txt");
	//PrevFSW25plus.RecordSample("PrevFSW25plus.txt");
    // NegFSW.RecordSample("NegFSW.txt");
    // PrevClients.RecordSample("PrevClients.txt");
    // NegClients.RecordSample("NegClients.txt");
	TotalHIV.RecordSample("TotalHIV.txt");
  TotHIV15.RecordSample("TotHIV15.txt");
  TotHIV15M.RecordSample("TotHIV15M.txt");
	TotHIV15F.RecordSample("TotHIV15F.txt");
	TotPaedHIV.RecordSample("TotPaedHIV.txt");
	TotHIV15to24.RecordSample("TotHIV15to24.txt");
	TotHIV15to24M.RecordSample("TotHIV15to24M.txt");
	TotHIV15to24F.RecordSample("TotHIV15to24F.txt");
	TotHIV15to49.RecordSample("TotHIV15to49.txt");
	TotHIV15to49M.RecordSample("TotHIV15to49M.txt");
	TotHIV15to49F.RecordSample("TotHIV15to49F.txt");
	TotHIV25to49.RecordSample("TotHIV25to49.txt");
	TotHIV25to49M.RecordSample("TotHIV25to49M.txt");
	TotHIV25to49F.RecordSample("TotHIV25to49F.txt");
	TotHIV50plus.RecordSample("TotHIV50plus.txt");
	TotHIV50plusM.RecordSample("TotHIV50plusM.txt");
	TotHIV50plusF.RecordSample("TotHIV50plusF.txt");
    // Prev15to24.RecordSample("Prev15to24.txt");
  Prev15to49.RecordSample("Prev15to49.txt");
    // Prev25plus.RecordSample("Prev25plus.txt");
    // Prev0to14.RecordSample("Prev0to14.txt");
    // Prev2to14.RecordSample("Prev2to14.txt");
    // HSRCcalib2002.RecordSample("HSRCcalib2002.txt");
    // HSRCcalib2005.RecordSample("HSRCcalib2005.txt");
    // HSRCcalib2008.RecordSample("HSRCcalib2008.txt");
    // HSRCcalib2012.RecordSample("HSRCcalib2012.txt");
    // HSRCcalib2017.RecordSample("HSRCcalib2017.txt");
    // DHScalib2016.RecordSample("DHScalib2016.txt");
	/*Prev0to1.RecordSample("Prev0to1.txt");
	Prev2to4M.RecordSample("Prev2to4M.txt");
	Prev2to4F.RecordSample("Prev2to4F.txt");
	Prev5to9M.RecordSample("Prev5to9M.txt");
	Prev5to9F.RecordSample("Prev5to9F.txt");
	Prev10to14M.RecordSample("Prev10to14M.txt");
	Prev10to14F.RecordSample("Prev10to14F.txt");*/
	/*MSMprev18to24.RecordSample("MSMprev18to24.txt");
	MSMprev25plus.RecordSample("MSMprev25plus.txt");*/
	MSMprev18plus.RecordSample("MSMprev18plus.txt");
	TotalMSM.RecordSample("TotalMSM.txt");

	// Write HIV incidence outputs to text files
  NewHIVinFSW.RecordSample("NewHIVinFSW.txt");
  NewHIVclients.RecordSample("NewHIVclients.txt");
  NewHIVinMSM.RecordSample("NewHIVinMSM.txt");
  NewAdultHIV.RecordSample("NewAdultHIV.txt");
  NewHIV15F.RecordSample("NewHIV15F.txt");
  NewHIV15M.RecordSample("NewHIV15M.txt");
  NewHIV15to24.RecordSample("NewHIV15to24.txt");
  NewHIV15to24F.RecordSample("NewHIV15to24F.txt");
  NewHIV15to24M.RecordSample("NewHIV15to24M.txt");
  NewHIV15to49.RecordSample("NewHIV15to49.txt");
  NewHIV15to49F.RecordSample("NewHIV15to49F.txt");
  NewHIV15to49M.RecordSample("NewHIV15to49M.txt");
  NewHIV25to49.RecordSample("NewHIV25to49.txt");
  NewHIV25to49F.RecordSample("NewHIV25to49F.txt");
  NewHIV25to49M.RecordSample("NewHIV25to49M.txt");
  NewHIV50.RecordSample("NewHIV50.txt");
  NewHIV50F.RecordSample("NewHIV50F.txt");
  NewHIV50M.RecordSample("NewHIV50M.txt");
  NewHIVU15.RecordSample("NewHIVU15.txt");
	HIVinc0to14.RecordSample("HIVinc0to14.txt");
	HIVinc15to49.RecordSample("HIVinc15to49.txt");
	HIVinc15to49M.RecordSample("HIVinc15to49M.txt");
	HIVinc15to49F.RecordSample("HIVinc15to49F.txt");
	HIVinc15to24.RecordSample("HIVinc15to24.txt");
	HIVinc15to24M.RecordSample("HIVinc15to24M.txt");
	HIVinc15to24F.RecordSample("HIVinc15to24F.txt");
	HIVinc25to49M.RecordSample("HIVinc25to49M.txt");
	HIVinc25to49F.RecordSample("HIVinc25to49F.txt");
	HIVinc25to49.RecordSample("HIVinc25to49.txt");
	HIVinc50F.RecordSample("HIVinc50F.txt");
	HIVinc50M.RecordSample("HIVinc50M.txt");
	HIVinc50.RecordSample("HIVinc50.txt");
	HIVinc15plusM.RecordSample("HIVinc15plusM.txt");
	HIVinc15plusF.RecordSample("HIVinc15plusF.txt");
	HIVincFSW.RecordSample("HIVincFSW.txt"); 
	HIVincMSM.RecordSample("HIVincMSM.txt");
	HIVincClients.RecordSample("HIVincClients.txt");
	/*HIVinc2000.RecordSample("HIVinc2000.txt");
	HIVinc2010.RecordSample("HIVinc2010.txt");
	PAFforCSW.RecordSample("PAFforCSW.txt");
	NewHIVatBirth.RecordSample("NewHIVatBirth.txt");
	NewHIVafterBirth.RecordSample("NewHIVafterBirth.txt");
  MTCTrateBirthDiag.RecordSample("MTCTrateBirthDiag.txt");
  MTCTrateAtBirth.RecordSample("MTCTrateAtBirth.txt");

	NewHIVto18mo.RecordSample("NewHIVto18mo.txt");
	NewHIVmothersBF.RecordSample("NewHIVmothersBF.txt");
	VertTransmKnownPos.RecordSample("VertTransmKnownPos.txt");*/
	TotalNewHIV.RecordSample("TotalNewHIV.txt");
  NewDiagnosesPregnancy.RecordSample("NewDiagnosesPregnancy.txt");
  RediagnosesPregnancy.RecordSample("RediagnosesPregnancy.txt");
  TotANCtests.RecordSample("TotANCtests.txt");

	// Write mortality outputs to text files
	/*Deaths0M.RecordSample("Deaths0M.txt");
	Deaths1M.RecordSample("Deaths1M.txt");
	Deaths5M.RecordSample("Deaths5M.txt");
	Deaths10M.RecordSample("Deaths10M.txt");
	Deaths20M.RecordSample("Deaths20M.txt");
	Deaths25M.RecordSample("Deaths25M.txt");
	Deaths30M.RecordSample("Deaths30M.txt");
	Deaths35M.RecordSample("Deaths35M.txt");
	Deaths40M.RecordSample("Deaths40M.txt");
	Deaths45M.RecordSample("Deaths45M.txt");
	Deaths50M.RecordSample("Deaths50M.txt");
	Deaths55M.RecordSample("Deaths55M.txt");
	Deaths0F.RecordSample("Deaths0F.txt");
	Deaths1F.RecordSample("Deaths1F.txt");
	Deaths5F.RecordSample("Deaths5F.txt");
	Deaths10F.RecordSample("Deaths10F.txt");
	Deaths20F.RecordSample("Deaths20F.txt");
	Deaths25F.RecordSample("Deaths25F.txt");
	Deaths30F.RecordSample("Deaths30F.txt");
	Deaths35F.RecordSample("Deaths35F.txt");
	Deaths40F.RecordSample("Deaths40F.txt");
	Deaths45F.RecordSample("Deaths45F.txt");
	Deaths50F.RecordSample("Deaths50F.txt");
	Deaths55F.RecordSample("Deaths55F.txt");
	AIDSdeathsPaed.RecordSample("AIDSdeathsPaed.txt");*/
	AIDSdeathsAdultM.RecordSample("AIDSdeathsAdultM.txt");
	AIDSdeathsAdultF.RecordSample("AIDSdeathsAdultF.txt");
	/*AIDSdeaths0.RecordSample("AIDSdeaths0.txt");
	AIDSdeaths1to4.RecordSample("AIDSdeaths1to4.txt");
	AIDSdeaths5to9.RecordSample("AIDSdeaths5to9.txt");
	AIDSdeaths10to14.RecordSample("AIDSdeaths10to14.txt");
	AIDSdeaths20to59M.RecordSample("AIDSdeaths20to59M.txt");
	AIDSdeaths20to59F.RecordSample("AIDSdeaths20to59F.txt");
	NonAIDSdeaths2005.RecordSample("NonAIDSdeaths2005.txt");
	IMR.RecordSample("IMR.txt");
	U5MR.RecordSample("U5MR.txt");
	M45q15.RecordSample("M45q15.txt");
	F45q15.RecordSample("F45q15.txt");
	LifeExpectM.RecordSample("LifeExpectM.txt");
	LifeExpectF.RecordSample("LifeExpectF.txt");
	AIDSdeathsUndiag.RecordSample("AIDSdeathsUndiag.txt");
	AIDSdeathsDiagPreART.RecordSample("AIDSdeathsDiagPreART.txt");
	AIDSdeaths1st6moART.RecordSample("AIDSdeaths1st6moART.txt");
	AIDSdeathsAfter6moART.RecordSample("AIDSdeathsAfter6moART.txt");
	DiagDeaths1to4.RecordSample("DiagDeaths1to4.txt");
	DiagDeaths5to9.RecordSample("DiagDeaths5to9.txt");
	ARTdeaths1to4.RecordSample("ARTdeaths1to4.txt");
	ARTdeaths5to9.RecordSample("ARTdeaths5to9.txt");
	CompletenessPaed.RecordSample("CompletenessPaed.txt");
	CompletenessAdj.RecordSample("CompletenessAdj.txt");*/

	// Write ART/disease stage outputs to text files
	/*AdultsUnder200.RecordSample("AdultsUnder200.txt");
	Adults200to349.RecordSample("Adults200to349.txt");
	Adults350to499.RecordSample("Adults350to499.txt");
	AdultsOver500.RecordSample("AdultsOver500.txt");*/
	StartingART0.RecordSample("StartingART0.txt");
	StartingART1.RecordSample("StartingART1.txt");
	StartingART1to2.RecordSample("StartingART1to2.txt");
	StartingART2to4.RecordSample("StartingART2to4.txt");
	StartingART3to5.RecordSample("StartingART3to5.txt");
	StartingART6to13.RecordSample("StartingART6to13.txt");
	StartingART6to9.RecordSample("StartingART6to9.txt");
	StartingART5to14.RecordSample("StartingART5to14.txt");
	StartingART10to14.RecordSample("StartingART10to14.txt");
	StartingART15to24M.RecordSample("StartingART15to24M.txt");
	StartingART25to34M.RecordSample("StartingART25to34M.txt");
	StartingART35to44M.RecordSample("StartingART35to44M.txt");
	StartingART45M.RecordSample("StartingART45M.txt");
	StartingART15to24F.RecordSample("StartingART15to24F.txt");
	StartingART25to34F.RecordSample("StartingART25to34F.txt");
	StartingART35to44F.RecordSample("StartingART35to44F.txt");
	StartingART45F.RecordSample("StartingART45F.txt");
	StartingART_M15.RecordSample("StartingART_M15.txt");
	StartingART_F15.RecordSample("StartingART_F15.txt");
	StartingARTtot.RecordSample("StartingARTtot.txt");
	ARTcoverageAdult.RecordSample("ARTcoverageAdult.txt");
	ARTcoverageDiag.RecordSample("ARTcoverageDiag.txt");
	ARTcoverageDiag15.RecordSample("ARTcoverageDiag15.txt");
	ARTcoverageDiagF.RecordSample("ARTcoverageDiagF.txt");
	ARTcoverageDiagM.RecordSample("ARTcoverageDiagM.txt");
	ARTcoverageMSM.RecordSample("ARTcoverageMSM.txt");
	ARTcoverageFSW.RecordSample("ARTcoverageFSW.txt");
	/*NewARTunder200.RecordSample("NewARTunder200.txt");
	NewART200to349.RecordSample("NewART200to349.txt");
	NewART350to499.RecordSample("NewART350to499.txt");
	NewARTover500.RecordSample("NewARTover500.txt");*/
	TotalART15F.RecordSample("TotalART15F.txt");
	TotalART15M.RecordSample("TotalART15M.txt");
	TotalART15F2L.RecordSample("TotalART15F2L.txt");
	TotalART15M2L.RecordSample("TotalART15M2L.txt");
	TotalART1to2.RecordSample("TotalART1to2.txt");
	TotalART3to5.RecordSample("TotalART3to5.txt");
	TotalART6to9.RecordSample("TotalART6to9.txt");
	TotalART10to14.RecordSample("TotalART10to14.txt");
	MSMonART.RecordSample("MSMonART.txt");

	/*TotalARTunder15.RecordSample("TotalARTunder15.txt");
	TotUnmet15F.RecordSample("TotUnmet15F.txt");
	TotUnmet15M.RecordSample("TotUnmet15M.txt");
	TotUnmetUnder15.RecordSample("TotUnmetUnder15.txt");
	TotNewNeed15F.RecordSample("TotNewNeed15F.txt");
	TotNewNeed15M.RecordSample("TotNewNeed15M.txt");*/
	VLsuppressed.RecordSample("VLsuppressed.txt");
	VLsuppressed15.RecordSample("VLsuppressed15.txt");
	VLsuppressed15total.RecordSample("VLsuppressed15total.txt");
	VLunsuppressed15total.RecordSample("VLunsuppressed15total.txt");
	VLunsuppressed15.RecordSample("VLunsuppressed15.txt");
	/*ARTerror.RecordSample("ARTerror.txt");
	AdultRootM.RecordSample("AdultRootM.txt");
	AdultRootF.RecordSample("AdultRootF.txt");
	AgeDbnOnART_M.RecordSample("AgeDbnOnART_M.txt");
	AgeDbnOnART_F.RecordSample("AgeDbnOnART_F.txt");*/
	AdultARTinterrupters.RecordSample("AdultARTinterrupters.txt");
	AdultInterruptPropn.RecordSample("AdultInterruptPropn.txt");
	

	// Write other outputs to text files
	TotPop.RecordSample("TotPop.txt");
	/*TotBirthsHIV.RecordSample("TotBirthsHIV.txt");
	TotBirthsART.RecordSample("TotBirthsART.txt");
	NegChildrenU15.RecordSample("NegChildrenU15.txt");
	Neg15to49.RecordSample("Neg15to49.txt");
	Neg15to24F.RecordSample("Neg15to24F.txt");
	Neg15to24M.RecordSample("Neg15to24M.txt");
	Neg25to49F.RecordSample("Neg25to49F.txt");
	Neg25to49M.RecordSample("Neg25to49M.txt");
	Neg50F.RecordSample("Neg50F.txt");
	Neg50M.RecordSample("Neg50M.txt");*/
	FSWcondomUse.RecordSample("FSWcondomUse.txt");
	/*PrevTested05.RecordSample("PrevTested05.txt");
	PrevTested08.RecordSample("PrevTested08.txt");
	PrevTested09.RecordSample("PrevTested09.txt");
	PrevTested12.RecordSample("PrevTested12.txt");
	PrevTested16.RecordSample("PrevTested16.txt");
	PrevTested17.RecordSample("PrevTested17.txt");
	AdultsEverTested.RecordSample("AdultsEverTested.txt");*/
	//TestingBias.RecordSample("TestingBias.txt");
	TotalHIVtests.RecordSample("TotalHIVtests.txt");
	/*HIVtestsPos.RecordSample("HIVtestsPos.txt");
	TotalHIVtestsU15.RecordSample("TotalHIVtestsU15.txt");
	HIVtestsPosU15.RecordSample("HIVtestsPosU15.txt");
	FalseNegPropn.RecordSample("FalseNegPropn.txt");*/
	// FirstHIVtestsPos.RecordSample("FirstHIVtestsPos.txt");
    Number1stHIVtestsPos.RecordSample("Number1stHIVtestsPos.txt");
    Prop1stHIVtestsPos.RecordSample("Prop1stHIVtestsPos.txt");
	AdultHIVtestsPos.RecordSample("AdultHIVtestsPos.txt");
	AdultHIVtestsNeg.RecordSample("AdultHIVtestsNeg.txt");
	/*TotSTestFixedPoint.RecordSample("TotSTestFixedPoint.txt");
	TotSTestTaxi.RecordSample("TotSTestTaxi.txt");
	TotSTestANC.RecordSample("TotSTestANC.txt");
	TotSTestIndex.RecordSample("TotSTestIndex.txt");
	TotSTestWork1.RecordSample("TotSTestWork1.txt");
	TotSTestWork2.RecordSample("TotSTestWork2.txt");
	PosSTestFixedPoint.RecordSample("PosSTestFixedPoint.txt");
	PosSTestTaxi.RecordSample("PosSTestTaxi.txt");
	PosSTestANC.RecordSample("PosSTestANC.txt");
	PosSTestIndex.RecordSample("PosSTestIndex.txt");
	PosSTestWork1.RecordSample("PosSTestWork1.txt");
	PosSTestWork2.RecordSample("PosSTestWork2.txt");
	STtoARTfixedPoint.RecordSample("STtoARTfixedPoint.txt");
	STtoARTtaxi.RecordSample("STtoARTtaxi.txt");
	STtoART_ANC.RecordSample("STtoART_ANC.txt");
	STtoARTindex.RecordSample("STtoARTindex.txt");
	STtoARTwork1.RecordSample("STtoARTwork1.txt");
	STtoARTwork2.RecordSample("STtoARTwork2.txt");
	STuptakeByYr.RecordSample("STuptakeByYr.txt");
	HIVtestUptakeF25.RecordSample("HIVtestUptakeF25.txt");
	OItestingRate.RecordSample("OItestingRate.txt");
	ProbTestedNextYr.RecordSample("ProbTestedNextYr.txt");*/
	DiagnosedHIV_M.RecordSample("DiagnosedHIV_M.txt");
	DiagnosedHIV_F.RecordSample("DiagnosedHIV_F.txt");
	/*UndiagnosedHIV_M.RecordSample("UndiagnosedHIV_M.txt");
	UndiagnosedHIV_F.RecordSample("UndiagnosedHIV_F.txt");
	UndiagnosedHIV_U15.RecordSample("UndiagnosedHIV_U15.txt");
	Undiagnosed2012.RecordSample("Undiagnosed2012.txt");
	DiagnosedUntreated2012.RecordSample("DiagnosedUntreated2012.txt");
	Treated2012.RecordSample("Treated2012.txt");
	UntreatedByCD4_2012.RecordSample("UntreatedByCD4_2012.txt");
	PaedCascade2018.RecordSample("PaedCascade2018.txt");
	OutRRdiagDeathsPIP.RecordSample("OutRRdiagDeathsPIP.txt");
	NewAIDSdiagTrend.RecordSample("NewAIDSdiagTrend.txt");
	NewAIDSdiagAge.RecordSample("NewAIDSdiagAge.txt");
	MSMpropn18to24.RecordSample("MSMpropn18to24.txt");
	MarriedPropn1996.RecordSample("MarriedPropn1996.txt");
	MarriedPropn2001.RecordSample("MarriedPropn2001.txt");
	MarriedPropn2007.RecordSample("MarriedPropn2007.txt");
	MarriedPropn2016.RecordSample("MarriedPropn2016.txt");*/

	// Investment case outputs
    // NonAIDSdeaths.RecordSample("NonAIDSdeaths.txt");
    // AIDSdeathsART.RecordSample("AIDSdeathsART.txt");
	LYlostAIDS.RecordSample("LYlostAIDS.txt");
	TotBirths.RecordSample("TotBirths.txt");
	MalesOver15.RecordSample("MalesOver15.txt");
	FemalesOver15.RecordSample("FemalesOver15.txt");
	MarriedM17to49.RecordSample("MarriedM17to49.txt");
	MarriedF17to49.RecordSample("MarriedF17to49.txt");
	MarriedM50.RecordSample("MarriedM50.txt");
	MarriedF50.RecordSample("MarriedF50.txt");
	/*TotInfants.RecordSample("TotInfants.txt");
	Children1to2.RecordSample("Children1to2.txt");
	Children3to5.RecordSample("Children3to5.txt");
	Children6to13.RecordSample("Children6to13.txt");
	Adolesc15to19.RecordSample("Adolesc15to19.txt");
	Children6to18.RecordSample("Children6to18.txt");
	Total15to24.RecordSample("Total15to24.txt");
	TotPaedHIV.RecordSample("TotPaedHIV.txt");
	Prev15to24M.RecordSample("Prev15to24M.txt");
	Prev15to24F.RecordSample("Prev15to24F.txt");
	Prev15to49M.RecordSample("Prev15to49M.txt");
	Prev15to49F.RecordSample("Prev15to49F.txt");
	NewARTunder200F.RecordSample("NewARTunder200F.txt");
	NewART200to349F.RecordSample("NewART200to349F.txt");
	NewART350to499F.RecordSample("NewART350to499F.txt");
	NewARTover500F.RecordSample("NewARTover500F.txt");
	StartingART1to2.RecordSample("StartingART1to2.txt");
	StartingART3to5.RecordSample("StartingART3to5.txt");
	StartingART6to13.RecordSample("StartingART6to13.txt");
	TotLateUnder15.RecordSample("TotLateUnder15.txt");
	TotEarlyInfants.RecordSample("TotEarlyInfants.txt");
	TotEarly1to4.RecordSample("TotEarly1to4.txt");*/
	PreARTunder200M.RecordSample("PreARTunder200M.txt");
	PreART200to349M.RecordSample("PreART200to349M.txt");
	PreART350to499M.RecordSample("PreART350to499M.txt");
	PreARTover500M.RecordSample("PreARTover500M.txt");
	PreARTunder200F.RecordSample("PreARTunder200F.txt");
	PreART200to349F.RecordSample("PreART200to349F.txt");
	PreART350to499F.RecordSample("PreART350to499F.txt");
	PreARTover500F.RecordSample("PreARTover500F.txt");
	/*DiscontinuedART_M.RecordSample("DiscontinuedART_M.txt");
	DiscontinuedART_F.RecordSample("DiscontinuedART_F.txt");
	TotNewNeed500M.RecordSample("TotNewNeed500M.txt");
	TotNewNeed500F.RecordSample("TotNewNeed500F.txt");*/
	TotSexActs.RecordSample("TotSexActs.txt");
	TotProtSexActs.RecordSample("TotProtSexActs.txt");
	CondomUse15to24F.RecordSample("CondomUse15to24F.txt");
	CondomUse25to49F.RecordSample("CondomUse25to49F.txt");
	//TotProtSexActs18.RecordSample("TotProtSexActs18.txt");
	MMC10to14.RecordSample("MMC10to14.txt");
	MMC15to19.RecordSample("MMC15to19.txt");
	MMC20to24.RecordSample("MMC20to24.txt");
	MMC25to49.RecordSample("MMC25to49.txt");
	MMCover50.RecordSample("MMCover50.txt");
	Circumcised15to49.RecordSample("Circumcised15to49.txt");
	/*AdultsEverTestedM.RecordSample("AdultsEverTestedM.txt");
	AdultsEverTestedF.RecordSample("AdultsEverTestedF.txt");
	BirthsDiagHIV.RecordSample("BirthsDiagHIV.txt");
	BirthsOver500.RecordSample("BirthsOver500.txt");
	Births350to499.RecordSample("Births350to499.txt");
	Births200to349.RecordSample("Births200to349.txt");
	BirthsUnder200.RecordSample("BirthsUnder200.txt");*/
	TotSexWorkers.RecordSample("TotSexWorkers.txt");
	SWsexActs.RecordSample("SWsexActs.txt");
	SWsexActsProt.RecordSample("SWsexActsProt.txt");
	MenOnPrEP.RecordSample("MenOnPrEP.txt");
	WomenOnPrEP.RecordSample("WomenOnPrEP.txt");
	FSWonPrEP.RecordSample("FSWonPrEP.txt");
	MSMonPrEP.RecordSample("MSMonPrEP.txt");
	AGYWonPrEP.RecordSample("AGYWonPrEP.txt");
	GenAdultOnPrEP.RecordSample("GenAdultOnPrEP.txt");
	PrEPcoverageFSW.RecordSample("PrEPcoverageFSW.txt");
	PrEPcoverageMSM.RecordSample("PrEPcoverageMSM.txt");
	PrEPeligibleMSM.RecordSample("PrEPeligibleMSM.txt");
	PrEPcoverageAGYW.RecordSample("PrEPcoverageAGYW.txt");
	PrEPcoverageAll.RecordSample("PrEPcoverageAll.txt");
	NewPrEP_M.RecordSample("NewPrEP_M.txt");
	NewPrEP_F.RecordSample("NewPrEP_F.txt");
	//WomenOnVM.RecordSample("WomenOnVM.txt");
	AdolescOnPrEP.RecordSample("AdolescOnPrEP.txt");
	FSWonART.RecordSample("FSWonART.txt");
	//DiscordantARTelig.RecordSample("DiscordantARTelig.txt");
	//DiscordantPrEPelig.RecordSample("DiscordantPrEPelig.txt");*/
	OnARTover500.RecordSample("OnARTover500.txt");
	OnART350to499.RecordSample("OnART350to499.txt");
	OnART200to349.RecordSample("OnART200to349.txt");
	OnARTunder200.RecordSample("OnARTunder200.txt");
	DiscARTover500.RecordSample("DiscARTover500.txt");
	DiscART350to499.RecordSample("DiscART350to499.txt");
	DiscART200to349.RecordSample("DiscART200to349.txt");
	DiscARTunder200.RecordSample("DiscARTunder200.txt");
	/*AdolRegTests.RecordSample("AdolRegTests.txt");
	ChildrenOnExtNVP.RecordSample("ChildrenOnExtNVP.txt");
	TotBirthDiagnosed.RecordSample("TotBirthDiagnosed.txt");*/

	/*GetSummaryOutputs("SummaryOutput.txt");
	GetAddedOutputs("AddnalOutput.txt");*/
	GetOutputsByAge("OutputByAge.txt");
}

void ReadPriors()
{
	int ir, ii, idum;
	std::ifstream file;

	ii = 0;

	file.open("Priors.txt");
	if (file.fail()) {
      std::cerr << "Could not open Priors.txt\n";
      exit(1);
    }

	for(ir=0; ir<MaxPriors; ir++){
		file.ignore(255,'\n');
		file.ignore(255,'\n');
		file>>InclPriors[ir][0];
		if(InclPriors[ir][0]==1){
			file>>PriorTypes[ii]>>aParameters[ii]>>bParameters[ii];
			InclPriors[ir][1] = ii;
			ii += 1;
		}
	}
	file.close();

	if(ii!=MCMCdim){
		std::cout<<"Error: Number of priors included is inconsistent with MCMCdim."<<std::endl;}
}

void ReadImportanceDbns()
{
	int ir;
	std::ifstream file;

	file.open("ImportanceDbns.txt");
	if (file.fail()) {
		std::cerr << "Could not open ImportanceDbns.txt\n";
		exit(1);
	}

	for (ir = 0; ir<MCMCdim; ir++){
		file.ignore(255, '\n');
		file.ignore(255, '\n');
		file >> aParameters2[ir] >> bParameters2[ir];
	}
	file.close();
}

void ReadPrevIMIS()
{
	std::ifstream file1;
	std::ifstream file2;
	std::ifstream file3;
	std::ifstream file4;
	std::ifstream file5;
	std::ifstream file6;
	int ir, ic, ii, totsim;

	totsim = InitSample + StepSample * (CurrIMISstep - 1);

	file1.open("MixtureMean.txt");
	if (file1.fail()) {
      std::cerr << "Could not open MixtureMean.txt\n";
      exit(1);
    }
	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<=CurrIMISstep; ic++){
			file1>>MixtureMean[ic][ir];}
	}
	file1.close();

	file2.open("Inverse.txt");
	if (file2.fail()) {
      std::cerr << "Could not open Inverse.txt\n";
      exit(1);
    }
	for(ii=0; ii<=CurrIMISstep; ii++){
		file2.ignore(255,'\n');
		for(ir=0; ir<MCMCdim; ir++){
			for(ic=0; ic<MCMCdim; ic++){
				file2>>invIMIS[ir][ic][ii];}
		}
		file2.ignore(255,'\n');
	}
	file2.close();

	file3.open("Cholesky.txt");
	if (file3.fail()) {
      std::cerr << "Could not open Cholesky.txt\n";
      exit(1);
    }
	for(ii=0; ii<=CurrIMISstep; ii++){
		file3.ignore(255,'\n');
		for(ir=0; ir<MCMCdim; ir++){
			for(ic=0; ic<MCMCdim; ic++){
				file3>>cholIMIS[ir][ic][ii];}
		}
		file3.ignore(255,'\n');
	}
	file3.close();

	file4.open("PosteriorMeans.txt");
	if (file4.fail()) {
      std::cerr << "Could not open PosteriorMeans.txt\n";
      exit(1);
    }
	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<CurrIMISstep; ic++){
			file4>>PosteriorMean[ic][ir];}
	}
	file4.close();

	file5.open("RandomParameter.txt");
	if (file5.fail()) {
      std::cerr << "Could not open RandomParameter.txt\n";
      exit(1);
    }
	for(ir=0; ir<totsim; ir++){
		for(ic=0; ic<MCMCdim+1; ic++){
			file5>>RandomParameterIMIS[ic][ir];}
	}
	file5.close();

	file6.open("FractionUnique.txt");
	if (file6.fail()) {
      std::cerr << "Could not open FractionUnique.txt\n";
      exit(1);
    }
	for(ir=0; ir<CurrIMISstep; ir++){
		file6>>FractionUnique[ir]>>LogIntegratedL[ir];}
	file6.close();

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			Cholesky1[ir][ic] = cholIMIS[ir][ic][CurrIMISstep];}
		NextMode[ir] = MixtureMean[CurrIMISstep][ir];
	}
}

void ReadPrevIMIS2(double completed)
{
	std::ifstream file5;
	int ir, ic, totsim;

	totsim = InitSample * completed;

	file5.open("RandomParameter.txt");
	if (file5.fail()) {
		std::cerr << "Could not open RandomParameter.txt\n";
		exit(1);
	}
	for (ir = 0; ir<totsim; ir++){
		for (ic = 0; ic<MCMCdim + 1; ic++){
			file5 >> RandomParameterIMIS[ic][ir];
		}
	}
	file5.close();
}

void SaveTempIMIS()
{
	int ir, ic, ii, totsim;

	std::ofstream file1("MixtureMean.txt");
	std::ofstream file2("Inverse.txt");
	std::ofstream file3("Cholesky.txt");
	std::ofstream file4("PosteriorMeans.txt");
	std::ofstream file5("RandomParameter.txt");
	std::ofstream file6("FractionUnique.txt");

	totsim = InitSample + StepSample * CurrIMISstep;

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<=CurrIMISstep+1; ic++){
			file1<<"	"<<std::setw(15)<<std::right<<MixtureMean[ic][ir];}
		file1<<std::endl;
	}
	file1.close();

	for(ii=0; ii<=CurrIMISstep+1; ii++){
		file2<<"invIMIS "<<ii<<std::endl;
		for(ir=0; ir<MCMCdim; ir++){
			for(ic=0; ic<MCMCdim; ic++){
				file2<<"	"<<std::setw(15)<<std::right<<invIMIS[ir][ic][ii];}
			file2<<std::endl;
		}
	}
	file2.close();

	for(ii=0; ii<=CurrIMISstep+1; ii++){
		file3<<"cholIMIS "<<ii<<std::endl;
		for(ir=0; ir<MCMCdim; ir++){
			for(ic=0; ic<MCMCdim; ic++){
				file3<<"	"<<std::setw(15)<<std::right<<cholIMIS[ir][ic][ii];}
			file3<<std::endl;
		}
	}
	file3.close();

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<=CurrIMISstep; ic++){
			file4<<"	"<<std::setw(15)<<std::right<<PosteriorMean[ic][ir];}
		file4<<std::endl;
	}
	file4.close();

	for(ir=0; ir<totsim; ir++){
		for(ic=0; ic<MCMCdim+1; ic++){
			file5<<"	"<<std::setw(15)<<std::right<<RandomParameterIMIS[ic][ir];}
		file5<<std::endl;
	}
	file5.close();

	for(ir=0; ir<=CurrIMISstep; ir++){
		file6<<FractionUnique[ir]<<"	"<<LogIntegratedL[ir]<<std::endl;}
	file6.close();
}

void SaveTempIMIS2(double completed)
{
	int ir, ic, totsim;

	std::ofstream file5("RandomParameter.txt");

	totsim = InitSample * completed;

	for (ir = 0; ir<totsim; ir++){
		for (ic = 0; ic<MCMCdim + 1; ic++){
			file5 << "	" << std::setw(15) << std::right << RandomParameterIMIS[ic][ir];
		}
		file5 << std::endl;
	}
	file5.close();
}

void GetMultNorm(double RandUnif[MCMCdim], double MultNorm[MCMCdim])
{
	// This function converts a vector of random variables from uniform (0, 1) into
	// a vector of multivariate normal random variables (2nd argument).

	int i, j, ind, s;
	double x, y, a, b, p, q, bound;
	double RandStdNorm[MCMCdim];

	ind = 2;

	for(i=0; i<MCMCdim; i++){
		a = 0.0; // mean of std normal
		b = 1.0; // std deviation of std normal
		p = RandUnif[i];
		q = 1 - RandUnif[i];
		s = 0; // Seems we need to set s and bound to 0 to avoid error messages
		bound = 0.0;
		cdfnor(&ind,&p,&q,&x,&a,&b,&s,&bound);
		RandStdNorm[i] = x;
	}
	for(i=0; i<MCMCdim; i++){
		MultNorm[i] = NextMode[i];
		for(j=0; j<MCMCdim; j++){
			MultNorm[i] += RandStdNorm[j] * Cholesky1[j][i];}
	}
}

double GetMultNormPDF(double MultNorm[MCMCdim], int Component)
{
	// This returns the natural log of the prob density function (PDF) for the multivariate
	// normal evaluated at the currently sampled parameter combination. The Component
	// argument determines which of the components of the mixture is used to determine
	// the multivariate normal parameters

	int ir, ic;
	double Inverse[MCMCdim][MCMCdim]; // Inverse of the covariance matrix
	double InverseX[MCMCdim]; // Inverse * (X - mu)
	double PDF, determinant;

	// Get the inverse of the relevant covariance matrix
	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			Inverse[ir][ic] = invIMIS[ir][ic][Component];
		}
	}

	// Get the determinant of the covariance matrix
	// Since det(AB) = det(A)*det(B) and det(A) = product of the diagonal elements of A
	// if A is triangular, it is easy to get the determinant of the covariance matrix
	// from the Cholesky decomposition
	determinant = 1.0;
	for(ir=0; ir<MCMCdim; ir++){
		determinant *= pow(cholIMIS[ir][ir][Component], 2.0);}

	// Calculate the exponent of the density
	PDF = 0.0;
	for(ir=0; ir<MCMCdim; ir++){
		InverseX[ir] = 0.0;
		for(ic=0; ic<MCMCdim; ic++){
			InverseX[ir] += Inverse[ir][ic] * (MultNorm[ic] - MixtureMean[Component][ic]);}
		PDF += (MultNorm[ir] - MixtureMean[Component][ir]) * InverseX[ir];
	}
	PDF = -PDF/2.0;

	// Update the PDF variable to reflect determinant etc
	PDF = PDF - 0.5 * log(determinant) - log(2.0 * 3.141592654) * MCMCdim/2.0;

	return PDF;
}

void GetMahalanobis(double distance[TotalSimulations])
{
	// This function calculates the Mahalanobis distance, which is stored in the 'distance'
	// array. Note that  the covariance matrix corresponds to the priors, which are
	// independent of one another, and this makes it relatively easy to calculate the
	// inverse of the covariance matrix (diagonal entries are just inverses of variances).
	// The distance is calculated for each parameter combination in the RandomParameters
	// array, relative to the NextMode vector.

	int i, j;
	double var[MCMCdim], mean;

	for(j=0; j<MCMCdim; j++){
		if(PriorTypes[j]==0){ // Beta prior
			var[j] = aParameters[j] * bParameters[j]/(pow(aParameters[j] + bParameters[j],
				2.0) * (aParameters[j] + bParameters[j] + 1.0));
			// Now apply the delta method to get the variance on the logit scale.
			mean = aParameters[j]/(aParameters[j] + bParameters[j]);
			var[j] *= pow(mean * (1.0 - mean), -2.0);
		}
		if(PriorTypes[j]==1){ // Gamma prior
			var[j] = aParameters[j]/pow(bParameters[j], 2.0);
			// Now apply the delta method to get the variance on the natural log scale.
			mean = aParameters[j]/bParameters[j];
			var[j] *= pow(mean, -2.0);
		}
		if(PriorTypes[j]==2){ // Normal prior
			mean = aParameters[j];
			var[j] = pow(bParameters[j], 2.0);
		}
	}

	for(i=0; i<InitSample + CurrIMISstep * StepSample; i++){
		distance[i] = 0.0;
		for(j=0; j<MCMCdim; j++){
			distance[i] += pow(RandomParameterIMIS[j][i] - NextMode[j], 2.0)/var[j];}
		distance[i] = sqrt(distance[i]);
	}
}

double GetPercentile(double values[TotalSimulations], double percentile)
{
	// Returns the percentile for the specified array. Note that although TotalSimulations
	// is the dimension of the arrays, the percentile is only calculated with respect to
	// the first n elements of the array, where n = InitSample + CurrIMISstep * StepSample

	int i, j, k, offset;

	sorted[0] = values[0];
	for(i=1; i<InitSample + CurrIMISstep * StepSample; i++){
		if(values[i]<sorted[0]){
			for(j=i; j>0; j--){sorted[j] = sorted[j-1];}
			sorted[0] = values[i];
		}
		else if(values[i]>=sorted[i-1]){
			sorted[i] = values[i];}
		else for(j=0; j<InitSample; j++){
			if(values[i]>=sorted[j] && values[i]<sorted[j+1]){
				for(k=i; k>j+1; k--){
					sorted[k] = sorted[k-1];}
				sorted[j+1] = values[i];
				break;
			}
		}
	}
	offset = (InitSample + CurrIMISstep * StepSample) * percentile;

	return sorted[offset];
}

void runIMIS(double CumSteps)
{
	int i, j, k, ir, ic, totsim;
	double WeightedMean[MCMCdim], sumweights, sumweights2, denom;
	double WeightedCov[MCMCdim][MCMCdim], Inverse[MCMCdim][MCMCdim];
	//double distance[TotalSimulations]; // Mahalanobis distance
	double DistancePropn, cutoff, minweight;
	char filout[18];
	double lookup, r[ResampleSize];
	double MaxLogLxWeight, x1, DoubleIntDif1, intpart1;
	//double CumLxWeight[TotalSimulations+1];

	ReadPriors();
	if (ImportanceSamplingStep1 == 1){ ReadImportanceDbns(); }
	if(CumSteps>0){
		CurrIMISstep = CumSteps;
		if (CumSteps >= 1.0){ ReadPrevIMIS(); }
		else{ ReadPrevIMIS2(CumSteps); }
	}
	std::cout<<"Finished reading inputs"<<std::endl;
	totsim = TotalSimulations;
	for(i=CumSteps; i<IMISsteps; i++){
		//if(ErrorInd==1){break;}
		CurrIMISstep = i;
		OneIMISstep(CumSteps+i);

		// Check if the convergence criterion has been met and calculate LogIntegratedL
		sumweights = 0.0;
		sumweights2 = 0.0;
		MaxLogLxWeight = -100000.0; // Arbitrarily low value
		FractionUnique[i] = 0.0;
		for(k=0; k<InitSample + i * StepSample; k++){
			if(LogLxWeight[k] > MaxLogLxWeight){
				MaxLogLxWeight = LogLxWeight[k];}
		}
		for(k=0; k<InitSample + i * StepSample; k++){
			weights[k] = exp(LogLxWeight[k] - MaxLogLxWeight);
			sumweights += weights[k];
		}
		for(k=0; k<InitSample + i * StepSample; k++){
			weights[k] = weights[k]/sumweights;
			sumweights2 += pow(weights[k], 2.0);
			for(j=0; j<MCMCdim; j++){
				PosteriorMean[i][j] += weights[k] * RandomParameterIMIS[j][k];}
			FractionUnique[i] += 1.0 - pow(1.0 - weights[k], ResampleSize);
		}
		EffectiveSS[i] = 1.0/sumweights2;
		Efficiency[i] = EffectiveSS[i]/(InitSample + i * StepSample);
		FractionUnique[i] = FractionUnique[i]/ResampleSize;
		denom = 1.0 * (InitSample + i * StepSample);
		LogIntegratedL[i] = log(sumweights) + MaxLogLxWeight - log(denom);
		//if(FractionUnique[i] >= 1.0 - exp(-1.0)){
		if (FractionUnique[i] >= 0.4){
			totsim = InitSample + i * StepSample;
			break;
		}

		// Calculate the new mixture components for the next IMIS step
		if(CurrIMISstep<IMISsteps-1){
			// (a) Calculate weights
			// NB! These are NOT the weights that are used in the resample step; they are
			// the weights used for calculating the weighted covariance matrix
			GetMahalanobis(Distance);
			DistancePropn = 1.0 * StepSample/(InitSample + i * StepSample);
			cutoff = GetPercentile(Distance, DistancePropn);
			minweight = 1.0/(InitSample + i * StepSample);
			sumweights = 0.0;
			sumweights2 = 0.0;
			for(k=0; k<InitSample + i * StepSample; k++){
				if(Distance[k]<cutoff){
					weights[k] = 0.5 * (weights[k] + minweight);}
				else{
					weights[k] = 0.0;}
				sumweights += weights[k];
			}
			for(k=0; k<InitSample + i * StepSample; k++){
				weights[k] = weights[k]/sumweights;
				sumweights2 += pow(weights[k], 2.0);
			}
			// (b) Calculate weighted means
			for(j=0; j<MCMCdim; j++){
				MixtureMean[CurrIMISstep+1][j] = NextMode[j];
				WeightedMean[j] = 0.0;
				for(k=0; k<InitSample + i * StepSample; k++){
					WeightedMean[j] += RandomParameterIMIS[j][k] * weights[k];
				}
			}
			// (c) Next calculate the weighted covariance matrix
			for(ir=0; ir<MCMCdim; ir++){
				for(ic=0; ic<MCMCdim; ic++){
					WeightedCov[ir][ic] = 0.0;
					for(k=0; k<InitSample + i * StepSample; k++){
						WeightedCov[ir][ic] += weights[k] * (RandomParameterIMIS[ir][k] -
							WeightedMean[ir]) * (RandomParameterIMIS[ic][k] -
							WeightedMean[ic]);
					}
					WeightedCov[ir][ic] = WeightedCov[ir][ic]/(1 - sumweights2);
				}
			}
			// (d) Then get the Cholesky decomposition & inverse of the weighted covar matrix
			GetCholesky1(WeightedCov);
			for(ir=0; ir<MCMCdim; ir++){
				for(ic=0; ic<MCMCdim; ic++){
					cholIMIS[ir][ic][i+1] = Cholesky1[ir][ic];
				}
			}
			GetInverse1(WeightedCov, Inverse);
			for(ir=0; ir<MCMCdim; ir++){
				for(ic=0; ic<MCMCdim; ic++){
					invIMIS[ir][ic][i+1] = Inverse[ir][ic];
				}
			}
		}
		std::cout<<"Finished IMIS step "<<i+1<<", FractionUnique = "<<FractionUnique[i]<<std::endl;
		// Save intermediate results
		/*x1 = CurrIMISstep/5.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			SaveTempIMIS();}*/
		SaveTempIMIS();
	}

	// Based on the code in the GenerateSample function

	// Calculate likelihood for each simulation
	CumLxWeight[0] = 0.0;
	for(i=0; i<totsim; i++){
		//CumLxWeight[i+1] = CumLxWeight[i] + exp(LogLxWeight[i] - MaxLogLxWeight) *
		//	(1.0 - ARTerrorHistory[i]);
		CumLxWeight[i + 1] = CumLxWeight[i] + exp(LogLxWeight[i] - MaxLogLxWeight);
	}

	// Generate random variables from the uniform (0, 1) distribution
	int seed = 3055;
	CRandomMersenne rg(seed);
	for(i=0; i<ResampleSize; i++){
		r[i] = rg.Random();}

	// Sample from the simulations and record ID numbers of the sampled simulations
	// in the sampleid array
	for(i=0; i<ResampleSize; i++){
		lookup = r[i]*CumLxWeight[totsim];
		for(j=0; j<TotalSimulations; j++){
			if(lookup>=CumLxWeight[j] && lookup<CumLxWeight[j+1]){
				SampleID[i] = j;
				break;
			}
		}
	}

	// Generate sample
	for(i=0; i<ResampleSize; i++){
		for(j=0; j<ModelParameters.columns; j++){
			if(PriorTypes[j]==0){
				ModelParameters.out[i][j] = 1.0/(1.0 + exp(-RandomParameterIMIS[j][SampleID[i]]));}
			else if(PriorTypes[j]==1){
				ModelParameters.out[i][j] = exp(RandomParameterIMIS[j][SampleID[i]]);}
			else{
				ModelParameters.out[i][j] = RandomParameterIMIS[j][SampleID[i]];}
		}
	}
	ModelParameters.RecordSample("ModelParameters.txt", 8);
	if (ProvModel == 1){ ARTerror.RecordSample("ARTerror.txt"); }

	std::ofstream file1("WeightedCov.txt");
	std::ofstream file2("Inverse1.txt");
	std::ofstream file3("Cholesky1.txt");
	std::ofstream file4("PosteriorMeans.txt");
	std::ofstream file5("RandomParameter.txt");
	std::ofstream file6("Weights.txt");
	std::ofstream file7("LogLxWeight.txt");
	std::ofstream file8("Diagnostics.txt");
	std::ofstream file9("Distance.txt");
	std::ofstream file10("Sorted.txt");
	std::ofstream file11("NextMode.txt");

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			file1<<"	"<<std::setw(15)<<std::right<<WeightedCov[ir][ic];}
		file1<<std::endl;
	}
	file1.close();

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			file2<<"	"<<std::setw(15)<<std::right<<invIMIS[ir][ic][1];}
		file2<<std::endl;
	}
	file2.close();

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<MCMCdim; ic++){
			file3<<"	"<<std::setw(15)<<std::right<<cholIMIS[ir][ic][1];}
		file3<<std::endl;
	}
	file3.close();

	for(ir=0; ir<MCMCdim; ir++){
		for(ic=0; ic<IMISsteps; ic++){
			file4<<"	"<<std::setw(15)<<std::right<<PosteriorMean[ic][ir];}
		file4<<std::endl;
	}
	file4.close();

	for(ir=0; ir<totsim; ir++){
		for(ic=0; ic<MCMCdim+1; ic++){
			file5<<"	"<<std::setw(15)<<std::right<<RandomParameterIMIS[ic][ir];}
		file5<<std::endl;
	}
	file5.close();

	for(ir=0; ir<totsim; ir++){
		file6<<weights[ir]<<std::endl;}
	file6.close();

	for(ir=0; ir<totsim; ir++){
		file7<<LogLxWeight[ir]<<std::endl;}
	file7.close();

	for(ir=0; ir<IMISsteps; ir++){
		file8<<FractionUnique[ir]<<"	"<<LogIntegratedL[ir]<<"	"<<EffectiveSS[ir]<<"	"<<Efficiency[ir]<<std::endl;}
	file8.close();

	for(ir=0; ir<totsim; ir++){
		file9<<Distance[ir]<<std::endl;}
	file9.close();

	for(ir=0; ir<totsim; ir++){
		file10<<sorted[ir]<<std::endl;}
	file10.close();

	for(ir=0; ir<MCMCdim + 1; ir++){
		file11<<NextMode[ir]<<std::endl;}
	file11.close();
}

void OneIMISstep(double CumSteps)
{
	int i, j, k, iy, NewSimulations, Nk, offset;
	double SamplingDensity[IMISsteps], PriorDensity, MultNorm[MCMCdim], TransfNorm[MCMCdim];
	double denominator, x, y, a, b, x1, DoubleIntDif1, intpart1, PriorDensity2;
	double x2, DoubleIntDif2, intpart2, completed;

	CurrSim = 0;
	if (CumSteps < 1.0){
		CurrSim = 1.0 * InitSample * CumSteps;
		NewSimulations = 1.0 * InitSample * (1.0 - CumSteps);
	}
	else{
		NewSimulations = StepSample;}
	for(i=1; i<=NewSimulations; i++){
		CurrSim += 1;
		if(CumSteps<1.0){
			offset = CurrSim - 1;
		}
		else{
			offset = InitSample + StepSample * (CurrIMISstep - 1) + CurrSim - 1;}
		CurrYear = StartYear;

		if(i==1){
			ReadAllFiles();}
		if(IMISind==0 || (IMISind==1 && FixedUncertainty==0 && CurrIMISstep==0)){
			SimulateParameters();}
		else{
			SimulateParameters_IMIS();}
		UpdateNonAIDSmort();
		SetInitialParameters();
		SetCD4byARTdur();
		CalcInterruptions();
		SetActivityByStage();
		SetInitSexActivity();
		UpdateMixingST();
		SetCurrYearParameters();
		SetFertByStage();
		SetProgression(0);
		UpdateARTmort();
		ARTerrorInd = 0;
		CurrYear = StartYear-1;
		for(iy=0; iy<ProjectionTerm; iy++){
			OneYear();
		}
		CalcLikelihood();
		LogL.out[i-1][0] = LogLikelihood;
		RandomParameterIMIS[MCMCdim][offset] = LogLikelihood;
		ARTerrorHistory[offset] = ARTerrorInd;
		x1 = offset/100.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			std::cout<<"Completed simulation "<<offset+1<<std::endl;}
		if (CumSteps < 1.0){
			x2 = i / 1000.0;
			DoubleIntDif2 = modf(x2, &intpart2);
			if (DoubleIntDif2 == 0.0){
				completed = 1.0 * CurrSim / InitSample;
				SaveTempIMIS2(completed);
			}
		}
		if(LogLikelihood<0.0 || LogLikelihood >0.0){}
		else{
			std::cout<<"Undefined likelihood for simulation "<<offset+1<<std::endl;
			for (iy = 0; iy < MCMCdim; iy++){
				std::cout << "RandomParameterIMIS["<<iy<<"]: " << RandomParameterIMIS[iy][CurrSim - 1] << std::endl;
			}
			//break;
		}
	}

	// Now recalculate the ratio of prior to sampling density in respect of ALL parameter
	// combinations calculated thus far.
	if(CurrIMISstep>0 || ImportanceSamplingStep1==1){
		Nk = InitSample + CurrIMISstep * StepSample;
		for(i=0; i<Nk; i++){
			PriorDensity = 0.0;
			PriorDensity2 = 0.0;
			for(k=0; k<MCMCdim; k++){
				MultNorm[k] = RandomParameterIMIS[k][i];
				if(PriorTypes[k]==0){TransfNorm[k] = 1.0/(1.0 + exp(-MultNorm[k]));}
				else if(PriorTypes[k]==1){TransfNorm[k] = exp(MultNorm[k]);}
				else{TransfNorm[k] = MultNorm[k];}
			}
			for(j=1; j<=CurrIMISstep; j++){
				SamplingDensity[j] = GetMultNormPDF(MultNorm, j);
				// Calculate the Jacobians (remembering that SamplingDensity is on a log scale)
				for(k=0; k<MCMCdim; k++){
					if(PriorTypes[k]==0){
						SamplingDensity[j] += -log(TransfNorm[k]) - log(1.0 - TransfNorm[k]);}
					else if(PriorTypes[k]==1){
						SamplingDensity[j] += -log(TransfNorm[k]);}
				}
			}
			for(k=0; k<MCMCdim; k++){
				x = TransfNorm[k];
				if(PriorTypes[k]==0){ // Beta prior
					a = aParameters[k];
					b = bParameters[k];
					y = beta_log(&a, &b);
					PriorDensity += log(x) * (aParameters[k]-1.0) + log(1.0-x) *
						(bParameters[k]-1.0) - y;
				}
				if(PriorTypes[k]==1){ // Gamma prior
					a = aParameters[k];
					y = gamma_log(&a);
					PriorDensity += log(bParameters[k]) * a + log(x) * (a-1.0) -
						bParameters[k] * x - y;
				}
				if(PriorTypes[k]==2){ // Normal prior
					PriorDensity += -0.5 * pow((x - aParameters[k])/bParameters[k],
						2.0) - 0.5 * log(2.0 * 3.141592654) - log(bParameters[k]);
				}
				if (ImportanceSamplingStep1 == 1){
					if (PriorTypes[k] == 0){ // Beta prior
						a = aParameters2[k];
						b = bParameters2[k];
						y = beta_log(&a, &b);
						PriorDensity2 += log(x) * (aParameters2[k] - 1.0) + log(1.0 - x) *
							(bParameters2[k] - 1.0) - y;
					}
					if (PriorTypes[k] == 1){ // Gamma prior
						a = aParameters2[k];
						y = gamma_log(&a);
						PriorDensity2 += log(bParameters2[k]) * a + log(x) * (a - 1.0) -
							bParameters2[k] * x - y;
					}
					if (PriorTypes[k] == 2){ // Normal prior
						PriorDensity2 += -0.5 * pow((x - aParameters2[k]) / bParameters2[k],
							2.0) - 0.5 * log(2.0 * 3.141592654) - log(bParameters2[k]);
					}
				}
			}
			if (ImportanceSamplingStep1 == 0){ denominator = 1.0 * InitSample / Nk; }
			else{ denominator = exp(PriorDensity2 - PriorDensity) * InitSample / Nk; }
			for(j=1; j<=CurrIMISstep; j++){
				denominator += exp(SamplingDensity[j] - PriorDensity) * StepSample/Nk;}
			LogLxWeight[i] = RandomParameterIMIS[MCMCdim][i] - log(denominator);
			// Check that LogLxWeight value is reasonable
			if(LogLxWeight[i] > 0.0 || LogLxWeight[i] <= 0.0){
					x = 0.0;} // Arbitrary operation
			else{
				std::cout<<"Error! Improper LogLxWeight - "<<LogLxWeight[i]<<std::endl;
				std::cout<<"LogLikelihood: "<<RandomParameterIMIS[MCMCdim][i]<<std::endl;
				std::cout<<"PriorDensity: "<<PriorDensity<<std::endl;
				for(j=1; j<=CurrIMISstep; j++){
					std::cout<<"SamplingDensity["<<j<<"]: "<<SamplingDensity[j]<<std::endl;}
				std::cout<<"CurrSim: "<<i<<std::endl;
				for(k=0; k<MCMCdim; k++){
					std::cout<<"RandomParameter "<<k<<": "<<RandomParameterIMIS[k][i]<<std::endl;}
				LogLxWeight[i] = -10000.0; // Arbitrarily low value
			}
		}
	}
	else{ // CurrIMISstep = 0 implies that ratio of prior to sampling density is 1
		for(i=0; i<InitSample; i++){
			LogLxWeight[i] = RandomParameterIMIS[MCMCdim][i];}
	}

	// Lastly, calculate NextMode using the updated LogLxWeight values.
	for(i=0; i<InitSample + CurrIMISstep * StepSample; i++){
		if(i==0 || LogLxWeight[i] > NextMode[MCMCdim]){
			NextMode[MCMCdim] = LogLxWeight[i];
			for(k=0; k<MCMCdim; k++){
				NextMode[k] = RandomParameterIMIS[k][i];}
		}
	}
}

void SimulateParameters_IMIS()
{
	int i, is, iy, offset, MCMCindex;
	double Temp, Temp2, Adj1996;
	double r[MCMCdim]; // Random variables from U(0, 1)
	double MultNorm[MCMCdim]; // Sampled parameters from multivariate normal dbn

	if(FixedUncertainty==0){
		offset = InitSample + (CurrIMISstep - 1) * StepSample + CurrSim - 1;
		int seed = 1784 + CurrSim * 73 + (CurrIMISstep * StepSample) * 29;
		CRandomMersenne rg(seed);
		for(i=0; i<MCMCdim; i++){
			r[i] = rg.Random();}
		GetMultNorm(r, MultNorm);
		for(i=0; i<MCMCdim; i++){
			RandomUniform.out[CurrSim-1][i] = r[i];
			RandomParameterIMIS[i][offset] = MultNorm[i];
			// Transform the multivariate normal values back to the original scale
			if(PriorTypes[i]==0){ // Beta prior
				MultNorm[i] = 1.0/(1.0 + exp(-MultNorm[i]));}
			if(PriorTypes[i]==1){ // Gamma prior
				MultNorm[i] = exp(MultNorm[i]);}
			// No adjustment necessary if the prior is normal
			ModelParameters.out[CurrSim-1][i] = MultNorm[i];
		}
	}
	if(InclPriors[0][0]==1){
		DualEfficacy = GetParameter(0);}
	if(InclPriors[1][0]==1){
		TransmBFacute = GetParameter(1);}
	if(InclPriors[2][0]==1){
		RRforEBF = GetParameter(2);}
	if(InclPriors[3][0]==1){
		TransmAcute = GetParameter(3);}
	if(InclPriors[4][0]==1){
		ProgToNeedLT = GetParameter(4);}
	if(InclPriors[5][0]==1){
		ExcessProgToNeed = GetParameter(5);}
	if(InclPriors[6][0]==1){
		RRprogressionPostnatal = GetParameter(6);}
	if(InclPriors[7][0]==1){
		AIDSmortLT = GetParameter(7);}
	if(InclPriors[8][0]==1){
		ExcessAIDSmort = GetParameter(8);}
	if(InclPriors[9][0]==1){
		Temp = GetParameter(9);
		TransmBFfirst3 = 1.0 - pow(1.0 - Temp, 1.0/12.0);
		TransmBFafter3 = TransmBFfirst3;
	}
	if(InclPriors[10][0]==1){
		PartnerRate20F = GetParameter(10);}
	if(InclPriors[11][0]==1){
		RRpartnerLow[0] = GetParameter(11);}
	if(InclPriors[12][0]==1){
		RRpartnerLow[1] = GetParameter(12);}
	if(InclPriors[13][0]==1){
		RRpartnerMarried[0] = GetParameter(13);}
	if(InclPriors[14][0]==1){
		RRpartnerMarried[1] = GetParameter(14);}
	if(InclPriors[15][0]==1){
		Assortativeness = GetParameter(15);}
	if(InclPriors[16][0]==1){
		CondomBias = GetParameter(16);}
	if(InclPriors[17][0]==1){
		VCTcondom = GetParameter(17);}
	if(InclPriors[18][0]==1){
		ARTcondom = 1.0 - pow(0.46, GetParameter(18));}
	if(InclPriors[19][0]==1){
		InitFSWprev = GetParameter(19) * 0.002;}
	if(InclPriors[20][0]==1){
		TransmST[1] = GetParameter(20);}
	if(InclPriors[21][0]==1){
		TransmLT[1] = GetParameter(21);}
	if(InclPriors[22][0]==1){
		TransmST[0] = GetParameter(22);
		TransmFSW[0] = TransmST[0];
	}
	if(InclPriors[23][0]==1){
		TransmLT[0] = GetParameter(23) * TransmST[0];
		TransmLT[1] = TransmST[1] * TransmLT[0] / TransmST[0];
	}
	if(InclPriors[24][0]==1){
		EctopyEffect[1] = GetParameter(24);}
	if(InclPriors[25][0]==1){
		VLeffectInfectivity = GetParameter(25);}
	if(InclPriors[26][0]==1){
		Temp = GetParameter(26);
		Temp2 = (CD4duration[0] + CD4duration[1] + CD4duration[2] +
			1.0/(CD4decline[2] + CD4mort[0]) + CD4decline[2]/((CD4decline[2] +
			CD4mort[0]) * CD4mort[1]))/Temp;
		for(is=0; is<3; is++){
			CD4decline[is] *= Temp2;}
		CD4mort[0] *= Temp2;
		CD4mort[1] *= Temp2;
	}
	if(InclPriors[27][0]==1){
		CD4decline[0] = GetParameter(27);}
	if(InclPriors[28][0]==1){
		CD4decline[1] = GetParameter(28);}
	if(InclPriors[29][0]==1){
		CD4decline[2] = GetParameter(29);}
	if(InclPriors[30][0]==1){
		CD4mort[1] = GetParameter(30);}
	if(InclPriors[31][0]==1){
		RRmort200to350 = GetParameter(31);}
	if(InclPriors[32][0]==1){
		RRuntreatedMortF = GetParameter(32);}
	if(InclPriors[33][0]==1){
		RRprevPrivateANC = GetParameter(33);}
	if(InclPriors[34][0]==1){
		IeDEAbias[0] = GetParameter(34);}
	if(InclPriors[35][0]==1){
		RRper10yr = 1 + GetParameter(35);}
	if(InclPriors[36][0]==1){ // Code to be added - trend in Brass alpha
		Temp = GetParameter(36);}
	if(InclPriors[37][0]==1){
		RRfertHIV = GetParameter(37);}
	if(InclPriors[38][0]==1){
		RRfertDiag = GetParameter(38);
		//RRfertHIV = 1.0 / RRfertDiag;
	}
	if(InclPriors[39][0]==1){
		ARTinterruptionRate = GetParameter(39);}
	if(InclPriors[40][0]==1){
		RR_ARTinterruption[34] = GetParameter(40);
		RR_ARTinterruption[31] = 1.0 - 0.25 * (1.0 - RR_ARTinterruption[34]);
		RR_ARTinterruption[32] = 1.0 - 0.50 * (1.0 - RR_ARTinterruption[34]);
		RR_ARTinterruption[33] = 1.0 - 0.75 * (1.0 - RR_ARTinterruption[34]);
	}
	if(InclPriors[41][0]==1){
		GammaMeanF = GetParameter(41);}
	if(InclPriors[42][0]==1){
		GammaSDF = GetParameter(42);}
	if(InclPriors[43][0]==1){
		VCTage[0] = GetParameter(43);}
	if(InclPriors[44][0]==1){
		VCTage[1] = GetParameter(44);}
	if(InclPriors[45][0]==1){
		VCTmale2002 = GetParameter(45);}
	if(InclPriors[46][0]==1){
		Temp = GetParameter(46);} // Redundant code
	if(InclPriors[47][0]==1){
		HCT1stTimeF25[21] = GetParameter(47);}
	if(InclPriors[48][0]==1){
		HCT1stTimeF25[24] = GetParameter(48);}
	if(InclPriors[49][0]==1){
		HIVeffectVCT = GetParameter(49);}
	if(InclPriors[50][0]==1){ // Previously OItoTBtestingRatio
		ORdiagOItreat = GetParameter(50);}
	if(InclPriors[51][0]==1){
		EctopyEffect[0] = GetParameter(51);}
	if(InclPriors[52][0]==1){
		RelInfecStage[0] = GetParameter(52);}
	if(InclPriors[53][0]==1){
		RelInfecStage[4] = GetParameter(53);
		RelInfecStage[3] = pow(RelInfecStage[4], 0.5);
	}
	if(InclPriors[54][0]==1){
		RednLogMort[0] = GetParameter(54);
		RednLogMort[1] = RednLogMort[0];
		RednLogMort[2] = RednLogMort[0];
	}
	if(InclPriors[55][0]==1){
		FSWcontactAge21 = GetParameter(55);}
	if(InclPriors[56][0]==1){
		HCT1stTimeF25[25] = GetParameter(56);}
	if(InclPriors[57][0]==1){
		QuadParam[0] = GetParameter(57);}
	if(InclPriors[58][0]==1){
		QuadParam[1] = GetParameter(58);}
	if(InclPriors[59][0]==1){
		VCTmale2010 = GetParameter(59);}
	if (InclPriors[60][0] == 1){
		RetestAdj = GetParameter(60);
		RetestAdjInit = RetestAdj;
	}
	if (InclPriors[61][0] == 1){
		RetestPos = GetParameter(61);
		RetestPosInit = RetestPos;
	}
	if (InclPriors[62][0] == 1){ // Previously Adj1996
		RetestART = GetParameter(62);
		RetestARTinit = RetestART;
	}
	if (InclPriors[63][0] == 1){
		HighRiskPropn[0] = 0.35 * GetParameter(63);
		HighRiskPropn[1] = 0.25 * GetParameter(63);
	}
	if (InclPriors[64][0] == 1){
		CondomAdjProv = GetParameter(64);}
	if (InclPriors[65][0] == 1){
		HighRiskPropn[1] = 0.25 * GetParameter(65);
	}
	if (InclPriors[66][0] == 1){
		ProvANCbias = GetParameter(66);}
	if (InclPriors[67][0] == 1){
		RRperCalYr = GetParameter(67);}
	/*if (InclPriors[68][0] == 1){
		BsplineCoef[0] = GetParameter(68);}
	else{
		BsplineCoef[0] = log(NumStartingART_M[15] + NumStartingART_F[15] + NumStartingART_P[15]); }
	if (InclPriors[69][0] == 1){
		BsplineCoef[1] = GetParameter(69);
		BsplineCoef[1] += BsplineCoef[0];
	}
	else{
		BsplineCoef[1] = log(NumStartingART_M[17] + NumStartingART_F[17] + NumStartingART_P[17]);}
	if (InclPriors[88][0] == 1){
		SDchangeBspline = GetParameter(88);}
	for (i = 70; i < 78; i++){
		if (InclPriors[i][0] == 1){
			//MCMCindex = InclPriors[i][1];
			//bParameters[MCMCindex] = SDchangeBspline;
			BsplineCoef[i - 68] = GetParameter(i);
			BsplineCoef[i - 68] += BsplineCoef[i - 69] * 2.0 - BsplineCoef[i - 70];
		}
		else if(2000+(i-68)*2<=ARTdataYr){
			BsplineCoef[i - 68] = log(NumStartingART_M[(i - 68) * 2 + 15] +
				NumStartingART_F[(i - 68) * 2 + 15] + NumStartingART_P[(i - 68) * 2 + 15]);
		}
	}*/
	if (InclPriors[68][0] == 1){
		RateARTstartF[19] = GetParameter(68); }
	if (InclPriors[69][0] == 1){
		RateARTstartF[25] = GetParameter(69); }
	if (InclPriors[70][0] == 1){
		RateARTstartF[26] = GetParameter(70); }
	if (InclPriors[71][0] == 1){
		RateARTstartF[29] = GetParameter(71); }
	if (InclPriors[72][0] == 1){
		RateARTstartF[31] = GetParameter(72); }
	if (InclPriors[73][0] == 1){
		RateARTstartF[15] = GetParameter(73); }
	if (InclPriors[76][0] == 1){
		RR_ARTstart1stMo = GetParameter(76); }
	if (InclPriors[77][0] == 1){
		RR_ARTstartM = GetParameter(77); }
	if (InclPriors[68][0] == 1 || InclPriors[69][0] == 1 || InclPriors[70][0] == 1 || InclPriors[71][0] == 1 || InclPriors[72][0] == 1){
		for (iy = 16; iy < 19; iy++){ RateARTstartF[iy] = RateARTstartF[15]; }
		for (iy = 20; iy < 25; iy++){
			RateARTstartF[iy] = RateARTstartF[19] * (25 - iy) / 6.0 + RateARTstartF[25] * (iy - 19) / 6.0; }
		for (iy = 27; iy <= 30; iy++){
			RateARTstartF[iy] = RateARTstartF[26]; }
		//RateARTstartF[30] = RateARTstartF[29];
		for (iy = 32; iy < 116; iy++){
			RateARTstartF[iy] = RateARTstartF[31]; }
		if (PropnalImmART == 1){
			for (iy = 15; iy < 116; iy++){
				HCT_ARTuptake[iy] = RateARTstartF[iy] * RR_ARTstart1stMo;
				if (HCT_ARTuptake[iy] > 0.9999){ HCT_ARTuptake[iy] = 0.9999; }
			}
		}
	}
	/*if (InclPriors[78][0] == 1){
		BsplineCoefP[0] = GetParameter(78);}
	else{
		BsplineCoefP[0] = log(NumStartingART_P[15]);}
	if (InclPriors[79][0] == 1){
		BsplineCoefP[1] = GetParameter(79);
		BsplineCoefP[1] += BsplineCoefP[0];
	}
	else{
		BsplineCoefP[1] = log(NumStartingART_P[17]);}
	for (i = 80; i < 88; i++){
		if (InclPriors[i][0] == 1){
			//MCMCindex = InclPriors[i][1];
			//bParameters[MCMCindex] = SDchangeBspline;
			BsplineCoefP[i-78] = GetParameter(i);
			BsplineCoefP[i-78] += BsplineCoefP[i - 79] * 2.0 - BsplineCoefP[i - 80];
		}
		else if (2000 + (i - 78) * 2 <= ARTdataYr){
			BsplineCoefP[i-78] = log(NumStartingART_P[(i-78)*2+15]);
		}
	}*/
	if (InclPriors[78][0] == 1){
		RateARTstartC[19] = GetParameter(78);}
	if (InclPriors[79][0] == 1){
		RateARTstartC[24] = GetParameter(79);}
	if (InclPriors[80][0] == 1){
		RateARTstartC[25] = GetParameter(80);}
	if (InclPriors[81][0] == 1){
		RateARTstartC[27] = GetParameter(81);}
	if (InclPriors[82][0] == 1){
		RateARTstartC[31] = GetParameter(82);}
	if (InclPriors[83][0] == 1){
		RateARTstartC[15] = GetParameter(83);}
	if (InclPriors[78][0] == 1 || InclPriors[79][0] == 1 || InclPriors[80][0] == 1 || InclPriors[81][0] == 1 || InclPriors[82][0] == 1){
		for (iy = 16; iy < 19; iy++){ RateARTstartC[iy] = RateARTstartC[15]; }
		for (iy = 20; iy < 24; iy++){
			RateARTstartC[iy] = RateARTstartC[19] * (24 - iy) / 5.0 + RateARTstartC[24] * (iy - 19) / 5.0;
		}
		for (iy = 26; iy < 31; iy++){ RateARTstartC[iy] = RateARTstartC[25]; }
		for (iy = 32; iy < 116; iy++){ RateARTstartC[iy] = RateARTstartC[31]; }
		if (PropnalImmART == 1){
			for (iy = 15; iy < 116; iy++){
				PaedARTuptake[iy] = RateARTstartC[iy] * RR_ARTstart1stMo;
				if (PaedARTuptake[iy] > 0.9999){ PaedARTuptake[iy] = 0.9999; }
			}
		}
	}
	if (InclPriors[89][0] == 1){
		AnnSwitchCumToCurr = GetParameter(89);}
	if (InclPriors[90][0] == 1){
		IeDEAbias[1] = GetParameter(90) * IeDEAbias[0];
		// Previous version of model did not adjust for IeDEAbias[0].
	}
	if (InclPriors[91][0] == 1){
		InfToVirulenceRatio = GetParameter(91);}
	if (InclPriors[92][0] == 1){
		MtoM_ST = GetParameter(92);}
	if (InclPriors[93][0] == 1){
		RRtestVirginTrend[0] = GetParameter(93);}
	if (InclPriors[94][0] == 1){
		RRtestPaedAdvanced = 1.0 / GetParameter(94);}
	if (InclPriors[95][0] == 1){
		ImmARTcorrectionP = GetParameter(95);}
	if (InclPriors[96][0] == 1){
		RiskCompensation = GetParameter(96);}
	if (InclPriors[97][0] == 1){
		RRearlyPaedART = GetParameter(97);}
	if (InclPriors[98][0] == 1){
		VCT_FSWentry = GetParameter(98);}
	if (InclPriors[99][0] == 1){
		TransmFSW[1] = GetParameter(99) / RelInfecRiskST[0][0][1];
		if (InclPriors[109][0] == 0){
			RRclientToFSW1985 = 27.0 * exp(-770.8 * TransmFSW[1] * RelInfecRiskST[0][0][1]);
		}
	}
	if (InclPriors[100][0] == 1){
		RRmortART2 = GetParameter(100);
		RRmortART1 = 0.5 * (RRmortART2 + 1.0);
	}
	if (InclPriors[101][0] == 1){
		RednLogMortP[0] = GetParameter(101);
		RednLogMortP[1] = RednLogMortP[0];
		RednLogMortP[2] = RednLogMortP[0];
	}
	if (InclPriors[102][0] == 1){
		ExcessProgRedn = GetParameter(102);}
	if (InclPriors[103][0] == 1){
		ExcessMortRedn = GetParameter(103);}
	if (InclPriors[104][0] == 1){
		RetestAdjMax = GetParameter(104);}
	if (InclPriors[105][0] == 1){
		RRtestVirginTrend[1] = GetParameter(105);}
	if (InclPriors[106][0] == 1){
		RRdiagDeathsPIP[0] = GetParameter(106);}
	if (InclPriors[107][0] == 1){
		RRfertART = GetParameter(107);}
	if (InclPriors[108][0] == 1){
		RRfertCD4[0] = GetParameter(108) + 1.0;}
	if (InclPriors[109][0] == 1){
		RRclientToFSW1985 = GetParameter(109);}
	if (InclPriors[110][0] == 1){
		ORsuppressionIeDEA = GetParameter(110);}
	if (InclPriors[111][0] == 1){
		RRinterruptionM = GetParameter(111);}
	if (InclPriors[112][0] == 1){
		RR_ARTstart100CD4 = GetParameter(112);}
	if (InclPriors[113][0] == 1){
		VirginTestAdjProv = GetParameter(113);}
	if (InclPriors[114][0] == 1){
		BFadjProv = GetParameter(114);}
	if (InclPriors[115][0] == 1){
		SpecificityANC = GetParameter(115);}
	if (InclPriors[116][0] == 1){
		MarriageConstant[0] = GetParameter(116);}
	if (InclPriors[117][0] == 1){
		MarriageConstant[1] = GetParameter(117);}
	if (InclPriors[118][0] == 1){
		MarriageTrend[0] = GetParameter(118);}
	if (InclPriors[119][0] == 1){
		MarriageTrend[1] = GetParameter(119);}
	if (InclPriors[120][0] == 1){
		MarriageShape[0] = GetParameter(120);}
	if (InclPriors[121][0] == 1){
		MarriageShape[1] = GetParameter(121);}
	if (InclPriors[122][0] == 1){
		ORremarriage[0] = 1.0 / GetParameter(122);}
	if (InclPriors[123][0] == 1){
		ORremarriage[1] = 1.0 / GetParameter(123);}
	if (InclPriors[124][0] == 1){
		DivorceAdj = GetParameter(124);}
	if (InclPriors[125][0] == 1){
		DivorceTrend = GetParameter(125);}
	if (InclPriors[126][0] == 1){
		SeTestingHistory[0] = GetParameter(126);}
	if (InclPriors[127][0] == 1){
		SeTestingHistory[1] = GetParameter(127);}
	if (InclPriors[128][0] == 1){
		SpTestingHistory = GetParameter(128);}
	if (InclPriors[129][0] == 1){
		RetestPosP = GetParameter(129);}
	if (InclPriors[130][0] == 1){
		COVIDimpactARTstart = GetParameter(130);}
	if (InclPriors[131][0] == 1){
		ProvAdjPaedMort = GetParameter(131);}

	// Update natural history calculations
	CD4duration[1] = (1.0/CD4decline[0]) - CD4duration[0];
	CD4duration[2] = 1.0/CD4decline[1];
	CD4duration[3] = 1.0/CD4decline[2];
	CD4mort[0] = CD4mort[1] * RRmort200to350;

	// Update HCT rollout rates
	if(InclPriors[46][0]==1 || InclPriors[47][0]==1 || InclPriors[48][0]==1){
		for(iy=5; iy<17; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + HCT1stTimeF25[17]/12.0;}
		for(iy=18; iy<21; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + (HCT1stTimeF25[21] -
				HCT1stTimeF25[17])/4.0;}
		for(iy=22; iy<24; iy++){
			HCT1stTimeF25[iy] = HCT1stTimeF25[iy-1] + (HCT1stTimeF25[24] -
				HCT1stTimeF25[21])/3.0;}
		HCT1stTimeF25[26] = HCT1stTimeF25[25] * 0.8;
	}
	/*if (InclPriors[62][0] == 1){
		NumbersTested[11] = Adj1996 * NumbersTested[17];
		for (iy = 5; iy < 11; iy++){
			NumbersTested[iy] = NumbersTested[11] * (iy - 5.0) / 6.0;
		}
		for (iy = 12; iy < 17; iy++){
			NumbersTested[iy] = NumbersTested[iy - 1] + (NumbersTested[17] -
				NumbersTested[11]) / 6.0;
		}
	}*/

	// Update annual ART initiation
	//if ((CalibARTtotals == 1 || CalibARTtotalsP == 1) && InputARTinitiationRates == 0){ GetAnnNewART(); }

	// Handle IeDEA bias
	if (InclPriors[34][0] == 1 && InclPriors[90][0] == 0){
		IeDEAbias[1] = IeDEAbias[0]; }

	if (InclPriors[95][0] == 1){
		PaedARTuptakeEID = BaseARTuptakeEID * ImmARTcorrectionP;
	}

	// Calculate RR_ARTinitiation
	if (InclPriors[112][0] == 1){
		// Same as in the SetActivityByStage function: Temp represents dif in average CD4 between
		// each CD4 category and the base CD4 category (<200)
		for (is = 0; is < 4; is++){
			if (is == 0){ Temp = 4.75; }
			if (is == 1){ Temp = 3.25; }
			if (is == 2){ Temp = 1.75; }
			if (is == 3){ Temp = 0.00; }
			RR_ARTinitiation[is + 1] = pow(RR_ARTstart100CD4, Temp);
		}
	}

	/*CD4transm[0] = 0.0;
	CD4transm[1] = 0.0;
	CD4transm[2] = 0.0;
	CD4transm[3] = 0.0;
	TransmAcute = 0.0;
	TransmBFacute = 0.0;
	TransmBFfirst3 = 0.0;
	TransmBFafter3 = 0.0;
	TransmART200 = 0.0;
	TransmARTpre200 = 0.0;
	TransmARTprePreg = 0.0;
	InitTransmART200 = 0.0;
	InitTransmARTpre200 = 0.0;*/
}

double GetParameter(int PriorIndex)
{
	int MCMCindex;
	double ParamValue;

	MCMCindex = InclPriors[PriorIndex][1];
	ParamValue = ModelParameters.out[CurrSim-1][MCMCindex];

	return ParamValue;
}

double ReturnNegLogL(double ParameterSet[20])
{
	int IncludePriorDensity = 1;
	// Set this to 1 if you want to calculate the posterior, otherwise this function
	// will only calculate the likelihood. Also not that if you set IncludePriorDensity
	// to 1, you must ensure that the MCMCdim value corresponds to the dimension of
	// the simplex AND ensure you have selected the corresponding prior densities in
	// the Priors.txt file.

	int i, iy, k;
	double LogPrior, x, y, a, b;

	CurrSim += 1;
	// Set parameters
	if (CalibARTtotals == 1){
		BsplineCoef[0] = ParameterSet[0]; // 2000 coefficient
		BsplineCoef[1] = ParameterSet[1] + BsplineCoef[0]; // 2002 coefficient
		for (i = 2; i < 10; i++){ // 2004 to 2018 coefficients
			BsplineCoef[i] = ParameterSet[i];
			BsplineCoef[i] += BsplineCoef[i - 1] * 2.0 - BsplineCoef[i - 2];
		}
		AnnSwitchCumToCurr = ParameterSet[10];
		GetAnnNewART();
	}
	if (CalibARTtotalsP == 1){
		BsplineCoefP[0] = ParameterSet[0]; // 2000 coefficient
		BsplineCoefP[1] = ParameterSet[1] + BsplineCoefP[0]; // 2002 coefficient
		for (i = 2; i < 10; i++){ // 2004 to 2018 coefficients
			BsplineCoefP[i] = ParameterSet[i];
			BsplineCoefP[i] += BsplineCoefP[i - 1] * 2.0 - BsplineCoefP[i - 2];
		}
		ImmARTcorrectionP = ParameterSet[10];
		PaedARTuptakeEID = BaseARTuptakeEID * ImmARTcorrectionP;
		GetAnnNewART();
	}

	CurrYear = StartYear;
	UpdateNonAIDSmort();
	SetInitialParameters();
	SetCD4byARTdur();
	CalcInterruptions();
	SetActivityByStage();
	SetInitSexActivity();
	UpdateMixingST();
	SetCurrYearParameters();
	SetFertByStage();
	SetProgression(0);
	UpdateARTmort();
	ARTerrorInd = 0;
	MaxARTerror = 0.0;
	CurrYear = StartYear - 1;
	for (iy = 0; iy<ProjectionTerm; iy++){
		OneYear();
	}
	CalcLikelihood();
	LogL.out[CurrSim - 1][0] = LogLikelihood - MaxARTerror;

	if (IncludePriorDensity==1){
		LogPrior = 0.0;
		// Code for calculating prior density adapted from the OneIMISstep function
		for (k = 0; k<MCMCdim; k++){
			x = ParameterSet[k];
			if (PriorTypes[k] == 0){ // Beta prior
				a = aParameters[k];
				b = bParameters[k];
				y = beta_log(&a, &b);
				LogPrior += log(x) * (aParameters[k] - 1.0) + log(1.0 - x) *
					(bParameters[k] - 1.0) - y;
			}
			if (PriorTypes[k] == 1){ // Gamma prior
				a = aParameters[k];
				y = gamma_log(&a);
				LogPrior += log(bParameters[k]) * a + log(x) * (a - 1.0) -
					bParameters[k] * x - y;
			}
			if (PriorTypes[k] == 2){ // Normal prior
				LogPrior += -0.5 * pow((x - aParameters[k]) / bParameters[k],
					2.0) - 0.5 * log(2.0 * 3.141592654) - log(bParameters[k]);
			}
		}
		LogLikelihood += LogPrior;
	}

	return -LogLikelihood;
}

void ReadInitSimplex(const char* input, double ParameterCombinations[21][20], int Dimension)
{
	int ir, ic;
	double sumvertices;
	std::ifstream file;

	file.open(input);
	if (file.fail()){
		std::cerr << "Could not open InitialSimplex.txt\n";
		exit(1);
	}
	for (ir = 0; ir<Dimension + 1; ir++){
		for (ic = 0; ic<Dimension; ic++){
			file >> ParameterCombinations[ir][ic];
		}
	}
	file.close();
}

void SaveFinalSimplex(const char* filout, double ParameterCombinations[21][20], int Dimension)
{
	int ic, ir;
	std::ofstream file(filout);

	for (ir = 0; ir<Dimension + 1; ir++){
		for (ic = 0; ic<Dimension; ic++){
			file << "	" << std::setw(15) << std::right << ParameterCombinations[ir][ic];
		}
		file << std::endl;
	}
	file.close();
}

void SaveNegLogL(const char* filout, double NegLogL[21])
{
	int ir;
	std::ofstream file(filout);

	for (ir = 0; ir<MCMCdim + 1; ir++){
		file << "	" << std::setw(15) << std::right << NegLogL[ir] << std::endl; }
	file.close();
}

void MaximizeLikelihood(double FTol, const char* input, const char* filout)
{
	// This function implements the Downhill Simplex Method, and is copied from the
	// AMOEBA function outlined on pp. 292-3 of Press et al, 1986, Numerical Recipes,
	// Cambridge, Cambridge University Press. I have changed some of the variable and
	// function names to make it easier to understand. For example, the ReturnNegLogL()
	// function takes the place of the FUNK() function (see further comments below).
	// Note that this function minimizes the NEGATIVE of the log likelihood, which is
	// equivalent to maximizing the log likelihood.
	// Note that one cannot maximize the likelihood if there are more than 20 parameters;
	// if there are more than 20, you need to change the dimensions of the arrays below.
	// Also note that I've modified the algorithm to prevent the last parameter lying
	// outside the interval [0, 1]. This restriction might not be appropriate for certain
	// parameters.

	// VERY IMPORTANT: Always check the FinalSimplex output file before concluding that
	// you have reached convergence. The log likelihood values might all be very close
	// together, but unless the parameters are also all close to one another, it does not
	// mean that convergence has been reached. Reduce FTol and run the algorithm again,
	// replacing "InitialSimplex.txt" with "FinalSimplex.txt", and replacing
	// "FinalSimplex.txt" with "FinalSimplex2.txt". (Incidentally, you can use the same
	// method if you reach the maximum number of iterations (500) and wish to continue.)

	int ic, ir, it; // Counters for parameters, vertices and iterations respectively
	int Dimension = MCMCdim; // Takes the place of NDIM in AMOEBA
	int Vertices = Dimension + 1; // Takes the place of MPTS in AMOEBA
	int offset;
	double NegLogL[21]; // Takes the place of Y[] in AMOEBA
	double Alpha = 1.0;
	double Beta = 0.5;
	double Gamma = 2.0;
	double ParameterCombinations[21][20]; // Takes the place of matrix P in AMOEBA
	double AveParam[20]; // Takes the place of PBAR in AMOEBA
	double AltParam1[20]; // Takes the place of PR in AMOEBA
	double AltParam2[20]; // Takes the place of PRR in AMOEBA
	double AltLogL1; // Takes the place of YPR in AMOEBA
	double AltLogL2; // Takes the place of YPRR in AMOEBA
	int iLowest, iHighest, i2ndHighest; // Take the place of ILO, IHI, INHI
	double RTol;
	int MaxIterations;

	// Determine the starting values of ParameterCombinations and NegLogL arrays
	ReadAllFiles(); // Added to the original code in STI-HIV Interaction model
	ReadPriors(); // Added to the original code in STI-HIV Interaction model
	CurrSim = 0; // Added to the original code in STI-HIV Interaction model
	ReadInitSimplex(input, ParameterCombinations, Dimension);
	for (ir = 0; ir<Vertices; ir++){
		for (ic = 0; ic<Dimension; ic++){
			AltParam1[ic] = ParameterCombinations[ir][ic];
		}
		NegLogL[ir] = ReturnNegLogL(AltParam1);
	}

	// Determine MaxIterations
	MaxIterations = 500;

	//Determine the starting values of iLowest, iHighest, i2ndHighest, RTol
	iLowest = 0;
	if (NegLogL[0] > NegLogL[1]){
		iHighest = 0;
		i2ndHighest = 1;
	}
	else{
		iHighest = 1;
		i2ndHighest = 0;
	}
	for (ir = 0; ir<Vertices; ir++){
		if (NegLogL[ir] < NegLogL[iLowest]){
			iLowest = ir;
		}
		if (NegLogL[ir] > NegLogL[iHighest]){
			i2ndHighest = iHighest;
			iHighest = ir;
		}
		else if (NegLogL[ir] > NegLogL[i2ndHighest]){
			if (ir != iHighest){
				i2ndHighest = ir;
			}
		}
	}
	RTol = 2.0 * fabs(NegLogL[iHighest] - NegLogL[iLowest]) / (fabs(NegLogL[iHighest]) +
		fabs(NegLogL[iLowest]));

	// Iterate until convergence is achieved
	it = 0;
	while (RTol > FTol && it < MaxIterations){
		it += 1;
		for (ic = 0; ic<Dimension; ic++){ // Calculate average
			AveParam[ic] = 0;
		}
		for (ir = 0; ir<Vertices; ir++){
			if (ir != iHighest){
				for (ic = 0; ic<Dimension; ic++){
					AveParam[ic] += ParameterCombinations[ir][ic];
				}
			}
		}
		for (ic = 0; ic<Dimension; ic++){
			AveParam[ic] = AveParam[ic] / Dimension;
			AltParam1[ic] = (1.0 + Alpha) * AveParam[ic] - Alpha *
				ParameterCombinations[iHighest][ic]; // Reflection step
		}
		// Restrict exp(-theta) to the [0, 1] range.
		if (AltParam1[10]<0.0001){ AltParam1[10] = 0.0001; }
		if (AltParam1[10]>0.9999){ AltParam1[10] = 0.9999; }
		AltLogL1 = ReturnNegLogL(AltParam1);
		if (AltLogL1 <= NegLogL[iLowest]){
			for (ic = 0; ic<Dimension; ic++){ // Extrapolation step
				AltParam2[ic] = Gamma * AltParam1[ic] + (1.0 - Gamma) * AveParam[ic];}
			// Restrict exp(-theta) to the [0, 1] range.
			if (AltParam2[10]<0.0001){ AltParam2[10] = 0.0001; }
			if (AltParam2[10]>0.9999){ AltParam2[10] = 0.9999; }
			AltLogL2 = ReturnNegLogL(AltParam2);
			if (AltLogL2 < NegLogL[iLowest]){
				for (ic = 0; ic<Dimension; ic++){
					ParameterCombinations[iHighest][ic] = AltParam2[ic];
				}
				NegLogL[iHighest] = AltLogL2;
			}
			else{
				for (ic = 0; ic<Dimension; ic++){
					ParameterCombinations[iHighest][ic] = AltParam1[ic];
				}
				NegLogL[iHighest] = AltLogL1;
			}
		}
		else if (AltLogL1 >= NegLogL[i2ndHighest]){
			if (AltLogL1 < NegLogL[iHighest]){
				for (ic = 0; ic<Dimension; ic++){
					ParameterCombinations[iHighest][ic] = AltParam1[ic];
				}
				NegLogL[iHighest] = AltLogL1;
			}
			for (ic = 0; ic<Dimension; ic++){ // Contraction step
				AltParam2[ic] = Beta * ParameterCombinations[iHighest][ic] + (1.0 - Beta) *
					AveParam[ic];
				// Not necessary to check if value is in [0, 1] range
			}
			AltLogL2 = ReturnNegLogL(AltParam2);
			if (AltLogL2 < NegLogL[iHighest]){
				for (ic = 0; ic<Dimension; ic++){
					ParameterCombinations[iHighest][ic] = AltParam2[ic];
				}
				NegLogL[iHighest] = AltLogL2;
			}
			else{
				for (ir = 0; ir<Vertices; ir++){
					if (ir != iLowest){
						for (ic = 0; ic<Dimension; ic++){
							AltParam1[ic] = 0.5 * (ParameterCombinations[ir][ic] +
								ParameterCombinations[iLowest][ic]);
							// Not necessary to check if value is in [0, 1] range
							ParameterCombinations[ir][ic] = AltParam1[ic];
						}
						NegLogL[ir] = ReturnNegLogL(AltParam1);
					}
				}
			}
		}
		else{
			for (ic = 0; ic<Dimension; ic++){
				ParameterCombinations[iHighest][ic] = AltParam1[ic];
			}
			NegLogL[iHighest] = AltLogL1;
		}

		// Lastly, update iLowest, iHighest, i2ndHighest, RTol (same code as before)
		iLowest = 0;
		if (NegLogL[0] > NegLogL[1]){
			iHighest = 0;
			i2ndHighest = 1;
		}
		else{
			iHighest = 1;
			i2ndHighest = 0;
		}
		for (ir = 0; ir<Vertices; ir++){
			if (NegLogL[ir] < NegLogL[iLowest]){
				iLowest = ir;
			}
			if (NegLogL[ir] > NegLogL[iHighest]){
				i2ndHighest = iHighest;
				iHighest = ir;
			}
			else if (NegLogL[ir] > NegLogL[i2ndHighest]){
				if (ir != iHighest){
					i2ndHighest = ir;
				}
			}
		}
		RTol = 2.0 * fabs(NegLogL[iHighest] - NegLogL[iLowest]) / (fabs(NegLogL[iHighest]) +
			fabs(NegLogL[iLowest]));
	}

	// Generate outputs
	SaveFinalSimplex(filout, ParameterCombinations, Dimension);
	SaveNegLogL("NegLogL.txt", NegLogL);
	FixedUncertainty = 1;
	for (ic = 0; ic < Dimension; ic++){
		AltParam1[ic] = ParameterCombinations[iLowest][ic];}
	CurrSim = 0;
	LogLikelihood = ReturnNegLogL(AltParam1);
	if (CalibARTtotals == 1 || CalibARTtotalsP == 1){
		StartingARTtot.RecordSample("StartingARTtot.txt");
		TotalART15F.RecordSample("TotalART15F.txt");
		TotalART15M.RecordSample("TotalART15M.txt");
		TotalARTunder15.RecordSample("TotalARTunder15.txt");
		ARTerror.RecordSample("ARTerror.txt");
	}
	if (CalibARTtotalsP == 1){
		StartingART0.RecordSample("StartingART0.txt");
		StartingART1.RecordSample("StartingART1.txt");
		StartingART2to4.RecordSample("StartingART2to4.txt");
		StartingART5to14.RecordSample("StartingART5to14.txt");
	}

	std::cout.precision(10);
	for (ir = 0; ir<Vertices; ir++){
		std::cout << "NegLogL[" << ir << "]: " << NegLogL[ir] << std::endl;
	}
	for (ic = 0; ic<Dimension; ic++){
		std::cout << "ML estimate of parameter " << ic + 1 << ": " << ParameterCombinations[iLowest][ic] << std::endl;
	}
	std::cout << "Number of iterations: " << it << std::endl;
}
