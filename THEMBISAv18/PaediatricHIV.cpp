// This is the main project file for VC++ application project 
// generated using an Application Wizard.

// #include "stdafx.h"
// #using <mscorlib.dll>
// using namespace System;

#include "PaediatricHIV.h"
#include "StatFunctions.h"
#include "randomc.h"
#include <time.h>

int main()
{
	int iy; 
	clock_t start, finish;
	double elapsed_time;

	start = clock();
	//GenerateSample();
	RunSample();	// Remember to set FixedUncertainty = 1 before running this function
	//runIMIS();
    /*ReadAssumptions();
	ReadASSAinputs();
	ReadPhaseIn();
	ReadHIVprevData();	// Not necessary to call this function if not calculating likelihood
	SetInitialParameters();
	CurrYear = StartYear-1;
	for(iy=0; iy<ProjectionTerm; iy++){
		OneYear();}
	CalcLikelihood();
	std::cout.precision(10);
	std::cout<<"Log likelihood: "<<LogLikelihood<<std::endl;
	
	std::cout<<"AliveTotal[5]: "<<AliveTotal[5]<<std::endl;
	std::cout<<"EligibleTotal[3]: "<<EligibleTotal[3]<<std::endl;
	std::cout<<"NonAIDSdeathsTotal[7]: "<<NonAIDSdeathsTotal[7]<<std::endl;
	std::cout<<"AIDSdeathsTotal[0]: "<<AIDSdeathsTotal[0]<<std::endl;
	std::cout<<"DeathsTotal[12]: "<<DeathsTotal[12]<<std::endl;
	std::cout<<"StartingARTtotal[4]: "<<StartingARTtotal[4]<<std::endl;
	std::cout<<"CentralMort[1]: "<<CentralMort[1]<<std::endl;
	std::cout<<"CentralMort[2]: "<<CentralMort[2]<<std::endl;
	std::cout<<"CentralMort[3]: "<<CentralMort[3]<<std::endl;
	std::cout<<"CentralMort[4]: "<<CentralMort[4]<<std::endl;
	std::cout<<"CentralMort[5]: "<<CentralMort[5]<<std::endl;
	std::cout<<"CentralMort[6]: "<<CentralMort[6]<<std::endl;
	std::cout<<"CentralMort[7]: "<<CentralMort[7]<<std::endl;
	std::cout<<"NewHIVatBirthTotal: "<<NewHIVatBirthTotal<<std::endl;
	std::cout<<"NewHIVafterBirthTotal: "<<NewHIVafterBirthTotal<<std::endl;
	std::cout<<"ARTinitiationStore: "<<ARTinitiationStore<<std::endl;*/

	finish = clock();
	elapsed_time = (finish - start);
	std::cout<<"Time taken: "<<elapsed_time<<std::endl;
	return 0;
}

GenderGroup::GenderGroup(int Gender)
{
	Sex = Gender;
}

void GenderGroup::GetStartYrProfile()
{
	int im;
	double PopByAge[180];
	double x1, intpart1, DoubleIntDif1;
	int x2;

	for(im=0; im<180; im++){
		x1 = im/12.0;
		DoubleIntDif1 = modf(x1, &intpart1);
		if(DoubleIntDif1==0.0){
			x2 = x1;
			PopByAge[im] = StartingPop[x2][Sex]/12.0;
		}
		else{
			PopByAge[im] = PopByAge[im - 1];}
		if(im<36){
			NegMatMF[im] = PopByAge[im] * PropnBF[im][0];
			NegChildFF[im] = PopByAge[im] * (1.0 - PropnBF[im][0]);
		}
		else{
			NegMatMF[im] = 0.0;
			NegChildFF[im] = PopByAge[im];
		}
		NegMatEBF[im] = 0.0;
		AcuteMatMF[im] = 0.0;
		AcuteMatEBF[im] = 0.0;
		UnawareMatMF[im] = 0.0;
		UnawareMatEBF[im] = 0.0;
		AwareMatMF[im] = 0.0;
		AwareMatEBF[im] = 0.0;
		ARTmatMF[im] = 0.0;
		ARTmatEBF[im] = 0.0;
		PosChildAtBirthNoPMTCT[im] = 0.0;
		PosChildAtBirthPMTCT[im] = 0.0;
		PosChildAfterBirth[im] = 0.0;
		ARTeligible[im] = 0.0;
		OnARTearly[im] = 0.0;
		OnARTlate1st3m[im] = 0.0;
		OnARTlateAfter3m[im] = 0.0;
		StoppedART[im] = 0.0;
		Total[im] = PopByAge[im];
	}
}

void GenderGroup::GetNonAIDSmort()
{
	int iy, im;
	double MortTable[15];
	double x1, intpart1, DoubleIntDif1;
	int x2;

	for(iy=0; iy<15; iy++){
		if(Sex==0){MortTable[iy] = NonAIDSmortM[iy][CurrYear-StartYear];}
		else{MortTable[iy] = NonAIDSmortF[iy][CurrYear-StartYear];}
	}

	if(InclMortData==1){
		for(iy=0; iy<15; iy++){
			MortTable[iy] = 1.0/(1.0 + exp(-ConstantLogitq) * pow(1.0/MortTable[iy] - 1.0,
				SlopeLogitq));
		}
		if(CurrYear>1996){
			for(iy=0; iy<15; iy++){
				MortTable[iy] *= pow(1.0 - AveAnnMortRedn - IncrMortRednWRTage * (iy - 7.0), 
					CurrYear - 1996);
			}
		}
	}

	for(im=0; im<12; im++){
		NonAIDSmort[im] = 1.0 - pow(1.0 - MortTable[0], pow((im + 1.0)/12.0, IMRshape) - 
			pow(im/12.0, IMRshape));
	}
	for(im=12; im<180; im++){
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

void GenderGroup::GetEndProfile1()
{
	int im;
	double NVI, NewNonVertCurrM; // non-vertical incidence of HIV

	// Set PropnBirths
	if(Sex==0){
		PropnBirths = SexRatio;}
	else{
		PropnBirths = 1.0 - SexRatio;}

	for(im=0; im<180; im++){

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
			AwareMatEBF_E[0] = (ChildNegMotherPosKnown - TestedPosOnART * (1.0 - TransmHAART)) 
				* EverFeedCurr[2] * PropnBirths/12.0;}
		else if(im<7){
			AwareMatEBF_E[im] = AwareMatEBF[im-1] * (1.0 - KnownMatEBFexit[im-1][0]) * 
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate NewMatAwareMF
		if(im<6){
			NewMatAwareMF[im] = AwareMatEBF[im] * KnownMatEBFexit[im][0] * KnownMatEBFexit[im][2];}

		// Calculate AwareMatMF_E
		if(im==0){
			AwareMatMF_E[0] = (ChildNegMotherPosKnown - TestedPosOnART * (1.0 - TransmHAART)) 
				* EverFeedCurr[1] * PropnBirths/12.0;}
		else if (im<36){
			AwareMatMF_E[im] = (AwareMatMF[im-1] * (1.0 - KnownMatMFexit[im-1][0]) + 
				NewMatAwareMF[im-1] * (-KnownMatMFexit[im-1][0]/log(1.0 - KnownMatMFexit[im-1][0]))) * 
				(1.0 - NonAIDSmort[im-1]);
		}
		if(im==2){
			AwareMatMF_E[im] += RescreenImm * (1.0 - SwitchingToFF * (1.0 - FFredn)) * (UnawareMatMF[im-1] * 
				(1.0 - ChronicMatExit[im-1][0]) + NewMatUnaware[im-1] * (-ChronicMatExit[im-1][0]/
				log(1.0 - ChronicMatExit[im-1][0]))) * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate ARTmatEBF_E
		if(im==0){
			ARTmatEBF_E[0] = TestedPosOnART * (1.0 - TransmHAART) * EverFeedCurr[2] * PropnBirths/12.0;}
		else if(im<7){
			ARTmatEBF_E[im] = ARTmatEBF[im-1] * (1.0 - ARTmatEBFexit[im-1][0]) * (1.0 - NonAIDSmort[im-1]);}

		// Calculate NewMatARTMF
		if(im<6){
			NewMatARTMF[im] = ARTmatEBF[im] * ARTmatEBFexit[im][0] * ARTmatEBFexit[im][2];}

		// Calculate ARTmatMF_E
		if(im==0){
			ARTmatMF_E[0] = TestedPosOnART * (1.0 - TransmHAART) * EverFeedCurr[1] * PropnBirths/12.0;}
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
				RescreenImm * SwitchingToFF * (1.0 - FFredn) * (1.0 - NonAIDSmort[im-1]);
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

		if(NonVertTransm==1 && im>0){
			NVI = NonVertIncidence2004 * NonVertAgeAdj[(im-1)/12] * NonVertCurrYear/12.0;
			NewNonVertCurrM = NVI * (NegMatMF_E[im] + NegMatEBF_E[im] + AcuteMatMF_E[im] + 
				AcuteMatEBF_E[im] + UnawareMatMF_E[im] + UnawareMatEBF_E[im] + AwareMatMF_E[im] + 
				AwareMatEBF_E[im] + ARTmatMF_E[im] + ARTmatEBF_E[im] + NegChildFF_E[im])/
				pow(1.0 - NonAIDSmort[im-1], 0.5);
			NewHIVpostnatal[im-1] += NewNonVertCurrM;
			NewNonVertCurrY += NewNonVertCurrM;
			NegMatMF_E[im] *= (1.0 - NVI);
			NegMatEBF_E[im] *= (1.0 - NVI);
			AcuteMatMF_E[im] *= (1.0 - NVI);
			AcuteMatEBF_E[im] *= (1.0 - NVI);
			UnawareMatMF_E[im] *= (1.0 - NVI);
			UnawareMatEBF_E[im] *= (1.0 - NVI);
			AwareMatMF_E[im] *= (1.0 - NVI);
			AwareMatEBF_E[im] *= (1.0 - NVI);
			ARTmatMF_E[im] *= (1.0 - NVI);
			ARTmatEBF_E[im] *= (1.0 - NVI);
			NegChildFF_E[im] *= (1.0 - NVI);
		}

		// Calculate PosChildAtBirthNoPMTCT_E
		if(im==0){
			PosChildAtBirthNoPMTCT_E[0] = ChildPosNoPMTCT * PropnBirths/12.0;}
		else{
			PosChildAtBirthNoPMTCT_E[im] = PosChildAtBirthNoPMTCT[im-1] * (1.0 - 
				ProgToNeedNoPMTCT[im-1]) * (1.0 - NonAIDSmort[im-1]);}
		if(im==2 && EarlyART==1 && CurrYear>=EarlyARTyr){
			PosChildAtBirthNoPMTCT_E[im] *= (1.0 - PCRuptake);}

		// Calculate PosChildAtBirthPMTCT_E
		if(im==0){
			PosChildAtBirthPMTCT_E[0] = ChildPosPMTCT * PropnBirths/12.0;}
		else{
			PosChildAtBirthPMTCT_E[im] = PosChildAtBirthPMTCT[im-1] * (1.0 - 
				ProgToNeedPMTCT[im-1]) * (1.0 - NonAIDSmort[im-1]);}
		if(im==2 && EarlyART==1 && CurrYear>=EarlyARTyr){
			PosChildAtBirthPMTCT_E[im] *= (1.0 - PCRuptake);}

		// Calculate PosChildAfterBirth_E
		if(im==0){
			PosChildAfterBirth_E[0] = 0.0;}
		else{
			PosChildAfterBirth_E[im] = PosChildAfterBirth[im-1] * pow(1.0 -
				ProgToNeedNoPMTCT[im-1], RRprogressionPostnatal) * (1.0 - NonAIDSmort[im-1]) +
				NewHIVpostnatal[im-1] * (-(1.0 - pow(1.0 - ProgToNeedNoPMTCT[im-1],
				RRprogressionPostnatal))/(RRprogressionPostnatal * log(1.0 - ProgToNeedNoPMTCT[im-1]))) * 
				pow(1.0 - NonAIDSmort[im-1], 0.5);
		}

		// Calculate NewEligible
		NewEligible[im] = PosChildAtBirthNoPMTCT[im] * ProgToNeedNoPMTCT[im] +
			PosChildAtBirthPMTCT[im] * ProgToNeedPMTCT[im] + PosChildAfterBirth[im] * (1.0 - 
			pow(1.0 - ProgToNeedNoPMTCT[im], RRprogressionPostnatal)) + NewHIVpostnatal[im] *
			(1.0 + (1.0 - pow(1.0 - ProgToNeedNoPMTCT[im], RRprogressionPostnatal))/
			(RRprogressionPostnatal * log(1.0 - ProgToNeedNoPMTCT[im])))/pow(1.0 - 
			NonAIDSmort[im], 0.5);
	}
}

void GenderGroup::GetEndProfile2()
{
	int im;

	for(im=0; im<180; im++){

		// Calculate ARTeligible_E
		if(im==0){
			ARTeligible_E[0] = 0.0;}
		else{
			ARTeligible_E[im] = (ARTeligible[im-1] * (1.0 - ARTeligibleExit[im-1][0]) +
				NewEligible[im-1] * (-AIDSmortNoART[im-1]/log(1.0 - AIDSmortNoART[im-1]))) * 
				(1.0 - NonAIDSmort[im-1]);
			if(EarlyART==1 && CurrYear>=EarlyARTyr && im==2){
				ARTeligible_E[im] *= (1.0 - PCRuptake);}
		}

		// Calculate OnARTearly_E
		if(EarlyART==1 && CurrYear>=EarlyARTyr){
			if(im==2){
				OnARTearly_E[im] = (PosChildAtBirthNoPMTCT[im-1] * (1.0 - ProgToNeedNoPMTCT[im-1]) +
					PosChildAtBirthPMTCT[im-1] * (1.0 - ProgToNeedPMTCT[im-1])) *  
					PCRuptake * (1.0 - NonAIDSmort[im-1]);
			}
			else if(im>2){
				OnARTearly_E[im] = OnARTearly[im-1] * (1.0 - EarlyARTexit[im-1][0]) *
					(1.0 - NonAIDSmort[im-1]);}
		}
		else{
			OnARTearly_E[im] = 0.0;}

		// Calculate StartingARTno6wkScreen
		StartingARTno6wkScreen[im] = ARTeligible[im] * ARTeligibleExit[im][0] * ARTeligibleExit[im][2];

		// Calculate OnARTlate1st3m_E
		if(im>1){
			OnARTlate1st3m_E[im] = (OnARTlate1st3m[im-1] * (1.0 - ARTexitHR[im-1][0]) + 
				StartingARTno6wkScreen[im-1] * (-ARTexitHR[im-1][0]/log(1.0 - ARTexitHR[im-1][0]))) * 
				(1.0 - NonAIDSmort[im-1]);
		}
		if(im==2 && EarlyART==1 && CurrYear>=EarlyARTyr){ 
			OnARTlate1st3m_E[im] += (ARTeligible[im-1] * (1.0 - ARTeligibleExit[im-1][0]) +
				NewEligible[im-1] * (-ARTeligibleExit[im-1][0]/log(1.0 - ARTeligibleExit[im-1][0]))) * 
				PCRuptake * (1.0 - NonAIDSmort[im-1]);
		}

		// Calculate StabilizingOnART
		StabilizingOnART[im] = OnARTlate1st3m[im] * ARTexitHR[im][0] * ARTexitHR[im][3] +
			StartingARTno6wkScreen[im] * (1.0 + ARTexitHR[im][0]/log(1.0 - ARTexitHR[im][0])) * 
			ARTexitHR[im][3];

		// Calculate OnARTlateAfter3m_E
		if(im>1){
			OnARTlateAfter3m_E[im] = (OnARTlateAfter3m[im-1] * (1.0 - ARTexitLR[im-1][0]) + 
				StabilizingOnART[im-1] * (-ARTexitLR[im-1][0]/log(1.0 - ARTexitLR[im-1][0]))) * 
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate StoppingART
		StoppingART[im] = OnARTearly[im] * EarlyARTexit[im][0] * EarlyARTexit[im][2] +
			OnARTlate1st3m[im] * ARTexitHR[im][0] * ARTexitHR[im][2] + OnARTlateAfter3m[im] * 
			ARTexitLR[im][0] * ARTexitLR[im][2] + StartingARTno6wkScreen[im] * (1.0 + 
			ARTexitHR[im][0]/log(1.0 - ARTexitHR[im][0])) * ARTexitHR[im][2] + StabilizingOnART[im] * 
			(1.0 + ARTexitLR[im][0]/log(1.0 - ARTexitLR[im][0])) * ARTexitLR[im][2];

		// Calculate StoppedART_E
		if(im>0){
			StoppedART_E[im] = (StoppedART[im-1] * (1.0 - AIDSmortNoART[im-1]) +
				StoppingART[im-1] * (-AIDSmortNoART[im-1]/log(1.0 - AIDSmortNoART[im-1]))) * 
				(1.0 - NonAIDSmort[im-1]);
		}

		// Calculate Total (at the START of the month)
		if(CurrMonth==0){
			Total[im] = NegMatMF[im] + NegMatEBF[im] + AcuteMatMF[im] + AcuteMatEBF[im] +
				UnawareMatMF[im] + UnawareMatEBF[im] + AwareMatMF[im] + AwareMatEBF[im] + 
				ARTmatMF[im] + ARTmatEBF[im] + NegChildFF[im] + PosChildAtBirthNoPMTCT[im] + 
				PosChildAtBirthPMTCT[im] + PosChildAfterBirth[im] + ARTeligible[im] + 
				OnARTearly[im] + OnARTlate1st3m[im] + OnARTlateAfter3m[im] + StoppedART[im];
		}

		// Calculate Total (at the END of the month)
		Total_E[im] = NegMatMF_E[im] + NegMatEBF_E[im] + AcuteMatMF_E[im] + AcuteMatEBF_E[im] +
			UnawareMatMF_E[im] + UnawareMatEBF_E[im] + AwareMatMF_E[im] + AwareMatEBF_E[im] + 
			ARTmatMF_E[im] + ARTmatEBF_E[im] + NegChildFF_E[im] + PosChildAtBirthNoPMTCT_E[im] + 
			PosChildAtBirthPMTCT_E[im] + PosChildAfterBirth_E[im] + ARTeligible_E[im] + 
			OnARTearly_E[im] + OnARTlate1st3m_E[im] + OnARTlateAfter3m_E[im] + StoppedART_E[im];

		// Calculate AIDSdeaths (formula corrected from original version of Excel model)
		AIDSdeaths[im] = (ARTeligible[im] * ARTeligibleExit[im][0] * ARTeligibleExit[im][1] +
			OnARTearly[im] * EarlyARTexit[im][0] * EarlyARTexit[im][1] + OnARTlate1st3m[im] * 
			ARTexitHR[im][0] * ARTexitHR[im][1] + OnARTlateAfter3m[im] * ARTexitLR[im][0] * 
			ARTexitLR[im][1] + StoppedART[im] * AIDSmortNoART[im] + NewEligible[im] * (1.0 +
			ARTeligibleExit[im][0]/log(1.0 - ARTeligibleExit[im][0])) * ARTeligibleExit[im][1] + 
			StartingARTno6wkScreen[im] * (1.0 + ARTexitHR[im][0]/log(1.0 - ARTexitHR[im][0])) * 
			ARTexitHR[im][1] + StabilizingOnART[im] * (1.0 + ARTexitLR[im][0]/log(1.0 - 
			ARTexitLR[im][0])) * ARTexitLR[im][1] + StoppingART[im] * (1.0 + AIDSmortNoART[im]/
			log(1.0 - AIDSmortNoART[im]))) * (1.0 - NonAIDSmort[im]);

		// Calculate AIDSdeathsUntreated
		AIDSdeathsUntreated[im] = (ARTeligible[im] * ARTeligibleExit[im][0] * ARTeligibleExit[im][1] +
			StoppedART[im] * AIDSmortNoART[im] + NewEligible[im] * (1.0 + ARTeligibleExit[im][0]/
			log(1.0 - ARTeligibleExit[im][0])) * ARTeligibleExit[im][1] + StoppingART[im] * 
			(1.0 + AIDSmortNoART[im]/log(1.0 - AIDSmortNoART[im]))) * (1.0 - NonAIDSmort[im]);

		// Calculate NonAIDSdeaths
		NonAIDSdeaths[im] = Total[im] * NonAIDSmort[im];
		if(im<24){
			NonAIDSdeaths[im] += NegChildFF[im] * NonAIDSmort[im] * (pow(NoBFmortAdj[im],
				BFbiasAdj) - 1.0);
		}

		// Get the age dbn of infant deaths and neonatal deaths
		if(im<12 && CurrMonth==0){
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
		}

		// Calculating StartingART 
		StartingART[im] = StartingARTno6wkScreen[im] * pow(1.0 - NonAIDSmort[im], 0.5);
		if(im==1 && EarlyART==1 && CurrYear>=EarlyARTyr){
			StartingART[im] += (ARTeligible[im] * (1.0 - ARTeligibleExit[im][0]) +
				PosChildAtBirthNoPMTCT[im] * (1.0 - ProgToNeedNoPMTCT[im]) +
				PosChildAtBirthPMTCT[im] * (1.0 - ProgToNeedPMTCT[im]) +
				NewEligible[im] * (-ARTeligibleExit[im][0]/log(1.0 - ARTeligibleExit[im][0]))) *
				PCRuptake * (1.0 - NonAIDSmort[im]);
		}
	}
}

void GenderGroup::UpdateStartProfile()
{
	int im;

	for(im=0; im<180; im++){
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
		OnARTearly[im] = OnARTearly_E[im];
		OnARTlate1st3m[im] = OnARTlate1st3m_E[im];
		OnARTlateAfter3m[im] = OnARTlateAfter3m_E[im];
		StoppedART[im] = StoppedART_E[im];
		Total[im] = Total_E[im];
	}
}

OutputArray::OutputArray(int n)
{
	columns = n;
}

void OutputArray::Record(char* filout, int n)
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

void OutputArray::RecordSample(char* filout, int n)
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

void PostOutputArray::RecordSample(char* filout)
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

void ReadAssumptions()
{
	std::ifstream file;
	int ia, ic;

	file.open("Assumptions.txt");
	file.ignore(255,'\n');
	file>>TransmUntreated;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>CD4transm[ic];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ic=0; ic<4; ic++){
		file>>CD4propn[ic];}
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
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRmortART1;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRmortART2;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>RRexcessEarlyART;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Discontinuation1;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Discontinuation2;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>PCRuptake;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ARTcoverage;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>IMRshape;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SexRatio;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>Sensitivity;
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
	file>>TransmHAART;
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
	file>>RednHAART;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MatARTchange;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MatARTall;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>EverFeed[0]>>EverFeed[1]>>EverFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>MedianFeed[0]>>MedianFeed[1]>>MedianFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ShapeFeed[0]>>ShapeFeed[1]>>ShapeFeed[2];
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SwitchingToFF;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AbruptWeaningFirst3;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AbruptWeaningAfter3;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>ConstantLogitq;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>SlopeLogitq;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>AveAnnMortRedn;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>IncrMortRednWRTage;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<24; ia++){
		file>>NoBFmortAdj[ia];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>BFbiasAdj;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>NonVertIncidence2004;
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<15; ia++){
		file>>NonVertAgeAdj[ia];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>EligibleHealthSeeking;
	file.close();
}

void ReadASSAinputs()
{
	int ia, iy;
	std::ifstream file;

	file.open("ASSAinputs.txt");
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>BirthsHIVposMothers[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>BirthsHIVnegMothers[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>HIVincidence[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<15; ia++){
		for(iy=0; iy<41; iy++){
			file>>NonAIDSmortM[ia][iy];}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<15; ia++){
		for(iy=0; iy<41; iy++){
			file>>NonAIDSmortF[ia][iy];}
	}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<15; ia++){
		file>>StartingPop[ia][0]>>StartingPop[ia][1];}
	file.close();
}

void ReadPhaseIn()
{
	int iy;
	std::ifstream file;

	file.open("PhaseIn.txt");
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>PregnantWomenTested[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>AZTrollout[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>MatARTrollout[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>MatARTuptake[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>RescreenPropnLate[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>RescreenPropnImm[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>ExtNVProllout[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>NumStartingART[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<26; iy++){
		file>>NonVertYearAdj[iy];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(iy=0; iy<41; iy++){
		file>>FFphaseOut[iy];}
	file.close();
}

void ReadHIVprevData()
{
	int ia, idum;
	std::ifstream file;

	file.open("HIVprevData.txt");
	file.ignore(255,'\n');
	for(ia=0; ia<3; ia++){
		file>>idum>>ObservedPrev05[ia][0]>>SEprev05[ia][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<3; ia++){
		file>>idum>>ObservedPrev05[ia][1]>>SEprev05[ia][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<3; ia++){
		file>>idum>>ObservedPrev08[ia][0]>>SEprev08[ia][0];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	for(ia=0; ia<3; ia++){
		file>>idum>>ObservedPrev08[ia][1]>>SEprev08[ia][1];}
	file.ignore(255,'\n');
	file.ignore(255,'\n');
	file>>idum>>ObservedPrevU208>>SEprevU208;
	file.close();
}

void ReadMortData()
{
	int ia, iy;
	std::ifstream file;

	file.open("MortData.txt");
	for(ia=0; ia<5; ia++){
		for(iy=0; iy<10; iy++){
			file>>RecordedDeaths[ia][iy];}
	}
}

void ReadARTinitiation()
{
	int ic, iy, skip1, skip2;
	std::ifstream file;
	
	file.open("TotalARTinitiation.txt");
	for(ic=0; ic<ResampleSize; ic++){
		file>>skip1>>skip2;
		for(iy=0; iy<TotalARTinitiation.columns; iy++){
			file>>TotalARTinitiation.out[ic][iy];}
	}
}

void SetInitialParameters()
{
	int im, ib;

	// Set breastfeeding rates
	for(im=0; im<36; im++){
		for(ib=0; ib<2; ib++){
			PropnBF[im][ib] = EverFeed[ib] * pow(0.5, pow(im/MedianFeed[ib], ShapeFeed[ib]));
		}
	}
	for(im=0; im<6; im++){
		PropnBF[im][2] = EverFeed[2] * pow(0.5, pow(im/MedianFeed[2], ShapeFeed[2]));
	}
	for(im=0; im<36; im++){
		for(ib=0; ib<2; ib++){
			RateOfBFchange[im][ib] = 1.0 - PropnBF[im + 1][ib]/PropnBF[im][ib];
		}
	}
	for(im=0; im<6; im++){
			RateOfBFchange[im][2] = 1.0 - PropnBF[im + 1][2]/PropnBF[im][2];
	}

	// Set rates of transition out of the HIV states
	for(im=0; im<180; im++){
		ProgToNeedNoPMTCT[im] = 1.0 - exp(-ProgAdjNoPMTCT * (ProgToNeedLT/12.0 + ExcessProgToNeed * 
			(pow(ExcessProgRedn, (im + 1.0)/12.0) - pow(ExcessProgRedn, im/12.0))/
			log(ExcessProgRedn)));
		ProgToNeedPMTCT[im] = 1.0 - exp(-ProgToNeedLT/12.0 - ExcessProgToNeed * (pow(ExcessProgRedn, 
			(im + 1.0)/12.0) - pow(ExcessProgRedn, im/12.0))/log(ExcessProgRedn));
		AIDSmortNoART[im] = 1.0 - exp(-AIDSmortLT/12.0 - ExcessAIDSmort * (pow(ExcessMortRedn, 
			(im + 1.0)/12.0) - pow(ExcessMortRedn, im/12.0))/log(ExcessMortRedn));
		AIDSmortEarlyART[im] = 1.0 - exp(-AIDSmortLT * RRmortART2/12.0 - ExcessAIDSmort * RRmortART2 *
			RRexcessEarlyART * (pow(ExcessMortRedn, (im + 1.0)/12.0) - pow(ExcessMortRedn, 
			im/12.0))/log(ExcessMortRedn));
	}

	// Set initial population profile, by sex, feeding and age in months
	Male.GetStartYrProfile();
	Female.GetStartYrProfile();

	// Set initial ART initiation rates to 0
	for(im=0; im<180; im++){
		ARTinitiation[im] = 0.0;}
}

void SetCurrYearParameters()
{
	double PosMothers, NegMothers, temp;
	int ic, ia, im;

	// Calculate the parameters in the 'ASSA input' sheet
	MatIncidence = HIVincidence[CurrYear-StartYear];
	//if(CurrYear>=2010){MatIncidence = HIVincidence[CurrYear-StartYear] * 0.5;}
	//MatIncidence = 0.0;
	VCTuptake = PregnantWomenTested[CurrYear-StartYear];
	//if(CurrYear>=2010){VCTuptake = 1.0;}
	//VCTuptake = 0.0;
	//if(CurrYear>=2010){NVPuptake=1.0;}
	AZTpropn = AZTrollout[CurrYear-StartYear];
	//if(CurrYear>=2008){AZTpropn = 0.072;}
	//if(CurrYear>=2010){AZTpropn = 1.0;}
	MatARTpropn = MatARTrollout[CurrYear-StartYear] * MatARTuptake[CurrYear-StartYear];
	//if(CurrYear>=2010){MatARTpropn = MatARTrollout[CurrYear-StartYear] * 0.6;}
	//if(CurrYear>=2010){MatARTpropn = MatARTrollout[CurrYear-StartYear] * 1.0;}
	//MatARTpropn = 0.0;
	RescreenLate = RescreenPropnLate[CurrYear-StartYear];
	//if(CurrYear>=2010){RescreenLate = 1.00;}
	RescreenImm = RescreenPropnImm[CurrYear-StartYear];
	//if(CurrYear>=2010){RescreenImm = 0.61;}
	ExtNVPpropn = ExtNVProllout[CurrYear-StartYear];
	//if(CurrYear>=2010){ExtNVPpropn = 1.0;}
	//ExtNVPpropn = 0.0;
	StartingART = NumStartingART[CurrYear-StartYear];
	//StartingART = 0.0;
	NonVertCurrYear = NonVertYearAdj[CurrYear-StartYear];
	FFredn = FFphaseOut[CurrYear-StartYear];
	/*if(CurrYear>=2010){
		if(CurrYear<2013){
			PCRuptake = 0.53 + (0.8 - 0.53) * (2.0 * (CurrYear - 2010) + 1.0)/6.0;
			ARTcoverage = 0.5 + (0.8 - 0.5) * (2.0 * (CurrYear - 2010) + 1.0)/6.0;
		}
		else{
			PCRuptake = 0.8;
			ARTcoverage = 0.8;
		}
	}
	else{
		PCRuptake = 0.53;
		ARTcoverage = 0.5;
	}*/
	/*if(CurrYear==2010){
		EverFeed[1] = 0.05;
		EverFeed[2] = 0.45;
		for(im=0; im<6; im++){
			PropnBF[im][2] = 0.45 * pow(0.5, pow(im/6.0, ShapeFeed[2]));}
		for(im=0; im<6; im++){
			RateOfBFchange[im][2] = 1.0 - PropnBF[im + 1][2]/PropnBF[im][2];}
	}
	if(CurrYear<2010){
		EverFeed[1] = 0.154;
		EverFeed[2] = 0.346;
	}*/
	EverFeedCurr[0] = EverFeed[0];
	EverFeedCurr[1] = (EverFeed[1]/(EverFeed[1] + EverFeed[2])) * (1.0 - (1.0 -
		EverFeed[1] - EverFeed[2]) * (1.0 - FFredn));
	EverFeedCurr[2] = (EverFeed[2]/(EverFeed[1] + EverFeed[2])) * (1.0 - (1.0 -
		EverFeed[1] - EverFeed[2]) * (1.0 - FFredn));

	// Calculate the parameters in the 'Births' sheet
	PosMothers = BirthsHIVposMothers[CurrYear-StartYear];
	NegMothers = BirthsHIVnegMothers[CurrYear-StartYear];
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

	// Calculate number of tests performed antenatally
	// (a) at 1st antenatal visit
	NewTestsPerformed = (PosMothers + NegMothers) * VCTuptake;
	// (b) in late pregnancy
	NewTestsPerformed += (PosMothers + NegMothers) * (1.0 - VCTuptake) * RescreenLate * 
		UntestedRescreen + (PosMothers * (1.0 - Sensitivity) + NegMothers) * VCTuptake * 
		RescreenLate * RescreenUptake;

	for(ic=0; ic<4; ic++){
		TestedPosCD4[ic] = (PTP + PTNRP + PUTLP) * CD4propn[ic];}
	TestedPosOnART = TestedPosCD4[3] * MatARTpropn;
	TestedPosCD4[3] *= (1.0 - MatARTpropn);
	if(CurrYear>=MatARTchange){
		TestedPosOnART += TestedPosCD4[2] * MatARTpropn;
		TestedPosCD4[2] *= (1.0 - MatARTpropn);
	}
	if(CurrYear>=MatARTall){
		TestedPosOnART += (TestedPosCD4[0] + TestedPosCD4[1]) * MatARTpropn;
		TestedPosCD4[0] *= (1.0 - MatARTpropn);
		TestedPosCD4[1] *= (1.0 - MatARTpropn);
	}
	UndiagnosedPos = PUTLN + PTNRN;
	RecentPosDet = NTNRP + NUTLP;
	RecentPosUndet = NTNRN + NUTLN + NRN * MatIncidence * (DeliveryWk - RescreenWk + 5.0)/52.0;

	ChildPosNoPMTCT = RecentPosDet * TransmAcute;
	for(ic=0; ic<4; ic++){
		ChildPosNoPMTCT += TestedPosCD4[ic] * CD4transm[ic];}
	ChildPosPMTCT = ChildPosNoPMTCT;
	ChildNegMotherPosKnown = -ChildPosNoPMTCT;
	ChildPosNoPMTCT *= (1.0 - NVPuptake) * (1.0 - AZTpropn * AZTpropnAdj);
	temp = ChildPosNoPMTCT;
	ChildPosNoPMTCT += UndiagnosedPos * TransmUntreated + RecentPosUndet * TransmAcute;
	ChildPosPMTCT *= (NVPuptake * (1.0 - NVPefficacy * (1.0 - AZTpropn) - DualEfficacy * AZTpropn) +
		(1.0 - NVPuptake) * AZTpropn * AZTpropnAdj * (1.0 - AZTefficacy));
	ChildPosPMTCT += TestedPosOnART * TransmHAART;
	ChildNegMotherPosKnown *= (1.0 - NVPuptake * (NVPefficacy * (1.0 - AZTpropn) + DualEfficacy *
		AZTpropn) - (1.0 - NVPuptake) * AZTpropn * AZTpropnAdj * AZTefficacy);
	ChildNegMotherPosKnown += TestedPosCD4[0] + TestedPosCD4[1] + TestedPosCD4[2] + TestedPosCD4[3] +
		RecentPosDet + TestedPosOnART * (1.0 - TransmHAART);
	ChildNegMotherPosUnknown = UndiagnosedPos * (1.0 - TransmUntreated) + RecentPosUndet * (1.0 - 
		TransmAcute);
	ChildNegMotherNeg = NRN * (1.0 - MatIncidence * (DeliveryWk - RescreenWk + 5.0)/52.0);

	VertTransm.out[CurrSim-1][CurrYear-1985] = (ChildPosNoPMTCT + ChildPosPMTCT)/(ChildPosNoPMTCT + 
		ChildPosPMTCT + ChildNegMotherPosKnown + ChildNegMotherPosUnknown);
	VertTransmKnownPos.out[CurrSim-1][CurrYear-1985] = (ChildPosPMTCT + temp)/(TestedPosOnART +
		TestedPosCD4[0] + TestedPosCD4[1] + TestedPosCD4[2] + TestedPosCD4[3] + RecentPosDet);
	VertTransmGotART.out[CurrSim-1][CurrYear-1985] = ChildPosPMTCT/(TestedPosOnART +
		(TestedPosCD4[0] + TestedPosCD4[1] + TestedPosCD4[2] + TestedPosCD4[3] + RecentPosDet) * 
		(NVPuptake + (1.0 - NVPuptake) * AZTpropn * AZTpropnAdj));

	// Calculate the rates in the 'Age-specific calcs' sheet
	Male.GetNonAIDSmort();
	Female.GetNonAIDSmort();
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
		AcuteMatExit[ia][0] = 1.0 - exp(-1.0/3.0) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute);
		AcuteMatExit[ia][1] = (-1.0/3.0)/log(exp(-1.0/3.0) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
		AcuteMatExit[ia][2] = log(1.0 - RateOfBFchange[ia][0])/
			log(exp(-1.0/3.0) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
		AcuteMatExit[ia][3] = log(1 - TransmBFacute)/
			log(exp(-1.0/3.0) * (1.0 - RateOfBFchange[ia][0]) * (1 - TransmBFacute));
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
	for(ia=0; ia<180; ia++){
		ARTeligibleExit[ia][0] = 1.0 - (1.0 - AIDSmortNoART[ia]) * exp(-ARTinitiation[ia]);
		ARTeligibleExit[ia][1] = log(1.0 - AIDSmortNoART[ia])/log((1.0 - AIDSmortNoART[ia]) * 
			exp(-ARTinitiation[ia]));
		ARTeligibleExit[ia][2] = -ARTinitiation[ia]/log((1.0 - AIDSmortNoART[ia]) * exp(-ARTinitiation[ia]));
		if(ia<5){
			EarlyARTexit[ia][0] = 1.0 - (1.0 - AIDSmortEarlyART[ia]) * exp(-Discontinuation1/12.0);
			EarlyARTexit[ia][1] = log(1.0 - AIDSmortEarlyART[ia])/log((1.0 - AIDSmortEarlyART[ia]) * 
				exp(-Discontinuation1/12.0));
			EarlyARTexit[ia][2] = (-Discontinuation1/12.0)/log((1.0 - AIDSmortEarlyART[ia]) * 
				exp(-Discontinuation1/12.0));
		}
		else{
			EarlyARTexit[ia][0] = 1.0 - (1.0 - AIDSmortEarlyART[ia]) * exp(-Discontinuation2/12.0);
			EarlyARTexit[ia][1] = log(1.0 - AIDSmortEarlyART[ia])/log((1.0 - AIDSmortEarlyART[ia]) * 
				exp(-Discontinuation2/12.0));
			EarlyARTexit[ia][2] = (-Discontinuation2/12.0)/log((1.0 - AIDSmortEarlyART[ia]) * 
				exp(-Discontinuation2/12.0));
		}
		ARTexitHR[ia][0] = 1.0 - exp(-1.0/3.0) * pow(1.0 - AIDSmortNoART[ia], RRmortART1) *
			exp(-Discontinuation1/12.0);
		ARTexitHR[ia][1] = log(pow(1.0 - AIDSmortNoART[ia], RRmortART1))/log(exp(-1.0/3.0) *
			pow(1.0 - AIDSmortNoART[ia], RRmortART1) * exp(-Discontinuation1/12.0));
		ARTexitHR[ia][2] = (-Discontinuation1/12.0)/log(exp(-1.0/3.0) *
			pow(1.0 - AIDSmortNoART[ia], RRmortART1) * exp(-Discontinuation1/12.0));
		ARTexitHR[ia][3] = (-1.0/3.0)/log(exp(-1.0/3.0) * pow(1.0 - AIDSmortNoART[ia], 
			RRmortART1) * exp(-Discontinuation1/12.0));
		ARTexitLR[ia][0] = 1.0 - pow(1.0 - AIDSmortNoART[ia], RRmortART2) *
			exp(-Discontinuation2/12.0);
		ARTexitLR[ia][1] = log(pow(1.0 - AIDSmortNoART[ia], RRmortART2))/log(
			pow(1.0 - AIDSmortNoART[ia], RRmortART2) * exp(-Discontinuation2/12.0));
		ARTexitLR[ia][2] = (-Discontinuation2/12.0)/log(pow(1.0 - AIDSmortNoART[ia], 
			RRmortART2) * exp(-Discontinuation2/12.0));
	}

	// Calculate number of tests performed at 6-week immunization visits (in mothers)
	// Note that this formula is approximate because knowledge of HIV status in mothers
	// of infected children is not recorded (i.e. only non-AIDS mortality is reflected).
	NewTestsPerformed += (PosMothers + NegMothers - PTP - PTNRP - PUTLP - NTNRP - NUTLP) *
		RescreenImm * (0.92/0.61) * (1.0 - (Male.NonAIDSmort[0] + Female.NonAIDSmort[0])/
		2.0) * (1.0 - (Male.NonAIDSmort[1] + Female.NonAIDSmort[1])/4.0);
}

void SetARTinitiationRates()
{
	int im;
	double sumA, sumB, TargetStartingART, root;

	sumA = 0.0;
	sumB = 0.0;
	for(im=0; im<180; im++){
		QuadraticA[im] = 0.5 * (Male.ARTeligible[im] + Female.ARTeligible[im]);
		QuadraticB[im] = Male.ARTeligible[im] * (-0.5 * log((1.0 - Male.NonAIDSmort[im]) * 
				(1.0 - AIDSmortNoART[im])) - 1.0) + Female.ARTeligible[im] * (-0.5 *  
				log((1.0 - Female.NonAIDSmort[im]) * (1.0 - AIDSmortNoART[im])) - 1.0);
		sumA += QuadraticA[im];
		sumB += QuadraticB[im];
	}

	if(CurrYear>=InterpolationStart){
		TargetStartingART = 0.0;
		for(im=0; im<180; im++){
			TargetStartingART += Male.NewEligible[im] + Female.NewEligible[im];}
		TargetStartingART *= ARTcoverage;
		//if(pow(sumB, 2.0) > 4.0 * sumA * TargetStartingART){
			root = (-sumB - sqrt(pow(sumB, 2.0) - 4.0 * sumA * TargetStartingART))/
				(2.0 * sumA);
		/*}
		else{ // This condition has been added to avoid problems with undefined rates of ART initiation.
			root = 0.5;
		}*/
	}
	else{
		root = (-sumB - sqrt(pow(sumB, 2.0) - 4.0 * sumA * StartingART/12.0))/
			(2.0 * sumA);
	}

	// The following line of code has been added to prevent implausibly high rates of ART initiation.
	//if(root>0.5){root = 0.5;}

	if(CurrYear<InterpolationStart || CurrYear>=UltimateYr){
		ARTinitiation[0] = root;}
	else{
		ARTinitiation[0] = ARTinitiationStore + (root - ARTinitiationStore) * (CurrYear - 
			InterpolationStart + 0.5)/(UltimateYr - InterpolationStart);
	}
	if(FixedARTinitiation==1 && FixedUncertainty==1 && CurrYear<InterpolationStart){
		ARTinitiation[0] = TotalARTinitiation.out[CurrSim-1][CurrYear-1985]/12.0;}
	for(im=1; im<180; im++){
		ARTinitiation[im] = ARTinitiation[im-1];}
	for(im=0; im<180; im++){
		ARTeligibleExit[im][0] = 1.0 - (1.0 - AIDSmortNoART[im]) * exp(-ARTinitiation[im]);
		ARTeligibleExit[im][1] = log(1.0 - AIDSmortNoART[im])/log((1.0 - AIDSmortNoART[im]) * 
			exp(-ARTinitiation[im]));
		ARTeligibleExit[im][2] = -ARTinitiation[im]/log((1.0 - AIDSmortNoART[im]) * exp(-ARTinitiation[im]));
	}
}

void GetCalibOutput()
{
	double ModelPrev[3][2];
	double denominator, numerator;
	int ia, ig, iag, start, end;

	for(iag=0; iag<3; iag++){
		if(iag==0){start = 2;}
		else{start = iag * 5;}
		end = (iag + 1) * 5;
		for(ig=0; ig<2; ig++){
			denominator = 0;
			numerator = 0;
			for(ia=start; ia<end; ia++){
				denominator += ChildrenAlive[ia][ig];
				numerator += ChildrenPositive[ia][ig];
			}
			ModelPrev[iag][ig] = numerator/denominator;
		}
	}

	if(CurrYear==2005){
		for(iag=0; iag<3; iag++){
			for(ig=0; ig<2; ig++){
				ModelPrev05[iag][ig] = ModelPrev[iag][ig];}
		}
	}
	if(CurrYear==2008){
		for(iag=0; iag<3; iag++){
			for(ig=0; ig<2; ig++){
				ModelPrev08[iag][ig] = ModelPrev[iag][ig];}
		}
		ModelPrevU208 = (ChildrenPositive[0][0] + ChildrenPositive[0][1] + ChildrenPositive[1][0] +
			ChildrenPositive[1][1])/(ChildrenAlive[0][0] + ChildrenAlive[0][1] + 
			ChildrenAlive[1][0] + ChildrenAlive[1][1]);
	}

	if(InclMortData==1){
		if(CurrYear>=1997 && CurrYear<2007){
			ModelDeaths[0][CurrYear-1997] = DeathsTotal[0];
			ModelDeaths[1][CurrYear-1997] = DeathsTotal[1];
			ModelDeaths[2][CurrYear-1997] = DeathsTotal[2] + DeathsTotal[3] + DeathsTotal[4];
			ModelDeaths[3][CurrYear-1997] = DeathsTotal[5] + DeathsTotal[6] + DeathsTotal[7] +
				DeathsTotal[8] + DeathsTotal[9];
			ModelDeaths[4][CurrYear-1997] = DeathsTotal[10] + DeathsTotal[11] + DeathsTotal[12] +
				DeathsTotal[13] + DeathsTotal[14];
		}
	}
}

void CalcOutput()
{
	int ia, ig;
	double numerator, denominator, temp;

	// Calculate prevalence statistics
	numerator = 0.0;
	denominator = 0.0;
	for(ia=0; ia<15; ia++){
		numerator += ChildrenPositive[ia][0] + ChildrenPositive[ia][1];
		denominator += ChildrenAlive[ia][0] + ChildrenAlive[ia][1];
	}
	Prev0to14.out[CurrSim-1][CurrYear-1985] = numerator/denominator;
	TotalHIV.out[CurrSim-1][CurrYear-1985] = numerator;

	numerator = 0.0;
	denominator = 0.0;
	for(ia=0; ia<2; ia++){
		numerator += ChildrenPositive[ia][0] + ChildrenPositive[ia][1];
		denominator += ChildrenAlive[ia][0] + ChildrenAlive[ia][1];
	}
	Prev0to1.out[CurrSim-1][CurrYear-1985] = numerator/denominator;

	numerator = 0.0;
	denominator = 0.0;
	for(ia=2; ia<5; ia++){
		numerator += ChildrenPositive[ia][0];
		denominator += ChildrenAlive[ia][0];
	}
	Prev2to4M.out[CurrSim-1][CurrYear-1985] = numerator/denominator;
	numerator = 0.0;
	denominator = 0.0;
	for(ia=2; ia<5; ia++){
		numerator += ChildrenPositive[ia][1];
		denominator += ChildrenAlive[ia][1];
	}
	Prev2to4F.out[CurrSim-1][CurrYear-1985] = numerator/denominator;

	numerator = 0.0;
	denominator = 0.0;
	for(ia=5; ia<10; ia++){
		numerator += ChildrenPositive[ia][0];
		denominator += ChildrenAlive[ia][0];
	}
	Prev5to9M.out[CurrSim-1][CurrYear-1985] = numerator/denominator;
	numerator = 0.0;
	denominator = 0.0;
	for(ia=5; ia<10; ia++){
		numerator += ChildrenPositive[ia][1];
		denominator += ChildrenAlive[ia][1];
	}
	Prev5to9F.out[CurrSim-1][CurrYear-1985] = numerator/denominator;

	numerator = 0.0;
	denominator = 0.0;
	for(ia=10; ia<15; ia++){
		numerator += ChildrenPositive[ia][0];
		denominator += ChildrenAlive[ia][0];
	}
	Prev10to14M.out[CurrSim-1][CurrYear-1985] = numerator/denominator;
	numerator = 0.0;
	denominator = 0.0;
	for(ia=10; ia<15; ia++){
		numerator += ChildrenPositive[ia][1];
		denominator += ChildrenAlive[ia][1];
	}
	Prev10to14F.out[CurrSim-1][CurrYear-1985] = numerator/denominator;

	for(ia=2; ia<10; ia++){
		Prev2004.out[CurrSim-1][ia-2] = (ChildrenPositive[ia][0] +
			ChildrenPositive[ia][1] + ARTeligibleTotal[ia] * (EligibleHealthSeeking -
			1.0))/(ChildrenAlive[ia][0] + ChildrenAlive[ia][1] + 
			ARTeligibleTotal[ia] * (EligibleHealthSeeking - 1.0));
	}

	if(CurrYear==2000){
		for(ia=0; ia<15; ia++){
			HIVageProfile2000.out[CurrSim-1][ia] = ChildrenPositive[ia][0] +
				ChildrenPositive[ia][1];
		}
	}
	if(CurrYear==2010){
		for(ia=0; ia<15; ia++){
			HIVageProfile2010.out[CurrSim-1][ia] = ChildrenPositive[ia][0] +
				ChildrenPositive[ia][1];
		}
	}
	if(CurrYear==2010){
		for(ia=0; ia<15; ia++){
			UntreatedAgeProfile.out[CurrSim-1][ia] = ARTeligibleTotal[ia] +
				OffARTtotal[ia];
			OnARTageProfile.out[CurrSim-1][ia] = OnARTtotal[ia];
			NotNeedingAgeProfile.out[CurrSim-1][ia] = NotNeedingTotal[ia];
			TreatedMortAgeProfile.out[CurrSim-1][ia] = AIDSdeathsTotal[ia] -
				AIDSdeathsUntreatedTotal[ia];
			UntreatedMortAgeProfile.out[CurrSim-1][ia] = 
				AIDSdeathsUntreatedTotal[ia];
			// Code for calculating HIV stage by month
			// Note that the _E extension means that to get the results at the start of the
			// 2010 projection year (say), we need to run the model to 2009.
			/*UntreatedAgeProfile.out[CurrSim-1][ia] = Male.ARTeligible_E[ia] +
				Female.ARTeligible_E[ia]; 
			OnARTageProfile.out[CurrSim-1][ia] = Male.OnARTearly_E[ia] +
				Male.OnARTlate1st3m_E[ia] + Male.OnARTlateAfter3m_E[ia] +
				Female.OnARTearly_E[ia] + Female.OnARTlate1st3m_E[ia] + 
				Female.OnARTlateAfter3m_E[ia];
			NotNeedingAgeProfile.out[CurrSim-1][ia] = Male.PosChildAtBirthNoPMTCT_E[ia] +
				Male.PosChildAtBirthPMTCT_E[ia] + Male.PosChildAfterBirth_E[ia] +
				Female.PosChildAtBirthNoPMTCT_E[ia] + Female.PosChildAtBirthPMTCT_E[ia] + 
				Female.PosChildAfterBirth_E[ia];*/
			// Code for calculating distribution of AIDS deaths by month
			/*TreatedMortAgeProfile.out[CurrSim-1][ia] = Male.AIDSdeaths[ia] +
				Female.AIDSdeaths[ia] - Male.AIDSdeathsUntreated[ia] -
				Female.AIDSdeathsUntreated[ia];
			UntreatedMortAgeProfile.out[CurrSim-1][ia] = Male.AIDSdeathsUntreated[ia] +
				Female.AIDSdeathsUntreated[ia];*/
		}
	}

	// Calculate # tests performed
	TestsPerformed.out[CurrSim-1][CurrYear-1985] = NewTestsPerformed;

	// Calculate ART initiation outputs
	temp = 0.0;
	denominator = 0.0;
	for(ia=0; ia<15; ia++){
		temp += StartingARTtotal[ia];
		denominator += EligibleTotal[ia] - 0.5 * EligibleMonthly[ia][0] + 0.5 * EligibleMonthly[ia][12];
	}
	NewOnART.out[CurrSim-1][CurrYear-1985] = temp;
	TotalARTinitiation.out[CurrSim-1][CurrYear-1985] = temp/(denominator/12.0);
	if(temp!=0.0){
		StartingARTunder12.out[CurrSim-1][CurrYear-1985] = StartingARTtotal[0]/temp;
		StartingART12to23.out[CurrSim-1][CurrYear-1985] = StartingARTtotal[1]/temp;
		StartingART24to59.out[CurrSim-1][CurrYear-1985] = (StartingARTtotal[2] + StartingARTtotal[3] +
			StartingARTtotal[4])/temp;
	}

	// Calculate HIV incidence statistics
	NewHIVatBirth.out[CurrSim-1][CurrYear-1985] = NewHIVatBirthTotal;
	NewHIVafterBirth.out[CurrSim-1][CurrYear-1985] = NewHIVafterBirthTotal;
	if(NonVertTransm==1){
		NewNonVertInc.out[CurrSim-1][CurrYear-1985] = Male.NewNonVertCurrY + Female.NewNonVertCurrY;}

	// Calculate demographic outputs
	IMR.out[CurrSim-1][CurrYear-1985] = 1.0 - exp(-CentralMort[0]);
	U5MR.out[CurrSim-1][CurrYear-1985] = 1.0 - exp(-(CentralMort[0] + CentralMort[1] +
		CentralMort[2] + CentralMort[3] + CentralMort[4]));
	Deaths0to4.out[CurrSim-1][CurrYear-1985] = DeathsTotal[0] + DeathsTotal[1] +
		DeathsTotal[2] + DeathsTotal[3] + DeathsTotal[4];
	DeathsYr1.out[CurrSim-1][CurrYear-1985] = DeathsTotal[0];
	Deaths1to4.out[CurrSim-1][CurrYear-1985] = DeathsTotal[1] +
		DeathsTotal[2] + DeathsTotal[3] + DeathsTotal[4];
	Deaths5to9.out[CurrSim-1][CurrYear-1985] = DeathsTotal[5] + DeathsTotal[6] +
		DeathsTotal[7] + DeathsTotal[8] + DeathsTotal[9];
	Deaths10to14.out[CurrSim-1][CurrYear-1985] = DeathsTotal[10] + DeathsTotal[11] +
		DeathsTotal[12] + DeathsTotal[13] + DeathsTotal[14];
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += DeathsTotal[ia];}
	Deaths0to14.out[CurrSim-1][CurrYear-1985] = temp;

	// Calculate other outputs
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += AIDSdeathsTotal[ia];}
	AIDSdeaths.out[CurrSim-1][CurrYear-1985] = temp;
	AIDSdeathsYr1.out[CurrSim-1][CurrYear-1985] = AIDSdeathsTotal[0];
	AIDSdeaths1to4.out[CurrSim-1][CurrYear-1985] = AIDSdeathsTotal[1] + 
		AIDSdeathsTotal[2] + AIDSdeathsTotal[3] + AIDSdeathsTotal[4];
	AIDSdeaths5to9.out[CurrSim-1][CurrYear-1985] = AIDSdeathsTotal[5] + 
		AIDSdeathsTotal[6] + AIDSdeathsTotal[7] + AIDSdeathsTotal[8] + 
		AIDSdeathsTotal[9];
	AIDSdeaths10to14.out[CurrSim-1][CurrYear-1985] = AIDSdeathsTotal[10] + 
		AIDSdeathsTotal[11] + AIDSdeathsTotal[12] + AIDSdeathsTotal[13] +
		AIDSdeathsTotal[14];
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += NotNeedingTotal[ia];}
	NotNeeding.out[CurrSim-1][CurrYear-1985] = temp;
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += ARTeligibleTotal[ia];}
	UntreatedNeed.out[CurrSim-1][CurrYear-1985] = temp;
	//UntreatedNeed.out[CurrSim-1][CurrYear-1985] = temp + NotNeedingTotal[0];
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += OnARTtotal[ia];}
	TreatedNeed.out[CurrSim-1][CurrYear-1985] = temp;
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += (ARTeligibleTotal[ia] + OnARTtotal[ia]) * (ia + 0.5);}
	MeanAgeNeed.out[CurrSim-1][CurrYear-1985] = temp/
		(UntreatedNeed.out[CurrSim-1][CurrYear-1985] + TreatedNeed.out[CurrSim-1][CurrYear-1985]);
	temp = 0.0;
	for(ia=0; ia<15; ia++){
		temp += OffARTtotal[ia];}
	DiscontinuedART.out[CurrSim-1][CurrYear-1985] = temp;
}

void GetMonthlyResults()
{
	int ia, im;

	NewHIVafterBirthMonthly[CurrMonth] = 0.0;
	for(ia=0; ia<15; ia++){
		AliveMonthly[ia][CurrMonth] = 0.0;
		EligibleMonthly[ia][CurrMonth] = 0.0; 
		NonAIDSdeathsMonthly[ia][CurrMonth] = 0.0;
		AIDSdeathsMonthly[ia][CurrMonth] = 0.0;
		AIDSdeathsUntreatedMonthly[ia][CurrMonth] = 0.0;
		StartingARTmonthly[ia][CurrMonth] = 0.0;
		for(im=0; im<12; im++){
			AliveMonthly[ia][CurrMonth] += Male.Total[ia*12+im] + Female.Total[ia*12+im];
			EligibleMonthly[ia][CurrMonth] += Male.ARTeligible[ia*12+im] + 
				Female.ARTeligible[ia*12+im]; 
			NonAIDSdeathsMonthly[ia][CurrMonth] += Male.NonAIDSdeaths[ia*12+im] +
				Female.NonAIDSdeaths[ia*12+im];
			AIDSdeathsMonthly[ia][CurrMonth] += Male.AIDSdeaths[ia*12+im] +
				Female.AIDSdeaths[ia*12+im];
			AIDSdeathsUntreatedMonthly[ia][CurrMonth] += Male.AIDSdeathsUntreated[ia*12+im] +
				Female.AIDSdeathsUntreated[ia*12+im];
			StartingARTmonthly[ia][CurrMonth] += Male.StartingART[ia*12+im] +
				Female.StartingART[ia*12+im];
			NewHIVafterBirthMonthly[CurrMonth] += Male.NewHIVpostnatal[ia*12+im] +
				Female.NewHIVpostnatal[ia*12+im];
		}
		DeathsMonthly[ia][CurrMonth] = NonAIDSdeathsMonthly[ia][CurrMonth] +
			AIDSdeathsMonthly[ia][CurrMonth];
	}
	NewHIVatBirthMonthly[CurrMonth] = Male.PosChildAtBirthNoPMTCT_E[0] + 
		Male.PosChildAtBirthPMTCT_E[0] + Female.PosChildAtBirthNoPMTCT_E[0] +
		Female.PosChildAtBirthPMTCT_E[0];

	if(CurrMonth==11){
		for(ia=0; ia<15; ia++){
			AliveMonthly[ia][12] = 0.0;
			EligibleMonthly[ia][12] = 0.0; 
			for(im=0; im<12; im++){
				AliveMonthly[ia][12] += Male.Total_E[ia*12+im] + Female.Total_E[ia*12+im];
				EligibleMonthly[ia][12] += Male.ARTeligible_E[ia*12+im] + 
					Female.ARTeligible_E[ia*12+im]; 
			}
		}
	}

	if(CurrMonth==0){
		for(ia=0; ia<15; ia++){
			ChildrenAlive[ia][0] = 0.0;
			ChildrenAlive[ia][1] = 0.0;
			ChildrenPositive[ia][0] = 0.0;
			ChildrenPositive[ia][1] = 0.0;
			NotNeedingTotal[ia] = 0.0;
			ARTeligibleTotal[ia] = 0.0;
			OnARTtotal[ia] = 0.0;
			OffARTtotal[ia] = 0.0;
			for(im=0; im<12; im++){
				ChildrenAlive[ia][0] += Male.Total[ia*12+im];
				ChildrenAlive[ia][1] += Female.Total[ia*12+im];
				ChildrenPositive[ia][0] += Male.PosChildAtBirthNoPMTCT[ia*12+im] +
					Male.PosChildAtBirthPMTCT[ia*12+im] + Male.PosChildAfterBirth[ia*12+im] + 
					Male.ARTeligible[ia*12+im] + Male.OnARTearly[ia*12+im] + 
					Male.OnARTlate1st3m[ia*12+im] + Male.OnARTlateAfter3m[ia*12+im] +
					Male.StoppedART[ia*12+im];
				ChildrenPositive[ia][1] += Female.PosChildAtBirthNoPMTCT[ia*12+im] +
					Female.PosChildAtBirthPMTCT[ia*12+im] + Female.PosChildAfterBirth[ia*12+im] + 
					Female.ARTeligible[ia*12+im] + Female.OnARTearly[ia*12+im] + 
					Female.OnARTlate1st3m[ia*12+im] + Female.OnARTlateAfter3m[ia*12+im] +
					Female.StoppedART[ia*12+im];
				NotNeedingTotal[ia] += Male.PosChildAtBirthNoPMTCT[ia*12+im] +
					Male.PosChildAtBirthPMTCT[ia*12+im] + Male.PosChildAfterBirth[ia*12+im] +
					Female.PosChildAtBirthNoPMTCT[ia*12+im] + Female.PosChildAtBirthPMTCT[ia*12+im] + 
					Female.PosChildAfterBirth[ia*12+im];
				ARTeligibleTotal[ia] += Male.ARTeligible[ia*12+im] + Female.ARTeligible[ia*12+im];
				OnARTtotal[ia] += Male.OnARTearly[ia*12+im] + Male.OnARTlate1st3m[ia*12+im] +
					Male.OnARTlateAfter3m[ia*12+im] + Female.OnARTearly[ia*12+im] + 
					Female.OnARTlate1st3m[ia*12+im] + Female.OnARTlateAfter3m[ia*12+im];
				OffARTtotal[ia] += Male.StoppedART[ia*12+im] + Female.StoppedART[ia*12+im];
			}
		}
	}
}

void GetAnnualResults()
{
	int ia, im;

	for(ia=0; ia<15; ia++){
		AliveTotal[ia] = 0.0; 
		EligibleTotal[ia] = 0.0;
		NonAIDSdeathsTotal[ia] = 0.0;
		AIDSdeathsTotal[ia] = 0.0;
		AIDSdeathsUntreatedTotal[ia] = 0.0;
		StartingARTtotal[ia] = 0.0;
		for(im=0; im<12; im++){
			AliveTotal[ia] += AliveMonthly[ia][im]; 
			EligibleTotal[ia] += EligibleMonthly[ia][im];
			NonAIDSdeathsTotal[ia] += NonAIDSdeathsMonthly[ia][im];
			AIDSdeathsTotal[ia] += AIDSdeathsMonthly[ia][im];
			AIDSdeathsUntreatedTotal[ia] += AIDSdeathsUntreatedMonthly[ia][im];
			StartingARTtotal[ia] += StartingARTmonthly[ia][im];
		}
		DeathsTotal[ia] = NonAIDSdeathsTotal[ia] + AIDSdeathsTotal[ia];
		CentralMort[ia] = DeathsTotal[ia]/((AliveTotal[ia] - 0.5 * AliveMonthly[ia][0] +
			0.5 * AliveMonthly[ia][12])/12.0);
	}

	NewHIVatBirthTotal = 0.0;
	NewHIVafterBirthTotal = 0.0;
	for(im=0; im<12; im++){
		NewHIVatBirthTotal += NewHIVatBirthMonthly[im];
		NewHIVafterBirthTotal += NewHIVafterBirthMonthly[im];
	}
}

void OneYear()
{
	int im;

	CurrYear +=1;
	SetCurrYearParameters();
	Male.NewNonVertCurrY = 0.0;
	Female.NewNonVertCurrY = 0.0;
	for(im=0; im<12; im++){
		CurrMonth = im;
		Male.GetEndProfile1();
		Female.GetEndProfile1();
		if(StartingART>0 || CurrYear>=InterpolationStart){
			SetARTinitiationRates();}
		Male.GetEndProfile2();
		Female.GetEndProfile2();
		GetMonthlyResults();
		Male.UpdateStartProfile();
		Female.UpdateStartProfile();
	}
	GetAnnualResults();
	GetCalibOutput();
	if(FixedUncertainty==1){CalcOutput();}
	if(CurrYear==InterpolationStart-1){
		ARTinitiationStore = ARTinitiation[0];}
}

void CalcLikelihood()
{
	int ia, ig;
	double SElogitPrev05, SElogitPrev08;

	LogLikelihood = 0.0;
	for(ia=0; ia<3; ia++){
		for(ig=0; ig<2; ig++){
			SElogitPrev05 = SEprev05[ia][ig]/(ObservedPrev05[ia][ig] * (1.0 - ObservedPrev05[ia][ig]));
			LogLikelihood -= 0.5 * pow((log(ModelPrev05[ia][ig]/(1.0 - ModelPrev05[ia][ig])) - 
				log(ObservedPrev05[ia][ig]/(1.0 - ObservedPrev05[ia][ig])))/SElogitPrev05, 2.0);
			SElogitPrev08 = SEprev08[ia][ig]/(ObservedPrev08[ia][ig] * (1.0 - ObservedPrev08[ia][ig]));
			LogLikelihood -= 0.5 * pow((log(ModelPrev08[ia][ig]/(1.0 - ModelPrev08[ia][ig])) - 
				log(ObservedPrev08[ia][ig]/(1.0 - ObservedPrev08[ia][ig])))/SElogitPrev08, 2.0);
		}
	}
	SElogitPrev08 = SEprevU208/(ObservedPrevU208 * (1.0 - ObservedPrevU208));
	LogLikelihood -= 0.5 * pow((log(ModelPrevU208/(1.0 - ModelPrevU208)) - 
		log(ObservedPrevU208/(1.0 - ObservedPrevU208)))/SElogitPrev08, 2.0);
	PrevLogL.out[CurrSim-1][0] = LogLikelihood;

	if(InclMortData==1){
		CalcMortLikelihood2();}
}

void CalcMortLikelihood()
{
	int ia, iy, ip, ir, id, InsufficientMortInd;
	double ymat[50], xmat[50][14], xTxmat[14][14], xTxinv[14][14], xTymat[14], BetaHat[14];
	double Sigma2hat;
	double completeness[10][5], teststat;

	InsufficientMortInd = 0;
	for(ia=0; ia<5; ia++){
		for(iy=0; iy<10; iy++){
			if(RecordedDeaths[ia][iy] > ModelDeaths[ia][iy]){
				InsufficientMortInd = 1;}
			else{
				ymat[ia*10+iy] = log((RecordedDeaths[ia][iy]/ModelDeaths[ia][iy])/(1.0 -
					RecordedDeaths[ia][iy]/ModelDeaths[ia][iy]));
			}
		}
	}
	if(InsufficientMortInd==1){
		LogLikelihood += -10000.00;} // Arbitrarily low value to prevent inclusion in resample
	/*else{
		for(ia=0; ia<5; ia++){
			for(iy=0; iy<10; iy++){
				xmat[ia*10+iy][iy] = 1.0;
				if(ia>0){xmat[ia*10+iy][9+ia] = 1.0;}
			}
		}
		for(ip=0; ip<14; ip++){
			for(ir=0; ir<14; ir++){
				xTxmat[ip][ir] = 0.0;
				for(id=0; id<50; id++){
					xTxmat[ip][ir] += xmat[id][ip] * xmat[id][ir];}
			}
		}
		GetInverse1(xTxmat, xTxinv);
		for(ip=0; ip<14; ip++){
			xTymat[ip] = 0.0;
			for(id=0; id<50; id++){
				xTymat[ip] += xmat[id][ip] * ymat[id];}
		}
		for(ip=0; ip<14; ip++){
			BetaHat[ip] = 0.0;
			for(ir=0; ir<14; ir++){
				BetaHat[ip] += xTxinv[ip][ir] * xTymat[ir];}
		}
		Sigma2hat = 0.0;
		for(iy=0; iy<10; iy++){
			Sigma2hat += pow(ymat[iy] - BetaHat[iy], 2.0);}
		for(ia=1; ia<5; ia++){
			for(iy=0; iy<10; iy++){
				Sigma2hat += pow(ymat[ia*10+iy] - BetaHat[iy] - BetaHat[9+ia], 2.0);}
		}
		Sigma2hat = Sigma2hat/50.0;
		for(iy=0; iy<10; iy++){
			LogLikelihood -= 0.5 * (pow(ymat[iy] - BetaHat[iy], 2.0)/Sigma2hat + 
				log(Sigma2hat));
		}
		for(ia=1; ia<5; ia++){
			for(iy=0; iy<10; iy++){
				LogLikelihood += 0.5 * (-pow(ymat[ia*10+iy] - BetaHat[iy] - BetaHat[9+ia], 2.0)/
					Sigma2hat - log(Sigma2hat));
			}
		}
	}
	if(BetaHat[10] < 0.0){ // Insufficient mortality at older ages
		InsufficientMortInd = 1;}
	for(ia=1; ia<4; ia++){
		if(BetaHat[10+ia] < BetaHat[9+ia]){ // Insufficient mortality at older ages
			InsufficientMortInd = 1;}
	}
	if(InsufficientMortInd==1){
		LogLikelihood = -10000.00;}*/ // Arbitrarily low value to prevent inclusion in resample
	else{
		for(ia=0; ia<5; ia++){
			for(iy=0; iy<10; iy++){
				completeness[ia][iy] = RecordedDeaths[ia][iy]/ModelDeaths[ia][iy];}
		}
		for(ia=1; ia<5; ia++){
			for(iy=0; iy<10; iy++){
				teststat = (completeness[ia-1][iy] - completeness[ia][iy])/pow(
					completeness[ia-1][iy]/ModelDeaths[ia-1][iy] + completeness[ia][iy]/
					ModelDeaths[ia][iy], 0.5);
				if(teststat>2.33){ // 2.33 is the 99th percentile of the standard normal dbn
					LogLikelihood += -3.00;}
			}
		}
	}
}

void CalcMortLikelihood2()
{
	int ia, iy, id;
	double Sigma2hat, NewtonEst[4], NewtonFunction[3], NewtonDeriv[3];
	double completeness[5][10], YearEffect[10], completed;

	for(iy=0; iy<10; iy++){
		NewtonEst[0] = 0.0;
		for(ia=0; ia<5; ia++){
			completeness[ia][iy] = RecordedDeaths[ia][iy]/ModelDeaths[ia][iy];
			NewtonEst[0] += (completeness[ia][iy] - RelCompleteness[ia])/
				(1.0 - RelCompleteness[ia]);
		}
		NewtonEst[0] = NewtonEst[0]/5.0;
		for(id=0; id<3; id++){
			if(NewtonEst[id]<=0.0){
				NewtonEst[id] = 0.0001;}
			NewtonFunction[id] = 0.0;
			NewtonDeriv[id] = 0.0;
			for(ia=0; ia<5; ia++){
				completed = NewtonEst[id] * (1.0 - RelCompleteness[ia]) +
					RelCompleteness[ia];
				NewtonFunction[id] += (1.0 - RelCompleteness[ia]) * (log(completeness[ia][iy]) -
					log(completed))/completed;
				NewtonDeriv[id] += pow(1.0 - RelCompleteness[ia], 2.0) * (log(completed) -
					log(completeness[ia][iy]) - 1.0)/pow(completed, 2.0);
			}
			NewtonEst[id+1] = NewtonEst[id] - NewtonFunction[id]/NewtonDeriv[id];
		}
		YearEffect[iy] = NewtonEst[3];
		if(YearEffect[iy] > 1.0){
			YearEffect[iy] = 1.0;}
		if(YearEffect[iy] <= 0.0){
			YearEffect[iy] = 0.0001;}
	}
	Sigma2hat = 0.0;
	for(iy=0; iy<10; iy++){
		for(ia=0; ia<5; ia++){
			Sigma2hat += pow(log(completeness[ia][iy]) - log(YearEffect[iy] * (1.0 - 
				RelCompleteness[ia]) + RelCompleteness[ia]), 2.0);
		}
	}
	Sigma2hat = Sigma2hat/50.0;
	for(ia=0; ia<5; ia++){
		for(iy=0; iy<10; iy++){
			LogLikelihood += 0.5 * (-pow(log(completeness[ia][iy]) - log(YearEffect[iy] * 
				(1.0 - RelCompleteness[ia]) + RelCompleteness[ia]), 2.0)/
				Sigma2hat - log(Sigma2hat));
		}
	}
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
	int ind, i;
	double x, y, a, b, p, q, bound;
	double r[MCMCdim]; // Random variables from U(0, 1)

	ind = 2;
	bound = 0.0;

	if(FixedUncertainty==0){
		int32 seed = time(0) + CurrSim + InitSample;
		CRandomMersenne rg(seed);
		for(i=0; i<RandomUniform.columns; i++){
			r[i] = rg.Random();
			RandomUniform.out[CurrSim-1][i] = r[i];
		}
	}
	else{
		for(i=0; i<RandomUniform.columns; i++){
			r[i] = RandomUniform.out[CurrSim-1][i];}
	}

	// Simulate propn of women testing positive who receive short-course ART
	a = 13.3125;
	b = 4.43750;
	p = r[0];
	q = 1 - r[0];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	NVPuptake = x;
	PaedParameters.out[CurrSim-1][0] = x;

	// Simulate prob of MTCT per month of mixed BF in 1st 3 months after mother seroconverts
	a = 23.7333;
	b = 124.600;
	p = r[1];
	q = 1 - r[1];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	TransmBFacute = x;
	PaedParameters.out[CurrSim-1][1] = x;

	// Simulate RR of MTCT after birth if mother breastfeeds exclusively
	a = 5.05556;
	b = 5.05556;
	p = r[2];
	q = 1 - r[2];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	RRforEBF = x;
	PaedParameters.out[CurrSim-1][2] = x;

	// Simulate prob of MTCT at birth if mother seroconverts after first antenatal visit
	a = 12.0941;
	b = 22.4555;
	p = r[3];
	q = 1 - r[3];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	TransmAcute = x;
	PaedParameters.out[CurrSim-1][3] = x;

	// Simulate the annual rate of progression to ART eligibility in older children
	a = 16.0000;
	b = 40.0000;
	p = r[4];
	q = 1 - r[4];
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	ProgToNeedLT = x;
	PaedParameters.out[CurrSim-1][4] = x;

	// Simulate the excess annual rate of progression to ART eligibility in newborn
	a = 64.0000;
	b = 32.0000;
	p = r[5];
	q = 1 - r[5];
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	ExcessProgToNeed = x;
	PaedParameters.out[CurrSim-1][5] = x;

	// Simulate relative rate of progression to ART eligibility, if infected after birth 
	a = 3.18889;
	b = 5.92222;
	p = r[6];
	q = 1 - r[6];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	RRprogressionPostnatal = x;
	PaedParameters.out[CurrSim-1][6] = x;

	// Simulate the annual rate of AIDS mortality in older children who are untreated & ART-eligible
	a = 16.0000;
	b = 133.333;
	p = r[7];
	q = 1 - r[7];
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	AIDSmortLT = x;
	PaedParameters.out[CurrSim-1][7] = x;

	// Simulate the excess annual rate of AIDS mortality in newborn who are untreated & ART-eligible
	a = 100.000;
	b = 28.5714;
	p = r[8];
	q = 1 - r[8];
	cdfgam(&ind,&p,&q,&x,&a,&b,0,&bound);
	ExcessAIDSmort = x;
	PaedParameters.out[CurrSim-1][8] = x;

	// Simulate the annual rate of MTCT by mixed breastfeeding
	a = 26.8296;
	b = 164.810;
	p = r[9];
	q = 1 - r[9];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	TransmBFfirst3 = 1.0 - pow(1.0 - x, 1.0/12.0);
	TransmBFafter3 = TransmBFfirst3;
	PaedParameters.out[CurrSim-1][9] = x;

	// Simulate the adjustment to the rate or progression in kids not exposed to PMTCT
	/*a = 12.0000;
	b = 3.00000;
	p = r[10];
	q = 1 - r[10];
	cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
	ProgAdjNoPMTCT = x;
	PaedParameters.out[CurrSim-1][10] = x;*/

	// Simulate the bias adjustment applied to the excess mort in non-breastfed children
	//BFbiasAdj = r[11];
	//PaedParameters.out[CurrSim-1][11] = r[11];

	if(NonVertTransm==1){
		a = 1.54969;
		b = 308.388;
		p = r[10];
		q = 1 - r[10];
		cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
		NonVertIncidence2004 = x;
		PaedParameters.out[CurrSim-1][10] = x;
	}

	if(InclMortData==1){
		// Simulate the constant parameter in relational logit system for qx
		a = 0.00000;
		b = 0.05000;
		p = r[12];
		q = 1 - r[12];
		cdfnor(&ind,&p,&q,&x,&a,&b,0,&bound);
		ConstantLogitq = x;
		PaedParameters.out[CurrSim-1][12] = x;

		// Simulate the slope parameter in relational logit system for qx
		a = 1.00000;
		b = 0.05000;
		p = r[13];
		q = 1 - r[13];
		cdfnor(&ind,&p,&q,&x,&a,&b,0,&bound);
		SlopeLogitq = x;
		PaedParameters.out[CurrSim-1][13] = x;

		// Simulate the average annual reduction in non-HIV mortality
		a = 0.02000;
		b = 0.00300;
		p = r[14];
		q = 1 - r[14];
		cdfnor(&ind,&p,&q,&x,&a,&b,0,&bound);
		AveAnnMortRedn = x;
		PaedParameters.out[CurrSim-1][14] = x;

		// Simulate the increase in average ann mortality reduction per yr of age
		a = -0.00140;
		b = 0.00070;
		p = r[15];
		q = 1 - r[15];
		cdfnor(&ind,&p,&q,&x,&a,&b,0,&bound);
		IncrMortRednWRTage = x;
		PaedParameters.out[CurrSim-1][15] = x;

		// Simulate the completeness of reported deaths in infants in 1996
		/*a = 41.7272;
		b = 55.3128;
		p = r[16];
		q = 1 - r[16];
		cdfbet(&ind,&p,&q,&x,&y,&a,&b,0,&bound);
		Completeness96[0] = x;
		PaedParameters.out[CurrSim-1][16] = x;*/

		// Simulate the completeness of reported deaths at older ages
		RelCompleteness[0] = 0.0;
		RelCompleteness[1] = r[16]/(1.0 + r[16] + r[17] + r[18] + r[19]);
		RelCompleteness[2] = (r[16] + r[17])/
			(1.0 + r[16] + r[17] + r[18] + r[19]);
		RelCompleteness[3] = (r[16] + r[17] + r[18])/
			(1.0 + r[16] + r[17] + r[18] + r[19]);
		RelCompleteness[4] = (r[16] + r[17] + r[18] + r[19])/
			(1.0 + r[16] + r[17] + r[18] + r[19]);
		PaedParameters.out[CurrSim-1][16] = r[16];
		PaedParameters.out[CurrSim-1][17] = r[17];
		PaedParameters.out[CurrSim-1][18] = r[18];
		PaedParameters.out[CurrSim-1][19] = r[19];
	}

	if(FixedUncertainty==0 && IMISind==1){
		for(i=0; i<MCMCdim; i++){
			if(PriorTypes[i]==0){ // Beta prior
				RandomParameterIMIS[i][CurrSim-1] = log(PaedParameters.out[CurrSim-1][i]/
					(1.0 - PaedParameters.out[CurrSim-1][i]));}
			else if(PriorTypes[i]==1){ // Gamma prior
				RandomParameterIMIS[i][CurrSim-1] = log(PaedParameters.out[CurrSim-1][i]);}
			else{ // Normal prior
				RandomParameterIMIS[i][CurrSim-1] = PaedParameters.out[CurrSim-1][i];}
		}
	}
}

void GenerateSample()
{
	int i, j, iy;
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
			ReadAssumptions();
			ReadASSAinputs();
			ReadPhaseIn();
			ReadHIVprevData();
			if(InclMortData==1){ReadMortData();}
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
	int32 seed = time(0);
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
	RandomUniform.RecordSample("RandomUniform.txt", 8);
}

void RunSample()
{
	// Remember to set FixedUncertainty to 1 before calling this function.

	std::ifstream file1;
	char filout[18];
	int i, c, ia, iy, idum;

	// Read in random numbers for each parameter combination in sample
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
		file1.open("PaedParameters.txt");
		for(i=0; i<ResampleSize; i++){
			file1>>idum>>idum;
			for(c=0; c<PaedParameters.columns; c++){
				file1>>PaedParameters.out[i][c];}
		}
		file1.close();
	}

	// Run the model for each of the sampled parameter combinations
	CurrSim = 0;
	for(i=1; i<=ResampleSize; i++){ // Same code as in the GenerateSample function
		CurrSim += 1;
		if(i==1){
			ReadAssumptions();
			ReadASSAinputs();
			ReadPhaseIn();
			ReadHIVprevData();
			if(FixedARTinitiation==1){
				ReadARTinitiation();}
		}
		if(IMISind==0){
			SimulateParameters();}
		else{
			SimulateParameters_IMIS();}
		SetInitialParameters();
	
		CurrYear = StartYear-1;
		for(iy=0; iy<ProjectionTerm; iy++){
			OneYear();
		}
		//CalcLikelihood();
		//LogL.out[i-1][0] = LogLikelihood;
	}

	// Write to text files
	Prev0to14.RecordSample("Prev0to14.txt");
	Prev0to1.RecordSample("Prev0to1.txt");
	Prev2to4M.RecordSample("Prev2to4M.txt");
	Prev2to4F.RecordSample("Prev2to4F.txt");
	Prev5to9M.RecordSample("Prev5to9M.txt");
	Prev5to9F.RecordSample("Prev5to9F.txt");
	Prev10to14M.RecordSample("Prev10to14M.txt");
	Prev10to14F.RecordSample("Prev10to14F.txt");
	Prev2004.RecordSample("Prev2004.txt");
	HIVageProfile2000.RecordSample("HIVageProfile2000.txt");
	HIVageProfile2010.RecordSample("HIVageProfile2010.txt");
	UntreatedAgeProfile.RecordSample("UntreatedAgeProfile.txt");
	OnARTageProfile.RecordSample("OnARTageProfile.txt");
	NotNeedingAgeProfile.RecordSample("NotNeedingAgeProfile.txt");
	TreatedMortAgeProfile.RecordSample("TreatedMortAgeProfile.txt");
	UntreatedMortAgeProfile.RecordSample("UntreatedMortAgeProfile.txt");
	TotalHIV.RecordSample("TotalHIV.txt");
	TestsPerformed.RecordSample("TestsPerformed.txt");
	NewOnART.RecordSample("NewOnART.txt");
	if(FixedARTinitiation==0){
		TotalARTinitiation.RecordSample("TotalARTinitiation.txt");}
	StartingARTunder12.RecordSample("StartingARTunder12.txt");
	StartingART12to23.RecordSample("StartingART12to23.txt");
	StartingART24to59.RecordSample("StartingART24to59.txt");
	NewHIVatBirth.RecordSample("NewHIVatBirth.txt");
	NewHIVafterBirth.RecordSample("NewHIVafterBirth.txt");
	if(NonVertTransm==1){
		NewNonVertInc.RecordSample("NewNonVertInc.txt");}
	AIDSdeaths.RecordSample("AIDSdeaths.txt");
	AIDSdeathsM1.RecordSample("AIDSdeathsM1.txt");
	AIDSdeathsYr1.RecordSample("AIDSdeathsYr1.txt");
	AIDSdeaths1to4.RecordSample("AIDSdeaths1to4.txt");
	AIDSdeaths5to9.RecordSample("AIDSdeaths5to9.txt");
	AIDSdeaths10to14.RecordSample("AIDSdeaths10to14.txt");
	NotNeeding.RecordSample("NotNeeding.txt");
	UntreatedNeed.RecordSample("UntreatedNeed.txt");
	TreatedNeed.RecordSample("TreatedNeed.txt");
	MeanAgeNeed.RecordSample("MeanAgeNeed.txt");
	DiscontinuedART.RecordSample("DiscontinuedART.txt");
	VertTransm.RecordSample("VertTransm.txt");
	VertTransmKnownPos.RecordSample("VertTransmKnownPos.txt");
	VertTransmGotART.RecordSample("VertTransmGotART.txt");
	IMR.RecordSample("IMR.txt");
	U5MR.RecordSample("U5MR.txt");
	Deaths0to4.RecordSample("Deaths0to4.txt");
	DeathsYr1.RecordSample("DeathsYr1.txt");
	DeathsM1.RecordSample("DeathsM1.txt");
	Deaths1to4.RecordSample("Deaths1to4.txt");
	Deaths5to9.RecordSample("Deaths5to9.txt");
	Deaths10to14.RecordSample("Deaths10to14.txt");
	Deaths0to14.RecordSample("Deaths0to14.txt");
	AgeDbnIMR.RecordSample("AgeDbnIMR.txt");
}

void ReadPriors()
{
	int ir, idum;
	std::ifstream file;

	file.open("Priors.txt");
	if(file==0)std::cout<<"File open error"<<std::endl;
	for(ir=0; ir<MCMCdim; ir++){
		file>>idum>>PriorTypes[ir]>>aParameters[ir]>>bParameters[ir];}
	file.close();
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

void runIMIS()
{
	int i, j, k, ir, ic, totsim;
	double WeightedMean[MCMCdim], sumweights, sumweights2, denom;
	double WeightedCov[MCMCdim][MCMCdim], Inverse[MCMCdim][MCMCdim];
	//double distance[TotalSimulations]; // Mahalanobis distance
	double DistancePropn, cutoff, minweight;
	char filout[18];
	double lookup, r[ResampleSize]; 
	double MaxLogLxWeight;
	//double CumLxWeight[TotalSimulations+1];

	ReadPriors();
	totsim = TotalSimulations;
	for(i=0; i<IMISsteps; i++){
		//if(ErrorInd==1){break;}
		CurrIMISstep = i;
		OneIMISstep();

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
		if(FractionUnique[i] >= 1.0 - exp(-1.0)){
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
		std::cout<<"Finished IMIS step "<<i+1<<std::std::endl;
	}
	
	// Based on the code in the GenerateSample function

	// Calculate likelihood for each simulation
	CumLxWeight[0] = 0.0;
	for(i=0; i<totsim; i++){
		CumLxWeight[i+1] = CumLxWeight[i] + exp(LogLxWeight[i] - MaxLogLxWeight);}
	
	// Generate random variables from the uniform (0, 1) distribution
	int32 seed = time(0);
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
		for(j=0; j<PaedParameters.columns; j++){
			if(PriorTypes[j]==0){
				PaedParameters.out[i][j] = 1.0/(1.0 + exp(-RandomParameterIMIS[j][SampleID[i]]));}
			else if(PriorTypes[j]==1){
				PaedParameters.out[i][j] = exp(RandomParameterIMIS[j][SampleID[i]]);}
			else{
				PaedParameters.out[i][j] = RandomParameterIMIS[j][SampleID[i]];}
		}
	}
	PaedParameters.RecordSample("PaedParameters.txt", 8);

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
}

void OneIMISstep()
{
	// This function is very similar to the GenerateSample function in the SIR analysis
	// (and most of the code is copied from the GenerateSample function). But unlike the
	// GenerateSample function, there is no resample code in this function, i.e. we are
	// only calculating the likelihood for each parameter combination. Also note that this
	// function has code at the end to recalculate the ratio of prior to sampling density
	// in respect of ALL parameter combinations generated so far.

	int i, j, k, iy, NewSimulations, Nk, offset;
	double SamplingDensity[IMISsteps], PriorDensity, MultNorm[MCMCdim], TransfNorm[MCMCdim];
	double denominator, x, y, a, b;

	CurrSim = 0;
	if(CurrIMISstep==0){
		NewSimulations = InitSample;}
	else{
		NewSimulations = StepSample;}
	for(i=1; i<=NewSimulations; i++){
		CurrSim += 1;
		if(CurrIMISstep==0){
			offset = CurrSim - 1;}
		else{
			offset = InitSample + StepSample * (CurrIMISstep - 1) + CurrSim - 1;}
		CurrYear = StartYear;

		if(i==1){
			ReadAssumptions();
			ReadASSAinputs();
			ReadPhaseIn();
			ReadHIVprevData();
			if(InclMortData==1){ReadMortData();}
		}
		if(IMISind==0 || (IMISind==1 && FixedUncertainty==0 && CurrIMISstep==0)){
			SimulateParameters();}
		else{
			SimulateParameters_IMIS();}
		SetInitialParameters();
	
		CurrYear = StartYear-1;
		for(iy=0; iy<ProjectionTerm; iy++){
			OneYear();}
		CalcLikelihood();
		LogL.out[i-1][0] = LogLikelihood;
		RandomParameterIMIS[MCMCdim][offset] = LogLikelihood;
	}

	// Now recalculate the ratio of prior to sampling density in respect of ALL parameter
	// combinations calculated thus far.
	if(CurrIMISstep>0){
		Nk = InitSample + CurrIMISstep * StepSample;
		for(i=0; i<Nk; i++){
			PriorDensity = 0.0;
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
			}
			denominator = 1.0 * InitSample/Nk;
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
	int ind, i, is, offset;
	double x, y, a, b, p, q, temp;
	double r[MCMCdim]; // Random variables from U(0, 1)
	double MultNorm[MCMCdim]; // Sampled parameters from multivariate normal dbn

	ind = 2;

	if(FixedUncertainty==0){
		offset = InitSample + (CurrIMISstep - 1) * StepSample + CurrSim - 1;
		int32 seed = time(0) + CurrSim + CurrIMISstep * StepSample;
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
			PaedParameters.out[CurrSim-1][i] = MultNorm[i];
		}
		NVPuptake = MultNorm[0];
		TransmBFacute = MultNorm[1];
		RRforEBF = MultNorm[2];
		TransmAcute = MultNorm[3];
		ProgToNeedLT = MultNorm[4];
		ExcessProgToNeed = MultNorm[5];
		RRprogressionPostnatal = MultNorm[6];
		AIDSmortLT = MultNorm[7];
		ExcessAIDSmort = MultNorm[8];
		TransmBFfirst3 = 1.0 - pow(1.0 - MultNorm[9], 1.0/12.0);
		TransmBFafter3 = TransmBFfirst3;
		//ProgAdjNoPMTCT = MultNorm[10];
		//BFbiasAdj = MultNorm[11];
		if(NonVertTransm==1){
			NonVertIncidence2004 = MultNorm[10];}
		if(InclMortData==1){
			ConstantLogitq = MultNorm[12];
			SlopeLogitq = MultNorm[13];
			AveAnnMortRedn = MultNorm[14];
			IncrMortRednWRTage = MultNorm[15];
			RelCompleteness[0] = 0.0;
			RelCompleteness[1] = MultNorm[16]/(1.0 + MultNorm[16] + MultNorm[17] +
				MultNorm[18] + MultNorm[19]);
			RelCompleteness[2] = (MultNorm[16] + MultNorm[17])/(1.0 + MultNorm[16] + 
				MultNorm[17] + MultNorm[18] + MultNorm[19]);
			RelCompleteness[3] = (MultNorm[16] + MultNorm[17] + MultNorm[18])/(1.0 +  
				MultNorm[16] + MultNorm[17] + MultNorm[18] + MultNorm[19]);
			RelCompleteness[4] = (MultNorm[16] + MultNorm[17] + MultNorm[18] + MultNorm[19])/  
				(1.0 + MultNorm[16] + MultNorm[17] + MultNorm[18] + MultNorm[19]);
		}
	}
	else{
		NVPuptake = PaedParameters.out[CurrSim-1][0];
		TransmBFacute = PaedParameters.out[CurrSim-1][1];
		//TransmBFacute = 0.0;
		RRforEBF = PaedParameters.out[CurrSim-1][2];
		TransmAcute = PaedParameters.out[CurrSim-1][3];
		ProgToNeedLT = PaedParameters.out[CurrSim-1][4];
		ExcessProgToNeed = PaedParameters.out[CurrSim-1][5];
		RRprogressionPostnatal = PaedParameters.out[CurrSim-1][6];
		AIDSmortLT = PaedParameters.out[CurrSim-1][7];
		ExcessAIDSmort = PaedParameters.out[CurrSim-1][8];
		TransmBFfirst3 = 1.0 - pow(1.0 - PaedParameters.out[CurrSim-1][9], 1.0/12.0);
		//TransmBFfirst3 = 0.0;
		TransmBFafter3 = TransmBFfirst3;
		//ProgAdjNoPMTCT = PaedParameters.out[CurrSim-1][10];
		//BFbiasAdj = PaedParameters.out[CurrSim-1][11];
		if(NonVertTransm==1){
			NonVertIncidence2004 = PaedParameters.out[CurrSim-1][10];}
		if(InclMortData==1){
			ConstantLogitq = PaedParameters.out[CurrSim-1][12];
			SlopeLogitq = PaedParameters.out[CurrSim-1][13];
			AveAnnMortRedn = PaedParameters.out[CurrSim-1][14];
			IncrMortRednWRTage = PaedParameters.out[CurrSim-1][15];
			// Technically the code below is unnecessary, since it only affects the
			// calculation of the likelihood, which is not necessary when FixedUncertainty=1
			RelCompleteness[0] = 0.0;
			temp = PaedParameters.out[CurrSim-1][16] + PaedParameters.out[CurrSim-1][17] +
				PaedParameters.out[CurrSim-1][18] + PaedParameters.out[CurrSim-1][19];
			RelCompleteness[1] = PaedParameters.out[CurrSim-1][16]/(1.0 + temp);
			RelCompleteness[2] = (PaedParameters.out[CurrSim-1][16] + 
				PaedParameters.out[CurrSim-1][17])/(1.0 + temp);
			RelCompleteness[3] = (PaedParameters.out[CurrSim-1][16] + 
				PaedParameters.out[CurrSim-1][17] + PaedParameters.out[CurrSim-1][18])/(1.0 +  temp);
			RelCompleteness[4] = temp/(1.0 + temp);
		}
	}
}
