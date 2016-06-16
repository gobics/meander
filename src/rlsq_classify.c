#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#include <stdint.h>
#include <inttypes.h>

#include <omp.h>

//only for timing
#include <time.h>

double scorecalc(int pSequence[], double ScoringVec[], int seqlength, int comprevVec[256], int offSet, int n2protect, int minDist, int maxDist, double nBias)
{
double xVec,*ProcessVec;
int *BitShift;
int nProcessCount;
int joffset;
nProcessCount = 0;

BitShift =  malloc(seqlength * sizeof(int));
ProcessVec =  calloc(n2protect, sizeof(double));
int i,j,val;


xVec = (double) nBias;
  for (i=0; i < (seqlength-1); i++)
  {
    if((pSequence[i] >= 0) & (pSequence[i] < 4) & (pSequence[i+1] >= 0) & (pSequence[i+1] < 4))
    {
    BitShift[i] = (pSequence[i] << 2) + pSequence[i+1];
    }

    else
    {
    BitShift[i] = -1;
    }
  }

  for (i=0; i<(seqlength-(maxDist+1)); i++)
  {
    for (j = minDist; j <= maxDist;j++)
    {
    joffset = ((j-minDist)*offSet);
      if ((BitShift[i] != -1) & (BitShift[i+j] != -1))
      {
        val = (BitShift[i] << 4) + BitShift[i+j];

        ProcessVec[val+joffset]++;
        ProcessVec[comprevVec[val]+joffset]++;
        nProcessCount++;
      }
    }
  }

  for(j=0; j<n2protect; j++)
  {
    xVec += (ProcessVec[j]/nProcessCount)*ScoringVec[j];
  }

free(BitShift);
free(ProcessVec);
return(xVec);
}



SEXP rlsq_classify(SEXP XVec, SEXP StartVec, SEXP StopVec, SEXP LengthVec, SEXP ScoringVec, SEXP bias, SEXP XminDist, SEXP XmaxDist)
{
	double *pReturnMat,*pScoringVec;
	double nBias;
	int64_t i;
	int *pXVec,*pStartVec,*pStopVec,*pLengthVec;

	SEXP ReturnVec;

	/*Protecting variables*/
	XVec = PROTECT(coerceVector(XVec, INTSXP));
	StartVec = PROTECT(coerceVector(StartVec, INTSXP));
	StopVec = PROTECT(coerceVector(StopVec, INTSXP));
	LengthVec = PROTECT(coerceVector(LengthVec, INTSXP));
	XminDist = PROTECT(coerceVector(XminDist, INTSXP));
	XmaxDist = PROTECT(coerceVector(XmaxDist, INTSXP));
	bias = PROTECT(coerceVector(bias, REALSXP));

	ScoringVec = PROTECT(coerceVector(ScoringVec, REALSXP));

	pXVec = INTEGER(XVec);
	pStartVec = INTEGER(StartVec);
	pStopVec = INTEGER(StopVec);
	pLengthVec = INTEGER(LengthVec);
	pScoringVec = REAL(ScoringVec);

	int minDist = asInteger(XminDist);
	int maxDist = asInteger(XmaxDist);
	nBias = asReal(bias);
	int vecLength = (1 << 8);
	int n2protect = vecLength * ((maxDist-minDist)+1);

	//printf("%lf\t%d\n", nBias, length(LengthVec));

	ReturnVec = PROTECT(allocVector(REALSXP, length(LengthVec)));
	pReturnMat = REAL(ReturnVec);


// Initialize reverse-comp Lookup
//A - T		C - G
//0 - 2		1 - 3
int CompVec[256] = {0};
int ABC[4] = {0,1,2,3};
int CBA[4] = {2,3,0,1};

int aa,ab,ac,ad;
	for (aa=0; aa<4; aa++)
	{
		for (ab=0; ab<4; ab++)
		{
			for (ac=0; ac<4; ac++)
			{
				for (ad=0; ad<4; ad++)
				{
				CompVec[(ABC[aa] << 6) + (ABC[ab] << 4) + (ABC[ac] << 2) + ABC[ad]] = (CBA[ad] << 6) + (CBA[ac] << 4) + (CBA[ab] << 2) + CBA[aa];
				}
			}
		}
	}

int nSeqLength;

nSeqLength = length(LengthVec);

#pragma omp parallel for
  for(i=0; i<nSeqLength; i++)
  {
  	pReturnMat[i] = scorecalc(&pXVec[pStartVec[i]], pScoringVec, pLengthVec[i], CompVec, vecLength, n2protect, minDist, maxDist, nBias);
  }

UNPROTECT(9);
return ReturnVec;
}
