#include <stdio.h>
#include "R.h"
#include <math.h>
#include <R_ext/Lapack.h>

void LapackGenEigen(double *A,  // square matrix A
		    double *B,  // vector x
		    double *W,  // eigen values
		    double *Z,  // eigen vectors 
		    int *IL,
		    int *IU,
		    int *nrows,
		    int *INFO)
{
  int ITYPE = 1;    // A*x = (lambda)*B*x
  char JOBZ = 'V';  // Compute eigenvalues and eigenvectors.
  char RANGE = 'I'; // the IL-th through IU-th eigenvalues will be found.
  char UPLO = 'U';  // 
  double VL, VU;    // not referenced
  //int IL=1,IU=3;    // which eigen values to get, from IL to IU
  int N = *nrows, LDA = *nrows, LDB = *nrows;
  //double abstolChar = 'S';
  //double ABSTOL = 2*DLAMCH_(&abstolChar);
  double ABSTOL = 0.0;
  int M;
  int LDZ = N;
  int LWORK = 8*N;
  double WORK[LWORK];
  int IWORK[5*N];
  int IFAIL[N];
  
  dsygvx_(&ITYPE, &JOBZ, &RANGE, &UPLO, &N, A, &LDA, B, &LDB,
	  &VL, &VU, IL, IU, &ABSTOL, &M, W, Z, &LDZ, &WORK[0],
	  &LWORK, &IWORK[0], &IFAIL[0], INFO);

  if (*INFO != 0) fprintf(stderr, "failure with error %d\n", *INFO);
}

