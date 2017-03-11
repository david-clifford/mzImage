#include <stdio.h>
#include "R.h"
#include <math.h>

void printMatrix(double *y, 
	      int *nrows, 
	      int *ncols)
{
  //printf("printMatch function\n");

  int i,j,ij,nr=*nrows,nc=*ncols;
  
  // print a matrix with nrows and ncols stored as a vector of length nrows*ncols

  for(i=1;i<=nr;i++) {
    for(j=1;j<=nc;j++) {
      ij = (j-1)*nr + (i-1);
      printf("%5.3f ",y[ij]);
    }
    printf("\n");
  }
}

void nearestNeighbourMean(double *y, 
			  int *nrows, 
			  int *ncols,
			  double *eY)
{
  //printf("nearestNeighbourMean\n");

  int i,j,ij,im1j,ijp1,ip1j,ijm1,num,nr=*nrows,nc=*ncols;
  double tot;
 
  // assume matrix is surrounded by rows and columns of NAs
  // so the indices here can run from 2 to nr-1 etc

  for(i=2;i<=(nr-1);i++) {
    for(j=2;j<=(nc-1);j++) {
      
      //printf("%d %d \n",i,j);

      ij = (j-1)*nr + (i-1);   //i,j
      im1j = (j-1)*nr + (i-2); //i-1,j
      ijp1 = (j)*nr + (i-1);   //i,j+1
      ip1j = (j-1)*nr + (i);   //i+1,j
      ijm1 = (j-2)*nr + (i-1);  //i,j-1
       
      tot = 0;
      num = 0;
      if(!ISNA(y[im1j])) {
	tot = tot + y[im1j];
	num++;
      }
      if(!ISNA(y[ijp1])) {
	tot = tot + y[ijp1];
	num++;
      }
      if(!ISNA(y[ip1j])) {
	tot = tot + y[ip1j];
	num++;
      }
      if(!ISNA(y[ijm1])) {
	tot = tot + y[ijm1];
	num++;
      }
      
      //printf("%f %d\n",tot,num);

      eY[ij] = (tot / num);
      
      //printf("%f\n",eY[ij]);

    }
  }

}

      
void nearestNeighbourQuadratic(double *y, 
			       int *nrows, 
			       int *ncols,
			       double *eY)
{
  //printf("nearestNeighbourQuadratic\n");

  int i,j,ij,im1j,ijp1,ip1j,ijm1,num,nr=*nrows,nc=*ncols;
  int im1jm1,im1jp1,ip1jm1,ip1jp1; // four diagonal neighbours
  double tot;
 
  // assume matrix is surrounded by rows and columns of NAs
  // so the indices here can run from 2 to nr-1 etc

  for(i=2;i<=(nr-1);i++) {
    for(j=2;j<=(nc-1);j++) {
      
      //printf("%d %d \n",i,j);
      
      ij = (j-1)*nr + (i-1);   //i,j
      im1j = (j-1)*nr + (i-2); //i-1,j
      ijp1 = (j)*nr + (i-1);   //i,j+1
      ip1j = (j-1)*nr + (i);   //i+1,j
      ijm1 = (j-2)*nr + (i-1);  //i,j-1
      
      im1jm1 = (j-2)*nr + (i-2); //i-1,j-1
      im1jp1 = (j)*nr + (i-2); //i-1,j+1
      ip1jm1 = (j-2)*nr + (i); //i+1,j-1
      ip1jp1 = (j)*nr + (i); //i+1,j+1

      tot = 0;
      num = 0;
      if(!ISNA(y[im1j])) {
	// Double Weight on four immediate neighbours
	tot = tot + 2*y[im1j];
	num++;
	num++;
      }
      if(!ISNA(y[ijp1])) {
	tot = tot + 2*y[ijp1];
	num++;
	num++;
      }
      if(!ISNA(y[ip1j])) {
	tot = tot + 2*y[ip1j];
	num++;
	num++;
      }
      if(!ISNA(y[ijm1])) {
	tot = tot + 2*y[ijm1];
	num++;
	num++;
      }
      
      if(!ISNA(y[im1jm1])) {
	// -1 weight on four diagonal neighbours
	tot = tot - y[im1jm1];
	num--;
      }
      if(!ISNA(y[im1jp1])) {
	tot = tot - y[im1jp1];
	num--;
      }
      if(!ISNA(y[ip1jm1])) {
	tot = tot - y[ip1jm1];
	num--;
      }
      if(!ISNA(y[ip1jp1])) {
	tot = tot - y[ip1jp1];
	num--;
      }

      //printf("%f %d\n",tot,num);

      eY[ij] = (tot / num);
      
      //printf("%f\n",eY[ij]);

    }
  }

}
