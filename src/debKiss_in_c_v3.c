/* file mymod.c */
  #include <R.h>
#include <Rinternals.h>
#include <math.h>
  static double parms[26];
  static double forc[1];
  
#define f parms[0]
#define rB parms[1]
#define Lp parms[2]
#define Lm parms[3]
#define Rm parms[4]
#define kap parms[5]
#define F_BV parms[6]
#define K_RV parms[7]
#define b_s parms[8]
#define z_s parms[9]
#define h_b parms[10]
#define z_b parms[11]
#define b_b parms[12]
#define kd parms[13]
#define Xu parms[14]
#define Xe parms[15]
#define XG parms[16]
#define XR parms[17]
#define SA parms[18]
#define SM parms[19]
#define SG parms[20]
#define SR1 parms[21]
#define SR2 parms[22]
#define yP parms[23]
#define Lm_TK parms[24]
#define L0 parms[25]

#define Cw forc[0]
  
/* initializer */
  void initmod(void (* odeparms)(int *, double *))
{
  int N=26;
  odeparms(&N, parms);
  }
  void forcc(void (* odeforcs)(int *, double *))
  {
    int N=1;
    odeforcs(&N, forc);
  }
  
/* Derivatives and 1 output variable */
  void derivs (int *neq, double *t, double *y, double *ydot,
               double *yout, int *ip)
{
  /* if (ip[0] <1) error("nout should be at least 1"); */
    float s = b_b*(y[3]>z_b)*(y[3]-z_b);
    float f_R = f;
    float s_A = ((SA*s < 1.0)*SA*s + (SA*s >= 1.0)*1.0);
    if (s_A < 1) {
      ydot[0] = rB*(1.0 + SM*s)/(1.0 + SG*s)*(f*Lm*(1.0 - s_A)/(1.0 + SM * s) - y[0]);
      if (ydot[0]<0) {
        f_R = (f - kap*y[0]/Lm*(1.0+SM*s)/(1.0-s_A))/(1.0-kap);
        
        if (f_R >= 0.0) {
          ydot[0] = 0.0;
        } 
        else {
          ydot[0] = (rB/yP)*(1.0+s*SM)*(f*Lm/kap*(1.0-s_A)/(1.0+s*SM) - y[0]);
        }
      } 
    }
    else {
      ydot[0] = (rB/yP)*(1.0+s*SM)*(f*Lm/kap*(1.0-s_A)/(1.0+s*SM) - y[0]);
    }
    
    /* safeguard for excessive shrinkage */
    ydot[0] = ydot[0]*(y[0]>(0.1*L0));
    
  /* ydot[1] = 0; */
  ydot[1] = (y[0]>Lp)*(Rm/(1+SR1*s)*exp(-SR2*s)*(f_R*Lm*y[0]*y[0]*(1-s_A) - Lp*Lp*Lp*(1+SM*s))/(Lm*Lm*Lm - Lp*Lp*Lp) > 0) *Rm/(1+SR1*s)*exp(-SR2*s)*(f_R*Lm*y[0]*y[0]*(1-s_A) - Lp*Lp*Lp*(1+SM*s))/(Lm*Lm*Lm - Lp*Lp*Lp);
  ydot[2] = -(b_s*(y[3]>z_s)*(y[3]-z_s) + h_b)*y[2];
  ydot[3] = kd*(Lm_TK*Cw/(Lm_TK - Xu*(Lm_TK - y[0])) - Lm_TK*y[3]/(Lm_TK - Xe*(Lm_TK - y[0]))) 
  - (ydot[0]>0)*XG*3.0/y[0]*ydot[0]*y[3]
  - XR*F_BV*K_RV*y[3]*ydot[1];
  yout[0] = y[0]+y[1]+y[2]+y[3];
  
  }
