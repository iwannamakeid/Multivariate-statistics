data data1 ;
	input X1 X2 X3;
	individual +1;
cards;
  3.7  48.5   9.3
  5.7  65.1   8.0
  3.8  47.2  10.9
  3.2  53.2  12.0
  3.1  55.5   9.7
  4.6  36.1   7.9
  2.4  24.8  14.0
  7.2  33.1   7.6
  6.7  47.4   8.5
  5.4  54.1  11.3
  3.9  36.9  12.7
  4.5  58.8  12.3
  3.5  27.8   9.8
  4.5  40.2   8.4
  1.5  13.5  10.1
  8.5  56.4   7.1
  4.5  71.6   8.2
  6.5  52.8  10.9
  4.1  44.1  11.2
  5.5  40.9   9.4
;
run;
Proc Print DATA=WORK.DATA1; Id Individual; run;
Proc IML;	*1-(a,b);		
	Use data1;
	Read ALL var{X1 X2 X3} into X; N=NROW(X); P=NCOL(X);
	close data1;
	A=SHAPE(1,N,1);
	MV = (A`*X)/N;			*표본평균 계산;
	M=REPEAT(MV,N,1); 		
	S=(X-M)`*(X-M)/(N-1);	*공분산행렬 계산;
	print MV, S; 
	InvS=inv(S);
	mu0={4 48 11}; *Ho: mu=[4,48,11] H1: mu != [4,48,11];
	T2=N*(MV-mu0)*invS*(MV-mu0)`; * Hotelling T-square값 계산;
	CriticalF=(((N-1)*P)/(N-P))*FINV(0.95, P, N-P); *유의수준 5%에서의 기각값;
	CriticalF2=(((N-1)*P)/(N-P))*FINV(0.9, P, N-P); *유의수준 10%에서의 기각값;
	print T2, CriticalF, CriticalF2;
	run;
data customer_goods ;
	input X1 X2 X3 X4;
	Item +1;
cards;
72 50 8 0.5
66.5 48 15 1.0
54 57 14 1.0
67 60 15 0.9
44 57 14 0.3
41 52 18 1.9
34.5 50 4 0.5
34.5 46 8.5 1.0
24 54 3 1.2
;
Proc Print DATA=producer_goods; Id item; run;
data producer_goods ;
	input X1 X2 X3 X4;
	Item +1;
cards;
57 57 12.5 0.9
100 54 17 0.5
100 32 16.5 0.7
96.5 65 20.5 0.9
79 51 18 0.9
78.5 53 18 1.2
48 50 21 1.6
155 44 20.5 1.4
84 64 13 0.8
105 35 17 1.8
;
Proc Print DATA=procuder_goods; Id item; run;

Proc IML;	*2-(b,c);		
* customer goods;
	X1=  {72 50 8 0.5,
			66.5 48 15 1.0,
			54 57 14 1.0,
			67 60 15 0.9,
			44 57 14 0.3,
			41 52 18 1.9,
			34.5 50 4 0.5,
			34.5 46 8.5 1.0,
			24 54 3 1.2};
*producer goods;
	X2 = {57 57 12.5 0.9,
			100 54 17 0.5,
			100 32 16.5 0.7,
			96.5 65 20.5 0.9,
			79 51 18 0.9,
			78.5 53 18 1.2,
			48 50 21 1.6,
			155 44 20.5 1.4,
			84 64 13 0.8,
			105 35 17 1.8};
	print X1,X2;
	N1=NROW(X1); 
	P1=NCOL(X1);
	N2=NROW(X2); 
	P2=NCOL(X2);
	A=SHAPE(1,N1,1);
	MV = (A`*X1)/N1;		*표본평균 계산;
	M=REPEAT(MV,N1,1); 		
	S1=(X1-M)`*(X1-M)/(N1-1);	*공분산행렬 계산;
	B=SHAPE(1,N2,1);
	MV2 = (B`*X2)/N2;			*표본평균 계산;
	M2=REPEAT(MV2,N2,1); 		
	S2=(X2-M2)`*(X2-M2)/(N2-1);*공분산행렬 계산;
	print MV, S1, MV2, S2; 
	C = S1/N1;
	D = S2/N2;
	InvS=inv(C+D); * 공분산 인버스 행렬 계산;
	T2=(MV-MV2)*invS*(MV-MV2)`; * Hotelling T-square값 계산;
	CriticalK=CINV(0.95, 4); *유의수준 5%에서의 기각값;
	print T2, CriticalK;
	run;
PROC IML;
Y = {81 74,
		461 423,
		20 16,
		450 450,
		246 87,
		166 115,
		63 50,
		64 50,
		155 113,
		151 38,
		166 156,
		37 27,
		223 218,
		138 138,
		72 39,
		245 231};
X = {72 33,
		134 18,
		84 20,
		98 58,
		48 13,
		142 49,
		113 38,
		90 24,
		30 18,
		260 34,
		116 20,
		87 27,
		69 32,
		100 27,
		315 39,
		188 65};
D = X-Y;
print X, Y;
N=NROW(D); 
P=NCOL(D);
A=SHAPE(1,N,1);
MV = (A`*D)/N;		*표본평균 계산;
M=REPEAT(MV,N,1); 		
S=(D-MV)`*(D-MV)/(N-1);	*공분산행렬 계산;
InvS = inv(S);              * 공분산 인버스 행렬 계산;
PRINT D, MV, S ;
T2=N*MV*invS*MV`;    * Hotelling T-square값 계산;
CriticalF=(((N-1)*P)/(N-P))*FINV(0.95, P, N-P); *유의수준 5%에서의 기각값;
PRINT T2, CriticalF;
run;
