typedef unsigned long int uint64_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;
struct W128_T
	{
	uint8_t array[16];
	} ;
typedef struct W128_T w128_t;
typedef long unsigned int size_t;
typedef long int __off_t;
typedef void _IO_lock_t;
typedef long int __off64_t;
struct DSFMT_T
	{
	w128_t  status[192];
	int idx;
	} ;
typedef struct DSFMT_T dsfmt_t;
static int verbose = 1;
dsfmt_t dsfmt;

typedef struct _IO_FILE FILE;
extern int printf(const char *__format, ...);
extern int fprintf(FILE *__stream, const char *__format, ...);

static double get_double(w128_t *w, int idx)
{
  double tmp = *(double *)&(w->array[idx*8]);
  if (verbose) printf("get_double %.16lX\n", *(uint64_t *)&tmp);
  return tmp;
}

static void copy(uint8_t *src, uint8_t *dest, int idx, int len)
{
for (int i = 0; i < len; i++)
  {
//    if (verbose) printf("dest[%d] = %.2X\n", i, src[i]);
    dest[idx+i] = src[i];
  }
}

static void put_double(w128_t *w, int idx, double arg)
{
  if (verbose) printf("put_double %.16lX\n", *(uint64_t *)&arg);
  copy((uint8_t *)&arg, w->array, idx*8, sizeof(double));
}

static uint64_t get_ulong(w128_t *w, int idx)
{
  uint64_t tmp = *(uint64_t *)&(w->array[idx*8]);
  if (verbose) printf("get_ulong %.16lX\n", tmp);
  return tmp;
}

static void put_ulong(w128_t *w, int idx, uint64_t arg)
{
  if (verbose) printf("put_ulong %.16lX\n", arg);
  copy((uint8_t *)&arg, w->array, idx*8, sizeof(uint64_t));
}

static uint32_t getpsfmt32(dsfmt_t *dsfmt, int ix)
{
  uint32_t tmp = *(uint32_t *)&(dsfmt->status[ix/4].array[(ix%4)*4]);
  if (verbose) printf("getpsfmt32 %d %.8X\n", ix, tmp);
  return tmp;
}

static void putpsfmt32(dsfmt_t *dsfmt, int idx, uint32_t arg)
{
  if (verbose) printf("putpsfmt32 %d %.8X\n", idx, arg);
  copy((uint8_t *)&arg, dsfmt->status[idx/4].array, (idx%4)*4, sizeof(uint32_t));
}

static void do_recursion(w128_t *r, int roff, w128_t *a, int aoff, w128_t *b, int boff, w128_t *lung)
{
  uint64_t  t0;
  uint64_t  t1;
  uint64_t  L0;
  uint64_t  L1;
  t0 = get_ulong(a,aoff*2); 
  t1 = get_ulong(a,aoff*2+1); 
  L0 = get_ulong(lung,0); 
  L1 = get_ulong(lung,1); 
  put_ulong(lung, 0, (t0 << 19) ^ (L1 >> 32) ^ (L1 << 32) ^ get_ulong(b,boff*2)); 
  put_ulong(lung, 1, (t1 << 19) ^ (L0 >> 32) ^ (L0 << 32) ^ get_ulong(b,boff*2+1)); 
  put_ulong(r, roff*2, (get_ulong(lung, 0) >> 12) ^ (get_ulong(lung,0) & 0x000ffafffffffb3fUL) ^ t0); 
  put_ulong(r, roff*2+1, (get_ulong(lung,1) >> 12) ^ (get_ulong(lung,1) & 0x000ffdfffc90fffdUL) ^ t1);
  if (verbose) printf("r->u[0], r->u[1] = %lu,%lu\n", get_ulong(r,roff*2), get_ulong(r,roff*2+1));
}

static void convert_o0o1(w128_t *w, int woff)
{
  if (verbose) printf("w->u[0], w->u[1] = %.16lX,%.16lX\n", get_ulong(w,woff*2), get_ulong(w,woff*2+1));
  put_ulong(w, woff*2, get_ulong(w,woff*2)|1); 
  put_ulong(w, woff*2+1, get_ulong(w,woff*2+1)|1); 
  put_double(w, woff*2, get_double(w,woff*2)-1.0); 
  put_double(w, woff*2+1, get_double(w,woff*2+1)-1.0); 
  unsigned long tmp0 = get_ulong (w, woff*2);
  unsigned long tmp1 = get_ulong (w, woff*2+1);
  if (verbose) printf("w'->u[0], w'->u[1] = %.16lX,%.16lX\n", tmp0, tmp1);
}

double rarray[((19937-128)/104+1)*2];

static void gen_rand_array_o0o1(dsfmt_t *dsfmt, w128_t *array, int size)
{
  int i;
  int j;
w128_t  lung;
if (verbose) printf("gen_rand_array_o0o1\n");
put_ulong(&lung, 0, get_ulong(dsfmt->status,((19937-128)/104+1)*2+0)); 
put_ulong(&lung, 1, get_ulong(dsfmt->status,((19937-128)/104+1)*2+1)); 
do_recursion(array, 0, dsfmt->status, 0, dsfmt->status, 117, &lung); 
for ( i = 1; i < ((19937-128)/104+1)-117; i++)
	{
	do_recursion(array, i, dsfmt->status, i, dsfmt->status, i+117, &lung); 
	}

for ( ; i < ((19937-128)/104+1); i++)
	{
	  do_recursion(array, i, dsfmt->status, i, array, i+117-((19937-128)/104+1), &lung); 
	}

for ( ; i < size-((19937-128)/104+1); i++)
	{
	  do_recursion(array, i, array, i-((19937-128)/104+1), array, i+117-((19937-128)/104+1), &lung); 
	  convert_o0o1(array,i-((19937-128)/104+1)); 
	}

for ( j = 0; j < 2*((19937-128)/104+1)-size; j++)
	{ 
	dsfmt->status[j] = array[j+size-((19937-128)/104+1)]; 
	}

for ( ; i < size; i++, j++)
	{
	  do_recursion(array, i, array, i-((19937-128)/104+1), array, i+117-((19937-128)/104+1), &lung); 
	  put_ulong(dsfmt->status, j*2+0, get_ulong(array,i*2+0)); 
	  put_ulong(dsfmt->status, j*2+1, get_ulong(array,i*2+1)); 
	  convert_o0o1(array, i-((19937-128)/104+1));
	}

for ( i = size-((19937-128)/104+1); i < size; i++)
	{ 
	  convert_o0o1(array, i); 
	}

put_ulong(dsfmt->status, ((19937-128)/104+1)*2+0, get_ulong(&lung,0)); 
put_ulong(dsfmt->status, ((19937-128)/104+1)*2+1, get_ulong(&lung,1)); 
}

int dsfmt_mexp=19937;
int rsize=sizeof (rarray)/sizeof (*rarray);
double  * rptr=0;

static void initial_mask(dsfmt_t *dsfmt)
{
int i;
for ( i = 0; i < ((19937-128)/104+1)*2; i++)
	{ 
	{
	  put_ulong(&dsfmt->status[i/2],i&1, (get_ulong(&dsfmt->status[i/2],i&1) & 0x000FFFFFFFFFFFFFUL) | 0x3FF0000000000000UL); 
	}
 }
}

static void period_certification(dsfmt_t *dsfmt)
{uint64_t  pcv[2] = {0x3d84e1ac0dc82880UL, 0x0000000000000001UL};
uint64_t  tmp[2];
uint64_t  inner;
int i;
 tmp[0] = get_ulong(&dsfmt->status[((19937-128)/104+1)],0) ^ 0x90014964b32f4329UL; 
tmp[1] = get_ulong(&dsfmt->status[((19937-128)/104+1)],1) ^ 0x3b8d12ac548a7c7aUL; 
inner = tmp[0] & pcv[0]; 
inner^=tmp[1] & pcv[1]; 
for ( i = 32; i>0; i>>=1)
	{ 
	{
	inner^=inner >> i; 
	if (verbose) printf("inner=%lu\n", inner);
	}
 }

inner&=1; 
if (inner==1) 
	{
	return;
	}

 put_ulong(&dsfmt->status[((19937-128)/104+1)], 1, get_ulong(&dsfmt->status[((19937-128)/104+1)], 1) ^ 1); 
 return;
}

void  * calloc(size_t __nmemb, size_t __size);
void free(void *ptr);

void dsfmt_fill_array_open_open(dsfmt_t *dsfmt, double array[], int size)
{
w128_t   *warray = calloc(size/2, sizeof(w128_t));
gen_rand_array_o0o1(dsfmt, warray, size/2);
for (int i = 0; i < size; i++)
	{
	array [i] = get_double(warray, i);
	}
free(warray);
}

extern void exit(int __status);

static int idxof(int i)
{
return i;
}

typedef unsigned int __mode_t;

extern double sqrt(double __x);

void dsfmt_chk_init_gen_rand(dsfmt_t *dsfmt, uint32_t seed, int mexp)
{int i;
if (mexp!=dsfmt_mexp) 
	{
	printf("DSFMT_MEXP doesn't match with dSFMT.c\n"); 
	exit(1); 
	}
 putpsfmt32(dsfmt, idxof(0), seed); 
for ( i = 1; i < (((19937-128)/104+1)+1)*4; i++)
	{ 
	{
	  uint32_t prev = getpsfmt32(dsfmt, idxof(i-1));
	  uint32_t tmp = 1812433253UL*(prev ^ (prev >> 30))+i; 
	  if (verbose) printf("prev = %u, loop[%d] = %u\n", prev, i, tmp);
	  putpsfmt32(dsfmt, idxof(i), tmp);
	}
 }

initial_mask(dsfmt); 
period_certification(dsfmt); 
dsfmt->idx = (((19937-128)/104+1)*2); }

void nextUniformRandom()
{
dsfmt_fill_array_open_open(&dsfmt, rarray, rsize); 
rptr = rarray;
}

static void dsfmt_init_gen_rand(dsfmt_t *dsfmt, uint32_t seed)
{
dsfmt_chk_init_gen_rand(dsfmt, seed, 19937);
}

extern int fclose(FILE *__stream);
double floor(double x);

double gaussianRand(double dSigma)
{
double d = 0;
int nRands = 12;
if (rptr>(rarray+rsize-nRands)) 
	{
	nextUniformRandom(); 
	}

for (int i = 0; i < nRands; i++) 
	{
	if (verbose) printf("rarray[%ld] = %ld\n", rptr-rarray, (uint64_t)floor(*rptr*1e12));
	d+=*rptr++; 
	}

d-=nRands/2.0; 
d*=dSigma/sqrt(nRands/12.0); 
return d; }

void  * xcalloc(size_t n, size_t size)
{
void  *ptr;
ptr = calloc(n, size); 
return ptr;
}

extern double fmin(double __x, double __y);

extern int fileno(FILE *__stream);

FILE   * fopen(const char *__filename, const char *__modes);

extern double fmax(double __x, double __y);

extern int fchmod(int __fd, __mode_t __mode);

int testGaussian(int argc, char **_)
{
double t = 5;
double dSq = 0;
double dAv = 0;
int N = 1000000;
int  *bin = (int  *) xcalloc((int) (t*10+1), sizeof(int));
verbose = argc > 1;
dsfmt_init_gen_rand(&dsfmt, 7); 
nextUniformRandom(); 
for (int ibin = 0; ibin<=t*10; ibin++) 
	{
	bin[ibin] = 0; 
	}

for (int i = 0; i < N; i++) 
	{
	double d = gaussianRand(t);
dSq+=d*d; 
dAv+=d; 
bin[(int) (fmax(fmin(d+5*t, 10*t), 0))]++; 
	}

const char  *data = "./g.dat";
FILE   *plot = fopen(data, "w");
fprintf(plot, "%f %f\n", dAv/N, sqrt(dSq/N-(dAv/N)*(dAv/N))); 
for (int i = 0; i<=t*10; i++) fprintf(plot, "%f\t%d\n", i-5*t, bin[i]); 
fclose(plot); 
data = "./g.sh"; 
plot = fopen(data, "w"); 
fprintf(plot, "#!/usr/bin/gnuplot\n"); 
fprintf(plot, "N(s,x)=exp(-(x/s)**2/2)/(s*sqrt(2*pi))\n"); 
fprintf(plot, "set term postscript enhanced color\n"); 
fprintf(plot, "set output \"g.eps\"\n"); 
fprintf(plot, "p N(%f,x), 'g.dat' u ($1+0.5):($2/1000000) w l\n", t); 
fprintf(plot, "#pause mouse\n"); 
fchmod(fileno(plot), 0700); 
fclose(plot); }
