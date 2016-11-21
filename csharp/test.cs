using System;
using System.IO;
using System.Runtime.InteropServices;
using uint64_t = System.UInt64;
using uint32_t = System.UInt32;
using posreal = System.Single;
using __off64_t = System.Int64;
using __mode_t = System.UInt32;

public struct w128_t {
  [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)]
  public ulong[] u;
}

[StructLayout(LayoutKind.Sequential)]
public struct dsfmt_t
     {
     [MarshalAs(UnmanagedType.ByValArray, SizeConst = 192)]
     public w128_t[] status;
     public int idx;
     }

class main
    {
        static DateTime startTime = DateTime.Now;

	static void elapsed(string comment)
	{
            TimeSpan time1 = DateTime.Now - startTime;
            Console.WriteLine(comment + Convert.ToString(time1.TotalMilliseconds/1000));
	}

	public struct Buffer1
	{
	    public string ProductCode;
	    public string SerialNumber; 
	    public DateTime Date; 
	}

static dsfmt_t dsfmt;

static void do_recursion(ref w128_t r, ref w128_t a, ref w128_t b, ref w128_t lung)
{uint64_t  t0;
	uint64_t  t1;
	uint64_t  L0;
	uint64_t  L1;
t0 = a.u[0]; 
t1 = a.u[1]; 
L0 = lung.u[0]; 
L1 = lung.u[1]; 
lung.u[0] = (t0 << 19) ^ (L1 >> 32) ^ (L1 << 32) ^ b.u[0]; 
lung.u[1] = (t1 << 19) ^ (L0 >> 32) ^ (L0 << 32) ^ b.u[1]; 
r.u[0] = (lung.u[0] >> 12) ^ (lung.u[0] & 0x000ffafffffffb3fUL) ^ t0; 
r.u[1] = (lung.u[1] >> 12) ^ (lung.u[1] & 0x000ffdfffc90fffdUL) ^ t1; }

static void convert_o0o1(ref w128_t w)
{
w.u[0]|=1; 
w.u[1]|=1; 
byte[] array0 = BitConverter.GetBytes(w.u[0]);
double result0 = BitConverter.ToDouble(array0, 0) - 1.0;
byte[] array1 = BitConverter.GetBytes(result0);
w.u[0]=BitConverter.ToUInt64(array1, 0); 
byte[] array2 = BitConverter.GetBytes(w.u[1]);
double result1 = BitConverter.ToDouble(array2, 0) - 1.0;
byte[] array3 = BitConverter.GetBytes(result1);
w.u[1]=BitConverter.ToUInt64(array3, 0); 
}

public enum rsize { rsize = ((19937-128)/104+1)*2 };
double[] rarray = new double[(int)rsize.rsize];

static void gen_rand_array_o0o1(ref dsfmt_t dsfmt, ref w128_t[] array, int size)
{int i;
	int j;
w128_t  lung;
lung = dsfmt.status[((19937-128)/104+1)]; 
do_recursion(ref array[0], ref dsfmt.status[0], ref dsfmt.status[117], ref lung); 
for ( i = 1; i < ((19937-128)/104+1)-117; i++)
	{ 
	{
	do_recursion(ref array[i], ref dsfmt.status[i], ref dsfmt.status[i+117], ref lung); 
	}
 }

for ( ; i < ((19937-128)/104+1); i++)
	{ do_recursion(ref array[i], ref dsfmt.status[i], ref array[i+117-((19937-128)/104+1)], ref lung);  }

for ( ; i < size-((19937-128)/104+1); i++)
	{ do_recursion(ref array[i], ref array[i-((19937-128)/104+1)], ref array[i+117-((19937-128)/104+1)], ref lung); 
convert_o0o1(ref array[i-((19937-128)/104+1)]);  }

for ( j = 0; j < 2*((19937-128)/104+1)-size; j++)
	{ 
	{
	dsfmt.status[j] = array[j+size-((19937-128)/104+1)]; 
	}
 }

for ( ; i < size; i++, j++)
	{ do_recursion(ref array[i], ref array[i-((19937-128)/104+1)], ref array[i+117-((19937-128)/104+1)], ref lung); 
dsfmt.status[j] = array[i]; 
convert_o0o1(ref array[i-((19937-128)/104+1)]);  }

for ( i = size-((19937-128)/104+1); i < size; i++)
	{ 
	{
	convert_o0o1(ref array[i]); 
	}
 }

dsfmt.status[((19937-128)/104+1)] = lung;
}

int dsfmt_mexp=19937;
int rptr=0;

static void initial_mask(ref dsfmt_t dsfmt)
{int i;
for ( i = 0; i < ((19937-128)/104+1); i++)
	{
	dsfmt.status[i].u[0] = (dsfmt.status[i].u[0] & 0x000FFFFFFFFFFFFFUL) | 0x3FF0000000000000UL; 
	dsfmt.status[i].u[1] = (dsfmt.status[i].u[1] & 0x000FFFFFFFFFFFFFUL) | 0x3FF0000000000000UL; 
	}
}

static void period_certification(ref dsfmt_t dsfmt)
{
uint64_t[] pcv = new uint64_t[] {0x3d84e1ac0dc82880UL, 0x0000000000000001UL};
uint64_t[] tmp = new uint64_t[2];
uint64_t  inner;
int i;
tmp[0] = (dsfmt.status[((19937-128)/104+1)].u[0] ^ 0x90014964b32f4329UL); 
tmp[1] = (dsfmt.status[((19937-128)/104+1)].u[1] ^ 0x3b8d12ac548a7c7aUL); 
inner = tmp[0] & pcv[0]; 
inner^=tmp[1] & pcv[1]; 
for ( i = 32; i>0; i>>=1)
	{ 
	{
	inner^=inner >> i; 
	}
 }

inner&=1; 
if (inner==1) 
	{
	return;
	}

dsfmt.status[((19937-128)/104+1)].u[1]^=1; 
return;}

void __assert_fail(string __assertion, string __file, uint __line, string __function)
{
Console.WriteLine(__assertion + __file + __line + __function);
Environment.Exit(1);
}

void dsfmt_fill_array_open_open(ref dsfmt_t dsfmt, double[] array, int size)
{
if (!(size%2==0)) __assert_fail("size % 2 == 0", "../simpleDMC_restructure/src/dSFMT.c", 511, "../simpleDMC_restructure/src/dSFMT.c"); 
if (!(size>=(((19937-128)/104+1)*2))) __assert_fail("size >= (((19937 - 128) / 104 + 1) * 2)", "../simpleDMC_restructure/src/dSFMT.c", 512, "../simpleDMC_restructure/src/dSFMT.c"); 
w128_t [] warray = new w128_t [size/2];
gen_rand_array_o0o1(ref dsfmt, ref warray, size/2);
}

void exit(int __status)
{
Environment.Exit(__status);
}

static int idxof(int i)
{return i; }

double sqrt(double __x)
{
return Math.Sqrt(__x);
}

uint getpsfmt32(ref dsfmt_t dsfmt, int ix)
{
w128_t tmp = dsfmt.status[ix/4];
byte[] array0 = BitConverter.GetBytes(tmp.u[ix%2]);
uint result0 = BitConverter.ToUInt32(array0, (ix%4)*4);
return result0;
}

void putpsfmt32(ref dsfmt_t dsfmt, int ix, uint value)
{
byte[] array0 = BitConverter.GetBytes(dsfmt.status[ix/4].u[ix%2]);
byte[] array1 = BitConverter.GetBytes(value);
for (int i = 0; i < 4; i++) array0[(ix%4)*4+i] = array1[i];
dsfmt.status[ix/4].u[ix%2] = BitConverter.ToUInt64(array0, (ix%4)*4); 
}

void dsfmt_chk_init_gen_rand(ref dsfmt_t dsfmt, uint32_t seed, int mexp)
{
int i;
if (mexp!=dsfmt_mexp) 
	{
	Console.WriteLine("DSFMT_MEXP doesn't match with dSFMT.c\n"); 
exit(1); 
	}

putpsfmt32(ref dsfmt, idxof(0), seed); 
for (i = 1; i < (((19937-128)/104+1)+1)*4; i++)
	{ 
	putpsfmt32(ref dsfmt, idxof(i),
	   1812433253U*(getpsfmt32(ref dsfmt, idxof(i-1)) ^ (getpsfmt32(ref dsfmt, idxof(i-1)) >> 30))+(uint)i); 
	}

initial_mask(ref dsfmt); 
period_certification(ref dsfmt); 
dsfmt.idx = (((19937-128)/104+1)*2); }

void nextUniformRandom()
{
dsfmt_fill_array_open_open(ref dsfmt, rarray, (int)rsize.rsize); 
rptr = 0;
}

void dsfmt_init_gen_rand(ref dsfmt_t dsfmt, uint32_t seed)
{
elapsed("init_gen_rand start Time: ");
dsfmt_chk_init_gen_rand(ref dsfmt, seed, 19937); 
elapsed("init_gen_rand end Time: ");
}

double gaussianRand(double dSigma)
{double d = 0;
int nRands = 12;
if (!(rptr>=(int)rsize.rsize)) __assert_fail("rptr >= rarray", "../simpleDMC_restructure/src/support.c", 132, "../simpleDMC_restructure/src/support.c"); 
if (rptr>((int)rsize.rsize-nRands)) 
	{
	nextUniformRandom(); 
	}

for (int i = 0; i < nRands; i++) 
	{
	d+=rarray[rptr++]; 
	}

d-=nRands/2.0; 
d*=dSigma/sqrt(nRands/12.0); 
return d; }

double fmin(double __x, double __y)
{
if ( __x < __y) return __x; else return __y;
}

double fmax(double __x, double __y)
{
if ( __x > __y) return __x; else return __y;
}

int fchmod(int __fd, __mode_t __mode)
{
return 0;
}

void testGaussian()
{
double t = 5;
double dSq = 0;
double dAv = 0;
int N = 1000000;
int[] bin = new int[(int) (t*10+1)];
dsfmt_init_gen_rand(ref dsfmt, 7); 
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

Console.WriteLine("dumping");
FileStream fs = File.OpenWrite("./g.dat");
StreamWriter plot = new StreamWriter(fs);
plot.WriteLine("{0} {1}", dAv/N, sqrt(dSq/N-(dAv/N)*(dAv/N))); 
for (int i = 0; i<=t*10; i++) plot.WriteLine("{0}\t{1}\n", i-5*t, bin[i]); 
fs.Close();
FileStream fs2 = File.OpenWrite("./g.sh");
StreamWriter plot2 = new StreamWriter(fs2);
plot2.WriteLine("#!/usr/bin/gnuplot\n"); 
plot2.WriteLine("N(s,x)=exp(-(x/s)**2/2)/(s*sqrt(2*pi))\n"); 
plot2.WriteLine("set term postscript enhanced color\n"); 
plot2.WriteLine("set output \"g.eps\"\n"); 
plot2.WriteLine("p N({0},x), 'g.dat' u ($1+0.5):($2/1000000) w l\n", t); 
plot2.WriteLine("#pause mouse\n"); 
fs2.Close(); 
}

        static void Main()
        {
	    main obj = new main();
	    elapsed("Start Time: ");
	    obj.testGaussian();
	    elapsed("Time elapsed: ");
        }
    }
