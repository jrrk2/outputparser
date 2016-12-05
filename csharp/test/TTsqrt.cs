
// Kiwi Scientific Accelerator

using System;
using KiwiSystem;



public class TTsqrt
{
  static uint [] T1 = new uint [] 
    {
      0,	1024,	3062,	5746,	9193,	13348,	18162,	23592,
      29598,	36145,	43202,	50740,	58733,	67158,	75992,	85215,
      83599,	71378,	60428,	50647,	41945,	34246,	27478,	21581,
      16499,	12183,	8588,	5674,	3403,	1742,	661,	130 
    };
   

  /* From test56  - Adding the FastBitConvert attribute makes KiwiC
    ignore the bodies of functions such as these and replaces the body
    with its own fast-path identity code based only on the signatures of the functions. */
  [Kiwi.FastBitConvert()]
  static ulong fast_from_double(double darg)
  {
    byte [] asbytes = BitConverter.GetBytes(darg);
    return BitConverter.ToUInt64(asbytes, 0);
  }

  [Kiwi.FastBitConvert()]
  static double fast_to_double(ulong farg)
  {
    byte [] asbytes = BitConverter.GetBytes(farg);
    double rr = BitConverter.ToDouble(asbytes, 0);
    return rr;
  }

  [Kiwi.FastBitConvert()]
  static uint fast_from_float(float darg)
  {
    byte [] asbytes = BitConverter.GetBytes(darg);
    return BitConverter.ToUInt32(asbytes, 0);
  }

  [Kiwi.FastBitConvert()]
  static float fast_to_float(uint farg)
  {
    byte [] asbytes = BitConverter.GetBytes(farg);
    float rr = BitConverter.ToSingle(asbytes, 0);
    return rr;
  }

  public static double Sqrt(double arg) // Double Precision
  {
    if (arg<=0.0) return 0.0;
    ulong u0 = fast_from_double(arg);
    ulong k = (u0>>1) +(0x1ff8L << 48);
    ulong y0 = k - ((ulong)T1[31 & (k>>(15+32))] << 32);
    double pp = fast_to_double(y0);
    for (int qit=0;qit<3; qit++)
    {
    //  Console.WriteLine("  iterate {0}", pp);
      pp = (pp + arg/pp) * 0.5; // Need 3 iterations for DP
    }
    return pp;
  }

  public static float Sqrt_sp(float arg) // Single Precision
  {
    if (arg<=0.0f) return 0.0f;
    uint u0 = fast_from_float(arg);
    //Console.WriteLine("Start approx with {0:X} {0}", u0, u0);
    const uint adjuster = (1 << 29) - (1 << 22) - 0x4C000;
    uint u1 = (u0>>1) + adjuster;
    float pp = fast_to_float(u1);
    //Console.WriteLine("Start iteration with {0:X} {0}", u1, pp);
    pp = (pp + arg/pp) * 0.5f; // Need 2 iterations for SP
    //Console.WriteLine("Middle {0}", pp);
    pp = (pp + arg/pp) * 0.5f; 
    return pp;
  }

}

// eof
