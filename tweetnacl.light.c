extern unsigned int __compcert_va_int32(void *);
extern unsigned long long __compcert_va_int64(void *);
extern double __compcert_va_float64(void *);
extern void *__compcert_va_composite(void *, unsigned long long);
extern long long __compcert_i64_dtos(double);
extern unsigned long long __compcert_i64_dtou(double);
extern double __compcert_i64_stod(long long);
extern double __compcert_i64_utod(unsigned long long);
extern float __compcert_i64_stof(long long);
extern float __compcert_i64_utof(unsigned long long);
extern long long __compcert_i64_sdiv(long long, long long);
extern unsigned long long __compcert_i64_udiv(unsigned long long, unsigned long long);
extern long long __compcert_i64_smod(long long, long long);
extern unsigned long long __compcert_i64_umod(unsigned long long, unsigned long long);
extern long long __compcert_i64_shl(long long, int);
extern unsigned long long __compcert_i64_shr(unsigned long long, int);
extern long long __compcert_i64_sar(long long, int);
extern long long __compcert_i64_smulh(long long, long long);
extern unsigned long long __compcert_i64_umulh(unsigned long long, unsigned long long);
extern void __builtin_debug(int, ...);
unsigned long long L32(unsigned long long, int);
unsigned long long ld32(unsigned char *);
unsigned long long dl64(unsigned char *);
void st32(unsigned char *, unsigned long long);
void ts64(unsigned char *, unsigned long long);
int vn(unsigned char *, unsigned char *, int);
int crypto_verify_16_tweet(unsigned char *, unsigned char *);
int crypto_verify_32_tweet(unsigned char *, unsigned char *);
void core(unsigned char *, unsigned char *, unsigned char *, unsigned char *, int);
int crypto_core_salsa20_tweet(unsigned char *, unsigned char *, unsigned char *, unsigned char *);
int crypto_core_hsalsa20_tweet(unsigned char *, unsigned char *, unsigned char *, unsigned char *);
int crypto_stream_salsa20_tweet_xor(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_stream_salsa20_tweet(unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_stream_xsalsa20_tweet(unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_stream_xsalsa20_tweet_xor(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
void add1305(unsigned long long *, unsigned long long *);
int crypto_onetimeauth_poly1305_tweet(unsigned char *, unsigned char *, unsigned long long, unsigned char *);
int crypto_onetimeauth_poly1305_tweet_verify(unsigned char *, unsigned char *, unsigned long long, unsigned char *);
int crypto_secretbox_xsalsa20poly1305_tweet(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_secretbox_xsalsa20poly1305_tweet_open(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
void set25519(long long *, long long *);
void car25519(long long *);
void sel25519(long long *, long long *, int);
void pack25519(unsigned char *, long long *);
int neq25519(long long *, long long *);
unsigned char par25519(long long *);
void unpack25519(long long *, unsigned char *);
void A(long long *, long long *, long long *);
void Z(long long *, long long *, long long *);
void M(long long *, long long *, long long *);
void S(long long *, long long *);
void inv25519(long long *, long long *);
void pow2523(long long *, long long *);
int crypto_scalarmult_curve25519_tweet(unsigned char *, unsigned char *, unsigned char *);
int crypto_scalarmult_curve25519_tweet_base(unsigned char *, unsigned char *);
int crypto_box_curve25519xsalsa20poly1305_tweet_beforenm(unsigned char *, unsigned char *, unsigned char *);
int crypto_box_curve25519xsalsa20poly1305_tweet_afternm(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_box_curve25519xsalsa20poly1305_tweet_open_afternm(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *);
int crypto_box_curve25519xsalsa20poly1305_tweet(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *, unsigned char *);
int crypto_box_curve25519xsalsa20poly1305_tweet_open(unsigned char *, unsigned char *, unsigned long long, unsigned char *, unsigned char *, unsigned char *);
unsigned long long R(unsigned long long, int);
unsigned long long Ch(unsigned long long, unsigned long long, unsigned long long);
unsigned long long Maj(unsigned long long, unsigned long long, unsigned long long);
unsigned long long Sigma0(unsigned long long);
unsigned long long Sigma1(unsigned long long);
unsigned long long sigma0(unsigned long long);
unsigned long long sigma1(unsigned long long);
int crypto_hashblocks_sha512_tweet(unsigned char *, unsigned char *, unsigned long long);
int crypto_hash_sha512_tweet(unsigned char *, unsigned char *, unsigned long long);
void add(long long (*)[16], long long (*)[16]);
void cswap(long long (*)[16], long long (*)[16], unsigned char);
void pack(unsigned char *, long long (*)[16]);
void scalarmult(long long (*)[16], long long (*)[16], unsigned char *);
void scalarbase(long long (*)[16], unsigned char *);
void modL(unsigned char *, long long *);
void reduce(unsigned char *);
int crypto_sign_ed25519_tweet(unsigned char *, unsigned long long *, unsigned char *, unsigned long long, unsigned char *);
int unpackneg(long long (*)[16], unsigned char *);
int crypto_sign_ed25519_tweet_open(unsigned char *, unsigned long long *, unsigned char *, unsigned long long, unsigned char *);
extern int printf(signed char *, ...);
int main(void);
signed char const __stringlit_4[3] = ",\012";

signed char const __stringlit_2[9] = "%s0x%.2x";

signed char const __stringlit_3[2] = ",";

signed char const __stringlit_1[1] = "";

unsigned char const _0[16];

unsigned char const _9[32] = { 9, /* skip 31 */ };

long long const gf0[16];

long long const gf1[16] = { 1LL, /* skip 120 */ };

long long const _121665[16] = { 56129LL, 1LL, /* skip 112 */ };

long long const D[16] = { 30883LL, 4953LL, 19914LL, 30187LL, 55467LL,
  16705LL, 2637LL, 112LL, 59544LL, 30585LL, 16505LL, 36039LL, 65139LL,
  11119LL, 27886LL, 20995LL, };

long long const D2[16] = { 61785LL, 9906LL, 39828LL, 60374LL, 45398LL,
  33411LL, 5274LL, 224LL, 53552LL, 61171LL, 33010LL, 6542LL, 64743LL,
  22239LL, 55772LL, 9222LL, };

long long const X[16] = { 54554LL, 36645LL, 11616LL, 51542LL, 42930LL,
  38181LL, 51040LL, 26924LL, 56412LL, 64982LL, 57905LL, 49316LL, 21502LL,
  52590LL, 14035LL, 8553LL, };

long long const Y[16] = { 26200LL, 26214LL, 26214LL, 26214LL, 26214LL,
  26214LL, 26214LL, 26214LL, 26214LL, 26214LL, 26214LL, 26214LL, 26214LL,
  26214LL, 26214LL, 26214LL, };

long long const I[16] = { 41136LL, 18958LL, 6951LL, 50414LL, 58488LL,
  44335LL, 6150LL, 12099LL, 55207LL, 15867LL, 153LL, 11085LL, 57099LL,
  20417LL, 9344LL, 11139LL, };

unsigned long long L32(unsigned long long x, int c)
{
  return x << c | (x & 4294967295U) >> 32 - c;
}

unsigned long long ld32(unsigned char *x)
{
  unsigned long long u;
  u = *(x + 3);
  u = u << 8 | *(x + 2);
  u = u << 8 | *(x + 1);
  return u << 8 | *(x + 0);
}

unsigned long long dl64(unsigned char *x)
{
  unsigned long long i;
  unsigned long long u;
  u = 0;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 8)) {
      break;
    }
    u = u << 8 | *(x + i);
  }
  return u;
}

void st32(unsigned char *x, unsigned long long u)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 4)) {
      break;
    }
    *(x + i) = u;
    u = u >> 8;
  }
}

void ts64(unsigned char *x, unsigned long long u)
{
  int i;
  i = 7;
  for (; 1; i = i - 1) {
    if (! (i >= 0)) {
      break;
    }
    *(x + i) = u;
    u = u >> 8;
  }
}

int vn(unsigned char *x, unsigned char *y, int n)
{
  unsigned long long i;
  unsigned long long d;
  d = 0;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < n)) {
      break;
    }
    d = d | *(x + i) ^ *(y + i);
  }
  return (1 & d - 1 >> 8) - 1;
}

int crypto_verify_16_tweet(unsigned char *x, unsigned char *y)
{
  register int $182;
  $182 = vn(x, y, 16);
  return $182;
}

int crypto_verify_32_tweet(unsigned char *x, unsigned char *y)
{
  register int $182;
  $182 = vn(x, y, 32);
  return $182;
}

void core(unsigned char *out, unsigned char *in, unsigned char *k, unsigned char *c, int h)
{
  unsigned long long w[16];
  unsigned long long x[16];
  unsigned long long y[16];
  unsigned long long t[4];
  int i;
  int j;
  int m;
  register unsigned long long $191;
  register unsigned long long $190;
  register unsigned long long $189;
  register unsigned long long $188;
  register unsigned long long $187;
  register unsigned long long $186;
  register unsigned long long $185;
  register unsigned long long $184;
  register unsigned long long $183;
  register unsigned long long $182;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 4)) {
      break;
    }
    $182 = ld32(c + 4 * i);
    *(x + 5 * i) = $182;
    $183 = ld32(k + 4 * i);
    *(x + (1 + i)) = $183;
    $184 = ld32(in + 4 * i);
    *(x + (6 + i)) = $184;
    $185 = ld32(k + 16 + 4 * i);
    *(x + (11 + i)) = $185;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(y + i) = *(x + i);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 20)) {
      break;
    }
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 4)) {
        break;
      }
      m = 0;
      for (; 1; m = m + 1) {
        if (! (m < 4)) {
          break;
        }
        *(t + m) = *(x + (5 * j + 4 * m) % 16);
      }
      $186 = L32(*(t + 0) + *(t + 3), 7);
      *(t + 1) = *(t + 1) ^ $186;
      $187 = L32(*(t + 1) + *(t + 0), 9);
      *(t + 2) = *(t + 2) ^ $187;
      $188 = L32(*(t + 2) + *(t + 1), 13);
      *(t + 3) = *(t + 3) ^ $188;
      $189 = L32(*(t + 3) + *(t + 2), 18);
      *(t + 0) = *(t + 0) ^ $189;
      m = 0;
      for (; 1; m = m + 1) {
        if (! (m < 4)) {
          break;
        }
        *(w + (4 * j + (j + m) % 4)) = *(t + m);
      }
    }
    m = 0;
    for (; 1; m = m + 1) {
      if (! (m < 16)) {
        break;
      }
      *(x + m) = *(w + m);
    }
  }
  if (h) {
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 16)) {
        break;
      }
      *(x + i) = *(x + i) + *(y + i);
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 4)) {
        break;
      }
      $190 = ld32(c + 4 * i);
      *(x + 5 * i) = *(x + 5 * i) - $190;
      $191 = ld32(in + 4 * i);
      *(x + (6 + i)) = *(x + (6 + i)) - $191;
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 4)) {
        break;
      }
      st32(out + 4 * i, *(x + 5 * i));
      st32(out + 16 + 4 * i, *(x + (6 + i)));
    }
  } else {
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 16)) {
        break;
      }
      st32(out + 4 * i, *(x + i) + *(y + i));
    }
  }
}

int crypto_core_salsa20_tweet(unsigned char *out, unsigned char *in, unsigned char *k, unsigned char *c)
{
  core(out, in, k, c, 0);
  return 0;
}

int crypto_core_hsalsa20_tweet(unsigned char *out, unsigned char *in, unsigned char *k, unsigned char *c)
{
  core(out, in, k, c, 1);
  return 0;
}

unsigned char const sigma[16] = { 101, 120, 112, 97, 110, 100, 32, 51, 50,
  45, 98, 121, 116, 101, 32, 107, };

int crypto_stream_salsa20_tweet_xor(unsigned char *c, unsigned char *m, unsigned long long b, unsigned char *n, unsigned char *k)
{
  unsigned char z[16];
  unsigned char x[64];
  unsigned long long u;
  unsigned long long i;
  register int $183;
  register int $182;
  if (!b) {
    return 0;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(z + i) = 0;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 8)) {
      break;
    }
    *(z + i) = *(n + i);
  }
  while (1) {
    if (! (b >= 64)) {
      break;
    }
    crypto_core_salsa20_tweet(x, z, k, sigma);
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 64)) {
        break;
      }
      if (m) {
        $182 = (int) *(m + i);
      } else {
        $182 = (int) 0;
      }
      *(c + i) = $182 ^ *(x + i);
    }
    u = 1;
    i = 8;
    for (; 1; i = i + 1) {
      if (! (i < 16)) {
        break;
      }
      u = u + (unsigned long long) *(z + i);
      *(z + i) = u;
      u = u >> 8;
    }
    b = b - 64;
    c = c + 64;
    if (m) {
      m = m + 64;
    }
  }
  if (b) {
    crypto_core_salsa20_tweet(x, z, k, sigma);
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < b)) {
        break;
      }
      if (m) {
        $183 = (int) *(m + i);
      } else {
        $183 = (int) 0;
      }
      *(c + i) = $183 ^ *(x + i);
    }
  }
  return 0;
}

int crypto_stream_salsa20_tweet(unsigned char *c, unsigned long long d, unsigned char *n, unsigned char *k)
{
  register int $182;
  $182 = crypto_stream_salsa20_tweet_xor(c, 0, d, n, k);
  return $182;
}

int crypto_stream_xsalsa20_tweet(unsigned char *c, unsigned long long d, unsigned char *n, unsigned char *k)
{
  unsigned char s[32];
  register int $182;
  crypto_core_hsalsa20_tweet(s, n, k, sigma);
  $182 = crypto_stream_salsa20_tweet(c, d, n + 16, s);
  return $182;
}

int crypto_stream_xsalsa20_tweet_xor(unsigned char *c, unsigned char *m, unsigned long long d, unsigned char *n, unsigned char *k)
{
  unsigned char s[32];
  register int $182;
  crypto_core_hsalsa20_tweet(s, n, k, sigma);
  $182 = crypto_stream_salsa20_tweet_xor(c, m, d, n + 16, s);
  return $182;
}

void add1305(unsigned long long *h, unsigned long long *c)
{
  unsigned long long j;
  unsigned long long u;
  u = 0;
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 17)) {
      break;
    }
    u = u + (*(h + j) + *(c + j));
    *(h + j) = u & 255;
    u = u >> 8;
  }
}

unsigned long long const minusp[17] = { 5LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL,
  0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 252LL, };

int crypto_onetimeauth_poly1305_tweet(unsigned char *out, unsigned char *m, unsigned long long n, unsigned char *k)
{
  unsigned long long s;
  unsigned long long i;
  unsigned long long j;
  unsigned long long u;
  unsigned long long x[17];
  unsigned long long r[17];
  unsigned long long h[17];
  unsigned long long c[17];
  unsigned long long g[17];
  register unsigned long long $184;
  register int $183;
  register unsigned long long $182;
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 17)) {
      break;
    }
    $182 = (unsigned long long) 0;
    *(h + j) = $182;
    *(r + j) = $182;
  }
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 16)) {
      break;
    }
    *(r + j) = *(k + j);
  }
  *(r + 3) = *(r + 3) & 15;
  *(r + 4) = *(r + 4) & 252;
  *(r + 7) = *(r + 7) & 15;
  *(r + 8) = *(r + 8) & 252;
  *(r + 11) = *(r + 11) & 15;
  *(r + 12) = *(r + 12) & 252;
  *(r + 15) = *(r + 15) & 15;
  while (1) {
    if (! (n > 0)) {
      break;
    }
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 17)) {
        break;
      }
      *(c + j) = 0;
    }
    j = 0;
    for (; 1; j = j + 1) {
      if (j < 16) {
        $183 = (_Bool) (j < n);
      } else {
        $183 = 0;
      }
      if (! $183) {
        break;
      }
      *(c + j) = *(m + j);
    }
    *(c + j) = 1;
    m = m + j;
    n = n - j;
    add1305(h, c);
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 17)) {
        break;
      }
      *(x + i) = 0;
      j = 0;
      for (; 1; j = j + 1) {
        if (! (j < 17)) {
          break;
        }
        if (j <= i) {
          $184 = (unsigned long long) *(r + (i - j));
        } else {
          $184 = (unsigned long long) (320 * *(r + (i + 17 - j)));
        }
        *(x + i) = *(x + i) + *(h + j) * $184;
      }
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 17)) {
        break;
      }
      *(h + i) = *(x + i);
    }
    u = 0;
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 16)) {
        break;
      }
      u = u + *(h + j);
      *(h + j) = u & 255;
      u = u >> 8;
    }
    u = u + *(h + 16);
    *(h + 16) = u & 3;
    u = 5 * (u >> 2);
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 16)) {
        break;
      }
      u = u + *(h + j);
      *(h + j) = u & 255;
      u = u >> 8;
    }
    u = u + *(h + 16);
    *(h + 16) = u;
  }
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 17)) {
      break;
    }
    *(g + j) = *(h + j);
  }
  add1305(h, minusp);
  s = -(*(h + 16) >> 7);
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 17)) {
      break;
    }
    *(h + j) = *(h + j) ^ s & (*(g + j) ^ *(h + j));
  }
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 16)) {
      break;
    }
    *(c + j) = *(k + (j + 16));
  }
  *(c + 16) = 0;
  add1305(h, c);
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 16)) {
      break;
    }
    *(out + j) = *(h + j);
  }
  return 0;
}

int crypto_onetimeauth_poly1305_tweet_verify(unsigned char *h, unsigned char *m, unsigned long long n, unsigned char *k)
{
  unsigned char x[16];
  register int $182;
  crypto_onetimeauth_poly1305_tweet(x, m, n, k);
  $182 = crypto_verify_16_tweet(h, x);
  return $182;
}

int crypto_secretbox_xsalsa20poly1305_tweet(unsigned char *c, unsigned char *m, unsigned long long d, unsigned char *n, unsigned char *k)
{
  int i;
  if (d < 32) {
    return -1;
  }
  crypto_stream_xsalsa20_tweet_xor(c, m, d, n, k);
  crypto_onetimeauth_poly1305_tweet(c + 16, c + 32, d - 32, c);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(c + i) = 0;
  }
  return 0;
}

int crypto_secretbox_xsalsa20poly1305_tweet_open(unsigned char *m, unsigned char *c, unsigned long long d, unsigned char *n, unsigned char *k)
{
  int i;
  unsigned char x[32];
  register int $182;
  if (d < 32) {
    return -1;
  }
  crypto_stream_xsalsa20_tweet(x, 32, n, k);
  $182 = crypto_onetimeauth_poly1305_tweet_verify(c + 16, c + 32, d - 32, x);
  if ($182 != 0) {
    return -1;
  }
  crypto_stream_xsalsa20_tweet_xor(m, c, d, n, k);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(m + i) = 0;
  }
  return 0;
}

void set25519(long long *r, long long *a)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(r + i) = *(a + i);
  }
}

void car25519(long long *o)
{
  int i;
  long long c;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + i) = *(o + i) + (1LL << 16);
    c = *(o + i) >> 16;
    *(o + (i + 1) * (i < 15)) =
      *(o + (i + 1) * (i < 15)) + (c - 1 + 37 * (c - 1) * (i == 15));
    *(o + i) = *(o + i) - (c << 16);
  }
}

void sel25519(long long *p, long long *q, int b)
{
  long long t;
  long long i;
  long long c;
  c = ~(b - 1);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    t = c & (*(p + i) ^ *(q + i));
    *(p + i) = *(p + i) ^ t;
    *(q + i) = *(q + i) ^ t;
  }
}

void pack25519(unsigned char *o, long long *n)
{
  int i;
  int j;
  int b;
  long long m[16];
  long long t[16];
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(t + i) = *(n + i);
  }
  car25519(t);
  car25519(t);
  car25519(t);
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 2)) {
      break;
    }
    *(m + 0) = *(t + 0) - 65517;
    i = 1;
    for (; 1; i = i + 1) {
      if (! (i < 15)) {
        break;
      }
      *(m + i) = *(t + i) - 65535 - (*(m + (i - 1)) >> 16 & 1);
      *(m + (i - 1)) = *(m + (i - 1)) & 65535;
    }
    *(m + 15) = *(t + 15) - 32767 - (*(m + 14) >> 16 & 1);
    b = *(m + 15) >> 16 & 1;
    *(m + 14) = *(m + 14) & 65535;
    sel25519(t, m, 1 - b);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + 2 * i) = *(t + i) & 255;
    *(o + (2 * i + 1)) = *(t + i) >> 8;
  }
}

int neq25519(long long *a, long long *b)
{
  unsigned char c[32];
  unsigned char d[32];
  register int $182;
  pack25519(c, a);
  pack25519(d, b);
  $182 = crypto_verify_32_tweet(c, d);
  return $182;
}

unsigned char par25519(long long *a)
{
  unsigned char d[32];
  pack25519(d, a);
  return *(d + 0) & 1;
}

void unpack25519(long long *o, unsigned char *n)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + i) = *(n + 2 * i) + ((long long) *(n + (2 * i + 1)) << 8);
  }
  *(o + 15) = *(o + 15) & 32767;
}

void A(long long *o, long long *a, long long *b)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + i) = *(a + i) + *(b + i);
  }
}

void Z(long long *o, long long *a, long long *b)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + i) = *(a + i) - *(b + i);
  }
}

void M(long long *o, long long *a, long long *b)
{
  long long i;
  long long j;
  long long t[31];
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 31)) {
      break;
    }
    *(t + i) = 0;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 16)) {
        break;
      }
      *(t + (i + j)) = *(t + (i + j)) + *(a + i) * *(b + j);
    }
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 15)) {
      break;
    }
    *(t + i) = *(t + i) + 38 * *(t + (i + 16));
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(o + i) = *(t + i);
  }
  car25519(o);
  car25519(o);
}

void S(long long *o, long long *a)
{
  M(o, a, a);
}

void inv25519(long long *o, long long *i)
{
  long long c[16];
  int a;
  register int $182;
  a = 0;
  for (; 1; a = a + 1) {
    if (! (a < 16)) {
      break;
    }
    *(c + a) = *(i + a);
  }
  a = 253;
  for (; 1; a = a - 1) {
    if (! (a >= 0)) {
      break;
    }
    S(c, c);
    if (a != 2) {
      $182 = (_Bool) (a != 4);
    } else {
      $182 = 0;
    }
    if ($182) {
      M(c, c, i);
    }
  }
  a = 0;
  for (; 1; a = a + 1) {
    if (! (a < 16)) {
      break;
    }
    *(o + a) = *(c + a);
  }
}

void pow2523(long long *o, long long *i)
{
  long long c[16];
  int a;
  a = 0;
  for (; 1; a = a + 1) {
    if (! (a < 16)) {
      break;
    }
    *(c + a) = *(i + a);
  }
  a = 250;
  for (; 1; a = a - 1) {
    if (! (a >= 0)) {
      break;
    }
    S(c, c);
    if (a != 1) {
      M(c, c, i);
    }
  }
  a = 0;
  for (; 1; a = a + 1) {
    if (! (a < 16)) {
      break;
    }
    *(o + a) = *(c + a);
  }
}

int crypto_scalarmult_curve25519_tweet(unsigned char *q, unsigned char *n, unsigned char *p)
{
  unsigned char z[32];
  long long x[80];
  long long r;
  long long i;
  long long a[16];
  long long b[16];
  long long c[16];
  long long d[16];
  long long e[16];
  long long f[16];
  register long long $184;
  register long long $183;
  register long long $182;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 31)) {
      break;
    }
    *(z + i) = *(n + i);
  }
  *(z + 31) = *(n + 31) & 127 | 64;
  *(z + 0) = *(z + 0) & 248;
  unpack25519(x, p);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(b + i) = *(x + i);
    $182 = (long long) 0;
    *(c + i) = $182;
    $183 = (long long) $182;
    *(a + i) = $183;
    *(d + i) = $183;
  }
  $184 = (long long) 1;
  *(d + 0) = $184;
  *(a + 0) = $184;
  i = 254;
  for (; 1; i = i - 1) {
    if (! (i >= 0)) {
      break;
    }
    r = *(z + (i >> 3)) >> (i & 7) & 1;
    sel25519(a, b, r);
    sel25519(c, d, r);
    A(e, a, c);
    Z(a, a, c);
    A(c, b, d);
    Z(b, b, d);
    S(d, e);
    S(f, a);
    M(a, c, a);
    M(c, b, e);
    A(e, a, c);
    Z(a, a, c);
    S(b, a);
    Z(c, d, f);
    M(a, c, _121665);
    A(a, a, d);
    M(c, c, a);
    M(a, d, f);
    M(d, b, x);
    S(b, e);
    sel25519(a, b, r);
    sel25519(c, d, r);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 16)) {
      break;
    }
    *(x + (i + 16)) = *(a + i);
    *(x + (i + 32)) = *(c + i);
    *(x + (i + 48)) = *(b + i);
    *(x + (i + 64)) = *(d + i);
  }
  inv25519(x + 32, x + 32);
  M(x + 16, x + 16, x + 32);
  pack25519(q, x + 16);
  return 0;
}

int crypto_scalarmult_curve25519_tweet_base(unsigned char *q, unsigned char *n)
{
  register int $182;
  $182 = crypto_scalarmult_curve25519_tweet(q, n, _9);
  return $182;
}

int crypto_box_curve25519xsalsa20poly1305_tweet_beforenm(unsigned char *k, unsigned char *y, unsigned char *x)
{
  unsigned char s[32];
  register int $182;
  crypto_scalarmult_curve25519_tweet(s, x, y);
  $182 = crypto_core_hsalsa20_tweet(k, _0, s, sigma);
  return $182;
}

int crypto_box_curve25519xsalsa20poly1305_tweet_afternm(unsigned char *c, unsigned char *m, unsigned long long d, unsigned char *n, unsigned char *k)
{
  register int $182;
  $182 = crypto_secretbox_xsalsa20poly1305_tweet(c, m, d, n, k);
  return $182;
}

int crypto_box_curve25519xsalsa20poly1305_tweet_open_afternm(unsigned char *m, unsigned char *c, unsigned long long d, unsigned char *n, unsigned char *k)
{
  register int $182;
  $182 = crypto_secretbox_xsalsa20poly1305_tweet_open(m, c, d, n, k);
  return $182;
}

int crypto_box_curve25519xsalsa20poly1305_tweet(unsigned char *c, unsigned char *m, unsigned long long d, unsigned char *n, unsigned char *y, unsigned char *x)
{
  unsigned char k[32];
  register int $182;
  crypto_box_curve25519xsalsa20poly1305_tweet_beforenm(k, y, x);
  $182 = crypto_box_curve25519xsalsa20poly1305_tweet_afternm(c, m, d, n, k);
  return $182;
}

int crypto_box_curve25519xsalsa20poly1305_tweet_open(unsigned char *m, unsigned char *c, unsigned long long d, unsigned char *n, unsigned char *y, unsigned char *x)
{
  unsigned char k[32];
  register int $182;
  crypto_box_curve25519xsalsa20poly1305_tweet_beforenm(k, y, x);
  $182 =
    crypto_box_curve25519xsalsa20poly1305_tweet_open_afternm
    (m, c, d, n, k);
  return $182;
}

unsigned long long R(unsigned long long x, int c)
{
  return x >> c | x << 64 - c;
}

unsigned long long Ch(unsigned long long x, unsigned long long y, unsigned long long z)
{
  return x & y ^ ~x & z;
}

unsigned long long Maj(unsigned long long x, unsigned long long y, unsigned long long z)
{
  return x & y ^ x & z ^ y & z;
}

unsigned long long Sigma0(unsigned long long x)
{
  register unsigned long long $184;
  register unsigned long long $183;
  register unsigned long long $182;
  $182 = R(x, 28);
  $183 = R(x, 34);
  $184 = R(x, 39);
  return $182 ^ $183 ^ $184;
}

unsigned long long Sigma1(unsigned long long x)
{
  register unsigned long long $184;
  register unsigned long long $183;
  register unsigned long long $182;
  $182 = R(x, 14);
  $183 = R(x, 18);
  $184 = R(x, 41);
  return $182 ^ $183 ^ $184;
}

unsigned long long sigma0(unsigned long long x)
{
  register unsigned long long $183;
  register unsigned long long $182;
  $182 = R(x, 1);
  $183 = R(x, 8);
  return $182 ^ $183 ^ x >> 7;
}

unsigned long long sigma1(unsigned long long x)
{
  register unsigned long long $183;
  register unsigned long long $182;
  $182 = R(x, 19);
  $183 = R(x, 61);
  return $182 ^ $183 ^ x >> 6;
}

unsigned long long const K[80] = { 4794697086780616226LL,
  8158064640168781261LL, -5349999486874862801LL, -1606136188198331460LL,
  4131703408338449720LL, 6480981068601479193LL, -7908458776815382629LL,
  -6116909921290321640LL, -2880145864133508542LL, 1334009975649890238LL,
  2608012711638119052LL, 6128411473006802146LL, 8268148722764581231LL,
  -9160688886553864527LL, -7215885187991268811LL, -4495734319001033068LL,
  -1973867731355612462LL, -1171420211273849373LL, 1135362057144423861LL,
  2597628984639134821LL, 3308224258029322869LL, 5365058923640841347LL,
  6679025012923562964LL, 8573033837759648693LL, -7476448914759557205LL,
  -6327057829258317296LL, -5763719355590565569LL, -4658551843659510044LL,
  -4116276920077217854LL, -3051310485924567259LL, 489312712824947311LL,
  1452737877330783856LL, 2861767655752347644LL, 3322285676063803686LL,
  5560940570517711597LL, 5996557281743188959LL, 7280758554555802590LL,
  8532644243296465576LL, -9096487096722542874LL, -7894198246740708037LL,
  -6719396339535248540LL, -6333637450476146687LL, -4446306890439682159LL,
  -4076793802049405392LL, -3345356375505022440LL, -2983346525034927856LL,
  -860691631967231958LL, 1182934255886127544LL, 1847814050463011016LL,
  2177327727835720531LL, 2830643537854262169LL, 3796741975233480872LL,
  4115178125766777443LL, 5681478168544905931LL, 6601373596472566643LL,
  7507060721942968483LL, 8399075790359081724LL, 8693463985226723168LL,
  -8878714635349349518LL, -8302665154208450068LL, -8016688836872298968LL,
  -6606660893046293015LL, -4685533653050689259LL, -4147400797238176981LL,
  -3880063495543823972LL, -3348786107499101689LL, -1523767162380948706LL,
  -757361751448694408LL, 500013540394364858LL, 748580250866718886LL,
  1242879168328830382LL, 1977374033974150939LL, 2944078676154940804LL,
  3659926193048069267LL, 4368137639120453308LL, 4836135668995329356LL,
  5532061633213252278LL, 6448918945643986474LL, 6902733635092675308LL,
  7801388544844847127LL, };

int crypto_hashblocks_sha512_tweet(unsigned char *x, unsigned char *m, unsigned long long n)
{
  unsigned long long z[8];
  unsigned long long b[8];
  unsigned long long a[8];
  unsigned long long w[16];
  unsigned long long t;
  int i;
  int j;
  register unsigned long long $190;
  register unsigned long long $189;
  register unsigned long long $188;
  register unsigned long long $187;
  register unsigned long long $186;
  register unsigned long long $185;
  register unsigned long long $184;
  register unsigned long long $183;
  register unsigned long long $182;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 8)) {
      break;
    }
    $182 = dl64(x + 8 * i);
    $183 = (unsigned long long) $182;
    *(a + i) = $183;
    *(z + i) = $183;
  }
  while (1) {
    if (! (n >= 128)) {
      break;
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 16)) {
        break;
      }
      $184 = dl64(m + 8 * i);
      *(w + i) = $184;
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 80)) {
        break;
      }
      j = 0;
      for (; 1; j = j + 1) {
        if (! (j < 8)) {
          break;
        }
        *(b + j) = *(a + j);
      }
      $185 = Sigma1(*(a + 4));
      $186 = Ch(*(a + 4), *(a + 5), *(a + 6));
      t = *(a + 7) + $185 + $186 + *(K + i) + *(w + i % 16);
      $187 = Sigma0(*(a + 0));
      $188 = Maj(*(a + 0), *(a + 1), *(a + 2));
      *(b + 7) = t + $187 + $188;
      *(b + 3) = *(b + 3) + t;
      j = 0;
      for (; 1; j = j + 1) {
        if (! (j < 8)) {
          break;
        }
        *(a + (j + 1) % 8) = *(b + j);
      }
      if (i % 16 == 15) {
        j = 0;
        for (; 1; j = j + 1) {
          if (! (j < 16)) {
            break;
          }
          $189 = sigma0(*(w + (j + 1) % 16));
          $190 = sigma1(*(w + (j + 14) % 16));
          *(w + j) = *(w + j) + (*(w + (j + 9) % 16) + $189 + $190);
        }
      }
    }
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < 8)) {
        break;
      }
      *(a + i) = *(a + i) + *(z + i);
      *(z + i) = *(a + i);
    }
    m = m + 128;
    n = n - 128;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 8)) {
      break;
    }
    ts64(x + 8 * i, *(z + i));
  }
  return n;
}

unsigned char const iv[64] = { 106, 9, 230, 103, 243, 188, 201, 8, 187, 103,
  174, 133, 132, 202, 167, 59, 60, 110, 243, 114, 254, 148, 248, 43, 165, 79,
  245, 58, 95, 29, 54, 241, 81, 14, 82, 127, 173, 230, 130, 209, 155, 5, 104,
  140, 43, 62, 108, 31, 31, 131, 217, 171, 251, 65, 189, 107, 91, 224, 205,
  25, 19, 126, 33, 121, };

int crypto_hash_sha512_tweet(unsigned char *out, unsigned char *m, unsigned long long n)
{
  unsigned char h[64];
  unsigned char x[256];
  unsigned long long i;
  unsigned long long b;
  b = n;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 64)) {
      break;
    }
    *(h + i) = *(iv + i);
  }
  crypto_hashblocks_sha512_tweet(h, m, n);
  m = m + n;
  n = n & 127;
  m = m - n;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 256)) {
      break;
    }
    *(x + i) = 0;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < n)) {
      break;
    }
    *(x + i) = *(m + i);
  }
  *(x + n) = 128;
  n = 256 - 128 * (n < 112);
  *(x + (n - 9)) = b >> 61;
  ts64(x + n - 8, b << 3);
  crypto_hashblocks_sha512_tweet(h, x, n);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 64)) {
      break;
    }
    *(out + i) = *(h + i);
  }
  return 0;
}

void add(long long (*p)[16], long long (*q)[16])
{
  long long a[16];
  long long b[16];
  long long c[16];
  long long d[16];
  long long t[16];
  long long e[16];
  long long f[16];
  long long g[16];
  long long h[16];
  Z(a, *(p + 1), *(p + 0));
  Z(t, *(q + 1), *(q + 0));
  M(a, a, t);
  A(b, *(p + 0), *(p + 1));
  A(t, *(q + 0), *(q + 1));
  M(b, b, t);
  M(c, *(p + 3), *(q + 3));
  M(c, c, D2);
  M(d, *(p + 2), *(q + 2));
  A(d, d, d);
  Z(e, b, a);
  Z(f, d, c);
  A(g, d, c);
  A(h, b, a);
  M(*(p + 0), e, f);
  M(*(p + 1), h, g);
  M(*(p + 2), g, f);
  M(*(p + 3), e, h);
}

void cswap(long long (*p)[16], long long (*q)[16], unsigned char b)
{
  int i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 4)) {
      break;
    }
    sel25519(*(p + i), *(q + i), b);
  }
}

void pack(unsigned char *r, long long (*p)[16])
{
  long long tx[16];
  long long ty[16];
  long long zi[16];
  register unsigned char $182;
  inv25519(zi, *(p + 2));
  M(tx, *(p + 0), zi);
  M(ty, *(p + 1), zi);
  pack25519(r, ty);
  $182 = par25519(tx);
  *(r + 31) = *(r + 31) ^ $182 << 7;
}

void scalarmult(long long (*p)[16], long long (*q)[16], unsigned char *s)
{
  int i;
  unsigned char b;
  set25519(*(p + 0), gf0);
  set25519(*(p + 1), gf1);
  set25519(*(p + 2), gf1);
  set25519(*(p + 3), gf0);
  i = 255;
  for (; 1; i = i - 1) {
    if (! (i >= 0)) {
      break;
    }
    b = *(s + i / 8) >> (i & 7) & 1;
    cswap(p, q, b);
    add(q, p);
    add(p, p);
    cswap(p, q, b);
  }
}

void scalarbase(long long (*p)[16], unsigned char *s)
{
  long long q[4][16];
  set25519(*(q + 0), X);
  set25519(*(q + 1), Y);
  set25519(*(q + 2), gf1);
  M(*(q + 3), X, Y);
  scalarmult(p, q, s);
}

unsigned long long const L[32] = { 237LL, 211LL, 245LL, 92LL, 26LL, 99LL,
  18LL, 88LL, 214LL, 156LL, 247LL, 162LL, 222LL, 249LL, 222LL, 20LL, 0LL,
  0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 0LL, 16LL,
  };

void modL(unsigned char *r, long long *x)
{
  long long carry;
  long long i;
  long long j;
  i = 63;
  for (; 1; i = i - 1) {
    if (! (i >= 32)) {
      break;
    }
    carry = 0;
    j = i - 32;
    for (; 1; j = j + 1) {
      if (! (j < i - 12)) {
        break;
      }
      *(x + j) = *(x + j) + (carry - 16 * *(x + i) * *(L + (j - (i - 32))));
      carry = *(x + j) + 128 >> 8;
      *(x + j) = *(x + j) - (carry << 8);
    }
    *(x + j) = *(x + j) + carry;
    *(x + i) = 0;
  }
  carry = 0;
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 32)) {
      break;
    }
    *(x + j) = *(x + j) + (carry - (*(x + 31) >> 4) * *(L + j));
    carry = *(x + j) >> 8;
    *(x + j) = *(x + j) & 255;
  }
  j = 0;
  for (; 1; j = j + 1) {
    if (! (j < 32)) {
      break;
    }
    *(x + j) = *(x + j) - carry * *(L + j);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(x + (i + 1)) = *(x + (i + 1)) + (*(x + i) >> 8);
    *(r + i) = *(x + i) & 255;
  }
}

void reduce(unsigned char *r)
{
  long long x[64];
  long long i;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 64)) {
      break;
    }
    *(x + i) = (unsigned long long) *(r + i);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 64)) {
      break;
    }
    *(r + i) = 0;
  }
  modL(r, x);
}

int crypto_sign_ed25519_tweet(unsigned char *sm, unsigned long long *smlen, unsigned char *m, unsigned long long n, unsigned char *sk)
{
  unsigned char d[64];
  unsigned char h[64];
  unsigned char r[64];
  long long i;
  long long j;
  long long x[64];
  long long p[4][16];
  crypto_hash_sha512_tweet(d, sk, 32);
  *(d + 0) = *(d + 0) & 248;
  *(d + 31) = *(d + 31) & 127;
  *(d + 31) = *(d + 31) | 64;
  *smlen = n + 64;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < n)) {
      break;
    }
    *(sm + (64 + i)) = *(m + i);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(sm + (32 + i)) = *(d + (32 + i));
  }
  crypto_hash_sha512_tweet(r, sm + 32, n + 32);
  reduce(r);
  scalarbase(p, r);
  pack(sm, p);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(sm + (i + 32)) = *(sk + (i + 32));
  }
  crypto_hash_sha512_tweet(h, sm, n + 64);
  reduce(h);
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 64)) {
      break;
    }
    *(x + i) = 0;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(x + i) = (unsigned long long) *(r + i);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    j = 0;
    for (; 1; j = j + 1) {
      if (! (j < 32)) {
        break;
      }
      *(x + (i + j)) =
        *(x + (i + j)) + *(h + i) * (unsigned long long) *(d + j);
    }
  }
  modL(sm + 32, x);
  return 0;
}

int unpackneg(long long (*r)[16], unsigned char *p)
{
  long long t[16];
  long long chk[16];
  long long num[16];
  long long den[16];
  long long den2[16];
  long long den4[16];
  long long den6[16];
  register unsigned char $184;
  register int $183;
  register int $182;
  set25519(*(r + 2), gf1);
  unpack25519(*(r + 1), p);
  S(num, *(r + 1));
  M(den, num, D);
  Z(num, num, *(r + 2));
  A(den, *(r + 2), den);
  S(den2, den);
  S(den4, den2);
  M(den6, den4, den2);
  M(t, den6, num);
  M(t, t, den);
  pow2523(t, t);
  M(t, t, num);
  M(t, t, den);
  M(t, t, den);
  M(*(r + 0), t, den);
  S(chk, *(r + 0));
  M(chk, chk, den);
  $182 = neq25519(chk, num);
  if ($182) {
    M(*(r + 0), *(r + 0), I);
  }
  S(chk, *(r + 0));
  M(chk, chk, den);
  $183 = neq25519(chk, num);
  if ($183) {
    return -1;
  }
  $184 = par25519(*(r + 0));
  if ($184 == *(p + 31) >> 7) {
    Z(*(r + 0), gf0, *(r + 0));
  }
  M(*(r + 3), *(r + 0), *(r + 1));
  return 0;
}

int crypto_sign_ed25519_tweet_open(unsigned char *m, unsigned long long *mlen, unsigned char *sm, unsigned long long n, unsigned char *pk)
{
  int i;
  unsigned char t[32];
  unsigned char h[64];
  long long p[4][16];
  long long q[4][16];
  register int $183;
  register int $182;
  *mlen = -1;
  if (n < 64) {
    return -1;
  }
  $182 = unpackneg(q, pk);
  if ($182) {
    return -1;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < n)) {
      break;
    }
    *(m + i) = *(sm + i);
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < 32)) {
      break;
    }
    *(m + (i + 32)) = *(pk + i);
  }
  crypto_hash_sha512_tweet(h, m, n);
  reduce(h);
  scalarmult(p, q, h);
  scalarbase(q, sm + 32);
  add(p, q);
  pack(t, p);
  n = n - 64;
  $183 = crypto_verify_32_tweet(sm, t);
  if ($183) {
    i = 0;
    for (; 1; i = i + 1) {
      if (! (i < n)) {
        break;
      }
      *(m + i) = 0;
    }
    return -1;
  }
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < n)) {
      break;
    }
    *(m + i) = *(sm + (i + 64));
  }
  *mlen = n;
  return 0;
}

int main(void)
{
  unsigned long long mlen;
  unsigned long long smlen;
  unsigned long long n;
  unsigned char secret_key[64];
  unsigned char plain_text[64];
  unsigned char signed_text[128];
  unsigned char actual[128];
  signed char *delim;
  int i;
  register signed char *$182;
  mlen = -1;
  smlen = -1;
  n = 8;
  *(secret_key + 0) = 59;
  *(secret_key + 1) = 193;
  *(secret_key + 2) = 131;
  *(secret_key + 3) = 246;
  *(secret_key + 4) = 219;
  *(secret_key + 5) = 92;
  *(secret_key + 6) = 230;
  *(secret_key + 7) = 150;
  *(secret_key + 8) = 193;
  *(secret_key + 9) = 253;
  *(secret_key + 10) = 11;
  *(secret_key + 11) = 194;
  *(secret_key + 12) = 43;
  *(secret_key + 13) = 85;
  *(secret_key + 14) = 187;
  *(secret_key + 15) = 77;
  *(secret_key + 16) = 245;
  *(secret_key + 17) = 78;
  *(secret_key + 18) = 153;
  *(secret_key + 19) = 222;
  *(secret_key + 20) = 25;
  *(secret_key + 21) = 98;
  *(secret_key + 22) = 88;
  *(secret_key + 23) = 14;
  *(secret_key + 24) = 103;
  *(secret_key + 25) = 109;
  *(secret_key + 26) = 144;
  *(secret_key + 27) = 191;
  *(secret_key + 28) = 62;
  *(secret_key + 29) = 134;
  *(secret_key + 30) = 132;
  *(secret_key + 31) = 198;
  *(secret_key + 32) = 104;
  *(secret_key + 33) = 35;
  *(secret_key + 34) = 198;
  *(secret_key + 35) = 236;
  *(secret_key + 36) = 77;
  *(secret_key + 37) = 96;
  *(secret_key + 38) = 188;
  *(secret_key + 39) = 224;
  *(secret_key + 40) = 67;
  *(secret_key + 41) = 193;
  *(secret_key + 42) = 103;
  *(secret_key + 43) = 99;
  *(secret_key + 44) = 243;
  *(secret_key + 45) = 99;
  *(secret_key + 46) = 156;
  *(secret_key + 47) = 122;
  *(secret_key + 48) = 135;
  *(secret_key + 49) = 155;
  *(secret_key + 50) = 99;
  *(secret_key + 51) = 64;
  *(secret_key + 52) = 17;
  *(secret_key + 53) = 49;
  *(secret_key + 54) = 216;
  *(secret_key + 55) = 158;
  *(secret_key + 56) = 209;
  *(secret_key + 57) = 113;
  *(secret_key + 58) = 190;
  *(secret_key + 59) = 250;
  *(secret_key + 60) = 218;
  *(secret_key + 61) = 63;
  *(secret_key + 62) = 1;
  *(secret_key + 63) = 247;
  *(plain_text + 0) = 90;
  *(plain_text + 1) = 164;
  *(plain_text + 2) = 159;
  *(plain_text + 3) = 132;
  *(plain_text + 4) = 206;
  *(plain_text + 5) = 75;
  *(plain_text + 6) = 53;
  *(plain_text + 7) = 185;
  *(plain_text + 8) = 44;
  *(plain_text + 9) = 165;
  *(plain_text + 10) = 222;
  *(plain_text + 11) = 132;
  *(plain_text + 12) = 175;
  *(plain_text + 13) = 188;
  *(plain_text + 14) = 235;
  *(plain_text + 15) = 193;
  *(plain_text + 16) = 69;
  *(plain_text + 17) = 0;
  *(plain_text + 18) = 127;
  *(plain_text + 19) = 243;
  *(plain_text + 20) = 23;
  *(plain_text + 21) = 212;
  *(plain_text + 22) = 170;
  *(plain_text + 23) = 132;
  *(plain_text + 24) = 6;
  *(plain_text + 25) = 27;
  *(plain_text + 26) = 172;
  *(plain_text + 27) = 123;
  *(plain_text + 28) = 206;
  *(plain_text + 29) = 45;
  *(plain_text + 30) = 124;
  *(plain_text + 31) = 80;
  *(plain_text + 32) = 194;
  *(plain_text + 33) = 243;
  *(plain_text + 34) = 109;
  *(plain_text + 35) = 194;
  *(plain_text + 36) = 145;
  *(plain_text + 37) = 233;
  *(plain_text + 38) = 5;
  *(plain_text + 39) = 12;
  *(plain_text + 40) = 251;
  *(plain_text + 41) = 93;
  *(plain_text + 42) = 19;
  *(plain_text + 43) = 104;
  *(plain_text + 44) = 155;
  *(plain_text + 45) = 210;
  *(plain_text + 46) = 207;
  *(plain_text + 47) = 109;
  *(plain_text + 48) = 72;
  *(plain_text + 49) = 88;
  *(plain_text + 50) = 172;
  *(plain_text + 51) = 157;
  *(plain_text + 52) = 233;
  *(plain_text + 53) = 15;
  *(plain_text + 54) = 213;
  *(plain_text + 55) = 228;
  *(plain_text + 56) = 40;
  *(plain_text + 57) = 229;
  *(plain_text + 58) = 200;
  *(plain_text + 59) = 229;
  *(plain_text + 60) = 95;
  *(plain_text + 61) = 163;
  *(plain_text + 62) = 79;
  *(plain_text + 63) = 251;
  *(signed_text + 0) = 205;
  *(signed_text + 1) = 199;
  *(signed_text + 2) = 186;
  *(signed_text + 3) = 178;
  *(signed_text + 4) = 205;
  *(signed_text + 5) = 97;
  *(signed_text + 6) = 158;
  *(signed_text + 7) = 184;
  *(signed_text + 8) = 22;
  *(signed_text + 9) = 79;
  *(signed_text + 10) = 198;
  *(signed_text + 11) = 219;
  *(signed_text + 12) = 93;
  *(signed_text + 13) = 241;
  *(signed_text + 14) = 249;
  *(signed_text + 15) = 48;
  *(signed_text + 16) = 187;
  *(signed_text + 17) = 32;
  *(signed_text + 18) = 219;
  *(signed_text + 19) = 211;
  *(signed_text + 20) = 12;
  *(signed_text + 21) = 180;
  *(signed_text + 22) = 184;
  *(signed_text + 23) = 129;
  *(signed_text + 24) = 190;
  *(signed_text + 25) = 182;
  *(signed_text + 26) = 79;
  *(signed_text + 27) = 121;
  *(signed_text + 28) = 222;
  *(signed_text + 29) = 247;
  *(signed_text + 30) = 88;
  *(signed_text + 31) = 84;
  *(signed_text + 32) = 101;
  *(signed_text + 33) = 20;
  *(signed_text + 34) = 104;
  *(signed_text + 35) = 85;
  *(signed_text + 36) = 20;
  *(signed_text + 37) = 138;
  *(signed_text + 38) = 9;
  *(signed_text + 39) = 188;
  *(signed_text + 40) = 94;
  *(signed_text + 41) = 110;
  *(signed_text + 42) = 98;
  *(signed_text + 43) = 106;
  *(signed_text + 44) = 122;
  *(signed_text + 45) = 163;
  *(signed_text + 46) = 25;
  *(signed_text + 47) = 40;
  *(signed_text + 48) = 81;
  *(signed_text + 49) = 180;
  *(signed_text + 50) = 42;
  *(signed_text + 51) = 56;
  *(signed_text + 52) = 128;
  *(signed_text + 53) = 117;
  *(signed_text + 54) = 201;
  *(signed_text + 55) = 14;
  *(signed_text + 56) = 205;
  *(signed_text + 57) = 134;
  *(signed_text + 58) = 137;
  *(signed_text + 59) = 214;
  *(signed_text + 60) = 32;
  *(signed_text + 61) = 146;
  *(signed_text + 62) = 203;
  *(signed_text + 63) = 12;
  *(signed_text + 64) = 90;
  *(signed_text + 65) = 164;
  *(signed_text + 66) = 159;
  *(signed_text + 67) = 132;
  *(signed_text + 68) = 206;
  *(signed_text + 69) = 75;
  *(signed_text + 70) = 53;
  *(signed_text + 71) = 185;
  *(signed_text + 72) = 44;
  *(signed_text + 73) = 165;
  *(signed_text + 74) = 222;
  *(signed_text + 75) = 132;
  *(signed_text + 76) = 175;
  *(signed_text + 77) = 188;
  *(signed_text + 78) = 235;
  *(signed_text + 79) = 193;
  *(signed_text + 80) = 69;
  *(signed_text + 81) = 0;
  *(signed_text + 82) = 127;
  *(signed_text + 83) = 243;
  *(signed_text + 84) = 23;
  *(signed_text + 85) = 212;
  *(signed_text + 86) = 170;
  *(signed_text + 87) = 132;
  *(signed_text + 88) = 6;
  *(signed_text + 89) = 27;
  *(signed_text + 90) = 172;
  *(signed_text + 91) = 123;
  *(signed_text + 92) = 206;
  *(signed_text + 93) = 45;
  *(signed_text + 94) = 124;
  *(signed_text + 95) = 80;
  *(signed_text + 96) = 194;
  *(signed_text + 97) = 243;
  *(signed_text + 98) = 109;
  *(signed_text + 99) = 194;
  *(signed_text + 100) = 145;
  *(signed_text + 101) = 233;
  *(signed_text + 102) = 5;
  *(signed_text + 103) = 12;
  *(signed_text + 104) = 251;
  *(signed_text + 105) = 93;
  *(signed_text + 106) = 19;
  *(signed_text + 107) = 104;
  *(signed_text + 108) = 155;
  *(signed_text + 109) = 210;
  *(signed_text + 110) = 207;
  *(signed_text + 111) = 109;
  *(signed_text + 112) = 72;
  *(signed_text + 113) = 88;
  *(signed_text + 114) = 172;
  *(signed_text + 115) = 157;
  *(signed_text + 116) = 233;
  *(signed_text + 117) = 15;
  *(signed_text + 118) = 213;
  *(signed_text + 119) = 228;
  *(signed_text + 120) = 40;
  *(signed_text + 121) = 229;
  *(signed_text + 122) = 200;
  *(signed_text + 123) = 229;
  *(signed_text + 124) = 95;
  *(signed_text + 125) = 163;
  *(signed_text + 126) = 79;
  *(signed_text + 127) = 251;
  crypto_sign_ed25519_tweet
    (actual, &smlen, plain_text, sizeof(unsigned char [64]), secret_key);
  delim = __stringlit_1;
  i = 0;
  for (; 1; i = i + 1) {
    if (! (i < smlen)) {
      break;
    }
    printf(__stringlit_2, delim, *(actual + i));
    if (i % 16 == 15) {
      $182 = (signed char *) __stringlit_4;
    } else {
      $182 = (signed char *) __stringlit_3;
    }
    delim = $182;
  }
  return 0;
}


