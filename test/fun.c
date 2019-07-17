long f00() { }
long f01() { return 10; }
long f02() { return 1 + 2; }
long f03(long x) { return x; }
long f04(long x) { return x + 5; }
long f05(long x) { return -x; }
long f06(long x) { return !x; }
long f07(long x, long y) { return x + y; }
long f08(long x, long y) { return x * y; }
long f09(long x, long y) { return x / y; }
long f10(long x, long y) { return x % y; }
long f11(long x, long y) { return x == y; }
long f12(long x, long y) { return x != y; }
long f13(long x, long y) { return x < y; }
long f14(long x, long y) { return x > y; }
long f15(long x, long y) { return x <= y; }
long f16(long x, long y) { return x >= y; }
long f17(long x, long y, long z) { return x + y + z; }
long f18(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a0;
}
long f19(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a1;
}
long f20(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a2;
}
long f21(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a3;
}
long f22(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a4;
}
long f23(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a5;
}
long f24(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a6;
}
long f25(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a7;
}
long f26(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a8;
}
long f27(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a9;
}
long f28(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a10;
}
long f29(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a11;
}

long f30(long x, long y) {
  return x * ! y;
}

long f31(long x) {
  return x / 2 / 3;
}

long f32(long x, long y, long z) {
  return x - y - z;
}

long f33(long a, long x, long b, long y) {
  return a * x + b * y;
}

long f34(long a, long b, long c) {
  return a < b < c;
}

long f35(long a, long b, long c) {
  return a == b == c;
}

long f36(long a, long b, long c) {
  return a == b < c;
}

long f37(long a, long b, long c,
         long d, long e, long f,
         long g, long h, long i) {
  return a + b * c == d + e * f < g + h * i;
}

long f38(long a, long b, long c,
         long d, long e, long f,
         long g, long h) {
  return a = b = c + d * e < f + g * h;
}

long f39(long x) {
  long y;
  y = x + 2;
  return y * y;
}

long f40(long x, long y, long z) {
  if (x) {
    return y + z;
  } else {
    return y * z;
  }
}

long f41(long x, long y, long z) {
  long p;
  p = 100;
  if (x > 0)
    if (y > 0)
      p = 200;
    else                        /* dangling else */
      p = 300;
  return p;
}

long f42(long n) {
  long i;
  long s;
  i = 0;
  s = 0;
  while (i < n * n) {
    s = s + i;
    i = i + 1;
  }
  return s;
}

/* primality check */
long f43(long n) {
  long p;
  p = 2;
  while (p * p <= n) {
    if (n % p == 0) return 0;
    p = p + 1;
  }
  return 1;
}

long f44(long n) {
  return f01() + 1;
}

long f45(long n) {
  return f03(n) + 1;
}

long f46(long x, long y) {
  return f07(x, y) + 1;
}

long f47() {
  return f18(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f48() {
  return f19(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f49() {
  return f20(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f50() {
  return f21(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f51() {
  return f22(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f52() {
  return f23(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f53() {
  return f24(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f54() {
  return f25(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f55() {
  return f26(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f56() {
  return f27(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f57() {
  return f28(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f58() {
  return f29(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}

long f59(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f60(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f61(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f62(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f63(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f64(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f23(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f65(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f24(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f66(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f25(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f67(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f26(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f68(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f27(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f69(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f28(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

long f70(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f29(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}

/* check if all args are passed correctly */
long f71(long a, long b, long c,
         long d, long e, long f,
         long g, long h, long i) {
  return f37(b, c, d, e, f, g, h, i, a) + 1;
}

/* call function twice */
long f72(long a, long b, long c,
         long d, long e, long f,
         long g, long h, long i) {
  long p;
  long q;
  p = f37(b, c, d, e, f, g, h, i, a);
  q = f37(c, d, e, f, g, h, i, a, b);
  return p + q;
}

/* fibonacci */
long f73(long n) {
  if (n <= 0) {
    return 1;
  } else if (n > 1000) {
    return f73(1000);
  } else {
    return f73(n - 1) * n;
  }
}

long f74(long n) {
  if (n < 2) {
    return 1;
  } else if (n > 30) {
    return f74(30);
  } else {
    return f74(n - 1) + f74(n - 2);
  }
}

