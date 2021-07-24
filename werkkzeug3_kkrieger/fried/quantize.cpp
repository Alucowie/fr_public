// This file is distributed under a BSD license. See LICENSE.txt for details.

// FRIED
// quantization functions.

#include "_types.hpp"
#include "fried_internal.hpp"

namespace FRIED
{
  // ---- the quantization tables themselves
  static bool tablesInitialized = false;
  static sInt qdescale[8][16];
  static sInt qrescale[8][16];

  // ---- macroblock AC scanning pattern

#if 0
  static sInt macscan[15] = { 1,5,4,8,9,6,2,3,7,10,13,12,14,15,11 };
  static sInt zigzag[15]  = { 4,1,2,5,8,12,9,6,3,7,10,13,14,11,15 };
#endif
  static sInt zigzag2[16] = { 0,4,1,2,5,8,12,9,6,3,7,10,13,14,11,15 };

  // ---- transform row norms (2-norm)

  static sF32 xformn[4] = { 1.0000f, 1.3260f, 1.0000f, 1.5104f };
  // exact values: 1.3260 ^= sqrt(3601/2048), 1.5104 ^= sqrt(73/32)

  // ---- helper functions

#if 0
  static sInt imuls(sInt a,sInt b,sInt s,sInt bias)
  {
    long long prod = ((long long) a) * b;

    if(prod >= 0)
      prod -= bias;
    else
      prod += bias;

    prod += 1 << (s - 1);
    return sInt(prod >> s);
  }
#endif

  static sInt imul14(sInt a,sInt b)
  {
#ifdef __GNUC__
#if defined(__x86_64__)
    asm (
      "mov   %1, %%eax\n\t"
      "imul  %2\n\t"
      "add   $8192, %%eax\n\t"
      "adc   $0, %%edx\n\t"
      "shrd  $14, %%edx, %%eax\n\t"

      "mov   %%eax, %0\n\t"
      : "=r" (a)
      : "r" (a), "r" (b)
    );
#endif
#if defined(__i386__)
    asm (
      "mov   %1, %%eax\n\t"
      "imul  %2\n\t"
      "add   $8192, %%eax\n\t"
      "adc   $0, %%edx\n\t"
      "shrd  $14, %%edx, %%eax\n\t"

      "mov   %%eax, %0\n\t"
      : "=r" (a)
      : "r" (a), "r" (b)
    );
#endif
#else
    __asm
    {
      mov   eax, [a];
      imul  [b];
      add   eax, 8192;
      adc   edx, 0;
      shrd  eax, edx, 14;

      mov   [a], eax;
    }
#endif

    return a;
  }

#if 0
  static sInt descaleOld(sInt x,sInt bias,sInt factor,sInt shift)
  {
    return imuls(x,factor,shift+14,bias);
  }
#endif

  static sInt descale(sInt x,sInt bias,sInt factor,sInt shift)
  {
    sInt p = imul14(x,factor);
    sInt scale = 1 << shift;
    sInt realbias = (scale >> 1) - (bias >> 14);

    if(p >= 0)
      p += realbias;
    else
      p -= realbias - scale + 1;

    return p >> shift;
  }

#if 0
  static sInt rescale(sInt x,sInt factor,sInt shift)
  {
    sInt p = x * factor;
    if(shift < 4)
      return p >> (4 - shift);
    else
      return p << (shift - 4);
  }
#endif

  static void initQuantTables()
  {
    if(tablesInitialized)
      return;

    for(sInt level=0;level<8;level++)
    {
      sF64 factor = pow(2.0,level / 8.0);
      
      for(sInt y=0;y<4;y++)
      {
        for(sInt x=0;x<4;x++)
        {
          sInt rescale = sInt(16 * factor * xformn[x] * xformn[y] + 0.5);
          sInt descale = (524288 + rescale) / (2 * rescale);

          qrescale[level][y*4+x] = rescale;
          qdescale[level][y*4+x] = descale;
        }
      }
    }

    tablesInitialized = true;
  }

  // ---- actual quantization functions

  sInt newQuantize(sInt qs,sInt *x,sInt npts,sInt cwidth)
  {
    sInt shift,*qtab,bias,i,f,n,*p;

    // prepare quantizer tables
    initQuantTables();

    shift = qs >> 3;
    qtab = qdescale[qs & 7];
    bias = 1024 << shift;

    // quantization by groups
    p = x;

    for(i=0;i<16;i++)
    {
      f = qtab[zigzag2[i]];

      for(n=0;n<cwidth;n++) {
        *p = descale(*p,bias,f,shift);
        p++;
      }
    }

    // now find number of zeroes
    for(n=npts-1;n>=0;n--)
      if(x[n])
        break;

    return n+1;
  }

#if 0
  static void rescaleLoop(sS16 *x,sInt count,sInt f,sInt shift)
  {
    if(shift < 4)
    {
      shift = 4 - shift;

      for(sInt i=0;i<count;i++)
        x[i] = (x[i] * f) >> shift;
    }
    else
    {
      shift -= 4;

      for(sInt i=0;i<count;i++)
        x[i] = (x[i] * f) << shift;
    }
  }
#endif

  static void rescaleLoopMMX(sS16 *x,sInt count,sInt f,sInt shift)
  {
    if(shift >= 4)
    {
      shift -= 4;

#ifdef __GNUC__
#if defined(__x86_64__)
      asm (
        "mov       %0, %%rsi\n\t"
        "mov       %1, %%ecx\n\t"
        "shr       $3, %%ecx\n\t"
        "jz        rescale1tail\n\t"

        "movd      %2, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "movd      %3, %%mm7\n\t"

  "rescale1lp:\n\t"
        "movq      (%%rsi), %%mm0\n\t"
        "movq      8(%%rsi), %%mm1\n\t"

        "pmullw    %%mm6, %%mm0\n\t"
        "pmullw    %%mm6, %%mm1\n\t"

        "psllw     %%mm7, %%mm0\n\t"
        "psllw     %%mm7, %%mm1\n\t"

        "movq      %%mm0, (%%rsi)\n\t"
        "movq      %%mm1, 8(%%rsi)\n\t"

        "add       $16, %%rsi\n\t"
        "dec       %%ecx;\n\t"
        "jnz       rescale1lp\n\t"

  "rescale1tail:\n\t"
        "mov       %1, %%edx\n\t"
        "and       $7, %%edx\n\t"
        "jz        rescale1end\n\t"
        "mov       %2, %%ebx\n\t"
        "mov       %3, %%ecx\n\t"

  "rescale1taillp:\n\t"
        "movsx     (%%rsi), %%eax\n\t"
        "imul      %%ebx, %%eax\n\t"
        "shl       %%cl, %%eax\n\t"
        "mov       %%ax, (%%rsi)\n\t"

        "add       $2, %%rsi\n\t"
        "dec       %%edx;\n\t"
        "jnz       rescale1taillp\n\t"

  "rescale1end:\n\t"
        "emms\n\t"
        :
        : "r" (x), "r" (count), "r" (f), "r" (shift)
      );
#endif
#if defined(__i386__)
      asm (
        "mov       %0, %%esi\n\t"
        "mov       %1, %%ecx\n\t"
        "shr       $3, %%ecx\n\t"
        "jz        rescale1tail\n\t"

        "movd      %2, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "movd      %3, %%mm7\n\t"

  "rescale1lp:\n\t"
        "movq      (%%esi), %%mm0\n\t"
        "movq      8(%%esi), %%mm1\n\t"

        "pmullw    %%mm6, %%mm0\n\t"
        "pmullw    %%mm6, %%mm1\n\t"

        "psllw     %%mm7, %%mm0\n\t"
        "psllw     %%mm7, %%mm1\n\t"

        "movq      %%mm0, (%%esi)\n\t"
        "movq      %%mm1, 8(%%esi)\n\t"

        "add       $16, %%esi\n\t"
        "dec       %%ecx;\n\t"
        "jnz       rescale1lp\n\t"

  "rescale1tail:\n\t"
        "mov       %1, %%edx\n\t"
        "and       $7, %%edx\n\t"
        "jz        rescale1end\n\t"
        "mov       %2, %%ebx\n\t"
        "mov       %3, %%ecx\n\t"

  "rescale1taillp:\n\t"
        "movsx     (%%esi), %%eax\n\t"
        "imul      %%ebx, %%eax\n\t"
        "shl       %%cl, %%eax\n\t"
        "mov       %%ax, (%%esi)\n\t"

        "add       $2, %%esi\n\t"
        "dec       %%edx;\n\t"
        "jnz       rescale1taillp\n\t"

  "rescale1end:\n\t"
        "emms\n\t"
        :
        : "r" (x), "r" (count), "r" (f), "r" (shift)
      );
#endif
#else
      __asm
      {
        mov       esi, [x];
        mov       ecx, [count];
        shr       ecx, 3;
        jz        rescale1tail;

        movd      mm6, [f];
        punpcklwd mm6, mm6;
        punpcklwd mm6, mm6;
        movd      mm7, [shift];

  rescale1lp:
        movq      mm0, [esi];
        movq      mm1, [esi+8];

        pmullw    mm0, mm6;
        pmullw    mm1, mm6;

        psllw     mm0, mm7;
        psllw     mm1, mm7;

        movq      [esi], mm0;
        movq      [esi+8], mm1;

        add       esi, 16;
        dec       ecx;
        jnz       rescale1lp;

  rescale1tail:
        mov       edx, [count];
        and       edx, 7;
        jz        rescale1end;
        mov       ebx, [f];
        mov       ecx, [shift];

  rescale1taillp:
        movsx     eax, word ptr [esi];
        imul      eax, ebx;
        shl       eax, cl;
        mov       word ptr [esi], ax;
        
        add       esi, 2;
        dec       edx;
        jnz       rescale1taillp;

  rescale1end:
        emms;
      }
#endif
    }
    else
    {
      shift = 4 - shift;

#ifdef __GNUC__
#if defined(__x86_64__)
      asm (
        "mov       %0, %%rsi\n\t"
        "mov       %1, %%ecx\n\t"
        "shr       $3, %%ecx\n\t"
        "jz        rescale2tail\n\t"

        "movd      %2, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "movd      %3, %%mm7\n\t"

  "rescale2lp:\n\t"
        "movq      (%%rsi), %%mm0\n\t"
        "movq      8(%%rsi), %%mm1\n\t"

        "pmullw    %%mm6, %%mm0;\n\t"
        "pmullw    %%mm6, %%mm1\n\t"

        "psraw     %%mm7, %%mm0\n\t"
        "psraw     %%mm7, %%mm1\n\t"

        "movq      %%mm0, (%%rsi)\n\t"
        "movq      %%mm1, 8(%%rsi)\n\t"

        "add       $16, %%rsi\n\t"
        "dec       %%ecx;\n\t"
        "jnz       rescale2lp\n\t"

  "rescale2tail:\n\t"
        "mov       %1, %%edx\n\t"
        "and       $7, %%edx\n\t"
        "jz        rescale2end\n\t"
        "mov       %2, %%ebx\n\t"
        "mov       %3, %%ecx\n\t"

  "rescale2taillp:\n\t"
        "movsx     (%%rsi), %%eax\n\t"
        "imul      %%ebx, %%eax\n\t"
        "shr       %%cl, %%eax\n\t"
        "mov       %%ax, (%%rsi)\n\t"

        "add       $2, %%rsi\n\t"
        "dec       %%edx;\n\t"
        "jnz       rescale2taillp\n\t"

  "rescale2end:\n\t"
        "emms\n\t"
        :
        : "r" (x), "r" (count), "r" (f), "r" (shift)
      );
#endif
#if defined(__i386__)
      asm (
        "mov       %0, %%esi\n\t"
        "mov       %1, %%ecx\n\t"
        "shr       $3, %%ecx\n\t"
        "jz        rescale2tail\n\t"

        "movd      %2, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "punpcklwd %%mm6, %%mm6\n\t"
        "movd      %3, %%mm7\n\t"

  "rescale2lp:\n\t"
        "movq      (%%esi), %%mm0\n\t"
        "movq      8(%%esi), %%mm1\n\t"

        "pmullw    %%mm6, %%mm0;\n\t"
        "pmullw    %%mm6, %%mm1\n\t"

        "psraw     %%mm7, %%mm0\n\t"
        "psraw     %%mm7, %%mm1\n\t"

        "movq      %%mm0, (%%esi)\n\t"
        "movq      %%mm1, 8(%%esi)\n\t"

        "add       $16, %%esi\n\t"
        "dec       %%ecx;\n\t"
        "jnz       rescale2lp\n\t"

  "rescale2tail:\n\t"
        "mov       %1, %%edx\n\t"
        "and       $7, %%edx\n\t"
        "jz        rescale2end\n\t"
        "mov       %2, %%ebx\n\t"
        "mov       %3, %%ecx\n\t"

  "rescale2taillp:\n\t"
        "movsx     (%%esi), %%eax\n\t"
        "imul      %%ebx, %%eax\n\t"
        "shr       %%cl, %%eax\n\t"
        "mov       %%ax, (%%esi)\n\t"

        "add       $2, %%esi\n\t"
        "dec       %%edx;\n\t"
        "jnz       rescale2taillp\n\t"

  "rescale2end:\n\t"
        "emms\n\t"
        :
        : "r" (x), "r" (count), "r" (f), "r" (shift)
      );
#endif
#else
      __asm
      {
        mov       esi, [x];
        mov       ecx, [count];
        shr       ecx, 3;
        jz        rescale2tail;

        movd      mm6, [f];
        punpcklwd mm6, mm6;
        punpcklwd mm6, mm6;
        movd      mm7, [shift];

  rescale2lp:
        movq      mm0, [esi];
        movq      mm1, [esi+8];

        pmullw    mm0, mm6;
        pmullw    mm1, mm6;

        psraw     mm0, mm7;
        psraw     mm1, mm7;

        movq      [esi], mm0;
        movq      [esi+8], mm1;

        add       esi, 16;
        dec       ecx;
        jnz       rescale2lp;

  rescale2tail:
        mov       edx, [count];
        and       edx, 7;
        jz        rescale2end;
        mov       ebx, [f];
        mov       ecx, [shift];

  rescale2taillp:
        movsx     eax, word ptr [esi];
        imul      eax, ebx;
        shr       eax, cl;
        mov       word ptr [esi], ax;
        
        add       esi, 2;
        dec       edx;
        jnz       rescale2taillp;

  rescale2end:
        emms;
      }
#endif
    }
  }

  void newDequantize(sInt qs,sS16 *x,sInt npts,sInt cwidth)
  {
    sInt shift,*qtab,i,f,count;

    // prepare quantizer tables
    initQuantTables();

    shift = qs >> 3;
    qtab = qrescale[qs & 7];

    // dequantization by subbands
    for(i=0;i<16;i++)
    {
      f = qtab[zigzag2[i]];
      count = sMin(npts,cwidth);

      if(count)
      {
        rescaleLoopMMX(x,count,f,shift);
        x += count;
        npts -= count;
      }
    }
  }
}
