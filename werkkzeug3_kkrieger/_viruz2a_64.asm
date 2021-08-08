; This file is distributed under a BSD license. See LICENSE.txt for details.

;#####################################################################################
;
;
; Farb-Rausch ViruZ II  -  wer das liest, ist rule
;
;
;#####################################################################################


%define		POLY			32  							 ; maximum polyphony
%define		LOWEST		39000000h					 ; out of 16bit range
%define		MDMAXOFFS 1024

%define     MAX_FRAME_SIZE 280

;%define     VUMETER

%define     FIXDENORMALS 1               ; there's really no good reason to turn this off.
%define     V2PROFILER 0                 ; profiling support. refer to tinyplayer
%define		RONAN
%define     FULLMIDI

section .bss

maindelbuf  resd 2*32768
chandelbuf  resd 16*2*2048

vcebuf			resd MAX_FRAME_SIZE
vcebuf2			resd MAX_FRAME_SIZE
chanbuf			resd 2*MAX_FRAME_SIZE
aux1buf			resd MAX_FRAME_SIZE
aux2buf			resd MAX_FRAME_SIZE
mixbuf			resd 2*MAX_FRAME_SIZE
auxabuf			resd 2*MAX_FRAME_SIZE
auxbbuf			resd 2*MAX_FRAME_SIZE


temp			resd 16
oldfpcw			resd 1

section .data.rel.ro

icnoisemul equ 196314165
icnoiseadd equ 907633515

global _AUX_
_AUX_:

fci12			     dd 0.083333333333
fc2            dd 2.0
fc3            dd 3.0
fc6            dd 6.0
fc15           dd 15.0
fc32bit        dd 2147483647.0
fci127				 dd 0.007874015748
fci128         dd 0.0078125
fci4           dd 0.25
fci2           dd 0.5
fcpi_2				 dd 1.5707963267948966192313216916398
fcpi           dd 3.1415926535897932384626433832795
fc1p5pi        dd 4.7123889803846898576939650749193
fc2pi					 dd	6.28318530717958647692528676655901
fc64           dd 64.0
fc32           dd 32.0
fci16					 dd 0.0625
fcipi_2				 dd 0.636619772367581343075535053490057
fc32768        dd 32768.0
fc256          dd 256.0
fc10           dd 10.0
fci32768       dd 0.000030517578125
fc1023         dd 1023.0
fcm12          dd -12.0
fcm16          dd -16.0
fci8192        dd 0.0110485434560398050687631931578883
fci64          dd 0.015625
fc48           dd 48.0

fciframe			 dd 0.0078125     ; 1/FRAME_SIZE
fccfframe      dd 11.0          ; for calcfreq2

fcOscPitchOffs dd 60.0
;fcfmmax        dd 4.0
fcfmmax        dd 2.0
fcattackmul    dd -0.09375 ;-0.0859375
fcattackadd    dd 7.0
fcsusmul       dd 0.0019375
fcgain         dd 0.6
fcgainh        dd 0.6
fcmdlfomul     dd 1973915.486621315192743764172335601
fcsamplesperms dd 44.1
fccrelease     dd 0.01
fccpdfalloff   dd 0.9998


%ifdef VUMETER
fcvufalloff    dd 0.9999
%endif

fcsinx3        dq -0.16666
fcsinx5        dq 0.0083143
fcsinx7        dq -0.00018542


; r(x) = (x + 0.43157974*x^3)/(1 + 0.76443945*x^2 + 0.05831938*x^4)
; second set of coeffs is for r(1/x)
fcatanx1       dq 1.0         , -0.431597974
fcatanx3       dq 0.43157974  , -1.0
fcatanxm0      dq 1.0         , 0.05831938
fcatanxm4      dq 0.05831938  , 1.0
fcatanxm2      dq 0.76443945
fcatanadd      dq 0.0         , 1.5707963267948966192313216916398



;section .data
SRfcobasefrq		dd 12740060.0   ; C-3: 130.81278265029931733892499676165 Hz * 2^32 / SR
SRfclinfreq			dd 1.0                                   ; 44100hz/SR
SRfcBoostSin		dd 0.0213697517873015514981556266069164  ; sin of 150hz/SR
SRfcBoostCos		dd 0.999771640780307951892102701484347   ; cos of 150hz/SR
SRcFrameSize    dq 128

fcoscbase       dd 261.6255653005986346778499935233
fcsrbase        dd 44100.0
fcboostfreq     dd 150.0
fcframebase     dd 128.0

dcoffset        dd 0.000003814697265625 ; 2^-18

;#####################################################################################
;  Performance Monitor
;#####################################################################################

%define V2Perf_TOTAL      0
%define V2Perf_OSC        1
%define V2Perf_VCF        2
%define V2Perf_DIST       3
%define V2Perf_BASS       4
%define V2Perf_MODDEL     5
%define V2Perf_COMPRESSOR 6
%define V2Perf_PARAEQ     7
%define V2Perf_REVERB     8
%define V2Perf_RONAN      9

%define V2Perf_MAX        10

%if V2PROFILER
global          _V2Perf
_V2Perf:
	;   1234567890123456
	db "Total           ",0,0,0,0,0,0,0,0
	db "Oscillators     ",0,0,0,0,0,0,0,0
	db "Filters         ",0,0,0,0,0,0,0,0
	db "Distortion      ",0,0,0,0,0,0,0,0
	db "Bass Boost      ",0,0,0,0,0,0,0,0
	db "ModDelay        ",0,0,0,0,0,0,0,0
	db "Compressor      ",0,0,0,0,0,0,0,0
	db "Equalizer       ",0,0,0,0,0,0,0,0
	db "Reverb          ",0,0,0,0,0,0,0,0
	db "Ronan           ",0,0,0,0,0,0,0,0
	times 24 db 0

V2PerfCounter:
	times V2Perf_MAX dd 0,0
%endif

%macro V2PerfEnter 1
%if V2PROFILER
	push    rax
	push    rdx
	rdtsc
	sub     dword [V2PerfCounter + %1*8], rax
	sbb     dword [V2PerfCounter + %1*8 + 4], rdx
	pop     rdx
	pop     rax
%endif
%endmacro

%macro V2PerfLeave 1
%if V2PROFILER
	push    rax
	push    rdx
	rdtsc
	add     dword [V2PerfCounter + %1*8], rax
	adc     dword [V2PerfCounter + %1*8 + 4], rdx
	pop     rdx
	pop     rax
%endif
%endmacro

%macro V2PerfClear 0
%if V2PROFILER
	push    rdi
	push    rax
	push    rcx
	mov     rdi, V2PerfCounter
	mov     rcx, V2Perf_MAX * 2
	xor     rax, rax
	rep     stosd
	pop     rcx
	pop     rax
	pop     rdi
%endif
%endmacro

%macro V2PerfCopy 0
%if V2PROFILER
	push    rsi
	push    rdi
	mov     rsi, V2PerfCounter
	mov     rdi, _V2Perf
%%lp:
	cmp     byte [rdi], 0
	je      %%end
	add     rdi, byte 16
	movsd
	movsd
	jmp     short %%lp
%%end:
	pop     rdi
	pop     rsi
%endif
%endmacro

%macro Pushad 0
	push    rsp
	push    rax
	push    rbx
	push    rcx
	push    rdx
	push    rbp
	push    rsi
	push    rdi
	push    r8
	push    r9
	push    r10
	push    r11
	push    r12
	push    r13
	push    r14
	push    r15
%endmacro

%macro Popad 0
	pop     r15
	pop     r14
	pop     r13
	pop     r12
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rdi
	pop     rsi
	pop     rbp
	pop     rdx
	pop     rcx
	pop     rbx
	pop     rax
	pop     rsp
%endmacro

;#####################################################################################
;  Helper Routines
;#####################################################################################

section .text

global fastatan

fastatan: ; fast atan
	; value in st0, -1 in st1, high 16bits in ax, "8" in rbx

	shl		ax, 1
	fld1					; <1> <val> <-1>
	fcmovb  st0, st2        ; <sign> <val> <-1>

	xor     rdx, rdx
	cmp     ah, 7fh
	fmul    st1, st0        ; <sign> <absval> <-1>
	cmovge  rdx, rbx
	fxch    st1             ; <x> <sign> <-1>

	; r(x)= (cx1*x + cx3*x^3)/(cxm0 + cxm2*x^2 + cxm4*x^4)
	fld     st0                       ; <x> <x> <sign> <-1>
	fmul    st0, st0                  ; <x²> <x> <sign> <-1>
	fld     st0                       ; <x²> <x²> <x> <sign> <-1>
	fld     st0                       ; <x²> <x²> <x²> <x> <sign> <-1>
	lea		r8, [rel fcatanx3]
	fmul    qword [r8 + rdx]          ; <x²*cx3> <x²> <x²> <x> <sign> <-1>
	fxch    st1                       ; <x²> <x²(cx3)> <x²> <x> <sign> <-1>
	lea     r8, [rel fcatanxm4]
	fmul    qword [r8 + rdx]          ; <x²(cxm4)> <x²(cx3)> <x²> <x> <sign> <-1>
	fxch    st1                       ; <x²(cx3)> <x²(cxm4)> <x²> <x> <sign> <-1>
	lea     r8, [rel fcatanx1]
	fadd    qword [r8 + rdx]          ; <cx1+x²(cx3)> <x²(cxm4)> <x²> <x> <sign> <-1>
	fxch    st1                       ; <x²(cxm4)> <cx1+x²(cx3)> <x²> <x> <sign> <-1>
	fadd    qword [rel fcatanxm2]     ; <cxm2+x²(cxm4)> <cx1+x²(cx3)> <x²> <x> <sign> <-1>
	fxch    st1                       ; <cx1+x²(cx3)> <cxm2+x²(cxm4)> <x²> <x> <sign> <-1>
	fmulp   st3, st0                  ; <cxm2+x²(cxm4)> <x²> <x(cx1)+x³(cx3)> <sign> <-1>
	fmulp   st1, st0                  ; <x²(cxm2)+x^4(cxm4)> <x(cx1)+x³(cx3)> <sign> <-1>
	lea     r8, [rel fcatanxm0]
	fadd    qword [r8 + rdx]          ; <cxm0+x²(cxm2)+x^4(cxm4)> <x(cx1)+x³(cx3)> <sign> <-1>
	fdivp   st1, st0                  ; <r(x)> <sign> <-1>
	lea     r8, [rel fcatanadd]
	fadd    qword [r8 + rdx]          ; <r(x)+adder) <sign> <-1>

	fmulp   st1, st0                  ; <sign*r'(x)> <-1>
	ret


global fastsinrc
global fastsin

; x+ax3+bx5+cx7
; ((((c*x²)+b)*x²+a)*x²+1)*x


fastsinrc: ; fast sinus with range check
	fld			dword [rel fc2pi]            ; <2pi> <x>
	fxch		st1                     ; <x> <2pi>
	fprem		                        ; <x'> <2pi>
	fxch		st1                     ; <2pi> <x'>
	fstp		st0                     ; <x'>

	fld1                            ; <1> <x>
	fldz                            ; <0> <1> <x>
	fsub    st0, st1								; <mul> <1> <x>
	fldpi                           ; <sub> <mul> <1> <x>

	fld     dword [rel fcpi_2]      ; <pi/2> <sub> <mul> <1> <x>
	fcomi   st0, st4
	fstp    st0                     ; <sub> <mul> <1> <x>
	fldz                            ; <0> <sub> <mul> <1> <x>
	fxch    st1                     ; <sub> <0> <mul> <1> <x>
	fcmovnb st0, st1                ; <sub'> <0> <mul> <1> <x>
	fxch    st1                     ; <0> <sub'> <mul> <1> <x>
	fstp    st0                     ; <sub'> <mul> <1> <x>
	fxch    st1                     ; <mul> <sub'> <1> <x>
	fcmovnb st0, st2                ; <mul'> <sub'> <1> <x>

	fld     dword [rel fc1p5pi]     ; <1.5pi> <mul'> <sub'> <1> <x>
	fcomi   st0, st4
	fstp    st0                     ; <mul'> <sub'> <1> <x>
	fld     dword [rel fc2pi]           ; <2pi> <mul'> <sub'> <1> <x>
	fxch    st1                     ; <mul'> <2pi> <sub'> <1> <x>
	fcmovb  st0, st3                ; <mul''> <2pi> <sub'> <1> <x>
	fxch    st2                     ; <sub'> <2pi> <mul''> <1> <x>
	fcmovb  st0, st1                ; <sub''> <2pi> <mul''> <1> <x>
	fsubp   st4, st0                ; <2pi> <mul''> <1> <x-sub>
	fstp    st0                     ; <mul''> <1> <x-sub>
	fmulp   st2, st0                ; <1> <mul(x-sub)>
	fstp    st0                     ; <mul(x-sub)>


fastsin: ; fast sinus approximation (st0 -> st0) from -pi/2 to pi/2, about -80dB error, should be ok
	fld		st0											  ; <x> <x>
	fmul	st0, st1                 ; <x²> <x>
	fld		qword [rel fcsinx7]           ; <c> <x²> <x>
	fmul	st0, st1                 ; <cx²> <x²> <x>
	fadd	qword [rel fcsinx5]          ; <b+cx²> <x²> <x>
	fmul  st0, st1                 ; <x²(b+cx²)> <x²> <x>
	fadd  qword [rel fcsinx3]          ; <a+x²(b+cx²)> <x²> <x>
	fmulp st1, st0                 ; <x²(a+x²(b+cx²)> <x>
	fld1                           ; <1> <x²(a+x²(b+cx²)> <x>
	faddp st1, st0                 ; <1+x²(a+x²(b+cx²)> <x>
	fmulp st1, st0                 ; <x(1+x²(a+x²(b+cx²))>
	ret



global calcNewSampleRate
calcNewSampleRate: ; new SR in rax
	mov	    [rel temp], rax
	fild	dword [rel temp]		  ; <sr>
	fld1						      ; <1> <sr>
	fdiv    st0, st1				  ; <1/sr> <sr>
	fld     dword [rel fcoscbase]	  ; <oscbHz> <1/sr> <sr>
	fmul    dword [rel fc32bit]		  ; <oscb32>  <1/sr> <sr>
	fmul    st0, st1				  ; <oscbout> <1/sr> <sr>
	fstp    dword [rel SRfcobasefrq]  ; <1/sr> <sr>
	fld     dword [rel fcsrbase]      ; <srbase> <1/sr> <sr>
	fmul    st0, st1                  ; <linfrq> <1/sr> <sr>
	fstp    dword [rel SRfclinfreq]	  ; <1/sr> <sr>
	fmul    dword [rel fc2pi]         ; <boalpha> <sr>
	fmul    dword [rel fcboostfreq]   ; <bof/sr> <sr>
	fsincos                           ; <cos> <sin> <sr>
	fstp    dword [rel SRfcBoostCos]  ; <sin> <sr>
	fstp    dword [rel SRfcBoostSin]  ; <sr>
	fmul    dword [rel fcframebase]   ; <framebase*sr>
	fdiv    dword [rel fcsrbase]      ; <framelen>
	fadd    dword [rel fci2]          ; <round framelen>
	fistp   dword [rel SRcFrameSize]  ; -
	ret


calcfreq2:
	fld1							  ;	 1 <0..1>
	fsubp	st1, st0		          ;  <-1..0>
	fmul    dword [rel fccfframe]     ;  <-11..0> -> <1/2048..1>

	fld1
	fld	    st1
	fprem
	f2xm1
	faddp	st1, st0
	fscale
	fstp	st1
	ret

global calcfreq2
global calcfreq
global pow2
global pow

calcfreq: ; (0..1 linear zu 2^-10..1)
	fld1									;	 1 <0..1>
	fsubp		st1, st0			;  <-1..0>
	fmul    dword [rel fc10]  ;  <-10..0> -> <1/1024..1>

	fld1
	fld	st1
	fprem
	f2xm1
	faddp		st1, st0
	fscale
	fstp	st1
	ret

pow2:

	fld1
	fld	st1
	fprem
	f2xm1
	faddp		st1, st0
	fscale
	fstp	st1
	ret

pow:
	fyl2x
	fld1
	fld	st1
	fprem
	f2xm1
	faddp		st1, st0
	fscale
	fstp	st1
	ret


;#####################################################################################
;  Oszillator
;#####################################################################################


global _OSC_
_OSC_:

struc syVOsc
	.mode:    resd 1
	.ring:    resd 1
	.pitch:   resd 1
	.detune:  resd 1
	.color:   resd 1
	.gain:    resd 1
endstruc

struc syWOsc
	.mode:    resd 1  ; dword: mode (0=tri/saw 1=pulse 2=sin 3=noise)
	.ring:    resd 1  ; dword: ringmod on/off
	.cnt:     resd 1  ; dword: wave counter (32bit)
	.freq:    resd 1  ; dword: wave counter inc (8x/sample)
	.brpt:    resd 1  ; dword: break point für tri/pulse (32bit)
	.nffrq:   resd 1  ; noise: filter freq
	.nfres:   resd 1  ; noise: filter reso
	.nseed:   resd 1  ; noise: random seed
	.gain:    resd 1  ; output gain
	.gain4:   resd 1  ; output gain (oversampled)
	.tmp:     resd 1  ; temp
	.nfl:     resd 1
	.nfb:     resd 1
	.note:    resd 1  ; note
	.pitch:   resd 1  ; note
endstruc


syOscInit:
	Pushad
	xor			rax, rax
	mov     [rbp + syWOsc.cnt], rax
	mov     [rbp + syWOsc.cnt], rax
	mov     [rbp + syWOsc.nfl], rax
	mov     [rbp + syWOsc.nfb], rax

	rdtsc
	mov     [rbp + syWOsc.nseed], rax
	Popad
	ret



syOscChgPitch:
	fld     dword [rbp + syWOsc.pitch]
	fld     st0
	fadd    dword [rel fc64]
	fmul    dword [rel fci128]
	call    calcfreq
	fmul    dword [rel SRfclinfreq]
	fstp    dword [rbp + syWOsc.nffrq]
	fadd    dword [rbp + syWOsc.note]
	fsub    dword [rel fcOscPitchOffs]
	fmul    dword [rel fci12]
	call    pow2
	fmul    dword [rel SRfcobasefrq]
	fistp   dword [rbp + syWOsc.freq]
	ret



syOscSet:
	Pushad
	fld     dword [rsi + syVOsc.mode]
	fistp   dword [rbp + syWOsc.mode]

	fld     dword [rsi + syVOsc.ring]
	fistp   dword [rbp + syWOsc.ring]

	fld     dword [rsi + syVOsc.pitch]
	fsub    dword [rel fc64]
	fld			dword [rsi + syVOsc.detune]
	fsub    dword [rel fc64]
	fmul    dword [rel fci128]
	faddp   st1, st0
	fstp    dword [rbp + syWOsc.pitch]
	call    syOscChgPitch

	fld     dword [rsi + syVOsc.gain]
	fmul    dword [rel fci128]
	fst     dword [rbp + syWOsc.gain]
	fmul    dword [rel fci4]
	fstp    dword [rbp + syWOsc.gain4]

	fld     dword [rsi + syVOsc.color] ; <c>
	fmul    dword [rel fci128]						 ; <c'>
	fld     st0                        ; <c'> <c'>
	fmul    dword [rel fc32bit]						 ; <bp> <c'>
	fistp   dword [rbp + syWOsc.brpt]  ; <c'>
	shl     dword [rbp + syWOsc.brpt],1

	fsqrt                              ; <rc'>
	fld1                               ; <1> <rc'>
	fsubrp  st1, st0                   ; <1-rc'>
	fstp    dword [rbp + syWOsc.nfres] ; -

	Popad
	ret


; rdi: dest buf
; rsi: source buf
; rcx: # of samples
; rbp: workspace

section .data.rel.ro

oscjtab:
	dq syOscRender.off
	dq syOscRender.mode0
	dq syOscRender.mode1
	dq syOscRender.mode2
	dq syOscRender.mode3
	dq syOscRender.mode4
	dq syOscRender.auxa
	dq syOscRender.auxb

section .text

syOscRender:
	Pushad
	V2PerfEnter V2Perf_OSC
	lea     rdi, [rdi + 4*rcx]
	neg     rcx
	movzx	rax, byte [rbp + syWOsc.mode]
	and		al, 7
	lea     r8, [rel oscjtab]
	jmp		qword [r8 + 8*rax]

section .data.rel.ro

.m0casetab:
	dq syOscRender.m0c2,     ; ...
	dq syOscRender.m0c1,     ; ..n , INVALID!
	dq syOscRender.m0c212,   ; .c.
	dq syOscRender.m0c21,    ; .cn
	dq syOscRender.m0c12,    ; o..
	dq syOscRender.m0c1,     ; o.n
	dq syOscRender.m0c2,     ; oc. , INVALID!
	dq syOscRender.m0c121    ; ocn

section .text

.mode0:     ; tri/saw
	mov		rax, [rbp + syWOsc.cnt]
	mov   rsi, [rbp + syWOsc.freq]

	; calc float helper values
	mov   rbx, rsi
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fld   dword [rbp + syWOsc.gain]     ; <g>
	fld1                                ; 1 <g>
	fsubr dword [rbp + syWOsc.tmp]      ; <f> <g>
	fld1                                ; <1> <f> <g>
	fdiv  st0, st1                      ; <1/f> <f> <g>
	fld   st2                           ; <g> <1/f> <f> <g>
	mov   rbx, [rbp + syWOsc.brpt]
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fld   dword [rbp + syWOsc.tmp]     ; <b> <g> <1/f> <f> <g>
	fld1                               ; <1> <b> <g> <1/f> <f> <g>
	fsubr  st0, st1                    ; <col> <b> <g> <1/f> <f> <g>

	; m1=2/col
	; m2=-2/(1-col)
	; c1=gain/2*m1 = gain/col
	; c2=gain/2*m2 = -gain/(1-col)
	fld    st0                         ; <col> <col> <b> <g> <1/f> <f> <g>
	fdivr  st0, st3                    ; <c1> <col> <b> <g> <1/f> <f> <g>
	fld1                               ; <1> <c1> <col> <b> <g> <1/f> <f> <g>
	fsubrp st2, st0                    ; <c1> <1-col> <b> <g> <1/f> <f> <g>
	fxch   st1                         ; <1-col> <c1> <b> <g> <1/f> <f> <g>

	fdivp  st3, st0                    ; <c1> <b> <g/(1-col)> <1/f> <f> <g>
	fxch   st2                         ; <g/(1-col)> <b> <c1> <1/f> <f> <g>
	fchs                               ; <c2> <b> <c1> <1/f> <f> <g>

	; calc state
	mov   rbx, rax
	sub   rbx, rsi                 ; ................................  c
	rcr   rdx, 1                   ; c...............................  .
	cmp   rbx, [rbp + syWOsc.brpt] ; c...............................  n
	rcl   rdx, 2                   ; ..............................nc  .


.m0loop:
	mov   rbx, rax
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx

	fld   dword [rbp + syWOsc.tmp] ; <p+b> <c2> <b> <c1> <1/f> <f> <g>
	fsub  st0, st2                 ; <p> <c2> <b> <c1> <1/f> <f> <g>
	cmp   rax, [rbp + syWOsc.brpt] ; ..............................oc  n
	rcl   rdx, 1                   ; .............................ocn  .
	and   rdx, 7                   ; 00000000000000000000000000000ocn
	lea     r8, [rel .m0casetab]
	jmp		qword [r8 + 8*rdx]

; cases: on entry <p> <c2> <b> <c1> <1/f> <f>, on exit: <y> <c2> <b> <c1> <1/f> <f>

.m0c21: ; y=-(g+c2(p-f+1)²-c1p²)*(1/f)
	fld1                             ; <1> <p> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st1                  ; <p+1> <p> <c2> <b> <c1> <1/f> <f> <g>
	fsub   st0, st6                  ; <p+1-f> <p> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st0                  ; <(p+1-f)²> <p> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st2                  ; <c2(p+1-f)²> <p> <c2> <b> <c1> <1/f> <f> <g>
	fxch   st1                       ; <p> <c2(p+1-f)²> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st0                  ; <p²> <c2(p+1-f)²> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st4                  ; <c1p²> <c2(p+1-f)²> <c2> <b> <c1> <1/f> <f> <g>
	fsubp  st1, st0                  ; <c2(p-f+1)²-c1p²> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st6                  ; <g+c2(p-f+1)²-c1p²> <c2> <b> <c1> <1/f> <f> <g>
	fchs                             ; <-(g+c2(p-f+1)²-c1p²)> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st4                  ; <y> <c2> <b> <c1> <1/f> <f> <g>
	jmp    short .m0pl


.m0c121: ; y=(-g-c1(1-f)(2p+1-f))*(1/f)
	fadd   st0, st0                  ; <2p> <c2> <b> <c1> <1/f> <f> <g>
	fld1                             ; <1> <2p> <c2> <b> <c1> <1/f> <f> <g>
	fsub   st0, st6                  ; <1-f> <2p> <c2> <b> <c1> <1/f> <f> <g>
	fxch   st1                       ; <2p> <1-f> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st1                  ; <2p+1-f> <1-f> <c2> <b> <c1> <1/f> <f> <g>
	fmulp  st1, st0                  ; <(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st3                  ; <c1(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st6                  ; <g+c1(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fchs                             ; <-g-c1(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st4                  ; <y> <c2> <b> <c1> <1/f> <f> <g>
	jmp    short .m0pl


.m0c212: ; y=(g-c2(1-f)(2p+1-f))*(1/f)
	fadd   st0, st0                  ; <2p> <c2> <b> <c1> <1/f> <f> <g>
	fld1                             ; <1> <2p> <c2> <b> <c1> <1/f> <f> <g>
	fsub   st0, st6                  ; <1-f> <2p> <c2> <b> <c1> <1/f> <f> <g>
	fxch   st1                       ; <2p> <1-f> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st1                  ; <2p+1-f> <1-f> <c2> <b> <c1> <1/f> <f> <g>
	fmulp  st1, st0                  ; <(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st1                  ; <c2(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fadd   st0, st6                  ; <g+c2(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fchs                             ; <-g-c2(1-f)(2p+1-f)> <c2> <b> <c1> <1/f> <f> <g>
	fmul   st0, st4                  ; <y> <c2> <b> <c1> <1/f> <f> <g>
	jmp    short .m0pl

.m0c12:  ; y=(c2(p²)-c1((p-f)²))*(1/f)
	fld   st0                       ; <p> <p> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st0                  ; <p²> <p> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st2                  ; <c2(p²)> <p> <c2> <b> <c1> <1/f> <f> <g>
	fxch  st1                       ; <p> <c2(p²)> <c2> <b> <c1> <1/f> <f> <g>
	fsub  st0, st6                  ; <p-f> <c2(p²)> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st0                  ; <(p-f)²> <c2(p²)> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st4                  ; <c1*(p-f)²> <c2(p²)> <c2> <b> <c1> <1/f> <f> <g>
	fsubp st1, st0                  ; <c2(p²)-c1*(p-f)²> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st4                  ; <y> <c2> <b> <c1> <1/f> <f> <g>
	jmp   short .m0pl

.m0c1:  ; y=c1(2p-f)
	fadd  st0, st0                  ; <2p> <c2> <b> <c1> <1/f> <f> <g>
	fsub  st0, st5                  ; <2p-f> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st3                  ; <y> <c2> <b> <c1> <1/f> <f> <g>
	jmp   short .m0pl

.m0c2:  ; y=c2(2p-f)
	fadd  st0, st0                  ; <2p> <c2> <b> <c1> <1/f> <f> <g>
	fsub  st0, st5                  ; <2p-f> <c2> <b> <c1> <1/f> <f> <g>
	fmul  st0, st1                  ; <y> <c2> <b> <c1> <1/f> <f> <g>

.m0pl:
	fadd  st0, st6                 ; <out> <c2> <b> <c1> <1/f> <f> <g>
	add   rax, rsi                 ; ...............................n  c
	rcl   rdx, 1                   ; ..............................nc  .
	test  byte [rbp + syWOsc.ring], 1
	jz    .m0noring
	fmul	dword [rdi + 4*rcx]      ; <out'> <c2> <b> <c1> <1/f> <f> <g>
	jmp   .m0store
.m0noring:
	fadd	dword [rdi + 4*rcx]      ; <out'> <c2> <b> <c1> <1/f> <f> <g>
.m0store:
	fstp	dword [rdi + 4*rcx]      ; <c2> <b> <c1> <1/f> <f> <g>
	inc		rcx
	jz   .m0end
	jmp  .m0loop
.m0end:
	mov   [rbp + syWOsc.cnt], rax
	fstp  st0 ; <b> <c1> <1/f> <f> <g>
	fstp  st0 ; <c1> <1/f> <f> <g>
	fstp  st0 ; <1/f> <f> <g>
	fstp  st0 ; <f> <g>
	fstp  st0 ; <g>
	fstp  st0 ; -
.off:
	V2PerfLeave V2Perf_OSC
	Popad
	ret



section .data.rel.ro

.m1casetab:
	dq syOscRender.m1c2     ; ...
	dq syOscRender.m1c1     ; ..n , INVALID!
	dq syOscRender.m1c212   ; .c.
	dq syOscRender.m1c21    ; .cn
	dq syOscRender.m1c12    ; o..
	dq syOscRender.m1c1     ; o.n
	dq syOscRender.m1c2     ; oc. , INVALID!
	dq syOscRender.m1c121   ; ocn

section .text

.mode1:     ; pulse
	mov		rax, [rbp + syWOsc.cnt]
	mov   rsi, [rbp + syWOsc.freq]

	; calc float helper values
	mov   rbx, rsi
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fld   dword [rbp + syWOsc.gain]   ; <g>
	fld   dword [rbp + syWOsc.tmp]    ; <f+1> <g>
	fld1                              ; <1> <f+1> <g>
	fsubp st1, st0                    ; <f> <g>
	fdivr st0, st1                    ; <gdf> <g>
	mov   rbx, [rbp + syWOsc.brpt]
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fld   dword [rbp + syWOsc.tmp]    ; <b> <gdf> <g>
	fld   st0                         ; <b> <b> <gdf> <g>
	fadd  st0, st0                    ; <2b> <b> <gdf> <g>
	fld   st0                         ; <2b> <2b> <b> <gdf> <g>
	fld1                              ; <1> <2b> <2b> <b> <gdf> <g>
	fadd  st0, st0                    ; <2> <2b> <2b> <b> <gdf> <g>
	fsub  st1, st0                    ; <2> <2b-2> <2b> <b> <gdf> <g>
	fadd  st0, st0                    ; <4> <2b-2> <2b> <b> <gdf> <g>
	fsubp st2, st0                    ; <2b-2> <2b-4> <b> <gdf> <g>
	fmul  st0, st3                    ; <gdf(2b-2)> <2b-4> <b> <gdf> <g>
	fsub  st0, st4                    ; <cc212> <2b-4> <b> <gdf> <g>
	fxch  st1                         ; <2b-4> <cc212> <b> <gdf> <g>
	fmul  st0, st3                    ; <gdf(2b-4)> <cc212> <b> <gdf> <g>
	fadd  st0, st4                    ; <cc121> <cc212> <b> <gdf> <g>


	; calc state
	mov   rbx, rax
	sub   rbx, rsi                 ; ................................  c
	rcr   rdx, 1                   ; c...............................  .
	cmp   rbx, [rbp + syWOsc.brpt] ; c...............................  n
	rcl   rdx, 2                   ; ..............................nc  .


.m1loop:
	mov   rbx, rax
	shr   rbx, 9
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	cmp   rax, [rbp + syWOsc.brpt] ; ..............................oc  n
	rcl   rdx, 1                   ; .............................ocn  .
	and   rdx, 7                   ; 00000000000000000000000000000ocn
	lea   r8, [rel .m1casetab]
	jmp	  qword [r8 + 8*rdx]

; cases: on entry <cc121> <cc212> <b> <gdf> <g>, on exit: <out> <cc121> <cc212> <b> <gdf> <g>
.m1c21:  ; p2->p1 : 2(p-1)/f - 1
	fld   dword [rbp + syWOsc.tmp]   ; <p> <cc121> <cc212> <b> <gdf> <g>
	fld1                             ; <1> <p> <cc121> <cc212> <b> <gdf> <g>
	fsubp st1, st0                   ; <p-1> <cc121> <cc212> <b> <gdf> <g>
	fadd  st0, st0                   ; <2(p-1)> <cc121> <cc212> <b> <gdf> <g>
	fmul  st0, st4                   ; <gdf*2(p-1)> <cc121> <cc212> <b> <gdf> <g>
	fsub  st0, st5                   ; <out> <cc121> <cc212> <b> <gdf> <g>
	jmp   short .m1pl

.m1c12:  ; p1->p2 : 2(b-p)/f + 1
	fld   st2                        ; <b> <cc121> <cc212> <b> <gdf> <g>
	fsub  dword [rbp + syWOsc.tmp]   ; <b-p> <cc121> <cc212> <b> <gdf> <g>
	fadd  st0, st0                   ; <2(b-p)> <cc121> <cc212> <b> <gdf> <g>
	fmul  st0, st4                   ; <gdf*2(b-p)> <cc121> <cc212> <b> <gdf> <g>
	fadd  st0, st5                   ; <out> <cc121> <cc212> <b> <gdf> <g>
	jmp   short .m1pl

.m1c121: ; p1->p2->p1 : (2b-4)/f + 1
	fld  st0                          ; <out> <cc121> <cc212> <b> <gdf> <g>
	jmp  short .m1pl

.m1c212: ; p2->p1->p2 : (2b-2)/f - 1
	fld  st1                          ; <out> <cc121> <cc212> <b> <gdf> <g>
	jmp  short .m1pl

.m1c1:   ; p1 : 1
	fld   st4                         ; <out> <cc121> <cc212> <b> <gdf> <g>
	jmp  short .m1pl

.m1c2:   ; p2 : -1
	fld   st4                         ; <out> <cc121> <cc212> <b> <gdf> <g>
	fchs

.m1pl:
	add   rax, rsi                 ; ...............................n  c
	rcl   rdx, 1                   ; ..............................nc  .
	test  byte [rbp + syWOsc.ring], 1
	jz    .m1noring
	fmul	dword [rdi + 4*rcx]
	jmp   short .m1store
.m1noring:
	fadd	dword [rdi + 4*rcx]
.m1store:
	fstp	dword [rdi + 4*rcx]
	inc		rcx
	jnz   .m1loop
	mov   [rbp + syWOsc.cnt], rax
	fstp  st0 ; <cc212> <b> <gdf> <g>
	fstp  st0 ; <b> <gdf> <g>
	fstp  st0 ; <gdf> <g>
	fstp  st0 ; <g>
	fstp  st0 ; -
	V2PerfLeave V2Perf_OSC
	Popad
	ret





.mode2:    ; sin
	mov		rax, [rbp + syWOsc.cnt]
	mov   rdx, [rbp + syWOsc.freq]

	fld   qword [rel fcsinx7]  ; <cx7>
	fld   qword [rel fcsinx5]  ; <cx5> <cx7>
	fld   qword [rel fcsinx3]  ; <cx3> <cx5> <cx7>

.m2loop1:
	mov   rbx, rax
	add   rbx, 0x40000000
	mov   rsi, rbx
	sar		rsi, 31
	xor   rbx, rsi
	shr   rbx, 8
	add   rax, rdx
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fld   dword [rbp + syWOsc.tmp] ; <x> <cx3> <cx5> <cx7>

	; scale/move to (-pi/4 .. pi/4)
	fmul  dword [rel fcpi]         ; <x'> <cx3> <cx5> <cx7>
	fsub  dword [rel fc1p5pi]      ; <x''> <cx3> <cx5> <cx7>

	; inline fastsin
	fld		st0										   ; <x> <x> <cx3> <cx5> <cx7>
	fmul	st0, st1                 ; <x²> <x> <cx3> <cx5> <cx7>
	fld	  st4                      ; <c> <x²> <x> <cx3> <cx5> <cx7>
	fmul	st0, st1                 ; <cx²> <x²> <x> <cx3> <cx5> <cx7>
	fadd	st0, st4                 ; <b+cx²> <x²> <x> <cx3> <cx5> <cx7>
	fmul  st0, st1                 ; <x²(b+cx²)> <x²> <x> <cx3> <cx5> <cx7>
	fadd  st0, st3                 ; <a+x²(b+cx²)> <x²> <x> <cx3> <cx5> <cx7>
	fmulp st1, st0                 ; <x²(a+x²(b+cx²)> <x> <cx3> <cx5> <cx7>
	fld1                           ; <1> <x²(a+x²(b+cx²)> <x> <cx3> <cx5> <cx7>
	faddp st1, st0                 ; <1+x²(a+x²(b+cx²)> <x> <cx3> <cx5> <cx7>
	fmulp st1, st0                 ; <x(1+x²(a+x²(b+cx²))> <cx3> <cx5> <cx7>

	fmul  dword [rbp + syWOsc.gain] ; <gain(y)> <cx3> <cx5> <cx7>
	test  byte [rbp + syWOsc.ring], 1
	jz    .m2noring
	fmul	dword [rdi + 4*rcx]       ; <out> <cx3> <cx5> <cx7>
	jmp   short .m2store
.m2noring:
	fadd	dword [rdi + 4*rcx]       ; <out> <cx3> <cx5> <cx7>
.m2store:
	fstp	dword [rdi + 4*rcx]       ; <cx3> <cx5> <cx7>
	inc   rcx
	jnz   .m2loop1
	mov   [rbp + syWOsc.cnt], rax

	fstp  st0                         ; <cx5> <cx7>
	fstp  st0                         ; <cx7>
	fstp  st0                         ; -
	V2PerfLeave V2Perf_OSC
	Popad
	ret


.mode3:     ; noise
	mov   rsi,	[rbp + syWOsc.nseed]
	fld   dword [rbp + syWOsc.nfres]   ; <r>
	fld   dword [rbp + syWOsc.nffrq]   ; <f> <r>
	fld   dword [rbp + syWOsc.nfl]			; <l> <f> <r>
	fld   dword [rbp + syWOsc.nfb]			; <b> <l> <f> <r>
.m3loop1:
	imul	rsi, icnoisemul
	add		rsi, icnoiseadd
	mov		rax, rsi
	shr   rax, 9
	or    rax, 40000000h
	mov		[rbp + syWOsc.tmp], rax
	fld   dword [rbp + syWOsc.tmp]    ; <n> <b> <l> <f> <r>
	fld1							  ; <1> <n> <b> <l> <f> <r>
	fsub	st1, st0				  ; <1> <n-1> <b> <l> <f> <r>
	fsub	st1, st0				  ; <1> <n-2> <b> <l> <f> <r>
	fsubp st1, st0					  ; <n'> <b> <l> <f> <r>

	fld   st1                         ; <b> <n'> <b> <l> <f> <r>
	fmul  st0, st4                    ; <b*f> <n'> <b> <l> <f> <r>
	fxch  st1													; <n'> <b*f> <b> <l> <f> <r>
	fld   st2                         ; <b> <n'> <b*f> <b> <l> <f> <r>
	fmul  st0, st6                    ; <b*r> <n'> <b*f> <b> <l> <f> <r>
	fsubp st1, st0                    ; <n-(b*r)> <b*f> <b> <l> <f> <r>
	fxch  st1                         ; <b*f> <n-(b*r)> <b> <l> <f> <r>
	faddp st3, st0                    ; <n-(b*r)> <b> <l'> <f> <r>
	fsub  st0, st2                    ; <h> <b> <l'> <f> <r>
	fld   st0                         ; <h> <h> <b> <l'> <f> <r>
	fmul  st0, st4                    ; <h*f> <h> <b> <l'> <f> <r>
	faddp st2, st0                    ; <h> <b'> <l'> <f> <r>
	fld   st2                         ; <l'> <h> <b'> <l'> <f> <r>
	faddp st1, st0                    ; <l+h> <b'> <l'> <f> <r>
	fmul  st0, st4                    ; <r(l+h)> <b'> <l'> <f> <r>
	fadd  st0, st1                    ; <r(l+h)+b> <b'> <l'> <f> <r>

	fmul	dword [rbp + syWOsc.gain] ; <out> <b'> <l'> <f> <r>
	test  byte [rbp + syWOsc.ring], 1
	jz    .m3noring
	fmul	dword [rdi + 4*rcx]
	jmp   .m3store
.m3noring:
	fadd	dword [rdi + 4*rcx]
.m3store:
	fstp	dword [rdi + 4*rcx]
	inc		rcx
	jnz   .m3loop1
	fstp	dword [rbp + syWOsc.nfb]			; <l> <f> <r>
	fstp	dword [rbp + syWOsc.nfl]			; <f> <r>
	fstp  st0                           ; <r>
	fstp  st0                           ; <->
	mov   [rbp + syWOsc.nseed], rsi
	V2PerfLeave V2Perf_OSC
	Popad
	ret


.mode4:     ; fm sin
	mov		rax, [rbp + syWOsc.cnt]
	mov   rdx, [rbp + syWOsc.freq]
.m4loop1:
	mov   rbx, rax
	fld   dword [rdi + 4*rcx]  ; -1 .. 1
	fmul  dword [rel fcfmmax]
	shr   rbx, 9
	add   rax, rdx
	or    rbx, 0x3f800000
	mov   [rbp + syWOsc.tmp], rbx
	fadd  dword [rbp + syWOsc.tmp]
	fmul  dword [rel fc2pi]
	call  fastsinrc
	fmul  dword [rbp + syWOsc.gain]
	test  byte [rbp + syWOsc.ring], 1
	jz    .m4store
	fmul	dword [rdi + 4*rcx]
.m4store:
	fstp	dword [rdi + 4*rcx]
	inc   rcx
	jnz   .m4loop1
	mov   [rbp + syWOsc.cnt], rax
	V2PerfLeave V2Perf_OSC
	Popad
	ret

; CHAOS

.auxa:			; copy
	lea   rsi,[rel auxabuf]
.auxaloop:
	fld		dword [rsi+0]
	fadd	dword [rsi+4]
	add		rsi,8
	fmul  dword [rbp + syWOsc.gain]
	fmul	dword [rel fcgain]
	test  byte [rbp + syWOsc.ring], 1
	jz    .auxastore
	fmul	dword [rdi + 4*rcx]
.auxastore:
	fstp	dword [rdi + 4*rcx]
	inc   rcx
	jnz   .auxaloop
	V2PerfLeave V2Perf_OSC
	Popad
	ret



.auxb:			; copy
	lea   rsi,[rel auxbbuf]
	jmp .auxaloop

;#####################################################################################
;  Envelope Generator
;#####################################################################################


global _ENV_
_ENV_:

struc syVEnv
	.ar:    resd 1
	.dr:    resd 1
	.sl:    resd 1
	.sr:    resd 1
	.rr:    resd 1
	.vol:   resd 1
endstruc

struc syWEnv
	.out:    resd 1
	.state:  resd 1  ; int state - 0: off, 1: attack, 2: decay, 3: sustain, 4: release
	.val:    resd 1  ; float outval (0.0-128.0)
	.atd:    resd 1  ; float attack delta (added every frame in phase 1, transition -> 2 at 128.0)
	.dcf:    resd 1  ; float decay factor (mul'ed every frame in phase 2, transision -> 3 at .sul)
	.sul:    resd 1  ; float sustain level (defines phase 2->3 transition point)
	.suf:    resd 1  ; float sustain factor (mul'ed every frame in phase 3, transition -> 4 at gate off or ->0 at 0.0)
	.ref:    resd 1  ; float release (mul'ed every frame in phase 4, transition ->0 at 0.0)
	.gain:   resd 1  ; float gain (0.1 .. 1.0)
endstruc

; init
; rbp: workspace
syEnvInit:
	Pushad
	xor rax, rax
	mov [rbp + syWEnv.state], rax  ; reset state to "off"
	Popad
	ret

; set
; rsi: values
; epb: workspace
syEnvSet:
	Pushad

	; ar: 2^7 (128) bis 2^-4 (0.03, ca. 10secs bei 344frames/sec)
	fld		dword [rsi + syVEnv.ar]		; 0..127
	fmul  dword [rel fcattackmul]				; 0..-12
	fadd  dword [rel fcattackadd]				; 7..-5
	call  pow2
	fstp  dword [rbp + syWEnv.atd]

	; dcf: 0 (5msecs dank volramping) bis fast 1 (durchgehend)
	fld		dword [rsi + syVEnv.dr]		; 0..127
	fmul  dword [rel fci128]            ; 0..~1
	fld1                            ; 1  0..~1
	fsubrp st1, st0                 ; 1..~0
	call  calcfreq2
	fld1
	fsubrp st1, st0
	fstp  dword [rbp + syWEnv.dcf]  ; 0..~1

	; sul: 0..127 ist schon ok
	fld		dword [rsi + syVEnv.sl]		; 0..127
	fstp  dword [rbp + syWEnv.sul]  ; 0..127

	; suf: 1/128 (15msecs bis weg) bis 128 (15msecs bis voll da)
	fld		dword [rsi + syVEnv.sr]		; 0..127
	fsub  dword [rel fc64]              ; -64 .. 63
	fmul  dword [rel fcsusmul]          ; -7 .. ~7
	call  pow2
	fstp  dword [rbp + syWEnv.suf]  ; 1/128 .. ~128

	; ref: 0 (5msecs dank volramping) bis fast 1 (durchgehend)
	fld		dword [rsi + syVEnv.rr]		; 0..127
	fmul  dword [rel fci128]
	fld1
	fsubrp st1, st0
	call  calcfreq2
	fld1
	fsubrp st1, st0
	fstp  dword [rbp + syWEnv.ref]  ; 0..~1

	fld   dword [rsi + syVEnv.vol]
	fmul  dword [rel fci128]
	fstp  dword [rbp + syWEnv.gain]

	Popad
	ret


; tick
; epb: workspace
; ATTENTION: rax: gate

section .data.rel.ro

syETTab:
	dq syEnvTick.state_off
	dq syEnvTick.state_atk
	dq syEnvTick.state_dec
	dq syEnvTick.state_sus
	dq syEnvTick.state_rel

section .text

syEnvTick:
	Pushad
	mov		rbx, [rbp + syWEnv.state]
	lea   r8, [rel syETTab]
	call  [r8 + 8*rbx]
	fld   dword [rbp + syWEnv.val]
	fmul  dword [rbp + syWEnv.gain]
	fstp  dword [rbp + syWEnv.out]
	Popad
	ret

.state_off:  ; envelope off
	or    rax, rax  ; gate on -> attack
	jz		.s0ok
	inc		byte [rbp + syWEnv.state]
	jmp   .state_atk
.s0ok:
	fldz
	fstp  dword [rbp + syWEnv.val]
	ret

.state_atk: ; attack
	or    rax, rax  ; gate off -> release
	jnz		.s1ok
	mov		byte [rbp + syWEnv.state], 4
	jmp   .state_rel
.s1ok:
	fld   dword [rbp + syWEnv.val]
	fadd  dword [rbp + syWEnv.atd]
	fstp	dword [rbp + syWEnv.val]
	mov   rcx,  43000000h           ; 128.0
	cmp		rcx,	[rbp + syWEnv.val]
	ja    .s1end                    ; val above -> decay
	mov   [rbp + syWEnv.val], rcx
	inc   byte [rbp + syWEnv.state]
.s1end:
	ret

.state_dec:
	or    rax, rax  ; gate off -> release
	jnz		.s2ok
	mov		byte [rbp + syWEnv.state], 4
	jmp   .state_rel
.s2ok:
	fld   dword [rbp + syWEnv.val]
	fmul  dword [rbp + syWEnv.dcf]
	fstp	dword [rbp + syWEnv.val]
	mov   rcx,  [rbp + syWEnv.sul] ; sustain level
	cmp		rcx,	[rbp + syWEnv.val]
	jb    .s4checkrunout            ; val below -> sustain
	mov   [rbp + syWEnv.val], rcx
	inc   byte [rbp + syWEnv.state]
	ret


.state_sus:
	or    rax, rax  ; gate off -> release
	jnz		.s3ok
	inc		byte [rbp + syWEnv.state]
	jmp   .state_rel
.s3ok:
	fld   dword [rbp + syWEnv.val]
	fmul  dword [rbp + syWEnv.suf]
	fstp	dword [rbp + syWEnv.val]
	mov   rcx,  LOWEST
	cmp		rcx,	[rbp + syWEnv.val]
	jb    .s3not2low               ; val below -> off
	xor   rcx, rcx
	mov   [rbp + syWEnv.val], rcx
	mov   [rbp + syWEnv.state], rcx
	ret
.s3not2low:
	mov   rcx,  43000000h           ; 128.0
	cmp		rcx,	[rbp + syWEnv.val]
	ja    .s3end                    ; val above -> decay
	mov   [rbp + syWEnv.val], rcx
.s3end:
	ret


.state_rel:
	or    rax, rax  ; gate off -> release
	jz		.s4ok
	mov		byte [rbp + syWEnv.state], 1
	jmp   .state_atk
.s4ok:
	fld   dword [rbp + syWEnv.val]
	fmul  dword [rbp + syWEnv.ref]
	fstp	dword [rbp + syWEnv.val]
.s4checkrunout:
	mov   rcx,  LOWEST
	cmp		rcx,	[rbp + syWEnv.val]
	jb    .s4end
	xor   rcx, rcx
	mov   [rbp + syWEnv.val], rcx
	mov   [rbp + syWEnv.state], rcx
.s4end:
	ret



;#####################################################################################
;  Filter
;#####################################################################################

global _VCF_
_VCF_:


struc syVFlt
	.mode:   resd 1
	.cutoff: resd 1
	.reso:   resd 1
endstruc

struc syWFlt
	.mode:   resd 1  ; int: 0 - bypass, 1 - low, 2 - band, 3 - high, 4 - notch, 5 - all
	.cfreq:  resd 1  ; float: frq (0-1)
	.res:    resd 1  ; float: res (0-1)
	.l:      resd 1
	.b:      resd 1
	.step:   resd 1
endstruc

syFltInit:
	Pushad
	xor rax, rax
	mov [rbp + syWFlt.l], rax
	mov [rbp + syWFlt.b], rax
	mov  al, 4
	mov [rbp + syWFlt.step], rax
	Popad
	ret


syFltSet:
	Pushad
	fld			dword [rsi + syVFlt.mode]
	fistp		dword [rbp + syWFlt.mode]

	fld			dword [rsi + syVFlt.cutoff]
	fmul		dword [rel fci128]
	call    calcfreq


	fld			dword [rsi + syVFlt.reso]  ; <r> <fr> <fr>
	fmul		dword [rel fci128]
	fld1															 ; <1> <r> <fr>
	fsubrp  st1, st0                   ; <1-res> <fr>
	fstp		dword [rbp + syWFlt.res]   ; <fr>

	fmul    dword [rel SRfclinfreq]
	fstp	  dword [rbp + syWFlt.cfreq] ; -

	Popad
	ret

section .data.rel.ro

syFRTab:
	dq syFltRender.mode0
	dq syFltRender.mode1
	dq syFltRender.mode2
	dq syFltRender.mode3
	dq syFltRender.mode4
	dq syFltRender.mode5
	dq syFltRender.mode0
	dq syFltRender.mode0

section .text

; rbp: workspace
; rcx: count
; rsi: source
; rdi: dest
syFltRender:
	Pushad
	V2PerfEnter V2Perf_VCF
	fld   dword [rbp + syWFlt.res]	; <r>
	fld   dword [rbp + syWFlt.cfreq]; <f> <r>
	fld   dword [rbp + syWFlt.l]		; <l> <f> <r>
	fld   dword [rbp + syWFlt.b]		; <b> <l> <f> <r>
	movzx	rax, byte [rbp + syWFlt.mode]
	and   al, 7
	lea   r8, [rel syFRTab]
	call  [r8 + 8*rax]
	fstp dword [rbp + syWFlt.b] ; <l''> <f> <r>
	fstp dword [rbp + syWFlt.l] ; <f> <r>
	fstp st0
	fstp st0
	V2PerfLeave V2Perf_VCF
	Popad
	ret

.process: ; <b> <l> <f> <r>
	fld			dword [rsi]									; <in> <b> <l> <f> <r>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	fld			st1                         ; <b> <in> <b> <l> <f> <r>
	fld			st2                         ; <b> <b> <in> <b> <l> <f> <r>
	fmul		st0, st5                    ; <b*f> <b> <in> <b> <l> <f> <r>
	fxch		st1                         ; <b> <b*f> <in> <b> <l> <f> <r>
	fmul		st0, st6                    ; <b*r> <b*f> <in> <b> <l> <f> <r>
	fxch		st1                         ; <b*f> <b*r> <in> <b> <l> <f> <r>
	add     rsi, [rbp + syWFlt.step]
	faddp		st4, st0										; <b*r> <in> <b> <l'> <f> <r>
	fsubr   st0, st1                    ; <in-b*r> <in> <b> <l'> <f> <r>
	fsub    st0, st3                    ; <h> <in> <b> <l'> <f> <r>
	fmul    st0, st4                    ; <f*h> <in> <b> <l'> <f> <r>
	faddp   st2, st0                    ; <in> <b'> <l'> <f> <r>
	fld     st1                         ; <b'> <in> <b'> <l'> <f> <r>
	fld     st2                         ; <b'> <b'> <in> <b'> <l'> <f> <r>
	fmul    st0, st5                    ; <b'*f> <b'> <in> <b'> <l'> <f> <r>
	fxch		st1													; <b'> <b'*f> <in> <b'> <l'> <f> <r>
	fmul    st0, st6                    ; <b'*r> <b'*f> <in> <b'> <l'> <f> <r>
	fxch    st1                         ; <b'*f> <b'*r> <in> <b'> <l'> <f> <r>
	faddp   st4, st0                    ; <b'*r> <in> <b'> <l''> <f> <r>
	fsubp   st1, st0                    ; <in-b'*r> <b'> <l''> <f> <r>
	fsub    st0, st2                    ; <h> <b'> <l''> <f> <r>
	fld     st0                         ; <h> <h> <b'> <l''> <f> <r>
	fmul    st0, st4                    ; <h*f> <h> <b'> <l''> <f> <r>
	faddp   st2, st0                    ; <h> <b''> <l''> <f> <r>
	ret


.mode0: ; bypass
	cmp		rsi, rdi
	je    .m0end
	rep   movsd
.m0end:
	ret

.mode1: ; low
	call	.process			; <h> <b''> <l''> <f> <r>
	fstp	st0					; <b''> <l''> <f> <r>
	fxch  st1					; <l''> <b''> <f> <r>
	fst   dword [rdi]			; -> l'' stored
	fxch  st1					; <b''> <l''> <f> <r>
	add   rdi, [rbp + syWFlt.step]
	dec rcx
	jnz .mode1
	ret

.mode2: ; band
	call	.process			; <h> <b''> <l''> <f> <r>
	fstp	st0					; <b''> <l''> <f> <r>
	fst   dword [rdi]			; -> b'' stored
	add   rdi, [rbp + syWFlt.step]
	dec rcx
	jnz	.mode2
	ret

.mode3: ; high
	call	.process			; <h> <b''> <l''> <f> <r>
	fstp  dword [rdi]			; <b''> <l''> <f> <r> -> h stored
	add   rdi, [rbp + syWFlt.step]
	dec rcx
	jnz	.mode3
	ret

.mode4: ; notch
	call	.process			; <h> <b''> <l''> <f> <r>
	fadd  st2					; <h+l> <b''> <l''> <f> <r>
	fstp  dword [rdi]			; <b''> <l''> <f> <r> -> h+l'' stored
	add   rdi, [rbp + syWFlt.step]
	dec rcx
	jnz	.mode4
	ret

.mode5: ; allpass
	call	.process			; <h> <b''> <l''> <f> <r>
	fadd  st1					; <h+b> <b''> <l''> <f> <r>
	fadd  st2					; <h+b+l> <b''> <l''> <f> <r>
	fstp  dword [rdi]			; <b''> <l''> <f> <r> -> h+b''+l'' stored
	add   rdi, [rbp + syWFlt.step]
	dec rcx
	jnz	.mode5
	ret



;#####################################################################################
; Low Frequency Oscillator
;#####################################################################################

global _LFO_
_LFO_:

struc syVLFO
	.mode:    resd 1  ; 0: saw, 1: tri, 2: pulse, 3: sin, 4: s&h
	.sync:    resd 1  ; 0: free  1: in sync with keyon
	.egmode:  resd 1  ; 0: continuous  1: one-shot (EG mode)
	.rate:    resd 1  ; rate (0Hz .. ~43hz)
	.phase:   resd 1  ; start phase shift
	.pol:     resd 1  ; polarity: + , . , +/-
	.amp:     resd 1  ; amplification (0 .. 1)
endstruc

struc syWLFO
	.out:	 resd 1  ; float: out
	.mode:   resd 1  ; int: mode
	.fs:     resd 1  ; int: sync flag
	.feg:    resd 1  ; int: eg flag
	.freq:   resd 1  ; int: freq
	.cntr:   resd 1  ; int: counter
	.cph:    resd 1  ; int: counter sync phase
	.gain:   resd 1  ; float: output gain
	.dc:     resd 1  ; float: output DC
	.nseed:  resd 1  ; int: random seed
	.last:   resd 1  ; int: last counter value (for s&h transition)
endstruc


syLFOInit:
	Pushad
	xor			rax, rax
	mov			[rbp + syWLFO.cntr], rax
	mov			[rbp + syWLFO.last], rax
	rdtsc
	mov			[rbp + syWLFO.nseed], rax
	Popad
	ret


syLFOSet:
	Pushad

	fld		dword [rsi + syVLFO.mode]
	fistp   dword [rbp + syWLFO.mode]
	fld     dword [rsi + syVLFO.sync]
	fistp   dword [rbp + syWLFO.fs]
	fld     dword [rsi + syVLFO.egmode]
	fistp   dword [rbp + syWLFO.feg]

	fld     dword [rsi + syVLFO.rate]
	fmul    dword [rel fci128]
	call    calcfreq
	fmul    dword [rel fc32bit]
	fmul    dword [rel fci2]
	fistp   dword [rbp + syWLFO.freq]

	fld     dword [rsi + syVLFO.phase]
	fmul    dword [rel fci128]
	fmul    dword [rel fc32bit]
	fistp   dword [rbp + syWLFO.cph]
	shl     dword [rbp + syWLFO.cph],1

	fld     dword [rsi + syVLFO.amp]
	fld     dword [rsi + syVLFO.pol]
	fistp   dword [rel temp]
	mov     rax,  [rel temp]
	fld     st0
	fchs
	fmul    dword [rel fci2]
	cmp     al, 2       ; +/- polarity?
	jz      .isp2
	fsub    st0, st0
.isp2:
	fstp    dword [rbp + syWLFO.dc]
	cmp     al, 1       ; +/- polarity?
	jnz     .isntp1
	fchs
.isntp1:
	fstp    dword [rbp + syWLFO.gain]

	Popad
	ret


syLFOKeyOn:
	Pushad
	mov rax, [rbp + syWLFO.fs]
	or  rax, rax
	jz  .end
	mov rax, [rbp + syWLFO.cph]
	mov [rbp + syWLFO.cntr], rax
	xor rax, rax
	not rax
	mov [rbp + syWLFO.last], rax
.end:
	Popad
	ret


section .data.rel.ro

syLTTab:
	dq syLFOTick.mode0
	dq syLFOTick.mode1
	dq syLFOTick.mode2
	dq syLFOTick.mode3
	dq syLFOTick.mode4
	dq syLFOTick.mode0
	dq syLFOTick.mode0
	dq syLFOTick.mode0

section .text

syLFOTick:
	Pushad
	mov		rax, [rbp + syWLFO.cntr]
	mov		rdx, [rbp + syWLFO.mode]
	and     dl, 7
	lea     r8, [rel syLTTab]
	call    [r8 + 8*rdx]
	fmul    dword [rbp + syWLFO.gain]
	fadd    dword [rbp + syWLFO.dc]
	fstp    dword [rbp + syWLFO.out]
	mov			rax, [rbp + syWLFO.cntr]
	add			rax, [rbp + syWLFO.freq]
	jnc     .isok
	mov     rdx, [rbp + syWLFO.feg]
	or      rdx, rdx
	jz      .isok
	xor     rax, rax
	not     rax
.isok:
	mov			[rbp + syWLFO.cntr], rax
	Popad
	ret

.mode0: ; saw
	shr			rax, 9
	or      rax, 3f800000h
	mov     [rel temp], rax
	fld     dword [rel temp] ; <1..2>
	fld1                 ; <1> <1..2>
	fsubp   st1, st0     ; <0..1>
	ret

.mode1: ; tri
	shl			rax, 1
	sbb     rbx, rbx
	xor     rax, rbx
	jmp     .mode0

.mode2: ; pulse
	shl     rax, 1
	sbb     rax, rax
	jmp			.mode0


.mode3: ; sin
	call    .mode0
	fmul    dword [rel fc2pi] ; <0..2pi>
	call    fastsinrc     ; <-1..1>
	fmul    dword [rel fci2]  ; <-0.5..0.5>
	fadd    dword [rel fci2]  ; <0..1>
	ret


.mode4: ; s&h
	cmp   rax, [rbp + syWLFO.last]
	mov		[rbp + syWLFO.last], rax
	jae   .nonew
	mov   rax, [rbp + syWLFO.nseed]
	imul	rax, icnoisemul
	add		rax, icnoiseadd
	mov		[rbp + syWLFO.nseed], rax
.nonew:
	mov   rax, [rbp + syWLFO.nseed]
	jmp   .mode0




;#####################################################################################
; INDUSTRIALGELÖT UND UNKAPUTTBARE ORGELN
;  Das Verzerr- und Verüb-Modul!
;#####################################################################################


; mode1:  overdrive ...  input gain, output gain, offset
; mode2:  clip...        input gain, output gain, offset
; mode3:  bitcrusher...  input gain, crush, xor
; mode4:  decimator...   -,  resamplingfreq, -
; mode5..9:  filters     -,  cutoff, reso

global _DIST_
_DIST_:

struc syVDist
	.mode:     resd 1		; 0: off, 1: overdrive, 2: clip, 3: bitcrusher, 4: decimator
							; modes 4 to 8: filters (see syVFlt)
	.ingain:   resd 1		; -12dB ... 36dB
	.param1:   resd 1		; outgain/crush/outfreq
	.param2:   resd 1		; offset/offset/xor/jitter
endstruc

struc syWDist
	.mode:     resd 1
	.gain1:    resd 1      ; float: input gain for all fx
	.gain2:    resd 1      ; float: output gain for od/clip
	.offs:     resd 1      ; float: offs for od/clip
	.crush1:   resd 1      ; float: crush factor^-1
	.crush2:   resd 1      ; int:	 crush factor^1
	.crxor:    resd 1      ; int:   xor value for crush
	.dcount:   resd 1      ; int:   decimator counter
	.dfreq:    resd 1      ; int:   decimator frequency
	.dvall:    resd 1      ; float: last decimator value (mono/left)
	.dvalr:    resd 1      ; float: last decimator value (right)
	.dlp1c:    resd 1      ; float: decimator pre-filter coefficient
	.dlp1bl:   resd 1      ; float: decimator pre-filter buffer (mono/left)
	.dlp1br:   resd 1      ; float: decimator pre-filter buffer (right)
	.dlp2c:    resd 1      ; float: decimator post-filter coefficient
	.dlp2bl:   resd 1      ; float: decimator post-filter buffer (mono/left)
	.dlp2br:   resd 1      ; float: decimator post-filter buffer (right)
	.fw1:      resb syWFlt_size  ; left/mono filter workspace
	.fw2:      resb syWFlt_size  ; right filter workspace
endstruc


syDistInit:
	Pushad
	xor			rax, rax
	mov			[rbp + syWDist.dcount], rax
	mov			[rbp + syWDist.dvall], rax
	mov			[rbp + syWDist.dvalr], rax
	mov			[rbp + syWDist.dlp1bl], rax
	mov			[rbp + syWDist.dlp1br], rax
	mov			[rbp + syWDist.dlp2bl], rax
	mov			[rbp + syWDist.dlp2br], rax
	lea     rbp, [rbp + syWDist.fw1]
	call    syFltInit
	lea     rbp, [rbp + syWDist.fw2 - syWDist.fw1]
	call    syFltInit
	Popad
	ret

section .data.rel.ro

syDSTab:
	dq syDistSet.mode0
	dq syDistSet.mode1
	dq syDistSet.mode2
	dq syDistSet.mode3
	dq syDistSet.mode4
	dq syDistSet.modeF
	dq syDistSet.modeF
	dq syDistSet.modeF
	dq syDistSet.modeF
	dq syDistSet.modeF
	dq syDistSet.mode0
	dq syDistSet.mode0
	dq syDistSet.mode0
	dq syDistSet.mode0
	dq syDistSet.mode0
	dq syDistSet.mode0

section .text

syDistSet:
	Pushad
	fld			dword [rsi + syVDist.mode]
	fistp   dword [rbp + syWDist.mode]

	fld     dword [rsi + syVDist.ingain]
	fsub    dword [rel fc32]
	fmul    dword [rel fci16]
	call    pow2
	fstp    dword [rbp + syWDist.gain1]

	fld     dword [rsi + syVDist.param1]
	mov     rax, [rbp + syWDist.mode]
	and     al, 15
	lea     r8, [rel syDSTab]
	call    [r8 + 8*rax]
	Popad
	ret

.mode0:
	fstp st0
	ret

.mode1: ; overdrive
	fmul    dword [rel fci128]
	fld			dword [rbp + syWDist.gain1]
	fld1
	fpatan
	fdivp   st1, st0
	jmp     .mode2b

.mode2: ; clip
	fmul    dword [rel fci128]
.mode2b:
	fstp    dword [rbp + syWDist.gain2]
	fld     dword [rsi + syVDist.param2]
	fsub    dword [rel fc64]
	fmul    dword [rel fci128]
	fadd    st0, st0
	fmul    dword [rbp + syWDist.gain1]
	fstp    dword [rbp + syWDist.offs]
	ret

.mode3: ; bitcrusher
	fmul    dword [rel fc256]              ; 0 .. 32xxx
	fld1
	faddp   st1, st0                       ; 1 .. 32xxx
	fld     dword [rel fc32768]            ; 32768 x
	fxch    st1														 ; x 32768
	fdiv    st1, st0                       ; x 32768/x
	fistp   dword [rbp + syWDist.crush2]
	fmul    dword [rbp + syWDist.gain1]
	fstp    dword [rbp + syWDist.crush1]
	fld     dword [rsi + syVDist.param2]
	fistp   dword [rel temp]
	mov     rax, [rel temp]
	shl     rax, 9
	mov     [rbp + syWDist.crxor], rax
	ret

.mode4: ; decimator
	fmul    dword [rel fci128]
	call    calcfreq
	fmul    dword [rel fc32bit]
	fistp   dword [rbp + syWDist.dfreq]
	shl     dword [rbp + syWDist.dfreq], 1
	fld     dword [rsi + syVDist.ingain]
	fmul    dword [rel fci127]
	fmul    st0, st0
	fstp    dword [rbp + syWDist.dlp1c]
	fld     dword [rsi + syVDist.param2]
	fmul    dword [rel fci127]
	fmul    st0, st0
	fstp    dword [rbp + syWDist.dlp2c]
	ret

.modeF: ; filters
	lea     r8, [rel temp]
	fstp    dword [r8 + syVFlt.cutoff]
	fld     dword [rsi + syVDist.param2]
	fstp    dword [r8 + syVFlt.reso]
	mov     rax, [rbp + syWDist.mode]
	lea     rax, [rax-4]
	mov     [r8 + syVFlt.mode], rax
	fild    dword [r8 + syVFlt.mode]
	fstp    dword [r8 + syVFlt.mode]
	lea     rsi, [r8]
	lea     rbp, [rbp + syWDist.fw1]
	call    syFltSet
	lea     rbp, [rbp + syWDist.fw2 - syWDist.fw1]
	jmp     syFltSet

section .data.rel.ro

syDRMTab:
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode1
	dq syDistRenderMono.mode2
	dq syDistRenderMono.mode3
	dq syDistRenderMono.mode4
	dq syDistRenderMono.modeF
	dq syDistRenderMono.modeF
	dq syDistRenderMono.modeF
	dq syDistRenderMono.modeF
	dq syDistRenderMono.modeF
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0

section .text

; rsi : sourceptr
; rdi : destptr
; rcx : size
syDistRenderMono:
	Pushad
	V2PerfEnter V2Perf_DIST
	mov			rdx, [rbp + syWDist.mode]
	lea     r8, [rel syDRMTab]
	call    [r8 + 8*rdx]
	V2PerfLeave V2Perf_DIST
	Popad
	ret

.mode0:  ; bypass
	cmp			rsi, rdi
	je			.m0end
	rep     movsd
.m0end:
	ret

.mode1:  ; overdrive
	fld1							; <1>
	fchs							; <-1>
	xor   rbx, rbx
	mov   bl, 8
.m1loop:
	fld		dword [rsi]
	lea   rsi, [rsi+4]
	fmul  dword [rbp + syWDist.gain1]
	fadd  dword [rbp + syWDist.offs]
	lea     r8, [rel temp]
	fst   dword [r8]
	mov   ax,   [r8+2]

	call  fastatan

	fmul  dword [rbp + syWDist.gain2]
	fstp  dword [rdi]
	lea   rdi, [rdi+4]
	dec rcx
	jnz .m1loop
	fstp  st0          ; -
	ret

.mode2: ; clip
	fld		dword [rsi]
	lea   rsi, [rsi+4]
	fmul  dword [rbp + syWDist.gain1]
	fadd  dword [rbp + syWDist.offs]

	fld1
	fcomi		st0, st1
	fxch		st1
	fcmovb  st0, st1
	fxch    st1
	fchs
	fcomi   st0, st1
	fcmovb  st0, st1
	fxch    st1
	fstp    st0

	fmul  dword [rbp + syWDist.gain2]
	fstp  dword [rdi]
	lea   rdi, [rdi+4]
	dec rcx
	jnz .mode2
	ret

.mode3: ; bitcrusher
	mov   rbx, 7fffh
	mov   rdx, -7fffh
.m3loop:
	fld		dword [rsi]
	lea		rsi,	[rsi+4]
	fmul	dword [rbp + syWDist.crush1]
	fistp dword [rel temp]
	mov		rax,	[rel temp]
	imul  rax,  [rbp + syWDist.crush2]
	cmp			rbx, rax
	cmovle  rax, rbx
	cmp			rdx, rax
	cmovge  rax, rdx
	xor   rax, [rbp + syWDist.crxor]
	mov   [rel temp],	rax
	fild  dword [rel temp]
	fmul  dword [rel fci32768]
	fstp  dword [rdi]
	lea   rdi, [rdi+4]
	dec   rcx
	jnz .m3loop
	ret


.mode4: ; decimator
	mov rax, [rbp + syWDist.dvall]
	mov rdx, [rbp + syWDist.dfreq]
	mov rbx, [rbp + syWDist.dcount]
.m4loop:
	add		rbx, rdx
	cmovc rax, [rsi]
	add		rsi, byte 4
	stosd
	dec rcx
	jnz .m4loop
	mov [rbp + syWDist.dcount], rbx
	mov [rbp + syWDist.dvall], rax
	ret



.modeF: ; filters
	lea   rbp, [rbp + syWDist.fw1]
	jmp   syFltRender

section .data.rel.ro

syDRSTab:
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode1
	dq syDistRenderMono.mode2
	dq syDistRenderMono.mode3
	dq syDistRenderStereo.mode4
	dq syDistRenderStereo.modeF
	dq syDistRenderStereo.modeF
	dq syDistRenderStereo.modeF
	dq syDistRenderStereo.modeF
	dq syDistRenderStereo.modeF
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0
	dq syDistRenderMono.mode0

section .text

syDistRenderStereo:
	Pushad
	V2PerfEnter V2Perf_DIST
	shl  rcx, 1
	mov			rdx, [rbp + syWDist.mode]
	lea     r8, [rel syDRSTab]
	call    [r8 + 8*rdx]
	V2PerfLeave V2Perf_DIST
	Popad
	ret


.mode4: ; decimator
	shr rcx, 1
	mov rax, [rbp + syWDist.dvall]
	mov rdx, [rbp + syWDist.dvalr]
	mov rbx, [rbp + syWDist.dcount]
.m4loop:
	add   rbx, [rbp + syWDist.dfreq]
	cmovc rax, [rsi]
	cmovc rdx, [rsi + 4]
	add   rsi, byte 8
	stosd
	xchg  rax, rdx
	stosd
	xchg  rax, rdx
	dec   rcx
	jnz .m4loop
	mov [rbp + syWDist.dcount], rbx
	mov [rbp + syWDist.dvall],  rax
	mov [rbp + syWDist.dvalr],  rdx
	ret


.modeF: ; filters
	shr   rcx, 1
	xor   rax, rax
	mov   al, 8
	mov   [rbp + syWDist.fw1 + syWFlt.step], rax
	mov   [rbp + syWDist.fw2 + syWFlt.step], rax
	lea   rbp, [rbp + syWDist.fw1]
	call  syFltRender
	lea   rbp, [rbp + syWDist.fw2 - syWDist.fw1]
	lea   rsi, [rsi + 4]
	lea   rdi, [rdi + 4]
	jmp   syFltRender


;#####################################################################################
; V2 - Voice
;#####################################################################################


global _V2V_
_V2V_:

struc syVV2
	.panning:  resd 1              ; panning
	.transp:   resd 1              ; transpose
	.osc1:     resb syVOsc_size    ; oszi 1
	.osc2:     resb syVOsc_size    ; oszi 2
	.osc3:     resb syVOsc_size    ; oszi 3
	.vcf1:     resb syVFlt_size	   ; filter 1
	.vcf2:     resb syVFlt_size	   ; filter 2
	.routing:  resd 1              ; 0: single  1: serial  2: parallel
	.fltbal:   resd 1              ; parallel filter balance
	.dist:     resb syVDist_size   ; distorter
	.aenv:     resb syVEnv_size    ; amplitude env
	.env2:     resb syVEnv_size    ; EG 2
	.lfo1:     resb syVLFO_size    ; LFO 1
	.lfo2:     resb syVLFO_size    ; LFO 2
	.oscsync:  resd 1              ; osc keysync flag
endstruc


struc syWV2
	.note:     resd 1
	.velo:     resd 1
	.gate:     resd 1

	.curvol:   resd 1
	.volramp:  resd 1

	.xpose:    resd 1
	.fmode:    resd 1
	.lvol:     resd 1
	.rvol:     resd 1
	.f1gain:   resd 1
	.f2gain:   resd 1

	.oks:      resd 1

	.osc1:     resb syWOsc_size
	.osc2:     resb syWOsc_size
	.osc3:     resb syWOsc_size
	.vcf1:     resb syWFlt_size	  ; filter 1
	.vcf2:     resb syWFlt_size	  ; filter 2
	.aenv:     resb syWEnv_size
	.env2:     resb syWEnv_size
	.lfo1:     resb syWLFO_size    ; LFO 1
	.lfo2:     resb syWLFO_size    ; LFO 2
	.dist:     resb syWDist_size   ; distorter
endstruc


; rbp: workspace
syV2Init:
	Pushad
	lea rbp, [rbp + syWV2.osc1 - 0]
	call syOscInit
	lea rbp, [rbp + syWV2.osc2 - syWV2.osc1]
	call syOscInit
	lea rbp, [rbp + syWV2.osc3 - syWV2.osc2]
	call syOscInit
	lea rbp, [rbp + syWV2.aenv - syWV2.osc3]
	call syEnvInit
	lea rbp, [rbp + syWV2.env2 - syWV2.aenv]
	call syEnvInit
	lea rbp, [rbp + syWV2.vcf1 - syWV2.env2]
	call syFltInit
	lea rbp, [rbp + syWV2.vcf2 - syWV2.vcf1]
	call syFltInit
	lea rbp, [rbp + syWV2.lfo1 - syWV2.vcf2]
	call syLFOInit
	lea rbp, [rbp + syWV2.lfo2 - syWV2.lfo1]
	call syLFOInit
	lea rbp, [rbp + syWV2.dist - syWV2.lfo2]
	call syDistInit
	Popad
	ret


; tick
; rbp: workspace
syV2Tick:
	Pushad


	; 1. EGs
	mov		rax, [rbp + syWV2.gate]
	lea		rbp, [rbp + syWV2.aenv - 0]
	call  syEnvTick
	lea		rbp, [rbp + syWV2.env2 - syWV2.aenv]
	call  syEnvTick

	; 2. LFOs
	lea		rbp, [rbp + syWV2.lfo1 - syWV2.env2]
	call  syLFOTick
	lea		rbp, [rbp + syWV2.lfo2 - syWV2.lfo1]
	call  syLFOTick

	lea		rbp, [rbp + 0 - syWV2.lfo2]

	; 3. Volume Ramping
	fld	 dword [rbp + syWV2.aenv + syWEnv.out]
	fmul dword [rel fci128]
	fsub dword [rbp + syWV2.curvol]
	fmul dword [rel fciframe]
	fstp dword [rbp + syWV2.volramp]

	Popad
	ret


; render
; rdi: dest buf
; rcx: # of samples
; rbp: workspace
syV2Render:
	Pushad

	; clear buffer
	push rcx
	lea  rdi, [rel vcebuf]
	xor  rax, rax
	rep  stosd
	pop  rcx

	lea  rdi, [rel vcebuf]
	lea  rbp, [rbp - 0 + syWV2.osc1]
	call syOscRender
	lea  rbp, [rbp - syWV2.osc1 + syWV2.osc2]
	call syOscRender
	lea  rbp, [rbp - syWV2.osc2 + syWV2.osc3]
	call syOscRender
	lea  rbp, [rbp - syWV2.osc3 + 0]

	; Filter + Routing
	; wenn parallel -> erst filter 2 in buf2 rendern
	mov  rdx, [rbp + syWV2.fmode]
	cmp  dl, 2   ; parallel?
	jne  .nopar1
	lea  rbp, [rbp - 0 + syWV2.vcf2]
	lea  rsi, [rel vcebuf]
	lea  rdi, [rel vcebuf2]
	call syFltRender
	lea  rbp, [rbp - syWV2.vcf2 + 0]
.nopar1:
	; dann auf jeden fall filter 1 rendern
	lea  rbp, [rbp - 0 + syWV2.vcf1]
	lea  rsi, [rel vcebuf]
	lea  rdi, [rel vcebuf]
	call syFltRender
	lea  rbp, [rbp - syWV2.vcf1 + 0]
	; dann fallunterscheidung...
	or   dl, dl   ; single?
	jz	 .fltend
	cmp  dl, 2    ; parallel?
	jnz  .nopar2
	push rcx      ; ja -> buf2 auf buf adden
	lea  rsi, [rel vcebuf2]
	lea  rdi, [rel vcebuf]
	fld  dword [rbp + syWV2.f1gain] ; <g1>
	fld  dword [rbp + syWV2.f2gain] ; <g2> <g1>
.parloop:
	fld		dword [rsi]             ; <v2> <g2> <g1>
	fmul  st0, st1                  ; <v2'> <g2> <g1>
	add		rsi, byte 4
	fld 	dword [rdi]             ; <v1> <v2'> <g2> <g1>
	fmul	st0, st3                ; <v1'> <v2'> <g2> <g1>
	faddp st1, st0                  ; <out> <g2> <g1>
	fstp  dword [rdi]               ; <g2> <g1>
	add   rdi, byte 4
	dec rcx
	jnz .parloop
	fstp st0                        ; <g1>
	fstp st0                        ; -
	pop rcx
	jmp .fltend
.nopar2:
	; ... also seriell ... filter 2 drüberrechnen
	lea  rbp, [rbp - 0 + syWV2.vcf2]
	lea  rsi, [rel vcebuf]
	lea  rdi, [rel vcebuf]
	call syFltRender
	lea  rbp, [rbp - syWV2.vcf2 + 0]

.fltend:

	; distortion
	lea  rsi, [rel vcebuf]
	lea  rdi, [rel vcebuf]
	lea  rbp, [rbp - 0 + syWV2.dist]
	call syDistRenderMono
	lea  rbp, [rbp - syWV2.dist + 0]

	; vcebuf (mono) nach chanbuf(stereo) kopieren
	lea	 rdi, [rel chanbuf]
	lea  rsi, [rel vcebuf]
	fld  dword [rbp + syWV2.curvol] ; cv
.copyloop1:
	fld		dword [rsi]		; out cv
	fmul  st1						; out' cv
	fxch  st1						; cv out'
	fadd  dword [rbp + syWV2.volramp] ; cv' out'
	fxch  st1						; out' cv'
	fld		st0						; out out cv
	fmul  dword [rbp + syWV2.lvol] ; outl out cv
	fxch  st1						; out outl cv
	fmul  dword [rbp + syWV2.rvol] ; outr outl cv
	fxch  st1						; outl outr cv
%ifdef FIXDENORMALS
	fadd  dword [rel dcoffset]
	fxch  st1
	fadd  dword [rel dcoffset]
	fxch  st1
%endif
	fadd  dword [rdi]		; l outr cv
	fxch  st1						; outr l cv
	fadd  dword [rdi+4]	; r l cv
	fxch	st1						; l r cv
	fstp dword [rdi]		; r cv
	fstp dword [rdi+4]	; cv
	add rsi, 4
	add rdi, 8
	dec rcx
	jnz .copyloop1
	fstp	dword [rbp + syWV2.curvol] ; -

	Popad
	ret



; set
; rsi: values
; rbp: workspace
syV2Set:
	Pushad

	fld     dword [rsi + syVV2.transp]
	fsub    dword [rel fc64]
	fst     dword [rbp + syWV2.xpose]

	fiadd	  dword [rbp + syWV2.note]
	fst		dword [rbp + syWV2.osc1 + syWOsc.note]
	fst		dword [rbp + syWV2.osc2 + syWOsc.note]
	fstp  dword [rbp + syWV2.osc3 + syWOsc.note]

	fld			dword [rsi + syVV2.routing]
	fistp		dword [rbp + syWV2.fmode]

	fld			dword [rsi + syVV2.oscsync]
	fistp		dword [rbp + syWV2.oks]

	; ... denn EQP - Panning rult.
	fld			dword [rsi + syVV2.panning] ; <p>
	fmul		dword [rel fci128]              ; <p'>
	fld			st0                         ; <p'> <p'>
	fld1			                          ; <1> <p'> <p'>
	fsubrp	st1, st0                    ; <1-p'> <p'>
	fsqrt																; <lv> <p'>
	fstp    dword [rbp + syWV2.lvol]    ; <p'>
	fsqrt																; <rv>
	fstp    dword [rbp + syWV2.rvol]

	; filter balance für parallel
	fld     dword [rsi + syVV2.fltbal]
	fsub    dword [rel fc64]
	fist    dword [rel temp]
	mov     rax, [rel temp]
	fmul    dword [rel fci64]         ; <x>
	or      rax, rax
	js      .fbmin
	fld1                              ; <1> <x>
	fsubr   st1, st0                  ; <1> <1-x>
	jmp     short .fbgoon
.fbmin:
	fld1                              ; <1> <x>
	fadd    st1, st0                  ; <g1> <g2>
	fxch    st1                       ; <g2> <g1>
.fbgoon:
	fstp    dword [rbp + syWV2.f2gain] ; <g1>
	fstp    dword [rbp + syWV2.f1gain] ; -


	lea			rbp, [rbp + syWV2.osc1 - 0]
	lea			rsi, [rsi + syVV2.osc1 - 0]
	call		syOscSet
	lea			rbp, [rbp + syWV2.osc2 - syWV2.osc1]
	lea			rsi, [rsi + syVV2.osc2 - syVV2.osc1]
	call		syOscSet
	lea			rbp, [rbp + syWV2.osc3 - syWV2.osc2]
	lea			rsi, [rsi + syVV2.osc3 - syVV2.osc2]
	call		syOscSet
	lea			rbp, [rbp + syWV2.aenv - syWV2.osc3]
	lea			rsi, [rsi + syVV2.aenv - syVV2.osc3]
	call		syEnvSet
	lea			rbp, [rbp + syWV2.env2 - syWV2.aenv]
	lea			rsi, [rsi + syVV2.env2 - syVV2.aenv]
	call		syEnvSet
	lea			rbp, [rbp + syWV2.vcf1 - syWV2.env2]
	lea			rsi, [rsi + syVV2.vcf1 - syVV2.env2]
	call		syFltSet
	lea			rbp, [rbp + syWV2.vcf2 - syWV2.vcf1]
	lea			rsi, [rsi + syVV2.vcf2 - syVV2.vcf1]
	call		syFltSet
	lea			rbp, [rbp + syWV2.lfo1 - syWV2.vcf2]
	lea			rsi, [rsi + syVV2.lfo1 - syVV2.vcf2]
	call		syLFOSet
	lea			rbp, [rbp + syWV2.lfo2 - syWV2.lfo1]
	lea			rsi, [rsi + syVV2.lfo2 - syVV2.lfo1]
	call		syLFOSet
	lea			rbp, [rbp + syWV2.dist - syWV2.lfo2]
	lea			rsi, [rsi + syVV2.dist - syVV2.lfo2]
	call		syDistSet
	Popad
	ret


; note on
; rax: note
; rbx: vel
; rbp: workspace
syV2NoteOn:
	Pushad
	mov		[rbp + syWV2.note], rax
	fild	dword [rbp + syWV2.note]
	fadd  dword [rbp + syWV2.xpose]

	fst		dword [rbp + syWV2.osc1 + syWOsc.note]
	fst		dword [rbp + syWV2.osc2 + syWOsc.note]
	fstp  dword [rbp + syWV2.osc3 + syWOsc.note]
	mov   [rel temp], rbx
	fild	dword [rel temp]
	fstp  dword [rbp + syWV2.velo]
	xor rax, rax
	inc rax
	mov [rbp + syWV2.gate], rax
	; reset EGs
	mov [rbp + syWV2.aenv + syWEnv.state], rax
	mov [rbp + syWV2.env2 + syWEnv.state], rax

	mov rax, [rbp + syWV2.oks]
	or	rax, rax
	jz .nosync

	xor rax, rax
	mov [rbp + syWV2.osc1 + syWOsc.cnt], rax
	mov [rbp + syWV2.osc2 + syWOsc.cnt], rax
	mov [rbp + syWV2.osc3 + syWOsc.cnt], rax

.nosync:

	;fldz
;	fst  dword [rbp + syWV2.curvol]
	;fstp dword [rbp + syWV2.volramp]

	lea  rbp, [rbp + syWV2.osc1 - 0]
	call syOscChgPitch
	lea  rbp, [rbp + syWV2.osc2 - syWV2.osc1]
	call syOscChgPitch
	lea  rbp, [rbp + syWV2.osc3 - syWV2.osc2]
	call syOscChgPitch

	lea  rbp, [rbp + syWV2.lfo1 - syWV2.osc3]
	call syLFOKeyOn
	lea  rbp, [rbp + syWV2.lfo2 - syWV2.lfo1]
	call syLFOKeyOn


	Popad
	ret


; note off
; rbp: workspace
syV2NoteOff:
	Pushad
	xor rax, rax
	mov [rbp + syWV2.gate], rax
	Popad
	ret

section .data.rel.ro
; table for mod sources
sVTab:
	dd	syWV2.aenv + syWEnv.out
	dd syWV2.env2 + syWEnv.out
	dd syWV2.lfo1 + syWLFO.out
	dd syWV2.lfo2 + syWLFO.out

section .text

storeV2Values:
	Pushad

	lea   r8, [rel data]
	mov   rbx, [r8 + SYN.chanmap + 4*rdx]    ; rbx = channel
	or    rbx, rbx
	jns   .doit
	jmp   .end      ; voice gar ned belegt?
.doit:
	movzx rax, byte [r8 + SYN.chans + 8*rbx] ; pgmnummer
	mov   rdi, [r8 + SYN.patchmap]
	mov   rdi, [rdi + 4*rax]				       ; rdi -> sounddaten
	add   rdi, [r8 + SYN.patchmap]

	mov   rax, rdx
	imul  rax, syVV2_size
	lea   rsi, [r8 + SYN.voicesv + rax]      ; rsi -> values
	mov   rax, rdx
	imul  rax, syWV2_size
	lea   rbp, [r8 + SYN.voicesw + rax]      ; rbp -> workspace

	; voicedependent daten übertragen
	xor   rcx, rcx
.goloop:
	movzx rax, byte [rdi + rcx]
	mov   [rel temp], rax
	fild  dword [rel temp]
	fstp  dword [rsi + 4*rcx]
	inc   rcx
	cmp   cl, v2sound.endvdvals
	jne .goloop

	; MODMATRIX!
	movzx rcx, byte [rdi + v2sound.modnum]
	lea		rdi, [rdi + v2sound.modmatrix]
	or	rcx, rcx
	jnz  .modloop
	jmp  .modend

.modloop:
	movzx		rax, byte [rdi + v2mod.source]  ; source
	or      rax, rax
	jnz     .mnotvel
	fld     dword [rbp + syWV2.velo]
	jmp			.mdo
.mnotvel:
	cmp     al, 8
	jae     .mnotctl
	lea     r8, [rel data]
	mov     r9, rax
	add     r9, r8
	movzx   rax, byte [r9 + SYN.chans + 8*rbx]
	mov     [rel temp], rax
	fild    dword [rel temp]
	jmp     .mdo
.mnotctl:
	cmp     al, 12
	jae     .mnotvm
	and     al, 3
	lea     r8, [rel sVTab]
	mov     rax, [r8 + 4*rax]
	fld     dword [rbp + rax]
	jmp     .mdo
.mnotvm:
	cmp     al, 13
	jne     .mnotnote
.mnotnote:
	fild    dword [rbp + syWV2.note]
	fsub    dword [rel fc48]
	fadd    st0, st0
	jmp     .mdo
.mdo:
	movzx   rax, byte [rdi + v2mod.val]
	mov     [rel temp], rax
	fild    dword [rel temp]
	fsub    dword [rel fc64]
	fmul    dword [rel fci128]
	fadd    st0, st0
	fmulp   st1, st0
	movzx   rax, byte [rdi + v2mod.dest]
	cmp     rax, v2sound.endvdvals
	jb     .misok
	fstp   st0
	jmp    .mlend
.misok:
	fadd    dword [rsi + 4*rax]
	fstp    dword [rel temp]
	; clippen
	mov     rdx, [rel temp]
	or      rdx, rdx
	jns     .mnoclip1
	xor     rdx, rdx
.mnoclip1:
	cmp     rdx, 43000000h
	jbe     .mnoclip2
	mov     rdx, 43000000h
.mnoclip2:
	mov			[rsi + 4*rax], rdx
.mlend:
	lea rdi, [rdi+3]
	dec rcx
	jz .modend
	jmp .modloop
.modend:

	call syV2Set

.end:
	Popad
	ret



;#####################################################################################
; Bass, Bass, wir brauchen Bass
; BASS BOOST (fixed low shelving EQ)
;#####################################################################################

global _BASS_
_BASS_:

struc syVBoost
	.amount:   resd 1    ; boost in dB (0..18)
endstruc

struc syWBoost
	.ena:      resd 1
	.a1:       resd 1
	.a2:       resd 1
	.b0:       resd 1
	.b1:       resd 1
	.b2:       resd 1
	.x1:       resd 2
	.x2:       resd 2
	.y1:       resd 2
	.y2:       resd 2
endstruc

syBoostInit:
	Pushad
	Popad
	ret



; fixed frequency: 150Hz -> omega is 0,0213713785958489335949839685937381

syBoostSet:
	Pushad

	fld       dword [rsi + syVBoost.amount]
	fist      dword [rel temp]
	mov       rax, [rel temp]
	mov       [rbp + syWBoost.ena], rax
	or        rax, rax
	jnz       .isena
	fstp      st0
	Popad
	ret

.isena:
	;    A  = 10^(dBgain/40) bzw ne stark gefakete version davon
	fmul      dword [rel fci128]
	call      pow2                           ; <A>

	;  beta  = sqrt[ (A^2 + 1)/S - (A-1)^2 ]    (for shelving EQ filters only)
	fld       st0                            ; <A> <A>
	fmul      st0, st0                       ; <A²> <A>
	fld1                                     ; <1> <A²> <A>
	faddp     st1, st0                       ; <A²+1> <A>
	fld       st1                            ; <A> <A²+1> <A>
	fld1                                     ; <1> <A> <A²+1> <A>
	fsubp     st1, st0                       ; <A-1> <A²+1> <A>
	fmul      st0, st0                       ; <(A-1)²> <A²+1> <A>
	fsubp     st1, st0                       ; <beta²> <A>
	fsqrt                                    ; <beta> <A>

	; zwischenvars: beta*sin, A+1, A-1, A+1*cos, A-1*cos
	fmul      dword [rel SRfcBoostSin]       ; <bs> <A>
	fld1                                     ; <1> <bs> <A>
	fld       st2                            ; <A> <1> <bs> <A>
	fld       st0                            ; <A> <A> <1> <bs> <A>
	fsub      st0, st2                       ; <A-> <A> <1> <bs> <A>
	fxch      st1                            ; <A> <A-> <1> <bs> <A>
	faddp     st2, st0                       ; <A-> <A+> <bs> <A>
	fxch      st1                            ; <A+> <A-> <bs> <A>
	fld       st0                            ; <A+> <A+> <A-> <bs> <A>
	fmul      dword [rel SRfcBoostCos]       ; <cA+> <A+> <A-> <bs> <A>
	fld       st2                            ; <A-> <cA+> <A+> <A-> <bs> <A>
	fmul      dword [rel SRfcBoostCos]       ; <cA-> <cA+> <A+> <A-> <bs> <A>

	;     a0 =        (A+1) + (A-1)*cos + beta*sin
	fld       st4                            ; <bs> <cA-> <cA+> <A+> <A-> <bs> <A>
	fadd      st0, st1                       ; <bs+cA-> <cA-> <cA+> <A+> <A-> <bs> <A>
	fadd      st0, st3                       ; <a0> <cA-> <cA+> <A+> <A-> <bs> <A>

	; zwischenvar: 1/a0
	fld1                                     ; <1> <a0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fdivrp    st1, st0                       ; <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>

	;     b1 =  2*A*[ (A-1) - (A+1)*cos
	fld       st4                            ; <A-> <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fsub      st0, st3                       ; <A- - cA+> <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fadd      st0, st0                       ; <2(A- - cA+)> <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fmul      st0, st7                       ; <b1> <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fmul      st0, st1                       ; <b1'> <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>
	fstp      dword [rbp + syWBoost.b1]      ; <ia0> <cA-> <cA+> <A+> <A-> <bs> <A>


	;     a1 =   -2*[ (A-1) + (A+1)*cos
	fxch      st4                            ; <A-> <cA-> <cA+> <A+> <ia0> <bs> <A>
	faddp     st2, st0                       ; <cA-> <cA+ + A-> <A+> <ia0> <bs> <A>
	fxch      st1                            ; <cA+ + A-> <cA-> <A+> <ia0> <bs> <A>
	fadd      st0, st0                       ; <2*(cA+ + A-)> <cA-> <A+> <ia0> <bs> <A>
	fchs                                     ; <a1> <cA-> <A+> <ia0> <bs> <A>
	fmul      st0, st3                       ; <a1'> <cA-> <A+> <ia0> <bs> <A>
	fstp      dword [rbp + syWBoost.a1]      ; <cA-> <A+> <ia0> <bs> <A>


	;     a2 =        (A+1) + (A-1)*cos - beta*sin
	fld       st0                            ; <cA-> <cA-> <A+> <ia0> <bs> <A>
	fadd      st0, st2                       ; <A+ + cA-> <cA-> <A+> <ia0> <bs> <A>
	fsub      st0, st4                       ; <a2> <cA-> <A+> <ia0> <bs> <A>
	fmul      st0, st3                       ; <a2'> <cA-> <A+> <ia0> <bs> <A>
	fstp      dword [rbp + syWBoost.a2]      ; <cA-> <A+> <ia0> <bs> <A>

	;     b0 =    A*[ (A+1) - (A-1)*cos + beta*sin ]
	fsubp     st1, st0                       ; <A+ - cA-> <ia0> <bs> <A>
	fxch      st1                            ; <ia0> <A+ - cA-> <bs> <A>
	fmulp     st3, st0                       ; <A+ - cA-> <bs> <A*ia0>
	fld       st0                            ; <A+ - cA-> <A+ - cA-> <bs> <A*ia0>
	fadd      st0, st2                       ; <A+ - ca- + bs> <A+ - cA-> <bs> <A*ia0>
	fmul      st0, st3                       ; <b0'> <A+ - cA-> <bs> <A*ia0>
	fstp      dword [rbp + syWBoost.b0]      ; <A+ - cA-> <bs> <A*ia0>

	;     b2 =    A*[ (A+1) - (A-1)*cos - beta*sin ]
	fsubrp    st1, st0                       ; <A+ - cA- - bs> <A*ia0>
	fmulp     st1, st0                       ; <b2'>
	fstp      dword [rbp + syWBoost.b2]      ; -

	Popad
	ret

; rsi: src/dest buffer
; rcx: # of samples

; y[n] = (b0/a0)*x[n] + (b1/a0)*x[n-1] + (b2/a0)*x[n-2] - (a1/a0)*y[n-1] - (a2/a0)*y[n-2]
syBoostProcChan:                        ; <y2> <x2> <y1> <x2>
	Pushad
.doloop:
	; y0 = b0'*in + b1'*x1 + b2'*x2 + a1'*y1 + a2'*y2
	fmul    dword [rbp + syWBoost.a2]   ; <y2a2> <x2> <y1> <x1>
	fxch    st1                         ; <x2> <y2a2> <y1> <x1>
	fmul    dword [rbp + syWBoost.b2]   ; <x2b2> <y2a2> <y1> <x1>
	fld     st3                         ; <x1> <x2b2> <y2a2> <y1> <x1>
	fmul    dword [rbp + syWBoost.b1]   ; <x1b1> <x2b2> <y2a2> <y1> <x1>
	fld     st3                         ; <y1> <x1b1> <x2b2> <y2a2> <y1> <x1>
	fmul    dword [rbp + syWBoost.a1]   ; <y1a1> <x1b1> <x2b2> <y2a2> <y1> <x1>
	fxch    st3                         ; <y2a2> <x1b1> <x2b2> <y1a1> <y1> <x1>
	fsubp   st2, st0                    ; <x1b1> <x2b2-y2a2> <y1a1> <y1> <x1>
	fsubrp  st2, st0                    ; <x2b2-y2a2> <x1b1-y1a1> <y1> <x1>
	fld     dword [rsi]                 ; <x0> <x2b2-y2a2> <x1b1-y1a1> <y1> <x1>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	fxch    st4                         ; <x1> <x2b2-y2a2> <x1b1-y1a1> <y1> <x0>
	fld     st4                         ; <x0> <x1> <x2b2-y2a2> <x1b1-y1a1> <y1> <x0>
	fmul    dword [rbp + syWBoost.b0]   ; <b0x0> <x1> <x2b2-y2a2> <x1b1-y1a1> <y1> <x0>
	fxch    st2                         ; <x2b2-y2a2> <x1> <b0x0> <x1b1-y1a1> <y1> <x0>
	faddp   st3, st0                    ; <x1> <b0x0> <x1b1-y1a1+x2b2-y2a2> <y1> <x0>
	fxch    st2                         ; <x1b1-y1a1+x2b2-y2a2> <b0x0> <x1> <y1> <x0>
	faddp   st1, st0                    ; <y0> <x1> <y1> <x0>
	fst     dword [rsi]
	fxch    st2                         ; <y1> <x1> <y0> <x0>
	lea     rsi, [rsi+8]                ; ... = <y2'> <x2'> <y1'> <x1'>
	dec     rcx
	jnz     .doloop
	Popad
	ret

syBoostRender:
	Pushad
	V2PerfEnter V2Perf_BASS

	test    byte [rbp + syWBoost.ena], 255
	jz      .nooo

	; left channel
	fld     dword [rbp + syWBoost.x1]     ; <x1>
	fld     dword [rbp + syWBoost.y1]     ; <y1> <x1>
	fld     dword [rbp + syWBoost.x2]     ; <x2> <y1> <x1>
	fld     dword [rbp + syWBoost.y2]     ; <y2> <x2> <y1> <x1>
	call    syBoostProcChan
	fstp    dword [rbp + syWBoost.y2]     ; <x2'> <y1> <x1>
	fstp    dword [rbp + syWBoost.x2]     ; <y1> <x1>
	fstp    dword [rbp + syWBoost.y1]     ; <x1>
	fstp    dword [rbp + syWBoost.x1]     ; -

	lea     rsi,  [rsi+4]

	; right channel
	fld     dword [rbp + syWBoost.x1 + 4] ; <x1>
	fld     dword [rbp + syWBoost.y1 + 4] ; <y1> <x1>
	fld     dword [rbp + syWBoost.x2 + 4] ; <x2> <y1> <x1>
	fld     dword [rbp + syWBoost.y2 + 4] ; <y2> <x2> <y1> <x1>
	call    syBoostProcChan
	fstp    dword [rbp + syWBoost.y2 + 4] ; <x2'> <y1> <x1>
	fstp    dword [rbp + syWBoost.x2 + 4] ; <y1> <x1>
	fstp    dword [rbp + syWBoost.y1 + 4] ; <x1>
	fstp    dword [rbp + syWBoost.x1 + 4] ; -

.nooo:
	V2PerfLeave V2Perf_BASS
	Popad
	ret




;#####################################################################################
; Böse Dinge, die man mit Jan Delay anstellen kann:
;  MODULATING DELAYTEIL
; (für chorus, flanger, aber auch als "großes" stereo delay. Mit Liebe verpackt, ganz für sie.)
;#####################################################################################

global _MODDEL_
_MODDEL_:

struc syVModDel
	.amount:   resd 1    ; dry/eff value (0=-eff, 64=dry, 127=eff)
	.fb:       resd 1    ; feedback (0=-100%, 64=0%, 127=~+100%)
	.llength:  resd 1    ; length of left delay
	.rlength:  resd 1    ; length of right delay
	.mrate:    resd 1    ; modulation rate
	.mdepth:   resd 1    ; modulation depth
	.mphase:   resd 1    ; modulation stereo phase (0=-180°, 64=0°, 127=180°)
endstruc

struc syWModDel
	.db1:      resd 1    ; ptr: delay buffer 1
	.db2:      resd 1    ; ptr: delay buffer 2
	.dbufmask: resd 1    ; int: delay buffer mask

	.dbptr:    resd 1    ; int: buffer write pos
	.db1offs:  resd 1    ; int: buffer 1 offset
	.db2offs:  resd 1    ; int: buffer 2 offset
	.mcnt:     resd 1    ; mod counter
	.mfreq:    resd 1    ; mod freq
	.mphase:   resd 1    ; mod phase
	.mmaxoffs: resd 1    ; mod max offs (2048samples*depth)

	.fbval:    resd 1    ; float: feedback val
	.dryout:   resd 1    ; float: dry out
	.effout:   resd 1    ; float: eff out
endstruc


syModDelInit:
	Pushad
	xor rax, rax
	mov [rbp + syWModDel.dbptr],rax
	mov [rbp + syWModDel.mcnt],rax
	mov rsi, [rbp + syWModDel.db1]
	mov rdi, [rbp + syWModDel.db2]
	mov rcx, [rbp + syWModDel.dbufmask]
.clloop:
	stosd
	mov	[rsi+4*rcx],rax
	dec	rcx
	jns	.clloop
	Popad
	ret

syModDelSet:
	Pushad
	fld			dword [rsi + syVModDel.amount]
	fsub		dword [rel fc64]
	fmul    dword [rel fci128]
	fadd    st0, st0
	fst     dword [rbp + syWModDel.effout]
	fabs
	fld1
	fsubrp  st1, st0
	fstp    dword [rbp + syWModDel.dryout]
	fld     dword [rsi + syVModDel.fb]
	fsub		dword [rel fc64]
	fmul    dword [rel fci128]
	fadd    st0, st0
	fstp    dword [rbp + syWModDel.fbval]

	fild    dword [rbp + syWModDel.dbufmask]
	fsub    dword [rel fc1023]
	fmul    dword [rel fci128]
	fld     dword [rsi + syVModDel.llength]
	fmul    st0, st1
	fistp   dword [rbp + syWModDel.db1offs]
	fld     dword [rsi + syVModDel.rlength]
	fmulp   st1, st0
	fistp   dword [rbp + syWModDel.db2offs]

	fld     dword [rsi + syVModDel.mrate]
	fmul    dword [rel fci128]
	call    calcfreq
	fmul    dword [rel fcmdlfomul]
	fmul    dword [rel SRfclinfreq]
	fistp   dword [rbp + syWModDel.mfreq]

	fld     dword [rsi + syVModDel.mdepth]
	fmul    dword [rel fci128]
	fmul    dword [rel fc1023]
	fistp   dword [rbp + syWModDel.mmaxoffs]

	fld     dword [rsi + syVModDel.mphase]
	fsub	dword [rel fc64]
	fmul    dword [rel fci128]
	fmul    dword [rel fc32bit]
	fistp   dword [rbp + syWModDel.mphase]
	shl     dword [rbp + syWModDel.mphase], 1

	Popad
	ret

syModDelProcessSample:
; fpu: <r> <l> <eff> <dry> <fb>
; rdx: buffer index

	push    rdx

	; step 1: rechtes dingens holen
	mov     rax, [rbp + syWModDel.mcnt]
	add     rax, [rbp + syWModDel.mphase]
	shl     rax, 1
	sbb     rbx, rbx
	xor     rax, rbx
	mov     rbx, [rbp + syWModDel.mmaxoffs]
	mul     rbx
	add     rdx, [rbp + syWModDel.db2offs]
	mov     rbx, [rsp]
	sub     rbx, rdx
	dec     rbx
	and     rbx, [rbp + syWModDel.dbufmask]
	shr     rax, 9
	or      rax, 3f800000h
	mov     [rel temp], rax
	fld     dword [rel temp] ; <1..2> <r> <l> <eff> <dry> <fb>
	fsubr   dword [rel fc2]  ; <x> <r> <l> <eff> <dry> <fb>
	mov     rax,  [rbp + syWModDel.db2]
	fld     dword [rax + 4*rbx] ; <in1> <x> <r> <l> <eff> <dry> <fb>
	inc     rbx
	and     rbx, [rbp + syWModDel.dbufmask]
	fld     dword [rax + 4*rbx] ; <in2> <in1> <x> <r> <l> <eff> <dry> <fb>
	fsub    st0, st1            ; <in2-in1> <in1> <x> <r> <l> <eff> <dry> <fb>
	mov     rbx, [rsp]
	fmulp   st2, st0            ; <in1> <x*(in2-in1)> <r> <l> <eff> <dry> <fb>
	faddp   st1, st0            ; <in> <r> <l> <eff> <dry> <fb>
	fld     st1                 ; <r> <in> <r> <l> <eff> <dry> <fb>
	fmul    st0, st5            ; <r*dry> <in> <r> <l> <eff> <dry> <fb>
	fld     st1                 ; <in> <r*dry> <in> <r> <l> <eff> <dry> <fb>
	fmul    st0, st5            ; <in*eff> <r*dry> <in> <r> <l> <eff> <dry> <fb>
	fxch    st2                 ; <in> <in*eff> <r*dry> <r> <l> <eff> <dry> <fb>
	fmul    st0, st7            ; <in*fb> <in*eff> <r*dry> <r> <l> <eff> <dry> <fb>
	fxch    st1                 ; <in*eff> <in*fb> <r*dry> <r> <l> <eff> <dry> <fb>
	faddp   st2, st0            ; <in*fb> <r'> <r> <l> <eff> <dry> <fb>
	faddp   st2, st0            ; <r'> <rb> <l> <eff> <dry> <fb>
	fxch    st1                 ; <rb> <r'> <l> <eff> <dry> <fb>
	fstp    dword [rax+4*rbx]   ; <r'> <l> <eff> <dry> <fb>
	fxch    st1                 ; <l> <r'> <eff> <dry> <fb>

	; step 2: linkes dingens holen
	mov     rax, [rbp + syWModDel.mcnt]
	shl     rax, 1
	sbb     rbx, rbx
	xor     rax, rbx
	mov     rbx, [rbp + syWModDel.mmaxoffs]
	mul     rbx
	add     rdx, [rbp + syWModDel.db1offs]
	mov     rbx, [rsp]
	sub     rbx, rdx
	and     rbx, [rbp + syWModDel.dbufmask]
	shr     rax, 9
	or      rax, 3f800000h
	mov     [rel temp], rax
	fld     dword [rel temp] ; <1..2> <l> <r'> <eff> <dry> <fb>
	fsubr   dword [rel fc2]  ; <x> <l> <r'> <eff> <dry> <fb>
	mov     rax,  [rbp + syWModDel.db1]
	fld     dword [rax + 4*rbx] ; <in1> <x> <l> <r'> <eff> <dry> <fb>
	inc     rbx
	and     rbx, [rbp + syWModDel.dbufmask]
	fld     dword [rax + 4*rbx] ; <in2> <in1> <x> <l> <r'> <eff> <dry> <fb>
	fsub    st0, st1            ; <in2-in1> <in1> <x> <l> <r'> <eff> <dry> <fb>
	mov     rbx, [rsp]
	fmulp   st2, st0            ; <in1> <x*(in2-in1)> <l> <r'> <eff> <dry> <fb>
	faddp   st1, st0            ; <in> <l> <r'> <eff> <dry> <fb>
	fld     st1                 ; <l> <in> <l> <r'> <eff> <dry> <fb>
	fmul    st0, st5            ; <l*dry> <in> <l> <r'> <eff> <dry> <fb>
	fld     st1                 ; <in> <l*dry> <in> <l> <r'> <eff> <dry> <fb>
	fmul    st0, st5            ; <in*eff> <l*dry> <in> <l> <r'> <eff> <dry> <fb>
	fxch    st2                 ; <in> <in*eff> <l*dry> <l> <r'> <eff> <dry> <fb>
	fmul    st0, st7            ; <in*fb> <in*eff> <l*dry> <l> <r'> <eff> <dry> <fb>
	fxch    st1                 ; <in*eff> <in*fb> <l*dry> <l> <r'> <eff> <dry> <fb>
	faddp   st2, st0            ; <in*fb> <l'> <l> <r'> <eff> <dry> <fb>
	faddp   st2, st0            ; <l'> <lb> <r'> <eff> <dry> <fb>
	fxch    st1                 ; <lb> <l'> <r'> <eff> <dry> <fb>
	fstp    dword [rax+4*rbx]   ; <l'> <r'> <eff> <dry> <fb>

	pop     rdx
	mov     rax, [rbp + syWModDel.mfreq]
	add     [rbp + syWModDel.mcnt], rax
	inc     rdx
	and     rdx,  [rbp + syWModDel.dbufmask]
	ret


syModDelRenderAux2Main:
	Pushad
	V2PerfEnter V2Perf_MODDEL

	mov     rax, [rbp + syWModDel.effout]
	or      rax, rax
	jz      .dont

	fld			dword [rbp + syWModDel.fbval]					;  <fb>
	fldz                                          ;  <"dry"=0> <fb>
	fld     dword [rbp + syWModDel.effout]				;  <eff> <dry> <fb>
	mov     rdx,	[rbp + syWModDel.dbptr]
	lea     rsi,  [rel aux2buf]
.rloop:
	fld     dword [rsi]						;  <m> <eff> <dry> <fb>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	fld     st0                   ;  <m> <m> <eff> <dry> <fb>
	lea     rsi, [rsi+4]
	call    syModDelProcessSample ;  <l'> <r'> <eff> <dry> <fb>
	fadd    dword [rdi]           ;  <lout> <r'> <eff> <dry> <fb>
	fstp    dword [rdi]           ;  <r'> <eff> <dry> <fb>
	fadd    dword [rdi+4]         ;  <rout> <eff> <dry> <fb>
	fstp    dword [rdi+4]         ;  <eff> <dry> <fb>
	lea     rdi, [rdi+8]

	dec     rcx
	jnz .rloop
	mov			[rbp + syWModDel.dbptr], rdx
	fstp    st0			                ; <dry> <fb>
	fstp    st0                     ; <fb>
	fstp    st0                     ; -

.dont:
	V2PerfLeave V2Perf_MODDEL
	Popad
	ret

syModDelRenderChan:
	Pushad
	V2PerfEnter V2Perf_MODDEL

	mov     rax, [rbp + syWModDel.effout]
	or      rax, rax
	jz      .dont

	fld			dword [rbp + syWModDel.fbval]					;  <fb>
	fld     dword [rbp + syWModDel.dryout]				;  <dry> <fb>
	fld     dword [rbp + syWModDel.effout]				;  <eff> <dry> <fb>
	mov     rdx,	[rbp + syWModDel.dbptr]
	lea     rsi,  [rel chanbuf]
.rloop:
	fld     dword [rsi]						;  <l> <eff> <dry> <fb>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	fld     dword [rsi+4]					;  <r> <l> <eff> <dry> <fb>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	call    syModDelProcessSample ;  <l'> <r'> <eff> <dry> <fb>
	fstp    dword [rsi]           ;  <r'> <eff> <dry> <fb>
	fstp    dword [rsi+4]         ;  <eff> <dry> <fb>
	lea     rsi, [rsi+8]
	dec     rcx
	jnz .rloop
	mov			[rbp + syWModDel.dbptr], rdx
	fstp    st0			                ; <dry> <fb>
	fstp    st0                     ; <fb>
	fstp    st0                     ; -

.dont:
	V2PerfLeave V2Perf_MODDEL
	Popad
	ret




;#####################################################################################
; Für Ronny und die Freunde des Lauten:
;  STEREO COMPRESSOR
;#####################################################################################

global _COMPRESSOR_
_COMPRESSOR_:

%define  COMPDLEN 5700

;PLAN:

; 1. Pegeldetection
; - entweder Peak mit fixem Falloff...
; - oder RMS über einen 8192-Samples-Buffer
; MODE: stereo/mono
; Zukunft: sidechain? low/highcut dafür?

; 2. Lookahead:
; - Delayline fürs Signal, Länge einstellbar (127msecs)

; 3. Pegelangleicher
; Werte: Threshold, Ratio (1:1 ... 1:inf), Attack (0..?), Decay (0..?)
; Zukunft:  Transientdingens (releasetime-anpassung)? Enhancer (high shelving EQ mit boost 1/reduction)?
;           Knee? (ATAN!)

struc syVComp
	.mode:         resd 1  ; 0: off / 1: Peak / 2: RMS
	.stereo:       resd 1  ; 0: mono / 1: stereo
	.autogain:     resd 1  ; 0: off / 1: on
	.lookahead:    resd 1  ; lookahead in ms
	.threshold:    resd 1  ; threshold (-54dB .. 6dB ?)
	.ratio:        resd 1  ; ratio (0 : 1:1 ... 127: 1:inf)
	.attack:       resd 1  ; attack value
	.release:      resd 1  ; release value
	.outgain:      resd 1  ; output gain
endstruc

struc syWComp
	.mode:         resd 1  ; int: mode (bit0: peak/rms, bit1: stereo, bit2: off)
	.oldmode:      resd 1  ; int: last mode

	.invol:        resd 1  ; flt: input gain (1/threshold, internal threshold is always 0dB)
	.ratio:        resd 1  ; flt: ratio
	.outvol:       resd 1  ; flt: output gain (outgain * threshold)
	.attack:       resd 1  ; flt: attack   (lpf coeff, 0..1)
	.release:      resd 1  ; flt: release  (lpf coeff, 0..1)

	.dblen:        resd 1  ; int: lookahead buffer length
	.dbcnt:        resd 1  ; int: lookahead buffer offset
	.curgain1:     resd 1  ; flt: left current gain
	.curgain2:     resd 1  ; flt: right current gain

	.pkval1:       resd 1  ; flt: left/mono peak value
	.pkval2:       resd 1  ; flt: right peak value
	.rmscnt:       resd 1  ; int: RMS buffer offset
	.rmsval1:      resd 1  ; flt: left/mono RMS current value
	.rmsval2:      resd 1  ; flt: right RMS current value

	.dbuf:         resd 2*COMPDLEN ; lookahead delay buffer
	.rmsbuf:       resd 2*8192     ; RMS ring buffer
endstruc

syCompInit:
	Pushad
	mov     al, 2
	mov     byte [rbp + syWComp.mode], al
	Popad
	ret

syCompSet:
	Pushad
	fld     dword [rsi + syVComp.mode]
	fistp   dword [rel temp]
	mov     rax, [rel temp]
	dec     rax
	and     rax, 5
	fld     dword [rsi + syVComp.stereo]
	fistp   dword [rel temp]
	mov     rbx,  [rel temp]
	add     rbx,  rbx
	add     rax,  rbx
	mov     [rbp + syWComp.mode], rax
	cmp     rax, [rbp + syWComp.oldmode]
	je      .norst
	mov     [rbp + syWComp.oldmode], rax
	mov     rcx, 2*8192
	xor     rax, rax
	mov     [rbp + syWComp.pkval1], rax
	mov     [rbp + syWComp.pkval2], rax
	mov     [rbp + syWComp.rmsval1], rax
	mov     [rbp + syWComp.rmsval2], rax
	lea     rdi, [rbp + syWComp.rmsbuf]
	rep     stosd
	fld1
	fst     dword [rbp + syWComp.curgain1]
	fstp    dword [rbp + syWComp.curgain2]

.norst:
	fld     dword [rsi + syVComp.lookahead]
	fmul    dword [rel fcsamplesperms]
	fistp   dword [rbp + syWComp.dblen]
	fld     dword [rsi + syVComp.threshold]
	fmul    dword [rel fci128]
	call    calcfreq
	fadd    st0, st0
	fadd    st0, st0
	fadd    st0, st0
	fld1
	fdiv    st0, st1
	fstp    dword [rbp + syWComp.invol]

	fld     dword [rsi + syVComp.autogain]
	fistp   dword [rel temp]
	mov     rax,  [rel temp]
	or      rax,  rax
	jz      .noag
	fstp    st0
	fld1
.noag:
	fld     dword [rsi + syVComp.outgain]
	fsub    dword [rel fc64]
	fmul    dword [rel fci16]
	call    pow2
	fmulp   st1, st0
	fstp    dword [rbp + syWComp.outvol]

	fld     dword [rsi + syVComp.ratio]
	fmul    dword [rel fci128]
	fstp    dword [rbp + syWComp.ratio]

	;attack: 0 (!) ... 200msecs (5Hz)
	fld     dword [rsi + syVComp.attack]
	fmul    dword [rel fci128]   ; 0 .. fast1
	fmul    dword [rel fcm12]    ; 0 .. fastminus12
	call    pow2             ; 1 .. 2^(fastminus12)
	fstp    dword [rbp + syWComp.attack]

	;release: 5ms bis 5s
	fld     dword [rsi + syVComp.release]
	fmul    dword [rel fci128]   ; 0 .. fast1
	fmul    dword [rel fcm16]    ; 0 .. fastminus16
	call    pow2             ; 1 .. 2^(fastminus16)
	fstp    dword [rbp + syWComp.release]

	Popad
	ret


syCompLDMonoPeak:
	Pushad
	fld     dword [rbp + syWComp.pkval1]     ; <pv>
.lp:
	fld     dword [rsi]					; <l> <pv>
	fadd    dword [rsi + 4]				; <l+r> <pv>
	fmul    dword [rel fci2]			; <in> <pv>
	fstp    dword [rel temp]			; <pv>
	fmul    dword [rel fccpdfalloff]	; <pv'>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	lea     rsi,  [rsi+8]
	lea     r8, [rel temp]
	fstp    dword [r8 + 4]      ; -
	mov     rax,  [r8]
	and     rax,  7fffffffh  ; fabs()
	cmp     rax,  [r8 + 4]
	jbe     .nonp
	mov     [r8 + 4], rax
.nonp:
	lea     r8, [rel temp]
	fld     dword [r8 + 4]      ; <npv>
	fld     st0
	fmul    dword [rbp + syWComp.invol]
	fst     dword [rdi]
	fstp    dword [rdi+4]
	lea     rdi,  [rdi+8]
	dec     rcx
	jnz     .lp
	fstp    dword [rbp + syWComp.pkval1]     ; -
	Popad
	ret

syCompLDMonoRMS:
	Pushad
	fld    dword [rbp + syWComp.rmsval1]  ; <rv>
	mov    rax,  [rbp + syWComp.rmscnt]
	lea    rdx,  [rbp + syWComp.rmsbuf]
.lp:
	fsub    dword [rdx + 4*rax]   ; <rv'>
	fld     dword [rsi]           ; <l> <rv'>
	fadd    dword [rsi + 4]       ; <l+r> <rv'>
	fmul    dword [rel fci2]          ; <in> <rv'>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
%endif
	lea     rsi,  [rsi+8]
	fmul    st0, st0              ; <in²> <rv'>
	fadd    st1, st0              ; <in²> <rv''>
	fstp    dword [rdx + 4*rax]   ; <rv''>
	fld     st0                   ; <rv''> <rv''>
	fsqrt                         ; <sqrv> <rv''>
	fmul    dword [rel fci8192]
	inc     rax
	and     ah, 0x1f
	fmul    dword [rbp + syWComp.invol]
	fst     dword [rdi]
	fstp    dword [rdi+4]         ; <rv''>
	lea     rdi,  [rdi+8]
	dec     rcx
	jnz     .lp
	mov     [rbp + syWComp.rmscnt], rax
	fstp    dword [rbp + syWComp.rmsval1]  ; -
	Popad
	ret

syCompLDStereoPeak:
	Pushad
	fld     dword [rbp + syWComp.pkval2]     ; <rpv>
	fld     dword [rbp + syWComp.pkval1]     ; <lpv> <rpv>
.lp:
	fmul    dword [rel fccpdfalloff]  ; <lpv'> <rpv>
	fxch    st1                   ; <rpv> <lpv'>
	fmul    dword [rel fccpdfalloff]  ; <rpv'> <lpv'>
	fxch    st1                   ; <lpv'> <rpv'>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
	fxch    st1
	fadd    dword [rel dcoffset]
	fxch    st1
%endif
	lea     r8, [rel temp]
	fstp    dword [r8]            ; <rpv'>
	fstp    dword [r8+4]          ; -
	mov     rax,  [rsi]
	and     rax,  7fffffffh       ; fabs()
	cmp     rax,  [r8]
	jbe     .nonp1
	mov     [r8], rax
.nonp1:
	mov     rax,  [rsi+4]
	and     rax,  7fffffffh       ; fabs()
	lea     r8, [rel temp]
	cmp     rax,  [r8+4]
	jbe     .nonp2
	mov     [r8+4], rax
.nonp2:
	lea     rsi,  [rsi+8]
	lea     r8, [rel temp]
	fld     dword [r8+4]          ; <nrpv>
	fld     st0
	fmul    dword [rbp + syWComp.invol]
	fstp    dword [rdi+4]
	fld     dword [r8]            ; <nlpv> <nrpv>
	fld     st0
	fmul    dword [rbp + syWComp.invol]
	fstp    dword [rdi]
	lea     rdi,  [rdi+8]
	dec     rcx
	jnz     .lp
	fstp    dword [rbp + syWComp.pkval1]     ; <nrpv>
	fstp    dword [rbp + syWComp.pkval2]     ; -
	Popad
	ret

syCompLDStereoRMS:
	Pushad
	fld    dword [rbp + syWComp.rmsval2]  ; <rrv>
	fld    dword [rbp + syWComp.rmsval1]  ; <lrv> <rrv>
	mov    rax,  [rbp + syWComp.rmscnt]
	lea    rdx,  [rbp + syWComp.rmsbuf]
.lp:
	fsub    dword [rdx + 8*rax]     ; <lrv'> <rrv>
	fxch    st1                     ; <rrv> <lrv'>
	fsub    dword [rdx + 8*rax + 4] ; <rrv'> <lrv'>
	fxch    st1                     ; <lrv'> <rrv'>
%if FIXDENORMALS
	fadd    dword [rel dcoffset]
	fxch    st1
	fadd    dword [rel dcoffset]
	fxch    st1
%endif

	fld     dword [rsi]             ; <l> <lrv'> <rrv'>
	fmul    st0, st0                ; <l²> <lrv'> <rrv'>
	fadd    st1, st0                ; <l²> <lrv''> <rrv'>
	fstp    dword [rdx + 8*rax]     ; <lrv''> <rrv'>
	fld     st0                     ; <lrv''> <lrv''> <rrv'>
	fsqrt                           ; <sqlrv> <lrv''> <rrv'>
	fmul    dword [rel fci8192]
	fmul    dword [rbp + syWComp.invol]
	fstp    dword [rdi]             ; <lrv''> <rrv'>

	fld     dword [rsi+4]           ; <r> <lrv''> <rrv'>
	fmul    st0, st0                ; <r²> <lrv''> <rrv'>
	fadd    st2, st0                ; <r²> <lrv''> <rrv''>
	fstp    dword [rdx + 8*rax + 4] ; <lrv''> <rrv''>
	fld     st1                     ; <rrv''> <lrv''> <rrv''>
	fsqrt                           ; <sqrrv> <lrv''> <rrv''>
	fmul    dword [rel fci8192]
	fmul    dword [rbp + syWComp.invol]
	fstp    dword [rdi+4]           ; <lrv''> <rrv''>

	lea     rsi,  [rsi+8]
	inc     rax
	and     ah, 0x1f
	lea     rdi,  [rdi+8]
	dec     rcx
	jnz     .lp
	mov     [rbp + syWComp.rmscnt], rax
	fstp    dword [rbp + syWComp.rmsval1]  ; <nrrv>
	fstp    dword [rbp + syWComp.rmsval2]  ; -
	Popad
	ret


; rbp: this
; rbx: ptr to lookahead line
; rcx: # of samples to process
; rdx: offset into lookahead line
; rsi: ptr to in/out buffer
; rdi: ptr to level buffer
; st0: current gain value
syCompProcChannel:
	Pushad
.cloop:

	fst     dword [rel temp]

	; lookahead
	fld     dword [rbx + 8*rdx]         ; <v> <gain>
	fld     dword [rsi]                 ; <nv> <v> <gain>
	fmul    dword [rbp + syWComp.invol] ; <nv'> <v> <gain>
	fstp    dword [rbx + 8*rdx]         ; <v> <gain>
	fmul    dword [rbp + syWComp.outvol]; <v'> <gain>
	inc     rdx
	cmp     rdx,  [rbp + syWComp.dblen]
	jbe     .norst
	xor     rdx, rdx
.norst:

	; destgain ermitteln
	mov     rax,  [rdi]
	cmp     rax,  3f800000h               ; 1.0
	jae     .docomp
	fld1                                  ; <dgain> <v> <gain>
	jmp     .cok
.docomp:
	fld     dword [rdi]                   ; <lvl> <v> <gain>
	fld1                                  ; <1> <lvl> <v> <gain>
	fsubp   st1, st0                      ; <lvl-1> <v> <gain>
	fmul    dword [rbp + syWComp.ratio]   ; <r*(lvl-1)> <v> <gain>
	fld1                                  ; <1> <r*(lvl-1)> <v> <gain>
	faddp   st1, st0                      ; <1+r*(lvl-1)> <v> <gain>
	fld1                                  ; <1> <1+r*(lvl-1)> <v> <gain>
	fdivrp  st1, st0                      ; <dgain> <v> <gain>
.cok:
	lea     rdi,  [rdi+8]

	lea     r8, [rel temp]
	fst     dword [r8+4]
	mov     rax,  [r8+4]
	cmp     rax,  [r8]
	jb      .attack
	fld     dword [rbp + syWComp.release] ; <spd> <dgain> <v> <gain>
	jmp     .cok2
.attack:
	fld     dword [rbp + syWComp.attack]  ; <spd> <dgain> <v> <gain>

.cok2:
	; und compressen
	fxch    st1                           ; <dg> <spd> <v> <gain>
	fsub    st0, st3                      ; <dg-gain> <spd> <v> <gain>
	fmulp   st1, st0                      ; <spd*(dg-d)> <v> <gain>
	faddp   st2, st0                      ; <v> <gain'>
	fmul    st0, st1                      ; <out> <gain'>
	fstp    dword [rsi]                   ; <gain'>
	lea     rsi,  [rsi+8]

	dec     rcx
	jnz     near .cloop
	mov     [rel temp], rdx
	Popad
	ret
; on exit: [temp] = new dline count

section .data.rel.ro

syCRMTab:
	dq syCompLDMonoPeak
	dq syCompLDMonoRMS
	dq syCompLDStereoPeak
	dq syCompLDStereoRMS

section .text

; rsi: input/output buffer
; rcx: # of samples
global syCompRender
syCompRender:
	Pushad
	V2PerfEnter V2Perf_COMPRESSOR

	fclex ; clear exceptions

	mov  rax, [rbp + syWComp.mode]
	test al, 4
	jz   .doit

	V2PerfLeave V2Perf_COMPRESSOR
	Popad
	ret

.doit:
	; STEP 1: level detect (fills LD buffers)

	lea     rdi,  [rel vcebuf]
	and     al, 3
	lea     r8, [rel syCRMTab]
	call    [r8 + 8*rax]

	; check for FPU exception
	fstsw ax
	or    al, al
	jns   .fpuok
	; if occured, clear LD buffer
	push rcx
	push rdi
	fldz
	add  rcx, rcx
	xor  rax, rax
	rep stosd
	fstp st0
	pop  rdi
	pop  rcx
.fpuok:

	; STEP 2: compress!
	lea     rbx, [rbp + syWComp.dbuf]
	mov     rdx, [rbp + syWComp.dbcnt]
	fld     dword [rbp + syWComp.curgain1]
	call    syCompProcChannel
	fstp    dword [rbp + syWComp.curgain1]
	lea     rsi, [rsi+4]
	lea     rdi, [rdi+4]
	lea     rbx, [rbx+4]
	fld     dword [rbp + syWComp.curgain2]
	call    syCompProcChannel
	fstp    dword [rbp + syWComp.curgain2]
	mov     rdx, [rel temp]
	mov     [rbp + syWComp.dbcnt], rdx

	V2PerfLeave V2Perf_COMPRESSOR
	Popad
	ret





;#####################################################################################
;#
;#                            E L I T E G R O U P
;#                             we are very good.
;#
;# World Domination Intro Sound System
;# -> Stereo reverb plugin (reads aux1)
;#
;# Written and (C) 1999 by The Artist Formerly Known As Doctor Roole
;#
;# This is a modified  Schroeder reverb (as found in  csound et al) consisting
;# of four parallel comb filter delay lines (with low pass filtered feedback),
;# followed by two  allpass filter delay lines  per channel. The  input signal
;# is feeded directly into half of the comb delays, while it's inverted before
;# being feeded into the other half to  minimize the response to DC offsets in
;# the incoming signal, which was a  great problem of the original implementa-
;# tion. Also, all of the comb delays are routed through 12dB/oct IIR low pass
;# filters before ferding the output  signal back to the input to simulate the
;# walls' high damping, which makes this  reverb sound a lot smoother and much
;# more realistic.
;#
;# This leaves nothing but the conclusion that we're simply better than you.
;#
;#####################################################################################

; lengths of delay lines in samples
lencl0 	equ 1309    		; left comb filter delay 0
lencl1	equ 1635		; left comb filter delay 1
lencl2 	equ 1811                ; left comb filter delay 2
lencl3 	equ 1926                ; left comb filter delay 3
lenal0 	equ 220                 ; left all pass delay 0
lenal1 	equ 74                  ; left all pass delay 1
lencr0 	equ 1327		; right comb filter delay 0
lencr1 	equ 1631                ; right comb filter delay 1
lencr2 	equ 1833                ; right comb filter delay 2
lencr3 	equ 1901                ; right comb filter delay 3
lenar0 	equ 205		; right all pass delay 0
lenar1 	equ 77		; right all pass delay 1

global _REVERB_
_REVERB_:

struc syVReverb
	.revtime:   resd 1
	.highcut:   resd 1
	.lowcut:    resd 1
	.vol:       resd 1
endstruc

struc syCReverb
	.gainc0	resd 1          ; feedback gain for comb filter delay 0
	.gainc1	resd 1          ; feedback gain for comb filter delay 1
	.gainc2	resd 1          ; feedback gain for comb filter delay 2
	.gainc3	resd 1          ; feedback gain for comb filter delay 3
	.gaina0	resd 1          ; feedback gain for allpass delay 0
	.gaina1	resd 1          ; feedback gain for allpass delay 1
	.gainin	resd 1          ; input gain
	.damp   resd 1          ; high cut   (1-val²)
	.lowcut resd 1          ; low cut    (val²)
endstruc

struc syWReverb
	.setup:     resb syCReverb_size
	; positions of delay lines
	.dyn:
	.poscl0: 	resd 1
	.poscl1: 	resd 1
	.poscl2: 	resd 1
	.poscl3: 	resd 1
	.posal0: 	resd 1
	.posal1: 	resd 1
	.poscr0: 	resd 1
	.poscr1: 	resd 1
	.poscr2: 	resd 1
	.poscr3: 	resd 1
	.posar0: 	resd 1
	.posar1: 	resd 1
	; comb delay low pass filter buffers (y(k-1))
	.lpfcl0:    resd 1
	.lpfcl1:    resd 1
	.lpfcl2:    resd 1
	.lpfcl3:    resd 1
	.lpfcr0:    resd 1
	.lpfcr1:    resd 1
	.lpfcr2:    resd 1
	.lpfcr3:    resd 1
	; memory for low cut filters
	.hpfcl:     resd 1
	.hpfcr:     resd 1
	; memory for the delay lines
	.linecl0: 	resd lencl0
	.linecl1: 	resd lencl1
	.linecl2: 	resd lencl2
	.linecl3: 	resd lencl3
	.lineal0: 	resd lenal0
	.lineal1: 	resd lenal1
	.linecr0: 	resd lencr0
	.linecr1: 	resd lencr1
	.linecr2: 	resd lencr2
	.linecr3: 	resd lencr3
	.linear0: 	resd lenar0
	.linear1: 	resd lenar1
endstruc

; see struct above
syRvDefs:
	dd 0.966384599, 0.958186359, 0.953783929, 0.950933178, 0.994260075, 0.998044717
	dd 1.0  ; input gain
	dd 0.8  ; high cut


syReverbInit:
	Pushad
	xor    rax, rax
	mov    rcx, syWReverb_size
	mov    rdi, rbp
	rep    stosb
	Popad
	ret

syReverbReset:
	Pushad
	xor    rax, rax
	mov    rcx, syWReverb_size-syWReverb.dyn
	lea    rdi, [rbp + syWReverb.dyn]
	rep    stosb
	Popad
	ret


syReverbSet:
	Pushad

	fld    dword [rsi + syVReverb.revtime]
	fld1
	faddp  st1, st0
	fld    dword [rel fc64]
	fdivrp st1, st0
	fmul   st0, st0
	fmul   dword [rel SRfclinfreq]
	xor    rcx, rcx
.rtloop:
	fld  st0
	lea  r8, [rel syRvDefs]
	fld  dword [r8 + 4*rcx]
	call pow
	fstp dword [rbp + syWReverb.setup + syCReverb.gainc0 + 4*rcx]
	inc  rcx
	cmp  cl, 6
	jne .rtloop
	fstp   st0

	fld    dword [rsi + syVReverb.highcut]
	fmul   dword [rel fci128]
	fmul   dword [rel SRfclinfreq]
	fstp   dword [rbp + syWReverb.setup + syCReverb.damp]
	fld    dword [rsi + syVReverb.vol]
	fmul   dword [rel fci128]
	fstp   dword [rbp + syWReverb.setup + syCReverb.gainin]

	fld    dword [rsi + syVReverb.lowcut]
	fmul   dword [rel fci128]
	fmul   st0, st0
	fmul   st0, st0
	fmul   dword [rel SRfclinfreq]
	fstp   dword [rbp + syWReverb.setup + syCReverb.lowcut]

	Popad
	ret


syReverbProcess:
	Pushad
	V2PerfEnter V2Perf_REVERB

	fclex

	lea    rsi,  [rel aux1buf]
	fld    dword [rbp + syWReverb.setup + syCReverb.lowcut]             	; <lc> <0>
	fld    dword [rbp + syWReverb.setup + syCReverb.damp]             	; <damp> <lc> <0>
	xor    rbx, rbx
	mov    rax, rcx

.sloop:          ; prinzipiell nur ne große schleife
	; step 1: get input sample
	fld		 dword [rsi]																  						; <in'> <damp> <lc> <0>
	fmul	 dword [rbp + syWReverb.setup + syCReverb.gainin]     	; <in> <damp> <lc> <0>
%if FIXDENORMALS
	fadd   dword [rel dcoffset]
%endif
	lea    rsi,	 [rsi+4]

	; step 2a: process the 4 left lpf filtered comb delays
	; left comb 0
	mov    rdx,  [rbp + syWReverb.poscl0]
	fld    dword [rbp + syWReverb.linecl0+4*rdx]    	; <dv> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc0]           	; <dv'> <in> <damp> <lc> <chk>
	fadd   st0,  st1                		; <nv>  <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcl0]           	; <v-lp> <in> <damp> <lc> <chk>
	fmul   st0,  st2                		; <d*(v-lp)> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcl0]           	; <dout> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcl0]           	; <dout> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecl0+4*rdx]    	; <asuml> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencl0
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscl0], rdx

	; left comb 1
	mov    rdx,  [rbp + syWReverb.poscl1]
	fld    dword [rbp + syWReverb.linecl1+4*rdx]    	; <dv> <asuml> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc1]           	; <dv'> <asuml> <in> <damp> <lc> <chk>
	fsub   st0,  st2                		; <nv>  <asuml> <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcl1]           	; <v-lp> <asuml> <in> <damp> <lc> <chk>
	fmul   st0,  st3                		; <d*(v-lp)> <asuml> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcl1]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcl1]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecl1+4*rdx]    	; <dout> <asuml> <in> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asuml'> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencl1
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscl1], rdx

	; left comb 2
	mov    rdx,  [rbp + syWReverb.poscl2]
	fld    dword [rbp + syWReverb.linecl2+4*rdx]    	; <dv> <asuml> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc2]           	; <dv'> <asuml> <in> <damp> <lc> <chk>
	fadd   st0,  st2                		; <nv>  <asuml> <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcl2]           	; <v-lp> <asuml> <in> <damp> <lc> <chk>
	fmul   st0,  st3                		; <d*(v-lp)> <asuml> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcl2]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcl2]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecl2+4*rdx]    	; <dout> <asuml> <in> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asuml'> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencl2
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscl2], rdx

	; left comb 3
	mov    rdx,  [rbp + syWReverb.poscl3]
	fld    dword [rbp + syWReverb.linecl3+4*rdx]    	; <dv> <asuml> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc3]           	; <dv'> <asuml> <in> <damp> <lc> <chk>
	fsub   st0,  st2                		; <nv>  <asuml> <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcl3]           	; <v-lp> <asuml> <in> <damp> <lc> <chk>
	fmul   st0,  st3                		; <d*(v-lp)> <asuml> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcl3]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcl3]           	; <dout> <asuml> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecl3+4*rdx]    	; <dout> <asuml> <in> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asuml'> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencl3
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscl3], rdx

	; step 2b: process the 2 left allpass delays
	; left allpass 0
	mov    rdx,  [rbp + syWReverb.posal0]
	fld    dword [rbp + syWReverb.lineal0+4*rdx]    	; <d0v> <asuml> <in> <damp> <lc> <chk>
	fld    st0                      		; <d0v> <d0v> <asuml> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina0]           	; <d0v'> <d0v> <asuml> <in> <damp> <lc> <chk>
	faddp  st2, st0                 		; <d0v> <d0z> <in> <damp> <lc> <chk>
	fxch   st0, st1                 		; <d0z> <d0v> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lineal0+4*rdx]
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina0]           	; <d0z'> <d0v> <in> <damp> <lc> <chk>
	fsubp  st1, st0                 		; <d0o> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dl, lenal0
	cmove  rdx, rbx
	mov    [rbp + syWReverb.posal0], rdx

	; left allpass 1
	mov    rdx,  [rbp + syWReverb.posal1]
	fld    dword [rbp + syWReverb.lineal1+4*rdx]    	; <d1v> <d0o> <in> <damp> <lc> <chk>
	fld    st0                      		; <d1v> <d1v> <d0o> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina1]           	; <d1v'> <d1v> <d0o> <in> <damp> <lc> <chk>
	faddp  st2, st0                 		; <d1v> <d1z> <in> <damp> <lc> <chk>
	fxch   st0, st1                 		; <d1z> <d1v> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lineal1+4*rdx]
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina1]           	; <d1z'> <d1v> <in> <damp> <lc> <chk>
	fsubp  st1, st0                 		; <d1o> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dl, lenal1
	cmove  rdx, rbx
	mov    [rbp + syWReverb.posal1], rdx

	; step 2c: low cut
	fld    dword [rbp + syWReverb.hpfcl]	; <hpf> <d1o> <in> <damp> <lc> <chk>
	fld    st0                            ; <hpf> <hpf> <d1o> <in> <damp> <lc> <chk>
	fsubr  st0, st2                       ; <d1o-hpf> <hpf> <d1o> <in> <damp> <lc> <chk>
	fmul   st0, st5                       ; <lc(d1o-hpf)> <hpf> <d1o> <in> <damp> <lc> <chk>
	faddp  st1, st0                       ; <hpf'> <d1o> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.hpfcl]
	fsubp  st1, st0                       ; <outl> <in> <damp> <lc> <chk>

	; step 2d: update left mixing buffer
	fadd  dword [rdi]
	fstp  dword [rdi]                     ; <in> <damp> <lc> <chk>


	; step 3a: process the 4 right lpf filtered comb delays
	; right comb 0
	mov    rdx,  [rbp + syWReverb.poscr0]
	fld    dword [rbp + syWReverb.linecr0+4*rdx]    	; <dv> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc0]           	; <dv'> <in> <damp> <lc> <chk>
	fadd   st0,  st1                		; <nv>  <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcr0]           	; <v-lp> <in> <damp> <lc> <chk>
	fmul   st0,  st2                		; <d*(v-lp)> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcr0]           	; <dout> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcr0]           	; <dout> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecr0+4*rdx]    	; <asumr> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencr0
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscr0], rdx

	; right comb 1
	mov    rdx,  [rbp + syWReverb.poscr1]
	fld    dword [rbp + syWReverb.linecr1+4*rdx]    	; <dv> <asumr> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc1]           	; <dv'> <asumr> <in> <damp> <lc> <chk>
	fsub   st0,  st2                		; <nv>  <asumr> <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcr1]           	; <v-lp> <asumr> <in> <damp> <lc> <chk>
	fmul   st0,  st3                		; <d*(v-lp)> <asumr> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcr1]           	; <dout> <asumr> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcr1]           	; <dout> <asumr> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecr1+4*rdx]    	; <dout> <asumr> <in> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asumr'> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencr1
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscr1], rdx

	; right comb 2
	mov    rdx,  [rbp + syWReverb.poscr2]
	fld    dword [rbp + syWReverb.linecr2+4*rdx]    	; <dv> <asumr> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc2]           	; <dv'> <asumr> <in> <damp> <lc> <chk>
	fadd   st0,  st2                		; <nv>  <asumr> <in> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcr2]           	; <v-lp> <asumr> <in> <damp> <lc> <chk>
	fmul   st0,  st3                		; <d*(v-lp)> <asumr> <in> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcr2]           	; <dout> <asumr> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcr2]           	; <dout> <asumr> <in> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecr2+4*rdx]    	; <dout> <asumr> <in> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asumr'> <in> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencr2
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscr2], rdx

	; right comb 3
	mov    rdx,  [rbp + syWReverb.poscr3]
	fld    dword [rbp + syWReverb.linecr3+4*rdx]    	; <dv> <asumr> <in> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gainc3]           	; <dv'> <asumr> <in> <damp> <lc> <chk>
	fsubrp st2,  st0                		; <asumr> <nv> <damp> <lc> <chk>
	fxch   st0,  st1                		; <nv> <asumr> <damp> <lc> <chk>
	fsub   dword [rbp + syWReverb.lpfcr3]           	; <v-lp> <asumr> <damp> <lc> <chk>
	fmul   st0,  st2                		; <d*(v-lp)> <asumr> <damp> <lc> <chk>
	fadd   dword [rbp + syWReverb.lpfcr3]           	; <dout> <asumr> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.lpfcr3]           	; <dout> <asumr> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linecr3+4*rdx]    	; <dout> <asumr> <damp> <lc> <chk>
	faddp  st1,  st0                		; <asumr'> <damp> <lc> <chk>
	inc    rdx
	cmp    dx,  lencr3
	cmove  rdx, rbx
	mov    [rbp + syWReverb.poscr3], rdx


	; step 2b: process the 2 right allpass delays
	; right allpass 0
	mov    rdx,  [rbp + syWReverb.posar0]
	fld    dword [rbp + syWReverb.linear0+4*rdx]    	; <d0v> <asumr> <damp> <lc> <chk>
	fld    st0                      		; <d0v> <d0v> <asumr> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina0]           	; <d0v'> <d0v> <asumr> <damp> <lc> <chk>
	faddp  st2, st0                 		; <d0v> <d0z> <damp> <lc> <chk>
	fxch   st0, st1                 		; <d0z> <d0v> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linear0+4*rdx]
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina0]           	; <d0z'> <d0v> <damp> <lc> <chk>
	fsubp  st1, st0                 		; <d0o> <damp> <lc> <chk>
	inc    rdx
	cmp    dl, lenar0
	cmove  rdx, rbx
	mov    [rbp + syWReverb.posar0], rdx

	; right allpass 1
	mov    rdx,  [rbp + syWReverb.posar1]
	fld    dword [rbp + syWReverb.linear1+4*rdx]    	; <d1v> <d0o> <damp> <lc> <chk>
	fld    st0                      		; <d1v> <d1v> <d0o> <damp> <lc> <chk>
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina1]           	; <d1v'> <d1v> <d0o> <damp> <lc> <chk>
	faddp  st2, st0                 		; <d1v> <d1z> <damp> <lc> <chk>
	fxch   st0, st1                 		; <d1z> <d1v> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.linear1+4*rdx]
	fmul   dword [rbp + syWReverb.setup + syCReverb.gaina1]           	; <d1z'> <d1v> <damp> <lc> <chk>
	fsubp  st1, st0                 		; <d1o> <damp> <lc> <chk>
	inc    rdx
	cmp    dl, lenar1
	cmove  rdx, rbx
	mov    [rbp + syWReverb.posar1], rdx


	; step 2c: low cut
	fld    dword [rbp + syWReverb.hpfcr]	; <hpf> <d1o> <damp> <lc> <chk>
	fld    st0                            ; <hpf> <hpf> <d1o>  <damp> <lc> <chk>
	fsubr  st0, st2                       ; <d1o-hpf> <hpf> <d1o> <damp> <lc> <chk>
	fmul   st0, st4                       ; <lc(d1o-hpf)> <hpf> <d1o> <damp> <lc> <chk>
	faddp  st1, st0                       ; <hpf'> <d1o> <damp> <lc> <chk>
	fst    dword [rbp + syWReverb.hpfcr]
	fsubp  st1, st0                       ; <outr> <damp> <lc> <chk>

	; step 2d: update left mixing buffer
	fadd  dword [rdi+4]
	fstp  dword [rdi+4]                   ; <damp> <lc> <chk>


	lea    rdi,  [rdi+8]
	dec    rcx
	jnz    near .sloop

	fstp   st0                      ; <lc> <chk>
	fstp   st0                      ; <chk>

	; FPU underflow protection
	fstsw  ax
	and    rax, byte 16
	jz     .dontpanic
	call   syReverbReset


.dontpanic:

	V2PerfLeave V2Perf_REVERB
	Popad
	ret

;#####################################################################################
; Oberdings
; CHANNELS RULEN
;#####################################################################################

global _V2CHAN_
_V2CHAN_:

struc syVChan
	.chanvol: resd 1
	.auxarcv: resd 1			; CHAOS aux a receive
	.auxbrcv: resd 1			; CHAOS aux b receive
	.auxasnd: resd 1			; CHAOS aux a send
	.auxbsnd: resd 1			; CHAOS aux b send
	.aux1:	  resd 1
	.aux2:    resd 1
	.fxroute: resd 1
	.boost:   resb syVBoost_size
	.cdist:   resb syVDist_size
	.chorus:  resb syVModDel_size
	.compr:   resb syVComp_size
endstruc

struc syWChan
	.chgain: resd 1
	.a1gain: resd 1
	.a2gain: resd 1
	.aasnd:  resd 1
	.absnd:  resd 1
	.aarcv:  resd 1
	.abrcv:  resd 1
	.fxr:    resd 1
	.boostw: resb syWBoost_size
	.distw:  resb syWDist_size
	.chrw:	 resb syWModDel_size
	.compw:  resb syWComp_size
endstruc


; rbp: wchan
syChanInit:
	Pushad
	lea		rbp, [rbp + syWChan.distw]
	call	syDistInit
	lea		rbp, [rbp + syWChan.chrw - syWChan.distw]
	call	syModDelInit
	lea		rbp, [rbp + syWChan.compw - syWChan.chrw]
	call  syCompInit
	lea		rbp, [rbp + syWChan.boostw - syWChan.compw]
	call  syBoostInit
	Popad
	ret

; rsi: vchan
; rbp: wchan
syChanSet:
	Pushad
	fld		dword [rsi + syVChan.auxarcv]
	fmul	dword [rel fci128]
	fstp	dword [rbp + syWChan.aarcv]
	fld		dword [rsi + syVChan.auxbrcv]
	fmul	dword [rel fci128]
	fstp	dword [rbp + syWChan.abrcv]
	fld		dword [rsi + syVChan.auxasnd]
	fmul	dword [rel fci128]
	fmul	dword [rel fcgain]
	fstp	dword [rbp + syWChan.aasnd]
	fld		dword [rsi + syVChan.auxbsnd]
	fmul	dword [rel fci128]
	fmul	dword [rel fcgain]
	fstp	dword [rbp + syWChan.absnd]
	fld		dword [rsi + syVChan.chanvol]
	fmul	dword [rel fci128]
	fmul	dword [rel fcgain]
	fst 	dword [rbp + syWChan.chgain]
	fld		dword [rsi + syVChan.aux1]
	fmul	dword [rel fci128]
	fmul	dword [rel fcgainh]
	fmul  st0, st1
	fstp	dword [rbp + syWChan.a1gain]
	fld		dword [rsi + syVChan.aux2]
	fmul	dword [rel fci128]
	fmul	dword [rel fcgainh]
	fmulp st1, st0
	fstp	dword [rbp + syWChan.a2gain]
	fld		dword [rsi + syVChan.fxroute]
	fistp dword [rbp + syWChan.fxr]
	lea   rsi,  [rsi + syVChan.cdist]
	lea		rbp,	[rbp + syWChan.distw]
	call	syDistSet
	lea   rsi,  [rsi + syVChan.chorus - syVChan.cdist]
	lea		rbp,	[rbp + syWChan.chrw - syWChan.distw]
	call	syModDelSet
	lea   rsi,  [rsi + syVChan.compr - syVChan.chorus]
	lea		rbp,  [rbp + syWChan.compw - syWChan.chrw]
	call	syCompSet
	lea   rsi,  [rsi + syVChan.boost - syVChan.compr]
	lea		rbp,  [rbp + syWChan.boostw - syWChan.compw]
	call	syBoostSet
	Popad
	ret

syChanProcess:
	Pushad

	; AuxA Receive		CHAOS
	mov   rax, [rbp + syWChan.aarcv]
	or    rax, rax
	jz    .noauxarcv
	push    rcx
	lea     rdi, [rel chanbuf]
	lea     rsi, [rel auxabuf]
.auxarcvloop:
	fld		dword [rsi]
	fmul    dword [rbp + syWChan.aarcv]
	fld		dword [rsi+4]
	fmul    dword [rbp + syWChan.aarcv]
	lea     rsi, [rsi + 8]
	fxch    st1
	fadd	dword [rdi]
	fxch    st1
	fadd	dword [rdi+4]
	fxch    st1
	fstp    dword [rdi]
	fstp    dword [rdi+4]
	lea     rdi, [rdi + 8]
	dec     rcx
	jnz		.auxarcvloop
	pop		rcx
.noauxarcv:

	; AuxB Receive		CHAOS
	mov   rax, [rbp + syWChan.abrcv]
	or    rax, rax
	jz    .noauxbrcv
	push	rcx
	lea		rdi, [rel chanbuf]
	lea   rsi, [rel auxbbuf]
.auxbrcvloop:
	fld		dword [rsi]
	fmul  dword [rbp + syWChan.abrcv]
	fld		dword [rsi+4]
	fmul  dword [rbp + syWChan.abrcv]
	lea   rsi, [rsi + 8]
	fxch  st1
	fadd	dword [rdi]
	fxch  st1
	fadd	dword [rdi+4]
	fxch  st1
	fstp  dword [rdi]
	fstp  dword [rdi+4]
	lea   rdi, [rdi + 8]
	dec rcx
	jnz		.auxbrcvloop
	pop		rcx
.noauxbrcv:
	lea		rsi, [rel chanbuf]
	mov		rdi, rsi

	lea		rbp, [rbp + syWChan.compw - 0]
	call  syCompRender
	lea		rbp, [rbp + syWChan.boostw - syWChan.compw]
	call  syBoostRender
	lea		rbp, [rbp + 0 - syWChan.boostw]

	mov   rax, [rbp + syWChan.fxr]
	or    rax, rax
	jnz   .otherway
	lea		rbp, [rbp - 0 + syWChan.distw]
	call	syDistRenderStereo
	lea		rbp, [rbp - syWChan.distw + syWChan.chrw]
	call	syModDelRenderChan
	lea		rbp, [rbp - syWChan.chrw + 0]
	jmp   short .weiterso
.otherway:
	lea		rbp, [rbp + syWChan.chrw - 0]
	call	syModDelRenderChan
	lea		rbp, [rbp + syWChan.distw - syWChan.chrw]
	call	syDistRenderStereo
	lea		rbp, [rbp + 0 - syWChan.distw]

.weiterso:

	; Aux1...
	mov   rax, [rbp + syWChan.a1gain]
	or    rax, rax
	jz    .noaux1

	push	rcx
	push  rsi
	lea   rdi, [rel aux1buf]
.aux1loop:
		fld		dword [rsi]
		fadd	dword [rsi+4]
		lea   rsi, [rsi+8]
		fmul  dword [rbp + syWChan.a1gain]
		fadd  dword [rdi]
		fstp  dword [rdi]
		lea   rdi, [rdi+4]
		dec		rcx
	jnz		.aux1loop
	pop   rsi
	pop		rcx

.noaux1:

	; ... und Aux2.
	mov   rax, [rbp + syWChan.a2gain]
	or    rax, rax
	jz    .noaux2
	push	rcx
	push  rsi
	lea   rdi, [rel aux2buf]
.aux2loop:
		fld		dword [rsi]
		fadd	dword [rsi+4]
		lea   rsi, [rsi+8]
		fmul  dword [rbp + syWChan.a2gain]
		fadd  dword [rdi]
		fstp  dword [rdi]
		lea   rdi, [rdi+4]
		dec		rcx
	jnz		.aux2loop
	pop   rsi
	pop		rcx

.noaux2:

	; AuxA...		CHAOS
	mov   rax, [rbp + syWChan.aasnd]
	or    rax, rax
	jz    .noauxa
	push	rcx
	push  rsi
	lea   rdi, [rel auxabuf]
.auxaloop:
		fld		dword [rsi]
		fmul  dword [rbp + syWChan.aasnd]
		fld		dword [rsi+4]
		fmul  dword [rbp + syWChan.aasnd]
		lea   rsi, [rsi + 8]
		fxch  st1
		fadd	dword [rdi]
		fxch  st1
		fadd	dword [rdi+4]
		fxch  st1
		fstp  dword [rdi]
		fstp  dword [rdi+4]
		lea   rdi, [rdi + 8]
		dec rcx
	jnz		.auxaloop
	pop   rsi
	pop		rcx
.noauxa:

	; AuxB...		CHAOS
	mov   rax, [rbp + syWChan.absnd]
	or    rax, rax
	jz    .noauxb
	push  rcx
	push  rsi
	lea   rdi, [rel auxbbuf]
.auxbloop:
	fld		dword [rsi]
	fmul  dword [rbp + syWChan.absnd]
	fld		dword [rsi+4]
	fmul  dword [rbp + syWChan.absnd]
	lea   rsi, [rsi + 8]
	fxch  st1
	fadd	dword [rdi]
	fxch  st1
	fadd	dword [rdi+4]
	fxch  st1
	fstp  dword [rdi]
	fstp  dword [rdi+4]
	lea   rdi, [rdi + 8]
	dec rcx
	jnz		.auxbloop
	pop   rsi
	pop		rcx
.noauxb:

	; Chanbuf in Mainbuffer kopieren
	lea		rdi, [rel mixbuf]
.ccloop:
	fld		dword [rsi]
	fmul  dword [rbp + syWChan.chgain]
	fld		dword [rsi+4]
	fmul  dword [rbp + syWChan.chgain]
	lea   rsi, [rsi + 8]
	fxch  st1
	fadd	dword [rdi]
	fxch  st1
	fadd	dword [rdi+4]
	fxch  st1
	fstp  dword [rdi]
	fstp  dword [rdi+4]
	lea   rdi, [rdi + 8]
	dec rcx
	jnz .ccloop

	Popad
	ret


; rbx: channel

storeChanValues:
	Pushad

	lea   r8, [rel data]
	movzx rax, byte [r8 + SYN.chans + 8*rbx] ; pgmnummer
	mov   rdi, [r8 + SYN.patchmap]
	mov   rdi, [rdi + 4*rax]				 ; rdi -> sounddaten
	add   rdi, [r8 + SYN.patchmap]

	mov   rax, rbx
	imul  rax, syVChan_size
	lea   rsi, [r8 + SYN.chansv + rax]       ; rsi -> values
	mov   rax, rbx
	imul  rax, syWChan_size
	lea   rbp, [r8 + SYN.chansw + rax]       ; rbp -> workspace
	push  rbp

	; channeldependent daten übertragen
	push rsi
	xor  rcx, rcx
	mov  cl, v2sound.chan
.goloop:
	movzx rax, byte [rdi + rcx]
	mov   [rel temp], rax
	fild  dword [rel temp]
	fstp  dword [rsi]
	lea   rsi, [rsi+4]
	inc   rcx
	cmp   cl, v2sound.endcdvals
	jne .goloop
	pop rsi

	; rbp: ptr auf voice
	mov   rax, [r8 + SYN.voicemap + 4*rbx]
	imul  rax, syWV2_size
	lea   rbp, [r8 + SYN.voicesw + rax]      ; rbp -> workspace


	; MODMATRIX!
	movzx rcx, byte [rdi + v2sound.modnum]
	lea		rdi, [rdi + v2sound.modmatrix]
	or		rcx, rcx
	jnz  .modloop
	jmp  .modend

.modloop:
	movzx   rax, byte [rdi + v2mod.dest]
	cmp     al, v2sound.chan
	jb      .mlegw
	cmp     al, v2sound.endcdvals
	jb      .mok
.mlegw:
	jmp     .mlend
.mok:
	movzx		rax, byte [rdi + v2mod.source]  ; source
	or      rax, rax
	jnz     .mnotvel
	fld     dword [rbp + syWV2.velo]
	jmp			.mdo
.mnotvel:
	cmp     al, 8
	jae     .mnotctl
;	jae     .mlend
	mov     r9, rax
	add     r9, r8
	movzx   rax, byte [r9 + SYN.chans + 8*rbx]
	mov     [rel temp], rax
	fild    dword [rel temp]
	jmp     .mdo
.mnotctl:
	cmp     al, 12
	jae     .mnotvm
	and     al, 3
	lea     r8, [rel sVTab]
	mov     rax, [r8 + 4*rax]
	fld     dword [rbp + rax]
	jmp     .mdo
.mnotvm:
	cmp     al, 13
	jne     .mnotnote
.mnotnote:
	fild    dword [rbp + syWV2.note]
	fsub    dword [rel fc48]
	fadd    st0, st0
.mdo:
	movzx   rax, byte [rdi + v2mod.val]
	mov     [rel temp], rax
	fild    dword [rel temp]
	fsub    dword [rel fc64]
	fmul    dword [rel fci128]
	fadd    st0, st0
	fmulp   st1, st0
	movzx   rax, byte [rdi + v2mod.dest]
	sub     al, v2sound.chan
	fadd    dword [rsi + 4*rax]
	fstp    dword [rel temp]
	; clippen
	mov     rdx, [rel temp]
	or      rdx, rdx
	jns     .mnoclip1
	xor     rdx, rdx
.mnoclip1:
	cmp     rdx, 43000000h
	jbe     .mnoclip2
	mov     rdx, 43000000h
.mnoclip2:
	mov			[rsi + 4*rax], rdx
.mlend:
	lea rdi, [rdi+3]
	dec rcx
	jz .modend
	jmp .modloop
.modend:

	pop rbp
	call syChanSet

.end:
	Popad
	ret



;#####################################################################################
;
; RONAN, DER FREUNDLICHE SPRECHROBOTER
;
;#####################################################################################


; FILTERBANK:

%ifdef RONAN

global _RONAN_
_RONAN_:

extern ronanCBInit
extern ronanCBTick
extern ronanCBNoteOn
extern ronanCBNoteOff
extern ronanCBSetCtl
extern ronanCBProcess
extern ronanCBSetSR

; rbp: this
syRonanInit:
	Pushad
	call ronanCBInit
	Popad
	ret

; rbp: this
syRonanNoteOn:
	Pushad
	call ronanCBNoteOn
	Popad
	ret

; rbp: this
syRonanNoteOff:
	Pushad
	call ronanCBNoteOff
	Popad
	ret

; rbp: this
syRonanTick:
	Pushad
	call ronanCBTick
	Popad
	ret

; rbp: this
; rsi: buffer
; rcx: count
syRonanProcess:
	Pushad
	V2PerfEnter V2Perf_RONAN
	sub     rsp, 8
	mov     [rsp - 8], ecx
	mov     [rsp - 4], esi
	call    ronanCBProcess
	V2PerfLeave V2Perf_RONAN
	Popad
	ret


%endif

;#####################################################################################
; Oberdings
; SOUNDDEFINITIONEN FÜR DEN WELTFRIEDEN
;#####################################################################################

struc v2sound
	; voice dependent sachen

	.voice:			resb syVV2_size/4
	.endvdvals:

	; globale pro-channel-sachen
	.chan:				resb syVChan_size/4
	.endcdvals:

	; manuelles rumgefake
	.maxpoly:		resb 1

	; modmatrix
	.modnum:			resb 1
	.modmatrix:
endstruc

struc v2mod
	.source:   resb 1   ; source: vel/ctl1-7/aenv/env2/lfo1/lfo2
	.val:      resb 1   ; 0 = -1 .. 128=1
	.dest:     resb 1   ; destination (index into v2sound)
endstruc



;#####################################################################################
; Oberdings
; ES GEHT LOS
;#####################################################################################

section .bss

struc CHANINFO
	.pgm:      resb 1
	.ctl:      resb 7
endstruc

struc SYN
	.patchmap: resd 1
	.mrstat:   resd 1
	.curalloc: resd 1
	.samplerate: resd 1
	.chanmap:  resd POLY
	.allocpos: resd POLY
	.chans:    resb 16*CHANINFO_size
	.voicemap: resd 16

	.tickd:    resd 1

	.voicesv:  resb POLY*syVV2_size
	.voicesw:  resb POLY*syWV2_size

	.chansv:   resb 16*syVChan_size
	.chansw:   resb 16*syWChan_size

	.globalsstart:
	.rvbparm:  resb syVReverb_size
	.delparm:  resb syVModDel_size
	.vlowcut:  resd 1
	.vhighcut: resd 1
	.cprparm:  resb syVComp_size
	.guicolor: resb 1			; CHAOS gui logo color (has nothing to do with sound but should be saved with patch)
	.globalsend:

	.reverb:   resb syWReverb_size
	.delay:    resb syWModDel_size
	.compr:    resb syWComp_size
	.lcfreq:   resd 1
	.lcbufl:   resd 1
	.lcbufr:   resd 1
	.hcfreq:   resd 1
	.hcbufl:   resd 1
	.hcbufr:   resd 1

%ifdef VUMETER
	.vumode:   resd 1
	.chanvu:   resd 2*16
	.mainvu:   resd 2
%endif
endstruc

global _DATA_
_DATA_:
data:		   resb SYN_size


section .text


;-------------------------------------------------------------------------------------
; Init

global synthInit
synthInit:
	Pushad

%ifdef RONAN
	movsxd rax, esi
	push rax
	call ronanCBSetSR
%endif

	xor rax, rax
	mov rcx, SYN_size
	lea rdi, [rel data]
	rep stosb

	movsxd rax, esi
	lea r8, [rel data]
	mov [r8 + SYN.samplerate], rax
	call calcNewSampleRate

	mov rax, rdi
	mov [r8 + SYN.patchmap], rax

	mov cl, POLY
	lea rbp, [r8 + SYN.voicesw]
.siloop1:
	call	syV2Init
	lea		rbp, [rbp+syWV2_size]
	mov   dword [r8 + SYN.chanmap - 4 + 4*rcx], -1
	dec   rcx
	jnz .siloop1

	mov cl, 0
	lea rbp, [r8 + SYN.chansw]
.siloop2:
	mov   byte [r8 + SYN.chans + 8*rcx + 7], 7fh
	mov   rax, rcx
	shl   rax, 14
	lea   r9, [rel chandelbuf]
	lea   rax, [r9 + rax]
	mov   [rbp + syWChan.chrw + syWModDel.db1], rax
	add   rax, 8192
	mov   [rbp + syWChan.chrw + syWModDel.db2], rax
	mov   rax, 2047
	mov   [rbp + syWChan.chrw + syWModDel.dbufmask], rax
	call	syChanInit
	lea		rbp, [rbp+syWChan_size]
	inc   rcx
	cmp   cl, 16
	jne .siloop2

	lea		rbp, [r8 + SYN.reverb]
	call  syReverbInit

	lea		rbp, [r8 + SYN.delay]
	lea   rax, [rel maindelbuf]
	mov   [rbp + syWModDel.db1], rax
	add   rax, 131072
	mov   [rbp + syWModDel.db2], rax
	mov   rax, 32767
	mov   [rbp + syWModDel.dbufmask], rax
	call	syModDelInit

%ifdef RONAN
	call  syRonanInit
%endif

	lea   rbp, [r8 + SYN.compr]
	call  syCompInit

	Popad
	ret 8


;-------------------------------------------------------------------------------------
; Renderloop!

section .bss

todo   resd 2
outptr resd 2
addflg resd 1

section .text


global synthRender
synthRender:
	Pushad

	; FPUsetup...
	fstcw	[rel oldfpcw]
	mov		ax, [rel oldfpcw]
	and		ax, 0f0ffh
	or		ax, 3fh
	mov		[rel temp], ax
	finit
	fldcw	[rel temp]

	mov   [rel outptr], rdi
	mov   [rel outptr+4], rdx
	movsxd rdi, ecx
	mov   [rel addflg], rdi
	movsxd rcx, esi
	mov   [rel todo], rcx

	lea r8, [rel data]
.fragloop:
	; step 1: fragmentieren
	mov   rax, [rel todo]
	or    rax, rax
	jnz   .doit

	fldcw [rel oldfpcw] ; nix zu tun? -> ende
	Popad
	ret 16
.doit:

	; neuer Frame nötig?
	mov   rbx, [r8 + SYN.tickd]
	or    rbx, rbx
	jnz   .nonewtick

	; tick abspielen
	xor   rdx, rdx
.tickloop:
	mov		rax, [r8 + SYN.chanmap + 4*rdx]
	or		rax, rax
	js		.tlnext
	mov		rax, rdx
	imul	rax, syWV2_size
	lea   rbp, [r8 + SYN.voicesw + rax]
	call  storeV2Values
	call  syV2Tick
	; checken ob EG ausgelaufen
	mov   rax, [rbp + syWV2.aenv + syWEnv.state]
	or    rax, rax
	jnz   .stillok
	; wenn ja -> ausmachen!
	not   rax
	mov		[r8 + SYN.chanmap + 4*rdx], rax
	jmp   .tlnext
.stillok:
.tlnext:
	inc   rdx
	cmp   dl, POLY
	jne .tickloop

	xor   rbx, rbx
.tickloop2:
	call  storeChanValues
	inc   rbx
	cmp   bl, 16
	jne		.tickloop2

%ifdef RONAN
	call  syRonanTick
%endif

	; frame rendern
	mov   rcx,	[rel SRcFrameSize]
	mov   [r8 + SYN.tickd], rcx
	call  .RenderBlock

.nonewtick:

	; daten in destination umkopieren
	mov   rax, [rel todo]
	mov   rbx, [r8 + SYN.tickd]
	mov   rcx, rbx

	mov   rsi, [rel SRcFrameSize]
	sub   rsi, rcx
	shl   rsi, 3
	lea	  r8, [rel mixbuf]
	add   rsi, r8

	cmp   rax, rcx
	jge   .tickdg
	mov   rcx, rax
.tickdg:
	sub   rax, rcx
	sub   rbx, rcx
	mov   [rel todo], rax
	mov   [r8 + SYN.tickd], rbx

	mov   rdi, [rel outptr]
	mov   rbx, [rel outptr+4]
	or    rbx, rbx
	jz    .interleaved

	; separate channels
	mov   rax, [rel addflg]
	or    rax, rax
	jz    .seploopreplace

.seploopadd:
	mov rax, [rdi]
	mov rdx, [rbx]
	add rax, [rsi]
	add rdx, [rsi+4]
	lea rsi, [rsi+8]
	mov [rdi], rax
	lea rdi, [rdi+4]
	mov [rbx], rdx
	lea rbx, [rbx+4]
	dec rcx
	jnz .seploopadd
	jz  .seploopend

.seploopreplace:
	mov rax, [rsi]
	mov rdx, [rsi+4]
	lea rsi, [rsi+8]
	mov [rdi], rax
	lea rdi, [rdi+4]
	mov [rbx], rdx
	lea rbx, [rbx+4]
	dec rcx
	jnz .seploopreplace

.seploopend:
	mov [rel outptr], rdi
	mov [rel outptr+4], rbx
	jmp .fragloop


.interleaved:
	shl   rcx, 1
	rep   movsd
	mov   [rel outptr], rdi

	jmp		.fragloop



.RenderBlock:
	Pushad
	V2PerfClear
	V2PerfEnter V2Perf_TOTAL

	; clear output buffer
	mov   [rel todo+4], rcx
	lea   rdi, [rel mixbuf]
	xor   rax, rax
	shl   rcx, 1
	rep   stosd

	; clear aux1
	mov   rcx, [rel todo+4]
	lea   rdi, [rel aux1buf]
	rep   stosd

	; clear aux2
	mov   rcx, [rel todo+4]
	lea   rdi, [rel aux2buf]
	rep   stosd

	; clear aux a / b
	mov 	rcx, [rel todo+4]
	add		rcx,rcx
	mov		rax,rcx
	lea		rdi, [rel auxabuf]
	rep		stosd
	mov 	rcx,rax
	lea		rdi, [rel auxbbuf]
	rep		stosd


	; process all channels
	xor   rcx, rcx
	lea r8, [rel data]
.chanloop:
	push rcx
	; check if voices are active
	xor rdx, rdx
.cchkloop:
	cmp rcx, [r8 + SYN.chanmap + 4*rdx]
	je  .dochan
	inc rdx
	cmp dl, POLY
	jne .cchkloop
%ifdef VUMETER
	xor		rax, rax
	mov		[r8 + SYN.chanvu + 8*rcx], rax
	mov		[r8 + SYN.chanvu + 8*rcx + 4], rax
%endif
	jmp .chanend
.dochan:
	; clear channel buffer
	lea rdi, [rel chanbuf]
	mov rcx, [rel todo+4]
	shl rcx, 1
	xor rax, rax
	rep stosd

	; process all voices belonging to that channel
	xor		rdx, rdx
.vceloop:
	pop		rcx
	push	rcx
	cmp		rcx, [r8 + SYN.chanmap + 4*rdx]
	jne		.vceend

	; alle Voices rendern
	mov		rax, rdx
	imul	rax, syWV2_size
	lea   rbp, [r8 + SYN.voicesw + rax]
	mov   rcx, [rel todo+4]
	call	syV2Render

.vceend:
	inc rdx
	cmp dl, POLY
	jne .vceloop

%ifdef RONAN
	; Wenn channel 16, dann Sprachsynth!
	pop		rcx
	push	rcx
	cmp   cl, 15
	jne   .noronan

	mov		rcx, [rel todo+4]
	lea   rsi, [rel chanbuf]
	call  syRonanProcess
.noronan:
%endif

	pop		rcx
	push	rcx
	imul	rcx, syWChan_size
	lea   rbp, [r8 + SYN.chansw + rcx]
	mov		rcx, [rel todo+4]
	call	syChanProcess

%ifdef VUMETER
	; VU-Meter der Channels bearbeiten
	pop		rdx
	push	rdx
	lea   rsi, [rel chanbuf]
	lea   rdi, [r8 + SYN.chanvu + 8*rdx]
	call  .vumeter
%endif

.chanend:
	pop rcx
	inc rcx
	cmp cl, 16
	je .clend
	jmp .chanloop

.clend:


	; Reverb/Delay draufrechnen
	mov		rcx, [rel todo+4]
	lea		rbp, [r8 + SYN.reverb]
	lea		rdi, [rel mixbuf]
	call	syReverbProcess
	lea   rbp, [r8 + SYN.delay]
	call  syModDelRenderAux2Main



	; lowcut/highcut
	mov		rcx, [rel todo+4]
	lea		rsi, [rel mixbuf]
	mov   rax, [r8 + SYN.hcfreq]
	fld   dword [r8 + SYN.hcbufr]   ; <hcbr>
	fld   dword [r8 + SYN.lcbufr]   ; <lcbr> <hcbr>
	fld   dword [r8 + SYN.hcbufl]   ; <hcbl> <lcbr> <hcbr>
	fld   dword [r8 + SYN.lcbufl]   ; <lcbl> <hcbl> <lcbr> <hcbr>
.lhcloop:
	; low cut
	fld			st0                       ; <lcbl> <lcbl> <hcbl> <lcbr> <hcbr>
	fsubr		dword [rsi]								; <in-l=lcoutl> <lcbl> <hcbl> <lcbr> <hcbr>
	fld			st0                       ; <in-l> <lcoutl> <lcbl> <hcbl> <lcbr> <hcbr>
	fmul		dword [r8 + SYN.lcfreq] ; <f*(in-l)> <lcoutl> <lcbl> <hcbl> <lcbr> <hcbr>
	faddp		st2, st0                  ; <lcoutl> <lcbl'> <hcbl> <lcbr> <hcbr>
	fld     st3                       ; <lcbr> <lcoutl> <lcbl'> <hcbl> <lcbr> <hcbr>
	fsubr		dword [rsi+4]							; <in-l=lcoutr> <lcoutl> <lcbl'> <hcbl> <lcbr> <hcbr>
	fld     st0                       ; <in-l> <lcoutr> <lcoutl> <lcbl'> <hcbl> <lcbr> <hcbr>
	fmul		dword [r8 + SYN.lcfreq] ; <f*(in-l)> <lcoutr> <lcoutl> <lcbl'> <hcbl> <lcbr> <hcbr>
	faddp   st5, st0                  ; <lcoutr> <lcoutl> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fxch    st1                       ; <lcoutl> <lcoutr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	cmp     rax, 3f800000h
	je      .nohighcut
	; high cut
	fld     st3                       ; <hcbl> <lcoutl> <lcoutr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fsubp   st1, st0                  ; <inl-hcbl> <lcoutr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fld     st5												; <hcbr> <inl-hcbl> <lcoutr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fsubp   st2, st0                  ; <inl-hcbl> <inr-hcbr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fmul    dword [r8 + SYN.hcfreq] ; <f*(inl-l)> <inr-hcbr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fxch    st1                       ; <inr-hcbr> <f*(inl-l)> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fmul    dword [r8 + SYN.hcfreq] ; <f*(inr-l)> <f*(inl-l)> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fxch    st1                       ; <f*(inl-l)> <f*(inr-l)> <lcbl'> <hcbl> <lcbr'> <hcbr>
	faddp   st3, st0                  ; <f*(inr-l)> <lcbl'> <hcbl'> <lcbr'> <hcbr>
	faddp   st4, st0                  ; <lcbl'> <hcbl'> <lcbr'> <hcbr'>
	fld     st3                       ; <hcoutr> <lcbl'> <hcbl'> <lcbr'> <hcbr'>
	fld     st2                       ; <hcoutl> <hcoutr> <lcbl'> <hcbl'> <lcbr'> <hcbr'>
.nohighcut:
	fstp    dword [rsi]               ; <outr> <lcbl'> <hcbl> <lcbr'> <hcbr>
	fstp    dword [rsi+4]             ; <lcbl'> <hcbl> <lcbr'> <hcbr>
	lea			rsi, [rsi+8]

	dec rcx
	jnz		.lhcloop

	fstp  dword [r8 + SYN.lcbufl]   ; <hcbl> <lcbr> <hcbr>
	fstp  dword [r8 + SYN.hcbufl]   ; <lcbr> <hcbr>
	fstp  dword [r8 + SYN.lcbufr]   ; <hcbr>
	fstp  dword [r8 + SYN.hcbufr]   ; -

	; Kompressor
	mov		rcx, [rel todo+4]
	lea		rsi, [rel mixbuf]
	lea		rdi, [rel mixbuf]
	lea		rbp, [r8 + SYN.compr]
	call  syCompRender

.renderend:
%ifdef VUMETER
	mov		rcx, [rel todo+4]
	lea   rsi, [rel mixbuf]
	lea   rdi, [r8 + SYN.mainvu]
	call  .vumeter
%endif

	V2PerfLeave V2Perf_TOTAL
	V2PerfCopy
	Popad
	ret



%ifdef VUMETER
.vumeter:
	; rsi: srcbuf, rcx: len, rdi: &outv
	Pushad
	mov   [rel temp], rcx
	lea   r8, [rel data]
	mov		rax, [r8 + SYN.vumode]
	jnz		.vurms

	; peak vu meter
.vploop:
	mov		rax, [rsi]
	and   rax, 0x7fffffff
	cmp   rax, [rdi]
	jbe   .vplbe1
	mov   [rdi], rax
.vplbe1:
	mov		rax, [rsi+4]
	and   rax, 0x7fffffff
	cmp   rax, [rdi+4]
	jbe   .vplbe2
	mov   [rdi+4], rax
.vplbe2:
	fld   dword [rdi]
	fmul  dword [rel fcvufalloff]
	fstp  dword [rdi]
	fld   dword [rdi+4]
	fmul  dword [rel fcvufalloff]
	fstp  dword [rdi+4]
	lea   rsi,  [rsi+8]
	dec rcx
	jnz .vploop
	Popad
	ret

.vurms:
	; rms vu meter
	fldz											; <lsum>
	fldz											; <rsum> <lsum>
.vrloop:
	fld     dword [rsi]     ; <l> <rsum> <lsum>
	fmul    st0, st0        ; <l²> <rsum> <lsum>
	faddp   st2, st0        ; <rsum> <lsum'>
	fld     dword [rsi+4]   ; <r> <rsum> <lsum>
	lea			rsi, [rsi+8]
	fmul    st0, st0        ; <r²> <rsum> <lsum>
	faddp   st1, st0        ; <rsum'> <lsum'>
	dec     rcx
	jnz .vrloop               ; <rsum> <lsum>
	fld1                      ; <1> <rsum> <lsum>
	fidiv  dword [rel temp]       ; <1/fl> <rsum> <lsum>
	fmul   st2, st0           ; <1/fl> <rsum> <lout>
	fmulp  st1, st0           ; <rout> <lout>
	fxch   st1                ; <lout> <rout>
	fstp   dword [rdi] ; <rout>
	fstp  dword  [rdi+4] ; -
	Popad
	ret
%endif





; ------------------------------------------------------------------------
; PROCESS MIDI MESSAGES

global _MIDI_
_MIDI_:

section .data.rel.ro
spMTab: dq ProcessNoteOff
		dq ProcessNoteOn
		dq ProcessPolyPressure
		dq ProcessControlChange
		dq ProcessProgramChange
		dq ProcessChannelPressure
		dq ProcessPitchBend
		dq ProcessRealTime

section .text

global synthProcessMIDI
synthProcessMIDI:
	Pushad

	fstcw	[rel oldfpcw]
	mov		ax, [rel oldfpcw]
	and		ax, 0f0ffh
	or		ax, 3fh
	mov		[rel temp], ax
	finit
	fldcw	[rel temp]

	mov		rsi, rdi
.mainloop:
	xor   rax, rax
	mov   al, [rsi]
	test  al, 80h
	jz    .nocmd
	cmp   al, 0fdh  ; end of stream ?
	jne   .noend
.end:
	fldcw [rel oldfpcw]
	Popad
	ret 4
.noend:
	lea   r8, [rel data]
	mov   [r8 + SYN.mrstat], rax ; update running status
	inc   rsi
	mov		al, [rsi]
.nocmd:
	mov   rbx, [r8 + SYN.mrstat]
	mov   rcx, rbx
	and   cl, 0fh
	shr   rbx, 4
	test  bl, 8
	jz    .end
	and   rbx, 07h
	lea   r8, [rel spMTab]
	call  [r8 + 8*rbx]
	jmp   .mainloop

; rsi: ptr auf midistream (muß nachher richtig sein!)
; rcx: channel aus commandbyte


; ------------------------------------------------------------------------
; Note Off


ProcessNoteOff:
	movzx rdi, byte [rsi]  ; Note
	; Nach Voice suchen, die die Note spielt
	xor		rdx, rdx

%ifdef RONAN
	cmp		cl, 15 ; Speechsynth-Channel?
	jne		.srvloop
	Pushad
	call  ronanCBNoteOff
	Popad
%endif

.srvloop:
	cmp   rcx, [r8 + SYN.chanmap+4*rdx] ; richtiger channel?
	jne		.srvlno
	mov		rax, rdx
	imul	rax, syWV2_size
	lea   rbp, [r8 + SYN.voicesw + rax]
	cmp   rdi, [rbp + syWV2.note]
	jne   .srvlno
	test  byte [rbp + syWV2.gate], 255
	jz    .srvlno

	call  syV2NoteOff
	jmp   .end
.srvlno:
	inc   rdx
	cmp   dl, POLY
	jne .srvloop

.end:
	add rsi, byte 2
	ret



; ------------------------------------------------------------------------
; Note On

ProcessNoteOn:
	; auf noteoff checken
	mov al, [rsi+1]
	or  al, al
	jnz .isnoteon
	jmp ProcessNoteOff
.isnoteon:

%ifdef RONAN
	cmp		cl, 15 ; Speechsynth-Channel?
	jne		.noronan
	Pushad
	call  ronanCBNoteOn
	Popad
.noronan:
%endif

	movzx rax, byte [r8 + SYN.chans + 8*rcx]   ; pgmnummer
	mov   rbp, [r8 + SYN.patchmap]
	mov		rbp, [rbp + 4*rax]           ; sounddef
	add		rbp, [r8 + SYN.patchmap]

	; maximale polyphonie erreicht?
	xor rbx, rbx
	mov rdx, POLY
.chmploop:
	mov rax, [r8 + SYN.chanmap+4*rdx - 4]
	cmp rax, rcx
	jne .notthischan
	inc rbx
.notthischan:
	dec rdx
	jnz .chmploop

	cmp bl,  byte [rbp + v2sound.maxpoly] ; poly erreicht?
	jae .killvoice	; ja -> alte voice töten

	; freie voice suchen
	xor rdx, rdx
.sfvloop1:
	mov	rax, [r8 + SYN.chanmap+4*rdx]
	or	rax, rax
	js	.foundfv
	inc rdx
	cmp dl, POLY
	jne .sfvloop1

	; keine freie? gut, dann die älteste mit gate aus
	mov	rdi, [r8 + SYN.curalloc]
	xor	rbx, rbx
	xor rdx, rdx
	not rdx
.sfvloop2:
	mov		rax, rbx
	imul	rax, syWV2_size
	mov   rax, [r8 + SYN.voicesw + rax + syWV2.gate]
	or    rax, rax
	jnz   .sfvl2no
	cmp		rdi, [r8 + SYN.allocpos+4*rbx]
	jbe		.sfvl2no
	mov		rdi, [r8 + SYN.allocpos+4*rbx]
	mov		rdx, rbx
.sfvl2no:
	inc rbx
	cmp bl, POLY
	jne .sfvloop2

	or	rdx, rdx
	jns	.foundfv

	; immer noch keine freie? gut, dann die ganz älteste
	mov rax, [r8 + SYN.curalloc]
	xor	rbx, rbx
.sfvloop3:
	cmp rax, [r8 + SYN.allocpos+4*rbx]
	jbe .sfvl3no
	mov rax, [r8 + SYN.allocpos+4*rbx]
	mov rdx, rbx
.sfvl3no:
	inc rbx
	cmp bl, POLY
	jne .sfvloop3
	; ok... die nehmen wir jezz.
.foundfv:
	jmp .donoteon

.killvoice:
	; älteste voice dieses channels suchen
	mov	rdi, [r8 + SYN.curalloc]
	xor	rbx, rbx
	xor rdx, rdx
	not rdx
.skvloop2:
	cmp		rcx, [r8 + SYN.chanmap+4*rbx]
	jne		.skvl2no
	mov		rax, rbx
	imul	rax, syWV2_size
	mov   rax, [r8 + SYN.voicesw + rax + syWV2.gate]
	or    rax, rax
	jnz   .skvl2no
	cmp		rdi, [r8 + SYN.allocpos+4*rbx]
	jbe		.skvl2no
	mov		rdi, [r8 + SYN.allocpos+4*rbx]
	mov		rdx, rbx
.skvl2no:
	inc rbx
	cmp bl, POLY
	jne .skvloop2

	or	rdx, rdx
	jns	.donoteon

	; nein? -> älteste voice suchen
	mov rax, [r8 + SYN.curalloc]
	xor rbx, rbx
.skvloop:
	cmp rcx, [r8 + SYN.chanmap+4*rbx]
	jne .skvlno
	cmp rax, [r8 + SYN.allocpos+4*rbx]
	jbe .skvlno
	mov rax, [r8 + SYN.allocpos+4*rbx]
	mov rdx, rbx
.skvlno:
	inc rbx
	cmp bl, POLY
	jne .skvloop
	; ... und damit haben wir die voice, die fliegt.

.donoteon:
	; (rcx immer noch chan, rdx voice, rbp ptr auf sound)
	mov [r8 + SYN.chanmap  + 4*rdx], rcx  ; channel
	mov [r8 + SYN.voicemap + 4*rcx], rdx  ; current voice
	mov rax, [r8 + SYN.curalloc]
	mov [r8 + SYN.allocpos + 4*rdx], rax  ; allocpos
	inc dword [r8 + SYN.curalloc]

	call		storeV2Values ; Values der Voice updaten
	mov			rax, rdx
	imul		rax, syWV2_size
	lea			rbp, [r8 + SYN.voicesw + rax]
	movzx		rax, byte [rsi]    ; Note
	movzx		rbx, byte [rsi+1]  ; Velo

	call		syV2NoteOn
	add			rsi, byte 2
	ret

; ------------------------------------------------------------------------
; Aftertouch


ProcessChannelPressure:
	add rsi, byte 1
	ret


; ------------------------------------------------------------------------
; Control Change


ProcessControlChange:
	movzx rax, byte [rsi]
	or	  rax, rax        ; und auf 1-7 checken
	jz    .end
	cmp   al, 7
%ifdef FULLMIDI
	ja    .chanmode
%else
	ja    .end
%endif

	lea	  rdi, [r8 + SYN.chans + 8*rcx]
	movzx rbx, byte [rsi+1]
	mov   [rdi+rax], bl

	cmp   cl, 15 ; sprachsynth?
	jne   .end
%ifdef RONAN
	Pushad
	sub   rsp, 8
	mov   [rsp - 8], ebx
	mov   [rsp - 4], eax
	call  ronanCBSetCtl
	Popad
%endif
.end:
	add rsi,2
	ret

%ifdef FULLMIDI
.chanmode:
	cmp al, 120
	jb  .end
	ja  .noalloff

	; CC #120 : All Sound Off
	xor rdx, rdx
	lea rbp, [r8 + SYN.voicesw]
.siloop1:
	cmp   byte [r8 + SYN.chanmap - 4 + 4*rdx], cl
	jnz   .noreset
	call	syV2Init
	mov   dword [r8 + SYN.chanmap - 4 + 4*rdx], -1
.noreset:
	lea		rbp, [rbp+syWV2_size]
	inc   dl
	cmp   dl, POLY
	jnz .siloop1

	jmp short .end


.noalloff:
	cmp al, 121
	ja  .noresetcc
	; CC #121 : Reset All Controllers
	; evtl TODO, who knows

	jmp short .end

.noresetcc:
	cmp al, 123
	jb  .end
	; CC #123 : All Notes Off
	; and CC #124-#127 - channel mode changes (ignored)

%ifdef RONAN
	cmp		cl, 15 ; Speechsynth-Channel?
	jne		.noronanoff
	Pushad
	call  ronanCBNoteOff
	Popad
.noronanoff:
%endif

	xor rdx, rdx
	lea rbp, [r8 + SYN.voicesw]
.noffloop1:
	cmp   byte [r8 + SYN.chanmap - 4 + 4*rdx], cl
	jnz   .nonoff
	call  syV2NoteOff
.nonoff:
	lea		rbp, [rbp+syWV2_size]
	inc   dl
	cmp   dl, POLY
	jnz .noffloop1

	jmp .end

%endif



; ------------------------------------------------------------------------
; Program change


ProcessProgramChange:
	; check ob selbes program
	lea		rdi, [r8 + SYN.chans + 8*rcx]
	mov   al, [rsi]
	and   al, 7fh
	cmp   al, [rdi]
	jz    .sameprg
	mov   [rdi], al

	; ok, alle voices, die was mit dem channel zu tun haben... NOT-AUS!
	xor rdx, rdx
	xor rax, rax
	not rax
.notausloop:
	cmp rcx, [r8 + SYN.chanmap + 4*rdx]
	jnz .nichtaus
	mov [r8 + SYN.chanmap + 4*rdx], rax
.nichtaus:
	inc rdx
	cmp dl, POLY
	jne .notausloop

	; pgm setzen und controller resetten
.sameprg:
	mov   cl, 6
	inc		rsi
	inc		rdi
	xor   rax, rax
	rep		stosb
	ret

; ------------------------------------------------------------------------
; Pitch Bend

ProcessPitchBend:
	add rsi, byte 2
	ret


; ------------------------------------------------------------------------
; Poly Aftertouch

ProcessPolyPressure:   ; unsupported
	add rsi, byte 2
	ret


; ------------------------------------------------------------------------
; Realtime/System messages


ProcessRealTime:
%ifdef FULLMIDI
	cmp cl, 0fh
	jne .noreset

	Pushad
	lea   r8, [rel data]
	mov esi, dword [r8 + SYN.samplerate]
	mov rdi, qword [r8 + SYN.patchmap]
	call synthInit
	Popad

.noreset:
%endif
	ret


; ------------------------------------------------------------------------
; Noch wichtiger.


global synthSetGlobals
synthSetGlobals:
	Pushad
	xor   rax, rax
	xor   rcx, rcx
	mov   rsi, rdi
	lea   r8, [rel data]
	lea   rdi, [r8 + SYN.rvbparm]
	mov   cl,  (SYN.globalsend-SYN.globalsstart)/4
.cvloop:
	lodsb
	mov   [rel temp], rax
	fild  dword [rel temp]
	fstp  dword [rdi]
	lea   rdi, [rdi+4]
	dec   rcx
	jnz .cvloop

	lea   rsi, [r8 + SYN.rvbparm]
	lea   rbp, [r8 + SYN.reverb]
	call	syReverbSet
	lea   rsi, [r8 + SYN.delparm]
	lea   rbp, [r8 + SYN.delay]
	call  syModDelSet
	lea   rsi, [r8 + SYN.cprparm]
	lea   rbp, [r8 + SYN.compr]
	call  syCompSet

	fld   dword [r8 + SYN.vlowcut]
	fld1
	faddp st1, st0
	fmul  dword [rel fci128]
	fmul  st0, st0
	fstp  dword [r8 + SYN.lcfreq]
	fld   dword [r8 + SYN.vhighcut]
	fld1
	faddp st1, st0
	fmul  dword [rel fci128]
	fmul  st0, st0
	fstp  dword [r8 + SYN.hcfreq]

	Popad
	ret	4


; ------------------------------------------------------------------------
; VU retrieval

%ifdef VUMETER

global _synthSetVUMode@4
_synthSetVUMode@4:
	mov rax, [rsp + 4]
	lea r8, [rel data]
	mov [r8 + SYN.vumode], rax
	ret 4


global _synthGetChannelVU@12
_synthGetChannelVU@12:
	Pushad
	mov  rcx, [rsp + 36]
	mov  rsi, [rsp + 40]
	mov  rdi, [rsp + 44]
	lea  r8, [rel data]
	mov  rax, [r8 + SYN.chanvu + 8*rcx]
	mov  [rsi], rax
	mov  rax, [r8 + SYN.chanvu + 8*rcx + 4]
	mov  [rdi], rax
	Popad
	ret 12

global _synthGetMainVU@8
_synthGetChannelVU@8:
	Pushad
	mov  rsi, [rsp + 36]
	mov  rdi, [rsp + 40]
	lea  r8, [rel data]
	mov  rax, [r8 + SYN.mainvu]
	mov  [rsi], rax
	mov  rax, [r8 + SYN.mainvu+4]
	mov  [rdi], rax
	Popad
	ret 8



%endif


; ------------------------------------------------------------------------
; Debugkram

global _synthGetPoly@4

_synthGetPoly@4:
	Pushad
	mov rcx, 17
	mov rdi, [rsp + 36]
	xor rax, rax
	rep stosd

	mov rdi, [rsp + 36]
	lea r8, [rel data]
.gploop:
	mov rbx, [r8 + SYN.chanmap + 4*rax]
	or  rbx, rbx
	js  .gplend
	inc dword [rdi + 4*rbx]
	inc dword [rdi + 4*16]
.gplend:
	inc rax
	cmp al, POLY
	jne .gploop

	Popad
	ret 4


global _synthGetPgm@4

_synthGetPgm@4:
	Pushad
	mov rdi, [rsp + 36]
	xor rcx, rcx

	lea r8, [rel data]
.gploop:
	movzx rax, byte [r8 + SYN.chans + 8*rcx]
	stosd
	inc rcx
	cmp cl, 16
	jne .gploop

	Popad
	ret 4


global _synthGetFrameSize@0
_synthGetFrameSize@0:
	mov rax, [rel SRcFrameSize]
	ret
