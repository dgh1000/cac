<CsoundSynthesizer>
<CsOptions>
-d -odac
</CsOptions>
<CsInstruments>
sr=44100
kr=44100
nchnls=2

zakinit 2,2

; ----------------------------------------------------------------------
;                          Catalog
;
; #        Description
;
; 100      Piano-like


; ----------------------------------------------------------------------
;                   Global tables
;
gif_i100_cos ftgen 0, 0, 8193, 11, 1
gi100_tab_dtime ftgen 0,0,8,-2, 0.035,0.035,0.025,0.019,0.010,0.0085,0.0055,0.0055
gi100_tab_gain  ftgen 0,0,8,-2, 4.9, 4.9, 3.0, 2.1, 1.7, 1.1, 0.4, 0.2
gi100_tab_decay ftgen 0,0,8,-2, 6.6,6.6,6.6,6.6,7.8,11.1,12,12
gi100_tab_mult1 ftgen 0,0,8,-2, 0.75,0.75,0.62,0.40,0.32,0.21,0.21,0.21
gi100_tab_mult2 ftgen 0,0,8,-2,  0.95, 0.95, 0.90,0.80, 0.72,0.67,0.60,0.59
gi100_tab_mult3 ftgen 0,0,8,-2, 0.87,0.86,0.83,0.76,0.68,0.35,0.33,0.33
gi100_tab_mult4 ftgen 0,0,8,-2, 0.68,0.68,0.58,0.6,0.27,0.10,0.09


; ----------------------------------------------------------------------
;                      Opcodes
;
;
; aout modern_waveguide ain, ifreq, idec, kpkpos
;
;  ifreq: frequency
;  idec: decary factor in dB/sec
;  apkpos: pick-up position (assuming a string)
;
opcode modern_waveguide, a, aiia

setksmps 1

/* now we combine the modern waveguide model
with the ideas developed with the KS model */

ain, ifun, idec, apkpos xin

ipi = -4*taninv(-1)

idts = sr/ifun      /* total delay time (samples) */
idtt = int(sr/ifun) /* truncated delay time */
idel = idts/sr      /* delay time (secs) */

 
ifac init 1          /* decay shortening factor (fdb gain) */
is   init 0.5        /* loss filter coefficient */

igf pow 10, -idec/(20*ifun) /* gain required for a certain decay */
ig  = cos(ipi*ifun/sr)      /* unitary gain with s=0.5 */

if igf > ig igoto stretch /* if decay needs lengthening */
ifac = igf/ig             /* if decay needs shortening */
goto continue

stretch:             
icosfun = cos(2*ipi*ifun/sr)
ia = 2 - 2*icosfun
ib = 2*icosfun - 2
ic = 1 - igf*igf 
id = sqrt(ib*ib - 4*ia*ic)
is1 = (-ib + id)/(ia*2)
is2 = (-ib - id)/(ia*2)
is = (is1 < is2 ? is1 : is2) 

continue:
ax1  init 0         /* filter delay variable */
apx1  init 0         /* allpass fwd delay variable */
apy1  init 0         /* allpass fdb delay variable */

idtt = ((idtt+is) > (idts) ? idtt - 1: idtt)
ifd = (idts - (idtt + is))  /* fractional delay */
icoef = (1-ifd)/(1+ifd)  /* allpass coefficient */

atmp    delayr 1.0

adel    deltapn idtt

   aflt = (adel*(1-is) + ax1*is)*ifac /* LP filter   */
   ax1 = adel
   alps  = icoef*(aflt - apy1) + apx1  /* AP filter  */
   apx1 = aflt
   apy1 = alps

;;;      tablew alps, awp, itab1, 0 ,0 ,1  

atap1   deltapn    (1-apkpos) * idtt / 2
atap2   deltapn    apkpos * idtt / 2
        delayw alps + ain

       aout = atap1 + atap2
   xout aout
endop


; ----------------------------------------------------------------------
;                       Piano-like
;
; i100 itime idur iamp ikey icps itail
;      
instr 100

idur = p3
iamp = p4
ikey = p5
icps = p6
itail = p7

; times for ampl
idtime   tablei (ikey-19.0)/12.0, gi100_tab_dtime
igain    tablei (ikey-19.0)/12.0, gi100_tab_gain
idecay   tablei (ikey-19.0)/12.0, gi100_tab_decay
imult1   tablei (ikey-19.0)/12.0, gi100_tab_mult1
imult2   tablei (ikey-19.0)/12.0, gi100_tab_mult2
imult3   tablei (ikey-19.0)/12.0, gi100_tab_mult3
imult4   tablei (ikey-19.0)/12.0, gi100_tab_mult4

print idtime

iat0 = 0
iat1 = iat0 + idtime
iat2 = iat1 + idtime
iat3 = iat2 + idtime

; magnitudes for ampl
ia0 = 0
ia1 = 1
ia2 = 1
ia3 = 0

; magnitudes for mult factor
im0 = imult1
im1 = imult2
im2 = imult3
im3 = imult4

kmultEnv linseg im0, (iat1-iat0), im1, (iat2-iat1), im2, (iat3-iat2), im3
asig gbuzz 1, icps, sr/2/icps, 1, kmultEnv, gif_i100_cos
kampEnv linseg ia0, (iat1-iat0), ia1, (iat2-iat1), ia2, (iat3-iat2), ia3
apk init 0.5
awg modern_waveguide asig*kampEnv, icps, idecay, apk
awg dcblock awg


ktail linseg 1, idur-itail, 1, itail, 0


     zawm ktail*igain*awg*iamp/20, 1 
endin


; ----------------------------------------------------------------------
;                           Mixer
; i1 itime idur iwet idry
;
instr 300 

iwet = p4
idry = p5

         ain  zar 1
              zacl 1, 1
asigL, asigR  reverbsc ain, ain, 0.7, 10000, sr, 0.1

aoutL = idry*ain + iwet*asigL
aoutL2 dcblock aoutL 
;aoutL3 compress aoutL2, aoutL2, 0, 60, 76, 2, 0.01, 0.1, 0.05
aoutL3 clip aoutL2, 0, 30000
aoutR = idry*ain + iwet*asigR
aoutR2 dcblock aoutR
;aoutR3 compress aoutR2, aoutR2, 0, 60, 76, 2, 0.01, 0.1, 0.05
aoutR3 clip aoutR2, 0, 30000

; compress docs:
;ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook


              outs aoutL3, aoutR3
endin

</CsInstruments>
<CsScore>
i300 0.0 10.100062 0.4 0.6
i100 0.0 0.4 1184.4203 64 329.62756 0.1
i100 0.4 0.40000078 1184.4203 65 349.22824 0.1
i100 0.8000008 0.40000105 1184.4203 67 391.99542 0.1
i100 1.2000018 0.40000105 1325.8959 69 440.0 0.1
i100 1.6000029 0.40000093 1484.2704 71 493.8833 0.1
i100 2.0000038 0.39999676 1661.5621 72 523.2511 0.1
i100 2.4000006 0.39999676 1860.0283 74 587.3295 0.1
i100 2.7999973 0.39999676 2082.2014 76 659.2551 0.1
i100 3.199994 0.39999676 2330.912 77 698.4565 0.1
i100 3.5999908 0.39999676 2609.3293 76 659.2551 0.1
i100 3.9999876 0.40000534 2921.0034 74 587.3295 0.1
i100 4.399993 0.40000534 3269.9136 72 523.2511 0.1
i100 4.7999983 0.40000534 3660.4988 71 493.8833 0.1
i100 5.2000036 0.40000534 4097.739 69 440.0 0.1
i100 5.600009 0.40000534 4587.206 67 391.99542 0.1
i100 6.0000143 0.40000534 5135.1445 65 349.22824 0.1
i100 6.4000196 0.40000534 5748.5283 64 329.62756 0.1
i100 6.800025 0.40000534 6435.1797 65 349.22824 0.1
i100 7.2000303 0.40000534 7203.8574 67 391.99542 0.1
i100 7.6000357 0.40000534 8064.345 69 440.0 0.1
i100 8.000041 0.40000534 9027.616 71 493.8833 0.1
i100 8.400046 0.40000534 10105.959 72 523.2511 0.1
i100 8.800052 0.40000534 11313.098 74 587.3295 0.1
i100 9.200057 0.40000534 12664.427 76 659.2551 0.1

</CsScore>
</CsoundSynthesizer>
