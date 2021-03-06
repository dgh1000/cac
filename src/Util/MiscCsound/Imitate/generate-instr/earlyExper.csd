<CsoundSynthesizer>
<CsOptions>
-o/Temp/CSoundOutput/out.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

#define SCALE(xmin'x'xmax'ymin'ymax) # $ymin + ( $ymax - $ymin ) * ($x - $xmin) / ($xmax - $xmin) #

zakinit 2, 2

instr 1

idur       = p3
iamp       = p4
ifreq      = p5
iloudChan  = p6
iloudA     = p7
iloudB     = p8
ipan       = p9


iatt       = p10
itail      = p11

;  tab0 - it1 - tab1 - it2 - tab2 - it3 - tab3 - it4 - tab4 - it5 - tab5
;  -- it6 - tab6 - idur

; -- this section: parameters of times of shifting between gen 10 tables ---
it1        = p12
it2        = p13
it3        = p14
it4        = p15
it5        = p16
it6        = p17


; -- this section: parameters giving table numbers
itab0      = p18
itab1      = p19
itab2      = p20
itab3      = p21
itab4      = p22
itab5      = p23
itab6      = p24

;  
it1_k_cycles = it1*kr
it2_k_cycles = it2*kr
it3_k_cycles = it3*kr
it4_k_cycles = it4*kr
it5_k_cycles = it5*kr
it6_k_cycles = it6*kr

asig0 oscili 1, ifreq, itab0
asig1 oscili 1, ifreq, itab1
asig2 oscili 1, ifreq, itab2
asig3 oscili 1, ifreq, itab3
asig4 oscili 1, ifreq, itab4
asig5 oscili 1, ifreq, itab5
asig6 oscili 1, ifreq, itab6

ktim timeinstk

if ktim < it1_k_cycles then
   kfrac = $SCALE(0'ktim'it1_k_cycles'0'1)
   kmult0 = 1 - kfrac
   kmult1 = kfrac
   kmult2 = 0
   kmult3 = 0
   kmult4 = 0
   kmult5 = 0
   kmult6 = 0
elseif ktim < it2_k_cycles then
   kfrac = $SCALE(it1_k_cycles'ktim'it2_k_cycles'0'1)
   kmult0 = 0
   kmult1 = 1 - kfrac
   kmult2 = kfrac
   kmult3 = 0
   kmult4 = 0
   kmult5 = 0
   kmult6 = 0
elseif ktim < it3_k_cycles then
   kfrac = $SCALE(it2_k_cycles'ktim'it3_k_cycles'0'1)
   kmult0 = 0
   kmult1 = 0
   kmult2 = 1 - kfrac
   kmult3 = kfrac
   kmult4 = 0
   kmult5 = 0
   kmult6 = 0
elseif ktim < it4_k_cycles then
   kfrac = $SCALE(it3_k_cycles'ktim'it4_k_cycles'0'1)
   kmult0 = 0
   kmult1 = 0
   kmult2 = 0
   kmult3 = 1 - kfrac
   kmult4 = kfrac
   kmult5 = 0
   kmult6 = 0
elseif ktim < it5_k_cycles then
   kfrac = $SCALE(it4_k_cycles'ktim'it5_k_cycles'0'1)
   kmult0 = 0
   kmult1 = 0
   kmult2 = 0
   kmult3 = 0
   kmult4 = 1 -kfrac
   kmult5 = kfrac
   kmult6 = 0
elseif ktim < it6_k_cycles then
   kfrac = $SCALE(it5_k_cycles'ktim'it6_k_cycles'0'1)
   kmult0 = 0
   kmult1 = 0
   kmult2 = 0
   kmult3 = 0
   kmult4 = 0
   kmult5 = 1 - kfrac
   kmult6 = kfrac
else
   kmult0 = 0
   kmult1 = 0
   kmult2 = 0
   kmult3 = 0
   kmult4 = 0
   kmult5 = 0
   kmult6 = 1
endif

aout = kmult0*asig0 + kmult1*asig1 + kmult2*asig2 + kmult3*asig3 + kmult4*asig4        + kmult5*asig5 + kmult6*asig6
kenv linseg 0, iatt, 1, idur-iatt-itail, 1, itail, 0

aout = aout*kenv*iamp/30000
aoutL, aoutR pan2 aout, ipan

     zawm aoutL, 1 
     zawm aoutR, 2

endin

; ----------------------------------------------------------------------
;                           Mixer
; i1 itime idur iwet idry
;
instr 3000 

iwet = p4
idry = p5
ifeedback = p6
ifco = p7
ipitchm = p8

         ainL  zar 1
         ainR  zar 2
              zacl 1, 2
; ifco was 10000
asigL, asigR  reverbsc ainL, ainR, ifeedback, ifco, sr, ipitchm

aoutL = idry*ainL + iwet*asigL
aoutL3 clip aoutL, 0, 30000
aoutR = idry*ainR + iwet*asigR
aoutR3 clip aoutR, 0, 30000

; compress docs:
;ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook


              outs aoutL3, aoutR3
endin

</CsInstruments>
<CsScore>

f1 0 8192 -10  32.871544 4.6627994 18.177391 1.0215905 1.5376866 3.5100327 0.3063765 0.26838702 0.8266965 0.0512725 1.0584879 0.044953 0.5612585 0.0 0.469071 0.0 0.43269497 0.0 0.443017 0.1364445 0.37788448 0.0 0.2699185 0.0 0.36462 0.04369 0.40267453 0.173123 0.186933 0.1116585 0.14330551 0.066521 0.1265645 0.065199 0.288472 0.013771 0.119744994 0.124960005 0.1223085 0.0451885 0.084662 0.0 0.10610899 0.0 0.1086635 0.0352545 0.171851 0.0234905 0.0878755 0.027408 0.058899 0.0246425 0.103966504 0.018663 0.1332475 0.150287 0.009056 0.12375001 0.095856994 0.046965502 0.093311995 0.068024 0.039079 0.092015505 0.128823 0.2449585 0.070295 0.339877 0.075861
f2 0 8192 -10  117.43469 2.9396899 47.318695 0.473464 4.1631203 2.130439 2.1450257 2.2566488 1.2607237 0.713893 0.8726387 0.142197 0.72638804 0.186132 0.45198098 0.06675234 0.36527562 0.19028366 0.36319968 0.23512232 0.26826432 0.19494434 0.4264767 0.30260068 0.13125734 0.26482967 0.18611099 0.28217968 0.16073133 0.23775534 0.20610066 0.057947334 0.17119665 0.055371 0.11018034 0.035597336 0.15175833 0.06934933 0.108995 0.06229633 0.10366999 0.07788867 0.085645325 0.024702 0.180716 0.037728 0.06366467 0.089792006 0.096527666 0.09939033 0.035386667 0.05043767 0.172572 0.112933 0.07427933 0.136389 0.009230333 0.060169667 0.125439 0.15033834 0.047926337 0.05041033 0.10817834 0.26765168 0.224128 0.23679167 0.066113 0.23253967 0.32869565
f3 0 8192 -10  157.04475 0.50729525 33.848454 1.5526958 8.910032 14.029458 2.851394 3.9484644 1.2136168 1.7780277 2.8752813 0.7204911 0.58597344 1.1771258 0.9268055 0.5213144 0.3543099 1.3872074 0.460599 0.39525545 0.69075114 0.4725871 0.42113236 0.72091246 0.69148386 0.39242643 0.52917945 0.95988953 0.7312456 1.2010874 0.104691215 0.095695995 0.073116116 0.05773745 0.08880311 0.053198557 0.04179789 0.09122934 0.026078777 0.04264833 0.07491168 0.15333323 0.06843789 0.11568333 0.098325886 0.041376114 0.067527995 0.11204367 0.08247855 0.14206089 0.04532111 0.06788345 0.065886445 0.08184356 0.11620978 0.086362556 0.031615555 0.037990227 0.088113226 0.07175588 0.090082444 0.10944422 0.08047578 0.09135533 0.22231913 0.07502844 0.15217645 0.17862177 0.24490023
f4 0 8192 -10  1277.9889 0.26282418 73.697624 2.3873167 13.697929 22.3031 5.5113616 6.997428 3.2806501 3.6383057 7.2239895 3.7261534 3.4355793 1.8528113 2.7012198 0.8233624 1.0326926 2.6872225 2.8533695 1.3420651 1.2283535 1.031328 1.2967095 1.624558 1.5390477 0.9981025 1.1089107 1.5442376 1.5610231 2.7662735 0.40868145 0.07624262 0.22490838 0.08553105 0.23897688 0.08682909 0.24345343 0.06966662 0.18452115 0.067213915 0.18033618 0.08536021 0.17320506 0.08369131 0.16674706 0.06337136 0.12538679 0.056528635 0.1027399 0.065160364 0.1316018 0.095390625 0.11477084 0.11469393 0.1239044 0.0678218 0.10036079 0.08386309 0.08373502 0.13221382 0.111334376 0.13735697 0.12945521 0.1593175 0.120002255 0.19759594 0.13070565 0.29873383 0.259118
f5 0 8192 -10  15255.288 411.7093 5187.1016 248.1085 279.61932 365.39752 32.501442 38.962795 17.325205 10.104293 11.897193 8.450608 8.156858 2.582041 6.165371 2.3794026 4.637447 3.7935424 4.479438 1.5424055 3.6684625 1.0370623 4.678376 2.2739487 2.9708483 1.0978585 2.690144 1.9100802 3.2399995 3.5437398 1.4182129 0.055085838 2.4297032 0.073515 2.3071263 0.06987655 2.179951 0.06075711 2.0220668 0.0742136 1.8882089 0.08905834 1.6689566 0.09030077 1.3482128 0.08841121 0.9844428 0.07830849 0.54233706 0.088153645 0.36647367 0.09660247 0.19912389 0.10433992 0.13868353 0.1019232 0.1131575 0.09901682 0.08649849 0.1572411 0.08329949 0.2218515 0.060354795 0.31586275 0.09349431 0.48480067 0.10316803 0.67050874 0.12005528

i1 
0.1       ; time
14        ; dur
10000     ; iamp       = p4
261       ; ifreq      = p5
0         ; iloudChan  = p6
0         ; iloudA     = p7
0         ; iloudB     = p8
0.5       ;ipan       = p9
0.1       ; iatt       = p10
0.1       ; itail      = p11
2         ; it1        = p12
4         ; it2        = p13
6         ; it3        = p14
8         ; it4        = p15
10        ; it5        = p16
12        ; it6        = p17
1         ; itab0      = p18
2         ; itab1      = p19
3         ; itab2      = p20
4         ; itab3      = p21
5         ; itab4      = p22
5         ; itab5      = p23
5         ; itab6      = p24

i3000 0 15 0 1 0.8 5000 0.1


</CsScore>
</CsoundSynthesizer>
