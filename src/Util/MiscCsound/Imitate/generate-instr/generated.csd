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

f1 0 8192 -10  169.94388 0.23825379 27.94899 0.8613151 4.92976 10.067612 1.8358104 2.2521307 1.098522 0.7919057 2.5529485 0.93741953 0.29170862 0.5310269 0.42303148 0.081544496 0.4154857 1.0642828 0.5917385 0.74362385 0.7701477 0.2602286 0.3055252 0.1049764 0.50489265 0.3968584 0.2736311 0.554534 0.61732835 1.5738274 0.0173732 0.0319601 0.024268499 0.033541497 0.0718598 0.0316053 0.0315514 0.0590267
f2 0 8192 -10  794.35834 0.063258186 62.62861 1.5072132 8.47305 21.072989 2.5716114 4.0362263 2.990633 3.333343 4.9399114 1.6098135 0.61946905 0.5346478 0.9085542 0.03318881 0.4473465 2.3581996 2.083093 0.5536772 0.73078203 0.7538634 0.6157306 0.4956946 0.79049635 0.5829026 0.23932652 0.87931854 0.71349436 1.3964328 0.10054153 0.039259285 0.038399667 0.06308019 0.040193386 0.059099335 0.055308137 0.033204716
f3 0 8192 -10  9269.228 110.710266 1193.736 34.01677 32.44 50.18304 8.107274 4.593121 5.2187366 3.009254 6.0064836 2.4492383 3.3467555 0.62305576 4.2631946 0.2410593 2.6709073 1.3459032 3.0576649 0.2501684 2.0370746 0.29358914 2.2936614 0.7173789 1.4275568 0.34654504 1.6301749 1.026906 1.6749274 1.8910713 0.3247621 0.008418975 0.15199673 0.0033336666 0.036163192 0.0065468573 0.019821668 0.002899905
f4 0 8192 -10  18630.547 691.5416 8443.6875 376.56287 681.3876 616.81226 55.181255 65.32353 50.56917 16.681961 9.506939 6.4392953 5.44562 1.4285594 3.5857904 2.4013133 2.7616065 1.6781397 2.2223768 1.205776 2.0114577 0.39073867 2.7278805 0.8770755 1.201982 0.5068473 1.474429 1.1650558 1.6611941 1.9145266 0.25820082 0.021168346 0.31804493 0.015894344 0.15298362 0.020356929 0.10637909 0.025565619
f5 0 8192 -10  25291.04 1243.2019 12104.169 900.7453 3357.9487 1703.2754 212.4046 210.12274 597.90393 145.47742 29.981283 37.0876 20.478285 14.186187 19.338621 4.284606 3.6081338 5.4064884 5.5234466 9.003669 4.6754494 2.7676404 6.6112876 2.4088125 2.3332915 2.119168 2.0507004 2.5310674 3.2368057 2.5251904 0.43488136 0.16023536 0.34263456 0.10713917 0.2656912 0.09691027 0.21670248 0.10941524
f6 0 8192 -10  26999.527 1538.8971 12117.928 1205.997 5619.708 1772.7837 412.7782 465.5919 462.32718 411.42773 110.34799 22.154345 16.6214 13.170188 33.524086 23.058626 10.223653 10.670639 11.511477 24.768957 12.496736 5.037631 11.26561 9.086978 4.1338005 10.246602 13.255222 6.12284 5.9704456 5.186284 1.3119109 0.3794325 0.6569367 0.23342907 0.60482806 0.19878933 0.46412152 0.20389889
f7 0 8192 -10  18248.697 1370.907 16906.283 1282.7865 3605.2332 1676.2894 616.4637 281.1827 201.14879 340.96246 206.11987 40.141182 15.987989 14.427053 12.43704 31.07458 29.987736 12.794411 8.168296 5.0981855 7.251206 5.0004244 7.0400257 5.1363187 3.9599247 3.425003 3.3472848 3.1426005 3.6570177 3.309008 1.0495495 0.44916973 0.6721137 0.22218448 0.50819683 0.17466742 0.49632165 0.20470265

i1 0.1 1.99 1000.0 261.66 0 0 0 0.5
0.002 0.01 
0.04 0.08 0.16 0.32 0.64 0.99 
1 2 3 4 5 6 7 

i3000 0 2.49 0 1 0.8 5000 0.1


</CsScore>
</CsoundSynthesizer>
