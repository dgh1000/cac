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

%s
%s
i3000 0 %f 0 1 0.8 5000 0.1


</CsScore>
</CsoundSynthesizer>
