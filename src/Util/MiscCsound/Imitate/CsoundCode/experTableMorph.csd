<CsoundSynthesizer>
<CsOptions>
-d -o/Temp/CSoundOutput/out.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

#define SCALE(xmin'x'xmax'ymin'ymax) # $ymin + ( $ymax - $ymin ) * ($x - $xmin) / ($xmax - $xmin) #

instr 1
idur = p3
iamp = p4
ifreq = p5
itab1 = p6
itab2 = p7
itab3 = p8

it1 = 1.0
it2 = 2.0


ik_cycles_1 = it1 * kr
ik_cycles_2 = it2 * kr

ktime timeinstk

if ktime < ik_cycles_1 then
  kfrac = $SCALE(0'ktime'ik_cycles_1'0'1)
  ktab1_mult = 1 - kfrac
  ktab2_mult = kfrac
  ktab3_mult = 0
elseif ktime < ik_cycles_2 then
  kfrac = $SCALE(ik_cycles_1'ktime'ik_cycles_2'0'1)
  ktab1_mult = 0
  ktab2_mult = 1 - kfrac
  ktab3_mult = kfrac
else
  ktab1_mult = 0
  ktab2_mult = 0
  ktab3_mult = 1
  
endif

asig1 oscili 1, ifreq, itab1
asig2 oscili 1, ifreq, itab2
asig3 oscili 1, ifreq, itab3

aout = asig1*ktab1_mult + asig2*ktab2_mult + asig3*ktab3_mult

kenv linseg 0, 0.1, 1, idur-0.2, 1, 0.1, 0

   outs kenv*iamp*aout, kenv* iamp*aout

endin

</CsInstruments>
<CsScore>

f1 0 8192 10 1
f2 0 8192 10 1 0.5 0.25 0.12 0.06
f3 0 8192 10 1 1 1 1 1 1 1

i1 0.1 5.0 10000 260 1 2 3

</CsScore>
</CsoundSynthesizer>
