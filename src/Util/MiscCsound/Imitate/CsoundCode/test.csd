<CsoundSynthesizer>
<CsOptions>
-d -o/Temp/CSoundOutput/out.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

instr 1
idur = p3
iamp = p4
ifreq = p5
itab = p6

asig oscili iamp, ifreq, itab
kenv linseg 0, 0.1, 1, idur-0.2, 1, 0.1, 0
  outs asig*kenv, asig*kenv

endin


</CsInstruments>
<CsScore>

f1 0 8192 10 17868.17200 674.91110 8456.40400 405.95395 735.44880 702.11633 59.31177 86.62862 32.10477 19.78334 13.67779 14.06354 12.13909 4.91981 4.91340 5.20044 3.31704 4.77846 3.70688 2.46914 3.10557 2.43337 4.44665 3.35152 2.49654 2.85618 2.03228 1.90452 3.61181 4.12304 0.71657



i1 0.1 5.0 10000 260 1


</CsScore>
</CsoundSynthesizer>
