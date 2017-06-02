
<CsoundSynthesizer>
<CsOptions>
-o/Temp/tmp.wav -W
</CsOptions>
<CsInstruments>
sr=44100
kr=44100
nchnls=2

instr 1

asig oscili 1, 261, 1

   outs asig, asig

endin


instr 2

iamp = p4
ifreq = p5
itab = p6

; table is 1,000,000 and frequency is 100 and sr is 100
; we need to take 1000000/100 seconds to read through table
; which means in one second we real 100/1000000

asig    oscili iamp, sr/ftlen(itab), itab
     outs asig, asig

endin


instr 3

iamp=p4
ifreq=p5
itab=p6
iwinfn=p7

ipitch=sr/ftlen(itab)
idensity=200
iscale_output = 0.1
iampoff=iamp/2
ipitchoff = 0
igdur = 0.1
imax_gdur= 0.1

aout grain iamp, ipitch, idensity, iampoff, ipitchoff, igdur, itab, iwinfn, imax_gdur

aout = iscale_output * aout

aout reson aout, 200, 100, 1


outs aout, aout

endin

</CsInstruments>
<CsScore>

f1 0 2048 -10  5718.645  6484.318  1741.5564  2357.1042  722.65015  500.05124  187.5016  290.75287  636.075  351.63657  176.69778  127.949615  170.22527  210.70197  105.32256  85.487564  201.1774  65.030624  24.013815  50.77111  25.208242  15.160026  9.76137  16.706898  14.026427  14.223672  2.339703  3.7383406  1.2511394  2.170452  1.8473132  1.0478064  1.0235987  1.0639712  0.6536059  1.262177  1.1761456  0.33649313  0.27036732  0.1271469  0.3018482  0.31211647  0.2750303  0.005313  0.16623624  0.6877967  0.1246109  0.14106825  0.23068494  0.16745237  0.057630993  0.030808562  0.030098794  0.05492271  0.09358192  0.0083057145  0.06666826 

f2 0 131072 1 "c:/Temp/source.wav" 0 0 1
f3 0 8192 20 1 1

; i1 0 4
; i2 0 4 10000 261 2
i3 0 4 10000 261 2 3

</CsScore>


</CsoundSynthesizer>