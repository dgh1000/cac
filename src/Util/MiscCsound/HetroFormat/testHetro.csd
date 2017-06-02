<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>
sr=44100
ksmps=10
nchnls=2

gi_sin ftgen 0, 0, 8192, 10, 1

instr 1

kt timeinstk
kt = kt/kr
aout adsyn 1, 1, 1, "pno-C4-fixed.het"

   outs aout,aout


endin

</CsInstruments>
<CsScore>

i1 0 6


</CsScore>
</CsoundSynthesizer>
