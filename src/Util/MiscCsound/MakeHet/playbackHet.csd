<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

gi_sin ftgen 0, 0, 8192, 10, 1


instr 1

ifile = p4

asig  adsyn 1, 1, 1, ifile

      outs asig, asig

endin

; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

i1 0.1 4.0 "/Temp/csound/hetFiles/pno60.het"


</CsScore>
</CsoundSynthesizer>
