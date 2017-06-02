<CsoundSynthesizer>
<CsOptions>
-oout.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

gi_sin ftgen 0, 0, 8192, 10, 1


instr 1

asig  adsyn 1, 2, 1, "c4.het"

      outs asig, asig

endin

; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

i1 0.1 2.0


</CsScore>
</CsoundSynthesizer>
