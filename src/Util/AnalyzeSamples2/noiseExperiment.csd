<CsoundSynthesizer>
<CsOptions>
-o/Temp/tmp.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

instr 1

iamp = p4

ar rand 1
ap pinkish ar, 1

ibase_freq = 250
iband_width = 10
inum = 20
isep = 1
isepmode=1 ; separation mode
iscl = 1   ; scale all peak responses 
afilt resony ap, ibase_freq, iband_width, inum, isep, isepmode, iscl
ares resony afilt, ibase_freq, iband_width, inum, isep, isepmode, iscl

aout = ares * iamp
; aout = ap
  outs aout, aout

endin


instr 2

an rand 10000
outs an, an

endin

; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

; is the duration of the analysis
i1 0 5 10000

;i2 0 300



</CsScore>
</CsoundSynthesizer>
