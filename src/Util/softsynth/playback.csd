
<CsoundSynthesizer>

<CsOptions>
; desktop:
;   dac1 is realtek digital optical output
;   M3  is internal midi pipe 03
;
; laptop:
;   dac7 is iFi driver (the one that works)
;   M2  is internal midi input 03
;  
; -+rtmidi=virtual is virtual midi keyboard
-odac1 

</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 44100
ksmps  = 1
nchnls = 2

instr 1

  Sfile      = "\\Users\\Mike\\out.wav"
  ilen       filelen Sfile
  p3         = ilen

  ainL, ainR diskin2 Sfile

  outs ainL, ainR

endin

</CsInstruments>

<CsScore>

i1 0 10

</CsScore>

</CsoundSynthesizer>
