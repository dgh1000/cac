<CsoundSynthesizer>

;
; printf specifiers
;  s:      output device name
;  d:      midi input device number
;  .3f:    sample duration
;  .3f:    sample begin offset (to get past the buildup phase of the
;           waveguide)
;  d:      table size
; 

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
-odac7 -realtime -M2

</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 44100
ksmps  = 1
nchnls = 2


; ----------------------------------------------------------------------

instr 1

  ipitch      = 0
  ivel        = 0
              midinoteonkey ipitch, ivel
  ;icps        pow 2, (ipitch-69)/12
  ;icps        = 440 * icps
  
  ; compute hammer envelope
  ;aenv        linseg 1, 10/icps, 1, 3/sr, 0
  ;ahammer     oscili 5000, icps

  ; a0point5    init 0.5
  ; asig        streson aenv*ahammer/32, icps, 0.999
  ; asig        modern_waveguide aenv*ahammer/32, icps, 10.0, a0point5

  asig        oscili 5000, icps
  kenv        linsegr 0, 0.05, 1, 0.05, 0

              outs asig*kenv, asig*kenv
              ; zawm asine*kenv + asig, 1
              ; zawm asine*kenv + asig, 2
endin




</CsInstruments>

<CsScore>

; f1 0 16777174 -1 "c:\\Users\\Mike\\samples\\pink\\pink-samples-short.wav" 0.0 0 0
; i1000 0 3600

f0 3600

</CsScore>

</CsoundSynthesizer>
