
<CsoundSynthesizer>
<CsOptions>
-oout.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=10
nchnls=2

gi_sin ftgen 0, 0, 8192, 10, 1

; control point every C ms
; 
; sample length L seconds
;
; 1000*L/C control points
;
; actual table length (power of two) is
;     T
;
; amount phasor has to advance, to advance one table point
;     1/T
;
; advance 1/T every C ms means advance how much per second?
;   (1/T) * (1000/C) = 1000/(T*C)
;
; how long to go from 0 to 1 in seconds?
;  T*C/1000
;
; frequency of phasor: 1000/(T*C)
;  
; 2**18 = 262144
gi_outTab ftgen 0, 0, 131072, -7, 0, 131072, 0

; center frequency 
gi_cf = 262
; milliseconds between control points
gi_contr_ms = 20


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;   instr 1 -- filter, track, and write to table
;        (this version tries to write control points every
;         C ms)
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 1

iphs_freq = 1000/(ftlen(gi_outTab) * gi_contr_ms)   ; phasor frequency

asig1 init 0
asig2 init 0
  fin "../HetroFormat/pno-C4-trim.wav", 0, 0, asig1, asig2
kenv   linseg 0, 0.1, 0, 0.001, 1, 0.001, 0

afilt reson asig1, gi_cf, gi_cf/2, 1
afilta = abs(afilt)
acnt  butterlp afilta, gi_cf/10

aidx  phasor iphs_freq

      tablew acnt, aidx, gi_outTab, 1, 0, 1

endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;   instr 2 -- filter, track, and write to table
;        (this version writes table at A-rate)
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 2


; phasor frequency -- write to table locations at sr
iphs_freq = sr/ftlen(gi_outTab)

asig1 init 0
asig2 init 0
  fin "../HetroFormat/pno-C4-trim.wav", 0, 0, asig1, asig2
kenv   linseg 0, 0.1, 0, 0.001, 1, 0.001, 0

afilt reson asig1, gi_cf, gi_cf/2, 1
afilta = abs(afilt)
acnt  butterlp afilta, gi_cf/10

aidx  phasor iphs_freq

      tablew acnt, aidx, gi_outTab, 1, 0, 1

endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; 
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;   instr 50 --- save the table
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 50

  ftsave "table.txt", 1, gi_outTab

endin

; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;  instr 100 --- playback of sine wave with amplitude following table
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 100

; osciliator frequency
ifreq = sr/ftlen(gi_outTab)

acnt oscili 1, ifreq, gi_outTab

aout     oscili acnt, gi_cf, gi_sin
        outs aout, aout
endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;  playback code-gen
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
 
</CsInstruments>
<CsScore>

i2 0 2.5

i100 3 2.5



</CsScore>
</CsoundSynthesizer>
