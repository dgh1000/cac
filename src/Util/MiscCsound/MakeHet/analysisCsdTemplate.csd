<CsoundSynthesizer>
<CsOptions>

</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 2

ifftsize = %d
; ioverlap needs to be at least ifftsize/4
ioverlap = %d
; iwinsize will equal ifftsize
iwinsize = %d
itablesize = %d
gi_a ftgen 0, 0, itablesize, 10, 1
gi_f ftgen 0, 0, itablesize, 10, 1

asig1 init 0
asig2 init 0
ktim init 0
   fin "%s", 0, 0, asig1, asig2
asig = (asig1+asig2)/2


;   
fsig  pvsanal asig, ifftsize, ioverlap, iwinsize, 0
kflag pvsftw fsig, gi_a, gi_f


if kflag==0 kgoto contin

ktim  timeinstk
     ; in the following  is the directory for holding table dumps
     SfileNameA   sprintfk "%s/a%%06d.txt", ktim
     SfileNameF   sprintfk "%s/f%%06d.txt", ktim
     ftsavek SfileNameA, ktim, 1, gi_a
     ftsavek SfileNameF, ktim, 1, gi_f
contin:


endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

; is the duration of the analysis
i2 0.1 %f


</CsScore>
</CsoundSynthesizer>
