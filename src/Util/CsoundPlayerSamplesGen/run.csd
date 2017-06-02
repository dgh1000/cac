<CsoundSynthesizer>
<CsOptions>
-d -W -op48.wav
</CsOptions>
<CsInstruments>


sr=44100
ksmps=10
nchnls=1

; i1 itime idur itab
instr 1
itab = p4
   ; if ftlen=sr then it takes 1 second to get through table
   ; as ftlen gets bigger should take longer
aout   oscili 1, sr/ftlen(itab), itab
  out aout
endin


</CsInstruments>



<CsScore>
f1 0 131072 -1 "c:/Temp/ariaOutput/viola/pinitial48.wav" 3.0 0 1
i1 0 1.0 1

</CsScore>
</CsoundSynthesizer>
