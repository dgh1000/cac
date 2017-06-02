<CsoundSynthesizer>
<CsOptions>
-o/Temp/tmp.wav -W
</CsOptions>
<CsInstruments>


sr=44100
kr=44100
nchnls=2


opcode rich_oscili, aa, kkiii
 kamp, kfreq, itab, ijitamt, ijitfreq xin
 
kjit1  randi ijitamt, ijitfreq, 2
kjit2  randi ijitamt, ijitfreq, 2
kjit3  randi ijitamt, ijitfreq, 2


asig1  oscili kamp, kfreq*(1+kjit1), itab
asig2  oscili kamp, kfreq*(1+kjit2), itab
asig3  oscili kamp, kfreq*(1+kjit3), itab

aLeft = 0.2*asig1 + 0.4*asig2 + 0.4*asig3
aRight = 0.4*asig1 + 0.3*asig2 + 0.3*asig3

aLeft = asig1
aRight = asig2


   xout aLeft, aRight
      

endop


instr 1

ijitamt = 0.001
ijitfreq = 20
  asigL, asigR rich_oscili 5000, 440, 1, ijitamt, ijitfreq

 outs asigL, asigR


endin

</CsInstruments>
<CsScore>

f1 0 8192 10 1 0.5 0.3 0.1 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.03 0.03 0.02 0.01 0.01

; is the duration of the analysis
i1 0 10 1


</CsScore>
</CsoundSynthesizer>
