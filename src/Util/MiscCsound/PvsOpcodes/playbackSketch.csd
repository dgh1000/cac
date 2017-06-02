
<CsoundSynthesizer>
<CsOptions>
-oout.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=10
nchnls=2

gi_sin ftgen 0, 0, 4096, 10, 1

instr 1

ifund = 262

imsPerSample = 5.0
itablen = ftlen(1)
iT = itablen * imsPerSample / 1000
iphasor_freq = 1.0 / iT

aphs phasor iphasor_freq

;    tablei <index>, <table>, <normalized>
aamp1 tablei aphs, 1, 1
asig1 oscili aamp1, 1*ifund, gi_sin
aamp2 tablei aphs, 2, 1
asig2 oscili aamp2, 2*ifund, gi_sin


aout = asig1 + asig2 + 

  outs aout, aout

endin



</CsInstruments>
<CsScore>

i1 

</CsScore>
</CsoundSynthesizer>
