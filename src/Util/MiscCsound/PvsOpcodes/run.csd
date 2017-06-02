<CsoundSynthesizer>
<CsOptions>
-o/Temp/CSoundOutput/out.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2

gi_sin ftgen 0, 0, 8192, 10, 1


instr 1

asig1 init 0
asig2 init 0
   fin "../HetroFormat/pno-C4-trim.wav", 0, 0, asig1, asig2

; I guess this is 513 bins?
fsig  pvsanal asig1, 1024, 256, 1024, 0
asyn  pvsadsyn fsig, 513, 2.0

   outs asyn, asyn
endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 2

inbins = 512
gi_a ftgen 0, 0, inbins, 10, 1
gi_f ftgen 0, 0, inbins, 10, 1

asig1 init 0
asig2 init 0
ktim init 0
   fin "/giga/vienna/05-Solo-violin/VI_PERF-LEGATO-waves/VI_mV_sus_mp_G#3.wav", 0, 0, asig1, asig2
asig = (asig1+asig2)/2


; I guess this is 513 bins?
;  overlap = 256, an
fsig  pvsanal asig, 1024, 256, 1024, 0
kflag pvsftw fsig, gi_a, gi_f


if kflag==0 kgoto contin

ktim  timeinstk
     SfileNameA   sprintfk "/Temp/csound/tables/a%06d.txt", ktim
     SfileNameF   sprintfk "/Temp/csound/tables/f%06d.txt", ktim
     ; printf "============%s\n", ktim, SfileName
     ftsavek SfileNameA, ktim, 1, gi_a
     ftsavek SfileNameF, ktim, 1, gi_f
contin:


  outs asig, asig

endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

i2 0.1 0.2


</CsScore>
</CsoundSynthesizer>
