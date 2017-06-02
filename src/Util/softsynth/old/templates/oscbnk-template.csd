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
; -odac7 -realtime -M2 -b512 -B2048
-d -W -o\Users\Mike\out.wav
</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 4410
ksmps  = 10
nchnls = 2



#define SCALE(b1' b2' a1' am' a2') #(($b2 - $b1)*($am - $a1)/($a2 - $a1)+$b1)#

zakinit 2, 2


instr 1

; kcps:       fundamental frequency
; kamd:       AM depth (0-1). only has effect is ilfomode is set to modulate
;             amplitude
; kfmd:       FM depth (in Hz)
; kpmd:       phase modulation depth
; iovrlap:    number of osillator units
; iseed:      seed value. put -1 to seed from current time
; kl1minf:    LFO1 min and max freq in Hz
; kl1maxf:
; kl2minf:    LFO2 min and max freq in Hz
; kl2maxf:
; ilfomode:   using bit fields, choose routing of LFO1 and LFO2. I imagine
;             I will modulate amplitude and phase, putting LFO1 to ampl and
;             LFO2 to phase, which means 64 + 2 = 66. can also send LFO signal
;             to EQ. Not sure what that means. So, not specifying EQ bands
;             means having just one randomized EQ band?
; keqminf:    parametric eq min and max freqnecy
; keqmaxf:
; keqminl:    parametric eq min and max level
; kdqmaxl:
; keqminq:    parametric eq min and max q
; keqmaxq:
; ieqmode:    parametric eq mode. again, I think this means one band on each 
;             oscillator (not using tables). set to -1 to disable
; kfn:        oscillator waveform table
; il1fn:      (optional, needed if using LFO's) LFO1 fn table
; il2fn:      (optional) LFO2 fn table
; ieqffn:     (following three optional) lookup tables for EQ
; ieqlfn
; ieqqfn:
; itabl:      (optional) fn table storing phase and frequency for all 
;             oscillators
; ioutfn:     funtion table to write phase and frequency values, apparently
;             the values chosen randomly

;ares oscbnk  kcps, kamd, kfmd, kpmd, iovrlap, iseed, kl1minf, kl1maxf, \
;      kl2minf, kl2maxf, ilfomode, keqminf, keqmaxf, keqminl, keqmaxl, \
;      keqminq, keqmaxq, ieqmode, kfn [, il1fn] [, il2fn] [, ieqffn]   \
;      [, ieqlfn] [, ieqqfn] [, itabl] [, ioutfn]

idur        = p3
iamp        = p4
ifreq       = p5
itail       = p6
idecayDbSec = p7
itab        = p8
isin        = p9

; configuration
iatt           = 0.01
idecayOctSec   = 0.5     ; change in decay filter in oct/sec
itail          = 0.05
ibaseLpCutoff  = 500    ; base cutoff of decay filter

; assigning arguments of oscil
iamd        = 0.001  ; am depth
ifmd        = 0      ; fm depth
ipmd        = 0.001  ; phase modulation depth
iovrlp      = 16     ; # oscils
iseed       = -1
kl1minf     = 5
kl1maxf     = 6
kl2minf     = 4.0
kl2maxf     = 8.0
ilfomode    = 64 + 2 ; LFO1 -> ampl, LFO2 -> phase
; eq config: skipping
ieqmode     = -1
asig        oscbnk ifreq,iamd,ifmd,ipmd,iovrlp,iseed,kl1minf,kl1maxf,kl2minf,\
                   kl2maxf,ilfomode,0,0,0,0,0,0,ieqmode,itab,isin,isin

; Used in envelop computation
idecayTime  = idur-iatt-itail

; amplitude envelope: kampl
kattAmpl    linseg 0, iatt,      1, idur, 1
ktailAmpl   linseg 1, idur-itail, 1, itail, 0
ifinalDecay = 1/pow(10, idecayDbSec * idecayTime / 20)
kdecayAmpl  expseg 1, iatt, 1, idecayTime, ifinalDecay, idur, ifinalDecay
kampl       = kattAmpl*kdecayAmpl*ktailAmpl

; decay filter envelop: kcutoff
ifinalCo    = 1/pow(2, idecayOctSec * idecayTime)
kcutoff     expseg ibaseLpCutoff, iatt, ibaseLpCutoff, idecayTime, ifinalCo, \
                   idur, ifinalCo

asig        tone asig, kcutoff
asig        = asig*kampl*iamp/4

            zawm asig, 1
            zawm asig, 2
endin

; ----------------------------------------------------------------------
; mixer
;
instr 1000
  
  asigL        zar 1
  asigR        zar 2
  ifeedback    = 0.75
  irvbCutoff   = 6000
  irvbPitchMod = 0.2
  iwet         = 0.3
  arvbL, arvbR reverbsc asigL, asigR, ifeedback, irvbCutoff, sr, irvbPitchMod
  aclipL       clip iwet*arvbL + (1-iwet)*asigL, 0, 24000
  aclipR       clip iwet*arvbR + (1-iwet)*asigR, 0, 24000

               zacl 1, 2
               outs aclipL, aclipR

endin


; ----------------------------------------------------------------------

</CsInstruments>

<CsScore>
f1 0 16385 -9 1 1.000 0.0 2 0.379 4.5 3 0.215 9.0 4 0.144 13.5 5 0.105 18.0 6 0.081 22.5 7 0.066 27.0 8 0.054 31.5 9 0.046 36.0 10 0.040 40.5 11 0.035 45.0 12 0.031 49.5 13 0.028 54.0 14 0.025 58.5 15 0.023 63.0 16 0.021 67.5 17 0.019 72.0 18 0.017 76.5 19 0.016 81.0 20 0.015 85.5 21 0.014 90.0 22 0.013 94.5 23 0.012 99.0 24 0.012 103.5 25 0.011 108.0 26 0.010 112.5 27 0.010 117.0 28 0.009 121.5 29 0.009 126.0 30 0.009 130.5 31 0.008 135.0 32 0.008 139.5 33 0.007 144.0 34 0.007 148.5 35 0.007 153.0 36 0.007 157.5 37 0.006 162.0 38 0.006 166.5 39 0.006 171.0 40 0.006 175.5 41 0.006 180.0 42 0.005 184.5 43 0.005 189.0 44 0.005 193.5 45 0.005 198.0 46 0.005 202.5 47 0.005 207.0 48 0.004 211.5 49 0.004 216.0 50 0.004 220.5 51 0.004 225.0 52 0.004 229.5 53 0.004 234.0 54 0.004 238.5 55 0.004 243.0 56 0.004 247.5 57 0.003 252.0 58 0.003 256.5 59 0.003 261.0 60 0.003 265.5 61 0.003 270.0 62 0.003 274.5 63 0.003 279.0 64 0.003 283.5 65 0.003 288.0 66 0.003 292.5 67 0.003 297.0 68 0.003 301.5 69 0.003 306.0 70 0.003 310.5 71 0.003 315.0 72 0.003 319.5 73 0.002 324.0 74 0.002 328.5 75 0.002 333.0 76 0.002 337.5 77 0.002 342.0 78 0.002 346.5 79 0.002 351.0 80 0.002 355.5 
;f1 0 16385 -9 1 1.000 0.0 2 0.435 36.0 3 0.268 72.0 4 0.189 108.0 5 0.145 144.0 6 0.116 180.0 7 0.097 216.0 8 0.082 252.0 9 0.072 288.0 10 0.063 324.0 
f2 0 16385 10 1

i1 0 3 5000 110 0.08 4 1 2
i1000 0 4


</CsScore>
 
</CsoundSynthesizer>
