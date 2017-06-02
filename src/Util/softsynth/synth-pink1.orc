; feeding pink noise into a waveguide

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
-d -odac7 -realtime -M2 -b256 -B2048

</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 4410
ksmps  = 10
nchnls = 2

zakinit 2, 2

; Instrument:modern_waveguide
; ------------------------------------------------------------
; User-defined opcode implmenting a wave guide model.
; ------------------------------------------------------------
;
; aout modern_waveguide ain, ifreq, idec, kpkpos
;
;  ifreq: frequency
;  idec: decary factor in dB/sec
;  apkpos: pick-up position (assuming a string)
;
opcode modern_waveguide, a, aiia

setksmps 1

/* now we combine the modern waveguide model
with the ideas developed with the KS model */

ain, ifun, idec, apkpos xin

ipi = -4*taninv(-1)

idts = sr/ifun      /* total delay time (samples) */
idtt = int(sr/ifun) /* truncated delay time */
idel = idts/sr      /* delay time (secs) */

 
ifac init 1          /* decay shortening factor (fdb gain) */
is   init 0.5        /* loss filter coefficient */

igf pow 10, -idec/(20*ifun) /* gain required for a certain decay */
ig  = cos(ipi*ifun/sr)      /* unitary gain with s=0.5 */

if igf > ig igoto stretch /* if decay needs lengthening */
ifac = igf/ig             /* if decay needs shortening */
goto continue

stretch:             
icosfun = cos(2*ipi*ifun/sr)
ia = 2 - 2*icosfun
ib = 2*icosfun - 2
ic = 1 - igf*igf 
id = sqrt(ib*ib - 4*ia*ic)
is1 = (-ib + id)/(ia*2)
is2 = (-ib - id)/(ia*2)
is = (is1 < is2 ? is1 : is2) 

continue:
ax1  init 0         /* filter delay variable */
apx1  init 0         /* allpass fwd delay variable */
apy1  init 0         /* allpass fdb delay variable */

idtt = ((idtt+is) > (idts) ? idtt - 1: idtt)
ifd = (idts - (idtt + is))  /* fractional delay */
icoef = (1-ifd)/(1+ifd)  /* allpass coefficient */

atmp    delayr 1.0

adel    deltapn idtt

   aflt = (adel*(1-is) + ax1*is)*ifac /* LP filter   */
   ax1 = adel
   alps  = icoef*(aflt - apy1) + apx1  /* AP filter  */
   apx1 = aflt
   apy1 = alps

;;;      tablew alps, awp, itab1, 0 ,0 ,1  

atap1   deltapn    (1-apkpos) * idtt / 2
atap2   deltapn    apkpos * idtt / 2
        delayw alps + ain

       aout = atap1 + atap2
   xout aout
endop

; ----------------------------------------------------------------------

instr 1

  ; configuration
  iampRatio   = 0.1
  isampOffset = 5.0

  ipitch      = 0
  ivel        = 0
              midinoteonkey ipitch, ivel
  icps        pow 2, (ipitch-69)/12
  icps        = 440 * icps

  ; compute iphaseBeg and iaccessFreq
  itableDur   = ftlen(ipitch)/sr
  iaccessFreq = 1/itableDur
  ; iphaseBeg is # of samples into the table proportional to table size
  iphaseBeg   = isampOffset*sr/ftlen(ipitch)

  ; compute hammer signal
  aphs        phasor iaccessFreq, iphaseBeg
  aidx        = aphs * ftlen(ipitch)
  ahammer     tablei aidx, ipitch

  ; compute hammer envelope
  iwavelen    = 1/icps
  aenv        linseg 0, iwavelen, 1, iwavelen, 1, iwavelen, 0

  ; waveguide
  a0point5    init 0.5
  asig        modern_waveguide aenv*ahammer, icps, 10.0, a0point5
  asig        dcblock asig

  ; envelope
  kampEnv     linsegr 0, 0.002, 1, 0.15, 0
  kampEnv     = kampEnv*iampRatio


              zawm asig*kampEnv, 1
              zawm asig*kampEnv, 2
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
  iwet         = 0.25
  arvbL, arvbR reverbsc asigL, asigR, ifeedback, irvbCutoff, sr, irvbPitchMod
  aclipL       clip iwet*arvbL + (1-iwet)*asigL, 0, 24000
  aclipR       clip iwet*arvbR + (1-iwet)*asigR, 0, 24000

               zacl 1, 2
               outs aclipL, aclipR

endin



</CsInstruments>

<CsScore>

; f1 0 16777216 -1 "c:\\Users\\Mike\\samples\\pink\\pink-samples-long.wav" 0.0 0 0



i1000 0 3600

f21 0 262144 -1 "c:\Users\Mike\samples\pink\pink21.wav" 0.0 0 0
f22 0 262144 -1 "c:\Users\Mike\samples\pink\pink22.wav" 0.0 0 0
f23 0 262144 -1 "c:\Users\Mike\samples\pink\pink23.wav" 0.0 0 0
f24 0 262144 -1 "c:\Users\Mike\samples\pink\pink24.wav" 0.0 0 0
f25 0 262144 -1 "c:\Users\Mike\samples\pink\pink25.wav" 0.0 0 0
f26 0 262144 -1 "c:\Users\Mike\samples\pink\pink26.wav" 0.0 0 0
f27 0 262144 -1 "c:\Users\Mike\samples\pink\pink27.wav" 0.0 0 0
f28 0 262144 -1 "c:\Users\Mike\samples\pink\pink28.wav" 0.0 0 0
f29 0 262144 -1 "c:\Users\Mike\samples\pink\pink29.wav" 0.0 0 0
f30 0 262144 -1 "c:\Users\Mike\samples\pink\pink30.wav" 0.0 0 0
f31 0 262144 -1 "c:\Users\Mike\samples\pink\pink31.wav" 0.0 0 0
f32 0 262144 -1 "c:\Users\Mike\samples\pink\pink32.wav" 0.0 0 0
f33 0 262144 -1 "c:\Users\Mike\samples\pink\pink33.wav" 0.0 0 0
f34 0 262144 -1 "c:\Users\Mike\samples\pink\pink34.wav" 0.0 0 0
f35 0 262144 -1 "c:\Users\Mike\samples\pink\pink35.wav" 0.0 0 0
f36 0 262144 -1 "c:\Users\Mike\samples\pink\pink36.wav" 0.0 0 0
f37 0 262144 -1 "c:\Users\Mike\samples\pink\pink37.wav" 0.0 0 0
f38 0 262144 -1 "c:\Users\Mike\samples\pink\pink38.wav" 0.0 0 0
f39 0 262144 -1 "c:\Users\Mike\samples\pink\pink39.wav" 0.0 0 0
f40 0 262144 -1 "c:\Users\Mike\samples\pink\pink40.wav" 0.0 0 0
f41 0 262144 -1 "c:\Users\Mike\samples\pink\pink41.wav" 0.0 0 0
f42 0 262144 -1 "c:\Users\Mike\samples\pink\pink42.wav" 0.0 0 0
f43 0 262144 -1 "c:\Users\Mike\samples\pink\pink43.wav" 0.0 0 0
f44 0 262144 -1 "c:\Users\Mike\samples\pink\pink44.wav" 0.0 0 0
f45 0 262144 -1 "c:\Users\Mike\samples\pink\pink45.wav" 0.0 0 0
f46 0 262144 -1 "c:\Users\Mike\samples\pink\pink46.wav" 0.0 0 0
f47 0 262144 -1 "c:\Users\Mike\samples\pink\pink47.wav" 0.0 0 0
f48 0 262144 -1 "c:\Users\Mike\samples\pink\pink48.wav" 0.0 0 0
f49 0 262144 -1 "c:\Users\Mike\samples\pink\pink49.wav" 0.0 0 0
f50 0 262144 -1 "c:\Users\Mike\samples\pink\pink50.wav" 0.0 0 0
f51 0 262144 -1 "c:\Users\Mike\samples\pink\pink51.wav" 0.0 0 0
f52 0 262144 -1 "c:\Users\Mike\samples\pink\pink52.wav" 0.0 0 0
f53 0 262144 -1 "c:\Users\Mike\samples\pink\pink53.wav" 0.0 0 0
f54 0 262144 -1 "c:\Users\Mike\samples\pink\pink54.wav" 0.0 0 0
f55 0 262144 -1 "c:\Users\Mike\samples\pink\pink55.wav" 0.0 0 0
f56 0 262144 -1 "c:\Users\Mike\samples\pink\pink56.wav" 0.0 0 0
f57 0 262144 -1 "c:\Users\Mike\samples\pink\pink57.wav" 0.0 0 0
f58 0 262144 -1 "c:\Users\Mike\samples\pink\pink58.wav" 0.0 0 0
f59 0 262144 -1 "c:\Users\Mike\samples\pink\pink59.wav" 0.0 0 0
f60 0 262144 -1 "c:\Users\Mike\samples\pink\pink60.wav" 0.0 0 0
f61 0 262144 -1 "c:\Users\Mike\samples\pink\pink61.wav" 0.0 0 0
f62 0 262144 -1 "c:\Users\Mike\samples\pink\pink62.wav" 0.0 0 0
f63 0 262144 -1 "c:\Users\Mike\samples\pink\pink63.wav" 0.0 0 0
f64 0 262144 -1 "c:\Users\Mike\samples\pink\pink64.wav" 0.0 0 0
f65 0 262144 -1 "c:\Users\Mike\samples\pink\pink65.wav" 0.0 0 0
f66 0 262144 -1 "c:\Users\Mike\samples\pink\pink66.wav" 0.0 0 0
f67 0 262144 -1 "c:\Users\Mike\samples\pink\pink67.wav" 0.0 0 0
f68 0 262144 -1 "c:\Users\Mike\samples\pink\pink68.wav" 0.0 0 0
f69 0 262144 -1 "c:\Users\Mike\samples\pink\pink69.wav" 0.0 0 0
f70 0 262144 -1 "c:\Users\Mike\samples\pink\pink70.wav" 0.0 0 0
f71 0 262144 -1 "c:\Users\Mike\samples\pink\pink71.wav" 0.0 0 0
f72 0 262144 -1 "c:\Users\Mike\samples\pink\pink72.wav" 0.0 0 0
f73 0 262144 -1 "c:\Users\Mike\samples\pink\pink73.wav" 0.0 0 0
f74 0 262144 -1 "c:\Users\Mike\samples\pink\pink74.wav" 0.0 0 0
f75 0 262144 -1 "c:\Users\Mike\samples\pink\pink75.wav" 0.0 0 0
f76 0 262144 -1 "c:\Users\Mike\samples\pink\pink76.wav" 0.0 0 0
f77 0 262144 -1 "c:\Users\Mike\samples\pink\pink77.wav" 0.0 0 0
f78 0 262144 -1 "c:\Users\Mike\samples\pink\pink78.wav" 0.0 0 0
f79 0 262144 -1 "c:\Users\Mike\samples\pink\pink79.wav" 0.0 0 0
f80 0 262144 -1 "c:\Users\Mike\samples\pink\pink80.wav" 0.0 0 0
f81 0 262144 -1 "c:\Users\Mike\samples\pink\pink81.wav" 0.0 0 0
f82 0 262144 -1 "c:\Users\Mike\samples\pink\pink82.wav" 0.0 0 0
f83 0 262144 -1 "c:\Users\Mike\samples\pink\pink83.wav" 0.0 0 0
f84 0 262144 -1 "c:\Users\Mike\samples\pink\pink84.wav" 0.0 0 0
f85 0 262144 -1 "c:\Users\Mike\samples\pink\pink85.wav" 0.0 0 0
f86 0 262144 -1 "c:\Users\Mike\samples\pink\pink86.wav" 0.0 0 0
f87 0 262144 -1 "c:\Users\Mike\samples\pink\pink87.wav" 0.0 0 0
f88 0 262144 -1 "c:\Users\Mike\samples\pink\pink88.wav" 0.0 0 0
f89 0 262144 -1 "c:\Users\Mike\samples\pink\pink89.wav" 0.0 0 0
f90 0 262144 -1 "c:\Users\Mike\samples\pink\pink90.wav" 0.0 0 0
f91 0 262144 -1 "c:\Users\Mike\samples\pink\pink91.wav" 0.0 0 0
f92 0 262144 -1 "c:\Users\Mike\samples\pink\pink92.wav" 0.0 0 0
f93 0 262144 -1 "c:\Users\Mike\samples\pink\pink93.wav" 0.0 0 0
f94 0 262144 -1 "c:\Users\Mike\samples\pink\pink94.wav" 0.0 0 0
f95 0 262144 -1 "c:\Users\Mike\samples\pink\pink95.wav" 0.0 0 0
f96 0 262144 -1 "c:\Users\Mike\samples\pink\pink96.wav" 0.0 0 0
f97 0 262144 -1 "c:\Users\Mike\samples\pink\pink97.wav" 0.0 0 0
f98 0 262144 -1 "c:\Users\Mike\samples\pink\pink98.wav" 0.0 0 0
f99 0 262144 -1 "c:\Users\Mike\samples\pink\pink99.wav" 0.0 0 0
f100 0 262144 -1 "c:\Users\Mike\samples\pink\pink100.wav" 0.0 0 0
f101 0 262144 -1 "c:\Users\Mike\samples\pink\pink101.wav" 0.0 0 0
f102 0 262144 -1 "c:\Users\Mike\samples\pink\pink102.wav" 0.0 0 0
f103 0 262144 -1 "c:\Users\Mike\samples\pink\pink103.wav" 0.0 0 0
f104 0 262144 -1 "c:\Users\Mike\samples\pink\pink104.wav" 0.0 0 0
f105 0 262144 -1 "c:\Users\Mike\samples\pink\pink105.wav" 0.0 0 0
f106 0 262144 -1 "c:\Users\Mike\samples\pink\pink106.wav" 0.0 0 0
f107 0 262144 -1 "c:\Users\Mike\samples\pink\pink107.wav" 0.0 0 0
f108 0 262144 -1 "c:\Users\Mike\samples\pink\pink108.wav" 0.0 0 0


</CsScore>

</CsoundSynthesizer>
