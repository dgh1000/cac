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
;  d:      table size (repeated)
; 

<CsOptions>
; desktop:
;   -odac1 is realtek digital optical output
;   -M3  is internal midi pipe 03
;
; laptop:
;   -odac7 is iFi driver
;   -M2  is internal midi input 03
;  
; -+rtmidi=virtual is virtual midi keyboard
-odac7  -realtime

</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 4410
ksmps  = 10
nchnls = 2

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

  idur        = p3
  iamp        = p4
  ipitch      = p5
  icps        = p6
  isampDur    = 3.0
  isampOffset = 2.0
  isampBegT   = (ipitch-21)*isampDur + isampOffset
  itableSize  = 16777216

  ; compute iphaseBeg and iaccessFreq
  itableDur   = itableSize/sr
  iaccessFreq = 1/itableDur
  ; iphaseBeg is # of samples into the table proportional to table size
  iphaseBeg   = (isampBegT+isampOffset)*sr/itableSize

  ; compute hammer signal
  aphs        phasor iaccessFreq, iphaseBeg
  aidx        = aphs * itableSize
  ahammer     tablei aidx, 1  

  ; debugging sin signal
  asin        oscili 10000, 1000

  ; compute hammer envelope
  aenv        linseg 1, 4/icps, 1, 3/sr, 0

  ; waveguide
  a0point5    init 0.5
  asig        modern_waveguide aenv*ahammer/4, icps, 6.0, a0point5

  ; envelope
  iattack     = 0.002
  itail       = 0.1

              outs asin, asin

endin


</CsInstruments>

<CsScore>

f0 3
f1 0 16777216 -1 "c:\\Users\\Mike\\samples\\pink\\pink-samples.wav" 0.0 0 0

i1 0.0 0.1 10000 60 256
i1 1.0 0.1 10000 60 256


</CsScore>

</CsoundSynthesizer>
