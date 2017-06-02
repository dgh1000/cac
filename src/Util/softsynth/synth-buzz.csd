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
-W -o\Users\Mike\out.wav
</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 4410
ksmps  = 10
nchnls = 2

#define SCALE(b1' b2' a1' am' a2') #(($b2 - $b1)*($am - $a1)/($a2 - $a1)+$b1)#

zakinit 2, 2

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

  ; table numbers
  icosineTab        = 1
  ipartialsRatioTab = 2
  iampScaleTab      = 3
  ilowpassCutMinTab = 4
  ihamCyclesTab     = 5
  iattackTab        = 6

  ; important configuration
  iampMin     = 2000
  iampMax     = 20000

  ; midi pitch and velocity. this works with either a MIDI-activated instance
  ; of this instrument, or an i statement
  ipitch      = p5
  ivel        = p4
              midinoteonkey ipitch, ivel
  icps        pow 2, (ipitch-69)/12
  icps        = 440*icps

  ; compute ampl
  iampLog     = $SCALE( log(iampMin) ' log(iampMax) ' 0 ' ivel ' 127 ') 
  iampScale   tablei ipitch, iampScaleTab, 0
  iamp        = exp(iampLog)*iampScale*0.5


  ; compute hammer signal
  ipartialsRatio tablei ipitch, ipartialsRatioTab, 0
  ahammer     gbuzz iamp, icps, sr/icps/2 - 1, 1, ipartialsRatioTab, icosineTab

  ; compute hammer envelope
  ihamCycles  tablei ipitch, ihamCyclesTab, 0
  print ihamCycles
  ihamDur     = ihamCycles/icps
  aenv        linseg 0, 0.1*ihamDur, 1, 0.8*ihamDur, 1, 0.1*ihamDur, 0

  ; waveguide
  a0point5    init 0.5
  asig        modern_waveguide aenv*ahammer, icps, 6.0, a0point5
  asig        dcblock asig

  ; envelope
  kampEnv     linsegr 0, 0.001, 1, 0.15, 0

  asig        = asig* kampEnv

              zawm asig, 1
              zawm asig, 2
endin

; ----------------------------------------------------------------------
; mixer
;
instr 1000
  
  asigL        zar 1
  asigR        zar 2
  ifeedback    = 0.85
  irvbCutoff   = 5000
  irvbPitchMod = 0.2
  iwet         = 0.3
  arvbL, arvbR reverbsc asigL, asigR, ifeedback, irvbCutoff, sr, irvbPitchMod
  aclipL       clip iwet*arvbL + (1-iwet)*asigL, 0, 24000
  aclipR       clip iwet*arvbR + (1-iwet)*asigR, 0, 24000

               zacl 1, 2
               outs aclipL, aclipR

endin

 </CsInstruments>

<CsScore>

;  icosineTab        = 1
f1 0 16384 11 1

;  ipartialsRatioTab = 2
;                0      12      24      36      48      60
;               72      84      96     108     128
f2 0 128   -7 0.80 12 0.80 12 0.80 12 0.80 12 0.80 12 0.80 12 \
              0.80 12 0.80 12 0.80 12 0.80 20 0.80

;  iampScale         = 3
;                0      12      24      36      48      60
;               72      84      96     108     128
f3 0 128   -7 1.00 12 1.00 12 1.00 12 1.00 12 1.00 12 1.00 12 \
              1.00 12 1.00 12 1.00 12 1.00 20 1.00 

;  ilowpassCutMin    = 4
f4 0 128   -7 5000  127 5000

;  ihamCyclesTab     = 5
f5 0 128   -7 2     127 2

;  iattackTab        = 6
f6 0 128   -7 0.001 127 0.001

; i1 t dur vel pitch 
i1   0   1  64    60

i1000 0 2

</CsScore>
 
</CsoundSynthesizer>
