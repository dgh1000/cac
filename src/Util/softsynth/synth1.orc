; pink noise with waveguide decay

sr     = 44100
kr     = 44100
ksmps  = 1
nchnls = 2

#define SCALE(b1' b2' a1' am' a2') #(($b2 - $b1)*($am - $a1)/($a2 - $a1)+$b1)#

zakinit 2, 2

gisinTable  ftgen 1, 0, 16384, 10, 1
; gicosTable  ftgen 3, 0, 16384, 11, 1
#include "/haskell/Util/softsynth/tables.txt"

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
; foo_mwg_delay: opcode to provide general amplitude and filtering envelope on
;      note with modern waveguide with an initial decay before hammer
; inputs
;   ainput
;   idur
;   ifreq
;   idelay
;   iwgDecay
;   ihammerDur  :: in multiples of cycles
;   ihpAttMax
;   itail

opcode foo_mwg_delay, a, aiiiiiii

  asigIn, idur, ifreq, idelay, iwgDecay, ihammerDur, ihpAttMax, itail xin

  ; hammer envelope
  ih    = ihammerDur/ifreq
  ahenv   linseg 0, idelay, 0, ih, 1, ih, 1, ih, 0

  ; waveguide
  apos  init 0.5
  asig  modern_waveguide asigIn*ahenv, ifreq, iwgDecay, apos

  ; ----------------------------------------------------------------------
  ; attack
  
  ; construct cutoff on highpass over time
  isoundDur = idur - idelay - itail 
  ihpAtt  min ihpAttMax, isoundDur/3
  kcohp   expseg 3500, idelay, 3500, ihpAtt, 30
  asig2   buthp asig, kcohp

  ; ------------------------------------------------------------
  ; tail:
  
  ; computations

  ; construct cutoff of lowpass over time
  idur1   = idur - itail
  kcolp   expseg 15000, idur1, 15000, itail, 150
  asig2   butlp asig2, kcolp

          xout asig2
endop


; ----------------------------------------------------------------------
; foo_mwg: opcode to provide general amplitude and filtering envelope on
;      note with modern waveguide with an initial decay before hammer
; inputs
;   ainput
;   idur
;   ifreq
;   iwgDecay
;   ihammerDur  :: in multiples of cycles
;   ihpAttMax
;   itail

opcode foo_mwg, a, aiiiiii

  asigIn, idur, ifreq, iwgDecay, ihammerDur, ihpAttMax, itail xin

  ; hammer envelope
  ih    = ihammerDur/ifreq
  ahenv1  linseg 0, ih/5, 1
  ahenv2  expseg 1,ih,  1,0.01,  0.01,idur,  0.01

  ; waveguide
  apos  init 0.5
  asig  modern_waveguide asigIn*ahenv1*ahenv2, ifreq, iwgDecay, apos

  ; ----------------------------------------------------------------------
  ; attack
  
  ; construct cutoff on highpass over time
  isoundDur = idur - itail 
  ihpAtt  min ihpAttMax, isoundDur/2
  kcohp   expseg 3000, ihpAtt, 30
  asig2   buthp asig, kcohp

  ; ------------------------------------------------------------
  ; tail:
  
  ; computations

  ; construct cutoff of lowpass over time
  kcolp   expseg 20000, idur-itail, 20000, itail, 100
  asig2   butlp asig2, kcolp

  ; ----------------------------------------------------------------------
  ; env2
  itail2   = itail/5
  kenv2     linseg 0.0,0.0005,  1.0,idur-itail2-0.0005,  1.0,itail2, 0.0

          xout kenv2*asig2
endop



; ----------------------------------------------------------------------
instr 1 
  
  idur    = p3
  iamp    = p4
  ifreq   = p5
  ipitch  = p6

  ; configuration
  ihpAttMax   = 0.04
  itail       = 0.1
  iwgDecay    = 0.5
  ihammerDur  = 1
  isampDelay  = 6.05
  isampDur    = 0.5
  ipinkTable  = 1
  ipinkAmp    = 0.3
  icosTable   = 2
  ibuzzAmp    = 20000

  ; noise source and waveguide 1
  ;anoi    noise 1, 0.5
  ;apos    init 0.5
  ;awg1    modern_waveguide anoi, ifreq, 30, apos
  ;awg1    dcblock awg1

  ; signal from sound file
  ;
  ;   compute signal start time
  isampT   = isampDelay + isampDur*(ipitch-21)
  itableDur = ftlen(ipinkTable)/sr
  iaccessFreq = 1/itableDur
  ;   compute start table phase
  isampPh    = isampT/itableDur
  ;   read table
  aphs      phasor iaccessFreq, isampPh
  aidx      = aphs*ftlen(ipinkTable)
  apink      tablei aidx, 1

  ; buzz
  i1        = log(0.95)
  i2        = log(0.80)
  ipartials = exp($SCALE(i1'i2'21'ipitch'108'))
  abuzz   gbuzz ibuzzAmp, ifreq, sr/ifreq/2-1, 1, ipartials, 2

  asig    foo_mwg apink*ipinkAmp + abuzz, idur, ifreq, iwgDecay, ihammerDur, ihpAttMax, itail
  asig    = asig*iamp/40000

          zawm asig, 1
          zawm asig, 2
endin

; ----------------------------------------------------------------------
; instr 2: gbuzz with no waveguide
instr 2

  idur   = p3
  iamp   = p4
  ifreq  = p5
  ipitch = p6

  ; configuration
  idecayDur       = 1.5
  ipartialsBeg1   = 0.85
  ipartialsEnd1   = 0.70
  iampEnd         = 0.40
  iampAttack      = 0.001

  ; gbuzz partials envelope and amplitude envelope
  kpartialsEnv    expseg ipartialsBeg1, idecayDur, ipartialsEnd1
  kbuzzAmp1       expseg 1.0, idecayDur, iampEnd
  kbuzzAmp2       linseg 0.0, iampAttack, 1.0
  kbuzzAmp        = kbuzzAmp1*kbuzzAmp2

  ; signal
  asig      gbuzz kbuzzAmp, ifreq, sr/ifreq/2-1, 1, kpartialsEnv, 2

  ; ampl envelope: attack and declick
  kamp            linseg 0, 0.001, 1, idur-0.003, 1, 0.002, 0

  asig            = asig*iamp*kamp
                  zawm asig, 1
                  zawm asig, 2
endin

; ----------------------------------------------------------------------
; instr 4: tweaking the partials
instr 4

  idur        = p3
  iamp        = p4
  ifreq       = p5

  asig1       gbuzz 1,    ifreq, sr/ifreq/2-1, 1,  0.7,  2
  asig2       gbuzz 1,  ifreq, sr/ifreq/16-1, 1,   0.99, 2
  knh         = sr/ifreq/8
  ; asig2       buzz  0.1, ifreq, knh, isinTable
  
  kenv        linseg 0, 0.002, 1, idur-0.004, 1, 0.002, 0

  asig        = kenv*asig2*iamp
              zawm asig, 1
              zawm asig, 2
endin

; ----------------------------------------------------------------------
; instr 3: large waveform tables with modern waveguide
instr 3

  idur        = p3
  iamp        = p4
  ifreq       = p5
  ipitch      = p6

  itab        = 12*ceil(ipitch/12)
  itab2       = 12+itab
  aham1       oscili 1, ifreq, itab
  aham2       oscili 1, ifreq, itab2

  iwl         = 1/ifreq
  khamenv     linseg 0, 0.0001, 1, iwl-0.0002, 1, 0.0001, 0
  apos        init 0.5
  awg1        modern_waveguide aham1*khamenv, ifreq,  8, apos
  awg2        modern_waveguide aham2*khamenv, ifreq, 10, apos


  kenv        linseg 0, 0.0005, 1, idur-0.0105, 1, 0.01, 0

  asig2       = kenv*(awg1+awg2)*iamp/3

              zawm asig2, 1
              zawm asig2, 2
endin

; ----------------------------------------------------------------------
;  instr 5: waveform table, no waveguide
instr 5
  idur   = p3
  iamp   = p4
  ifreq  = p5
  ipitch = p6
  iatt   = p7
  idecay = p8
  itail  = p9

  itab          = 12*ceil(ipitch/12)
  ipan          = $SCALE(0'1'-10'ipitch'130')
  kenvAtt       linseg 0, iatt, 1
  idecayDelta   = idur-iatt-itail
  itmp          pow 10, idecay*idecayDelta/20
  kenvDecay     expseg 1.0, idecayDelta, 1/itmp
  kenvTail      linseg 1, idur-itail, 1, itail, 0
  kamp          = iamp*kenvAtt*kenvDecay*kenvTail
  asig          oscil3 kamp, ifreq, itab
  ipan          = $SCALE(0.2'0.8'21'ipitch'108')
  asigL,asigR   pan2 asig, ipan
                zawm asigL, 1
                zawm asigR, 2
endin


; ----------------------------------------------------------------------
; instr 6 : single oscili with random initial phase and frequency 
;   and amplitude jitter

instr 6

  idur          = p3
  iamp          = p4
  ifreq         = p5
  ipitch        = p6    ; midi pitch
  iatt          = p7    ; in seconds
  idec          = p8    ; in dB/second
  itail         = p9    ; in seconds
  ifreqJit      = p10   ; frequency jitter multiplier will vary between
                        ;   1-ifreqJit and 1+freqJit
  iampJit       = p11   ; amplitude jitter multiplier will vary between
                        ;   1-ampJit and 1+ampJit
  itab          = p12


  ; configuation
  ijitCpsMin    = 1
  ijitCpsMax    = 2

  
  kenvAtt       linseg 0, iatt, 1
  idecayDelta   = idur-iatt-itail
  itmp          pow 10, idec*idecayDelta/20
  kenvDecay     expseg 1.0, idecayDelta, 1/itmp
  kenvTail      linseg 1, idur-itail, 1, itail, 0
  kampJit       jitter iampJit,  ijitCpsMin, ijitCpsMax
  kampJit       = 1+kampJit
  kfreqJit      jitter ifreqJit, ijitCpsMin, ijitCpsMax
  kfreqJit      = 1+kfreqJit
  kamp          = iamp*kenvAtt*kenvDecay*kenvTail*kampJit
  kfreq         = ifreq*kfreqJit
  irandPhs      random 0.00, 0.99
  asig          oscili kamp, kfreq, itab, irandPhs

  ; panning
  ipan          = $SCALE(0.2'0.8'21'ipitch'108')
  asigL,asigR   pan2 asig, ipan

                zawm asigL, 1
                zawm asigR, 2
  ;;;           outs asigL, asigR
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
