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
-f -d -W -o\Users\Mike\out.wav
</CsOptions>

<CsInstruments>

sr     = 44100
kr     = 4410
ksmps  = 10
nchnls = 2

#define SCALE(b1' b2' a1' am' a2') #(($b2 - $b1)*($am - $a1)/($a2 - $a1)+$b1)#

zakinit 2, 2

gisinTable  ftgen 1, 0, 16384, 10, 1
; gicosTable  ftgen 3, 0, 16384, 11, 1
; #include "/haskell/Util/softsynth/tables.txt"

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
;  oscbnk signal source
; ----------------------------------------------------------------------
opcode oscbnko, a, ii

  ifreq, itab xin

  ; For reference: oscbnk documentation
  ;
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
  ;
  ;ares oscbnk  kcps, kamd, kfmd, kpmd, iovrlap, iseed, kl1minf, kl1maxf, \
  ;      kl2minf, kl2maxf, ilfomode, keqminf, keqmaxf, keqminl, keqmaxl, \
  ;      keqminq, keqmaxq, ieqmode, kfn [, il1fn] [, il2fn] [, ieqffn]   \
  ;      [, ieqlfn] [, ieqqfn] [, itabl] [, ioutfn]
  

  ; assigning arguments of oscbnk
  iamd        = 0.001  ; am depth
  ifmd        = 0      ; fm depth
  ipmd        = 0.001  ; phase modulation depth
  iovrlp      = 16     ; # oscils
  iseed       = -1
  kl1minf     = 5
  kl1maxf     = 6
  kl2minf     = 4.0
  kl2maxf     = 6.0
  ilfomode    = 64 + 2 ; LFO1 -> ampl, LFO2 -> phase
  ; eq config: mnot using
  ieqmode     = -1
  asig       oscbnk ifreq,iamd,ifmd,ipmd,iovrlp,iseed,kl1minf,kl1maxf,kl2minf,\
                    kl2maxf,ilfomode,0,0,0,0,0,0,ieqmode,itab,1,1

              xout asig

endop


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; 
;   single oscillator: no filters, simple ampl envelope
;
instr 7

  idur           = p3
  iampl          = p4
  ifreq          = p5
  ipitch         = p6
  iatt           = p7
  idecayDbSec    = p8
  irelease       = p9
  itab           = p10

  
  asig           oscil iampl, ifreq, itab
  kattEnv        linseg 0, iatt, 1
  idecayDur      = idur-iatt-itail
  iexpPoint      = 1/pow(10, idecayDur*idecayDbSec/20)
  kdecayEnv      expseg 1, iatt, 1, idecayDur, iexpPoint
  krelEnv        linseg 1, idur-iatt, 1, irelease, 0
  asig           = asig*iampl*kattEnv*kdecayEnv*krelEnv
  ipan           = $SCALE(0'1'-10'ipitch'130')
  asigL, asigR   pan2 asig, ipan
                 zawm asigL, 1
                 zawm asigR, 2
endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
;   single oscillator, so it has harmonic partials.
;     
;   tries to simulate piano sound through envelopes and filters
;
instr 8


  idur             = p3
  iampl            = p4
  ifreq            = p5
  ipitch           = p6
  iatt             = p7
  idecayDbSec      = p8        ; ampl decay in dB/sec during entire decay
  ilpCo1           = p9        ; low pass cutoff at note beginning
  ilpCo2           = p10
  ilpPhase1Dur     = p11
  ilpDecOctSec     = p12       ; rate of low pass decrease in octave/sec
  irel             = p13       ; release time
  ilpEnd           = p14       ; low pass cutoff at very end of release
  imidiPitPerTable = p15       ; each table covers this many midi pitches

  
  ; compute tables and pan from ipitch
  itab          = imidiPitPerTable*ceil(ipitch/imidiPitPerTable)
  ipan          = $SCALE(0'1'-10'ipitch'130')


  ; ----------------------------------------------------------------------
  ; signal source: single oscili on table
  ; ----------------------------------------------------------------------
  asig          oscili 1, ifreq, itab


  ; ----------------------------------------------------------------------
  ; signal source: vco2
  ;
  ; ----------------------------------------------------------------------
  ; documentation
  ;   ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]
  ;       ^
  ;       imode = 4 is sawtooth
  ;
  ;   ares vco xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] \
  ;    [, iphs] [, iskip]
  ; 
  ;       ^ iwave=1 is sawtooth
  ;       

  ; asig          vco2 1, ifreq, 4, 0.01

  ; ----------------------------------------------------------------------
  ; signal source: oscbnk
  ; ----------------------------------------------------------------------
  ; asig            oscbnko, ifreq, itab
  ; asig            = asig/5

  ; ----------------------------------------------------------------------
  ; filtering and envelopes
  ; ----------------------------------------------------------------------
 
  ; envelope on attack amplitude 
  kattEnv       linseg 0, 0.01, 1

  ; envelope on decay amplitude
  idecayTime    = idur-iatt-irel
  idecay2       = 1/pow(10, idecayDbSec*idecayTime/20)
  kdecayEnv     expseg 1, iatt, 1, idecayTime, idecay2

  ; envelope on release amplitude
  krelEnv       linseg 1, idur-irel, 1, irel, 0

  ; combined ampl env
  kamplEnv      = kattEnv*kdecayEnv*krelEnv

  ; envelop on lowpass cutoff,  used in decay and release
  iphase2Time   = idecayTime-ilpPhase1Dur
  ilpCo3        = ilpCo2/pow(2,iphase2Time*ilpDecOctSec)
  klpCo         expseg ilpCo1, iatt, ilpCo1, ilpPhase1Dur, ilpCo2, \
                       iphase2Time, ilpCo3, irel, ilpEnd

  ; low pass to create decay in timbre brightness
  asig          tone  asig, klpCo

  ; final computations and output
  asig          = asig*iampl*kamplEnv
  asigL, asigR  pan2 asig, ipan

                zawm asigL, 1
                zawm asigR, 2
endin

; ----------------------------------------------------------------------
;  instr 9: single sine wave with jitter

instr 9

  idur          = p3
  iampl         = p4
  ifreq         = p5
  ipitch        = p6
  iatt          = p7
  idecayDbSec   = p8
  irel          = p9
  ifreqJit      = p10        ; freq jitter amplitude in hertz
  iamplJit      = p11        ; ampl jitter as fraction of nominal amplitude
  ijitCpsMin    = p12
  ijitCpsMax    = p13
  itestHpCo     = p14

  ; compute pan
  ipan =        $SCALE(0'1'-10'ipitch'130')

  ; compute frequency and ampl jitter
  kamplJit_     jitter iamplJit,  ijitCpsMin, ijitCpsMax
  kamplJit      = 1+kamplJit_
  kfreqJit      jitter ifreqJit, ijitCpsMin, ijitCpsMax

  asig          oscili 1, ifreq+kfreqJit, 1
  kattEnv       linseg 0, iatt, 1
  idecayTime    = idur-iatt-irel
  idecay2       = 1/pow(10, idecayDbSec*idecayTime/20)
  kdecayEnv     expseg 1, iatt, 1, idecayTime, idecay2
  krelEnv       linseg 1, idur-iatt, 1, irel, 0
  kamplEnv      = kattEnv*kdecayEnv*krelEnv
  asig          = asig*iampl*kamplEnv
  ;  asig          buthp asig, itestHpCo
  asigL, asigR  pan2 asig, ipan

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

</CsInstruments>

<CsScore>

%s

</CsScore>
 
</CsoundSynthesizer>
