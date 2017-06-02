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
  idur           = p3
  iamp           = p4
  ifreq          = p5
  ipitch         = p6
  iatt           = p7
  idecayDbSec    = p8
  irel           = p9


  ipan          = $SCALE(0'1'-10'ipitch'130')
  isinTab       = 1

  ; ----------------------------------------------------------------------
  ; filtering and envelopes
  ; ----------------------------------------------------------------------
 
  ; envelope on attack amplitude 
  kattEnv       linseg 0, iatt, 1



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
; myfilter_env
;
;   implements high pass and low pass filters to simulate a piano tone
;   and exponential decay
;
opcode pianoFilter, a, aiiiiiiiii

  asig, idur, iatt, ihpCo1, ihpCo2, idecayDbSec, ilpCo1, ilpDecOctSec, itail, \ 
               ilpEnd xin


  ; ----------------------------------------------------------------------
  ; filtering and envelopes
  ; ----------------------------------------------------------------------

 
  ; attack amplitude and cutoff
  kattEnv      linseg 0, iatt, 1, idur, 1
  khpCo        expseg ihpCo1, iatt, ihpCo2, idur, ihpCo2
  
  ; decay ampl and cutoff
  idecayTime   = idur-iatt-itail
  itemp1       pow 10, idecayDbSec * idecayTime / 20
  idecay2      = 1/itemp1
  kdecayEnv    expseg 1, iatt, 1, idecayTime, idecay2, idur, idecay2
  itemp2       pow 2, idecayTime*ilpDecOctSec
  ilpCo2       = ilpCo1/itemp2
  klpCo        expseg ilpCo1, iatt, ilpCo1, idecayTime, ilpCo2, \
                      itail, ilpEnd
  
  ; tail ampl
  ktailEnv     linseg 1, iatt+idecayTime, 1, itail, 0

  ; combined ampl env
  kamplEnv      = kattEnv*kdecayEnv*ktailEnv
  
  ; high pass to create nice attack
  asig          buthp asig, khpCo
  asig          buthp asig, khpCo

  ; low pass to create nice decay
  asig          tone  asig, klpCo

                xout kamplEnv*asig
endop


; ----------------------------------------------------------------------
;   waveform source can be oscbnk or simple oscili depending on which I
;   uncomment
; instr 7
;
;
;idur             = p3
;iampl            = p4
;ifreq            = p5
;ipitch           = p6
;iatt             = p7
;ihpCo1           = p8        ; high pass cutoff at note beginning
;ihpCo2           = p9        ; high pass cutoff at end of attack
;idecayDbSec      = p10       ; ampl decay in dB/sec
;ilpCo1           = p11       ; low pass cutoff at note beginning
;ilpDelta1        = p12
;ilpCo2           = p13
;ilpDecOctSec     = p14       ; rate of low pass change in octave/sec
;itail            = p15
;ilpEnd           = p16       ; low pass cutoff at very end of note
;imidiPitPerTable = p17       ; each table covers this many midi pitches
;
;
; compute tables and pan
;itab          = imidiPitPerTable*ceil(ipitch/imidiPitPerTable)
;ipan          = $SCALE(0'1'-10'ipitch'130')
;isinTab       = 1
;
;
;asig        pianoFilter asig, idur, iatt, ihpCo1, ihpCo2, idecayDbSec, ilpCo1,\
;                        ilpDecOctSec, itail, ilpEnd
;asig        = asig*iampl/4
;
;            zawm asig, 1
;            zawm asig, 2
;endin
;

