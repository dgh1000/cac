<CsoundSynthesizer>
<CsOptions>
-o/Temp/tmp.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=1
nchnls=2


; ----------------------------------------------------------------------
;    notes on specific parameters for files
; for clarinet C4, have fundamental 256
;    ifftsize = 2*sr/F = 344
;    ioverlap = 86
;    iwinsize = 344
;    itablesize 


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
instr 2

; ------------------------------
; To configure, set
; 
;   ifftsize
;     - ifftsize/2 + 1 is number of bins, from fundamental to Nyquist
;     - must be even
;     -  IF WE WANT TO GRAB HARMONIC STRUCTURE
;       for a note with fundamental F, let's say I want to have two fft bins
;       per harmonic just for resolution, so thats 
;           2*sr/2/F = sr/F bins      approx= iffsize/2
;       so  iffsize = 2*sr/F
;
;     - IF WE WANT TO UNDERSTAND EVOLUTION OF FREQUENCY CONTENT OVER TIME
;       ON A FINE-GRAINED LEVEL, NEED SMALLER FFT
;         
;         
;   ioverlap
;      - determines underlying analysis rate as sr/overlap
;          duration of frame is overlap/sr
;      - needs to be at least ifftsize/4
;      - for analysis frames M seconds apart, we need M =  overlap/sr
;             or overlap = M*sr
;             fftsize = 4 * overlap
;   iwinsize
;      - must be at least fftsize. Let's say that'll be it.
;      
;   itablesize
;      - must be at least number of FFT bins (i.e. ifftsize/2+1)
;   Sinput
; - AND SET OUTPUT DIRECTORY BELOW
; - AND SET DURATION OF ANALYSIS
; 


ifftsize = %d
; ioverlap needs to be at least ifftsize/4
ioverlap = %d
; iwinsize will equal ifftsize
iwinsize = %d
; size of the allocated tables for storing pvsanal output
itablesize = %d
; ----------------------------------------
; input sound file
; "/giga/vienna/18-Clarinet-Bb/KLB_PERF-LEGATO-waves/KLB_pA_sus_mp_C4.wav"
Sinput = "%s"
; ----------------------------------------
; output directory
;  "/Temp/csound/tables/clarinet-C4"
Sout = "%s"
;

gi_a ftgen 0, 0, itablesize, 10, 1
gi_f ftgen 0, 0, itablesize, 10, 1

asig1 init 0
asig2 init 0
ktim init 0
   fin Sinput, %d, 0, asig1, asig2

; what should go here is
;  "asig = (asig1+asig2)/2"  : to average channels, maybe not good idea if
;                              recording is not mono-compatible
; OR "asig = asig1"          : to pick left channel
; OR "asig = asig2"          : to pick right channel
%s


;   this uses wintype = Hmming window
fsig  pvsanal asig, ifftsize, ioverlap, iwinsize, 0
kflag pvsftw fsig, gi_a, gi_f


if kflag==0 kgoto contin

ktim  timeinstk
     ; in the following  is the directory for holding table dumps
     ;  ================================================================
     ;    ALSO SET OUTPUT DIRECTORY HERE
     ;  ================================================================
     SfileNameA   sprintfk "%%s/a%%06d.txt", Sout, ktim
     SfileNameF   sprintfk "%%s/f%%06d.txt", Sout, ktim
     ftsavek SfileNameA, ktim, 1, gi_a
     ftsavek SfileNameF, ktim, 1, gi_f
contin:


endin


; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

</CsInstruments>
<CsScore>

; is the duration of the analysis
i2 0 %f


</CsScore>
</CsoundSynthesizer>
