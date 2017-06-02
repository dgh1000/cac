<CsoundSynthesizer>
<CsOptions>
-oout.wav -W
</CsOptions>
<CsInstruments>
sr=44100
ksmps=10
nchnls=2
gi_sin ftgen 0, 0, 8192, 10, 1
gi_outTab1 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab2 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab3 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab4 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab5 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab6 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab7 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab8 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab9 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab10 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab11 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab12 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab13 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab14 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab15 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab16 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab17 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab18 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab19 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab20 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab21 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab22 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab23 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab24 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab25 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab26 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab27 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab28 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab29 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab30 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab31 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab32 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab33 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab34 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab35 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab36 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab37 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab38 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab39 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab40 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab41 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab42 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab43 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab44 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab45 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab46 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab47 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab48 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab49 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab50 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab51 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab52 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab53 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab54 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab55 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab56 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab57 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab58 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab59 ftgen 0, 0, 131072, -7, 0, 131072, 0
gi_outTab60 ftgen 0, 0, 131072, -7, 0, 131072, 0
instr 1
 asigIn1 init 0
 asigIn2 init 0
fin "pno-C4-trim.wav ", 0, 0, asigIn1, asigIn2
asigIn = asigIn1 + asigIn2
afilt1 resonx asigIn, 262.0, 8.733334, 1, 1
afilt2 resonx asigIn, 524.0, 8.733334, 1, 1
afilt3 resonx asigIn, 786.0, 8.733334, 1, 1
afilt4 resonx asigIn, 1048.0, 8.733334, 1, 1
afilt5 resonx asigIn, 1310.0, 8.733334, 1, 1
afilt6 resonx asigIn, 1572.0, 8.733334, 1, 1
afilt7 resonx asigIn, 1834.0, 8.733334, 1, 1
afilt8 resonx asigIn, 2096.0, 8.733334, 1, 1
afilt9 resonx asigIn, 2358.0, 8.733334, 1, 1
afilt10 resonx asigIn, 2620.0, 8.733334, 1, 1
afilt11 resonx asigIn, 2882.0, 8.733334, 1, 1
afilt12 resonx asigIn, 3144.0, 8.733334, 1, 1
afilt13 resonx asigIn, 3406.0, 8.733334, 1, 1
afilt14 resonx asigIn, 3668.0, 8.733334, 1, 1
afilt15 resonx asigIn, 3930.0, 8.733334, 1, 1
afilt16 resonx asigIn, 4192.0, 8.733334, 1, 1
afilt17 resonx asigIn, 4454.0, 8.733334, 1, 1
afilt18 resonx asigIn, 4716.0, 8.733334, 1, 1
afilt19 resonx asigIn, 4978.0, 8.733334, 1, 1
afilt20 resonx asigIn, 5240.0, 8.733334, 1, 1
afilt21 resonx asigIn, 5502.0, 8.733334, 1, 1
afilt22 resonx asigIn, 5764.0, 8.733334, 1, 1
afilt23 resonx asigIn, 6026.0, 8.733334, 1, 1
afilt24 resonx asigIn, 6288.0, 8.733334, 1, 1
afilt25 resonx asigIn, 6550.0, 8.733334, 1, 1
afilt26 resonx asigIn, 6812.0, 8.733334, 1, 1
afilt27 resonx asigIn, 7074.0, 8.733334, 1, 1
afilt28 resonx asigIn, 7336.0, 8.733334, 1, 1
afilt29 resonx asigIn, 7598.0, 8.733334, 1, 1
afilt30 resonx asigIn, 7860.0, 8.733334, 2, 1
afilt31 resonx asigIn, 8122.0, 8.733334, 2, 1
afilt32 resonx asigIn, 8384.0, 8.733334, 2, 1
afilt33 resonx asigIn, 8646.0, 8.733334, 2, 1
afilt34 resonx asigIn, 8908.0, 8.733334, 2, 1
afilt35 resonx asigIn, 9170.0, 8.733334, 2, 1
afilt36 resonx asigIn, 9432.0, 8.733334, 2, 1
afilt37 resonx asigIn, 9694.0, 8.733334, 2, 1
afilt38 resonx asigIn, 9956.0, 8.733334, 2, 1
afilt39 resonx asigIn, 10218.0, 8.733334, 2, 1
afilt40 resonx asigIn, 10480.0, 8.733334, 2, 1
afilt41 resonx asigIn, 10742.0, 8.733334, 2, 1
afilt42 resonx asigIn, 11004.0, 8.733334, 2, 1
afilt43 resonx asigIn, 11266.0, 8.733334, 2, 1
afilt44 resonx asigIn, 11528.0, 8.733334, 2, 1
afilt45 resonx asigIn, 11790.0, 8.733334, 2, 1
afilt46 resonx asigIn, 12052.0, 8.733334, 2, 1
afilt47 resonx asigIn, 12314.0, 8.733334, 2, 1
afilt48 resonx asigIn, 12576.0, 8.733334, 2, 1
afilt49 resonx asigIn, 12838.0, 8.733334, 2, 1
afilt50 resonx asigIn, 13100.0, 8.733334, 2, 1
afilt51 resonx asigIn, 13362.0, 8.733334, 3, 1
afilt52 resonx asigIn, 13624.0, 8.733334, 3, 1
afilt53 resonx asigIn, 13886.0, 8.733334, 3, 1
afilt54 resonx asigIn, 14148.0, 8.733334, 3, 1
afilt55 resonx asigIn, 14410.0, 8.733334, 3, 1
afilt56 resonx asigIn, 14672.0, 8.733334, 3, 1
afilt57 resonx asigIn, 14934.0, 8.733334, 3, 1
afilt58 resonx asigIn, 15196.0, 8.733334, 3, 1
afilt59 resonx asigIn, 15458.0, 8.733334, 3, 1
afilt60 resonx asigIn, 15720.0, 8.733334, 3, 1
afilt1a = abs(afilt1)
afilt2a = abs(afilt2)
afilt3a = abs(afilt3)
afilt4a = abs(afilt4)
afilt5a = abs(afilt5)
afilt6a = abs(afilt6)
afilt7a = abs(afilt7)
afilt8a = abs(afilt8)
afilt9a = abs(afilt9)
afilt10a = abs(afilt10)
afilt11a = abs(afilt11)
afilt12a = abs(afilt12)
afilt13a = abs(afilt13)
afilt14a = abs(afilt14)
afilt15a = abs(afilt15)
afilt16a = abs(afilt16)
afilt17a = abs(afilt17)
afilt18a = abs(afilt18)
afilt19a = abs(afilt19)
afilt20a = abs(afilt20)
afilt21a = abs(afilt21)
afilt22a = abs(afilt22)
afilt23a = abs(afilt23)
afilt24a = abs(afilt24)
afilt25a = abs(afilt25)
afilt26a = abs(afilt26)
afilt27a = abs(afilt27)
afilt28a = abs(afilt28)
afilt29a = abs(afilt29)
afilt30a = abs(afilt30)
afilt31a = abs(afilt31)
afilt32a = abs(afilt32)
afilt33a = abs(afilt33)
afilt34a = abs(afilt34)
afilt35a = abs(afilt35)
afilt36a = abs(afilt36)
afilt37a = abs(afilt37)
afilt38a = abs(afilt38)
afilt39a = abs(afilt39)
afilt40a = abs(afilt40)
afilt41a = abs(afilt41)
afilt42a = abs(afilt42)
afilt43a = abs(afilt43)
afilt44a = abs(afilt44)
afilt45a = abs(afilt45)
afilt46a = abs(afilt46)
afilt47a = abs(afilt47)
afilt48a = abs(afilt48)
afilt49a = abs(afilt49)
afilt50a = abs(afilt50)
afilt51a = abs(afilt51)
afilt52a = abs(afilt52)
afilt53a = abs(afilt53)
afilt54a = abs(afilt54)
afilt55a = abs(afilt55)
afilt56a = abs(afilt56)
afilt57a = abs(afilt57)
afilt58a = abs(afilt58)
afilt59a = abs(afilt59)
afilt60a = abs(afilt60)
atrack1 butterlp afilt1a, 8.733334
atrack2 butterlp afilt2a, 17.466667
atrack3 butterlp afilt3a, 26.2
atrack4 butterlp afilt4a, 34.933334
atrack5 butterlp afilt5a, 43.666668
atrack6 butterlp afilt6a, 52.4
atrack7 butterlp afilt7a, 61.133335
atrack8 butterlp afilt8a, 69.86667
atrack9 butterlp afilt9a, 78.6
atrack10 butterlp afilt10a, 87.333336
atrack11 butterlp afilt11a, 96.066666
atrack12 butterlp afilt12a, 104.8
atrack13 butterlp afilt13a, 113.53333
atrack14 butterlp afilt14a, 122.26667
atrack15 butterlp afilt15a, 131.0
atrack16 butterlp afilt16a, 139.73334
atrack17 butterlp afilt17a, 148.46666
atrack18 butterlp afilt18a, 157.2
atrack19 butterlp afilt19a, 165.93333
atrack20 butterlp afilt20a, 174.66667
atrack21 butterlp afilt21a, 183.4
atrack22 butterlp afilt22a, 192.13333
atrack23 butterlp afilt23a, 200.86667
atrack24 butterlp afilt24a, 209.6
atrack25 butterlp afilt25a, 218.33333
atrack26 butterlp afilt26a, 227.06667
atrack27 butterlp afilt27a, 235.8
atrack28 butterlp afilt28a, 244.53334
atrack29 butterlp afilt29a, 253.26666
atrack30 butterlp afilt30a, 262.0
atrack31 butterlp afilt31a, 270.73334
atrack32 butterlp afilt32a, 279.46667
atrack33 butterlp afilt33a, 288.2
atrack34 butterlp afilt34a, 296.93332
atrack35 butterlp afilt35a, 305.66666
atrack36 butterlp afilt36a, 314.4
atrack37 butterlp afilt37a, 323.13333
atrack38 butterlp afilt38a, 331.86667
atrack39 butterlp afilt39a, 340.6
atrack40 butterlp afilt40a, 349.33334
atrack41 butterlp afilt41a, 358.06668
atrack42 butterlp afilt42a, 366.8
atrack43 butterlp afilt43a, 375.53333
atrack44 butterlp afilt44a, 384.26666
atrack45 butterlp afilt45a, 393.0
atrack46 butterlp afilt46a, 401.73334
atrack47 butterlp afilt47a, 410.46667
atrack48 butterlp afilt48a, 419.2
atrack49 butterlp afilt49a, 427.93332
atrack50 butterlp afilt50a, 436.66666
atrack51 butterlp afilt51a, 445.4
atrack52 butterlp afilt52a, 454.13333
atrack53 butterlp afilt53a, 462.86667
atrack54 butterlp afilt54a, 471.6
atrack55 butterlp afilt55a, 480.33334
atrack56 butterlp afilt56a, 489.06668
atrack57 butterlp afilt57a, 497.8
atrack58 butterlp afilt58a, 506.53333
atrack59 butterlp afilt59a, 515.26666
atrack60 butterlp afilt60a, 524.0
aidx phasor sr/ftlen(gi_outTab1)
tablew atrack1, aidx, gi_outTab1, 1, 0, 1
tablew atrack2, aidx, gi_outTab2, 1, 0, 1
tablew atrack3, aidx, gi_outTab3, 1, 0, 1
tablew atrack4, aidx, gi_outTab4, 1, 0, 1
tablew atrack5, aidx, gi_outTab5, 1, 0, 1
tablew atrack6, aidx, gi_outTab6, 1, 0, 1
tablew atrack7, aidx, gi_outTab7, 1, 0, 1
tablew atrack8, aidx, gi_outTab8, 1, 0, 1
tablew atrack9, aidx, gi_outTab9, 1, 0, 1
tablew atrack10, aidx, gi_outTab10, 1, 0, 1
tablew atrack11, aidx, gi_outTab11, 1, 0, 1
tablew atrack12, aidx, gi_outTab12, 1, 0, 1
tablew atrack13, aidx, gi_outTab13, 1, 0, 1
tablew atrack14, aidx, gi_outTab14, 1, 0, 1
tablew atrack15, aidx, gi_outTab15, 1, 0, 1
tablew atrack16, aidx, gi_outTab16, 1, 0, 1
tablew atrack17, aidx, gi_outTab17, 1, 0, 1
tablew atrack18, aidx, gi_outTab18, 1, 0, 1
tablew atrack19, aidx, gi_outTab19, 1, 0, 1
tablew atrack20, aidx, gi_outTab20, 1, 0, 1
tablew atrack21, aidx, gi_outTab21, 1, 0, 1
tablew atrack22, aidx, gi_outTab22, 1, 0, 1
tablew atrack23, aidx, gi_outTab23, 1, 0, 1
tablew atrack24, aidx, gi_outTab24, 1, 0, 1
tablew atrack25, aidx, gi_outTab25, 1, 0, 1
tablew atrack26, aidx, gi_outTab26, 1, 0, 1
tablew atrack27, aidx, gi_outTab27, 1, 0, 1
tablew atrack28, aidx, gi_outTab28, 1, 0, 1
tablew atrack29, aidx, gi_outTab29, 1, 0, 1
tablew atrack30, aidx, gi_outTab30, 1, 0, 1
tablew atrack31, aidx, gi_outTab31, 1, 0, 1
tablew atrack32, aidx, gi_outTab32, 1, 0, 1
tablew atrack33, aidx, gi_outTab33, 1, 0, 1
tablew atrack34, aidx, gi_outTab34, 1, 0, 1
tablew atrack35, aidx, gi_outTab35, 1, 0, 1
tablew atrack36, aidx, gi_outTab36, 1, 0, 1
tablew atrack37, aidx, gi_outTab37, 1, 0, 1
tablew atrack38, aidx, gi_outTab38, 1, 0, 1
tablew atrack39, aidx, gi_outTab39, 1, 0, 1
tablew atrack40, aidx, gi_outTab40, 1, 0, 1
tablew atrack41, aidx, gi_outTab41, 1, 0, 1
tablew atrack42, aidx, gi_outTab42, 1, 0, 1
tablew atrack43, aidx, gi_outTab43, 1, 0, 1
tablew atrack44, aidx, gi_outTab44, 1, 0, 1
tablew atrack45, aidx, gi_outTab45, 1, 0, 1
tablew atrack46, aidx, gi_outTab46, 1, 0, 1
tablew atrack47, aidx, gi_outTab47, 1, 0, 1
tablew atrack48, aidx, gi_outTab48, 1, 0, 1
tablew atrack49, aidx, gi_outTab49, 1, 0, 1
tablew atrack50, aidx, gi_outTab50, 1, 0, 1
tablew atrack51, aidx, gi_outTab51, 1, 0, 1
tablew atrack52, aidx, gi_outTab52, 1, 0, 1
tablew atrack53, aidx, gi_outTab53, 1, 0, 1
tablew atrack54, aidx, gi_outTab54, 1, 0, 1
tablew atrack55, aidx, gi_outTab55, 1, 0, 1
tablew atrack56, aidx, gi_outTab56, 1, 0, 1
tablew atrack57, aidx, gi_outTab57, 1, 0, 1
tablew atrack58, aidx, gi_outTab58, 1, 0, 1
tablew atrack59, aidx, gi_outTab59, 1, 0, 1
tablew atrack60, aidx, gi_outTab60, 1, 0, 1
outs asigIn1, asigIn2
endin
instr 2
ifreq=sr/ftlen(gi_outTab1)
aenv1 oscili 1.0, ifreq, gi_outTab1
asig1 oscili aenv1, 262.0, gi_sin
aenv2 oscili 1.0, ifreq, gi_outTab2
asig2 oscili aenv2, 524.0, gi_sin
aenv3 oscili 1.0, ifreq, gi_outTab3
asig3 oscili aenv3, 786.0, gi_sin
aenv4 oscili 1.0, ifreq, gi_outTab4
asig4 oscili aenv4, 1048.0, gi_sin
aenv5 oscili 1.0, ifreq, gi_outTab5
asig5 oscili aenv5, 1310.0, gi_sin
aenv6 oscili 1.0, ifreq, gi_outTab6
asig6 oscili aenv6, 1572.0, gi_sin
aenv7 oscili 1.0, ifreq, gi_outTab7
asig7 oscili aenv7, 1834.0, gi_sin
aenv8 oscili 1.0, ifreq, gi_outTab8
asig8 oscili aenv8, 2096.0, gi_sin
aenv9 oscili 1.0, ifreq, gi_outTab9
asig9 oscili aenv9, 2358.0, gi_sin
aenv10 oscili 1.0, ifreq, gi_outTab10
asig10 oscili aenv10, 2620.0, gi_sin
aenv11 oscili 1.0, ifreq, gi_outTab11
asig11 oscili aenv11, 2882.0, gi_sin
aenv12 oscili 1.0, ifreq, gi_outTab12
asig12 oscili aenv12, 3144.0, gi_sin
aenv13 oscili 1.0, ifreq, gi_outTab13
asig13 oscili aenv13, 3406.0, gi_sin
aenv14 oscili 1.0, ifreq, gi_outTab14
asig14 oscili aenv14, 3668.0, gi_sin
aenv15 oscili 1.0, ifreq, gi_outTab15
asig15 oscili aenv15, 3930.0, gi_sin
aenv16 oscili 1.0, ifreq, gi_outTab16
asig16 oscili aenv16, 4192.0, gi_sin
aenv17 oscili 1.0, ifreq, gi_outTab17
asig17 oscili aenv17, 4454.0, gi_sin
aenv18 oscili 1.0, ifreq, gi_outTab18
asig18 oscili aenv18, 4716.0, gi_sin
aenv19 oscili 1.0, ifreq, gi_outTab19
asig19 oscili aenv19, 4978.0, gi_sin
aenv20 oscili 1.0, ifreq, gi_outTab20
asig20 oscili aenv20, 5240.0, gi_sin
aenv21 oscili 1.0, ifreq, gi_outTab21
asig21 oscili aenv21, 5502.0, gi_sin
aenv22 oscili 1.0, ifreq, gi_outTab22
asig22 oscili aenv22, 5764.0, gi_sin
aenv23 oscili 1.0, ifreq, gi_outTab23
asig23 oscili aenv23, 6026.0, gi_sin
aenv24 oscili 1.0, ifreq, gi_outTab24
asig24 oscili aenv24, 6288.0, gi_sin
aenv25 oscili 1.0, ifreq, gi_outTab25
asig25 oscili aenv25, 6550.0, gi_sin
aenv26 oscili 1.0, ifreq, gi_outTab26
asig26 oscili aenv26, 6812.0, gi_sin
aenv27 oscili 1.0, ifreq, gi_outTab27
asig27 oscili aenv27, 7074.0, gi_sin
aenv28 oscili 1.0, ifreq, gi_outTab28
asig28 oscili aenv28, 7336.0, gi_sin
aenv29 oscili 1.0, ifreq, gi_outTab29
asig29 oscili aenv29, 7598.0, gi_sin
aenv30 oscili 1.0, ifreq, gi_outTab30
asig30 oscili aenv30, 7860.0, gi_sin
aenv31 oscili 1.0, ifreq, gi_outTab31
asig31 oscili aenv31, 8122.0, gi_sin
aenv32 oscili 1.0, ifreq, gi_outTab32
asig32 oscili aenv32, 8384.0, gi_sin
aenv33 oscili 1.0, ifreq, gi_outTab33
asig33 oscili aenv33, 8646.0, gi_sin
aenv34 oscili 1.0, ifreq, gi_outTab34
asig34 oscili aenv34, 8908.0, gi_sin
aenv35 oscili 1.0, ifreq, gi_outTab35
asig35 oscili aenv35, 9170.0, gi_sin
aenv36 oscili 1.0, ifreq, gi_outTab36
asig36 oscili aenv36, 9432.0, gi_sin
aenv37 oscili 1.0, ifreq, gi_outTab37
asig37 oscili aenv37, 9694.0, gi_sin
aenv38 oscili 1.0, ifreq, gi_outTab38
asig38 oscili aenv38, 9956.0, gi_sin
aenv39 oscili 1.0, ifreq, gi_outTab39
asig39 oscili aenv39, 10218.0, gi_sin
aenv40 oscili 1.0, ifreq, gi_outTab40
asig40 oscili aenv40, 10480.0, gi_sin
aenv41 oscili 1.0, ifreq, gi_outTab41
asig41 oscili aenv41, 10742.0, gi_sin
aenv42 oscili 1.0, ifreq, gi_outTab42
asig42 oscili aenv42, 11004.0, gi_sin
aenv43 oscili 1.0, ifreq, gi_outTab43
asig43 oscili aenv43, 11266.0, gi_sin
aenv44 oscili 1.0, ifreq, gi_outTab44
asig44 oscili aenv44, 11528.0, gi_sin
aenv45 oscili 1.0, ifreq, gi_outTab45
asig45 oscili aenv45, 11790.0, gi_sin
aenv46 oscili 1.0, ifreq, gi_outTab46
asig46 oscili aenv46, 12052.0, gi_sin
aenv47 oscili 1.0, ifreq, gi_outTab47
asig47 oscili aenv47, 12314.0, gi_sin
aenv48 oscili 1.0, ifreq, gi_outTab48
asig48 oscili aenv48, 12576.0, gi_sin
aenv49 oscili 1.0, ifreq, gi_outTab49
asig49 oscili aenv49, 12838.0, gi_sin
aenv50 oscili 1.0, ifreq, gi_outTab50
asig50 oscili aenv50, 13100.0, gi_sin
aenv51 oscili 1.0, ifreq, gi_outTab51
asig51 oscili aenv51, 13362.0, gi_sin
aenv52 oscili 1.0, ifreq, gi_outTab52
asig52 oscili aenv52, 13624.0, gi_sin
aenv53 oscili 1.0, ifreq, gi_outTab53
asig53 oscili aenv53, 13886.0, gi_sin
aenv54 oscili 1.0, ifreq, gi_outTab54
asig54 oscili aenv54, 14148.0, gi_sin
aenv55 oscili 1.0, ifreq, gi_outTab55
asig55 oscili aenv55, 14410.0, gi_sin
aenv56 oscili 1.0, ifreq, gi_outTab56
asig56 oscili aenv56, 14672.0, gi_sin
aenv57 oscili 1.0, ifreq, gi_outTab57
asig57 oscili aenv57, 14934.0, gi_sin
aenv58 oscili 1.0, ifreq, gi_outTab58
asig58 oscili aenv58, 15196.0, gi_sin
aenv59 oscili 1.0, ifreq, gi_outTab59
asig59 oscili aenv59, 15458.0, gi_sin
aenv60 oscili 1.0, ifreq, gi_outTab60
asig60 oscili aenv60, 15720.0, gi_sin
aout = asig1+asig2+asig3+asig4+asig5+asig6+asig7+asig8+asig9+asig10+asig11+asig12+asig13+asig14+asig15+asig16+asig17+asig18+asig19+asig20+asig21+asig22+asig23+asig24+asig25+asig26+asig27+asig28+asig29+asig30+asig31+asig32+asig33+asig34+asig35+asig36+asig37+asig38+asig39+asig40+asig41+asig42+asig43+asig44+asig45+asig46+asig47+asig48+asig49+asig50+asig51+asig52+asig53+asig54+asig55+asig56+asig57+asig58+asig59+asig60
outs aout, aout
 endin
</CsInstruments>
<CsScore>
 i1 0 2.5
 i2 3.0 3.0
</CsScore>
</CsoundSynthesizer>
