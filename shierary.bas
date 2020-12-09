'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
' Shierary (Weather, Rice, Wheat, Oilpalm, Forestfire, Potatoes, Teak)
' source code develop by team, lead by Prof. Dr. Ir. Handoko, M.Sc,
'
' SEAMEO BIOTROP
' Agrisoft
' Laboratory of Agricultural Meteorology, IPB
' 
' Bogor - Indonesia
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'General parameters
Global DAS, s, ILD, TET, TW, GW, dILD, dTW, TWp, Photo, Wveg, dRes, senesS, dWpg, dGW2, dGW1
Global Date, pminS, SW1
Global input1, output1, input2, Output2
Global j, k%, lhv, alpha, swc, wp1
Global Tsa, Tsm, rew, rdepth, tlayer, FC1
Global lat
Global Train, TU, wind, Etm
Global Irain(1 To 366), Iradiation(1 To 366), ITmax(1 To 366), ITmin(1 To 366), IRH(1 To 366), Angin(1 To 366), Leaf(1 To 366)
Global pi, gamma, dair, cp

'Soil information:
Global dlayer, U, CEs1, CEs2, times, Es

'Wilting point(%,mm):
Global WP

'Field capacity :
Global FC

'Soil Nitrogen
Global kam, knit, pH, OM


'Cultivar information
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'Phenology
Global Tb, TUat, TUtm, TUanth, TUmat, Tave, dsmat, smat

'At Sowing
Global s1, s2, s3, s4, LW, SW, RW, NactL, NactS, NactR, NH4, NO3

'Weather
Global sinld, d, cosld, sinb, arg, arccos, dlen, esat, ea, RH, vpd, delta
Global albs, albc, alb
Global sangot, nN, radiasi, Rlw, Rn
Global f1

'Soil_evaporation:
Global Fint, Rain, Inf, Irrig, Esm, p
Global timeso, rained
Global Esx

'Dry matter
Global km, LUE, kg, Nmin, mgmax
Global pG, varietas, pS, pL, pR, Grno, wpg, slw
Global wdf, Sint
Global GDMp, SLNitrogen, NL, ndf, GDMa
Global Q10, RmL, RmS, RmR, RmG
Global dLW, dSW, dRW, dGW
Global drdepth
Global pw

'Nitrogen for crops
Global NLtrans, NStrans, Npool, Nup
Global NmaxL, NmaxS, NmaxR, NdemL, NdemS, NdemR
Global NdemT, NS, NR
Global NdemG, dNsur, NG
Global NactG, NT

'Transpiration
Global Swccrit, Tsa1
 
'Water balance
Global perc, runoff
Global Irrigasi

'Soil Nitrogen
Global pupukNH4, pupuk, vola1, vola2
Global NO3c, NH4c, Nminc
Global rwf, dNH4, leach
Global nlayer, leaching
Global rpH, dNO3p, dNO3a
Global Nuptmf, Nupt
Global TNup

'Others
Global Lokasi, kl, tlp, aa, bh, n%
Global Tleaching, TSoil, Tvola, Nawal
Global OptIrigasi, OptTadah, OptIr64, OptCiliwung As Boolean

'Module for weather model
Global nday(1 To 13), tnday(1 To 14), fw(1 To 13), ar(1 To 13), CH(1 To 13)
Global HH(1 To 13), P00(1 To 13), P10(1 To 13), P11(1 To 13)
Global CHf(1 To 13), HHf(1 To 13)
Global g1, g2, gv
Global rained_yesterday, Lintang2, Bujur2, Tinggi2
Global Grain(1 To 366), Gradiasi(1 To 366), Gsunshine(1 To 366)
Global GHari(1 To 366), GBulan(1 To 366), GTmax(1 To 366), GTmin(1 To 366), GTave(1 To 366), GRH(1 To 366)
Global TBulan, Train, THH, Tsol
Global Tsunshine, TTmax, TTmin, TRH
Global output
Global lintang, bujur
Global tinggi, chb1, chb2, chb3, chb4, chb5, chb6, chb7, chb8
Global chb9, chb10, chb11, chb12
Global hhb1, hhb2, hhb3, hhb4, hhb5, hhb6
Global hhb7, hhb8, hhb9, hhb10, hhb11, hhb12

Global lintang1, bujur1, alti, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, ch10, ch11, ch12
Global hh1, hh2, hh3, hh4, hh5, hh6, hh7, hh8, hh9, hh10, hh11, hh12

Global zg, prod, kount, ag, bg, L, rn1, uag, x, rn2
Global ubg, y, w, rn3
Global hari2(1 To 366), bulan2(1 To 366), sunshine2(1 To 366), ITave2(1 To 366)
Global Tlayu, Klap
Global input3, output3, persen


'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Initialization()

pi = 3.1415926
gamma = 66.1
lhv = 2.454
dair = 1.204
cp = 1010

'Soil Information:

'Soil Evaporation :
    dlayer = 1000
    tlayer = dlayer
    alpha = 5.08
    U = 12
    CEs1 = U
    CEs2 = 0
    times = 0
    Es = 0
    
'Wilting Point(%,mm):
    pWP = tlp * dlayer / 100
    wp1 = pWP

'Field Capacity :
    pFC = kl * dlayer / 100
    FC1 = pFC
    swc = FC1

'Soil Nitrogen
    kam = 96 * 10 ^ (-6)
    knit = 0.05
    pH = 6.2
    OM = 0.00165 * 200 * 10000 * 1.12
    
'Cultivar information
'Phenology
    Tb = 17
    TUat = 230
    TUtm = 310
    If OptIr64 = True Then
      TUanth = 135
      TUmat = 305
    Else
      TUanth = 185
      TUmat = 355
    End If

'Dry Matter
    km = 0.015
    k = 0.5
    LUE = 0.002
    kg = 0.14
    Nmin = 1
    mgmax = 30
    
'At Sowing
    j = Date
    s1 = 0: s2 = 0: s3 = 0: s4 = 0: s = 0
    TWp = 25
    dLW = 0
    dTW = 0
    SW = 0
    RW = 0
    slw = 0
    NactL = 7: NactS = 5: NactR = 5
    'NL = NactL * LW / 100
    'NH4 = 25 * 1.1 * dlayer / 100
    NH4 = 0.72 * dlayer / 100
    'NO3 = 25 * 1.1 * dlayer / 100
    NO3 = 41.5 * dlayer / 100
    rdepth = 50
    Nawal = NH4 + NO3
    wdf = 1

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Dry_Matter()

Dry_Matter:

If s < 0.25 Then Call Seeding_Period
If s < 0.25 Then GoTo Moves_Planting

    If s < 0.75 Then
        pG = 0
    
        If varietas = 1 Then
            pS = 0.2801 * 2.718282 ^ (1.1352 * s)
            pR = 0.0714 * s ^ (-0.8007)
        Else
            'pS = 0.2368 * 2.302585 * (Log(s)) + 0.6619
            pS = -1.78 * s ^ 3 + 3.5175 * s ^ 2 - 0.7329 * s + 0.3638
            'pR = 0.0998 * (s ^ (-0.7525))
            pR = 0.3027 * s ^ 2 - 0.5559 * s + 0.3331
            
        End If
        
    Else
            'pS = -2.2185 * s ^ 2 + 2.7389 * s - 0.1496
            'pL = 0
            pS = 0
            pR = 0
            pG = 0.483 * (s ^ 10.634)
           
        If Grno > 0 Then GoTo Pass1
        If TW > 1500 Then Grno = (-903 + 288.03 * 0.434 * Log(TW)) * 10 ^ 6
        If Grno <= 0 Then Grno = 0
        
        GoTo Pass2
        
Pass1:

        wpg = GW / Grno * 10 ^ 6
       
Pass2:

    End If

    slw = 127.68 * 10 ^ (0.00083617 * TU)
     
'Intercepted radiation (MJ/m2/d) :
        'ILD = LW / slw * wdf
        LW = ILD * slw
        'If ILD < 0 Then ILD = 0
        Sint = radiasi * (1 - Exp(-k * ILD))

'Potential gross dry matter production (kg/ha/d) :
    GDMp = LUE * Sint * 10 ^ 4

'Actual gross dry matter production (kg/ha/d) :

'Nitrogen availability factor :
    
    'If wdf = 0 Then GoTo BiomassAct
    If ILD > 0 Then
        SLNitrogen = NL / ILD
        
        'SLNitrogen = NL / (LW / (slw * wdf))
        'If SLNitrogen < 30 Then
            'If SLNitrogen < 3 Then ndf = 0
            If SLNitrogen >= 3 Then ndf = 1 - Exp(-0.17 * (SLNitrogen - 3)) Else ndf = 0
        'Else
            'ndf = 1
        'End If
    Else
        ndf = 0
    End If
  
    If ndf < 0 Then ndf = 0

    If wdf < ndf Then GDMa = (1 - kg) * GDMp * wdf Else GDMa = (1 - kg) * GDMp * ndf
    
'Respiration (kg/ha/d):
    Q10 = 2 ^ ((Tave - 20) / 10)
    RmL = km * Q10 * LW: RmS = km * Q10 * SW: RmR = km * Q10 * RW: RmG = km * Q10 * GW
   
'Biomasa sharing:    
    dLW = LW - dLW
    dRW = (pR * GDMa - RmR)
    'dSW = (pS * GDMa - RmS)
    dSW = pS * (dLW + dRW) / (1 - pS)
    SW = SW + dSW
                 
        pminS = 0.1992 * s - 0.1495
        dGW1 = (pG * GDMa - RmG)
        dGW2 = pminS * SW
        dGW = dGW1 + dGW2
        
    If s > 0.75 Then SW = SW - dGW2
    'Else
        'GoTo hitung
    'End If
        
        
'Calculation:
        RW = RW + dRW
        LW = LW + dLW
        GW = GW + dGW
        TW = SW + RW + LW + GW
    
    If wpg > mgmax Then GW = mgmax * Grno * 10 ^ -6: wpg = mgmax
        
    'TW = SW + RW + LW + GW
    
'Root depth (mm) :
    If s >= 0 Then
        If Tave > 17 Then drdepth = 2.2 * (Tave - 17) Else drdepth = 0
    Else
        drdepth = 0
    End If
    rdepth = rdepth + drdepth
    If rdepth > tlayer Then rdepth = tlayer
    
Moves_Planting:

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Phenology()

    If s >= 0.75 Then GoTo Maturity
    If s >= 0.5 Then GoTo Anthesis
    If s >= 0.25 Then GoTo Shoot

Emergence:
    If Tave > Tb Then s1 = s1 + 0.25 * (Tave - Tb) / TUat Else GoTo Stage
    If s1 > 0.25 Then s1 = 0.25
    If s1 < 0.25 Then GoTo Stage

Shoot:
    If Tave > Tb Then s2 = s2 + 0.25 * (Tave - Tb) / TUtm Else GoTo Stage
    If s2 > 0.25 Then s2 = 0.25
    If s2 < 0.25 Then GoTo Stage

Anthesis:
    If Tave > Tb Then s3 = s3 + 0.25 * (Tave - Tb) / TUanth Else GoTo Stage
    If s3 > 0.25 Then s3 = 0.25
    If s3 < 0.25 Then GoTo Stage

Maturity:
    If Tave > Tb Then dsmat = 0.25 * (Tave - Tb) / TUmat Else dsmat = 0
    s4 = s4 + dsmat
    If s4 > 0.25 Then s4 = 0.25
    smat = smat + dsmat

Stage:
    s = s1 + s2 + s3 + s4
    If Tave > Tb Then TU = TU + (Tave - Tb)
 
End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Soil_Evaporation()

'Foliar interception, Fint (Zinke, 1967  ), mm :
     If ILD < 3 Then Fint = 0.4233 * ILD Else Fint = 1.27
     If Rain < Fint Then Fint = Rain

'Infiltration (INF), mm :
     Inf = Rain - Fint + Irrig

'Tranpiration max, mm:
     
     Tsm = Etm * (1 - Exp(-0.3 * ILD))
     
     Esm = Etm - Tsm
     If Esm < 0 Then Esm = 0

     p = Inf
     If CEs1 >= U Then GoTo Stage2

Stage1:
     If p >= CEs1 Then CEs1 = 0 Else CEs1 = CEs1 - p

Cumes1:
     CEs1 = CEs1 + Esm
     If CEs1 < U Then Es = Esm Else GoTo Transition
     GoTo BufferEvap

Transition:
     Es = Esm - 0.4 * (CEs1 - U)
     CEs2 = 0.6 * (CEs1 - U)
     times = (CEs2 / alpha) ^ 2
     GoTo BufferEvap

Stage2:
     If p >= CEs2 Then GoTo Storm
     times = times + 1
     timeso = times - 1
     Es = alpha * Sqr(times) - alpha * Sqr(timeso)
     If p > 0 Then GoTo Rained
     If Es > Esm Then Es = Esm

Cumes2:
     CEs2 = CEs2 + Es - p
     GoTo BufferEvap

Storm:
     p = p - CEs2
     CEs1 = U - p
     If p > U Then CEs1 = 0
     GoTo Cumes1

Rained:
     Esx = 0.8 * p
     If Esx <= Es Then Esx = Es + p
     If Esx > Esm Then Esx = Esm
     Es = Esx
     GoTo Cumes2

BufferEvap:
     If swc < 0.5 * wp1 Then Es = 0
     If swc > FC1 Then Es = Esm

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Weather()

'Solar Declination (degree):
    i = DAS
    ij = i + j
    If ij > 365 Then ij = ij - 365
    d = -23.4 * Cos(2 * pi * (ij + 10) / 365)

'Daylength, dlen (hours):
    sinld = Sin(lat * pi / 180) * Sin(d * pi / 180)
    cosld = Cos(lat * pi / 180) * Cos(d * pi / 180)
    sinb = Sin(-0.833 * pi / 180)
    arg = (sinb - sinld) / cosld
    arccos = Atn(-arg / Sqr(-arg * arg + 1)) + 2 * Atn(1)
    dlen = 24 / pi * arccos
    
'Vapour pressure (mb):
    esat = 6.1078 * Exp(17.239 * Tave / (Tave + 237.3))
    ea = RH * esat / 100
    vpd = esat - ea

'Slope of vapour pressure (Pa/oC):
    delta = 47.139 * Exp(0.055129 * Tave)

'Albedo (unitless) :
    albs = 0.05
    albc = 0.25 * (0.23 - 0.05) * ILD
    alb = albs + albc

'Long wave radiation (MJ/m2/d):
    sangot = 58.75 * (sinld + cosld)
    nN = (radiasi / sangot - 0.16) / 0.62
    Rlw = 2 * (10 ^ -9) * ((Tave + 273) ^ 4) * (0.56 - 0.08 * Sqr(ea)) * (0.1 + 0.9 * nN)

'Net radiation (MJ/m2/d):
    Rn = (1 - alb) * radiasi - Rlw
    
'Aerodynamic function (MJ/(oC.m2.d)):
    f1 = 0.64 * (1 + 0.54 * wind * 1000 / 3600)

'Energy limited evapotranspiration (mm):
    Etm = (delta * Rn + f1 * vpd * 100) / ((delta + gamma) * lhv)
    If Etm < 0 Then Etm = 0

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Soil_Nitrogen()

If DAS = 25 Or DAS = 45 Then pupukNH4 = pupuk / 2 Else pupukNH4 = 0
  NH4 = NH4 + pupukNH4
  vola1 = vola1 + pupukNH4 - vola2
  vola2 = 0.015 * vola1
  Tvola = Tvola + vola2
  
'Concentration of nitrat and amonium:
    If (swc + perc) > 0 Then NO3c = NO3 / (swc + perc) Else NO3c = 0
    If (swc + perc) > 0 Then NH4c = NH4 / (swc + perc) Else NH4c = 0
    Nminc = NO3c + NH4c

'Mineralisation :
    rwf = -0.039 + 1.02 * swc / FC1

'Amonificaation:
    dNH4 = kam * Q10 * OM

'Leaching:
If swc >= wp1 Then
    If runoff <= 0 Then
        leach = 0
    Else
        leach = NO3 * runoff / (runoff + FC1)
    End If
Else
    dNH4 = 0: rwf = 0: leach = 0

End If

Tleaching = Tleaching + leach
    'IF l = nlayer THEN leaching = leach(l)

'pH Influence:
    If pH < 5 Or pH > 8 Then
        rpH = 0
    Else
        If pH <= 7 Then rpH = (pH - 5) / 2 Else rpH = 8 - pH
    End If

'Potential Nitrification:
    dNO3p = knit * NH4

'Actual Nitrification:
    If rwf <= rpH Then
        dNO3a = Q10 * dNO3p * rwf
    Else
        dNO3a = Q10 * dNO3p * rpH
    End If

    OM = OM - dNH4: NH4 = NH4 - dNO3a + dNH4
    NO3 = NO3 - leach + dNO3a
    If NO3 < 0 Then NO3 = 0
    Tsoil = (NO3 + NH4)

'The rate of N uptake by plants per layer:
    If NdemT > 0 Then
        Nuptmf = Tsa * Nminc
        Nupt = Nuptmf
    Else
        Nuptmf = 0: Nupt = 0
    End If
    
    Nup = Nuptmf + Nupt

    If Nup > NdemT Then
        Nup = NdemT
    End If

'Total N uptake by plants:
    If Nup > (NH4 + NO3) Then
        Nup = (NH4 + NO3): NH4 = 0: NO3 = 0
    End If

    If (NO3 + NH4) > 0 Then
        NO3 = NO3 - Nup * NO3 / (NO3 + NH4)
        NH4 = NH4 - Nup * NH4 / (NO3 + NH4)
    Else
        NO3 = 0: NH4 = 0
    End If

    TNup = TNup + Nup

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Water_Balance()

swc = swc + Inf - Es - Tsa
If swc > (FC1 + 100) Then GoTo Runoff
perc = 0

GoTo BufferWbal

Runoff:
  runoff = swc - (FC1 + 100)
  swc = (FC1 + 100)

BufferWbal:
  If swc < 0 Then swc = 0
  If OptIrigasi = True Then swc = FC1 + 100
        
End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Transpiration()

    'Relative Extractable Water (rew, unitless):
            Swccrit = wp1 + 0.4 * (FC1 - wp1)
            rew = (swc - wp1) / (Swccrit - wp1)
            If swc <= wp1 Then rew = 0
            If swc > FC1 Then rew = 1
            
    'Actual transpiration (mm) :
            Tsa = Tsm * rew * rdepth / tlayer
            If Tsa > Tsm Then Tsa = Tsm

            If Tsm > 0 Then wdf = Tsa / Tsm Else wdf = 0

            Tsa1 = Tsa1 + Tsa
   
End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Plant_Nitrogen()

'Content of N (kg/ha) per organ :
    If s < 0.25 Then GoTo Seedling

'Translocation N from Leaf/Stem ke Seed:
        
        
        If dLW < 0 And NactL > 1 Then NLtrans = -dLW * (NactL - 1) / 100 Else NLtrans = 0
        If dSW < 0 And NactS > 1 Then NStrans = -dSW * (NactS - 1) / 100 Else NStrans = 0

'Number of N is available :
    Npool = Nup + NLtrans + NStrans
    If Npool < 0 Then Npool = 0

    If s > 0.75 Then GoTo Filling

'Maximum consentration :
        NmaxL = 7 - 10 * (s - 0.25)
        NmaxS = 5 - 6 * (s - 0.25)
        NmaxR = 5 - 6 * (s - 0.25)

'Requirement of N for vegetative organs :
        If dLW > 0 And NmaxL > NactL Then NdemL = dLW * NmaxL / 100 Else NdemL = 0
        If dSW > 0 And NmaxS > NactS Then NdemS = dSW * NmaxS / 100 Else NdemS = 0
        If dRW > 0 And NmaxR > NactR Then NdemR = dRW * NmaxR / 100 Else NdemR = 0

'Total requirement of N for vegetative organs :
        NdemT = NdemL + NdemS + NdemR
        If NdemT > 0 And s <= 0.75 Then

            NL = NL + (NdemL / NdemT) * Npool - NLtrans
            NS = NS + (NdemS / NdemT) * Npool - NStrans
            NR = NR + (NdemR / NdemT) * Npool

        End If

    GoTo Consentration

Filling:

    NmaxL = 2: NmaxS = 2: NmaxR = 2

    NL = NL - NLtrans: NS = NS - NStrans
    If Npool > NdemG Then
        dNsur = Npool - NdemG: NdemT = 0
    Else
        dNsur = 0: NdemT = NdemG - Npool
    End If

    NG = NG + Npool - dNsur

    If GW > 0 Then
        NactG = NG / GW * 100
        If NactG > 3 Then NactG = 3
        NdemG = (3 - NactG) * GW / 100
    Else
        NactG = 0: NdemG = 0
    End If

Consentration:

    If NL < 0 Then NL = 0: If NS < 0 Then NS = 0
    If NR < 0 Then NR = 0: If NG < 0 Then NG = 0
    NT = NL + NS + NR + NG

    If LW > 0 Then NactL = NL / LW * 100: If SW > 0 Then NactS = NS / SW * 100
    If RW > 0 Then NactR = NR / RW * 100

    If NactL < 1 Then NactL = 1: If NactS < 1 Then NactS = 1: If NactR < 1 Then NactR = 1
    If NactL > NmaxL Then NactL = NmaxL
    If NactS > NmaxS Then NactS = NmaxS
    If NactR > NmaxR Then NactR = NmaxR

GoTo Back

Seedling:

    NL = NactL * LW / 100: NS = NactS * SW / 100
    NR = NactR * RW / 100: NG = NactG * GW / 100

Back:

End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Seeding_Period()
    SW = s * TWp
    RW = s * TWp
    slw = 127.68 * 10 ^ (0.00083617 * TU)
    LW = ILD * slw * wdf

    If ILD = 0 Then SW = 0 And RW = 0
    
TW = SW + RW + LW + (TWp - TWp * s)


End Sub

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Public Sub Reset_Var()
DAS = 0: s = 0: ILD = 0: TET = 0: TW = 0: GW = 0
j = 0: k% = 0: lhv = 0: alpha = 0: swc = 0: wp1 = 0
Tsa = 0: Tsm = 0: rew = 0: rdepth = 0: tlayer = 0: FC1 = 0
Train = 0: TU = 0: wind = 0: Etm = 0
pi = 0: gamma = 0: dair = 0: cp = 0
dlayer = 0: U = 0: CEs1 = 0: CEs2 = 0: times = 0: Es = 0
WP = 0: FC = 0
kam = 0: knit = 0: pH = 0: OM = 0
Tb = 0: TUat = 0: TUtm = 0: TUanth = 0: TUmat = 0
km = 0: LUE = 0: kg = 0: Nmin = 0: mgmax = 0
s1 = 0: s2 = 0: s3 = 0: s4 = 0: LW = 0: SW = 0: RW = 0: NactL = 0: NactS = 0:
NactR = 0: NH4 = 0: NO3 = 0
sinld = 0: d = 0: cosld = 0: sinb = 0: arg = 0: arccos = 0: dlen = 0: esat = 0:
ea = 0: RH = 0: vpd = 0: delta = 0
albs = 0: albc = 0: alb = 0
sangot = 0: nN = 0: radiasi = 0: Rlw = 0: Rn = 0
f1 = 0

Fint = 0: Rain = 0: Inf = 0: Esm = 0: p = 0
timeso = 0: rained = 0
Esx = 0
   
'Phenology
Tave = 0: dsmat = 0: smat = 0

'Dry Matter
pG = 0: varietas = 0: pS = 0: pL = 0: pR = 0: Grno = 0: wpg = 0: slw = 0
wdf = 0: Sint = 0: GDMp = 0: SLNitrogen = 0: NL = 0: ndf = 0: GDMa = 0
Q10 = 0: RmL = 0: RmS = 0: RmR = 0: RmG = 0
dLW = 0: dSW = 0: dRW = 0: dGW = 0
drdepth = 0

'Plant Nitrogen
NLtrans = 0: NStrans = 0: Npool = 0: Nup = 0
NmaxL = 0: NmaxS = 0: NmaxR = 0: NdemL = 0: NdemS = 0: NdemR = 0
NdemT = 0: NS = 0: NR = 0: NdemG = 0: dNsur = 0: NG = 0
NactG = 0: NT = 0

'Transpiration
Swccrit = 0: Tsa1 = 0
 
'Water balance
perc = 0: runoff = 0

'Soil Nitrogen
pupukNH4 = 0: vola1 = 0: vola2 = 0
NO3c = 0: NH4c = 0: Nminc = 0: rwf = 0: dNH4 = 0: leach = 0
nlayer = 0: leaching = 0: rpH = 0: dNO3p = 0: dNO3a = 0: Nuptmf = 0: Nupt = 0: TNup = 0

'Others
Tleaching = 0: TSoil = 0: Tvola = 0: Nawal = 0

End Sub
