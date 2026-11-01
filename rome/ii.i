
globals:
  lb: library

  rw: read word

  ln: list nil
  lc: list cons

  bt: boolean true
  bf: boolean false
  bo: boolean or
  ba: boolean and

  ne: number equal
  nn: number newline
  ns: number space

extract the library:

  WX
    X Vm_ X Vi_ X (Y Vlb Vl_) Vr_
    X Vm_ X Vi_ X Vl_ Vr_
  M

extract eq from library:

  W GlbT X E X Vne E

lists:

  W Vln XEXL

  W Vlc X Vi_ X Vr_ (X (X Vi_ X Vr_ Vo_) X E Vo_)

boolean:

  W Vbt XXHEUL
  W Vbf XXEL

  W Vbo X Va_ X Vb_ A Va_ X GbtT Vb_
  W Vba X Va_ X Vb_ A Va_ X Vb_ GbfT

define space and newline:

  W Vns N W Vnn N

read_word:

  WX Vrw
  X Vw_
    CX (F Vt0 F Vt1 F Vt2 F Vt3 Vt4) XL
    COW CON CJVc_
      AAA
        AA GboT (AA GneT Vt0 (X Gc_Vt1 GnnT)) (AA GneT Vt2 (X Gc_Vt3 GnsT))
        X X E X GlnT XEXL
          X Vt5 X (AA GlcT Vc_ Vwt) (AA GrwVt5 Vrw Vwt)
        Vt4 Vw_

  HWAIYUL Vrw

word_eq:

  WX Vwe
  X V_t
  X Vw0 X V_w AAAA Vw0
    X Vi0 X Vr0 X Vw1 AA Vw1
      X Vi1 X Vr1 X (F Vt0 F Vt1 Vt2)
        AA GbaT
          AA GneT Vt0 (X Vi0 Vi1)
          AAAA GweVt1 Vwe Vt2 Vr0 Vr1
      XE GbfT
    X Vw1 AA Vw1
      XE XE XE GbfT
      XE GbtT
    V_w V_t

  HWAIYUL Vwe

dict_take:

  WX Vdt
  X Vt0
  X Vd_ X V_w X V_a X V_o AAAAAA Vd_
    X (X Vw1 V_v) X V_r X (F Vt1 F Vt2 F Vt3 Vt4) X Vw0
      AAAAAAA GweT Vt1 Gw0Vt2 Gw1Vt3
        X XE XVv_ XVr_ XE XVv_ Vr_
          X (F Vt5 Vt6) XVv_ XVr_ XVa_ XVo_ 
            AA GlcT (X Vw1 Vv_) (AAAAAA GdtVt5 Vdt Vt6 Vr_ Vw0 Va_ Vo_)
        Vt4 V_v V_r
    XE XE X Vo_ X Vo_ GlnT
    Vt0 V_w V_a V_o
    
  HWAIYUL Vdt

dict_get:

  WVdg
  X (F Vt0 Vt1) X Vt_
  X Vd_ X Vw_ X Va_
  W Vr_ AAAAA GdtT Vt0 Vd_ Gw_Vt1 Va_ Vv_
  X Gv_Vt_ (AA GlcT (X Vw_ Vv_) Vr_)

read_comment:

  WX Vrc
  CX Vt0 XL
  COW CON CJVc_
    AAA GneT Vt0 (X Vc_ GnnT)
      X XEXL
        CX Vt1 XL
        A GrcVt1 Vrc

  HWAIYUL Vrc

operations:

  W Vod

  comment:
  
    AA GlcT X "WAAGrwTHTM# UL"
      GrcT

  set:

    AA GlcT X "WAAGrwTHTMset UL"
      C AGrwT Vw_
      C XE X
        X (X V_s X V_t X Vd_ Vr0) Vr1
        X (X V_s X V_t X (AA GlcT (X Vw_ Vv_) Vd_) Vr0) Vr1
        COXJ Vv_

  take:

    AA GlcT X "WAAGrwTHTMtake UL"
      C AGrwT Vw_
      C X Vt_ X
        X (X V_s X V_t X Vd_ Vr0) Vr1
        X (X V_s X V_t X (AAAAA GdtT Vt_ Vd_ Vw_ E Vv_) Vr0) Vr1
        COXJ Vv_

  get:

    AA GlcT X "WAAGrwTHTMget UL"
      C AGrwT Vw_
      C X Vt_ X
        X (X V_s X (X (F Vt0 Vt1) V_t) X Vd_ Vr0) Vr1
        X (X V_s X (X Vt0 V_t) X (AAAAAA GdgT Vt_ Vt1 Vd_ Vw_ E Vv_) Vr0) Vr1
        COXJ Vv_

  x:

    AA GlcT X "WAAGrwTHTMx UL"
      COXJ (X Va_ X Vb_ (X Va_ Vb_))

  def:

    AA GlcT X "WAAGrwTHTMdef UL"
      C AGrwT Vw_
      C XE X
          X V_m X V_i X Vd_ V_r
          X V_m X V_i X (AA GlcT (X Vw_ Vo_) Vd_) V_r
        COXJ (X Vb_ XL)

      W Vo_
        C XEX
            X (X Vs_ X (X Vt0 Vt2) Vr0) Vr1
            X (X Vs_ X (X Vt1 Vt2) Vr0) Vr1
          COXJ (A Vb_ (X Vt0 Vt1) )

  open paren:

    AA GlcT X "WAAGrwTHTM( UL"
      C OW
        J XL

  close paren:

    AA GlcT X "WAAGrwTHTM) UL"
      C J Vx_
        COXJ Vx_

  open brace:

    AA GlcT X "WAAGrwTHTM{ UL"
      XEX
        X Vm_ X Vi_ X Vo_ X Vc_ Vr_
        X (X (X XL E) X Vt_ X GlnT E) X Vi_ X Vo_ X (X XL X Vt_ X Vm_ Vc_) Vr_

  in:

    AA GlcT X "WAAGrwTHTMin UL"
      C XEX
          X Vm0 X Vi_ X Vo_ X (X Vf_ X Vt_ X Vm1 Vc_) Vr_
          X Vm0 X Vi_ X Vo_ X (X (X Vp_ (A Vf_ (X Vv_ Vp_))) X Vt_ X Vm1 Vc_) Vr_
        COXJ (X Vv_ XL)

  close brace:

    AA GlcT X "WAAGrwTHTM} UL"
      C XEX
          X (X (X Vv_ E) X XL E) X Vi_ X Vo_ X (X Vf_ X Vt_ X Vm_ Vc_) Vr_
          X Vm_ X Vi_ X Vo_ X Vc_ Vr_
        COXJ (A Vf_ (X Vt_ Vv_))

  outer:

    AA GlcT X "WAAGrwTHTMouter UL"
      C AGrwT Vw_
      C X E X
          X (X Vs0 Vm0) X V_i X V_d X (X V_f X V_t X (X Vs1 Vm1) V_c) V_r
          X (X Vs0 Vm1) X V_i X V_d X V_c V_r
      C X Vt_ X
          X V_m X V_i X Vd_ V_r
          X V_m X V_i X (AAAAAA GdgT IF Vt_ Vd_ Vw_ E Vo_) V_r
      C Vo_
        X E X
          X (X Vs0 Vm1) X V_i X V_d X V_c V_r
          X (X Vs0 Vm0) X V_i X V_d X (X V_f X V_t X (X Vs1 Vm1) V_c) V_r

  library:

    AA GlcT X "WAAGrwTHTMlibrary UL"
      COXJ GlbT

  io:

    AA GlcT X "WAAGrwTHTMio UL"
      C XEX
          X V_m X (X Vi0 Vo_) X V_d V_r
          X V_m X (X Vi1 Vo_) X V_d V_r
        COXJ (X (X GlnT Vi0) (X E Vi1))

  debug:

    AA GlcT X "WAAGrwTHTM? UL"
      C J Vo_
      XE X (X E X (X E Vo_) E) E

  boot:

    AA GlcT X "WAAGrwTHTMboot UL"
      C J (X (X GlnT Vi_) (X E Vo_))
      XE X (X E X (X Vi_ Vo_) E) E

  noop:

    AA GlcT X "WAAGrwTHTM UL"
      XEXL

  GlnT

W Ven
C AGrwT Vw_
C X Vt_ X
  X Vm_ X Vi_ X Vd_ Vr_
  X Vm_ X Vi_ X (AAAAAA GdgT IF Vt_ Vd_ Vw_ E Vo_) Vr_
C Vo_
C X Vt_ X
  X Vm_ X Vi_ X Vd_ X Vs_ X Ve_ Vr_
  X Vm_ X Vi_ X Vd_ X Vs_ X Ge_Vt_ Vr_
Ve_

W Vst
  X (X (X XL E) X (X T E) X GlnT E)
  X Vio
  X Vod
  X E
  X GenT
  E

WE AA Ven T Vst

W X Vio E B

