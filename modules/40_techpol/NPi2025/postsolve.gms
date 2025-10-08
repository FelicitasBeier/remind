*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/40_techpol/NPi2025/postsolve.gms

o40_Renewable(t,regi,RenShareTargetType) =     sum(TargetType2ShareEnty(RenShareTargetType,enty),
    sum(TargetType2TotalEnty(RenShareTargetType,enty2),
*** Renewable SE production of output SE carrier (enty2) as main product
      sum(en2en(enty,enty2,te),
        vm_prodSe.l(t,regi,enty,enty2,te)
      )
      +
*** Renewable SE production of output SE carrier (enty2) as second product
      sum(pc2te(enty,enty3,te,enty2),
        max(0, pm_prodCouple(regi,enty,enty3,te,enty2)) 
        * vm_prodSe.l(t,regi,enty,enty3,te)
      ) 
    )
  ) * pm_conv_TWa_EJ;

o40_Total(t,regi,RenShareTargetType) = sum(TargetType2TotalEnty(RenShareTargetType,enty2),
*** Total SE production of SE output SE carrier (enty2) as main product
      sum(en2en(enty,enty2,te),
        vm_prodSe.l(t,regi,enty,enty2,te)
      )
      +
*** Total SE production of SE output SE carrier (enty2) as second product
      sum(pc2te(enty,enty3,te,enty2),
        max(0, pm_prodCouple(regi,enty,enty3,te,enty2)) 
        * vm_prodSe.l(t,regi,enty,enty3,te)
      )
    ) * pm_conv_TWa_EJ;




*** EOF ./modules/40_techpol/NPi2025/postsolve.gms