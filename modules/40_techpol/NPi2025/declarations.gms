*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

*** SOF ./modules/40_techpol/NPi2025/declarations.gms


Parameter 
    p40_TechBound(ttot,all_regi,all_te)          "NPI capacity targets for solar (pv, csp), wind (total, onshore, offshore), nuclear, hydro, biomass, nuclear (GW)"
    p40_ElecBioBound(ttot,all_regi)              "level for lower bound on biomass tech. absolute capacities, in GW"
    p40_noncombust_acc_eff(ttot,iso_regi,all_te) "Efficiency used for the accounting of non-combustibles PE, e.g. 0.45 for 45% under substitution method, eq 1 for all carriers under direct accounting method"
    p40_PEgasBound(ttot,iso_regi)                "level for lower bound of gas share in PE, e.g. 0.2 for 20%"
    p40_PElowcarbonBound(ttot,iso_regi)          "Lower bound on low carbon share, e.g. 0.2 for 20%"
    p40_El_RenShare(ttot,iso_regi)               "Lower bound on low carbon share, e.g. 0.2 for 20%"
    p40_CoalBound(ttot,iso_regi)                 "level for upper bound on absolute capacities, in GW for all technologies except electromobility"
    p40_FE_RenShare(ttot,iso_regi)               "Lower bound on ren share, e.g. 0.2 for 20%"
*   p40_ElCap_RenShare(ttot,all_regi)            "Lower bound on low carbon share in total installed capacity, e.g. 0.2 for 20%";

    p40_RenShareTargets(ttot,all_regi,RenShareTargetType)  "renewable share targets in NPi per REMIND region aggregated from country-level targets [share]"
;
    
Equation 
    q40_ElecBioBound                              "equation low-carbon push technology policy for bio power"
    q40_windBound				                  "lower bound on combined wind onshore and offshore"

    q40_RenShare                                  "constraint to enforce minimum share of renewables based on renewable share targets of NPi"
;

*** some outpute parameters to check q40_RenShare equation
Parameters
o40_Renewable(ttot,all_regi,RenShareTargetType)                "Renewable energy per target type [EJ/yr]"
o40_Total(ttot,all_regi,RenShareTargetType)                    "Total energy per target type [EJ/yr]"
o40_Total_MainProd(ttot,all_regi,RenShareTargetType)           "Total energy per target type from main product [EJ/yr]"
o40_Total_CoupledProd(ttot,all_regi,RenShareTargetType)        "Total energy per target type from coupled product [EJ/yr]"
;

*** Sets calculations (should be moved to separate file)

*** Fill TargetType2ShareEnty and TargetType2TotalEnty  mappings
*** For renewable electricity share targets include all electricity produced from primary energy renewables as well as electricity produced from hydrogen.
*** Hydrogen is by assumption exclusively low-carbon hydrogen in REMIND and predominantly renewable-based hydrogen. 
TargetType2ShareEnty("RenElec",enty)$( peRe(enty) OR sameas(enty,"seh2")  ) = YES;
TargetType2TotalEnty("RenElec","seel") = YES;
*** For non-biomass renewable electricity share targets include all electricity produced from primary energy renewables except bioenergy as well as electricity produced from hydrogen.
TargetType2ShareEnty("NonBioRenElec",enty)$( ( peRe(enty) OR sameas(enty,"seh2") ) AND NOT peBio(enty)  ) = YES;
TargetType2TotalEnty("NonBioRenElec","seel") = YES;
*** For non-fossil electricity share targets include all electricity produced non-fossil sources. 
TargetType2ShareEnty("NonFossilElec",enty)$( NOT peFos(enty)  ) = YES;
TargetType2TotalEnty("NonFossilElec","seel") = YES;
*** For renewable share target in total final energy, use share of renewable-based secondary energy in total secondary energy as an approximation. 
*** Tracing back final energy to its primary energy origin (e.g. fossil or renewable) is more complicated.
*** Hence, this measure is not exactly the renewable share in final energy due to transmission losses between secondary energy and final energy.
TargetType2ShareEnty("RenFE",enty)$( peRe(enty) OR sameas(enty,"seh2")  ) = YES;
TargetType2TotalEnty("RenFE",entySe) = YES;

*** EOF ./modules/40_techpol/NPi2025/declarations.gms


