*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/40_techpol/NPi2025/sets.gms


*** Sets neeed for renewable share targets
Sets
RenShareTargetType     "Renewable share target types"
/
    RenElec                     "renewable share in secondary energy electricity"
    NonBioRenElec               "non-biomass renewable share in secondary energy electricity"
    NonFossilElec               "non-fossil share in secondary energy electricity"
    RenFE                       "renewable share in total final energy"
/
;

Sets
*** Mappings needed for renewable share targets (set filled with entries in sets_calculations.gms)
TargetType2ShareEnty(RenShareTargetType,all_enty)    "map renewable share target type to energy carriers used to calculate numerator of share, e.g. renewable electricity"
/
/

TargetType2TotalEnty(RenShareTargetType,all_enty)    "map renewable share target type to energy carriers used to calculate denominator of share, e.g. total electricity"
/
/
;


*** EOF ./modules/40_techpol/NPi2025/sets.gms