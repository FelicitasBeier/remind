*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/40_techpol/NPi2025/sets_calculations.gms


*** Fill TargetType2InOutEnty mapping
*** mapping of renewable share target types to energy carriers used to calculate the respective shares

loop(RenShareTargetType,
*** renewable electricity defined as electricity from all renewable PEs and SE hydrogen
    if (sameas(RenShareTargetType,"Renewable"),

        loop (en2en(enty,enty2,te)$( (peRe(enty) OR sameas(enty,"seh2")) AND sameas(enty2,"seel") ),
            TargetType2InOutEnty(hareTargetType,enty,enty2) = YES;
        );
    );
*** renewable non-biomass electricity defined as electricity from all non-biomass renewable PEs and SE hydrogen
    if (sameas(RenShareTargetType,"NonBioRenewable"),
        loop (en2en(enty,enty2,te)$( (peRe(enty) OR sameas(enty,"seh2")) 
                                        AND NOT peBio(enty)
                                        AND sameas(enty2,"seel") ),
            TargetType2InOutEnty(hareTargetType,enty,enty2) = YES;
        );
    );
*** non-fossil electricity defined as electricity from all PE and Sources except PE fossils
    if (sameas(RenShareTargetType,"NonFossil"),
        loop (en2en(enty,enty2,te)$( NOT peFos(enty) 
                                        AND sameas(enty2,"seel") ) ,
            TargetType2InOutEnty(hareTargetType,enty,enty2) = YES;
        );
    );
*** renewable share in final energy here approcimated by share of all renewables in total secondary energy
*** it has not been done on final energy level because this would require a more involved tracking of renewables in final energy that is difficult to implement.
*** here, renewable sources include all PE renewable as well as SE hydrogen which can practically almost always be considered low-carbon hydrogen in REMIND. 
    if (sameas(RenShareTargetType,"FE"),
        loop (en2en(enty,enty2,te)$( (peRe(enty) OR sameas(enty,"seh2")) AND entySe(enty2)  ),
            TargetType2InOutEnty(hareTargetType,enty,enty2) = YES;
        );
    );
);





*** EOF ./modules/40_techpol/NPi2025/sets_calculations.gms