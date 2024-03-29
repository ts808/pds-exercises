------------------------------ MODULE AB --------------------------------
EXTENDS Integers

CONSTANT Data  

VARIABLES AVar,   
          BVar    
          

TypeOK == /\ AVar \in Data \X {0,1}
          /\ BVar \in Data \X {0,1}


vars == << AVar, BVar >>


Init == /\ AVar \in Data \X {1} 
        /\ BVar = AVar


A == /\ AVar = BVar
     /\ \E d \in Data: AVar' = <<d, 1 - AVar[2]>>
     /\ BVar' = BVar

B == /\ AVar # BVar
     /\ BVar' = AVar
     /\ AVar' = AVar

Next == A \/ B

Spec == Init /\ [][Next]_vars

Inv == (AVar[2] = BVar[2]) => (AVar = BVar)

FairSpec == Spec /\ WF_vars(Next) 
=============================================================================
