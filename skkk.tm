-- Write
* -> * Write1 Right
-- Write1
_ -> $ Write2 Right
-- Write2
_ -> @ Start Left

-- Start
s -> s MarkS1 Left
k -> k MarkK1 Left
i -> i MarkI1 Left
_ -> _ Roll Right
* -> * Start Left
-- Delay
* -> * Start Left

-- MarkS1
` -> ` MarkS2 Left
* -> * Delay Right
-- MarkS2
` -> ` MarkS3 Left
* -> * Delay Right
-- MarkS3
` -> ` MarkS Right
* -> * Delay Right
-- MarkK1
` -> ` MarkK2 Left
* -> * Delay Right
-- MarkK2
` -> ` MarkK Right
* -> * Delay Right
-- MarkI1
` -> ` MarkI Right
* -> * Delay Right
-- MarkS
s -> S Start Left
* -> * MarkS Right
-- MarkK
k -> K Start Left
* -> * MarkK Right
-- MarkI
i -> I Start Left
* -> * MarkI Right

-- Roll
$ -> _ End Right
S -> S Back Right
K -> K RollP Right
I -> I RollP Right
* -> * Roll Right
-- RollP
$ -> $ Run Left
S -> S Back Right
K -> K RollP Right
I -> I RollP Right
* -> * RollP Right
-- Back
$ -> $ Run Left
S -> s Back Right
K -> K Back Right
I -> I Back Right
* -> * Back Right

-- Run
` -> . Apply Right
s -> . StackS Right
k -> . StackK Right
i -> . StackI Right
S -> . SearchS1 Left
K -> . SearchK1 Left
I -> . SearchI Left
. -> . Run Left
_ -> _ Restart Right

-- SearchI
` -> . Run Left
-- SearchK1
` -> . SearchK2 Left
-- SearchK2
` -> . ProcK Right
-- SearchS1
` -> . SearchS2 Left
-- SearchS2
` -> . SearchS3 Left
-- SearchS3
` -> . ProcS Right

-- BackK
. -> ` StackK Right

-- ProcK
@ -> _ LeftK Left
* -> * ProcK Right

-- LeftK
_ -> @ LeftK1 Left
-- LeftK1
_ -> _ LeftK2 Left
* -> * LeftK1 Left
-- LeftK2
$ -> $ PosK Right
_ -> _ PosK Right
* -> * LeftK2 Left
-- PosK
* -> ^ DelK Right
-- DelK
_ -> _ Copy Right
* -> _ DelK Right
-- Copy
` -> _ CopyA Left
s -> _ CopyS Left
k -> _ CopyK Left
i -> _ CopyI Left
_ -> _ Copy Right
@ -> _ EndK Left
-- CopyA
_ -> _ CopyA Left
^ -> ` StubK Right
-- CopyS
_ -> _ CopyS Left
^ -> s StubK Right
-- CopyK
_ -> _ CopyK Left
^ -> k StubK Right
-- CopyI
_ -> _ CopyI Left
^ -> i StubK Right
-- StubK
* -> ^ Copy Right
-- EndK
_ -> _ EndK Left
^ -> _ Put@ Right

-- ProcS
@ -> @ ProcS1 Left
* -> * ProcS Right
-- ProcS1
_ -> _ ProcS2 Left
-- ProcS2
_ -> _ ProcS3 Left
* -> * ProcS2 Left
-- ProcS3
_ -> _ NextDup Left
* -> * ProcS3 Left
-- NextDup
` -> ' DupA Right
s -> S DupS Right
k -> K DupK Right
i -> I DupI Right
$ -> $ AddA Right
_ -> _ AddA Right
-- DupA
_ -> _ DupA1 Right
* -> * DupA Right
-- DupA1
_ -> _ ReplaceA Right
* -> * DupA1 Right
-- DupS
_ -> _ DupS1 Right
* -> * DupS Right
-- DupS1
_ -> _ ReplaceS Right
* -> * DupS1 Right
-- DupK
_ -> _ DupK1 Right
* -> * DupK Right
-- DupK1
_ -> _ ReplaceK Right
* -> * DupK1 Right
-- DupI
_ -> _ DupI1 Right
* -> * DupI Right
-- DupI1
_ -> _ ReplaceI Right
* -> * DupI1 Right

-- ReplaceA
` -> ` ReplaceA Right
s -> ` ReplaceS Right
k -> ` ReplaceK Right
i -> ` ReplaceI Right
_ -> ` SearchPop Right
-- ReplaceS
` -> s ReplaceA Right
s -> s ReplaceS Right
k -> s ReplaceK Right
i -> s ReplaceI Right
_ -> s SearchPop Right
-- ReplaceK
` -> k ReplaceA Right
s -> k ReplaceS Right
k -> k ReplaceK Right
i -> k ReplaceI Right
_ -> k SearchPop Right
-- ReplaceI
` -> i ReplaceA Right
s -> i ReplaceS Right
k -> i ReplaceK Right
i -> i ReplaceI Right
_ -> i SearchPop Right
-- SearchPop
* -> _ Search@ Right
-- Search@
* -> @ SearchDup Left

-- SearchDup
' -> ` NextDup Left
S -> s NextDup Left
K -> k NextDup Left
I -> i NextDup Left
* -> * SearchDup Left

-- AddA
_ -> _ AddA1 Right
* -> * AddA Right
-- AddA1
_ -> ` Add2A Right
* -> * AddA1 Right
-- Add2A
@ -> _ Next@ Right
* -> * Add2A Right
-- Next@
_ -> @ Rep1 Left
-- Rep1
_ -> _ Rep2 Left
-- Rep2
_ -> ` ApA Left

-- StackS
@ -> s Put_ Right
* -> * StackS Right
-- StackK
@ -> k Put_ Right
* -> * StackK Right
-- StackI
@ -> i Put_ Right
* -> * StackI Right

-- Put_
_ -> _ Put@ Right

-- Put@
_ -> @ Done Left

-- Apply
@ -> @ Ap1 Left
* -> * Apply Right

-- Ap1
_ -> _ ApA Left
* -> * Ap1 Left

-- ApA
_ -> ` Done Left
` -> ` ApA Left
s -> ` ApS Left
k -> ` ApK Left
i -> ` ApI Left
-- ApS
_ -> s Done Left
` -> s ApA Left
s -> s ApS Left
k -> s ApK Left
i -> s ApI Left
-- ApK
_ -> k Done Left
` -> k ApA Left
s -> k ApS Left
k -> k ApK Left
i -> k ApI Left
-- ApI
_ -> i Done Left
` -> i ApA Left
s -> i ApS Left
k -> i ApK Left
i -> i ApI Left

-- Done
$ -> $ Run Left
* -> * Done Left

-- Restart
. -> _ Restart Right
@ -> _ Reflect Left
* -> * Restart Right

-- Reflect
$ -> $ Refl@ Left
* -> * Reflect Left
-- Refl@
_ -> @ rStart Right




-- rStart
s -> s rMarkS1 Right
k -> k rMarkK1 Right
i -> i rMarkI1 Right
_ -> _ rRoll Left
* -> * rStart Right
-- rDelay
* -> * rStart Right

-- rMarkS1
` -> ` rMarkS2 Right
* -> * rDelay Left
-- rMarkS2
` -> ` rMarkS3 Right
* -> * rDelay Left
-- rMarkS3
` -> ` rMarkS Left
* -> * rDelay Left
-- rMarkK1
` -> ` rMarkK2 Right
* -> * rDelay Left
-- rMarkK2
` -> ` rMarkK Left
* -> * rDelay Left
-- rMarkI1
` -> ` rMarkI Left
* -> * rDelay Left
-- rMarkS
s -> S rStart Right
* -> * rMarkS Left
-- rMarkK
k -> K rStart Right
* -> * rMarkK Left
-- rMarkI
i -> I rStart Right
* -> * rMarkI Left

-- rRoll
$ -> $ rRun Right
S -> S rBack Left
K -> K rRollP Left
I -> I rRollP Left
* -> * rRoll Left
-- rRollP
$ -> $ rRun Right
S -> S rBack Left
K -> K rRollP Left
I -> I rRollP Left
* -> * rRollP Left
-- rBack
$ -> $ rRun Right
S -> s rBack Left
K -> K rBack Left
I -> I rBack Left
* -> * rBack Left

-- rRun
` -> . rApply Left
s -> . rStackS Left
k -> . rStackK Left
i -> . rStackI Left
S -> . rSearchS1 Right
K -> . rSearchK1 Right
I -> . rSearchI Right
. -> . rRun Right
_ -> _ rRestart Left

-- rSearchI
` -> . rRun Right
-- rSearchK1
` -> . rSearchK2 Right
-- rSearchK2
` -> . rProcK Left
-- rSearchS1
` -> . rSearchS2 Right
-- rSearchS2
` -> . rSearchS3 Right
-- rSearchS3
` -> . rProcS Left

-- rBackK
. -> ` rStackK Left

-- rProcK
@ -> _ rLeftK Right
* -> * rProcK Left

-- rLeftK
_ -> @ rLeftK1 Right
-- rLeftK1
_ -> _ rLeftK2 Right
* -> * rLeftK1 Right
-- rLeftK2
$ -> $ rPosK Left
_ -> _ rPosK Left
* -> * rLeftK2 Right
-- rPosK
* -> ^ rDelK Left
-- rDelK
_ -> _ rCopy Left
* -> _ rDelK Left
-- rCopy
` -> _ rCopyA Right
s -> _ rCopyS Right
k -> _ rCopyK Right
i -> _ rCopyI Right
_ -> _ rCopy Left
@ -> _ rEndK Right
-- rCopyA
_ -> _ rCopyA Right
^ -> ` rStubK Left
-- rCopyS
_ -> _ rCopyS Right
^ -> s rStubK Left
-- rCopyK
_ -> _ rCopyK Right
^ -> k rStubK Left
-- rCopyI
_ -> _ rCopyI Right
^ -> i rStubK Left
-- rStubK
* -> ^ rCopy Left
-- rEndK
_ -> _ rEndK Right
^ -> _ rPut@ Left

-- rProcS
@ -> @ rProcS1 Right
* -> * rProcS Left
-- rProcS1
_ -> _ rProcS2 Right
-- rProcS2
_ -> _ rProcS3 Right
* -> * rProcS2 Right
-- rProcS3
_ -> _ rNextDup Right
* -> * rProcS3 Right
-- rNextDup
` -> ' rDupA Left
s -> S rDupS Left
k -> K rDupK Left
i -> I rDupI Left
$ -> $ rAddA Left
_ -> _ rAddA Left
-- rDupA
_ -> _ rDupA1 Left
* -> * rDupA Left
-- rDupA1
_ -> _ rReplaceA Left
* -> * rDupA1 Left
-- rDupS
_ -> _ rDupS1 Left
* -> * rDupS Left
-- rDupS1
_ -> _ rReplaceS Left
* -> * rDupS1 Left
-- rDupK
_ -> _ rDupK1 Left
* -> * rDupK Left
-- rDupK1
_ -> _ rReplaceK Left
* -> * rDupK1 Left
-- rDupI
_ -> _ rDupI1 Left
* -> * rDupI Left
-- rDupI1
_ -> _ rReplaceI Left
* -> * rDupI1 Left

-- rReplaceA
` -> ` rReplaceA Left
s -> ` rReplaceS Left
k -> ` rReplaceK Left
i -> ` rReplaceI Left
_ -> ` rSearchPop Left
-- rReplaceS
` -> s rReplaceA Left
s -> s rReplaceS Left
k -> s rReplaceK Left
i -> s rReplaceI Left
_ -> s rSearchPop Left
-- rReplaceK
` -> k rReplaceA Left
s -> k rReplaceS Left
k -> k rReplaceK Left
i -> k rReplaceI Left
_ -> k rSearchPop Left
-- rReplaceI
` -> i rReplaceA Left
s -> i rReplaceS Left
k -> i rReplaceK Left
i -> i rReplaceI Left
_ -> i rSearchPop Left
-- rSearchPop
* -> _ rSearch@ Left
-- rSearch@
* -> @ rSearchDup Right

-- rSearchDup
' -> ` rNextDup Right
S -> s rNextDup Right
K -> k rNextDup Right
I -> i rNextDup Right
* -> * rSearchDup Right

-- rAddA
_ -> _ rAddA1 Left
* -> * rAddA Left
-- rAddA1
_ -> ` rAdd2A Left
* -> * rAddA1 Left
-- rAdd2A
@ -> _ rNext@ Left
* -> * rAdd2A Left
-- rNext@
_ -> @ rRep1 Right
-- rRep1
_ -> _ rRep2 Right
-- rRep2
_ -> ` rApA Right

-- rStackS
@ -> s rPut_ Left
* -> * rStackS Left
-- rStackK
@ -> k rPut_ Left
* -> * rStackK Left
-- rStackI
@ -> i rPut_ Left
* -> * rStackI Left

-- rPut_
_ -> _ rPut@ Left

-- rPut@
_ -> @ rDone Right

-- rApply
@ -> @ rAp1 Right
* -> * rApply Left

-- rAp1
_ -> _ rApA Right
* -> * rAp1 Right

-- rApA
_ -> ` rDone Right
` -> ` rApA Right
s -> ` rApS Right
k -> ` rApK Right
i -> ` rApI Right
-- rApS
_ -> s rDone Right
` -> s rApA Right
s -> s rApS Right
k -> s rApK Right
i -> s rApI Right
-- rApK
_ -> k rDone Right
` -> k rApA Right
s -> k rApS Right
k -> k rApK Right
i -> k rApI Right
-- rApI
_ -> i rDone Right
` -> i rApA Right
s -> i rApS Right
k -> i rApK Right
i -> i rApI Right

-- rDone
$ -> $ rRun Right
* -> * rDone Right

-- rRestart
. -> _ rRestart Left
@ -> _ rReflect Right
* -> * rRestart Left

-- rReflect
$ -> $ rRefl@ Right
* -> * rReflect Right
-- rRefl@
_ -> @ Start Left

-- End
@ -> _ End2 Left
-- End2
* .

Write
```skkk
Result : 1
