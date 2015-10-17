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
$ .
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
@ -> _ MirrorS Left
* -> * Restart Right
-- MirrorS
$ -> $ GetNext Right
* -> * MirrorS Left
-- MirrorF
$ -> $ GetNext Right
* -> * MirrorF Right
-- GetNext
` -> . TransA Left
s -> . TransS Left
k -> . TransK Left
i -> . TransI Left
. -> . GetNext Right
_ -> _ ResEnd Left
-- TransA
_ -> ` MirrorF Right
* -> * TransA Left
-- TransS
_ -> s MirrorF Right
* -> * TransS Left
-- TransK
_ -> k MirrorF Right
* -> * TransK Left
-- TransI
_ -> i MirrorF Right
* -> * TransI Left

-- ResEnd
. -> _ ResEnd Left
$ -> $ Finish@ Right

-- Finish@
_ -> @ Start Left

Write
```s`k``sii``s``s`ksk`k``sii`k`sk
