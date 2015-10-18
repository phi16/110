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
@ -> _ Reflect Left
* -> * Restart Right

-- Reflect
$ -> $ Refl@ Left
* -> * Reflect Left
-- Refl@
_ -> @ rStart Right




-- rStart
s -> s rMarkrS1 Right
k -> k rMarkrK1 Right
i -> i rMarkrI1 Right
_ -> _ rRoll Left
* -> * rStart Right
-- rDelay
* -> * rStart Right

-- rMarkrS1
` -> ` rMarkrS2 Right
* -> * rDelay Left
-- rMarkrS2
` -> ` rMarkrS3 Right
* -> * rDelay Left
-- rMarkrS3
` -> ` rMarkrS Left
* -> * rDelay Left
-- rMarkrK1
` -> ` rMarkrK2 Right
* -> * rDelay Left
-- rMarkrK2
` -> ` rMarkrK Left
* -> * rDelay Left
-- rMarkrI1
` -> ` rMarkrI Left
* -> * rDelay Left
-- rMarkrS
s -> S rStart Right
* -> * rMarkrS Left
-- rMarkrK
k -> K rStart Right
* -> * rMarkrK Left
-- rMarkrI
i -> I rStart Right
* -> * rMarkrI Left

-- rRoll
$ -> $ rRun Right
S -> S rBack Left
K -> K rRollrP Left
I -> I rRollrP Left
* -> * rRoll Left
-- rRollrP
$ -> $ rRun Right
S -> S rBack Left
K -> K rRollrP Left
I -> I rRollrP Left
* -> * rRollrP Left
-- rBack
$ -> $ rRun Right
S -> s rBack Left
K -> K rBack Left
I -> I rBack Left
* -> * rBack Left

-- rRun
` -> . rApply Left
s -> . rStackrS Left
k -> . rStackrK Left
i -> . rStackrI Left
S -> . rSearchrS1 Right
K -> . rSearchrK1 Right
I -> . rSearchrI Right
. -> . rRun Right
_ -> _ rRestart Left

-- rSearchrI
` -> . rRun Right
-- rSearchrK1
` -> . rSearchrK2 Right
-- rSearchrK2
` -> . rProcrK Left
-- rSearchrS1
` -> . rSearchrS2 Right
-- rSearchrS2
` -> . rSearchrS3 Right
-- rSearchrS3
` -> . rProcrS Left

-- rBackrK
. -> ` rStackrK Left

-- rProcrK
@ -> _ rLeftrK Right
* -> * rProcrK Left

-- rLeftrK
_ -> @ rLeftrK1 Right
-- rLeftrK1
_ -> _ rLeftrK2 Right
* -> * rLeftrK1 Right
-- rLeftrK2
$ -> $ rPosrK Left
_ -> _ rPosrK Left
* -> * rLeftrK2 Right
-- rPosrK
* -> ^ rDelrK Left
-- rDelrK
_ -> _ rCopy Left
* -> _ rDelrK Left
-- rCopy
` -> _ rCopyrA Right
s -> _ rCopyrS Right
k -> _ rCopyrK Right
i -> _ rCopyrI Right
_ -> _ rCopy Left
@ -> _ rEndrK Right
-- rCopyrA
_ -> _ rCopyrA Right
^ -> ` rStubrK Left
-- rCopyrS
_ -> _ rCopyrS Right
^ -> s rStubrK Left
-- rCopyrK
_ -> _ rCopyrK Right
^ -> k rStubrK Left
-- rCopyrI
_ -> _ rCopyrI Right
^ -> i rStubrK Left
-- rStubrK
* -> ^ rCopy Left
-- rEndrK
_ -> _ rEndrK Right
^ -> _ rPut@ Left

-- rProcrS
@ -> @ rProcrS1 Right
* -> * rProcrS Left
-- rProcrS1
_ -> _ rProcrS2 Right
-- rProcrS2
_ -> _ rProcrS3 Right
* -> * rProcrS2 Right
-- rProcrS3
_ -> _ rNextrDup Right
* -> * rProcrS3 Right
-- rNextrDup
` -> ' rDuprA Left
s -> S rDuprS Left
k -> K rDuprK Left
i -> I rDuprI Left
$ -> $ rAddrA Left
_ -> _ rAddrA Left
-- rDuprA
_ -> _ rDuprA1 Left
* -> * rDuprA Left
-- rDuprA1
_ -> _ rReplacerA Left
* -> * rDuprA1 Left
-- rDuprS
_ -> _ rDuprS1 Left
* -> * rDuprS Left
-- rDuprS1
_ -> _ rReplacerS Left
* -> * rDuprS1 Left
-- rDuprK
_ -> _ rDuprK1 Left
* -> * rDuprK Left
-- rDuprK1
_ -> _ rReplacerK Left
* -> * rDuprK1 Left
-- rDuprI
_ -> _ rDuprI1 Left
* -> * rDuprI Left
-- rDuprI1
_ -> _ rReplacerI Left
* -> * rDuprI1 Left

-- rReplacerA
` -> ` rReplacerA Left
s -> ` rReplacerS Left
k -> ` rReplacerK Left
i -> ` rReplacerI Left
_ -> ` rSearchrPop Left
-- rReplacerS
` -> s rReplacerA Left
s -> s rReplacerS Left
k -> s rReplacerK Left
i -> s rReplacerI Left
_ -> s rSearchrPop Left
-- rReplacerK
` -> k rReplacerA Left
s -> k rReplacerS Left
k -> k rReplacerK Left
i -> k rReplacerI Left
_ -> k rSearchrPop Left
-- rReplacerI
` -> i rReplacerA Left
s -> i rReplacerS Left
k -> i rReplacerK Left
i -> i rReplacerI Left
_ -> i rSearchrPop Left
-- rSearchrPop
* -> _ rSearch@ Left
-- rSearch@
* -> @ rSearchrDup Right

-- rSearchrDup
' -> ` rNextrDup Right
S -> s rNextrDup Right
K -> k rNextrDup Right
I -> i rNextrDup Right
* -> * rSearchrDup Right

-- rAddrA
_ -> _ rAddrA1 Left
* -> * rAddrA Left
-- rAddrA1
_ -> ` rAdd2rA Left
* -> * rAddrA1 Left
-- rAdd2rA
@ -> _ rNext@ Left
* -> * rAdd2rA Left
-- rNext@
_ -> @ rRep1 Right
-- rRep1
_ -> _ rRep2 Right
-- rRep2
_ -> ` rAprA Right

-- rStackrS
@ -> s rPut_ Left
* -> * rStackrS Left
-- rStackrK
@ -> k rPut_ Left
* -> * rStackrK Left
-- rStackrI
@ -> i rPut_ Left
* -> * rStackrI Left

-- rPut_
_ -> _ rPut@ Left

-- rPut@
_ -> @ rDone Right

-- rApply
@ -> @ rAp1 Right
* -> * rApply Left

-- rAp1
_ -> _ rAprA Right
* -> * rAp1 Right

-- rAprA
_ -> ` rDone Right
` -> ` rAprA Right
s -> ` rAprS Right
k -> ` rAprK Right
i -> ` rAprI Right
-- rAprS
_ -> s rDone Right
` -> s rAprA Right
s -> s rAprS Right
k -> s rAprK Right
i -> s rAprI Right
-- rAprK
_ -> k rDone Right
` -> k rAprA Right
s -> k rAprS Right
k -> k rAprK Right
i -> k rAprI Right
-- rAprI
_ -> i rDone Right
` -> i rAprA Right
s -> i rAprS Right
k -> i rAprK Right
i -> i rAprI Right

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

Write
``````s``s``s`ksk`k``sii``s``s`ksk`k``sii``s`k`s``s``s`ki``s``si`k`k`ki`kk`ki``s`k`s``s`k`s``s`kskk``s``s`ksk`k``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s``s`ksk`k``s`k`s`k`si``s`k`s`kk``s`k`sik`k`kk`k`k`ki``s``s`kski`s``s`ksk`ki
