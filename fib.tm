-- Write
a -> a Write1 Right
-- Write1
_ -> | Write2 Right
-- Write2
_ -> $ Start Left

-- Start
a -> a Search Left
* -> * Start Left
-- Search
_ -> _ Elim Right
* -> * Search Left
-- Elim
a -> _ Calc Right
-- Calc
$ -> $ Back Left
* -> * Calc Right
-- Back
| -> | Orig Right
_ -> _ Back1 Left 
-- Back1
| -> | Orig Right
_ -> _ Back2 Left
* -> * Back1 Left
-- Back2
| -> | Copy Right
_ -> _ Copy Right
* -> * Back2 Left
-- Orig
$ -> 1 Append_ Right
* -> * Orig Right

-- Copy
1 -> I Move Right
_ -> _ Copy2 Right
-- Move
$ -> 1 Put$ Right
* -> * Move Right
-- Put$
_ -> $ RevI Left
-- RevI
I -> 1 Copy Right
* -> * RevI Left

-- Copy2
1 -> I Move2 Right
_ -> _ Done Right
-- Move2
$ -> 1 Put$2 Right
* -> * Move2 Right
-- Put$2
_ -> $ RevI2 Left
-- RevI2
I -> 1 Copy2 Right
* -> * RevI2 Left

-- Done
$ -> _ Done_ Right
* -> * Done Right
-- Done_
_ -> $ Finish Left

-- Append_
* -> _ Append$ Right
-- Append$
* -> $ Start Left

-- Finish
| -> | Check Left
* -> * Finish Left
-- Check
a -> a Start Right
* .

Write
aaaaaaaa
Result : 63
