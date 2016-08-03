Wells background sounds
=======================

```
Original sound number / LR balance / Name

 1 LLLR  CarrionCrow
 2 LRRRR Chaffinch
12 -     CommonPheasant
 7 LLLLR EuropeanGreenfinch
11 -     EuropeanNightjar
 5 LRR   Fieldfare
 6 LLR   GrasshopperWarbler
 3 LR    GreatSpottedWoodpecker
 8 LLLR  GreatTit
 4 LRR   HerringGull
 9 LLLR  HouseSparrow
13 -     LittleGrebe
10 ?     ReedBunting
```

```
R db reduction
-3 LLR
-6 LLLR
-9 LLLLR
```

Tapping only: `ecasound -i $i -o $OUT -ea:350`


Wells script sounds processing from Cam's originals
===================================================

1. Pitch reduction by 6%
1. Amplification: `ecasound -i $i -o $OUT -ea:600`
1. `oggenc -q 1 --downmix $OUT`

