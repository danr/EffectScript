cd boot
bnfc EffectScript.bnfc > /dev/null
alex -g LexEffectScript.x
happy -gca ParEffectScript.y -iinfo
w=13
cat info | grep \(reduce -B $w | grep ^State -A $w
