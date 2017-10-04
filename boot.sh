cd boot
rm ParEffectScript.y info 2> /dev/null
bnfc EffectScript.bnfc > /dev/null
alex -g LexEffectScript.x
happy -gca ParEffectScript.y -iinfo
w=16
cat info | grep \(reduce -B $w | grep ^State -A $w
