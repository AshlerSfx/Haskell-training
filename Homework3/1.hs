--func x l = map (\y -> x * y) l применяем eta - редукцию по l
--func x = map (\y -> y*x) заменяем лямбда выражение
--func x = map (*x) применяем композицию f.g
--func x = map.(*) $x eta-редукция x

func = map.(*)
