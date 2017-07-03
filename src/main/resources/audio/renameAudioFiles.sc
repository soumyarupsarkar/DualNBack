(ls!).filter(_.toString.endsWith(".wav")).map(fn => fn -> cwd/fn.toString.takeRight(5)).map(Function.tupled(mv))

