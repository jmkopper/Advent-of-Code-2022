def priority(c: Char): Int = 
  val z = c.toInt
  return if (z <= 90) z - 38 else z - 96

def commonChar(xs: Seq[String]): Char =
  (('a' to 'z') ++ ('A' to 'Z')).find(c => xs.forall(_.contains(c))).get

@main def hello: Unit = 
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.mkString.split("\n") finally source.close()
  println(f"Part 1: ${lines.map(x => priority(commonChar(List(x.take(x.length/2), x.drop(x.length/2))))).sum}")
  println(f"Part 2: ${lines.grouped(3).map(x => priority(commonChar(x))).sum}")
