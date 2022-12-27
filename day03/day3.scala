def priority(c: Char): Int = 
  val z = c.toInt
  return if (z <= 90) then z - 38 else z - 96

def allSame(xs: Seq[String]): Char =
	val common = xs.foldLeft(('a' to 'z').toSet ++ ('A' to 'Z').toSet)((a,b) => a & (b.toSet))
	return common.head

@main def hello: Unit = 
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.mkString.split("\n") finally source.close()
  println(f"Part 1: ${lines.map(x => priority(allSame(List(x.take(x.length/2), x.drop(x.length/2))))).sum}")
  println(f"Part 2: ${lines.grouped(3).map(x => priority(allSame(x))).sum}")
