import com.kitro.collections._

val f = (i: Int) => List(i - 1, i, i + 1)
val list = List(5, 6, 7)
println(list.map(f))// prints List(4, 5, 6, 5, 6, 7, 6, 7, 8)

val ko = Packet("lala")
val ko1 = ko.flatMap(Packet("lo"))(x => Packet(x.toUpperCase))

println(ko1)
