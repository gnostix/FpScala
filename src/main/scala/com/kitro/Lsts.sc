val li1 = List(List(1,2), List(3,4))
li1.flatMap(x => x)
li1.flatten

List(1,2,3).flatten(x => List(x))