object ejemplos_y_pruebas {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
   val l1 = List(1,2,3,4)                         //> l1  : List[Int] = List(1, 2, 3, 4)

 l1.foreach((n: Int) => println(n + " x10: " + (n*10)))
                                                  //> 1 x10: 10
                                                  //| 2 x10: 20
                                                  //| 3 x10: 30
                                                  //| 4 x10: 40

//------
 val l2 = new Array[String](3)                    //> l2  : Array[String] = Array(null, null, null)


 l2(0) = "Me"

 l2(0) = "Soy"

 println(l2(0))                                   //> Soy
//--------
 var tupla1 = (1 , "uno", List(1))                //> tupla1  : (Int, String, List[Int]) = (1,uno,List(1))


 tupla1._1                                        //> res0: Int = 1


 tupla1._2                                        //> res1: String = uno


 tupla1._3                                        //> res2: List[Int] = List(1)

//------------
//conjuntos - pag25

//------------
 val m = Map( "qw" -> List('q', 'w'), "as" -> List('a','s'))
                                                  //> m  : scala.collection.immutable.Map[String,List[Char]] = Map(qw -> List(q, w
                                                  //| ), as -> List(a, s))


 println(m("qw"))                                 //> List(q, w)
//--------------

println("""Bien
          venido""")                              //> Bien
                                                  //|           venido
  
  2.unary_-                                       //> res3: Int = -2
  
  0 max 5                                         //> res4: Int = 5
  
  class Complejo(r:Int,c:Int){
    mostrar()
    def mostrar() ={
      println(r + " + "+ c +"i")
    }
  }
  
  val test = new Complejo(2,3)                    //> 2 + 3i
                                                  //| test  : ejemplos_y_pruebas.Complejo = ejemplos_y_pruebas$$anonfun$main$1$Com
                                                  //| plejo$1@215be6bb


  for(i <- 1 to 10
    if(i%2==0)
    if(i > 3)
    )
  {
    println("soy el for : " + i)                  //> soy el for : 4
                                                  //| soy el for : 6
                                                  //| soy el for : 8
                                                  //| soy el for : 10
  }
  
  for(i <- 1 to 3;
      j <- 1 to 4
    )
  {
    println("soy el for : "
      + i + "-" + j)                              //> soy el for : 1-1
                                                  //| soy el for : 1-2
                                                  //| soy el for : 1-3
                                                  //| soy el for : 1-4
                                                  //| soy el for : 2-1
                                                  //| soy el for : 2-2
                                                  //| soy el for : 2-3
                                                  //| soy el for : 2-4
                                                  //| soy el for : 3-1
                                                  //| soy el for : 3-2
                                                  //| soy el for : 3-3
                                                  //| soy el for : 3-4
  }


  var a = Array(Array(1,2,3)
      ,Array(4,5,6),Array(7,8,9))                 //> a  : Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9
                                                  //| ))
  var b = new Array[Array[Int]](3)                //> b  : Array[Array[Int]] = Array(null, null, null)
  
  for(i<-0 until 3)
    b(i) = new Array[Int](3)
  
  for(i<-0 to 2;c = a(i);j<-0 to 2)
    b(j)(i)=c(j)
    
  println(a)                                      //> [[I@62ee68d8
  println(b)                                      //> [[I@735b5592

  for(i <-1 until 5) yield i                      //> res5: scala.collection.immutable.IndexedSeq[Int] = Vector(1, 2, 3, 4)

//-------
def suma(a:Int,b:Int) ={
  val sum=a+b
  println("suma_> " + sum )
 }                                                //> suma: (a: Int, b: Int)Unit

val temp = suma(3,_:Int)                          //> temp  : Int => Unit = <function1>
temp(2)                                           //> suma_> 5




}