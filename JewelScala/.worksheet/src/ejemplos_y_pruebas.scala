object ejemplos_y_pruebas {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(71); 
  println("Welcome to the Scala worksheet");$skip(29); 
  
   val l1 = List(1,2,3,4);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(57); 

 l1.foreach((n: Int) => println(n + " x10: " + (n*10)));$skip(41); 

//------
 val l2 = new Array[String](3);System.out.println("""l2  : Array[String] = """ + $show(l2 ));$skip(16); 


 l2(0) = "Me";$skip(16); 

 l2(0) = "Soy";$skip(17); 

 println(l2(0));$skip(46); 
//--------
 var tupla1 = (1 , "uno", List(1));System.out.println("""tupla1  : (Int, String, List[Int]) = """ + $show(tupla1 ));$skip(13); val res$0 = 


 tupla1._1;System.out.println("""res0: Int = """ + $show(res$0));$skip(13); val res$1 = 


 tupla1._2;System.out.println("""res1: String = """ + $show(res$1));$skip(13); val res$2 = 


 tupla1._3;System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(113); 

//------------
//conjuntos - pag25

//------------
 val m = Map( "qw" -> List('q', 'w'), "as" -> List('a','s'));System.out.println("""m  : scala.collection.immutable.Map[String,List[Char]] = """ + $show(m ));$skip(20); 


 println(m("qw"));$skip(55); 
//--------------

println("""Bien
          venido""");$skip(15); val res$3 = 
  
  2.unary_-;System.out.println("""res3: Int = """ + $show(res$3));$skip(13); val res$4 = 
  
  0 max 5
  
  class Complejo(r:Int,c:Int){
    mostrar()
    def mostrar() ={
      println(r + " + "+ c +"i")
    }
  };System.out.println("""res4: Int = """ + $show(res$4));$skip(146); 
  
  val test = new Complejo(2,3);System.out.println("""test  : ejemplos_y_pruebas.Complejo = """ + $show(test ));$skip(93); 


  for(i <- 1 to 10
    if(i%2==0)
    if(i > 3)
    )
  {
    println("soy el for : " + i)
  };$skip(103); 
  
  for(i <- 1 to 3;
      j <- 1 to 4
    )
  {
    println("soy el for : "
      + i + "-" + j)
  };$skip(69); 


  var a = Array(Array(1,2,3)
      ,Array(4,5,6),Array(7,8,9));System.out.println("""a  : Array[Array[Int]] = """ + $show(a ));$skip(35); 
  var b = new Array[Array[Int]](3);System.out.println("""b  : Array[Array[Int]] = """ + $show(b ));$skip(52); 
  
  for(i<-0 until 3)
    b(i) = new Array[Int](3);$skip(56); 
  
  for(i<-0 to 2;c = a(i);j<-0 to 2)
    b(j)(i)=c(j);$skip(18); 
    
  println(a);$skip(13); 
  println(b);$skip(30); val res$5 = 

  for(i <-1 until 5) yield i;System.out.println("""res5: scala.collection.immutable.IndexedSeq[Int] = """ + $show(res$5));$skip(81); 

//-------
def suma(a:Int,b:Int) ={
  val sum=a+b
  println("suma_> " + sum )
 };System.out.println("""suma: (a: Int, b: Int)Unit""");$skip(26); 

val temp = suma(3,_:Int);System.out.println("""temp  : Int => Unit = """ + $show(temp ));$skip(8); 
temp(2)}




}
