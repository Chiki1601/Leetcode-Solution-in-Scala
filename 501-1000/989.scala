object Solution {
    def addToArrayForm1(num: Array[Int], k: Int): List[Int] = {
        var n= num.foldLeft(BigInt(0))  (_*10+_)  + k   // use ( ) or { } both ok
        var res=List[Int]()
        while (n!=0) {
            res = (n%10).toInt :: res
            n=n/10
        }
        res
    }
    def addToArrayForm(num: Array[Int], k: Int): List[Int] = {
        var carry=0
        var res=Array.fill[Int](num.size.max((Math.floor(Math.log10(k))+1).toInt)) {0}
        for (i <- 0 until res.size) {
           var a= if (i<num.size) num(num.size-1-i) else 0
           var b=  (k/(Math.pow(10,i).toInt))%10
            res(res.size-1-i)=(carry+a+b)%10
            carry=(carry+a+b)/10
        }
        (if (carry!=0) List(1) else Nil ) ::: res.toList
    }
    def addToArrayForm3(num: Array[Int], k: Int): List[Int] = {
        // init value z must be BigInt to avoid overflow with case like  [9,9,9,9,9,9,9,9,9,9] 1
       // optionally specify return type of lambda : var n= (num.foldLeft(BigInt(0)) { _*10+_} : BigInt) + k 
       // another syntax for specify lambda return type:  val x: () => Long = () => System.currentTimeMillis 
        //var n= num.foldLeft(BigInt(0)) {  _*10+_ } + k  //use BigInt(0) to help compiler infer init value type and return type , ok, with or without parenthesis (... ) +k both ok
        //var n= num.foldLeft(BigInt(0))  (_*10+_)  + k   // use ( ) or { } both ok
       //var n= (num.foldLeft(BigInt(0)) { _*10+_ }: BigInt) + k   // with or w/o :BigInt return tyep for lambda both ok
       //var n= (num.foldLeft(BigInt(0)) ( _*10+_ ): BigInt) + k  //ok
        //var n= (num.foldLeft(BigInt(0)) {(a:BigInt,b:Int) => a*10+b }:BigInt) + k  //ok
        var n= num.foldLeft(BigInt(0))  (_*10+_)  + k   // use ( ) or { } both ok
        var res=List[Int]()
        while (n!=0) {
            res = (n%10).toInt :: res
            n=n/10
        }
        res
    }
}
