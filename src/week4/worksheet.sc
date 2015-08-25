package week4

object worksheet {
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat = throw new Error("0.predecessor")
    def successor: Nat = new Succ(this)
    def +(that: Nat): Nat = that
    def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
  }
  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def successor = new Succ(this)
    def +(that: Nat): Nat = new Succ(n + that)
    def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
  }
  
  Zero + new Succ(Zero)                           //> res0: week4.worksheet.Nat = week4.worksheet$Succ@48533e64
}