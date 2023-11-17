trait Subscriber:
  def handler(pub: Publisher): Unit

trait Publisher:
  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish: Unit =
    subscribers.foreach(_.handler(this))
end Publisher

class BankAccount extends Publisher:
  private var balance = 0

  def currentBalance = balance

  def deposit(am: Int): Unit =
    if am > 0 then
      balance += am
      publish

  def withdraw(amount: Int): Unit =
    if 0 < amount && amount <= balance then
      balance -= amount
      publish
    else throw Error("insufficient funds")

class Consolidator(observed: List[BankAccount]) extends Subscriber:
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute() // total is assigned in ‘compute()‘

  private def compute() =
    total = observed.map(_.currentBalance).sum

  def handler(pub: Publisher) = compute()
  def totalBalance = total
end Consolidator
