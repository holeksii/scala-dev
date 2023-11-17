class Animal
class Dog extends Animal
class Cat extends Animal
class Crocodile extends Animal

// class MyList[T]

// if Dog extends Animal (Dog <- Animal), then MyList[Dog] extends MyList[Animal] (MyList[Dog] <- MyList[Animal])
// the variance question

// 1 - yes => generic type is COVARIANT
// class MyList[+T]
// val animal: Animal = new Dog
// val animals: MyList[Animal] = new MyList[Dog]

// 2 - no => generic type is INVARIANT or NONVARIANT
// class MyList[T]
// val animal: Animal = new Dog
// val animals: MyList[Animal] = new MyList[Dog] // error

// 3 - hell no => generic type is CONTRAVARIANT
// class Vet[-T]
// val animal: Animal = new Dog
// val vet: Vet[Dog] = new Vet[Animal]
