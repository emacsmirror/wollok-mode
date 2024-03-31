
const adultAge = 21   // a single line comment

/* asdad este es un comentariooo
sd ds dasd as das ldkhsasd
s dsd sd ssd ssssd asd sd sd
 */
 "asdd \"d sd s ds"
adultAge = 18 //this will crash!!

asd.tsds();

const a = 1
var b = a + 10  // add
b = b - 1       // substract
b = b * 2       // multiply
b = b / 2       // divide
b = b % 2       // module
b = b ** 3      // raised to (3 in this case)

b++             // b = b + 1
b--             // b = b - 1

b += 2          // b = b + 2
b -= 1          // b = b - 1
b *= 3          // b = b * 3
b %= 2          // b = b % 2
b /= 2          // b = b / 2
5.between(2, 7) // whether 5 is between 2 and 7 ==> true
3.min(6)        // the smaller of 3 and 6 ==> 3
3.max(6)        // the greater of 3 and 6 ==> 6
(3.1416).truncate(0)  // integer part of 3.1416 ==> 3 -- (3.1416).truncate(2) = 3.14
(3.1416).roundUp(0)   // the smallest integer greater than 3.1416 ==> 4  -- (3.1416).roundUp(2) = 3.15
const fact = true and true
const isTrue = true
const isFalse = false

const willBeFalse = isTrue and isFalse

const willBeTrue = isTrue or isFalse

const willBeTrue = not false

const hoy = new Date()
        // takes the current day
const unDiaCualquiera = new Date(day = 30, month = 6, year = 1973)

program helloWorld {
   console.println("Hello World!")
}

const myObject = object {
    // code here
}
console.println(myObject)

var sds = 235;

asdsdfasdF() {

}

object myObject {
  method fly() {
         // ""some code
         return
  }
}

class muClass {
}

const myObject = object {

}

class myClass {

}
const aBird = new Bird()
aBird.fly(23)
const pepita = new Bird(energy = 100, weight = 1) // Valid (energy = 100, weight = 1)

const pepita = new Bird(weight = 1, energy = 100) // Valid (energy = 100, weight = 1)

const pepita = new Bird(weight = 2)   // Valid (energy = 0, weight = 2)

const pepita = new Bird(energy = 100) // Error (weight is not initialized)

const pepita = new Bird()

class SwimmingBird inherits Bird {

   method swim(meters) {
       energy = energy - (meters * 1.2)
   }
}

const aBird = new SwimmingBird()
aBird.swim(10)
aBird.fly(50)



object lassie inherits Dog {
   // ...
}

package birds {

   class Bird {
        method fly(to) {
             // ...
        }
   }

   class Plane {
        method fly(to) {
            // ...
        }
   }

}


program "aProgram" {
    pepita.fly(20)
    const t = new Trainer()
    t.train(pepita)
}


mixin Flies {
  method fly() {
    console.println("I'm flying")
  }
}

program helloWorld {
  console.println(new C().saludar()) // prints "hi", M1 has precedence over M2
}

super()

program t {
    const w = object inherits Energy and Warrior {}
    assert.equals(100, w.energy())
}

const warrior1 = object inherits GetsHurt and Energy(energy = 150) and Attacks(power = 20) and Warrior(name = "Mati") {
  ...
}

   class Bird {
       method fly() { ... }
       method eat() { ... }
   }

   class SwimmingBird inherits Bird {
       method swim() { ... }
   }

   const bird = new Bird()
   const duck = new SwimmingBird()

   class Superman {
       method fly() { ... }
       method throwLaserFromEyes() { ... }
   }

  var flier = bird
  flier = new Superman()

class MyException inherits wollok.lang.Exception {}
class A {
    method m1() {
        throw new MyException()
    }
}


import wollok.game.*

object wollok {
  var property position = game.origin()
  method howAreYou() = "I am Wolloktastik!"
  method image() = "wollok.png"
}

object box {
  var property position = game.center()
  method image() = "box.png"
  method goUp(){
    position = position.up(1)
  }
}

const aString = "hello"
const another = 'world'
const article = "This is the title" + console.newline() + "This is a very short article."
const hoy = new Date()
        // takes the current day
const unDiaCualquiera = new Date(day = 30, month = 6, year = 1973)
        // input is day, month, year

const myObject = object {
    // code here
}

try
    a.m1()
catch e : MySubclassException
    result = 3
catch e : MyException
    result = 2
try {
    a.m1()
}
catch e : MyException
    console.println("Exception raised!") // OK!
then always
    counter = counter + 1
}

console.println(myObject)

program firstWollokGameProgram {
  game.addVisualCharacter(wollok)  //Para que se pueda mover con las flechas
  game.addVisual(box)
  // Once the character collides with the box, the former speaks and the latter moves
  game.whenCollideDo(wollok, { element =>
    element.goUp()
    game.say(wollok,wollok.howAreYou())
  })
  game.start()
}
