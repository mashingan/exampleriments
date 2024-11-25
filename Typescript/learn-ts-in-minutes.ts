// run it with: `bun learn-ts-in-minutes.ts`
let isDone: boolean = false
let lines: number = 42
let thename: string = "Anderson"

let isDone1 = false
let lines1 = 42
let name2 = "Anders"

let notSure: any = 4
notSure = "maybe a string"
notSure = false

const numLivesForCat = 9
// numLivesForCat = 10

let list: number[] = [1, 2, 3, 4]
let list2: Array<number> = list

enum Color { Red, Green, Blue }
let col: Color = Color.Green
console.log(col)
console.log(Color[col])

function bigHorribleAlert(): void {
	console.log("Cannot use alert in console")
}

let f1 = function(i: number): number { return i*i }
let f2 = function(i: number) { return i*i }
let f3 = (i: number): number => { return i*i }
let f4 = (i: number) => { return i*i }
let f5 = (i: number) => i*i;

interface Person {
	name: string
	age?: number
	move(): void
}

let p: Person = { name: "Bobby", move: () => {} }
let validPerson: Person = { name: "Bobby", age: 42, move: () => {}, }
// let invalidPerson: Person =  { name: "Bobby", age: true,  }

interface SearchFunc {
	(source: string, subString: string): boolean
}

let mySearch: SearchFunc
mySearch = function(src: string, sub: string): boolean {
	return src.search(sub) != -1
}

class Point {
	x: number
	constructor(x: number, public y: number = 0) {
		this.x = x
	}

	dist(): number { return Math.sqrt(this.x * this.x + this.y * this.y) }

	static origin = new Point(0, 0)

}

class PointPerson implements Person {
	name: string
	move() {}
}

let p1 = new Point(10, 20)
let p2 = new Point(25)

class Point3D extends Point {
	constructor(x: number, y: number, public z: number = 0) {
		super(x, y)
	}

	dist(): number {
		let d = super.dist()
		return Math.sqrt(d*d + this.z * this.z)
	}
}

module Geometry {
	export class Square {
		constructor(public sideLength: number = 0) {}
		area() {
			return Math.pow(this.sideLength, 2)
		}
	}
}

let s1 = new Geometry.Square(5)
import G = Geometry

class Tuple<T1, T2> {
	constructor(public item1: T1, public item2: T2) {}
}

interface Pair<T> {
	item1: T
	item2: T
}

let pairToTuple = function<T>(pair: Pair<T>) {
	return new Tuple(pair.item1, pair.item2)
}

let tuple = pairToTuple({ item1: "hello", item2: "world" })
let name3 = "Tyrone"
let greeting = `Hi ${name3}, how are your?`
let multiline = `This is multiline
for example of multilines string`

interface PersonROnly {
	readonly name: string
	readonly age: number
}

var p1ro: PersonROnly = { name: "Tyrone", age: 24, }
// p1ro.age = 25
var p2ro = { name: "John", age: 24, }
var p3ro: PersonROnly = p2ro
// p3ro.age = 35 // cannot change because p3ro is readonly
p2ro.age = 35

class Car {
	readonly make: string
	readonly model: string
	readonly year = 2099

	constructor() {
		this.make = "Unknown Make"
		this.model = "Unknown model"
	}
}

let numarray: Array<number> = [0, 1, 2, 3, 4]
let moreNum: ReadonlyArray<number> = numarray
/*
moreNum[5] = 5 // error, element are read only
moreNum.push(5) // error, array are read only
moreNum.length = 3 // error, array length is read only
numarray = moreNum // error, mutating methods are missing
*/

type State =
	| { type: "loading" }
	| { type: "success", value: number }
	| { type: "error", message: string }


	/*
declare const st: State
if (st.type === "success") {
	console.log(st.value)
} else if (st.type === "error") {
	console.error(st.message)
}
*/

type OrderSize = "regular" | "large"
type OrderItem = "Espresso" | "Cappuccino"
type Order = `A ${OrderSize} ${OrderItem}`

let order1: Order = "A regular Cappuccino"
let order2: Order = "A large Espresso"
//let order3: Order = "A small Espresso" // error

let arrayOfAnyType = [1, "string", false]
for (const val of arrayOfAnyType) {
	console.log(val)
}
let lnum = [4, 5, 6]
for (const i of lnum) {
	console.log(i)
}

for (const i in lnum) {
	console.log(i)
}

let foo = {}
// foo.bar = 123 // property bar not exists
// foo.baz = "Hello world" // property baz not exists

interface Foo {
	bar: number
	baz: string
}

let foo2 = {} as Foo
foo2.bar = 123
foo2.baz = "Hello world"
