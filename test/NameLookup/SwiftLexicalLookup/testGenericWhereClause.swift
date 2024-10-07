// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation

protocol P1 {
    associatedtype A
    func f() -> A
}

protocol P2 {
    associatedtype A: P2
    associatedtype B: P2 where Self.A.A == Self.B.A
}

protocol P3 {
    associatedtype A: P3
}

struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}

struct Recur: P2 {
    typealias A = Recur
    typealias B = Recur
}

struct NonRecur: P2 {
    typealias A = Recur
    typealias B = Recur
}

struct Generic<T> {}

class Super<T, U> {}

extension Super: P2 where T: P2, U: P2 {
    typealias A = T
    typealias B = T
  
    func foo() -> Int { fatalError() }
}

class Sub: Super<NonRecur, Recur> {}

struct RecurGeneric<T: P3>: P3 {
    typealias A = RecurGeneric<T>
}

struct Specialize: P3 {
    typealias A = RecurGeneric<Specialize>
}
