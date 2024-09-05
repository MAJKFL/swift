// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation
class X {
  typealias A = Int
  
  class Foo {
    let a: A = 123
    
    func bar<A, B: A>() {
      let a: A = 123
      let a: B = SomeType(a)
      let s: Self = self // expected-error{{cannot convert value of type 'X.Foo' to specified type 'Self'}}
    }
  }
}
