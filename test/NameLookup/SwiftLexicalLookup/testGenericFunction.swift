// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation
class X {
  typealias A = Int
  
  class Foo {
    let a: A = 123
    
    func bar<A: X, B: A>(a: A, b: B) -> A { // expected-error{{type 'B' constrained to non-protocol, non-class type 'A'}}
      let a: A = A.def // expected-note {{'a' previously declared here}} expected-error {{type 'A' has no member 'def'}}
      let a: B = SomeType(a) // expected-error {{invalid redeclaration of 'a'}} expected-error {{cannot find 'SomeType' in scope}}
      let s: Self = self // expected-error{{cannot convert value of type 'X.Foo' to specified type 'Self'}}
    }
  }
}
